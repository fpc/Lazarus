{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Called after checking what packages need compile.
    Check source files and compiled files for name conflicts between packages.

  ToDo:
    - don't check when building clean
    - save ignore date for ?
    - ignore
    - clear ignore date on clean build
}
unit InterPkgConflictFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, contnrs, InterfaceBase, Forms, ComCtrls,
  Controls, ButtonPanel, Themes, Graphics, StdCtrls, Buttons, FileProcs,
  LazFileUtils, AvgLvlTree, BasicCodeTools, DefineTemplates, CodeToolManager,
  // IDEIntf
  ProjectIntf, CompOptsIntf, IDEWindowIntf, LazIDEIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, CompilerOptions, EnvironmentOpts, IDEProcs, DialogProcs,
  TransferMacros, LazConf, IDECmdLine, PackageDefs, PackageSystem, InputHistory;

type
  TPGInterPkgOwnerInfo = class
  public
    Name: string;
    Owner: TObject;
    HasOptionUr: boolean;
    CompOptions: TBaseCompilerOptions;
    BaseDir: string;
    SrcDirs: string; // unitpath without inherited
    IncDirs: string; // incpath without inherited and without SrcDirs
    UnitOutDir: string; // can be empty -> if empty FPC creates ppu in SrcDirs
  end;

  { TPGInterPkgFile }

  TPGInterPkgFile = class
  public
    FullFilename: string;
    ShortFilename: string;
    AnUnitName: string;
    OwnerInfo: TPGInterPkgOwnerInfo;
    constructor Create(TheFullFilename, TheUnitName: string; Owner: TPGInterPkgOwnerInfo);
  end;

  { TPGIPAmbiguousFile }

  TPGIPAmbiguousFile = class
  public
    Filename: string;
    OwnerInfo: TPGInterPkgOwnerInfo;
    ConflictFilename: string;
    ConflictOwner: TPGInterPkgOwnerInfo;
    procedure Switch; virtual;
  end;

  { TPGIPAmbiguousCompiledFile }

  TPGIPAmbiguousCompiledFile = class(TPGIPAmbiguousFile)
  public
    SrcFilename: string; // the corresponding .pas/.pp file, if it exists
    ConflictSrcFilename: string; // the corresponding .pas/.pp file, if it exists
    procedure Switch; override;
  end;

  { TPGIPConflictsDialog }

  TPGIPConflictsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ConflictsTreeView: TTreeView;
    IDEDialogLayoutStorage1: TIDEDialogLayoutStorage;
    ImageList1: TImageList;
    procedure ConflictsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ConflictsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteSelectedFilesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    DeleteSelectedFilesButton: TButton;
    FImgIndexChecked: integer;
    FImgIndexUnchecked: integer;
    procedure UpdateButtons;
    procedure IgnoreConflicts;
    procedure IgnoreConflict(AmbFile: TPGIPAmbiguousFile);
  public
    SrcFiles: TObjectList; // list of TPGIPAmbiguousFile
    CompiledFiles: TObjectList; // list of TPGIPAmbiguousCompiledFile
    FilesChanged: boolean;
    procedure Init(TheSrcFiles, TheCompiledFiles: TObjectList);
  end;

function CheckInterPkgFiles(IDEObject: TObject;
  PkgList: TFPList; out FilesChanged: boolean
  ): boolean; // returns false if user cancelled

implementation

{$R *.lfm}

function ComparePGInterPkgFullFilenames(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  Result:=CompareFilenames(F1.FullFilename,F2.FullFilename);
end;

function ComparePGInterPkgUnitnames(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  Result:=CompareDottedIdentifiers(PChar(Pointer(F1.AnUnitName)),PChar(Pointer(F2.AnUnitName)));
end;

function ComparePGInterPkgShortFilename(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  Result:=CompareFilenames(F1.ShortFilename,F2.ShortFilename);
end;

{ TPGIPAmbiguousCompiledFile }

procedure TPGIPAmbiguousCompiledFile.Switch;
var
  s: String;
begin
  inherited Switch;
  s:=SrcFilename; SrcFilename:=ConflictSrcFilename; ConflictSrcFilename:=s;
end;

{ TPGIPAmbiguousFile }

procedure TPGIPAmbiguousFile.Switch;
var
  s: String;
  o: TPGInterPkgOwnerInfo;
begin
  s:=Filename; Filename:=ConflictFilename; ConflictFilename:=s;
  o:=OwnerInfo; OwnerInfo:=ConflictOwner; ConflictOwner:=o;
end;

{ TPGIPConflictsDialog }

procedure TPGIPConflictsDialog.ConflictsTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  Detail: TThemedButton;
  Details: TThemedElementDetails;
  aSize: TSize;
  NodeRect: Classes.TRect;
  r: TRect;
begin
  if Stage<>cdPostPaint then exit;
  if TObject(Node.Data) is TPGIPAmbiguousFile then begin
    if Node.ImageIndex=FImgIndexChecked then
      Detail := tbCheckBoxCheckedNormal
    else
      Detail := tbCheckBoxUncheckedNormal;
    Details := ThemeServices.GetElementDetails(Detail);
    aSize := ThemeServices.GetDetailSize(Details);
    NodeRect:=Node.DisplayRect(false);
    r:=Bounds(Node.DisplayIconLeft+(ImageList1.Width-aSize.cx) div 2,
       NodeRect.Top+(NodeRect.Bottom-NodeRect.Top-aSize.cy) div 2,
       aSize.cx,aSize.cy);
    ThemeServices.DrawElement(ConflictsTreeView.Canvas.Handle,Details,r);
  end;
end;

procedure TPGIPConflictsDialog.ConflictsTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node:=ConflictsTreeView.GetNodeAt(X,Y);
  if Node=nil then exit;
  if TObject(Node.Data) is TPGIPAmbiguousFile then begin
    if (X>=Node.DisplayIconLeft) and (X<Node.DisplayTextLeft) then begin
      if Node.ImageIndex=FImgIndexChecked then
        Node.ImageIndex:=FImgIndexUnchecked
      else
        Node.ImageIndex:=FImgIndexChecked;
      Node.SelectedIndex:=Node.ImageIndex;
      UpdateButtons;
    end;
  end;
end;

procedure TPGIPConflictsDialog.DeleteSelectedFilesButtonClick(Sender: TObject);

  function DeleteFileGroup(aFilename: string): boolean;
  begin
    {$IFDEF VerboseCheckInterPkgFiles}
    debugln(['DeleteFileGroup ',aFilename]);
    {$ENDIF}
    if DeleteFileInteractive(aFilename)<>mrOk then exit(false);
    if FilenameIsPascalUnit(aFilename) then
    begin
      DeleteFileUTF8(ChangeFileExt(aFilename,'.ppu'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.o'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.rst'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.rsj'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.lfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.dfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.xfm'));
    end else if (CompareFileExt(aFilename,'ppu')=0)
    or (CompareFileExt(aFilename,'o')=0) then begin
      DeleteFileUTF8(ChangeFileExt(aFilename,'.ppu'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.o'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.rst'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.rsj'));
      if FileExistsCached(ChangeFileExt(aFilename,'.pas'))
      or FileExistsCached(ChangeFileExt(aFilename,'.pp'))
      or FileExistsCached(ChangeFileExt(aFilename,'.p')) then begin
        // delete only compiled file
      end else begin
        // no source in this directory => delete copied lfm file
        DeleteFileUTF8(ChangeFileExt(aFilename,'.lfm'));
        DeleteFileUTF8(ChangeFileExt(aFilename,'.dfm'));
        DeleteFileUTF8(ChangeFileExt(aFilename,'.xfm'));
      end;
    end;
    Result:=true;
  end;

var
  Node: TTreeNode;
  AmbFile: TPGIPAmbiguousFile;
  LastAmbFile: TPGIPAmbiguousFile;
  NextNode: TTreeNode;
  DeleteFile1: Boolean;
  DeleteFile2: Boolean;
begin
  ConflictsTreeView.Items.BeginUpdate;
  try
    Node:=ConflictsTreeView.Items.GetFirstNode;
    LastAmbFile:=nil;
    while Node<>nil do
    begin
      NextNode:=Node.GetNext;
      if TObject(Node.Data) is TPGIPAmbiguousFile then
      begin
        AmbFile:=TPGIPAmbiguousFile(Node.Data);
        {$IFDEF VerboseCheckInterPkgFiles}
        debugln(['TPGIPConflictsDialog.DeleteSelectedFilesButtonClick ',Node.Text,' File=',AmbFile.Filename]);
        {$ENDIF}
        if AmbFile<>LastAmbFile then
        begin
          DeleteFile1:=Node.ImageIndex=FImgIndexChecked;
          DeleteFile2:=false;
        end else begin
          DeleteFile2:=Node.ImageIndex=FImgIndexChecked;
          {$IFDEF VerboseCheckInterPkgFiles}
          debugln(['TPGIPConflictsDialog.DeleteSelectedFilesButtonClick Delete 1=',DeleteFile1,' 2=',DeleteFile2]);
          {$ENDIF}
          if DeleteFile1 then
            if not DeleteFileGroup(AmbFile.Filename) then exit;
          if DeleteFile2 then
            if not DeleteFileGroup(AmbFile.ConflictFilename) then exit;
          if not FileExistsUTF8(AmbFile.Filename)
          or not FileExistsUTF8(AmbFile.ConflictFilename) then begin
            // conflict does not exist anymore
            FilesChanged:=true;
            Node:=Node.Parent;
            NextNode:=Node.GetNextSkipChildren;
            Node.Delete;
          end;
        end;
        LastAmbFile:=AmbFile;
      end;
      Node:=NextNode;
    end;
  finally
    ConflictsTreeView.Items.EndUpdate;
    UpdateButtons;
  end;
end;

procedure TPGIPConflictsDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TPGIPConflictsDialog.FormCreate(Sender: TObject);
var
  Details: TThemedElementDetails;
  aSize: TSize;
  Img: TBitmap;
begin
  IDEDialogLayoutList.ApplyLayout(Self,Width,Height);

  DeleteSelectedFilesButton:=TButton.Create(Self);
  with DeleteSelectedFilesButton do
  begin
    Name:='DeleteSelectedFilesButton';
    Caption:='Delete selected files';
    Align:=alLeft;
    AutoSize:=true;
    OnClick:=@DeleteSelectedFilesButtonClick;
    Parent:=ButtonPanel1;
  end;

  ButtonPanel1.OKButton.Kind:=bkIgnore;
  ButtonPanel1.OKButton.Caption:='Ignore';
  ButtonPanel1.OKButton.OnClick:=@OkButtonClick;

  Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
  aSize := ThemeServices.GetDetailSize(Details);
  ImageList1.Width:=Max(16,aSize.cx);
  ImageList1.Height:=Max(16,aSize.cy);
  // add empty images
  Img:=TBitmap.Create;
  Img.TransparentMode:=tmFixed;
  Img.TransparentColor:=0;
  Img.Transparent:=true;
  Img.SetSize(ImageList1.Width,ImageList1.Height);
  FImgIndexChecked:=ImageList1.Add(Img,nil);
  FImgIndexUnchecked:=ImageList1.Add(Img,nil);
  Img.Free;
end;

procedure TPGIPConflictsDialog.OkButtonClick(Sender: TObject);
begin
  IgnoreConflicts;
end;

procedure TPGIPConflictsDialog.UpdateButtons;
var
  Node: TTreeNode;
  DeleteCount: Integer;
  ConflictCount: Integer;
begin
  DeleteCount:=0;
  ConflictCount:=0;
  Node:=ConflictsTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if TObject(Node.Data) is TPGIPAmbiguousFile then
    begin
      inc(ConflictCount);
      if Node.ImageIndex=FImgIndexChecked then
        inc(DeleteCount);
    end;
    Node:=Node.GetNext;
  end;
  DeleteSelectedFilesButton.Enabled:=DeleteCount>0;
  if ConflictCount=0 then
    IgnoreConflicts;
end;

procedure TPGIPConflictsDialog.IgnoreConflicts;
var
  Node: TTreeNode;
  AmbFile: TPGIPAmbiguousFile;
  LastAmbFile: TPGIPAmbiguousFile;
begin
  Node:=ConflictsTreeView.Items.GetFirstNode;
  LastAmbFile:=nil;
  while Node<>nil do begin
    if TObject(Node.Data) is TPGIPAmbiguousFile then
    begin
      AmbFile:=TPGIPAmbiguousFile(Node.Data);
      if AmbFile<>LastAmbFile then begin
        IgnoreConflict(AmbFile);
      end;
      LastAmbFile:=AmbFile;
    end;
    Node:=Node.GetNext;
  end;

  ModalResult:=mrOk;
end;

procedure TPGIPConflictsDialog.IgnoreConflict(AmbFile: TPGIPAmbiguousFile);
begin

  //InputHistories.Ignores.Add(Identifier);
end;

procedure TPGIPConflictsDialog.Init(TheSrcFiles, TheCompiledFiles: TObjectList);

  function AddChild(ParentNode: TTreeNode; Caption: string): TTreeNode;
  begin
    Result:=ConflictsTreeView.Items.AddChild(ParentNode,Caption);
  end;

var
  MainNode: TTreeNode;
  i: Integer;
  AmbFile: TPGIPAmbiguousCompiledFile;
  ItemNode: TTreeNode;
  s: String;
  ConflictNode: TTreeNode;
begin
  SrcFiles:=TheSrcFiles;
  CompiledFiles:=TheCompiledFiles;

  ConflictsTreeView.Items.BeginUpdate;
  ConflictsTreeView.Items.Clear;
  ConflictsTreeView.Images:=ImageList1;
  if CompiledFiles.Count>0 then
  begin
    MainNode:=ConflictsTreeView.Items.Add(nil,'Conflicting compiled files');
    for i:=0 to CompiledFiles.Count-1 do
    begin
      AmbFile:=TPGIPAmbiguousCompiledFile(CompiledFiles[i]);
      s:=ExtractFilename(AmbFile.Filename);
      ConflictNode:=AddChild(MainNode,s);
      begin
        // first file
        s:=ExtractFilename(AmbFile.Filename);
        if AmbFile.OwnerInfo.Owner is TLazPackage then
          s+=' of package '+AmbFile.OwnerInfo.Name
        else
          s+=' of '+AmbFile.OwnerInfo.Name;
        ItemNode:=AddChild(ConflictNode,s);
        ItemNode.ImageIndex:=FImgIndexChecked; // default: delete
        ItemNode.Data:=AmbFile;
        begin
          // file path and src
          AddChild(ItemNode,'File: '+AmbFile.Filename);
          if not FilenameIsPascalSource(AmbFile.Filename) then begin
            if AmbFile.SrcFilename='' then
              s:='No source found'
            else
              s:='Source file: '+AmbFile.SrcFilename;
            AddChild(ItemNode,s);
          end;
        end;
        ItemNode.SelectedIndex:=ItemNode.ImageIndex;

        // conflict file owner
        s:=ExtractFilename(AmbFile.ConflictFilename);
        if AmbFile.ConflictOwner.Owner is TLazPackage then
          s+=' of package '+AmbFile.ConflictOwner.Name
        else
          s+=' of '+AmbFile.ConflictOwner.Name;
        ItemNode:=AddChild(ConflictNode,s);
        ItemNode.ImageIndex:=FImgIndexUnchecked; // default: keep
        ItemNode.Data:=AmbFile;
        begin
          // file path
          AddChild(ItemNode,'File: '+AmbFile.ConflictFilename);
          if not FilenameIsPascalSource(AmbFile.ConflictFilename) then begin
            if AmbFile.ConflictSrcFilename='' then begin
              s:='No source found';
              ItemNode.ImageIndex:=FImgIndexChecked; // default: delete
            end
            else
              s:='Source file: '+AmbFile.ConflictSrcFilename;
            AddChild(ItemNode,s);
          end;
        end;
        ItemNode.SelectedIndex:=ItemNode.ImageIndex;
      end;
    end;
    MainNode.Expand(true);
  end;

  ConflictsTreeView.Items.EndUpdate;

  UpdateButtons;
end;

{ TPGInterPkgFile }

constructor TPGInterPkgFile.Create(TheFullFilename, TheUnitName: string;
  Owner: TPGInterPkgOwnerInfo);
begin
  FullFilename:=TheFullFilename;
  ShortFilename:=ExtractFileName(FullFilename);
  AnUnitName:=TheUnitName;
  OwnerInfo:=Owner;
end;

function CheckInterPkgFiles(IDEObject: TObject; PkgList: TFPList; out
  FilesChanged: boolean): boolean;
{ Scan all source and output directories (Note: they are already cached, because
  this method is called after the checks if a compile is needed).
  Report strange ppu files and duplicate file names.

  IDEObject can be a TProject, TLazPackage or TLazPackageGraph(building IDE)
  PkgList is list of TLazPackage
}
var
  OwnerInfos: TObjectList; // list of TPGInterPkgOwnerInfo
  TargetOS: String;
  TargetCPU: String;
  LCLWidgetType: String;
  FullFiles: TAvgLvlTree; // tree of TPGInterPkgFile sorted for FullFilename
  Units: TAvgLvlTree; // tree of TPGInterPkgFile sorted for AnUnitName
  ShortFiles: TAvgLvlTree; // tree of TPGInterPkgFile sorted for ShortFilename
  AmbiguousCompiledFiles: TObjectList; // list of TPGIPAmbiguousCompiledFile
  AmbiguousSrcFiles: TObjectList; // list of TPGIPAmbiguousFile

  procedure AddOwnerInfo(TheOwner: TObject);
  var
    LazDir: String;
    CustomOptions: String;
    p: Integer;
    OwnerInfo: TPGInterPkgOwnerInfo;
  begin
    OwnerInfo:=TPGInterPkgOwnerInfo.Create;
    OwnerInfos.Add(OwnerInfo);
    OwnerInfo.Owner:=TheOwner;
    if TheOwner is TLazPackage then
    begin
      OwnerInfo.Name:=TLazPackage(TheOwner).IDAsString;
      OwnerInfo.CompOptions:=TLazPackage(TheOwner).LazCompilerOptions as TBaseCompilerOptions;
    end else if TheOwner is TLazProject then
    begin
      OwnerInfo.Name:=TLazProject(TheOwner).GetTitleOrName;
      OwnerInfo.CompOptions:=TLazProject(TheOwner).LazCompilerOptions as TBaseCompilerOptions;
    end
    else if TheOwner=PackageGraph then begin
      // building IDE
      OwnerInfo.Name:='#IDE';
      LazDir:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
      OwnerInfo.BaseDir:=LazDir;
      OwnerInfo.SrcDirs:=LazDir+'ide'
        +';'+LazDir+'debugger'
        +';'+LazDir+'packager'
        +';'+LazDir+'designer'
        +';'+LazDir+'converter';
      OwnerInfo.IncDirs:=OwnerInfo.SrcDirs
        +';'+LazDir+'ide'+PathDelim+'include'+PathDelim+TargetOS
        +';'+LazDir+'ide'+PathDelim+'include'+PathDelim+GetDefaultSrcOSForTargetOS(TargetOS);
      OwnerInfo.UnitOutDir:=LazDir+'units'+PathDelim+TargetCPU+'-'+TargetOS+PathDelim+LCLWidgetType;
    end;
    if OwnerInfo.CompOptions<>nil then begin
      OwnerInfo.BaseDir:=OwnerInfo.CompOptions.BaseDirectory;
      OwnerInfo.SrcDirs:=OwnerInfo.CompOptions.GetPath(
                                    pcosUnitPath,icoNone,false,coptParsed,true);
      OwnerInfo.IncDirs:=OwnerInfo.CompOptions.GetPath(
                                 pcosIncludePath,icoNone,false,coptParsed,true);
      if OwnerInfo.CompOptions.UnitOutputDirectory<>'' then
        OwnerInfo.UnitOutDir:=OwnerInfo.CompOptions.GetUnitOutputDirectory(false);
      CustomOptions:=OwnerInfo.CompOptions.ParsedOpts.GetParsedValue(pcosCustomOptions);
      p:=1;
      OwnerInfo.HasOptionUr:=FindNextFPCParameter(CustomOptions,'-Ur',p)>0;
    end;
    OwnerInfo.IncDirs:=TrimSearchPath(RemoveSearchPaths(OwnerInfo.IncDirs,OwnerInfo.SrcDirs),'');
    OwnerInfo.UnitOutDir:=TrimFilename(OwnerInfo.UnitOutDir);
    OwnerInfo.SrcDirs:=TrimSearchPath(OwnerInfo.SrcDirs,'');
    {$IFDEF VerboseCheckInterPkgFiles}
    debugln(['AddOwnerInfo Name="',OwnerInfo.Name,'"',
      ' SrcDirs="',CreateRelativeSearchPath(OwnerInfo.SrcDirs,OwnerInfo.BaseDir),'"',
      ' IncDirs="',CreateRelativeSearchPath(OwnerInfo.IncDirs,OwnerInfo.BaseDir),'"',
      ' UnitOutDir="',CreateRelativeSearchPath(OwnerInfo.UnitOutDir,OwnerInfo.BaseDir),'"',
      '']);
    {$ENDIF}
  end;

  procedure CollectFilesInDir(OwnerInfo: TPGInterPkgOwnerInfo; Dir: string;
    var SearchedDirs: string; IsIncDir: boolean);
  var
    Files: TStrings;
    aFilename: String;
    Ext: String;
    AnUnitName: String;
    NewFile: TPGInterPkgFile;
  begin
    if Dir='' then exit;
    if not FilenameIsAbsolute(Dir) then
    begin
      debugln(['Inconsistency: CollectFilesInDir dir no absolute: "',Dir,'" Owner=',OwnerInfo.Name]);
      exit;
    end;
    if SearchDirectoryInSearchPath(SearchedDirs,Dir)>0 then exit;
    SearchedDirs+=';'+Dir;
    Files:=nil;
    try
      CodeToolBoss.DirectoryCachePool.GetListing(Dir,Files,false);
      for aFilename in Files do
      begin
        if (aFilename='') or (aFilename='.') or (aFilename='..') then continue;
        if CompareFilenames(aFilename,'fpmake.pp')=0 then continue;
        Ext:=LowerCase(ExtractFileExt(aFilename));
        AnUnitName:='';
        case Ext of
        '.ppu','.o','.pas','.pp','.p':
          if not IsIncDir then
          begin
            AnUnitName:=ExtractFilename(aFilename);
            if not IsDottedIdentifier(AnUnitName) then continue;
          end;
        '.inc': ;
        else
          continue;
        end;
        NewFile:=TPGInterPkgFile.Create(AppendPathDelim(Dir)+aFilename,
                                        AnUnitName,OwnerInfo);
        FullFiles.Add(NewFile);
        ShortFiles.Add(NewFile);
        if AnUnitName<>'' then
          Units.Add(NewFile);
      end;
    finally
      Files.Free;
    end;
  end;

  procedure CollectFilesOfOwner(OwnerInfo: TPGInterPkgOwnerInfo);
  var
    SearchedDirs: String;
    SearchPath: String;
    p: Integer;
    Dir: String;
  begin
    // find all unit and include FullFiles in src, inc and out dirs
    SearchedDirs:='';
    CollectFilesInDir(OwnerInfo,OwnerInfo.UnitOutDir,SearchedDirs,false);
    SearchPath:=OwnerInfo.SrcDirs;
    p:=1;
    repeat
      Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
      if Dir='' then break;
      CollectFilesInDir(OwnerInfo,Dir,SearchedDirs,false);
    until false;
    SearchPath:=OwnerInfo.IncDirs;
    p:=1;
    repeat
      Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
      if Dir='' then break;
      CollectFilesInDir(OwnerInfo,Dir,SearchedDirs,true);
    until false;
  end;

  procedure RemoveSecondaryFiles;
  // remove each .o file if there is an .ppu file, so that there is only one
  // warning per ppu file
  var
    Node: TAvgLvlTreeNode;
    ONode: TAvgLvlTreeNode;
    OFile: TPGInterPkgFile;
    PPUFileName: String;
    SearchFile: TPGInterPkgFile;
    PPUNode: TAvgLvlTreeNode;
  begin
    Node:=Units.FindLowest;
    while Node<>nil do begin
      // for each .o file
      ONode:=Node;
      Node:=Node.Successor;
      OFile:=TPGInterPkgFile(ONode.Data);
      if CompareFileExt(OFile.ShortFilename,'o')<>0 then continue;
      // search corresponding .ppu
      PPUFileName:=ChangeFileExt(OFile.FullFilename,'.ppu');
      SearchFile:=TPGInterPkgFile.Create(PPUFileName,'',nil);
      PPUNode:=FullFiles.Find(SearchFile);
      SearchFile.Free;
      if PPUNode=nil then continue;
      // remove .o file
      ShortFiles.RemovePointer(OFile);
      FullFiles.RemovePointer(OFile);
      Units.Delete(ONode);
      OFile.Free;
    end;
  end;

  function OwnerHasDependency(Owner1, Owner2: TPGInterPkgOwnerInfo): boolean;
  // returns true if Owner1 depends on Owner2
  begin
    if Owner1=Owner2 then exit(true);
    if Owner1.Owner is TLazPackage then
    begin
      if Owner2.Owner is TLazPackage then
      begin
        Result:=PackageGraph.FindDependencyRecursively(
          TLazPackage(Owner1.Owner).FirstRequiredDependency,
          TLazPackage(Owner2.Owner))<>nil;
      end else begin
        // Owner1 is package, Owner2 is project/IDE => not possible
        Result:=false;
      end;
    end else begin
      // Owner1 is project or IDE => true
      Result:=true;
    end;
  end;

  function OptionUrAllowsDuplicate(File1, File2: TPGInterPkgFile): boolean;
  begin
    Result:=true;
    if File1.OwnerInfo.HasOptionUr
    and File2.OwnerInfo.HasOptionUr then
      exit;
    if File1.OwnerInfo.HasOptionUr
    and OwnerHasDependency(File2.OwnerInfo,File1.OwnerInfo) then
      exit;
    if File2.OwnerInfo.HasOptionUr
    and OwnerHasDependency(File1.OwnerInfo,File2.OwnerInfo) then
      exit;
    Result:=false;
  end;

  procedure CheckPPUFilesInWrongDirs;
  { Check if a ppu/o of pkg A has a unit (source or ppu) in another package B
    Unless A uses B and B has -Ur or A has -Ur and B uses A
    => IDE: delete+retry or ignore or cancel
    => lazbuild: warn }

    function FindUnitSource(aPPUFile: TPGInterPkgFile): TPGInterPkgFile;
    // find a unit source (e.g. .pas) in same package

      function FindSrc(Node: TAvgLvlTreeNode; Left: boolean): TPGInterPkgFile;
      begin
        while Node<>nil do begin
          Result:=TPGInterPkgFile(Node.Data);
          if CompareFilenames(ExtractFileNameOnly(Result.ShortFilename),
            ExtractFileNameOnly(aPPUFile.ShortFilename))<>0 then break;
          if (aPPUFile.OwnerInfo=Result.OwnerInfo)
          and FilenameIsPascalSource(Result.ShortFilename) then exit;
          if Left then
            Node:=Node.Precessor
          else
            Node:=Node.Successor;
        end;
        Result:=nil;
      end;

    var
      StartNode: TAvgLvlTreeNode;
    begin
      StartNode:=ShortFiles.FindPointer(aPPUFile);
      Result:=FindSrc(StartNode,true);
      if Result<>nil then exit;
      Result:=FindSrc(StartNode,false);
    end;

  var
    CurNode: TAvgLvlTreeNode;
    aPPUFile: TPGInterPkgFile;
    Ext: String;
    FirstNodeSameUnitname: TAvgLvlTreeNode;
    OtherNode: TAvgLvlTreeNode;
    OtherFile: TPGInterPkgFile;
    SrcFile: TPGInterPkgFile;
    AmbiguousFile: TPGIPAmbiguousCompiledFile;
    OtherSrcFile: TPGInterPkgFile;
    IsBackward: Boolean;
  begin
    CurNode:=Units.FindLowest;
    FirstNodeSameUnitname:=nil;
    while CurNode<>nil do begin
      aPPUFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameUnitname=nil)
      or (ComparePGInterPkgUnitnames(aPPUFile,TPGInterPkgFile(FirstNodeSameUnitname.Data))<>0) then
        FirstNodeSameUnitname:=CurNode;
      CurNode:=CurNode.Successor;
      Ext:=lowercase(ExtractFileExt(aPPUFile.ShortFilename));
      if (Ext<>'.ppu') and (Ext<>'.o') then continue;
      // check units with same name
      SrcFile:=nil;

      OtherNode:=FirstNodeSameUnitname;
      IsBackward:=true;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if aPPUFile=OtherFile then
          IsBackward:=false;
        if (ComparePGInterPkgUnitnames(aPPUFile,OtherFile)<>0) then break;
        // other unit with same name found
        OtherNode:=OtherNode.Successor;
        if OtherFile.OwnerInfo=aPPUFile.OwnerInfo then continue;
        if (not FilenameIsPascalSource(OtherFile.ShortFilename))
        and IsBackward then
          continue; // report ppu/ppu conflicts only once

        if CompareFilenames(aPPUFile.FullFilename,OtherFile.FullFilename)=0 then
        begin
          // two packages share directories
          // it would would require a lenghty codetools check to find out
          // if this is right or wrong
          // => skip
          continue;
        end;

        // ppu in one package, unit with same name in another package
        if OptionUrAllowsDuplicate(aPPUFile,OtherFile) then continue;
        //debugln(['CheckPPUFilesInWrongDirs duplicate units found: file1="',aPPUFile.FullFilename,'"(',aPPUFile.OwnerInfo.Name,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        // check if this package has a source for this ppu
        if SrcFile=nil then
          SrcFile:=FindUnitSource(aPPUFile);
        if FilenameIsPascalSource(OtherFile.ShortFilename) then
          OtherSrcFile:=nil
        else
          OtherSrcFile:=FindUnitSource(OtherFile);
        AmbiguousFile:=TPGIPAmbiguousCompiledFile.Create;
        AmbiguousFile.Filename:=aPPUFile.FullFilename;
        AmbiguousFile.OwnerInfo:=aPPUFile.OwnerInfo;
        AmbiguousFile.ConflictFilename:=OtherFile.FullFilename;
        AmbiguousFile.ConflictOwner:=OtherFile.OwnerInfo;
        if SrcFile<>nil then
          AmbiguousFile.SrcFilename:=SrcFile.FullFilename;
        if OtherSrcFile<>nil then
          AmbiguousFile.ConflictSrcFilename:=OtherSrcFile.FullFilename;
        if (SrcFile<>nil) and (OtherSrcFile=nil) then
          AmbiguousFile.Switch;

        if ConsoleVerbosity>=0 then
        begin
          DbgOut('Warning: CheckPPUFilesInWrongDirs duplicate units found:');
          DbgOut(' file1="',AmbiguousFile.Filename,'"(',AmbiguousFile.OwnerInfo.Name);
          if AmbiguousFile.SrcFilename<>'' then
            DbgOut(',source=',AmbiguousFile.SrcFilename);
          DbgOut(') file2="',AmbiguousFile.ConflictFilename,'"(',AmbiguousFile.ConflictOwner.Name);
          if AmbiguousFile.ConflictSrcFilename<>'' then
            DbgOut(',source=',AmbiguousFile.ConflictSrcFilename);
          debugln(')');
        end;

        AmbiguousCompiledFiles.Add(AmbiguousFile);
      end;
    end;
  end;

  procedure CheckDuplicateSrcFiles;
  { Check if a src file in pkg A exists in another package B
    Unless A uses B and B has -Ur or A has -Ur and B uses A
    => IDE: ignore or cancel
    => lazbuild: warn }
  var
    CurNode: TAvgLvlTreeNode;
    CurFile: TPGInterPkgFile;
    FirstNodeSameShortName: TAvgLvlTreeNode;
    OtherNode: TAvgLvlTreeNode;
    OtherFile: TPGInterPkgFile;
    AmbiguousFile: TPGIPAmbiguousFile;
  begin
    CurNode:=ShortFiles.FindLowest;
    FirstNodeSameShortName:=nil;
    while CurNode<>nil do begin
      CurFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameShortName=nil)
      or (ComparePGInterPkgShortFilename(CurFile,TPGInterPkgFile(FirstNodeSameShortName.Data))<>0) then
        FirstNodeSameShortName:=CurNode;
      CurNode:=CurNode.Successor;
      if not FilenameIsPascalSource(CurFile.ShortFilename) then continue;
      // check files with same short name
      OtherNode:=FirstNodeSameShortName;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        OtherNode:=OtherNode.Successor;
        if (ComparePGInterPkgShortFilename(CurFile,OtherFile)<>0) then break;
        // other file with same short name found
        if OtherFile.OwnerInfo=CurFile.OwnerInfo then continue;

        if CompareFilenames(CurFile.FullFilename,OtherFile.FullFilename)=0 then
        begin
          // two packages share directories
          // it would would require a lenghty codetools check to find out
          // if this is right or wrong
          // => skip
          continue;
        end;

        // two files with same short name in different packages
        if OptionUrAllowsDuplicate(CurFile,OtherFile) then continue;
        if ConsoleVerbosity>=0 then
        begin
          debugln(['Warning: CheckDuplicateSrcFiles duplicate file found: file1="',CurFile.FullFilename,'"(',CurFile.OwnerInfo.Name,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        end;
        AmbiguousFile:=TPGIPAmbiguousFile.Create;
        AmbiguousFile.Filename:=CurFile.FullFilename;
        AmbiguousFile.OwnerInfo:=CurFile.OwnerInfo;
        AmbiguousFile.ConflictFilename:=OtherFile.FullFilename;
        AmbiguousFile.ConflictOwner:=OtherFile.OwnerInfo;
        AmbiguousSrcFiles.Add(AmbiguousFile);
      end;
    end;
  end;

var
  i: Integer;
  Dlg: TPGIPConflictsDialog;
begin
  Result:=true;
  FilesChanged:=false;
  if (PkgList=nil) or (PkgList.Count=0) then exit;
  OwnerInfos:=TObjectList.create(true);
  FullFiles:=TAvgLvlTree.Create(@ComparePGInterPkgFullFilenames);
  Units:=TAvgLvlTree.Create(@ComparePGInterPkgUnitnames);
  ShortFiles:=TAvgLvlTree.Create(@ComparePGInterPkgShortFilename);
  AmbiguousCompiledFiles:=TObjectList.create(true);
  AmbiguousSrcFiles:=TObjectList.create(true);
  Dlg:=nil;
  try
    // get target OS, CPU and LCLWidgetType
    TargetOS:='$(TargetOS)';
    GlobalMacroList.SubstituteStr(TargetOS);
    if TargetOS='' then TargetOS:=GetCompiledTargetOS;
    TargetCPU:='$(TargetCPU)';
    GlobalMacroList.SubstituteStr(TargetCPU);
    if TargetCPU='' then TargetCPU:=GetCompiledTargetCPU;
    LCLWidgetType:='$(LCLWidgetType)';
    GlobalMacroList.SubstituteStr(LCLWidgetType);
    if LCLWidgetType='' then LCLWidgetType:=LCLPlatformDirNames[GetDefaultLCLWidgetType];

    {$IFDEF VerboseCheckInterPkgFiles}
    debugln(['CheckInterPkgFiles TargetOS=',TargetOS,' TargetCPU=',TargetCPU,' LCLWidgetType=',LCLWidgetType]);
    {$ENDIF}

    // get search paths
    AddOwnerInfo(IDEObject);
    for i:=0 to PkgList.Count-1 do
      AddOwnerInfo(TObject(PkgList[i]));

    // collect FullFiles
    for i:=0 to OwnerInfos.Count-1 do
      CollectFilesOfOwner(TPGInterPkgOwnerInfo(OwnerInfos[i]));
    RemoveSecondaryFiles;

    // checks
    CheckPPUFilesInWrongDirs;
    CheckDuplicateSrcFiles;
    if (AmbiguousSrcFiles.Count=0)
    and (AmbiguousCompiledFiles.Count=0) then exit;

    // show warnings
    if LazarusIDE<>nil then begin
      // IDE
      Dlg:=TPGIPConflictsDialog.Create(nil);
      Dlg.Init(AmbiguousSrcFiles,AmbiguousCompiledFiles);
      if Dlg.ShowModal<>mrOK then exit(false);
      FilesChanged:=Dlg.FilesChanged;
    end;
  finally
    AmbiguousCompiledFiles.Free;
    AmbiguousSrcFiles.Free;
    Units.Free;
    ShortFiles.Free;
    FullFiles.FreeAndClear;
    FullFiles.Free;
    OwnerInfos.Free;
  end;
end;

end.

