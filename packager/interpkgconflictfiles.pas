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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Called after checking what packages need compile.
    Check source files and compiled files for name conflicts between packages.

  ToDo:
    - project compiler option verbosity: dialog on duplicate files
    - save date ignore
    - use date ignore
    - clear ignore on
      - clean build
      - error cant find include file
      unit_f_cant_find_ppu=10022_F_Can't find unit $1 used by $2
      unit_u_ppu_invalid_header=10007_U_PPU Invalid Header (no PPU at the begin)
      unit_f_cant_compile_unit=10021_F_Can't compile unit $1, no sources available
      unit_f_cant_find_ppu=10022_F_Can't find unit $1 used by $2
      unit_w_unit_name_error=10023_W_Unit $1 was not found but $2 exists
      unit_f_unit_name_error=10024_F_Unit $1 searched but $2 found
      unit_u_recompile_crc_change=10028_U_Recompiling $1, checksum changed for $2
      unit_u_recompile_source_found_alone=10029_U_Recompiling $1, source found only
      unit_u_recompile_staticlib_is_older=10030_U_Recompiling unit, static lib is older than ppufile
      unit_u_recompile_sharedlib_is_older=10031_U_Recompiling unit, shared lib is older than ppufile
      unit_u_recompile_obj_and_asm_older=10032_U_Recompiling unit, obj and asm are older than ppufile
      unit_u_recompile_obj_older_than_asm=10033_U_Recompiling unit, obj is older than asm
      unit_w_cant_compile_unit_with_changed_incfile=10040_W_Can't recompile unit $1, but found modifed include files
      unit_u_source_modified=10041_U_File $1 is newer than the one used for creating PPU file $2
}
unit InterPkgConflictFiles;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, types, math, contnrs, Laz_AVL_Tree,
  // LCL
  Forms, ComCtrls, Controls, ButtonPanel, Themes, Graphics, StdCtrls, Buttons,
  InterfaceBase,
  // CodeTools
  BasicCodeTools, DefineTemplates, CodeToolManager, FileProcs,
  // LazUtils
  LazFileUtils, LazFileCache, LazTracer,
  // IDEIntf
  ProjectIntf, CompOptsIntf, IDEWindowIntf, LazIDEIntf, IDEMsgIntf, IDEExternToolIntf,
  // IDE
  CompilerOptions, EnvironmentOpts, IDEProcs, DialogProcs, LazarusIDEStrConsts,
  TransferMacros, PackageDefs, PackageSystem;

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
  TPGInterPkgFileArray = array of TPGInterPkgFile;

  { TPGIPAmbiguousFileGroup }

  TPGIPAmbiguousFileGroup = class
  public
    CompiledFiles: TPGInterPkgFileArray;
    Sources: TPGInterPkgFileArray;
    function Add(SrcFile, PPUFile: TPGInterPkgFile): integer;
    function IndexOfOwner(OwnerInfo: TPGInterPkgOwnerInfo): integer;
    procedure Switch(Index1, Index2: integer);
  end;

  TPGIPCategory = (
    pgipOrphanedCompiled,
    pgipDuplicateSource
    );

  { TPGIPConflictsDialog }

  TPGIPConflictsDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ConflictsTreeView: TTreeView;
    IDEDialogLayoutStorage1: TIDEDialogLayoutStorage;
    ImageList1: TImageList;
    procedure ConflictsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var {%H-}PaintImages, {%H-}DefaultDraw: Boolean);
    procedure ConflictsTreeViewMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure DeleteSelectedFilesButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    DeleteSelectedFilesButton: TButton;
    FImgIndexChecked: integer;
    FImgIndexUnchecked: integer;
    FCategoryNodes: array[TPGIPCategory] of TTreeNode;
    procedure UpdateButtons;
    procedure IgnoreConflicts;
  public
    FileGroups: TObjectList; // list of TPGIPAmbiguousFileGroup
    FilesChanged: boolean;
    procedure Init(Groups: TObjectList);
  end;

function CheckInterPkgFiles(IDEObject: TObject;
  PkgList: TFPList; out FilesChanged: boolean
  ): boolean; // returns false if user cancelled
function FilenameIsCompiledSource(aFilename: string): boolean;

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
  Assert(Assigned(F1), 'ComparePGInterPkgUnitnames: File1=Nil.');
  Assert(Assigned(F2), 'ComparePGInterPkgUnitnames: File2=Nil.');
  Result:=CompareDottedIdentifiers(PChar(F1.AnUnitName),PChar(F2.AnUnitName));
end;

function ComparePGInterPkgShortFilename(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  // compare case insensitive to find cross platform duplicates
  // Note: do not use CompareFilenamesIgnoreCase, because of Turkish ı, I
  Result:=CompareText(F1.ShortFilename,F2.ShortFilename);
end;

{ TPGIPAmbiguousFileGroup }

function TPGIPAmbiguousFileGroup.Add(SrcFile, PPUFile: TPGInterPkgFile): integer;
begin
  if (SrcFile=nil) and (PPUFile=nil) then
    RaiseGDBException('');
  if (SrcFile<>nil) and (PPUFile<>nil) and (PPUFile.OwnerInfo<>SrcFile.OwnerInfo) then
    RaiseGDBException('bug: not equal: PPUFile.OwnerInfo='+PPUFile.OwnerInfo.Name+' SrcFile.OwnerInfo='+SrcFile.OwnerInfo.Name);
  if (SrcFile<>nil) and FilenameIsCompiledSource(SrcFile.ShortFilename) then
    RaiseGDBException('bug: src is compiled file: SrcFile.Filename='+SrcFile.FullFilename);
  if (PPUFile<>nil) and not FilenameIsCompiledSource(PPUFile.ShortFilename) then
    RaiseGDBException('bug: compiled file is source:'+PPUFile.FullFilename);
  Result:=length(CompiledFiles);
  SetLength(CompiledFiles,Result+1);
  SetLength(Sources,Result+1);
  Sources[Result]:=SrcFile;
  CompiledFiles[Result]:=PPUFile
end;

function TPGIPAmbiguousFileGroup.IndexOfOwner(OwnerInfo: TPGInterPkgOwnerInfo
  ): integer;
begin
  Result:=length(Sources)-1;
  while (Result>=0) do
  begin
    if (Sources[Result]<>nil) then
    begin
      if (Sources[Result].OwnerInfo=OwnerInfo) then exit;
    end else begin
      if (CompiledFiles[Result].OwnerInfo=OwnerInfo) then exit;
    end;
    dec(Result);
  end;
end;

procedure TPGIPAmbiguousFileGroup.Switch(Index1, Index2: integer);
var
  aFile: TPGInterPkgFile;
begin
  aFile:=Sources[Index1]; Sources[Index1]:=Sources[Index2]; Sources[Index2]:=aFile;
  aFile:=CompiledFiles[Index1]; CompiledFiles[Index1]:=CompiledFiles[Index2]; CompiledFiles[Index2]:=aFile;
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
  if TObject(Node.Data) is TPGIPAmbiguousFileGroup then begin
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
  if TObject(Node.Data) is TPGIPAmbiguousFileGroup then begin
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

  function DeleteFileAndAssociates(aFile: TPGInterPkgFile): boolean;
  var
    aFilename: String;
  begin
    if aFile=nil then exit(true);
    aFilename:=aFile.FullFilename;
    {$IFDEF VerboseCheckInterPkgFiles}
    debugln(['DeleteFileGroup ',aFilename]);
    {$ENDIF}
    if DeleteFileInteractive(aFilename)<>mrOk then exit(false);
    if FilenameIsPascalUnit(aFilename) then
    begin
      // unit source -> delete compiled files and resources
      DeleteFileUTF8(ChangeFileExt(aFilename,'.ppu'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.o'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.rst'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.rsj'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.lfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.dfm'));
      DeleteFileUTF8(ChangeFileExt(aFilename,'.xfm'));
    end else if FilenameIsCompiledSource(aFilename) then begin
      // compiled file -> delete compiled files. Keep sources.
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
  NextNode: TTreeNode;
  FileGroup: TPGIPAmbiguousFileGroup;
  IndexInGroup: integer;
  ConflictCount: Integer;
begin
  ConflictsTreeView.Items.BeginUpdate;
  try
    Node:=ConflictsTreeView.Items.GetFirstNode;
    IndexInGroup:=-1;
    ConflictCount:=0;
    while Node<>nil do
    begin
      NextNode:=Node.GetNext;
      if TObject(Node.Data) is TPGIPAmbiguousFileGroup then
      begin
        FileGroup:=TPGIPAmbiguousFileGroup(Node.Data);
        inc(IndexInGroup);
        if Node.ImageIndex=FImgIndexChecked then
        begin
          if not DeleteFileAndAssociates(FileGroup.Sources[IndexInGroup]) then exit;
          if not DeleteFileAndAssociates(FileGroup.CompiledFiles[IndexInGroup]) then exit;
        end;
        if ((FileGroup.Sources[IndexInGroup]<>nil)
        and FileExistsUTF8(FileGroup.Sources[IndexInGroup].FullFilename))
        or ((FileGroup.CompiledFiles[IndexInGroup]<>nil)
        and FileExistsUTF8(FileGroup.CompiledFiles[IndexInGroup].FullFilename))
        then
          inc(ConflictCount);
        if IndexInGroup=length(FileGroup.Sources)-1 then
        begin
          if ConflictCount<=1 then begin
            // conflict does not exist anymore
            FilesChanged:=true;
            Node:=Node.Parent;
            NextNode:=Node.GetNextSkipChildren;
            Node.Delete;
          end;
          IndexInGroup:=-1;
          ConflictCount:=0;
        end;
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
  IDEDialogLayoutList.ApplyLayout(Self);

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
    if TObject(Node.Data) is TPGIPAmbiguousFileGroup then
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
begin
  // ToDo
  ModalResult:=mrOk;
end;

procedure TPGIPConflictsDialog.Init(Groups: TObjectList);

  function AddChild(ParentNode: TTreeNode; Caption: string): TTreeNode;
  begin
    Result:=ConflictsTreeView.Items.AddChild(ParentNode,Caption);
  end;

var
  i, j: Integer;
  ItemNode: TTreeNode;
  s: String;
  FileGroupNode: TTreeNode;
  FileGroup: TPGIPAmbiguousFileGroup;
  SrcFile: TPGInterPkgFile;
  CompiledFile: TPGInterPkgFile;
  CurFile: TPGInterPkgFile;
  c: TPGIPCategory;
begin
  FileGroups:=Groups;

  ConflictsTreeView.Items.BeginUpdate;
  ConflictsTreeView.Items.Clear;
  ConflictsTreeView.Images:=ImageList1;
  for c in TPGIPCategory do
    FCategoryNodes[c]:=nil;
  for i:=0 to FileGroups.Count-1 do
  begin
    FileGroup:=TPGIPAmbiguousFileGroup(FileGroups[i]);

    // category
    if FileGroup.Sources[0]=nil then
    begin
      // orphaned compiled file
      CurFile:=FileGroup.CompiledFiles[0];
      c:=pgipOrphanedCompiled;
      if FCategoryNodes[c]=nil then
        FCategoryNodes[c]:=
          ConflictsTreeView.Items.Add(nil,'Orphaned compiled files');
    end else begin
      // duplicate source file
      CurFile:=FileGroup.Sources[0];
      c:=pgipDuplicateSource;
      if FCategoryNodes[c]=nil then
        FCategoryNodes[c]:=
          ConflictsTreeView.Items.Add(nil,'Duplicate source files');
    end;

    // file group
    s:=ExtractFilename(CurFile.ShortFilename);
    FileGroupNode:=AddChild(FCategoryNodes[c],s);

    for j:=0 to length(FileGroup.Sources)-1 do
    begin
      SrcFile:=FileGroup.Sources[j];
      CompiledFile:=FileGroup.CompiledFiles[j];

      if SrcFile<>nil then
        CurFile:=SrcFile
      else
        CurFile:=CompiledFile;

      s:=ExtractFilename(CurFile.ShortFilename);
      if CurFile.OwnerInfo.Owner is TLazPackage then
        s+=' of package '+CurFile.OwnerInfo.Name
      else
        s+=' of '+CurFile.OwnerInfo.Name;
      ItemNode:=AddChild(FileGroupNode,s);
      if SrcFile=nil then
        ItemNode.ImageIndex:=FImgIndexChecked // default: delete
      else
        ItemNode.ImageIndex:=FImgIndexUnchecked; // default: keep
      ItemNode.SelectedIndex:=ItemNode.ImageIndex;
      ItemNode.Data:=FileGroup;
      begin
        // file paths of compiled and src
        if CompiledFile<>nil then
          AddChild(ItemNode,'Compiled: '+CompiledFile.FullFilename);
        if SrcFile<>nil then
          AddChild(ItemNode,'Source: '+SrcFile.FullFilename)
        else
          AddChild(ItemNode,'No source found');
      end;
    end;
  end;
  // expand all nodes
  for c in TPGIPCategory do
    if FCategoryNodes[c]<>nil then
      FCategoryNodes[c].Expand(true);

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
  FullFiles: TAvlTree; // tree of TPGInterPkgFile sorted for FullFilename
  Units: TAvlTree; // tree of TPGInterPkgFile sorted for AnUnitName
  ShortFiles: TAvlTree; // tree of TPGInterPkgFile sorted for ShortFilename
  AmbiguousFileGroups: TObjectList; // list of TPGIPAmbiguousFileGroup

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
    var SearchedDirs: string; {%H-}IsIncDir: boolean);
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
        '.ppu','.o','.rst','.rsj','.pas','.pp','.p':
          begin
            AnUnitName:=ExtractFileNameOnly(aFilename);
            if not IsDottedIdentifier(AnUnitName) then continue;
          end;
        '.inc', '.lfm', '.dfm': ;
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
    Node: TAvlTreeNode;
    ONode: TAvlTreeNode;
    OFile: TPGInterPkgFile;
    PPUFileName: String;
    SearchFile: TPGInterPkgFile;
    PPUNode: TAvlTreeNode;
  begin
    Node:=Units.FindLowest;
    while Node<>nil do begin
      // for each .o file
      ONode:=Node;
      Node:=Node.Successor;
      OFile:=TPGInterPkgFile(ONode.Data);
      if not FilenameIsCompiledSource(OFile.ShortFilename) then continue;
      if CompareFileExt(OFile.ShortFilename,'.ppu',false)=0 then continue;
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

  function CheckIfFilesCanConflict(FileGroup: TPGIPAmbiguousFileGroup;
    File1, File2: TPGInterPkgFile): boolean;
  var
    FileDir1: String;
    FileDir2: String;
  begin
    Result:=false;
    // report only one unit per package
    if File1.OwnerInfo=File2.OwnerInfo then exit;
    if (FileGroup<>nil) and (FileGroup.IndexOfOwner(File1.OwnerInfo)>=0)
    then exit;
    // check -Ur
    if OptionUrAllowsDuplicate(File2,File1) then
      exit;
    // check shared directories
    if CompareFilenames(File2.FullFilename,File1.FullFilename)=0 then
    begin
      // Two packages share directories
      // It would would require a lenghty codetools check to find out if
      // this is right or wrong
      // => skip
      exit;
    end;
    FileDir1:=ExtractFilePath(File1.FullFilename);
    FileDir2:=ExtractFilePath(File2.FullFilename);
    if (FindPathInSearchPath(FileDir1,File2.OwnerInfo.SrcDirs)>0)
    or (FindPathInSearchPath(FileDir2,File1.OwnerInfo.SrcDirs)>0) then
    begin
      // File1 in SrcDirs of file owner 2
      // or File2 in SrcDirs of file owner 1
      // => a warning about sharing source directories is enough
      //    don't warn every shared file
      // => skip
      exit;
    end;

    Result:=true;
  end;

  procedure FindUnitSourcePPU(var TheUnit: TPGInterPkgFile; out UnitPPU: TPGInterPkgFile);
  // find in same package the source of a ppu, or the ppu of a source
  var
    SearchPPU: Boolean;
    AnUnitName: string;

    function FindOther(Node: TAvlTreeNode; Left: boolean): TPGInterPkgFile;
    var
      IsPPU: Boolean;
    begin
      while Node<>nil do begin
        Result:=TPGInterPkgFile(Node.Data);
        if CompareFilenames(ExtractFileNameOnly(Result.ShortFilename),
          AnUnitName)<>0 then break;
        if (TheUnit.OwnerInfo=Result.OwnerInfo) then
        begin
          IsPPU:=FilenameIsCompiledSource(Result.ShortFilename);
          if SearchPPU=IsPPU then exit;
        end;
        if Left then
          Node:=Node.Precessor
        else
          Node:=Node.Successor;
      end;
      Result:=nil;
    end;

  var
    StartNode: TAvlTreeNode;
    h: TPGInterPkgFile;
  begin
    UnitPPU:=nil;
    AnUnitName:=ExtractFileNameOnly(TheUnit.ShortFilename);
    SearchPPU:=FilenameIsPascalUnit(TheUnit.ShortFilename); // search opposite
    StartNode:=ShortFiles.FindPointer(TheUnit);
    UnitPPU:=FindOther(StartNode,true);
    if UnitPPU=nil then
      UnitPPU:=FindOther(StartNode,false);
    if not SearchPPU then begin
      h:=TheUnit;
      TheUnit:=UnitPPU;
      UnitPPU:=h;
    end;
  end;

  procedure CheckDuplicateUnits;
  { Check two or more packages have the same unit (ppu/o/pas/pp/p)
    Unless A uses B and B has -Ur or A has -Ur and B uses A }
  var
    CurNode: TAvlTreeNode;
    CurUnit: TPGInterPkgFile;
    FirstNodeSameUnitname: TAvlTreeNode;
    OtherNode: TAvlTreeNode;
    OtherFile: TPGInterPkgFile;
    PPUFile: TPGInterPkgFile;
    FileGroup: TPGIPAmbiguousFileGroup;
    OtherPPUFile: TPGInterPkgFile;
    i: Integer;
    Msg: String;
    SrcFile: TPGInterPkgFile;
  begin
    CurNode:=Units.FindLowest;
    FirstNodeSameUnitname:=nil;
    while CurNode<>nil do begin
      CurUnit:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameUnitname=nil)
      or (ComparePGInterPkgUnitnames(CurUnit,TPGInterPkgFile(FirstNodeSameUnitname.Data))<>0) then
        FirstNodeSameUnitname:=CurNode;
      CurNode:=CurNode.Successor;
      if CurUnit.OwnerInfo.HasOptionUr then continue;

      // CurUnit is an unit without -Ur
      // => check units with same name
      FileGroup:=nil;
      PPUFile:=nil;
      SrcFile:=nil;
      OtherNode:=FirstNodeSameUnitname;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if (ComparePGInterPkgUnitnames(CurUnit,OtherFile)<>0) then break;
        // other unit with same name found
        OtherNode:=OtherNode.Successor;

        if not CheckIfFilesCanConflict(FileGroup,CurUnit,OtherFile) then
          continue;
        //debugln(['CheckPPUFilesInWrongDirs duplicate units found: file1="',CurUnit.FullFilename,'"(',CurUnit.OwnerInfo.Name,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        FindUnitSourcePPU(OtherFile,OtherPPUFile);
        if FileGroup=nil then begin
          SrcFile:=CurUnit;
          FindUnitSourcePPU(SrcFile,PPUFile);
        end;
        if (SrcFile<>nil) and (OtherFile<>nil)
        and (CompareFilenames(SrcFile.FullFilename,OtherFile.FullFilename)=0) then
        begin
          // two packages share source directories
          // -> do not warn single files
          continue;
        end;

        if (PPUFile<>nil) and (OtherPPUFile<>nil)
        and (CompareFilenames(PPUFile.FullFilename,OtherPPUFile.FullFilename)=0)
        and (OtherFile=nil) then begin
          // the same ppu is in both packages
          // ... and only one package has a source
          // for example: two packages share output directories
          // => ok
          continue;
        end;

        if FileGroup=nil then begin
          FileGroup:=TPGIPAmbiguousFileGroup.Create;
          FileGroup.Add(SrcFile,PPUFile);
          AmbiguousFileGroups.Add(FileGroup);
        end;
        FileGroup.Add(OtherFile,OtherPPUFile);
        if (PPUFile<>nil) and (OtherPPUFile=nil) then
        begin
          // put the orphaned ppu at top
          FileGroup.Switch(0,length(FileGroup.Sources)-1);
        end;
      end;

      // create Warnings
      if FileGroup<>nil then begin
        for i:=0 to length(FileGroup.Sources)-1 do
        begin
          SrcFile:=FileGroup.Sources[i];
          PPUFile:=FileGroup.CompiledFiles[i];
          if SrcFile<>nil then
          begin
            Msg:=Format(lisDuplicateUnitIn, [SrcFile.AnUnitName, SrcFile.
              OwnerInfo.Name]);
            if PPUFile<>nil then
              Msg+=', ppu="'+PPUFile.FullFilename+'"';
            Msg+=', source="'+SrcFile.FullFilename+'"';
          end else begin
            Msg:=Format(lisDuplicateUnitIn, [PPUFile.AnUnitName, PPUFile.
              OwnerInfo.Name]);
            Msg+=', orphaned ppu "'+PPUFile.FullFilename+'"';
          end;
          if IDEMessagesWindow<>nil then
            IDEMessagesWindow.AddCustomMessage(mluNote,Msg)
          else
            debugln('Warning: (lazarus) ',Msg);
        end;
      end;

      // all duplicates of this unitname were found -> skip to next unitname
      CurNode:=OtherNode;
    end;
  end;

  procedure CheckDuplicateSrcFiles;
  { Check if a src file in pkg A exists in another package B
    Unless A uses B and B has -Ur or A has -Ur and B uses A
    => IDE: ignore or cancel
    => lazbuild: warn }
  var
    CurNode: TAvlTreeNode;
    CurFile: TPGInterPkgFile;
    FirstNodeSameShortName: TAvlTreeNode;
    OtherNode: TAvlTreeNode;
    OtherFile: TPGInterPkgFile;
    FileGroup: TPGIPAmbiguousFileGroup;
    i: Integer;
    Msg: String;
  begin
    CurNode:=ShortFiles.FindLowest;
    FirstNodeSameShortName:=nil;
    while CurNode<>nil do begin
      CurFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameShortName=nil)
      or (ComparePGInterPkgShortFilename(CurFile,TPGInterPkgFile(FirstNodeSameShortName.Data))<>0) then
        FirstNodeSameShortName:=CurNode;
      CurNode:=CurNode.Successor;
      if CurFile.AnUnitName<>'' then
        continue; // units were already checked in CheckDuplicateUnits

      // check files with same short name
      FileGroup:=nil;
      OtherNode:=FirstNodeSameShortName;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if (ComparePGInterPkgShortFilename(CurFile,OtherFile)<>0) then break;
        OtherNode:=OtherNode.Successor;

        if OtherFile.AnUnitName<>'' then
          continue; // units were already checked in CheckDuplicateUnits

        // other file with same short name found
        if not CheckIfFilesCanConflict(FileGroup,CurFile,OtherFile) then
          continue;

        if FileGroup=nil then begin
          FileGroup:=TPGIPAmbiguousFileGroup.Create;
          FileGroup.Add(CurFile,nil);
          AmbiguousFileGroups.Add(FileGroup);
        end;
        FileGroup.Add(OtherFile,nil);
      end;

      // create Warnings
      if FileGroup<>nil then begin
        for i:=0 to length(FileGroup.Sources)-1 do
        begin
          CurFile:=FileGroup.Sources[i];
          Msg:='Duplicate file "'+ExtractFileName(CurFile.ShortFilename)+'"';
          Msg+=' in "'+CurFile.OwnerInfo.Name+'"';
          Msg+=', path="'+CurFile.FullFilename+'"';
          if IDEMessagesWindow<>nil then
            IDEMessagesWindow.AddCustomMessage(mluWarning,Msg)
          else
            debugln('Warning: (lazarus) ',Msg);
        end;
      end;

      // all duplicates of this file were found -> skip to next group
      CurNode:=OtherNode;
    end;
  end;

var
  i: Integer;
  {$IFDEF EnableCheckInterPkgFiles}
  Dlg: TPGIPConflictsDialog;
  {$ENDIF}
begin
  Result:=true;
  FilesChanged:=false;
  if (PkgList=nil) or (PkgList.Count=0) then exit;
  OwnerInfos:=TObjectList.create(true);
  FullFiles:=TAvlTree.Create(@ComparePGInterPkgFullFilenames);
  Units:=TAvlTree.Create(@ComparePGInterPkgUnitnames);
  ShortFiles:=TAvlTree.Create(@ComparePGInterPkgShortFilename);
  AmbiguousFileGroups:=TObjectList.create(true);
  {$IFDEF EnableCheckInterPkgFiles}
  Dlg:=nil;
  {$ENDIF}
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
    if LCLWidgetType='' then
      LCLWidgetType:=GetLCLWidgetTypeName;

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
    CheckDuplicateUnits;
    CheckDuplicateSrcFiles;
    if (AmbiguousFileGroups.Count=0) then exit;

    // show warnings
    if LazarusIDE<>nil then begin
      {$IFDEF EnableCheckInterPkgFiles}
      // IDE
      Dlg:=TPGIPConflictsDialog.Create(nil);
      Dlg.Init(AmbiguousFileGroups);
      if Dlg.ShowModal<>mrOK then exit(false);
      FilesChanged:=Dlg.FilesChanged;
      {$ENDIF}
    end;
  finally
    {$IFDEF EnableCheckInterPkgFiles}
    Dlg.Free;
    {$ENDIF}
    AmbiguousFileGroups.Free;
    Units.Free;
    ShortFiles.Free;
    FullFiles.FreeAndClear;
    FullFiles.Free;
    OwnerInfos.Free;
  end;
end;

function FilenameIsCompiledSource(aFilename: string): boolean;
var
  Ext: String;
begin
  Ext:=lowercase(ExtractFileExt(aFilename));
  Result:=(Ext='.ppu') or (Ext='.o') or (Ext='.rst') or (Ext='.rsj');
end;

end.

