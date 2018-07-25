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

  Abstract:
    Dialog to clean up a project and its packages and to compile the project.
}
unit BuildProjectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Laz_AVL_Tree,
  // LCL
  Forms, Controls, Dialogs, ButtonPanel, StdCtrls, ComCtrls, Masks, LCLIntf,
  // LazUtils
  LazFileUtils, LazFileCache, LazStringUtils, AvgLvlTree,
  // codetools
  FileProcs, CodeToolManager, DirectoryCacher,
  // IDEIntf
  IDEDialogs, IDEImagesIntf, PackageIntf,
  // IDE
  PackageDefs, PackageSystem, InputHistory, LazarusIDEStrConsts, Project,
  EnvironmentOpts, IDEProcs;

type
  TBuildProjectDialogItem = class
  public
    IsDirectory: boolean;
    Filename: string;
  end;

  { TCleanBuildProjectDialog }

  TCleanBuildProjectDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    DeleteButton: TButton;
    PkgOutCheckBox: TCheckBox;
    PkgOutMaskComboBox: TComboBox;
    PkgSrcCheckBox: TCheckBox;
    PkgSrcMaskComboBox: TComboBox;
    PreviewLabel: TLabel;
    FilesTreeView: TTreeView;
    ProjOutCheckBox: TCheckBox;
    ProjOutMaskComboBox: TComboBox;
    ProjSrcCheckBox: TCheckBox;
    ProjSrcMaskComboBox: TComboBox;
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FilesTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure PkgOutCheckBoxChange(Sender: TObject);
    procedure PkgSrcCheckBoxChange(Sender: TObject);
    procedure ProjOutCheckBoxChange(Sender: TObject);
    procedure ProjOutMaskComboBoxChange(Sender: TObject);
    procedure ProjSrcCheckBoxChange(Sender: TObject);
  private
    ImageIndexDirectory: Integer;
    ImageIndexFile: Integer;
    FIdleConnected: boolean;
    procedure SetIdleConnected(const AValue: boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  private
    FProject: TProject;
    FUpdateNeeded: boolean;
    procedure ClearFilesTreeView;
    procedure UpdateFilesTreeView(Immediately: boolean = false);
    procedure AddProjOutDirectory;
    procedure AddProjSrcDirectories;
    procedure AddPkgOutDirectories;
    procedure AddPkgSrcDirectory;
    procedure AddDirectory(aTVPath, aDirectory, aFileMask: string);
    procedure AddDirectories(aTVPath, aSearchPath, aFileMask: string);
    function GetAllFilesFromTree: TFilenameToStringTree;
    function DeleteFiles: TModalResult;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  public
    procedure Init(AProject: TProject);
  end;

function ShowBuildProjectDialog(AProject: TProject): TModalResult;

implementation

function ShowBuildProjectDialog(AProject: TProject): TModalResult;
var
  CleanBuildProjectDialog: TCleanBuildProjectDialog;
begin
  CleanBuildProjectDialog:=TCleanBuildProjectDialog.Create(nil);
  try
    CleanBuildProjectDialog.Init(AProject);
    Result:=CleanBuildProjectDialog.ShowModal;
  finally
    CleanBuildProjectDialog.Free;
  end;
end;

{$R *.lfm}

{ TCleanBuildProjectDialog }

procedure TCleanBuildProjectDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisCleanUpAndBuildProject;

  ProjOutCheckBox.Caption:=lisProjectOutputDirectory;
  ProjSrcCheckBox.Caption:=lisProjectSourceDirectories;
  PkgOutCheckBox.Caption:=lisPackageOutputDirectories;
  PkgSrcCheckBox.Caption:=lisPackageSourceDirectories;
  PreviewLabel.Caption:=lisTheseFilesWillBeDeleted;

  ButtonPanel1.OKButton.Caption:=lisCleanUpAndBuild;
  ButtonPanel1.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel1.CancelButton.Caption:=lisCancel;

  DeleteButton.Caption:=lisDelete;

  FilesTreeView.Images:=IDEImages.Images_16;
  ImageIndexDirectory := IDEImages.LoadImage('pkg_files');
  ImageIndexFile := IDEImages.LoadImage('laz_delete');

  ButtonPanel1.OKButton.ModalResult:=mrNone;
end;

procedure TCleanBuildProjectDialog.FormDestroy(Sender: TObject);
begin
  ClearFilesTreeView;
  FProject:=nil;
  IdleConnected:=false;
end;

procedure TCleanBuildProjectDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);

  procedure StoreCombo(AComboBox: TComboBox);
  begin
    // store all masks into one history list
    ProjOutMaskComboBox.AddHistoryItem(AComboBox.Text,30,true,false);
  end;

begin
  EnvironmentOptions.CleanBuildProjOut:=ProjOutCheckBox.Checked;
  EnvironmentOptions.CleanBuildProjSrc:=ProjSrcCheckBox.Checked;
  EnvironmentOptions.CleanBuildPkgOut :=PkgOutCheckBox.Checked;
  EnvironmentOptions.CleanBuildPkgSrc :=PkgSrcCheckBox.Checked;

  FProject.CleanOutputFileMask:=ProjOutMaskComboBox.Text;
  FProject.CleanSourcesFileMask:=ProjSrcMaskComboBox.Text;
  InputHistories.CleanOutputFileMask:=PkgOutMaskComboBox.Text;
  InputHistories.CleanSourcesFileMask:=PkgSrcMaskComboBox.Text;

  // combine history lists
  StoreCombo(ProjOutMaskComboBox);
  StoreCombo(ProjSrcMaskComboBox);
  StoreCombo(PkgOutMaskComboBox);
  StoreCombo(PkgSrcMaskComboBox);
  InputHistories.HistoryLists.GetList(hlCleanBuildFileMask,true,
    rltFile).Assign(ProjOutMaskComboBox.Items);
end;

procedure TCleanBuildProjectDialog.ButtonPanel1OKButtonClick(Sender: TObject);
begin
  if DeleteFiles<>mrOk then exit;
  ModalResult:=mrOk;
end;

procedure TCleanBuildProjectDialog.DeleteButtonClick(Sender: TObject);
begin
  DeleteFiles;
end;

procedure TCleanBuildProjectDialog.FilesTreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  Item: TBuildProjectDialogItem;
  Filename: String;
begin
  Node:=FilesTreeView.GetNodeAt(X,Y);
  if Node=nil then exit;
  if (Button=mbLeft) and (ssDouble in Shift) and (Node.Data<>nil) then begin
    Item:=TBuildProjectDialogItem(Node.Data);
    Filename:=Item.Filename;
    if Item.IsDirectory then exit;
    Filename:=ExtractFilePath(Filename);
    Filename:=ChompPathDelim(Filename);
    debugln(['TBuildProjectDialog.FilesTreeViewMouseDown Filename="',Filename,'"']);
    if FilenameIsAbsolute(Filename) then
      OpenURL('file://'+StringReplace(Filename,'\','/',[rfReplaceAll]));
  end;
end;

procedure TCleanBuildProjectDialog.FormResize(Sender: TObject);
var
  r: Integer;
begin
  r:=ProjOutCheckBox.Left
    +Max(Max(ProjOutCheckBox.Width,ProjSrcCheckBox.Width),
         Max(PkgOutCheckBox.Width,PkgSrcCheckBox.Width));
  ProjOutMaskComboBox.Left:=r+10;
end;

procedure TCleanBuildProjectDialog.HelpButtonClick(Sender: TObject);
begin
  OpenUrl('http://wiki.freepascal.org/IDE_Window:_Clean_up_build_files_dialog');
end;

procedure TCleanBuildProjectDialog.PkgOutCheckBoxChange(Sender: TObject);
begin
  PkgOutMaskComboBox.Enabled:=PkgOutCheckBox.Checked;
  UpdateFilesTreeView;
end;

procedure TCleanBuildProjectDialog.PkgSrcCheckBoxChange(Sender: TObject);
begin
  PkgSrcMaskComboBox.Enabled:=PkgSrcCheckBox.Checked;
  UpdateFilesTreeView;
end;

procedure TCleanBuildProjectDialog.ProjOutCheckBoxChange(Sender: TObject);
begin
  ProjOutMaskComboBox.Enabled:=ProjOutCheckBox.Checked;
  UpdateFilesTreeView;
end;

procedure TCleanBuildProjectDialog.ProjOutMaskComboBoxChange(Sender: TObject);
begin
  UpdateFilesTreeView;
end;

procedure TCleanBuildProjectDialog.ProjSrcCheckBoxChange(Sender: TObject);
begin
  ProjSrcMaskComboBox.Enabled:=ProjSrcCheckBox.Checked;
  UpdateFilesTreeView;
end;

procedure TCleanBuildProjectDialog.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCleanBuildProjectDialog.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if FProject=nil then exit;
  if not FUpdateNeeded then exit;
  IdleConnected:=false;
  UpdateFilesTreeView(true);
end;

procedure TCleanBuildProjectDialog.ClearFilesTreeView;
var
  Node: TTreeNode;
begin
  Node:=FilesTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if (Node.Data<>nil) then
      TObject(Node.Data).Free;
    Node:=Node.GetNext;
  end;
  FilesTreeView.Items.Clear;
end;

procedure TCleanBuildProjectDialog.UpdateFilesTreeView(Immediately: boolean);

  function CreateTVChildCounts(TVNode: TTreeNode): integer;
  var
    ChildNode: TTreeNode;
  begin
    Result:=0;
    if TVNode=nil then exit;
    ChildNode:=TVNode.GetFirstChild;
    while ChildNode<>nil do begin
      inc(Result,CreateTVChildCounts(ChildNode));
      ChildNode:=ChildNode.GetNextSibling;
    end;
    if (Result=0) and (TVNode.Count>0) then
      // has children, but no grand children => is a directory
      inc(Result,TVNode.Count);
    if Result>0 then
      TVNode.Text:=Format(lisCBPFiles, [TVNode.Text, IntToStr(Result)]);
  end;

var
  i: Integer;
  TVNode: TTreeNode;
begin
  if not Immediately then begin
    FUpdateNeeded:=true;
    IdleConnected:=true;
    exit;
  end;
  FUpdateNeeded:=false;

  FilesTreeView.BeginUpdate;
  ClearFilesTreeView;
  if FProject<>nil then begin
    if ProjOutCheckBox.Checked then AddProjOutDirectory;
    if ProjSrcCheckBox.Checked then AddProjSrcDirectories;
    if PkgOutCheckBox.Checked then AddPkgOutDirectories;
    if PkgSrcCheckBox.Checked then AddPkgSrcDirectory;
  end;
  for i:=0 to FilesTreeView.Items.TopLvlCount-1 do begin
    TVNode:=FilesTreeView.Items.TopLvlItems[i];
    CreateTVChildCounts(TVNode);
    TVNode.Expand(true);
  end;
  FilesTreeView.EndUpdate;
end;

procedure TCleanBuildProjectDialog.AddProjOutDirectory;
begin
  AddDirectory(lisProjectOutputDirectory,
    FProject.CompilerOptions.GetUnitOutputDirectory(false),
    ProjOutMaskComboBox.Text);
end;

procedure TCleanBuildProjectDialog.AddProjSrcDirectories;
begin
  AddDirectories(lisProjectOutputDirectory,
    FProject.SourceDirectories.CreateSearchPathFromAllFiles,
    ProjSrcMaskComboBox.Text);
end;

procedure TCleanBuildProjectDialog.AddPkgOutDirectories;
var
  List: TFPList;
  i: Integer;
  Pkg: TLazPackage;
begin
  List:=nil;
  try
    PackageGraph.GetAllRequiredPackages(nil,FProject.FirstRequiredDependency,
      List,[pirSkipDesignTimeOnly]);
    if List=nil then exit;
    for i:=0 to List.Count-1 do begin
      Pkg:=TLazPackage(List[i]);
      if Pkg.AutoUpdate=pupManually then continue;
      AddDirectory(Pkg.Name,Pkg.CompilerOptions.GetUnitOutputDirectory(false),
        PkgOutMaskComboBox.Text);
    end;
  finally
    List.Free;
  end;
end;

procedure TCleanBuildProjectDialog.AddPkgSrcDirectory;
var
  List: TFPList;
  i: Integer;
  Pkg: TLazPackage;
begin
  List:=nil;
  try
    PackageGraph.GetAllRequiredPackages(nil,FProject.FirstRequiredDependency,
      List);
    if List=nil then exit;
    for i:=0 to List.Count-1 do begin
      Pkg:=TLazPackage(List[i]);
      AddDirectories(Pkg.Name,Pkg.SourceDirectories.CreateSearchPathFromAllFiles,
        PkgSrcMaskComboBox.Text);
    end;
  finally
    List.Free;
  end;
end;

procedure TCleanBuildProjectDialog.AddDirectory(aTVPath, aDirectory,
  aFileMask: string);
var
  Cache: TCTDirectoryCache;
  Files: TStrings;
  TVFiles: TStringList;
  MaskList: TMaskList;
  p: SizeInt;
  NodeText: String;
  TVNode: TTreeNode;
  ParentTVNode: TTreeNode;
  i: Integer;
  Item: TBuildProjectDialogItem;
begin
  //debugln(['TBuildProjectDialog.AddDirectory aTVPath="',aTVPath,'" aDirectory="',aDirectory,'" aFileMask="',aFileMask,'"']);
  aDirectory:=ChompPathDelim(aDirectory);
  if (aDirectory='') or (aFileMask='')
  or (not FilenameIsAbsolute(aDirectory))
  or (not DirPathExistsCached(aDirectory))
  then exit;
  // get directory listing from cache
  Cache:=CodeToolBoss.DirectoryCachePool.GetCache(aDirectory,true,false);
  if Cache=nil then exit;
  Files:=TStringList.Create;
  TVFiles:=TStringList.Create;
  MaskList:=TMaskList.Create(aFileMask,';');
  try
    if MaskList.Count=0 then exit;
    Cache.GetFiles(Files,false);

    //debugln(['TBuildProjectDialog.AddDirectory AllFiles="',Files.Text,'"']);
    // filter files
    for i:=0 to Files.Count-1 do
      if MaskList.Matches(Files[i]) then
        TVFiles.Add(Files[i]);
    //debugln(['TBuildProjectDialog.AddDirectory FilteredFiles="',TVFiles.Text,'"']);
    if TVFiles.Count=0 then exit;

    // create tree node for aTVPath
    ParentTVNode:=nil;
    p:=System.Pos('/',aTVPath);
    if p>0 then begin
      NodeText:=copy(aTVPath,1,p-1);
      Delete(aTVPath,1,p);
    end else begin
      NodeText:=aTVPath;
    end;
    if ParentTVNode=nil then
      TVNode:=FilesTreeView.Items.FindTopLvlNode(NodeText)
    else
      TVNode:=ParentTVNode.FindNode(NodeText);
    if TVNode=nil then
      TVNode:=FilesTreeView.Items.AddChild(ParentTVNode,NodeText);
    TVNode.ImageIndex:=ImageIndexDirectory;
    TVNode.SelectedIndex:=ImageIndexDirectory;
    ParentTVNode:=TVNode;

    // create tree node for directory
    NodeText:=FProject.GetShortFilename(aDirectory,true);
    TVNode:=ParentTVNode.GetFirstChild;
    while (TVNode<>nil) and (CompareFilenames(TVNode.Text,NodeText)<0) do
      TVNode:=TVNode.GetNextSibling;
    if TVNode=nil then
      TVNode:=FilesTreeView.Items.AddChild(ParentTVNode,NodeText)
    else if (CompareFilenames(TVNode.Text,NodeText)<>0) then
      TVNode:=FilesTreeView.Items.Add(TVNode,NodeText);
    if TVNode.Data=nil then begin
      Item:=TBuildProjectDialogItem.Create;
      Item.IsDirectory:=true;
      Item.Filename:=aDirectory;
      TVNode.Data:=Item;
    end;
    TVNode.ImageIndex:=ImageIndexDirectory;
    TVNode.SelectedIndex:=ImageIndexDirectory;
    ParentTVNode:=TVNode;

    // add files
    aDirectory:=AppendPathDelim(aDirectory);
    for i:=0 to TVFiles.Count-1 do begin
      Item:=TBuildProjectDialogItem.Create;
      Item.Filename:=aDirectory+TVFiles[i];
      TVNode:=FilesTreeView.Items.AddChildObject(ParentTVNode,TVFiles[i],Item);
      TVNode.ImageIndex:=ImageIndexFile;
      TVNode.SelectedIndex:=ImageIndexFile;
    end;
  finally
    MaskList.Free;
    Files.Free;
    TVFiles.Free;
  end;
end;

procedure TCleanBuildProjectDialog.AddDirectories(aTVPath, aSearchPath,
  aFileMask: string);
var
  Directory: String;
  p: Integer;
begin
  p:=1;
  while p<=length(aSearchPath) do begin
    Directory:=TrimFilename(GetNextDelimitedItem(aSearchPath,';',p));
    if FilenameIsAbsolute(Directory) then
      AddDirectory(aTVPath,Directory,aFileMask);
  end;
end;

function TCleanBuildProjectDialog.GetAllFilesFromTree: TFilenameToStringTree;
var
  Node: TTreeNode;
  Item: TBuildProjectDialogItem;
begin
  Result:=TFilenameToStringTree.Create(false);
  Node:=FilesTreeView.Items.GetFirstNode;
  while Node<>nil do begin
    if (Node.Data<>nil) and (TObject(Node.Data) is TBuildProjectDialogItem) then
    begin
      Item:=TBuildProjectDialogItem(Node.Data);
      if not Item.IsDirectory then
        Result[Item.Filename]:='1';
    end;
    Node:=Node.GetNext;
  end;
end;

function TCleanBuildProjectDialog.DeleteFiles: TModalResult;
var
  Files: TFilenameToStringTree;
  Node: TAVLTreeNode;
  Item: PStringToStringItem;
  MaskList: TMaskList;
  Filename: String;
  SourceFiles: TStringList;
  Quiet: Boolean;
begin
  Files:=GetAllFilesFromTree;
  MaskList:=TMaskList.Create('*.pas;*.pp;*.p;*.inc;*.lpr;*.lpi;*.lps;*.lpk',';');
  SourceFiles:=TStringList.Create;
  try
    // warn before deleting sources
    Node:=Files.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      Filename:=Item^.Name;
      if MaskList.Matches(ExtractFilename(Filename)) then
        SourceFiles.Add(Filename);
      Node:=Files.Tree.FindSuccessor(Node);
    end;
    if SourceFiles.Count>0 then begin
      Result:=IDEMessageDialog(lisCCOWarningCaption,
        Format(lisCBPReallyDeleteSourceFiles, [IntToStr(SourceFiles.Count),
          LineEnding+LineEnding, copy(SourceFiles.Text, 1, 1000)]),
        mtWarning, [mbYes, mbNo]);
      if Result<>mrYes then exit(mrCancel);
    end;

    // delete
    Node:=Files.Tree.FindLowest;
    Quiet:=false;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      Node:=Files.Tree.FindSuccessor(Node);
      Filename:=Item^.Name;
      //debugln(['TBuildProjectDialog.DeleteFiles ',Filename,' ',FileExistsUTF8(Filename)]);
      repeat
        if FileExistsUTF8(Filename) and (not DeleteFileUTF8(Filename))
        and (not Quiet) then begin
          Result:=IDEQuestionDialog(lisDeleteFileFailed,
              Format(lisPkgMangUnableToDeleteFile, [Filename]),
              mtError, [mrRetry,
                        mrCancel,
                        mrNo, lisCCOSkip,
                        mrNoToAll, lisSkipErrors]);
          if Result=mrNoToAll then begin
            Quiet:=true;
            break;
          end;
          if Result=mrNo then break;
          if Result<>mrRetry then exit(mrCancel);
        end else break;
      until false;
    end;

    Result:=mrOk;
  finally
    InvalidateFileStateCache;
    UpdateFilesTreeView;
    SourceFiles.Free;
    MaskList.Free;
    Files.Free;
  end;
end;

procedure TCleanBuildProjectDialog.Init(AProject: TProject);
var
  List: THistoryList;
begin
  List:=InputHistories.HistoryLists.GetList(hlCleanBuildFileMask,true,rltFile);
  ProjOutMaskComboBox.Items.Assign(List);
  ProjOutMaskComboBox.Text:=AProject.CleanOutputFileMask;
  ProjSrcMaskComboBox.Items.Assign(List);
  ProjSrcMaskComboBox.Text:=AProject.CleanSourcesFileMask;
  PkgOutMaskComboBox.Items.Assign(List);
  PkgOutMaskComboBox.Text:=InputHistories.CleanOutputFileMask;
  PkgSrcMaskComboBox.Items.Assign(List);
  PkgSrcMaskComboBox.Text:=InputHistories.CleanSourcesFileMask;

  ProjOutCheckBox.Checked:=EnvironmentOptions.CleanBuildProjOut;
  ProjSrcCheckBox.Checked:=EnvironmentOptions.CleanBuildProjSrc;
  PkgOutCheckBox.Checked :=EnvironmentOptions.CleanBuildPkgOut;
  PkgSrcCheckBox.Checked :=EnvironmentOptions.CleanBuildPkgSrc;

  if AProject.CompilerOptions.UnitOutputDirectory='' then begin
    ProjOutCheckBox.Enabled:=false;
    ProjOutCheckBox.Checked:=false;
    ProjOutMaskComboBox.Enabled:=false;
  end;

  FProject:=AProject;
  UpdateFilesTreeView;
end;

end.

