{
 /***************************************************************************
                          projectinspector.pas
                          --------------------


 ***************************************************************************/

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
    TProjectInspectorForm is the form of the project inspector.

  ToDo:
    - project groups:
      - activate
   popup menu:
      - copy file name
      - save
      - options
      - activate
      - compile
      - build
      - view source
      - close
      - remove project
      - build sooner Ctrl+Up
      - build later Ctrl+Down
      - compile all from here
      - build all from here
}
unit ProjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, Forms, Controls, Buttons, ComCtrls,
  Menus, Dialogs, FileUtil, LazFileUtils, LazFileCache, ExtCtrls, Graphics,
  TreeFilterEdit,
  // IDEIntf
  IDEHelpIntf, IDECommands, IDEDialogs, IDEImagesIntf, LazIDEIntf, ProjectIntf,
  PackageIntf, PackageLinkIntf, MainIntf,
  // IDE
  LazarusIDEStrConsts, IDEProcs, DialogProcs, IDEOptionDefs, EnvironmentOpts,
  PackageDefs, Project, PackageEditor, AddToProjectDlg, AddPkgDependencyDlg,
  InputHistory, MainBase, ProjPackChecks, PackageLinks, AddFPMakeDependencyDlg;

type
  TOnAddUnitToProject =
    function(Sender: TObject; AnUnitInfo: TUnitInfo): TModalresult of object;
  TRemoveProjInspFileEvent =
    function(Sender: TObject; AnUnitInfo: TUnitInfo): TModalResult of object;
  TRemoveProjInspDepEvent = function(Sender: TObject;
                           ADependency: TPkgDependency): TModalResult of object;
  TAddProjInspDepEvent = function(Sender: TObject;
                           ADependency: TPkgDependency): TModalResult of object;

  TProjectInspectorFlag = (
    pifNeedUpdateFiles,
    pifNeedUpdateDependencies,
    pifNeedUpdateButtons,
    pifNeedUpdateTitle
    );
  TProjectInspectorFlags = set of TProjectInspectorFlag;

  { TProjectInspectorForm }

  TProjectInspectorForm = class(TForm,IFilesEditorInterface)
    AddPopupMenu: TPopupMenu;
    BtnPanel: TPanel;
    DirectoryHierarchyButton: TSpeedButton;
    FilterEdit: TTreeFilterEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    mnuAddFPMakeReq: TMenuItem;
    mnuAddEditorFiles: TMenuItem;
    mnuAddDiskFile: TMenuItem;
    mnuAddDiskFiles: TMenuItem;
    mnuAddReq: TMenuItem;
    OpenButton: TSpeedButton;
    ItemsTreeView: TTreeView;
    ItemsPopupMenu: TPopupMenu;
    SortAlphabeticallyButton: TSpeedButton;
    // toolbar
    ToolBar: TToolBar;
    // toolbuttons
    AddBitBtn: TToolButton;
    RemoveBitBtn: TToolButton;
    OptionsBitBtn: TToolButton;
    HelpBitBtn: TToolButton;
    procedure CopyMoveToDirMenuItemClick(Sender: TObject);
    procedure DirectoryHierarchyButtonClick(Sender: TObject);
    procedure FilterEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ItemsPopupMenuPopup(Sender: TObject);
    procedure ItemsTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; Stage: TCustomDrawStage;
      var {%H-}PaintImages, {%H-}DefaultDraw: Boolean);
    procedure ItemsTreeViewDblClick(Sender: TObject);
    procedure ItemsTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ItemsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ItemsTreeViewKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure mnuAddBitBtnClick(Sender: TObject);
    procedure mnuAddDiskFilesClick(Sender: TObject);
    procedure mnuAddEditorFilesClick(Sender: TObject);
    procedure mnuAddFPMakeReqClick(Sender: TObject);
    procedure mnuAddReqClick(Sender: TObject);
    procedure MoveDependencyUpClick(Sender: TObject);
    procedure MoveDependencyDownClick(Sender: TObject);
    procedure SetDependencyDefaultFilenameMenuItemClick(Sender: TObject);
    procedure SetDependencyPreferredFilenameMenuItemClick(Sender: TObject);
    procedure ClearDependencyFilenameMenuItemClick(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure HelpBitBtnClick(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure RemoveNonExistingFilesMenuItemClick(Sender: TObject);
    procedure SortAlphabeticallyButtonClick(Sender: TObject);
    procedure EnableI18NForLFMMenuItemClick(Sender: TObject);
    procedure DisableI18NForLFMMenuItemClick(Sender: TObject);
  private
    FIdleConnected: boolean;
    FOnAddDependency: TAddProjInspDepEvent;
    FOnAddUnitToProject: TOnAddUnitToProject;
    FOnCopyMoveFiles: TNotifyEvent;
    FOnDragDropTreeView: TDragDropEvent;
    FOnDragOverTreeView: TOnDragOverTreeView;
    FOnReAddDependency: TAddProjInspDepEvent;
    FOnRemoveDependency: TRemoveProjInspDepEvent;
    FOnRemoveFile: TRemoveProjInspFileEvent;
    FOnShowOptions: TNotifyEvent;
    FShowDirectoryHierarchy: boolean;
    FSortAlphabetically: boolean;
    FUpdateLock: integer;
    FLazProject: TProject;
    FFilesNode: TTreeNode;
    FNextSelectedPart: TObject;// select this file/dependency on next update
    FDependenciesNode: TTreeNode;
    FRemovedDependenciesNode: TTreeNode;
    ImageIndexFiles: integer;
    ImageIndexRequired: integer;
    ImageIndexConflict: integer;
    ImageIndexAvailableOnline: integer;
    ImageIndexRemovedRequired: integer;
    ImageIndexProject: integer;
    ImageIndexUnit: integer;
    ImageIndexRegisterUnit: integer;
    ImageIndexText: integer;
    ImageIndexBinary: integer;
    ImageIndexDirectory: integer;
    FFlags: TProjectInspectorFlags;
    FProjectNodeDataList : array [TPENodeType] of TPENodeData;
    procedure AddMenuItemClick(Sender: TObject);
    function AddOneFile(aFilename: string): TModalResult;
    procedure DoAddMoreDialog(AInitTab: TAddToProjectType);
    procedure DoAddDepDialog;
    procedure DoAddFPMakeDepDialog;
    procedure FreeNodeData(Typ: TPENodeType);
    function CreateNodeData(Typ: TPENodeType; aName: string; aRemoved: boolean): TPENodeData;
    procedure SetDependencyDefaultFilename(AsPreferred: boolean);
    procedure SetIdleConnected(AValue: boolean);
    procedure SetLazProject(const AValue: TProject);
    procedure SetShowDirectoryHierarchy(const AValue: boolean);
    procedure SetSortAlphabetically(const AValue: boolean);
    procedure SetupComponents;
    function OnTreeViewGetImageIndex({%H-}Str: String; Data: TObject; var {%H-}AIsEnabled: Boolean): Integer;
    procedure OnProjectBeginUpdate(Sender: TObject);
    procedure OnProjectEndUpdate(Sender: TObject; ProjectChanged: boolean);
    procedure EnableI18NForSelectedLFM(TheEnable: boolean);
    procedure DoOnPackageListAvailable(Sender: TObject);
    function FindOnlinePackageLink(const ADependency: TPkgDependency): TPackageLink;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure IdleHandler(Sender: TObject; var {%H-}Done: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function IsUpdateLocked: boolean; inline;
    procedure UpdateTitle;
    procedure UpdateProjectFiles;
    procedure UpdateRequiredPackages;
    procedure UpdateButtons;
    procedure UpdatePending;
    function CanUpdate(Flag: TProjectInspectorFlag): boolean;
    function GetSingleSelectedDependency: TPkgDependency;
    function TreeViewToInspector(TV: TTreeView): TProjectInspectorForm;
  public
    // IFilesEditorInterface
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateAll(Immediately: boolean = false);
    function GetNodeData(TVNode: TTreeNode): TPENodeData;
    function GetNodeItem(NodeData: TPENodeData): TObject;
    function GetNodeDataItem(TVNode: TTreeNode; out NodeData: TPENodeData;
      out Item: TObject): boolean;
    function ExtendIncSearchPath(NewIncPaths: string): boolean;
    function ExtendUnitSearchPath(NewUnitPaths: string): boolean;
    function FilesBaseDirectory: string;
    function FilesEditForm: TCustomForm;
    function FilesEditTreeView: TTreeView;
    function FilesOwner: TObject;
    function FilesOwnerName: string;
    function FilesOwnerReadOnly: boolean;
    function FirstRequiredDependency: TPkgDependency;
    function GetNodeFilename(Node: TTreeNode): string;
    function IsDirectoryNode(Node: TTreeNode): boolean;
    function TVNodeFiles: TTreeNode;
    function TVNodeRequiredPackages: TTreeNode;
  public
    property LazProject: TProject read FLazProject write SetLazProject;
    property OnShowOptions: TNotifyEvent read FOnShowOptions write FOnShowOptions;
    property OnAddUnitToProject: TOnAddUnitToProject read FOnAddUnitToProject
                                                     write FOnAddUnitToProject;
    property OnAddDependency: TAddProjInspDepEvent
                             read FOnAddDependency write FOnAddDependency;
    property OnRemoveFile: TRemoveProjInspFileEvent read FOnRemoveFile
                                                    write FOnRemoveFile;
    property OnRemoveDependency: TRemoveProjInspDepEvent
                             read FOnRemoveDependency write FOnRemoveDependency;
    property OnReAddDependency: TAddProjInspDepEvent
                             read FOnReAddDependency write FOnReAddDependency;
    property OnDragDropTreeView: TDragDropEvent read FOnDragDropTreeView
                                                      write FOnDragDropTreeView;
    property OnDragOverTreeView: TOnDragOverTreeView read FOnDragOverTreeView
                                                      write FOnDragOverTreeView;
    property OnCopyMoveFiles: TNotifyEvent read FOnCopyMoveFiles
                                           write FOnCopyMoveFiles;
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
    property ShowDirectoryHierarchy: boolean read FShowDirectoryHierarchy write SetShowDirectoryHierarchy;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  ProjInspector: TProjectInspectorForm = nil;


implementation

{$R *.lfm}

{ TProjectInspectorForm }

// inline
function TProjectInspectorForm.IsUpdateLocked: boolean;
begin
  Result:=FUpdateLock>0;
end;

function TProjectInspectorForm.TVNodeFiles: TTreeNode;
begin
  Result:=FFilesNode;
end;

function TProjectInspectorForm.TVNodeRequiredPackages: TTreeNode;
begin
  Result:=FDependenciesNode;
end;

procedure TProjectInspectorForm.ItemsTreeViewDblClick(Sender: TObject);
begin
  OpenButtonClick(Self);
end;

procedure TProjectInspectorForm.ItemsTreeViewDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  OnDragDropTreeView(Sender,Source,X,Y);
end;

procedure TProjectInspectorForm.ItemsTreeViewDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  TargetTVNode: TTreeNode;
  TargetTVType: TTreeViewInsertMarkType;
begin
  if not OnDragOverTreeView(Sender,Source,X,Y, TargetTVNode, TargetTVType) then
  begin
    ItemsTreeView.SetInsertMark(nil,tvimNone);
    Accept:=false;
    exit;
  end;

  if State=dsDragLeave then
    ItemsTreeView.SetInsertMark(nil,tvimNone)
  else
    ItemsTreeView.SetInsertMark(TargetTVNode,TargetTVType);
  Accept:=true;
end;

procedure TProjectInspectorForm.ItemsTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  Handled := True;
  try
    if Key = VK_ESCAPE then
      Close
    else if Key = VK_RETURN then
      OpenButtonClick(Nil)
    else if Key = VK_DELETE then
      RemoveBitBtnClick(Nil)
    else if Key = VK_INSERT then
      AddMenuItemClick(Nil)
    else
      Handled := False;
  finally
    if Handled then
      Key := VK_UNKNOWN;
  end;
end;

procedure TProjectInspectorForm.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TProjectInspectorForm.mnuAddBitBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  i: Integer;
  ADirectory: String;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    ADirectory:=LazProject.Directory;
    if not FilenameIsAbsolute(ADirectory) then ADirectory:='';
    if ADirectory<>'' then
      OpenDialog.InitialDir:=ADirectory;
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options
                          +[ofFileMustExist,ofPathMustExist,ofAllowMultiSelect];
    OpenDialog.Filter:=dlgFilterAll+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+dlgFilterLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+dlgFilterLazarusInclude+' (*.inc)|*.inc'
                 +'|'+dlgFilterLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm';
    if OpenDialog.Execute then begin
      for i:=0 to OpenDialog.Files.Count-1 do
        if not (AddOneFile(OpenDialog.Files[i]) in [mrOk, mrIgnore]) then break;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TProjectInspectorForm.mnuAddDiskFilesClick(Sender: TObject);
begin
  DoAddMoreDialog(a2pFiles);
end;

procedure TProjectInspectorForm.mnuAddEditorFilesClick(Sender: TObject);
begin
  DoAddMoreDialog(a2pEditorFiles);
end;

procedure TProjectInspectorForm.mnuAddFPMakeReqClick(Sender: TObject);
begin
  DoAddFPMakeDepDialog;
end;

procedure TProjectInspectorForm.mnuAddReqClick(Sender: TObject);
begin
  DoAddDepDialog;
end;

procedure TProjectInspectorForm.MoveDependencyUpClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  Dependency:=GetSingleSelectedDependency;
  if SortAlphabetically or (Dependency=nil) or Dependency.Removed
  or (Dependency.PrevRequiresDependency=nil) then exit;
  LazProject.MoveRequiredDependencyUp(Dependency);
end;

procedure TProjectInspectorForm.MoveDependencyDownClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  Dependency:=GetSingleSelectedDependency;
  if SortAlphabetically or (Dependency=nil) or Dependency.Removed
  or (Dependency.NextRequiresDependency=nil) then exit;
  LazProject.MoveRequiredDependencyDown(Dependency);
end;

procedure TProjectInspectorForm.SetDependencyDefaultFilenameMenuItemClick(Sender: TObject);
begin
  SetDependencyDefaultFilename(false);
end;

procedure TProjectInspectorForm.SetDependencyPreferredFilenameMenuItemClick(Sender: TObject);
begin
  SetDependencyDefaultFilename(true);
end;

procedure TProjectInspectorForm.ClearDependencyFilenameMenuItemClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  BeginUpdate;
  try
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if not (Item is TPkgDependency) then continue;
      CurDependency:=TPkgDependency(Item);
      if CurDependency.DefaultFilename='' then exit;
      CurDependency.DefaultFilename:='';
      CurDependency.PreferDefaultFilename:=false;
      LazProject.Modified:=true;
      UpdateRequiredPackages;
    end;
  finally
    EndUpdate;
  end;
end;

function TProjectInspectorForm.AddOneFile(aFilename: string): TModalResult;
var
  NewFile: TUnitInfo;
begin
  Result := mrOK;
  aFilename:=CleanAndExpandFilename(aFilename);
  NewFile:=LazProject.UnitInfoWithFilename(aFilename);
  if NewFile<>nil then begin
    if NewFile.IsPartOfProject then Exit(mrIgnore);
  end else begin
    NewFile:=TUnitInfo.Create(nil);
    NewFile.Filename:=aFilename;
    LazProject.AddFile(NewFile,false);
  end;
  NewFile.IsPartOfProject:=true;
  if Assigned(OnAddUnitToProject) then begin
    Result:=OnAddUnitToProject(Self,NewFile);
    if Result<>mrOK then Exit;
  end;
  FNextSelectedPart:=NewFile;
end;

procedure TProjectInspectorForm.AddMenuItemClick(Sender: TObject);

  function _NodeTreeIsIn(xIterNode, xParentNode: TTreeNode): Boolean;
  begin
    Result := (xIterNode = xParentNode);
    if not Result and Assigned(xIterNode) then
      Result := _NodeTreeIsIn(xIterNode.Parent, xParentNode);
  end;

begin
  //check the selected item in ItemsTreeView
  // -> if it's "Required Packages", call "New Requirement" (mnuAddReqClick)
  // -> otherwise (selected = "Files") call "Add files from file system" (AddBitBtnClick)
  if _NodeTreeIsIn(ItemsTreeView.Selected, FDependenciesNode) then
    mnuAddReqClick(Sender)
  else
    mnuAddBitBtnClick(Sender);
end;

procedure TProjectInspectorForm.DoAddMoreDialog(AInitTab: TAddToProjectType);
var
  AddResult: TAddToProjectResult;
  i: Integer;
begin
  AddResult:=nil;
  if ShowAddToProjectDlg(LazProject,AddResult,AInitTab)<>mrOk then exit;

  case AddResult.AddType of
  a2pFiles:
    begin
      BeginUpdate;
      for i:=0 to AddResult.FileNames.Count-1 do
        if not (AddOneFile(AddResult.FileNames[i]) in [mrOk, mrIgnore]) then break;
      UpdateAll;
      EndUpdate;
    end;
  else
    Showmessage('Not implemented');
  end;

  AddResult.Free;
end;

procedure TProjectInspectorForm.DoAddDepDialog;
var
  Deps: TPkgDependencyList;
  i: Integer;
  Resu: TModalResult;
begin
  Resu:=ShowAddPkgDependencyDlg(LazProject, Deps);
  try
    if (Resu<>mrOK) or (Deps.Count=0) or (OnAddDependency=nil) then exit;
    try
      BeginUpdate;
      for i := 0 to Deps.Count-1 do
        OnAddDependency(Self, Deps[i]);
      FNextSelectedPart:=Deps[Deps.Count-1];
      UpdateRequiredPackages;
    finally
      EndUpdate;
    end;
  finally
    Deps.Free;
  end;
end;

procedure TProjectInspectorForm.DoAddFPMakeDepDialog;
var
  Deps: TPkgDependencyList;
  i: Integer;
  Resu: TModalResult;
begin
  Resu:=ShowAddFPMakeDependencyDlg(LazProject, Deps);
  try
    if (Resu<>mrOK) or (Deps.Count=0) then exit;
    try
      BeginUpdate;
      for i := 0 to Deps.Count-1 do
        OnAddDependency(Self, Deps[i]);
      FNextSelectedPart:=Deps[Deps.Count-1];
    finally
      EndUpdate;
    end;
  finally
    Deps.Free;
  end;
end;

procedure TProjectInspectorForm.CopyMoveToDirMenuItemClick(Sender: TObject);
begin
  OnCopyMoveFiles(Self);
end;

procedure TProjectInspectorForm.DirectoryHierarchyButtonClick(Sender: TObject);
begin
  ShowDirectoryHierarchy:=DirectoryHierarchyButton.Down;
end;

procedure TProjectInspectorForm.FilterEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    OpenButtonClick(Nil);
    Key := VK_UNKNOWN;
  end;
end;

procedure TProjectInspectorForm.FormCreate(Sender: TObject);
begin
  if OPMInterface <> nil then
    OPMInterface.OnPackageListAvailable := @DoOnPackageListAvailable;
end;

procedure TProjectInspectorForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
begin
  {$IFDEF VerboseProjInspDrag}
  debugln(['TProjectInspectorForm.FormDropFiles ',length(FileNames)]);
  {$ENDIF}
  if length(FileNames)=0 then exit;
  BeginUpdate;
  try
    for i:=0 to high(Filenames) do
      if not (AddOneFile(FileNames[i]) in [mrOk, mrIgnore]) then break;
    UpdateAll;
  finally
    EndUpdate;
  end;
end;

procedure TProjectInspectorForm.ItemsPopupMenuPopup(Sender: TObject);
var
  ItemCnt: integer;

  function AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent;
    EnabledFlag: boolean = True): TMenuItem;
  begin
    if ItemsPopupMenu.Items.Count<=ItemCnt then begin
      Result:=TMenuItem.Create(Self);
      ItemsPopupMenu.Items.Add(Result);
    end else
      Result:=ItemsPopupMenu.Items[ItemCnt];
    Result.Caption:=ACaption;
    Result.OnClick:=AnEvent;
    Result.Enabled:=EnabledFlag;
    Result.Checked:=false;
    Result.ShowAlwaysCheckable:=false;
    Result.Visible:=true;
    Result.RadioItem:=false;
    Result.ImageIndex:=-1;
    inc(ItemCnt);
  end;

var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  CanRemoveCount: Integer;
  CanOpenCount: Integer;
  HasLFMCount: Integer;
  CurUnitInfo: TUnitInfo;
  DisabledI18NForLFMCount: Integer;
  CanReAddCount: Integer;
  SingleSelectedDep: TPkgDependency;
  DepCount: Integer;
  Dependency: TPkgDependency;
  HasValidDep: Integer;
  CanClearDep: Integer;
  CanMoveFileCount: Integer;
begin
  ItemCnt:=0;

  CanRemoveCount:=0;
  CanOpenCount:=0;
  CanMoveFileCount:=0;
  HasLFMCount:=0;
  DisabledI18NForLFMCount:=0;
  CanReAddCount:=0;
  SingleSelectedDep:=nil;
  DepCount:=0;
  HasValidDep:=0;
  CanClearDep:=0;
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if Item is TUnitInfo then begin
      CurUnitInfo:=TUnitInfo(Item);
      inc(CanOpenCount);
      if (CurUnitInfo<>LazProject.MainUnitInfo) and (not NodeData.Removed) then begin
        inc(CanRemoveCount);
        inc(CanMoveFileCount);
      end;
      if FilenameIsPascalSource(CurUnitInfo.Filename)
      and FileExistsCached(ChangeFileExt(CurUnitInfo.Filename,'.lfm')) then begin
        inc(HasLFMCount);
        if CurUnitInfo.DisableI18NForLFM then
          inc(DisabledI18NForLFMCount);
      end;
    end else if Item is TPkgDependency then begin
      Dependency:=TPkgDependency(Item);
      if NodeData.Removed then begin
        inc(CanReAddCount);
      end else begin
        inc(DepCount);
        if DepCount=1 then
          SingleSelectedDep:=Dependency
        else
          SingleSelectedDep:=nil;
        inc(CanRemoveCount);
        if Dependency.DependencyType=pdtLazarus then
          inc(CanOpenCount);
        if Dependency.RequiredPackage<>nil then
          inc(HasValidDep);
        if (Dependency.DefaultFilename<>'') then
          inc(CanClearDep);
      end;
    end;
  end;

  if ItemsTreeView.Selected = FFilesNode then
  begin
    // Only the Files node is selected.
    Assert(AddBitBtn.Enabled, 'AddBitBtn not Enabled');
    AddPopupMenuItem(lisBtnDlgAdd, @mnuAddBitBtnClick);
    if not LazProject.IsVirtual then
      AddPopupMenuItem(lisRemoveNonExistingFiles,@RemoveNonExistingFilesMenuItemClick);
  end
  else if ItemsTreeView.Selected = FDependenciesNode then
  begin
    // Only the Required Packages node is selected.
    AddPopupMenuItem(lisBtnDlgAdd, @mnuAddReqClick);
  end
  else begin
    // Files, dependencies or everything mixed is selected.
    if CanOpenCount>0 then
      AddPopupMenuItem(lisOpen, @OpenButtonClick);
    if CanRemoveCount>0 then
      AddPopupMenuItem(lisRemove, @RemoveBitBtnClick);
    // files section
    if CanMoveFileCount>0 then
      AddPopupMenuItem(lisCopyMoveFileToDirectory,@CopyMoveToDirMenuItemClick);
  end;

  if LazProject.EnableI18N and LazProject.EnableI18NForLFM
  and (HasLFMCount>0) then begin
    AddPopupMenuItem(lisEnableI18NForLFM,
      @EnableI18NForLFMMenuItemClick, DisabledI18NForLFMCount>0);
    AddPopupMenuItem(lisDisableI18NForLFM,
      @DisableI18NForLFMMenuItemClick, DisabledI18NForLFMCount<HasLFMCount);
  end;

  // Required packages section
  if CanReAddCount>0 then
    AddPopupMenuItem(lisPckEditReAddDependency, @ReAddMenuItemClick, true);
  if SingleSelectedDep<>nil then begin
    AddPopupMenuItem(lisPckEditMoveDependencyUp, @MoveDependencyUpClick,
                     (SingleSelectedDep.PrevRequiresDependency<>nil));
    AddPopupMenuItem(lisPckEditMoveDependencyDown, @MoveDependencyDownClick,
                     (SingleSelectedDep.NextRequiresDependency<>nil));
  end;
  if HasValidDep>0 then begin
    AddPopupMenuItem(lisPckEditStoreFileNameAsDefaultForThisDependency,
                     @SetDependencyDefaultFilenameMenuItemClick, true);
    AddPopupMenuItem(lisPckEditStoreFileNameAsPreferredForThisDependency,
                     @SetDependencyPreferredFilenameMenuItemClick, true);
  end;
  if CanClearDep>0 then begin
    AddPopupMenuItem(lisPckEditClearDefaultPreferredFilenameOfDependency,
                     @ClearDependencyFilenameMenuItemClick, true);
  end;

  while ItemsPopupMenu.Items.Count>ItemCnt do
    ItemsPopupMenu.Items.Delete(ItemsPopupMenu.Items.Count-1);
end;

procedure TProjectInspectorForm.ItemsTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  NodeData: TPENodeData;
  r: TRect;
  y: Integer;
begin
  if Stage=cdPostPaint then begin
    NodeData:=GetNodeData(Node);
    if (NodeData<>nil) then begin
      if  (NodeData.Typ=penFile) and (not NodeData.Removed)
      and FilenameIsAbsolute(NodeData.Name)
      and (not FileExistsCached(NodeData.Name))
      then begin
        r:=Node.DisplayRect(true);
        ItemsTreeView.Canvas.Pen.Color:=clRed;
        y:=(r.Top+r.Bottom) div 2;
        ItemsTreeView.Canvas.Line(r.Left,y,r.Right,y);
      end;
    end;
  end;
end;

procedure TProjectInspectorForm.OpenButtonClick(Sender: TObject);
var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  CurFile: TUnitInfo;
  CurDependency: TPkgDependency;
  PackageLink: TPackageLink;
  PkgLinks: TList;
  NeedToRebuild: Boolean;
begin
  PkgLinks := TList.Create;
  try
    NeedToRebuild := False;
    BeginUpdate;
    try
      for i:=0 to ItemsTreeView.SelectionCount-1 do begin
        TVNode:=ItemsTreeView.Selections[i];
        if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
        if Item is TUnitInfo then begin
          CurFile:=TUnitInfo(Item);
          if LazarusIDE.DoOpenEditorFile(CurFile.Filename,-1,-1,[ofAddToRecent])<>mrOk then begin
             PkgLinks.Free;
             Exit;
          end;
        end else if (Item is TPkgDependency) and (TPkgDependency(Item).DependencyType=pdtLazarus) then begin
          CurDependency:=TPkgDependency(Item);
          if CurDependency.LoadPackageResult = lprSuccess then begin
            if PackageEditingInterface.DoOpenPackageWithName(CurDependency.PackageName,[],false)<>mrOk then begin
              PkgLinks.Free;
              Exit;
            end;
          end else begin
            if OPMInterface<>nil then begin
              PackageLink:=FindOnlinePackageLink(CurDependency);
              if PackageLink<>nil then
                PkgLinks.Add(PackageLink);
            end;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
    if (OPMInterface<>nil) and (PkgLinks.Count>0) then
      OPMInterface.InstallPackages(PkgLinks, NeedToRebuild);
  finally
    PkgLinks.Free;
  end;
  if (OPMInterface<>nil) and (NeedToRebuild) then
    MainIDEInterface.DoBuildLazarus([])
end;

procedure TProjectInspectorForm.OptionsBitBtnClick(Sender: TObject);
begin
  if Assigned(OnShowOptions) then OnShowOptions(Self);
end;

procedure TProjectInspectorForm.HelpBitBtnClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TProjectInspectorForm.ReAddMenuItemClick(Sender: TObject);
var
  Dependency: TPkgDependency;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  BeginUpdate;
  try
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if not NodeData.Removed then continue;
      if not (Item is TPkgDependency) then continue;
      Dependency:=TPkgDependency(Item);
      if not CheckAddingProjectDependency(LazProject,Dependency) then exit;
      if Assigned(OnReAddDependency) then
        OnReAddDependency(Self,Dependency);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TProjectInspectorForm.RemoveBitBtnClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  Msg, Cap: String;
  DeleteCount: Integer;
  CurFile: TUnitInfo;
begin
  BeginUpdate;
  try
    // check selection
    Msg:='';
    DeleteCount:=0;
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if Item is TUnitInfo then begin
        CurFile:=TUnitInfo(Item);
        if CurFile=LazProject.MainUnitInfo then continue;
        // remove file
        inc(DeleteCount);
        Msg:=Format(lisProjInspRemoveFileFromProject, [CurFile.Filename]);
      end else if Item is TPkgDependency then begin
        CurDependency:=TPkgDependency(item);
        if NodeData.Removed then continue;
        // remove dependency
        inc(DeleteCount);
        Msg:=Format(lisProjInspDeleteDependencyFor, [CurDependency.AsString]);
      end;
    end;

    // ask for confirmation
    if DeleteCount=0 then exit;
    if DeleteCount>1 then
      Msg:=Format(lisProjInspRemoveItemsF, [IntToStr(DeleteCount)]);
    if CurFile<>nil then
      Cap:=lisProjInspConfirmRemovingFile
    else
      Cap:=lisProjInspConfirmDeletingDependency;
    if IDEMessageDialog(Cap,
      Msg, mtConfirmation,[mbYes,mbNo])<>mrYes then exit;

    // delete
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if Item is TUnitInfo then begin
        CurFile:=TUnitInfo(Item);
        if CurFile=LazProject.MainUnitInfo then continue;
        // remove file
        if Assigned(OnRemoveFile) then OnRemoveFile(Self,CurFile);
      end else if Item is TPkgDependency then begin
        CurDependency:=TPkgDependency(item);
        if NodeData.Removed then continue;
        // remove dependency
        if Assigned(OnRemoveDependency) then
          OnRemoveDependency(Self,CurDependency);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TProjectInspectorForm.RemoveNonExistingFilesMenuItemClick(Sender: TObject);
var
  AnUnitInfo: TUnitInfo;
  NextUnitInfo: TUnitInfo;
  HasChanged: Boolean;
begin
  if LazProject.IsVirtual then exit;
  BeginUpdate;
  try
    HasChanged:=false;
    AnUnitInfo:=LazProject.FirstPartOfProject;
    while AnUnitInfo<>nil do begin
      NextUnitInfo:=AnUnitInfo.NextPartOfProject;
      if not (AnUnitInfo.IsVirtual or FileExistsUTF8(AnUnitInfo.Filename)) then begin
        AnUnitInfo.IsPartOfProject:=false;
        HasChanged:=true;
      end;
      AnUnitInfo:=NextUnitInfo;
    end;
    if HasChanged then begin
      LazProject.Modified:=true;
      UpdateProjectFiles;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TProjectInspectorForm.SortAlphabeticallyButtonClick(Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallyButton.Down;
end;

procedure TProjectInspectorForm.EnableI18NForLFMMenuItemClick(Sender: TObject);
begin
  EnableI18NForSelectedLFM(true);
end;

procedure TProjectInspectorForm.DisableI18NForLFMMenuItemClick(Sender: TObject);
begin
  EnableI18NForSelectedLFM(false);
end;

procedure TProjectInspectorForm.SetLazProject(const AValue: TProject);
begin
  if FLazProject=AValue then exit;
  if FLazProject<>nil then begin
    dec(FUpdateLock,LazProject.UpdateLock);
    FLazProject.OnBeginUpdate:=nil;
    FLazProject.OnEndUpdate:=nil;
  end;
  FLazProject:=AValue;
  if FLazProject<>nil then begin
    inc(FUpdateLock,LazProject.UpdateLock);
    FLazProject.OnBeginUpdate:=@OnProjectBeginUpdate;
    FLazProject.OnEndUpdate:=@OnProjectEndUpdate;
  end;
  UpdateAll;
end;

procedure TProjectInspectorForm.SetShowDirectoryHierarchy(const AValue: boolean);
begin
  if FShowDirectoryHierarchy=AValue then exit;
  FShowDirectoryHierarchy:=AValue;
  DirectoryHierarchyButton.Down:=FShowDirectoryHierarchy;
  FilterEdit.ShowDirHierarchy:=FShowDirectoryHierarchy;
  FilterEdit.InvalidateFilter;
  EnvironmentOptions.ProjInspShowDirHierarchy := ShowDirectoryHierarchy;
end;

procedure TProjectInspectorForm.SetSortAlphabetically(const AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallyButton.Down:=FSortAlphabetically;
  FilterEdit.SortData:=FSortAlphabetically;
  FilterEdit.InvalidateFilter;
  EnvironmentOptions.ProjInspSortAlphabetically := SortAlphabetically;
end;

procedure TProjectInspectorForm.SetDependencyDefaultFilename(AsPreferred: boolean);
var
  NewFilename: String;
  CurDependency: TPkgDependency;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
begin
  BeginUpdate;
  try
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if NodeData.Removed then continue;
      if not (Item is TPkgDependency) then continue;
      CurDependency:=TPkgDependency(Item);
      if CurDependency.RequiredPackage=nil then continue;
      NewFilename:=CurDependency.RequiredPackage.Filename;
      if (NewFilename=CurDependency.DefaultFilename) // do not use CompareFilenames
      and (CurDependency.PreferDefaultFilename=AsPreferred) then continue;
      CurDependency.DefaultFilename:=NewFilename;
      CurDependency.PreferDefaultFilename:=AsPreferred;
      LazProject.Modified:=true;
      UpdateRequiredPackages;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TProjectInspectorForm.SetIdleConnected(AValue: boolean);
begin
  if csDestroying in ComponentState then
    AValue:=false;
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@IdleHandler)
  else
    Application.RemoveOnIdleHandler(@IdleHandler);
end;

procedure TProjectInspectorForm.SetupComponents;

  function CreateToolButton(AName, ACaption, AHint, AImageName: String; AOnClick: TNotifyEvent): TToolButton;
  begin
    Result := TToolButton.Create(Self);
    Result.Name := AName;
    Result.Caption := ACaption;
    Result.Hint := AHint;
    if AImageName <> '' then
      Result.ImageIndex := IDEImages.LoadImage(AImageName);
    Result.ShowHint := True;
    Result.OnClick := AOnClick;
    Result.AutoSize := True;
    Result.Parent := ToolBar;
  end;

  function CreateDivider: TToolButton;
  begin
    Result := TToolButton.Create(Self);
    Result.Style := tbsDivider;
    Result.AutoSize := True;
    Result.Parent := ToolBar;
  end;

begin
  ImageIndexFiles           := IDEImages.LoadImage('pkg_files');
  ImageIndexRequired        := IDEImages.LoadImage('pkg_required');
  ImageIndexConflict        := IDEImages.LoadImage('pkg_conflict');
  ImageIndexAvailableOnline := IDEImages.LoadImage('pkg_install');
  ImageIndexRemovedRequired := IDEImages.LoadImage('pkg_removedrequired');
  ImageIndexProject         := IDEImages.LoadImage('item_project_source');
  ImageIndexUnit            := IDEImages.LoadImage('item_unit');
  ImageIndexRegisterUnit    := IDEImages.LoadImage('pkg_registerunit');
  ImageIndexText            := IDEImages.LoadImage('pkg_text');
  ImageIndexBinary          := IDEImages.LoadImage('pkg_binary');
  ImageIndexDirectory       := IDEImages.LoadImage('pkg_files');

  ItemsTreeView.Images      := IDEImages.Images_16;
  ToolBar.Images            := IDEImages.Images_16;
  FilterEdit.OnGetImageIndex:=@OnTreeViewGetImageIndex;

  AddBitBtn     := CreateToolButton('AddBitBtn', lisAdd, lisClickToSeeTheChoices, 'laz_add', nil);
  AddBitBtn.Style:=tbsButtonDrop;
  RemoveBitBtn  := CreateToolButton('RemoveBitBtn', lisRemove, lisPckEditRemoveSelectedItem, 'laz_delete', @RemoveBitBtnClick);
  CreateDivider;
  OptionsBitBtn := CreateToolButton('OptionsBitBtn', lisOptions, lisPckEditEditGeneralOptions, 'menu_environment_options', @OptionsBitBtnClick);
  OptionsBitBtn.DropdownMenu := TSetBuildModeToolButton.TBuildModeMenu.Create(Self);
  OptionsBitBtn.Style := tbsDropDown;
  HelpBitBtn    := CreateToolButton('HelpBitBtn', GetButtonCaption(idButtonHelp), lisMenuOnlineHelp, 'btn_help', @HelpBitBtnClick);

  AddBitBtn.DropdownMenu:=AddPopupMenu;
  mnuAddDiskFile.Caption:=lisPckEditAddFilesFromFileSystem;
  mnuAddDiskFiles.Caption:=lisAddFilesInDirectory;
  mnuAddEditorFiles.Caption:=lisProjAddEditorFile;
  mnuAddReq.Caption:=lisProjAddNewRequirement;
  mnuAddFPMakeReq.Caption:=lisProjAddNewFPMakeRequirement;

  IDEImages.AssignImage(OpenButton, 'laz_open');
  OpenButton.Caption:='';
  OpenButton.Hint:=lisOpenFile2;
  SortAlphabeticallyButton.Hint:=lisPESortFilesAlphabetically;
  IDEImages.AssignImage(SortAlphabeticallyButton, 'pkg_sortalphabetically');
  DirectoryHierarchyButton.Hint:=lisPEShowDirectoryHierarchy;
  IDEImages.AssignImage(DirectoryHierarchyButton, 'pkg_hierarchical');

  with ItemsTreeView do begin
    FFilesNode:=Items.Add(nil, dlgEnvFiles);
    FFilesNode.ImageIndex:=ImageIndexFiles;
    FFilesNode.SelectedIndex:=FFilesNode.ImageIndex;
    FDependenciesNode:=Items.Add(nil, lisPckEditRequiredPackages);
    FDependenciesNode.ImageIndex:=ImageIndexRequired;
    FDependenciesNode.SelectedIndex:=FDependenciesNode.ImageIndex;
  end;
end;

function TProjectInspectorForm.OnTreeViewGetImageIndex(Str: String; Data: TObject;
                                                var AIsEnabled: Boolean): Integer;
var
  NodeData: TPENodeData;
  Item: TObject;
begin
  Result := -1;
  if not (Data is TPENodeData) then exit;
  NodeData:=TPENodeData(Data);
  Item:=GetNodeItem(NodeData);
  if Item=nil then exit;

  if Item is TUnitInfo then begin
    if FilenameIsPascalUnit(TUnitInfo(Item).Filename) then
      Result:=ImageIndexUnit
    else if (LazProject<>nil) and (LazProject.MainUnitinfo=Item) then
      Result:=ImageIndexProject
    else
      Result:=ImageIndexText;
  end
  else if Item is TPkgDependency then begin
    if TPkgDependency(Item).Removed then
      Result:=ImageIndexRemovedRequired
    else if TPkgDependency(Item).LoadPackageResult=lprSuccess then
      Result:=ImageIndexRequired
    else begin
      Result:=ImageIndexConflict;
      if OPMInterface<>nil then
        if FindOnlinePackageLink(TPkgDependency(Item))<>nil then
          Result:=ImageIndexAvailableOnline;
    end;
  end;
end;

procedure TProjectInspectorForm.UpdateProjectFiles;
var
  CurFile: TUnitInfo;
  FilesBranch: TTreeFilterBranch;
  Filename: String;
  ANodeData : TPENodeData;
begin
  if not CanUpdate(pifNeedUpdateFiles) then exit;
  ItemsTreeView.BeginUpdate;
  try
    FilesBranch:=FilterEdit.GetCleanBranch(FFilesNode);
    FilesBranch.ClearNodeData;
    FreeNodeData(penFile);
    if LazProject<>nil then begin
      FilterEdit.SelectedPart:=FNextSelectedPart;
      FilterEdit.ShowDirHierarchy:=ShowDirectoryHierarchy;
      FilterEdit.SortData:=SortAlphabetically;
      FilterEdit.ImageIndexDirectory:=ImageIndexDirectory;
      // collect and sort files
      CurFile:=LazProject.FirstPartOfProject;
      while CurFile<>nil do begin
        Filename:=CurFile.GetShortFilename(true);
        if Filename<>'' then Begin
          ANodeData := CreateNodeData(penFile, CurFile.Filename, False);
          FilesBranch.AddNodeData(Filename, ANodeData, CurFile.Filename);
        end;
        CurFile:=CurFile.NextPartOfProject;
      end;
    end;
    FilterEdit.InvalidateFilter;            // Data is shown by FilterEdit.
  finally
    ItemsTreeView.EndUpdate;
  end;
  UpdateButtons;
end;

procedure TProjectInspectorForm.UpdateRequiredPackages;
var
  Dependency: TPkgDependency;
  RequiredBranch, RemovedBranch: TTreeFilterBranch;
  NodeText, AFilename: String;
  ANodeData : TPENodeData;
begin
  if not CanUpdate(pifNeedUpdateDependencies) then exit;
  ItemsTreeView.BeginUpdate;
  try
    RequiredBranch:=FilterEdit.GetCleanBranch(FDependenciesNode);
    RequiredBranch.ClearNodeData;
    FreeNodeData(penDependency);
    Dependency:=Nil;
    if LazProject<>nil then begin
      // required packages
      Dependency:=LazProject.FirstRequiredDependency;
      while Dependency<>nil do begin
        // Figure out the item's caption
        NodeText:=Dependency.AsString;
        if Dependency.DefaultFilename<>'' then begin
          AFilename:=Dependency.MakeFilenameRelativeToOwner(Dependency.DefaultFilename);
          if Dependency.PreferDefaultFilename then
            NodeText:=Format(lisCEIn, [NodeText,AFilename])  // like the 'in' keyword in the uses section
          else
            NodeText:=Format(lisPckEditDefault, [NodeText, AFilename]);
        end;
        if Dependency.LoadPackageResult<>lprSuccess then
          if OPMInterface<>nil then
            if FindOnlinePackageLink(Dependency)<>nil then
              NodeText:=NodeText+' '+lisPckEditAvailableOnline;
        if Dependency.DependencyType=pdtFPMake then
          NodeText:=NodeText+' '+lisPckEditFPMakePackage;
        // Add the required package under the branch
        ANodeData := CreateNodeData(penDependency, Dependency.PackageName, False);
        RequiredBranch.AddNodeData(NodeText, ANodeData);
        Dependency:=Dependency.NextRequiresDependency;
      end;

      // removed required packages
      Dependency:=LazProject.FirstRemovedDependency;
      if Dependency<>nil then begin
        // Create root node for removed dependencies if not done yet.
        if FRemovedDependenciesNode=nil then begin
          FRemovedDependenciesNode:=ItemsTreeView.Items.Add(FDependenciesNode,
                                                  lisProjInspRemovedRequiredPackages);
          FRemovedDependenciesNode.ImageIndex:=ImageIndexRemovedRequired;
          FRemovedDependenciesNode.SelectedIndex:=FRemovedDependenciesNode.ImageIndex;
        end;
        RemovedBranch:=FilterEdit.GetCleanBranch(FRemovedDependenciesNode);
        // Add all removed dependencies under the branch
        while Dependency<>nil do begin
          ANodeData := CreateNodeData(penDependency, Dependency.PackageName, True);
          RemovedBranch.AddNodeData(Dependency.AsString, ANodeData);
          Dependency:=Dependency.NextRequiresDependency;
        end;
      end;
    end;

    // Dependency is set to removed required packages if there is active project
    if (Dependency=nil) and (FRemovedDependenciesNode<>nil) then begin
      // No removed dependencies -> delete the root node
      FilterEdit.DeleteBranch(FRemovedDependenciesNode);
      FreeThenNil(FRemovedDependenciesNode);
    end;
    FilterEdit.InvalidateFilter;
  finally
    ItemsTreeView.EndUpdate;
  end;
  UpdateButtons;
end;

procedure TProjectInspectorForm.OnProjectBeginUpdate(Sender: TObject);
begin
  BeginUpdate;
end;

procedure TProjectInspectorForm.OnProjectEndUpdate(Sender: TObject;
  ProjectChanged: boolean);
begin
  if ProjectChanged then
    UpdateAll;
  EndUpdate;
end;

procedure TProjectInspectorForm.EnableI18NForSelectedLFM(TheEnable: boolean);
var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  CurUnitInfo: TUnitInfo;
begin
  for i:=0 to ItemsTreeView.SelectionCount-1 do begin
    TVNode:=ItemsTreeView.Selections[i];
    if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
    if not (Item is TUnitInfo) then continue;
    CurUnitInfo:=TUnitInfo(Item);
    if not FilenameIsPascalSource(CurUnitInfo.Filename) then continue;
    CurUnitInfo.DisableI18NForLFM:=not TheEnable;
  end;
end;

procedure TProjectInspectorForm.DoOnPackageListAvailable(Sender: TObject);
var
  CurDependency: TPkgDependency;
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  NodeText: String;
  ImageIndex: Integer;
begin
  BeginUpdate;
  try
    for i:=0 to ItemsTreeView.Items.Count-1 do begin
      TVNode:=ItemsTreeView.Items[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if not (Item is TPkgDependency) or (TPkgDependency(Item).DependencyType=pdtFPMake) then continue;
      CurDependency:=TPkgDependency(Item);
      NodeText:=CurDependency.AsString;
      ImageIndex:=ImageIndexRequired;
      if CurDependency.LoadPackageResult<>lprSuccess then begin
        ImageIndex:=ImageIndexConflict;
        if OPMInterface<>nil then begin
          if FindOnlinePackageLink(CurDependency)<>nil then begin
            NodeText:=NodeText+' '+lisPckEditAvailableOnline;
            ImageIndex:=ImageIndexAvailableOnline;
          end;
        end;
      end;
      TVNode.Text:=NodeText;
      TVNode.ImageIndex:=ImageIndex;
      TVNode.SelectedIndex:=ImageIndex;
    end;
  finally
    EndUpdate;
  end;
end;

function TProjectInspectorForm.FindOnlinePackageLink(
  const ADependency: TPkgDependency): TPackageLink;
var
  PackageLink: TPackageLink;
begin
  Result := nil;
  PackageLink := LazPackageLinks.FindLinkWithPkgName(ADependency.AsString);
  if (PackageLink <> nil) and (PackageLink.Origin = ploOnline) and
      (ADependency.IsCompatible(PackageLink.Version)) then
    Result := PackageLink;
end;

procedure TProjectInspectorForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

procedure TProjectInspectorForm.IdleHandler(Sender: TObject; var Done: Boolean);
begin
  if IsUpdateLocked then begin
    IdleConnected:=false;
    exit;
  end;
  UpdatePending;
end;

function TProjectInspectorForm.GetSingleSelectedDependency: TPkgDependency;
var
  Item: TObject;
  NodeData: TPENodeData;
begin
  Result:=nil;
  if not GetNodeDataItem(ItemsTreeView.Selected,NodeData,Item) then exit;
  if Item is TPkgDependency then
    Result:=TPkgDependency(Item);
end;

function TProjectInspectorForm.TreeViewToInspector(TV: TTreeView): TProjectInspectorForm;
begin
  if TV=ItemsTreeView then
    Result:=Self
  else
    Result:=nil;
end;

constructor TProjectInspectorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:=NonModalIDEWindowNames[nmiwProjectInspector];
  Caption:=lisMenuProjectInspector;
  KeyPreview:=true;
  SetupComponents;
  KeyPreview:=true;
  SortAlphabetically := EnvironmentOptions.ProjInspSortAlphabetically;
  ShowDirectoryHierarchy := EnvironmentOptions.ProjInspShowDirHierarchy;
end;

destructor TProjectInspectorForm.Destroy;
var
  nt: TPENodeType;
begin
  IdleConnected:=false;
  LazProject:=nil;
  inherited Destroy;
  for nt:=Low(TPENodeType) to High(TPENodeType) do
    FreeNodeData(nt);
  if ProjInspector=Self then
    ProjInspector:=nil;
end;

function TProjectInspectorForm.ExtendIncSearchPath(NewIncPaths: string): boolean;
begin
  Result:=LazProject.ExtendIncSearchPath(NewIncPaths);
end;

function TProjectInspectorForm.ExtendUnitSearchPath(NewUnitPaths: string): boolean;
begin
  Result:=LazProject.ExtendUnitSearchPath(NewUnitPaths);
end;

function TProjectInspectorForm.FilesBaseDirectory: string;
begin
  if LazProject<>nil then
    Result:=LazProject.Directory
  else
    Result:='';
end;

function TProjectInspectorForm.FilesEditForm: TCustomForm;
begin
  Result:=Self;
end;

function TProjectInspectorForm.FilesEditTreeView: TTreeView;
begin
  Result:=ItemsTreeView;
end;

function TProjectInspectorForm.FilesOwner: TObject;
begin
  Result:=LazProject;
end;

function TProjectInspectorForm.FilesOwnerName: string;
begin
  Result:=lisProject3;
end;

function TProjectInspectorForm.FilesOwnerReadOnly: boolean;
begin
  Result:=false;
end;

function TProjectInspectorForm.FirstRequiredDependency: TPkgDependency;
begin
  if LazProject<>nil then
    Result:=LazProject.FirstRequiredDependency
  else
    Result:=nil;
end;

function TProjectInspectorForm.GetNodeFilename(Node: TTreeNode): string;
var
  Item: TFileNameItem;
begin
  Result:='';
  if Node=nil then exit;
  if Node=FFilesNode then
    exit(FilesBaseDirectory);
  Item:=TFileNameItem(Node.Data);
  if (Item is TFileNameItem) then begin
    Result:=Item.Filename;
  end else if Node.HasAsParent(FFilesNode) then begin
    // directory node
    Result:=Node.Text;
  end else
    exit;
  if not FilenameIsAbsolute(Result) then
    Result:=AppendPathDelim(FilesBaseDirectory)+Result;
end;

function TProjectInspectorForm.IsDirectoryNode(Node: TTreeNode): boolean;
begin
  Result:=(Node<>nil) and (Node.Data=nil) and Node.HasAsParent(FFilesNode);
end;

procedure TProjectInspectorForm.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TProjectInspectorForm.EndUpdate;
begin
  if FUpdateLock=0 then RaiseException('TProjectInspectorForm.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then
    IdleConnected:=true;
end;

procedure TProjectInspectorForm.UpdateAll(Immediately: boolean);
begin
  ItemsTreeView.BeginUpdate;
  try
    UpdateTitle;
    UpdateProjectFiles;
    UpdateRequiredPackages;
    UpdateButtons;
    if Immediately then
      UpdatePending;
  finally
    ItemsTreeView.EndUpdate;
  end;
end;

procedure TProjectInspectorForm.UpdateTitle;
var
  NewCaption: String;
  IconStream: TStream;
begin
  if not CanUpdate(pifNeedUpdateTitle) then exit;

  Icon.Clear;
  if LazProject=nil then
  begin
    Caption:=lisMenuProjectInspector;
  end else
  begin
    NewCaption:=LazProject.GetTitle;
    if NewCaption='' then
      NewCaption:=ExtractFilenameOnly(LazProject.ProjectInfoFile);
    NewCaption:=Format(lisProjInspProjectInspector, [NewCaption]);
    if (LazProject.ActiveBuildMode.GetCaption<>'') then
      NewCaption := NewCaption + ' ['+LazProject.ActiveBuildMode.GetCaption+']';
    Caption := NewCaption;

    if not LazProject.ProjResources.ProjectIcon.IsEmpty then
    begin
      IconStream := LazProject.ProjResources.ProjectIcon.GetStream;
      if IconStream<>nil then
        try
          Icon.LoadFromStream(IconStream);
        finally
          IconStream.Free;
        end;
    end;
  end;
end;

procedure TProjectInspectorForm.UpdateButtons;
var
  i: Integer;
  TVNode: TTreeNode;
  NodeData: TPENodeData;
  Item: TObject;
  CanRemoveCount: Integer;
  CurUnitInfo: TUnitInfo;
  CanOpenCount: Integer;
begin
  if not CanUpdate(pifNeedUpdateButtons) then exit;

  if LazProject<>nil then begin
    AddBitBtn.Enabled:=true;

    CanRemoveCount:=0;
    CanOpenCount:=0;
    for i:=0 to ItemsTreeView.SelectionCount-1 do begin
      TVNode:=ItemsTreeView.Selections[i];
      if not GetNodeDataItem(TVNode,NodeData,Item) then continue;
      if Item is TUnitInfo then begin
        CurUnitInfo:=TUnitInfo(Item);
        inc(CanOpenCount);
        if CurUnitInfo<>LazProject.MainUnitInfo then
          inc(CanRemoveCount);
      end else if Item is TPkgDependency then begin
        if not NodeData.Removed and (TPkgDependency(Item).DependencyType=pdtLazarus) then begin
          inc(CanRemoveCount);
          inc(CanOpenCount);
        end;
      end;
    end;

    RemoveBitBtn.Enabled:=(CanRemoveCount>0);
    OpenButton.Enabled:=(CanOpenCount>0);
    OptionsBitBtn.Enabled:=true;
  end else begin
    AddBitBtn.Enabled:=false;
    RemoveBitBtn.Enabled:=false;
    OpenButton.Enabled:=false;
    OptionsBitBtn.Enabled:=false;
  end;
end;

procedure TProjectInspectorForm.UpdatePending;
begin
  ItemsTreeView.BeginUpdate;
  try
    if pifNeedUpdateFiles in FFlags then
      UpdateProjectFiles;
    if pifNeedUpdateDependencies in FFlags then
      UpdateRequiredPackages;
    if pifNeedUpdateTitle in FFlags then
      UpdateTitle;
    if pifNeedUpdateButtons in FFlags then
      UpdateButtons;
    IdleConnected:=false;
  finally
    ItemsTreeView.EndUpdate;
  end;
end;

function TProjectInspectorForm.CanUpdate(Flag: TProjectInspectorFlag): boolean;
begin
  Result:=false;
  if csDestroying in ComponentState then exit;
  if LazProject=nil then exit;
  if IsUpdateLocked then begin
    Include(fFlags,Flag);
    IdleConnected:=true;
    Result:=false;
  end else begin
    Exclude(fFlags,Flag);
    Result:=true;
  end;
end;

procedure TProjectInspectorForm.FreeNodeData(Typ: TPENodeType);
var
  NodeData,
  n: TPENodeData;
begin
  NodeData:=FProjectNodeDataList[Typ];
  while NodeData<>nil do begin
    n:=NodeData;
    NodeData:=NodeData.Next;
    n.Free;
  end;
  FProjectNodeDataList[Typ]:=nil;
End;

function TProjectInspectorForm.CreateNodeData(Typ: TPENodeType;
  aName: string; aRemoved: boolean): TPENodeData;
Begin
  Result := TPENodeData.Create(Typ,aName,aRemoved);
  Result.Next := FProjectNodeDataList[Typ];
  FProjectNodeDataList[Typ] := Result;
end;

function TProjectInspectorForm.GetNodeData(TVNode: TTreeNode): TPENodeData;
var
  o: TObject;
begin
  Result:=nil;
  if (TVNode=nil) then exit;
  o:=TObject(TVNode.Data);
  if o is TFileNameItem then
    o:=TObject(TFileNameItem(o).Data);
  if o is TPENodeData then
    Result:=TPENodeData(o);
end;

function TProjectInspectorForm.GetNodeItem(NodeData: TPENodeData): TObject;
begin
  Result:=nil;
  if (LazProject=nil) or (NodeData=nil) then exit;
  case NodeData.Typ of
  penFile:
    if NodeData.Removed then
      Result:=nil
    else
      Result:=LazProject.UnitInfoWithFilename(NodeData.Name,[pfsfOnlyProjectFiles]);
  penDependency:
    if NodeData.Removed then
      Result:=LazProject.FindRemovedDependencyByName(NodeData.Name)
    else
      Result:=LazProject.FindDependencyByName(NodeData.Name);
  end;
end;

function TProjectInspectorForm.GetNodeDataItem(TVNode: TTreeNode; out
  NodeData: TPENodeData; out Item: TObject): boolean;
begin
  Result:=false;
  Item:=nil;
  NodeData:=GetNodeData(TVNode);
  Item:=GetNodeItem(NodeData);
  Result:=Item<>nil;
end;

end.

