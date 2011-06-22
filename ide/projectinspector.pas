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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TProjectInspectorForm is the form of the project inspector.

  ToDo:
    - show lfm/lrs files as sub items
    - dnd move
    - project groups:
      - activate
   popup menu:
      - add
      - remove
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
  Classes, SysUtils, LCLProc, AvgLvlTree, Forms, Controls, Buttons,
  ComCtrls, StdCtrls, Menus, Dialogs, Graphics, FileUtil, ExtCtrls,
  LazIDEIntf, IDECommands,
  LazarusIDEStrConsts, IDEProcs, IDEOptionDefs, EnvironmentOpts,
  Project, AddToProjectDlg, PackageSystem, PackageDefs;
  
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
    pifAllChanged,
    pifItemsChanged,
    pifFilesChanged,
    pifButtonsChanged,
    pifTitleChanged
    );
  TProjectInspectorFlags = set of TProjectInspectorFlag;

  { TProjInspFileItem }

  TProjInspFileItem = class
  public
    Filename: string;
    constructor Create(AFilename: string);
  end;

  { TProjectInspectorForm }

  TProjectInspectorForm = class(TForm)
    AddBitBtn: TSpeedButton;
    BtnPanel: TPanel;
    DirectoryHierarchySpeedButton: TSpeedButton;
    FilterEdit: TEdit;
    OpenBitBtn: TSpeedButton;
    ItemsTreeView: TTreeView;
    ItemsPopupMenu: TPopupMenu;
    OptionsBitBtn: TSpeedButton;
    RemoveBitBtn: TSpeedButton;
    SortAlphabeticallySpeedButton: TSpeedButton;
    procedure AddBitBtnClick(Sender: TObject);
    procedure DirectoryHierarchySpeedButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterEditEnter(Sender: TObject);
    procedure FilterEditExit(Sender: TObject);
    procedure ItemsPopupMenuPopup(Sender: TObject);
    procedure ItemsTreeViewDblClick(Sender: TObject);
    procedure ItemsTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure MoveDependencyUpClick(Sender: TObject);
    procedure MoveDependencyDownClick(Sender: TObject);
    procedure SetDependencyDefaultFilenameMenuItemClick(Sender: TObject);
    procedure SetDependencyPreferredFilenameMenuItemClick(Sender: TObject);
    procedure ClearDependencyFilenameMenuItemClick(Sender: TObject);
    procedure OpenBitBtnClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure ProjectInspectorFormShow(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure RemoveNonExistingFilesMenuItemClick(Sender: TObject);
    procedure SortAlphabeticallySpeedButtonClick(Sender: TObject);
    procedure ToggleI18NForLFMMenuItemClick(Sender: TObject);
  private
    FFilter: string;
    FIdleConnected: boolean;
    FOnAddDependency: TAddProjInspDepEvent;
    FOnAddUnitToProject: TOnAddUnitToProject;
    FOnOpen: TNotifyEvent;
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
    DependenciesNode: TTreeNode;
    RemovedDependenciesNode: TTreeNode;
    ImageIndexFiles: integer;
    ImageIndexRequired: integer;
    ImageIndexConflict: integer;
    ImageIndexRemovedRequired: integer;
    ImageIndexProject: integer;
    ImageIndexUnit: integer;
    ImageIndexRegisterUnit: integer;
    ImageIndexText: integer;
    ImageIndexBinary: integer;
    ImageIndexDirectory: integer;
    FFlags: TProjectInspectorFlags;
    procedure SetDependencyDefaultFilename(AsPreferred: boolean);
    procedure SetFilter(const AValue: string);
    procedure SetIdleConnected(const AValue: boolean);
    procedure SetLazProject(const AValue: TProject);
    procedure SetShowDirectoryHierarchy(const AValue: boolean);
    procedure SetSortAlphabetically(const AValue: boolean);
    procedure SetupComponents;
    procedure UpdateProjectFiles(Immediately: boolean);
    procedure UpdateRequiredPackages;
    procedure UpdateRemovedRequiredPackages;
    function GetImageIndexOfFile(AFile: TUnitInfo): integer;
    procedure OnProjectBeginUpdate(Sender: TObject);
    procedure OnProjectEndUpdate(Sender: TObject; ProjectChanged: boolean);
    function FitsFilter(aFilename: string): boolean;
    function CompareProjFilenames(AFilename1, AFilename2: string): integer;
    procedure FreeTVNodeData(Node: TTreeNode);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure IdleHandler(Sender: TObject; var Done: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure UpdateAll(Immediately: boolean);
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateItems(Immediately: boolean);
    function GetSelectedFile: TUnitInfo;
    function GetSelectedDependency: TPkgDependency;
    function StoreCurrentTreeSelection: TStringList;
    procedure ApplyTreeSelection(ASelection: TStringList; FreeList: boolean);
  public
    property LazProject: TProject read FLazProject write SetLazProject;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
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
    property Filter: string read FFilter write SetFilter;
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
    property ShowDirectoryHierarchy: boolean read FShowDirectoryHierarchy write SetShowDirectoryHierarchy;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;
  
var
  ProjInspector: TProjectInspectorForm = nil;


implementation

{$R *.lfm}

uses
  IDEImagesIntf;

{ TProjInspFileItem }

constructor TProjInspFileItem.Create(AFilename: string);
begin
  Filename:=AFilename;
end;

{ TProjectInspectorForm }

procedure TProjectInspectorForm.ItemsTreeViewDblClick(Sender: TObject);
begin
  OpenBitBtnClick(Self);
end;

procedure TProjectInspectorForm.ItemsTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key:=0;
    Close;
  end;
end;

procedure TProjectInspectorForm.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TProjectInspectorForm.MoveDependencyUpClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  Dependency:=GetSelectedDependency;
  if (Dependency=nil) or (Dependency.Removed)
  or (Dependency.PrevRequiresDependency=nil) then exit;
  LazProject.MoveRequiredDependencyUp(Dependency);
end;

procedure TProjectInspectorForm.MoveDependencyDownClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  Dependency:=GetSelectedDependency;
  if (Dependency=nil) or (Dependency.Removed)
  or (Dependency.NextRequiresDependency=nil) then exit;
  LazProject.MoveRequiredDependencyDown(Dependency);
end;

procedure TProjectInspectorForm.SetDependencyDefaultFilenameMenuItemClick(
  Sender: TObject);
begin
  SetDependencyDefaultFilename(false);
end;

procedure TProjectInspectorForm.SetDependencyPreferredFilenameMenuItemClick(
  Sender: TObject);
begin
  SetDependencyDefaultFilename(true);
end;

procedure TProjectInspectorForm.ClearDependencyFilenameMenuItemClick(
  Sender: TObject);
var
  CurDependency: TPkgDependency;
begin
  CurDependency:=GetSelectedDependency;
  if (CurDependency=nil) then exit;
  if CurDependency.RequiredPackage=nil then exit;
  if CurDependency.DefaultFilename='' then exit;
  CurDependency.DefaultFilename:='';
  CurDependency.PreferDefaultFilename:=false;
  LazProject.Modified:=true;
  UpdateRequiredPackages;
  UpdateButtons;
end;

procedure TProjectInspectorForm.AddBitBtnClick(Sender: TObject);
var
  AddResult: TAddToProjectResult;
  i: Integer;
  NewFilename: string;
  NewFile: TUnitInfo;
begin
  if ShowAddToProjectDlg(LazProject,AddResult)<>mrOk then exit;
  
  case AddResult.AddType of
  a2pFiles:
    begin
      BeginUpdate;
      for i:=0 to AddResult.FileNames.Count-1 do begin
        NewFilename:=AddResult.FileNames[i];
        NewFile:=LazProject.UnitInfoWithFilename(NewFilename);
        if NewFile<>nil then begin
          if NewFile.IsPartOfProject then continue;
        end else begin
          NewFile:=TUnitInfo.Create(nil);
          NewFile.Filename:=NewFilename;
          LazProject.AddFile(NewFile,false);
        end;
        NewFile.IsPartOfProject:=true;
        if Assigned(OnAddUnitToProject) then begin
          if OnAddUnitToProject(Self,NewFile)<>mrOk then break;
        end;
        FNextSelectedPart:=NewFile;
      end;
      UpdateAll(false);
      EndUpdate;
    end;
  
  a2pRequiredPkg:
    begin
      BeginUpdate;
      if Assigned(OnAddDependency) then
        OnAddDependency(Self,AddResult.Dependency);
      FNextSelectedPart:=AddResult.Dependency;
      UpdateItems(false);
      EndUpdate;
    end;
  
  end;
  
  AddResult.Free;
end;

procedure TProjectInspectorForm.DirectoryHierarchySpeedButtonClick(
  Sender: TObject);
begin
  ShowDirectoryHierarchy:=DirectoryHierarchySpeedButton.Down;
end;

procedure TProjectInspectorForm.FilterEditChange(Sender: TObject);
begin
  Filter:=FilterEdit.Text;
end;

procedure TProjectInspectorForm.FilterEditEnter(Sender: TObject);
begin
  if FilterEdit.Text=lisCEFilter then
    FilterEdit.Text:='';
end;

procedure TProjectInspectorForm.FilterEditExit(Sender: TObject);
begin
  if FilterEdit.Text=lisCEFilter then
    FilterEdit.Text:='';
end;

procedure TProjectInspectorForm.ItemsPopupMenuPopup(Sender: TObject);
var
  ItemCnt: integer;

  function AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent;
    EnabledFlag: boolean): TMenuItem;
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
  CurFile: TUnitInfo;
  CurDependency: TPkgDependency;
  Item: TMenuItem;
begin
  ItemCnt:=0;
  CurFile:=GetSelectedFile;
  if CurFile<>nil then begin
    AddPopupMenuItem(lisOpenFile, @OpenBitBtnClick, true);
    AddPopupMenuItem(lisPckEditRemoveFile, @RemoveBitBtnClick,
      RemoveBitBtn.Enabled);
    if FilenameIsPascalSource(CurFile.Filename) then begin
      Item:=AddPopupMenuItem(lisDisableI18NForLFM,
                             @ToggleI18NForLFMMenuItemClick,true);
      Item.Checked:=CurFile.DisableI18NForLFM;
      Item.ShowAlwaysCheckable:=true;
    end;
  end;
  CurDependency:=GetSelectedDependency;
  if CurDependency<>nil then begin
    if CurDependency.Removed then begin
      AddPopupMenuItem(lisMenuOpenPackage, @OpenBitBtnClick, true);
      AddPopupMenuItem(lisPckEditReAddDependency, @ReAddMenuItemClick,
                       AddBitBtn.Enabled);
    end else begin
      AddPopupMenuItem(lisMenuOpenPackage, @OpenBitBtnClick, true);
      AddPopupMenuItem(lisPckEditRemoveDependency, @RemoveBitBtnClick,
                       RemoveBitBtn.Enabled);
      AddPopupMenuItem(lisPckEditMoveDependencyUp, @MoveDependencyUpClick,
                       (CurDependency.PrevRequiresDependency<>nil));
      AddPopupMenuItem(lisPckEditMoveDependencyDown, @MoveDependencyDownClick,
                       (CurDependency.NextRequiresDependency<>nil));
      AddPopupMenuItem(lisPckEditStoreFileNameAsDefaultForThisDependency,
                       @SetDependencyDefaultFilenameMenuItemClick,
                       (CurDependency.RequiredPackage<>nil));
      AddPopupMenuItem(lisPckEditStoreFileNameAsPreferredForThisDependency,
                       @SetDependencyPreferredFilenameMenuItemClick,
                       (CurDependency.RequiredPackage<>nil));
      AddPopupMenuItem(lisPckEditClearDefaultPreferredFilenameOfDependency,
                       @ClearDependencyFilenameMenuItemClick,
                       (CurDependency.DefaultFilename<>''));
    end;
  end;

  AddPopupMenuItem(lisRemoveNonExistingFiles,@RemoveNonExistingFilesMenuItemClick,
          not LazProject.IsVirtual);

  while ItemsPopupMenu.Items.Count>ItemCnt do
    ItemsPopupMenu.Items.Delete(ItemsPopupMenu.Items.Count-1);
end;

procedure TProjectInspectorForm.OpenBitBtnClick(Sender: TObject);
begin
  if Assigned(OnOpen) then OnOpen(Self);
end;

procedure TProjectInspectorForm.OptionsBitBtnClick(Sender: TObject);
begin
  if Assigned(OnShowOptions) then OnShowOptions(Self);
end;

procedure TProjectInspectorForm.ProjectInspectorFormShow(Sender: TObject);
begin
  UpdateAll(false);
end;

procedure TProjectInspectorForm.ReAddMenuItemClick(Sender: TObject);
var
  Dependency: TPkgDependency;
begin
  Dependency:=GetSelectedDependency;
  if (Dependency=nil) or (not Dependency.Removed)
  or (not CheckAddingDependency(LazProject,Dependency)) then exit;
  BeginUpdate;
  if Assigned(OnReAddDependency) then OnReAddDependency(Self,Dependency);
  EndUpdate;
end;

procedure TProjectInspectorForm.RemoveBitBtnClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  CurFile: TUnitInfo;
begin
  CurDependency:=GetSelectedDependency;
  if (CurDependency<>nil) and (not CurDependency.Removed) then begin
    if MessageDlg(lisProjInspConfirmDeletingDependency,
      Format(lisProjInspDeleteDependencyFor, [CurDependency.AsString]),
      mtConfirmation,[mbYes,mbNo],0)<>mrYes
    then exit;
    if Assigned(OnRemoveDependency) then OnRemoveDependency(Self,CurDependency);
    exit;
  end;
  
  CurFile:=GetSelectedFile;
  if CurFile<>nil then begin
    if (not CurFile.IsPartOfProject) or (CurFile=LazProject.MainUnitInfo)
    then exit;
    if MessageDlg(lisProjInspConfirmRemovingFile,
      Format(lisProjInspRemoveFileFromProject, [CurFile.Filename]),
      mtConfirmation,[mbYes,mbNo],0)<>mrYes
    then exit;
    if Assigned(OnRemoveFile) then OnRemoveFile(Self,CurFile);
  end;
end;

procedure TProjectInspectorForm.RemoveNonExistingFilesMenuItemClick(
  Sender: TObject);
var
  AnUnitInfo: TUnitInfo;
  NextUnitInfo: TUnitInfo;
  HasChanged: Boolean;
begin
  if LazProject.IsVirtual then exit;
  HasChanged:=false;
  AnUnitInfo:=LazProject.FirstPartOfProject;
  while AnUnitInfo<>nil do begin
    NextUnitInfo:=AnUnitInfo.NextPartOfProject;
    if (not AnUnitInfo.IsVirtual)
    and (not FileExistsUTF8(AnUnitInfo.Filename)) then begin
      AnUnitInfo.IsPartOfProject:=false;
      HasChanged:=true;
    end;
    AnUnitInfo:=NextUnitInfo;
  end;
  if HasChanged then begin
    LazProject.Modified:=true;
    UpdateProjectFiles(false);
  end;
end;

procedure TProjectInspectorForm.SortAlphabeticallySpeedButtonClick(
  Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallySpeedButton.Down;
end;

procedure TProjectInspectorForm.ToggleI18NForLFMMenuItemClick(Sender: TObject);
var
  CurFile: TUnitInfo;
begin
  CurFile:=GetSelectedFile;
  if CurFile=nil then exit;
  CurFile.DisableI18NForLFM:=not CurFile.DisableI18NForLFM;
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
  UpdateAll(false);
end;

procedure TProjectInspectorForm.SetShowDirectoryHierarchy(const AValue: boolean
  );
begin
  if FShowDirectoryHierarchy=AValue then exit;
  FShowDirectoryHierarchy:=AValue;
  DirectoryHierarchySpeedButton.Down:=ShowDirectoryHierarchy;
  UpdateProjectFiles(false);
end;

procedure TProjectInspectorForm.SetSortAlphabetically(const AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallySpeedButton.Down:=SortAlphabetically;
  UpdateProjectFiles(false);
end;

procedure TProjectInspectorForm.SetDependencyDefaultFilename(
  AsPreferred: boolean);
var
  NewFilename: String;
  CurDependency: TPkgDependency;
begin
  CurDependency:=GetSelectedDependency;
  if (CurDependency=nil) then exit;
  if CurDependency.RequiredPackage=nil then exit;
  NewFilename:=CurDependency.RequiredPackage.Filename;
  if (NewFilename=CurDependency.DefaultFilename)
  and (CurDependency.PreferDefaultFilename=AsPreferred) then exit;
  CurDependency.DefaultFilename:=NewFilename;
  CurDependency.PreferDefaultFilename:=AsPreferred;
  LazProject.Modified:=true;
  UpdateRequiredPackages;
  UpdateButtons;
end;

procedure TProjectInspectorForm.SetFilter(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=AValue;
  if NewValue=lisCEFilter then NewValue:='';
  NewValue:=LowerCase(NewValue);
  //debugln(['TProjectInspectorForm.SetFilter Old="',Filter,'" New="',NewValue,'"']);
  if FFilter=NewValue then exit;
  FFilter:=NewValue;
  if not FilterEdit.Focused then
    if Filter='' then
      FilterEdit.Text:=lisCEFilter
    else
      FilterEdit.Text:=Filter;
  UpdateProjectFiles(false);
end;

procedure TProjectInspectorForm.SetIdleConnected(const AValue: boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if FIdleConnected then
    Application.AddOnIdleHandler(@IdleHandler)
  else
    Application.RemoveOnIdleHandler(@IdleHandler);
end;

procedure TProjectInspectorForm.SetupComponents;
begin
  ItemsTreeView.Images := IDEImages.Images_16;
  ImageIndexFiles := IDEImages.LoadImage(16, 'pkg_files');
  ImageIndexRequired := IDEImages.LoadImage(16, 'pkg_required');
  ImageIndexConflict := IDEImages.LoadImage(16, 'pkg_conflict');
  ImageIndexRemovedRequired := IDEImages.LoadImage(16, 'pkg_removedrequired');
  ImageIndexProject := IDEImages.LoadImage(16, 'item_project');
  ImageIndexUnit := IDEImages.LoadImage(16, 'item_unit');
  ImageIndexRegisterUnit := IDEImages.LoadImage(16, 'pkg_registerunit');
  ImageIndexText := IDEImages.LoadImage(16, 'pkg_text');
  ImageIndexBinary := IDEImages.LoadImage(16, 'pkg_binary');
  ImageIndexDirectory := IDEImages.LoadImage(16, 'pkg_files');

  OpenBitBtn.LoadGlyphFromLazarusResource('laz_open');
  AddBitBtn.LoadGlyphFromLazarusResource('laz_add');
  RemoveBitBtn.LoadGlyphFromLazarusResource('laz_delete');
  OptionsBitBtn.LoadGlyphFromLazarusResource('menu_environment_options');

  OpenBitBtn.Caption:='';
  AddBitBtn.Caption:='';
  RemoveBitBtn.Caption:='';
  OptionsBitBtn.Caption:='';
  OpenBitBtn.Hint:=lisOpenFile;
  AddBitBtn.Hint:=lisCodeTemplAdd;
  RemoveBitBtn.Hint:=lisExtToolRemove;
  OptionsBitBtn.Hint:=dlgFROpts;
  FilterEdit.Text:=lisCEFilter;
  SortAlphabeticallySpeedButton.Hint:=lisPESortFilesAlphabetically;
  SortAlphabeticallySpeedButton.LoadGlyphFromLazarusResource('pkg_sortalphabetically');
  DirectoryHierarchySpeedButton.Hint:=lisPEShowDirectoryHierarchy;
  DirectoryHierarchySpeedButton.LoadGlyphFromLazarusResource('pkg_hierarchical');

  with ItemsTreeView do begin
    FFilesNode:=Items.Add(nil, dlgEnvFiles);
    FFilesNode.ImageIndex:=ImageIndexFiles;
    FFilesNode.SelectedIndex:=FFilesNode.ImageIndex;
    DependenciesNode:=Items.Add(nil, lisPckEditRequiredPackages);
    DependenciesNode.ImageIndex:=ImageIndexRequired;
    DependenciesNode.SelectedIndex:=DependenciesNode.ImageIndex;
  end;
end;

procedure TProjectInspectorForm.UpdateProjectFiles(Immediately: boolean);
var
  CurFile: TUnitInfo;
  CurNode: TTreeNode;
  OldSelection: TStringList;
  Files: TStringList;
  TVNodeStack: TFPList;
  ExpandedState: TTreeNodeExpandedState;
  Filename: String;
  i: Integer;
begin
  if (not Immediately) or (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifFilesChanged);
    IdleConnected:=true;
    exit;
  end;
  Exclude(FFlags,pifFilesChanged);

  ItemsTreeView.BeginUpdate;
  if FNextSelectedPart=nil then
    OldSelection:=StoreCurrentTreeSelection
  else
    OldSelection:=nil;
  Files:=TStringList.Create;
  TVNodeStack:=TFPList.Create;
  ExpandedState:=TTreeNodeExpandedState.Create(ItemsTreeView);
  try
    FreeTVNodeData(FFilesNode);
    if LazProject<>nil then begin
      // collect and sort files
      CurFile:=LazProject.FirstPartOfProject;
      while CurFile<>nil do begin
        Filename:=CurFile.GetShortFilename(true);
        if (Filename<>'') and FitsFilter(Filename) then begin
          i:=Files.Count-1;
          while i>=0 do begin
            if CompareProjFilenames(Filename,Files[i])>=0 then break;
            dec(i);
          end;
          Files.Insert(i+1,Filename);
          Files.Objects[i+1]:=CurFile;
        end;
        CurFile:=CurFile.NextPartOfProject;
      end;
      //debugln(['TProjectInspectorForm.UpdateFiles filtered=',Files.Count,' of ',LazProject.FileCount,' Filter="',Filter,'" Hierachy=',ShowDirectoryHierarchy,' SortAlpha=',SortAlphabetically]);

      // update treeview nodes
      if Files.Count=0 then
        FFilesNode.DeleteChildren
      else begin
        CurNode:=FFilesNode.GetFirstChild;
        for i:=0 to Files.Count-1 do begin
          Filename:=Files[i];
          CurFile:=TUnitInfo(Files.Objects[i]);
          TVClearUnneededAndCreateHierachy(ItemsTreeView,FFilesNode,Filename,
                        TVNodeStack,ShowDirectoryHierarchy,ImageIndexDirectory);
          CurNode:=TTreeNode(TVNodeStack[TVNodeStack.Count-1]);
          if FNextSelectedPart<>nil then begin
            CurNode.Selected:=FNextSelectedPart=CurFile;
            FNextSelectedPart:=nil;
          end;
          CurNode.Data:=TProjInspFileItem.Create(CurFile.Filename);
          CurNode.ImageIndex:=GetImageIndexOfFile(CurFile);
          CurNode.SelectedIndex:=CurNode.ImageIndex;
          CurNode.DeleteChildren;
        end;
        TVDeleteUnneededNodes(TVNodeStack,0);
      end;

    end else begin
      // delete file nodes
      FFilesNode.DeleteChildren;
    end;
    ExpandedState.Apply(ItemsTreeView);
    FFilesNode.Expanded:=true;
  finally
    ExpandedState.Free;
    TVNodeStack.Free;
    Files.Free;
  end;
  if OldSelection<>nil then
    ApplyTreeSelection(OldSelection,true);
  ItemsTreeView.EndUpdate;
end;

procedure TProjectInspectorForm.UpdateRequiredPackages;
var
  Dependency: TPkgDependency;
  NodeText, AFilename: String;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
begin
  ItemsTreeView.BeginUpdate;
  if LazProject<>nil then begin
    Dependency:=LazProject.FirstRequiredDependency;
    CurNode:=DependenciesNode.GetFirstChild;
    while Dependency<>nil do begin
      NodeText:=Dependency.AsString;
      if Dependency.DefaultFilename<>'' then begin
        AFilename:=Dependency.MakeFilenameRelativeToOwner(
                                                    Dependency.DefaultFilename);
        if Dependency.PreferDefaultFilename then
          NodeText:=Format(lisCEIn, [NodeText,AFilename])  // like the 'in' keyword in the uses section
        else
          NodeText:=Format(lisPckEditDefault, [NodeText, AFilename]);
      end;
      if CurNode=nil then
        CurNode:=ItemsTreeView.Items.AddChild(DependenciesNode,NodeText)
      else
        CurNode.Text:=NodeText;
      if Dependency.LoadPackageResult=lprSuccess then
        CurNode.ImageIndex:=ImageIndexRequired
      else
        CurNode.ImageIndex:=ImageIndexConflict;
      CurNode.SelectedIndex:=CurNode.ImageIndex;
      Dependency:=Dependency.NextRequiresDependency;
      CurNode:=CurNode.GetNextSibling;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    DependenciesNode.Expanded:=true;
  end else begin
    // delete dependency nodes
    DependenciesNode.HasChildren:=false;
  end;
  ItemsTreeView.EndUpdate;
end;

procedure TProjectInspectorForm.UpdateRemovedRequiredPackages;
var
  Dependency: TPkgDependency;
  NodeText: String;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
begin
  ItemsTreeView.BeginUpdate;
  if (LazProject<>nil) and (LazProject.FirstRemovedDependency<>nil) then begin
    Dependency:=LazProject.FirstRemovedDependency;
    if RemovedDependenciesNode=nil then begin
      RemovedDependenciesNode:=
        ItemsTreeView.Items.Add(DependenciesNode,
          lisProjInspRemovedRequiredPackages);
      RemovedDependenciesNode.ImageIndex:=ImageIndexRemovedRequired;
      RemovedDependenciesNode.SelectedIndex:=RemovedDependenciesNode.ImageIndex;
    end;
    CurNode:=RemovedDependenciesNode.GetFirstChild;
    while Dependency<>nil do begin
      NodeText:=Dependency.AsString;
      if CurNode=nil then
        CurNode:=ItemsTreeView.Items.AddChild(RemovedDependenciesNode,NodeText)
      else
        CurNode.Text:=NodeText;
      CurNode.ImageIndex:=RemovedDependenciesNode.ImageIndex;
      CurNode.SelectedIndex:=CurNode.ImageIndex;
      Dependency:=Dependency.NextRequiresDependency;
      CurNode:=CurNode.GetNextSibling;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    RemovedDependenciesNode.Expanded:=true;
  end else begin
    // delete removed dependency nodes
    if RemovedDependenciesNode<>nil then
      FreeThenNil(RemovedDependenciesNode);
  end;
  ItemsTreeView.EndUpdate;
end;

function TProjectInspectorForm.GetImageIndexOfFile(AFile: TUnitInfo): integer;
begin
  if FilenameIsPascalUnit(AFile.Filename) then
    Result:=ImageIndexUnit
  else if (LazProject<>nil) and (LazProject.MainUnitinfo=AFile) then
    Result:=ImageIndexProject
  else
    Result:=ImageIndexText;
end;

procedure TProjectInspectorForm.OnProjectBeginUpdate(Sender: TObject);
begin
  BeginUpdate;
end;

procedure TProjectInspectorForm.OnProjectEndUpdate(Sender: TObject;
  ProjectChanged: boolean);
begin
  UpdateAll(false);
  EndUpdate;
end;

function TProjectInspectorForm.FitsFilter(aFilename: string): boolean;
begin
  Result:=(Filter='') or (System.Pos(Filter,lowercase(aFilename))>0);
end;

function TProjectInspectorForm.CompareProjFilenames(AFilename1,
  AFilename2: string): integer;
begin
  if SortAlphabetically then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else if ShowDirectoryHierarchy then
    Result:=CompareFilenames(ExtractFilePath(AFilename1), ExtractFilePath(AFilename2))
  else
    Result:=0;
end;

procedure TProjectInspectorForm.FreeTVNodeData(Node: TTreeNode);
var
  Child: TTreeNode;
begin
  if Node=nil then exit;
  if (Node.Data<>nil) then begin
    TObject(Node.Data).Free;
    Node.Data:=nil;
  end;
  Child:=Node.GetFirstChild;
  while Child<>nil do
  begin
    FreeTVNodeData(Child);
    Child:=Child.GetNextSibling;
  end;
end;

procedure TProjectInspectorForm.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

procedure TProjectInspectorForm.IdleHandler(Sender: TObject; var Done: Boolean
  );
begin
  if (not Visible) or (FUpdateLock>0) then begin
    IdleConnected:=false;
    exit;
  end;
  if pifAllChanged in FFlags then
    UpdateAll(true)
  else if pifItemsChanged in FFlags then
    UpdateItems(true)
  else if pifFilesChanged in FFlags then
    UpdateProjectFiles(true)
  else if pifTitleChanged in FFlags then
    UpdateTitle
  else if pifButtonsChanged in FFlags then
    UpdateButtons
  else
    IdleConnected:=false;
end;

function TProjectInspectorForm.GetSelectedFile: TUnitInfo;
var
  CurNode: TTreeNode;
  Item: TProjInspFileItem;
begin
  Result:=nil;
  if LazProject=nil then exit;
  CurNode:=ItemsTreeView.Selected;
  if (CurNode=nil) then exit;
  //debugln(['TProjectInspectorForm.GetCurrentFile ',DbgSName(TObject(CurNode.Data)),' ',CurNode.Text]);
  if TObject(CurNode.Data) is TProjInspFileItem then
  begin
    Item:=TProjInspFileItem(CurNode.Data);
    //debugln(['TProjectInspectorForm.GetCurrentFile Item=',Item.Filename,' ',Item.IsDirectory]);
    Result:=LazProject.UnitInfoWithFilename(Item.Filename);
  end;
end;

function TProjectInspectorForm.GetSelectedDependency: TPkgDependency;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  if LazProject=nil then exit;
  CurNode:=ItemsTreeView.Selected;
  if (CurNode=nil) then exit;
  NodeIndex:=CurNode.Index;
  if (CurNode.Parent=DependenciesNode) then begin
    Result:=GetDependencyWithIndex(LazProject.FirstRequiredDependency,
                                   pdlRequires,NodeIndex);
  end;
  if (CurNode.Parent=RemovedDependenciesNode) then begin
    Result:=GetDependencyWithIndex(LazProject.FirstRemovedDependency,
                                   pdlRequires,NodeIndex);
  end;
end;

function TProjectInspectorForm.StoreCurrentTreeSelection: TStringList;
var
  ANode: TTreeNode;
begin
  Result:=TStringList.Create;
  ANode:=ItemsTreeView.Selected;
  while ANode<>nil do begin
    Result.Insert(0,ANode.Text);
    ANode:=ANode.Parent;
  end;
end;

procedure TProjectInspectorForm.ApplyTreeSelection(ASelection: TStringList;
  FreeList: boolean);
var
  ANode: TTreeNode;
  CurText: string;
begin
  ANode:=nil;
  while ASelection.Count>0 do begin
    CurText:=ASelection[0];
    if ANode=nil then
      ANode:=ItemsTreeView.Items.GetFirstNode
    else
      ANode:=ANode.GetFirstChild;
    while (ANode<>nil) and (ANode.Text<>CurText) do
      ANode:=ANode.GetNextSibling;
    if ANode=nil then break;
    ASelection.Delete(0);
  end;
  if ANode<>nil then ItemsTreeView.Selected:=ANode;
  if FreeList then ASelection.Free;
end;

constructor TProjectInspectorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:=NonModalIDEWindowNames[nmiwProjectInspector];
  Caption:=lisMenuProjectInspector;
  KeyPreview:=true;

  SetupComponents;
  KeyPreview:=true;
end;

destructor TProjectInspectorForm.Destroy;
begin
  IdleConnected:=false;
  BeginUpdate;
  FreeTVNodeData(FFilesNode);
  LazProject:=nil;
  inherited Destroy;
  if ProjInspector=Self then ProjInspector:=nil;
end;

procedure TProjectInspectorForm.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TProjectInspectorForm.EndUpdate;
begin
  if FUpdateLock=0 then RaiseException('TProjectInspectorForm.EndUpdate');
  dec(FUpdateLock);
end;

function TProjectInspectorForm.IsUpdateLocked: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TProjectInspectorForm.UpdateAll(Immediately: boolean);
begin
  if (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifAllChanged);
    IdleConnected:=true;
    exit;
  end;
  Exclude(FFlags,pifAllChanged);
  UpdateTitle;
  UpdateButtons;
  UpdateItems(true);
end;

procedure TProjectInspectorForm.UpdateTitle;
var
  NewCaption: String;
begin
  if (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifTitleChanged);
    IdleConnected:=true;
    exit;
  end;
  Exclude(FFlags,pifTitleChanged);
  if LazProject=nil then
    Caption:=lisMenuProjectInspector
  else begin
    NewCaption:=LazProject.Title;
    if NewCaption='' then
      NewCaption:=ExtractFilenameOnly(LazProject.ProjectInfoFile);
    Caption:=Format(lisProjInspProjectInspector, [NewCaption]);
  end;
end;

procedure TProjectInspectorForm.UpdateButtons;
var
  CurFile: TUnitInfo;
  CurDependency: TPkgDependency;
begin
  if (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifButtonsChanged);
    IdleConnected:=true;
    exit;
  end;
  Exclude(FFlags,pifButtonsChanged);
  if LazProject<>nil then begin
    AddBitBtn.Enabled:=true;
    CurFile:=GetSelectedFile;
    CurDependency:=GetSelectedDependency;
    RemoveBitBtn.Enabled:=((CurFile<>nil) and (CurFile<>LazProject.MainUnitInfo))
                      or ((CurDependency<>nil) and (not CurDependency.Removed));
    OpenBitBtn.Enabled:=((CurFile<>nil)
                     or ((CurDependency<>nil) and (not CurDependency.Removed)));
    OptionsBitBtn.Enabled:=true;
  end else begin
    AddBitBtn.Enabled:=false;
    RemoveBitBtn.Enabled:=false;
    OpenBitBtn.Enabled:=false;
    OptionsBitBtn.Enabled:=false;
  end;
end;

procedure TProjectInspectorForm.UpdateItems(Immediately: boolean);
begin
  if (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifItemsChanged);
    IdleConnected:=true;
    exit;
  end;
  Exclude(FFlags,pifItemsChanged);
  ItemsTreeView.BeginUpdate;
  UpdateProjectFiles(true);
  UpdateRequiredPackages;
  UpdateRemovedRequiredPackages;
  ItemsTreeView.EndUpdate;
end;

end.

