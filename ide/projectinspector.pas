{  $Id$  }
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
}
unit ProjectInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Buttons, ComCtrls,
  StdCtrls, Menus, Dialogs, Graphics, FileUtil,
  IDECommands,
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
    pifItemsChanged,
    pifButtonsChanged,
    pifTitleChanged
    );
  TProjectInspectorFlags = set of TProjectInspectorFlag;

  { TProjectInspectorForm }

  TProjectInspectorForm = class(TForm)
    OpenBitBtn: TBitBtn;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    OptionsBitBtn: TBitBtn;
    ItemsTreeView: TTreeView;
    ItemsPopupMenu: TPopupMenu;
    procedure AddBitBtnClick(Sender: TObject);
    procedure ItemsPopupMenuPopup(Sender: TObject);
    procedure ItemsTreeViewDblClick(Sender: TObject);
    procedure ItemsTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure MoveDependencyUpClick(Sender: TObject);
    procedure MoveDependencyDownClick(Sender: TObject);
    procedure SetDependencyFilenameMenuItemClick(Sender: TObject);
    procedure ClearDependencyFilenameMenuItemClick(Sender: TObject);
    procedure OpenBitBtnClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure ProjectInspectorFormShow(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RemoveBitBtnClick(Sender: TObject);
  private
    FOnAddDependency: TAddProjInspDepEvent;
    FOnAddUnitToProject: TOnAddUnitToProject;
    FOnOpen: TNotifyEvent;
    FOnReAddDependency: TAddProjInspDepEvent;
    FOnRemoveDependency: TRemoveProjInspDepEvent;
    FOnRemoveFile: TRemoveProjInspFileEvent;
    FOnShowOptions: TNotifyEvent;
    FUpdateLock: integer;
    FLazProject: TProject;
    FilesNode: TTreeNode;
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
    FFlags: TProjectInspectorFlags;
    procedure SetLazProject(const AValue: TProject);
    procedure SetupComponents;
    procedure UpdateProjectItems;
    procedure UpdateRequiredPackages;
    procedure UpdateRemovedRequiredPackages;
    function GetImageIndexOfFile(AFile: TUnitInfo): integer;
    procedure OnProjectBeginUpdate(Sender: TObject);
    procedure OnProjectEndUpdate(Sender: TObject; ProjectChanged: boolean);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdateLocked: boolean;
    procedure UpdateAll;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateItems;
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
  end;
  
var
  ProjInspector: TProjectInspectorForm = nil;


implementation
uses
  IDEImagesIntf;

{ TProjectInspectorForm }

procedure TProjectInspectorForm.ItemsTreeViewDblClick(Sender: TObject);
begin
  OpenBitBtnClick(Self);
end;

procedure TProjectInspectorForm.ItemsTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Close;
end;

procedure TProjectInspectorForm.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TProjectInspectorForm.MoveDependencyUpClick(Sender: TObject);
var
  Dependency: TPkgDependency;
  OldSelection: TStringList;
begin
  Dependency:=GetSelectedDependency;
  if (Dependency=nil) or (Dependency.Removed)
  or (Dependency.PrevRequiresDependency=nil) then exit;
  ItemsTreeView.BeginUpdate;
  OldSelection:=StoreCurrentTreeSelection;
  LazProject.MoveRequiredDependencyUp(Dependency);
  ApplyTreeSelection(OldSelection,true);
  ItemsTreeView.EndUpdate;
end;

procedure TProjectInspectorForm.MoveDependencyDownClick(Sender: TObject);
var
  Dependency: TPkgDependency;
  OldSelection: TStringList;
begin
  Dependency:=GetSelectedDependency;
  if (Dependency=nil) or (Dependency.Removed)
  or (Dependency.NextRequiresDependency=nil) then exit;
  ItemsTreeView.BeginUpdate;
  OldSelection:=StoreCurrentTreeSelection;
  LazProject.MoveRequiredDependencyDown(Dependency);
  ApplyTreeSelection(OldSelection,true);
  ItemsTreeView.EndUpdate;
end;

procedure TProjectInspectorForm.SetDependencyFilenameMenuItemClick(
  Sender: TObject);
var
  CurDependency: TPkgDependency;
  NewFilename: String;
begin
  CurDependency:=GetSelectedDependency;
  if (CurDependency=nil) then exit;
  if CurDependency.RequiredPackage=nil then exit;
  NewFilename:=CurDependency.RequiredPackage.Filename;
  if NewFilename=CurDependency.DefaultFilename then exit;
  CurDependency.DefaultFilename:=NewFilename;
  LazProject.Modified:=true;
  UpdateRequiredPackages;
  UpdateButtons;
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
      end;
      UpdateAll;
      EndUpdate;
    end;
  
  a2pRequiredPkg:
    begin
      BeginUpdate;
      if Assigned(OnAddDependency) then
        OnAddDependency(Self,AddResult.Dependency);
      UpdateItems;
      EndUpdate;
    end;
  
  end;
  
  AddResult.Free;
end;

procedure TProjectInspectorForm.ItemsPopupMenuPopup(Sender: TObject);
var
  ItemCnt: integer;

  procedure AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent;
    EnabledFlag: boolean);
  var
    CurMenuItem: TMenuItem;
  begin
    if ItemsPopupMenu.Items.Count<=ItemCnt then begin
      CurMenuItem:=TMenuItem.Create(Self);
      ItemsPopupMenu.Items.Add(CurMenuItem);
    end else
      CurMenuItem:=ItemsPopupMenu.Items[ItemCnt];
    CurMenuItem.Caption:=ACaption;
    CurMenuItem.OnClick:=AnEvent;
    CurMenuItem.Enabled:=EnabledFlag;
    inc(ItemCnt);
  end;

var
  CurFile: TUnitInfo;
  CurDependency: TPkgDependency;
begin
  ItemCnt:=0;
  CurFile:=GetSelectedFile;
  if CurFile<>nil then begin
    AddPopupMenuItem(lisOpenFile, @OpenBitBtnClick, true);
    AddPopupMenuItem(lisPckEditRemoveFile, @RemoveBitBtnClick,
      RemoveBitBtn.Enabled);
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
      AddPopupMenuItem(lisPckEditSetDependencyDefaultFilename,
                       @SetDependencyFilenameMenuItemClick,
                       (CurDependency.RequiredPackage<>nil));
      AddPopupMenuItem(lisPckEditClearDependencyDefaultFilename,
                       @ClearDependencyFilenameMenuItemClick,
                       (CurDependency.DefaultFilename<>''));
    end;
  end;

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
  UpdateAll;
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

  OpenBitBtn.Caption:=lisMenuOpen;
  AddBitBtn.Caption:=lisCodeTemplAdd;
  RemoveBitBtn.Caption:=lisExtToolRemove;
  OptionsBitBtn.Caption:=dlgFROpts;

  with ItemsTreeView do begin
    FilesNode:=Items.Add(nil, dlgEnvFiles);
    FilesNode.ImageIndex:=ImageIndexFiles;
    FilesNode.SelectedIndex:=FilesNode.ImageIndex;
    DependenciesNode:=Items.Add(nil, lisPckEditRequiredPackages);
    DependenciesNode.ImageIndex:=ImageIndexRequired;
    DependenciesNode.SelectedIndex:=DependenciesNode.ImageIndex;
  end;
end;

procedure TProjectInspectorForm.UpdateProjectItems;
var
  CurFile: TUnitInfo;
  CurNode: TTreeNode;
  NodeText: String;
  NextNode: TTreeNode;
begin
  ItemsTreeView.BeginUpdate;
  if LazProject<>nil then begin
    CurFile:=LazProject.FirstPartOfProject;
    CurNode:=FilesNode.GetFirstChild;
    while CurFile<>nil do begin
      NodeText:=
        CreateRelativePath(CurFile.Filename,LazProject.ProjectDirectory);
      if CurNode=nil then
        CurNode:=ItemsTreeView.Items.AddChild(FilesNode,NodeText)
      else
        CurNode.Text:=NodeText;
      CurNode.ImageIndex:=GetImageIndexOfFile(CurFile);
      CurNode.SelectedIndex:=CurNode.ImageIndex;
      CurFile:=CurFile.NextPartOfProject;
      CurNode:=CurNode.GetNextSibling;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    FilesNode.Expanded:=true;
  end else begin
    // delete file nodes
    FilesNode.HasChildren:=false;
  end;
  ItemsTreeView.EndUpdate;
end;

procedure TProjectInspectorForm.UpdateRequiredPackages;
var
  Dependency: TPkgDependency;
  NodeText: String;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
begin
  ItemsTreeView.BeginUpdate;
  if LazProject<>nil then begin
    Dependency:=LazProject.FirstRequiredDependency;
    CurNode:=DependenciesNode.GetFirstChild;
    while Dependency<>nil do begin
      NodeText:=Dependency.AsString;
      if Dependency.DefaultFilename<>'' then
        NodeText:=NodeText+' in '
                  +Dependency.MakeFilenameRelativeToOwner(
                                                    Dependency.DefaultFilename);
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
  UpdateAll;
  EndUpdate;
end;

procedure TProjectInspectorForm.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

function TProjectInspectorForm.GetSelectedFile: TUnitInfo;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  if LazProject=nil then exit;
  CurNode:=ItemsTreeView.Selected;
  if (CurNode=nil) or (CurNode.Parent<>FilesNode) then exit;
  NodeIndex:=CurNode.Index;
  Result:=LazProject.FirstPartOfProject;
  while (NodeIndex>0) and (Result<>nil) do begin
    Result:=Result.NextPartOfProject;
    dec(NodeIndex);
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
    while ANode.Text<>CurText do ANode:=ANode.GetNextSibling;
    if ANode=nil then break;
    ASelection.Delete(0);
  end;
  if ANode<>nil then ItemsTreeView.Selected:=ANode;
  if FreeList then ASelection.Free;
end;

constructor TProjectInspectorForm.Create(TheOwner: TComponent);
var
  ALayout: TIDEWindowLayout;
begin
  inherited Create(TheOwner);
  Name:=NonModalIDEWindowNames[nmiwProjectInspector];
  Caption:=lisMenuProjectInspector;
  KeyPreview:=true;

  ALayout:=EnvironmentOptions.IDEWindowLayoutList.ItemByFormID(Name);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;

  SetupComponents;
  KeyPreview:=true;
end;

destructor TProjectInspectorForm.Destroy;
begin
  BeginUpdate;
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
  if FUpdateLock=0 then begin
    if pifTitleChanged in FFlags then UpdateTitle;
    if pifButtonsChanged in FFlags then UpdateButtons;
    if pifItemsChanged in FFlags then UpdateItems;
  end;
end;

function TProjectInspectorForm.IsUpdateLocked: boolean;
begin
  Result:=FUpdateLock>0;
end;

procedure TProjectInspectorForm.UpdateAll;
begin
  UpdateTitle;
  UpdateButtons;
  UpdateItems;
end;

procedure TProjectInspectorForm.UpdateTitle;
var
  NewCaption: String;
begin
  if (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifTitleChanged);
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

procedure TProjectInspectorForm.UpdateItems;
begin
  if (FUpdateLock>0) or (not Visible) then begin
    Include(FFlags,pifItemsChanged);
    exit;
  end;
  Exclude(FFlags,pifItemsChanged);
  ItemsTreeView.BeginUpdate;
  UpdateProjectItems;
  UpdateRequiredPackages;
  UpdateRemovedRequiredPackages;
  ItemsTreeView.EndUpdate;
end;


initialization
  {$I projectinspector.lrs}
  {$I projectinspector.lrs}

end.

