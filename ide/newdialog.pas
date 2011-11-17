{
 /***************************************************************************
                            newdialog.pas
                            -------------


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
    TNewOtherDialog is the dialog, which is shown, when the user selects the
    File->New... menuitem and lets the user choose what to create.

}
unit NewDialog;

{$mode objfpc}{$H+}

interface

uses
  Buttons, SysUtils, Classes, LCLProc, ComCtrls, Controls, Dialogs,
  Forms, StdCtrls, ExtCtrls, FileProcs, ButtonPanel,
  IDEWindowIntf, IDEImagesIntf, NewItemIntf, PackageIntf, ProjectIntf,
  LazIDEIntf, IDEHelpIntf,
  InputHistory, LazarusIDEStrConsts, Project, MainIntf;

type
  { TNewLazIDEItemCategory }

  TNewLazIDEItemCategory = class(TNewIDEItemCategory)
  public
    function Description: string; override;
  end;


  { TNewLazIDEItemCategories }

  TNewLazIDEItemCategories = class(TNewIDEItemCategories)
  private
    FItems: TList;
  protected
    function GetItems(Index: integer): TNewIDEItemCategory; override;
    procedure SetItems(Index: integer; const AValue: TNewIDEItemCategory); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(ACategory: TNewIDEItemCategory); override;
    function Count: integer; override;
    function IndexOf(const CategoryName: string): integer; override;
    function FindByName(const CategoryName: string): TNewIDEItemCategory; override;
    procedure RegisterItem(const Paths: string; NewItem: TNewIDEItemTemplate); override;
    procedure UnregisterItem(NewItem: TNewIDEItemTemplate); override;
    function FindCategoryByPath(const Path: string;
      ErrorOnNotFound: boolean): TNewIDEItemCategory; override;
  end;


  //----------------------------------------------------------------------------
  // standard categories for new dialog

  { TNewLazIDEItemCategoryFile }

  TNewLazIDEItemCategoryFile = class(TNewLazIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;

  { TNewLazIDEItemCategoryInheritedItem }

  TNewLazIDEItemCategoryInheritedItem = class(TNewLazIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;
  
  { TNewLazIDEItemCategoryProject }

  TNewLazIDEItemCategoryProject = class(TNewLazIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;

  { TNewLazIDEItemCategoryPackage }

  TNewLazIDEItemCategoryPackage = class(TNewLazIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;
  
  //----------------------------------------------------------------------------


  { TNewOtherDialog }

  TNewOtherDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    DescriptionGroupBox: TGroupBox;
    DescriptionLabel: TLabel;
    ItemsTreeView: TTreeView;
    InheritableComponentsListView: TListView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure HelpButtonClick(Sender: TObject);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    ImageIndexFolder: integer;
    ImageIndexTemplate: integer;
    FNewItem: TNewIDEItemTemplate;
    procedure FillProjectInheritableItemsList;
    procedure FillItemsTree;
    procedure SetupComponents;
    procedure UpdateDescription;
    function FindItem(const aName: string): TTreeNode;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property NewItem: TNewIDEItemTemplate Read FNewItem;
  end;

function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;


implementation


{$R *.lfm}

function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;
var
  NewOtherDialog: TNewOtherDialog;
begin
  NewItem := nil;
  NewOtherDialog := TNewOtherDialog.Create(nil);
  Result  := NewOtherDialog.ShowModal;
  if Result = mrOk then
    NewItem := NewOtherDialog.NewItem;
  IDEDialogLayoutList.SaveLayout(NewOtherDialog);
  NewOtherDialog.Free;
end;

{ TNewOtherDialog }

procedure TNewOtherDialog.OKButtonClick(Sender: TObject);
var
  AInheritedNode: TListItem;
  ANode: TTreeNode;
  NewFile: TNewItemProjectFile;
  AncestorComponent: TComponent;
  AnUnitInfo: TUnitInfo;
  InhCompItem: TFileDescInheritedComponent;
begin

  ANode := ItemsTreeView.Selected;
  if (ANode = nil) or (ANode.Data = nil) or
    (not (TObject(ANode.Data) is TNewIDEItemTemplate)) then
  begin
    // don't show message, when double clicking in treeview
    if not (Sender is TTreeView) then
      MessageDlg(lisNewDlgNoItemSelected,
        lisNewDlgPleaseSelectAnItemFirst, mtInformation, [mbOK], 0);
    FNewItem := nil;
    ModalResult:=mrNone;
    exit;
  end;
  FNewItem := TNewIDEItemTemplate(ANode.Data);

  InputHistories.NewFileType:=FNewItem.Name;
  //debugln(['TNewOtherDialog.OKButtonClick InputHistories.NewFileType=',InputHistories.NewFileType]);

  // if the selected item is an inherited one
  if FNewItem is TNewItemProjectFile then
  begin
    //
    InputHistories.NewProjectType:=FNewItem.Name;

    NewFile:=TNewItemProjectFile(FNewItem);
    if (NewFile.Descriptor is TFileDescInheritedItem) then
    begin
      // If we are inheriting from a form
      if (NewFile.Descriptor is TFileDescInheritedComponent) then begin
        InhCompItem:=TFileDescInheritedComponent(NewFile.Descriptor);
        AInheritedNode := InheritableComponentsListView.Selected;
        if Assigned(AInheritedNode) then begin
          // load the ancestor component
          AnUnitInfo:=TUnitInfo(AInheritedNode.Data);
          if LazarusIDE.DoOpenComponent(AnUnitInfo.Filename,
            [ofOnlyIfExists,ofQuiet,ofLoadHiddenResource,ofUseCache],[],
            AncestorComponent)<>mrOk then
          begin
            MessageDlg(lisErrorOpeningComponent,
              lisUnableToOpenAncestorComponent, mtError, [mbCancel], 0);
            exit;
          end;
          // Set the resource class of the file descriptor
          InhCompItem.ResourceClass := TPersistentClass(AncestorComponent.ClassType);
          InhCompItem.InheritedUnit := AnUnitInfo;
          InhCompItem.DeclareClassVariable := not AncestorComponent.ClassType.InheritsFrom(TFrame);
          //DebugLn(['TNewOtherDialog.OKButtonClick ',InhCompItem.InheritedUnit.Filename,' ',dbgsname(InhCompItem.ResourceClass)]);
        end;
      end
      else
      begin
        MessageDlg(lisNewDlgNoItemSelected,
          lisNewDlgPleaseSelectAnItemFirst, mtInformation, [mbOK], 0);
        FNewItem := nil;
        Exit;
      end
    end;
  end;

  ModalResult := mrOk;
end;

// Fill the list of inheritable items in the project
procedure TNewOtherDialog.FillProjectInheritableItemsList;
var
  aComponentList: TStringList;
  i: integer;
  alistItem: TListItem;
  AnUnitInfo: TUnitInfo;
Begin
  try
    // Auxiliar stringlist to sort component list
    aComponentList := TStringList.Create;

    // Loop trough project units which have a component
    for i := 0 to Project1.UnitCount-1 do begin
      if (not Project1.Units[i].IsPartOfProject)
      or (not FilenameIsPascalUnit(Project1.Units[i].Filename)) then
        continue;

      if Project1.Units[i].ComponentName<>'' then
        aComponentList.AddObject(Project1.Units[i].ComponentName, Project1.Units[i]);
    end;

    // Sort lists (by component name)
    aComponentList.Sort;

    // Populate components listview, keeping references to each UnitInfo
    for i := 0 to aComponentList.Count-1 do
    begin
      alistItem := InheritableComponentsListView.Items.Add;
      alistItem.Caption := aComponentList[i];
      AnUnitInfo:=TUnitInfo(aComponentList.Objects[i]);
      alistItem.SubItems.Add(AnUnitInfo.ShortFilename);
      aListItem.Data := aComponentList.Objects[i];
    end;

  finally
    aComponentList.Free;
  end;
end;

procedure TNewOtherDialog.FillItemsTree;
var
  NewParentNode: TTreeNode;
  CategoryID:    integer;
  Category:      TNewIDEItemCategory;
  TemplateID:    integer;
  Template:      TNewIDEItemTemplate;
begin
  ItemsTreeView.BeginUpdate;
  ItemsTreeView.Items.Clear;
  for CategoryID := 0 to NewIDEItems.Count - 1 do
  begin
    Category := NewIDEItems[CategoryID];
    if not Category.VisibleInNewDialog then continue;
    NewParentNode := ItemsTreeView.Items.AddObject(nil,
                                              Category.LocalizedName, Category);
    
    NewParentNode.ImageIndex := ImageIndexFolder;
    NewParentNode.SelectedIndex := ImageIndexFolder;
    
    for TemplateID := 0 to Category.Count - 1 do
    begin
      Template := Category[TemplateID];
      //DebugLn('TNewOtherDialog.FillItemsTree ',Template.Name,' ',dbgs(Template.VisibleInNewDialog));
      if Template.VisibleInNewDialog then
        with ItemsTreeView.Items.AddChildObject(NewParentNode,
                                               Template.LocalizedName, Template)
        do begin
          ImageIndex := ImageIndexTemplate;
          SelectedIndex := ImageIndexTemplate;
        end;
    end;
    NewParentNode.Expand(True);
  end;
  ItemsTreeView.EndUpdate;
end;

procedure TNewOtherDialog.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  ButtonPanel.OKButton.Enabled := (ItemsTreeView.Selected <> nil) and
    (TObject(ItemsTreeView.Selected.Data) is TNewIDEItemTemplate);
  UpdateDescription;
end;

procedure TNewOtherDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TNewOtherDialog.SetupComponents;
begin
  ItemsTreeView.Images := IDEImages.Images_16;
  ImageIndexTemplate := IDEImages.LoadImage(16, 'template');
  ImageIndexFolder := IDEImages.LoadImage(16, 'folder');

  DescriptionGroupBox.Caption := lisCodeHelpDescrTag;
  DescriptionLabel.Caption := '';

  ButtonPanel.OKButton.Caption := lisOk;
  ButtonPanel.HelpButton.Caption := lisMenuHelp;
  ButtonPanel.CancelButton.Caption := dlgCancel;
end;

procedure TNewOtherDialog.UpdateDescription;
var
  Desc:  string;
  ANode: TTreeNode;
  aNewItemTemplate: TNewIDEItemTemplate;
begin
  ANode := ItemsTreeView.Selected;
  InheritableComponentsListView.Visible := false;
  if (ANode <> nil) and (ANode.Data <> nil) then
  begin
    if TObject(ANode.Data) is TNewLazIDEItemCategory then begin
      Desc := TNewLazIDEItemCategory(ANode.Data).Description;
    end else
    begin
      aNewItemTemplate := TNewIDEItemTemplate(ANode.Data);
      Desc := aNewItemTemplate.Description;
      if aNewItemTemplate is TNewItemProjectFile then
      begin
        if TNewItemProjectFile(aNewItemTemplate).Descriptor is TFileDescInheritedComponent
        then begin
          InheritableComponentsListView.Visible := true;
          InheritableComponentsListView.Height:=InheritableComponentsListView.Parent.ClientHeight-50;
          if InheritableComponentsListView.Items.Count>0 then
            InheritableComponentsListView.Selected := InheritableComponentsListView.Items[0];
        end
      end;
    end;
  end
  else begin
    Desc := '';
  end;
  DescriptionLabel.Caption := Desc;
end;

function TNewOtherDialog.FindItem(const aName: string): TTreeNode;
begin
  if aName='' then exit(nil);
  Result:=ItemsTreeView.Items.GetFirstNode;
  while Result<>nil do begin
    if (Result.Data<>nil)
    and (TObject(Result.Data) is TNewIDEItemTemplate)
    and (CompareText(TNewIDEItemTemplate(Result.Data).Name,aName)=0) then
      exit;
    Result:=Result.GetNext;
  end;
end;

constructor TNewOtherDialog.Create(TheOwner: TComponent);
var
  Node: TTreeNode;
begin
  inherited Create(TheOwner);
  Caption := lisMenuNewOther;
  SetupComponents;
  FillItemsTree;
  FillProjectInheritableItemsList;
  InheritableComponentsListView.Visible := false;
  IDEDialogLayoutList.ApplyLayout(Self, 570, 400);

  Node:=FindItem(InputHistories.NewFileType);
  if Node=nil then
    Node:=FindItem(InputHistories.NewProjectType);
  if Node<>nil then
    ItemsTreeView.Selected:=Node;
end;

destructor TNewOtherDialog.Destroy;
begin
  inherited Destroy;
end;

{ TNewLazIDEItemCategory }

function TNewLazIDEItemCategory.Description: string;
begin
  if Name = 'File' then
    Result := Format(lisNewDlgCreateANewEditorFileChooseAType, [#13])
  else if Name = 'Project' then
    Result := Format(lisNewDlgCreateANewProjectChooseAType, [#13])
  else
    Result := '';
end;

{ TNewLazIDEItemCategories }

function TNewLazIDEItemCategories.GetItems(Index: integer): TNewIDEItemCategory;
begin
  Result := TNewIDEItemCategory(FItems[Index]);
end;

procedure TNewLazIDEItemCategories.SetItems(Index: integer;
  const AValue: TNewIDEItemCategory);
begin
  FItems[Index] := AValue;
end;

constructor TNewLazIDEItemCategories.Create;
begin
  FItems := TList.Create;
end;

destructor TNewLazIDEItemCategories.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TNewLazIDEItemCategories.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    Items[i].Free;
  FItems.Clear;
end;

procedure TNewLazIDEItemCategories.Add(ACategory: TNewIDEItemCategory);
begin
  FItems.Add(ACategory);
end;

function TNewLazIDEItemCategories.Count: integer;
begin
  Result := FItems.Count;
end;

function TNewLazIDEItemCategories.IndexOf(const CategoryName: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (AnsiCompareText(CategoryName, Items[Result].Name) <> 0) do
    Dec(Result);
end;

function TNewLazIDEItemCategories.FindByName(
  const CategoryName: string): TNewIDEItemCategory;
var
  i: longint;
begin
  i := IndexOf(CategoryName);
  if i >= 0 then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TNewLazIDEItemCategories.RegisterItem(const Paths: string;
  NewItem: TNewIDEItemTemplate);

  procedure AddToPath(const Path: string);
  var
    CurCategory: TNewIDEItemCategory;
  begin
    CurCategory := FindCategoryByPath(Path, True);
    CurCategory.Add(NewItem);
  end;

var
  StartPos: integer;
  EndPos:   integer;
  Path:     string;
begin
  // go through all paths
  EndPos := 1;
  while EndPos <= length(Paths) do
  begin
    StartPos := EndPos;
    while (StartPos <= length(Paths)) and (Paths[StartPos] = ';') do
      Inc(StartPos);
    EndPos := StartPos;
    while (EndPos <= length(Paths)) and (Paths[EndPos] <> ';') do
      Inc(EndPos);
    if EndPos > StartPos then
    begin
      Path := copy(Paths, StartPos, EndPos - StartPos);
      AddToPath(Path);
    end;
  end;
end;

procedure TNewLazIDEItemCategories.UnregisterItem(NewItem: TNewIDEItemTemplate);
begin
  raise Exception.Create('TODO TNewLazIDEItemCategories.UnregisterItem');
end;

function TNewLazIDEItemCategories.FindCategoryByPath(const Path: string;
  ErrorOnNotFound: boolean): TNewIDEItemCategory;
var
  StartPos: integer;
  EndPos:   integer;
  CategoryName: string;
begin
  Result := nil;
  EndPos := 1;
  while EndPos <= length(Path) do
  begin
    StartPos := EndPos;
    while (StartPos <= length(Path)) and (Path[StartPos] = '/') do
      Inc(StartPos);
    EndPos := StartPos;
    while (EndPos <= length(Path)) and (Path[EndPos] <> '/') do
      Inc(EndPos);
    if EndPos > StartPos then
    begin
      CategoryName := copy(Path, StartPos, EndPos - StartPos);
      if Result = nil then
        Result := FindByName(CategoryName)
      else
        Result := Result.FindCategoryByName(CategoryName);
      if (Result = nil) then
        if ErrorOnNotFound then
          raise Exception.Create(
            'Unknown category: ' + CategoryName + ' in Path ' + Path)
        else
          exit;
    end;
  end;
end;

{ TNewLazIDEItemCategoryFile }

function TNewLazIDEItemCategoryFile.LocalizedName: string;
begin
  Result := lisDebugOptionsFrmModule;
end;

function TNewLazIDEItemCategoryFile.Description: string;
begin
  Result := lisChooseOneOfTheseItemsToCreateANewFile;
end;

{ TNewLazIDEItemCategoryProject }

function TNewLazIDEItemCategoryProject.LocalizedName: string;
begin
  Result := dlgEnvProject;
end;

function TNewLazIDEItemCategoryProject.Description: string;
begin
  Result := lisChooseOneOfTheseItemsToCreateANewProject;
end;

{ TNewLazIDEItemCategoryPackage }

function TNewLazIDEItemCategoryPackage.LocalizedName: string;
begin
  Result := lisPackage;
end;

function TNewLazIDEItemCategoryPackage.Description: string;
begin
  Result := lisChooseOneOfTheseItemsToCreateANewPackage;
end;


{ TNewLazIDEItemCategoryInheritedItem }

function TNewLazIDEItemCategoryInheritedItem.LocalizedName: string;
begin
  Result := lisInheritedItem;
end;

function TNewLazIDEItemCategoryInheritedItem.Description: string;
begin
  Result := lisChooseOneOfTheseItemsToInheritFromAnExistingOne;
end;

end.
