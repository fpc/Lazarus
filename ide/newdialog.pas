{  $Id$  }
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
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  IDEWindowIntf,
  LazarusIDEStrConsts,
  LCLProc,
  LResources,
  NewItemIntf,
  PackageIntf,
  ProjectIntf,
  StdCtrls,
  SysUtils;

type
  { TNewLazIDEItemCategory }

  TNewLazIDEItemCategory = class(TNewIDEItemCategory)
  private
    FItems: TList;
  protected
    function GetCount: integer; override;
    function GetItems(Index: integer): TNewIDEItemTemplate; override;
  public
    constructor Create;
    constructor Create(const AName: string); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(ATemplate: TNewIDEItemTemplate); override;
    function LocalizedName: string; override;
    function Description: string; override;
    function IndexOfCategory(const CategoryName: string): integer; override;
    function FindCategoryByName(const CategoryName: string
                                ): TNewIDEItemCategory; override;
  public
    property Count: integer Read GetCount;
    property Items[Index: integer]: TNewIDEItemTemplate Read GetItems; default;
    property Name: string Read FName;
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
    procedure Add(ACategoryName: string); override;
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
    DescriptionGroupBox: TGroupBox;
    DescriptionLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    ItemsTreeView: TTreeView;
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FNewItem: TNewIDEItemTemplate;
    procedure FillItemsTree;
    procedure SetupComponents;
    procedure UpdateDescription;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property NewItem: TNewIDEItemTemplate Read FNewItem;
  end;

function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;


implementation


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

procedure TNewOtherDialog.OkButtonClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := ItemsTreeView.Selected;
  if (ANode = nil) or (ANode.Data = nil) or
    (not (TObject(ANode.Data) is TNewIDEItemTemplate)) then
  begin
    MessageDlg(lisNewDlgNoItemSelected,
      lisNewDlgPleaseSelectAnItemFirst, mtInformation, [mbOK], 0);
    FNewItem := nil;
    exit;
  end;
  FNewItem    := TNewIDEItemTemplate(ANode.Data);
  ModalResult := mrOk;
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
    Category      := NewIDEItems[CategoryID];
    if not Category.VisibleInNewDialog then continue;
    NewParentNode := ItemsTreeView.Items.AddObject(nil, Category.Name, Category);
    for TemplateID := 0 to Category.Count - 1 do
    begin
      Template := Category[TemplateID];
      //DebugLn('TNewOtherDialog.FillItemsTree ',Template.Name,' ',dbgs(Template.VisibleInNewDialog));
      if Template.VisibleInNewDialog then
        ItemsTreeView.Items.AddChildObject(NewParentNode, Template.Name,
          Template);
    end;
    NewParentNode.Expand(True);
  end;
  ItemsTreeView.EndUpdate;
end;

procedure TNewOtherDialog.ItemsTreeViewSelectionChanged(Sender: TObject);
begin
  OkButton.Enabled := (ItemsTreeView.Selected <> nil) and
    (TObject(ItemsTreeView.Selected.Data) is TNewIDEItemTemplate);
  UpdateDescription;
end;

procedure TNewOtherDialog.SetupComponents;
begin
  DescriptionGroupBox.Caption := lisToDoLDescription;
  DescriptionLabel.Caption := '';
  OkButton.Caption := lisLazBuildOk;
  CancelButton.Caption := dlgCancel;
  DefaultControl := OkButton;
  CancelControl  := CancelButton;
end;

procedure TNewOtherDialog.UpdateDescription;
var
  Desc:  string;
  ANode: TTreeNode;
begin
  ANode := ItemsTreeView.Selected;
  if (ANode <> nil) and (ANode.Data <> nil) then
  begin
    if TObject(ANode.Data) is TNewLazIDEItemCategory then
      Desc := TNewLazIDEItemCategory(ANode.Data).Description
    else
      Desc := TNewIDEItemTemplate(ANode.Data).Description;
  end
  else
    Desc := '';
  DescriptionLabel.Caption := Desc;
end;

constructor TNewOtherDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := lisMenuNewOther;
  SetupComponents;
  FillItemsTree;
  IDEDialogLayoutList.ApplyLayout(Self, 400, 300);
end;

destructor TNewOtherDialog.Destroy;
begin
  inherited Destroy;
end;

{ TNewLazIDEItemCategory }

function TNewLazIDEItemCategory.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TNewLazIDEItemCategory.GetItems(Index: integer): TNewIDEItemTemplate;
begin
  Result := TNewIDEItemTemplate(FItems[Index]);
end;

constructor TNewLazIDEItemCategory.Create;
begin
  raise Exception.Create('TNewLazIDEItemCategory.Create: call Create(Name) instead');
end;

constructor TNewLazIDEItemCategory.Create(const AName: string);
begin
  inherited Create(AName);
  FItems := TList.Create;
  FName  := AName;
  //debugln('TNewLazIDEItemCategory.Create ',Name);
end;

destructor TNewLazIDEItemCategory.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TNewLazIDEItemCategory.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    Items[i].Free;
  FItems.Clear;
end;

procedure TNewLazIDEItemCategory.Add(ATemplate: TNewIDEItemTemplate);
begin
  //debugln('TNewLazIDEItemCategory.Add ',Name);
  FItems.Add(ATemplate);
  ATemplate.Category := Self;
end;

function TNewLazIDEItemCategory.LocalizedName: string;
begin
  // ToDo:
  Result := Name;
end;

function TNewLazIDEItemCategory.Description: string;
begin
  if Name = 'File' then
    Result := Format(lisNewDlgCreateANewEditorFileChooseAType, [#13])
  else if Name = 'Project' then
    Result := Format(lisNewDlgCreateANewProjectChooseAType, [#13])
  else
    Result := '';
end;

function TNewLazIDEItemCategory.IndexOfCategory(const CategoryName: string): integer;
begin
  // TODO
  Result := -1;
end;

function TNewLazIDEItemCategory.FindCategoryByName(
  const CategoryName: string): TNewIDEItemCategory;
var
  i: longint;
begin
  i := IndexOfCategory(CategoryName);
  if i >= 0 then
    Result := nil // TODO
  else
    Result := nil;
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

procedure TNewLazIDEItemCategories.Add(ACategoryName: string);
var
  I: integer;
begin
  I := IndexOf(ACategoryName);
  if I <> -1 then
  begin
    Items[I].Free;
    FItems.Delete(I);
  end;
  Add(TNewLazIDEItemCategoryFile.Create(ACategoryName));
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
  Result := lisToDoLFile;
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

initialization
  {$I newdialog.lrs}

end.
