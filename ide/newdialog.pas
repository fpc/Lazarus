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
  Classes, SysUtils, LCLProc, Forms, Controls, StdCtrls, Buttons, ComCtrls,
  Dialogs, LResources, ProjectIntf, PackageIntf, NewItemIntf,
  IDEOptionDefs, LazarusIDEStrConsts;
  
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
    function LocalizedName: string;  override;
    function Description: string;  override;
    function IndexOfCategory(const CategoryName: string): integer; override;
    function FindCategoryByName(const CategoryName: string): TNewIDEItemCategory; override;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TNewIDEItemTemplate read GetItems; default;
    property Name: string read FName;
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
    ItemsTreeView: TTreeView;
    DescriptionGroupBox: TGroupBox;
    DescriptionLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure ItemsTreeViewClick(Sender: TObject);
    procedure ItemsTreeViewDblClick(Sender: TObject);
    procedure NewOtherDialogResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FNewItem: TNewIDEItemTemplate;
    procedure FillItemsTree;
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property NewItem: TNewIDEItemTemplate read FNewItem;
  end;
  
function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;


implementation


function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;
var
  NewOtherDialog: TNewOtherDialog;
begin
  NewItem:=nil;
  NewOtherDialog:=TNewOtherDialog.Create(nil);
  Result:=NewOtherDialog.ShowModal;
  if Result=mrOk then begin
    NewItem:=NewOtherDialog.NewItem;
  end;
  IDEDialogLayoutList.SaveLayout(NewOtherDialog);
  NewOtherDialog.Free;
end;

{ TNewOtherDialog }

procedure TNewOtherDialog.NewOtherDialogResize(Sender: TObject);
var
  NewLeft: Integer;
begin
  with ItemsTreeView do begin
    SetBounds(5,5,(Parent.ClientWidth-2*Left) div 2,Parent.ClientHeight-Top-45);
  end;

  with DescriptionGroupBox do begin
    NewLeft:=ItemsTreeView.Left+ItemsTreeView.Width+5;
    SetBounds(NewLeft,ItemsTreeView.Top,
              (Parent.ClientWidth-NewLeft-5),ItemsTreeView.Height);
  end;

  with OkButton do begin
    SetBounds(Parent.ClientWidth-200,Parent.ClientHeight-35,75,25);
  end;

  with CancelButton do begin
    SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,
              OkButton.Width,OkButton.Height);
  end;
end;

procedure TNewOtherDialog.OkButtonClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode:=ItemsTreeView.Selected;
  if (ANode=nil) or (ANode.Data=nil)
  or (not (TObject(ANode.Data) is TNewIDEItemTemplate))
  then begin
    MessageDlg(lisNewDlgNoItemSelected,
      lisNewDlgPleaseSelectAnItemFirst, mtInformation, [mbOk], 0);
    FNewItem:=nil;
    exit;
  end;
  FNewItem:=TNewIDEItemTemplate(ANode.Data);
  ModalResult:=mrOk;
end;

procedure TNewOtherDialog.FillItemsTree;
var
  NewParentNode: TTreeNode;
  CategoryID: Integer;
  Category: TNewIDEItemCategory;
  TemplateID: Integer;
  Template: TNewIDEItemTemplate;
begin
  ItemsTreeView.BeginUpdate;
  ItemsTreeView.Items.Clear;
  for CategoryID:=0 to NewIDEItems.Count-1 do begin
    Category:=NewIDEItems[CategoryID];
    NewParentNode:=ItemsTreeView.Items.AddObject(nil,Category.Name,Category);
    for TemplateID:=0 to Category.Count-1 do begin
      Template:=Category[TemplateID];
      ItemsTreeView.Items.AddChildObject(NewParentNode,Template.Name,Template);
    end;
    NewParentNode.Expand(true);
  end;
  ItemsTreeView.EndUpdate;
end;

procedure TNewOtherDialog.ItemsTreeViewClick(Sender: TObject);
var
  Desc: String;
  ANode: TTreeNode;
begin
  ANode:=ItemsTreeView.Selected;
  if (ANode<>nil) and (ANode.Data<>nil) then begin
    if TObject(ANode.Data) is TNewLazIDEItemCategory then
      Desc:=TNewLazIDEItemCategory(ANode.Data).Description
    else
      Desc:=TNewIDEItemTemplate(ANode.Data).Description;
  end else begin
    Desc:='';
  end;
  DescriptionLabel.Caption:=Desc;
end;

procedure TNewOtherDialog.ItemsTreeViewDblClick(Sender: TObject);
begin
  OkButton.Click;
end;

procedure TNewOtherDialog.SetupComponents;
begin
  ItemsTreeView:=TTreeView.Create(Self);
  with ItemsTreeView do begin
    Name:='ItemsTreeView';
    Parent:=Self;
    Left:=5;
    Top:=5;
    OnClick:=@ItemsTreeViewClick;
    OnDblClick:=@ItemsTreeViewDblClick;
  end;
  
  DescriptionGroupBox:=TGroupBox.Create(Self);
  with DescriptionGroupBox do begin
    Name:='DescriptionGroupBox';
    Parent:=Self;
    Left:=5;
    Top:=5;
    Caption:=lisToDoLDescription;
  end;
  
  DescriptionLabel:=TLabel.Create(Self);
  with DescriptionLabel do begin
    Name:='DescriptionLabel';
    Parent:=DescriptionGroupBox;
    Align:=alClient;
    Caption:='';
    WordWrap:=true;
  end;
  
  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Left:=100;
    Top:=100;
    Caption:=lisLazBuildOk;
    OnClick:=@OkButtonClick;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Left:=150;
    Top:=100;
    Caption:=dlgCancel;
    ModalResult := mrCancel;
  end;
  CancelControl:=CancelButton;
end;

constructor TNewOtherDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(Classname)=nil then begin
    Name:='NewOtherDialog';
    Caption := lisMenuNewOther;
    Width:=400;
    Height:=300;
    Position:=poScreenCenter;
    OnResize:=@NewOtherDialogResize;
    SetupComponents;
    FillItemsTree;
  end;
  IDEDialogLayoutList.ApplyLayout(Self,400,300);
  OnResize(nil);
end;

destructor TNewOtherDialog.Destroy;
begin
  inherited Destroy;
end;

{ TNewLazIDEItemCategory }

function TNewLazIDEItemCategory.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TNewLazIDEItemCategory.GetItems(Index: integer): TNewIDEItemTemplate;
begin
  Result:=TNewIDEItemTemplate(FItems[Index]);
end;

constructor TNewLazIDEItemCategory.Create;
begin
  raise Exception.Create('TNewLazIDEItemCategory.Create: call Create(Name) instead');
end;

constructor TNewLazIDEItemCategory.Create(const AName: string);
begin
  FItems:=TList.Create;
  FName:=AName;
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
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TNewLazIDEItemCategory.Add(ATemplate: TNewIDEItemTemplate);
begin
  //debugln('TNewLazIDEItemCategory.Add ',Name);
  FItems.Add(ATemplate);
  ATemplate.Category:=Self;
end;

function TNewLazIDEItemCategory.LocalizedName: string;
begin
  // ToDo:
  Result:=Name;
end;

function TNewLazIDEItemCategory.Description: string;
begin
  if Name='File' then begin
    Result:=Format(lisNewDlgCreateANewEditorFileChooseAType, [#13]);
  end else if Name='Project' then begin
    Result:=Format(lisNewDlgCreateANewProjectChooseAType, [#13]);
  end else
    Result:='';
end;

function TNewLazIDEItemCategory.IndexOfCategory(const CategoryName: string
  ): integer;
begin
  // TODO
  Result:=-1;
end;

function TNewLazIDEItemCategory.FindCategoryByName(const CategoryName: string
  ): TNewIDEItemCategory;
var
  i: LongInt;
begin
  i:=IndexOfCategory(CategoryName);
  if i>=0 then
    Result:=nil // TODO
  else
    Result:=nil;
end;

{ TNewLazIDEItemCategories }

function TNewLazIDEItemCategories.GetItems(Index: integer): TNewIDEItemCategory;
begin
  Result:=TNewIDEItemCategory(FItems[Index]);
end;

procedure TNewLazIDEItemCategories.SetItems(Index: integer;
  const AValue: TNewIDEItemCategory);
begin
  FItems[Index]:=AValue;
end;

constructor TNewLazIDEItemCategories.Create;
begin
  FItems:=TList.Create;
end;

destructor TNewLazIDEItemCategories.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TNewLazIDEItemCategories.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TNewLazIDEItemCategories.Add(ACategory: TNewIDEItemCategory);
begin
  FItems.Add(ACategory);
end;

function TNewLazIDEItemCategories.Count: integer;
begin
  Result:=FItems.Count;
end;

function TNewLazIDEItemCategories.IndexOf(const CategoryName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(CategoryName,Items[Result].Name)<>0) do
    dec(Result);
end;

function TNewLazIDEItemCategories.FindByName(const CategoryName: string
  ): TNewIDEItemCategory;
var
  i: LongInt;
begin
  i:=IndexOf(CategoryName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TNewLazIDEItemCategories.RegisterItem(const Paths: string;
  NewItem: TNewIDEItemTemplate);
  
  procedure AddToPath(const Path: string);
  var
    CurCategory: TNewIDEItemCategory;
  begin
    CurCategory:=FindCategoryByPath(Path,true);
    CurCategory.Add(NewItem);
  end;
  
var
  StartPos: Integer;
  EndPos: Integer;
  Path: String;
begin
  // go through all paths
  EndPos:=1;
  while EndPos<=length(Paths) do begin
    StartPos:=EndPos;
    while (StartPos<=length(Paths)) and (Paths[StartPos]=';') do
      inc(StartPos);
    EndPos:=StartPos;
    while (EndPos<=length(Paths)) and (Paths[EndPos]<>';') do
      inc(EndPos);
    if EndPos>StartPos then begin
      Path:=copy(Paths,StartPos,EndPos-StartPos);
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
  StartPos: Integer;
  EndPos: Integer;
  CategoryName: String;
begin
  Result:=nil;
  EndPos:=1;
  while EndPos<=length(Path) do begin
    StartPos:=EndPos;
    while (StartPos<=length(Path)) and (Path[StartPos]='/') do
      inc(StartPos);
    EndPos:=StartPos;
    while (EndPos<=length(Path)) and (Path[EndPos]<>'/') do
      inc(EndPos);
    if EndPos>StartPos then begin
      CategoryName:=copy(Path,StartPos,EndPos-StartPos);
      if Result=nil then
        Result:=FindByName(CategoryName)
      else
        Result:=Result.FindCategoryByName(CategoryName);
      if (Result=nil) then begin
        if ErrorOnNotFound then
          raise Exception.Create(
            'Unknown category: '+CategoryName+' in Path '+Path)
        else
         exit;
      end;
    end;
  end;
end;

{ TNewLazIDEItemCategoryFile }

function TNewLazIDEItemCategoryFile.LocalizedName: string;
begin
  Result:='File';
end;

function TNewLazIDEItemCategoryFile.Description: string;
begin
  Result:='Choose one of these items to create a new File';
end;

{ TNewLazIDEItemCategoryProject }

function TNewLazIDEItemCategoryProject.LocalizedName: string;
begin
  Result:='Project';
end;

function TNewLazIDEItemCategoryProject.Description: string;
begin
  Result:='Choose one of these items to create a new Project';
end;

{ TNewLazIDEItemCategoryPackage }

function TNewLazIDEItemCategoryPackage.LocalizedName: string;
begin
  Result:='Package';
end;

function TNewLazIDEItemCategoryPackage.Description: string;
begin
  Result:='Choose one of these items to create a new Package';
end;

end.

