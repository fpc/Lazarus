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
  Dialogs, LResources, ProjectIntf, PackageIntf,
  IDEOptionDefs, LazarusIDEStrConsts;
  
type
  // Items that can be created in the IDE:
  {TNewIDEItemType = (
    niiNone,
    niiCustom,     // for experts (IDE plugins)
    
    niiUnit,       // pascal unit
    niiForm,       // pascal unit with lcl form
    niiDataModule, // pascal nuit with datamodule
    niiText,       // text file
    
    niiApplication,// Project: Application
    niiFPCProject, // Project: with hidden main file
    niiCustomProject,// Project: pascal program without any specials
    
    niiPackage     // standard package
  );
  TNewIDEItemTypes = set of TNewIDEItemType;}

  // Flags/Options for the items
  TNewIDEItemFlag = (
    niifCopy,
    niifInherited,
    niifUse
    );
  TNewIDEItemFlags = set of TNewIDEItemFlag;

  TNewIDEItemTemplate = class;


  { TNewIDEItemCategory }
  
  TNewIDEItemCategory = class
  private
    FItems: TList;
    FName: string;
    function GetCount: integer;
    function GetItems(Index: integer): TNewIDEItemTemplate;
  public
    constructor Create;
    constructor Create(const AName: string);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ATemplate: TNewIDEItemTemplate);
    function LocalizedName: string; virtual;
    function Description: string; virtual;
    function IndexOfCategory(const CategoryName: string): integer;
    function FindCategoryByName(const CategoryName: string): TNewIDEItemCategory;
  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TNewIDEItemTemplate read GetItems; default;
    property Name: string read FName;
  end;
  

  { TNewIDEItemCategories }
  
  TNewIDEItemCategories = class
  private
    FItems: TList;
    function GetItems(Index: integer): TNewIDEItemCategory;
    procedure SetItems(Index: integer; const AValue: TNewIDEItemCategory);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ACategory: TNewIDEItemCategory);
    function Count: integer;
    function IndexOf(const CategoryName: string): integer;
    function FindByName(const CategoryName: string): TNewIDEItemCategory;
    procedure RegisterItem(const Paths: string; NewItem: TNewIDEItemTemplate);
    procedure UnregisterItem(NewItem: TNewIDEItemTemplate);
    function FindCategoryByPath(const Path: string;
                                 ErrorOnNotFound: boolean): TNewIDEItemCategory;
  public
    property Items[Index: integer]: TNewIDEItemCategory
                                          read GetItems write SetItems; default;
  end;


  { TNewIDEItemTemplate }
  
  TNewIDEItemTemplate = class(TPersistent)
  private
    FAllowedFlags: TNewIDEItemFlags;
    FDefaultFlag: TNewIDEItemFlag;
    FName: string;
    fCategory: TNewIDEItemCategory;
  public
    constructor Create(const AName: string; ADefaultFlag: TNewIDEItemFlag;
                       TheAllowedFlags: TNewIDEItemFlags);
    function LocalizedName: string; virtual;
    function Description: string; virtual;
    function CreateCopy: TNewIDEItemTemplate; virtual;
    procedure Assign(Source: TPersistent); override;
  public
    property DefaultFlag: TNewIDEItemFlag read FDefaultFlag;
    property AllowedFlags: TNewIDEItemFlags read FAllowedFlags;
    property Name: string read FName;
    property Category: TNewIDEItemCategory read fCategory; // main category
  end;
  TNewIDEItemTemplateClass = class of TNewIDEItemTemplate;
  
  //----------------------------------------------------------------------------
  // standard categories for new dialog

  { TNewIDEItemCategoryFile }

  TNewIDEItemCategoryFile = class(TNewIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;

  { TNewIDEItemCategoryProject }

  TNewIDEItemCategoryProject = class(TNewIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;

  { TNewIDEItemCategoryPackage }

  TNewIDEItemCategoryPackage = class(TNewIDEItemCategory)
  public
    function LocalizedName: string; override;
    function Description: string; override;
  end;

  //----------------------------------------------------------------------------
  // standard items for new dialog
  
  { TNewItemProjectFile - a new item for project file descriptors }
  
  TNewItemProjectFile = class(TNewIDEItemTemplate)
  private
    FDescriptor: TProjectFileDescriptor;
  public
    function LocalizedName: string; override;
    function Description: string; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Descriptor: TProjectFileDescriptor read FDescriptor write FDescriptor;
  end;
  
  { TNewItemProject - a new item for project descriptors }

  TNewItemProject = class(TNewIDEItemTemplate)
  private
    FDescriptor: TProjectDescriptor;
  public
    function LocalizedName: string; override;
    function Description: string; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Descriptor: TProjectDescriptor read FDescriptor write FDescriptor;
  end;

  { TNewItemPackage - a new item for package descriptors }

  TNewItemPackage = class(TNewIDEItemTemplate)
  private
    FDescriptor: TPackageDescriptor;
  public
    function LocalizedName: string; override;
    function Description: string; override;
    procedure Assign(Source: TPersistent); override;
  public
    property Descriptor: TPackageDescriptor read FDescriptor write FDescriptor;
  end;

  //----------------------------------------------------------------------------


  { TNewOtherDialog }

  TNewOtherDialog = class(TForm)
    ItemsTreeView: TTreeView;
    DescriptionGroupBox: TGroupBox;
    DescriptionLabel: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure ItemsTreeViewClick(Sender: TObject);
    procedure NewOtherDialogResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    procedure FillItemsTree;
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    function GetNewItem: TNewIDEItemTemplate;
  end;
  
function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;

var
  NewIDEItems: TNewIDEItemCategories;// will be set by the IDE


procedure RegisterNewDialogItem(const Paths: string;
  NewItem: TNewIDEItemTemplate);
procedure UnregisterNewDialogItem(NewItem: TNewIDEItemTemplate);

implementation


function ShowNewIDEItemDialog(var NewItem: TNewIDEItemTemplate): TModalResult;
var
  NewOtherDialog: TNewOtherDialog;
begin
  NewItem:=nil;
  NewOtherDialog:=TNewOtherDialog.Create(Application);
  Result:=NewOtherDialog.ShowModal;
  if Result=mrOk then begin
    NewItem:=NewOtherDialog.GetNewItem;
  end;
  IDEDialogLayoutList.SaveLayout(NewOtherDialog);
  NewOtherDialog.Free;
end;

procedure RegisterNewDialogItem(const Paths: string;
  NewItem: TNewIDEItemTemplate);
begin
  if NewIDEItems=nil then
    raise Exception.Create('RegisterNewDialogItem NewIDEItems=nil');
  NewIDEItems.RegisterItem(Paths,NewItem);
end;

procedure UnregisterNewDialogItem(NewItem: TNewIDEItemTemplate);
begin
  if NewIDEItems=nil then
    raise Exception.Create('RegisterNewDialogItem NewIDEItems=nil');
  NewIDEItems.UnregisterItem(NewItem);
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
    exit;
  end;

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

procedure TNewOtherDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TNewOtherDialog.ItemsTreeViewClick(Sender: TObject);
var
  Desc: String;
  ANode: TTreeNode;
begin
  ANode:=ItemsTreeView.Selected;
  if (ANode<>nil) and (ANode.Data<>nil) then begin
    if TObject(ANode.Data) is TNewIDEItemCategory then
      Desc:=TNewIDEItemCategory(ANode.Data).Description
    else
      Desc:=TNewIDEItemTemplate(ANode.Data).Description;
  end else begin
    Desc:='';
  end;
  DescriptionLabel.Caption:=Desc;
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
    OnClick:=@CancelButtonClick;
  end;
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

function TNewOtherDialog.GetNewItem: TNewIDEItemTemplate;
var
  ANode: TTreeNode;
begin
  ANode:=ItemsTreeView.Selected;
  if (ANode=nil) or (ANode.Data=nil)
  or (not (TObject(ANode.Data) is TNewIDEItemTemplate))
  then exit;
  Result:=TNewIDEItemTemplate(ANode.Data).CreateCopy;
end;

{ TNewIDEItemCategory }

function TNewIDEItemCategory.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TNewIDEItemCategory.GetItems(Index: integer): TNewIDEItemTemplate;
begin
  Result:=TNewIDEItemTemplate(FItems[Index]);
end;

constructor TNewIDEItemCategory.Create;
begin
  raise Exception.Create('TNewIDEItemCategory.Create: call Create(Name) instead');
end;

constructor TNewIDEItemCategory.Create(const AName: string);
begin
  FItems:=TList.Create;
  FName:=AName;
  //debugln('TNewIDEItemCategory.Create ',Name);
end;

destructor TNewIDEItemCategory.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TNewIDEItemCategory.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TNewIDEItemCategory.Add(ATemplate: TNewIDEItemTemplate);
begin
  //debugln('TNewIDEItemCategory.Add ',Name);
  FItems.Add(ATemplate);
  ATemplate.fCategory:=Self;
end;

function TNewIDEItemCategory.LocalizedName: string;
begin
  // ToDo:
  Result:=Name;
end;

function TNewIDEItemCategory.Description: string;
begin
  if Name='File' then begin
    Result:=Format(lisNewDlgCreateANewEditorFileChooseAType, [#13]);
  end else if Name='Project' then begin
    Result:=Format(lisNewDlgCreateANewProjectChooseAType, [#13]);
  end else
    Result:='';
end;

function TNewIDEItemCategory.IndexOfCategory(const CategoryName: string
  ): integer;
begin
  // TODO
  Result:=-1;
end;

function TNewIDEItemCategory.FindCategoryByName(const CategoryName: string
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

{ TNewIDEItemTemplate }

constructor TNewIDEItemTemplate.Create(const AName: string;
  ADefaultFlag: TNewIDEItemFlag; TheAllowedFlags: TNewIDEItemFlags);
begin
  FName:=AName;
  FDefaultFlag:=ADefaultFlag;
  FAllowedFlags:=TheAllowedFlags;
  Include(FAllowedFlags,FDefaultFlag);
end;

function TNewIDEItemTemplate.LocalizedName: string;
begin
  Result:=Name;
end;

function TNewIDEItemTemplate.Description: string;
begin
  Result:='<Description not set>';
end;

function TNewIDEItemTemplate.CreateCopy: TNewIDEItemTemplate;
begin
  Result:=TNewIDEItemTemplateClass(ClassType).Create(
                                                 Name,DefaultFlag,AllowedFlags);
  Result.Assign(Self);
end;

procedure TNewIDEItemTemplate.Assign(Source: TPersistent);
var
  Src: TNewIDEItemTemplate;
begin
  if Source is TNewIDEItemTemplate then begin
    Src:=TNewIDEItemTemplate(Source);
    FName:=Src.Name;
    FDefaultFlag:=Src.DefaultFlag;
    FAllowedFlags:=Src.AllowedFlags;
  end else
    inherited Assign(Source);
end;

{ TNewIDEItemCategories }

function TNewIDEItemCategories.GetItems(Index: integer): TNewIDEItemCategory;
begin
  Result:=TNewIDEItemCategory(FItems[Index]);
end;

procedure TNewIDEItemCategories.SetItems(Index: integer;
  const AValue: TNewIDEItemCategory);
begin
  FItems[Index]:=AValue;
end;

constructor TNewIDEItemCategories.Create;
begin
  FItems:=TList.Create;
end;

destructor TNewIDEItemCategories.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TNewIDEItemCategories.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TNewIDEItemCategories.Add(ACategory: TNewIDEItemCategory);
begin
  FItems.Add(ACategory);
end;

function TNewIDEItemCategories.Count: integer;
begin
  Result:=FItems.Count;
end;

function TNewIDEItemCategories.IndexOf(const CategoryName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(CategoryName,Items[Result].Name)<>0) do
    dec(Result);
end;

function TNewIDEItemCategories.FindByName(const CategoryName: string
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

procedure TNewIDEItemCategories.RegisterItem(const Paths: string;
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

procedure TNewIDEItemCategories.UnregisterItem(NewItem: TNewIDEItemTemplate);
begin
  raise Exception.Create('TODO TNewIDEItemCategories.UnregisterItem');
end;

function TNewIDEItemCategories.FindCategoryByPath(const Path: string;
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

{ TNewItemProjectFile }

function TNewItemProjectFile.LocalizedName: string;
begin
  Result:=Descriptor.GetLocalizedName;
end;

function TNewItemProjectFile.Description: string;
begin
  Result:=Descriptor.GetLocalizedDescription;
end;

procedure TNewItemProjectFile.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNewItemProjectFile then
    FDescriptor:=TNewItemProjectFile(Source).Descriptor;
end;

{ TNewItemProject }

function TNewItemProject.LocalizedName: string;
begin
  Result:=Descriptor.GetLocalizedName;
end;

function TNewItemProject.Description: string;
begin
  Result:=Descriptor.GetLocalizedDescription;
end;

procedure TNewItemProject.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNewItemProject then
    FDescriptor:=TNewItemProject(Source).Descriptor;
end;

{ TNewItemPackage }

function TNewItemPackage.LocalizedName: string;
begin
  Result:=Descriptor.GetLocalizedName;
end;

function TNewItemPackage.Description: string;
begin
  Result:=Descriptor.GetLocalizedDescription;
end;

procedure TNewItemPackage.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TNewItemPackage then
    FDescriptor:=TNewItemPackage(Source).Descriptor;
end;

{ TNewIDEItemCategoryFile }

function TNewIDEItemCategoryFile.LocalizedName: string;
begin
  Result:='File';
end;

function TNewIDEItemCategoryFile.Description: string;
begin
  Result:='Choose one of these items to create a new File';
end;

{ TNewIDEItemCategoryProject }

function TNewIDEItemCategoryProject.LocalizedName: string;
begin
  Result:='Project';
end;

function TNewIDEItemCategoryProject.Description: string;
begin
  Result:='Choose one of these items to create a new Project';
end;

{ TNewIDEItemCategoryPackage }

function TNewIDEItemCategoryPackage.LocalizedName: string;
begin
  Result:='Package';
end;

function TNewIDEItemCategoryPackage.Description: string;
begin
  Result:='Choose one of these items to create a new Package';
end;

end.

