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
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, ComCtrls,
  Dialogs, LResources, IDEOptionDefs, LazarusIDEStrConsts;
  
type
  // Items that can be created in the IDE:
  TNewIDEItemType = (
    niiNone,
    niiCustom,     // for experts (IDE plugins)
    niiUnit,       // pascal unit
    niiForm,       // pascal unit with lcl form
    niiDataModule, // pascal nuit with datamodule
    niiText,       // text file
    niiApplication,// Project: Application
    niiFPCProject, // Project: with hidden main file
    niiCustomProject,// Project: pascal program without any specials
    niiCGIApplication,// Project: TCGIApplication using package cgilaz
    niiPackage     // standard package
  );
  TNewIDEItemTypes = set of TNewIDEItemType;

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
    constructor Create(const AName: string);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ATemplate: TNewIDEItemTemplate);
    function LocalizedName: string;
    function Description: string;
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
    constructor CreateWithDefaults;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ACategory: TNewIDEItemCategory);
    function Count: integer;
  public
    property Items[Index: integer]: TNewIDEItemCategory
                                          read GetItems write SetItems; default;
  end;


  { TNewIDEItemTemplate }
  
  TNewIDEItemTemplate = class
  private
    FAllowedFlags: TNewIDEItemFlags;
    FDefaultFlag: TNewIDEItemFlag;
    FName: string;
    fCategory: TNewIDEItemCategory;
    FTheType: TNewIDEItemType;
  public
    constructor Create(AType: TNewIDEItemType; const AName: string;
                       ADefaultFlag: TNewIDEItemFlag;
                       TheAllowedFlags: TNewIDEItemFlags);
    function LocalizedName: string;
    function Description: string;
  public
    property TheType: TNewIDEItemType read FTheType;
    property DefaultFlag: TNewIDEItemFlag read FDefaultFlag;
    property AllowedFlags: TNewIDEItemFlags read FAllowedFlags;
    property Name: string read FName;
    property Category: TNewIDEItemCategory read fCategory;
  end;
  
  
  { TNewIDEItem }

  TNewIDEItem = class
  private
    FFlag: TNewIDEItemFlag;
    FTheType: TNewIDEItemType;
    procedure SetFlag(const AValue: TNewIDEItemFlag);
    procedure SetTheType(const AValue: TNewIDEItemType);
  public
    constructor Create;
    procedure Assign(Source: TNewIDEItem);
    procedure Assign(Source: TNewIDEItemTemplate);
    function CreateCopy: TNewIDEItem;
  public
    property TheType: TNewIDEItemType read FTheType write SetTheType;
    property Flag: TNewIDEItemFlag read FFlag write SetFlag;
  end;
  
  
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
    FItemType: TNewIDEItem;
    procedure FillItemsTree;
    procedure SetupComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property ItemType: TNewIDEItem read FItemType;
  end;
  
function ShowNewIDEItemDialog(var ItemType: TNewIDEItem): TModalResult;
function NewIDEItems: TNewIDEItemCategories;

implementation

var
  InternalNewIDEItems: TNewIDEItemCategories;

function ShowNewIDEItemDialog(var ItemType: TNewIDEItem): TModalResult;
var
  NewOtherDialog: TNewOtherDialog;
begin
  ItemType:=nil;
  NewOtherDialog:=TNewOtherDialog.Create(Application);
  Result:=NewOtherDialog.ShowModal;
  if Result=mrOk then begin
    ItemType:=NewOtherDialog.ItemType.CreateCopy;
  end;
  IDEDialogLayoutList.SaveLayout(NewOtherDialog);
  NewOtherDialog.Free;
end;

function NewIDEItems: TNewIDEItemCategories;
begin
  if InternalNewIDEItems=nil then
    InternalNewIDEItems:=TNewIDEItemCategories.CreateWithDefaults;
  Result:=InternalNewIDEItems;
end;

procedure InternalFinal;
begin
  InternalNewIDEItems.Free;
  InternalNewIDEItems:=nil;
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
  Template: TNewIDEItemTemplate;
begin
  ANode:=ItemsTreeView.Selected;
  if (ANode=nil) or (ANode.Data=nil)
  or (not (TObject(ANode.Data) is TNewIDEItemTemplate))
  then begin
    MessageDlg(lisNewDlgNoItemSelected,
      lisNewDlgPleaseSelectAnItemFirst, mtInformation, [mbOk], 0);
    exit;
  end;
  Template:=TNewIDEItemTemplate(ANode.Data);
  FItemType.Assign(Template);

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
  FItemType:=TNewIDEItem.Create;
  if LazarusResources.Find(Classname)=nil then begin
    Name:='NewOtherDialog';
    Caption := 'New ...';
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
  FItemType.Free;
  inherited Destroy;
end;

{ TNewIDEItem }

procedure TNewIDEItem.SetTheType(const AValue: TNewIDEItemType);
begin
  if FTheType=AValue then exit;
  FTheType:=AValue;
end;

constructor TNewIDEItem.Create;
begin
  FTheType:=niiNone;
  FFlag:=niifCopy;
end;

procedure TNewIDEItem.SetFlag(const AValue: TNewIDEItemFlag);
begin
  if FFlag=AValue then exit;
  FFlag:=AValue;
end;

procedure TNewIDEItem.Assign(Source: TNewIDEItem);
begin
  TheType:=Source.TheType;
  Flag:=Source.Flag;
end;

procedure TNewIDEItem.Assign(Source: TNewIDEItemTemplate);
begin
  TheType:=Source.TheType;
  Flag:=Source.DefaultFlag;
end;

function TNewIDEItem.CreateCopy: TNewIDEItem;
begin
  Result:=TNewIDEItem.Create;
  Result.Assign(Self);
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

constructor TNewIDEItemCategory.Create(const AName: string);
begin
  FItems:=TList.Create;
  FName:=AName;
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

{ TNewIDEItemTemplate }

constructor TNewIDEItemTemplate.Create(AType: TNewIDEItemType;
  const AName: string; ADefaultFlag: TNewIDEItemFlag;
  TheAllowedFlags: TNewIDEItemFlags);
begin
  FTheType:=AType;
  FName:=AName;
  FDefaultFlag:=ADefaultFlag;
  FAllowedFlags:=TheAllowedFlags;
  Include(FAllowedFlags,FDefaultFlag);
end;

function TNewIDEItemTemplate.LocalizedName: string;
begin
  // ToDo: translate
  Result:=Name;
end;

function TNewIDEItemTemplate.Description: string;
begin
  case TheType of
    niiNone:
      Result:='';
      
    niiCustom:
      Result:='';
      
    niiUnit:
      Result:=lisNewDlgCreateANewPascalUnit;
      
    niiForm:
      Result:=lisNewDlgCreateANewUnitWithALCLForm;

    niiDataModule:
      Result:=lisNewDlgCreateANewUnitWithADataModule;

    niiText:
      Result:=lisNewDlgCreateANewEmptyTextFile;
      
    niiApplication:
      Result:=Format(
        lisNewDlgCreateANewGraphicalApplication, [#13#13]);

    niiFPCProject:
      Result:=Format(
        lisNewDlgCreateANewProgram, [#13#13]);

    niiCustomProject:
      Result:=lisNewDlgCreateANewCustomProgram;

    niiCGIApplication:
      Result:=Format(lisNewCreateANewCgiApplicationTheProgramFileIsMaintained, [
        #13]);

    niiPackage:
      Result:=Format(
        lisNewDlgCreateANewStandardPackageAPackageIsACollectionOfUn, [#13#13]);

  else
    Result:=''
  end;
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

constructor TNewIDEItemCategories.CreateWithDefaults;
var
  NewCategory: TNewIDEItemCategory;
begin
  FItems:=TList.Create;

  // category file
  NewCategory:=TNewIDEItemCategory.Create('File');
  Add(NewCategory);
  NewCategory.Add(TNewIDEItemTemplate.Create(niiUnit,'Unit',niifCopy,[]));
  NewCategory.Add(TNewIDEItemTemplate.Create(niiForm,'Form',niifCopy,[]));
  NewCategory.Add(TNewIDEItemTemplate.Create(niiDataModule,'Data Module',
                                             niifCopy,[]));
  NewCategory.Add(TNewIDEItemTemplate.Create(niiText,'Text',niifCopy,[]));
  
  // category project
  NewCategory:=TNewIDEItemCategory.Create('Project');
  Add(NewCategory);
  NewCategory.Add(
    TNewIDEItemTemplate.Create(niiApplication,'Application',niifCopy,[]));
  NewCategory.Add(
    TNewIDEItemTemplate.Create(niiFPCProject,'FPC Project',niifCopy,[]));
  NewCategory.Add(
    TNewIDEItemTemplate.Create(niiCustomProject,'Custom Project',niifCopy,[]));
  {$IFDEF HasCGIModules}
  NewCategory.Add(
    TNewIDEItemTemplate.Create(niiCGIApplication,'CGI Application',niifCopy,[]));
  {$ENDIF}

  // category package
  NewCategory:=TNewIDEItemCategory.Create('Package');
  Add(NewCategory);
  NewCategory.Add(
    TNewIDEItemTemplate.Create(niiPackage,'Standard Package',niifCopy,[]));
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

finalization
  InternalFinal;

end.

