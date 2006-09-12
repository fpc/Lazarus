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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

{
 Property/component editor for TMenuItems objects (TMainMenu and TPopupMenu)

 Author: Olivier guilbaud  (golivier@free.fr)
 
 History (DD/MM/YYYY)
   20/01/2003 OG - Create
   28/01/2003 OG - Add Property editor
   27/03/2003 OG - Entire rewrite. Not use an Menu because dont work properly
   01/04/2003 OG - Add after/before buttons
                 - First version
}

{#todo checked or radio state display}
{#toto editing caption directly in treeview}
{#todo Delete all childrens in btnDelItem() before delete an item (bug ?)}
{#todo localizing all strings}

unit MenuPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LMessages, Forms, Controls, Graphics, Dialogs,
  LResources, ComCtrls, StdCtrls, Buttons, ExtCtrls, Menus, PropEdits,
  ComponentEditors;

Implementation

Type
 {TMenuItemsPropertyEditorDlg}
  TMenuItemsPropertyEditorDlg = Class(TForm)
  private
    fEditor: TComponentEditor;
    fMenu  : TMenu;
    TV     : TTreeView;

    procedure OnShowEditorEditor(Sender : TObject);
    procedure OnHideEditorEditor(Sender : TObject);
    procedure OnActivateEditorEditor(Sender : TObject);
    
    procedure btnAddMainItem(Sender : TObject);
    procedure btnAddItem(Sender : TObject);
    procedure btnAddSubItem(Sender : TObject);
    procedure btnDelItem(Sender : TObject);
    procedure btnAddItemBefore(Sender : TObject);
    
    procedure TVOnClick(Sender : TObject);
    procedure btnOKClick(Sender : TObject);

    function  CreateNewItem(ItmParent : TMenuItem; atPos : Integer):TMenuItem;
    procedure SelectOnObjectInspector(aComponent : TComponent; Modify: boolean);
    procedure InitTVMenus;
    
    function FindTreeNode(Itm : TMenuItem): TTreeNode;
  public
    constructor Create(aOwner : TComponent); override;

    property Menu   : TMenu read fMenu write fMenu;
    property Editor : TComponentEditor read fEditor write fEditor;
  end;
  
  TMenuComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowMenuEditor;

    procedure AddNewItemToDesigner(aItem : TMenuItem);
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;

    function Menu : TMenu;
  end;

  {TMenuItemsPropertyEditor
   Property editor for the TMenuItems properties.
   Brings up the dialog for editing menus items}
   TMenuItemsPropertyEditor = Class(TClassPropertyEditor)
   public
     procedure Edit; override;
     function GetAttributes: TPropertyAttributes; override;
   end;

Var
  //Global variable for menu editor. It's created at the first call
  //of ShowEditorMenu. It is impossible to edit more than one menu at a time
  MenuEditorDlg : TMenuItemsPropertyEditorDlg;


Procedure ShowEditorMenu(aMenu : TMenu; aEditor : TComponentEditor);
begin
  If not Assigned(MenuEditorDlg) then
    MenuEditorDlg:=TMenuItemsPropertyEditorDlg.Create(Application);

  MenuEditorDlg.Editor:=aEditor;
  MenuEditorDlg.Menu  :=aMenu;
  
  MenuEditorDlg.Show;
end;

//This function finds the Designer of a Component
function GetDesignerOfComponent(aComponent: TComponent): TComponentEditorDesigner;
var
  OwnerForm: TCustomForm;
begin
  Result:=nil;
  OwnerForm:=GetDesignerForm(AComponent);
  if (OwnerForm<>nil) then
    Result:=OwnerForm.Designer as TComponentEditorDesigner;
end;


{Create the menus editor dialog}
constructor TMenuItemsPropertyEditorDlg.Create(aOwner : TComponent);
Var Cmp : TWinControl;
begin
  inherited Create(aOwner);
  OnShow:=@OnShowEditorEditor;
  OnHide:=@OnHideEditorEditor;
  OnActivate:=@OnActivateEditorEditor;
  
  //Size of window
  Height:=300;
  Width :=546;
  BorderStyle:=bsSingle;
  Position :=poScreenCenter;
  Caption  :='Menu editor';

  Cmp:=TPanel.Create(self);
  With TPanel(Cmp) do
  begin
    Parent:=Self;
    Height:=41;
    Align :=alBottom;
  end;

  //Btn Ok
  With TBitBtn.Create(self) do
  begin
    Parent:=Cmp;
    Left  :=464;
    Width :=72;
    Top   :=8;
    Height:=25;
    Kind  :=bkOk;
    OnClick:=@btnOKClick;
  end;

  //Right panel
  Cmp:=TPanel.Create(self);
  With TPanel(Cmp) do
  begin
    Width  :=185;
    Top    :=0;
    Left   :=361;
    Align :=alRight;
    Parent :=Self;
    Caption:='';
  end;

  //btn add main item
  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Enabled:=True;
    Left   :=7;
    Width  :=171;
    Top    :=15;
    height :=25;
    Caption:='Add main item';
    OnClick:=@btnAddMainItem;
  end;

  //btn Add item before
  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Enabled:=True;
    Left   :=7;
    Width  :=171;
    Top    :=48;
    height :=25;
    Caption:='Add item before';
    OnClick:=@btnAddItembefore;
  end;

  //btn Add item after
  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Enabled:=True;
    Left   :=7;
    Width  :=171;
    Top    :=81;
    height :=25;
    Caption:='Add item after';
    OnClick:=@btnAddItem;
  end;

  //btn Add sub item
  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Enabled:=True;
    Left   :=7;
    Width  :=171;
    Top    :=114;
    height :=25;
    Caption:='Add sub item';
    OnClick:=@btnAddSubItem;
  end;

  //btn delete item
  With TButton.Create(self) do
  begin
    Parent :=Cmp;
    Enabled:=True;
    Left   :=7;
    Width  :=171;
    Top    :=154;
    height :=25;
    Caption:='Delete item';
    OnClick:=@btnDelItem;
  end;

  TV:=TTreeView.Create(self);
  With TV do
  begin
    Parent    :=self;
    Left      :=5;
    Align     :=alClient;
    //ScrollBars:=ssautoboth;
    
    //Options of TV
    RightClickSelect:=True;
    ReadOnly:=True;
    ShowButtons:=False;
    AutoExpand:=True;
    HideSelection:=False;
    RowSelect:=True;
    
    OnClick :=@TVOnClick;
  end;
end;


//Init the TreeView menu and select the first item
procedure TMenuItemsPropertyEditorDlg.OnShowEditorEditor(Sender: TObject);
begin
  InitTVMenus;
  If Assigned(fMenu) and (fMenu.Items.Count>0) then
  begin
    //Select the first item on inspector object
    SelectOnObjectInspector(fMenu.Items.Items[0],false);
    
    //expand the treeview node
    TV.Items.Items[0].Expand(False);
  end;
end;

//clear the TreeView work menu when hide dialog
procedure TMenuItemsPropertyEditorDlg.OnHideEditorEditor(Sender: TObject);
begin
  TV.Items.Clear;
end;

//Update TV on selected item
procedure TMenuItemsPropertyEditorDlg.OnActivateEditorEditor(Sender: TObject);
Var MnuI : TMenuItem;
begin
  If Assigned(TV.Selected) and Assigned(fMenu) then
  begin
    MnuI:=TMenuItem(TV.Selected.Data);

    InitTVMenus;
    
    TV.Selected:=FindTreeNode(MnuI);
  end;
end;


//Add new Main Item
procedure TMenuItemsPropertyEditorDlg.btnAddMainItem(Sender: TObject);
begin
  if Assigned(fMenu) then
    CreateNewItem(fMenu.Items,0);
end;

//Add an item after
procedure TMenuItemsPropertyEditorDlg.btnAddItem(Sender: TObject);
Var Itm : TMenuItem;
begin
  If Assigned(TV.Selected) and Assigned(fMenu) then
  begin
    Itm:=TMenuItem(TV.Selected.Data);
    CreateNewItem(Itm.Parent,1);
  end;
end;

//Add an sub item
procedure TMenuItemsPropertyEditorDlg.btnAddSubItem(Sender: TObject);
Var Itm : TMenuItem;
begin
  If Assigned(TV.Selected) and Assigned(fMenu) then
  begin
    Itm:=TMenuItem(TV.Selected.Data);
    CreateNewItem(Itm,0);
  end;
end;

//Delete an item
procedure TMenuItemsPropertyEditorDlg.btnDelItem(Sender: TObject);
Var TN   : TTreeNode;
    Itm  : TMenuItem;

  procedure InternalDelItem(ItemDel : TTreeNode);
  begin
    if ItemDel.Count>0 then
    begin
      TN:=ItemDel;
      MessageDlg('Error','You must delete all childrens items before.',mtError,
                 [mbCancel]);
      Exit;
    end;
    //delete all child before
{    While ItemDel.Count<>0 do
       InternalDelItem(ItemDel.Items[0]);
}
    //Delete this item
    Itm:=TMenuItem(ItemDel.Data);
    Itm.Free;

    ItemDel.Data:=nil;
    ItemDel.Delete;
  end;
  
begin
  If Assigned(TV.Selected) and Assigned(fMenu) then
  begin
    Itm:=TMenuItem(TV.Selected.Data);
    If Assigned(Itm) then
    begin
      TV.BeginUpdate;
      try
        //Save parent for select after
        TN:=FindTreeNode(Itm.Parent);
        InternalDelItem(TV.Selected);
      
        //If not saved parent, and Count<>0, item first selected
        If not Assigned(TN) and (fMenu.Items.Count<>0) then
           TN:=FindTreeNode(fMenu.Items.Items[0]);

        TV.Selected:=TN;
        TVOnClick(nil);
      finally
        TV.EndUpdate;
      end;
    end;
  end;
end;

//Add an item before the selected item
procedure TMenuItemsPropertyEditorDlg.btnAddItembefore(Sender: TObject);
Var Itm : TMenuItem;
begin
  If Assigned(TV.Selected) and Assigned(fMenu) then
  begin
    Itm:=TMenuItem(TV.Selected.Data);
    CreateNewItem(Itm.Parent,-1);
  end;
end;

//Update inspector object
procedure TMenuItemsPropertyEditorDlg.TVOnClick(Sender: TObject);
Var Itm : TMenuItem;
begin
  If Assigned(TV.Selected) and Assigned(fMenu) then
  begin
    Itm:=TMenuItem(TV.Selected.Data);

    //Modify the object inspector
    SelectOnObjectInspector(Itm,false);
  end;
end;

//Close the editor
procedure TMenuItemsPropertyEditorDlg.btnOKClick(Sender: TObject);
begin
  Close;
end;

{
Abstact
   Create a new item menu
   
parameters
   ItmParent
   atPos      -1 before
               0 at end
               1 after
}
Function TMenuItemsPropertyEditorDlg.CreateNewItem(ItmParent : TMenuItem;
  atPos : Integer):TMenuItem;
var
  TN,TNP  : TTreeNode;
  NewItem : TMenuItem;
begin
  Result:=nil;

  if Assigned(fMenu) and Assigned(fEditor) then
  begin
    NewItem:=TMenuItem.Create(fMenu.Owner);
    TV.BeginUpdate;
    Try
      //Init an unique name
      NewItem.Caption:=
        fEditor.GetDesigner.CreateUniqueComponentName(NewItem.ClassName);
      NewItem.Name:=NewItem.Caption;
      
      //Add the item on the parent
      ItmParent.Add(NewItem);

      //Update TreeView
      TNP:=self.FindTreeNode(ItmParent);
      If Assigned(TNP) then
      begin
        if Assigned(TV.Selected) and (atPos<>0) then
        begin
          if atPos=-1 then //before
             TN:=TV.Items.Insert(TV.Selected,NewItem.Caption)
          else
             TN:=TV.Items.InsertBehind(TV.Selected,NewItem.Caption);
        end
        else TN:=TV.Items.AddChild(TNP,NewItem.Caption);
      end
      else
        TN:=TV.Items.Add(nil,NewItem.Caption);

      TN.Data:=NewItem; //Save the menu item
      TV.Selected:=TN;

      //Modify the object inspector
      fEditor.GetDesigner.PropertyEditorHook.ComponentAdded(NewItem,true);
      fEditor.GetDesigner.Modified;
      //SelectOnObjectInspector(NewItem);
    finally
      TV.EndUpdate;
      Result:=NewItem;
    end;
  end;
end;

//Modify the object inspector for select the aComponent
procedure TMenuItemsPropertyEditorDlg.SelectOnObjectInspector(
  aComponent : TComponent; Modify: boolean);
begin
  If Assigned(fMenu) and Assigned(fEditor) and Assigned(aComponent) then
  begin
    fEditor.GetDesigner.SelectOnlyThisComponent(aComponent);
    if Modify then
      fEditor.GetDesigner.Modified;
  end;
end;

//Initialise TreeView with items of fMenu
//Use  lDupMenuItems() recursive for this
procedure TMenuItemsPropertyEditorDlg.InitTVMenus;

  procedure InternalInitTVItems(ItmParent : TTreeNode; M : TMenuItem);
  Var TN  : TTreeNode;
      j   : Integer;
  begin
    if M.Count>0 then
    begin
      for j:=0 to M.Count-1 do
      begin
        if Assigned(ItmParent) then
           TN:=TV.Items.AddChild(ItmParent,M.Items[j].Caption)
        else
           TN:=TV.Items.Add(nil,M.Items[j].Caption);

        //Show the separator
        if M.Caption='-' then
           TV.Selected.Text:='______________________';

        TN.Data:=M.Items[j]; //Save the menu item
        TV.Selected:=TN;
        
        //init the sub menu if exists
        if M.Items[j].Count>0 then
           InternalInitTVItems(TN,M.Items[j]);
         end;
      end;
    end;

begin
  TV.Items.Clear;
  
  If Assigned(fMenu) then
     InternalInitTVItems(nil,fMenu.Items);
end;

//Find
function TMenuItemsPropertyEditorDlg.FindTreeNode(Itm: TMenuItem): TTreeNode;
var i : Integer;

  function InternalFindNode(aNodeParent : TTreeNode) : TTreeNode;
  Var j : Integer;
  begin
    Result:=nil;
    for j:=0 to aNodeParent.Count-1 do
    begin
      if TMenuItem(aNodeParent.Items[j].Data)=Itm then
        Result:=aNodeParent.Items[j]
      else if aNodeParent.Items[j].Count<>0 then
                Result:=InternalFindNode(aNodeParent.Items[j]);

      if Assigned(Result) then
        Break;
    end;
  end;
  
begin
  Result:=nil;
  if Assigned(fMenu) and Assigned(Itm) then
  begin
    for i:=0 to TV.Items.Count-1 do
    begin
      if TMenuItem(TV.Items.Items[i].Data)=Itm then
        Result:=TV.Items.Items[i]
      else
        Result:=InternalFindNode(TV.Items.Items[i]);
        
      if Assigned(Result) then
         Break;
    end;
  end;
end;

{ TMenuComponentEditor }

procedure TMenuComponentEditor.DoShowMenuEditor;
begin
  ShowEditorMenu(Menu,Self);
end;

procedure TMenuComponentEditor.AddNewItemToDesigner(aItem : TMenuItem);
var
  Hook: TPropertyEditorHook;
  NewName: string;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  NewName:=GetDesigner.CreateUniqueComponentName(aItem.ClassName);
  aItem.Caption:=NewName;
  aItem.Name:=NewName;

  Hook.ComponentAdded(aItem,true);
  GetDesigner.Modified;
end;

procedure TMenuComponentEditor.Edit;
begin
  DoShowMenuEditor;
end;

procedure TMenuComponentEditor.ExecuteVerb(Index: Integer);
begin
  Case index of
   0 : DoShowMenuEditor;
  end;
end;

function TMenuComponentEditor.GetVerb(Index: Integer): string;
begin
  Case index of
    0 : Result:='Menu editor'+' ...';
  end;
end;

function TMenuComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TMenuComponentEditor.Menu: TMenu;
begin
  Result:=TMenu(GetComponent);
end;

{ TMenuItemsPropertyEditor }

//Find the component editor and call
procedure TMenuItemsPropertyEditor.Edit;
Var DI : TComponentEditorDesigner;
    Ds : TBaseComponentEditor;
    Me : TMenu;
begin
  Me:=TMenuItem(GetOrdValue).GetParentMenu;
  DI:=GetDesignerOfComponent(Me);
  If Assigned(DI) then
  begin
    Ds:=GetComponentEditor(Me,DI);
    If Assigned(Ds) then
      Ds.ExecuteVerb(0);
  end;
end;

function TMenuItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog,paReadOnly,paRevertable];
end;

initialization
  //Init a globale variable
  MenuEditorDlg:=nil;

  //Initialization of properties Items of TMainMenu and TPopupMenu
  RegisterPropertyEditor(ClassTypeInfo(TMenuItem), TPopupMenu,'Items',
    TMenuItemsPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TMenuItem), TMainMenu,'Items',
    TMenuItemsPropertyEditor);

  //Register a component editor for with mouse right click, the popup
  //add "Menu editor ..."
  RegisterComponentEditor(TPopupMenu,TMenuComponentEditor);
  RegisterComponentEditor(TMainMenu,TMenuComponentEditor);

end.

