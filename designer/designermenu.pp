{  $Id$  }
{
 /***************************************************************************
                            designermenu.pas
                            ----------------


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

  Author: Martin Patik

}
unit DesignerMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, Graphics, GraphType, Buttons,
  StdCtrls, ExtCtrls, ComponentEditors;

type

  PDesignerMenuItem = ^TDesignerMenuItem;
  TDesignerMenuItem = record
    SelfPanel: TPanel;
    SubMenuPanel: TPanel;
    ParentMenu: PDesignerMenuItem;
    SubMenu: PDesignerMenuItem;
    PrevItem: PDesignerMenuItem;
    NextItem: PDesignerMenuItem;
    Index: Integer;
    Caption: string;
    coord: TRect;
    Level: Integer;
    Selected: Boolean;
    Active: Boolean;
    SelfPanelCreated: Boolean;
    SubMenuPanelCreated: Boolean;
    ID: string;
  end;
  
  TDesignerMainMenu = class(TCustomControl)
  private
    fRoot: PDesignerMenuItem;
    fPanel: TPanel;
    fDesignerMenuItemIdent: Integer;
    fParentCanvas: TCanvas;
    fSelectedDesignerMenuItem: string;
    fEditor: TComponentEditor;
    DesignerPopupMenu: TPopupMenu;
    
    fMenu:TMenu;
    
  public
    constructor CreateWithMenu(aOwner: TComponent; aMenu: TMenu; aEditor: TComponentEditor);
    destructor Destroy; override;
    
    property Root: PDesignerMenuItem read FRoot write FRoot;
    property Panel: TPanel read FPanel write FPanel;
    property DesignerMenuItemIdent: Integer read FDesignerMenuItemIdent write FDesignerMenuItemIdent;
    property SelectedDesignerMenuItem: string read FSelectedDesignerMenuItem write FSelectedDesignerMenuItem;
    property Editor: TComponentEditor read fEditor write fEditor;
    
    property ParentCanvas: TCanvas read FParentCanvas write FParentCanvas;
                           
    procedure LoadMainMenu;
    function SaveMainMenu: TMainMenu;
    
    procedure Init(MenuItem: PDesignerMenuItem);
    procedure Link(MenuItem: TMenuItem; ParentM: PDesignerMenuItem);
    procedure Draw(MenuItem: PDesignerMenuItem; FormPanel,SubMenuPanel: TPanel);
    procedure SetCoordinates(Coord_Left,Coord_Top,Coord_Right: Integer;MenuItem: PDesignerMenuItem);
    function GetSubMenuHeight(MenuItem: PDesignerMenuItem; LeftPos,TopPos: Integer; Ident: string): TRect;
    procedure MenuItemMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    
    procedure AddNewItemBeforeClick(Sender: TObject);
    procedure AddNewItemAfterClick(Sender: TObject);
    procedure AddSubMenuClick(Sender: TObject);
    procedure MoveUpCLick(Sender: TObject);
    procedure MoveDownCLick(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    
    procedure AddNewItemBefore(MenuItem: PDesignerMenuItem; Ident: string);
    procedure AddNewItemAfter(MenuItem: PDesignerMenuItem; Ident: string);
    procedure AddSubMenu(MenuItem: PDesignerMenuItem; Ident: string);
    function MoveUp(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
    function MoveDown(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
    function DeleteItem(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
    
    procedure InitIndexSequence;
    function CreateIndexSequence(MenuItem: PDesignerMenuItem; Ident: string; Ind: Integer): Boolean;
    procedure VypisIndexSequence;
    
    procedure UpdateMenu(MenuItem: TMenuItem; DesignerMenuItem: PDesignerMenuItem; Ind,Action: Integer);
    
    function ChangeMenuItem(MenuItem: PDesignerMenuItem; Action: Integer; Ident: string): Boolean;
    
    function GetDesignerMenuItem(DesignerMenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
    
  end;
  

implementation

const
  DESIGNER_MENU_ITEM_HEIGHT=24;
  DESIGNER_MENU_ITEM_SPACE=10;
  MIN_DESIGNER_MENU_ITEM_WIDTH=100;
  DESIGNER_MENU_ITEM_PANEL_HEIGHT=22;
  MIN_SUB_MENU_PANEL_WIDTH=100;

  // Length of a field for storing index positions of DesignerMenuItem, we use it to find the right MenuItem
  INDEX_SEQUENCE_LENGTH=10;

var 
  temp_level: Integer;
  temp_newitemcounter: Integer;
  
  temp_panel: TPanel;
  
  index_sequence: Array[1..INDEX_SEQUENCE_LENGTH] of Integer;

//
constructor TDesignerMainMenu.CreateWithMenu(aOwner: TComponent; aMenu: TMenu; aEditor: TComponentEditor);
var
  PopupMenuItem: TMenuItem;
begin
  inherited Create(aOwner);
  
  // creates PopupMenu for and its items the menu editor
  DesignerPopupMenu:=TPopupMenu.Create(aOwner);
  
  PopupMenuItem:=TMenuItem.Create(aOwner);
  PopupMenuItem.Caption:='New Item (before)';
  PopupMenuItem.OnClick:=@AddNewItemBeforeClick;
  DesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(aOwner);
  PopupMenuItem.Caption:='New Item (after)';
  PopupMenuItem.OnClick:=@AddNewItemAfterClick;
  DesignerPopupMenu.Items.Add(PopupMenuItem);
  
  PopupMenuItem:=TMenuItem.Create(aOwner);
  PopupMenuItem.Caption:='Add SubMenu (after)';
  PopupMenuItem.OnClick:=@AddSubMenuClick;
  DesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(aOwner);
  PopupMenuItem.Caption:='Move Up(left)';
  PopupMenuItem.OnClick:=@MoveUpClick;
  DesignerPopupMenu.Items.Add(PopupMenuItem);
  
  PopupMenuItem:=TMenuItem.Create(aOwner);
  PopupMenuItem.Caption:='Move Down(right)';
  PopupMenuItem.OnClick:=@MoveDownClick;
  DesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(aOwner);
  PopupMenuItem.Caption:='Delete Item';
  PopupMenuItem.OnClick:=@DeleteItemClick;
  DesignerPopupMenu.Items.Add(PopupMenuItem);

  new(Root);
  fMenu:=aMenu;
  Editor:=aEditor;
  DesignerMenuItemIdent:=1;
  //if fMenu is TMainMenu then
    temp_level:=1;
  //if fMenu is TPopupMenu then
    //temp_level:=2;
  //temp_newitemcounter:=0;
  temp_panel:=TPanel.Create(self);
  temp_panel.Visible:=false;
end;

destructor TDesignerMainMenu.Destroy;
begin
  SaveMainMenu;
  Dispose(Root);
  inherited Destroy;
end;

// ------------------------------------------------------------------------------------//
// Initialize new DesignerMenuItem (sets ID, defaluts values, creates SelfPanel ...) --//
// ------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.Init(MenuItem: PDesignerMenuItem);
var temp: Integer;
begin

  temp:=DesignerMenuItemIdent;
  Str(temp,MenuItem^.ID);
  
  MenuItem^.Selected:=false;
  MenuItem^.Active:=false;
  
  MenuItem^.coord.Left:=0;
  MenuItem^.coord.Top:=0;
  MenuItem^.coord.Right:=0;
  MenuItem^.coord.Bottom:=0;
  
  MenuItem^.SelfPanelCreated:=false;
  MenuItem^.SubMenuPanelCreated:=false;
  
  MenuItem^.SelfPanel:=TPanel.Create(self);
  MenuItem^.SelfPanel.Name:=MenuItem^.ID;
  MenuItem^.SelfPanel.Height:=DESIGNER_MENU_ITEM_PANEL_HEIGHT;
  Menuitem^.SelfPanel.OnMouseDown:=@MenuItemMouseDown;
  MenuItem^.SubMenuPanel:=TPanel.Create(self);
  
  DesignerMenuItemIdent:=DesignerMenuItemIdent + 1;
end;

// --------------------------------------------------------------------------
// Loads the MainMenu from the Designer form and creates the DesignerMainMenu
// --------------------------------------------------------------------------
procedure TDesignerMainMenu.LoadMainMenu;
var
  prevtemp,temp: PDesignerMenuItem;
  i: Integer;
  firstmenuitem: TMenuItem;
begin

  if (fMenu.Items.Count = 0) then
  begin
    firstmenuitem:=TMenuItem.Create(self);
    firstmenuitem.Caption:='New Item 1';
    fMenu.Items.Add(firstmenuitem);
    inc(temp_newitemcounter);
  end;
  
  prevtemp:=nil;
  for i:= 0 to fMenu.Items.Count-1 do
  begin
    new(temp);
    temp^.Caption:=fMenu.Items[i].Caption;
    temp^.Level:=temp_level;
    temp^.NextItem:=nil;
    temp^.SubMenu:=nil;
    temp^.ParentMenu:=nil;
    temp^.Index:=i;
    if (i=0) then
    begin
      temp^.PrevItem:=nil;
      Root:=temp;
    end else
    begin
      temp^.PrevItem:=prevtemp;
      prevtemp^.NextItem:=temp;
    end;
    Init(temp);
    prevtemp:=temp;
    Link(fMenu.Items[i],temp);
  end;
  Root^.Selected:=true;
end;

procedure TDesignerMainMenu.Link(MenuItem: TMenuItem; ParentM: PDesignerMenuItem);
var
  prevtemp,temp: PDesignerMenuItem;
  i: Integer;
begin
  inc(temp_level);
  if (MenuItem.Count > 0) then
  begin
    prevtemp:=nil;
    for i:= 0 to MenuItem.Count-1 do
    begin
      new(temp); 
      temp^.Caption:=MenuItem.Items[i].Caption;
      temp^.Level:=temp_level;
      temp^.NextItem:=nil;
      temp^.SubMenu:=nil;
      temp^.Index:=i;
      if (i=0) then
      begin
        temp^.ParentMenu:=ParentM;
        temp^.PrevItem:=nil;
        ParentM^.SubMenu:=temp;
      end else
      begin
        temp^.PrevItem:=prevtemp;
        prevtemp^.NextItem:=temp;
        temp^.ParentMenu:=nil;
      end;
      Init(temp);
      prevtemp:=temp;
      Link(MenuItem.Items[i],temp);
    end;
  end;
  dec(temp_level);
end;

// ------------------------------------------------------------------
// Draw the the whole DesignerMenu with active MenuItems and SubMenus
// ------------------------------------------------------------------
procedure TDesignerMainMenu.Draw(MenuItem: PDesignerMenuItem; FormPanel,SubMenuPanel: TPanel);
var
  SubMenuDimensions: TRect;
begin
  if (MenuItem^.NextItem <> nil) then Draw(MenuItem^.NextItem, FormPanel, SubMenuPanel);
  with MenuItem^.SelfPanel do
  begin
    if (fMenu is TPopupMenu) and (MenuItem^.Level = 1) then
    begin
       Parent:=temp_panel;
       writeln('Je to temp_panel');
    end else
      Parent:=FormPanel;
    Visible:=true;
    if (MenuItem^.Level > 1) and (fMenu is TMainMenu) then
    begin
      Left:=1;
      Top:=MenuItem^.coord.top;
      Width:=Parent.width - 4
    end else
    begin
      Left:=MenuItem^.coord.left;
      Top:=MenuItem^.coord.top;
      Width:=ParentCanvas.TextWidth(MenuItem^.Caption) + 10;
    end;
    if (MenuItem^.Selected) then
    begin
      if (MenuItem^.Level > 1) then
      begin
        Color:=clWhite;
        Bevelouter:=bvnone;
      end else
      begin
        Color:=clSilver;
        Bevelouter:=bvraised;
        Bevelwidth:=1;
      end;
    end else
    if (MenuItem^.Active) then
    begin
      Color:=clSilver;
      Bevelouter:=bvlowered;
      Bevelwidth:=1;
    end else
    begin
      Color:=clSilver;
      Bevelouter:=bvnone;
    end;
    Caption:=MenuItem^.Caption;
    if (fMenu is TPopupMenu) and (MenuItem^.Level = 1) then
    begin
      SubMenuDimensions:=GetSubMenuHeight(Root,1, 1, MenuItem^.ID);
      with temp_panel do
      begin
        Parent:=FormPanel;
        Visible:=true;
        Left:=SubMenuDimensions.left;
        Top:=SubMenuDimensions.top;
        Width:=SubMenuDimensions.right;
        Height:=SubMenuDimensions.bottom;
      end;
    end;
    if (((MenuItem^.Selected) or (MenuItem^.Active)) and (MenuItem^.SubMenu <> nil)) then
    begin
      if (fMenu is TpopupMenu) and (MenuItem^.Level = 1) then
        SubMenuDimensions:=GetSubMenuHeight(Root, MenuItem^.coord.right + 1, 1, MenuItem^.SubMenu^.ID)
      else
        SubMenuDimensions:=GetSubMenuHeight(Root, 1, 1, MenuItem^.SubMenu^.ID);
      with MenuItem^.SubMenuPanel do
      begin
        Parent:=SubMenuPanel;
        Visible:=true;
        Left:=SubMenuDimensions.left;
        Top:=SubMenuDimensions.top;
        Width:=SubMenuDimensions.right - SubMenuDimensions.left;
        Height:=SubMenuDimensions.bottom - SubMenuDimensions.top;
      end;
    end;
  end;
  if ((MenuItem^.SubMenu <> nil) and ((MenuItem^.Selected) or (MenuItem^.Active))) then Draw(MenuItem^.SubMenu, MenuItem^.SubMenuPanel,SubMenuPanel);
end;

// ----------------------------------------------------------
// Saves DesignerMainMenu to Menu placed on the Designer Form
// ----------------------------------------------------------
function TDesignerMainMenu.SaveMainMenu: TMainMenu;
var
  temp_menuitem: PDesignerMenuItem;
  mi: TMenuItem;
  temp_item: TMenuItem;
  editormenu: TMainMenu;
  i: Integer;
begin
  Result:=nil;
  editormenu:=TMainMenu.Create(self);
  temp_menuitem:=Root;
  //for i:=0 to (fMenu.Items.Count - 1) do
  //begin
    //fMenu.Items[0].Free;
  //end;
  //while (temp_menuitem^.NextItem <> nil) do
  //begin
    //mi:=TMenuItem(fMenu.Owner);
    //mi.Name:='kjdskueksdk su';
    //mi.Caption:=temp_menuitem^.Caption;
    //fMenu.Items.Add(mi);
    //writeln('New MenuItem -> ',mi.Caption,' ( ',mi.Name,' )');
    //temp_menuitem:=temp_menuitem^.NextItem;
  //end;
  //AppMenu:=editormenu;
end;

// -------------------------------------------------------
// Set the coordinates (position) of each DesignerMenuItem
// -------------------------------------------------------
procedure TDesignerMainMenu.SetCoordinates(Coord_Left,Coord_Top,Coord_Right: Integer; MenuItem: PDesignerMenuItem);
var
  temp_menuitem: PDesignerMenuItem;
begin
  MenuItem^.coord.Left:=Coord_Left;
  MenuItem^.coord.Top:=Coord_Top;
  MenuItem^.coord.Bottom:=MenuItem^.coord.top + DESIGNER_MENU_ITEM_PANEL_HEIGHT;
  
  if (MenuItem^.Level = 1) and (fMenu is TMainMenu) then
  begin
    MenuItem^.coord.Right:=MenuItem^.coord.Left + Canvas.TextWidth(MenuItem^.Caption) + 10;
  end else
  begin
    // is this DesignerMenuItem wider than its predecessors?
    if (Canvas.TextWidth(MenuItem^.Caption) + DESIGNER_MENU_ITEM_SPACE > Coord_Right) then
    begin
      MenuItem^.coord.right:=Canvas.TextWidth(MenuItem^.Caption) + DESIGNER_MENU_ITEM_SPACE;
      // we have to set the width of all predecessors of this DesignerMenuItem to its size
      temp_menuitem:=MenuItem;
      while (temp_menuitem^.PrevItem <> nil) do
      begin
        temp_menuitem:=temp_menuitem^.PrevItem;
        temp_menuitem^.coord.right:=MenuItem^.coord.right;
      end;
    // if not wider then keep size of the predecessor
    end else
      MenuItem^.coord.right:=Coord_Right;
  end;
  
  writeln(MenuItem^.coord.left,' , ',MenuItem^.coord.top,' , ',MenuItem^.coord.right,' , ',MenuItem^.coord.bottom);
  
  if (MenuItem^.SubMenu <> nil) then SetCoordinates(1,1,MIN_DESIGNER_MENU_ITEM_WIDTH,MenuItem^.SubMenu);
  if (MenuItem^.NextItem <> nil) then 
  begin
    if (MenuItem^.Level = 1) and (fMenu is TMainMenu) then
      SetCoordinates(MenuItem^.coord.Right + 1,1,0,MenuItem^.NextItem)
    else
      SetCoordinates(MenuItem^.coord.left,MenuItem^.coord.Bottom + 1,MenuItem^.coord.right,MenuItem^.NextItem);
  end;
  
end;

// -------------------------------------------------------------------------------------------------------------------//
// Determines a position of the SubMenuPanel of some DesignerMenuItem ------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------//
function TDesignerMainMenu.GetSubMenuHeight(MenuItem: PDesignerMenuItem; LeftPos,TopPos: Integer; Ident: string): TRect;
var
  coords: TRect;
  number_of_submenu_items: Integer;
begin

  coords.left:=0;
  coords.right:=0;
  coords.top:=0;
  coords.bottom:=0;
  
  // is this the DesignerMenuItem which is the first in the submenu for which we build SubMenuPanel?
  if (MenuItem^.ID = Ident) then
  begin
    coords.left:=LeftPos;
    coords.top:=TopPos;
    coords.right:=coords.left + MenuItem^.coord.right;
    // sets the bottom coordinate of submenupanel depending on number of submenuitems
    number_of_submenu_items:=1;
    while(MenuItem^.NextItem <> nil) do
    begin
      inc(number_of_submenu_items);
      MenuItem:=MenuItem^.NextItem;
    end;
    coords.bottom:=coords.top + (number_of_submenu_items * DESIGNER_MENU_ITEM_HEIGHT);
    // return coordinates
    GetSubMenuHeight:=coords;
    exit;
  end else
  // oterwhise we will search in submenu or nextitem until we found it
  begin
    if (MenuItem^.SubMenu <> nil) then
      if (MenuItem^.Level > 1) then
        coords:=GetSubMenuHeight(MenuItem^.SubMenu,LeftPos + MenuItem^.coord.right,TopPos,Ident)
      else
        coords:=GetSubMenuHeight(MenuItem^.SubMenu,LeftPos,TopPos + MenuItem^.coord.bottom,Ident);
    // have we found the desired DesignerMenuItem in the submenu?
    if (coords.right > 0) then
    begin
      GetSubMenuHeight:=coords;
      exit;
    end;
    // we haven't found it, so we will search in items which follows
    if (MenuItem^.NextItem <> nil) then
      if (MenuItem^.Level > 1) then
        coords:=GetSubMenuHeight(MenuItem^.NextItem,LeftPos,TopPos + DESIGNER_MENU_ITEM_HEIGHT,Ident)
      else
        coords:=GetSubMenuHeight(MenuItem^.NextItem,LeftPos + MenuItem^.coord.right,TopPos,Ident);
    // we had to found it in the nextitems, because we haven't found it anywhere else
    GetSubMenuHeight:=coords;
  end;
end;

// --------------------------------------------------------
// Function that changes MenuItem (Remove, Add SubMenu ...)
// --------------------------------------------------------
function TDesignerMainMenu.ChangeMenuItem(MenuItem: PDesignerMenuItem; Action: Integer; Ident: string): Boolean;
var
  completed: boolean;
begin
  completed:=false;
  case Action of
  // Test if this MenuItem has been selected
  1: begin
       if (MenuItem^.ID = Ident) then
       begin
         MenuItem^.Selected:=true;
         MenuItem^.Active:=false;
         completed:=true;
       end else
         MenuItem^.Selected:=false;
       if (MenuItem^.SubMenu <> nil) then
       begin
         if (ChangeMenuItem(MenuItem^.SubMenu,Action,Ident) = true) then
         begin
           MenuItem^.Active:=true;
           completed:=true;
         end else MenuItem^.Active:=false;
       end;
       if (MenuItem^.NextItem <> nil) then
       begin
         if (ChangeMenuItem(MenuItem^.NextItem,Action,Ident) = true) then
           completed:=true;
       end;
     end;
  // Destroy all created panels of this MenuItem
  2: begin
       if (((MenuItem^.Selected) and (MenuItem^.SubMenu <> nil)) or (MenuItem^.Active)) then
       begin
         ChangeMenuItem(MenuItem^.SubMenu,Action,MenuItem^.SubMenu^.ID);
         //FreeAndNil(MenuItem^.SubMenuPanel);
         MenuItem^.SubMenuPanel.visible:=false;
       end;
       if (MenuItem^.NextItem <> nil) then
         ChangeMenuItem(MenuItem^.NextItem,Action,MenuItem^.NextItem^.ID);
         MenuItem^.SelfPanel.visible:=false;
       //FreeAndNil(MenuItem^.SelfPanel);
       //MenuItem^.SelfPanel.Free;
       ChangeMenuItem:=true;
     end;
 end;
 if (completed) then ChangeMenuItem:=true;
end;

// ----------------------------------------------------------------------------//
// We have clicked on some DesignerMenuItem -----------------------------------//
// ----------------------------------------------------------------------------//
procedure TDesignerMainMenu.MenuItemMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  writeln ('<<-- MOUSE DOWN -->>');
  SelectedDesignerMenuItem:=TPanel(Sender).Name;
  ChangeMenuItem(Root,2,Root^.ID);
  ChangeMenuItem(Root,1,TPanel(Sender).Name);
  Parent.Invalidate;
  // TEST
  InitIndexSequence;
  CreateIndexSequence(Root,TPanel(Sender).Name,1);
  VypisIndexSequence;
  // TEST
  if (Button = mbRight) then
    Parent.PopupMenu:=DesignerPopupMenu;
//  writeln('Vlastnik: ',AppMenu.Parent.Name);
end;

// -------------------------------------------------------------//
// New Item (before) has been selected from context menu -------//
// -------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemBeforeClick(Sender: TObject);
begin
  inc(temp_newitemcounter);
  AddNewItemBefore(Root, SelectedDesignerMenuItem);
  SetCoordinates(1, 1, 0, Root);
  Parent.Invalidate;
  
  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
  //VypisIndexSequence;
  
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.PrevItem, 1, 2);
end;

// ------------------------------------------------------------//
// New Item (after) has been selected from context menu -------//
// ------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemAfterClick(Sender: TObject);
begin
  inc(temp_newitemcounter);
  AddNewItemAfter(Root, SelectedDesignerMenuItem);
  SetCoordinates(1, 1, 0, Root);
  Parent.Invalidate;
  
  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem,1);
  //VypisIndexSequence;
  
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.NextItem, 1, 1);
end;

// ------------------------------------------------------------//
// Add SubMenu has been selected from context menu ------------//
// ------------------------------------------------------------//
procedure TDesignerMainMenu.AddSubMenuClick(Sender: TObject);
begin
  inc(temp_newitemcounter);
  AddSubMenu(Root,SelectedDesignerMenuItem);
  SetCoordinates(1,1,0,Root);
  Parent.Invalidate;
  
  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem,1);
  
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.SubMenu, 1, 3);
end;

// ----------------------------------------------------//
// Move Up has been selected from context menu --------//
// ----------------------------------------------------//
procedure TDesignerMainMenu.MoveUpClick(Sender: TObject);
begin
  if (MoveUp(Root,SelectedDesignerMenuItem) > 0) then
  begin
    SetCoordinates(1,1,0,Root);
    Parent.Invalidate;
    
    InitIndexSequence;
    CreateIndexSequence(Root, SelectedDesignerMenuItem,1);
    
    UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 4);
  end;
end;

// ----------------------------------------------------//
// Delete Item has been selected from context menu --------//
// ----------------------------------------------------//
procedure TDesignerMainMenu.DeleteItemClick(Sender: TObject);
begin
end;

// ------------------------------------------------------//
// Move Down has been selected from context menu --------//
// ------------------------------------------------------//
procedure TDesignerMainMenu.MoveDownClick(Sender: TObject);
begin
  if (MoveDown(Root,SelectedDesignerMenuItem) > 0) then
  begin
    SetCoordinates(1,1,0,Root);
    Parent.Invalidate;

    InitIndexSequence;
    CreateIndexSequence(Root, SelectedDesignerMenuItem,1);

    UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 5);
  end;
end;

// ------------------------------------------------------------------------------------//
// Adds new DesignerMenuItem before DesignerMenuItem with ID=Ident --------------------//
// ------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemBefore(MenuItem: PDesignerMenuItem; Ident: string);
var
   new_menuitem,temp_menuitem: PDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  if (MenuItem^.ID = Ident) then
  begin
    new(new_menuitem);
    Str(temp_newitemcounter,temp_newitemcounterstring);
    new_menuitem^.Caption:='New Item' + temp_newitemcounterstring;
    new_menuitem^.Level:=MenuItem^.Level;
    writeln (new_menuitem^.Caption,' -> Level: ',new_menuitem^.Level);
    new_menuitem^.NextItem:=MenuItem;
    new_menuitem^.SubMenu:=nil;
    if (MenuItem^.ParentMenu <> nil) then
    begin
      new_menuitem^.ParentMenu:=MenuItem^.ParentMenu;
      new_menuitem^.PrevItem:=nil;
      MenuItem^.ParentMenu^.SubMenu:=new_menuitem;
      MenuItem^.ParentMenu:=nil;
    end else
    begin
      new_menuitem^.ParentMenu:=nil;
      if (MenuItem^.PrevItem <> nil) then
      begin
        new_menuitem^.PrevItem:=MenuItem^.PrevItem;
        MenuItem^.PrevItem^.NextItem:=new_menuitem;
      end else
      begin
        new_menuitem^.PrevItem:=nil;
        Root:=new_menuitem;
      end;
    end;
    MenuItem^.PrevItem:=new_menuitem;
    // now we have to set the index of this DesignerMenuItem
    if (new_menuitem^.NextItem <> nil) then
    begin
      temp_menuitem:=new_menuitem;
      while (temp_menuitem <> nil) do
      begin
        if (temp_menuitem^.PrevItem <> nil) then
          temp_menuitem^.Index:=temp_menuitem^.PrevItem^.Index + 1
        else
          temp_menuitem^.Index:=0;
        temp_menuitem:=temp_menuitem^.NextItem;
      end;
    end else
      new_menuitem^.Index:=MenuItem^.Index + 1;

    Init(new_menuitem);
  end else
  begin
    if (MenuItem^.SubMenu <> nil) then
      AddNewItemBefore(MenuItem^.SubMenu,Ident);
    if (MenuItem^.NextItem <> nil) then
      AddNewItemBefore(MenuItem^.NextItem,Ident);
  end;
end;

// -----------------------------------------------------------------------------------//
// Adds new DesignerMenuItem after DesignerMenuItem with ID=Ident --------------------//
// -----------------------------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemAfter(MenuItem: PDesignerMenuItem; Ident: string);
var
   new_menuitem,temp_menuitem: PDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  temp_menuitem:=nil;
  if (MenuItem^.ID = Ident) then
  begin
    new(new_menuitem);
    Str(temp_newitemcounter,temp_newitemcounterstring);
    new_menuitem^.Caption:='New Item' + temp_newitemcounterstring;
    new_menuitem^.Level:=MenuItem^.Level;
    new_menuitem^.PrevItem:=MenuItem;
    new_menuitem^.ParentMenu:=nil;
    new_menuitem^.SubMenu:=nil;
    if (MenuItem^.NextItem <> nil) then
    begin
      new_menuitem^.NextItem:=MenuItem^.NextItem;
      MenuItem^.NextItem^.PrevItem:=new_menuitem;
    end else
      new_menuitem^.NextItem:=nil;
    MenuItem^.NextItem:=new_menuitem;
    // now we have to set the index of this DesignerMenuItem
    if (new_menuitem^.NextItem <> nil) then
    begin
      temp_menuitem:=new_menuitem;
      while (temp_menuitem <> nil) do
      begin
        temp_menuitem^.Index:=temp_menuitem^.PrevItem^.Index + 1;
        temp_menuitem:=temp_menuitem^.NextItem;
      end;
    end else
      new_menuitem^.Index:=MenuItem^.Index + 1;
      
    Init(new_menuitem);
  end else
  begin
    if (MenuItem^.SubMenu <> nil) then
      AddNewItemAfter(MenuItem^.SubMenu,Ident);
    if (MenuItem^.NextItem <> nil) then
      AddNewItemAfter(MenuItem^.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//
procedure TDesignerMainMenu.AddSubMenu(MenuItem: PDesignerMenuItem; Ident: string);
var
   new_menuitem: PDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  if ((MenuItem^.ID = Ident) and (MenuItem^.SubMenu = nil)) then
  begin
    new(new_menuitem);
    Str(temp_newitemcounter,temp_newitemcounterstring);
    new_menuitem^.Caption:='New Item' + temp_newitemcounterstring;
    new_menuitem^.Level:=MenuItem^.Level + 1;
    new_menuitem^.PrevItem:=nil;
    new_menuitem^.ParentMenu:=MenuItem;
    new_menuitem^.SubMenu:=nil;
    new_menuitem^.NextItem:=nil;
    MenuItem^.SubMenu:=new_menuitem;
    // now we have to set the index of this DesignerMenuItem
    new_menuitem^.Index:=0;
    Init(MenuItem^.SubMenu);
  end else
  begin
    if (MenuItem^.SubMenu <> nil) then
      AddSubMenu(MenuItem^.SubMenu,Ident);
    if (MenuItem^.NextItem <> nil) then
      AddSubMenu(MenuItem^.NextItem,Ident);
  end;
  
  // DEBUG
  //mi:=TMenuItem.Create(self);
  //mi.Name:='MenuEditor_' + Editor.GetDesigner.CreateUniqueComponentName(mi.ClassName);
  //writeln('Unique Name : ',mi.Name);

end;

// ------------------------------------------------------------------------------//

function TDesignerMainMenu.MoveUp(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
var
  temp_designermenuitem: PDesignerMenuItem;
begin
  Result:=0;
  if (DesignerMenuItem^.ID = Ident) then
  begin
    if (DesignerMenuItem^.PrevItem <> nil) then
    begin
      temp_designermenuitem:=DesignerMenuItem^.PrevItem;
      temp_designermenuitem^.Index:=DesignerMenuItem^.Index;
      temp_designermenuitem^.NextItem:=DesignerMenuItem^.NextItem;
      DesignerMenuItem^.Index:=temp_designermenuitem^.Index - 1;
      DesignerMenuItem^.PrevItem:=temp_designermenuitem^.PrevItem;
      DesignerMenuItem^.ParentMenu:=temp_designermenuitem^.ParentMenu;
      temp_designermenuitem^.ParentMenu:=nil;
      temp_designermenuitem^.PrevItem:=DesignerMenuItem;
      if (DesignerMenuItem^.ParentMenu = nil) and (DesignerMenuItem^.PrevItem = nil) then
        Root:=DesignerMenuItem;
      DesignerMenuItem^.NextItem:=temp_designermenuitem;
      if (DesignerMenuItem^.ParentMenu <> nil) then
        DesignerMenuItem^.ParentMenu^.SubMenu:=DesignerMenuItem;
      if (DesignerMenuItem^.PrevItem <> nil) then
        DesignerMenuItem^.PrevItem^.NextItem:=DesignerMenuItem;
      Result:=1;
    end;
  end else
  begin
    if (DesignerMenuItem^.SubMenu <> nil) then
      Result:=MoveUp(DesignerMenuItem^.SubMenu,Ident);
    if (Result = 0) then
      if (DesignerMenuItem^.NextItem <> nil) then
        Result:=MoveUp(DesignerMenuItem^.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//

function TDesignerMainMenu.MoveDown(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
var
  temp_designermenuitem: PDesignerMenuItem;
begin
  Result:=0;
  if (DesignerMenuItem^.ID = Ident) then
  begin
    if (DesignerMenuItem^.NextItem <> nil) then
    begin
      temp_designermenuitem:=DesignerMenuItem^.NextItem;
      temp_designermenuitem^.PrevItem:=DesignerMenuItem^.PrevItem;
      DesignerMenuItem^.NextItem:=temp_designermenuitem^.NextItem;
      temp_designermenuitem^.NextItem:=DesignerMenuItem;
      DesignerMenuItem^.PrevItem:=temp_designermenuitem;
      temp_designermenuitem^.Index:=DesignerMenuItem^.Index;
      DesignerMenuItem^.Index:=temp_designermenuitem^.Index + 1;
      temp_designermenuitem^.ParentMenu:=DesignerMenuItem^.ParentMenu;
      DesignerMenuItem^.ParentMenu:=nil;
      if (temp_designermenuitem^.ParentMenu = nil) and (temp_designermenuitem^.PrevItem = nil) then
        Root:=temp_designermenuitem;
      if (temp_designermenuitem^.ParentMenu <> nil) then
        temp_designermenuitem^.ParentMenu^.SubMenu:=temp_designermenuitem;
      if (DesignerMenuItem^.NextItem <> nil) then
        DesignerMenuItem^.NextItem^.PrevItem:=DesignerMenuItem;
      if (temp_designermenuitem^.PrevItem <> nil) then
        temp_designermenuitem^.PrevItem^.NextItem:=temp_designermenuitem;
      Result:=1;
    end;
  end else
  begin
    if (DesignerMenuItem^.SubMenu <> nil) then
      Result:=MoveDown(DesignerMenuItem^.SubMenu,Ident);
    if (Result = 0) then
      if (DesignerMenuItem^.NextItem <> nil) then
        Result:=MoveDown(DesignerMenuItem^.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//

function TDesignerMainMenu.DeleteItem(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
begin
end;

// -------------------------------------------------------------------------------------------------------------------
// Finds DesignerMenuItem with identification Ident and returns a pointer to it
// -------------------------------------------------------------------------------------------------------------------
function TDesignerMainMenu.GetDesignerMenuItem(DesignerMenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
begin
  Result:=nil;
  if (DesignerMenuItem^.ID = Ident) then
    Result:=DesignerMenuItem
  else
  begin
    if (DesignerMenuItem^.SubMenu <> nil) then
      Result:=GetDesignerMenuItem(DesignerMenuItem^.SubMenu, Ident);
    if (Result = nil) then
      if (DesignerMenuItem^.NextItem <> nil) then
        Result:=GetDesignerMenuItem(DesignerMenuItem^.NextItem, Ident);
  end;
end;

// ------------------------------------------------
// ------------------------------------------------
// Procedures for updating the Menu of the Designer


procedure TDesignerMainMenu.InitIndexSequence;
var
  i: Integer;
begin
  for i:=1 to INDEX_SEQUENCE_LENGTH do
    index_sequence[i]:=-1;
end;

function TDEsignerMainMenu.CreateIndexSequence(MenuItem: PDesignerMenuItem; Ident: string; Ind: Integer): Boolean;
begin
  Result:=false;
  index_sequence[Ind]:=MenuItem^.Index;
  if (MenuItem^.ID = Ident) then
    Result:=true
  else
  begin
    if (MenuItem^.SubMenu <> nil) then
    begin
      if (CreateIndexSequence(MenuItem^.SubMenu,Ident,Ind + 1)) then
        Result:=true
      else
        index_sequence[Ind + 1]:=-1;
    end;
    if not (Result) then
      if (MenuItem^.NextItem <> nil) then
        if (CreateIndexSequence(MenuItem^.NextItem,Ident,Ind)) then
          Result:=true;
  end;
end;

procedure TDEsignerMainMenu.VypisIndexSequence;
var
  i: Integer;
begin
  for i:=1 to INDEX_SEQUENCE_LENGTH do
    write (index_sequence[i],' ');
  writeln;
end;

// ------------------------------------------------------------------
// UPDATE Menu (type of update is specified via the Action parameter)
// ------------------------------------------------------------------
procedure TDesignerMainMenu.UpdateMenu(MenuItem: TMenuItem; DesignerMenuItem: PDesignerMenuItem; Ind, Action: Integer);
var
  temp_menuitem: TMenuItem;
begin
  case Action of
   // Insert new MenuItem after selected MenuItem
  1: begin
       if (index_sequence[Ind + 1] = -1) then
       begin
         temp_menuitem:=TMenuItem.Create(self);
         temp_menuitem.Caption:=DesignerMenuItem^.Caption;
         MenuItem.Insert(index_sequence[Ind] + 1, temp_menuitem);
       end else
       begin
         UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, Action);
       end;
     end;
   // Insert new MenuItem before selected MenuItem
   2: begin
       if (index_sequence[Ind + 1] = -1) then
       begin
         temp_menuitem:=TMenuItem.Create(self);
         temp_menuitem.Caption:=DesignerMenuItem^.Caption;
         MenuItem.Insert(index_sequence[Ind] - 1, temp_menuitem);
       end else
       begin
         UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, Action);
       end;
      end;
    // Creates SubMenu to an existing MenuItem
    3: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=TMenuItem.Create(self);
           temp_menuitem.Caption:=DesignerMenuItem^.Caption;
           MenuItem[index_sequence[Ind]].Add(temp_menuitem);
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, Action);
       end;
    // Moves Up(left) an MenuItem (changes positions of this MenuItem and its predecesor)
    4: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=MenuItem[index_sequence[Ind] + 1];
           MenuItem.Delete(index_sequence[Ind] + 1);
           MenuItem.Insert(index_sequence[Ind], temp_menuitem);
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, Action)
       end;
    // Moves Down(right) an MenuItem (changes positions of this MenuItem and its ancestor)
    5: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=MenuItem[index_sequence[Ind]];
           MenuItem.Delete(index_sequence[Ind]);
           MenuItem.Insert(index_sequence[Ind] - 1, temp_menuitem);
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, Action)
       end;
  // Sonething else
  end;
end;
 
end.

