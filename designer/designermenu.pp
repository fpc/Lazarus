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
  Classes, SysUtils, LCLProc, Forms, Controls, Menus, Graphics,
  GraphType, Buttons, StdCtrls, ExtCtrls, ComponentEditors, LazConf, ComCtrls,
  Arrow, ButtonPanel, Laz_XMLCfg, LazarusIDEStrConsts, PropEdits, IDEProcs;

const
  // Length of a field for storing index positions of DesignerMenuItem, we use it to find the right MenuItem
  INDEX_SEQUENCE_LENGTH = 100;

type

  { TTemplateMenuForm }

  TTemplateMenuForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Edit_template_description: TEdit;
    Label_template_view: TLabel;
    Label_template_description: TLabel;
    ListBoxView: TListBox;
    TemplatesListBox: TListBox;
    procedure CancelBitBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OKBitBtnClick(Sender: TObject);
    procedure TemplatesListBoxClick(Sender: TObject);
  private
    SelectedMenuTemplate: Integer;
    Description: string;
    subitem_level: Integer;
    fAction: Integer;
  public
    function GetSelectedMenuTemplate: Integer;
    function GetDescription: string;
    procedure TemplateView(templatemenuitem: string; default_template: Integer);
  end;

  PDesignerMenuItem = ^TDesignerMenuItem;
  TDesignerMenuItem = record
    SelfPanel: TPanel;
    SubMenuPanel: TPanel;
    SubMenuArrow: TArrow;
    CaptionLabel: TLabel;
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
  
  { TDesignerMainMenu }

  TDesignerMainMenu = class(TCustomControl)
  private
    fRoot: PDesignerMenuItem;
    fPanel: TPanel;
    fDesignerMenuItemIdent: Integer;
    fParentCanvas: TCanvas;
    fSelectedDesignerMenuItem: string;
    fMenu:TMenu;
    fDefaultComponentEditor: TDefaultComponentEditor;
    FDesignerPopupMenu: TPopupMenu;
    TemplateMenuForm: TTemplateMenuForm;
    temp_panel: TPanel;
    temp_level: Integer;
    temp_newitemcounter: Integer;
    index_sequence: Array[1..INDEX_SEQUENCE_LENGTH] of Integer;
    function GetDesigner: TComponentEditorDesigner;
    procedure SetRoot(const AValue: PDesignerMenuItem);
  protected
    procedure PersistentDeleting(APersistent: TPersistent);
    function SearchItemByPanel(DesignerMenuItem: PDesignerMenuItem; APanel: TPanel): PDesignerMenuItem;
    procedure ClearAllMenus;
  public
    // Constructor and destructor
    constructor CreateWithMenu(AOwner: TComponent; AMenu: TMenu);
    destructor Destroy; override;
    
    // Properties for  accesing private variables
    property Root: PDesignerMenuItem read FRoot write SetRoot;
    property Panel: TPanel read FPanel write FPanel;
    property DesignerMenuItemIdent: Integer read FDesignerMenuItemIdent write FDesignerMenuItemIdent;
    property SelectedDesignerMenuItem: string read FSelectedDesignerMenuItem write FSelectedDesignerMenuItem;
    property ParentCanvas: TCanvas read FParentCanvas write FParentCanvas;
    property Menu: TMenu read fMenu;

    // Loading menu functions and initialization function
    procedure LoadMainMenu;
    procedure Init(MenuItem: PDesignerMenuItem);
    procedure Link(MenuItem: TMenuItem; ParentM: PDesignerMenuItem);
    
    // Draw function and supplementary functions for setting coordinates
    procedure RealignDesigner;
    procedure Draw(MenuItem: PDesignerMenuItem; FormPanel,SubMenuPanel: TPanel); //draw function
    procedure SetCoordinates(Coord_Left,Coord_Top,Coord_Right: Integer;MenuItem: PDesignerMenuItem); //coord. of each designermenuitem
    function GetSubMenuHeight(MenuItem: PDesignerMenuItem; LeftPos,TopPos: Integer; Ident: string): TRect; //width and height of submenu panel
    function GetMaxCoordinates(DesignerMenuItem: PDesignerMenuItem; Max_Width, Max_Height: Integer): TRect; //width and height of all expanded menu items
    
    // Event handling
    procedure MenuItemMouseDown(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    procedure MenuItemDblClick(Sender: TObject);
    procedure AddNewItemBeforeClick(Sender: TObject);
    procedure AddNewItemAfterClick(Sender: TObject);
    procedure AddSubMenuClick(Sender: TObject);
    procedure HandleOnClickEventClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure InsertFromTemplateClick(Sender: TObject);
    procedure SaveAsTemplateClick(Sender: TObject);
    procedure DeleteFromTemplateClick(Sender: TObject);
    procedure OnDesignerModified(Sender: TObject);
    procedure OnComponentAdded(Sender: TObject);

    // Functions for editing menus
    function AddNewItemBefore(MenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
    function AddNewItemAfter(MenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
    function AddSubMenu(MenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
    function MoveUp(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
    function MoveDown(DesignerMenuItem: PDesignerMenuItem; Ident: string): Integer;
    function DeleteItem(DesignerMenuItem: PDesignerMenuItem): Integer;
    function ChangeCaption(DesignerMenuItem: PDesignerMenuItem; const newcaption: string): Integer;
    procedure InsertFromTemplate(Item,Ident: string);
    procedure SaveAsTemplate(Item,Ident: string);
    procedure ReplaceInTemplate(old_Item, new_Item: string);
    function ChangeMenuItem(MenuItem: PDesignerMenuItem; TheAction: Integer;
                           Ident: string): Boolean;
    
    //  Function for updating the real menu (which is the edited one) and supplementary functions for
    //  building a search index which is needed to locate an MenuItem in real menu which has to be
    //  update
    procedure InitIndexSequence;
    function CreateIndexSequence(MenuItem: PDesignerMenuItem; Ident: string; Ind: Integer): Boolean;
    function UpdateMenu(MenuItem: TMenuItem;
           DesignerMenuItem: PDesignerMenuItem; Ind,TheAction: Integer): TMenuItem;
    
    procedure HideDesignerMenuItem(DesignerMenuItem: PDesignerMenuItem);
    function GetDesignerMenuItem(DesignerMenuItem: PDesignerMenuItem; const Ident: string): PDesignerMenuItem;
    function FindDesignerMenuItem(AMenuItem: TMenuItem): PDesignerMenuItem;
  end;
  

implementation

{$R *.lfm}

const
  DESIGNER_MENU_ITEM_HEIGHT=20;
  DESIGNER_MENU_ITEM_SPACE=30;
  MIN_DESIGNER_MENU_ITEM_WIDTH=100;
  DESIGNER_MENU_ITEM_PANEL_HEIGHT=22;
  MIN_SUB_MENU_PANEL_WIDTH=100;
  POSITION_LEFT=10;
  POSITION_TOP=10;
  NUMBER_OF_DEFAULT_TEMPLATES = 3;

  // Name of the file where menu templates are stored
  MenuTemplatesFile='menutemplates.xml';

var 
  TemplateMenuFormCreateAction: Integer;
  XMLConfig: TXMLConfig = nil;

function TDesignerMainMenu.GetDesigner: TComponentEditorDesigner;
begin
  Result:=nil;
  if fMenu=nil then exit;
  Result:=FindRootDesigner(fMenu) as TComponentEditorDesigner;
end;

procedure TDesignerMainMenu.SetRoot(const AValue: PDesignerMenuItem);
begin
  if FRoot <> nil then
  begin
    Dispose(FRoot);
  end;
  FRoot := AValue;
end;

//
constructor TDesignerMainMenu.CreateWithMenu(AOwner: TComponent; aMenu: TMenu);
var
  PopupMenuItem: TMenuItem;
begin
  inherited Create(AOwner);
  
  XMLConfig := TXMLConfig.Create(SetDirSeparators(GetPrimaryConfigPath + '/' + MenuTemplatesFile));
  
  // creates PopupMenu for and its items the menu editor
  FDesignerPopupMenu := TPopupMenu.Create(Self);
  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorInsertNewItemAfter;
  PopupMenuItem.OnClick := @AddNewItemAfterClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorInsertNewItemBefore;
  PopupMenuItem.OnClick := @AddNewItemBeforeClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorDeleteItem;
  PopupMenuItem.OnClick := @DeleteItemClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);
  
  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := '-';
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorCreateSubMenu;
  PopupMenuItem.OnClick := @AddSubMenuClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := '-';
  FDesignerPopupMenu.Items.Add(PopupMenuItem);
  
  PopupMenuItem:=TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorHandleOnClickEvent;
  PopupMenuItem.OnClick := @HandleOnCLickEventClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(Self);
  PopupMenuItem.Caption := '-';
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorMoveUp;
  PopupMenuItem.OnClick := @MoveUpClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);
  
  PopupMenuItem:=TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorMoveDown;
  PopupMenuItem.OnClick := @MoveDownClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);
  
  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := '-';
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorInsertFromTemplate;
  PopupMenuItem.OnClick := @InsertFromTemplateClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorSaveAsTemplate;
  PopupMenuItem.OnClick := @SaveAsTemplateClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(Self);
  PopupMenuItem.Caption := lisMenuEditorDeleteFromTemplate;
  PopupMenuItem.OnClick := @DeleteFromTemplateClick;
  FDesignerPopupMenu.Items.Add(PopupMenuItem);

  //Handler for renaming a caption in the OI for some menuitem to rename also a
  // designermenuitem
  GlobalDesignHook.AddHandlerModified(@OnDesignerModified);
  //GlobalDesignHook.AddHandlerPersistentAdded(@OnComponentAdded);
  GlobalDesignHook.AddHandlerPersistentDeleting(@PersistentDeleting);

  New(FRoot);
  FillChar(FRoot^, SizeOf(FRoot^), #0);
  fMenu := AMenu;

  temp_level := 1;
  temp_newitemcounter := 1;
  
  temp_panel := TPanel.Create(Self);
  temp_panel.Visible := False;
end;

destructor TDesignerMainMenu.Destroy;
begin
  if GlobalDesignHook<>nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  ClearAllMenus;
  FreeAndNil(XMLConfig);
  inherited Destroy;
end;

// ------------------------------------------------------------------------------------//
// Initialize new DesignerMenuItem (sets ID, defaluts values, creates SelfPanel ...) --//
// ------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.Init(MenuItem: PDesignerMenuItem);
var temp: Integer;
begin

  temp:=DesignerMenuItemIdent;
  Str(temp, MenuItem^.ID);
  
  MenuItem^.Selected:=false;
  MenuItem^.Active:=false;
  
  MenuItem^.coord.Left:=0;
  MenuItem^.coord.Top:=0;
  MenuItem^.coord.Right:=0;
  MenuItem^.coord.Bottom:=0;
  
  MenuItem^.SelfPanelCreated:=false;
  MenuItem^.SubMenuPanelCreated:=false;
  
  MenuItem^.SubMenuPanel:=TPanel.Create(self);
  
  MenuItem^.SelfPanel:=TPanel.Create(self);
  MenuItem^.SelfPanel.Name:='SelfPanel_' + MenuItem^.ID;
  MenuItem^.SelfPanel.Caption:='';
  MenuItem^.SelfPanel.Height:=DESIGNER_MENU_ITEM_HEIGHT;
  Menuitem^.SelfPanel.OnMouseDown:=@MenuItemMouseDown;
  Menuitem^.SelfPanel.OnDblClick:=@MenuItemDblClick;
  MenuItem^.SelfPanel.PopupMenu := FDesignerPopupMenu;

  MenuItem^.CaptionLabel:=TLabel.Create(self);
  MenuItem^.CaptionLabel.Name:='CaptionLabel_' + MenuItem^.ID;
  MenuItem^.CaptionLabel.Parent:=MenuItem^.SelfPanel;
  MenuItem^.CaptionLabel.Left:=MenuItem^.SelfPanel.Left + 5;
  MenuItem^.CaptionLabel.Top:=2;
  MenuItem^.CaptionLabel.Height:=DESIGNER_MENU_ITEM_HEIGHT - 4;
  MenuItem^.CaptionLabel.OnMouseDown:=@MenuItemMouseDown;
  Menuitem^.CaptionLabel.OnDblClick:=@MenuItemDblClick;

  MenuItem^.SubMenuArrow:=TArrow.Create(self);
  MenuItem^.SubMenuArrow.Name:='SubMenuArrow_' + MenuItem^.ID;
  MenuItem^.SubMenuArrow.Parent:=MenuItem^.SelfPanel;
  MenuItem^.SubMenuArrow.ArrowType:=atright;
  MenuItem^.SubMenuArrow.Width:=20;
  MenuItem^.SubMenuArrow.Height:=13;
  MenuItem^.SubMenuArrow.ShadowType:=stout;
  MenuItem^.SubMenuArrow.Visible:=false;
  MenuItem^.SubMenuArrow.OnMouseDown:=@MenuItemMouseDown;
  Menuitem^.SubMenuArrow.OnDblClick:=@MenuItemDblClick;

  DesignerMenuItemIdent:=DesignerMenuItemIdent + 1;
  inc(temp_newitemcounter);
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
    firstmenuitem := TMenuItem.Create(fMenu.Owner);
    firstmenuitem.Name := GetDesigner.CreateUniqueComponentName(firstmenuitem.ClassName);
    firstmenuitem.Caption := 'New Item1';

    fMenu.Items.Insert(0, firstmenuitem);
    GetDesigner.PropertyEditorHook.PersistentAdded(firstmenuitem, true);
    GetDesigner.Modified;
  end;

  prevtemp := nil;
  for i := 0 to fMenu.Items.Count - 1 do
  begin
    new(temp);
    temp^.Caption := fMenu.Items[i].Caption;
    temp^.Level := temp_level;
    temp^.NextItem := nil;
    temp^.SubMenu := nil;
    temp^.ParentMenu := nil;
    temp^.Index := i;
    if (i = 0) then
    begin
      temp^.PrevItem := nil;
      Root := temp;
    end else
    begin
      temp^.PrevItem := prevtemp;
      prevtemp^.NextItem := temp;
    end;
    Init(temp);
    prevtemp := temp;
    Link(fMenu.Items[i], temp);
  end;
  Root^.Selected := True;
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

procedure TDesignerMainMenu.RealignDesigner;
var
  temp_coord: TRect;
begin
  temp_coord:=GetMaxCoordinates(Root, 0, 0);
  Panel.Width:=temp_coord.Right + 10;
  Panel.Height:=temp_coord.Bottom + 10;
  Draw(Root, Panel, Panel);
end;

//------------------------------------------------------------------------------------------//
// Draw the the whole DesignerMenu with active MenuItems and SubMenus ----------------------//
//------------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.Draw(MenuItem: PDesignerMenuItem; FormPanel,SubMenuPanel: TPanel);
var
  SubMenuDimensions: TRect;
begin
  if MenuItem^.SelfPanel = nil then exit;
  with MenuItem^.SelfPanel do
  begin
    if (fMenu is TPopupMenu) and (MenuItem^.Level = 1) then
    begin
      if (MenuItem^.PrevItem = nil) then
      begin
        SubMenuDimensions:=GetSubMenuHeight(Root, 0, 0, MenuItem^.ID);
        with temp_panel do
        begin
          Parent:=FormPanel;
          Visible:=true;
          Left:=POSITION_LEFT;
          Top:=POSITION_TOP;
          Width:=SubMenuDimensions.right;
          Height:=SubMenuDimensions.bottom;
        end;
      end;
      Parent:=temp_panel;
    end else
      Parent:=FormPanel;
      
    Visible:=true;
    if (MenuItem^.Level > 1) and (fMenu is TMainMenu) then
    begin
      Left:=2;
      Top:=MenuItem^.coord.Top - Parent.Top + 2;
      Width:=Parent.width - 4;
      Height:=DESIGNER_MENU_ITEM_HEIGHT;
    end else
    begin
      Top:=MenuItem^.coord.Top - Parent.Top + 2;
      Height:=DESIGNER_MENU_ITEM_HEIGHT;
      if (fMenu is TPopupMenu) then
      begin
        Width:=Parent.width - 4;
        Left:=2;
      end else
      begin
        Left:=MenuItem^.coord.Left;
        Width:=ParentCanvas.TextWidth(MenuItem^.Caption) + DESIGNER_MENU_ITEM_SPACE;
      end;
    end;
    if (MenuItem^.Selected) or ((MenuItem^.Level = 1) and (fMenu is TMainMenu)) then
    begin
      Bevelouter:=bvraised;
    end else
    begin
      Bevelouter:=bvnone;
    end;
  end;
  
  MenuItem^.CaptionLabel.Caption:=MenuItem^.Caption;
  
  if (MenuItem^.NextItem <> nil) then Draw(MenuItem^.NextItem, FormPanel, SubMenuPanel);
  with MenuItem^.SelfPanel do
  begin
    if ((MenuItem^.SubMenu <> nil) and
      (((MenuItem^.Selected) and ((MenuItem^.Submenu^.Selected = False) or (MenuItem^.SubMenu^.Active = False))) or
      ((MenuItem^.SubMenu^.Selected) or (MenuItem^.SubMenu^.Active)))) then
    begin
      if (fMenu is TpopupMenu) and (MenuItem^.Level = 1) then
        SubMenuDimensions:=GetSubMenuHeight(GetDesignerMenuItem(Root, MenuItem^.SubMenu^.ID), MenuItem^.coord.right + 1, 0, MenuItem^.SubMenu^.ID)
      else
        SubMenuDimensions:=GetSubMenuHeight(GetDesignerMenuItem(Root, MenuItem^.SubMenu^.ID), 0, 1, MenuItem^.SubMenu^.ID);
      with MenuItem^.SubMenuPanel do
      begin
        Parent:=SubMenuPanel;
        Visible:=true;
        if (fMenu is TMainMenu) and (MenuItem^.Level = 1) then
        begin
          Left:=MenuItem^.coord.Left;
          Top:=MenuItem^.coord.Bottom + 1;
        end else
        begin
          Left:=MenuItem^.coord.Right - 4;
          Top:=MenuItem^.coord.Top + 4;
        end;
        Width:=SubMenuDimensions.right;
        Height:=SubMenuDimensions.bottom;
      end;
    end;
  end;
  
  if (MenuItem^.SubMenu <> nil) then
  begin
    if (MenuItem^.Level = 1) and (fMenu is TMainMenu) then
    begin
      MenuItem^.SubMenuArrow.ArrowType:=atdown;
    end else
    begin
      MenuItem^.SubMenuArrow.ArrowType:=atright;
    end;
    MenuItem^.SubMenuArrow.Left:=MenuItem^.SelfPanel.Width - MenuItem^.SubMenuArrow.Width - 1;
    MenuItem^.SubMenuArrow.Top:=(MenuItem^.SelfPanel.Height - MenuItem^.SubMenuArrow.Height) div 2;
    MenuItem^.SubMenuArrow.Visible:=true;
  end else
  begin
    MenuItem^.SubMenuArrow.Left:=MenuItem^.SelfPanel.Width - MenuItem^.SubMenuArrow.Width - 1;
    MenuItem^.SubMenuArrow.Top:=(MenuItem^.SelfPanel.Height - MenuItem^.SubMenuArrow.Height) div 2;
    MenuItem^.SubMenuArrow.Visible:=false;
  end;

  MenuItem^.CaptionLabel.Width:=MenuItem^.SelfPanel.Width - 10;
  if ((MenuItem^.SubMenu <> nil) and ((MenuItem^.Selected) or (MenuItem^.Active))) then
  begin
    if ((((MenuItem^.Submenu^.Active=False) or (MenuItem^.SubMenu^.Selected=False)) and (MenuItem^.Selected)) or
      ((MenuItem^.Submenu^.Active) or (MenuItem^.SubMenu^.Selected))) then
      Draw(MenuItem^.SubMenu, MenuItem^.SubMenuPanel,SubMenuPanel);
  end;
end;


// --------------------------------------------------------------------------------------------------------------//
// Set the coordinates (position) of each DesignerMenuItem ------------------------------------------------------//
// --------------------------------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.SetCoordinates(Coord_Left,Coord_Top,Coord_Right: Integer; MenuItem: PDesignerMenuItem);
var
  temp_menuitem: PDesignerMenuItem;
begin
  MenuItem^.coord.Left:=Coord_Left;
  MenuItem^.coord.Top:=Coord_Top;
  MenuItem^.coord.Bottom:=MenuItem^.coord.top + DESIGNER_MENU_ITEM_HEIGHT;   // Bylo DESIGNER_MENU_ITEM_PANEL_HEIGHT
  
  if (MenuItem^.Level = 1) and (fMenu is TPopupMenu) then
    Coord_Right:=MIN_DESIGNER_MENU_ITEM_WIDTH;
  
  if (MenuItem^.Level = 1) and (fMenu is TMainMenu) then
  begin
    MenuItem^.coord.Right:=MenuItem^.coord.Left + Canvas.TextWidth(MenuItem^.Caption) + DESIGNER_MENU_ITEM_SPACE;
  end else
  begin
    // is this DesignerMenuItem wider than its predecessors?
    if (Canvas.TextWidth(MenuItem^.Caption) + DESIGNER_MENU_ITEM_SPACE > Coord_Right) then
    begin
      MenuItem^.coord.right:=MenuItem^.coord.Left + Canvas.TextWidth(MenuItem^.Caption) + DESIGNER_MENU_ITEM_SPACE;
      Coord_Right:=MenuItem^.coord.Right - MenuItem^.coord.Left;
      // we have to set the width of all predecessors of this DesignerMenuItem to its size
      temp_menuitem:=MenuItem;
      while (temp_menuitem^.PrevItem <> nil) do
      begin
        temp_menuitem:=temp_menuitem^.PrevItem;
        temp_menuitem^.coord.right:=MenuItem^.coord.right;
      end;
    // if not wider then keep size of the predecessor
    end else
    begin
      MenuItem^.coord.right:=MenuItem^.coord.Left + Coord_Right;
      Coord_Right:=MenuItem^.coord.Right - MenuItem^.coord.Left;
    end;
  end;
  
  if (MenuItem^.SubMenu <> nil) then
  begin
    if (fMenu is TMainMenu) and (MenuItem^.Level = 1) then
      SetCoordinates(MenuItem^.coord.Left, MenuItem^.coord.Bottom + 1, MIN_DESIGNER_MENU_ITEM_WIDTH, MenuItem^.SubMenu)
    else
      SetCoordinates(MenuItem^.coord.Right - 4, MenuItem^.coord.Top + 4, MIN_DESIGNER_MENU_ITEM_WIDTH, MenuItem^.SubMenu)
  end;
  if (MenuItem^.NextItem <> nil) then 
  begin
    if (MenuItem^.Level = 1) and (fMenu is TMainMenu) then
      SetCoordinates(MenuItem^.coord.Right + 1, POSITION_TOP, 0,MenuItem^.NextItem)
    else
      SetCoordinates(MenuItem^.coord.left, MenuItem^.coord.Bottom, Coord_Right, MenuItem^.NextItem);
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

  coords.right:=MenuItem^.coord.Right - MenuItem^.coord.Left + 4;
  // sets the bottom coordinate of submenupanel depending on number of submenuitems
  number_of_submenu_items:=1;
  while(MenuItem^.NextItem <> nil) do
  begin
    inc(number_of_submenu_items);
    MenuItem:=MenuItem^.NextItem;
  end;
  coords.bottom:=number_of_submenu_items * DESIGNER_MENU_ITEM_HEIGHT + 5;
  // return coordinates
  GetSubMenuHeight:=coords;
end;

// -------------------------------------------------------------------------------------------------------------------//
// Determines a position of the SubMenuPanel of some DesignerMenuItem ------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------//

function TDesignerMainMenu.GetMaxCoordinates(DesignerMenuItem: PDesignerMenuItem; Max_Width, Max_Height: Integer): TRect;
var
  temp_coord: TRect;
begin
  Result:=Rect(0,0,0,0);
  if (DesignerMenuItem^.coord.Right > Max_Width) then
    Max_Width:=DesignerMenuItem^.coord.Right;
  if (DesignerMenuItem^.coord.Bottom > Max_Height) then
    Max_Height:=DesignerMenuItem^.coord.Bottom;
  if (DesignerMenuItem^.SubMenu = nil) and (DesignerMEnuItem^.NextItem = nil) then
  begin
    Result.Right:=Max_Width;
    Result.Bottom:=Max_Height;
    exit;
  end;
  if DesignerMenuItem^.SubMenu <> nil then
  begin
    temp_coord:=GetMaxCoordinates(DesignerMenuItem^.SubMenu, Max_Width, Max_Height);
    Max_Width:=temp_coord.Right;
    Max_Height:=temp_coord.Bottom;
  end;
  if (DesignerMenuItem^.NextItem <> nil) then
    temp_coord:=GetMaxCoordinates(DesignerMenuItem^.NextItem, Max_Width, Max_Height);
    
  Result:=temp_coord;
end;

// --------------------------------------------------------
// Function that changes MenuItem (Remove, Add SubMenu ...)
// --------------------------------------------------------
function TDesignerMainMenu.ChangeMenuItem(MenuItem: PDesignerMenuItem;
  TheAction: Integer; Ident: string): Boolean;
var
  completed: boolean;
begin
  completed:=false;
  case TheAction of
  // Test if this MenuItem has been selected
  1: begin
       if (MenuItem^.ID = Ident) then
       begin
         MenuItem^.Selected:=true;
         MenuItem^.Active:=false;
         completed:=true;
       end else begin
         MenuItem^.Selected:=false;
       end;
         
       if (MenuItem^.SubMenu <> nil) then
       begin
         if (ChangeMenuItem(MenuItem^.SubMenu,TheAction,Ident) = true) then
         begin
           MenuItem^.Active:=true;
           completed:=true;
         end else MenuItem^.Active:=false;
       end;
       if (MenuItem^.NextItem <> nil) then
       begin
        if (ChangeMenuItem(MenuItem^.NextItem,TheAction,Ident)= true) then
        begin
          if MenuItem^.Level > 1 then
          begin
            MenuItem^.Active := True;
            completed := true;
          end;
        end;
       end;
     end;
  // Destroy all created panels of this MenuItem
  2: begin
       MenuItem^.Active := False;
       MenuItem^.Selected := False;

       
       if (MenuItem^.SubMenu<> nil) then
       begin
         ChangeMenuItem(MenuItem^.SubMenu,TheAction,MenuItem^.SubMenu^.ID);
         MenuItem^.SubMenuPanel.visible:=false;
       end;
       
       
       if (MenuItem^.NextItem <> nil) then
         ChangeMenuItem(MenuItem^.NextItem,TheAction,MenuItem^.NextItem^.ID);
         
       MenuItem^.SelfPanel.visible:=false;
     end;
  end;
  ChangeMenuItem := completed;
end;

// -------------------------------------------------------------------------------------------------------------------//
// We have clicked on some DesignerMenuItem --------------------------------------------------------------------------//
// -------------------------------------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.MenuItemMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DesignerItem: PDesignerMenuItem;
begin
  ChangeMenuItem(Root, 2, Root^.ID);
  InitIndexSequence;
  if (Sender is TPanel) then
    DesignerItem := SearchItemByPanel(Root, TPanel(Sender))
  else
  if (Sender is TLabel) then
    DesignerItem := SearchItemByPanel(Root, TPanel(TLabel(Sender).Parent))
  else
  if (Sender is TArrow) then
    DesignerItem := SearchItemByPanel(Root, TPanel(TArrow(Sender).Parent))
  else
    DesignerItem := nil;

  if DesignerItem <> nil then
  begin
    SelectedDesignerMenuItem := DesignerItem^.ID;
    ChangeMenuItem(Root, 1, SelectedDesignerMenuItem);
    CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
    UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 9);

    RealignDesigner;
  end;
end;

procedure TDesignerMainMenu.MenuItemDblClick(Sender: TObject);
begin
  HandleOnClickEventClick(Sender);
end;

// -------------------------------------------------------------//
// New Item (before) has been selected from context menu -------//
// -------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemBeforeClick(Sender: TObject);
var
  NewItem: PDesignerMenuItem;
begin
  NewItem := AddNewItemBefore(Root, SelectedDesignerMenuItem);
  if NewItem = nil then
    Exit;
   
  NewItem^.Active := True; // set visible
  SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);

  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.PrevItem, 1, 2);

  RealignDesigner;
end;

// ------------------------------------------------------------//
// New Item (after) has been selected from context menu -------//
// ------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemAfterClick(Sender: TObject);
var
  NewItem: PDesignerMenuItem;
begin
  NewItem := AddNewItemAfter(Root, SelectedDesignerMenuItem);
  if NewItem = nil then
    Exit;
   
  NewItem^.Active := True; // set visible
  SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root); 

  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);  
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.NextItem, 1, 1);

  RealignDesigner;
end;

// ------------------------------------------------------------//
// Add SubMenu has been selected from context menu ------------//
// ------------------------------------------------------------//
procedure TDesignerMainMenu.AddSubMenuClick(Sender: TObject);
var
  NewItem: PDesignerMenuItem;
begin
  NewItem := AddSubMenu(Root, SelectedDesignerMenuItem);
  if NewItem = nil then
    Exit;
   
  NewItem^.Active := True; // set visible
  SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);

  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem,1);
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.SubMenu, 1, 3);

  RealignDesigner;
end;

// -----------------------------------------------------------------------//
// "Handle OnClick Event" has been selected from context menu ------------//
// -----------------------------------------------------------------------//
procedure TDesignerMainMenu.HandleOnClickEventClick(Sender: TObject);
var
  temp_menuitem: TMenuItem;
begin
  temp_menuitem:=UpdateMenu(fMenu.Items,
                    GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 10);
  fDefaultComponentEditor:=
             TDefaultComponentEditor.Create(temp_menuitem, GetDesigner);
  fDefaultComponentEditor.Edit;
  fDefaultComponentEditor.Free;
end;

// ----------------------------------------------------//
// Move Up has been selected from context menu --------//
// ----------------------------------------------------//
procedure TDesignerMainMenu.MoveUpClick(Sender: TObject);
begin
  if (MoveUp(Root, SelectedDesignerMenuItem) > 0) then
  begin
    SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);

    InitIndexSequence;
    CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
    
    UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 4);

    RealignDesigner;
  end;
end;

// --------------------------------------------------------//
// Delete Item has been selected from context menu --------//
// --------------------------------------------------------//
procedure TDesignerMainMenu.DeleteItemClick(Sender: TObject);
var
  temp_returnvalue: Integer;
  Item, NextSelectedItem: PDesignerMenuItem;
begin
  //SelectedDesignerMenuItem:=GetSelectedDesignerMenuItem(Root);
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
  Item := GetDesignerMenuItem(Root, SelectedDesignerMenuItem);

  // look for next selected item
  NextSelectedItem := Item^.NextItem;
  if NextSelectedItem = nil then
    NextSelectedItem := Item^.PrevItem;
  if NextSelectedItem = nil then
    NextSelectedItem := Item^.ParentMenu;
  if NextSelectedItem = nil then
    NextSelectedItem := Root;
    
  temp_returnvalue := DeleteItem(Item);
  if (temp_returnvalue > 0) then
  begin
    SelectedDesignerMenuItem := NextSelectedItem^.ID;
    
    ChangeMenuItem(Root, 2, Root^.ID);
    SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);

    if (temp_returnvalue = 1) then
      UpdateMenu(fMenu.Items, nil, 1, 7);
    if (temp_returnvalue = 2) then
      UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1 , 8);
    ChangeMenuItem(Root, 1, SelectedDesignerMenuItem);    

    RealignDesigner;
  end;
end;

procedure TDesignerMainMenu.PersistentDeleting(APersistent: TPersistent);
var
  MenuItem: TMenuItem absolute APersistent;
  Item: PDesignerMenuItem;
begin
  if APersistent is TMenuItem then
  begin
    Item := FindDesignerMenuItem(MenuItem);
    // how we can compare them?
    if (Item <> nil) and (Item^.Caption = MenuItem.Caption) then
    begin
      DeleteItem(Item);
      SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);
      RealignDesigner;
    end;
  end;
  inherited;
end;

function TDesignerMainMenu.SearchItemByPanel(
  DesignerMenuItem: PDesignerMenuItem; APanel: TPanel): PDesignerMenuItem;
begin
  if DesignerMenuItem <> nil then
  begin
    if DesignerMenuItem^.SelfPanel = APanel then
    begin
      Result := DesignerMenuItem;
    end else
    begin
      Result := SearchItemByPanel(DesignerMenuItem^.SubMenu, APanel);
      if Result = nil then
        Result := SearchItemByPanel(DesignerMenuItem^.NextItem, APanel);
    end;
  end else
    Result := nil;
end;

procedure TDesignerMainMenu.ClearAllMenus; 

  procedure DeleteRecursive(var AMenu: PDesignerMenuItem);
  begin
    if not Assigned(AMenu) then Exit;
    if Assigned(AMenu^.NextItem) then DeleteRecursive(AMenu^.NextItem);
    if Assigned(AMenu^.SubMenu) then DeleteRecursive(AMenu^.SubMenu);   
    Dispose(AMenu);
    AMenu:=nil;
  end;

begin
  DeleteRecursive(fRoot);
end;

// -----------------------------------------------------------------//
// Insert From Template has been selected from context menu --------//
// -----------------------------------------------------------------//
procedure TDesignerMainMenu.InsertFromTemplateClick(Sender: TObject);
var
  templatemenuitem: string;
  temp_designermenuitem: PDesignerMenuItem;
begin
  TemplateMenuFormCreateAction:=1;
  TemplateMenuForm:=TTemplateMenuForm.Create(self);
  
  if (TemplateMenuForm.ShowModal = mrOK) then
  begin
  
    if (GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.SubMenu <> nil) then
    begin
      HideDesignerMenuItem(GetDesignerMenuItem(Root, SelectedDesignerMenuItem));
      GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.SubMenu:=nil;
      GetDesignerMenuItem(Root, SelectedDesignerMenuItem)^.SubMenuPanel.Visible:=false;
      InitIndexSequence;
      CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
      UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 8);
    end;
    
    if (TemplateMenuForm.GetSelectedMenuTemplate > NUMBER_OF_DEFAULT_TEMPLATES) then
    begin
      Str(TemplateMenuForm.GetSelectedMenuTemplate - NUMBER_OF_DEFAULT_TEMPLATES, templatemenuitem);
      templatemenuitem:='menu_' + templatemenuitem;
      InitIndexSequence;
      CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
      ChangeCaption(GetDesignerMenuItem(Root, SelectedDesignerMenuItem), XMLConfig.Getvalue(templatemenuitem + '/Name/Value', ''));
      UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 6);
      InsertFromTemplate(templatemenuitem, SelectedDesignerMenuItem);
    end else
    // Some of default templates has been selected
    begin
      temp_designermenuitem:=GetDesignerMenuItem(Root, SelectedDesignerMenuItem);
      case TemplateMenuForm.GetSelectedMenuTemplate of
      1: Begin
           // Change a caption of selected designermenuitem fo "File"
           ChangeCaption (temp_designermenuitem, lisMenuTemplateFile);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add an submenu with first item and set it's caption to "New"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddSubMenuClick(self);
           temp_designermenuitem:=temp_designermenuitem^.SubMenu;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateNew);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Open"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateOpen);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Open Recent"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateOpenRecent);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Save"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateSave);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Save As"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateSaveAs);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Close"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateClose);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Exit"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateExit);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
         end;
      2: begin
           // Change a caption of selected designermenuitem fo "Edit"
           ChangeCaption (temp_designermenuitem, lisMenuTemplateEdit);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add an submenu with first item and set it's caption to "Undo"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddSubMenuClick(self);
           temp_designermenuitem:=temp_designermenuitem^.SubMenu;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateUndo);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Redo"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateRedo);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Cut"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateCut);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Copy"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateCopy);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Paste"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplatePaste);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Find"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateFind);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Find Next"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateFindNext);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
         end;
      3: begin
           // Change a caption of selected designermenuitem fo "Help"
           ChangeCaption (temp_designermenuitem, lisMenuTemplateHelp);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add an submenu with first item and set it's caption to "Contents"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddSubMenuClick(self);
           temp_designermenuitem:=temp_designermenuitem^.SubMenu;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateContents);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Tutorial"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateTutorial);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "About"
           SelectedDesignerMenuItem:=temp_designermenuitem^.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem^.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateAbout);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6 );
         end;
      end;
    end;
  end;
end;

procedure TDesignerMainMenu.InsertFromTemplate(Item,Ident: string);
var
  i: Integer;
  templatesubmenuitem,str_i: string;
  tempdesignermenuitem: PDesignerMenuItem;
begin
  InitIndexSequence;
  CreateIndexSequence(Root, GetDesignerMenuItem(Root, Ident)^.ID,1);
  ChangeCaption(GetDesignerMenuItem(Root, Ident), XMLConfig.Getvalue(Item + '/Name/Value',''));
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, Ident), 1, 6);
  if (XMLConfig.GetValue(Item + '/SubItems/Value','') = 'true') then
  begin
    i:=1;
    Str(i,str_i);
    templatesubmenuitem:=Item + '/subitem_' + str_i;
    while(XMLConfig.Getvalue(templatesubmenuitem + '/Name/Value','does_not_exist') <> 'does_not_exist') do
    begin
      if (i=1) then
      begin
        tempdesignermenuitem:=AddSubMenu(Root, GetDesignerMenuItem(Root, Ident)^.ID);
        SetCoordinates(POSITION_LEFT,POSITION_TOP,0,Root);
        ChangeCaption(tempdesignermenuitem, XMLConfig.GetValue(templatesubmenuitem + '/Name/Value',''));
        InitIndexSequence;
        CreateIndexSequence(Root, tempdesignermenuitem^.ParentMenu^.ID,1);
        UpdateMenu(fMenu.Items, tempdesignermenuitem, 1, 3);
      end else
      begin
        tempdesignermenuitem:=AddNewItemAfter(Root, tempdesignermenuitem^.ID);
        SetCoordinates(POSITION_LEFT,POSITION_TOP,0,Root);
        ChangeCaption(tempdesignermenuitem,XMLConfig.GetValue(templatesubmenuitem + '/Name/Value',''));
        InitIndexSequence;
        CreateIndexSequence(Root, tempdesignermenuitem^.PrevItem^.ID,1);
        UpdateMenu(fMenu.Items, tempdesignermenuitem, 1, 1);
      end;
      InsertFromTemplate(templatesubmenuitem,tempdesignermenuitem^.ID);
      Inc(i);
      Str(i,str_i);
      templatesubmenuitem:=Item + '/subitem_' + str_i;
    end;
  end else
  begin
  end;
  RealignDesigner;
end;

// -----------------------------------------------------------------//
// Save As Template has been selected from context menu ------------//
// -----------------------------------------------------------------//

procedure TDesignerMainMenu.SaveAsTemplateClick(Sender: TObject);
var
  i: Integer;
  str_i, templatemenuitem: string;
begin
  i:=1;
  Str(i,str_i);
  templatemenuitem:='menu_' + str_i;

  TemplateMenuFormCreateAction:=2;
  TemplateMenuForm:=TTemplateMenuForm.Create(self);
  
  if (TemplateMenuForm.ShowModal = mrOK) then
  begin
    while (XMLConfig.GetValue(templatemenuitem + '/Name/Value','does_not_exists') <> 'does_not_exists') do
    begin
      Inc(i);
      Str(i,str_i);
      templatemenuitem:='menu_' + str_i;
    end;
    XMLConfig.SetValue(templatemenuitem + '/Description/Value', TemplateMenuForm.GetDescription);
    SaveAsTemplate(templatemenuitem, SelectedDesignerMenuItem);
    InvalidateFileStateCache;
    XMLConfig.Flush;
  end;
end;


procedure TDesignerMainMenu.SaveAsTemplate(Item,Ident: string);
var
  i: Integer;
  str_i: string;
  temp_designermenuitem,temp_designersubmenuitem: PDesignerMenuItem;
begin
  i:=1;
  Str(i, str_i);
  temp_designermenuitem:=GetDesignerMenuItem(Root, Ident);
  XMLConfig.SetValue(Item + '/Name/Value', temp_designermenuitem^.Caption);
  temp_designersubmenuitem:=temp_designermenuitem^.SubMenu;
  if (temp_designermenuitem^.SubMenu <> nil) then
    XMLConfig.SetValue(Item + '/SubItems/Value', 'true');
  while (temp_designersubmenuitem <> nil) do
  begin
    XMLConfig.SetValue(Item + '/subitem_' + str_i + '/Name/Value', temp_designersubmenuitem^.Caption);
    SaveAsTemplate(Item + '/subitem_' + str_i, temp_designersubmenuitem^.ID);
    temp_designersubmenuitem:=temp_designersubmenuitem^.NextItem;
    Inc(i);
    Str(i, str_i);
  end;
end;

// -----------------------------------------------------------------//
// Delete From Template has been selected from context menu ------------//
// -----------------------------------------------------------------//

procedure TDesignerMainMenu.DeleteFromTemplateClick(Sender: TObject);
var
  i,j: Integer;
  str_i,str_j, old_templatemenuitem, new_templatemenuitem: string;
begin
  //SelectedDesignerMenuItem:=GetSelectedDesignerMenuItem(Root);
  i:=1;
  j:=1;

  TemplateMenuFormCreateAction:=3;
  TemplateMenuForm:=TTemplateMenuForm.Create(self);
  
  if (TemplateMenuForm.ShowModal = mrOK) and (TemplateMenuForm.GetSelectedMenuTemplate > 0) then
  begin
    i:=TemplateMenuForm.GetSelectedMenuTemplate;
    Str(i + 1, new_templatemenuitem);
    new_templatemenuitem:='menu_' + new_templatemenuitem;
    
    if (XMLConfig.GetValue(new_templatemenuitem + '/Name/Value', 'does_not_exists') <> 'does_not_exists') then
    begin
    
      i:=TemplateMenuForm.GetSelectedMenuTemplate;
      Str(i, new_templatemenuitem);
      new_templatemenuitem:='menu_' + new_templatemenuitem;

    
      // This deletes all subitems in menuitem, which will be replaced
      str_i:='1';
      while (XMLConfig.GetValue(new_templatemenuitem + '/subitem_' + str_i + '/Name/Value', 'does_not_exists') <> 'does_not_exists') do
      begin
        XMLConfig.DeletePath(new_templatemenuitem + '/subitem_' + str_i);
        Inc(i);
        Str(i, str_i);
      end;

      i:=TemplateMenuForm.GetSelectedMenuTemplate;
      Str(i + 1, old_templatemenuitem);
      old_templatemenuitem:='menu_' + old_templatemenuitem;
    

      while (XMLConfig.GetValue(old_templatemenuitem + '/Name/Value', 'does_not_exists') <> 'does_not_exists') do
      begin
        // This deletes all subitems in menuitem, which will be replaced
        str_j:='1';
        while (XMLConfig.GetValue(new_templatemenuitem + '/subitem_' + str_j + '/Name/Value', 'does_not_exists') <> 'does_not_exists') do
        begin
          XMLConfig.DeletePath(new_templatemenuitem + '/subitem_' + str_j);
          Inc(j);
          Str(j, str_j);
        end;
        // (Re)place the old menuitem on other place(on other menuitem) in the menutemplates.xml file
        ReplaceInTemplate(old_templatemenuitem, new_templatemenuitem);
      
        Inc(i);
        Str(i, new_templatemenuitem);
        new_templatemenuitem:='menu_' + new_templatemenuitem;
        Str(i + 1, old_templatemenuitem);
        old_templatemenuitem:='menu_' + old_templatemenuitem;
      end;
      
      XMLConfig.DeletePath(new_templatemenuitem);
      
     end else
     begin
       i:=TemplateMenuForm.GetSelectedMenuTemplate;
       Str(i, old_templatemenuitem);
       old_templatemenuitem:='menu_' + old_templatemenuitem;
       XMLConfig.DeletePath(old_templatemenuitem);
     end;
    InvalidateFileStateCache;
    XMLConfig.Flush;
  end;
end;

procedure TDesignerMainMenu.ReplaceInTemplate(old_Item, new_Item: string);
var
  i: Integer;
  str_i: string;
begin
  //DebugLn('Old Item: ',old_Item);
  //DebugLn('New Item: ',new_Item);
  //DebugLn('Tak se na to mrknem: ', XMLConfig.GetValue(old_Item + '/Name/Value',''));

  XMLConfig.SetValue(new_Item + '/Name/Value', XMLConfig.GetValue(old_Item + '/Name/Value',''));
  if (XMLConfig.GetValue(old_Item + '/Description/Value', 'does_not_exists') <> 'does_not_exists') then
    XMLConfig.SetValue(new_Item + '/Description/Value', XMLConfig.GetValue(old_Item + '/Description/Value',''));
  if (XMLConfig.GetValue(old_Item + '/SubItems/Value','') = 'true') then
  begin
    XMLConfig.SetValue(new_Item + '/SubItems/Value', 'true');
    i:=1;
    Str(i,str_i);
    old_Item:=old_Item + '/subitem_';
    new_Item:=new_Item + '/subitem_';
    while(XMLConfig.GetValue(old_Item + str_i + '/Name/Value', 'does_not_exist') <> 'does_not_exist') do
    begin
      ReplaceInTemplate(old_Item + str_i, new_Item + str_i);
      Inc(i);
      Str(i, str_i);
    end;
  end;
end;

// --------------------------------------------------------------------------------------------------------------//
// Some property og some object has changed -> we need to know if caption of some menuitem has changed ----------//
// --------------------------------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.OnDesignerModified(Sender: TObject);
var
  Selection: TPersistentSelectionList;
  i: Integer;
  Instance: TPersistent;
  MenuItem: TMenuItem;
  InvalidateNeeded: Boolean;
  DesignerMenuItem: PDesignerMenuItem;
begin
  Selection := TPersistentSelectionList.Create;
  GlobalDesignHook.GetSelection(Selection);
  try
    InvalidateNeeded:=false;
    for i := Selection.Count - 1 downto 0 do
    begin
      Instance := Selection[i];
      if Instance is TMenuItem
      then begin
        MenuItem:=TMenuItem(Instance);
        // ToDo
        // how to get the Designer menu item?
        DesignerMenuItem:=FindDesignerMenuItem(MenuItem);
        //writeln('TDesignerMainMenu.OnDesignerModified A ',MenuItem.Name,' ',DesignerMenuItem<>nil,' ',MenuItem.Caption);
        if DesignerMenuItem = nil then Continue;
        
        ChangeCaption(DesignerMenuItem, MenuItem.Caption);
        InvalidateNeeded := true;
      end;
    end;
    if InvalidateNeeded then
      RealignDesigner;
  finally
    Selection.Free;
  end;
end;

procedure TDesignerMainMenu.OnComponentAdded(Sender: TObject);
begin

end;

// ------------------------------------------------------//
// Move Down has been selected from context menu --------//
// ------------------------------------------------------//
procedure TDesignerMainMenu.MoveDownClick(Sender: TObject);
begin
  if (MoveDown(Root, SelectedDesignerMenuItem) > 0) then
  begin
    SetCoordinates(POSITION_LEFT,POSITION_TOP,0,Root);

    InitIndexSequence;
    CreateIndexSequence(Root, SelectedDesignerMenuItem,1);

    UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem), 1, 5);

    RealignDesigner;
  end;
end;

// ------------------------------------------------------------------------------------//
// Adds new DesignerMenuItem before DesignerMenuItem with ID=Ident --------------------//
// ------------------------------------------------------------------------------------//
function TDesignerMainMenu.AddNewItemBefore(MenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
var
   new_menuitem,temp_menuitem: PDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  Result:=nil;
  if (MenuItem^.ID = Ident) then
  begin
    new(new_menuitem);
    Str(temp_newitemcounter, temp_newitemcounterstring);
    new_menuitem^.Caption:='New Item' + temp_newitemcounterstring;
    new_menuitem^.Level:=MenuItem^.Level;
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
        fRoot:=new_menuitem;
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
    Result := new_menuitem;
  end else
  begin
    if (MenuItem^.SubMenu <> nil) then
      Result := AddNewItemBefore(MenuItem^.SubMenu,Ident);
    if (Result = nil) and (MenuItem^.NextItem <> nil) then
      Result := AddNewItemBefore(MenuItem^.NextItem, Ident);
  end;
end;

// -----------------------------------------------------------------------------------//
// Adds new DesignerMenuItem after DesignerMenuItem with ID=Ident --------------------//
// -----------------------------------------------------------------------------------//
function TDesignerMainMenu.AddNewItemAfter(MenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
var
   new_menuitem,temp_menuitem: PDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  Result:=nil;
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
    Result:=new_menuitem;
  end else
  begin
    if (MenuItem^.SubMenu <> nil) then
      Result:=AddNewItemAfter(MenuItem^.SubMenu,Ident);
    if (Result = nil) and (MenuItem^.NextItem <> nil) then
      Result:=AddNewItemAfter(MenuItem^.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//
function TDesignerMainMenu.AddSubMenu(MenuItem: PDesignerMenuItem; Ident: string): PDesignerMenuItem;
var
   new_menuitem: PDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  Result:=nil;
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
    Result:=MenuItem^.SubMenu;
  end else
  begin
    if (MenuItem^.SubMenu <> nil) then
      Result:=AddSubMenu(MenuItem^.SubMenu,Ident);
    if (Result = nil) and (MenuItem^.NextItem <> nil) then
      Result:=AddSubMenu(MenuItem^.NextItem,Ident);
  end;
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
        FRoot:=DesignerMenuItem;
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
        FRoot:=temp_designermenuitem;
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
// ------------------------------------------------------------------------------//
function TDesignerMainMenu.DeleteItem(DesignerMenuItem: PDesignerMenuItem): Integer;
var
  temp_previousdesignermenuitem,temp_nextdesignermenuitem: PDesignerMenuItem;
  temp_parentmenudesignermenuitem: PDesignerMenuItem;
  temp_designermenuitem: PDesignerMenuItem;
  i: Integer;
begin
  Result:=0;
  
  if DesignerMenuItem = nil then exit;

  temp_parentmenudesignermenuitem:=DesignerMenuItem^.ParentMenu;
  temp_previousdesignermenuitem:=DesignerMenuItem^.PrevItem;
  temp_nextdesignermenuitem:=DesignerMenuItem^.NextItem;
  
  if (temp_parentmenudesignermenuitem = nil) and (temp_previousdesignermenuitem = nil) and
     (temp_nextdesignermenuitem = nil)then
  begin
    if (DesignerMenuItem^.SubMenu <> nil) then
    begin
      DesignerMenuItem^.SubMenuPanel.Visible:=false;
      temp_designermenuitem:=DesignerMenuItem^.SubMenu;
      while (temp_designermenuitem <> nil) do
      begin
        temp_designermenuitem^.SelfPanel.Visible:=false;
        temp_designermenuitem:=temp_designermenuitem^.NextItem;
      end;
      Dispose(DesignerMenuItem^.SubMenu);
    end;
    DesignerMenuItem^.SubMenu:=nil;
    DesignerMenuItem^.NextItem:=nil;
    Result:=2;
  end else
  begin
    if (temp_previousdesignermenuitem <> nil) then
      temp_previousdesignermenuitem^.NextItem:=temp_nextdesignermenuitem;
    if (temp_nextdesignermenuitem <> nil) then
    begin
      temp_nextdesignermenuitem^.PrevItem:=temp_previousdesignermenuitem;
      temp_designermenuitem:=temp_nextdesignermenuitem;
      i:=DesignerMenuItem^.Index;
      while (temp_designermenuitem <> nil) do
      begin
        temp_designermenuitem^.Index:=i;
        Inc(i);
        temp_designermenuitem:=temp_designermenuitem^.NextItem;
      end;
    end;
    if (temp_parentmenudesignermenuitem = nil) and (temp_previousdesignermenuitem = nil) then
    begin
      temp_nextdesignermenuitem^.ParentMenu:=nil;
      FRoot:=temp_nextdesignermenuitem;
    end;
    if (temp_parentmenudesignermenuitem <> nil) then
    begin
      if (temp_nextdesignermenuitem <> nil) then
      begin
        temp_parentmenudesignermenuitem^.SubMenu:=temp_nextdesignermenuitem;
        temp_nextdesignermenuitem^.ParentMenu:=temp_parentmenudesignermenuitem;
      end else
      begin
        temp_parentmenudesignermenuitem^.SubMenu:=nil;
        temp_parentmenudesignermenuitem^.SubMenuPanel.Visible:=false;
        temp_parentmenudesignermenuitem^.Active:=false;
        temp_parentmenudesignermenuitem^.Selected:=true;
      end;
    end;
    DesignerMenuItem^.SelfPanel.Visible:=false;
    DesignerMenuItem^.SubMenuPanel.Visible:=false;
    Dispose(DesignerMenuItem);
    Result:=1;
  end;
end;

// ------------------------------------------------------------------------------//
// ------------------------------------------------------------------------------//
function TDesignerMainMenu.ChangeCaption(DesignerMenuItem: PDesignerMenuItem;
  const newcaption: string): Integer;
begin
  Result:=0;
  if DesignerMenuItem^.Caption=NewCaption then exit;
  InitIndexSequence;
  CreateIndexSequence(Root, DesignerMenuItem^.ID, 1);
  DesignerMenuItem^.Caption:=newcaption;
  Result:=1;
end;

procedure TDesignerMainMenu.HideDesignerMenuItem(DesignerMenuItem: PDesignerMenuItem);
begin
  if (DesignerMenuItem^.SubMenu <> nil) then
  begin
    HideDesignerMenuItem(DesignerMenuItem^.SubMenu);
    DesignerMenuItem^.SubMenuPanel.Visible:=false;
    DesignerMenuItem^.SubMenu:=nil;
  end;
  if (DesignerMenuItem^.NextItem <> nil) then
  begin
    HideDesignerMenuItem(DesignerMenuItem^.NextItem);
    DesignerMenuItem^.NextItem:=nil;
  end;
  DesignerMenuItem^.SelfPanel.Visible:=false;
end;

// -------------------------------------------------------------------------------------------------------------------
// Finds DesignerMenuItem with identification Ident and returns a pointer to it
// -------------------------------------------------------------------------------------------------------------------
function TDesignerMainMenu.GetDesignerMenuItem(
  DesignerMenuItem: PDesignerMenuItem; const Ident: string): PDesignerMenuItem;
begin
  Result:=nil;
  if DesignerMenuItem=nil then exit;
  if (AnsiCompareText(DesignerMenuItem^.ID,Ident)=0) then
    Result:=DesignerMenuItem
  else
  begin
    Result:=GetDesignerMenuItem(DesignerMenuItem^.SubMenu, Ident);
    if Result<>nil then exit;
    Result:=GetDesignerMenuItem(DesignerMenuItem^.NextItem, Ident);
  end;
end;

function TDesignerMainMenu.FindDesignerMenuItem(AMenuItem: TMenuItem
  ): PDesignerMenuItem;
// search the corresponding designer menu item

  function FindRecursive(TheMenuItem: TMenuItem): PDesignerMenuItem;
  var
    ParentDesignerMenuItem: PDesignerMenuItem;
    i: Integer;
  begin
    Result := nil;
    if TheMenuItem = nil then
      exit;
    // find parent
    if TheMenuItem.Parent = nil then
    begin
      // this is TMenu.Items -> no corresponding
    end
    else
    if TheMenuItem.Parent.Parent = nil then
    begin
      // top level menu item
      if (TheMenuItem.GetParentMenu = fMenu) then
      begin
        // root item
        Result:=Root;
      end;
    end else
    begin
      // sub menu item
      // -> search parent
      ParentDesignerMenuItem := FindRecursive(TheMenuItem.Parent);
      if ParentDesignerMenuItem <> nil then
        Result := ParentDesignerMenuItem^.SubMenu;
    end;
    if Result <> nil then
    begin
      i := TheMenuItem.MenuIndex;
      while (Result <> nil) and (i > 0) do
      begin
        Result := Result^.NextItem;
        dec(i);
      end;
    end;
  end;

begin
  Result := FindRecursive(AMenuItem);
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

function TDEsignerMainMenu.CreateIndexSequence(MenuItem: PDesignerMenuItem;
  Ident: string; Ind: Integer): Boolean;
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

{procedure TDEsignerMainMenu.VypisIndexSequence;
var
  i: Integer;
begin
  for i:=1 to INDEX_SEQUENCE_LENGTH do
    write (index_sequence[i],' ');
  writeln;
end;}

// ------------------------------------------------------------------
// UPDATE Menu (type of update is specified via the Action parameter)
// ------------------------------------------------------------------
function TDesignerMainMenu.UpdateMenu(MenuItem: TMenuItem;
  DesignerMenuItem: PDesignerMenuItem; Ind, TheAction: Integer): TMenuItem;
var
  i: Integer;
  temp_menuitem: TMenuItem;
begin
  case TheAction of
   // Insert new MenuItem after selected MenuItem
  1: begin
       if (index_sequence[Ind + 1] = -1) then
       begin
         temp_menuitem:=TMenuItem.Create(fMenu.Owner);
         temp_menuitem.Caption:=DesignerMenuItem^.Caption;
         
         // code from Mattias (one of mail he sent me)
         temp_menuitem.Name:=GetDesigner.CreateUniqueComponentName(temp_menuitem.ClassName);
         MenuItem.Insert(index_sequence[Ind] + 1, temp_menuitem);
         GetDesigner.PropertyEditorHook.PersistentAdded(temp_menuitem, true);
         GetDesigner.Modified;
         
       end else
       begin
         UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction);
       end;
     end;
   // Insert new MenuItem before selected MenuItem
   2: begin
       if (index_sequence[Ind + 1] = -1) then
       begin
         temp_menuitem:=TMenuItem.Create(fMenu.Owner);
         temp_menuitem.Caption:=DesignerMenuItem^.Caption;
         
         // code from Mattias (one of mail he sent me)
         temp_menuitem.Name:=GetDesigner.CreateUniqueComponentName(temp_menuitem.ClassName);
         MenuItem.Insert(index_sequence[Ind] - 1, temp_menuitem);
         GetDesigner.PropertyEditorHook.PersistentAdded(temp_menuitem, true);
         GetDesigner.Modified;
       end else
       begin
         UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction);
       end;
      end;
    // Creates SubMenu to an existing MenuItem
    3: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
         temp_menuitem:=TMenuItem.Create(fMenu.Owner);
           temp_menuitem.Caption:=DesignerMenuItem^.Caption;

           // code from Mattias (one of mail he sent me)
           temp_menuitem.Name:=GetDesigner.CreateUniqueComponentName(temp_menuitem.ClassName);
           MenuItem[index_sequence[Ind]].Add(temp_menuitem);
           GetDesigner.PropertyEditorHook.PersistentAdded(temp_menuitem, true);
           GetDesigner.Modified;
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction);
       end;
    // Moves Up(left) an MenuItem (changes positions of this MenuItem and its predecesor)
    4: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=MenuItem[index_sequence[Ind] + 1];
           MenuItem.Delete(index_sequence[Ind] + 1);
           MenuItem.Insert(index_sequence[Ind], temp_menuitem);
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction)
       end;
    // Moves Down(right) an MenuItem (changes positions of this MenuItem and its ancestor)
    5: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=MenuItem[index_sequence[Ind]];
           MenuItem.Delete(index_sequence[Ind]);
           MenuItem.Insert(index_sequence[Ind] - 1, temp_menuitem);
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction)
       end;
    // Changes a caption of MenuItem
    6: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           //writeln(MenuItem[index_sequence[Ind]].Caption);
           MenuItem[index_sequence[Ind]].Caption:=DesignerMenuItem^.Caption;
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction)
       end;
    // Deletes a MenuItem
    7: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           //MenuItem.Remove(MenuItem[index_sequence[Ind]]);
           temp_menuitem:=MenuItem[index_sequence[Ind]];
           GlobalDesignHook.DeletePersistent(TPersistent(temp_menuitem));
           //MenuItem[index_sequence[Ind]].Free;
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction)
       end;
    // Deletes a SubMenu of MenuItem
    8: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           for i := MenuItem[index_sequence[Ind]].Count - 1 downto 0 do
           begin
             temp_menuitem:=MenuItem[index_sequence[Ind]].Items[i];
             GlobalDesignHook.DeletePersistent(TPersistent(temp_menuitem));
             //MenuItem[index_sequence[Ind]].Delete(i);
             //MenuItem[index_sequence[Ind]].Items.Free
           end;
         end else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction);
       end;
    // Selectes a MenuItem in the OI
    9: begin
         if (index_sequence[Ind + 1] = -1) then
           GetDesigner.SelectOnlyThisComponent(MenuItem[index_sequence[Ind]])
         else
           UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction);
       end;
   // Return an MenuItem
   10: begin
         if (index_sequence[Ind + 1] = -1) then
           Result:=MenuItem[index_sequence[Ind]]
         else
           Result:=UpdateMenu(MenuItem.Items[index_sequence[Ind]], DesignerMenuItem, Ind + 1, TheAction);
       end;
  // Sonething else
  end;
end;

// ---------------------/
// TTemplateMenuForm ---/
// ---------------------/

procedure TTemplateMenuForm.FormCreate(Sender: TObject);
var
  i: Integer;
  templatemenuitem, str_i: string;
begin
  fAction:=TemplateMenuFormCreateAction;
  subitem_level:=1;

  Label_template_description.Caption:=lisMenuEditorSelectTemplate;
  Label_template_view.Caption:=lisMenuEditorTemplatePreview;
  Edit_template_description.Text:=lisMenuEditorNewTemplateDescription;
  
  // Templates from menutemplates.xml
  i:=1;
  Str(i,str_i);
  templatemenuitem:='menu_';
  while (XMLConfig.GetValue(templatemenuitem + str_i + '/Name/Value','does_not_exists') <> 'does_not_exists') do
  begin
    TemplatesListBox.Items.Add(XMLConfig.GetValue(templatemenuitem + str_i + '/Description/Value','does_not_exists'));
    Inc(i);
    Str(i,str_i);
  end;

  // Select the first menu on list and show it in "Template Preview"
  if (TemplatesListBox.Items.Count > 0) then
    TemplatesListBox.Selected[0]:=true;

  case fAction of
  1: begin
       // content of "Caption" is generated from LazarusIDEStrConsts
       Caption:=lisMenuEditorInsertFromTemplate;
       TemplatesListBox.Items.Add(lisMenuTemplateDescriptionStandardFileMenu);
       TemplatesListBox.Items.Add(lisMenuTemplateDescriptionStandardEditMenu);
       TemplatesListBox.Items.Add(lisMenuTemplateDescriptionStandardHelpMenu);
       TemplateView('', 1);
     end;
  2: begin
       // content of "Caption" is generated from LazarusIDEStrConsts
       Caption:=lisMenuEditorSaveAsTemplate;
       Edit_template_description.Visible:=true;
       if TemplatesListBox.Items.Count > 0 then
         TemplateView('menu_1/subitem_', 0);
     end;
  3: begin
       // content of "Caption" is generated from LazarusIDEStrConsts
       Caption:=lisMenuEditorDeleteFromTemplate;
       if TemplatesListBox.Items.Count > 0 then
         TemplateView('menu_1/subitem_', 0);
     end;
  end;
  ButtonPanel1.OKButton.OnClick := @OKBitBtnClick;
  ButtonPanel1.CancelButton.OnClick := @CancelBitBtnClick;
end;

procedure TTemplateMenuForm.FormResize(Sender: TObject);
begin
  TemplatesListBox.Width:=(Width div 2)-14;
end;

procedure TTemplateMenuForm.CancelBitBtnClick(Sender: TObject);
begin
  SelectedMenuTemplate := 0;
end;

procedure TTemplateMenuForm.OKBitBtnClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to TemplatesListBox.Items.Count-1 do
    if TemplatesListBox.Selected[i] then SelectedMenuTemplate:=i + 1;
  if (fAction = 2) and (Edit_template_description.Text <> '') then
  begin
    Description:=Edit_template_description.Text;
  end else
  begin
    if (fAction = 2) then
      // content of "Description" is generated from LazarusIDEStrConsts
      Description:=lisMenuEditorNewTemplateDescription;
  end;
end;

procedure TTemplateMenuForm.TemplatesListBoxClick(Sender: TObject);
var i: Integer;
    str_i: string;
begin
  ListBoxView.Clear;
  for i:=0 to TemplatesListBox.Items.Count-1 do
    if TemplatesListBox.Selected[i] then SelectedMenuTemplate:=i + 1;

  if (fAction <> 1) then
  begin
   Str(SelectedMenuTemplate, str_i);
   TemplateView('menu_' + str_i + '/subitem_', 0);
  end else
  begin
    if (SelectedMenuTemplate > NUMBER_OF_DEFAULT_TEMPLATES) then
    begin
      Str(SelectedMenuTemplate - NUMBER_OF_DEFAULT_TEMPLATES, str_i);
      TemplateView('menu_' + str_i + '/subitem_', 0);
    end else
      TemplateView('', SelectedMenuTemplate);
  end;
  ListBoxView.Selected[0]:=false;
end;

function TTemplateMenuForm.GetSelectedMenuTemplate: Integer;
begin
  Result:=SelectedMenuTemplate;
end;

function TTemplateMenuForm.GetDescription: string;
begin
   Result:=Description;
end;

procedure TTemplateMenuForm.TemplateView(templatemenuitem: string; default_template: Integer);
var
  i,j: Integer;
  item, subitem_level_space, str_i, temp_string: string;
begin
  if (default_template > 0) then
  begin
    case default_template of
    1: begin
         ListBoxView.Items.Add(lisMenuTemplateFile);
         ListBoxView.Items.Add(' ' + lisMenuTemplateNew);
         ListBoxView.Items.Add(' -');
         ListBoxView.Items.Add(' ' + lisMenuTemplateOpen);
         ListBoxView.Items.Add(' ' + lisMenuTemplateOpenRecent);
         ListBoxView.Items.Add(' ' + lisMenuTemplateSave);
         ListBoxView.Items.Add(' ' + lisMenuTemplateSaveAs);
         ListBoxView.Items.Add(' ' + lisMenuTemplateClose);
         ListBoxView.Items.Add(' -');
         ListBoxView.Items.Add(' ' + lisMenuTemplateExit);
       end;
    2: begin
         ListBoxView.Items.Add(lisMenuTemplateEdit);
         ListBoxView.Items.Add(' ' + lisMenuTemplateUndo);
         ListBoxView.Items.Add(' ' + lisMenuTemplateRedo);
         ListBoxView.Items.Add(' -');
         ListBoxView.Items.Add(' ' + lisMenuTemplateCut);
         ListBoxView.Items.Add(' ' + lisMenuTemplateCopy);
         ListBoxView.Items.Add(' ' + lisMenuTemplatePaste);
         ListBoxView.Items.Add(' -');
         ListBoxView.Items.Add(' ' + lisMenuTemplateFind);
         ListBoxView.Items.Add(' ' + lisMenuTemplateFindNext);
       end;
    3: begin
         ListBoxView.Items.Add(lisMenuTemplateHelp);
         ListBoxView.Items.Add(' ' + lisMenuTemplateContents);
         ListBoxView.Items.Add(' ' + lisMenuTemplateTutorial);
         ListBoxView.Items.Add(' -');
         ListBoxView.Items.Add(' ' + lisMenuTemplateAbout);
       end;
    end;
  end else
  begin
    if (subitem_level = 1) then
    begin
      temp_string:=templatemenuitem;
      Delete(temp_string,7,9);
      ListBoxView.Items.Add(XMLConfig.GetValue(temp_string + '/Name/Value',''));
    end;
    Inc(subitem_level);
    i:=1;
    Str(i, str_i);
    while (XMLConfig.GetValue(templatemenuitem + str_i + '/Name/Value','does_not_exists') <> 'does_not_exists') do
    begin
      subitem_level_space:='';
      for j:=1 to subitem_level do
        subitem_level_space:=subitem_level_space + ' ';
      item:=subitem_level_space + XMLConfig.GetValue(templatemenuitem + str_i + '/Name/Value','does_not_exists');
      ListBoxView.Items.Add(item);
      if (XMLConfig.GetValue(templatemenuitem + str_i + '/SubItems/Value', '') = 'true') then
        TemplateView(templatemenuitem + str_i + '/subitem_', 0);
      Inc(i);
      Str(i, str_i);
    end;
    Dec(subitem_level);
  end;
end;

end.
