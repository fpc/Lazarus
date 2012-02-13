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

  TDesignerMenuItem = class
    SelfPanel: TPanel;
    SubMenuPanel: TPanel;
    SubMenuArrow: TArrow;
    CaptionLabel: TLabel;
    ParentMenu: TDesignerMenuItem;
    SubMenu: TDesignerMenuItem;
    PrevItem: TDesignerMenuItem;
    NextItem: TDesignerMenuItem;
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
    fRoot: TDesignerMenuItem;
    fPanel: TPanel;
    fDesignerMenuItemIdent: Integer;
    fParentCanvas: TCanvas;
    fSelectedDesignerMenuItem: string;
    fMenu: TMenu;
    fDefaultComponentEditor: TDefaultComponentEditor;
    FDesignerPopupMenu: TPopupMenu;
    TemplateMenuForm: TTemplateMenuForm;
    temp_panel: TPanel;
    temp_level: Integer;
    temp_newitemcounter: Integer;
    index_sequence: Array[1..INDEX_SEQUENCE_LENGTH] of Integer;
    function GetDesigner: TComponentEditorDesigner;
    procedure SetRoot(const AValue: TDesignerMenuItem);
  protected
    procedure PersistentDeleting(APersistent: TPersistent);
    function SearchItemByPanel(DMenuItem: TDesignerMenuItem; APanel: TPanel): TDesignerMenuItem;
    procedure ClearAllMenus;
  public
    // Constructor and destructor
    constructor CreateWithMenu(AOwner: TComponent; AMenu: TMenu);
    destructor Destroy; override;
    
    // Properties for  accesing private variables
    property Root: TDesignerMenuItem read FRoot write SetRoot;
    property Panel: TPanel read FPanel write FPanel;
    property DesignerMenuItemIdent: Integer read FDesignerMenuItemIdent write FDesignerMenuItemIdent;
    property SelectedDesignerMenuItem: string read FSelectedDesignerMenuItem write FSelectedDesignerMenuItem;
    property ParentCanvas: TCanvas read FParentCanvas write FParentCanvas;
    property Menu: TMenu read fMenu;

    // Loading menu functions and initialization function
    procedure LoadMainMenu;
    procedure Init(DMenuItem: TDesignerMenuItem);
    procedure Link(AMenuItem: TMenuItem; ParentMI: TDesignerMenuItem);
    
    // Draw function and supplementary functions for setting coordinates
    procedure RealignDesigner;
    procedure Draw(DMenuItem: TDesignerMenuItem; FormPanel,SubMenuPanel: TPanel); //draw function
    procedure SetCoordinates(Coord_Left,Coord_Top,Coord_Right: Integer; DMenuItem: TDesignerMenuItem); //coord. of each designermenuitem
    function GetSubMenuHeight(DMenuItem: TDesignerMenuItem; LeftPos,TopPos: Integer; Ident: string): TRect; //width and height of submenu panel
    function GetMaxCoordinates(DMenuItem: TDesignerMenuItem; Max_Width, Max_Height: Integer): TRect; //width and height of all expanded menu items
    
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
    function AddNewItemBefore(DMenuItem: TDesignerMenuItem; Ident: string): TDesignerMenuItem;
    function AddNewItemAfter(DMenuItem: TDesignerMenuItem; Ident: string): TDesignerMenuItem;
    function AddSubMenu(DMenuItem: TDesignerMenuItem; Ident: string): TDesignerMenuItem;
    function MoveUp(DMenuItem: TDesignerMenuItem; Ident: string): Integer;
    function MoveDown(DMenuItem: TDesignerMenuItem; Ident: string): Integer;
    function DeleteItem(DMenuItem: TDesignerMenuItem): Integer;
    function ChangeCaption(DMenuItem: TDesignerMenuItem; const newcaption: string): Integer;
    procedure InsertFromTemplate(Item,Ident: string);
    procedure SaveAsTemplate(Item,Ident: string);
    procedure ReplaceInTemplate(old_Item, new_Item: string);
    function ChangeMenuItem(DMenuItem: TDesignerMenuItem; TheAction: Integer; Ident: string): Boolean;
    
    //  Function for updating the real menu (which is the edited one) and supplementary functions for
    //  building a search index which is needed to locate an MenuItem in real menu which has to be update
    procedure InitIndexSequence;
    function CreateIndexSequence(DMenuItem: TDesignerMenuItem; Ident: string; Ind: Integer): Boolean;
    function UpdateMenu(AMenuItem: TMenuItem; DMenuItem: TDesignerMenuItem; Ind,TheAction: Integer): TMenuItem;
    
    procedure HideDesignerMenuItem(DMenuItem: TDesignerMenuItem);
    function GetDesignerMenuItem(DMenuItem: TDesignerMenuItem; const Ident: string): TDesignerMenuItem;
    function FindDesignerMenuItem(AMenuItem: TMenuItem): TDesignerMenuItem;
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

procedure TDesignerMainMenu.SetRoot(const AValue: TDesignerMenuItem);
begin
  if FRoot <> nil then
    FRoot.Free;
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

  FRoot := TDesignerMenuItem.Create;
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
procedure TDesignerMainMenu.Init(DMenuItem: TDesignerMenuItem);
var
  i: Integer;
begin
  i:=DesignerMenuItemIdent;
  Str(i, DMenuItem.ID);      Assert(DMenuItem.ID = IntToStr(i));

  DMenuItem.Selected:=false;
  DMenuItem.Active:=false;
  
  DMenuItem.coord.Left:=0;
  DMenuItem.coord.Top:=0;
  DMenuItem.coord.Right:=0;
  DMenuItem.coord.Bottom:=0;
  
  DMenuItem.SelfPanelCreated:=false;
  DMenuItem.SubMenuPanelCreated:=false;
  
  DMenuItem.SubMenuPanel:=TPanel.Create(self);
  
  DMenuItem.SelfPanel:=TPanel.Create(self);
  DMenuItem.SelfPanel.Name:='SelfPanel_' + DMenuItem.ID;
  DMenuItem.SelfPanel.Caption:='';
  DMenuItem.SelfPanel.Height:=DESIGNER_MENU_ITEM_HEIGHT;
  DMenuItem.SelfPanel.OnMouseDown:=@MenuItemMouseDown;
  DMenuItem.SelfPanel.OnDblClick:=@MenuItemDblClick;
  DMenuItem.SelfPanel.PopupMenu := FDesignerPopupMenu;

  DMenuItem.CaptionLabel:=TLabel.Create(self);
  DMenuItem.CaptionLabel.Name:='CaptionLabel_' + DMenuItem.ID;
  DMenuItem.CaptionLabel.Parent:=DMenuItem.SelfPanel;
  DMenuItem.CaptionLabel.Left:=DMenuItem.SelfPanel.Left + 5;
  DMenuItem.CaptionLabel.Top:=2;
  DMenuItem.CaptionLabel.Height:=DESIGNER_MENU_ITEM_HEIGHT - 4;
  DMenuItem.CaptionLabel.OnMouseDown:=@MenuItemMouseDown;
  DMenuItem.CaptionLabel.OnDblClick:=@MenuItemDblClick;

  DMenuItem.SubMenuArrow:=TArrow.Create(self);
  DMenuItem.SubMenuArrow.Name:='SubMenuArrow_' + DMenuItem.ID;
  DMenuItem.SubMenuArrow.Parent:=DMenuItem.SelfPanel;
  DMenuItem.SubMenuArrow.ArrowType:=atright;
  DMenuItem.SubMenuArrow.Width:=20;
  DMenuItem.SubMenuArrow.Height:=13;
  DMenuItem.SubMenuArrow.ShadowType:=stout;
  DMenuItem.SubMenuArrow.Visible:=false;
  DMenuItem.SubMenuArrow.OnMouseDown:=@MenuItemMouseDown;
  DMenuItem.SubMenuArrow.OnDblClick:=@MenuItemDblClick;

  DesignerMenuItemIdent:=DesignerMenuItemIdent + 1;
  inc(temp_newitemcounter);
end;

// --------------------------------------------------------------------------
// Loads the MainMenu from the Designer form and creates the DesignerMainMenu
// --------------------------------------------------------------------------
procedure TDesignerMainMenu.LoadMainMenu;
var
  PrevMI, MI: TDesignerMenuItem;
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

  PrevMI := nil;
  for i := 0 to fMenu.Items.Count - 1 do
  begin
    MI := TDesignerMenuItem.Create;
    MI.Caption := fMenu.Items[i].Caption;
    MI.Level := temp_level;
    MI.NextItem := nil;
    MI.SubMenu := nil;
    MI.ParentMenu := nil;
    MI.Index := i;
    if (i = 0) then
    begin
      MI.PrevItem := nil;
      Root := MI;
    end else
    begin
      MI.PrevItem := PrevMI;
      PrevMI.NextItem := MI;
    end;
    Init(MI);
    PrevMI := MI;
    Link(fMenu.Items[i], MI);
  end;
  Root.Selected := True;
end;

procedure TDesignerMainMenu.Link(AMenuItem: TMenuItem; ParentMI: TDesignerMenuItem);
var
  PrevMI, MI: TDesignerMenuItem;
  i: Integer;
begin
  inc(temp_level);
  if (AMenuItem.Count > 0) then
  begin
    PrevMI:=nil;
    for i:= 0 to AMenuItem.Count-1 do
    begin
      MI := TDesignerMenuItem.Create;
      MI.Caption:=AMenuItem.Items[i].Caption;
      MI.Level:=temp_level;
      MI.NextItem:=nil;
      MI.SubMenu:=nil;
      MI.Index:=i;
      if (i=0) then
      begin
        MI.ParentMenu:=ParentMI;
        MI.PrevItem:=nil;
        ParentMI.SubMenu:=MI;
      end else
      begin
        MI.PrevItem:=PrevMI;
        PrevMI.NextItem:=MI;
        MI.ParentMenu:=nil;
      end;
      Init(MI);
      PrevMI:=MI;
      Link(AMenuItem.Items[i],MI);
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
// Draw the whole DesignerMenu with active MenuItems and SubMenus ----------------------//
//------------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.Draw(DMenuItem: TDesignerMenuItem; FormPanel,SubMenuPanel: TPanel);
var
  SubMenuDimensions: TRect;
begin
  if DMenuItem.SelfPanel = nil then exit;
  with DMenuItem.SelfPanel do
  begin
    if (fMenu is TPopupMenu) and (DMenuItem.Level = 1) then
    begin
      if (DMenuItem.PrevItem = nil) then
      begin
        SubMenuDimensions:=GetSubMenuHeight(Root, 0, 0, DMenuItem.ID);
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
    if (DMenuItem.Level > 1) and (fMenu is TMainMenu) then
    begin
      Left:=2;
      Top:=DMenuItem.coord.Top - Parent.Top + 2;
      Width:=Parent.width - 4;
      Height:=DESIGNER_MENU_ITEM_HEIGHT;
    end else
    begin
      Top:=DMenuItem.coord.Top - Parent.Top + 2;
      Height:=DESIGNER_MENU_ITEM_HEIGHT;
      if (fMenu is TPopupMenu) then
      begin
        Width:=Parent.width - 4;
        Left:=2;
      end else
      begin
        Left:=DMenuItem.coord.Left;
        Width:=ParentCanvas.TextWidth(DMenuItem.Caption) + DESIGNER_MENU_ITEM_SPACE;
      end;
    end;
    if (DMenuItem.Selected) or ((DMenuItem.Level = 1) and (fMenu is TMainMenu)) then
    begin
      Bevelouter:=bvraised;
    end else
    begin
      Bevelouter:=bvnone;
    end;
  end;
  
  DMenuItem.CaptionLabel.Caption:=DMenuItem.Caption;
  
  if (DMenuItem.NextItem <> nil) then Draw(DMenuItem.NextItem, FormPanel, SubMenuPanel);
  with DMenuItem.SelfPanel do
  begin
    if ((DMenuItem.SubMenu <> nil) and
      (((DMenuItem.Selected) and ((DMenuItem.Submenu.Selected = False) or (DMenuItem.SubMenu.Active = False))) or
      ((DMenuItem.SubMenu.Selected) or (DMenuItem.SubMenu.Active)))) then
    begin
      if (fMenu is TpopupMenu) and (DMenuItem.Level = 1) then
        SubMenuDimensions:=GetSubMenuHeight(GetDesignerMenuItem(Root, DMenuItem.SubMenu.ID), DMenuItem.coord.right + 1, 0, DMenuItem.SubMenu.ID)
      else
        SubMenuDimensions:=GetSubMenuHeight(GetDesignerMenuItem(Root, DMenuItem.SubMenu.ID), 0, 1, DMenuItem.SubMenu.ID);
      with DMenuItem.SubMenuPanel do
      begin
        Parent:=SubMenuPanel;
        Visible:=true;
        if (fMenu is TMainMenu) and (DMenuItem.Level = 1) then
        begin
          Left:=DMenuItem.coord.Left;
          Top:=DMenuItem.coord.Bottom + 1;
        end else
        begin
          Left:=DMenuItem.coord.Right - 4;
          Top:=DMenuItem.coord.Top + 4;
        end;
        Width:=SubMenuDimensions.right;
        Height:=SubMenuDimensions.bottom;
      end;
    end;
  end;
  
  if (DMenuItem.SubMenu <> nil) then
  begin
    if (DMenuItem.Level = 1) and (fMenu is TMainMenu) then
    begin
      DMenuItem.SubMenuArrow.ArrowType:=atdown;
    end else
    begin
      DMenuItem.SubMenuArrow.ArrowType:=atright;
    end;
    DMenuItem.SubMenuArrow.Left:=DMenuItem.SelfPanel.Width - DMenuItem.SubMenuArrow.Width - 1;
    DMenuItem.SubMenuArrow.Top:=(DMenuItem.SelfPanel.Height - DMenuItem.SubMenuArrow.Height) div 2;
    DMenuItem.SubMenuArrow.Visible:=true;
  end else
  begin
    DMenuItem.SubMenuArrow.Left:=DMenuItem.SelfPanel.Width - DMenuItem.SubMenuArrow.Width - 1;
    DMenuItem.SubMenuArrow.Top:=(DMenuItem.SelfPanel.Height - DMenuItem.SubMenuArrow.Height) div 2;
    DMenuItem.SubMenuArrow.Visible:=false;
  end;

  DMenuItem.CaptionLabel.Width:=DMenuItem.SelfPanel.Width - 10;
  if ((DMenuItem.SubMenu <> nil) and ((DMenuItem.Selected) or (DMenuItem.Active))) then
  begin
    if ((((DMenuItem.Submenu.Active=False) or (DMenuItem.SubMenu.Selected=False)) and (DMenuItem.Selected)) or
      ((DMenuItem.Submenu.Active) or (DMenuItem.SubMenu.Selected))) then
      Draw(DMenuItem.SubMenu, DMenuItem.SubMenuPanel,SubMenuPanel);
  end;
end;


// --------------------------------------------------------------------------------------------------------------//
// Set the coordinates (position) of each DesignerMenuItem ------------------------------------------------------//
// --------------------------------------------------------------------------------------------------------------//
procedure TDesignerMainMenu.SetCoordinates(Coord_Left,Coord_Top,Coord_Right: Integer; DMenuItem: TDesignerMenuItem);
var
  temp_menuitem: TDesignerMenuItem;
begin
  DMenuItem.coord.Left:=Coord_Left;
  DMenuItem.coord.Top:=Coord_Top;
  DMenuItem.coord.Bottom:=DMenuItem.coord.top + DESIGNER_MENU_ITEM_HEIGHT;   // Bylo DESIGNER_MENU_ITEM_PANEL_HEIGHT
  
  if (DMenuItem.Level = 1) and (fMenu is TPopupMenu) then
    Coord_Right:=MIN_DESIGNER_MENU_ITEM_WIDTH;
  
  if (DMenuItem.Level = 1) and (fMenu is TMainMenu) then
    DMenuItem.coord.Right:=DMenuItem.coord.Left + Canvas.TextWidth(DMenuItem.Caption) + DESIGNER_MENU_ITEM_SPACE
  else begin
    // is this DesignerMenuItem wider than its predecessors?
    if (Canvas.TextWidth(DMenuItem.Caption) + DESIGNER_MENU_ITEM_SPACE > Coord_Right) then
    begin
      DMenuItem.coord.right:=DMenuItem.coord.Left + Canvas.TextWidth(DMenuItem.Caption) + DESIGNER_MENU_ITEM_SPACE;
      Coord_Right:=DMenuItem.coord.Right - DMenuItem.coord.Left;
      // we have to set the width of all predecessors of this DesignerMenuItem to its size
      temp_menuitem:=DMenuItem;
      while (temp_menuitem.PrevItem <> nil) do
      begin
        temp_menuitem:=temp_menuitem.PrevItem;
        temp_menuitem.coord.right:=DMenuItem.coord.right;
      end;
    // if not wider then keep size of the predecessor
    end else
    begin
      DMenuItem.coord.right:=DMenuItem.coord.Left + Coord_Right;
      Coord_Right:=DMenuItem.coord.Right - DMenuItem.coord.Left;
    end;
  end;
  
  if (DMenuItem.SubMenu <> nil) then
  begin
    if (fMenu is TMainMenu) and (DMenuItem.Level = 1) then
      SetCoordinates(DMenuItem.coord.Left, DMenuItem.coord.Bottom + 1, MIN_DESIGNER_MENU_ITEM_WIDTH, DMenuItem.SubMenu)
    else
      SetCoordinates(DMenuItem.coord.Right - 4, DMenuItem.coord.Top + 4, MIN_DESIGNER_MENU_ITEM_WIDTH, DMenuItem.SubMenu)
  end;
  if (DMenuItem.NextItem <> nil) then
  begin
    if (DMenuItem.Level = 1) and (fMenu is TMainMenu) then
      SetCoordinates(DMenuItem.coord.Right + 1, POSITION_TOP, 0,DMenuItem.NextItem)
    else
      SetCoordinates(DMenuItem.coord.left, DMenuItem.coord.Bottom, Coord_Right, DMenuItem.NextItem);
  end;
end;

// -------------------------------------------------------------------------------------------------------------------//
// Determines a position of the SubMenuPanel of some DesignerMenuItem ------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------//
function TDesignerMainMenu.GetSubMenuHeight(DMenuItem: TDesignerMenuItem; LeftPos,TopPos: Integer; Ident: string): TRect;
var
  coords: TRect;
  SubItemCount: Integer;
begin
  coords.right:=DMenuItem.coord.Right - DMenuItem.coord.Left + 4;
  // sets the bottom coordinate of submenupanel depending on number of submenuitems
  SubItemCount:=1;
  while(DMenuItem.NextItem <> nil) do
  begin
    inc(SubItemCount);
    DMenuItem:=DMenuItem.NextItem;
  end;
  coords.bottom:=SubItemCount * DESIGNER_MENU_ITEM_HEIGHT + 5;
  // return coordinates
  GetSubMenuHeight:=coords;
end;

// -------------------------------------------------------------------------------------------------------------------//
// Determines a position of the SubMenuPanel of some DesignerMenuItem ------------------------------------------------//
//--------------------------------------------------------------------------------------------------------------------//

function TDesignerMainMenu.GetMaxCoordinates(DMenuItem: TDesignerMenuItem; Max_Width, Max_Height: Integer): TRect;
var
  temp_coord: TRect;
begin
  Result:=Rect(0,0,0,0);
  if (DMenuItem.coord.Right > Max_Width) then
    Max_Width:=DMenuItem.coord.Right;
  if (DMenuItem.coord.Bottom > Max_Height) then
    Max_Height:=DMenuItem.coord.Bottom;
  if (DMenuItem.SubMenu = nil) and (DMenuItem.NextItem = nil) then
  begin
    Result.Right:=Max_Width;
    Result.Bottom:=Max_Height;
    exit;
  end;
  if DMenuItem.SubMenu <> nil then
  begin
    temp_coord:=GetMaxCoordinates(DMenuItem.SubMenu, Max_Width, Max_Height);
    Max_Width:=temp_coord.Right;
    Max_Height:=temp_coord.Bottom;
  end;
  if (DMenuItem.NextItem <> nil) then
    temp_coord:=GetMaxCoordinates(DMenuItem.NextItem, Max_Width, Max_Height);
    
  Result:=temp_coord;
end;

// --------------------------------------------------------
// Function that changes MenuItem (Remove, Add SubMenu ...)
// --------------------------------------------------------
function TDesignerMainMenu.ChangeMenuItem(DMenuItem: TDesignerMenuItem;
  TheAction: Integer; Ident: string): Boolean;
var
  completed: boolean;
begin
  completed:=false;
  case TheAction of
  // Test if this DMenuItem has been selected
  1: begin
       if (DMenuItem.ID = Ident) then
       begin
         DMenuItem.Selected:=true;
         DMenuItem.Active:=false;
         completed:=true;
       end else begin
         DMenuItem.Selected:=false;
       end;
         
       if (DMenuItem.SubMenu <> nil) then
       begin
         if (ChangeMenuItem(DMenuItem.SubMenu,TheAction,Ident) = true) then
         begin
           DMenuItem.Active:=true;
           completed:=true;
         end else DMenuItem.Active:=false;
       end;
       if (DMenuItem.NextItem <> nil) then
       begin
        if (ChangeMenuItem(DMenuItem.NextItem,TheAction,Ident)= true) then
        begin
          if DMenuItem.Level > 1 then
          begin
            DMenuItem.Active := True;
            completed := true;
          end;
        end;
       end;
     end;
  // Destroy all created panels of this DMenuItem
  2: begin
       DMenuItem.Active := False;
       DMenuItem.Selected := False;
       if (DMenuItem.SubMenu<> nil) then
       begin
         ChangeMenuItem(DMenuItem.SubMenu,TheAction,DMenuItem.SubMenu.ID);
         DMenuItem.SubMenuPanel.visible:=false;
       end;
       if (DMenuItem.NextItem <> nil) then
         ChangeMenuItem(DMenuItem.NextItem,TheAction,DMenuItem.NextItem.ID);
       DMenuItem.SelfPanel.visible:=false;
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
  DesignerItem: TDesignerMenuItem;
begin
  ChangeMenuItem(Root, 2, Root.ID);
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
    SelectedDesignerMenuItem := DesignerItem.ID;
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
  NewItem: TDesignerMenuItem;
begin
  NewItem := AddNewItemBefore(Root, SelectedDesignerMenuItem);
  if NewItem = nil then
    Exit;
  NewItem.Active := True; // set visible
  SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);
  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem).PrevItem, 1, 2);
  RealignDesigner;
end;

// ------------------------------------------------------------//
// New Item (after) has been selected from context menu -------//
// ------------------------------------------------------------//
procedure TDesignerMainMenu.AddNewItemAfterClick(Sender: TObject);
var
  NewItem: TDesignerMenuItem;
begin
  NewItem := AddNewItemAfter(Root, SelectedDesignerMenuItem);
  if NewItem = nil then
    Exit;
  NewItem.Active := True; // set visible
  SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root); 
  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);  
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem).NextItem, 1, 1);
  RealignDesigner;
end;

// ------------------------------------------------------------//
// Add SubMenu has been selected from context menu ------------//
// ------------------------------------------------------------//
procedure TDesignerMainMenu.AddSubMenuClick(Sender: TObject);
var
  NewItem: TDesignerMenuItem;
begin
  NewItem := AddSubMenu(Root, SelectedDesignerMenuItem);
  if NewItem = nil then
    Exit;
  NewItem.Active := True; // set visible
  SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);
  InitIndexSequence;
  CreateIndexSequence(Root, SelectedDesignerMenuItem,1);
  UpdateMenu(fMenu.Items, GetDesignerMenuItem(Root, SelectedDesignerMenuItem).SubMenu, 1, 3);
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
  fDefaultComponentEditor:=TDefaultComponentEditor.Create(temp_menuitem, GetDesigner);
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
  Item, NextSelectedItem: TDesignerMenuItem;
begin
  //SelectedDesignerMenuItem:=GetSelectedDesignerMenuItem(Root);
  CreateIndexSequence(Root, SelectedDesignerMenuItem, 1);
  Item := GetDesignerMenuItem(Root, SelectedDesignerMenuItem);

  // look for next selected item
  NextSelectedItem := Item.NextItem;
  if NextSelectedItem = nil then
    NextSelectedItem := Item.PrevItem;
  if NextSelectedItem = nil then
    NextSelectedItem := Item.ParentMenu;
  if NextSelectedItem = nil then
    NextSelectedItem := Root;
    
  temp_returnvalue := DeleteItem(Item);
  if (temp_returnvalue > 0) then
  begin
    SelectedDesignerMenuItem := NextSelectedItem.ID;
    
    ChangeMenuItem(Root, 2, Root.ID);
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
  Item: TDesignerMenuItem;
begin
  if APersistent is TMenuItem then
  begin
    Item := FindDesignerMenuItem(MenuItem);
    // how we can compare them?
    if (Item <> nil) and (Item.Caption = MenuItem.Caption) then
    begin
      DeleteItem(Item);
      SetCoordinates(POSITION_LEFT, POSITION_TOP, 0, Root);
      RealignDesigner;
    end;
  end;
  inherited;
end;

function TDesignerMainMenu.SearchItemByPanel(
  DMenuItem: TDesignerMenuItem; APanel: TPanel): TDesignerMenuItem;
begin
  if DMenuItem <> nil then
  begin
    if DMenuItem.SelfPanel = APanel then
      Result := DMenuItem
    else begin
      Result := SearchItemByPanel(DMenuItem.SubMenu, APanel);
      if Result = nil then
        Result := SearchItemByPanel(DMenuItem.NextItem, APanel);
    end;
  end else
    Result := nil;
end;

procedure TDesignerMainMenu.ClearAllMenus; 

  procedure DeleteRecursive(var AMenu: TDesignerMenuItem);
  begin
    if not Assigned(AMenu) then Exit;
    if Assigned(AMenu.NextItem) then DeleteRecursive(AMenu.NextItem);
    if Assigned(AMenu.SubMenu) then DeleteRecursive(AMenu.SubMenu);
    FreeAndNil(AMenu);
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
  temp_designermenuitem: TDesignerMenuItem;
begin
  TemplateMenuFormCreateAction:=1;
  TemplateMenuForm:=TTemplateMenuForm.Create(self);
  
  if (TemplateMenuForm.ShowModal = mrOK) then
  begin
  
    if (GetDesignerMenuItem(Root, SelectedDesignerMenuItem).SubMenu <> nil) then
    begin
      HideDesignerMenuItem(GetDesignerMenuItem(Root, SelectedDesignerMenuItem));
      GetDesignerMenuItem(Root, SelectedDesignerMenuItem).SubMenu:=nil;
      GetDesignerMenuItem(Root, SelectedDesignerMenuItem).SubMenuPanel.Visible:=false;
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
    end
    // Some of default templates has been selected
    else begin
      temp_designermenuitem:=GetDesignerMenuItem(Root, SelectedDesignerMenuItem);
      case TemplateMenuForm.GetSelectedMenuTemplate of
      1: Begin
           // Change a caption of selected designermenuitem fo "File"
           ChangeCaption (temp_designermenuitem, lisMenuTemplateFile);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add an submenu with first item and set it's caption to "New"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddSubMenuClick(self);
           temp_designermenuitem:=temp_designermenuitem.SubMenu;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateNew);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Open"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateOpen);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Open Recent"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateOpenRecent);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Save"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateSave);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Save As"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateSaveAs);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
           
           // Add new item and set it's caption to "Close"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateClose);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Exit"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateExit);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
         end;
      2: begin
           // Change a caption of selected designermenuitem fo "Edit"
           ChangeCaption (temp_designermenuitem, lisEdit);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add an submenu with first item and set it's caption to "Undo"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddSubMenuClick(self);
           temp_designermenuitem:=temp_designermenuitem.SubMenu;
           ChangeCaption (temp_designermenuitem, lisUndo);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Redo"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisRedo);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Cut"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisCut);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Copy"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisCopy);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Paste"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisPaste);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Find"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateFind);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Find Next"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateFindNext);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);
         end;
      3: begin
           // Change a caption of selected designermenuitem fo "Help"
           ChangeCaption (temp_designermenuitem, lisMenuTemplateHelp);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add an submenu with first item and set it's caption to "Contents"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddSubMenuClick(self);
           temp_designermenuitem:=temp_designermenuitem.SubMenu;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateContents);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "Tutorial"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, lisMenuTemplateTutorial);
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new separator
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
           ChangeCaption (temp_designermenuitem, '-');
           UpdateMenu(fMenu.Items, temp_designermenuitem, 1, 6);

           // Add new item and set it's caption to "About"
           SelectedDesignerMenuItem:=temp_designermenuitem.ID;
           AddNewItemAfterClick(self);
           temp_designermenuitem:=temp_designermenuitem.NextItem;
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
  templatesubmenuitem, str_i: string;
  tempdesignermenuitem: TDesignerMenuItem;
begin
  InitIndexSequence;
  CreateIndexSequence(Root, GetDesignerMenuItem(Root, Ident).ID,1);
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
        tempdesignermenuitem:=AddSubMenu(Root, GetDesignerMenuItem(Root, Ident).ID);
        SetCoordinates(POSITION_LEFT,POSITION_TOP,0,Root);
        ChangeCaption(tempdesignermenuitem, XMLConfig.GetValue(templatesubmenuitem + '/Name/Value',''));
        InitIndexSequence;
        CreateIndexSequence(Root, tempdesignermenuitem.ParentMenu.ID,1);
        UpdateMenu(fMenu.Items, tempdesignermenuitem, 1, 3);
      end else
      begin
        tempdesignermenuitem:=AddNewItemAfter(Root, tempdesignermenuitem.ID);
        SetCoordinates(POSITION_LEFT,POSITION_TOP,0,Root);
        ChangeCaption(tempdesignermenuitem,XMLConfig.GetValue(templatesubmenuitem + '/Name/Value',''));
        InitIndexSequence;
        CreateIndexSequence(Root, tempdesignermenuitem.PrevItem.ID,1);
        UpdateMenu(fMenu.Items, tempdesignermenuitem, 1, 1);
      end;
      InsertFromTemplate(templatesubmenuitem,tempdesignermenuitem.ID);
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
  temp_designermenuitem, temp_designersubmenuitem: TDesignerMenuItem;
begin
  i:=1;
  Str(i, str_i);
  temp_designermenuitem:=GetDesignerMenuItem(Root, Ident);
  XMLConfig.SetValue(Item + '/Name/Value', temp_designermenuitem.Caption);
  temp_designersubmenuitem:=temp_designermenuitem.SubMenu;
  if (temp_designermenuitem.SubMenu <> nil) then
    XMLConfig.SetValue(Item + '/SubItems/Value', 'true');
  while (temp_designersubmenuitem <> nil) do
  begin
    XMLConfig.SetValue(Item + '/subitem_' + str_i + '/Name/Value', temp_designersubmenuitem.Caption);
    SaveAsTemplate(Item + '/subitem_' + str_i, temp_designersubmenuitem.ID);
    temp_designersubmenuitem:=temp_designersubmenuitem.NextItem;
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
  str_i, str_j, old_templatemenuitem, new_templatemenuitem: string;
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
  DesignerMenuItem: TDesignerMenuItem;
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
function TDesignerMainMenu.AddNewItemBefore(DMenuItem: TDesignerMenuItem; Ident: string): TDesignerMenuItem;
var
   NewMI, TempMI: TDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  Result:=nil;
  if (DMenuItem.ID = Ident) then
  begin
    NewMI := TDesignerMenuItem.Create;
    Str(temp_newitemcounter, temp_newitemcounterstring);
    NewMI.Caption:='New Item' + temp_newitemcounterstring;
    NewMI.Level:=DMenuItem.Level;
    NewMI.NextItem:=DMenuItem;
    NewMI.SubMenu:=nil;
    if (DMenuItem.ParentMenu <> nil) then
    begin
      NewMI.ParentMenu:=DMenuItem.ParentMenu;
      NewMI.PrevItem:=nil;
      DMenuItem.ParentMenu.SubMenu:=NewMI;
      DMenuItem.ParentMenu:=nil;
    end else
    begin
      NewMI.ParentMenu:=nil;
      if (DMenuItem.PrevItem <> nil) then
      begin
        NewMI.PrevItem:=DMenuItem.PrevItem;
        DMenuItem.PrevItem.NextItem:=NewMI;
      end else
      begin
        NewMI.PrevItem:=nil;
        fRoot:=NewMI;
      end;
    end;
    DMenuItem.PrevItem:=NewMI;
    // now we have to set the index of this DesignerMenuItem
    if (NewMI.NextItem <> nil) then
    begin
      TempMI:=NewMI;
      while (TempMI <> nil) do
      begin
        if (TempMI.PrevItem <> nil) then
          TempMI.Index:=TempMI.PrevItem.Index + 1
        else
          TempMI.Index:=0;
        TempMI:=TempMI.NextItem;
      end;
    end else
      NewMI.Index:=DMenuItem.Index + 1;
    Init(NewMI);
    Result := NewMI;
  end else
  begin
    if (DMenuItem.SubMenu <> nil) then
      Result := AddNewItemBefore(DMenuItem.SubMenu,Ident);
    if (Result = nil) and (DMenuItem.NextItem <> nil) then
      Result := AddNewItemBefore(DMenuItem.NextItem, Ident);
  end;
end;

// -----------------------------------------------------------------------------------//
// Adds new DesignerMenuItem after DesignerMenuItem with ID=Ident --------------------//
// -----------------------------------------------------------------------------------//
function TDesignerMainMenu.AddNewItemAfter(DMenuItem: TDesignerMenuItem; Ident: string): TDesignerMenuItem;
var
   NewMI, TempMI: TDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  Result:=nil;
  TempMI:=nil;
  if (DMenuItem.ID = Ident) then
  begin
    NewMI := TDesignerMenuItem.Create;
    Str(temp_newitemcounter,temp_newitemcounterstring);
    NewMI.Caption:='New Item' + temp_newitemcounterstring;
    NewMI.Level:=DMenuItem.Level;
    NewMI.PrevItem:=DMenuItem;
    NewMI.ParentMenu:=nil;
    NewMI.SubMenu:=nil;
    if (DMenuItem.NextItem <> nil) then
    begin
      NewMI.NextItem:=DMenuItem.NextItem;
      DMenuItem.NextItem.PrevItem:=NewMI;
    end else
      NewMI.NextItem:=nil;
    DMenuItem.NextItem:=NewMI;
    // now we have to set the index of this DesignerMenuItem
    if (NewMI.NextItem <> nil) then
    begin
      TempMI:=NewMI;
      while (TempMI <> nil) do
      begin
        TempMI.Index:=TempMI.PrevItem.Index + 1;
        TempMI:=TempMI.NextItem;
      end;
    end else
      NewMI.Index:=DMenuItem.Index + 1;
    Init(NewMI);
    Result:=NewMI;
  end else
  begin
    if (DMenuItem.SubMenu <> nil) then
      Result:=AddNewItemAfter(DMenuItem.SubMenu,Ident);
    if (Result = nil) and (DMenuItem.NextItem <> nil) then
      Result:=AddNewItemAfter(DMenuItem.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//
function TDesignerMainMenu.AddSubMenu(DMenuItem: TDesignerMenuItem; Ident: string): TDesignerMenuItem;
var
   new_menuitem: TDesignerMenuItem;
   temp_newitemcounterstring: string;
begin
  Result:=nil;
  if ((DMenuItem.ID = Ident) and (DMenuItem.SubMenu = nil)) then
  begin
    new_menuitem := TDesignerMenuItem.Create;
    Str(temp_newitemcounter,temp_newitemcounterstring);
    new_menuitem.Caption:='New Item' + temp_newitemcounterstring;
    new_menuitem.Level:=DMenuItem.Level + 1;
    new_menuitem.PrevItem:=nil;
    new_menuitem.ParentMenu:=DMenuItem;
    new_menuitem.SubMenu:=nil;
    new_menuitem.NextItem:=nil;
    DMenuItem.SubMenu:=new_menuitem;
    // now we have to set the index of this DesignerMenuItem
    new_menuitem.Index:=0;
    Init(DMenuItem.SubMenu);
    Result:=DMenuItem.SubMenu;
  end else
  begin
    if (DMenuItem.SubMenu <> nil) then
      Result:=AddSubMenu(DMenuItem.SubMenu,Ident);
    if (Result = nil) and (DMenuItem.NextItem <> nil) then
      Result:=AddSubMenu(DMenuItem.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//

function TDesignerMainMenu.MoveUp(DMenuItem: TDesignerMenuItem; Ident: string): Integer;
var
  TempMI: TDesignerMenuItem;
begin
  Result:=0;
  if (DMenuItem.ID = Ident) then
  begin
    if (DMenuItem.PrevItem <> nil) then
    begin
      TempMI:=DMenuItem.PrevItem;
      TempMI.Index:=DMenuItem.Index;
      TempMI.NextItem:=DMenuItem.NextItem;
      DMenuItem.Index:=TempMI.Index - 1;
      DMenuItem.PrevItem:=TempMI.PrevItem;
      DMenuItem.ParentMenu:=TempMI.ParentMenu;
      TempMI.ParentMenu:=nil;
      TempMI.PrevItem:=DMenuItem;
      if (DMenuItem.ParentMenu = nil) and (DMenuItem.PrevItem = nil) then
        FRoot:=DMenuItem;
      DMenuItem.NextItem:=TempMI;
      if (DMenuItem.ParentMenu <> nil) then
        DMenuItem.ParentMenu.SubMenu:=DMenuItem;
      if (DMenuItem.PrevItem <> nil) then
        DMenuItem.PrevItem.NextItem:=DMenuItem;
      Result:=1;
    end;
  end else
  begin
    if (DMenuItem.SubMenu <> nil) then
      Result:=MoveUp(DMenuItem.SubMenu,Ident);
    if (Result = 0) then
      if (DMenuItem.NextItem <> nil) then
        Result:=MoveUp(DMenuItem.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//

function TDesignerMainMenu.MoveDown(DMenuItem: TDesignerMenuItem; Ident: string): Integer;
var
  TempMI: TDesignerMenuItem;
begin
  Result:=0;
  if (DMenuItem.ID = Ident) then
  begin
    if (DMenuItem.NextItem <> nil) then
    begin
      TempMI:=DMenuItem.NextItem;
      TempMI.PrevItem:=DMenuItem.PrevItem;
      DMenuItem.NextItem:=TempMI.NextItem;
      TempMI.NextItem:=DMenuItem;
      DMenuItem.PrevItem:=TempMI;
      TempMI.Index:=DMenuItem.Index;
      DMenuItem.Index:=TempMI.Index + 1;
      TempMI.ParentMenu:=DMenuItem.ParentMenu;
      DMenuItem.ParentMenu:=nil;
      if (TempMI.ParentMenu = nil) and (TempMI.PrevItem = nil) then
        FRoot:=TempMI;
      if (TempMI.ParentMenu <> nil) then
        TempMI.ParentMenu.SubMenu:=TempMI;
      if (DMenuItem.NextItem <> nil) then
        DMenuItem.NextItem.PrevItem:=DMenuItem;
      if (TempMI.PrevItem <> nil) then
        TempMI.PrevItem.NextItem:=TempMI;
      Result:=1;
    end;
  end else
  begin
    if (DMenuItem.SubMenu <> nil) then
      Result:=MoveDown(DMenuItem.SubMenu,Ident);
    if (Result = 0) then
      if (DMenuItem.NextItem <> nil) then
        Result:=MoveDown(DMenuItem.NextItem,Ident);
  end;
end;

// ------------------------------------------------------------------------------//
// ------------------------------------------------------------------------------//
function TDesignerMainMenu.DeleteItem(DMenuItem: TDesignerMenuItem): Integer;
var
  TempPreviousMI, TempNextMI: TDesignerMenuItem;
  TempParentMenuMI: TDesignerMenuItem;
  TempMI: TDesignerMenuItem;
  i: Integer;
begin
  Result:=0;
  if DMenuItem = nil then exit;

  TempParentMenuMI:=DMenuItem.ParentMenu;
  TempPreviousMI:=DMenuItem.PrevItem;
  TempNextMI:=DMenuItem.NextItem;
  
  if (TempParentMenuMI = nil) and (TempPreviousMI = nil) and
     (TempNextMI = nil)then
  begin
    if (DMenuItem.SubMenu <> nil) then
    begin
      DMenuItem.SubMenuPanel.Visible:=false;
      TempMI:=DMenuItem.SubMenu;
      while (TempMI <> nil) do
      begin
        TempMI.SelfPanel.Visible:=false;
        TempMI:=TempMI.NextItem;
      end;
      DMenuItem.SubMenu.Free;
    end;
    DMenuItem.SubMenu:=nil;
    DMenuItem.NextItem:=nil;
    Result:=2;
  end else
  begin
    if (TempPreviousMI <> nil) then
      TempPreviousMI.NextItem:=TempNextMI;
    if (TempNextMI <> nil) then
    begin
      TempNextMI.PrevItem:=TempPreviousMI;
      TempMI:=TempNextMI;
      i:=DMenuItem.Index;
      while (TempMI <> nil) do
      begin
        TempMI.Index:=i;
        Inc(i);
        TempMI:=TempMI.NextItem;
      end;
    end;
    if (TempParentMenuMI = nil) and (TempPreviousMI = nil) then
    begin
      TempNextMI.ParentMenu:=nil;
      FRoot:=TempNextMI;
    end;
    if (TempParentMenuMI <> nil) then
    begin
      if (TempNextMI <> nil) then
      begin
        TempParentMenuMI.SubMenu:=TempNextMI;
        TempNextMI.ParentMenu:=TempParentMenuMI;
      end else
      begin
        TempParentMenuMI.SubMenu:=nil;
        TempParentMenuMI.SubMenuPanel.Visible:=false;
        TempParentMenuMI.Active:=false;
        TempParentMenuMI.Selected:=true;
      end;
    end;
    DMenuItem.SelfPanel.Visible:=false;
    DMenuItem.SubMenuPanel.Visible:=false;
    DMenuItem.Free;
    Result:=1;
  end;
end;

// ------------------------------------------------------------------------------//
// ------------------------------------------------------------------------------//
function TDesignerMainMenu.ChangeCaption(DMenuItem: TDesignerMenuItem;
  const newcaption: string): Integer;
begin
  Result:=0;
  if DMenuItem.Caption=NewCaption then exit;
  InitIndexSequence;
  CreateIndexSequence(Root, DMenuItem.ID, 1);
  DMenuItem.Caption:=newcaption;
  Result:=1;
end;

procedure TDesignerMainMenu.HideDesignerMenuItem(DMenuItem: TDesignerMenuItem);
begin
  if (DMenuItem.SubMenu <> nil) then
  begin
    HideDesignerMenuItem(DMenuItem.SubMenu);
    DMenuItem.SubMenuPanel.Visible:=false;
    DMenuItem.SubMenu:=nil;
  end;
  if (DMenuItem.NextItem <> nil) then
  begin
    HideDesignerMenuItem(DMenuItem.NextItem);
    DMenuItem.NextItem:=nil;
  end;
  DMenuItem.SelfPanel.Visible:=false;
end;

// -------------------------------------------------------------------------------------------------------------------
// Finds DesignerMenuItem with identification Ident and returns a pointer to it
// -------------------------------------------------------------------------------------------------------------------
function TDesignerMainMenu.GetDesignerMenuItem(DMenuItem: TDesignerMenuItem;
  const Ident: string): TDesignerMenuItem;
begin
  Result:=nil;
  if DMenuItem=nil then exit;
  if (AnsiCompareText(DMenuItem.ID,Ident)=0) then
    Result:=DMenuItem
  else
  begin
    Result:=GetDesignerMenuItem(DMenuItem.SubMenu, Ident);
    if Result<>nil then exit;
    Result:=GetDesignerMenuItem(DMenuItem.NextItem, Ident);
  end;
end;

function TDesignerMainMenu.FindDesignerMenuItem(AMenuItem: TMenuItem): TDesignerMenuItem;
// search the corresponding designer menu item

  function FindRecursive(TheMenuItem: TMenuItem): TDesignerMenuItem;
  var
    ParentDesignerMenuItem: TDesignerMenuItem;
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
        Result := ParentDesignerMenuItem.SubMenu;
    end;
    if Result <> nil then
    begin
      i := TheMenuItem.MenuIndex;
      while (Result <> nil) and (i > 0) do
      begin
        Result := Result.NextItem;
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

function TDEsignerMainMenu.CreateIndexSequence(DMenuItem: TDesignerMenuItem;
  Ident: string; Ind: Integer): Boolean;
begin
  Result:=false;
  index_sequence[Ind]:=DMenuItem.Index;
  if (DMenuItem.ID = Ident) then
    Result:=true
  else
  begin
    if (DMenuItem.SubMenu <> nil) then
    begin
      if (CreateIndexSequence(DMenuItem.SubMenu,Ident,Ind + 1)) then
        Result:=true
      else
        index_sequence[Ind + 1]:=-1;
    end;
    if not (Result) then
      if (DMenuItem.NextItem <> nil) then
        if (CreateIndexSequence(DMenuItem.NextItem,Ident,Ind)) then
          Result:=true;
  end;
end;

// ------------------------------------------------------------------
// UPDATE Menu (type of update is specified via the Action parameter)
// ------------------------------------------------------------------
function TDesignerMainMenu.UpdateMenu(AMenuItem: TMenuItem;
  DMenuItem: TDesignerMenuItem; Ind, TheAction: Integer): TMenuItem;
var
  i: Integer;
  temp_menuitem: TMenuItem;
begin
  case TheAction of
   // Insert new AMenuItem after selected AMenuItem
  1: begin
       if (index_sequence[Ind + 1] = -1) then
       begin
         temp_menuitem:=TMenuItem.Create(fMenu.Owner);
         temp_menuitem.Caption:=DMenuItem.Caption;
         
         // code from Mattias (one of mail he sent me)
         temp_menuitem.Name:=GetDesigner.CreateUniqueComponentName(temp_menuitem.ClassName);
         AMenuItem.Insert(index_sequence[Ind] + 1, temp_menuitem);
         GetDesigner.PropertyEditorHook.PersistentAdded(temp_menuitem, true);
         GetDesigner.Modified;
         
       end else
       begin
         UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction);
       end;
     end;
   // Insert new AMenuItem before selected AMenuItem
   2: begin
       if (index_sequence[Ind + 1] = -1) then
       begin
         temp_menuitem:=TMenuItem.Create(fMenu.Owner);
         temp_menuitem.Caption:=DMenuItem.Caption;
         
         // code from Mattias (one of mail he sent me)
         temp_menuitem.Name:=GetDesigner.CreateUniqueComponentName(temp_menuitem.ClassName);
         AMenuItem.Insert(index_sequence[Ind] - 1, temp_menuitem);
         GetDesigner.PropertyEditorHook.PersistentAdded(temp_menuitem, true);
         GetDesigner.Modified;
       end else
       begin
         UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction);
       end;
      end;
    // Creates SubMenu to an existing AMenuItem
    3: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
         temp_menuitem:=TMenuItem.Create(fMenu.Owner);
           temp_menuitem.Caption:=DMenuItem.Caption;

           // code from Mattias (one of mail he sent me)
           temp_menuitem.Name:=GetDesigner.CreateUniqueComponentName(temp_menuitem.ClassName);
           AMenuItem[index_sequence[Ind]].Add(temp_menuitem);
           GetDesigner.PropertyEditorHook.PersistentAdded(temp_menuitem, true);
           GetDesigner.Modified;
         end else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction);
       end;
    // Moves Up(left) an AMenuItem (changes positions of this AMenuItem and its predecesor)
    4: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=AMenuItem[index_sequence[Ind] + 1];
           AMenuItem.Delete(index_sequence[Ind] + 1);
           AMenuItem.Insert(index_sequence[Ind], temp_menuitem);
         end else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction)
       end;
    // Moves Down(right) an AMenuItem (changes positions of this AMenuItem and its ancestor)
    5: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           temp_menuitem:=AMenuItem[index_sequence[Ind]];
           AMenuItem.Delete(index_sequence[Ind]);
           AMenuItem.Insert(index_sequence[Ind] - 1, temp_menuitem);
         end else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction)
       end;
    // Changes a caption of AMenuItem
    6: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           //writeln(AMenuItem[index_sequence[Ind]].Caption);
           AMenuItem[index_sequence[Ind]].Caption:=DMenuItem.Caption;
         end else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction)
       end;
    // Deletes a AMenuItem
    7: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           //AMenuItem.Remove(AMenuItem[index_sequence[Ind]]);
           temp_menuitem:=AMenuItem[index_sequence[Ind]];
           GlobalDesignHook.DeletePersistent(TPersistent(temp_menuitem));
           //AMenuItem[index_sequence[Ind]].Free;
         end else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction)
       end;
    // Deletes a SubMenu of AMenuItem
    8: begin
         if (index_sequence[Ind + 1] = -1) then
         begin
           for i := AMenuItem[index_sequence[Ind]].Count - 1 downto 0 do
           begin
             temp_menuitem:=AMenuItem[index_sequence[Ind]].Items[i];
             GlobalDesignHook.DeletePersistent(TPersistent(temp_menuitem));
             //AMenuItem[index_sequence[Ind]].Delete(i);
             //AMenuItem[index_sequence[Ind]].Items.Free
           end;
         end else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction);
       end;
    // Selectes a AMenuItem in the OI
    9: begin
         if (index_sequence[Ind + 1] = -1) then
           GetDesigner.SelectOnlyThisComponent(AMenuItem[index_sequence[Ind]])
         else
           UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction);
       end;
   // Return an AMenuItem
   10: begin
         if (index_sequence[Ind + 1] = -1) then
           Result:=AMenuItem[index_sequence[Ind]]
         else
           Result:=UpdateMenu(AMenuItem.Items[index_sequence[Ind]], DMenuItem, Ind + 1, TheAction);
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
var
  i: Integer;
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
         ListBoxView.Items.Add(lisEdit);
         ListBoxView.Items.Add(' ' + lisUndo);
         ListBoxView.Items.Add(' ' + lisRedo);
         ListBoxView.Items.Add(' -');
         ListBoxView.Items.Add(' ' + lisCut);
         ListBoxView.Items.Add(' ' + lisCopy);
         ListBoxView.Items.Add(' ' + lisPaste);
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
