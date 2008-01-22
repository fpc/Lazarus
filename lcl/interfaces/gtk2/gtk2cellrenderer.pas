{ $Id: gtk2wsstdctrls.pp 9520 2006-06-28 21:26:52Z mattias $}
{
 *****************************************************************************
 *                             Gtk2CellRenderer.pas                          *
 *                             --------------------                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  An extended gtk_cell_renderer, to provide hooks for the LCL.
  For example for custom drawing.
 
}
unit Gtk2CellRenderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLProc, Controls, StdCtrls, LMessages,
  Gtk2Int, gtk2, gdk2, glib2, GtkProc, GtkDef;
  
type
  PLCLIntfCellRenderer = ^TLCLIntfCellRenderer;
  TLCLIntfCellRenderer = record
    // ! the TextRenderer must be the first attribute of this record !
    TextRenderer: TGtkCellRendererText;
    ItemIndex: integer; // for TCustomListBox: the ItemIndex of the cell
  end;

  PLCLIntfCellRendererClass = ^TLCLIntfCellRendererClass;
  TLCLIntfCellRendererClass = record
    ParentClass: TGtkCellRendererTextClass;
    DefaultGtkRender : procedure (cell:PGtkCellRenderer; window:PGdkWindow;
                 widget:PGtkWidget; background_area:PGdkRectangle;
                 cell_area:PGdkRectangle;
                 expose_area:PGdkRectangle; flags:TGtkCellRendererState); cdecl;
  end;

function LCLIntfCellRenderer_GetType: TGtkType;
function LCLIntfCellRenderer_New: PGtkCellRenderer;

implementation
uses GtkExtra;

procedure LCLIntfCellRenderer_Render(cell: PGtkCellRenderer; Window: PGdkWindow;
  Widget: PGtkWidget; background_area: PGdkRectangle; cell_area: PGdkRectangle;
  expose_area: PGdkRectangle; flags: TGtkCellRendererState); cdecl;
var
  CellClass: PLCLIntfCellRendererClass;
  WidgetInfo: PWidgetInfo;
  LCLObject: TObject;
  MainWidget: PGtkWidget;
  AWinControl: TWinControl;
  ItemIndex: Integer;
  AreaRect: TRect;
  State: TBaseOwnerDrawState;
  Msg: TLMDrawListItem;
  ItemPath: PGtkTreePath;
  Column: PGtkTreeViewColumn;
  DCWidget: PGtkWidget;
begin
  {DebugLn(['LCLIntfCellRenderer_Render cell=',dbgs(cell),
    ' ',GetWidgetDebugReport(Widget),' ',
    ' background_area=',dbgGRect(background_area),
    ' cell_area=',dbgGRect(cell_area),
    ' expose_area=',dbgGRect(expose_area)]);}

  // draw default
  CellClass:=PLCLIntfCellRendererClass(gtk_object_get_class(cell));
  CellClass^.DefaultGtkRender(cell, Window, Widget, background_area,cell_area,
                              expose_area, flags);
  
  // send LM_DrawListItem message
  MainWidget:=GetMainWidget(Widget);
  if MainWidget=nil then exit;
  WidgetInfo:=GetWidgetInfo(MainWidget,false);
  if WidgetInfo=nil then WidgetInfo:=GetWidgetInfo(cell,false);
  if WidgetInfo=nil then exit;
  LCLObject:=WidgetInfo^.LCLObject; // the listbox or combobox
  AWinControl:=LCLObject as TWinControl;
  if [csDestroying,csLoading]*AWinControl.ComponentState<>[] then exit;
  
  // check if the LCL object wants item paint messages
  if AWinControl is TCustomListbox then
    if TCustomListbox(AWinControl).Style = lbStandard then
      exit;
  if AWinControl is TCustomCombobox then
    if TCustomCombobox(AWinControl).Style < csOwnerDrawFixed then
      exit;

  // get itemindex and area
  
  AreaRect:=Bounds(background_area^.x,background_area^.y,
                   background_area^.Width,background_area^.Height);

  ItemIndex:=-1;
  if GTK_IS_TREE_VIEW(Widget) then begin
    //check the four possible corners of the cell
    if gtk_tree_view_get_path_at_pos(PGtkTreeView(Widget),cell_area^.x, cell_area^.y, ItemPath, column, nil, nil)
    or gtk_tree_view_get_path_at_pos(PGtkTreeView(Widget),cell_area^.x, cell_area^.y+cell_area^.height, ItemPath, column, nil, nil)
    or gtk_tree_view_get_path_at_pos(PGtkTreeView(Widget),cell_area^.x+cell_area^.width, cell_area^.y, ItemPath, column, nil, nil)
    or gtk_tree_view_get_path_at_pos(PGtkTreeView(Widget),cell_area^.x+cell_area^.width, cell_area^.y+cell_area^.height, ItemPath, column, nil, nil)
    then
    begin
      ItemIndex := StrToInt(gtk_tree_path_to_string(ItemPath));
      gtk_tree_path_free(ItemPath);
    end;
  end
  else if AWinControl is TCustomComboBox then begin
    ItemPath := gtk_cell_view_get_displayed_row(Widget);
    if ItemPath <> nil then
    begin
      ItemIndex := StrToInt(gtk_tree_path_to_string(ItemPath));
      gtk_tree_path_free(ItemPath);
      if (Widget^.parent <> nil) and (GtkWidgetIsA(Widget^.parent,gtk_menu_item_get_type)) then
        AreaRect:=Bounds(0,0,
                         Widget^.Parent^.allocation.width,
                         Widget^.Parent^.allocation.height)
      else
        AreaRect:=Bounds(0,0,
                         Widget^.allocation.width,
                         Widget^.allocation.height);
    end;
  end;

  if ItemIndex < 0 then ItemIndex := 0;

  // collect state flags
  State:=[odPainted];
  if (flags and GTK_CELL_RENDERER_SELECTED)>0 then
    Include(State, odSelected);
  if not GTK_WIDGET_SENSITIVE(Widget) then
    Include(State, odInactive);
  if GTK_WIDGET_HAS_DEFAULT(Widget) then
    Include(State, odDefault);
  if (flags and GTK_CELL_RENDERER_FOCUSED) <> 0 then
    Include(State, odFocused);
    
  if AWinControl is TCustomCombobox then begin
    if TCustomComboBox(AWinControl).DroppedDown
    and ((flags and GTK_CELL_RENDERER_PRELIT)>0) then
    Include(State,odSelected);
  end;

  // create message and deliver
  FillChar(Msg,SizeOf(Msg),0);
  Msg.Msg:=LM_DrawListItem;
  New(Msg.DrawListItemStruct);
  try
    FillChar(Msg.DrawListItemStruct^,SizeOf(TDrawListItemStruct),0);
    with Msg.DrawListItemStruct^ do begin
      ItemID:=ItemIndex;
      Area:=AreaRect;
      //DebugLn(['LCLIntfCellRenderer_Render Widget=',GetWidgetDebugReport(Widget^.parent),' Area=',dbgs(Area)]);
      DCWidget:=Widget;
      if (DCWidget^.parent<>nil)
      and (GtkWidgetIsA(DCWidget^.parent,gtk_menu_item_get_type)) then begin
        // the Widget is a sub widget of a menu item
        // -> allow the LCL to paint over the whole menu item
        DCWidget:=DCWidget^.parent;
      end;
      DC:=GTK2WidgetSet.CreateDCForWidget(DCWidget,Window,false);
      ItemState:=State;
    end;
    DeliverMessage(AWinControl, Msg);
    GTK2WidgetSet.ReleaseDC(HWnd(PtrUInt(Widget)),Msg.DrawListItemStruct^.DC);
  finally
    Dispose(Msg.DrawListItemStruct);
  end;

  //DebugLn(['LCLIntfCellRenderer_Render END ',DbgSName(LCLObject)]);
end;

procedure LCLIntfCellRenderer_ClassInit(aClass: Pointer); cdecl;
//aClass: PLCLIntfCellRendererClass
var
  LCLClass: PLCLIntfCellRendererClass;
  RendererClass: PGtkCellRendererClass;
begin
  //DebugLn(['LCLIntfCellRenderer_ClassInit ']);
  LCLClass:=PLCLIntfCellRendererClass(aClass);
  RendererClass:=GTK_CELL_RENDERER_CLASS(aClass);
  LCLClass^.DefaultGtkRender:=RendererClass^.render;
  RendererClass^.render:=@LCLIntfCellRenderer_Render;
end;

procedure LCLIntfCellRenderer_Init(Instance:PGTypeInstance;
  theClass: Pointer); cdecl;
// Instance: PLCLIntfCellRenderer;
// theClass: PLCLIntfCellRendererClass
begin
  //DebugLn(['LCLIntfCellRenderer_Init ']);
end;

function LCLIntfCellRenderer_GetType: TGtkType;
const
  CR_NAME = 'LCLIntfCellRenderer';
  crType: TGtkType = 0;
  crInfo: TGTKTypeInfo = (
    type_name: CR_NAME;
    object_size: SizeOf(TLCLIntfCellRenderer)+100; // a TLCLIntfCellRenderer
    class_size: SizeOf(TLCLIntfCellRendererClass)+100;
    class_init_func: @LCLIntfCellRenderer_ClassInit;
    object_init_func : @LCLIntfCellRenderer_Init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (crType = 0)
  then begin
    crType := gtk_type_from_name(CR_NAME);
    if crType = 0
    then crType := gtk_type_unique(gtk_cell_renderer_text_get_type, @crInfo);
  end;
  Result := crType;
end;

function LCLIntfCellRenderer_New: PGtkCellRenderer;
begin
  Result := g_object_new(LCLIntfCellRenderer_GetType, nil,[]);
end;

end.

