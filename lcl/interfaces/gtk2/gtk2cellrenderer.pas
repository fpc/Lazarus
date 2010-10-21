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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  Classes, SysUtils, LCLType, LCLProc, Controls, StdCtrls, ComCtrls, LMessages,
  Gtk2Int, gtk2, gdk2, glib2, Gtk2Proc, Gtk2Def;
  
type
  PLCLIntfCellRenderer = ^TLCLIntfCellRenderer;
  TLCLIntfCellRenderer = record
    // ! the TextRenderer must be the first attribute of this record !
    TextRenderer: TGtkCellRendererText;
    Index: integer;
    ColumnIndex: Integer; // for TListView
  end;

  PLCLIntfCellRendererClass = ^TLCLIntfCellRendererClass;
  TLCLIntfCellRendererClass = record
    ParentClass: TGtkCellRendererTextClass;
    DefaultGtkGetSize: procedure(cell: PGtkCellRenderer;
                                 widget: PGtkWidget;
                                 cell_area: PGdkRectangle;
                                 x_offset: pgint;
                                 y_offset: pgint;
                                 width: pgint;
                                 height: pgint); cdecl;
    DefaultGtkRender: procedure(cell: PGtkCellRenderer;
                                window: PGdkWindow;
                                widget: PGtkWidget;
                                background_area: PGdkRectangle;
                                cell_area: PGdkRectangle;
                                expose_area:PGdkRectangle;
                                flags: TGtkCellRendererState); cdecl;
  end;

function LCLIntfCellRenderer_GetType: TGtkType;
function LCLIntfCellRenderer_New: PGtkCellRenderer;
procedure LCLIntfCellRenderer_CellDataFunc(cell_layout:PGtkCellLayout;
                                           cell: PGtkCellRenderer;
                                           tree_model: PGtkTreeModel;
                                           iter: PGtkTreeIter;
                                           data: gpointer); cdecl;

implementation
uses Gtk2Extra;

type
  TCustomListViewAccess = class(TCustomListView);

function GetControl(cell: PGtkCellRenderer; Widget: PGtkWidget): TWinControl;
var
  WidgetInfo: PWidgetInfo;
  LCLObject: TObject;
  MainWidget: PGtkWidget;
begin
  Result := nil;
  MainWidget := GetMainWidget(Widget);
  if MainWidget = nil then
    exit;
  WidgetInfo := GetWidgetInfo(MainWidget, false);
  if WidgetInfo = nil then
    WidgetInfo := GetWidgetInfo(cell, false);
  if WidgetInfo = nil then
    exit;
  LCLObject := WidgetInfo^.LCLObject; // the listbox or combobox
  Result := LCLObject as TWinControl;
end;

function GetItemIndex(cell: PLCLIntfCellRenderer; widget: PGtkWidget): Integer;
begin
  Result:=cell^.Index;
end;

procedure LCLIntfCellRenderer_GetSize(cell: PGtkCellRenderer; widget: PGtkWidget;
  cell_area: PGdkRectangle; x_offset, y_offset, width, height: pgint); cdecl;
var
  CellClass: PLCLIntfCellRendererClass;
  AWinControl: TWinControl;
  ItemIndex: Integer;
  Msg: TLMMeasureItem;
  MeasureItemStruct: TMeasureItemStruct;
begin
  CellClass:=PLCLIntfCellRendererClass(gtk_object_get_class(cell));
  CellClass^.DefaultGtkGetSize(cell, Widget, cell_area, x_offset, y_offset,
                               width, height);
  //DebugLn(['LCLIntfCellRenderer_GetSize ',GetWidgetDebugReport(Widget)]);
  AWinControl := GetControl(cell, widget);
  if [csDestroying,csLoading]*AWinControl.ComponentState<>[] then exit;

  if AWinControl is TCustomListbox then
    if TCustomListbox(AWinControl).Style < lbOwnerDrawVariable then
      exit;
  if AWinControl is TCustomCombobox then
    if TCustomCombobox(AWinControl).Style < csOwnerDrawVariable then
      exit;

  ItemIndex := GetItemIndex(PLCLIntfCellRenderer(cell), Widget);
  if ItemIndex < 0 then
    ItemIndex := 0;

  MeasureItemStruct.itemID := UINT(ItemIndex);
  MeasureItemStruct.itemWidth := UINT(width^);
  MeasureItemStruct.itemHeight := UINT(height^);
  Msg.Msg := LM_MEASUREITEM;
  Msg.MeasureItemStruct := @MeasureItemStruct;
  DeliverMessage(AWinControl, Msg);
  width^ := gint(MeasureItemStruct.itemWidth);
  height^ := gint(MeasureItemStruct.itemHeight);
end;

function GtkCellRendererStateToListViewDrawState(CellState: TGtkCellRendererState): TCustomDrawState;
begin
  Result := [];
  if CellState and GTK_CELL_RENDERER_SELECTED > 0 then Result := Result + [cdsSelected];
  if CellState and GTK_CELL_RENDERER_PRELIT > 0 then Result := Result + [cdsHot];
  if CellState and GTK_CELL_RENDERER_INSENSITIVE > 0 then Result := Result + [cdsDisabled, cdsGrayed];
  if CellState and GTK_CELL_RENDERER_FOCUSED > 0 then Result := Result + [cdsFocused];
end;

procedure LCLIntfCellRenderer_Render(cell: PGtkCellRenderer; Window: PGdkWindow;
  Widget: PGtkWidget; background_area: PGdkRectangle; cell_area: PGdkRectangle;
  expose_area: PGdkRectangle; flags: TGtkCellRendererState); cdecl;
var
  CellClass: PLCLIntfCellRendererClass;
  AWinControl: TWinControl;
  ItemIndex: Integer;
  ColumnIndex: Integer;
  AreaRect: TRect;
  State: TOwnerDrawState;
  Msg: TLMDrawListItem;
  DCWidget: PGtkWidget;
  LVTarget: TCustomDrawTarget;
  LVStage: TCustomDrawStage;
  LVState: TCustomDrawState;
  LVSubItem: Integer;
  TmpDC1,
  TmpDC2: HDC;
  SkipDefaultPaint: Boolean;
begin
  {DebugLn(['LCLIntfCellRenderer_Render cell=',dbgs(cell),
    ' ',GetWidgetDebugReport(Widget),' ',
    ' background_area=',dbgGRect(background_area),
    ' cell_area=',dbgGRect(cell_area),
    ' expose_area=',dbgGRect(expose_area)]);}

  ColumnIndex := PLCLIntfCellRenderer(cell)^.ColumnIndex;

  if ColumnIndex > -1 then // listview
  begin
    AWinControl := GetControl(cell, widget);
    AreaRect := Bounds(background_area^.x, background_area^.y,
                     background_area^.Width, background_area^.Height);

    ItemIndex := GetItemIndex(PLCLIntfCellRenderer(cell), Widget);

    if ItemIndex < 0 then
      ItemIndex := 0;

    if ColumnIndex > 0 then
      LVTarget := dtSubItem
    else
      LVTarget := dtItem;
    LVSubItem := ColumnIndex-1;
    LVStage := cdPrePaint;
    LVState := GtkCellRendererStateToListViewDrawState(flags);
    DCWidget:=Widget;
    TmpDC1:=GTK2WidgetSet.CreateDCForWidget(DCWidget,Window,false);
    TmpDC2 := TCustomListViewAccess(AWinControl).Canvas.Handle;
    TCustomListViewAccess(AWinControl).Canvas.Handle := TmpDC1;
    // paint
    SkipDefaultPaint := cdrSkipDefault in TCustomListViewAccess(AWinControl).IntfCustomDraw(LVTarget, LVStage, ItemIndex, LVSubItem, LVState, @AreaRect);

    if SkipDefaultPaint then
    begin
      GTK2WidgetSet.ReleaseDC(HWnd(PtrUInt(Widget)),TmpDC1);
      TCustomListViewAccess(AWinControl).Canvas.Handle := TmpDC2;
      Exit;
    end;
  end;

  // draw default
  CellClass := PLCLIntfCellRendererClass(gtk_object_get_class(cell));
  CellClass^.DefaultGtkRender(cell, Window, Widget, background_area, cell_area,
                              expose_area, flags);
  
  if ColumnIndex < 0 then  // is a listbox or combobox
  begin
    // send LM_DrawListItem message
    AWinControl := GetControl(cell, widget);
    if [csDestroying,csLoading]*AWinControl.ComponentState<>[] then exit;
  
    // check if the LCL object wants item paint messages
    if AWinControl is TCustomListbox then
      if TCustomListbox(AWinControl).Style = lbStandard then
        exit;
    if AWinControl is TCustomCombobox then
      if TCustomCombobox(AWinControl).Style < csOwnerDrawFixed then
        exit;

    // get itemindex and area
  
    AreaRect := Bounds(background_area^.x, background_area^.y,
                     background_area^.Width, background_area^.Height);

    ItemIndex := GetItemIndex(PLCLIntfCellRenderer(cell), Widget);

    if ItemIndex < 0 then
      ItemIndex := 0;

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
  end
  else // is a listview
  begin
    LVStage := cdPostPaint;
    // paint
    TCustomListViewAccess(AWinControl).IntfCustomDraw(LVTarget, LVStage, ItemIndex, LVSubItem, LVState, @AreaRect);

    TCustomListViewAccess(AWinControl).Canvas.Handle := TmpDC2;
    GTK2WidgetSet.ReleaseDC(HWnd(PtrUInt(Widget)),TmpDC1);
    Exit;
  end;

  // ListBox and ComboBox
  // create message and deliverFillChar(Msg,SizeOf(Msg),0);
  Msg.Msg:=LM_DrawListItem;
  New(Msg.DrawListItemStruct);
  try
    FillChar(Msg.DrawListItemStruct^,SizeOf(TDrawListItemStruct),0);
    with Msg.DrawListItemStruct^ do begin
      ItemID:=UINT(ItemIndex);
      Area:=AreaRect;
      //DebugLn(['LCLIntfCellRenderer_Render Widget=',GetWidgetDebugReport(Widget^.parent),' Area=',dbgs(Area)]);
      DCWidget:=Widget;
      if (DCWidget^.parent<>nil)
      and (GtkWidgetIsA(DCWidget^.parent,gtk_menu_item_get_type)) then begin
        // the Widget is a sub widget of a menu item
        // -> allow the LCL to paint over the whole menu item
        DCWidget:=DCWidget^.parent;
        Area:=Rect(0,0,DCWidget^.allocation.width,DCWidget^.allocation.height);
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
  LCLClass := PLCLIntfCellRendererClass(aClass);
  RendererClass := GTK_CELL_RENDERER_CLASS(aClass);
  LCLClass^.DefaultGtkGetSize := RendererClass^.get_size;
  LCLClass^.DefaultGtkRender := RendererClass^.render;
  RendererClass^.get_size := @LCLIntfCellRenderer_GetSize;
  RendererClass^.render := @LCLIntfCellRenderer_Render;
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

procedure LCLIntfCellRenderer_CellDataFunc(cell_layout:PGtkCellLayout;
  cell: PGtkCellRenderer; tree_model: PGtkTreeModel; iter: PGtkTreeIter;
  data: gpointer); cdecl;
var
  LCLCellRenderer: PLCLIntfCellRenderer absolute cell;
  WidgetInfo: PWidgetInfo;
  APath: PGtkTreePath;
  Str: String;
  ListColumn: TListColumn;
  ListItem: TListItem;
  Value: TGValue;
begin
  if G_IS_OBJECT(cell) = false then
    exit;
  //DebugLn(['LCLIntfCellRenderer_CellDataFunc stamp=',iter^.stamp,' tree_model=',dbgs(tree_model),' cell=',dbgs(cell)]);
  APath := gtk_tree_model_get_path(tree_model,iter);
  LCLCellRenderer^.Index := gtk_tree_path_get_indices(APath)^;
  LCLCellRenderer^.ColumnIndex := -1;
  gtk_tree_path_free(APath);

  Value.g_type := G_TYPE_STRING;


  WidgetInfo := PWidgetInfo(data);
  if (WidgetInfo <> nil) and (WidgetInfo^.LCLObject.InheritsFrom(TCustomListView)) then
  begin
    gtk_tree_model_get(tree_model, iter, [0, @ListItem, -1]);
    if (ListItem = nil) and TCustomListView(WidgetInfo^.LCLObject).OwnerData then
      ListItem := TCustomListView(WidgetInfo^.LCLObject).Items[LCLCellRenderer^.Index];
    if ListItem = nil then
      Exit;
    ListColumn := TListColumn(g_object_get_data(G_OBJECT(cell_layout), 'TListColumn'));
    if ListColumn = nil then
      LCLCellRenderer^.ColumnIndex := -1
    else
      LCLCellRenderer^.ColumnIndex := ListColumn.Index;

    if LCLCellRenderer^.ColumnIndex <= 0 then
      Str := ListItem.Caption
    else
      if ListColumn.Index-1 <= ListItem.SubItems.Count-1 then
        Str := ListItem.SubItems.Strings[LCLCellRenderer^.ColumnIndex-1];

    Value.data[0].v_pointer := PChar(Str);
    g_object_set_property(PGObject(cell), 'text', @Value);
  end;

  //DebugLn(['LCLIntfCellRenderer_CellDataFunc ItemIndex=',LCLCellRenderer^.Index]);
end;

end.
