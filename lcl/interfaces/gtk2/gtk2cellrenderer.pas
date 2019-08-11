{ $Id: gtk2wsstdctrls.pp 9520 2006-06-28 21:26:52Z mattias $}
{
 *****************************************************************************
 *                             Gtk2CellRenderer.pas                          *
 *                             --------------------                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 
  An extended gtk_cell_renderer, to provide hooks for the LCL.
  For example for custom drawing.
 
}
unit Gtk2CellRenderer;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  gtk2, gdk2, glib2,
  // LCL
  LCLType, LCLProc, Controls, StdCtrls, ComCtrls, LMessages,
  Gtk2Int, Gtk2Proc, Gtk2Def;
  
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

uses
  Gtk2Extra;

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
  WidgetInfo := GetWidgetInfo(MainWidget);
  if WidgetInfo = nil then
    WidgetInfo := GetWidgetInfo(cell);
  if WidgetInfo = nil then
    exit;
  LCLObject := WidgetInfo^.LCLObject; // the listbox or combobox
  Result := LCLObject as TWinControl;
end;

function GetItemIndex(cell: PLCLIntfCellRenderer; {%H-}widget: PGtkWidget): Integer;
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
  // DebugLn(['LCLIntfCellRenderer_GetSize ',GetWidgetDebugReport(Widget)]);
  AWinControl := GetControl(cell, widget);
  if [csDestroying,csLoading]*AWinControl.ComponentState<>[] then exit;

  if AWinControl is TCustomListbox then
    if TCustomListbox(AWinControl).Style < lbOwnerDrawFixed then
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
  OwnerDrawnListView: Boolean;
begin
  {DebugLn(['LCLIntfCellRenderer_Render cell=',dbgs(cell),
    ' ',GetWidgetDebugReport(Widget),' ',
    ' background_area=',dbgGRect(background_area),
    ' cell_area=',dbgGRect(cell_area),
    ' expose_area=',dbgGRect(expose_area)]);}

  ColumnIndex := PLCLIntfCellRenderer(cell)^.ColumnIndex;

  AWinControl := GetControl(cell, widget);
  if (ColumnIndex = -1) and (AWinControl <> nil) and
    (AWinControl.FCompStyle = csListView) then
      ColumnIndex := 0;

  OwnerDrawnListView := False;
  if ColumnIndex > -1 then // listview
  begin
    OwnerDrawnListView := TCustomListViewAccess(AWinControl).OwnerDraw and
      (TCustomListViewAccess(AWinControl).ViewStyle = vsReport);

    AreaRect := Bounds(background_area^.x, background_area^.y,
                     background_area^.Width, background_area^.Height);


    ItemIndex := GetItemIndex(PLCLIntfCellRenderer(cell), Widget);

    if ItemIndex < 0 then
      ItemIndex := 0;

    if ColumnIndex > 0 then
      LVTarget := dtSubItem
    else
      LVTarget := dtItem;
    if AWinControl.FCompStyle = csListView then
      LVSubItem := ColumnIndex
    else
      LVSubItem := ColumnIndex - 1;
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
      GTK2WidgetSet.ReleaseDC(HWnd({%H-}PtrUInt(Widget)),TmpDC1);
      TCustomListViewAccess(AWinControl).Canvas.Handle := TmpDC2;
      if not OwnerDrawnListView then
        Exit;
    end;
  end;

  // draw default
  CellClass := PLCLIntfCellRendererClass(gtk_object_get_class(cell));

  // do not call DefaultGtkRender when we are custom drawn listbox.issue #23093
  if ColumnIndex < 0 then
  begin
    if [csDestroying,csLoading,csDesigning]*AWinControl.ComponentState<>[] then
      AWinControl := nil;
    if AWinControl is TCustomListbox then
      if TCustomListbox(AWinControl).Style = lbStandard then
        AWinControl := nil;
    if AWinControl is TCustomCombobox then
      AWinControl := nil;
  end;
  // do default draw only if we are not customdrawn.
  if (ColumnIndex > -1) or ((ColumnIndex < 0) and (AWinControl = nil)) then
  begin
    if not OwnerDrawnListView then
      CellClass^.DefaultGtkRender(cell, Window, Widget, background_area, cell_area,
        expose_area, flags);
  end;
  
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
    State:=[odBackgroundPainted];
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
    GTK2WidgetSet.ReleaseDC(HWnd({%H-}PtrUInt(Widget)),TmpDC1);
    if not OwnerDrawnListView then
      Exit;
  end;

  // ListBox and ComboBox
  // create message and deliverFillChar(Msg,SizeOf(Msg),0);
  if OwnerDrawnListView then
  begin
    // we are TListView (GtkTreeView) with OwnerDraw + vsReport
    Msg.Msg := CN_DRAWITEM;

    // collect state flags
    State := [];
    if (flags and GTK_CELL_RENDERER_SELECTED)>0 then
      Include(State, odSelected);
    if not GTK_WIDGET_SENSITIVE(Widget) then
      Include(State, odInactive);
    if GTK_WIDGET_HAS_DEFAULT(Widget) then
      Include(State, odDefault);
    if (flags and GTK_CELL_RENDERER_FOCUSED) <> 0 then
      Include(State, odFocused);

    AreaRect := Bounds(expose_area^.x, expose_area^.y,
                     expose_area^.Width, expose_area^.Height);
    if gtk_tree_view_get_headers_visible(PGtkTreeView(Widget)) then
    begin
      inc(AreaRect.Top, background_area^.height);
      inc(AreaRect.Bottom, background_area^.height);
    end;
  end else
    Msg.Msg:=LM_DrawListItem;
  New(Msg.DrawListItemStruct);
  try
    FillChar(Msg.DrawListItemStruct^,SizeOf(TDrawListItemStruct),0);
    with Msg.DrawListItemStruct^ do
    begin
      ItemID:=UINT(ItemIndex);
      Area:=AreaRect;
      // DebugLn(['LCLIntfCellRenderer_Render Widget=',GetWidgetDebugReport(Widget^.parent),' Area=',dbgs(Area)]);
      DCWidget:=Widget;
      if (DCWidget^.parent<>nil) and
        (GtkWidgetIsA(DCWidget^.parent,gtk_menu_item_get_type)) then
      begin
        // the Widget is a sub widget of a menu item
        // -> allow the LCL to paint over the whole menu item
        DCWidget := DCWidget^.parent;
        Area:=Rect(0,0,DCWidget^.allocation.width,DCWidget^.allocation.height);
      end;
      DC := GTK2WidgetSet.CreateDCForWidget(DCWidget,Window,false);
      ItemState:=State;
    end;
    DeliverMessage(AWinControl, Msg);
    GTK2WidgetSet.ReleaseDC(HWnd({%H-}PtrUInt(Widget)),Msg.DrawListItemStruct^.DC);
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

procedure LCLIntfCellRenderer_Init({%H-}Instance:PGTypeInstance;
  {%H-}theClass: Pointer); cdecl;
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
var
  IID: TGUID;
  S: AnsiString;
begin
  if (crType = 0)
  then begin
    // patch by tk: in case of shared library we must create unique name
    if CreateGUID(IID) = 0 then
      S := Format('LCL%d%d%d', [Word(IID.time_low), Word(IID.time_low shr 16), IID.time_mid]);
    crType := gtk_type_from_name(PAnsiChar(S));
    if crType = 0 then begin
      crInfo.type_name:=PAnsiChar(S);
      crType := gtk_type_unique(gtk_cell_renderer_text_get_type, @crInfo);
    end;
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

  APath := gtk_tree_model_get_path(tree_model,iter);
  LCLCellRenderer^.Index := gtk_tree_path_get_indices(APath)^;
  LCLCellRenderer^.ColumnIndex := -1;
  gtk_tree_path_free(APath);

  WidgetInfo := PWidgetInfo(data);
  // DebugLn(['LCLIntfCellRenderer_CellDataFunc stamp=',iter^.stamp,' tree_model=',dbgs(tree_model),' cell=',dbgs(cell),' WidgetInfo=',WidgetInfo <> nil,' Time=',TimeToStr(Now)]);

  if (WidgetInfo <> nil) and
    (WidgetInfo^.LCLObject is TCustomComboBox) and
    (TCustomComboBox(WidgetInfo^.LCLObject).Style = csDropDownList) and
    not (TCustomComboBox(WidgetInfo^.LCLObject).DroppedDown) then
  begin
    Value.g_type := G_TYPE_UINT;
    Value.data[0].v_uint := 0;
    g_object_get_property(PgObject(cell),'ypad',@Value);
    Value.data[0].v_int := 0;
    g_object_set_property(PGObject(cell), 'ypad', @Value);
  end else
  if (WidgetInfo <> nil) and (WidgetInfo^.LCLObject.InheritsFrom(TCustomListView)) then
  begin
    // DebugLn(['LCLIntfCellRenderer_CellDataFunc stamp=',iter^.stamp,' tree_model=',dbgs(tree_model),' cell=',dbgs(cell),' WidgetInfo=',WidgetInfo <> nil,' Time=',TimeToStr(Now)]);
    Value.g_type := G_TYPE_STRING;
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

  // DebugLn(['LCLIntfCellRenderer_CellDataFunc ItemIndex=',LCLCellRenderer^.Index]);
end;

end.
