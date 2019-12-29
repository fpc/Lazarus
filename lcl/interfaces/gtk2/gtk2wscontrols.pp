{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSControls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk2WSControls;

{$mode objfpc}{$H+}
{$I gtk2defines.inc}

interface

uses
  // RTL
  Classes, SysUtils,
  Gtk2, Gdk2, Glib2, gdk2pixbuf,
  // LCL
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, ComCtrls,
////////////////////////////////////////////////////
  Graphics,

  Gtk2Globals, Gtk2Def,
  Gtk2Proc, LCLType, LCLProc,
  WSControls, WSProc, Gtk2WinapiWindow;
  

type

  { TGtk2WSDragImageListResolution }

  TGtk2WSDragImageListResolution = class(TWSDragImageListResolution)
  published
    class function BeginDrag(const ADragImageList: TDragImageListResolution; {%H-}Window: HWND; AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const {%H-}ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const {%H-}ADragImageList: TDragImageListResolution); override;
    class function HideDragImage(const {%H-}ADragImageList: TDragImageListResolution;
      {%H-}ALockedWindow: HWND; {%H-}DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const {%H-}ADragImageList: TDragImageListResolution;
      {%H-}ALockedWindow: HWND; X, Y: Integer; {%H-}DoLock: Boolean): Boolean; override;
  end;

  { TGtkWSControl }

  TGtk2WSControl = class(TWSControl)
  published
  end;


  { TGtk2WSWinControl }

  TGtk2WSWinControl = class(TWSWinControl)
  private
  protected
  public
    // Internal public
    class procedure SetCallbacks(const AGTKObject: PGTKObject; const AComponent: TComponent);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;

    class procedure AddControl(const AControl: TControl); override;
    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;
    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;

    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const {%H-}AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, {%H-}UseRightToLeftReading, {%H-}UseRightToLeftScrollBar : Boolean); override;

    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;
    class procedure Repaint(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
  end;

  { TGtk2WSGraphicControl }

  TGtk2WSGraphicControl = class(TWSGraphicControl)
  published
  end;

  { TGtk2WSCustomControl }

  TGtk2WSCustomControl = class(TWSCustomControl)
  published
  end;

  { TGtk2WSImageList }

  TGtk2WSImageList = class(TWSImageList)
  published
  end;

  { TGtkWSBaseScrollingWinControl }
  {
    TGtkWSBaseScrollingWinControl is a shared gtk only base implementation of
    all scrolling widgets, like TListView, TScrollingWinControl etc.
    It only creates a scrolling widget and handles the LM_HSCROLL and LM_VSCROLL
    messages
  }
  PBaseScrollingWinControlData = ^TBaseScrollingWinControlData;
  TBaseScrollingWinControlData = record
    HValue: Integer;
    HScroll: PGTKWidget;
    VValue: Integer;
    VScroll: PGTKWidget;
  end;

  { TGtk2WSBaseScrollingWinControl }

  TGtk2WSBaseScrollingWinControl = class(TWSWinControl)
  public
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
  end;

function GetWidgetHAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
function GetWidgetVAdjustment(AWidget: PGTKWidget): PGTKAdjustment;

var
  HasWarnedLibOverlay: boolean = false;

implementation

uses
  Gtk2Int, LMessages, Gtk2WSPrivate, Forms;

{ TGtk2WSWinControl }


class function TGtk2WSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
  ScrollBar: PGtkWidget;
  Adjustment: PGtkAdjustment;
begin
  Widget := GTK2WidgetSet.CreateAPIWidget(AWinControl);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle({%H-}PtrUInt(Widget));
  if Result = 0 then Exit;

  WidgetInfo := GetWidgetInfo(Widget); // Widget info already created in CreateAPIWidget
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);

  TGtk2WSWinControl.SetCallbacks(GTK_OBJECT(Widget), AWinControl);

  // scrollbars
  if (GetWidgetClassName(GTK_SCROLLED_WINDOW(Widget)^.vscrollbar)='OsScrollbar')
  or (GetWidgetClassName(GTK_SCROLLED_WINDOW(Widget)^.hscrollbar)='OsScrollbar')
  then begin
    // ubuntu liboverlay scrollbar is active
    if not HasWarnedLibOverlay then begin
      HasWarnedLibOverlay:=true;
      debugln(['WARNING: liboverlay_scrollbar is active for control=',AWinControl,'. Set environment option LIBOVERLAY_SCROLLBAR=0 before starting this application, otherwise scrollbars will not work properly.']);
    end;
  end;

  ScrollBar:=GTK_SCROLLED_WINDOW(Widget)^.hscrollbar;
  if (GetWidgetClassName(ScrollBar)='OsScrollbar')
  then begin
    // the ubuntu scroll bar eats the change-value signal => use value-changed
    Adjustment:=gtk_scrolled_window_get_hadjustment(GTK_SCROLLED_WINDOW(Widget));
    g_signal_connect_after(Adjustment, 'value-changed',TGCallback(@Gtk2RangeUbuntuScrollCB), WidgetInfo);
  end else begin
    g_signal_connect_after(ScrollBar, 'change-value',
      TGCallback(@Gtk2RangeScrollCB), WidgetInfo);
  end;

  ScrollBar:=GTK_SCROLLED_WINDOW(Widget)^.vscrollbar;
  if (GetWidgetClassName(ScrollBar)='OsScrollbar')
  then begin
    // the ubuntu scroll bar eats the change-value signal => use value-changed
    Adjustment:=gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(Widget));
    g_signal_connect_after(Adjustment, 'value-changed',TGCallback(@Gtk2RangeUbuntuScrollCB), WidgetInfo);
  end else begin
    g_signal_connect_after(ScrollBar, 'change-value',
      TGCallback(@Gtk2RangeScrollCB), WidgetInfo);
  end;

  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.hscrollbar, 'button-press-event',
    TGCallback(@Gtk2RangeScrollPressCB), WidgetInfo);
  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.hscrollbar, 'button-release-event',
    TGCallback(@Gtk2RangeScrollReleaseCB), WidgetInfo);
    g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.vscrollbar, 'button-press-event',
    TGCallback(@Gtk2RangeScrollPressCB), WidgetInfo);
  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.vscrollbar, 'button-release-event',
    TGCallback(@Gtk2RangeScrollReleaseCB), WidgetInfo);

  g_signal_connect(Widget, 'scroll-event', TGCallback(@Gtk2ScrolledWindowScrollCB), WidgetInfo);
end;

class procedure TGtk2WSWinControl.SetBiDiMode(const AWinControl : TWinControl;
  UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean
  );
const
  WidgetDirection : array[boolean] of longint = (GTK_TEXT_DIR_LTR, GTK_TEXT_DIR_RTL);
var
  Info: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBiDiMode') then
    Exit;
  gtk_widget_set_direction({%H-}PGtkWidget(AWinControl.Handle),
    WidgetDirection[UseRightToLeftAlign]);
  Info := GetWidgetInfo({%H-}PGtkWidget(AWinControl.Handle));
  if Info <> nil then
  begin
    if Info^.CoreWidget <> nil then
      gtk_widget_set_direction(Info^.CoreWidget,
        WidgetDirection[UseRightToLeftAlign]);
    if Info^.ClientWidget <> nil then
      gtk_widget_set_direction(Info^.ClientWidget,
        WidgetDirection[UseRightToLeftAlign]);
  end;
end;

function Gtk1GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  CS: PChar;
  Handle: HWND;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetText')
  then Exit(False);
  Result := true;
  Handle := AWinControl.Handle;
  case AWinControl.fCompStyle of
    csComboBox:
      begin
        AText := StrPas(gtk_entry_get_text(PGtkEntry({%H-}PGtkCombo(Handle)^.entry)));
      end;
    csEdit: AText:= StrPas(gtk_entry_get_text({%H-}PgtkEntry(Handle)));
    csSpinEdit: AText:= StrPas(gtk_entry_get_text(@{%H-}PGtkSpinButton(Handle)^.entry));
    csMemo:
      begin
        CS := gtk_editable_get_chars(PGtkEditable(
          GetOrCreateWidgetInfo({%H-}Pointer(Handle))^.CoreWidget), 0, -1);
        AText := StrPas(CS);
        g_free(CS);
      end;
  else
    Result := false;
  end;
end;


class function TGtk2WSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  TextBuf: PGtkTextBuffer;
  StartIter,
  EndIter: TGtkTextIter;
  CS: PChar;
  Handle: HWND;
begin
  Result := true;
  Handle := AWinControl.Handle;
  case AWinControl.fCompStyle of
    csMemo:
      begin
        TextBuf := gtk_text_view_get_buffer(PGtkTextView(GetOrCreateWidgetInfo({%H-}Pointer(Handle))^.CoreWidget));
        gtk_text_buffer_get_start_iter(TextBuf, @StartIter);
        gtk_text_buffer_get_end_iter(TextBuf, @EndIter);
        CS := gtk_text_buffer_get_text(TextBuf, @StartIter, @EndIter, False);
        AText := StrPas(CS);
        g_free(CS);
      end;
    else
      Result:=Gtk1GetText(AWinControl, AText);
  end;
end;


procedure Gtk1SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle')
  then Exit;

  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  if GtkWidgetIsA(Widget, GTKAPIWidget_GetType) then
    GTKAPIWidget_SetShadowType(PGTKAPIWidget(Widget), BorderStyleShadowMap[ABorderStyle])
  else
  if GTK_IS_FRAME(Widget) then
    gtk_frame_set_shadow_type(PGtkFrame(Widget), BorderStyleShadowMap[ABorderStyle])
  else
  if GTK_IS_VIEWPORT(Widget) then
    gtk_viewport_set_shadow_type(PGtkViewport(Widget), BorderStyleShadowMap[ABorderStyle]);
end;

class procedure TGtk2WSWinControl.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBorderStyle')
  then Exit;
  
  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  if GTK_IS_SCROLLED_WINDOW(Widget) then
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget), BorderStyleShadowMap[ABorderStyle])
  else
  if GTK_IS_ENTRY(Widget) then
    gtk_entry_set_has_frame(PGtkEntry(Widget), ABorderStyle <> bsNone)
  else
    Gtk1SetBorderStyle(AWinControl, ABorderStyle);
end;

function GetWidgetHAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
begin
  if GtkWidgetIsA(AWidget,GTK_TYPE_SCROLLED_WINDOW) then
    Result:=gtk_scrolled_window_get_hadjustment(PGTKScrolledWindow(AWidget))
  else if GtkWidgetIsA(AWidget,GTK_TYPE_TREE_VIEW) then
    Result:=gtk_tree_view_get_hadjustment(PGtkTreeView(AWidget))
  else
    Result:=nil;
end;

function GetWidgetVAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
begin
  if GtkWidgetIsA(AWidget,GTK_TYPE_SCROLLED_WINDOW) then
    Result:=gtk_scrolled_window_get_vadjustment(PGTKScrolledWindow(AWidget))
  else if GtkWidgetIsA(AWidget,GTK_TYPE_TREE_VIEW) then
    Result:=gtk_tree_view_get_vadjustment(PGtkTreeView(AWidget))
  else
    Result:=nil;
end;

{ TGtk2WSDragImageListResolution }

class function TGtk2WSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer
  ): Boolean;
var
  ABitmap: TBitmap;
  GDIObject: PGDIObject;
  Pixmap: PGdkPixmap;
  Mask: PGdkBitmap;
begin
  ABitmap := TBitmap.Create;
  ADragImageList.GetBitmap(AIndex, ABitmap);

  if (ABitmap.Handle = 0) or (ABitmap.Width = 0) or (ABitmap.Height = 0) then
  begin
    Result := False;
    Exit;
  end;

  GDIObject := {%H-}PGDIObject(ABitmap.Handle);

  Pixmap := nil;
  Mask := nil;
  case GDIObject^.GDIBitmapType of
    gbBitmap:
      begin
        Pixmap := GDIObject^.GDIBitmapObject;
        gdk_bitmap_ref(Pixmap);
        Mask := nil;
      end;
    gbPixmap:
      begin
        Pixmap := GDIObject^.GDIPixmapObject.Image;
        Mask := GDIObject^.GDIPixmapObject.Mask;
        gdk_pixmap_ref(Pixmap);
        gdk_bitmap_ref(Mask);
      end;
    gbPixbuf:
      begin
        Pixmap := nil;
        Mask := nil;
        // todo: TEST
        //gdk_pixbuf_render_pixmap_and_mask(GDIObject^.GDIPixbufObject, Pixmap, Mask, $80);
        gdk_pixbuf_render_pixmap_and_mask(GDIObject^.GDIPixbufObject, Pixmap, Mask, $80);
      end;
  end;

  Result := Gtk2Widgetset.DragImageList_BeginDrag(Pixmap, Mask, ADragImageList.DragHotSpot);
  if Result then
    Gtk2Widgetset.DragImageList_DragMove(X, Y);
  gdk_pixmap_unref(Pixmap);
  gdk_bitmap_unref(Mask);
  ABitmap.Free;
end;

class function TGtk2WSDragImageListResolution.DragMove(
  const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean;
begin
  Result := Gtk2Widgetset.DragImageList_DragMove(X, Y);
end;

class procedure TGtk2WSDragImageListResolution.EndDrag(
  const ADragImageList: TDragImageListResolution);
begin
  Gtk2Widgetset.DragImageList_EndDrag;
end;

class function TGtk2WSDragImageListResolution.HideDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND; DoUnLock: Boolean
  ): Boolean;
begin
  Result := Gtk2Widgetset.DragImageList_SetVisible(False);
end;

class function TGtk2WSDragImageListResolution.ShowDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND; X, Y: Integer;
  DoLock: Boolean): Boolean;
begin
  Result := Gtk2Widgetset.DragImageList_DragMove(X, Y) and Gtk2Widgetset.DragImageList_SetVisible(True);
end;


{ TGtk2WSWinControl }

type
  TWinControlHack = class(TWinControl)
  end;

function Gtk2TreeViewEditorEvent(widget: PGtkWidget; event: PGdkEvent; data: GPointer): gboolean; cdecl;
var
  R: TRect;
  Alloc: TGtkAllocation;
  w: PGtkWidget;
  AOrientation: TGtkOrientation;
begin
  Result := CallBackDefaultReturn;
  case event^._type of
    GDK_FOCUS_CHANGE:
    begin
      // cheat GtkTreeView container , so we are visible and ready for input.
      if event^.focus_change._in = 1 then
      begin
        R := TWinControl(Data).BoundsRect;
        Alloc.x := R.Left;
        Alloc.y := R.Top;
        Alloc.width := R.Right - R.Left;
        Alloc.height := R.Bottom - R.Top;
        gtk_widget_size_allocate(Widget, @Alloc);
      end else
      begin
        w := gtk_widget_get_parent(Widget);
        if Assigned(w) and GTK_IS_ICON_VIEW(w) then
        begin
          //gtk2 does not layout items correctly when iconArrangement is iaTop.
          //so we force it to do so.
          AOrientation := gtk_icon_view_get_orientation(PGtkIconView(w));
          if AOrientation = GTK_ORIENTATION_HORIZONTAL then
            gtk_icon_view_set_orientation(PGtkIconView(w), GTK_ORIENTATION_VERTICAL)
          else
            gtk_icon_view_set_orientation(PGtkIconView(w), GTK_ORIENTATION_HORIZONTAL);
          gtk_icon_view_set_orientation(PGtkIconView(w), AOrientation)
        end;
      end;
    end;
  end;
end;

class procedure TGtk2WSWinControl.AddControl(const AControl: TControl);
var
  AParent: TWinControl;
  ParentWidget: PGTKWidget;
  ChildWidget: PGTKWidget;
  pFixed: PGTKWidget;
begin
  AParent := TWinControl(AControl).Parent;
  // DebugLn('LM_AddChild: ',dbgsName(AControl),' ',dbgs(AParent<>nil));
  if not Assigned(AParent) then
    Assert(true, Format('Trace: [TGtkWSWinControl.AddControl] %s --> Parent is not assigned', [AControl.ClassName]))
  else
  begin
    // DebugLn(Format('Trace:  [TGtkWSWinControl.AddControl] %s --> Calling Add Child: %s', [AParent.ClassName, AControl.ClassName]));

    ParentWidget := {%H-}PGtkwidget(AParent.Handle);
    pFixed := GetFixedWidget(ParentWidget);

    // gtk2 is pretty tricky about adding editor into control
    if (AParent.FCompStyle = csListView) and
      (TWinControl(AControl).FCompStyle = csEdit) then
    begin
      ChildWidget := {%H-}PGtkWidget(TWinControl(AControl).Handle);
      ParentWidget := gtk_bin_get_child(PGtkBin(PFixed)); // treeview
      // MUST allocate some size before adding it to container !
      gtk_widget_set_size_request(ChildWidget, 80, 25);
      gtk_widget_set_parent(ChildWidget, ParentWidget);
      // now we connect our GtkEntry directly to event filter
      g_signal_connect(PGtkObject(ChildWidget), 'event',
        gtk_signal_func(@Gtk2TreeViewEditorEvent), AControl);
    end else
    begin
      if pFixed <> ParentWidget then
      begin
        // parent changed for child
        ChildWidget := {%H-}PGtkWidget(TWinControl(AControl).Handle);
        FixedPutControl(pFixed, ChildWidget, AControl.Left, AControl.Top);
        RegroupAccelerator(ChildWidget);
      end;
    end;
  end;
end;

class function TGtk2WSWinControl.CanFocus(const AWinControl: TWinControl): Boolean;
var
  Widget, FocusWidget: PGtkWidget;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := {%H-}PGtkWidget(AWinControl.Handle);
    FocusWidget := FindFocusWidget(Widget);
    Result := (FocusWidget <> nil) and GTK_WIDGET_CAN_FOCUS(FocusWidget);
  end else
    Result := False;
end;

class procedure TGtk2WSWinControl.ConstraintsChange(const AWinControl: TWinControl);
var
  Widget: PGtkWidget;
  Geometry: TGdkGeometry;
begin
  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  if (Widget <> nil) and (GtkWidgetIsA(Widget, gtk_window_get_type)) then
  begin
    with Geometry do
    begin
      if AWinControl.Constraints.MinWidth > 0 then
        min_width := AWinControl.Constraints.MinWidth
      else
        min_width := 1;
      if AWinControl.Constraints.MaxWidth > 0 then
        max_width := AWinControl.Constraints.MaxWidth
      else
        max_width := 32767;
      if AWinControl.Constraints.MinHeight > 0 then
        min_height := AWinControl.Constraints.MinHeight
      else
        min_height := 1;
      if AWinControl.Constraints.MaxHeight > 0 then
        max_height := AWinControl.Constraints.MaxHeight
      else
        max_height := 32767;

      base_width := AWinControl.Width;
      base_height := AWinControl.Height;
      width_inc := 1;
      height_inc := 1;
      min_aspect := 0;
      max_aspect := 1;
      win_gravity := gtk_window_get_gravity(PGtkWindow(Widget));
    end;
    //debugln('TGtk2WSWinControl.ConstraintsChange A ',GetWidgetDebugReport(Widget),' max=',dbgs(Geometry.max_width),'x',dbgs(Geometry.max_height));
    gtk_window_set_geometry_hints(PGtkWindow(Widget), nil, @Geometry,
      GDK_HINT_POS or GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE);
  end;
end;

class procedure TGtk2WSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  //DebugLn('TGtk2WSWinControl.DestroyHandle ',DbgSName(AWinControl));
  Gtk2WidgetSet.DestroyLCLComponent(AWinControl);
end;

class procedure TGtk2WSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Invalidate')
  then Exit;

  //DebugLn('Trace:Trying to invalidate window... !!!');
  gtk_widget_queue_draw({%H-}PGtkWidget(AWinControl.Handle));
end;

class procedure TGtk2WSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  // other methods use ShowHide also, can't move code
  Gtk2WidgetSet.SetVisible(AWinControl, AWinControl.HandleObjectShouldBeVisible);
  InvalidateLastWFPResult(AWinControl, AWinControl.BoundsRect);
end;

class procedure TGtk2WSWinControl.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
var
  Scrolled: PGtkScrolledWindow;
  Adjustment: PGtkAdjustment;
  h, v: Double;
  NewPos: Double;
begin
  if not AWinControl.HandleAllocated then exit;
  Scrolled := GTK_SCROLLED_WINDOW({%H-}Pointer(AWinControl.Handle));
  if not GTK_IS_SCROLLED_WINDOW(Scrolled) then
    exit;
  Adjustment := gtk_scrolled_window_get_hadjustment(Scrolled);
  if Adjustment <> nil then
  begin
    h := gtk_adjustment_get_value(Adjustment);
    NewPos := Adjustment^.upper - Adjustment^.page_size;
    if h - DeltaX <= NewPos then
      NewPos := h - DeltaX;
    gtk_adjustment_set_value(Adjustment, NewPos);
  end;
  Adjustment := gtk_scrolled_window_get_vadjustment(Scrolled);
  if Adjustment <> nil then
  begin
    v := gtk_adjustment_get_value(Adjustment);
    NewPos := Adjustment^.upper - Adjustment^.page_size;
    if v - DeltaY <= NewPos then
      NewPos := v - DeltaY;
    gtk_adjustment_set_value(Adjustment, NewPos);
  end;
  AWinControl.Invalidate;
end;

class procedure TGtk2WSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  AForm: TCustomForm;
  Geometry: TGdkGeometry;
  AHints: TGdkWindowHints;
  AFixedWidthHeight: Boolean;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds')
  then Exit;
  ResizeHandle(AWinControl);
  InvalidateLastWFPResult(AWinControl, Rect(ALeft, ATop, AWidth, AHeight));
  if not AWinControl.Visible then // Gtk2WSForms.ShowHide will correct visibility
    exit;
  if (not (AWinControl is TCustomForm)) or (AWinControl.Parent<>nil)
  or (AWinControl.ParentWindow<>0) then
    exit;
  AForm := TCustomForm(AWinControl);
  if not (csDesigning in AForm.ComponentState) and
    AForm.HandleObjectShouldBeVisible then
  begin
    // we must set fixed size, gtk_window_set_resizable does not work
    // as expected for some reason.issue #20741.
    // Constraints fix issue #29563
    AFixedWidthHeight := AForm.BorderStyle in [bsDialog, bsSingle, bsToolWindow];
    FillChar(Geometry{%H-}, SizeOf(TGdkGeometry), 0);
    with Geometry do
    begin
      if not AFixedWidthHeight and (AForm.Constraints.MinWidth > 0) then
        min_width := AForm.Constraints.MinWidth
      else
      if AFixedWidthHeight then
        min_width := AForm.Width;
      if not AFixedWidthHeight and (AForm.Constraints.MaxWidth > 0) then
        max_width := AForm.Constraints.MaxWidth
      else
      if AFixedWidthHeight then
        max_width := AForm.Width;
      if not AFixedWidthHeight and (AForm.Constraints.MinHeight > 0) then
        min_height := AForm.Constraints.MinHeight
      else
        if AFixedWidthHeight then
          min_height := AForm.Height;
      if not AFixedWidthHeight and (AForm.Constraints.MaxHeight > 0) then
        max_height := AForm.Constraints.MaxHeight
      else
        if AFixedWidthHeight then
          max_height := AForm.Height;

      if AForm.Constraints.MaxHeight = 0 then
        max_height := 32767;
      if AForm.Constraints.MaxWidth = 0 then
        max_width := 32767;

      base_width := AForm.Width;
      base_height := AForm.Height;
      width_inc := 1;
      height_inc := 1;
      min_aspect := 0;
      max_aspect := 1;
      win_gravity := gtk_window_get_gravity({%H-}PGtkWindow(AForm.Handle));
    end;
    //debugln('TGtk2WSWinControl.SetBounds A maxw=',dbgs(Geometry.max_width),' maxh=',dbgs(Geometry.max_height),
    //' base w=',dbgs(Geometry.base_width),' h=',dbgs(Geometry.base_height));
    if AFixedWidthHeight then
      gtk_window_set_geometry_hints({%H-}PGtkWindow(AForm.Handle), nil, @Geometry,
        GDK_HINT_POS or GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE)
    else
    begin
      if AForm.BorderStyle <> bsNone then
      begin
        AHints := GDK_HINT_POS or GDK_HINT_BASE_SIZE;
        if (AForm.Constraints.MinHeight > 0) or (AForm.Constraints.MinWidth > 0) then
          AHints := AHints or GDK_HINT_MIN_SIZE;
        if (AForm.Constraints.MaxHeight > 0) or (AForm.Constraints.MaxWidth > 0) then begin
          AHints := AHints or GDK_HINT_MAX_SIZE;
          { Work around for only one maximum specified; see TGtk2WSWinControl.ConstraintsChange }
          if AForm.Constraints.MaxHeight = 0 then
            Geometry.max_height := 32767;
          if AForm.Constraints.MaxWidth = 0 then
            Geometry.max_width := 32767;
        end;
        {$IFDEF HASX}
        if (AHints and GDK_HINT_MIN_SIZE = 0) and (AHints and GDK_HINT_MAX_SIZE = 0) and
          (Gtk2WidgetSet.GetWindowManager = 'openbox') then
        else
        {$ENDIF}
          gtk_window_set_geometry_hints({%H-}PGtkWindow(AForm.Handle), nil, @Geometry,
            AHints);
      end;
      gtk_window_resize({%H-}PGtkWindow(AForm.Handle), AForm.Width, AForm.Height);
    end;
  end;
end;


class procedure TGtk2WSWinControl.SetCallbacks(const AGTKObject: PGTKObject;
  const AComponent: TComponent);
begin
  Gtk2WidgetSet.SetCommonCallbacks(AGtkObject, AComponent);
end;

class procedure TGtk2WSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl;
  const AOldPos, ANewPos: Integer; const AChildren: TFPList);
var
  n: Integer;
  child: TWinControlHack;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition')
  then Exit;

  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)') then
    Exit;

  if (ANewPos <= 0) or (ANewPos >= AChildren.Count - 1) then
  begin
    // simple
    Child := TWinControlHack(AChild);
    if ANewPos <= 0 then // bottom
      TGtkPrivateWidgetClass(
          Child.WidgetSetClass.WSPrivate).SetZPosition(Child, wszpBack)
    else
      TGtkPrivateWidgetClass(
          Child.WidgetSetClass.WSPrivate).SetZPosition(Child, wszpFront);
  end else
  begin
    for n := 1 to AChildren.Count - 1 do
    begin
      Child := TWinControlHack(AChildren[n]);
      if Child.HandleAllocated then
        TGtkPrivateWidgetClass(
          Child.WidgetSetClass.WSPrivate).SetZPosition(Child, wszpBack);
    end;
  end;
end;

class procedure TGtk2WSWinControl.SetCursor(const AWinControl: TWinControl; const ACursor: HCursor);
var
  WidgetInfo: PWidgetInfo;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetCursor')
  then Exit;

  WidgetInfo := GetWidgetInfo({%H-}Pointer(AWinControl.Handle));
  if WidgetInfo^.ControlCursor <> ACursor then
  begin
    WidgetInfo^.ControlCursor := ACursor;
    TGtkPrivateWidgetClass(AWinControl.WidgetSetClass.WSPrivate).UpdateCursor(WidgetInfo);
  end;
end;

class procedure TGtk2WSWinControl.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont')
  then Exit;

  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  if GtkWidgetIsA(Widget, GTKAPIWidget_GetType) then
    exit;

  //DebugLn('TGtk2WSWinControl.SetFont ',DbgSName(AWinControl));
  Gtk2WidgetSet.SetWidgetFont(Widget, AFont);
  Gtk2WidgetSet.SetWidgetColor(Widget, AFont.Color, clNone,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED,
                               GTK_STYLE_TEXT]);
end;

class procedure TGtk2WSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
var
  Widget: PGtkWidget;
  Allocation: TGTKAllocation;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetPos')
  then Exit;
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtk2WSWinControl.SetPos ',DbgSName(AWinControl),' ',ALeft,',',ATop]);
  {$ENDIF}

  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  Allocation.X := gint16(ALeft);
  Allocation.Y := gint16(ATop);
  Allocation.Width := guint16(Widget^.Allocation.Width);
  Allocation.Height := guint16(Widget^.Allocation.Height);
  gtk_widget_size_allocate(Widget, @Allocation);// Beware: this triggers callbacks
end;

class procedure TGtk2WSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
var
  Widget: PGtkWidget;
  Allocation: TGTKAllocation;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetSize')
  then Exit;
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TGtk2WSWinControl.SetSize ',DbgSName(AWinControl),' ',AWidth,',',AHeight]);
  {$ENDIF}

  Widget := {%H-}PGtkWidget(AWinControl.Handle);
  Allocation.X := Widget^.Allocation.X;
  Allocation.Y := Widget^.Allocation.Y;
  Allocation.Width := guint16(AWidth);
  Allocation.Height := guint16(AHeight);
  gtk_widget_size_allocate(Widget, @Allocation);// Beware: this triggers callbacks
end;

class procedure TGtk2WSWinControl.SetColor(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor')
  then Exit;

  // do not change color of scrollbar. issue #22996
  if (AWinControl.FCompStyle = csScrollBar) then
    exit;

  if ((csOpaque in AWinControl.ControlStyle) and
      GtkWidgetIsA({%H-}pGtkWidget(AWinControl.handle),GTKAPIWidget_GetType)) then
    Exit;

  //DebugLn('TGtk2WSWinControl.SetColor ',DbgSName(AWinControl));
  Gtk2WidgetSet.SetWidgetColor({%H-}PGtkWidget(AWinControl.Handle),
                              AWinControl.Font.Color, AWinControl.Color,
                              [GTK_STATE_NORMAL, GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT, GTK_STATE_SELECTED]);
//    GtkWidgetSet.setWidgetFont(pGtkWidget(AWinControl.handle),aWinControl.font);
  UpdateWidgetStyleOfControl(AWinControl);
end;

class procedure TGtk2WSWinControl.SetText(const AWinControl: TWinControl;
  const AText: string);

  procedure SetNotebookPageTabLabel;
  var
    NoteBookWidget: PGtkWidget; // the notebook
    PageWidget: PGtkWidget;     // the page (content widget)
    TabWidget: PGtkWidget;      // the tab (hbox containing a pixmap, a label
                                //          and a close button)
    TabLabelWidget: PGtkWidget; // the label in the tab
    MenuWidget: PGtkWidget;     // the popup menu (hbox containing a pixmap and
                                // a label)
    MenuLabelWidget: PGtkWidget; // the label in the popup menu item
    NewText: PChar;
  begin
    if (AWinControl.Parent <> nil) and (AWinControl.Parent is TTabControl) then
      exit;

    // dig through the hierachy to get the labels
    NoteBookWidget:={%H-}PGtkWidget((AWinControl.Parent).Handle);
    PageWidget:={%H-}PGtkWidget(AWinControl.Handle);
    TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
    if TabWidget<>nil then
      TabLabelWidget:=g_object_get_data(PGObject(TabWidget), 'TabLabel')
    else
      TabLabelWidget:=nil;
    MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                            PageWidget);
    if MenuWidget<>nil then
      MenuLabelWidget:=g_object_get_data(PGObject(MenuWidget), 'TabMenuLabel')
    else
      MenuLabelWidget:=nil;
    // set new text
    NewText:=PChar(AText);
    if TabLabelWidget<>nil then
      gtk_label_set_text(pGtkLabel(TabLabelWidget), NewText);
    if MenuLabelWidget<>nil then
      gtk_label_set_text(pGtkLabel(MenuLabelWidget), NewText);
  end;

var
  P : Pointer;
  aLabel, pLabel: pchar;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText')
  then Exit;

  //TODO: create classprocedures for this in the corresponding classes

  P := {%H-}Pointer(AWinControl.Handle);
  Assert(p <> nil, 'Trace:WARNING: [TGtkWidgetSet.SetLabel] --> got nil pointer');
  //DebugLn('Trace:Setting Str1 in SetLabel');
  pLabel := pchar(AText);

  case AWinControl.fCompStyle of
    csBitBtn,
    csButton: DebugLn('[WARNING] Obsolete call to TGTKOBject.SetLabel for ', AWinControl.ClassName);

    csForm,
    csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog,
    csPreviewFileDialog,
    csColorDialog,
    csFontDialog:
      if GtkWidgetIsA(p,gtk_window_get_type) then
        gtk_window_set_title(pGtkWindow(p),PLabel);

    csCheckBox,
    csToggleBox,
    csRadioButton:
      begin
        aLabel := Ampersands2Underscore(PLabel);
        try
          gtk_label_set_text(
            pGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(p)^.Button))),
                      aLabel);
          gtk_label_parse_uline(
            pGtkLabel(gtk_bin_get_child(PGtkBin(@PGTKToggleButton(p)^.Button))),
                      aLabel);
        finally
          StrDispose(aLabel);
        end;
      end;

    csEdit:
      begin
        LockOnChange(PGtkObject(p),+1);
        gtk_entry_set_text(pGtkEntry(P), pLabel);
        LockOnChange(PGtkObject(p),-1);
      end;

    csSpinEdit:
      begin
        LockOnChange(PGtkObject(p),+1);
        gtk_entry_set_text(pGtkEntry(P), pLabel);
        gtk_spin_button_update(PGtkSpinButton(p));
        LockOnChange(PGtkObject(p),-1);
      end;

    csMemo:
      begin
        P:= GetOrCreateWidgetInfo(P)^.CoreWidget;
        //debugln('TGtk2WSWinControl.SetText A ',dbgs(gtk_text_get_length(PGtkText(P))),' AText="',AText,'"');
        gtk_text_freeze(PGtkText(P));
        gtk_text_set_point(PGtkText(P), 0);
        gtk_text_forward_delete(PGtkText(P), gtk_text_get_length(PGtkText(P)));
        gtk_text_insert(PGtkText(P), nil, nil, nil, pLabel, -1);
        gtk_text_thaw(PGtkText(P));
        //debugln('TGtk2WSWinControl.SetText B ',dbgs(gtk_text_get_length(PGtkText(P))));
      end;

    csPage:
      SetNotebookPageTabLabel;

      // else
      // DebugLn('WARNING: [TGtkWidgetSet.SetLabel] --> not handled for class ',Sender.ClassName);
  end;
  //DebugLn(Format('trace:  [TGtkWidgetSet.SetLabel] %s --> END', [AWinControl.ClassName]));
end;

class procedure TGtk2WSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
var
  GtkWidget, FixedWidget: PGtkWidget;
  GdkBitmap: PGDKBitmap;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetShape') then
    Exit;

  GtkWidget := {%H-}PGtkWidget(AWinControl.Handle);
  FixedWidget := GetFixedWidget(GtkWidget);

  if AShape <> 0 then
  begin
    if Gtk2Widgetset.IsValidGDIObjectType(AShape, gdiBitmap) then
      GdkBitmap := {%H-}PGdiObject(AShape)^.GDIBitmapObject
    else
      GdkBitmap := nil;
  end
  else
    GdkBitmap := nil;

  gtk_widget_shape_combine_mask(GtkWidget, GdkBitmap, 0, 0);
  if FixedWidget <> GtkWidget then
    gtk_widget_shape_combine_mask(FixedWidget, GdkBitmap, 0, 0);
end;

{
  Paint control to X, Y point of device context.
}
class procedure TGtk2WSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
var
  DC: TGtkDeviceContext absolute ADC;

  procedure PaintGtkForm(AWindow: PGdkWindow);
  var
    W, H: gint;
    Pixbuf: PGdkPixbuf;
    MenuPixBuf: PGdkPixBuf;
    AMenuBar: PGtkWidget;
    OffsetY: Integer;
  begin
    OffsetY := 0;
    MenuPixBuf := nil;

    if Assigned(TCustomForm(AWinControl).Menu) then
    begin
      AMenuBar := {%H-}PGtkWidget(TCustomForm(AWinControl).Menu.Handle);
      if GTK_IS_MENU_BAR(AMenuBar) and GTK_WIDGET_VISIBLE(AMenuBar) then
      begin
        OffsetY := AMenuBar^.allocation.height;
        MenuPixbuf := gdk_pixbuf_get_from_drawable(nil, AMenuBar^.Window, nil,
          0, 0, 0, 0, AMenuBar^.allocation.Width, AMenuBar^.Allocation.Height);

        gdk_pixbuf_render_to_drawable(MenuPixbuf, DC.Drawable, DC.GC, 0, 0, X, Y + OffsetY,
          AMenuBar^.allocation.Width, AMenuBar^.Allocation.Height, GDK_RGB_DITHER_NONE, 0, 0);

        gdk_pixbuf_unref(MenuPixbuf);
      end;
    end;
    gdk_window_get_size(AWindow, @W, @H);

    Pixbuf := gdk_pixbuf_get_from_drawable(nil, AWindow, nil,
      0, 0, 0, 0, W, H);

    // put menubar into form screenshoot too
    if OffsetY <> 0 then
    begin
      MenuPixBuf := gdk_pixbuf_scale_simple(PixBuf, W, H - OffsetY,GDK_INTERP_NEAREST);
      gdk_pixbuf_render_to_drawable(MenuPixbuf, DC.Drawable, DC.GC, 0, 0, X, Y + (OffsetY * 2),
        -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
      if MenuPixBuf <> nil then
        gdk_pixbuf_unref(MenuPixBuf);
    end else
      gdk_pixbuf_render_to_drawable(Pixbuf, DC.Drawable, DC.GC, 0, 0, X, Y,
        -1, -1, GDK_RGB_DITHER_NONE, 0, 0);

    gdk_pixbuf_unref(Pixbuf);
  end;

  procedure PaintWindow(AWindow: PGdkWindow);
  var
    W, H: gint;
    Pixbuf: PGdkPixbuf;
  begin
    gdk_window_get_size(AWindow, @W, @H);
    // for some reason gdk_window_copy_area does not work
    Pixbuf := gdk_pixbuf_get_from_drawable(nil, AWindow, nil,
      0, 0, 0, 0, W, H);
    gdk_pixbuf_render_to_drawable(Pixbuf, DC.Drawable, DC.GC, 0, 0, X, Y,
      -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
    gdk_pixbuf_unref(Pixbuf);
  end;

  procedure PaintWidget(AWidget: PGtkWidget);
  var
    AWindow: PGdkWindow;
  begin
    if (AWinControl.FCompStyle = csForm) then
      PaintGtkForm(AWidget^.window)
    else
    begin
      AWindow := GetControlWindow(AWidget);
      if AWindow <> nil then
        PaintWindow(AWindow);
    end;
  end;

begin
  if not WSCheckHandleAllocated(AWinControl, 'PaintTo') then
    Exit;
  PaintWidget(GetFixedWidget({%H-}PGtkWidget(AWinControl.Handle)));
end;

class procedure TGtk2WSWinControl.Repaint(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Repaint')
  then Exit;
  gtk_widget_queue_draw({%H-}PGtkWidget(AWinControl.Handle));
end;

{ TGtk2WSBaseScrollingWinControl }

function Gtk2WSBaseScrollingWinControl_HValueChanged(AAdjustment: PGTKAdjustment; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  ScrollingData: PBaseScrollingWinControlData;
  Msg: TLMHScroll;
  OldValue, V, U, L, StepI, PageI: Integer;
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;

  ScrollingData := AInfo^.UserData;

  // round values
  V := Round(AAdjustment^.Value);
  U := Round(AAdjustment^.Upper);
  L := Round(AAdjustment^.Lower);
  StepI := Round(AAdjustment^.Step_Increment);
  PageI := Round(AAdjustment^.Page_Increment);

  OldValue := ScrollingData^.HValue;
  ScrollingData^.HValue := V;

  // get keystates
  Mask := 0;
  if ScrollingData^.HScroll <> nil then
  begin
    {$IFDEF UseGDKErrorTrap}
    BeginGDKErrorTrap;
    {$ENDIF}
    gdk_window_get_pointer(GetControlWindow(ScrollingData^.HScroll), @X, @Y, @Mask);
    {$IFDEF UseGDKErrorTrap}
    EndGDKErrorTrap;
    {$ENDIF}
  end;

  Msg.msg := LM_HSCROLL;
  // get scrollcode
  if ssLeft in GTKEventStateToShiftState(Word(Mask))
  then Msg.ScrollCode := SB_THUMBTRACK
  else if V <= L
  then Msg.ScrollCode := SB_TOP
  else if V >= U
  then Msg.ScrollCode := SB_BOTTOM
  else if V - OldValue = StepI
  then Msg.ScrollCode := SB_LINERIGHT
  else if OldValue - V = StepI
  then Msg.ScrollCode := SB_LINELEFT
  else if V - OldValue = PageI
  then Msg.ScrollCode := SB_PAGERIGHT
  else if OldValue - V = PageI
  then Msg.ScrollCode := SB_PAGELEFT
  else Msg.ScrollCode := SB_THUMBPOSITION;
  Msg.Pos := V;
  if V < High(Msg.SmallPos)
  then Msg.SmallPos := V
  else Msg.SmallPos := High(Msg.SmallPos);
  Msg.ScrollBar := HWND({%H-}PtrUInt(ScrollingData^.HScroll));

  Result := (DeliverMessage(AInfo^.LCLObject, Msg) <> 0) xor CallBackDefaultReturn;
end;

function Gtk2WSBaseScrollingWinControl_VValueChanged(AAdjustment: PGTKAdjustment; AInfo: PWidgetInfo): GBoolean; cdecl;
var
  ScrollingData: PBaseScrollingWinControlData;
  Msg: TLMHScroll;
  OldValue, V, U, L, StepI, PageI: Integer;
  X, Y: GInt;
  Mask: TGdkModifierType;
begin
  Result := CallBackDefaultReturn;
  if AInfo^.ChangeLock > 0 then Exit;

  ScrollingData := AInfo^.UserData;

  // round values
  V := Round(AAdjustment^.Value);
  U := Round(AAdjustment^.Upper);
  L := Round(AAdjustment^.Lower);
  StepI := Round(AAdjustment^.Step_Increment);
  PageI := Round(AAdjustment^.Page_Increment);

  OldValue := ScrollingData^.VValue;
  ScrollingData^.VValue := V;

  // get keystates
  Mask := 0;
  if ScrollingData^.VScroll <> nil then
  begin
    {$IFDEF UseGDKErrorTrap}
    BeginGDKErrorTrap;
    {$ENDIF}
    gdk_window_get_pointer(GetControlWindow(ScrollingData^.VScroll), @X, @Y, @Mask);
    {$IFDEF UseGDKErrorTrap}
    EndGDKErrorTrap;
    {$ENDIF}
  end;

  Msg.msg := LM_VSCROLL;
  // Get scrollcode
  if ssLeft in GTKEventStateToShiftState(Word(Mask))
  then Msg.ScrollCode := SB_THUMBTRACK
  else if V <= L
  then Msg.ScrollCode := SB_TOP
  else if V >= U
  then Msg.ScrollCode := SB_BOTTOM
  else if V - OldValue = StepI
  then Msg.ScrollCode := SB_LINEDOWN
  else if OldValue - V = StepI
  then Msg.ScrollCode := SB_LINEUP
  else if V - OldValue = PageI
  then Msg.ScrollCode := SB_PAGEDOWN
  else if OldValue - V = PageI
  then Msg.ScrollCode := SB_PAGEUP
  else Msg.ScrollCode := SB_THUMBPOSITION;
  Msg.Pos := V;
  if V < High(Msg.SmallPos)
  then Msg.SmallPos := V
  else Msg.SmallPos := High(Msg.SmallPos);
  Msg.ScrollBar := HWND({%H-}PtrUInt(ScrollingData^.HScroll));

  Result := (DeliverMessage(AInfo^.LCLObject, Msg) <> 0) xor CallBackDefaultReturn;
end;

class function TGtk2WSBaseScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  ScrollingData: PBaseScrollingWinControlData;
  Allocation: TGTKAllocation;
begin
  Widget := gtk_scrolled_window_new(nil, nil);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget,dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle({%H-}PtrUInt(Widget));
  if Result = 0 then Exit;

  gtk_widget_show(Widget);

  WidgetInfo := CreateWidgetInfo(Widget, AWinControl, AParams);
  New(ScrollingData);
  ScrollingData^.HValue := 0;
  ScrollingData^.VValue := 0;
  ScrollingData^.HScroll := PGtkScrolledWindow(Widget)^.HScrollbar;
  ScrollingData^.VScroll := PGtkScrolledWindow(Widget)^.VScrollbar;
  WidgetInfo^.UserData := ScrollingData;
  WidgetInfo^.DataOwner := True;

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);
  Set_RC_Name(AWinControl, Widget);

  // SetCallbacks isn't called here, it should be done in the 'derived' class
end;

class procedure TGtk2WSBaseScrollingWinControl.SetCallbacks(
  const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget),
                                TComponent(AWidgetInfo^.LCLObject));
  SignalConnect(
    PGtkWidget(GetWidgetHAdjustment(AWidget)),
    'value-changed',
    @Gtk2WSBaseScrollingWinControl_HValueChanged,
    AWidgetInfo
  );
  SignalConnect(
    PGtkWidget(GetWidgetVAdjustment(AWidget)),
    'value-changed',
    @Gtk2WSBaseScrollingWinControl_VValueChanged,
    AWidgetInfo
  );
end;


end.
