{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSControls.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls,
////////////////////////////////////////////////////
  Classes, SysUtils,
  Graphics,

  gdk2pixbuf,
  Gtk2, Gdk2, Glib2, Gtk2Globals, Gtk2Def,
  Gtk2Proc, LCLType, LCLProc,
  WSControls, WSProc, Gtk2WinapiWindow;
  

type

  { TGtk2WSDragImageList }

  TGtk2WSDragImageList = class(TWSDragImageList)
  published
    class function BeginDrag(const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const ADragImageList: TDragImageList; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const ADragImageList: TDragImageList); override;
    class function HideDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;
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
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;

    class procedure ShowHide(const AWinControl: TWinControl); override;

    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
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

procedure GtkWindowShowModal(GtkWindow: PGtkWindow);
function GetWidgetHAdjustment(AWidget: PGTKWidget): PGTKAdjustment;
function GetWidgetVAdjustment(AWidget: PGTKWidget): PGTKAdjustment;

implementation

uses
  Gtk2Int, LMessages, Math, Gtk2WSPrivate, Forms;

{ TGtk2WSWinControl }

function Gtk2ScrolledWindowScrollCB(AScrollWindow: PGtkScrolledWindow; AEvent: PGdkEventScroll; AWidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Msg: TLMVScroll;
  AValue: Double;
  Range: PGtkRange;
begin
  case AEvent^.direction of
    GDK_SCROLL_UP,
    GDK_SCROLL_DOWN: Msg.Msg := LM_VSCROLL;
    GDK_SCROLL_LEFT,
    GDK_SCROLL_RIGHT: Msg.Msg := LM_HSCROLL;
  end;

  case Msg.Msg of
    LM_VSCROLL: Range := GTK_RANGE(AScrollWindow^.vscrollbar);
    LM_HSCROLL: Range := GTK_RANGE(AScrollWindow^.hscrollbar);
  end;
  
  AValue :=  power(Range^.adjustment^.page_size, 2 / 3);
  
  if (AEvent^.direction = GDK_SCROLL_UP) or
     (AEvent^.direction = GDK_SCROLL_LEFT)
  then
    AValue := -AValue;
    
  AValue := gtk_range_get_value(Range) + AValue;
  
  AValue := Max(AValue, Range^.adjustment^.lower);
  AValue := Min(AValue, Range^.adjustment^.upper - Range^.adjustment^.page_size);

  with Msg do
  begin
    Pos := Round(AValue);
    if Pos < High(SmallPos) then
      SmallPos := Pos
    else
      SmallPos := High(SmallPos);

    ScrollBar := HWND(PtrUInt(Range));
    ScrollCode := SB_THUMBPOSITION;
  end;
  Result := DeliverMessage(AWidgetInfo^.LCLObject, Msg) <> 0;
end;


class function TGtk2WSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  Widget := GTK2WidgetSet.CreateAPIWidget(AWinControl);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Widget, dbgsName(AWinControl));
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  if Result = 0 then Exit;

  WidgetInfo := GetWidgetInfo(Widget); // Widget info already created in CreateAPIWidget
  WidgetInfo^.Style := AParams.Style;
  WidgetInfo^.ExStyle := AParams.ExStyle;
  WidgetInfo^.WndProc := PtrUInt(AParams.WindowClass.lpfnWndProc);

  // set allocation
  Allocation.X := AParams.X;
  Allocation.Y := AParams.Y;
  Allocation.Width := AParams.Width;
  Allocation.Height := AParams.Height;
  gtk_widget_size_allocate(Widget, @Allocation);

  Set_RC_Name(AWinControl, Widget);

  TGtk2WSWinControl.SetCallbacks(GTK_OBJECT(Widget), AWinControl);

  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.hscrollbar, 'change-value',
    TGCallback(@Gtk2RangeScrollCB), WidgetInfo);
  g_signal_connect(GTK_SCROLLED_WINDOW(Widget)^.vscrollbar, 'change-value',
    TGCallback(@Gtk2RangeScrollCB), WidgetInfo);

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
  gtk_widget_set_direction(PGtkWidget(AWinControl.Handle),
    WidgetDirection[UseRightToLeftAlign]);
  Info := GetWidgetInfo(PGtkWidget(AWinControl.Handle));
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
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetText')
  then Exit;

  Result := true;
  Handle := AWinControl.Handle;
  case AWinControl.fCompStyle of
    csComboBox:
      begin
        AText := StrPas(gtk_entry_get_text(PGtkEntry(PGtkCombo(Handle)^.entry)));
      end;

    csEdit, csSpinEdit:
       AText:= StrPas(gtk_entry_get_text(PgtkEntry(Handle)));

    csMemo:
      begin
        CS := gtk_editable_get_chars(PGtkOldEditable(
          GetWidgetInfo(Pointer(Handle), True)^.CoreWidget), 0, -1);
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
        TextBuf := gtk_text_view_get_buffer(PGtkTextView(GetWidgetInfo(Pointer(Handle), True)^.CoreWidget));
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

  Widget := PGtkWidget(AWinControl.Handle);
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
  
  Widget := PGtkWidget(AWinControl.Handle);
  if GTK_IS_SCROLLED_WINDOW(Widget) then
    gtk_scrolled_window_set_shadow_type(PGtkScrolledWindow(Widget), BorderStyleShadowMap[ABorderStyle])
  else
  if GTK_IS_ENTRY(Widget) then
    gtk_entry_set_has_frame(PGtkEntry(Widget), ABorderStyle <> bsNone)
  else
    Gtk1SetBorderStyle(AWinControl, ABorderStyle);
end;


procedure GtkWindowShowModal(GtkWindow: PGtkWindow);
begin
  if (GtkWindow=nil) then exit;
  UnsetResizeRequest(PgtkWidget(GtkWindow));

  if ModalWindows=nil then ModalWindows:=TFPList.Create;
  ModalWindows.Add(GtkWindow);
  {$IFNDEF gtk_no_set_modal}
  gtk_window_set_modal(GtkWindow, true);
  {$ENDIF}
  gtk_window_present(GtkWindow);

  {$IFDEF VerboseTransient}
  DebugLn('TGtkWidgetSet.ShowModal ',Sender.ClassName);
  {$ENDIF}
  //TGtk2WidgetSet(WidgetSet).
  GTK2WidgetSet.UpdateTransientWindows;
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

{ TGtk2WSDragImageList }

class function TGtk2WSDragImageList.BeginDrag(
  const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer
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

  GDIObject := PGDIObject(ABitmap.Handle);

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

class function TGtk2WSDragImageList.DragMove(
  const ADragImageList: TDragImageList; X, Y: Integer): Boolean;
begin
  Result := Gtk2Widgetset.DragImageList_DragMove(X, Y);
end;

class procedure TGtk2WSDragImageList.EndDrag(
  const ADragImageList: TDragImageList);
begin
  Gtk2Widgetset.DragImageList_EndDrag;
end;

class function TGtk2WSDragImageList.HideDragImage(
  const ADragImageList: TDragImageList; ALockedWindow: HWND; DoUnLock: Boolean
  ): Boolean;
begin
  Result := Gtk2Widgetset.DragImageList_SetVisible(False);
end;

class function TGtk2WSDragImageList.ShowDragImage(
  const ADragImageList: TDragImageList; ALockedWindow: HWND; X, Y: Integer;
  DoLock: Boolean): Boolean;
begin
  Result := Gtk2Widgetset.DragImageList_DragMove(X, Y) and Gtk2Widgetset.DragImageList_SetVisible(True);
end;


{ TGtk2WSWinControl }

type
  TWinControlHack = class(TWinControl)
  end;

class procedure TGtk2WSWinControl.AddControl(const AControl: TControl);
var
  AParent: TWinControl;
  ParentWidget: PGTKWidget;
  ChildWidget: PGTKWidget;
  pFixed: PGTKWidget;
begin
  {$IFDEF OldToolBar}
  if (AControl.Parent is TToolbar) then
    exit;
  {$ENDIF}

  AParent := TWinControl(AControl).Parent;
  //debugln('LM_AddChild: ',TWinControl(Sender).Name,' ',dbgs(AParent<>nil));
  if not Assigned(AParent) then
    Assert(true, Format('Trace: [TGtkWSWinControl.AddControl] %s --> Parent is not assigned', [AControl.ClassName]))
  else
  begin
    Assert(False, Format('Trace:  [TGtkWSWinControl.AddControl] %s --> Calling Add Child: %s', [AParent.ClassName, AControl.ClassName]));
    ParentWidget := Pgtkwidget(AParent.Handle);
    pFixed := GetFixedWidget(ParentWidget);
    if pFixed <> ParentWidget then
    begin
      // parent changed for child
      ChildWidget := PGtkWidget(TWinControl(AControl).Handle);
      FixedPutControl(pFixed, ChildWidget, AControl.Left, AControl.Top);
      RegroupAccelerator(ChildWidget);
    end;
  end;
end;

class function TGtk2WSWinControl.CanFocus(const AWinControl: TWinControl): Boolean;
var
  Widget, FocusWidget: PGtkWidget;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := PGtkWidget(AWinControl.Handle);
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
  Widget := PGtkWidget(AWinControl.Handle);
  if (Widget <> nil) and (GtkWidgetIsA(Widget, gtk_window_get_type)) then
  begin
    with Geometry, AWinControl do
    begin
      if Constraints.MinWidth > 0 then
        min_width := Constraints.MinWidth
      else
        min_width := 1;
      if Constraints.MaxWidth > 0 then
        max_width := Constraints.MaxWidth
      else
        max_width := 32767;
      if Constraints.MinHeight > 0 then
        min_height := Constraints.MinHeight
      else
        min_height := 1;
      if Constraints.MaxHeight > 0 then
        max_height := Constraints.MaxHeight
      else
        max_height := 32767;

      base_width := Width;
      base_height := Height;
      width_inc := 1;
      height_inc := 1;
      min_aspect := 0;
      max_aspect := 1;
    end;
    //debugln('TGtk2WSWinControl.ConstraintsChange A ',GetWidgetDebugReport(Widget),' max=',dbgs(Geometry.max_width),'x',dbgs(Geometry.max_height));
    gtk_window_set_geometry_hints(PGtkWindow(Widget), nil, @Geometry,
                                  GDK_HINT_MIN_SIZE or GDK_HINT_MAX_SIZE);
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

  Assert(false, 'Trace:Trying to invalidate window... !!!');
  gtk_widget_queue_draw(PGtkWidget(AWinControl.Handle));
end;

class procedure TGtk2WSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  // other methods use ShowHide also, can't move code
  Gtk2WidgetSet.SetVisible(AWinControl, AWinControl.HandleObjectShouldBeVisible);
  InvalidateLastWFPResult(AWinControl, AWinControl.BoundsRect);
end;

class procedure TGtk2WSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds')
  then Exit;

  ResizeHandle(AWinControl);
  InvalidateLastWFPResult(AWinControl, Rect(ALeft, ATop, AWidth, AHeight));
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

  WidgetInfo := GetWidgetInfo(Pointer(AWinControl.Handle));
  if (WidgetInfo^.ControlCursor = ACursor) and
     (WidgetInfo^.DefaultCursor <> HCursor(-1)) then Exit;
  if ACursor <> Screen.Cursors[crDefault] then
    WidgetInfo^.ControlCursor := ACursor
  else
  begin
    if WidgetInfo^.DefaultCursor = HCursor(-1) then
      TGtkPrivateWidgetClass(AWinControl.WidgetSetClass.WSPrivate).SetDefaultCursor(WidgetInfo);
    WidgetInfo^.ControlCursor := WidgetInfo^.DefaultCursor;
  end;
  TGtkPrivateWidgetClass(AWinControl.WidgetSetClass.WSPrivate).UpdateCursor(WidgetInfo);
end;

class procedure TGtk2WSWinControl.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont')
  then Exit;

  Widget := PGtkWidget(AWinControl.Handle);
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

  Widget := PGtkWidget(AWinControl.Handle);
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

  Widget := PGtkWidget(AWinControl.Handle);
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

  if ((csOpaque in AWinControl.ControlStyle) and
      GtkWidgetIsA(pGtkWidget(AWinControl.handle),GTKAPIWidget_GetType)) then
    Exit;

  //DebugLn('TGtk2WSWinControl.SetColor ',DbgSName(AWinControl));
  Gtk2WidgetSet.SetWidgetColor(PGtkWidget(AWinControl.Handle),
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
    // dig through the hierachy to get the labels
    NoteBookWidget:=PGtkWidget((AWinControl.Parent).Handle);
    PageWidget:=PGtkWidget(AWinControl.Handle);
    TabWidget:=gtk_notebook_get_tab_label(PGtkNoteBook(NotebookWidget),
                                          PageWidget);
    if TabWidget<>nil then
      TabLabelWidget:=gtk_object_get_data(PGtkObject(TabWidget), 'TabLabel')
    else
      TabLabelWidget:=nil;
    MenuWidget:=gtk_notebook_get_menu_label(PGtkNoteBook(NotebookWidget),
                                            PageWidget);
    if MenuWidget<>nil then
      MenuLabelWidget:=gtk_object_get_data(PGtkObject(MenuWidget), 'TabLabel')
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

  P := Pointer(AWinControl.Handle);
  Assert(p = nil, 'Trace:WARNING: [TGtkWidgetSet.SetLabel] --> got nil pointer');
  Assert(False, 'Trace:Setting Str1 in SetLabel');
  pLabel := pchar(AText);

  case AWinControl.fCompStyle of
    csBitBtn,
    csButton: DebugLn('[WARNING] Obsolete call to TGTKOBject.SetLabel for ', AWinControl.ClassName);

    {$IFDEF OldToolBar}
    csToolButton:
      with PgtkButton(P)^ do
      begin
        //aLabel := StrAlloc(Length(AnsiString(PLabel)) + 1);
        aLabel := Ampersands2Underscore(PLabel);
        Try
          //StrPCopy(aLabel, AnsiString(PLabel));
          //Accel := Ampersands2Underscore(aLabel);
          if gtk_bin_get_child(P) = nil then
          begin
            Assert(False, Format('trace:  [TGtkWidgetSet.SetLabel] %s has no child label', [AWinControl.ClassName]));
             gtk_container_add(P, gtk_label_new(aLabel));
          end else
          begin
            Assert(False, Format('trace:  [TGtkWidgetSet.SetLabel] %s has child label', [AWinControl.ClassName]));
            gtk_label_set_text(pgtkLabel( gtk_bin_get_child(P)), aLabel);
          end;
          //If Accel <> -1 then
          AccelKey:=gtk_label_parse_uline(PGtkLabel( gtk_bin_get_child(P)), aLabel);
          Accelerate(AWinControl,PGtkWidget(P),AccelKey,0,'clicked');
        finally
          StrDispose(aLabel);
        end;
      end;
    {$ENDIF OldToolBar}

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
        P:= GetWidgetInfo(P, True)^.CoreWidget;
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
  Assert(False, Format('trace:  [TGtkWidgetSet.SetLabel] %s --> END', [AWinControl.ClassName]));
end;

class procedure TGtk2WSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
var
  GtkWidget, FixedWidget: PGtkWidget;
  GdkBitmap: PGDKBitmap;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetShape') then
    Exit;

  GtkWidget := PGtkWidget(AWinControl.Handle);
  FixedWidget := GetFixedWidget(GtkWidget);

  if AShape <> 0 then
  begin
    if Gtk2Widgetset.IsValidGDIObjectType(AShape, gdiBitmap) then
      GdkBitmap := PGdiObject(AShape)^.GDIBitmapObject
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
    AOffset: TPoint;
    AWindow: PGdkWindow;
  begin
    AWindow := GetControlWindow(AWidget);

    if AWindow <> nil then
      PaintWindow(AWindow);
  end;

begin
  if not WSCheckHandleAllocated(AWinControl, 'PaintTo')
  then Exit;
  PaintWidget(GetFixedWidget(PGtkWidget(AWinControl.Handle)));
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
  Msg.ScrollBar := HWND(PtrUInt(ScrollingData^.HScroll));

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
  Msg.ScrollBar := HWND(PtrUInt(ScrollingData^.HScroll));

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

  Result := THandle(PtrUInt(Widget));
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
