{ $Id$}
{
 *****************************************************************************
 *                              Gtk2WSForms.pp                               * 
 *                              --------------                               * 
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
unit Gtk2WSForms;

{$mode objfpc}{$H+}
{$I gtkdefines.inc}
interface

uses
  // Bindings
  Gtk2, Glib2, Gdk2, Gdk2Pixbuf,
  {$IFDEF HASX}
  Gdk2x, X, XLib, ctypes,
  {$ENDIF}
  // RTL, FCL, LCL
  SysUtils, Classes, LCLProc, LCLType, Controls, LMessages, InterfaceBase,
  Graphics, Dialogs,Forms, Math,
  // Widgetset
  WSDialogs, WSLCLClasses, WSControls, WSForms, WSProc,
  Gtk2Int, GtkProc, gtk2proc, GtkDef, GtkExtra, GtkGlobals, Gtk2WSControls,
  // Gtk 1 stuff
  gtkwsforms;

type

  { TGtk2WSScrollingWinControl }

  TGtk2WSScrollingWinControl = class(TGtkWSScrollingWinControl)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
  end;

  { TGtk2WSScrollBox }

  TGtk2WSScrollBox = class(TGtkWSScrollBox)
  published
  end;

  { TGtk2WSCustomFrame }

  TGtk2WSCustomFrame = class(TGtkWSCustomFrame)
  published
  end;

  { TGtk2WSFrame }

  TGtk2WSFrame = class(TGtkWSFrame)
  published
  end;

  { TGtk2WSCustomForm }

  TGtk2WSCustomForm = class(TGtkWSCustomForm)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); override;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm;
       const AlphaBlend: Boolean; const Alpha: Byte); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle,
                       AOldFormStyle: TFormStyle); override;
{    class function GetDefaultClientRect(const AWinControl: TWinControl;
             const aLeft, aTop, aWidth, aHeight: integer; var aClientRect: TRect
             ): boolean; override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;}
  end;

  { TGtk2WSForm }

  TGtk2WSForm = class(TGtkWSForm)
  published
  end;

  { TGtk2WSHintWindow }

  TGtk2WSHintWindow = class(TGtkWSHintWindow)
  published
  end;

  { TGtk2WSScreen }

  TGtk2WSScreen = class(TGtkWSScreen)
  published
  end;

  { TGtk2WSApplicationProperties }

  TGtk2WSApplicationProperties = class(TGtkWSApplicationProperties)
  published
  end;

implementation

{ TGtk2WSCustomForm }

function gtk2WSDelayedWindowStateChange(Data: Pointer): gboolean; cdecl;
var
  AnForm: TCustomForm absolute data;
  AEvent: TGdkEventWindowState;
begin
  Result := False;
  AEvent := GetWidgetInfo(PGtkWidget(AnForm.Handle))^.FormWindowState;
  GTKWindowStateEventCB(PGtkWidget(AnForm.Handle), @AEvent, Data);
  // remove idle handler, because in fast switching hide/show there could
  // be dozen of added idle handlers, only one should be here.
  // also reset our internal flag on send_event.
  GetWidgetInfo(PGtkWidget(AnForm.Handle))^.FormWindowState.send_event := 0;
  g_idle_remove_by_data(Data);
end;

function Gtk2FormEvent(widget: PGtkWidget; event: PGdkEvent; data: GPointer): gboolean; cdecl;
var
  ACtl: TWinControl;
  Mess : TLMessage;
  WInfo: PWidgetInfo;
  {$IFDEF HASX}
  XDisplay: PDisplay;
  Window: TWindow;
  RevertStatus: Integer;
  {$ENDIF}

begin
  Result := CallBackDefaultReturn;
  case event^._type of
    GDK_WINDOW_STATE:
      begin
        if (GDK_WINDOW_STATE_WITHDRAWN and event^.window_state.changed_mask) = 1 then
          exit;
        WInfo := GetWidgetInfo(Widget);
        if (WInfo <> nil) then
        begin
          if (WInfo^.FormWindowState.new_window_state <> event^.window_state.new_window_state)
           and (WInfo^.FormWindowState.send_event <> 2) then
          begin
            WInfo^.FormWindowState := Event^.window_state;
            // needed to lock recursions, normally send_event can be 0 or 1
            // we add 2 to know if recursion occured.
            WInfo^.FormWindowState.send_event := 2;
            g_idle_add(@gtk2WSDelayedWindowStateChange, Data);
          end else
          begin
            // our send_event flag is 2, mean recursion occured
            // so we have to normalize things first.
            while WInfo^.FormWindowState.send_event = 2 do
            begin
             Application.Idle(True);
             Application.ProcessMessages;
            end;
            WInfo^.FormWindowState.send_event := 0;
            Result := GTKWindowStateEventCB(Widget, @event^.window_state, Data);
          end;
        end;
      end;
    GDK_ENTER_NOTIFY:
      begin
        FillChar(Mess, SizeOf(Mess), #0);
        Mess.msg := CM_MOUSEENTER;
        DeliverMessage(Data, Mess);
      end;
    GDK_LEAVE_NOTIFY:
      begin
        FillChar(Mess, SizeOf(Mess), #0);
        Mess.msg := CM_MOUSELEAVE;
        DeliverMessage(Data, Mess);
      end;
    GDK_FOCUS_CHANGE:
      begin
        ACtl := TWinControl(Data);
        if PGdkEventFocus(event)^._in = 0 then
        begin
          {$IFDEF HASX}
          XDisplay := gdk_display;
          XGetInputFocus(XDisplay, @Window, @RevertStatus);
          // Window - 1 is our frame  !
          if (RevertStatus = RevertToParent) and
            (GDK_WINDOW_XID(Widget^.Window) = Window - 1) then
            exit(True);
          {$ENDIF}
          with Gtk2WidgetSet do
          begin
            LastFocusOut := PGtkWidget(ACtl.Handle);
            if LastFocusOut = LastFocusIn then
              StartFocusTimer;
          end;
        end else
        begin
          with Gtk2WidgetSet do
          begin
            LastFocusIn := PGtkWidget(ACtl.Handle);
            if not AppActive then
              AppActive := True;
          end;
        end;
      end;
  end;
end;

class procedure TGtk2WSCustomForm.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  if (TWinControl(AWidgetInfo^.LCLObject).Parent = nil) and (TWinControl(AWidgetInfo^.LCLObject).ParentWindow = 0) then
    with TGTK2WidgetSet(Widgetset) do
    begin
      SetCallback(LM_CONFIGUREEVENT, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_CLOSEQUERY, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallBack(LM_ACTIVATE, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      if (gtk_major_version = 2) and (gtk_minor_version <= 8) then
      begin
        SetCallback(LM_HSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
        SetCallback(LM_VSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      end;
    end;

  g_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget), 'event',
    gtk_signal_func(@Gtk2FormEvent), AWidgetInfo^.LCLObject);
end;

class function TGtk2WSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  WidgetInfo: PWidgetInfo;
  p: pointer;          // ptr to the newly created GtkWidget
  Box: Pointer;
  ABorderStyle: TFormBorderStyle;
  WindowType: TGtkWindowType;
  ACustomForm: TCustomForm;
  AResizable: gint;
begin
  // Start of old CreateForm method

  ACustomForm := TCustomForm(AWinControl);

  if (AParams.Style and WS_CHILD) = 0 then
  begin
    if csDesigning in ACustomForm.ComponentState then
      ABorderStyle := bsSizeable
    else
      ABorderStyle := ACustomForm.BorderStyle;
  end
  else
    ABorderStyle := bsNone;

  // Maps the border style
  WindowType := FormStyleMap[ABorderStyle];
  if (csDesigning in ACustomForm.ComponentState) then
    WindowType := GTK_WINDOW_TOPLEVEL;

  if (AParams.Style and WS_CHILD) = 0 then
  begin
    // create a floating form
    P := gtk_window_new(WindowType);

    // This is done with the expectation to avoid the button blinking for forms
    //that hide it, but currently it doesn't seem to make a difference.
    gtk_window_set_skip_taskbar_hint(P, True);

    if (ABorderStyle = bsNone) and (ACustomForm.FormStyle in fsAllStayOnTop) then
      gtk_window_set_decorated(PGtkWindow(P), False);

    // Sets the window as resizable or not
    // Depends on the WM supporting this
    if (csDesigning in ACustomForm.ComponentState) then
      AResizable := 1
    else
      AResizable := FormResizableMap[ABorderStyle];

    // gtk_window_set_policy is deprecated in Gtk2
    gtk_window_set_resizable(GTK_WINDOW(P), gboolean(AResizable));

    // Sets the title
    gtk_window_set_title(PGtkWindow(P), AParams.Caption);

    if (AParams.WndParent <> 0) then
      gtk_window_set_transient_for(PGtkWindow(P), PGtkWindow(AParams.WndParent))
    else
    if not (csDesigning in ACustomForm.ComponentState) and (ACustomForm.FormStyle = fsStayOnTop) then
        gtk_window_set_keep_above(PGtkWindow(P), gboolean(True));

    // the clipboard needs a widget
    if (ClipboardWidget = nil) then
      Gtk2WidgetSet.SetClipboardWidget(P);
  end
  else
  begin
    // create a form as child control
    P := gtk_hbox_new(false, 0);
  end;

  WidgetInfo := CreateWidgetInfo(P, AWinControl, AParams);
  WidgetInfo^.FormBorderStyle := Ord(ABorderStyle);

  FillChar(WidgetInfo^.FormWindowState, SizeOf(WidgetInfo^.FormWindowState), #0);
  WidgetInfo^.FormWindowState.new_window_state := GDK_WINDOW_STATE_WITHDRAWN;

  Box := CreateFormContents(ACustomForm, P, WidgetInfo);
  gtk_container_add(PGtkContainer(P), Box);

  //so we can double buffer ourselves, eg, the Form Designer
  if csDesigning in AWinControl.ComponentState then
    gtk_widget_set_double_buffered(Box, False);

  gtk_widget_show(Box);

  // main menu
  if (ACustomForm.Menu <> nil) and (ACustomForm.Menu.HandleAllocated) then
    gtk_box_pack_start(Box, PGtkWidget(ACustomForm.Menu.Handle), False, False,0);

  // End of the old CreateForm method

  {$IFNDEF NoStyle}
  if (AParams.Style and WS_CHILD) = 0 then
    gtk_widget_set_app_paintable(P, True);
  {$ENDIF}

  if not (csDesigning in AWinControl.ComponentState) then
    WidgetInfo^.UserData := Pointer(1);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P, dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(P));
  Set_RC_Name(AWinControl, P);
  SetCallbacks(P, WidgetInfo);
end;

class procedure TGtk2WSCustomForm.SetIcon(const AForm: TCustomForm;
  const Small, Big: HICON);
var
  List: PGList;
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon')
  then Exit;

  if (AForm.Parent <> nil) or (AForm.ParentWindow <> 0) then Exit;

  List := nil;
  if Small <> 0 then
    List := g_list_append(List, PGdkPixbuf(Small));
  if Big <> 0 then
    List := g_list_append(List, PGdkPixbuf(Big));
  gtk_window_set_icon_list(PGtkWindow(AForm.Handle), List);
  if List <> nil
  then  g_list_free(List);
end;

class procedure TGtk2WSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetAlphaBlend') then
    Exit;
  if Assigned(gtk_window_set_opacity) and GTK_IS_WINDOW(PGtkWidget(ACustomForm.Handle)) then
    if AlphaBlend then
      gtk_window_set_opacity(PGtkWindow(ACustomForm.Handle), Alpha / 255)
    else
      gtk_window_set_opacity(PGtkWindow(ACustomForm.Handle), 1);
end;

class procedure TGtk2WSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  // WindowType: TGtkWindowType;
  Resizable: gint;
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormBorderStyle') then
    exit;
  if (csDesigning in AForm.ComponentState) then
    exit;

  Widget := PGtkWidget(AForm.Handle);
  WidgetInfo := GetWidgetInfo(Widget);

  if (WidgetInfo^.FormBorderStyle <> Ord(AFormBorderStyle)) then
  begin
    if (AForm.Parent<>nil) then
    begin
      // a nested form
      // at the moment the gtk interface does not support any border for this
    end else if (AFormBorderStyle <> bsNone) then
    begin
      // the borderstyle can be only set on creation
      RecreateWnd(AForm);
    end else
    begin
      // TODO: set window hint WindowType := FormStyleMap[AFormBorderStyle];
      Resizable := FormResizableMap[AFormBorderStyle];
      if (AFormBorderStyle = bsNone) then
        gtk_window_set_decorated(PGtkWindow(Widget), False);
      gtk_window_set_resizable(GTK_WINDOW(Widget), gboolean(Resizable));
      WidgetInfo^.FormBorderStyle := Ord(AFormBorderStyle);
    end;
  end;
end;

class procedure TGtk2WSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
var
  above   : gboolean;
  Widget  : PGtkWidget;
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormStyle') then
    exit;
  if (csDesigning in AForm.ComponentState) then
    exit;
  above := AFormStyle in fsAllStayOnTop;
  Widget := PGtkWidget(AForm.Handle);
  gtk_window_set_keep_above(PGtkWindow(Widget), above);
end;

{class function TGtk2WSCustomForm.GetDefaultClientRect(
  const AWinControl: TWinControl; const aLeft, aTop, aWidth, aHeight: integer;
  var aClientRect: TRect): boolean;
begin
  Result:=false;
  if AWinControl.HandleAllocated then begin

  end else begin
    FrameBorders:=GetStyleFormFrameBorders(TCustomForm(AWinControl).Menu<>nil);
    aClientRect:=Rect(0,0,
                 Max(0,aWidth-FrameBorders.Left-FrameBorders.Right),
                 Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
    Result:=true;
  end;
end;

class procedure TGtk2WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  gtk_window_get_modal();
end;}

{ TGtk2WSScrollingWinControl }

class function TGtk2WSScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Scrolled: PGtkScrolledWindow;
  Layout: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Adjustment: PGtkAdjustment;
begin
  // create a gtk_scrolled_window for the scrollbars
  Scrolled := PGtkScrolledWindow(gtk_scrolled_window_new(nil, nil));
  gtk_scrolled_window_set_shadow_type(Scrolled,
    BorderStyleShadowMap[TScrollingWinControl(AWinControl).BorderStyle]);

  GTK_WIDGET_UNSET_FLAGS(Scrolled^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(Scrolled^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(Scrolled, GTK_POLICY_NEVER, GTK_POLICY_NEVER);
  gtk_object_set_data(PGtkObject(Scrolled), odnScrollArea, Scrolled);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Scrolled, dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Scrolled, AWinControl, AParams);

  Adjustment := gtk_scrolled_window_get_vadjustment(Scrolled);
  if Adjustment <> nil then
    gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar, Scrolled^.vscrollbar);

  Adjustment := gtk_scrolled_window_get_hadjustment(Scrolled);
  if Adjustment <> nil then
    gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar, Scrolled^.hscrollbar);

  // create a gtk_layout for the client area, so childs can be added at
  // free x,y positions and the scrollbars automatically scrolls the childs

  Layout := gtk_layout_new(nil, nil);
  gtk_container_add(PGTKContainer(Scrolled), Layout);
  gtk_widget_show(Layout);
  SetFixedWidget(Scrolled, Layout);
  SetMainWidget(Scrolled, Layout);


  Result := TLCLIntfHandle(PtrUInt(Scrolled));

  Set_RC_Name(AWinControl, PGtkWidget(Scrolled));
  SetCallBacks(PGtkWidget(Scrolled), WidgetInfo);
  if (gtk_major_version >= 2) and (gtk_minor_version > 8) then
  begin
    g_signal_connect(Scrolled^.hscrollbar, 'change-value',
      TGCallback(@Gtk2RangeScrollCB), WidgetInfo);
    g_signal_connect(Scrolled^.vscrollbar, 'change-value',
      TGCallback(@Gtk2RangeScrollCB), WidgetInfo);
  end;
end;

class procedure TGtk2WSScrollingWinControl.SetColor(
  const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor')
  then Exit;

  Gtk2WidgetSet.SetWidgetColor(PGtkBin(AWinControl.Handle)^.child,
                               clNone, AWinControl.Color,
                               [GTK_STATE_NORMAL, GTK_STATE_ACTIVE,
                                GTK_STATE_PRELIGHT, GTK_STATE_SELECTED]);
end;

class procedure TGtk2WSScrollingWinControl.ScrollBy(
  const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer);
var
  GtkWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  if not AWinControl.HandleAllocated then exit;
  {gtk-2.8 have problem with gtk_window_scroll
  also for >= gtk2-2.18 GDK_NATIVE_WINDOW env variable must be setted
  or we get paint glitches.}
  if (gtk_major_version >= 2) and (gtk_minor_version > 8) then
  begin
    GtkWidget := PGtkWidget(AWinControl.Handle);
    WidgetInfo := GetWidgetInfo(GtkWidget);
    if WidgetInfo <> nil then
      gdk_window_scroll(WidgetInfo^.ClientWidget^.window, -DeltaX, -DeltaY);
  end;
end;

end.
