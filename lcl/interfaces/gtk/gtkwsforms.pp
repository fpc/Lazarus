{ $Id$}
{
 *****************************************************************************
 *                               GtkWSForms.pp                               * 
 *                               -------------                               * 
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
unit GtkWSForms;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF GTK2}
  Gtk2, Glib2, Gdk2, Gdk2Pixbuf,
  {$ELSE}
  Gtk, Glib, Gdk, GdkPixbuf, X, Xlib,
  {$ENDIF}
  SysUtils, Classes, LCLProc, LCLType, Controls, LMessages, InterfaceBase,
  Graphics, Dialogs,Forms, Math,
  WSDialogs, WSLCLClasses, WSControls, WSForms, WSProc,
  GtkInt, GtkProc, GtkDef, GtkExtra, GtkGlobals, GtkWSControls, GtkWSPrivate;

type

  { TGtkWSScrollingWinControl }

  TGtkWSScrollingWinControl = class(TWSScrollingWinControl)
  protected
    class procedure  SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtkWSScrollBox }

  TGtkWSScrollBox = class(TWSScrollBox)
  published
  end;

  { TGtkWSCustomFrame }

  TGtkWSCustomFrame = class(TWSCustomFrame)
  published
  end;

  { TGtkWSFrame }

  TGtkWSFrame = class(TWSFrame)
  published
  end;

  { TGtkWSCustomForm }

  TGtkWSCustomForm = class(TWSCustomForm)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetDefaultColor(const AControl: TControl; const ADefaultColorType: TDefaultColorType): TColor; override;

    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const AForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetPopupParent(const ACustomForm: TCustomForm;
       const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;
  end;

  { TGtkWSForm }

  TGtkWSForm = class(TWSForm)
  published
  end;

  { TGtkWSHintWindow }

  TGtkWSHintWindow = class(TWSHintWindow)
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtkWSScreen }

  TGtkWSScreen = class(TWSScreen)
  published
  end;

  { TGtkWSApplicationProperties }

  TGtkWSApplicationProperties = class(TWSApplicationProperties)
  published
  end;


implementation

class procedure TGtkWSScrollingWinControl.SetCallbacks(
  const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  UseScrollCallback: Boolean;
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  with TGTKWidgetSet(Widgetset) do
  begin
    {$ifdef gtk1}
    UseScrollCallBack := True;
    {$else}
    UseScrollCallBack := (gtk_major_version = 2) and (gtk_minor_version <= 8);
    {$endif}
    if UseScrollCallBack then
    begin
      SetCallback(LM_HSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_VSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
    end;
  end;
end;

class function TGtkWSScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams
  ): TLCLIntfHandle;
var
  Scrolled: PGtkScrolledWindow;
  Frame: PGtkFrame;
  Layout: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Adjustment: PGtkAdjustment;
begin
  // create a gtk_frame for the outer border
  Frame := PGtkFrame(gtk_frame_new(nil));
  gtk_frame_set_shadow_type(Frame, BorderStyleShadowMap[TScrollingWinControl(AWinControl).BorderStyle]);
  // create a gtk_scrolled_window for the scrollbars
  Scrolled := PGtkScrolledWindow(gtk_scrolled_window_new(nil, nil));
  gtk_container_add(PGTKContainer(Frame), PGtkWidget(Scrolled));
  gtk_widget_show(PGtkWidget(Scrolled));

  GTK_WIDGET_UNSET_FLAGS(Scrolled^.hscrollbar, GTK_CAN_FOCUS);
  GTK_WIDGET_UNSET_FLAGS(Scrolled^.vscrollbar, GTK_CAN_FOCUS);
  gtk_scrolled_window_set_policy(Scrolled, GTK_POLICY_NEVER, GTK_POLICY_NEVER);
  gtk_object_set_data(PGtkObject(Frame), odnScrollArea, Scrolled);
  
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(Frame, dbgsName(AWinControl));
  {$ENDIF}

  WidgetInfo := CreateWidgetInfo(Frame, AWinControl, AParams);

  Adjustment := gtk_scrolled_window_get_vadjustment(Scrolled);
  if Adjustment <> nil then
    gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar, Scrolled^.vscrollbar);

  Adjustment := gtk_scrolled_window_get_hadjustment(Scrolled);
  if Adjustment <> nil then
    gtk_object_set_data(PGTKObject(Adjustment), odnScrollBar, Scrolled^.hscrollbar);

  // create a gtk_layout for the client area, so children can be added at
  // free x,y positions and the scrollbars automatically scrolls the children
  Layout := gtk_layout_new(nil, nil);
  gtk_container_add(PGTKContainer(Scrolled), Layout);
  gtk_widget_show(Layout);
  SetFixedWidget(Frame, Layout);
  SetMainWidget(Frame, Layout);
  
  Result := TLCLIntfHandle(PtrUInt(Frame));
  Set_RC_Name(AWinControl, PGtkWidget(Frame));
  SetCallBacks(PGtkWidget(Frame), WidgetInfo);
end;

class procedure TGtkWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl;
  const DeltaX, DeltaY: integer);
begin
  {$IFDEF VerboseGtkToDos}{$note implement me}{$ENDIF}
end;

class procedure TGtkWSScrollingWinControl.SetColor(
  const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor')
  then Exit;

  GtkWidgetSet.SetWidgetColor(PGtkBin(PGtkBin(AWinControl.Handle)^.child)^.child,
                              clNone, AWinControl.Color,
                              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,
                               GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
end;

{ TGtkWSCustomForm }

{$IFDEF GTK1}

function GtkFormEvent(widget: PGtkWidget; event: PGdkEvent; data: GPointer): gboolean; cdecl;
var
  ACtl: TWinControl;
  XDisplay: PDisplay;
  Window: TWindow;
  RevertStatus: Integer;

begin
  Result := False;
  case event^.thetype of
    GDK_FOCUS_CHANGE:
      begin
        ACtl := TWinControl(Data);
        if PGdkEventFocus(event)^.thein = 0 then
        begin

          XDisplay := gdk_display;
          XGetInputFocus(XDisplay, @Window, @RevertStatus);
          // Window - 1 is our frame  !

          if (RevertStatus = RevertToParent) and
            (GDK_WINDOW_XWINDOW(PGdkWindowPrivate(Widget^.Window)) = Window - 1) then
            exit(True);
          with GtkWidgetSet do
          begin
            if ACtl.HandleAllocated then
              LastFocusOut := PGtkWidget(ACtl.Handle)
            else
              LastFocusOut := Widget;
            if LastFocusOut = LastFocusIn then
              StartFocusTimer;
          end;
        end else
        begin
          with GtkWidgetSet do
          begin
            if ACtl.HandleAllocated then
              LastFocusIn := PGtkWidget(ACtl.Handle)
            else
              LastFocusIn := Widget;
            if not AppActive then
              AppActive := True;
          end;
        end;
      end;
  end;
end;

function GtkWSFormMapEvent(Widget: PGtkWidget; Event: PGdkEvent;
  WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Message: TLMSize;
  AForm: TCustomForm;
begin
  Result := True;
  FillChar(Message, 0, SizeOf(Message));
  AForm := TCustomForm(WidgetInfo^.LCLObject);
  Message.Width := AForm.Width;
  Message.Height := AForm.Height;
  if WidgetInfo^.UserData <> nil then begin
    if AForm.WindowState = wsMaximized then
      WidgetSet.ShowWindow(AForm.Handle, SW_MAXIMIZE)
    else if AForm.WindowState = wsMinimized then
      WidgetSet.ShowWindow(AForm.Handle, SW_MINIMIZE);
    WidgetInfo^.UserData := nil;
  end;

  Message.Msg := LM_SIZE;
  if GDK_WINDOW_GET_MAXIMIZED(PGdkWindowPrivate(Widget^.window)) = True then
  begin
    Message.SizeType := SIZEFULLSCREEN or Size_SourceIsInterface;
  end
  else
  begin
    Message.SizeType := SIZENORMAL or Size_SourceIsInterface;
  end;
  
  DeliverMessage(WidgetInfo^.LCLObject, Message);
end;

function GtkWSFormUnMapEvent(Widget: PGtkWidget; Event: PGdkEvent; WidgetInfo: PWidgetInfo): gboolean; cdecl;
var
  Message: TLMSize;
  AForm: TCustomForm;
begin
  Result := True;
  FillChar(Message, 0, SizeOf(Message));
  AForm := TCustomForm(WidgetInfo^.LCLObject);
  
  // ignore the unmap signals when we switch desktops
  // as this results in irritating behavior when we return to the desktop
  if GDK_GET_CURRENT_DESKTOP <> GDK_WINDOW_GET_DESKTOP(PGdkWindowPrivate(Widget^.Window)) then Exit;

  Message.Msg := LM_SIZE;
  Message.SizeType := SIZEICONIC or Size_SourceIsInterface;
  Message.Width := AForm.Width;
  Message.Height := AForm.Height;

  DeliverMessage(WidgetInfo^.LCLObject, Message);
end;
{$ENDIF}

class procedure TGtkWSCustomForm.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
{$IFDEF Gtk1}
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  if (TControl(AWidgetInfo^.LCLObject).Parent = nil) then
    with TGTKWidgetSet(Widgetset) do
    begin
      SetCallback(LM_CONFIGUREEVENT, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_CLOSEQUERY, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallBack(LM_Activate, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_HSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
      SetCallback(LM_VSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
    end;
  gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'event', TGtkSignalFunc(@GtkFormEvent), AWidgetInfo);
  gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'map-event', TGtkSignalFunc(@GtkWSFormMapEvent), AWidgetInfo);
  gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'unmap-event', TGtkSignalFunc(@GtkWSFormUnMapEvent), AWidgetInfo);
{$ENDIF}
end;

class function TGtkWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  WidgetInfo: PWidgetInfo;
  p: pointer;          // ptr to the newly created GtkWidget
  Box: Pointer;
  ABorderStyle: TFormBorderStyle;
  WindowType: TGtkWindowType;
  ACustomForm: TCustomForm;
  AResizable: gint;
  PopupParent: TCustomForm;
begin
  // Start of old CreateForm method

  ACustomForm := TCustomForm(AWinControl);
  
  if ACustomForm.Parent = nil then
  begin
    if csDesigning in ACustomForm.ComponentState then
      ABorderStyle:=bsSizeable
    else
      ABorderStyle:=ACustomForm.BorderStyle;
  end
  else
    ABorderStyle:=bsNone;

  case ACustomForm.PopupMode of
    pmNone:
      PopupParent := nil;
    pmAuto:
      PopupParent := Screen.ActiveForm;
    pmExplicit:
      PopupParent := ACustomForm.PopupParent;
  end;

  // Maps the border style
  WindowType := FormStyleMap[ABorderStyle];
  if (ABorderStyle=bsNone) and (ACustomForm.FormStyle in fsAllStayOnTop) then
    WindowType := GTK_WINDOW_POPUP;
  if (csDesigning in ACustomForm.ComponentState) then
    WindowType := GTK_WINDOW_TOPLEVEL;

  if ACustomForm.Parent = nil then
  begin
    // create a floating form
    P := gtk_window_new(WindowType);

    // Sets the window as resizable or not
    // Depends on the WM supporting this
    if (csDesigning in ACustomForm.ComponentState) then AResizable := 1
    else AResizable := FormResizableMap[ABorderStyle];

    // gtk_window_set_policy is deprecated in Gtk2
    {$IFDEF Gtk2}
      gtk_window_set_resizable(GTK_WINDOW(P), gboolean(AResizable));
    {$ELSE}
      gtk_window_set_policy(GTK_WINDOW(P), AResizable, AResizable, 0);
    {$ENDIF}

    // Sets the title
    gtk_window_set_title(PGtkWindow(P), AParams.Caption);

    if PopupParent <> nil then
      gtk_window_set_transient_for(PGtkWindow(P), PGtkWindow(PopupParent.Handle));

    // the clipboard needs a widget
    if (ClipboardWidget = nil) then
      GtkWidgetSet.SetClipboardWidget(P);
  end
  else
  begin
    // create a form as child control
    P := gtk_hbox_new(false, 0);
  end;
  
  WidgetInfo := CreateWidgetInfo(P, AWinControl, AParams);

  Box := CreateFormContents(ACustomForm, P);
  gtk_container_add(PGtkContainer(P), Box);

  {$IfDef GTK2}
    //so we can double buffer ourselves, eg, the Form Designer
    gtk_widget_set_double_buffered(Box, False);
  {$EndIf}
  
  gtk_widget_show(Box);

  // main menu
  if (ACustomForm.Menu <> nil) and (ACustomForm.Menu.HandleAllocated) then
  begin
    gtk_box_pack_start(Box, PGtkWidget(ACustomForm.Menu.Handle), False, False,0);
  end;

  // End of the old CreateForm method

  {$IFNDEF NoStyle}
  if (ACustomForm.Parent = nil) then
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

class function TGtkWSCustomForm.GetDefaultColor(const AControl: TControl;
  const ADefaultColorType: TDefaultColorType): TColor;
begin
  if ADefaultColorType = dctFont then
    Result := clWindowText
  else
    Result := clForm;
end;

class procedure TGtkWSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
  if AValue then
    gtk_drag_dest_set(PGtkWidget(AForm.Handle), GTK_DEST_DEFAULT_ALL,
      @FileDragTarget, 1, GDK_ACTION_COPY or GDK_ACTION_MOVE)
  else
    gtk_drag_dest_unset(PGtkWidget(AForm.Handle));
end;

class procedure TGtkWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  if not WSCheckHandleAllocated(AForm, 'SetFormBorderStyle')
  then Exit;

  // Avoids blinking the window under design unnecessarely
  if not (csDesigning in AForm.ComponentState) then
    RecreateWnd(AForm);
end;

class procedure TGtkWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
var
  APixbuf: PGdkPixbuf;
  Window: PGdkWindow;
  Image: PGdkPixmap;
  Mask: PGdkBitmap;
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon')
  then Exit;

  if AForm.Parent <> nil then Exit;

  Window := GetControlWindow(PGtkWidget(AForm.Handle));
  if Window = nil then Exit;

  APixbuf := PGdkPixbuf(Big);
  Image := nil;
  Mask := nil;
  if APixbuf <> nil then
    gdk_pixbuf_render_pixmap_and_mask(APixbuf, Image, Mask, $80);

  gdk_window_set_icon(Window, nil, Image, Mask);
end;

class procedure TGtkWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  if not WSCheckHandleAllocated(AForm, 'SetShowInTaskbar')
  then Exit;

  SetFormShowInTaskbar(AForm,AValue);
end;

class procedure TGtkWSCustomForm.ShowModal(const AForm: TCustomForm);
var
  GtkWindow: PGtkWindow;
begin
  if not WSCheckHandleAllocated(AForm, 'ShowModal')
  then Exit;

  if AForm.Parent <> nil then Exit;
  ReleaseMouseCapture;

  GtkWindow := PGtkWindow(AForm.Handle);
  gtk_window_set_default_size(GtkWindow, Max(1,AForm.Width), Max(1,AForm.Height));
  gtk_widget_set_uposition(PGtkWidget(GtkWindow), AForm.Left, AForm.Top);
  GtkWindowShowModal(GtkWindow);
end;

class procedure TGtkWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  if not WSCheckHandleAllocated(AForm, 'SetBorderIcons')
  then Exit;
  
  inherited SetBorderIcons(AForm, ABorderIcons);
end;

class procedure TGtkWSCustomForm.SetColor(const AWinControl: TWinControl);
begin
  TGtkWSWinControl.SetColor(AWinControl);
end;

class procedure TGtkWSCustomForm.SetPopupParent(const ACustomForm: TCustomForm;
  const APopupMode: TPopupMode; const APopupParent: TCustomForm);
var
  PopupParent: TCustomForm;
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetPopupParent') then Exit;

  case APopupMode of
    pmNone:
      PopupParent := nil;
    pmAuto:
      PopupParent := Screen.ActiveForm;
    pmExplicit:
      PopupParent := APopupParent;
  end;
  if PopupParent <> nil then
    gtk_window_set_transient_for(PGtkWindow(ACustomForm.Handle), PGtkWindow(PopupParent.Handle))
  else
    gtk_window_set_transient_for(PGtkWindow(ACustomForm.Handle), nil);
end;

{ TGtkWSHintWindow }

class procedure TGtkWSHintWindow.SetCallbacks(const AWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  if (TControl(AWidgetInfo^.LCLObject).Parent = nil) then
    with TGTKWidgetSet(Widgetset) do
    begin
      SetCallback(LM_CONFIGUREEVENT, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
    end;
end;

class function TGtkWSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  TempWidget : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  ACustomForm: TCustomForm;
  AWindow: PGdkWindow;
  WidgetInfo: PWidgetInfo;
begin
  ACustomForm := TCustomForm(AWinControl);

  p := gtk_window_new(gtk_window_popup);
  WidgetInfo := CreateWidgetInfo(p, AWinControl, AParams);
  gtk_window_set_policy(GTK_WINDOW(p), 0, 0, 0);

  // Create the form client area
  TempWidget := CreateFixedClientWidget;
  gtk_container_add(p, TempWidget);
  gtk_widget_show(TempWidget);
  SetFixedWidget(p, TempWidget);
  SetMainWidget(p, TempWidget);

  ACustomForm.FormStyle := fsStayOnTop;
  ACustomForm.BorderStyle := bsNone;
  gtk_widget_realize(p);
  AWindow := GetControlWindow(P);
  {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}

  gdk_window_set_decorations(AWindow, GetWindowDecorations(ACustomForm));
    
  gdk_window_set_functions(AWindow, GetWindowFunction(ACustomForm));

  {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
  gtk_widget_show_all(TempWidget);// Important: do not show the window yet, only make its content visible

  {$IFNDEF NoStyle}
  if (ACustomForm.Parent = nil) then
    gtk_widget_set_app_paintable(P, True);
  {$ENDIF}

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P,dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(PtrUInt(P));
  Set_RC_Name(AWinControl, P);
  SetCallbacks(P, WidgetInfo);
end;

end.
