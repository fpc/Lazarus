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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Gtk2, Glib2, gdk2,
  {$ELSE}
  Gtk, gdk, Glib, X, Xlib,
  {$ENDIF}
  SysUtils, Classes, LCLProc, LCLType, Controls, LMessages, InterfaceBase,
  Graphics, Dialogs,Forms, Math,
  WSDialogs, WSLCLClasses, WSControls, WSForms, WSProc,
  GtkInt, GtkProc, GtkDef, GtkExtra, GtkGlobals, GtkWSControls, GtkWSPrivate;

type

  { TGtkWSScrollingWinControl }

  TGtkWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
    class procedure  SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtkWSScrollBox }

  TGtkWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TGtkWSCustomFrame }

  TGtkWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TGtkWSFrame }

  TGtkWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TGtkWSCustomForm }

  TGtkWSCustomForm = class(TWSCustomForm)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const AForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
  end;

  { TGtkWSForm }

  TGtkWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TGtkWSHintWindow }

  TGtkWSHintWindow = class(TWSHintWindow)
  private
  protected
    class procedure SetCallbacks(const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TGtkWSScreen }

  TGtkWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TGtkWSApplicationProperties }

  TGtkWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

class procedure TGtkWSScrollingWinControl.SetCallbacks(
  const AWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AWidget), TComponent(AWidgetInfo^.LCLObject));
  with TGTKWidgetSet(Widgetset) do
  begin
    SetCallback(LM_HSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
    SetCallback(LM_VSCROLL, PGtkObject(AWidget), AWidgetInfo^.LCLObject);
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
  Frame := PGtkFrame(gtk_frame_new(nil));
  gtk_frame_set_shadow_type(Frame, BorderStyleShadowMap[TScrollingWinControl(AWinControl).BorderStyle]);
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
  {$note implement me}
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
function GtkWSFormMapEvent(Widget: PGtkWidget; Event: PGdkEvent; WidgetInfo: PWidgetInfo): gboolean; cdecl;
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
  {$IFDEF GTK1}
    gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'map-event', TGtkSignalFunc(@GtkWSFormMapEvent), AWidgetInfo);
    gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'unmap-event', TGtkSignalFunc(@GtkWSFormUnMapEvent), AWidgetInfo);
  {$ENDIF}
  {$IFDEF Gtk2}
    g_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget), 'window-state-event',
                     gtk_signal_func(@GTKWindowStateEventCB),
                     AWidgetInfo^.LCLObject);
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
    
  WindowType := FormStyleMap[ABorderStyle];
  if (ABorderStyle=bsNone) and (ACustomForm.FormStyle in fsAllStayOnTop)
  and (not (csDesigning in ACustomForm.ComponentState)) then begin
    WindowType:=GTK_WINDOW_POPUP;
  end;

  if ACustomForm.Parent = nil then
  begin
    // create a floating form
    P := gtk_window_new(WindowType);

    // gtk_window_set_policy is deprecated in Gtk2
    {$IFDEF Gtk2}
      gtk_window_set_resizable(GTK_WINDOW(P), gboolean(FormResizableMap[ABorderStyle]));
    {$ELSE}
      gtk_window_set_policy(GTK_WINDOW(P), FormResizableMap[ABorderStyle],
        FormResizableMap[ABorderStyle], 0);
    {$ENDIF}

    gtk_window_set_title(PGtkWindow(P), AParams.Caption);

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

  RecreateWnd(AForm);
end;

class procedure TGtkWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
var
  GdiObject: PGdiObject;
  Window: PGdkWindow;
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon')
  then Exit;

  if AForm.Parent <> nil then Exit;
  if AIcon = 0 then Exit;

  Window := GetControlWindow(PGtkWidget(AForm.Handle));
  if Window = nil then Exit;

  GdiObject := PGdiObject(AIcon);
  gdk_window_set_icon(Window, nil, GdiObject^.GDIPixmapObject.Image, GdiObject^.GDIPixmapObject.Mask);
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollingWinControl, TGtkWSScrollingWinControl, TGtkPrivateScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TGtkWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TGtkWSCustomFrame);
//  RegisterWSComponent(TFrame, TGtkWSFrame);
  RegisterWSComponent(TCustomForm, TGtkWSCustomForm);
//  RegisterWSComponent(TForm, TGtkWSForm);
  RegisterWSComponent(THintWindow, TGtkWSHintWindow);
//  RegisterWSComponent(TScreen, TGtkWSScreen);
//  RegisterWSComponent(TApplicationProperties, TGtkWSApplicationProperties);
////////////////////////////////////////////////////
end.
