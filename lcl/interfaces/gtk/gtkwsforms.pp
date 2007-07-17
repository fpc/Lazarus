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
  gtkInt, gtkProc, gtkWSControls, gtkDef, gtkExtra, gtkGlobals, GtkWSPrivate;

type

  { TGtkWSScrollingWinControl }

  TGtkWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
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
    class procedure  SetCallbacks(const AWinControl: TWinControl; const AWidgetInfo: PWidgetInfo); virtual;
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;
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

{ TGtkWSCustomForm }

class procedure TGtkWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl;
  const DeltaX, DeltaY: integer);
begin
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

class procedure TGtkWSCustomForm.SetCallbacks(const AWinControl: TWinControl;
  const AWidgetInfo: PWidgetInfo);
begin
  {$IFDEF GTK1}
    gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'map-event', TGtkSignalFunc(@GtkWSFormMapEvent), AWidgetInfo);
    gtk_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget),'unmap-event', TGtkSignalFunc(@GtkWSFormUnMapEvent), AWidgetInfo);
  {$ENDIF}
  {$IFDEF Gtk2}
    g_signal_connect(PGtkObject(AWidgetInfo^.CoreWidget), 'window-state-event',
                     gtk_signal_func(@GTKWindowStateEventCB),
                     AWinControl);
  {$ENDIF}
end;

class function TGtkWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  AWidgetInfo: PWidgetInfo;
  p          : pointer;          // ptr to the newly created GtkWidget
  Box: Pointer;
  ABorderStyle: TFormBorderStyle;
  PCaption: PChar;
  WindowType: TGtkWindowType;
  ACustomForm: TCustomForm;
begin
//  p := GtkWidgetSet.CreateForm(TCustomForm(AWinControl));

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

    PCaption := PChar(ACustomForm.Caption);
    if (PCaption = nil) then PCaption := #0;
    gtk_window_set_title(pGtkWindow(P), PCaption);

    // the clipboard needs a widget
    if (ClipboardWidget = nil) then
      GtkWidgetSet.SetClipboardWidget(P);

    //drag icons
    if Drag_Icon = nil then begin
      {$IFDEF DebugGDK}BeginGDKErrorTrap;{$ENDIF}
      Drag_Icon := gdk_pixmap_colormap_create_from_xpm_d (nil,
                           gtk_widget_get_colormap (P), Drag_Mask,
                           nil, @IMGDrag_Icon[0]);
      {$IFDEF DebugGDK}EndGDKErrorTrap;{$ENDIF}
    end;
  end
  else
  begin
    // create a form as child control
    P := gtk_hbox_new(false, 0);
  end;

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
    gtk_box_pack_start(Box, PGtkWidget(ACustomForm.Menu.Handle),False,False,0);
  end;

  // End of the old CreateForm method

  GtkWidgetSet.FinishComponentCreate(AWinControl, P);

  AWidgetInfo := GetWidgetInfo(P);
  if not (csDesigning in AWinControl.ComponentState) then
    AWidgetInfo^.UserData := Pointer(1);
    
  // enable widget as file drag destination
  gtk_drag_dest_set(AWidgetInfo^.CoreWidget, GTK_DEST_DEFAULT_ALL,
    @FileDragTarget, 1, GDK_ACTION_COPY);
    
  SetCallbacks(AWinControl, AWidgetInfo);

  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P,dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(P);
end;

class procedure TGtkWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  inherited SetFormBorderStyle(AForm, AFormBorderStyle);
  // the form border style can only be set at creation time.
  // This is Delphi compatible, so no Recreatewnd needed.
end;

class procedure TGtkWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
var
  FormIconGdiObject: PGdiObject;
  AWindow     : PGdkWindow;
begin
  if AForm.Parent = nil then
  begin
    if AForm.HandleAllocated and (AIcon <> 0) then begin
      FormIconGdiObject:=PGdiObject(AIcon);
      //DebugLn('LM_SETFORMICON ',FormIconGdiObject<>nil,' ',pgtkWidget(Handle)^.Window<>nil);
      if (FormIconGdiObject <> nil) then begin
        AWindow:=GetControlWindow(PGtkWidget(AForm.Handle));
        if AWindow<>nil then begin
          gdk_window_set_icon(AWindow, nil,
            FormIconGdiObject^.GDIBitmapObject,
            FormIconGdiObject^.GDIBitmapMaskObject);
        end;
      end;
    end;
  end;
end;

class procedure TGtkWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  SetFormShowInTaskbar(AForm,AValue);
end;

class procedure TGtkWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  GtkWindow: PGtkWindow;
begin
  ReleaseMouseCapture;
  if ACustomForm.Parent=nil then begin
    GtkWindow:=PGtkWindow(ACustomForm.Handle);
    gtk_window_set_default_size(GtkWindow,
                          Max(1,ACustomForm.Width),Max(1,ACustomForm.Height));
    gtk_widget_set_uposition(PGtkWidget(GtkWindow),
                             ACustomForm.Left, ACustomForm.Top);
  end;
  GtkWindowShowModal(GtkWindow);
end;

class procedure TGtkWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
  
  procedure RaiseNotImplemented;
  begin
    raise Exception.Create('TGtkWSCustomForm.SetBorderIcons BorderIcons not supported by gtk interface');
  end;
  
begin
  if (AForm.ComponentState*[csDesigning,csLoading]=[csDesigning]) then begin
    if (AForm.BorderIcons<>DefaultBorderIcons[AForm.BorderStyle]) then
      RaiseNotImplemented;
  end;
  inherited SetBorderIcons(AForm, ABorderIcons);
end;

{ TGtkWSHintWindow }

class function TGtkWSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  TempWidget : PGTKWidget;       // pointer to gtk-widget (local use when neccessary)
  p          : pointer;          // ptr to the newly created GtkWidget
  ACustomForm: TCustomForm;
  AWindow: PGdkWindow;
begin
  ACustomForm := TCustomForm(AWinControl);

  p := gtk_window_new(gtk_window_popup);
  gtk_window_set_policy (GTK_WINDOW (p), 0, 0, 0);

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

  GtkWidgetSet.FinishComponentCreate(AWinControl, P);
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkCreated(P,dbgsName(AWinControl));
  {$ENDIF}
  Result := TLCLIntfHandle(P);
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
