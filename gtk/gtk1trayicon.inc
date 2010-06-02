{%MainUnit gtkwsextctrls.pp}
{
 gtk1trayicon.pas

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Felipe Monteiro de Carvalho and Andrew Haines

 Special thanks for: Danny Milosavljevic and the Lazarus Team

 Gtk1 specific code. Works on gnome also.
}

{ TGtkWSCustomTrayIcon }

type

  { TGtk1TrayIconHandle }

  TGtk1TrayIconHandle = class(TObject)
  private
    plug: PGtkWidget;
    drawingarea: PGtkWidget;
    fDisplay: PDisplay;
    fcanvas: TCanvas;
    fWindow: TWindow;
    fScreenID: longint;
    fTrayParent: TWindow;
    //fOwner: TComponent;
    fEmbedded: Boolean;
    //fMsgCount: Integer;
    fTrayIcon: TCustomTrayIcon;
    function GetCanvas: TCanvas;
    function NotifyExpose(Event: PGdkEventExpose; Widget: PGtkWidget): Boolean; cdecl;
    function NotifyMouseMove(Event: PGdkEventMotion; Widget: PGtkWidget): Boolean; cdecl;
    function NotifyMouseDown(Event: PGdkEventButton; Widget: PGtkWidget): Boolean; cdecl;
    function NotifyMouseUp(Event: PGdkEventButton; Widget: PGtkWidget): Boolean; cdecl;
    procedure PaintForm(Sender: TObject);
    function  Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
  public
    destructor Destroy; override;
    procedure CreateForm(id: Integer);
    function  GetPosition: TPoint;
    procedure Hide;
    procedure SetEmbedded;
    procedure SetMinSize(AWidth, AHeight: Integer);
    function  TrayParent(UseCachedValue: Boolean = True): TWindow;
  public
    property Canvas: TCanvas read GetCanvas;
  end;

const
  SYSTEM_TRAY_REQUEST_DOCK   = 0;
  SYSTEM_TRAY_BEGIN_MESSAGE  = 1;
  SYSTEM_TRAY_CANCEL_MESSAGE = 2;
  
// Temp ErrorHandler
function TempX11ErrorHandler(Display:PDisplay; ErrorEv:PXErrorEvent):longint;cdecl;
begin
  WriteLn('Error: ' + IntToStr(ErrorEv^.error_code));
  Result:=0;
end;

function TGtk1TrayIconHandle.GetCanvas: TCanvas;
begin
  if Assigned(FCanvas) then Exit(FCanvas);
  
  Result := TCanvas.Create;
  Result.Handle:= GetDC(HWND(drawingarea));
  FCanvas := Result;
end;

function TGtk1TrayIconHandle.NotifyExpose(Event: PGdkEventExpose;
  Widget: PGtkWidget): Boolean; cdecl;
begin
  Result := False;

  PaintForm(fTrayIcon);
end;

function TGtk1TrayIconHandle.NotifyMouseMove(Event: PGdkEventMotion;
  Widget: PGtkWidget): Boolean; cdecl;
begin
  Result := False;

  if Assigned(fTrayIcon.OnMouseMove) then
     fTrayIcon.OnMouseMove(fTrayIcon, [], Trunc(Event^.x), Trunc(Event^.y));
end;

function TGtk1TrayIconHandle.NotifyMouseDown(Event: PGdkEventButton;
  Widget: PGtkWidget): Boolean; cdecl;
var
  Button: TMouseButton;
begin
  Result := False;

  case  Event^.button of
    GDK_RIGHTBUTTON: Button := mbRight;
    GDK_MIDDLEBUTTON: Button := mbMiddle;
    GDK_LEFTBUTTON: Button := mbLeft;
  end;
  
  if Assigned(fTrayIcon.OnMouseDown) then
    fTrayIcon.OnMouseDown(fTrayIcon, Button, [], Trunc(Event^.x), Trunc(Event^.y));
end;

function TGtk1TrayIconHandle.NotifyMouseUp(Event: PGdkEventButton;
  Widget: PGtkWidget): Boolean; cdecl;
var
  Button: TMouseButton;
begin
  Result := False;
  
  case  Event^.button of
    3: Button := mbRight;
    2: Button := mbMiddle;
    1: Button := mbLeft;
  end;

  if Button = mbLeft then
    case gdk_event_get_type(Event) of
       GDK_BUTTON_PRESS:
         if Assigned(fTrayIcon.OnClick) then
           fTrayIcon.OnClick(fTrayIcon);
       GDK_2BUTTON_PRESS:
         if Assigned(fTrayIcon.OnDblClick) then
           fTrayIcon.OnDblClick(fTrayIcon);
    end;

  { Just using GetPosition to get the screen position and then add
    Event^.x and Event^.y to it won't work. It seams that this will
    cause a small difference with Mouse.CursorPos, and using
    TPopupMenu.PopUp will result in a wrong position for the menu }
  if (Button = mbRight) and (fTrayIcon.PopUpMenu <> nil) then
    fTrayIcon.PopUpMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);

  if Assigned(fTrayIcon.OnMouseUp) then
    fTrayIcon.OnMouseUp(fTrayIcon, Button, [], Trunc(Event^.x), Trunc(Event^.y));
end;

{*******************************************************************
*  TGtk1TrayIconHandle.Send_Message ()
*
*  DESCRIPTION:    Sends a message to the X client
*
*******************************************************************}
function TGtk1TrayIconHandle.Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
var
  Ev: TXEvent;
//  fmt: Integer;
begin
  FillChar(Ev, SizeOf(TXEvent), $0);

  ev.xclient._type := ClientMessage;
  ev.xclient.window := window;
  ev.xclient.message_type := XInternAtom (fDisplay, '_NET_SYSTEM_TRAY_OPCODE', False );
  ev.xclient.format := 32;
  ev.xclient.data.l[0] := CurrentTime;
  ev.xclient.data.l[1] := msg;
  ev.xclient.data.l[2] := data1;
  ev.xclient.data.l[3] := data2;
  ev.xclient.data.l[4] := data3;

  XSendEvent(fDisplay, fTrayParent, False, NoEventMask, @ev);
  XSync(fDisplay, False);
  Result := false;//(untrap_errors() = 0);
end;

function TGtk1TrayIconHandle.TrayParent(UseCachedValue: Boolean = True): TWindow;
var
  buf: array[0..32] of char;
  selection_atom: TAtom;
begin
  if (fTrayParent <> 0) and UseCachedValue then
    Exit(fTrayParent);
  fDisplay := gdk_display;
  fScreenID := gdk_screen;
  //fScreenID := XScreenNumberOfScreen(fScreen); // and it's number
  XGrabServer(fDisplay);
  
  buf :=  PChar('_NET_SYSTEM_TRAY_S' + IntToStr(fScreenID));
  selection_atom := XInternAtom(fDisplay, buf, false);
  fTrayParent := XGetSelectionOwner(fDisplay, selection_atom);

  XUngrabServer(fDisplay);

  Result := fTrayParent;
end;

destructor TGtk1TrayIconHandle.Destroy;
begin
  if Assigned(FCanvas) and FCanvas.HandleAllocated then
  begin
    ReleaseDC(HWND(drawingarea), fcanvas.Handle);
    FCanvas.Free;
  end;
  if Assigned(drawingarea) then
  begin
    g_signal_handlers_destroy(G_OBJECT(drawingarea));
    gtk_widget_destroy(drawingarea);
  end;
  if Assigned(plug) then
    gtk_widget_destroy(plug);

  inherited Destroy;
end;

{*******************************************************************
*  TGtk1TrayIconHandle.SetEmbedded ()
*
*  DESCRIPTION:
*
*******************************************************************}
procedure TGtk1TrayIconHandle.SetEmbedded;
var
  old_error: TXErrorHandler;
begin
  fEmbedded := False;
  if TrayParent = None then
    Exit;

  // so we have a TWindow
  gtk_widget_realize(plug);

  old_error := XSetErrorHandler(@TempX11ErrorHandler);
  Sleep(80);
  xsync(fdisplay,true);
  
  XGrabServer(fDisplay);
  XSelectInput(fDisplay, TrayParent, StructureNotifyMask);
  XUngrabServer(fDisplay);
  XFlush(fDisplay);
  
  fWindow := GDK_WINDOW_XWINDOW (Pointer(PGtkWidget(plug)^.window));

  Send_Message(TrayParent, SYSTEM_TRAY_REQUEST_DOCK, fWindow, 0, 0);

  GTK_WIDGET_SET_FLAGS(plug,GTK_VISIBLE);
  GTK_WIDGET_SET_FLAGS(plug,GTK_MAPPED);
  
  gtk_widget_show_all(plug);

  XSetErrorHandler(old_error);
  fEmbedded:=True;
end;

procedure TGtk1TrayIconHandle.Hide;
begin
  gtk_widget_hide_all(drawingarea);
  fEmbedded := False;
end;

{*******************************************************************
*  TGtk1TrayIconHandle.CreateForm ()
*
*  DESCRIPTION:
*
*******************************************************************}
procedure TGtk1TrayIconHandle.CreateForm(id: Integer);
begin
  plug := gtk_plug_new(0);
  drawingarea := gtk_event_box_new;
  
  gtk_container_add(GTK_CONTAINER(plug), drawingarea);
  
  //gtk_widget_add_events(drawingarea, GDK_MOTION_NOTIFY);
  
  gtk_signal_connect_object_after(G_OBJECT(drawingarea), 'expose-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyExpose), G_OBJECT(Self));
  gtk_signal_connect_object(G_OBJECT(drawingarea), 'motion-notify-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyMouseMove), G_OBJECT(Self));
  gtk_signal_connect_object(G_OBJECT(drawingarea), 'button-press-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyMouseDown), G_OBJECT(Self));
  gtk_signal_connect_object(G_OBJECT(drawingarea), 'button-release-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyMouseUp), G_OBJECT(Self));
  //

  fEmbedded := False;
  GetCanvas;
end;

{*******************************************************************
*  TGtk1TrayIconHandle.GetPosition ()
*
*  DESCRIPTION:    Returns the (x, y) position of the icon on the screen
*
*******************************************************************}
function TGtk1TrayIconHandle.GetPosition: TPoint;
var
  WindowHandle: PGDKWindow;
begin
  Result := Point(0, 0);

  if not Assigned(plug) then Exit;

  WindowHandle := plug^.window;
  
  if not Assigned(WindowHandle) then Exit;
  
  gdk_window_get_origin(WindowHandle, @Result.X, @Result.Y);
end;

{*******************************************************************
*  TGtk1TrayIconHandle.SetMinSize ()
*
*  DESCRIPTION:    Attemps to avoid problems on Gnome
*
*******************************************************************}
procedure TGtk1TrayIconHandle.SetMinSize(AWidth, AHeight: Integer);
begin
  gtk_widget_set_usize(drawingarea, AWidth, AHeight);
end;

{*******************************************************************
*  TGtk1TrayIconHandle.PaintForm ()
*
*  DESCRIPTION:    Paint method of the Icon Window
*
*******************************************************************}
procedure TGtk1TrayIconHandle.PaintForm(Sender: TObject);
begin
  if fTrayIcon.ShowIcon then Canvas.Draw(0, 0, fTrayIcon.Icon);

  if Assigned(fTrayIcon.OnPaint) then fTrayIcon.OnPaint(Self);
end;

{*******************************************************************
*  TGtkWSCustomTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TGtkWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  TrayIconHandle: TGtk1TrayIconHandle;
begin
  Result := False;

  TrayIconHandle := TGtk1TrayIconHandle(ATrayIcon.Handle);
  TrayIconHandle.Free;
  
  ATrayIcon.Handle := 0;
  
  Result := True;
end;

{*******************************************************************
*  TGtkWSCustomTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TGtkWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  TrayIconHandle: TGtk1TrayIconHandle;
begin
  Result := False;

  TrayIconHandle := TGtk1TrayIconHandle.Create;
  TrayIconHandle.fTrayIcon := ATrayIcon;
  
  ATrayIcon.Handle := PtrInt(TrayIconHandle);
    
  TrayIconHandle.CreateForm(0);
  TrayIconHandle.SetMinSize(ATrayIcon.Icon.Width, ATrayIcon.Icon.Height);
  TrayIconHandle.SetEmbedded;
  
  Result := True;
end;

{*******************************************************************
*  TGtkWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*******************************************************************}
class procedure TGtkWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  TrayIconHandle: TGtk1TrayIconHandle;
begin
  TrayIconHandle := TGtk1TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit;
end;

{*******************************************************************
*  TGtkWSCustomTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*******************************************************************}
class function TGtkWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
var
  TrayIconHandle: TGtk1TrayIconHandle;
begin
  Result := Point(0, 0);

  TrayIconHandle := TGtk1TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit;

  Result := TrayIconHandle.GetPosition;
end;

class function TGtkWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas;
var
  TrayIconHandle: TGtk1TrayIconHandle;
begin
  TrayIconHandle := TGtk1TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit(ATrayIcon.Icon.Canvas);
  
  Result := TrayIconHandle.Canvas;
end;


