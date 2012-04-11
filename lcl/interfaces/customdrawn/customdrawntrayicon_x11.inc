{%mainunit customdrawnwsextctrls.pas}

type

  { TX11TrayIconHandle }

  TX11TrayIconHandle = class(TObject)
  private
    //plug: PGtkWidget;
    //drawingarea: PGtkWidget;
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
    procedure PaintForm(Sender: TObject);
    function  Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
  public
    constructor create;
    destructor Destroy; override;
    procedure CreateForm(id: Integer);
    function  GetPosition: TPoint;
    procedure Hide;
    procedure SetEmbedded;
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
  DebugLn('[TX11TrayIconHandle] Error: ' + IntToStr(ErrorEv^.error_code));
  Result:=0;
end;

function TX11TrayIconHandle.GetCanvas: TCanvas;
begin
  Result := nil;
{  if Assigned(FCanvas) then Exit(FCanvas);

  Result := TCanvas.Create;
  Result.Handle:= GetDC(HWND(drawingarea));
  FCanvas := Result;}
end;

{function TX11TrayIconHandle.NotifyExpose(Event: PGdkEventExpose;
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
end;       }

{*******************************************************************
*  TX11TrayIconHandle.Send_Message ()
*
*  DESCRIPTION:    Sends a message to the X client
*
*******************************************************************}
function TX11TrayIconHandle.Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
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

constructor TX11TrayIconHandle.create;
begin
  inherited create;
end;

function TX11TrayIconHandle.TrayParent(UseCachedValue: Boolean = True): TWindow;
var
  buf: array[0..32] of char;
  selection_atom: TAtom;
begin
  if (fTrayParent <> 0) and UseCachedValue then
    Exit(fTrayParent);
  fDisplay := CDWidgetset.FDisplay;
  fScreenID := CDWidgetSet.FScreen;

  buf :=  PChar('_NET_SYSTEM_TRAY_S' + IntToStr(fScreenID));
  selection_atom := XInternAtom(fDisplay, buf, false);
  fTrayParent := XGetSelectionOwner(fDisplay, selection_atom);

  Result := fTrayParent;
end;

destructor TX11TrayIconHandle.Destroy;
begin
  if Assigned(FCanvas) and FCanvas.HandleAllocated then
  begin
    //ReleaseDC(HWND(drawingarea), fcanvas.Handle);
    FCanvas.Free;
  end;
{  if Assigned(drawingarea) then
  begin
    g_signal_handlers_destroy(G_OBJECT(drawingarea));
    gtk_widget_destroy(drawingarea);
  end;
  if Assigned(plug) then
    gtk_widget_destroy(plug);}

  inherited Destroy;
end;

{*******************************************************************
*  TX11TrayIconHandle.SetEmbedded ()
*
*  DESCRIPTION:
*
*******************************************************************}
procedure TX11TrayIconHandle.SetEmbedded;
var
  old_error: TXErrorHandler;
begin
  fEmbedded := False;
  if TrayParent = None then
    Exit;

{  // so we have a TWindow
  gtk_widget_realize(plug);}

  old_error := XSetErrorHandler(@TempX11ErrorHandler);
  Sleep(80);
  xsync(fdisplay,true);

  XSelectInput(fDisplay, TrayParent, StructureNotifyMask);

  //fWindow := GDK_WINDOW_XWINDOW (Pointer(PGtkWidget(plug)^.window));

  Send_Message(TrayParent, SYSTEM_TRAY_REQUEST_DOCK, fWindow, 0, 0);

  //GTK_WIDGET_SET_FLAGS(plug,GTK_VISIBLE);
  //GTK_WIDGET_SET_FLAGS(plug,GTK_MAPPED);

  //gtk_widget_show_all(plug);

  XSetErrorHandler(old_error);
  fEmbedded:=True;
end;

procedure TX11TrayIconHandle.Hide;
begin
  //gtk_widget_hide_all(drawingarea);
  fEmbedded := False;
end;

{*******************************************************************
*  TX11TrayIconHandle.CreateForm ()
*
*  DESCRIPTION:
*
*******************************************************************}
procedure TX11TrayIconHandle.CreateForm(id: Integer);
begin
{  plug := gtk_plug_new(0);
  drawingarea := gtk_event_box_new;

  gtk_container_add(GTK_CONTAINER(plug), drawingarea);

  //gtk_widget_add_events(drawingarea, GDK_MOTION_NOTIFY);

  gtk_signal_connect_object_after(G_OBJECT(drawingarea), 'expose-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyExpose), G_OBJECT(Self));
  gtk_signal_connect_object(G_OBJECT(drawingarea), 'motion-notify-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyMouseMove), G_OBJECT(Self));
  gtk_signal_connect_object(G_OBJECT(drawingarea), 'button-press-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyMouseDown), G_OBJECT(Self));
  gtk_signal_connect_object(G_OBJECT(drawingarea), 'button-release-event', TGtkSignalFunc(@TGtk1TrayIconHandle.NotifyMouseUp), G_OBJECT(Self));}

  fEmbedded := False;
  GetCanvas;
end;

{*******************************************************************
*  TX11TrayIconHandle.GetPosition ()
*
*  DESCRIPTION:    Returns the (x, y) position of the icon on the screen
*
*******************************************************************}
function TX11TrayIconHandle.GetPosition: TPoint;
{var
  WindowHandle: PGDKWindow;}
begin
  Result := Point(0, 0);

{  if not Assigned(plug) then Exit;

  WindowHandle := plug^.window;

  if not Assigned(WindowHandle) then Exit;

  gdk_window_get_origin(WindowHandle, @Result.X, @Result.Y);}
end;

{*******************************************************************
*  TX11TrayIconHandle.PaintForm ()
*
*  DESCRIPTION:    Paint method of the Icon Window
*
*******************************************************************}
procedure TX11TrayIconHandle.PaintForm(Sender: TObject);
begin
  if fTrayIcon.ShowIcon then Canvas.Draw(0, 0, fTrayIcon.Icon);

  if Assigned(fTrayIcon.OnPaint) then fTrayIcon.OnPaint(Self);
end;

{*******************************************************************
*  TCDWSCustomTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TCDWSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  TrayIconHandle: TX11TrayIconHandle;
begin
  Result := False;

  TrayIconHandle := TX11TrayIconHandle(ATrayIcon.Handle);
  TrayIconHandle.Free;

  ATrayIcon.Handle := 0;

  Result := True;
end;

{*******************************************************************
*  TCDWSCustomTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TCDWSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  TrayIconHandle: TX11TrayIconHandle;
begin
  Result := False;

  TrayIconHandle := TX11TrayIconHandle.Create;
  TrayIconHandle.fTrayIcon := ATrayIcon;

  ATrayIcon.Handle := PtrInt(TrayIconHandle);

  TrayIconHandle.CreateForm(0);
  TrayIconHandle.SetEmbedded;

  Result := True;
end;

{*******************************************************************
*  TCDWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*******************************************************************}
class procedure TCDWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
{var
  TrayIconHandle: TGtk1TrayIconHandle;}
begin
{  TrayIconHandle := TGtk1TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit;}
end;

class function TCDWSCustomTrayIcon.ShowBalloonHint(
  const ATrayIcon: TCustomTrayIcon): Boolean;
begin
  Result := False;
end;

{*******************************************************************
*  TCDWSCustomTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*******************************************************************}
class function TCDWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
var
  TrayIconHandle: TX11TrayIconHandle;
begin
  Result := Point(0, 0);

  TrayIconHandle := TX11TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit;

  Result := TrayIconHandle.GetPosition;
end;

{class function TGtkWSCustomTrayIcon.GetCanvas(const ATrayIcon: TCustomTrayIcon): TCanvas;
var
  TrayIconHandle: TX11TrayIconHandle;
begin
  TrayIconHandle := TX11TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit(ATrayIcon.Icon.Canvas);

  Result := TrayIconHandle.Canvas;
end;     }

