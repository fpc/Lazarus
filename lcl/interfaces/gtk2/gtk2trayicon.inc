{%MainUnit gtk2wsextctrls.pp}
{
 gtk2trayicon.inc

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

 Gtk2 specific code.
}

{ TGtk2WSCustomTrayIcon }

type
  TGtk2WSTrayIconHandle = record
    AImage: PGtkWidget;
  end;

  PGtk2WSTrayIconHandle = ^TGtk2WSTrayIconHandle;

const
  SYSTEM_TRAY_REQUEST_DOCK   = 0;
  SYSTEM_TRAY_BEGIN_MESSAGE  = 1;
  SYSTEM_TRAY_CANCEL_MESSAGE = 2;

var
{$ifdef HasX}
  fDisplay: PDisplay;
  fWindow: TWindow;
  fScreen: PScreen;
  fScreenID: longint;
  fTrayParent: TWindow;
{$endif}

  GtkForm: PGtkWidget;
  Tips: PGtkTooltips;

{$ifdef HasX}
{*******************************************************************
*  TempX11ErrorHandler ()
*
*  DESCRIPTION:    Temp ErrorHandler
*
*  PARAMETERS:     ?
*
*  RETURNS:        ?
*
*******************************************************************}
function TempX11ErrorHandler(Display:PDisplay; ErrorEv:PXErrorEvent):longint;cdecl;
begin
  WriteLn('Error: ' + IntToStr(ErrorEv^.error_code));
  Result:=0;
end;

{*******************************************************************
*  Send_Message ()
*
*  DESCRIPTION:    Sends a message to the X client
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function SendMessage(window: TWindow; msg: Integer; data1, data2, data3: Integer): boolean;
var
  Ev: TXEvent;
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

{*******************************************************************
*  SetEmbedded ()
*
*  DESCRIPTION:    Docks the GtkPlug into the system tray
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure SetEmbedded;
var
  old_error: TXErrorHandler;
  buf: array [0..32] of char;
  selection_atom : TAtom;
begin
  old_error := XSetErrorHandler(@TempX11ErrorHandler);

  xsync(fdisplay,true);
  buf :=  PChar('_NET_SYSTEM_TRAY_S' + IntToStr(fScreenID));
  selection_atom := XInternAtom(fDisplay, buf, false);
  XGrabServer(fDisplay);

  fTrayParent := XGetSelectionOwner(fDisplay, selection_atom);
  if fTrayParent <> None then
  begin
    XSelectInput(fDisplay, fTrayParent, StructureNotifyMask);
  end;

  XUngrabServer(fDisplay);
  XFlush(fDisplay);

  if fTrayParent <> None then
    SendMessage(fTrayParent, SYSTEM_TRAY_REQUEST_DOCK, fWindow, 0, 0);

  XSetErrorHandler(old_error);
end;
{$endif}

{*******************************************************************
*  realize_cb ()
*
*  DESCRIPTION:    Callback function for the realize signal
*                  Sets the systray icon after the widget is realized
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure realize_cb(widget: PGtkWidget; user_data: gpointer); cdecl;
begin
{$ifdef HasGdk2X}

  fDisplay := GDK_WINDOW_XDISPLAY(GtkForm^.window);
  fWindow := GDK_WINDOW_XWINDOW(GtkForm^.window);

{  Doesn´t work

  gdk_screen := gtk_widget_get_screen(GtkForm);
  fScreen := GDK_SCREEN_XSCREEN(gdk_screen); // get the real screen}

  fScreen := XDefaultScreenOfDisplay(fDisplay);
  fScreenID := XScreenNumberOfScreen(fScreen); // and it's number
  
  SetEmbedded;
{$endif}
end;

{*******************************************************************
*  button_release_cb ()
*
*  DESCRIPTION:    Callback function for Mouse Click
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function button_release_cb(widget: PGtkWidget; event: PGdkEventButton;
 user_data: gpointer): gboolean; cdecl;
var
  vwsTrayIcon: TCustomTrayIcon;
begin
  vwsTrayIcon := TCustomTrayIcon(user_data);

  Result := False;
  
  case event^.button of
    1:
    begin
      if Assigned(vwsTrayIcon.OnClick) then vwsTrayIcon.OnClick(vwsTrayIcon);
      if Assigned(vwsTrayIcon.OnMouseUp) then
       vwsTrayIcon.OnMouseUp(vwsTrayIcon, mbLeft, [], Round(event^.X), Round(event^.Y));
    end;
    
    2: if Assigned(vwsTrayIcon.OnMouseUp) then
        vwsTrayIcon.OnMouseUp(vwsTrayIcon, mbMiddle, [], Round(event^.X), Round(event^.Y));

    3:
    begin
      if Assigned(vwsTrayIcon.OnMouseUp) then
       vwsTrayIcon.OnMouseUp(vwsTrayIcon, mbRight, [], Round(event^.X), Round(event^.Y));
      if Assigned(vwsTrayIcon.PopUpMenu) then
       vwsTrayIcon.PopUpMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
     end;
  end;
end;

{*******************************************************************
*  button_press_cb ()
*
*  DESCRIPTION:    Callback function for Mouse Click
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function button_press_cb(widget: PGtkWidget; event: PGdkEventButton;
 user_data: gpointer): gboolean; cdecl;
var
  vwsTrayIcon: TCustomTrayIcon;
begin
  vwsTrayIcon := TCustomTrayIcon(user_data);

  Result := False;

  if (event^._type = GDK_2BUTTON_PRESS) and Assigned(vwsTrayIcon.OnDblClick) then
   vwsTrayIcon.OnDblClick(vwsTrayIcon)
  else
  begin
    case event^.button of
      1: if Assigned(vwsTrayIcon.OnMouseDown) then
          vwsTrayIcon.OnMouseDown(vwsTrayIcon, mbLeft, [], Round(event^.X), Round(event^.Y));

      2: if Assigned(vwsTrayIcon.OnMouseDown) then
          vwsTrayIcon.OnMouseDown(vwsTrayIcon, mbMiddle, [], Round(event^.X), Round(event^.Y));

      3: if Assigned(vwsTrayIcon.OnMouseDown) then
          vwsTrayIcon.OnMouseDown(vwsTrayIcon, mbRight, [], Round(event^.X), Round(event^.Y));
    end;
  end;
end;

{*******************************************************************
*  popup_cb ()
*
*  DESCRIPTION:    Callback function for the popup menu
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function popup_cb(widget: PGtkWidget; user_data: gpointer): Boolean; cdecl;
var
  vwsTrayIcon: TCustomTrayIcon;
begin
  vwsTrayIcon := TCustomTrayIcon(user_data);

  Result := True;

  if Assigned(vwsTrayIcon.PopUpMenu) then
   vwsTrayIcon.PopUpMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{*******************************************************************
*  motion_cb ()
*
*  DESCRIPTION:    Callback function for the OnMouseMove event
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function motion_cb(widget: PGtkWidget; event: PGdkEventMotion; user_data: gpointer): Boolean; cdecl;
var
  vwsTrayIcon: TCustomTrayIcon;
begin
  vwsTrayIcon := TCustomTrayIcon(user_data);

  Result := False;

  if Assigned(vwsTrayIcon.OnMouseMove) then
   vwsTrayIcon.OnMouseMove(vwsTrayIcon, [], Round(event^.X), Round(event^.Y));
end;

{*******************************************************************
*  TGtk2WSCustomTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TGtk2WSCustomTrayIcon.Hide(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  TrayHandle: PGtk2WSTrayIconHandle absolute ATrayIcon.Handle;
begin
  Result := False;

  gtk_widget_destroy(GtkForm);

  GtkForm := nil;

  g_object_unref(Tips);

  Tips := nil;

  { Free and nil the handle }

  FreeMem(TrayHandle);

  TrayHandle := nil;

  Result := True;
end;

{*******************************************************************
*  TGtk2WSCustomTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
class function TGtk2WSCustomTrayIcon.Show(const ATrayIcon: TCustomTrayIcon): Boolean;
var
  TrayHandle: PGtk2WSTrayIconHandle absolute ATrayIcon.Handle;
begin
  Result := False;

  ATrayIcon.Handle := PtrInt(GetMem(SizeOf(TGtk2WSTrayIconHandle)));

  {*******************************************************************
  *  Creates the GtkPlug
  *******************************************************************}

  GtkForm := gtk_plug_new(0);

  Tips := gtk_tooltips_new;

  g_object_ref(Tips);

  gtk_object_sink(GTK_OBJECT(Tips));

  gtk_tooltips_set_tip(GTK_TOOLTIPS(Tips), GtkForm, PChar(ATrayIcon.Hint), '');

  {*******************************************************************
  *  Connects the signals
  *******************************************************************}

  gtk_widget_add_events(GtkForm, GDK_ALL_EVENTS_MASK);

  g_signal_connect(GtkForm, 'realize', TGCallback(@realize_cb), ATrayIcon);

  g_signal_connect(GtkForm, 'popup-menu', TGCallback(@popup_cb), ATrayIcon);

  g_signal_connect(GtkForm, 'motion-notify-event', TGCallback(@motion_cb), ATrayIcon);

  g_signal_connect(GtkForm, 'button-press-event', TGCallback(@button_press_cb), ATrayIcon);

  g_signal_connect(GtkForm, 'button-release-event', TGCallback(@button_release_cb), ATrayIcon);

  {*******************************************************************
  *  Draws the icon
  *******************************************************************}

  TrayHandle^.AImage := gtk_image_new_from_pixbuf(PGdkPixbuf(ATrayIcon.Icon.Handle));

  gtk_widget_show(TrayHandle^.AImage);

  gtk_container_add(GTK_CONTAINER(GtkForm), TrayHandle^.AImage);

  {*******************************************************************
  *  Now shows the GtkPlug
  *******************************************************************}

  gtk_widget_show(GtkForm);

  Result := True;
end;

{*******************************************************************
*  TGtk2WSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
class procedure TGtk2WSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  TrayHandle: PGtk2WSTrayIconHandle absolute ATrayIcon.Handle;
begin
  // Updates the tooltips
  if Assigned(Tips) then gtk_tooltips_set_tip(GTK_TOOLTIPS(Tips), GtkForm, PChar(ATrayIcon.Hint), '');

  // Updates the icon
  
  if (TrayHandle <> nil) and (TrayHandle^.AImage <> nil) then
  begin
    gtk_image_set_from_pixbuf(GTK_IMAGE(TrayHandle^.AImage), PGdkPixbuf(ATrayIcon.Icon.Handle));
  end;
end;

{*******************************************************************
*  TGtk2WSCustomTrayIcon.GetPosition ()
*
*  DESCRIPTION:    Returns the position of the tray icon on the display.
*                  This function is utilized to show message boxes near
*                  the icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
class function TGtk2WSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
var
  WindowHandle: PGDKWindow;
begin
  Result := Point(0, 0);

  if not Assigned(GtkForm) then Exit;

  WindowHandle := GtkForm^.window;

  if not Assigned(WindowHandle) then Exit;

  gdk_window_get_origin(WindowHandle, @Result.X, @Result.Y);
end;

