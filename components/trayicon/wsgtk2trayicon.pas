{
 wsgtk2trayicon.pas

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
unit WSGtk2TrayIcon;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

{$PACKRECORDS C}
  
interface

uses
  Graphics, Classes, ExtCtrls, SysUtils, Forms, Controls, Dialogs,
  Menus, WSCommonTrayIcon, x, xlib, xutil, gtk2, gdk2, gdk2x, glib2, gtkdef;

type

  { TWidgetTrayIcon }

  TWidgetTrayIcon = class(TCustomWidgetTrayIcon)
    private
      fOwner: TComponent;
      fEmbedded: Boolean;
      fMsgCount: Integer;
      Tips: PGtkTooltips;
      procedure CreateForm(id: Integer);
      procedure RemoveForm(id: Integer);
      function GetCanvas: TCanvas;
    protected
    public
      function Hide: Boolean; override;
      function Show: Boolean; override;
      property Canvas: TCanvas read GetCanvas;
      procedure InternalUpdate; override;
      procedure PaintForm(Sender: TObject);
      function GetPosition: TPoint; override;
    published
  end;

const
  SYSTEM_TRAY_REQUEST_DOCK   = 0;
  SYSTEM_TRAY_BEGIN_MESSAGE  = 1;
  SYSTEM_TRAY_CANCEL_MESSAGE = 2;

implementation

uses WSTrayIcon;

var
  fDisplay: PDisplay;
  fWindow: TWindow;
  fScreen: PScreen;
  fScreenID: longint;
  GtkForm: PGtkWidget;
  fTrayParent: TWindow;

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
  fmt: Integer;
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
var
  gdk_screen: PGdkScreen;
begin
  fDisplay := GDK_WINDOW_XDISPLAY(GtkForm^.window);
  fWindow := GDK_WINDOW_XWINDOW(GtkForm^.window);

{  Doesn´t work

  gdk_screen := gtk_widget_get_screen(GtkForm);
  fScreen := GDK_SCREEN_XSCREEN(gdk_screen); // get the real screen}

  fScreen := XDefaultScreenOfDisplay(fDisplay);
  fScreenID := XScreenNumberOfScreen(fScreen); // and it's number
  
  SetEmbedded;
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
  vwsTrayIcon: TWSTrayIcon;
begin
  vwsTrayIcon := TWSTrayIcon(user_data);

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

    3: if Assigned(vwsTrayIcon.OnMouseUp) then
        vwsTrayIcon.OnMouseUp(vwsTrayIcon, mbRight, [], Round(event^.X), Round(event^.Y));
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
  vwsTrayIcon: TWSTrayIcon;
begin
  vwsTrayIcon := TWSTrayIcon(user_data);

  Result := False;

  if (event^._type = GDK_2BUTTON_PRESS) and Assigned(vwsTrayIcon.OnDblClick) then
   vwsTrayIcon.OnDblClick(vwsTrayIcon)
  else
  begin
    case event^.button of
      1: if Assigned(vwsTrayIcon.OnMouseUp) then
          vwsTrayIcon.OnMouseDown(vwsTrayIcon, mbLeft, [], Round(event^.X), Round(event^.Y));

      2: if Assigned(vwsTrayIcon.OnMouseUp) then
         vwsTrayIcon.OnMouseDown(vwsTrayIcon, mbMiddle, [], Round(event^.X), Round(event^.Y));

      3:
      begin
        if Assigned(vwsTrayIcon.OnMouseUp) then
         vwsTrayIcon.OnMouseDown(vwsTrayIcon, mbRight, [], Round(event^.X), Round(event^.Y));
        if Assigned(vwsTrayIcon.PopUpMenu) then
         vwsTrayIcon.PopUpMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end;
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
  vwsTrayIcon: TWSTrayIcon;
begin
  vwsTrayIcon := TWSTrayIcon(user_data);

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
  vwsTrayIcon: TWSTrayIcon;
begin
  vwsTrayIcon := TWSTrayIcon(user_data);

  Result := False;

  if Assigned(vwsTrayIcon.OnMouseMove) then
   vwsTrayIcon.OnMouseMove(vwsTrayIcon, [], Round(event^.X), Round(event^.Y));
end;

{ TWidgetTrayIcon }

{*******************************************************************
*  TWidgetTrayIcon.CreateForm ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.CreateForm(id: Integer);
var
  AImage: PGtkWidget;
  GDIObject: PgdiObject;
begin
  {*******************************************************************
  *  Creates the GtkPlug
  *******************************************************************}
  
  fEmbedded := False;

  GtkForm := gtk_plug_new(0);

  Tips := gtk_tooltips_new;

  g_object_ref(Tips);
  
  gtk_object_sink(GTK_OBJECT(Tips));
  
  gtk_tooltips_set_tip(GTK_TOOLTIPS(Tips), GtkForm, PChar(Hint), '');

  {*******************************************************************
  *  Connects the signals
  *******************************************************************}
  
  gtk_widget_add_events(GtkForm, GDK_ALL_EVENTS_MASK);

  g_signal_connect(GtkForm, 'realize', TGCallback(@realize_cb), Self);

  g_signal_connect(GtkForm, 'popup-menu', TGCallback(@popup_cb), Self);

  g_signal_connect(GtkForm, 'motion-notify-event', TGCallback(@motion_cb), Self);

  g_signal_connect(GtkForm, 'button-press-event', TGCallback(@button_press_cb), Self);

  g_signal_connect(GtkForm, 'button-release-event', TGCallback(@button_release_cb), Self);

  {*******************************************************************
  *  Draws the icon
  *******************************************************************}

  GDIObject := PgdiObject(Icon.Handle);

  AImage := gtk_image_new_from_pixmap(GDIObject^.GDIPixmapObject,
   GDIObject^.GDIBitmapMaskObject);

  gtk_widget_show(AImage);

  gtk_container_add(GTK_CONTAINER(GtkForm), AImage);

  {*******************************************************************
  *  Now shows the GtkPlug
  *******************************************************************}

  gtk_widget_show(GtkForm);
end;

{*******************************************************************
*  TWidgetTrayIcon.RemoveForm ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.RemoveForm(id: Integer);
begin
  gtk_widget_destroy(GtkForm);
  
  GtkForm := nil;

  g_object_unref(Tips);

  Tips := nil;
end;

{*******************************************************************
*  TWidgetTrayIcon.GetCanvas ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TWidgetTrayIcon.GetCanvas: TCanvas;
begin
  Result := Icon.Canvas;
end;

{*******************************************************************
*  TWidgetTrayIcon.Hide ()
*
*  DESCRIPTION:    Hides the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
function TWidgetTrayIcon.Hide: Boolean;
begin
  Result := False;

  if not vVisible then Exit;

  RemoveForm(0);

  vVisible := False;
  
  Result := True;
end;

{*******************************************************************
*  TWidgetTrayIcon.Show ()
*
*  DESCRIPTION:    Shows the main tray icon of the program
*
*  PARAMETERS:     None
*
*  RETURNS:        True if sucessfull, otherwise False
*
*******************************************************************}
function TWidgetTrayIcon.Show: Boolean;
begin
  Result := False;

  if vVisible then Exit;
  
  CreateForm(0);

  fEmbedded := True;

  vVisible := True;

  Result := True;
end;

{*******************************************************************
*  TWidgetTrayIcon.PaintForm ()
*
*  DESCRIPTION:    Paint method of the Icon Window
*
*  PARAMETERS:     Sender of the event
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.PaintForm(Sender: TObject);
begin
//  if ShowIcon then GtkForm.Canvas.Draw(0, 0, Icon);

  if Assigned(OnPaint) then OnPaint(Self);
end;

{*******************************************************************
*  TWidgetTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.InternalUpdate;
begin
  if Assigned(Tips) then gtk_tooltips_set_tip(GTK_TOOLTIPS(Tips), GtkForm, PChar(Hint), '');
end;

{*******************************************************************
*  TWidgetTrayIcon.GetPosition ()
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
function TWidgetTrayIcon.GetPosition: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

end.

