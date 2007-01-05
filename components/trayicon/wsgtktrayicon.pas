{
 wsgtktrayicon.pas

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

 Gtk1 specific code. Works on gnome also.
}
unit wsgtktrayicon;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Graphics, Classes, ExtCtrls, SysUtils, Forms, Controls, Dialogs,
  Menus, wscommontrayicon, x, xlib, xutil, gtk, gdk;

type

  { TWidgetTrayIcon }

  TWidgetTrayIcon = class(TCustomWidgetTrayIcon)
    private
      fDisplay: PDisplay;
      fWindow: TWindow;
      fScreen: PScreen;
      fScreenID: longint;
      fTrayParent: TWindow;
      fOwner: TComponent;
      GtkForm: TForm;
      fEmbedded: Boolean;
      fMsgCount: Integer;
      procedure SetEmbedded;
      function Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
      procedure SetMinSize(AWidth, AHeight: Integer);
      procedure PaintForm(Sender: TObject);
      procedure CreateForm(id: Integer);
      procedure RemoveForm(id: Integer);
      function GetCanvas: TCanvas;
    protected
    public
      function Hide: Boolean; override;
      function Show: Boolean; override;
      property Canvas: TCanvas read GetCanvas;
      procedure InternalUpdate; override;
      function GetPosition: TPoint; override;
    published
  end;

const
  SYSTEM_TRAY_REQUEST_DOCK   = 0;
  SYSTEM_TRAY_BEGIN_MESSAGE  = 1;
  SYSTEM_TRAY_CANCEL_MESSAGE = 2;
  
implementation

// Temp ErrorHandler
function TempX11ErrorHandler(Display:PDisplay; ErrorEv:PXErrorEvent):longint;cdecl;
begin
  WriteLn('Error: ' + IntToStr(ErrorEv^.error_code));
  Result:=0;
end;


{ TWidgetTrayIcon }

{*******************************************************************
*  TWidgetTrayIcon.SetEmbedded ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.SetEmbedded;
var
  old_error: TXErrorHandler;
  buf: array [0..32] of char;
  selection_atom : TAtom;
begin
  old_error := XSetErrorHandler(@TempX11ErrorHandler);
  Sleep(80);
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
    Send_Message(fTrayParent, SYSTEM_TRAY_REQUEST_DOCK, fWindow, 0, 0);

  XSetErrorHandler(old_error);
end;

{*******************************************************************
*  TWidgetTrayIcon.Send_Message ()
*
*  DESCRIPTION:    Sends a message to the X client
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
function TWidgetTrayIcon.Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
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
begin
  GtkForm := TForm.Create(nil);
  fEmbedded := False;
  //fWindow := GDK_WINDOW_XWINDOW (Pointer(PGtkWidget(GtkForm.Handle)^.window));
  //SHowMessage(IntToStr(Integer(fWindow)));
  //GtkForm.Parent := TWinConTrol(fOwner);
  GtkForm.WindowState := wsMinimized;
  GtkForm.BorderStyle := bsNone; //without this gnome will make a 1 pixel wide window!
  //GtkForm.Canvas.AutoRedraw := True; //not working :(

  // needed because some things aparently don't get fully initialized until
  // visible at least once!  This is Gtk related NOT LCL related.
  GtkForm.Visible :=True;
  GtkForm.Width := 22;
  GtkForm.Height := 22;
  GtkForm.Visible := False;

  Application.ProcessMessages;

  fDisplay := GDK_WINDOW_XDISPLAY(Pointer(PGtkWidget(GtkForm.Handle)^.window));
  fWindow := GDK_WINDOW_XWINDOW (Pointer(PGtkWidget(GtkForm.Handle)^.window));
  fScreen := XDefaultScreenOfDisplay(fDisplay); // get the screen
  fScreenID := XScreenNumberOfScreen(fScreen); // and it's number
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
  GtkForm.Free;
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
  Result := GtkForm.Canvas;
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
  
  SetEmbedded;

  GTK_WIDGET_SET_FLAGS(PGtkWidget(GtkForm.Handle),GTK_VISIBLE);
  GTK_WIDGET_SET_FLAGS(PGtkWidget(GtkForm.Handle),GTK_MAPPED);

  GtkForm.Width := 22; //needed for gnome
  GtkForm.Height := 22;
  SetMinSize(Icon.Width, Icon.Height);

  GtkForm.OnMouseDown := Self.OnMouseDown;
  GtkForm.OnMouseMove := Self.OnMouseMove;
  GtkForm.OnMouseUp := Self.OnMouseUp;
  GtkForm.OnClick := Self.OnClick;
  GtkForm.OnDblClick := Self.OnDblClick;
  GtkForm.OnPaint := PaintForm;
  GtkForm.PopupMenu := Self.PopUpMenu;
  GtkForm.Hint := Self.Hint;

  fEmbedded := True;
  
  vVisible := True;

  Result := True;
end;

{*******************************************************************
*  TWidgetTrayIcon.SetMinSize ()
*
*  DESCRIPTION:    Attemps to avoid problems on Gnome
*
*  PARAMETERS:
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.SetMinSize(AWidth, AHeight: Integer);
var
  size_hints: TXSizeHints;
begin
  FillChar(size_hints, SizeOf(TXSizeHints), $0);

  size_hints.flags := PSize or PMinSize or PMaxSize;
  size_hints.min_width := AWidth;
  size_hints.max_width := 100;
  size_hints.min_height := AHeight;
  size_hints.max_height := 100;

  XSetStandardProperties(fDisplay, fWindow, nil, nil, None, nil, 0, @size_hints);
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
  if ShowIcon then GtkForm.Canvas.Draw(0, 0, Icon);
  
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
  if Assigned(GtkForm) then GtkForm.PopupMenu := Self.PopUpMenu;
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


