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

 Gtk2 specific code.
}
unit wsgtk2trayicon;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Graphics, Classes, ExtCtrls, SysUtils, StdCtrls, Forms, Controls, Dialogs,
  Menus, x, xlib, xutil, gtk2, gdk2;

type

  { TWidgetTrayIcon }

  TWidgetTrayIcon = class(TObject)
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
      uID: Cardinal;
      Icon: TIcon;
      ShowIcon, ShowToolTip: Boolean;
      PopUpMenu: TPopUpMenu;
      ToolTip: string;
      OnPaint, OnClick, OnDblClick: TNotifyEvent;
      OnMouseDown, OnMouseUp: TMouseEvent;
      OnMouseMove: TMouseMoveEvent;
      constructor Create;
      destructor Destroy; override;
      function Hide: Boolean;
      function Show: Boolean;
      property Canvas: TCanvas read GetCanvas;
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
end;


{ TWidgetTrayIcon }

{*******************************************************************
*  TWidgetTrayIcon.Create ()
*
*  DESCRIPTION:    Creates a object from the TWidgetTrayIcon class
*
*  PARAMETERS:     None
*
*  RETURNS:        A pointer to the newly created object
*
*******************************************************************}
constructor TWidgetTrayIcon.Create;
begin
  inherited Create;

  Icon := TIcon.Create;

  uID := 3;
end;

{*******************************************************************
*  TWidgetTrayIcon.Destroy ()
*
*  DESCRIPTION:    Destroys a object derived from the TWidgetTrayIcon class
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
destructor TWidgetTrayIcon.Destroy;
begin
  Icon.Free;

  inherited Destroy;
end;

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

function TWidgetTrayIcon.Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
var
  Ev: TXEvent;
  fmt: Integer;
begin
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

  fDisplay := (PGdkWindowPrivate(PGtkWidget(GtkForm.Handle)^.window))^.xdisplay;
  fWindow := GDK_WINDOW_XWINDOW (Pointer(PGtkWidget(GtkForm.Handle)^.window));
  fScreen := XDefaultScreenOfDisplay(fDisplay); // get the screen
  fScreenID := XScreenNumberOfScreen(fScreen); // and it's number
end;

procedure TWidgetTrayIcon.RemoveForm(id: Integer);
begin
  GtkForm.Free;
end;

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
  RemoveForm(0);
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
  GtkForm.OnPaint := PaintForm;
  GtkForm.PopupMenu := Self.PopUpMenu;

  fEmbedded := True;
end;

procedure TWidgetTrayIcon.SetMinSize(AWidth, AHeight: Integer);
var
  size_hints: TXSizeHints;
begin
  size_hints.flags := PSize or PMinSize or PMaxSize;
  size_hints.min_width := AWidth;
  size_hints.max_width := 100;
  size_hints.min_height := AHeight;
  size_hints.max_height := 100;
  XSetStandardProperties(fDisplay, fWindow, nil, nil, None, nil, 0, @size_hints);
end;

procedure TWidgetTrayIcon.PaintForm(Sender: TObject);
begin
  if ShowIcon then GtkForm.Canvas.Draw(0, 0, Icon);

  if Assigned(OnPaint) then OnPaint(Self);
end;

end.

