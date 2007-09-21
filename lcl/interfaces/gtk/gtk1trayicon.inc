{%MainUnit gtkwsextctrls.pp}
{
 gtk1trayicon.pas

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

{ TGtkWSCustomTrayIcon }

type
  TGtk1TrayIconHandle = class(TObject)
  public
    fDisplay: PDisplay;
    fWindow: TWindow;
    fScreen: PScreen;
    fScreenID: longint;
    fTrayParent: TWindow;
    fOwner: TComponent;
    GtkForm: TForm;
    fEmbedded: Boolean;
    fMsgCount: Integer;
    fTrayIcon: TCustomTrayIcon;
    function  Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
    procedure SetEmbedded;
    procedure CreateForm(id: Integer);
    procedure SetMinSize(AWidth, AHeight: Integer);
    procedure PaintForm(Sender: TObject);
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


{*******************************************************************
*  TGtk1TrayIconHandle.Send_Message ()
*
*  DESCRIPTION:    Sends a message to the X client
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
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

{*******************************************************************
*  TGtk1TrayIconHandle.SetEmbedded ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TGtk1TrayIconHandle.SetEmbedded;
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
*  TGtk1TrayIconHandle.CreateForm ()
*
*  DESCRIPTION:
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TGtk1TrayIconHandle.CreateForm(id: Integer);
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
*  TGtk1TrayIconHandle.SetMinSize ()
*
*  DESCRIPTION:    Attemps to avoid problems on Gnome
*
*  PARAMETERS:
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TGtk1TrayIconHandle.SetMinSize(AWidth, AHeight: Integer);
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
*  TGtk1TrayIconHandle.PaintForm ()
*
*  DESCRIPTION:    Paint method of the Icon Window
*
*  PARAMETERS:     Sender of the event
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TGtk1TrayIconHandle.PaintForm(Sender: TObject);
begin
  if fTrayIcon.ShowIcon then GtkForm.Canvas.Draw(0, 0, fTrayIcon.Icon);

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

  TrayIconHandle.GtkForm.Free;

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
  
  TrayIconHandle.SetEmbedded;

  GTK_WIDGET_SET_FLAGS(PGtkWidget(TrayIconHandle.GtkForm.Handle),GTK_VISIBLE);
  GTK_WIDGET_SET_FLAGS(PGtkWidget(TrayIconHandle.GtkForm.Handle),GTK_MAPPED);

  TrayIconHandle.GtkForm.Width := 22; //needed for gnome
  TrayIconHandle.GtkForm.Height := 22;
  TrayIconHandle.SetMinSize(ATrayIcon.Icon.Width, ATrayIcon.Icon.Height);

  TrayIconHandle.GtkForm.OnMouseDown := ATrayIcon.OnMouseDown;
  TrayIconHandle.GtkForm.OnMouseMove := ATrayIcon.OnMouseMove;
  TrayIconHandle.GtkForm.OnMouseUp := ATrayIcon.OnMouseUp;
  TrayIconHandle.GtkForm.OnClick := ATrayIcon.OnClick;
  TrayIconHandle.GtkForm.OnDblClick := ATrayIcon.OnDblClick;
  TrayIconHandle.GtkForm.OnPaint := @TrayIconHandle.PaintForm;
  TrayIconHandle.GtkForm.PopupMenu := ATrayIcon.PopUpMenu;
  TrayIconHandle.GtkForm.Hint := ATrayIcon.Hint;

  TrayIconHandle.fEmbedded := True;
  
  Result := True;
end;

{*******************************************************************
*  TGtkWSCustomTrayIcon.InternalUpdate ()
*
*  DESCRIPTION:    Makes modifications to the Icon while running
*                  i.e. without hiding it and showing again
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
class procedure TGtkWSCustomTrayIcon.InternalUpdate(const ATrayIcon: TCustomTrayIcon);
var
  TrayIconHandle: TGtk1TrayIconHandle;
begin
  TrayIconHandle := TGtk1TrayIconHandle(ATrayIcon.Handle);

  if not Assigned(TrayIconHandle) then Exit;

  if Assigned(TrayIconHandle.GtkForm) then
   TrayIconHandle.GtkForm.PopupMenu := ATrayIcon.PopUpMenu;
end;

{*******************************************************************
*  TGtkWSCustomTrayIcon.GetPosition ()
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
class function TGtkWSCustomTrayIcon.GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;


