{
 wsx11trayicon.pas

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

 X11 specific code.
}
unit wsx11trayicon;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Menus, Controls, Lclintf,
  wscommontrayicon, X, XLib, XUtil;

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
      fEmbedded: Boolean;
      fMsgCount: Integer;

      fDrawable: TWindow;
      fWindowID: TWindow;
      fVisual: PVisual;
      fDepth: Integer;
      fRootWindow: TWindow;
      gc: Xlib.TGC;
{      fIcon: TIcon;
      fEmbedded: Boolean;
      fMenu: TPopupMenu;
      fMouseEnter: TNotifyEvent;
      fMouseLeave: TNotifyEvent;
      fMouseDown: TMouseEvent;
      fMouseUp: TMouseEvent;
      fMouseMove: TMouseMoveEvent;
      fClick: TNotifyEvent;
      fTrayParent: TWindow;
      fWidth, fHeight: Integer;
      fIconWidth,
      fIconHeight: Integer;
      fTimer: TTimer;
      Image: xlib.PXImage;
      fMsgCount: Integer;
      WindowHandle: Cardinal;}
      procedure SetEmbedded;
      procedure InitWM;
      procedure SetMinSize(AWidth, AHeight: Integer);
      function Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
      function AttachIcon: TWindow;
      function GetCanvas: TCanvas;
    protected
    public
      hIcon, hSmallIcon: Cardinal;
      ShowToolTip: Boolean;
      ToolTip: string;
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
  Event: TXEvent;
  buf: array[0..19] of char;
  GCVals: TXGCValues;
begin
  fDisplay := XOpenDisplay(nil);
  fScreen := XDefaultScreenOfDisplay(fDisplay);
  fVisual := DefaultVisualOfScreen(fScreen);
  fDepth:= DefaultDepthOfScreen(fScreen);
  fRootWindow := RootWindow(fDisplay, DefaultScreen(fDisplay));
  fWindowID  := XCreateSimpleWindow(fDisplay, XRootWindow(fDisplay, 0), 0, 0, 34, 34, 0,
                            fScreen^.black_pixel, fScreen^.white_pixel);
  fDrawable := fWindowID;

  GCVals.background := WhitePixel(fDisplay, DefaultScreen(fDisplay));
  GCVals.foreground := BlackPixel(fDisplay, DefaultScreen(fDisplay));
  XSelectInput(fDisplay, fWindowID, ButtonPressMask or ButtonReleaseMask or PointerMotionMask
    or EnterWindowMask or LeaveWindowMask or VisibilityChangeMask or ExposureMask
    or SubstructureNotifyMask or ResizeRedirectMask);


  gc := XCreateGC(fDisplay, fWindowID, GCForeground or GCBackground, @GCVals);

  buf := 'TEST';
  XChangeProperty(fDisplay, fWindowID, XInternAtom(fDisplay,'_NET_WM_NAME', false), XInternAtom(fDisplay, 'UTF8_STRING', False), 8, PropModeAppend, @buf, 4);
  XChangeProperty(fDisplay, fWindowID, XInternAtom(fDisplay,'_NET_WM_VISIBLE_NAME', false), XInternAtom(fDisplay, 'UTF8_STRING', False), 8, PropModeAppend, @buf, 4);
  buf := 'CONTEXT';
  XChangeProperty(fDisplay, fWindowID,  XInternAtom(fDisplay, '_MB_SYSTEM_TRAY_CONTEXT', False), XInternAtom(fDisplay, 'UTF8_STRING', False), 8, PropModeAppend, @buf, 7);

  XSync (fdisplay, False);
  AttachIcon;
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

procedure TWidgetTrayIcon.InitWM;
var
  // set the class hint
  classhint: TXClassHint;
  hints: PXWMHints;
begin
  classhint.res_name  := pchar('TTrayIcon');
  classhint.res_class := pchar('TTrayIcon');

  XSetClassHint(fDisplay, fWindowID, @classhint);

  // set the Window Manager hints

  hints := XGetWMHints(fDisplay, fWindowID);	// init hints
  if Hints <> nil then begin
    hints^.flags := WindowGroupHint or IconWindowHint or StateHint;	// set the window group hint
    hints^.window_group := fWindowID;		// set the window hint
    hints^.initial_state := NormalState;//WithdrawnState;	// initial state
    hints^.icon_window := fWindowID;		// in WM, this should be winId() of separate widget
    hints^.icon_x := 0;
    hints^.icon_y := 0;
    XSetWMHints(fDisplay, fWindowID, hints);	// set the window hints for WM to use.
    XFree( hints );
  end;
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

function TWidgetTrayIcon.AttachIcon: TWindow;
var
buf: array [0..32] of char;
selection_atom : TAtom;
Manager_Window: TWindow;
old_error: TXErrorHandler;
data: array [0..3] of longint;
begin
  old_error := XSetErrorHandler(@TempX11ErrorHandler);
  initWM;
  //SetMinSize(fIconWidth, fIconHeight);
  fScreenID := XScreenNumberOfScreen(fScreen);
  buf :=  PChar('_NET_SYSTEM_TRAY_S' + IntToStr(fScreenID));

  selection_atom := XInternAtom(fDisplay, buf, false);
  XGrabServer(fDisplay);
  Manager_Window := XGetSelectionOwner(fDisplay, selection_atom);
  if Manager_Window <> None then
  begin
    XSelectInput(fDisplay, Manager_Window, StructureNotifyMask);
    Result := Manager_Window;
    fTrayParent := Result;
  end;
  XUngrabServer(fDisplay);
  XFlush(fDisplay);
  data[0] := 34;
  data[1] := 34;
  data[2] := 34;
  data[3] := 34;

  if ( manager_window <> None ) then
    send_message(Manager_Window, SYSTEM_TRAY_REQUEST_DOCK, fWindowID, 0, 0);
  SetMinSize(Icon.Width,Icon.Height);
  XChangeProperty(fDisplay, fWindowID, XInternAtom( fdisplay, '_NET_WM_ICON_GEOMETRY',False),
          TAtom(6), 32, PropModeReplace, @data, 4);
//   XResizeWindow(fDisplay, fWindowID, 22, 22);
  XSetErrorHandler(old_error);
  //fTimer.Enabled := True;
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

  if fWindowID <> 0 then XDestroyWindow(fDisplay, fWindowID);
  fWindowID := 0;
  XFreeGC(fDisplay, gc);
  XCloseDisplay(fDisplay);
  fDisplay := nil;

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

//  CreateForm(0);

  SetEmbedded;

{  GTK_WIDGET_SET_FLAGS(PGtkWidget(GtkForm.Handle),GTK_VISIBLE);
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
  GtkForm.Hint := Self.Hint;}

  fEmbedded := True;

  vVisible := True;

  Result := True;
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

