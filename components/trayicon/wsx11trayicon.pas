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
      fDepth, fWidth, fHeight: Integer;
      fRootWindow: TWindow;
      gc: Xlib.TGC;
      fImage: xlib.PXImage;

      fTimer: TTimer;

      procedure SetEmbedded;
      procedure InitWM;
      procedure SetMinSize(AWidth, AHeight: Integer);
      function Send_Message(window: TWindow; msg: Integer;data1, data2,data3: Integer): boolean;
      function AttachIcon: TWindow;
      function GetCanvas: TCanvas;
      procedure OnEventTimer(Sender: TObject);
      procedure RePaint;
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

uses WSTrayIcon;

// Temp ErrorHandler
function TempX11ErrorHandler(Display:PDisplay; ErrorEv:PXErrorEvent): longint; cdecl;
begin
  WriteLn('Error: ' + IntToStr(ErrorEv^.error_code));
  Result := 0;
end;

// Processes X11 events
function ProcessEvent(display:PDisplay; event:PXEvent; p : TXPointer): TBool; cdecl;
begin
  Result := True;
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

{*******************************************************************
*  TWidgetTrayIcon.InitWM ()
*
*  DESCRIPTION:    Initializes the Window Manager hints
*
*  PARAMETERS:
*
*  RETURNS:        Nothing
*
*******************************************************************}
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

{*******************************************************************
*  TWidgetTrayIcon.AttachIcon ()
*
*  DESCRIPTION:    Attachs a icon to the Tray
*
*  PARAMETERS:
*
*  RETURNS:        Nothing
*
*******************************************************************}
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
  SetMinSize(Icon.Width, Icon.Height);
  XChangeProperty(fDisplay, fWindowID, XInternAtom( fdisplay, '_NET_WM_ICON_GEOMETRY',False),
          TAtom(6), 32, PropModeReplace, @data, 4);
//  XResizeWindow(fDisplay, fWindowID, 22, 22);
  XSetErrorHandler(old_error);
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
*  TWidgetTrayIcon.OnEventTimer ()
*
*  DESCRIPTION:    Processes X messages
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.OnEventTimer(Sender: TObject);
var
  ev: TXEvent;
  sShift: TShiftState;
  Btn: TMouseButton;
  BtnPressEv: PXButtonPressedEvent;
  BtnReleaseEv : PXButtonReleasedEvent;
  MouseMotionEv: PXMotionEvent;
  ResizeEv : PXResizeRequestEvent;
  ClientEv: PXClientMessageEvent;
begin
  if (fDisplay = nil) then Exit;

  while XCheckIfEvent(fDisplay, @ev, @ProcessEvent, nil) do
  begin
    sShift := [];
    
    case ev._type of
      ButtonRelease:
      begin
        BtnReleaseEv := PXButtonReleasedEvent(@ev);
        case BtnReleaseEv^.button of
         1:
         begin
           if Assigned(OnClick) then OnClick(Self);
           if Assigned(OnMouseUp) then
            OnMouseUp(Self, mbLeft, [], Round(BtnReleaseEv^.X), Round(BtnReleaseEv^.Y));
         end;

         2: if Assigned(OnMouseUp) then
             OnMouseUp(Self, mbMiddle, [], Round(BtnReleaseEv^.X), Round(BtnReleaseEv^.Y));

         3:
         begin
           if Assigned(OnMouseUp) then
            OnMouseUp(Self, mbRight, [], Round(BtnReleaseEv^.X), Round(BtnReleaseEv^.Y));
           if Assigned(PopUpMenu) then
            PopUpMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
         end;
        end;
      end;
      ButtonPress:
      begin
        BtnPressEv := PXButtonPressedEvent(@ev);
        case BtnPressEv^.button of
          1: if Assigned(OnMouseUp) then
             OnMouseDown(Self, mbLeft, [], Round(BtnPressEv^.X), Round(BtnPressEv^.Y));

          2: if Assigned(OnMouseUp) then
             OnMouseDown(Self, mbMiddle, [], Round(BtnPressEv^.X), Round(BtnPressEv^.Y));

          3:
          begin
            if Assigned(OnMouseUp) then
             OnMouseDown(Self, mbRight, [], Round(BtnPressEv^.X), Round(BtnPressEv^.Y));
            if Assigned(PopUpMenu) then
             PopUpMenu.PopUp(BtnPressEv^.x_root, BtnPressEv^.y_root);
          end;
        end;
      end;
      Expose, GraphicsExpose, VisibilityNotify, VisibilityUnobscured, VisibilityPartiallyObscured:
      begin
        Repaint;
      end;
{      EnterNotify:
      begin
        if Assigned(MouseEnter) then MouseEnter(Self);
      end;
      LeaveNotify:
      begin
        if Assigned(MouseLeave) then MouseLeave(Self);
      end;}
      MotionNotify:
      begin
        MouseMotionEv := PXMotionEvent(@ev);
        
        if Button1Mask in [MouseMotionEv^.state] then sShift += [ssLeft];
        if Button2Mask in [MouseMotionEv^.state] then sShift += [ssMiddle];
        if Button3Mask in [MouseMotionEv^.state] then sShift += [ssRight];

        if Assigned(OnMouseMove) then
          OnMouseMove(Self, sShift, Round(MouseMotionEv^.X), Round(MouseMotionEv^.Y));
      end;
      ResizeRequest:
      begin
        ResizeEv := PXResizeRequestEvent(@ev);
        fWidth := ResizeEv^.width;
        fHeight := ResizeEv^.height;

        if fImage <> nil then
        begin
          XClearWindow(fDisplay,fWindowID);
          XFree(fImage);
          fImage := nil;
        end;
        
        if vVisible then Repaint;
      end;
      CLientMessage:
      begin
        ClientEv := PXClientMessageEvent(@Ev);
      end;
    else
//      Writeln('Unprocessed X11 event for the tray icon: ', ev._type);
    end;
  end;
end;

{*******************************************************************
*  TWidgetTrayIcon.RePaint ()
*
*  DESCRIPTION:    Paints the icon
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TWidgetTrayIcon.RePaint;

  function SwapColor(Color: TColor): TColor;
  var
    fcolor: Array [0..3] of byte;
    tmp: byte;
  begin
    move(color, fcolor, sizeof(fcolor));
    tmp := fcolor[0];
    fcolor[0] := fcolor[2];
    fcolor[2] := tmp;
    result := TColor(fColor);
  end;

var
  bitmap_pad: integer;
  Pixel: TColor;
  x,y: Integer;
  fTop, fLeft: Integer;
begin
  if (fImage = nil) then
  begin
    if fDepth > 16 then bitmap_pad := 32
    else if fDepth > 8 then bitmap_pad := 16
    else bitmap_pad := 8;
    fImage := XCreateImage(fDisplay, fVisual, fDepth, ZPixmap, 0, nil,
                                    34, 34, bitmap_pad, 0);
    fImage^.data := AllocMem(fImage^.bytes_per_line * fHeight * 4);
    fleft := 0;
    ftop := 0;
    if fWidth > Icon.Width then fLeft := (fWidth - Icon.Width) div 2;
    if fHeight > Icon.Height then fTop := (fHeight- Icon.Height) div 2;
    for Y := 0 to fHeight do
    begin
      for X := 0 to fwidth do
        begin
         // Causes an error in gdk_colormap_get_visual
         if (y-ftop > Icon.Height)
         or (x-fleft > Icon.Width)
         or (X < fLeft) or (X > fLeft + fWidth)
         or (Y < fTop) or (Y > fTop + fHeight)
         then
           pixel := SwapColor(Icon.TransparentColor)
         else
            pixel := SwapColor(Icon.Canvas.Pixels[x-fLeft, y-fTop]);

         XPutPixel(fImage, X,  Y, (pixel));
       end;
    end;
  end;
  XPutImage(fDisplay, fDrawable, gc, fImage, 0, 0, 0, 0, fWidth, fHeight);
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

  fTimer.OnTimer := nil;
  fTimer.Enabled := False;
  fTimer.Free;

  XFree(fImage);
  fImage := nil;

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
  
  { Timer to process messages }

  fTimer := TTimer.Create(fOwner);
  fTimer.Interval := 10;
  fTimer.OnTimer := @OnEventTimer;
  fTimer.Enabled := True;

  { Painting code }

  fWidth := 24;
  fHeight := 24;
  fImage := nil;

  { Creates the tray window }

  SetEmbedded;
  
  { needed for gnome }
  
//  SetMinSize(22, 22);

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

