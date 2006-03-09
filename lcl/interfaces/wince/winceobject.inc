{%MainUnit winceint.pp}

{******************************************************************************
                                   TGtkWidgetSet
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ TWinCEWidgetSet }

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TWinCEWidgetSet.Create;
begin
  Inherited Create;

  WinCEWidgetSet := Self;
end;

destructor TWinCEWidgetSet.Destroy;
begin
  WinCEWidgetSet := nil;

  inherited Destroy;
end;

procedure TWinCEWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
var
  WindowClass: WndClass;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWidgetSet.AppInit');
  {$endif}

  FillChar(WindowClass, SizeOf(WindowClass), #0);

  with WindowClass do
  begin
    Style := CS_HREDRAW or CS_VREDRAW;
    LPFnWndProc := @WindowProc;
    CbClsExtra := 0;
    CbWndExtra := 0;
    hInstance := System.HInstance;
    hIcon := Windows.LoadIcon(System.hInstance, nil);
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := GetSysColorBrush(Color_BtnFace);
    LPSzMenuName := nil;
    LPSzClassName := @ClsName;
  end;
  if Windows.RegisterClass(@WindowClass) = 0 then WriteLn('Register failed');

  // initialize ScreenInfo
{  Handle := GetDesktopWindow;
  DC := Windows.GetDC(Handle);
  ScreenInfo.PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenInfo.PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);
  ScreenInfo.ColorDepth := GetDeviceCaps(DC, BITSPIXEL);
  ReleaseDC(Handle, DC);}
end;

procedure TWinCEWidgetSet.AppMinimize;
begin

end;

procedure TWinCEWidgetSet.AppBringToFront;
begin

end;

procedure TWinCEWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer;
  AColor: TGraphicsColor);
begin

end;

function TWinCEWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer
  ): TGraphicsColor;
begin
  Result:=clNone;
end;

procedure TWinCEWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TWinCEWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AppProcessMessages
  Params:  None
  Returns: Nothing

  Handle all pending messages
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.AppProcessMessages;
begin

end;

procedure TWinCEWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
var
  AMessage: TMsg;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWidgetSet.AppRun');
  {$endif}

  while Windows.GetMessage(@AMessage, 0, 0, 0) do
  begin
    Windows.TranslateMessage(@AMessage);
    Windows.DispatchMessage(@AMessage);
  end;
end;

procedure TWinCEWidgetSet.AppWaitMessage;
begin

end;

procedure TWinCEWidgetSet.AppTerminate;
begin

end;

function TWinCEWidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin

end;

procedure TWinCEWidgetSet.AttachMenuToWindow(AMenuObject: TComponent);
begin
  inherited AttachMenuToWindow(AMenuObject);
end;

function TWinCEWidgetSet.CreateComponent(Sender: TObject): THandle;
begin

end;

function TWinCEWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc
  ): integer;
begin

end;

function TWinCEWidgetSet.DestroyTimer(TimerHandle: Integer): boolean;
begin

end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

