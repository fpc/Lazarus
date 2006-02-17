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

end;

destructor TWinCEWidgetSet.Destroy;
begin

  inherited Destroy;
end;

procedure TWinCEWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin

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
var
  AMessage: TMsg;
  AccelTable: HACCEL;
  retVal, index: dword;
Begin
  while Windows.PeekMessage(AMessage, HWnd(Nil), 0, 0,PM_REMOVE) do
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

