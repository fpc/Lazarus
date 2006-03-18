{%MainUnit qtint.pp}
{
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
//---------------------------------------------------------------

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TQtWidgetSet.Create;
begin
  inherited Create;

  QtWidgetSet := Self;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TQtWidgetSet.Destroy;
begin
  QtWidgetSet := nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Creates a new timer and sets the callback event.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateTimer(Interval: integer;
  TimerFunc: TFNTimerProc): integer;
var
  QtTimer: TQtTimer;
begin
  QtTimer := TQtTimer.CreateTimer(Interval, TimerFunc, App);
  
  Result := Integer(QtTimer);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DestroyTimer(TimerHandle: integer): boolean;
begin
  TQtTimer(TimerHandle).Free;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  App := QApplication_Create(@argc, argv);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppRun
  Params:  None
  Returns: Nothing

  Enter the main message loop
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  QApplication_Exec;
end;

procedure TQtWidgetSet.AppWaitMessage;
begin

end;

procedure TQtWidgetSet.AppProcessMessages;
begin

end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppTerminate;
begin
  QApplication_closeAllWindows;
end;

procedure TQtWidgetSet.AppMinimize;
begin

end;

procedure TQtWidgetSet.AppBringToFront;
begin

end;

function TQtWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer
  ): TGraphicsColor;
begin
  Result:=clNone;
end;

procedure TQtWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer;
  AColor: TGraphicsColor);
begin

end;

procedure TQtWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TQtWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

function TQtWidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.CreateComponent
  Params:   sender - object for which to create visual representation
  Returns:  nothing

  Deprecated
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateComponent(Sender : TObject): THandle;
begin
  Result := 0;
end;

//------------------------------------------------------------------------
