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

function TQtWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc
  ): integer;
begin
  Result:=0;
end;

function TQtWidgetSet.DestroyTimer(TimerHandle: integer): boolean;
begin
  Result:=false;
end;

procedure TQtWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
  App := QApplication_Create(@argc, argv);
//  InitializeEngine;  ??
end;

procedure TQtWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
  QApplication_Exec;
end;

procedure TQtWidgetSet.AppWaitMessage;
begin

end;

procedure TQtWidgetSet.AppProcessMessages;
begin
//  MainLoop;
end;

procedure TQtWidgetSet.AppTerminate;
begin
  Assert(False, 'Trace:*******APP TERMINATE CALLED*****');
//  Shutdown;
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
