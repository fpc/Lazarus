{%MainUnit qtint.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
function TQtWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): THandle;
var
  QtTimer: TQtTimer;
begin
  QtTimer := TQtTimer.CreateTimer(Interval, TimerFunc, App);
  
  Result := PtrInt(QtTimer);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
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

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Waits until a message arrives, processes that and returns control out of the function
  
  Utilized on Modal dialogs
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppWaitMessage;
begin
  QCoreApplication_processEvents(QEventLoopWaitForMoreEvents);
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Processes all messages on the quoue
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppProcessMessages;
begin
  QCoreApplication_processEvents();
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.AppTerminate;
begin
  QCoreApplication_quit;
end;

procedure TQtWidgetSet.AppMinimize;
begin

end;

procedure TQtWidgetSet.AppBringToFront;
begin

end;

function TQtWidgetSet.WidgetSetName: string;
begin
  Result:='qt';
end;

function TQtWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
{var
  Color: QColorH;}
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  
  if (TQtDeviceContext(CanvasHandle).vImage <> nil) then
  begin
    Result := TColor(QImage_pixel(TQtDeviceContext(CanvasHandle).vImage, X, Y));

{    Color := QColor_create(QImage_pixel(TQtDeviceContext(CanvasHandle).vImage, X, Y));

    Result := RGBToColor(QColor_red(Color), QColor_green(Color), QColor_blue(Color));

    QColor_destroy(Color);}
  end;
end;

procedure TQtWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
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

  Deprecated, never call this function
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateComponent(Sender : TObject): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.IsValidDC
  Params:   DC     -  handle to a device context (TQtDeviceContext)
  Returns:  True   -  if the DC is valid
 ------------------------------------------------------------------------------}
function TQtWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := (DC <> 0);
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.IsValidGDIObject
  Params:   GDIObject  -  handle to a GDI Object (TQtFont, TQtBrush, etc)
  Returns:  True       -  if the DC is valid
  
  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TQtWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
begin
  Result := False;
  
  if GDIObject = 0 then Exit;
  
  aObject := TObject(GDIObject);

  if aObject is TObject then
  begin
    Result := (aObject is TQtFont) or (aObject is TQtBrush) or (aObject is TQtImage);
  end;
end;

//------------------------------------------------------------------------
