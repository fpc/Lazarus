{%MainUnit fpguiint.pp}
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
  Method: TFpGuiWidgetSet.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TFpGuiWidgetSet.Create;
begin
  inherited Create;

  FpGuiWidgetSet := Self;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TFpGuiWidgetSet.Destroy;
begin
  FpGuiWidgetSet := nil;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.CreateTimer
  Params:  None
  Returns: Nothing

  Creates a new timer and sets the callback event.
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.CreateTimer(Interval: integer; TimerFunc: TFNTimerProc): THandle;
begin
  Result := PtrInt(0);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.DestroyTimer
  Params:  None
  Returns: Nothing

  Destroys a timer.
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppInit
  Params:  None
  Returns: Nothing

  Initializes the application
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
begin
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppRun
  Params:  None
  Returns: Nothing

  Enter the main message loop
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppRun(const ALoop: TApplicationMainLoop);
begin
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppWaitMessage
  Params:  None
  Returns: Nothing

  Wait till an OS application message is received
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppWaitMessage;
begin
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppProcessMessage
  Params:  None
  Returns: Nothing

  Handle the messages in the queue
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppProcessMessages;
begin
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppTerminate
  Params:  None
  Returns: Nothing

  Implements Application.Terminate and MainForm.Close.
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppTerminate;
begin
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppMinimize
  Params:  None
  Returns: Nothing

  Minimizes the application window.
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppMinimize;
begin
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWidgetSet.AppBringToFront
  Params:  None
  Returns: Nothing

  Brings the application window to the front
 ------------------------------------------------------------------------------}
procedure TFpGuiWidgetSet.AppBringToFront;
begin

end;

function TFpGuiWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
begin
  Result:=clNone;
end;

procedure TFpGuiWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
begin

end;

procedure TFpGuiWidgetSet.DCRedraw(CanvasHandle: HDC);
begin

end;

procedure TFpGuiWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

function TFpGuiWidgetSet.InitHintFont(HintFont: TObject): Boolean;
begin
  Result:=false;
end;

{------------------------------------------------------------------------------
  Function: TFpGuiWidgetSet.CreateComponent
  Params:   sender - object for which to create visual representation
  Returns:  nothing

  Deprecated, never call this function
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.CreateComponent(Sender : TObject): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TFpGuiWidgetSet.IsValidDC
  Params:   DC     -  handle to a device context (TFpGuiDeviceContext)
  Returns:  True   -  if the DC is valid
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.IsValidDC(const DC: HDC): Boolean;
begin
  Result := (DC <> 0);
end;

{------------------------------------------------------------------------------
  Function: TFpGuiWidgetSet.IsValidGDIObject
  Params:   GDIObject  -  handle to a GDI Object (TFpGuiFont, TFpGuiBrush, etc)
  Returns:  True       -  if the DC is valid
  
  Remark: All handles for GDI objects must be pascal objects so we can
 distinguish between them
 ------------------------------------------------------------------------------}
function TFpGuiWidgetSet.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
begin
  Result := False;
  
  if GDIObject = 0 then Exit;
  
  aObject := TObject(GDIObject);

  if aObject is TObject then
  begin
//    Result := (aObject is TFpGuiFont) or (aObject is TFpGuiBrush) or (aObject is TFpGuiImage);
  end;
end;

//------------------------------------------------------------------------
