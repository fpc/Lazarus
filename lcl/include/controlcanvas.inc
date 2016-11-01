{%MainUnit ../controls.pp}
{******************************************************************************
                                     TControlCanvas
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method:  TControlCanvas.SetControl
  Params:  AControl: The control this canvas belongs to
  Returns: Nothing

  Sets the owner of this canvas
 ------------------------------------------------------------------------------}
procedure TControlCanvas.SetControl(AControl: TControl);
begin
  if FControl <> AControl then
  begin
    FreeHandle;
    FControl := AControl;
  end;
end;

function TControlCanvas.GetDefaultColor(const ADefaultColorType: TDefaultColorType): TColor;
begin
  if Assigned(FControl) then
    Result := FControl.GetDefaultColor(ADefaultColorType)
  else
    Result := inherited GetDefaultColor(ADefaultColorType);
end;

{------------------------------------------------------------------------------
  Method: TControlCanvas.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TControlCanvas.Create;
begin
  inherited Create;
  FDeviceContext := 0;
  FControl := nil;
  FWindowHandle := 0;
end;

{------------------------------------------------------------------------------
  Method: TControlCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TControlCanvas.Destroy;
begin
  FreeHandle;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TControlCanvas.CreateHandle;
var
  WinControl: TWinControl;
begin
  //DebugLn('[TControlCanvas.CreateHandle] ',FControl<>nil,' DC=',DbgS(FDeviceContext,8),' WinHandle=',DbgS(FWindowHandle,8));
  if FControl = nil then
    inherited CreateHandle
  else
  begin
    {$IFDEF VerboseCanvas}
    if not ControlIsPainting and
      (WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_NO) then
      debugln(['TControlCanvas.CreateHandle WARNING: accessing the canvas of '+DbgSName(FControl)+' is not supported outside of paint message']);
    {$ENDIF}
    if (FDeviceContext = 0) then
    begin
      // access to window handle can cause another TControlCanvas.CreateHandle
      // as result we get a resource leak. To prevent this require handle before
      // accessing it
      if FControl is TWinControl then
        WinControl := TWinControl(FControl)
      else
        WinControl := FControl.Parent;
      WinControl.HandleNeeded;

      // important: keep the condition here again because
      // CreateWnd (called via WinControl.HandleNeeded above) could execute
      // this function again and thus allocate FDeviceContext twice
      // resulting in a memory leak
      if FDeviceContext = 0 then
      begin
        // store the handle locally since  we need it to check (and do not
        // want to fire creation events)
        FDeviceContext := FControl.GetDeviceContext(FWindowHandle);
      end;
    end;
    Handle := FDeviceContext;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TControlCanvas.FreeHandle
  Params:  None
  Returns: Nothing

  Frees the handle
 ------------------------------------------------------------------------------}
procedure TControlCanvas.FreeHandle;
begin
  inherited;
  if FDeviceContext <> 0 then
  begin
    ReleaseDC(FWindowHandle, FDeviceContext);
    FDeviceContext := 0;
  end;
end;

function TControlCanvas.ControlIsPainting: boolean;
begin
  Result := Assigned(FControl) and FControl.IsProcessingPaintMsg;
end;
