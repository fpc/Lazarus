{%MainUnit customdrawnint.pas}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
//---------------------------------------------------------------

procedure TCDWidgetSet.AccumulatingDebugOut(AStr: string);
begin
  AccumulatedStr := AccumulatedStr + AStr;
end;

procedure TCDWidgetSet.CDSetFocusToControl(ALCLControl, AIntfControl: TWinControl);
begin
  if (CDWidgetset.FocusedControl <> ALCLControl) then
  begin
    // First kill focus in the previous control
    if CDWidgetset.FocusedControl <> nil then
      LCLSendKillFocusMsg(CDWidgetset.FocusedControl);
    CDWidgetset.FocusedControl := ALCLControl;
    LCLSendSetFocusMsg(ALCLControl);

    // The same for intf controls
    if CDWidgetset.FocusedIntfControl <> nil then
      LCLSendKillFocusMsg(CDWidgetset.FocusedIntfControl);
    CDWidgetset.FocusedIntfControl := AIntfControl;
    if AIntfControl <> nil then LCLSendSetFocusMsg(AIntfControl);
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCDWidgetSet.Create;
begin
  inherited Create;

  CDWidgetSet := Self;
  FTerminating := False;

  {$ifndef CD_UseNativeText}
  // Create the dummy screen DC
  ScreenBitmapRawImage.Init;
  ScreenBitmapHeight := 100;
  ScreenBitmapWidth := 100;
  ScreenBitmapRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(ScreenBitmapWidth, ScreenBitmapHeight);
  ScreenBitmapRawImage.CreateData(True);
  ScreenImage := TLazIntfImage.Create(0, 0);
  ScreenImage.SetRawImage(ScreenBitmapRawImage);
  ScreenDC := TLazCanvas.Create(ScreenImage);
  {$endif}

  BackendCreate;
end;

{------------------------------------------------------------------------------
  Method: TQtWidgetSet.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCDWidgetSet.Destroy;
begin
  BackendDestroy;

  {$ifndef CD_UseNativeText}
  // Free the dummy screen DC
  ScreenImage.Free;
  ScreenDC.Free;
  {$endif}

  CDWidgetSet := nil;
  inherited Destroy;
end;

function TCDWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:= lpCustomDrawn;
end;

function TCDWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
    {$ifdef CD_Cocoa}
    lcNeedMininimizeAppWithMainForm,
    lcApplicationTitle,
    lcFormIcon:
      Result := LCL_CAPABILITY_NO;
    {$endif}
    lcReceivesLMClearCutCopyPasteReliably: Result := LCL_CAPABILITY_NO;
    lcAntialiasingEnabledByDefault: Result := LCL_CAPABILITY_NO;
    lcAllowChildControlsInNativeControls: Result := LCL_CAPABILITY_YES
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;

function TCDWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  LazCanvas: TLazCanvas;
begin
  Result := clNone;

  if not IsValidDC(CanvasHandle) then Exit;
  LazCanvas := TLazCanvas(CanvasHandle);
  
  Result := FPColorToTColor(LazCanvas.Colors[X, Y]);
end;

procedure TCDWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  LazCanvas: TLazCanvas;
begin
  if not IsValidDC(CanvasHandle) then Exit;
  LazCanvas := TLazCanvas(CanvasHandle);

  LazCanvas.Colors[X, Y] := TColorToFPColor(AColor);
end;

procedure TCDWidgetSet.DCRedraw(CanvasHandle: HDC);
begin
  // TODO: implement me
end;

procedure TCDWidgetSet.DCSetAntialiasing(CanvasHandle: HDC; AEnabled: Boolean);
{var
  DC: TQtDeviceContext;}
begin
{  if IsValidDC(CanvasHandle) then
  begin
    if CanvasHandle = 1 then
      DC := QtDefaultContext
    else
      DC := TQtDeviceContext(CanvasHandle);
    DC.setRenderHint(QPainterAntialiasing, AEnabled);
  end;}
end;

procedure TCDWidgetSet.SetDesigning(AComponent: TComponent);
begin

end;

function CDMessageBoxFunction(Text, Caption : PChar; Flags : Longint) : Integer;
begin
  Result := CDWidgetset.MessageBox(0, Text, Caption, Flags);
end;

//------------------------------------------------------------------------
