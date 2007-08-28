{%MainUnit ../lclintf.pp}
{ $Id: lclintf.inc 8032 2005-11-02 12:44:29Z vincents $
******************************************************************************
  All interface communication related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    WidgetSet.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

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
 ******************************************************************************

******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function TWinCEWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
var
  listlen: dword;
  lListIndex: pdword;
begin
  listlen := Length(FWaitHandles);
  if FWaitHandleCount = listlen then
  begin
    inc(listlen, 16);
    SetLength(FWaitHandles, listlen);
    SetLength(FWaitHandlers, listlen);
  end;
  New(lListIndex);
  FWaitHandles[FWaitHandleCount] := AHandle;
  FWaitHandlers[FWaitHandleCount].ListIndex := lListIndex;
  FWaitHandlers[FWaitHandleCount].UserData := AData;
  FWaitHandlers[FWaitHandleCount].OnEvent := AEventHandler;
  lListIndex^ := FWaitHandleCount;
  Inc(FWaitHandleCount);
{$ifdef DEBUG_ASYNCEVENTS}  
  DebugLn('Waiting for handle: ', IntToHex(AHandle, 8));
{$endif}
  Result := lListIndex;
end;

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage:
          ABitmap:
          AMask:
          ASkipMask: Wehn set, no mask is created
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;

  function GetMask(APrec, AShift: Byte): Cardinal;
  begin
    Result := ($FFFFFFFF shr (32-APrec)) shl AShift;
  end;

var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[0..3] of Cardinal; // reserve extra color for colormasks
  end;
  BitsPtr: Pointer;
  DataSize: Integer;
  Align: Byte;
begin

  FillChar(Info, sizeof(Info), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  Info.Header.biWidth := ADesc.Width;
  Info.Header.biHeight := -ADesc.Height;
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := ADesc.BitsPerPixel;
  // TODO: palette support
  Info.Header.biClrUsed := 0;
  Info.Header.biClrImportant := 0;
  Info.Header.biSizeImage := BytesPerLine(Info.Header.biWidth, Info.Header.biBitCount) * ADesc.Height;
  // CE only supports bitfields
  Info.Header.biCompression := BI_BITFIELDS;

  // when 24bpp, CE only supports B8G8R8 encoding
  // TODO: check the description
  Info.Colors[0] := GetMask(ADesc.RedPrec, ADesc.RedShift);
  Info.Colors[1] := GetMask(ADesc.GreenPrec, ADesc.GreenShift);
  Info.Colors[2] := GetMask(ADesc.BluePrec, ADesc.BlueShift);

  DC := Windows.GetDC(0);
  AMask := 0;
  // Use createDIBSection, since only devicedepth bitmaps can be selected into a DC
  // when they are created with createDIBitmap
  //  ABitmap := Windows.CreateDIBitmap(DC, Info.Header, CBM_INIT, ARawImage.Data, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS);
  ABitmap := Windows.CreateDIBSection(DC, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS, BitsPtr, 0, 0);
  //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Image');
  Windows.ReleaseDC(0, DC);

  Result := ABitmap <> 0;
  if not Result then Exit;
  if BitsPtr = nil then Exit;

  // copy the image data
  DataSize := (((ADesc.BitsPerPixel * ADesc.Width) shr 3) + 3) and not 3;
  Align := DataSize and 3;
  if Align <> 0
  then Inc(DataSize, 4 - Align);
  DataSize := DataSize * ADesc.Height;
  if DataSize > ARawImage.DataSize
  then DataSize := ARawImage.DataSize;
  Move(ARawImage.Data^, BitsPtr^, DataSize);

  if ASkipMask then Exit;
  if ARawImage.Mask = nil then Exit;
  if ARawImage.MaskSize = 0 then Exit;

  AMask := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Mask);
  Result := AMask <> 0;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: ABitmap:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
var
  BitmapInfo: Windows.TBitmap;
begin
  Result := Windows.GetObject(ABitmap, SizeOf(BitmapInfo), @BitmapInfo) > 0;
  if Result then
    FillRawImageDescription(BitmapInfo, ADesc);
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromDevice
  Params: ADC:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
var
  DC: HDC;
begin
  Result := True;

  ADesc.Init;

  if ADC = 0
  then DC := Windows.GetDC(0)
  else DC := ADC;

  ADesc.Format := ricfRGBA;
  ADesc.Width := Windows.GetDeviceCaps(DC, HORZRES);
  ADesc.Height := Windows.GetDeviceCaps(DC, VERTRES);
  ADesc.Depth := Windows.GetDeviceCaps(DC, BITSPIXEL) * Windows.GetDeviceCaps(DC, PLANES);
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.LineEnd := rileDWordBoundary;

  if (Windows.GetDeviceCaps(DC, RASTERCAPS) and RC_PALETTE) <> 0
  then begin
    // has palette
    ADesc.PaletteColorCount := Windows.GetDeviceCaps(DC, SIZEPALETTE);
    ADesc.BitsPerPixel := Windows.GetDeviceCaps(DC, COLORRES);
  end
  else begin
    ADesc.BitsPerPixel := ADesc.Depth;
  end;

  if ADC = 0
  then Windows.ReleaseDC(0, DC);

  FillRawImageDescriptionColors(ADesc);

  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary;
  ADesc.MaskBitOrder := riboReversedBits;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromBitmap
  Params: ABitmap:
          AMask:
          ARect:
          ARawImage:
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; const ARect: TRect): Boolean;
var
  WinBmp: Windows.TBitmap;
  R: TRect;
begin
  ARawImage.Init;
  Result := Windows.GetObject(ABitmap, SizeOf(WinBmp), @WinBmp) > 0;
  if not Result then Exit;

  FillRawImageDescription(WinBmp, ARawImage.Description);
  R := ARect;
  if R.Top > WinBmp.bmHeight then
    R.Top := WinBmp.bmHeight;
  if R.Bottom > WinBmp.bmHeight then
    R.Bottom := WinBmp.bmHeight;
  if R.Left > WinBmp.bmWidth then
    R.Left := WinBmp.bmWidth;
  if R.Right > WinBmp.bmWidth then
    R.Right := WinBmp.bmWidth;

  ARawImage.Description.Width := R.Right - R.Left;
  ARawImage.Description.Height := R.Bottom - R.Top;

  // copy bitmap
  Result := GetBitmapBytes(ABitmap, R, ARawImage.Description.LineEnd, ARawImage.Data, ARawImage.DataSize);

  // check mask
  if AMask <> 0 then
  begin
    Result := Windows.GetObject(AMask, SizeOf(WinBmp), @WinBmp) > 0;
    if not Result then exit;

    Result := GetBitmapBytes(AMask, R, ARawImage.Description.MaskLineEnd, ARawImage.Mask, ARawImage.MaskSize);
  end
  else begin
    ARawImage.Description.MaskBitsPerPixel := 0;;
  end;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromDevice
  Params: ADC:
          ARect:
          ARawImage:
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
var
  bmp: HBITMAP;
begin
  bmp := Windows.GetCurrentObject(ADC, OBJ_BITMAP);
  if bmp = 0
  then Result := False
  else Result := RawImage_FromBitmap(ARawImage, bmp, 0, ARect);
end;

procedure TWinCEWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
var
  lListIndex: pdword absolute AHandler;
  I: dword;
begin
  if AHandler = nil then exit;
{$ifdef DEBUG_ASYNCEVENTS}
  DebugLn('Removing handle: ', IntToHex(FWaitHandles[lListIndex^], 8));
  if Length(FWaitHandles) > 0 then
    DebugLn(' WaitHandleCount=', IntToStr(FWaitHandleCount), ', WaitHandle[0]=', IntToHex(FWaitHandles[0], 8));
{$endif}    
  // swap with last one
  if FWaitHandleCount >= 2 then
  begin
    I := lListIndex^;
    FWaitHandles[I] := FWaitHandles[FWaitHandleCount-1];
    FWaitHandlers[I] := FWaitHandlers[FWaitHandleCount-1];
    FWaitHandlers[I].ListIndex^ := I;
  end;
  Dec(FWaitHandleCount);
  Dispose(lListIndex);
  AHandler := nil;
end;

function TWinCEWidgetSet.AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
var
  lHandler: PPipeEventInfo;
begin
  if AEventHandler = nil then exit;
  New(lHandler);
  lHandler^.Handle := AHandle;
  lHandler^.UserData := AData;
  lHandler^.OnEvent := AEventHandler;
  lHandler^.Prev := nil;
  lHandler^.Next := FWaitPipeHandlers;
  if FWaitPipeHandlers <> nil then
    FWaitPipeHandlers^.Prev := lHandler;
  FWaitPipeHandlers := lHandler;
  Result := lHandler;
end;

procedure TWinCEWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
var
  lHandler: PPipeEventInfo absolute AHandler;
begin
  if AHandler = nil then exit;
  if lHandler^.Prev <> nil then
    lHandler^.Prev^.Next := lHandler^.Next
  else
    FWaitPipeHandlers := lHandler^.Next;
  if lHandler^.Next <> nil then
    lHandler^.Next^.Prev := lHandler^.Prev;
  Dispose(lHandler);
  AHandler := nil;
end;

function TWinCEWidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
var
  lProcessEvent: PProcessEvent;
begin
  if AEventHandler = nil then exit;
  New(lProcessEvent);
  lProcessEvent^.Handle := AHandle;
  lProcessEvent^.UserData := AData;
  lProcessEvent^.OnEvent := AEventHandler;
  lProcessEvent^.Handler := AddEventHandler(AHandle, 0, 
    @HandleProcessEvent, PtrInt(lProcessEvent));
  Result := lProcessEvent;
end;

procedure TWinCEWidgetSet.HandleProcessEvent(AData: PtrInt; AFlags: dword);
var
  lProcessEvent: PProcessEvent absolute AData;
  exitcode: dword;
begin
  if not Windows.GetExitCodeProcess(lProcessEvent^.Handle, exitcode) then
    exitcode := 0;
  lProcessEvent^.OnEvent(lProcessEvent^.UserData, cerExit, exitcode);
end;

procedure TWinCEWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lProcessEvent: PProcessEvent absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lProcessEvent^.Handler);
  Dispose(lProcessEvent);
  AHandler := nil;
end;

{------------------------------------------------------------------------------
  Function:
  Params:

  Returns:

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  case ACursor of
    crSqlWait..crDrag,
    crHandPoint: 
      begin
        // will be created later by CreateCursor
      end;
  else
    Result := Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]);
  end;
end;

procedure TWinCEWidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
const
    { up, down, left, right }
  ArrowTypeToState: array[TArrowType] of dword = (DFCS_SCROLLUP, DFCS_SCROLLDOWN,
    DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
var
  drawRect: Windows.RECT;
  canvasHandle: HDC;
begin
  drawRect := TControl(Arrow).ClientRect;
  canvasHandle := TCanvas(Canvas).Handle;
  Windows.FillRect(canvasHandle, drawRect, GetSysColorBrush(COLOR_BTNFACE));
  dec(drawRect.Left, 2);
  dec(drawRect.Top, 2);
  inc(drawRect.Right, 2);
  inc(drawRect.Bottom, 2);
  Windows.DrawFrameControl(TCanvas(Canvas).Handle, @drawRect,
	DFC_SCROLL, ArrowTypeToState[TArrow(Arrow).ArrowType]);
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  //TODO: Implement
  Result := '';
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints;
  SizeRect: TRect;
  Height, Width: Integer;
  FixedHeight, FixedWidth: boolean;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    FixedHeight := false;
    FixedWidth := false;
    {if SizeConstraints.Control is TCustomCalendar then
    begin
      FixedHeight := true;
      FixedWidth := true;
    end;}
    
    if (FixedHeight or FixedWidth)
      and TWinControl(SizeConstraints.Control).HandleAllocated then 
    begin
      Windows.GetWindowRect(TWinControl(SizeConstraints.Control).Handle, @SizeRect);
      if FixedHeight then
        Height := SizeRect.Bottom - SizeRect.Top
      else
        Height := 0;
      if FixedWidth then
        Width := SizeRect.Right - SizeRect.Left
      else
        Width := 0;
      SizeConstraints.SetInterfaceConstraints(Width, Height, Width, Height);
    end;
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
