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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
          ASkipMask: When set, no mask is created
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
  DC: HDC;
  BitsPtr: Pointer;
  DataSize: PtrUInt;
begin
  Result := False;
  AMask := 0;

  if not ((ADesc.BitsPerPixel = 1) and (ADesc.LineEnd = rileWordBoundary)) then
  begin
    DC := Windows.GetDC(0);
    AMask := 0;
    ABitmap := CreateDIBSectionFromDescription(DC, ADesc, BitsPtr);
    //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Image');
    Windows.ReleaseDC(0, DC);

    Result := ABitmap <> 0;
    if not Result then Exit;
    if BitsPtr = nil then Exit;

    // copy the image data
    DataSize := BytesPerLine(ADesc.Width, ADesc.BitsPerPixel) * ADesc.Height;
    if DataSize > ARawImage.DataSize
    then DataSize := ARawImage.DataSize;
    Move(ARawImage.Data^, BitsPtr^, DataSize);
  end
  else
    ABitmap := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Data);

  if ASkipMask then Exit(True);

  AMask := Windows.CreateBitmap(ADesc.Width, ADesc.Height, 1, 1, ARawImage.Mask);
  //DbgDumpBitmap(ABitmap, 'CreateBitmaps - Mask');
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
function TWinceWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  WinBmp: Windows.TBitmap;
  R: TRect;
begin
  ARawImage.Init;
  Result := Windows.GetObject(ABitmap, SizeOf(WinBmp), @WinBmp) > 0;
  if not Result then Exit;

  FillRawImageDescription(WinBmp, ARawImage.Description);

  if ARect = nil
  then begin
    R := Rect(0, 0, WinBmp.bmWidth, WinBmp.bmHeight);
  end
  else begin
    R := ARect^;
    if R.Top > WinBmp.bmHeight then
      R.Top := WinBmp.bmHeight;
    if R.Bottom > WinBmp.bmHeight then
      R.Bottom := WinBmp.bmHeight;
    if R.Left > WinBmp.bmWidth then
      R.Left := WinBmp.bmWidth;
    if R.Right > WinBmp.bmWidth then
      R.Right := WinBmp.bmWidth;
  end;

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
    //DebugLn(Format('AMask = %d, MaskSize = %d, Mask = %d, Result = %s', [AMask, ARawImage.MaskSize, PtrUInt(ARawImage.Mask), BoolToStr(Result)]));
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
const
  FILL_PIXEL: array[0..3] of Byte = ($00, $00, $00, $FF);
var
  BitsPtr: Pointer;
  Desc: TRawImageDescription;
  copyDC, fillDC: HDC;
  bmp, copyOld, fillOld, copyBmp, fillBmp: HBITMAP;
  DeviceSize: TPoint;
begin
  if Windows.GetObjectType(ADC) = OBJ_MEMDC
  then begin
    // we can use bitmap directly
    bmp := Windows.GetCurrentObject(ADC, OBJ_BITMAP);
    copyBmp := 0;
  end
  else begin
    // we need to copy the image
    // use a dibsection, so we can easily retrieve the bytes
    copyDC := Windows.CreateCompatibleDC(ADC);

    Desc.Init;
    GetDeviceSize(ADC, DeviceSize);
    Desc.Width := DeviceSize.X;
    Desc.Height := DeviceSize.Y;
    Desc.BitsPerPixel := Windows.GetDeviceCaps(ADC, BITSPIXEL);

    FillRawImageDescriptionColors(Desc);
    copyBmp := CreateDIBSectionFromDescription(copyDC, Desc, BitsPtr);
    copyOld := Windows.SelectObject(copyDC, copyBmp);

    // prefill bitmap, to create an alpha channel in case of 32bpp bitmap
    if Desc.BitsPerPixel > 24
    then begin
      // using a stretchblt is faster than filling the memory ourselves,
      // which is in its turn faster than using a 24bpp bitmap
      fillBmp := Windows.CreateBitmap(1, 1, 1, 32, @FILL_PIXEL);
      fillDC := Windows.CreateCompatibleDC(ADC);
      fillOld := Windows.SelectObject(fillDC, fillBmp);

      Windows.StretchBlt(copyDC, 0, 0, Desc.Width, Desc.Height, fillDC, 0, 0, 1, 1, SRCCOPY);

      Windows.SelectObject(fillDC, fillOld);
      Windows.DeleteDC(fillDC);
      Windows.DeleteObject(fillBmp);

      Windows.BitBlt(copyDC, 0, 0, Desc.Width, Desc.Height, ADC, 0, 0, SRCPAINT);
    end
    else begin
      // copy image
      Windows.BitBlt(copyDC, 0, 0, Desc.Width, Desc.Height, ADC, 0, 0, SRCCOPY);
    end;

    Windows.SelectObject(copyDC, copyOld);
    Windows.DeleteDC(copyDC);

    bmp := copyBmp;
  end;

  if bmp = 0 then Exit(False);

  Result := RawImage_FromBitmap(ARawImage, bmp, 0, @ARect);
  if copyBmp <> 0
  then Windows.DeleteObject(copyBmp);
end;

{------------------------------------------------------------------------------
  Function: RawImage_QueryDescription
  Params: AFlags:
          ADesc:
  Returns:

 ------------------------------------------------------------------------------}
function TWinceWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  if riqfAlpha in AFlags
  then begin
    //always return rgba description
    if not (riqfUpdate in AFlags)
    then ADesc.Init;

    ADesc.Format := ricfRGBA;
    ADesc.Depth := 32;
    ADesc.BitOrder := riboReversedBits;
    ADesc.ByteOrder := riboLSBFirst;
    ADesc.LineOrder := riloTopToBottom;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.BitsPerPixel := 32;

    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    if riqfRGB in AFlags
    then begin
      ADesc.RedPrec := 8;
      ADesc.GreenPrec := 8;
      ADesc.BluePrec := 8;
      ADesc.RedShift := 16;
      ADesc.GreenShift := 8;
      ADesc.BlueShift := 0;
    end;
    
    AFlags := AFlags - [riqfRGB, riqfAlpha, riqfUpdate];
    if AFlags = [] then Exit(True);
    
    // continue with default
    Include(AFlags, riqfUpdate);
  end;

  Result := inherited RawImage_QueryDescription(AFlags, ADesc);
  // reduce mem
  if Result and (ADesc.Depth = 24) 
  then ADesc.BitsPerPixel := 24;
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
        // will be created later by CreateIconIndirect
      end;
  else
    Result := Windows.LoadCursorW(0, LclCursorToWin32CursorMap[ACursor]);
  end;
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
  SizeConstraints: TSizeConstraints absolute Constraints;
  SizeRect: TRect;
  Height, Width: Integer;
  FixedHeight, FixedWidth: boolean;
begin
  Result := True;
  if Constraints is TSizeConstraints then
  begin
    if (SizeConstraints.Control=nil) then Exit;

    FixedHeight := false;
    FixedWidth := false;

    if SizeConstraints.Control is TCustomComboBox then
    begin
      // wince combo (but not csSimple) has fixed height
      FixedHeight := TCustomComboBox(SizeConstraints.Control).Style <> csSimple;
    end;

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

{------------------------------------------------------------------------------
  Function: GetDeviceSize
  Params: DC
          P
  Returns: true on success

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
begin
  p.x := Windows.GetDeviceCaps(DC, DESKTOPHORZRES);
  if p.x = 0
  then p.x := Windows.GetDeviceCaps(DC, HORZRES);
  p.y := Windows.GetDeviceCaps(DC, DESKTOPVERTRES);
  if p.y = 0
  then p.y := Windows.GetDeviceCaps(DC, VERTRES);

  Result := True;
end;


{------------------------------------------------------------------------------
  function TWin32WidgetSet.IntfSendsUTF8KeyPress: boolean;

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result := true;
end;


{------------------------------------------------------------------------------
  Method: CallbackAllocateHWnd
  Params: None
  Returns: Nothing

  Callback for the AllocateHWnd function
 ------------------------------------------------------------------------------}
procedure CallbackAllocateHWnd(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam); stdcall;
var
  Msg: TLMessage;
  PMethod: ^TLCLWndMethod;
begin
  FillChar(Msg, SizeOf(Msg), #0);

  Msg.msg := uMsg;
  Msg.wParam := wParam;
  Msg.lParam := lParam;

  {------------------------------------------------------------------------------
    Here we get the callback WndMethod associated with this window
   ------------------------------------------------------------------------------}
  PMethod := Pointer(Widgetset.GetWindowLong(ahwnd, GWL_USERDATA));

  if Assigned(PMethod) then PMethod^(Msg);

  Windows.DefWindowProc(ahwnd, uMsg, wParam, lParam);
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.AllocateHWnd
  Params: Method - The callback method for the window. Can be nil
  Returns: A window handle

  Allocates a non-visible window that can be utilized to receive and send message

  On Windows, you must call Windows.DefWindowProc(MyHandle, Msg.msg, Msg.wParam, msg.lParam);
  in your callback function, if you provide one at all, of course.
 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.AllocateHWnd(Method: TLCLWndMethod): HWND;
var
  PMethod: ^TLCLWndMethod;
begin
  Result := Windows.CreateWindow(@ClsName[0],
   '', WS_OVERLAPPED, 0, 0, 0, 0, 0, 0, MainInstance, nil);

  {------------------------------------------------------------------------------
    SetWindowLong has only space for 1 pointer on each slot, but a method is
   referenced as a structure with 2 pointers, so here we allocate memory for
   the structure before it can be used to transport data between the callback
   and this function
   ------------------------------------------------------------------------------}
  if Assigned(Method) then
  begin
    Getmem(PMethod, SizeOf(TMethod));
    PMethod^ := Method;

    Self.SetWindowLong(Result, GWL_USERDATA, PtrInt(PMethod));
  end;

  Self.SetWindowLong(Result, GWL_WNDPROC, PtrInt(@CallbackAllocateHWnd))
end;

{------------------------------------------------------------------------------
  Method: TWinCEWidgetSet.DeallocateHWnd
  Params: Wnd - A Window handle, that was created with AllocateHWnd
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TWinCEWidgetSet.DeallocateHWnd(Wnd: HWND);
var
  PMethod: ^TLCLWndMethod;
begin
  PMethod := Pointer(Self.GetWindowLong(Wnd, GWL_USERDATA));

  if Wnd <> 0 then Windows.DestroyWindow(Wnd);

  {------------------------------------------------------------------------------
    This must be done after DestroyWindow, otherwise a Access Violation will
   happen when WM_CLOSE message is sent to the callback

    This memory is for the TMethod structure allocated on AllocateHWnd
   ------------------------------------------------------------------------------}
  if Assigned(PMethod) then Freemem(PMethod);
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
