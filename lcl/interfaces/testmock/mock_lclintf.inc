{%MainUnit testmockint.pp}

{ $Id$ }
{******************************************************************************
                                  TWidgetSet

                                   WinApi stuff

  All interface communication related MOCKING goes here.

  !! In this file only winapi related code as defined in lclintfh
  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove


function TTestMockWidgetSet.AddEventHandler(AHandle: TLCLHandle; AFlags: dword; AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.AddProcessEventHandler(AHandle: TLCLHandle; AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.AddPipeEventHandler(AHandle: TLCLHandle; AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.AllocateHWnd(Method: TLCLWndMethod): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.AskUser(const DialogCaption, DialogMessage: string; DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


procedure TTestMockWidgetSet.CallDefaultWndHandler(Sender: TObject; var Message);
begin
  // TODO: simiulate messages
//  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
//  inherited;
end;

function TTestMockWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  if FormatID > 0 then
    exit(TestMockRegisteredClipBoardFormats[FormatID-1]);
  assert(False, 'TTestMockWidgetSet.ClipboardFormatToMimeType: unregistered');
  Result := '';
end;

function TTestMockWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

// ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
function TTestMockWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := TestMockRegisteredClipBoardFormats.IndexOf(AMimeType) + 1;
  if Result <= 0 then
    Result := TestMockRegisteredClipBoardFormats.Add(AMimeType) + 1;
end;

function TTestMockWidgetSet.ClipboardFormatNeedsNullByte(const AFormat: TPredefinedClipboardFormat): Boolean;
begin
  Result := False;
end;

function TTestMockWidgetSet.CreateEmptyRegion: hRGN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateRegionCopy(SrcRGN: hRGN): hRGN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  if (ACursor >= crLow) and (ACursor <= crHigh) then begin
    Result := HCURSOR(ACursor);
  end
  else begin
    {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
    Result := inherited;
  end;
end;


function TTestMockWidgetSet.DCClipRegionValid(DC: HDC): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

procedure TTestMockWidgetSet.DeallocateHWnd(Wnd: HWND);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.DestroyRubberBand(ARubberBand: HWND);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;


function TTestMockWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.FontIsMonoSpace(Font: HFont): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TGraphicsBevelCut): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetAvailableNativeCanvasTypes(DC: HDC; AAllowFallbackToParent: Boolean = False): TNativeCanvasTypes;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetAvailableNativeHandleTypes(Handle: HWND; AAllowFallbackToParent: Boolean = False): TNativeHandleTypes;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetClientBounds(handle : HWND; var ARect: TRect) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCmdLineParamDescForInterface: TStringList;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  // no WS constraints
  Result := True;
end;

function TTestMockWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetDeviceSize(DC: HDC; var p: TPoint): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetNativeCanvas(DC: HDC; AHandleType: TNativeCanvasType; AAllowFallbackToParent: Boolean = False): PtrInt;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetNativeHandle(Handle: HWND; AHandleType: TNativeHandleType; AAllowFallbackToParent: Boolean = False): PtrInt;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetWindowRelativePosition(Handle : hwnd; var Left, Top: integer): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsMobilePlatform: Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsCDIntfControl(AWinControl: TObject): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey : Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey : Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RadialPie(DC: HDC; x1, y1, x2, y2, Angle1, Angle2: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

procedure TTestMockWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


procedure TTestMockWidgetSet.SendCachedLCLMessages;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.SetCanvasScaleFactor(DC: HDC; const AScaleFactor: double);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

procedure TTestMockWidgetSet.SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

procedure TTestMockWidgetSet.SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.ShowSelectItemDialog(const AItems: TStrings; APos: TPoint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer;  Str: PChar; Count: Longint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


