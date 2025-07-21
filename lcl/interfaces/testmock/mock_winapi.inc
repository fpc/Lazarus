{%MainUnit testmockint.pp}

{ $Id$ }
{******************************************************************************
                                  TWidgetSet

                                   WinApi stuff

  All Winapi related MOCKING goes here.

  !! In this file only winapi related code as defined in winapih.inc
  !! Keep this alphabetical !!
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
//##apiwiz##sps##   // Do not remove


function TTestMockWidgetSet.Arc(DC: HDC; Left,Top,Right,Bottom,Angle16Deg, Angle16DegLength: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.AngleChord(DC: HDC; x1,y1,x2,y2,angle1,angle2 : Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.BeginPaint(Handle : hwnd; var PS : TPaintStruct): hdc;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.CallNextHookEx(hhk : HHOOK; ncode : Integer; WParam: WParam; LParam: LParam) : Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; WParam: WParam; LParam: LParam): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.CharLower --> independent
//function TTestMockWidgetSet.CharLowerBuff --> independent
//function TTestMockWidgetSet.CharUpper --> independent
function TTestMockWidgetSet.ClientToScreen(Handle : HWND; var P : TPoint) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CombineRgn(Dest, Src1, Src2 : HRGN; fnCombineMode : Longint) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateBrushWithRadialGradient(const LogBrush: TLogRadialGradient): HBRUSH;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; width, Height: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateDIBitmap(DC: HDC; var InfoHeader: TBitmapInfoHeader;
  dwUsage: DWORD; InitBits: PChar; var InitInfo: TBitmapInfo;
  wUsage: UINT): HBITMAP;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateDIBSection(DC: HDC; const BitmapInfo: tagBitmapInfo; Usage: UINT;
  var Bits: Pointer; SectionHandle: TLCLHandle; Offset: DWORD): HBITMAP;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateEllipticRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.CreateEllipticRgnIndirect --> independent
//function TTestMockWidgetSet.CreateFont --> independent
function TTestMockWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := HFONT(TTestMockFontHandle.Create(LogFont, ''));
end;

function TTestMockWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
begin
  Result := HFONT(TTestMockFontHandle.Create(LogFont, LongFontName));
end;

function TTestMockWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreatePalette(const LogPalette: TLogPalette): HPalette;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreatePatternBrush(ABitmap: HBITMAP): HBRUSH;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.CreatePen --> independent
function TTestMockWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.CreateRectRgnIndirect --> independent
function TTestMockWidgetSet.CreateRoundRectRgn(X1, Y1, X2, Y2, nWidthEllipse, nHeightEllipse: Integer): HRGN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


procedure TTestMockWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
begin
//  assert(IsMockHandleOrNIl(GDIObject, TTestMockFontHandle), 'TTestMockWidgetSet.DeleteObject: IsMockHandleOrNIl(GDIObject, TTestMockFontHandle)');
  if (GDIObject <> 0) and (TObject(GDIObject) is TTestMockFontHandle) then begin
    TTestMockFontHandle(GDIObject).Destroy;
    Result := True;
    exit;
  end;

  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DestroyCaret(Handle : HWND): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DestroyCursor(Handle: HCURSOR): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DestroyIcon(Handle: HICON): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DrawFrameControl(DC: HDC; const Rect : TRect; uType, uState : Cardinal) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect; Flags: Cardinal): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
var
  W: TTestMockWindowHandle;
  n: TTestMockWindowHandle.TTestMockScrollBarBtns;
begin
{$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}

  if wSBflags AND SB_CTL <> 0 then begin
  // TControlScrollBar
    {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
    Result := inherited;
  end
  else begin
    assert(IsMockHandle(Wnd, TTestMockWindowHandle), 'TTestMockWidgetSet.EnableScrollBar: TObject(Wnd) is TTestMockWindowHandle');
    w := TTestMockWindowHandle(Wnd);
    case wArrows of
      {ESB_ENABLE_BOTH}   0:  n := [sbBtnUpLeft, sbBtnDownRight];
      {ESB_DISABLE_LEFT}
      {ESB_DISABLE_LTUP}
      {ESB_DISABLE_UP}    1:  n := [sbBtnUpLeft];
      {ESB_DISABLE_DOWN}
      {ESB_DISABLE_RIGHT}
      {ESB_DISABLE_RTDN}  2:  n := [sbBtnDownRight];
      {ESB_DISABLE_BOTH}  3:  n := [];
    end;

    Result := False;
    if wSBflags AND SB_Horz <> 0 then begin
      Result := W.MockScrollBars[sbHorizontal].ScrollBtnDisabled  <> n;
      W.MockScrollBars[sbHorizontal].ScrollBtnDisabled := n;
    end;
    if wSBflags AND SB_Vert <> 0 then begin
      Result := W.MockScrollBars[sbVertical].ScrollBtnDisabled  <> n;
      W.MockScrollBars[sbVertical].ScrollBtnDisabled := n;
    end;
    //TODO: redraw / send messages
  end;

end;

function TTestMockWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  assert(IsMockHandle(hWnd, TTestMockWindowHandle), 'TTestMockWidgetSet.EnableWindow: TObject(hWnd) is TTestMockWindowHandle');

  Result := not TTestMockWindowHandle(hWnd).Enabled;
  TTestMockWindowHandle(hWnd).Enabled := bEnable;
  (* TODO: Send messages: WM_ENABLE then WM_CANCELMODE (if being disabled)  *)
end;

function TTestMockWidgetSet.EndPaint(Handle : hwnd; var PS : TPaintStruct): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

procedure TTestMockWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect; lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
var
  i: Integer;
  m: TTestMockMonitor;
  r: TRect;
begin
  for i := 0 to TestMockMonitorList.Count - 1 do begin
    m := TestMockMonitorList[i];
    r := m.Bounds;
    lpfnEnum(HMONITOR(m), 1, @r, dwData);
  end;
  Result := True;
end;

function TTestMockWidgetSet.EnumFontFamilies(DC: HDC; Family: Pchar; EnumFontFamProc: FontEnumProc; LParam:Lparam):longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont:PLogFont; Callback: FontEnumExProc; Lparam:LParam; Flags: dword): longint;
begin
  exit(0);
  // TODO:
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.EqualRect --> independent
function TTestMockWidgetSet.EqualRgn(Rgn1: HRGN; Rgn2: HRGN): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ExtCreatePen(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): BOOL;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.FloodFill(DC: HDC; X, Y: Integer;
                   Color: TGraphicsColor;
                   FillStyle: TGraphicsFillStyle;
                   Brush: HBRUSH): Boolean;
                   begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.GetActiveWindow : HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetBkColor(DC: HDC): TColorRef;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCapture : HWND;
begin
  Result := 0;
//  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
end;

function TTestMockWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetClientRect(handle : HWND; var Rect: TRect) : Boolean;
begin
  // TODO: Adjust for border // Allow custom adjustments
  assert(IsMockHandle(handle, TTestMockWindowHandle), 'TTestMockWidgetSet.GetClientRect: handle is TTestMockWindowHandle');
  Rect := TTestMockWindowHandle(handle).MockBounds;
  Result := True;
end;

function TTestMockWidgetSet.GetClipBox(DC : hDC; lpRect : PRect) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCompositorExtendedBorder(handle : HWND; var Borders: TRect) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCursorPos(var lpPoint: TPoint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetCharABCWidths(DC: HDC; p2, p3: UINT; const ABCStructs): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//pbd
function TTestMockWidgetSet.GetDC(hWnd: HWND): HDC;
begin
  assert(IsMockHandleOrNIl(hWnd, TTestMockWindowHandle), 'TTestMockWidgetSet.RemoveProp: TObject(Handle) is TTestMockWindowHandle');

  Result := HDC(TTestMockDeviceContext.Create(TTestMockWindowHandle(hWnd)));
end;

function TTestMockWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetDoubleClickTime: UINT;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetFocus: HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetFontLanguageInfo(DC: HDC): DWord;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetForegroundWindow: HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetIconInfo(AIcon: HICON; AIconInfo: PIconInfo): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetMapMode(DC: HDC): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
var
  m: TTestMockMonitor;
  s: DWORD;
begin
  assert(TObject(hMonitor) is TTestMockMonitor, 'TTestMockWidgetSet.GetMonitorInfo: TObject(hMonitor) is TTestMockMonitor');
  m := TTestMockMonitor(hMonitor);
  s := lpmi^.cbSize;
  if s >= PtrUInt(@PMonitorInfo(nil)^.rcWork)  then lpmi^.rcMonitor := m.Bounds;
  if s >= PtrUInt(@PMonitorInfo(nil)^.dwFlags) then lpmi^.rcWork := m.Bounds;
  if s >= SizeOf(TMonitorInfo) then lpmi^.dwFlags := MONITORINFOF_PRIMARY;
  Result := True;
end;

function TTestMockWidgetSet.GetDpiForMonitor(hmonitor: HMONITOR; dpiType: TMonitorDpiType; out dpiX: UINT; out dpiY: UINT): HRESULT;
var
  m: TTestMockMonitor;
begin
  assert(TObject(hMonitor) is TTestMockMonitor, 'TTestMockWidgetSet.GetDpiForMonitor: TObject(hMonitor) is TTestMockMonitor');
  // TODO: dpi type
  m := TTestMockMonitor(hMonitor);
  dpiX := m.DPI;
  dpiY := m.DPI;
  Result := S_OK;
end;

function TTestMockWidgetSet.GetObject(GDIObject: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
var M: TTestMockHandle absolute GDIObject;
begin
  assert(IsMockHandle(GDIObject, TTestMockHandle), 'TTestMockWidgetSet.GetObject: IsMockHandle(GDIObject)');

  if M is TTestMockFontHandle then begin
    Move(TTestMockFontHandle(M).FLogFont, Buf^, max(BufSize, sizeof(TLogFont)));
    exit;
  end;

  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetPaletteEntries(Palette: HPALETTE; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetParent(Handle : HWND): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetProp(Handle : hwnd; Str : PChar): Pointer;
begin
  assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.GetProp: TObject(Handle) is TTestMockWindowHandle');
  Result := TTestMockWindowHandle(Handle).GetProp(Str);
end;

function TTestMockWidgetSet.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetROP2(DC: HDC): integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetScrollInfo(Handle: HWND; SBStyle: Integer; var ScrollInfo: TScrollInfo): Boolean;
var
  w: TTestMockWindowHandle;
begin
  if SBStyle * SB_CTL <> 0 then begin
    assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.GetScrollInfo: TObject(Handle) is TTestMockWindowHandle');
    Result := inherited;
  end
  else begin
    assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.SetScrollInfo: TObject(Handle) is TTestMockWindowHandle');
    Result := False;
    w := TTestMockWindowHandle(Handle);
    if SBStyle * SB_Horz <> 0 then begin
      ScrollInfo := w.MockScrollBars[sbHorizontal].ScrollInfo;
      Result := True;
    end;
    if SBStyle * SB_Vert <> 0 then begin
      ScrollInfo := w.MockScrollBars[sbVertical].ScrollInfo;
      Result := True;
    end;
  end;
end;

//function TTestMockWidgetSet.GetScrollPos --> independent
//function TTestMockWidgetSet.GetScrollRange --> independent
function TTestMockWidgetSet.GetStockObject(Value: Integer): TLCLHandle;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetSystemPaletteEntries(DC: HDC; StartIndex, NumEntries: UINT;
  var PaletteEntries): UINT;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetTextColor(DC: HDC) : TColorRef;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetTextExtentExPoint(DC: HDC; Str: PChar; Count, MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: TSize): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  tm: TTextMetric;
begin
  DebugLn(['WARNING:  TTestMockWidgetSet.GetTextExtentPoint => use GetTextExtentPoint32']);
  //assert(False, 'TTestMockWidgetSet.GetTextExtentPoint: False');
  assert(IsMockHandle(DC, TTestMockDeviceContext), 'TTestMockWidgetSet.GetTextExtentPoint: IsMockHandle(DC, TTestMockDeviceContext)');
  GetTextMetrics(DC, tm);
  Size.cx := UTF8Length(Str, Count) * tm.tmWeight;
  Size.cy := tm.tmHeight;
  Result := True;
end;

function TTestMockWidgetSet.GetTextExtentPoint32(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  tm: TTextMetric;
begin
  assert(IsMockHandle(DC, TTestMockDeviceContext), 'TTestMockWidgetSet.GetTextExtentPoint32: IsMockHandle(DC, TTestMockDeviceContext)');
  GetTextMetrics(DC, tm);
  Size.cx := UTF8Length(Str, Count) * tm.tmWeight;
  Size.cx := tm.tmHeight;
  Result := True;
end;

function TTestMockWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
begin
  assert(IsMockHandle(DC, TTestMockDeviceContext), 'TTestMockWidgetSet.GetTextMetrics: IsMockHandle(DC, TTestMockDeviceContext)');
  TM.tmHeight           := 15;
  TM.tmAscent           := 10;
  TM.tmDescent          := 5;
  TM.tmInternalLeading  := 20;
  TM.tmExternalLeading  := 20;
  TM.tmAveCharWidth     := 20;
  TM.tmMaxCharWidth     := 20;
  TM.tmWeight           := 1;
  TM.tmOverhang         := 0;
  TM.tmDigitizedAspectX := 1;
  TM.tmDigitizedAspectY := 1;
  TM.tmFirstChar        := #32;
  TM.tmLastChar         := #127;
  TM.tmDefaultChar      := #32;
  TM.tmBreakChar        := #10;
  TM.tmItalic           := 0;
  TM.tmUnderlined       := 0;
  TM.tmStruckOut        := 0;
  TM.tmPitchAndFamily   := 0;
  TM.tmCharSet          := 0;
  Result := True;
end;

function TTestMockWidgetSet.GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetWindowExtEx(DC: HDC; Size: PSize): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetWindowLong(Handle : hwnd; int : Integer): PtrInt;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetWindowRect(Handle : hwnd; var Rect : TRect): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.GetWindowSize(Handle : hwnd; var Width, Height: integer): boolean;
var
  r: TRect;
begin
  assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.GetWindowSize: TObject(Handle) is TTestMockWindowHandle');
  r := TTestMockWindowHandle(Handle).MockBounds;
  Width  := r.Width;
  Height := r.Height;
  Result := True;
end;

function TTestMockWidgetSet.GetWindowOrgEx(dc : hdc; var P: TPoint): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

// because of delphi compatibility
function TTestMockWidgetSet.GetWindowOrgEx(dc : hdc; P : PPoint): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

// an old declaration
function TTestMockWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex; NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): Boolean;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


//function TTestMockWidgetSet.IndexToOverlayMask --> independent
//function TTestMockWidgetSet.InflateRect --> independent
procedure TTestMockWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right,Bottom: Integer): Integer;
begin
  //exit(SimpleRegion); // or NullRegion;
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.IntersectRect --> independent
function TTestMockWidgetSet.InvalidateRect(aHandle : HWND; ARect : pRect; bErase : Boolean) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.InvalidateRgn(Handle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.IsCharAlphaNumeric --> independent
function TTestMockWidgetSet.IsDBCSLeadByte(TestChar: Byte): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.IsRectEmpty --> independent
function TTestMockWidgetSet.IsIconic(handle: HWND): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsWindow(handle: HWND): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsWindowEnabled(handle: HWND): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsWindowVisible(handle: HWND): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.IsZoomed(handle: HWND): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


//function TTestMockWidgetSet.KillTimer --> independent

procedure TTestMockWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.LoadBitmap(hInstance: TLCLHandle; lpBitmapName: PChar): HBitmap;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.LoadCursor(hInstance: TLCLHandle; lpCursorName: PChar): HCursor;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.LoadIcon(hInstance: TLCLHandle; lpIconName: PChar): HIcon;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


//function TTestMockWidgetSet.MakeLong --> independent
//function TTestMockWidgetSet.MakeROP4 --> independent
//function TTestMockWidgetSet.MakeWord --> independent
function TTestMockWidgetSet.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.MaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP; XMask, YMask: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal = MB_OK): integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.MonitorFromPoint(ptScreenCoords: TPoint; dwFlags: DWord): HMONITOR;
var
  i: Integer;
  m: TTestMockMonitor;
  b: TRect;
  d, Dist: Integer;
begin
  Result := 0;
  Dist := 0;
  for i := 0 to TestMockMonitorList.Count - 1 do begin
    m := TestMockMonitorList[i];
    b := m.Bounds;
    if b.Contains(ptScreenCoords) then
      exit(HMONITOR(m));

    case dwFlags of
      MONITOR_DEFAULTTOPRIMARY:
        if i = 0 then Result := HMONITOR(m); // TODO: primary flag
      MONITOR_DEFAULTTONEAREST: begin
          d := min(
            Min(abs(b.Top - ptScreenCoords.y), abs(b.Bottom - ptScreenCoords.y)),
            Min(abs(b.Left - ptScreenCoords.x), abs(b.Right - ptScreenCoords.x))
          );
          if (Result = 0) or (d < Dist) then begin
            Result := HMONITOR(m);
            Dist := d;
          end;
        end;
    end;
  end;
end;

function TTestMockWidgetSet.MonitorFromRect(lprcScreenCoords: PRect; dwFlags: DWord): HMONITOR;
var
  i: Integer;
  m, best: TTestMockMonitor;
  b, ScrRct, ISect: TRect;
  d, Dist, o, Over: Integer;
begin
  Result := 0;
  best := nil;
  Dist := 0;
  Over := 0;
  ScrRct := lprcScreenCoords^;
  for i := 0 to TestMockMonitorList.Count - 1 do begin
    m := TestMockMonitorList[i];
    b := m.Bounds;
    IntersectRect(ISect, b, ScrRct);
    o := ISect.Width * ISect.Height;
    if o > Over then begin
      best := m;
      Over := o;
    end;

    case dwFlags of
      MONITOR_DEFAULTTOPRIMARY:
        if i = 0 then Result := HMONITOR(m); // TODO: primary flag
      MONITOR_DEFAULTTONEAREST: begin
          d := min(
            Min(abs(b.Top - ScrRct.Bottom), abs(b.Bottom - ScrRct.Top)),
            Min(abs(b.Left - ScrRct.Right), abs(b.Right - ScrRct.Left))
          );
          if (Result = 0) or (d < Dist) then begin
            Result := HMONITOR(m);
            Dist := d;
          end;
        end;
    end;
  end;
  if best <> nil then
    Result := HMONITOR(best);
end;

function TTestMockWidgetSet.MonitorFromWindow(hWnd: HWND; dwFlags: DWord): HMONITOR;
var
  b: TRect;
begin
  assert(IsMockHandle(hWnd, TTestMockWindowHandle), 'TTestMockWidgetSet.MonitorFromWindow: TObject(hWnd) is TTestMockWindowHandle');
  b := TTestMockWindowHandle(hWnd).MockBounds;
  Result := MonitorFromRect(@b , dwFlags);
end;


function TTestMockWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


//function TTestMockWidgetSet.OffsetRect --> independent
function TTestMockWidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.PaintRgn(DC: HDC; RGN: HRGN): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.Pie(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.PolyBezier(DC: HDC; Points: PPoint;  NumPts: Integer; Filled, Continuous: boolean): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.Polygon(DC: HDC; Points: PPoint;  NumPts: Integer; Winding: boolean): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.PtInRect --> independent

function TTestMockWidgetSet.RealizePalette(DC: HDC): Cardinal;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RectInRegion(RGN: HRGN; ARect: TRect): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RectVisible(DC: HDC; const ARect: TRect): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ReleaseCapture : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  assert(IsMockHandle(DC, TTestMockDeviceContext), 'TTestMockWidgetSet.ReleaseDC: IsMockHandle(DC, TTestMockDeviceContext)');
  assert(IsMockHandleOrNIl(hWnd, TTestMockWindowHandle), 'TTestMockWidgetSet.ReleaseDC: TObject(hWnd) is TTestMockWindowHandle');;

  if TTestMockDeviceContext(DC).Handle <> hWnd then
    assert(False, 'TTestMockWidgetSet.ReleaseDC: wrong handle for releaseDC');

  TTestMockDeviceContext(DC).Free;
end;

function TTestMockWidgetSet.RemoveProp(Handle: HWND; Str: PChar): TLCLHandle;
begin
  assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.RemoveProp: TObject(Handle) is TTestMockWindowHandle');
  Result := TLCLHandle(TTestMockWindowHandle(Handle).RemoveProp(Str));
end;

function TTestMockWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
begin
  Result := True;
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
//  Result := inherited;
end;

function TTestMockWidgetSet.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.SaveDC(DC: HDC): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
begin
  Result := 1;
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  //Result := inherited;
end;

//function TTestMockWidgetSet.ScrollWindow --> independent deprecated calls ScrollWindowEx
function TTestMockWidgetSet.ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: PRect; hrgnUpdate: HRGN; prcUpdate: PRect; flags: UINT): Boolean;
begin
  assert(TObject(hWnd) is TTestMockWindowHandle, 'TTestMockWidgetSet.ScrollWindowEx: TObject(hWnd) is TTestMockWindowHandle');
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SelectClipRGN(DC : hDC; RGN : HRGN) : Longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
begin
  assert(IsMockHandle(DC, TTestMockDeviceContext), 'TTestMockWidgetSet.SelectObject: IsMockHandle(DC, TTestMockDeviceContext)');

  if (GDIObj = HGDIOBJ(TTestMockFontHandle)) then begin // TODO: Always create a font for a DC
    Result := HGDIOBJ(TTestMockDeviceContext(DC).Mock_Font);
    if Result = 0 then Result := HGDIOBJ(TTestMockFontHandle);
    TTestMockDeviceContext(DC).Mock_Font := nil;
    exit;
  end;
  if (GDIObj <> 0) and (TObject(GDIObj) is TTestMockFontHandle) then begin
    Result := HGDIOBJ(TTestMockDeviceContext(DC).Mock_Font);
    if Result = 0 then Result := HGDIOBJ(TTestMockFontHandle);
    TTestMockDeviceContext(DC).Mock_Font := TTestMockFontHandle(GDIObj);
    exit;
  end;

  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SelectPalette(DC: HDC; Palette: HPALETTE; ForceBackground: Boolean): HPALETTE;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetBkColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//pbd
function TTestMockWidgetSet.SetBkMode(DC: HDC; bkMode : Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetCapture (AHandle: HWND): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetCaretPosEx(handle: HWnd; X, Y: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetCursor(hCursor: HICON): HCURSOR;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetFocus(hWnd: HWND): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetForegroundWindow(hWnd : HWND): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetMapMode(DC: HDC; fnMapMode : Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetProp(Handle: hwnd; Str : PChar; Data : Pointer) : Boolean;
begin
  assert(TObject(Handle) is TTestMockWindowHandle, 'TTestMockWidgetSet.SetProp: TObject(Handle) is TTestMockWindowHandle');
  TTestMockWindowHandle(Handle).SetProp(Str, Data);
end;

//function TTestMockWidgetSet.SetRect --> independent
function TTestMockWidgetSet.SetRectRgn(aRGN: HRGN; X1, Y1, X2, Y2 : Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.SetRectEmpty --> independent
function TTestMockWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetScrollInfo(Handle: HWND; SBStyle: Integer; ScrollInfo: TScrollInfo; Redraw : Boolean): Integer;
var
  w: TTestMockWindowHandle;
begin
  if SBStyle AND SB_CTL <> 0 then begin
  // TControlScrollBar
    {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
    Result := inherited;
  end
  else begin
    assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.SetScrollInfo: TObject(Handle) is TTestMockWindowHandle');
    w := TTestMockWindowHandle(Handle);
    Result := 0;
    if SBStyle AND SB_Horz <> 0 then begin
      Result := W.MockScrollBars[sbHorizontal].ScrollInfo.nPos;
      W.MockScrollBars[sbHorizontal].ScrollInfo := ScrollInfo;
    end;
    if SBStyle AND SB_Vert <> 0 then begin
      Result := W.MockScrollBars[sbVertical].ScrollInfo.nPos;
      W.MockScrollBars[sbVertical].ScrollInfo := ScrollInfo;
    end;
    // TODO: redraw / send messages
  end;
end;

//function TTestMockWidgetSet.SetScrollPos --> independent
//function TTestMockWidgetSet.SetScrollRange --> independent
function TTestMockWidgetSet.SetStretchBltMode(DC: HDC; StretchMode: Integer): Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetTextCharacterExtra(_hdc : hdc; nCharExtra : Integer):Integer;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.SetTimer --> independent
function TTestMockWidgetSet.SetWindowLong(Handle: HWND; Idx: Integer; NewLong : PtrInt): PtrInt;
begin
  assert(TObject(Handle) is TTestMockWindowHandle, 'TTestMockWidgetSet.SetWindowLong: TObject(Handle) is TTestMockWindowHandle');
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  inherited;
end;

function TTestMockWidgetSet.SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetWindowOrgEx(dc : hdc; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND;
       X, Y, cx, cy: Integer; uFlags: UINT): Boolean;
var W: TTestMockWindowHandle absolute hWnd;
begin
  assert(IsMockHandle(hWnd, TTestMockWindowHandle), 'TTestMockWidgetSet.SetWindowPos: IsMockHandle(hWnd, TTestMockWindowHandle)');

  // TODO : more flags SWP_NOZORDER or SWP_NOACTIVATE
  // TODO: messages depending on flags

  if uFlags * SWP_NOMOVE <> 0 then begin
    X := W.MockBounds.Left;
    Y := W.MockBounds.Top;
  end;
  if uFlags * SWP_NOMOVE <> 0 then begin
    cX := W.MockBounds.Width;
    cY := W.MockBounds.Height;
  end;
  W.MockBounds := Rect(X, Y, cx, cy);

  TestMockMessageDispatcher.DoNewWindowPos(W); // SWP_NOMOVE SWP_NOSIZE ?

  if uFlags * SWP_NOACTIVATE <> 0 then begin
//TODO : send activate message
  end;


  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: Boolean): longint;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
var
  W: TTestMockWindowHandle;
begin
  if wBar AND SB_CTL <> 0 then begin
  // TControlScrollBar
    {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
    Result := inherited;
  end
  else begin
    assert(IsMockHandle(Handle, TTestMockWindowHandle), 'TTestMockWidgetSet.ShowScrollBar: TObject(Handle) is TTestMockWindowHandle');
    W := TTestMockWindowHandle(Handle);
    Result := True;
    if wBar AND SB_Horz <> 0 then begin
      W.MockScrollBars[sbHorizontal].ScrollVisible := bShow;
    end;
    if wBar AND SB_Vert <> 0 then begin
      W.MockScrollBars[sbVertical].ScrollVisible := bShow;
    end;
    // TODO: redraw / send messages
  end;
end;

function TTestMockWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: Cardinal): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

function TTestMockWidgetSet.StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHeight, SrcX,
  SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; var BitsInfo: TBitmapInfo;
  Usage: UINT; Rop: DWORD): Integer;
  begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;

//function TTestMockWidgetSet.SubtractRect --> independent
function TTestMockWidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


function TTestMockWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


//function TTestMockWidgetSet.UnionRect --> independent

function TTestMockWidgetSet.WindowFromPoint(Point : TPoint) : HWND;
begin
  {$IFDEF MOCKINT_TEST_ASSERT_NO_BYPASS_TO_BASE} assert(false, 'missing mock implementation for '+{$I %CurrentRoutine%}); {$ENDIF}
  Result := inherited;
end;


