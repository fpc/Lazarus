{%MainUnit cocoaint.pas}
{ $Id: cocoawinapi.inc 15525 2008-06-23 06:39:58Z paul $ }
{******************************************************************************
  All Cocoa Winapi implementations.
  This are the implementations of the overrides of the Cocoa Interface for the
  methods defined in the
  lcl/include/winapi.inc

 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

function CocoaCombineMode(ACombineMode: Integer): TCocoaCombine;
begin
  case ACombineMode of
    RGN_AND:  Result:=cc_And;
    RGN_OR:   Result:=cc_Or;
    RGN_XOR:  Result:=cc_Xor;
    RGN_DIFF: Result:=cc_Diff;
  else
    Result:=cc_Copy;
  end;
end;

const
  CocoaRegionTypeToWin32Map: array[TCocoaRegionType] of Integer = (
 { crt_Error     } ERROR,
 { crt_Empty     } NULLREGION,
 { crt_Rectangle } SIMPLEREGION,
 { crt_Complex   } COMPLEXREGION
  );

function TCocoaWidgetSet.Arc(DC: HDC; Left, Top, Right, Bottom, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited Arc(DC, Left, Top, Right, Bottom, angle1, angle2);
end;

function TCocoaWidgetSet.AngleChord(DC: HDC; x1, y1, x2, y2, angle1,
  angle2: Integer): Boolean;
begin
  Result:=inherited AngleChord(DC, x1, y1, x2, y2, angle1, angle2);
end;

function TCocoaWidgetSet.BeginPaint(Handle: hWnd; var PS: TPaintStruct): hdc;
begin
  Result := inherited BeginPaint(Handle, PS);
  PS.hdc := Result;
end;

function TCocoaWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
    Width, Height, 0, 0, 0, Rop);
end;

function TCocoaWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint): Boolean;
begin
  Result := Handle <> 0;

  if Result then
  begin
    // 1. convert client to window
    with NSObject(Handle).lclClientFrame do
    begin
      inc(P.X, Left);
      inc(P.Y, Top);
    end;
    // 2. convert window to screen
    NSObject(Handle).lclLocalToScreen(P.X, P.Y);
  end;
end;

function TCocoaWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;
begin
  Result := LCLType.Error;
  if (Dest = 0) or (Src1 = 0) or (fnCombineMode<RGN_AND) or (fnCombineMode>RGN_COPY) then Exit;
  if (fnCombineMode <> RGN_COPY) and (Src2 = 0) then Exit;

  TCocoaRegion(Dest).CombineWith(TCocoaRegion(Src1), cc_Copy);

  if fnCombineMode <> RGN_COPY then
    TCocoaRegion(Dest). CombineWith(TCocoaRegion(Src2), CocoaCombineMode(fnCombineMode));
end;

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:  Width      - Bitmap width, in pixels
           Height     - Bitmap height, in pixels
           Planes     - Number of color planes
           BitCount   - Number of bits required to identify a color (TODO)
           BitmapBits - Pointer to array containing color data (TODO)
  Returns: A handle to a bitmap

  Creates a bitmap with the specified width, height and color format
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  bmpType: TCocoaBitmapType;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateBitmap');
  {$ENDIF}

  // WORKAROUND: force context supported depths
  if BitmapBits = nil then
  begin
    if BitCount = 24 then BitCount := 32;
    // if BitCount = 1 then BitCount := 8;
  end;

  case BitCount of
    1:  bmpType := cbtMono;
    8:  bmpType := cbtGray;
    32: bmpType := cbtARGB;
  else
    bmpType := cbtRGB;
  end;

  // winapi Bitmaps are on a word boundary
  Result := HBITMAP(TCocoaBitmap.Create(Width, Height, BitCount, BitCount, cbaWord, bmpType, BitmapBits));
end;

function TCocoaWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
begin
  Result := HBrush(TCocoaBrush.Create(LogBrush));
end;

function TCocoaWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateCompatibleBitmap');
  {$ENDIF}

  Result := HBITMAP(TCocoaBitmap.Create(Width, Height, 32, 32, cbaDQWord, cbtARGB, nil));
end;

{------------------------------------------------------------------------------
  Method:  CreateCompatibleDC
  Params:  DC - Handle to memory device context
  Returns: Handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateCompatibleDC');
  {$ENDIF}

  Result := HDC(TCocoaContext.Create);
end;

//todo:
//function TCocoaWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
//begin
//end;

function TCocoaWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := HFont(TCocoaFont.Create(LogFont, LogFont.lfFaceName));
end;

function TCocoaWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont;
  const LongFontName: string): HFONT;
begin
  Result := HFont(TCocoaFont.Create(LogFont, LongFontName));
end;

function Create32BitAlphaBitmap(ABitmap, AMask: TCocoaBitmap): TCocoaBitmap;
var
  ARawImage: TRawImage;
  Desc: TRawImageDescription absolute ARawimage.Description;

  ImgHandle, ImgMaskHandle: HBitmap;
  ImagePtr: PRawImage;
  DevImage: TRawImage;
  DevDesc: TRawImageDescription;
  SrcImage, DstImage: TLazIntfImage;
  W, H: Integer;
begin
  Result := nil;

  if not RawImage_FromBitmap(ARawImage, HBITMAP(ABitmap), HBITMAP(AMask)) then
    Exit;

  ImgMaskHandle := 0;

  W := Desc.Width;
  if W < 1 then W := 1;
  H := Desc.Height;
  if H < 1 then H := 1;

  QueryDescription(DevDesc, [riqfRGB, riqfAlpha], W, H);

  if DevDesc.IsEqual(Desc)
  then begin
    // image is compatible, so use it
    DstImage := nil;
    ImagePtr := @ARawImage;
  end
  else begin
    // create compatible copy
    SrcImage := TLazIntfImage.Create(ARawImage, False);
    DstImage := TLazIntfImage.Create(0,0,[]);
    DstImage.DataDescription := DevDesc;
    DstImage.CopyPixels(SrcImage);
    SrcImage.Free;
    DstImage.GetRawImage(DevImage);
    ImagePtr := @DevImage;
  end;

  try
    if not RawImage_CreateBitmaps(ImagePtr^, ImgHandle, ImgMaskHandle, True) then Exit;

    Result := TCocoaBitmap(ImgHandle);
  finally
    ARawImage.FreeData;
    DstImage.Free;
  end;
end;

function TCocoaWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
var
  ABitmap: TCocoaBitmap;
begin
  Result := 0;
  if IconInfo^.hbmColor = 0 then Exit;

  ABitmap := Create32BitAlphaBitmap(TCocoaBitmap(IconInfo^.hbmColor), TCocoaBitmap(IconInfo^.hbmMask));

  if IconInfo^.fIcon then
    Result := HICON(ABitmap)
  else
    Result := HICON(TCocoaCursor.CreateFromBitmap(ABitmap, GetNSPoint(IconInfo^.xHotSpot, IconInfo^.yHotSpot)));
end;

function TCocoaWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
begin
  Result := HPen(TCocoaPen.Create(LogPen));
end;

{------------------------------------------------------------------------------
  Method:  CreatePolygonRgn
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode
  Returns: The new polygonal region

  Creates a new polygonal region from the specified points
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer;
  FillMode: integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreatePolygonRgn NumPts: ' + DbgS(NumPts) +
      ' FillMode: ' + DbgS(FillMode));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(Points, NumPts, FillMode=ALTERNATE));
end;

function TCocoaWidgetSet.CreateRectRgn(X1, Y1, X2, Y2: Integer): HRGN;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.CreateRectRgn R: ' + DbgS(Classes.Rect(X1, Y1, X2, Y2)));
  {$ENDIF}

  Result := HRGN(TCocoaRegion.Create(X1, Y1, X2, Y2));
end;

function TCocoaWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  gdi: TCocoaGDIObject;
begin
  Result := False;
  if GDIObject = 0 then
    Exit(True);

  gdi := CheckGDIOBJ(GdiObject);

  if not Assigned(gdi) then
    Exit;

  if gdi.Global then
    Exit;

  if gdi.RefCount = 0 then gdi.Destroy;
end;

function TCocoaWidgetSet.DestroyIcon(Handle: HICON): Boolean;
var
  Ico: TObject;
begin
  Result := Handle <> 0;
  if not Result then
    Exit;
  Ico := TObject(Handle);
  Result := (Ico is TCocoaBitmap) or (Ico is TCocoaCursor);
  if Result then
    Ico.Destroy;
end;

function TCocoaWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx);
  if Result then
    ctx.DrawFocusRect(Rect);
end;

function TCocoaWidgetSet.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal;
  grfFlags: Cardinal): Boolean;
begin
  Result := inherited DrawEdge(DC, Rect, edge, grfFlags);
end;

function TCocoaWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx);
  if Result then
    ctx.Ellipse(x1, y1, x2, y2);
end;

function TCocoaWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  Result := hWnd <> 0;
  if Result then
    NSObject(hWnd).lclSetEnabled(bEnable)
end;

function TCocoaWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
begin
  Result:=inherited EndPaint(Handle, PS);
end;

function TCocoaWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
var
  fname: NSString;
  ELogFont: TEnumLogFontEx;
  Metric: TNewTextMetricEx;
  FontName: AnsiString;
begin
  Result := 0;
  if not Assigned(Callback) then Exit;
  for fname in NSFontManager.sharedFontManager.availableFontFamilies do
  begin
    try
      FontName := NSStringToString(fname);
      FillChar(ELogFont, SizeOf(ELogFont), #0);
      FillChar(Metric, SizeOf(Metric), #0);
      ELogFont.elfLogFont.lfFaceName := FontName;
      ELogFont.elfFullName := FontName;
      //todo: read the data from all fonts of the fontfamily
      Result := CallBack(ELogFont, Metric, TRUETYPE_FONTTYPE, lparam);
      if Result = 0 then Break;
    except
      Break;
    end;
  end;
end;

function TCocoaWidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect;
  lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
var
  i: integer;
begin
  Result := True;
  for i := 0 to NSScreen.screens.count - 1 do
  begin
    Result := Result and lpfnEnum(HMONITOR(NSScreen.screens.objectAtIndex(i)), 0, nil, dwData);
    if not Result then break;
  end;
end;

function TCocoaWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
    Result := CocoaRegionTypeToWin32Map[ctx.SetClipRegion(TCocoaRegion(rgn), CocoaCombineMode(Mode))]
  else
    Result := ERROR;
end;

function TCocoaWidgetSet.ExtCreatePen(dwPenStyle, dwWidth: DWord;
  const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
begin
  Result := HPEN(TCocoaPen.Create(dwPenStyle, dwWidth, lplb, dwStyleCount, lpStyle));
end;

function TCocoaWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx);
  if Assigned(ctx) then
    ctx.TextOut(X, Y, Options, Rect, Str, Count, Dx);
end;

function TCocoaWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  ctx: TCocoaContext;
  br: TCocoaGDIObject;
begin
  ctx := CheckDC(DC);
  br := CheckGDIOBJ(Brush);
  Result := Assigned(ctx) and (not Assigned(br) or (br is TCocoaBrush));
  if not Result then Exit;

  with Rect do
    ctx.Rectangle(Left, Top, Right, Bottom, True, TCocoaBrush(br));
end;

function TCocoaWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool;
var
  OldRgn: TCocoaRegion;
  R: TRect;
  Clipped: Boolean;
  ctx: TCocoaContext;
  br: TCocoaGDIObject;
  I: Integer;
begin
  ctx := CheckDC(DC);
  br := CheckGDIOBJ(hbr);
  Result := Assigned(ctx) and (not Assigned(br) or (br is TCocoaBrush));
  if not Result then Exit;

  Clipped := ctx.Clipped;
  I := ctx.SaveDC;
  if Clipped then
    OldRgn := TCocoaRegion.CreateDefault;
  try
    if Clipped then
      ctx.CopyClipRegion(OldRgn);
    if SelectClipRgn(DC, RegionHnd) <> ERROR then
    begin
      R := TCocoaRegion(RegionHnd).GetBounds;
      with R do
        ctx.Rectangle(Left, Top, Right, Bottom, True, TCocoaBrush(br));
      if Clipped then
        SelectClipRgn(DC, HRGN(OldRgn));
      Result := True;
    end;
  finally
    if Clipped then
      OldRgn.Free;
    ctx.RestoreDC(I);
  end;
end;

function TCocoaWidgetSet.Frame3d(DC: HDC; var ARect: TRect;
  const FrameWidth: integer; const Style: TBevelCut): Boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx) and (FrameWidth > 0);
  if Result then
    ctx.Frame3d(ARect, FrameWidth, Style);
end;

function TCocoaWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
  begin
    ctx.FrameRect(ARect, TCocoaBrush(hBr));
    Result := -1;
  end
  else
    Result := 0;
end;

function TCocoaWidgetSet.GetActiveWindow: HWND;
begin
  // return the currect application active window
  Result := HWND(NSApp.keyWindow);
end;

function TCocoaWidgetSet.GetBkColor(DC: HDC): TColorRef;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
    Result := ctx.BkColor
  else
    Result := CLR_INVALID;
end;

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TCocoaWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
begin
  {$ifdef VerboseCocoaWinAPI}
    DebugLn('TCocoaWidgetSet.ShowWindow');
  {$endif}

  case nCmdShow of
    SW_SHOW, SW_SHOWNORMAL:
      NSWindow(hwnd).orderFront(nil);
    SW_HIDE:
      NSWindow(hwnd).orderOut(nil);
    SW_MINIMIZE:
      NSWindow(hwnd).miniaturize(nil);
    SW_MAXIMIZE:
      NSWindow(hwnd).zoom(nil);
  end;
  Result:=true;
end;

function TCocoaWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal
  ): Boolean;
begin
  Result := StretchMaskBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
    SrcWidth, SrcHeight, 0, 0, 0, Rop);
end;

function TCocoaWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width,
  Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcCtx, DestCtx: TCocoaContext;
begin
  DestCtx := CheckDC(DestDC);
  SrcCtx := CheckDC(SrcDC);

  Result := Assigned(DestCtx) and Assigned(SrcCtx);

  if not Result then
    Exit;

  Result := DestCtx.StretchDraw(X, Y, Width, Height,
    SrcCtx, XSrc, YSrc, SrcWidth, SrcHeight,
    TCocoaBitmap(Mask), XMask, YMask, Rop);
end;

{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - Handle of window
           Rect   - Record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the screen bounding rectangle of the specified window
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetWindowRect(Handle: hwnd; var ARect: TRect): Integer;
var
  dx, dy: Integer;
begin
  if Handle <> 0 then
  begin
    ARect := NSObject(Handle).lclFrame;
    if not NSObject(Handle).isKindOfClass_(NSWindow) then
    begin
      dx := 0;
      dy := 0;
      NSObject(Handle).lclLocalToScreen(dx, dx);
      MoveRect(ARect, dx, dy);
    end;
    Result := 1;
  end else
    Result := 0;
end;

function TCocoaWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  if Handle<>0
    then Result:=NSObject(Handle).lclIsEnabled
    else Result:=False;
end;

function TCocoaWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  if Handle<>0
    then Result:=NSObject(Handle).lclIsVisible
    else Result:=False;
end;

function TCocoaWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
begin
  Result := Handle <> 0;
  if Result then
    ARect := NSObject(handle).lclClientFrame;
end;

function TCocoaWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
begin
  Result := Handle <> 0;
  if Result then
  begin
    ARect := NSObject(handle).lclClientFrame;
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
  end;
end;

function TCocoaWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) and Assigned(lpRect) then
  begin
    lpRect^ := ctx.GetClipRect;
    Result := COMPLEXREGION;
  end
  else
    Result := ERROR;
end;

function TCocoaWidgetSet.GetClipRGN(DC: hDC; RGN: hRGN): Longint;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) and (RGN <> 0) then
    Result := CocoaRegionTypeToWin32Map[ctx.CopyClipRegion(TCocoaRegion(RGN))]
  else
    Result := ERROR;
end;

function TCocoaWidgetSet.GetCursorPos(var lpPoint: TPoint ): Boolean;
begin
  with NSEvent.mouseLocation do
  begin
    lpPoint.x := Round(x);
    // cocoa returns cursor with inverted y coordinate
    lpPoint.y := Round(NSScreen.mainScreen.frame.size.height-y);
  end;
  Result := True;
end;

function TCocoaWidgetSet.GetMonitorInfo(hMonitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
var
  ScreenID: NSScreen absolute hMonitor;
begin
  Result := (lpmi <> nil) and (lpmi^.cbSize >= SizeOf(TMonitorInfo));
  if not Result then Exit;
  NSToLCLRect(ScreenID.frame, lpmi^.rcMonitor);
  NSToLCLRect(ScreenID.visibleFrame, lpmi^.rcWork);
  if ScreenID = NSScreen.mainScreen then
    lpmi^.dwFlags := MONITORINFOF_PRIMARY
  else
    lpmi^.dwFlags := 0;
end;

function TCocoaWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
var
  AObject: TCocoaGDIObject;
  DIB: TDIBSection;
  Width, Height, RequiredSize, i: Integer;

  APen: TCocoaPen absolute AObject;
  ALogPen: PLogPen absolute Buf;
  AExtLogPen: PExtLogPen absolute Buf;
begin
  Result := 0;

  AObject := CheckGDIObj(GDIObj);

  if AObject is TCocoaBitmap then
  begin
    if Buf = nil then
    begin
      Result := SizeOf(TDIBSection);
      Exit;
    end;

    Width := TCocoaBitmap(AObject).Width;
    Height := TCocoaBitmap(AObject).Height;

    FillChar(DIB, SizeOf(TDIBSection), 0);

    {dsBM - BITMAP}
    DIB.dsBm.bmType := $4D42;
    DIB.dsBm.bmWidth := Width;
    DIB.dsBm.bmHeight := Height;
    DIB.dsBm.bmWidthBytes := 0;
    DIB.dsBm.bmPlanes := 1;
    DIB.dsBm.bmBitsPixel := 32;
    DIB.dsBm.bmBits := nil;

    {dsBmih - BITMAPINFOHEADER}
    DIB.dsBmih.biSize := 40;
    DIB.dsBmih.biWidth := Width;
    DIB.dsBmih.biHeight := Height;
    DIB.dsBmih.biPlanes := DIB.dsBm.bmPlanes;
    DIB.dsBmih.biCompression := 0;
    DIB.dsBmih.biSizeImage := 0;
    DIB.dsBmih.biXPelsPerMeter := 0;
    DIB.dsBmih.biYPelsPerMeter := 0;
    DIB.dsBmih.biClrUsed   := 0;
    DIB.dsBmih.biClrImportant := 0;
    DIB.dsBmih.biBitCount := 32;

    if BufSize >= SizeOf(TDIBSection) then
    begin
      PDIBSection(Buf)^ := DIB;
      Result := SizeOf(TDIBSection);
    end
    else
      if BufSize > 0 then
      begin
        System.Move(DIB, Buf^, BufSize);
        Result := BufSize;
      end;
  end
  else
  if AObject is TCocoaPen then
  begin
    if APen.IsExtPen then
    begin
      RequiredSize := SizeOf(TExtLogPen);
      if Length(APen.Dashes) > 1 then
        inc(RequiredSize, (Length(APen.Dashes) - 1) * SizeOf(DWord));
      if Buf = nil then
        Result := RequiredSize
      else
      if BufSize >= RequiredSize then
      begin
        Result := RequiredSize;
        AExtLogPen^.elpPenStyle := APen.Style;
        if APen.IsGeometric then
        begin
          case APen.JoinStyle of
            kCGLineJoinRound:
              AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_ROUND;
            kCGLineJoinBevel:
              AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_BEVEL;
            kCGLineJoinMiter:
              AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_MITER;
          end;

          case APen.CapStyle of
            kCGLineCapRound:
              AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_ROUND;
            kCGLineCapSquare:
              AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_SQUARE;
            kCGLineCapButt:
              AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_FLAT;
          end;
          AExtLogPen^.elpWidth := APen.Width;
        end
        else
          AExtLogPen^.elpWidth := 1;

        AExtLogPen^.elpBrushStyle := BS_SOLID;
        AExtLogPen^.elpColor := APen.ColorRef;
        AExtLogPen^.elpHatch := 0;

        AExtLogPen^.elpNumEntries := Length(APen.Dashes);
        if AExtLogPen^.elpNumEntries > 0 then
        begin
          for i := 0 to AExtLogPen^.elpNumEntries - 1 do
            PDword(@AExtLogPen^.elpStyleEntry)[i] := Trunc(APen.Dashes[i]);
        end
        else
          AExtLogPen^.elpStyleEntry[0] := 0;
      end;
    end
    else
    begin
      if Buf = nil then
        Result := SizeOf(TLogPen)
      else
      if BufSize >= SizeOf(TLogPen) then
      begin
        Result := SizeOf(TLogPen);
        ALogPen^.lopnStyle := APen.Style;
        ALogPen^.lopnWidth := Types.Point(APen.Width, 0);
        ALogPen^.lopnColor := APen.ColorRef;
      end;
    end;
  end
end;

function TCocoaWidgetSet.GetParent(Handle : HWND): HWND;
begin
  if Handle<>0 then
    Result:=HWND(NSObject(Handle).lclParent)
  else
    Result:=0;
end;

function TCocoaWidgetSet.GetWindowRelativePosition(Handle: hwnd; var Left, Top: Integer): boolean;
begin
  Result := Handle <> 0;
  if Result then
    NSObject(handle).lclRelativePos(Left, Top);
end;

function TCocoaWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: Integer): boolean;
var
  r: TRect;
begin
  Result := Handle <> 0;
  if Result then
  begin
    r := NSObject(Handle).lclFrame;
    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
  end;
end;

function TCocoaWidgetSet.InvalidateRect(aHandle : HWND; Rect : pRect; bErase : Boolean): Boolean;
begin
  Result := aHandle <> 0;
  if Result then
  begin
    if Assigned(Rect) then
      NSObject(aHandle).lclInvalidateRect(Rect^)
    else
      NSObject(aHandle).lclInvalidate;
  end;
end;

function TCocoaWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
  Result := InvalidateRect(Handle, nil, False);
end;

function TCocoaWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
var
  PropStorage: TStringList;
  I: Integer;
begin
  if Handle <> 0 then
  begin
    PropStorage := NSObject(Handle).lclGetPropStorage;
    if Assigned(PropStorage) then
    begin
      I := PropStorage.IndexOf(Str);
      if I <> -1 then
        Result := PropStorage.Objects[I]
      else
        Result := nil;
    end
    else
      Result := nil;
  end;
end;

function TCocoaWidgetSet.GetRgnBox(RGN: HRGN; lpRect: PRect): Longint;
begin
  Result := ERROR;
  if Assigned(lpRect) then
    lpRect^ := Types.Rect(0, 0, 0, 0);

  if not (TObject(RGN) is TCocoaRegion) then
    Exit;

  if Assigned(lpRect) then
  begin
    lpRect^ := TCocoaRegion(RGN).GetBounds;
    Result := CocoaRegionTypeToWin32Map[TCocoaRegion(RGN).GetType];
  end;
end;

function TCocoaWidgetSet.GetROP2(DC: HDC): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
    Result := ctx.ROP2
  else
    Result := 0;
end;

function TCocoaWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean;
var
  PropStorage: TStringList;
begin
  Result := Handle <> 0;
  if Result then
  begin
    PropStorage := NSObject(Handle).lclGetPropStorage;
    Result := Assigned(PropStorage);
    if Result then
      PropStorage.AddObject(Str, TObject(Data));
  end;
end;

function TCocoaWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
  begin
    Result := ctx.ROP2;
    ctx.ROP2 := Mode;
  end
  else
    Result := 0;
end;

{----------------------------- WINDOWS SCROLLING ------------------------------}

function TCocoaWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
begin
  Result:=0;
end;

function TCocoaWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
begin
  Result:=False;
end;

function TCocoaWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
begin
  Result:=False;
end;

function TCocoaWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  Result := 0;

  case Value of
    BLACK_BRUSH:         // Black brush.
      Result := FStockBlackBrush;
    DKGRAY_BRUSH:        // Dark gray brush.
      Result := FStockDKGrayBrush;
    GRAY_BRUSH:          // Gray brush.
      Result := FStockGrayBrush;
    LTGRAY_BRUSH:        // Light gray brush.
      Result := FStockLtGrayBrush;
    NULL_BRUSH:          // Null brush (equivalent to HOLLOW_BRUSH).
      Result := FStockNullBrush;
    WHITE_BRUSH:         // White brush.
      Result := FStockWhiteBrush;

    BLACK_PEN:           // Black pen.
      Result := FStockBlackPen;
    NULL_PEN:            // Null pen.
      Result := FStockNullPen;
    WHITE_PEN:           // White pen.
      Result := FStockWhitePen;

    {System font. By default, Windows uses the system font to draw menus,
     dialog box controls, and text. In Windows versions 3.0 and later,
     the system font is a proportionally spaced font; earlier versions of
     Windows used a monospace system font.}
{    DEFAULT_GUI_FONT, SYSTEM_FONT:
      begin

        If FStockSystemFont <> 0 then
        begin
          DeleteObject(FStockSystemFont);
          FStockSystemFont := 0;
        end;

        If FStockSystemFont = 0 then
          FStockSystemFont := CreateDefaultFont;
        Result := FStockSystemFont;
      end;}
  end;
end;

function SysColorToNSColor(nIndex: Integer): NSColor;
begin
  case NIndex of
    COLOR_GRADIENTACTIVECAPTION, COLOR_ACTIVECAPTION,
    COLOR_WINDOWFRAME, COLOR_ACTIVEBORDER:
      Result := NSColor.windowFrameColor;
    COLOR_GRADIENTINACTIVECAPTION, COLOR_INACTIVECAPTION, COLOR_INACTIVEBORDER:
      Result := NSColor.windowBackgroundColor;
    COLOR_CAPTIONTEXT,
    COLOR_INACTIVECAPTIONTEXT:
      Result := NSColor.windowFrameTextColor;
    COLOR_BACKGROUND,
    COLOR_WINDOW, COLOR_FORM:
      Result := NSColor.windowBackgroundColor;
    COLOR_MENU:
      Result := NSColor.controlBackgroundColor;
    COLOR_MENUTEXT:
      Result := NSColor.controlTextColor;
    COLOR_MENUBAR:
      Result := NSColor.selectedTextBackgroundColor;
    COLOR_MENUHILIGHT:
      Result := NSColor.selectedMenuItemColor;
    COLOR_INFOTEXT, COLOR_WINDOWTEXT:
      Result := NSColor.controlTextColor;
    COLOR_APPWORKSPACE:
      Result := NSColor.windowBackgroundColor;
    COLOR_HIGHLIGHT,
    COLOR_HOTLIGHT:
      Result := NSColor.alternateSelectedControlColor;
    COLOR_HIGHLIGHTTEXT:
      Result := NSColor.alternateSelectedControlTextColor;
    COLOR_SCROLLBAR:
      Result := NSColor.scrollBarColor;
    COLOR_BTNFACE:
      Result := NSColor.controlColor;
    COLOR_BTNSHADOW:
      Result := NSColor.controlShadowColor;
    COLOR_BTNHIGHLIGHT:
      Result := NSColor.controlHighlightColor;
    COLOR_BTNTEXT:
      Result := NSColor.controlTextColor;
    COLOR_GRAYTEXT:
      Result := NSColor.disabledControlTextColor;
    COLOR_3DDKSHADOW:
      Result := NSColor.controlDarkShadowColor;
    COLOR_3DLIGHT:
      Result := NSColor.controlLightHighlightColor;
    COLOR_INFOBK:
      Result := NSColor.colorWithCalibratedRed_green_blue_alpha(249 / $FF, 252 / $FF, 201 / $FF, 1);
  else
    Result := nil;
  end;
end;

function TCocoaWidgetSet.GetSysColor(nIndex: Integer): DWORD;
var
  Color: NSColor;
  SysBrush: HBrush;
begin
  // 1. get the system brush - it has a NSColor reference
  SysBrush := GetSysColorBrush(nIndex);
  if SysBrush = 0 then
  begin
    Result := 0;
    Exit;
  end;

  Color := TCocoaBrush(SysBrush).Color;

  if Assigned(Color) then
    Result := NSColorToColorRef(Color)
  else
    Result := 0;
end;

function TCocoaWidgetSet.GetSysColorBrush(nIndex: Integer): HBRUSH;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    Result := 0;
    Exit;
  end;

  if (FSysColorBrushes[nIndex] = 0) then
    FSysColorBrushes[nIndex] := HBrush(TCocoaBrush.Create(SysColorToNSColor(nIndex), True));
  Result := FSysColorBrushes[nIndex]
end;

function TCocoaWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer; ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
begin
  Result:=0;
end;

function TCocoaWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
begin
  Result:=False;
end;

{----------------------------------- DRAWING ----------------------------------}


type
  TPointArray = array [word] of TPoint;
  PPointArray = ^TPointArray;

function TCocoaWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  ctx.LineTo(x,y);
end;

function TCocoaWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Result then Exit;

  if Assigned(OldPoint) then OldPoint^:=ctx.PenPos;
  ctx.MoveTo(x,y);
end;

function TCocoaWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer; Winding: boolean): boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx) and Assigned(Points) and (NumPts>=2);
  if not Result then Exit;

  ctx.Polygon(PPointArray(Points)^, NumPts, Winding);
  Result:=True;
end;

function TCocoaWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx) and Assigned(Points) and (NumPts>0);
  if not Result then Exit;

  ctx.Polyline(PPointArray(Points)^, NumPts);
  Result:=True;
end;

function TCocoaWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal;
  wParam: WParam; lParam: LParam): Boolean;
var
  Info: NSDictionary;
  Event: NSEvent;
begin
  Result := Handle <> 0;
  if Result then
  begin
    Info := PrepareUserEventInfo(Handle, Msg, WParam, LParam);
    // if we will want a postmessage using notification center
    // NSDistributedNotificationCenter.defaultCenter.postNotificationName_object_userInfo_deliverImmediately(NSMessageNotification, nil, Info, False);
    Event := PrepareUserEvent(Handle, Info);
    NSApp.postEvent_atStart(Event, False);
  end;
end;

function TCocoaWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx);
  if Result then
    ctx.Rectangle(X1, Y1, X2, Y2, False, nil);
end;

{------------------------------- SYNC OBJECTS ---------------------------------}

procedure TCocoaWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
begin
  CritSection:=TCriticalSection(NSRecursiveLock.alloc);
end;

procedure TCocoaWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
begin
  if CritSection=0 then Exit;
  NSRecursiveLock(CritSection).release;
  CritSection:=0;
end;

function TCocoaWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  Result := hDC <> 0;
  if Result then
    TCocoaContext(hDC).Free;
end;

procedure TCocoaWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
begin
  if CritSection=0 then Exit;
  NSRecursiveLock(CritSection).lock;
end;

procedure TCocoaWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
begin
  if CritSection=0 then Exit;
  NSRecursiveLock(CritSection).unlock;
end;

{------------------------------- DEVICE CONTEXT -------------------------------}

function TCocoaWidgetSet.GetDC(hWnd: HWND): HDC;
var
  ctx: TCocoaContext;
begin
  if hWnd=0 then
  begin
    if not Assigned(ScreenContext) then ScreenContext:=TCocoaContext.Create;
    Result:=HDC(ScreenContext);
  end
  else
  begin
    // ToDo: Not finished yet
    Result := 0;

{    if NSObject(hWnd) is TCocoaGroupBox then ctx := TCocoaGroupBox(hWnd).Context;

    if ctx <> nil then
      Result := HDC(ctx)
    // use dummy context if we are outside paint event
    else
    begin}
      ctx := TCocoaContext.Create({NSObject(HWnd)});
      Result := HDC(ctx);
{    end;}
  end;

  {$IFDEF VerboseWinAPI}
    DebugLn('[TCocoaWidgetSet.GetDC] hWnd: %x Result: %x', [hWnd, Result]);
  {$ENDIF}
end;

function TCocoaWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if not Assigned(ctx) then
    Exit(0);

  // todo: change implementation for printers
  case Index of
    HORZSIZE:
      Result := Round(NSScreen.mainScreen.frame.size.width / 72 * 25.4);
    VERTSIZE:
      Result := Round(NSScreen.mainScreen.frame.size.height / 72 * 25.4);
    HORZRES:
      Result := Round(NSScreen.mainScreen.frame.size.width);
    BITSPIXEL:
      Result := CGDisplayBitsPerPixel(CGMainDisplayID);
    PLANES:
      Result := 1;
    SIZEPALETTE:
      Result := 0;
    LOGPIXELSX:
      Result := 72;
    LOGPIXELSY:
      Result := 72;
    VERTRES:
      Result := Round(NSScreen.mainScreen.frame.size.height);
    NUMRESERVED:
      Result := 0;
  else
    DebugLn('TCocoaWidgetSet.GetDeviceCaps TODO Index: ' + DbgS(Index));
  end;
end;

function TCocoaWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx);
  if Result then
    with ctx.Size do
    begin
      P.X := cx;
      P.Y := cy;
    end;
end;

function TCocoaWidgetSet.GetFocus: HWND;
var
  Obj: NSObject;
begin
  Result := HWND(NSApp.keyWindow);
  if Result <> 0 then
  begin
    Obj := NSWindow(Result).firstResponder;
    if Assigned(Obj) and Obj.isKindOfClass(NSView) then
      Result := HWND(Obj);
  end;
end;

function TCocoaWidgetSet.GetForegroundWindow: HWND;
//var
//  App: NSRunningApplication;
begin
  // return the currect active window in the system
{ this is not possible because we can't access another application NSApplication
  for App in NSWorkSpace.sharedWorkspace.runningApplications do
    if App.isActive then
    begin
      Result := HWND(App.keyWindow);
      Exit;
    end;
}
  if NSApp.isActive then
    Result := HWND(NSApp.keyWindow)
  else
    Result := 0;
end;

function TCocoaWidgetSet.SelectObject(ADC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  dc: TCocoaContext;
  gdi: TCocoaGDIObject;
const
  SName = 'TCarbonWidgetSet.SelectObject';
begin
  {$IFDEF VerboseWinAPI}
    DebugLn(Format('TCocoaWidgetSet.SelectObject DC: %x GDIObj: %x', [ADC, GDIObj]));
  {$ENDIF}
  Result := 0;

  dc:=CheckDC(ADC);
  gdi:=CheckGDIOBJ(GDIObj);
  if not Assigned(dc) then Exit;

  if gdi is TCocoaBrush then
  begin // select brush
    Result := HBRUSH(dc.Brush);
    dc.Brush := TCocoaBrush(gdi);
  end else if gdi is TCocoaPen then
  begin // select pen
    Result := HPEN(dc.Pen);
    dc.Pen := TCocoaPen(gdi);
  end else if gdi is TCocoaFont then
  begin // select font
    Result := HFONT(dc.Font);
    dc.Font := TCocoaFont(gdi);
  end else if gdi is TCocoaRegion then
  begin // select region
    Result := HRGN(dc.Region);
    dc.Region := TCocoaRegion(gdi);
  end else if gdi is TCocoaBitmap then
  begin // select bitmap
    {if not (ADC is TCarbonBitmapContext) then
    begin
      DebugLn(SName + ' Error - The specified device context is not bitmap context!');
      Exit;
    end;}
    Result := HBITMAP(dc.Bitmap);
    dc.Bitmap:=TCocoaBitmap(gdi);
    //TCarbonBitmapContext(ADC).Bitmap := TCarbonBitmap(GDIObj);
  end;

  if Result<>0 then TCocoaGDIObject(Result).Release;
  if Assigned(gdi) then gdi.AddRef;

  {$IFDEF VerboseWinAPI}
    DebugLn(Format('TCocoaWidgetSet.SelectObject Result: %x', [Result]));
  {$ENDIF}
end;

function TCocoaWidgetSet.SendMessage(Handle: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): LResult;
var
  Info: NSDictionary;
  Event: NSEvent;
begin
  if Handle <> 0 then
  begin
    Info := PrepareUserEventInfo(Handle, Msg, WParam, LParam);
    Event := PrepareUserEvent(Handle, Info);
    NSApp.sendEvent(Event);
    Result := NSNumber(Info.objectForKey(NSMessageResult)).integerValue;
  end;
end;

function TCocoaWidgetSet.SetActiveWindow(Handle: HWND): HWND;
var
  Obj: NSObject;
begin
  Obj := NSObject(Handle);
  if Assigned(Obj) and NSApp.isActive then
  begin
    Result := HWND(NSApp.keyWindow);
    if Obj.isKindOfClass(NSWindow) then
      NSwindow(Obj).makeKeyWindow
    else
    if Obj.isKindOfClass(NSView) then
      NSView(Obj).window.makeKeyWindow
    else
      Result := 0;
  end;
end;

function TCocoaWidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
  begin
    Result := ctx.BkColor;
    ctx.BkColor := Color;
  end
  else
    Result := CLR_INVALID;
end;

function TCocoaWidgetSet.SetBkMode(DC: HDC; bkMode: Integer): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
  begin
    Result := ctx.BkMode;
    ctx.BkMode := bkMode;
  end
  else
    Result := 0;
end;

function TCocoaWidgetSet.RectVisible(DC: HDC; const ARect: TRect): Boolean;
var
  ClipBox: CGRect;
  ctx : TCocoaContext;
  R: TRect;
begin
  ctx := CheckDC(DC);
  Result := Assigned(ctx) and (ARect.Right > ARect.Left) and (ARect.Bottom > ARect.Top);

  if not Result then Exit;

  // In Quartz 2D there is no direct access to clipping path of CGContext,
  // therefore we can only test bounding box of the clipping path.

  ClipBox := CGContextGetClipBoundingBox(ctx.CGContext);
  Result := IntersectRect(R, ARect, CGRectToRect(ClipBox));
end;

function TCocoaWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
var
  ctx : TCocoaContext;
begin
  Result := False;
  ctx:=CheckDC(DC);
  if not Assigned(ctx) then Exit;

  {$IFDEF VerboseWinAPI}
  DebugLn('TCarbonWidgetSet.MoveWindowOrgEx DC: ' + DbgS(DC) + ' ' + DbgS(DX) + ', ' + DbgS(DY));
  {$ENDIF}
  ctx.SetOrigin(dX, dY);
  Result := True;
end;

function TCocoaWidgetSet.GetWindowOrgEx(dc : hdc; P : PPoint): Integer;
var
  ctx : TCocoaContext;
begin
  ctx:=CheckDC(dc);
  if not Assigned(ctx) or not Assigned(P) then
    Result:=0
  else begin
    ctx.GetOrigin(p^.X, p^.Y);
    Result:=1;
  end;
end;

function TCocoaWidgetSet.SetCursor(ACursor: HCURSOR): HCURSOR;
begin
  Result := HCURSOR(TCocoaCursor(ACursor).Install);
end;

function TCocoaWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
var
  CursorPos: CGPoint;
begin
  Result := False;

  CursorPos.X := X;
  CursorPos.Y := Y;
  if CGWarpMouseCursorPosition(CursorPos) <> noErr then Exit;
  Result := True;
end;

function TCocoaWidgetSet.SetFocus(Handle: HWND): HWND;
var
  Obj: NSObject;
begin
  if Handle <> 0 then
  begin
    if Result = Handle then
      Exit;
    Obj := NSObject(Handle);
    if Obj.isKindOfClass(NSWindow) then
    begin
      NSWindow(Obj).makeKeyWindow;
      NSWindow(Obj).makeFirstResponder(nil);
    end
    else
    if Obj.isKindOfClass(NSView) then
    begin
      NSView(Obj).window.makeKeyWindow;
      NSView(Obj).window.makeFirstResponder(NSView(Obj));
    end;
  end;
end;

function TCocoaWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
var
  Obj: NSObject;
begin
  Result := HWnd <> 0;
  if Result then
  begin
    NSApp.activateIgnoringOtherApps(True);
    Obj := NSObject(HWnd);
    if Obj.isKindOfClass(NSWindow) then
      NSwindow(Obj).makeKeyAndOrderFront(NSApp)
    else
    if Obj.isKindOfClass(NSView) then
      NSView(Obj).window.makeKeyAndOrderFront(NSApp)
    else
      Result := False;
  end;
end;

{------------------------------- FONT AND TEXT --------------------------------}

function TCocoaWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
  begin
    Result := ctx.TextColor;
    ctx.TextColor := Color
  end
  else
    Result := CLR_INVALID;
end;

{------------------------------------------------------------------------------
  Method:  GetSystemMetrics
  Params:  NIndex - System metric to retrieve
  Returns: The requested system metric value

  Retrieves various system metrics.
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
begin
  Result := 0;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.GetSystemMetrics NIndex: ' + DbgS(NIndex));
  {$ENDIF}

  case NIndex of
{    SM_CXHSCROLL,
    SM_CYHSCROLL,
    SM_CXVSCROLL,
    SM_CYVSCROLL:
      Result := 10;//GetCarbonThemeMetric(kThemeMetricScrollBarWidth);}
    SM_CXSCREEN,
    SM_CXVIRTUALSCREEN: Result := Round(NSScreen.mainScreen.frame.size.width);
    SM_CYSCREEN,
    SM_CYVIRTUALSCREEN: Result := Round(NSScreen.mainScreen.frame.size.height);
    SM_XVIRTUALSCREEN: Result := Round(NSScreen.mainScreen.frame.origin.x);
    SM_YVIRTUALSCREEN: Result := Round(NSScreen.mainScreen.frame.origin.y);
    SM_CXSMICON,
    SM_CYSMICON:
      Result := 16;
    SM_CXICON,
    SM_CYICON:
      Result := 128;
    SM_CXCURSOR,
    SM_CYCURSOR:
      begin
{        if TCarbonCursor.HardwareCursorsSupported then
          Result := 64 else}
          Result := 16;
      end;
{    SM_CXHTHUMB:
      Result := 16;//GetCarbonThemeMetric(kThemeMetricScrollBarMinThumbWidth);
    SM_CYVTHUMB:
      Result := 16;//GetCarbonThemeMetric(kThemeMetricScrollBarMinThumbHeight);}
    SM_SWSCROLLBARSPACING:
      Result:=0;
  else
    DebugLn('TCocoaWidgetSet.GetSystemMetrics TODO ', DbgS(NIndex));;
  end;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.GetSystemMetrics Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCocoaWidgetSet.GetTextColor(DC: HDC) : TColorRef;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
    Result := ctx.TextColor
  else
    Result := CLR_INVALID;
end;

{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  DC    - Handle of device context
           Str   - Text string
           Count - Number of characters in string
           Size  - The record for the dimensions of the string
  Returns: If the function succeeds

  Computes the width and height of the specified string of text
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  ctx : TCocoaContext;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn('[TCocoaWidgetSet.GetTextExtentPoint] DC: %x Str: %s Count: %d', [DC, Str, Count]);
  {$ENDIF}
  ctx:=CheckDC(DC);
  Result:=Assigned(ctx);
  if not Assigned(ctx) then Exit(False);
  Result := ctx.GetTextExtentPoint(Str, Count, Size);
  {$IFDEF VerboseWinAPI}
    DebugLn('[TCocoaWidgetSet.GetTextExtentPoint] Size: %d,%d', [Size.cx, Size.cy]);
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  GetTextMetrics
  Params:  DC - Handle of device context
           TM - The Record for the text metrics
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font
  TODO: get exact max. and av. char width, pitch and charset
 ------------------------------------------------------------------------------}
function TCocoaWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  ctx : TCocoaContext;
begin
  Result := False;

  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.GetTextMetrics DC: ' + DbgS(DC));
  {$ENDIF}

  ctx:=CheckDC(DC);
  if not Assigned(ctx) then Exit(False);
  Result := ctx.GetTextMetrics(TM);

  {$IFDEF VerboseWinAPI}
    DebugLn('TCocoaWidgetSet.GetTextMetrics Result: ' + DbgS(Result) +
      ' TextMetric: ' + DbgS(TM));
  {$ENDIF}
end;

function TCocoaWidgetSet.TextOut(DC: HDC; X,Y: Integer; Str: Pchar; Count: Integer) : Boolean;
begin
  Result := ExtTextOut(DC, X, Y, 0, nil, Str, Count, nil);
end;

function TCocoaWidgetSet.SaveDC(DC: HDC): Integer;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
    Result := ctx.SaveDC
  else
    Result:=0;
end;

function TCocoaWidgetSet.ScreenToClient(Handle: HWND; var P: TPoint): Integer;
begin
  Result := Ord(Handle <> 0);

  if Result = 1 then
  begin
    // 1. convert screen to window
    NSObject(Handle).lclScreenToLocal(P.X, P.Y);
    // 2. convert window to client
    with NSObject(Handle).lclClientFrame do
    begin
      dec(P.X, Left);
      dec(P.Y, Top);
    end;
  end;
end;

function TCocoaWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
begin
  Result := ExtSelectClipRgn(DC, RGN, RGN_COPY);
end;

function TCocoaWidgetSet.SetSysColors(cElements: Integer; const lpaElements; const lpaRgbValues): Boolean;
var
  n: Integer;
  Element: LongInt;
  Color: NSColor;
begin
  Result := False;
  if cElements > MAX_SYS_COLORS then Exit;

  for n := 0 to cElements - 1 do
  begin
    Element := PInteger(@lpaElements)[n];
    if (Element > MAX_SYS_COLORS) or (Element < 0) then 
      Exit;
    Color := ColorToNSColor(PDWord(@lpaRgbValues)[n]);
    if (FSysColorBrushes[Element] <> 0) then
      TCocoaBrush(FSysColorBrushes[Element]).Color := Color
    else
      FSysColorBrushes[Element] := HBrush(TCocoaBrush.Create(Color, True));
  end;

  Result := True;
end;

function TCocoaWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  ctx: TCocoaContext;
begin
  ctx := CheckDC(DC);
  if Assigned(ctx) then
    Result := ctx.RestoreDC(SavedDC)
  else
    Result := False;
end;

function TCocoaWidgetSet.RoundRect(DC: HDC; X1, Y1, X2, Y2: Integer; RX,
  RY: Integer): Boolean;
begin
  Result:=inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
