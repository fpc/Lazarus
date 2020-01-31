{%MainUnit customdrawnint.pp}
{******************************************************************************
  All CustomDrawn backend independent Winapi implementations.

  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: Arc
  Params: DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Arc(DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDWinAPI}
  DebugLn(Format(':>[WinAPI Polygon] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then Exit(False);

  //LazDC.Arc(...);
  Result := True;
end;

(*{------------------------------------------------------------------------------
  Function: AngleChord
  Params: DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.AngleChord(DC: HDC; x1, y1, x2, y2, angle1, angle2: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI AngleChord] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC);
  if Result then
    QPainter_drawChord(TQtDeviceContext(DC).Widget, x1, y1, x2, y2, Angle1, Angle2);
end;*)

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TCDWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}
  Result := 0;

  if Handle = 0 then Exit;

  (*  Widget := TQtWidget(Handle);
  if Widget <> nil then
    DC := TQtDeviceContext.Create(Widget.PaintData.PaintWidget, True)
  else
    DC := TQtDeviceContext.Create(nil, True);

  PS.hdc := HDC(DC);

  if Handle<>0 then
  begin
    // if current handle has paintdata information,
    // setup hdc with it
    //DC.DebugClipRect('BeginPaint: Before');
    if Widget.PaintData.ClipRegion <> nil then
    begin
      //Write('>>> Setting Paint ClipRegion: ');
      //DebugRegion('PaintData.ClipRegion: ', Widget.PaintData.ClipRegion);
      DC.setClipRegion(Widget.PaintData.ClipRegion);
      DC.setClipping(True);
    end;
    if Widget.PaintData.ClipRect <> nil then
    begin
      New(DC.vClipRect);
      DC.vClipRect^ := Widget.PaintData.ClipRect^;
    end;
  end;

  Result := PS.hdc;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}*)
end;

function TCDWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  {$ifdef VerboseCDDrawing}
    WriteLn('Trace:> [TCDWidgetSet.BitBlt]');
  {$endif}

  Result := StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width,
                       Height, ROP);

  {$ifdef VerboseCDDrawing}
    WriteLn('Trace:< [TCDWidgetSet.BitBlt]');
  {$endif}
end;

(*function TQtWidgetSet.CallNextHookEx(hHk: HHOOK; ncode : Integer; wParam: WParam; lParam : LParam) : Integer;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  	WriteLn('***** [WinAPI TQtWidgetSet.CallNextHookEx] missing implementation ');
  {$endif}
  Result := 0;
end;

function TQtWidgetSet.CallWindowProc(lpPrevWndFunc : TFarProc; Handle : HWND; Msg : UINT; wParam: WParam; lParam : lParam) : Integer;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  	WriteLn('***** [WinAPI TQtWidgetSet.CallWindowProc] missing implementation ');
  {$endif}
  Result := -1;
end;

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint) : Boolean;
var
  APoint: TQtPoint;
  Pt: TPoint;
begin
  Result := IsValidHandle(Handle);
  if Result then
  begin
    APoint := QtPoint(P.X, P.Y);

    QWidget_mapToGlobal(TQtWidget(Handle).GetContainerWidget, @APoint, @APoint);
    if TQtWidget(Handle).ChildOfComplexWidget = ccwScrollingWinControl then
    begin
      Pt := TQtCustomControl(Handle).viewport.ScrolledOffset;
      dec(APoint.X, Pt.X);
      dec(APoint.Y, Pt.Y);
    end;
    P := Point(APoint.x, APoint.y);
  end;
end;*)

{------------------------------------------------------------------------------
  Method:  ClipboardFormatToMimeType
  Params:  FormatID - a registered format identifier (can't be a predefined format)
  Returns: the corresponding mime type as string
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  {$ifdef VerboseCDClipboard}
  DebugLn(Format('[TCDWidgetSet.ClipboardFormatToMimeType] FormatID=%d', [FormatID]));
  {$endif}
  if FClipBoardFormats.Count > Integer(FormatID) then
    Result := FClipBoardFormats[FormatID]
  else
    Result := '';
end;

function TCDWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
var
  i: Integer;
  Str: string;
begin
  {$ifdef VerboseCDClipboard}
  DebugLn('[TCDWidgetSet.GenericClipboardGetFormats]');
  {$endif}
  Result := False;
  Count := 0;
  List := nil;

  Count := FClipBoardFormats.Count;
  GetMem(List, Count * SizeOf(TClipboardFormat));

  for i := 0 to Count - 1 do
  begin
    Str := FClipBoardFormats.Strings[i];
    List[i] := ClipboardRegisterFormat(Str);
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  ClipboardRegisterFormat
  Params:  AMimeType - a string (usually a MIME type) identifying a new format
                       type to register
  Returns: the registered Format identifier (TClipboardFormat)
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
var
  Index: Integer;
begin
  Index := FClipBoardFormats.IndexOf(AMimeType);
  if Index < 0 then
    Index := FClipBoardFormats.Add(AMimeType);
  Result := Index;
  {$ifdef VerboseCDClipboard}
  DebugLn(Format('[TCDWidgetSet.ClipboardRegisterFormat] AMimeType=%s Result=%d', [AMimeType, Index]));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: CombineRgn
  Params:  Dest, Src1, Src2, fnCombineMode
  Returns: longint

  Combine the 2 Source Regions into the Destination Region using the specified
  Combine Mode. The Destination must already be initialized. The Return value
  is the Destination's Region type, or ERROR.

  The Combine Mode can be one of the following:
      RGN_AND  : Gets a region of all points which are in both source regions

      RGN_COPY : Gets an exact copy of the first source region

      RGN_DIFF : Gets a region of all points which are in the first source
                 region but not in the second.(Source1 - Source2)

      RGN_OR   : Gets a region of all points which are in either the first
                 source region or in the second.(Source1 + Source2)

      RGN_XOR  : Gets all points which are in either the first Source Region
                 or in the second, but not in both.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;
var
  DestRgn: TLazRegion absolute Dest;
  Src1Rgn: TLazRegion absolute Src1;
  Src2Rgn: TLazRegion absolute Src2;
begin
  Result := ERROR;

  if not IsValidGDIObject(Dest) or not IsValidGDIObject(Src1) then Exit;

  if (fnCombineMode<>RGN_COPY) and not IsValidGDIObject(Src2) then Exit;

  // If the operation is a copy, execute it now, as it will not involve Src2
  // The common code would not work in this case
  if fnCombineMode = RGN_COPY then
  begin
    if Dest <> Src1 then DestRgn.Assign(Src1Rgn);
    Result := DestRgn.GetRegionKind();
    Exit;
  end;

  // Now operations which involve Src2, consider both cases: Dest=Src1 and Dest<>Src1
  if Dest = Src1 then
    DestRgn.CombineWith(Src2Rgn, fnCombineMode)
  else
  begin
    DestRgn.Assign(Src1Rgn);
    DestRgn.CombineWith(Src2Rgn, fnCombineMode);
  end;

  Result := DestRgn.GetRegionKind();
end;

{------------------------------------------------------------------------------
  Method:  CreateBitmap
  Params:
  Returns:

  This functions is for TBitmap support.
  Specifically it is utilized on when a handle for a bitmap is needed
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  lRawImage: TRawImage;
  lMask: HBitmap;
  NewBits: Pointer;
  NewBitsSize: PtrUInt;
  RSS: PtrUInt;
  ARowStride: PtrUInt;
begin
  {$ifdef VerboseCDBitmap}
    DebugLn('Trace:> [WinAPI CreateBitmap]',
     ' Width:', dbgs(Width),
     ' Height:', dbgs(Height),
     ' Planes:', dbgs(Planes),
     ' BitCount:', dbgs(BitCount),
     ' BitmapBits: ', dbgs(BitmapBits));
  {$endif}

  // for win32 data is aligned to WORD
  // for ARM speed optimization the best is realign data to DWORD

  Result := 0;
  NewBits := nil;
  lRawImage.Init;

  case BitCount of
    1: lRawImage.Description.Init_BPP1(Width, Height);
    15, 16: lRawImage.Description.Init_BPP16_R5G6B5(Width, Height);
    24: lRawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(Width, Height);
    32: lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(Width, Height);
  else
    lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(Width, Height);
  end;

  RSS := GetBytesPerLine(Width, BitCount, rileWordBoundary);
  if BitmapBits <> nil then
  begin
    {$ifdef VerboseCDBitmap}
      DebugLn('Trace: [WinAPI CreateBitmap] BitmapBits <> nil');
    {$endif}
    ARowStride := GetBytesPerLine(Width, BitCount, rileDWordBoundary);
    if not CopyImageData(Width, Height, RSS, BitCount, BitmapBits, Types.Rect(0, 0, Width, Height),
      riloBottomToTop, riloBottomToTop, rileDWordBoundary, NewBits, NewBitsSize) then
    begin
      {$ifdef VerboseCDBitmap}
        DebugLn('Trace: [WinAPI CreateBitmap] CopyImageData failed');
      {$endif}
      // this was never tested
      ARowStride := RSS;
      NewBitsSize := RSS * Height;
      NewBits := AllocMem(NewBitsSize);
      System.Move(BitmapBits^, NewBits^, NewBitsSize);
    end;
    lRawImage.Data := NewBits;
    lRawImage.DataSize := NewBitsSize;
    //Result := HBitmap(TQtImage.Create(NewBits, Width, Height, ARowStride, Format, True));
    RawImage_CreateBitmaps(lRawImage, Result, lMask, True);
  end
  else
  begin
    {$ifdef VerboseCDBitmap}
      DebugLn('Trace: [WinAPI CreateBitmap] Creating Data');
    {$endif}
    lRawImage.CreateData(True);
    RawImage_CreateBitmaps(lRawImage, Result, lMask, True);
  end;

  {$ifdef VerboseCDBitmap}
    DebugLn('Trace:< [WinAPI CreateBitmap] Bitmap:', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  lBrush: TFPCustomBrush;
begin
  lBrush := TFPCustomBrush.Create;
  Result := HBRUSH(lBrush);

  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[TCDWidgetSet.CreateBrushIndirect]  Style: %d, Color: %8x Result:%x',
      [LogBrush.lbStyle, LogBrush.lbColor, Result]));
  {$endif}

  // brush color
  lBrush.FPColor := TColorToFPColor(LogBrush.lbColor);

  // brush style
  case LogBrush.lbStyle of
    BS_NULL:  lBrush.Style := bsClear; // Same as BS_HOLLOW.
    BS_SOLID: lBrush.Style := bsSolid;
{    BS_HATCHED: // Hatched brushes.
    begin
      case LogBrush.lbHatch of
        HS_BDIAGONAL: QtBrush.Style := QtBDiagPattern;
        HS_CROSS: QtBrush.Style := QtCrossPattern;
        HS_DIAGCROSS: QtBrush.Style := QtDiagCrossPattern;
        HS_FDIAGONAL: QtBrush.Style := QtFDiagPattern;
        HS_HORIZONTAL: QtBrush.Style := QtHorPattern;
        HS_VERTICAL: QtBrush.Style := QtVerPattern;
      else
        QtBrush.Style := QtSolidPattern;
      end;
    end;

    BS_DIBPATTERN,     // A pattern brush defined by a device-independent
           // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERN, the
           // lbHatch member contains a handle to a packed DIB.Windows 95:
           // Creating brushes from bitmaps or DIBs larger than 8x8 pixels
           // is not supported. If a larger bitmap is given, only a portion
           // of the bitmap is used.
    BS_DIBPATTERN8X8,  // Same as BS_DIBPATTERN.
    BS_DIBPATTERNPT,   // A pattern brush defined by a device-independent
           // bitmap (DIB) specification. If lbStyle is BS_DIBPATTERNPT, the
           // lbHatch member contains a pointer to a packed DIB.
    BS_PATTERN,        // Pattern brush defined by a memory bitmap.
    BS_PATTERN8X8:     // Same as BS_PATTERN.
    begin
      QtBrush.setTextureImage(TQtImage(LogBrush.lbHatch).FHandle);
      QtBrush.Style := QtTexturePattern;
    end;  }
  else
    DebugLn(Format('Unsupported Brush Style %d',[LogBrush.lbStyle]));
  end;

  {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI CreateBrushIndirect] Result: ', dbghex(Result));
  {$endif}
end;

(*function TQtWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; Width, Height: Integer): Boolean;
begin
  Result := (Handle <> 0) and
    QtCaret.CreateCaret(TQtWidget(Handle), Bitmap, Width, Height);
end;*)

{ In LCL-CustomDrawn it is completely irrelevant if a Bitmap is compatible with the screen,
  so just create any standard bitmap }
function TCDWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  Result := CreateBitmap(Width, Height, 1, 32, nil);
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.

  This is utilized for example for creating a Canvas for a Bitmap, by later using
  SelectObject to select the bitmap
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI CreateCompatibleDC] DC: ', dbghex(DC));
  {$endif}
  Result := HDC(TLazCanvas.Create(nil));
end;

{------------------------------------------------------------------------------
  Function: CreateEllipticRgn
  Params:  p1 - X position of the top-left corner
           p2 - Y position of the top-left corner
           p3 - X position of the bottom-right corner
           p4 - Y position of the bottom-right corner
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
var
  lRegion: TLazRegion;
begin
  {$ifdef VerboseCDRegions}
    DebugLn('[WinAPI CreateEllipticRgn] ');
  {$endif}
  lRegion := TLazRegion.Create;
  lRegion.AddEllipse(p1, p2, p3, p4);
  Result := HRGN(lRegion);
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := CreateFontIndirectEx(LogFont, '');
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirectEx
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
var
  lFont: TLazCDCustomFont;
//  FamilyName: string;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.CreateFontIndirectEx]  LongFontName=%s lfHeight=%d',
      [LongFontName, LogFont.lfHeight]));
  {$endif}

  lFont := TLazCDCustomFont.Create;
  Result := HFONT(lFont);

  {$ifndef CD_UseNativeText}
  lFont.ftFont.Name := BackendGetFontPath(LogFont, LongFontName);
  lFont.ftFont.Hinted := true;
  lFont.ftFont.ClearType := true;
  lFont.ftFont.Quality := grqHighQuality;
  {$endif}

(*const
  QStyleStategy: array [DEFAULT_QUALITY..CLEARTYPE_NATURAL_QUALITY] of QFontStyleStrategy = (
 { DEFAULT_QUALITY           } QFontPreferDefault,
 { DRAFT_QUALITY             } QFontPreferMatch,
 { PROOF_QUALITY             } QFontPreferQuality,
 { NONANTIALIASED_QUALITY    } QFontNoAntialias,
 { ANTIALIASED_QUALITY       } QFontPreferAntialias,
 { CLEARTYPE_QUALITY         } QFontPreferAntialias,
 { CLEARTYPE_NATURAL_QUALITY } QFontPreferAntialias
  );*)

  lFont.Size := Abs(LogFont.lfHeight);

(*  // Some values at available on Qt documentation at a table
  // Others are guesses. The best would be to test different values for those
  // See: http://doc.trolltech.com/4.1/qfont.html#Weight-enum
  case LogFont.lfWeight of
    FW_THIN       : QtFont.setWeight(10);
    FW_EXTRALIGHT : QtFont.setWeight(15);
    FW_LIGHT      : QtFont.setWeight(25);
    FW_NORMAL     : QtFont.setWeight(50);
    FW_MEDIUM     : QtFont.setWeight(55);
    FW_SEMIBOLD   : QtFont.setWeight(63);
    FW_BOLD       : QtFont.setWeight(75);
    FW_EXTRABOLD  : QtFont.setWeight(80);
    FW_HEAVY      : QtFont.setWeight(87);
  end;

  QtFont.Angle := LogFont.lfEscapement;

  //LogFont.lfOrientation;

  QtFont.setItalic(LogFont.lfItalic = High(Byte));
  QtFont.setUnderline(LogFont.lfUnderline = High(Byte));
  QtFont.setStrikeOut(LogFont.lfStrikeOut = High(Byte));

  FamilyName := StrPas(LogFont.lfFaceName);

  if (CompareText(FamilyName, 'default') <> 0) then
    QtFont.setFamily(FamilyName)
  else
    QtFont.setFamily(UTF16ToUTF8(GetDefaultAppFontName));

  if (LogFont.lfQuality >= Low(QStyleStategy)) and (LogFont.lfQuality <= High(QStyleStategy)) then
    QtFont.setStyleStrategy(QStyleStategy[LogFont.lfQuality]);*)
end;

function TCDWidgetSet.CreateIconIndirect(IconInfo: PIconInfo): HICON;
//var
//  AIcon: TCDIcon;
{  APixmap, ATemp: QPixmapH;
  AMask: QBitmapH;}
begin
  Result := 0;
{  if IsValidGDIObject(IconInfo^.hbmColor) then
  begin
    APixmap := QPixmap_create();
    QPixmap_fromImage(APixmap, TQtImage(IconInfo^.hbmColor).FHandle);
    if IconInfo^.hbmMask <> 0 then
    begin
      ATemp := QPixmap_create();
      QPixmap_fromImage(ATemp, TQtImage(IconInfo^.hbmMask).FHandle);
      AMask := QBitmap_create(ATemp);
      QPixmap_setMask(APixmap, AMask);
      QPixmap_destroy(ATemp);
      QBitmap_destroy(AMask);
    end;
    if IconInfo^.fIcon then
    begin
      AIcon := TQtIcon.Create;
      AIcon.addPixmap(APixmap);
      Result := HICON(AIcon);
    end else
      Result := HCURSOR(TQtCursor.Create(APixmap, IconInfo^.xHotspot, IconInfo^.yHotspot));
    QPixmap_destroy(APixmap);
  end;}
end;

(*{------------------------------------------------------------------------------
  Function:  CreatePatternBrush
  Params:  HBITMAP
  Returns: HBRUSH
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreatePatternBrush(ABitmap: HBITMAP): HBRUSH;
{var
  Image: QImageH;
  QtBrush: TQtBrush;}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI CreatePatternBrush]',' Bitmap=', dbghex(ABitmap));
  {$endif}
  Result := 0;
{  if ABitmap = 0 then
    exit;
  QtBrush := TQtBrush.Create(True);
  Image := QImage_create(TQtImage(ABitmap).FHandle);
  try
    QtBrush.setTextureImage(Image);
  finally
    QImage_destroy(Image);
  end;

  Result := //HBRUSH(QtBrush);}
end;*)

{------------------------------------------------------------------------------
  Function:  CreatePenIndirect
  Params:  none
  Returns: HPEN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  lPen: TFPCustomPen;
begin
  lPen := TFPCustomPen.Create;
  Result := HBRUSH(lPen);

  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[TCDWidgetSet.CreatePenIndirect]  Style: %d, Color: %8x Result:"%x',
      [LogPen.lopnStyle, LogPen.lopnColor, Result]));
  {$endif}

  lPen.FPColor := TColorToFPColor(LogPen.lopnColor);

  case LogPen.lopnStyle and PS_STYLE_MASK of
    PS_SOLID:     lPen.Style := psSolid;
    PS_DASH:      lPen.Style := psDash;
    PS_DOT:       lPen.Style := psDot;
    PS_DASHDOT:   lPen.Style := psDashDot;
    PS_DASHDOTDOT:lPen.Style := psDashDotDot;
    PS_NULL:      lPen.Style := psClear;
  else
    lPen.Style := psSolid;
  end;

  lPen.Width := Max(1, LogPen.lopnWidth.X);
end;

{------------------------------------------------------------------------------
  Function: CreatePolygonRgn
  Params:  none
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN;
var
  lLazRegion: TLazRegion;
  lPoints: array of TPoint;
  i: Integer;
  lFillMode: TLazRegionFillMode;
begin
  lLazRegion := TLazRegion.Create;
  SetLength(lPoints, NumPts);
  for i := 0 to NumPts-1 do
    lPoints[i] := Points[i];

  {fillmode can be ALTERNATE or WINDING as msdn says}
  if FillMode = ALTERNATE then lFillMode := rfmOddEven
  else lFillMode := rfmWinding;

  lLazRegion.AddPolygon(lPoints, lFillMode);
  Result := HRGN(lLazRegion);

  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI CreatePolygonRgn] Result: ', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: CreateRectRgn
  Params:  none
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TCDWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
var
  lLazRegion: TLazRegion;
begin
  lLazRegion := TLazRegion.Create;
  lLazRegion.SetAsSimpleRectRegion(Types.Rect(X1, Y1, X2, Y2));
  Result := HRGN(lLazRegion);
  {$ifdef VerboseCDWinAPI}
    DebugLn('Trace: [WinAPI CreateRectRgn] Result: ', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Procedure: DeleteCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.DoneCriticalsection(ACritSec^);
  Dispose(ACritSec);
  CritSection:=0;
end;

{------------------------------------------------------------------------------
  Function: DeleteDC
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI DeleteDC] Handle: ', dbghex(hDC));
  {$endif}

  Result := False;
  if not IsValidDC(hDC) then exit;
  Result := True;
  TLazCanvas(hDC).Free;
end;

{------------------------------------------------------------------------------
  Function: DeleteObject
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TCDWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
  {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
    ObjType: string;
  {$endif}
begin
  {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
    DebugLn('Trace:> [WinAPI DeleteObject] GDIObject: ', dbghex(GDIObject));
    ObjType := 'Unidentifyed';
  {$endif}

  Result := False;

  if GDIObject = 0 then Exit(True);

  if not IsValidGDIObject(GDIObject) then Exit;

  aObject := TObject(GDIObject);

(*  if (aObject is TQtResource) and TQtResource(aObject).FShared then
    Exit(True);*)

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TFPCustomFont then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Font';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else if aObject is TFPCustomBrush then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Brush';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else if aObject is TCDBitmap then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Image';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Region
   ------------------------------------------------------------------------------}
  else if aObject is TLazRegion then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Region';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Pen
   ------------------------------------------------------------------------------}
  else if aObject is TFPCustomPen then
  begin
    {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
      ObjType := 'Pen';
    {$endif}
  end;

(*  if AObject is TQtResource then
    if TQtResource(AObject).Owner <> nil then
    begin
      // this is an owned (default) resource, let owner free it
      DebugLn('WARNING: Trying to Free a default resource');
      AObject := nil;
    end;*)

  if AObject <> nil then
  begin
    //WriteLn('Delete object: ', PtrUInt(AObject));
    FreeThenNil(AObject);
  end;

  Result := True;

  {$if defined(VerboseCDDrawing) or defined(VerboseCDBitmap)}
    DebugLn('Trace:< [WinAPI DeleteObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

(*function TQtWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result := (Handle <> 0) and QtCaret.DestroyCaret;
end;

{------------------------------------------------------------------------------
  Method:  DestroyIcon
  Params:  Handle
  Returns: Result of destroying
 ------------------------------------------------------------------------------}

function TQtWidgetSet.DestroyIcon(Handle: HICON): Boolean;
begin
  Result := (Handle <> 0) and
            (
              (TObject(Handle) is TQtIcon) or
              (TObject(Handle) is TQtCursor)
            );
  if Result then
    TObject(Handle).Free;
end;

{------------------------------------------------------------------------------
  Method:  DPtoLP
  Params:  DC: HDC; var Points; Count: Integer
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DPtoLP(DC: HDC; var Points; Count: Integer): BOOL;
var
  P: PPoint;
  QtPoint: TQtPoint;
  Matrix: QTransformH;
  MatrixInv: QTransformH;
  QtDC: TQtDeviceContext;
  Inverted: Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DPtoLP] ');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then
    Exit;

  QtDC := TQtDeviceContext(DC);

  Matrix := QTransform_create;
  MatrixInv := QTransform_create;
  QPainter_combinedTransform(QtDC.Widget, Matrix);
  P := @Points;
  try
    while Count > 0 do
    begin
      Dec(Count);
      Inverted := QTransform_isInvertible(Matrix);
      QTransform_inverted(Matrix, MatrixInv, @Inverted);
      QtPoint.X := P^.X;
      QtPoint.Y := P^.Y;
      QTransform_map(MatrixInv, PQtPoint(@QtPoint), PQtPoint(@QtPoint));
      P^.X := QtPoint.X;
      P^.Y := QtPoint.Y;
      Inc(P);
    end;

    Result := True;
  finally
    QTransform_destroy(MatrixInv);
    QTransform_destroy(Matrix);
  end;
end;

{------------------------------------------------------------------------------
  Method:  DrawEdge
  Params: DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawEdge(DC: HDC; var Rect: TRect; edge: Cardinal; grfFlags: Cardinal): Boolean;
var
  Brush: HBRUSH;
  ColorDark, ColorLight: TColorRef;
  ClientRect: TRect;
  QtDC: TQtDeviceContext;

  procedure InternalDrawEdge(Outer: Boolean; const R: TRect);
  var
    X1, Y1, X2, Y2: Integer;
    ColorLeftTop, ColorRightBottom: TColor;
    EdgeQtColor: TQColor;
    APen, OldPen: TQtPen;
  begin
    X1 := R.Left;
    Y1 := R.Top;
    X2 := R.Right;
    Y2 := R.Bottom;

    ColorLeftTop := clNone;
    ColorRightBottom := clNone;

    if Outer then
    begin
      if Edge and BDR_RAISEDOUTER <> 0 then
      begin
        ColorLeftTop := ColorLight;
        ColorRightBottom := ColorDark;
      end
      else if Edge and BDR_SUNKENOUTER <> 0 then
      begin
        ColorLeftTop := ColorDark;
        ColorRightBottom := ColorLight;
      end;
    end
    else
    begin
      if Edge and BDR_RAISEDINNER <> 0 then
      begin
        ColorLeftTop := ColorLight;
        ColorRightBottom := ColorDark;
      end
      else if Edge and BDR_SUNKENINNER <> 0 then
      begin
        ColorLeftTop := ColorDark;
        ColorRightBottom := ColorLight;
      end;
    end;

    if grfFlags and BF_DIAGONAL = 0 then
    begin

      APen := TQtPen.Create(True);
      ColorRefToTQColor(TColorRef(ColorLeftTop), EdgeQtColor);
      APen.setColor(EdgeQtColor);
      OldPen := QtDC.setPen(APen);

      if grfFlags and BF_LEFT <> 0 then
        QtDC.DrawLine(X1, Y1, X1, Y2);
      if grfFlags and BF_TOP <> 0 then
        QtDC.DrawLine(X1, Y1, X2, Y1);

      QtDC.setPen(OldPen);
      APen.Free;
      APen := TQtPen.Create(True);

      ColorRefToTQColor(TColorRef(ColorRightBottom), EdgeQtColor);
      APen.setColor(EdgeQtColor);
      OldPen := QtDC.SetPen(APen);

      if grfFlags and BF_RIGHT <> 0 then
        QtDC.DrawLine(X2, Y1, X2, Y2);
      if grfFlags and BF_BOTTOM <> 0 then
        QtDC.DrawLine(X1, Y2, X2, Y2);
      QtDC.SetPen(OldPen);
      APen.Free;
    end
    else
    begin

      APen := TQtPen.Create(True);
      ColorRefToTQColor(TColorRef(ColorLeftTop), EdgeQtColor);
      APen.setColor(EdgeQtColor);
      OldPen := QtDC.setPen(APen);

      if (grfFlags and BF_DIAGONAL_ENDTOPLEFT = BF_DIAGONAL_ENDTOPLEFT) or
         (grfFlags and BF_DIAGONAL_ENDBOTTOMRIGHT = BF_DIAGONAL_ENDBOTTOMRIGHT) then
        QtDC.DrawLine(X1, Y1, X2, Y2)
      else
        QtDC.DrawLine(X1, Y2, X2, Y1);
      QtDC.setPen(OldPen);
      APen.Free;
    end;
  end;

begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DrawEdge] ');
  {$endif}

  Result := False;
  if not IsValidDC(DC) or IsRectEmpty(Rect) then exit;

  QtDC := TQtDeviceContext(DC);

  ClientRect := Rect;
  Dec(ClientRect.Right, 1);
  Dec(ClientRect.Bottom, 1);
  QtDC.save;
  try
    ColorDark := ColorToRGB(cl3DDkShadow);
    ColorLight := ColorToRGB(cl3DLight);
    if grfFlags and BF_FLAT <> 0 then
      ColorLight := clSilver;
    if grfFlags and BF_MONO <> 0 then
    begin
      ColorDark := TColorRef(clBlack);
      ColorLight := TColorRef(clWhite);
    end;
    try
      if Edge and (BDR_SUNKENOUTER or BDR_RAISEDOUTER) <> 0 then
        InternalDrawEdge(True, ClientRect);
      InflateRect(ClientRect, -1, -1);
      if grfFlags and BF_MONO = 0 then
      begin
        ColorLight := ColorToRGB(clBtnHiLight);
        ColorDark := ColorToRGB(clBtnShadow);
      end;
      if Edge and (BDR_SUNKENINNER or BDR_RAISEDINNER) <> 0 then
      begin
        InternalDrawEdge(False, ClientRect);
        InflateRect(ClientRect, -1, -1);
      end;
    finally
    end;

    inc(ClientRect.Right);
    inc(ClientRect.Bottom);

    if grfFlags and BF_MIDDLE <> 0 then
    begin
      Brush := CreateSolidBrush(TColorRef(clBtnFace));
      try
        FillRect(DC, ClientRect, Brush);
      finally
        DeleteObject(Brush);
      end;
    end;

    if grfFlags and BF_ADJUST <> 0 then
      Rect := ClientRect;

    Result := True;
  finally
    QtDC.Restore;
  end;

end;*)

{------------------------------------------------------------------------------
  Method: DrawFocusRect
  Params: DC: HDC; const Rect: TRect
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TCDWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format('[DrawFocusRect] DC: %x', [PtrUInt(DC)]));
  {$endif}
  Result := False;

  if not IsValidDC(DC) then exit;

  // Drawer.DrawFocusRect alters the Pen and Brush, so we save the state here
  LazDC.SaveState();
  GetDefaultDrawer().DrawFocusRect(LazDC, Types.Point(Rect.Left, Rect.Top),
    Types.Size(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top));
  LazDC.RestoreState(-1);
end;

function TCDWidgetSet.DrawFrameControl(DC: HDC; const Rect: TRect; uType,
  uState: Cardinal): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  lControlStateEx: TCDControlStateEx;
  lState: TCDControlState;
  lSize: Types.TSize;
begin
  Result := False;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI DrawFrameControl] Invalid DC!');
    {$endif}
    Exit(False);
  end;

  case uType of
    DFC_BUTTON:
    begin
      lSize := Types.Size(Rect.Right-Rect.Left, Rect.Bottom-Rect.Top);

      if (DFCS_BUTTONPUSH and uState) <> 0 then
        lControlStateEx := TCDButtonStateEx.Create
      else
        lControlStateEx := TCDControlStateEx.Create;

      try
        lControlStateEx.Font := TFont.Create;
        lControlStateEx.ParentRGBColor := clSilver;
        lControlStateEx.FPParentRGBColor := colSilver;
        lControlStateEx.RGBColor := GetDefaultDrawer().FallbackPalette.BtnFace;
        lControlStateEx.FPRGBColor := TColorToFPColor(lControlStateEx.RGBColor);

        //if uState and DFCS_FLAT <> 0 then lState := [csfEnabled];
        if uState and DFCS_INACTIVE = 0 then lState := lState + [csfEnabled];
        if uState and DFCS_PUSHED <> 0 then lState := lState + [csfSunken];

        if (uState and $1F) in [DFCS_BUTTONCHECK, DFCS_BUTTON3STATE] then
        begin
          //Element := QStyleCE_CheckBox
        end
        else if (DFCS_BUTTONRADIO and uState) <> 0 then
        begin
          //Element := QStyleCE_RadioButton
        end
        else if (DFCS_BUTTONPUSH and uState) <> 0 then
        begin
          GetDefaultDrawer().DrawButton(LazDC, Types.Point(0,0), lSize, lState, TCDButtonStateEx(lControlStateEx));
        end
        else if (DFCS_BUTTONRADIOIMAGE and uState) <> 0 then
        begin
          //Element := QStyleCE_RadioButton
          //TODO: what to implement here ?
        end
        else if (DFCS_BUTTONRADIOMASK and uState) <> 0 then
        begin
          //Element := QStyleCE_RadioButton
          //TODO: what to implement here ?
        end;
      finally
        lControlStateEx.Font.Free;
        lControlStateEx.Free;
      end;
    end;
    DFC_CAPTION: ; // title bar captions
    DFC_MENU: ; // menu
    DFC_SCROLL:
    begin
    end;//DrawScrollBarArrows;
  end;
  {function uStatetoQStyleState: QStyleState;
  begin
    Result := QStyleState_None;
    if (uState and DFCS_INACTIVE = 0) then
      Result := Result or QStyleState_Enabled;

    if (uState and DFCS_PUSHED <> 0) then
      Result := Result or QStyleState_MouseOver or QStyleState_Sunken
    else
      Result := Result or QStyleState_Raised;

    if (uState and DFCS_CHECKED <> 0) then
      Result := Result or QStyleState_On
    else
      Result := Result or QStyleState_Off;

    if ((uState and DFCS_HOT <> 0) or (uState and DFCS_PUSHED <> 0)) then
      Result := Result or QStyleState_MouseOver or QStyleState_Active;

    if (uType <> DFC_BUTTON) and
      ((uState and DFCS_FLAT <> 0) and not (uState and DFCS_PUSHED <> 0)) then
      Result := Result and not QStyleState_Raised;

    // DFCS_TRANSPARENT = 2048;
    //DFCS_ADJUSTRECT = 8192;
    //DFCS_FLAT = 16384;
    //DFCS_MONO = 32768;
  end;

  procedure DrawScrollBarArrows;
  var
    Opt: QStyleOptionH;
    Element: QStylePrimitiveElement;
    State: QStyleState;
  begin
    //TODO: DFCS_SCROLLCOMBOBOX and DFCS_SCROLLSIZEGRIP
    State := uStatetoQStyleState;
    Element := QStylePE_CustomBase;
    if (uState and $1F) in [DFCS_SCROLLUP] then
      Element := QStylePE_IndicatorArrowUp
    else
    if (uState and $1F) in [DFCS_SCROLLDOWN] then
      Element := QStylePE_IndicatorArrowDown
    else
    if (uState and $1F) in [DFCS_SCROLLLEFT] then
      Element := QStylePE_IndicatorArrowLeft
    else
    if (uState and $1F) in [DFCS_SCROLLRIGHT] then
      Element := QStylePE_IndicatorArrowRight;

    if Element = QStylePE_CustomBase then
      exit;
    Opt := QStyleOption_create(1, 0);
    QStyleOption_setRect(Opt, @Rect);
    QStyleOption_setState(Opt, State);
    QStyle_drawPrimitive(QApplication_style(), Element, Opt, Painter, Widget);
    QStyleOption_destroy(Opt);
  end;}
end;

(*{------------------------------------------------------------------------------
  Method:  DrawText
  Params:  DC, Str, Count, Rect, Flags
  Returns: If the string was drawn, or CalcRect run

  if DT_CALCRECT is one of the Flags passed to this function, then:

  * DrawText should not draw the text, but determine the size that would be required to write it.
  * If there are multiple lines of text, this function will keep Rect.Width fixed and
    expand Rect.Height to fit the text.
  * If there is one line of text, Rect is reduced or expanded to fit it.
  * The result will the height of the text.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawText(DC: HDC; Str: PChar; Count: Integer;
  var ARect: TRect; Flags: Cardinal): Integer;
var
  WideStr: WideString;
  R: TRect;
  QtDC: TQtDeviceContext;
  F: Integer;
  Pt: TPoint;
  ClipRect: TRect;
  B: Boolean;
  S: String;
  i: Integer;

  procedure CalculateOffsetWithAngle(const AFontAngle: Integer;
    var TextLeft,TextTop: Integer);
  var
    OffsX, OffsY: integer;
    Angle: Integer;
    Size: TSize;
  begin
    OffsX := R.Right - R.Left;
    OffsY := R.Bottom - R.Top;
    Size.cX := OffsX;
    Size.cy := OffsY;
    Angle := AFontAngle div 10;
    if Angle < 0 then
      Angle := 360 + Angle;

    if Angle <= 90 then
    begin
      OffsX := 0;
      OffsY := Trunc(Size.cx * sin(Angle * Pi / 180));
    end else
    if Angle <= 180 then
    begin
      OffsX := Trunc(Size.cx * -cos(Angle * Pi / 180));
      OffsY := Trunc(Size.cx * sin(Angle * Pi / 180) +
         Size.cy * cos((180 - Angle) * Pi / 180));
    end else
    if Angle <= 270 then
    begin
      OffsX := Trunc(Size.cx * -cos(Angle * Pi / 180) +
        Size.cy * sin((Angle - 180) * Pi / 180));
      OffsY := Trunc(Size.cy * sin((270 - Angle) * Pi / 180));
    end else
    if Angle <= 360 then
    begin
      OffsX := Trunc(Size.cy * sin((360 - Angle) * Pi / 180));
      OffsY := 0;
    end;
    TextTop := OffsY;
    TextLeft := OffsX;
  end;

begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DrawText] DC: ', dbghex(DC), ' Str: ', string(Str),
     ' CalcRect: ', dbgs((Flags and DT_CALCRECT) = DT_CALCRECT),' ARect ',dbgs(ARect));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
    Exit;

  QtDC :=TQtDeviceContext(DC);

  if Count >= 0 then
    WideStr := GetUtf8String(Copy(Str, 1, Count))
  else
    WideStr := GetUtf8String(Str);


  B := QtDC.getClipping;
  if B and
    (Flags and DT_NOCLIP = DT_NOCLIP) and
    (Flags and DT_WORDBREAK = DT_WORDBREAK) then
  begin
    ClipRect := QtDC.getClipRegion.getBoundingRect;
    //this is just to get same behaviour as gtk2 and win32
    //IMO, we should change ARect.Left and/or ARect.Top if smaller than
    //clip rect (map to clipRect). Then multiline text is drawn ok.
    //look at issue http://bugs.freepascal.org/view.php?id=17678 . zeljko.
    if (ARect.Left < ClipRect.Left) or (ARect.Top < ClipRect.Top) then
    begin
      {$note remove ifdef if I'm wrong about DT_WORDBREAK OBSERVATION}
      {$IFDEF QT_DRAWTEXT_MAP_TO_CLIPRECT}
      if ARect.Left < ClipRect.Left then
        ARect.Left := ClipRect.Left;
      if ARect.Top < ClipRect.Top then
        ARect.Top := ClipRect.Top;
      {$ELSE}
      Flags := Flags and not DT_WORDBREAK;
      {$ENDIF}
    end;
  end;

  F := DTFlagsToQtFlags(Flags);

  QtDC.Metrics.BoundingRect(@R, @ARect, F, @WideStr);

  //TODO: result should be different when DT_VCENTER or DT_BOTTOM is set
  Result := R.Bottom - R.Top;

  if (Flags and DT_CALCRECT) = DT_CALCRECT then
  begin
    if (Flags and DT_WORDBREAK = DT_WORDBREAK) and
    ((R.Bottom - R.Top) > (ARect.Bottom - ARect.Top)) then
      // MSDN says do not touch rect width when we have DT_WORDBREAK flag
      // and new text is multiline (if R height > ARect height).See #17329.
    else
      ARect.Right := ARect.Left + R.Right - R.Left;
    ARect.Bottom := ARect.Top + R.Bottom - R.Top;
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI DrawText] Rect=', dbgs(ARect));
    {$endif}
    Exit;
  end;

  // if our Font.Orientation <> 0 we must recalculate X,Y offset
  // also it works only with DT_TOP DT_LEFT. Qt can handle multiline
  // text in this case too.
  Pt := Point(0, 0);
  if (QtDC.Font.Angle <> 0) and
    (Flags and DT_VCENTER = 0) and (Flags and DT_CENTER = 0) and
    (Flags and DT_RIGHT = 0) and (Flags and  DT_BOTTOM = 0) then
  begin
    Pt := Point(ARect.Left, ARect.Top);
    CalculateOffsetWithAngle(QtDC.font.Angle, Pt.X, Pt.Y);
  end;

  // we cannot fit into rectangle, so use DT_SINGLELINE.See #17329.
  // http://msdn.microsoft.com/en-us/library/dd162498%28v=VS.85%29.aspx
  if B and
    (Flags and DT_NOCLIP = DT_NOCLIP) and
    (Flags and DT_WORDBREAK = DT_WORDBREAK) and
    (Flags and DT_SINGLELINE = DT_SINGLELINE) and
    ((R.Bottom - R.Top) >= (ARect.Bottom - ARect.Top)) then
  begin
    Flags := Flags and not DT_WORDBREAK;
    F := DTFlagsToQtFlags(Flags);
  end;

  {$warning HARDCODED WORKAROUND for qt-4.7.1 QPainter bug.}
  { Bug triggers when we try to paint multiline text which contains 1
   space. eg "Save project\nCtrl+S". In this case QPainter draws
   Save
   project (in two lines, so Ctrl+S is invisible. See issue #18631.
   But does not trigger with qt-4.6.XX and maybe with 4.7.0.
   Opened nokia issue: http://bugreports.qt.nokia.com/browse/QTBUG-17020
   UPDATE: it's fixed in qt-4.7.4 git and qt-4.8}
  if (QtVersionMajor = 4) and (QtVersionMinor = 7) and (QtVersionMicro < 4) and
   (Flags and DT_WORDBREAK = DT_WORDBREAK) and
   ((Flags and DT_VCENTER = DT_VCENTER) or (Flags and DT_CENTER = DT_CENTER))
    and not (Flags and DT_NOCLIP = DT_NOCLIP) and
    not (Flags and DT_MODIFYSTRING = DT_MODIFYSTRING) and
    not (Flags and DT_END_ELLIPSIS = DT_END_ELLIPSIS) then
  begin
    S := StrPas(Str);
    if length(S) > 0 then
    begin
      i := Pos(' ', S);
      if (AnsiPos(LineEnding, S) > i) and
        (S[length(S)] <> LineEnding) then
      begin
        Flags := Flags and not DT_WORDBREAK;
        F := DTFlagsToQtFlags(Flags);
      end;
    end;
  end;

  if (Flags and DT_MODIFYSTRING = DT_MODIFYSTRING) and
  (Flags and DT_END_ELLIPSIS = DT_END_ELLIPSIS) and
  (Flags and DT_WORDBREAK = 0) then
  begin
    // windows are removing trailing spaces in this case
    // and we are doing same thing too.
    WideStr := TrimLeft(WideStr);
    with ARect do
      WideStr := QtDC.Metrics.elidedText(WideStr, QtElideRight, Right - Left, 0);
  end;

  with ARect do
    QtDC.DrawText(Left + Pt.X, Top + Pt.Y, Right-Left, Bottom-Top, F, @WideStr);
end;*)

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  R: TRect;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Ellipse] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI Rectangle] Invalid DC!');
    {$endif}
    Exit(False);
  end;

//  R := NormalizeRect(Rect(X1, Y1, X2, Y2));
//  if IsRectEmpty(R) then Exit(True);

  LazDC.Ellipse(X1, Y1, X2, Y2);
  Result := True;
end;

(*function TQtWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
begin
  {maybe we can put creating of scrollbar here instead of SetScrollInfo() }
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.EnableScrollbar] missing implementation ');
  {$endif}
end;

function TQtWidgetSet.EnableWindow(hWnd: HWND; bEnable: Boolean): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI EnableWindow] ');
  {$endif}
  Result := False;
  if HWND <> 0 then
  begin
    Result := not TQtWidget(hwnd).getEnabled;
    TQtWidget(hWnd).setEnabled(bEnable);
  end;
end;

{------------------------------------------------------------------------------
  Function: EndPaint
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.EndPaint(Handle: hwnd; var PS: TPaintStruct): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI EndPaint] Handle: ', dbghex(Handle),
     ' PS.HDC: ', dbghex(PS.HDC));
  {$endif}

  Result := 1;

  if IsValidDC(PS.HDC) and (TObject(PS.HDC) is TQtDeviceContext) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Freeing resources');
    {$endif}
    TQtDeviceContext(PS.HDC).Free;
  end;
end;*)

{------------------------------------------------------------------------------
  Procedure: EnterCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
end;

function TCDWidgetSet.EnumDisplayMonitors(hdc: HDC; lprcClip: PRect;
  lpfnEnum: MonitorEnumProc; dwData: LPARAM): LongBool;
begin
  Result := lpfnEnum(1, 0, nil, dwData);
end;

(*
function CharsetToQtCharSet(const ALCLCharset: Byte): QFontDatabaseWritingSystem;
begin
  Result := QFontDatabaseAny;
  case ALCLCharset of
    SYMBOL_CHARSET: Result := QFontDatabaseSymbol;
    FCS_ISO_8859_1 .. FCS_ISO_8859_4,
    FCS_ISO_8859_9,FCS_ISO_8859_10,
    FCS_ISO_8859_15,
    EASTEUROPE_CHARSET: Result := QFontDatabaseLatin;
    FCS_ISO_8859_5,
    RUSSIAN_CHARSET: Result := QFontDatabaseCyrillic;
    FCS_ISO_8859_6,
    ARABIC_CHARSET: Result := QFontDatabaseArabic;
    FCS_ISO_8859_7,
    GREEK_CHARSET: Result := QFontDatabaseGreek;
    FCS_ISO_8859_8,
    HEBREW_CHARSET: Result := QFontDatabaseHebrew;
    SHIFTJIS_CHARSET: Result := QFontDatabaseJapanese;
    HANGEUL_CHARSET: Result := QFontDatabaseKorean;
    GB2312_CHARSET: Result := QFontDatabaseSimplifiedChinese;
    CHINESEBIG5_CHARSET: Result := QFontDatabaseTraditionalChinese;
    THAI_CHARSET: Result := QFontDatabaseThai;
  end;
end;

function QtCharsetToCharset(AWritingSystem: QFontDatabaseWritingSystem;
  AList: TFPList): Byte;
begin
  Result := DEFAULT_CHARSET;
  case AWritingSystem of
    QFontDatabaseAny:
    begin
      Result := FCS_ISO_10646_1;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseSymbol:
    begin
      Result := SYMBOL_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseThai:
    begin
      Result := THAI_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseTraditionalChinese:
    begin
      Result := CHINESEBIG5_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseSimplifiedChinese:
    begin
      Result := GB2312_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseKorean:
    begin
      Result := HANGEUL_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseJapanese:
    begin
      Result := SHIFTJIS_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseHebrew:
    begin
      Result := HEBREW_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_8)));
    end;
    QFontDatabaseGreek:
    begin
      Result := GREEK_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_7)));
    end;
    QFontDatabaseArabic:
    begin
      Result := ARABIC_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
    end;
    QFontDatabaseCyrillic:
    begin
      Result := RUSSIAN_CHARSET;
      AList.Add(TObject(PtrUInt(Result)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_5)));
    end;
    QFontDatabaseLatin:
    begin
      Result := FCS_ISO_10646_1;
      AList.Add(TObject(PtrUInt(Result)));
      AList.Add(TObject(PtrUInt(ANSI_CHARSET)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_1)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_2)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_3)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_4)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_9)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_10)));
      AList.Add(TObject(PtrUInt(FCS_ISO_8859_15)));
      AList.Add(TObject(PtrUInt(EASTEUROPE_CHARSET)));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: EnumFontFamiliesEx
  Params:
    hdc
        [in] Handle to the device context.
    lpLogfont
        [in] Pointer to a LOGFONT structure that contains information about the
        fonts to enumerate. The function examines the following members.

        Member 	Description
        lfCharset 	If set to DEFAULT_CHARSET, the function enumerates all fonts
                    in all character sets. If set to a valid character set value,
                    the function enumerates only fonts in the specified character
                    set.
        lfFaceName 	If set to an empty string, the function enumerates one font
                    in each available typeface name. If set to a valid typeface
                    name, the function enumerates all fonts with the
                    specified name.

        lfPitchAndFamily 	Must be set to zero for all language versions of
                          the operating system.

    lpEnumFontFamExProc
        [in] Pointer to the application definedcallback function. For more
             information, see the EnumFontFamExProc function.
    lParam
        [in] Specifies an applicationdefined value. The function passes this value
             to the callback function along with font information.
    dwFlags
        This parameter is not used and must be zero.

  Returns:

  The return value is the last value returned by the callback function.
  This value depends on which font families are available for the
  specified device.

 ------------------------------------------------------------------------------}
function TQtWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont; Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
var
  EnumLogFont: TEnumLogFontEx;
  Metric: TNewTextMetricEx;
  FontList: TStringList;
  FontType: Integer;
  FontDB: QFontDatabaseH;
  i: Integer;
  y: Integer;
  AStyle: String;
  StylesCount: Integer;
  StylesList: QStringListH;
  ScriptList: QStringListH;
  CharsetList: TFPList;

  function QtGetFontFamiliesDefault(var List:TStringList;
    const AWritingSystem: QFontDatabaseWritingSystem = QFontDatabaseAny):integer;
  var
    StrLst: QStringlistH;
    WStr: WideString;
    j: integer;
  begin
    Result := -1;
    StrLst := QStringList_create;
    try
      QFontDatabase_families(FontDB, StrLst, AWritingSystem);
      Result := QStringList_size(StrLst);
      for j := 0 to Result - 1 do
      begin
        QStringList_at(StrLst, @WStr, j);
        List.Add(UTF16ToUTF8(WStr));
      end;
    finally
      QStringList_destroy(StrLst);
    end;
  end;

  function QtGetFontFamilies(var List: TStringList;
    const APitch: Byte;
    const AFamilyName: String;
    const AWritingSystem: QFontDatabaseWritingSystem = QFontDatabaseAny): Integer;
  var
    StrLst: QStringlistH;
    NewList: QStringListH;
    WStr: WideString;
    j: integer;
  begin
    Result := -1;
    StrLst := QStringList_create();
    NewList := QStringList_create();

    try
      QFontDatabase_families(FontDB, StrLst, AWritingSystem);
      for j := 0 to QStringList_size(StrLst) - 1 do
      begin
        QStringList_at(StrLst, @WStr, j);
        if APitch <> DEFAULT_PITCH then
        begin
          case APitch of
            FIXED_PITCH, MONO_FONT:
            begin
              if QFontDatabase_isFixedPitch(FontDB, @WStr) then
                QStringList_append(NewList, @WStr);
            end;
            VARIABLE_PITCH:
            begin
              if QFontDatabase_isScalable(FontDB, @WStr) then
                QStringList_append(NewList, @WStr);
            end;
          end;
        end else
          QStringList_append(NewList, @WStr);
      end;

      if AFamilyName <> '' then
      begin
        for j := QStringList_size(NewList) - 1 downto 0 do
        begin
          QStringList_at(NewList, @WStr, j);
          if UTF16ToUTF8(WStr) <> AFamilyName then
            QStringList_removeAt(NewList, j);
        end;
      end;
      for j := 0 to QStringList_size(NewList) - 1 do
      begin
        QStringList_at(NewList, @WStr, j);
        List.Add(UTF16ToUTF8(WStr));
      end;
      Result := List.Count;
    finally
      QStringList_destroy(StrLst);
      QStringList_destroy(NewList);
    end;
  end;

  function GetStyleAt(AIndex: Integer): String;
  var
    WStr: WideString;
  begin
    Result := '';
    if (AIndex >= 0) and (AIndex < QStringList_size(StylesList)) then
    begin
      QStringList_at(StylesList, @WStr, AIndex);
      Result := UTF16ToUTF8(WStr);
    end;
  end;

  function GetWritingSystems(AFontName: String; AList: QStringListH;
    ACharsetList: TFPList): Boolean;
  var
    WStr: WideString;
    Arr: TPtrIntArray;
    j: Integer;
  begin
    Result := False;
    QStringList_clear(AList);
    if Assigned(CharSetList) then
      CharSetList.Clear;
    WStr := UTF8ToUTF16(AFontName);
    QFontDatabase_writingSystems(FontDB, @Arr, @WStr);
    Result := length(Arr) > 0;
    for j := 0 to High(Arr) do
    begin
      if Assigned(ACharsetList) then
        QtCharsetToCharset(QFontDatabaseWritingSystem(Arr[j]), ACharsetList);
      QFontDatabase_writingSystemName(@WStr, QFontDatabaseWritingSystem(Arr[j]));
      QStringList_append(AList, @WStr);
    end;
  end;

  function FillLogFontA(AFontName: String; var ALogFontA: TLogFontA;
    var AMetric: TNewTextMetricEx; var AFontType: Integer;
    out AStyle: String): Integer;
  var
    Font: QFontH;
    WStr: WideString;
  begin
    WStr := UTF8ToUTF16(AFontName);
    Font := QFont_create(@WStr);
    ALogFontA.lfItalic := Byte(QFont_italic(Font));
    ALogFontA.lfWeight := QFont_weight(Font);
    ALogFontA.lfHeight := QFont_pointSize(Font);
    ALogFontA.lfUnderline := Byte(QFont_underline(Font));
    ALogFontA.lfStrikeOut := Byte(QFont_strikeOut(Font));

    if QFont_styleStrategy(Font) = QFontPreferBitmap then
      AFontType := AFontType  or RASTER_FONTTYPE;
    if QFont_styleStrategy(Font) = QFontPreferDevice then
      AFontType := AFontType  or DEVICE_FONTTYPE;

    if not (QFont_styleStrategy(Font) = QFontPreferDefault) then
      AFontType := AFontType and not TRUETYPE_FONTTYPE;

    QStringList_clear(StylesList);
    QFontDatabase_styles(FontDB, StylesList, @WStr);
    AStyle := '';
    Result := QStringList_size(StylesList);

    if Result > 0 then
      AStyle := GetStyleAt(0);
    // fill script and charset list
    GetWritingSystems(AFontName, ScriptList, CharsetList);

    QFont_destroy(Font);
  end;

begin
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI EnumFontFamiliesEx] Charset=',lpLogFont^.lfCharSet,
  ' face ',lpLogFont^.lfFaceName,' pitchAndFamily=',lpLogFont^.lfPitchAndFamily);
  {$endif}
  Result := 0;
  Metric.ntmentm.ntmAvgWidth := 0; // just to shutup compiler
  FontDB := QFontDatabase_create();
  try
    if (lpLogFont^.lfCharSet = DEFAULT_CHARSET) and
       (lpLogFont^.lfFaceName= '') and
       (lpLogFont^.lfPitchAndFamily = 0) then
    begin
      FontType := 0;
      FontList := TStringList.create;
      try
        if QtGetFontFamiliesDefault(FontList) > 0 then
        begin
          for i := 0 to FontList.Count - 1 do
          begin
            EnumLogFont.elfLogFont.lfFaceName := FontList[i];
            Result := Callback(EnumLogFont, Metric, FontType, LParam);
          end;
        end;
      finally
        FontList.free;
      end;
    end else
    begin
      Result := 0;
      FontType := TRUETYPE_FONTTYPE;
      FontList := TStringList.create;
      StylesList := QStringList_create();
      ScriptList := QStringList_create();
      CharsetList := TFPList.Create;
      try
        if QtGetFontFamilies(FontList, lpLogFont^.lfPitchAndFamily,
          lpLogFont^.lfFaceName, CharsetToQtCharSet(lpLogFont^.lfCharSet)) > 0 then
        begin
          StylesList := QStringList_create();
          for i := 0 to FontList.Count - 1 do
          begin
            EnumLogFont.elfLogFont.lfFaceName := FontList[i];
            EnumLogFont.elfLogFont.lfPitchAndFamily := lpLogFont^.lfPitchAndFamily;
            EnumLogFont.elfFullName := FontList[i];

            StylesCount := FillLogFontA(FontList[i], EnumLogFont.elfLogFont, Metric, FontType,
              AStyle);
            EnumLogFont.elfStyle := AStyle;
            if CharSetList.Count > 0 then
              EnumLogFont.elfLogFont.lfCharSet := PtrUInt(CharsetList.Items[0]);
            Result := Callback(EnumLogFont, Metric, FontType, LParam);
            for y := 1 to StylesCount - 1 do
            begin
              AStyle := GetStyleAt(y);
              EnumLogFont.elfStyle := AStyle;
              Result := Callback(EnumLogFont, Metric, FontType, LParam);
            end;
            for y := 1 to CharsetList.Count - 1 do
            begin
              EnumLogFont.elfLogFont.lfCharSet := PtrUInt(CharsetList.Items[y]);
              Result := Callback(EnumLogFont, Metric, FontType, LParam);
            end;
          end;
        end;
      finally
        FontList.free;
        QStringList_destroy(StylesList);
        CharSetList.Free;
      end;
    end;
  finally
    QFontDatabase_destroy(FontDB);
  end;
end;


{------------------------------------------------------------------------------
  Function: ExcludeClipRect
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TQtWidgetSet.ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer;
var
  Region: QRegionH;
  ClipRegion: QRegionH;
  ExRegion: QRegionH;
  QtDC: TQtDeviceContext;
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ExcludeClipRect]');
  {$endif}

  Result := ERROR;
  if not IsValidDC(DC) then Exit;

  QtDC := TQtDeviceContext(DC);

  {ExcludeClipRect on X11 paint engine is pretty slow with complex regions
   eg. setting clipRegion with hundreds of rects (usually created by
   calling ExcludeClipRect for many children on widget) dramatically kills
   performance of our application.
   To get rid of it we are using trick from webkit. If numRects is over
   25 then create an new rect region with boundsRect of NewRegion.
   see issue http://bugs.freepascal.org/view.php?id=19698.
   If you want accurate ExcludeClipRect use graphicssystem Raster or
   see comment in TQtWidgetSet.ExtSelectClipRgn}
  ExRegion := QRegion_create(Left, Top, Right - Left, Bottom - Top, QRegionRectangle);
  Region := QRegion_create;
  ClipRegion := QRegion_create;
  try
    QPainter_clipRegion(QtDC.Widget, ClipRegion);
    QRegion_subtracted(ClipRegion, Region, ExRegion);

    // only for X11 paintEngine.
    if (QPaintEngine_type(QtDC.PaintEngine) = QPaintEngineX11) and
      not QRegion_isEmpty(Region) and
      (QRegion_numRects(Region) > 25) then
    begin
      QRegion_boundingRect(Region, @R);
      QRegion_setRects(Region, @R, 1);
    end;

    QtDC.setClipRegion(Region);
    QtDC.setClipping(True);
    if QRegion_isEmpty(Region) then
      Result := NULLREGION
    else
    if QRegion_numRects(Region) = 1 then
      Result := SIMPLEREGION
    else
      Result := COMPLEXREGION;

  finally
    QRegion_destroy(ClipRegion);
    QRegion_destroy(Region);
    QRegion_destroy(ExRegion);
  end;
end;*)

function TCDWidgetSet.ExtCreatePen(dwPenStyle, dwWidth: DWord;
  const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord): HPEN;
var
  lPen: TFPCustomPen;
begin
  lPen := TFPCustomPen.Create;
  Result := HBRUSH(lPen);
//  QtPen.IsExtPen := True;

//  {$ifdef VerboseCDDrawing}
//    DebugLn(Format(':>[TCDWidgetSet.ExtCreatePen]  Style: %d, Color: %8x Result:"%x',
//      [LogPen.lopnStyle, LogPen.lopnColor, Result]));
//  {$endif}

  case dwPenStyle and PS_STYLE_MASK of
    PS_SOLID:     lPen.Style := psSolid;
    PS_DASH:      lPen.Style := psDash;
    PS_DOT:       lPen.Style := psDot;
    PS_DASHDOT:   lPen.Style := psDashDot;
    PS_DASHDOTDOT:lPen.Style := psDashDotDot;
//    PS_USERSTYLE: QtPen.setStyle(QtCustomDashLine);
    PS_NULL:      lPen.Style := psClear;
  else
    lPen.Style := psSolid;
  end;

  lPen.Width := 1;
  if (dwPenStyle and PS_TYPE_MASK) = PS_COSMETIC then
    lPen.Width := 1
  else if (dwPenStyle and PS_TYPE_MASK) = PS_GEOMETRIC then
  begin
    lPen.Width := dwWidth;
    {case dwPenStyle and PS_JOIN_MASK of
      PS_JOIN_ROUND: QtPen.setJoinStyle(QtRoundJoin);
      PS_JOIN_BEVEL: QtPen.setJoinStyle(QtBevelJoin);
      PS_JOIN_MITER: QtPen.setJoinStyle(QtMiterJoin);
    end;

    case dwPenStyle and PS_ENDCAP_MASK of
      PS_ENDCAP_ROUND: QtPen.setCapStyle(QtRoundCap);
      PS_ENDCAP_SQUARE: QtPen.setCapStyle(QtSquareCap);
      PS_ENDCAP_FLAT: QtPen.setCapStyle(QtFlatCap);
    end;}
  end;

{  if (dwPenStyle and PS_STYLE_MASK) = PS_USERSTYLE then
    QtPen.setDashPattern(lpStyle, dwStyleCount);}

  lPen.FPColor := TColorToFPColor(ColorToRGB(lplb.lbColor));

  Result := HPEN(lPen);
end;

function TCDWidgetSet.ExtSelectClipRGN(dc: hdc; rgn: hrgn; Mode: Longint) : Integer;
var
  LazDC: TLazCanvas absolute DC;
  lRegion: TLazRegion absolute rgn;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[TCDWidgetSet.SelectClipRGN] DC=', dbgs(DC),' RGN=', dbghex(RGN));
  {$endif}

  Result := ERROR;

  // Activating this code break the drawing of TStringGrid. ToDo: Find out why
{  if not IsValidDC(DC) then exit;

  // RGN=0 indicates that the clipping region should be removed
  if (RGN = 0) then
  begin
    TLazCanvas(LazDC.ClipRegion).Clear;
    LazDC.Clipping := False;
    Result := NullRegion;
    Exit;
  end;

  if LazDC.ClipRegion = nil then
    LazDC.ClipRegion := TLazRegion.Create;

  // Never use LazDC.ClipRegion := RGN because we really need to make a copy of it
  // The original handle might be freed afterwards
  CombineRgn(HRGN(LazDC.ClipRegion), HRGN(LazDC.ClipRegion), RGN, Mode);
  LazDC.Clipping := True;
  Result := TLazRegion(RGN).GetRegionKind();}
end;

{$ifndef CD_UseNativeText}
{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  lDestCanvas: TLazCanvas absolute DC;
  lDestIntfImage: TLazIntfImage;
  lFontSize: Integer;
  FTDrawer: TIntfFreeTypeDrawer;
  ftFont: TFreeTypeFont;
  RealX, RealY: Integer;
  FreeFTFont: Boolean = false;
  lLogFont: TLogFont;
begin
  {$ifdef VerboseCDText}
    DebugLn(Format(':>[WinAPI ExtTextOut] DC=%x Str=%s X=%d Y=%d',
      [DC, StrPas(Str), X, Y]));
  {$endif}

  Result := False;

  if (Str = nil) or (Str = '') then Exit;

  if ((Options and (ETO_OPAQUE + ETO_CLIPPED)) <> 0) and (Rect = nil) then
    exit;

  if Rect <> nil then Rect^ := Bounds(0, 0, 0, 0);

  if not IsValidDC(DC) then Exit;
  lDestIntfImage := TLazIntfImage(lDestCanvas.Image);

  if (lDestCanvas.Font = nil) or (lDestCanvas.Font.Size = 0) then lFontSize := DefaultFontSize
  else lFontSize := Abs(lDestCanvas.Font.Size);

  // Preparations finished, draw it using LazFreeType

  FTDrawer := TIntfFreeTypeDrawer.Create(lDestIntfImage);
  ftFont := TFreeTypeFont(lDestCanvas.ExtraFontData);
  if ftFont = nil then
  begin
    ftFont := TFreeTypeFont.Create;
    ftFont.Name := BackendGetFontPath(lLogFont, '');
    ftFont.Hinted := true;
    ftFont.ClearType := true;
    ftFont.Quality := grqHighQuality;
    FreeFTFont := True;
  end;
  try
    ftFont.SizeInPoints:= lFontSize;
    //lFontSize:= MulDiv(lFontSize,72,ftFont.DPI); // convert points to pixels
    lFontSize := Round(ftFont.TextHeight(Str) * 0.75);// ToDo: Find out why this 75% factor works
    RealX := X + lDestCanvas.WindowOrg.X + lDestCanvas.BaseWindowOrg.X;
    RealY := Y + lDestCanvas.WindowOrg.Y + lDestCanvas.BaseWindowOrg.Y + lFontSize;
    FTDrawer.DrawText(Str, ftFont, RealX, RealY, colBlack, 255);
  finally
    if FreeFTFont then ftFont.Free;
    FTDrawer.Free;
  end;

  {$ifdef VerboseCDText}
    DebugLn(':<[WinAPI ExtTextOut]');
  {$endif}

  Result := True;

{   if ((Options and ETO_OPAQUE) <> 0) then
     QtDC.fillRect(Rect^.Left, Rect^.Top, Rect^.Right - Rect^.Left, Rect^.Bottom - Rect^.Top);

  if Str <> nil then
  begin
    if Count >= 0 then
      WideStr := GetUtf8String(Copy(Str, 1, Count))
    else
      WideStr := GetUtf8String(Str);

    if (Options and ETO_CLIPPED <> 0) then
    begin
      B := QtDC.getClipping;
      if not B then
      begin
        QtDC.save;
        QtDC.setClipRect(Rect^);
      end;
      QtDC.drawText(X, Y, Rect^.Right - Rect^.Left, Rect^.Bottom - Rect^.Top, 0, @WideStr);
      if not B then
        QtDC.restore;
    end else
      QtDC.drawText(X, Y, @WideStr);
  end;}

  Result := True;
end;
{$endif}

{------------------------------------------------------------------------------
  Function: FillRect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  lOldBrush: HGDIOBJ;
begin
  Result := False;

  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI FillRect Rect=', dbgs(Rect),' Brush=', dbghex(Brush));
  {$endif}

  if not IsValidDC(DC) then
    exit;
  if not IsValidGdiObject(Brush) then
    exit;

  lOldBrush := SelectObject(DC, Brush);
  LazDC.FillRect(Rect);
  SelectObject(DC, lOldBrush);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: FillRgn
  Params:  DC: HDC; RegionHnd: HRGN; hbr: HBRUSH
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TCDWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool;
var
  LazDC: TLazCanvas absolute DC;
  lRegion: TLazRegion absolute RegionHnd;
  lRegionRect: TRect;
  lOldBrush: HGDIOBJ;
  lOldRegion: TLazRegion;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[TCDWidgetSet.FillRgn] Rgn=', dbgs(RegionHnd),' Brush=', dbghex(hbr));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then exit;
  if hbr = 0 then Exit;
  if RegionHnd = 0 then Exit;

  lOldBrush := SelectObject(DC, hbr);
  try
    lOldRegion := TLazRegion.Create;
    lOldRegion.Assign(TLazRegion(LazDC.ClipRegion));
    lRegionRect := lRegion.GetBoundingRect();
    LazDC.Rectangle(lRegionRect);
  finally
    TLazRegion(LazDC.ClipRegion).Assign(lOldRegion);
    lOldRegion.Free;
    SelectObject(DC, lOldBrush);
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: Frame3D
  Params:  none
  Returns: Nothing

  Draws a 3d border in the native drawer style.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Frame3d(DC : HDC; var ARect : TRect;
  const FrameWidth : integer; const Style : TBevelCut) : boolean;
var
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[TCDWidgetSet.Frame3d Rect=', dbgs(ARect));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then exit;

  LazDC := TLazCanvas(DC);

  GetDefaultDrawer().DrawFrame3D(LazDC, Types.Point(ARect.Left, ARect.Top),
    Types.Size(ARect), FrameWidth, Style);

  InflateRect(ARect, -FrameWidth, -FrameWidth);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: FrameRect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH): Integer;
var
  LazDC: TLazCanvas absolute DC;
  lOldBrush, lOldPen, lFramePen, lFrameBrush: HGDIOBJ;
  lLogPen: TLogPen;
  lLogBrush: TLogBrush;
begin
  Result := 0;

  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI FillRect Rect=', dbgs(ARect),' Brush=', dbghex(hBr));
  {$endif}

  if not IsValidDC(DC) then
    exit;
  if not IsValidGdiObject(hBr) then
    exit;

  // Creates temporary pen and brush to help the drawing
  lLogPen.lopnStyle := PS_SOLID;
  lLogPen.lopnWidth := Types.Point(1, 1);
  lLogPen.lopnColor := FPColorToTColor(TFPCustomBrush(hBR).FPColor);
  lFramePen := CreatePenIndirect(lLogPen);

  lLogBrush.lbStyle := BS_NULL;
  lFrameBrush := CreateBrushIndirect(lLogBrush);

  // Do the drawing
  lOldBrush := SelectObject(DC, lFrameBrush);
  lOldPen := SelectObject(DC, lFramePen);
  LazDC.Rectangle(ARect);
  SelectObject(DC, lOldBrush);
  SelectObject(DC, lOldPen);

  // Delete the helper objects
  DeleteObject(lFramePen);
  DeleteObject(lFrameBrush);

  Result := 1;
end;

(*function TQtWidgetSet.GetActiveWindow: HWND;
var
  Widget: QWidgetH;
  W: TQtWidget;
  SubW: TQtWidget;
  Area: QMdiAreaH;
begin
  Widget := QApplication_activeWindow;
  if Widget <> nil then
  begin
    W := QtObjectFromWidgetH(Widget);
    if W <> nil then
    begin
      if TQtMainWindow(W).MDIAreaHandle <> nil then
      begin
        Area := QMdiAreaH(TQtMainWindow(W).MDIAreaHandle.Widget);
        SubW := QtObjectFromWidgetH(QMdiArea_activeSubWindow(Area));
        if SubW <> nil then
          Result := HWND(SubW)
        else
          Result := HWND(W);
      end else
        Result := HWND(W);
    end;
  end else
    Result := 0;
end;


{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetBitmapBits
  Params:  none
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
var
  Image: QImageH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetBitmapBits]',' Bitmap=', dbghex(Bitmap),' Count=',Count);
  {$endif}

  Result := 0;

  if (Bitmap = 0) or (Count <= 0) then
    Exit;

  Image := QImage_create(TQtImage(Bitmap).FHandle);
  try
    Result := (QImage_width(Image) * QImage_height(Image) * QImage_depth(Image) + 7) div 8;
    if Count < Result then
      Result := Count;
    if Result > 0 then
      Move(QImage_bits(Image)^, Bits^, Result);
  finally
    QImage_destroy(Image);
  end;
end;

function TQtWidgetSet.GetBkColor(DC: HDC): TColorRef;
var
  QtDC: TQtDeviceContext;
begin
  Result := CLR_INVALID;
  if not IsValidDC(DC) then Exit;
  QtDC := TQtDeviceContext(DC);
  Result := QtDC.GetBkColor;
end;

function TQtWidgetSet.GetCapture: HWND;
var
  w: QWidgetH;
  Widget: TQtWidget;
  {$IFDEF MSWINDOWS}
  AWin: HWND;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  AWin := Windows.GetCapture;
  if AWin <> 0 then
    w := QWidget_find(AWin)
  else
    w := nil;

  if (w = nil) and (QApplication_mouseButtons() > 0) then
    w := QApplication_focusWidget()
  else
    if w <> QWidget_mouseGrabber then
      w := QWidget_mouseGrabber;

  {$ELSE}
  w := QWidget_mouseGrabber();
  {$ENDIF}

  if w <> nil then
  begin
    // Capture widget can be child of complex control. In any case we should return TQtWidget as result.
    // So we will look for parent while not found apropriate LCL handle.
    Widget := GetFirstQtObjectFromWidgetH(w);
    Result := HWND(Widget);
  end
  else
    Result := 0;
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI GetCapture] Capture = ', Result);
  {$endif}
end;

function TQtWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := QtCaret.GetCaretPos(lpPoint);
end;

function TQtWidgetSet.GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean;
begin
  ShowHideOnFocus := QtCaret.GetQtCaretRespondToFocus;
  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
var
  lObject: TObject;
begin
  {$ifdef VerboseCDWinAPI}
  DebugLn(Format(':>[WinAPI GetClientBounds] Handle=%x', [Handle]));
  {$endif}
  // ToDO check if the window is native or not and process accordingly
  // For now just assume it is native
  Result := False;
  if Handle=0 then Exit;
  lObject := TObject(Handle);
  if lObject is TCDForm then
  begin
    // Initial size guessed
    if TCDForm(lObject).Image <> nil then
      ARect := Bounds(0, 0, TCDForm(lObject).Image.Width, TCDForm(lObject).Image.Height)
    else ARect := Bounds(0, 0, 0, 0);

    // Now ask for the real size
    Result := BackendGetClientBounds(Handle, ARect)
  end
  else
  begin
    // If we return WinControl.BoundsRect then the controls get a x2 factor
    // when Align=alClient, strange. Region.GetBoundingRect() works fine.
    //  ARect := TCDWinControl(lObject).WinControl.BoundsRect; <<-- don't do this

    ARect := TCDWinControl(lObject).Region.GetBoundingRect();
  end;
  {$ifdef VerboseCDWinAPI}
  DebugLn(Format(':<[WinAPI GetClientBounds] ARect.Left=%d ARect.Top=%d'
    + ' ARect.Right=%d ARect.Bottom=%d',
    [ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
  Left and Top are always 0,0
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseCDWinAPI}
  DebugLn(Format('[WinAPI GetClientRect] Handle=%x', [Handle]));
  {$endif}
  GetClientBounds(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetClipBox
  Params: dc, lprect
  Returns: Integer

  Returns the smallest rectangle which includes the entire current
  Clipping Region, or if no Clipping Region is set, the current
  dimensions of the Drawable.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  LazDC: TLazCanvas;
  lClipRegion: TFPCustomRegion;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetClipBox] DC ' + dbghex(DC));
  {$endif}

  Result := NULLREGION;
  if lpRect <> nil then
    lpRect^ := Types.Rect(0,0,0,0);

  if DC = 0 then DC := HDC(ScreenDC);

  if not IsValidDC(DC) then
    Result := ERROR;

  if Result = ERROR then Exit;

  LazDC := TLazCanvas(DC);

  if (lpRect<>nil) then
  begin
    lClipRegion := LazDC.ClipRegion;
    if lClipRegion = nil then
    begin
      Result := NULLREGION;
      lpRect^ := Types.Bounds(0, 0, LazDC.Width, LazDC.Height);
    end
    else
    begin
      Result := SIMPLEREGION;
      lpRect^ := lClipRegion.GetBoundingRect();
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer

  This routine assumes that RGN has been created previously
  and it copies the current Clipping Region to RGN

  The result can be one of the following constants
     0 = no clipping set
     1 = ok
    -1 = error
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN): Longint;
var
  LazDC: TLazCanvas absolute DC;
  lDestRegion: TLazRegion absolute RGN;
  lDCRegion: TLazRegion;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetClipRGN] DC ' + dbghex(DC));
  {$endif}

  Result := -1;
  if not IsValidDC(DC) then exit;
  if Rgn = 0 then Exit;

  lDCRegion := TLazRegion(LazDC.ClipRegion);
  if lDCRegion = nil then
    Result := 0
  else
  begin
    lDestRegion.Assign(lDCRegion);
    Result := 1;
  end;
end;

(*function TQtWidgetSet.GetCmdLineParamDescForInterface: string;
  function b(const s: string): string;
  begin
    Result:=BreakString(s,75,22)+LineEnding+LineEnding;
  end;
begin
  Result:=
     b(rsqtOptionNoGrab)
    +b(rsqtOptionDoGrab)
    +b(rsqtOptionSync)
    +b(rsqtOptionStyle)
    +b(rsqtOptionStyleSheet)
    +b(rsqtOptionGraphicsStyle)
    +b(rsqtOptionSession)
    +b(rsqtOptionWidgetCount)
    +b(rsqtOptionReverse)
    {$IFDEF HASX11}
    +b(rsqtOptionX11Display)
    +b(rsqtOptionX11Geometry)
    +b(rsqtOptionX11Font)
    +b(rsqtOptionX11BgColor)
    +b(rsqtOptionX11FgColor)
    +b(rsqtOptionX11BtnColor)
    +b(rsqtOptionX11Name)
    +b(rsqtOptionX11Title)
    +b(rsqtOptionX11Visual)
    +b(rsqtOptionX11NCols)
    +b(rsqtOptionX11CMap)
    +b(rsqtOptionX11IM)
    +b(rsqtOptionX11InputStyle)
    {$ENDIF}
    ;
end;*)

{------------------------------------------------------------------------------
  Method: GetCurrentObject
  Params:
    DC - A handle to the DC
    uObjectType - The object type to be queried
  Returns: If the function succeeds, the return value is a handle to the specified object.
    If the function fails, the return value is NULL.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetCurrentObject(DC: HDC; uObjectType: UINT): HGDIOBJ;
var
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format('[TCDWidgetSet.GetCurrentObject uObjectType=%d', [uObjectType]));
  {$endif}

  Result := 0;
  if not IsValidDC(DC) then exit;
  LazDC := TLazCanvas(DC);

  case uObjectType of
    OBJ_BITMAP: Result := HGDIOBJ(LazDC.SelectedBitmap);
    OBJ_BRUSH: Result := HGDIOBJ(LazDC.AssignedBrush);
    OBJ_FONT: Result := HGDIOBJ(LazDC.AssignedFont);
    OBJ_PEN: Result := HGDIOBJ(LazDC.AssignedPen);
  end;
end;

(*{------------------------------------------------------------------------------
  Function: GetCursorPos
  Params:  lpPoint: The cursorposition
  Returns: True if succesful

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetCursorPos(var lpPoint: TPoint ): Boolean;
var
  vPoint: TQtPoint;
begin
  QCursor_pos(@vPoint);

  lpPoint.x := vPoint.x;
  lpPoint.y := vPoint.y;

  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: GetDC
  Params:  hWnd is any widget.
  Returns: Nothing

  This function is Called:
  - Once on app startup with hWnd = 0
  - Twice for every TLabel on the TCustomLabel.CalcSize function
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetDC(hWnd: HWND): HDC;
var
  lObject: TObject;
  lWinControl: TWinControl;
  lFormHandle: TCDForm;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(':>[WinAPI GetDC] hWnd: ', dbghex(hWnd));
  {$endif}

  Result := 0;

  // Screen DC
  if HWnd = 0 then Result := HDC(CDWidgetset.ScreenDC);

  // Invalid DC
  if not IsValidDC(HWnd) then Exit;

  lObject := TObject(HWnd);

  // Control DC -> Search for the corresponding form
  if lObject is TCDWinControl then
  begin
    lWinControl := TCDWinControl(lObject).WinControl;
    lWinControl := Forms.GetParentForm(lWinControl);
    lFormHandle := TCDForm(lWinControl.Handle);
  end
  // Form DC
  else if lObject is TCDForm then
    lFormHandle := TCDForm(hWnd)
  else
    raise Exception.Create('Invalid handle for GetDC');

  // Now get Form DC
  Result := HDC(lFormHandle.Canvas);

  // If the Form DC doesn't yet exist, just give the ScreenDC
  // Anyone asking for a DC outside the Paint event can't expect
  // to receive something which can be drawn to anyway
  if Result = 0 then Result := HDC(CDWidgetset.ScreenDC);

  {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI GetDC] Result: ', dbghex(Result));
  {$endif}
end;

(*function TQtWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC;
  WindowHandle: HWND; var OriginDiff: TPoint): boolean;
var
  QtDC: TQtDeviceContext absolute PaintDC;
  Matrix: QTransformH;
  P: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDCOriginRelativeToWindow] PaintDC ' + dbghex(PaintDC));
  {$endif}
  Result := IsValidDC(PaintDC);
  if not Result then
    exit;
  Matrix := QPainter_transform(QtDC.Widget);
  OriginDiff := Point(0, 0);
  P := Point(0, 0);
  if WindowHandle <> 0 then
    P := TQtWidget(WindowHandle).getClientOffset;
  if Matrix <> nil then
  begin
    OriginDiff.X := Round(QTransform_Dx(Matrix)) - P.X;
    OriginDiff.Y := Round(QTransform_Dy(Matrix)) - P.Y;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetDeviceCaps
  Params: DC: HDC; Index: Integer
  Returns: Integer

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceCaps(DC: HDC; Index: Integer): Integer;
var
  QtDC: TQtDeviceContext;
  PaintDevice: QPaintDeviceH;
  PaintEngine: QPaintEngineH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceCaps] DC ' + dbghex(DC));
  {$endif}

  Result := 0;
  if DC = 0 then
    DC := HDC(QtScreenContext);

  if not IsValidDC(DC) then exit;

  QtDC := TQtDeviceContext(DC);

  PaintEngine := QtDC.PaintEngine;
  if PaintEngine = nil then
    exit;
  PaintDevice := QPaintEngine_paintDevice(PaintEngine);

  case Index of
    HORZSIZE:
      Result := QPaintDevice_widthMM(PaintDevice);
    VERTSIZE:
      Result := QPaintDevice_heightMM(PaintDevice);
    HORZRES:
      Result := QPaintDevice_width(PaintDevice);
    BITSPIXEL:
      Result := QPaintDevice_depth(PaintDevice);
    PLANES:
      Result := 1;
    SIZEPALETTE:
      Result := QPaintDevice_numColors(PaintDevice);
    LOGPIXELSX:
      Result := QPaintDevice_logicalDpiX(PaintDevice);
    LOGPIXELSY:
      Result := QPaintDevice_logicalDpiY(PaintDevice);
    VERTRES:
      Result := QPaintDevice_height(PaintDevice);
    NUMRESERVED:
      Result := 0;
    else
      Result := 0;
  end;
end;*)

function TCDWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
Var
  ScrSize: TPoint;
  LazDC: TLazCanvas;
begin
  Result:= False;

  // Screen size
  if IsScreenDC(DC) or (DC = 0) then
  begin
    P.X:= GetSystemMetrics(SM_CXSCREEN);
    P.Y:= GetSystemMetrics(SM_CYSCREEN);
    Exit(True);
  end;

  if not IsValidDC(DC) then exit;
  LazDC := TLazCanvas(DC);

  P.X := LazDC.Width;
  P.Y := LazDC.Height;

  Result := True;
end;

(*function TQtWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetDIBits] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: GetDoubleClickTime
  Params: none
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDoubleClickTime: UINT;
begin
  Result := QApplication_doubleClickInterval;
end;*)

{------------------------------------------------------------------------------
  Function: GetFocus
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetFocus: HWND;
begin
  Result := 0;
  // Don't return the intfcontrol, we try to pretend it doesn't exist
  {if FocusedIntfControl <> nil then Result := FocusedIntfControl.Handle
  else}
  if FocusedControl <> nil then Result := FocusedControl.Handle;
end;

(*function TQtWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
const
  StateDown    = SmallInt($FF80);
  {StateToggled = SmallInt($0001);}
begin
  Result := 0;

  case nVirtKey of
    VK_LSHIFT:   nVirtKey := VK_SHIFT;
    VK_LCONTROL: nVirtKey := VK_CONTROL;
    VK_LMENU:    nVirtKey := VK_MENU;
  end;

  // where to track toggle state?

  case nVirtKey of
    VK_LBUTTON:
      if (QApplication_mouseButtons and QtLeftButton) > 0 then
        Result := Result or StateDown;
    VK_RBUTTON:
      if (QApplication_mouseButtons and QtRightButton) > 0 then
        Result := Result or StateDown;
    VK_MBUTTON:
      if (QApplication_mouseButtons and QtMidButton) > 0 then
        Result := Result or StateDown;
    VK_XBUTTON1:
      if (QApplication_mouseButtons and QtXButton1) > 0 then
        Result := Result or StateDown;
    VK_XBUTTON2:
      if (QApplication_mouseButtons and QtXButton2) > 0 then
        Result := Result or StateDown;
    VK_MENU:
      if (QApplication_keyboardModifiers and QtAltModifier) > 0 then
        Result := Result or StateDown;
    VK_SHIFT:
      if (QApplication_keyboardModifiers and QtShiftModifier) > 0 then
        Result := Result or StateDown;
    VK_CONTROL:
      if (QApplication_keyboardModifiers and QtControlModifier) > 0 then
        Result := Result or StateDown;
    VK_LWIN, VK_RWIN:
      if (QApplication_keyboardModifiers and QtMetaModifier) > 0 then
        Result := Result or StateDown;
   {$ifdef VerboseQtWinAPI}
    else
      DebugLn('TQtWidgetSet.GetKeyState TODO ', DbgSVKCode(Word(nVirtkey)));
   {$endif}
  end;
end;

function TQtWidgetSet.GetMapMode(DC: HDC): Integer;
begin
  if IsValidDC(DC) then
    Result := TQtDeviceContext(DC).vMapMode
  else
    Result := 0;
end;
*)
function TCDWidgetSet.GetMonitorInfo(Monitor: HMONITOR; lpmi: PMonitorInfo): Boolean;
begin
  Result := (lpmi <> nil) and (lpmi^.cbSize >= SizeOf(TMonitorInfo)) and (Monitor = 1);
  if not Result then Exit;
  lpmi^.rcMonitor:=Types.Rect(0, 0, GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
  lpmi^.rcWork:=lpmi^.rcMonitor;
  lpmi^.dwFlags := MONITORINFOF_PRIMARY
end;
(*
{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceSize
  Params:  none
  Returns: True if successful

  Return the size of a device
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceSize]');
  {$endif}

  Result := False;

  P.X := 0;
  P.Y := 0;

  if not IsValidDC(DC) then Exit;

  if (TObject(DC) is TQtDeviceContext) then
    P := TQtDeviceContext(DC).getDeviceSize;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetObject
  Params:  none
  Returns: The size written to the buffer

  Necessary for TBitmap support
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetObject(GDIObj: HGDIOBJ; BufSize: Integer; Buf: Pointer): Integer;
const
  QtPenStyleToWinStyleMap: array[QtPenStyle] of UINT =
  (
 { QtNoPen          } PS_NULL,
 { QtSolidLine      } PS_SOLID,
 { QtDashLine       } PS_DASH,
 { QtDotLine        } PS_DOT,
 { QtDashDotLine    } PS_DASHDOT,
 { QtDashDotDotLine } PS_DASHDOTDOT,
 { QtCustomDashLine } PS_USERSTYLE
  );
var
  aObject: TObject;
  AFont: TQtFont absolute aObject;
  APen: TQtPen absolute aObject;
  ABrush: TQtBrush absolute aObject;
  BitmapSection : TDIBSECTION;
  ALogFont: PLogFont absolute Buf;
  ALogPen: PLogPen absolute Buf;
  AExtLogPen: PExtLogPen absolute Buf;
  ALogBrush: PLogBrush absolute Buf;
  Dashes: TQRealArray;
  i: integer;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetObject] GDIObj: ' + dbghex(GDIObj));
    ObjType := '';
  {$endif}

  Result := 0;

  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI GetObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TQtFont then
  begin
    if Buf = nil then
      Result := SizeOf(TLogFont)
    else
    if BufSize >= SizeOf(TLogFont) then
    begin
      Result := SizeOf(TLogFont);

      FillChar(ALogFont^, SizeOf(ALogFont^), 0);
      ALogFont^.lfHeight := AFont.getPixelSize;
      ALogFont^.lfEscapement := AFont.Angle;
      case AFont.getWeight of
        10: ALogFont^.lfWeight := FW_THIN;
        15: ALogFont^.lfWeight := FW_EXTRALIGHT;
        25: ALogFont^.lfWeight := FW_LIGHT;
        50: ALogFont^.lfWeight := FW_NORMAL;
        55: ALogFont^.lfWeight := FW_MEDIUM;
        63: ALogFont^.lfWeight := FW_SEMIBOLD;
        75: ALogFont^.lfWeight := FW_BOLD;
        80: ALogFont^.lfWeight := FW_EXTRABOLD;
        87: ALogFont^.lfWeight := FW_HEAVY;
      end;

      ALogFont^.lfItalic := Ord(AFont.getItalic) * High(Byte);
      ALogFont^.lfUnderline := Ord(AFont.getUnderline) * High(Byte);
      ALogFont^.lfStrikeOut := Ord(AFont.getStrikeOut) * High(Byte);
      ALogFont^.lfCharSet := DEFAULT_CHARSET;
      case AFont.getStyleStategy of
        QFontPreferMatch: ALogFont^.lfQuality := DRAFT_QUALITY;
        QFontPreferQuality: ALogFont^.lfQuality := PROOF_QUALITY;
        QFontNoAntialias: ALogFont^.lfQuality := NONANTIALIASED_QUALITY;
        QFontPreferAntialias: ALogFont^.lfQuality := ANTIALIASED_QUALITY;
      else
        ALogFont^.lfQuality := DEFAULT_QUALITY;
      end;
      ALogFont^.lfFaceName := UTF16ToUTF8(AFont.getFamily);
    end;
  end
  {------------------------------------------------------------------------------
    Pen
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtPen then
  begin
    if not APen.IsExtPen then
    begin
      if Buf = nil then
        Result := SizeOf(TLogPen)
      else
      if BufSize >= SizeOf(TLogPen) then
      begin
        Result := SizeOf(TLogPen);
        TQColorToColorRef(APen.getColor, ALogPen^.lopnColor);
        if APen.getCosmetic then
          ALogPen^.lopnWidth := Point(1, 0)
        else
          ALogPen^.lopnWidth := Point(APen.getWidth, 0);
        ALogPen^.lopnStyle := QtPenStyleToWinStyleMap[APen.getStyle];
      end;
    end
    else
    begin
      i := SizeOf(TExtLogPen);
      if APen.getStyle = QtCustomDashLine then
      begin
        Dashes := APen.getDashPattern;
        inc(i, (Length(Dashes) - 1) * SizeOf(DWord));
      end
      else
        Dashes := nil;
      if Buf = nil then
        Result := i
      else
      if BufSize >= i then
      begin
        Result := i;
        AExtLogPen^.elpPenStyle := QtPenStyleToWinStyleMap[APen.getStyle];

        if not APen.getCosmetic then
        begin
          AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_GEOMETRIC;

          case APen.getJoinStyle of
            QtMiterJoin: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_MITER;
            QtBevelJoin: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_BEVEL;
            QtRoundJoin: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_JOIN_ROUND;
          end;

          case APen.getCapStyle of
            QtFlatCap: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_FLAT;
            QtSquareCap: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_SQUARE;
            QtRoundCap: AExtLogPen^.elpPenStyle := AExtLogPen^.elpPenStyle or PS_ENDCAP_ROUND;
          end;

          AExtLogPen^.elpWidth := APen.getWidth;
        end
        else
          AExtLogPen^.elpWidth := 1;

        AExtLogPen^.elpBrushStyle := BS_SOLID;
        TQColorToColorRef(APen.getColor, AExtLogPen^.elpColor);
        AExtLogPen^.elpHatch := 0;

        AExtLogPen^.elpNumEntries := Length(Dashes);
        if AExtLogPen^.elpNumEntries > 0 then
        begin
          for i := 0 to AExtLogPen^.elpNumEntries - 1 do
            PDword(@AExtLogPen^.elpStyleEntry)[i] := Trunc(Dashes[i]);
        end
        else
          AExtLogPen^.elpStyleEntry[0] := 0;
      end;
    end;
  end
  {------------------------------------------------------------------------------
    Region
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtRegion then
  begin
    {TODO: implement Region}
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Region';
    {$endif}
  end else
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  if aObject is TQtBrush then
  begin
    if Buf = nil then
      Result := SizeOf(TLogBrush)
    else
    if BufSize >= SizeOf(TLogBrush) then
    begin
      Result := SizeOf(TLogBrush);
      TQColorToColorRef(ABrush.getColor^, ALogBrush^.lbColor);
      ABrush.GetLbStyle(ALogBrush^.lbStyle, ALogBrush^.lbHatch);
    end;
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    if Buf = nil then
      Result := SizeOf(TDIBSECTION)
    else
    begin
      BitmapSection.dsOffset := 0;
      FillChar(BitmapSection, SizeOf(TDIBSECTION), 0);

      with TQtImage(aObject) do
      begin
        {dsBM - BITMAP}
        BitmapSection.dsBm.bmType := $4D42;
        BitmapSection.dsBm.bmWidth := width;
        BitmapSection.dsBm.bmHeight := height;
        BitmapSection.dsBm.bmWidthBytes := bytesPerLine;
        BitmapSection.dsBm.bmPlanes := 1;//Does Bitmap Format support more?
        BitmapSection.dsBm.bmBitsPixel := depth;
        BitmapSection.dsBm.bmBits := bits;

        {dsBmih - BITMAPINFOHEADER}
        BitmapSection.dsBmih.biSize := 40;
        BitmapSection.dsBmih.biWidth := BitmapSection.dsBm.bmWidth;
        BitmapSection.dsBmih.biHeight := BitmapSection.dsBm.bmHeight;
        BitmapSection.dsBmih.biPlanes := BitmapSection.dsBm.bmPlanes;
        BitmapSection.dsBmih.biBitCount := BitmapSection.dsBm.bmBitsPixel;

        BitmapSection.dsBmih.biCompression := 0;

        BitmapSection.dsBmih.biSizeImage := numBytes;
        BitmapSection.dsBmih.biXPelsPerMeter := dotsPerMeterX;
        BitmapSection.dsBmih.biYPelsPerMeter := dotsPerMeterY;

        BitmapSection.dsBmih.biClrUsed := 0;
        BitmapSection.dsBmih.biClrImportant := 0;
      end;

      if BufSize >= SizeOf(BitmapSection) then
      begin
        PDIBSECTION(Buf)^ := BitmapSection;
        Result := SizeOf(TDIBSECTION);
      end
      else if BufSize > 0 then
      begin
        Move(BitmapSection, Buf^, BufSize);
        Result := BufSize;
      end;
    end;
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;*)

function TCDWidgetSet.GetParent(Handle : HWND): HWND;
var
  lHandle: TCDWinControl absolute Handle;
  lWinControl: TWinControl;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.GetParent] Handle: ', [Handle]));
  {$endif}

  Result := 0;

  // Invalid DC
  if Handle = 0 then Exit;
  if not IsValidDC(Handle) then Exit;

  lWinControl := lHandle.GetWinControl();
  if lWinControl = nil then Exit;
  lWinControl := lWinControl.Parent;
  if lWinControl = nil then Exit;
  Result := lWinControl.Handle;
end;

function TCDWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
begin
  if Handle<>0 then
    result := TCDWinControl(Handle).Props[str]
  else
    result := nil;
end;

function TCDWidgetSet.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
var
  lLazRegion: TLazRegion absolute RGN;
begin
  if RGN = 0 then
  begin
    Result := ERROR;
    if lpRect <> nil then lpRect^ := Types.Rect(0,0,0,0);
    Exit();
  end;

  //Result := lLazRegion.IsSimpleRectRegion(); TQtRegion(RGN).GetRegionType;
  Result := SIMPLEREGION;
  if lpRect <> nil then lpRect^ := lLazRegion.GetBoundingRect();

  {$ifdef VerboseCDWinAPI}
  Debugln('Trace:> [WinAPI GetRgnBox] Handle: ' + dbghex(RGN));
  {$endif}
end;

(*function TQtWidgetSet.GetROP2(DC: HDC): Integer;
var
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('> TQtWidgetSet.GetROP2() DC ',dbghex(DC));
  {$endif}
  Result := R2_COPYPEN;
  if not IsValidDC(DC) then
    exit;
  Result := QtDC.Rop2;
  {$ifdef VerboseQtWinAPI}
  writeln('< TQtWidgetSet.GetROP2() DC ',dbghex(DC),' Result ',Result);
  {$endif}
end;

function TQtWidgetSet.GetScrollBarSize(Handle: HWND; BarKind: Integer): integer;
var
  w: TQtWidget;
  ScrollBar: TQtScrollBar;
begin
	{$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetScrollBarSize] Handle: ' + dbghex(Handle),' BarKind: ',BarKind);
  {$endif}
  Result := 0;
  if Handle = 0 then exit;

  w := TQtWidget(Handle);

  {TODO: find out what to do with TCustomForm descendants }
  if w is TQtAbstractScrollArea then
  begin
    if BarKind in [SM_CXVSCROLL, SM_CYVSCROLL] then
      ScrollBar := TQtAbstractScrollArea(w).verticalScrollBar
    else
      ScrollBar := TQtAbstractScrollArea(w).horizontalScrollBar;
  end else
  if w is TQtScrollBar then
    ScrollBar := TQtScrollBar(w)
  else
    ScrollBar := nil;
  if ScrollBar <> nil then
  begin
    if BarKind in [SM_CXHSCROLL, SM_CYVSCROLL] then
      Result := ScrollBar.getWidth
    else
      Result := ScrollBar.getHeight;
  end;
end;

function TQtWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
var
  w: TQtWidget;
  ScrollBar: TQtScrollBar;
begin
	{$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetScrollBarVisible] Handle: ' + dbghex(Handle),' SBStyle: ',SBStyle);
  {$endif}
  Result := False;
  if Handle = 0 then exit;

  w := TQtWidget(Handle);

  {TODO: find out what to do with TCustomForm descendants }
  if w is TQtAbstractScrollArea then
  begin
    if SBStyle = SB_VERT then
      ScrollBar := TQtAbstractScrollArea(w).verticalScrollBar
    else
      ScrollBar := TQtAbstractScrollArea(w).horizontalScrollBar;
  end else
  if w is TQtScrollBar then
    ScrollBar := TQtScrollBar(w)
  else
    ScrollBar := nil;

  if ScrollBar <> nil then
    Result := ScrollBar.getVisible;
end;

{------------------------------------------------------------------------------
  Function: GetScrollInfo
  Params: BarFlag
           SB_CTL Retrieves the parameters for a scroll bar control. The hwnd
           parameter must be the handle to the scroll bar control.
           SB_HORZ Retrieves the parameters for the window's standard horizontal
           scroll bar.
           SB_VERT Retrieves the parameters for the window's standard vertical
           scroll bar.

          ScrollInfo returns TScrollInfo structure.

  Returns: boolean

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
var
  QtScrollBar: TQtScrollBar;
begin
  Result := False;

  if Handle = 0 then exit;

  if (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) or
   (csFreeNotification in TQtWidget(Handle).LCLObject.ComponentState) then
    exit;

  QtScrollBar := nil;

  if not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomScrollBar) then
  begin
    if (TQtWidget(Handle) is TQtAbstractScrollArea) then
    begin
      case BarFlag of
        SB_HORZ: QtScrollBar := TQtAbstractScrollArea(Handle).horizontalScrollBar;
        SB_VERT: QtScrollBar := TQtAbstractScrollArea(Handle).verticalScrollBar;
      end;
    end else
      Result := False;
  end
  else
    QtScrollBar := TQtScrollBar(TScrollBar(TQtWidget(Handle).LCLObject).Handle);

  if Assigned(QtScrollBar) then
  begin
    // POS
    if (ScrollInfo.fMask and SIF_POS) <> 0 then
    begin
      if QtScrollBar.ChildOfComplexWidget = ccwAbstractScrollArea then
        ScrollInfo.nPos := QtScrollBar.getSliderPosition
      else
        ScrollInfo.nPos := QtScrollBar.getValue;
    end;

    // RANGE
    if (ScrollInfo.fMask and SIF_RANGE) <> 0 then
    begin
      ScrollInfo.nMin:= QtScrollBar.getMin;
      ScrollInfo.nMax:= QtScrollBar.getMax + QtScrollBar.getPageStep;
    end;
    // PAGE
    if (ScrollInfo.fMask and SIF_PAGE) <> 0 then
      ScrollInfo.nPage := QtScrollBar.getPageStep;

    // TRACKPOS
    if (ScrollInfo.fMask and SIF_TRACKPOS) <> 0 then
      ScrollInfo.nTrackPos := QtScrollBar.getSliderPosition;

    Result := True;
  end;
end;*)

function TCDWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(Format('Trace:> [WinAPI GetStockObject] Value: %d', [Value]));
  {$endif}

  Result := 0;

  case Value of
    BLACK_BRUSH:         // Black brush.
      Result := THandle(FStockBlackBrush);
    DKGRAY_BRUSH:        // Dark gray brush.
      Result := THandle(FStockDKGrayBrush);
    GRAY_BRUSH:          // Gray brush.
      Result := THandle(FStockGrayBrush);
    LTGRAY_BRUSH:        // Light gray brush.
      Result := THandle(FStockLtGrayBrush);
    NULL_BRUSH:          // Null brush (equivalent to HOLLOW_BRUSH).
      Result := THandle(FStockNullBrush);
    WHITE_BRUSH:         // White brush.
      Result := THandle(FStockWhiteBrush);

    BLACK_PEN:           // Black pen.
      Result := THandle(FStockBlackPen);
    NULL_PEN:            // Null pen.
      Result := THandle(FStockNullPen);
    WHITE_PEN:           // White pen.
      Result := THandle(FStockWhitePen);

    {System font. By default, Windows uses the system font to draw menus,
     dialog box controls, and text. In Windows versions 3.0 and later,
     the system font is a proportionally spaced font; earlier versions of
     Windows used a monospace system font.}
    DEFAULT_GUI_FONT, SYSTEM_FONT:
      Result := THandle(FDefaultGUIFont);

  {$ifdef VerboseCDWinAPI}
  else
    DebugLn(Format('[WinAPI GetStockObject] UNHANDLED Value: %d', [Value]));
  {$endif}
  end;
end;

{------------------------------------------------------------------------------
  Function: TCDWidgetSet.GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetSysColor(nIndex: Integer): DWORD;
begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    DebugLn('[TCDWidgetSet.GetSysColor] Unknown lcl system color: ');
    Result := 0;
    Exit;
  end;

  case nIndex of
    COLOR_SCROLLBAR               : Result:=GetDefaultDrawer().FallbackPalette.ScrollBar;
    COLOR_BACKGROUND              : Result:=GetDefaultDrawer().FallbackPalette.Background;
    COLOR_ACTIVECAPTION           : Result:=GetDefaultDrawer().FallbackPalette.ActiveCaption;
    COLOR_INACTIVECAPTION         : Result:=GetDefaultDrawer().FallbackPalette.InactiveCaption;
    COLOR_MENU                    : Result:=GetDefaultDrawer().FallbackPalette.Menu;
    COLOR_WINDOW                  : Result:=GetDefaultDrawer().FallbackPalette.Window;
    COLOR_WINDOWFRAME             : Result:=GetDefaultDrawer().FallbackPalette.WindowFrame;
    COLOR_MENUTEXT                : Result:=GetDefaultDrawer().FallbackPalette.MenuText;
    COLOR_WINDOWTEXT              : Result:=GetDefaultDrawer().FallbackPalette.WindowText;
    COLOR_CAPTIONTEXT             : Result:=GetDefaultDrawer().FallbackPalette.CaptionText;
    COLOR_ACTIVEBORDER            : Result:=GetDefaultDrawer().FallbackPalette.ActiveBorder;
    COLOR_INACTIVEBORDER          : Result:=GetDefaultDrawer().FallbackPalette.InactiveBorder;
    COLOR_APPWORKSPACE            : Result:=GetDefaultDrawer().FallbackPalette.AppWorkspace;
    COLOR_HIGHLIGHT               : Result:=GetDefaultDrawer().FallbackPalette.Highlight;
    COLOR_HIGHLIGHTTEXT           : Result:=GetDefaultDrawer().FallbackPalette.HighlightText;
    COLOR_BTNFACE                 : Result:=GetDefaultDrawer().FallbackPalette.BtnFace;
    COLOR_BTNSHADOW               : Result:=GetDefaultDrawer().FallbackPalette.BtnShadow;
    COLOR_GRAYTEXT                : Result:=GetDefaultDrawer().FallbackPalette.GrayText;
    COLOR_BTNTEXT                 : Result:=GetDefaultDrawer().FallbackPalette.BtnText;
    COLOR_INACTIVECAPTIONTEXT     : Result:=GetDefaultDrawer().FallbackPalette.InactiveCaptionText;
    COLOR_BTNHIGHLIGHT            : Result:=GetDefaultDrawer().FallbackPalette.BtnHighlight;
    COLOR_3DDKSHADOW              : Result:=GetDefaultDrawer().FallbackPalette.color3DDkShadow;
    COLOR_3DLIGHT                 : Result:=GetDefaultDrawer().FallbackPalette.color3DLight;
    COLOR_INFOTEXT                : Result:=GetDefaultDrawer().FallbackPalette.InfoText;
    COLOR_INFOBK                  : Result:=GetDefaultDrawer().FallbackPalette.InfoBk;
    //
    COLOR_HOTLIGHT                : Result:=GetDefaultDrawer().FallbackPalette.HotLight;
    COLOR_GRADIENTACTIVECAPTION   : Result:=GetDefaultDrawer().FallbackPalette.GradientActiveCaption;
    COLOR_GRADIENTINACTIVECAPTION : Result:=GetDefaultDrawer().FallbackPalette.GradientInactiveCaption;
    COLOR_MENUHILIGHT             : Result:=GetDefaultDrawer().FallbackPalette.MenuHighlight;
    COLOR_MENUBAR                 : Result:=GetDefaultDrawer().FallbackPalette.MenuBar;
    //
    COLOR_FORM                    : Result:=GetDefaultDrawer().FallbackPalette.Form;
  else
    Result:=0;
  end;
end;

(*function TQtWidgetSet.GetSysColorBrush(nIndex: Integer): HBrush;

  function GetBrush(Group: QPaletteColorGroup; Role: QPaletteColorRole; ClassName: PAnsiChar = nil): HBrush;
  var
    Handle: QPaletteH;
  begin
    Handle := QPalette_create;
    if ClassName = nil then
      QApplication_palette(Handle)
    else
      QApplication_palette(Handle, ClassName);
    if FSysColorBrushes[nIndex] = 0 then
      Result := HBrush(TQtBrush.Create(False))
    else
      Result := FSysColorBrushes[nIndex];
    TQtBrush(Result).FHandle := QBrush_create(QPalette_brush(Handle, Group, Role));
    TQtBrush(Result).FShared := True;

    QPalette_destroy(Handle);
  end;

  function GetSolidBrush(AColor: TColor): HBrush;
  var
    Color: TQColor;
  begin
    if FSysColorBrushes[nIndex] = 0 then
      Result := HBrush(TQtBrush.Create(True))
    else
      Result := FSysColorBrushes[nIndex];
    Color := QBrush_Color(TQtBrush(Result).FHandle)^;
    ColorRefToTQColor(ColorToRGB(AColor), Color);
    QBrush_setColor(TQtBrush(Result).FHandle, @Color);
    TQtBrush(Result).FShared := True;
  end;

begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    Result := 0;
    Exit;
  end;

  if (FSysColorBrushes[nIndex] = 0) or
    (
    (FSysColorBrushes[nIndex] <> 0) and
    (TQtBrush(FSysColorBrushes[nIndex]).FHandle = nil)
    ) then
  begin
    case nIndex of
      COLOR_SCROLLBAR               : Result:=GetBrush(QPaletteActive,   QPaletteButton);
      COLOR_BACKGROUND              : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
      COLOR_WINDOW                  : Result:=GetBrush(QPaletteInActive, QPaletteBase);
      COLOR_WINDOWFRAME             : Result:=GetBrush(QPaletteActive,   QPaletteShadow);
      COLOR_WINDOWTEXT              : Result:=GetBrush(QPaletteActive,   QPaletteWindowText);
      COLOR_ACTIVEBORDER            : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
      COLOR_INACTIVEBORDER          : Result:=GetBrush(QPaletteInactive, QPaletteWindow);
      COLOR_APPWORKSPACE            : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
      COLOR_HIGHLIGHT               : Result:=GetBrush(QPaletteActive,   QPaletteHighlight);
      COLOR_HIGHLIGHTTEXT           : Result:=GetBrush(QPaletteActive,   QPaletteHighlightedText);
      COLOR_BTNFACE                 : Result:=GetBrush(QPaletteActive,   QPaletteButton);
      COLOR_BTNSHADOW               : Result:=GetBrush(QPaletteActive,   QPaletteDark);
      COLOR_GRAYTEXT                : Result:=GetBrush(QPaletteActive,   QPaletteText);
      COLOR_BTNTEXT                 : Result:=GetBrush(QPaletteActive,   QPaletteButtonText);
      COLOR_BTNHIGHLIGHT            : Result:=GetBrush(QPaletteActive,   QPaletteLight);
      COLOR_3DDKSHADOW              : Result:=GetBrush(QPaletteActive,   QPaletteShadow);
      COLOR_3DLIGHT                 : Result:=GetBrush(QPaletteActive,   QPaletteMidlight);
      COLOR_INFOTEXT                : Result:=GetBrush(QPaletteInActive, QPaletteToolTipText);
      COLOR_INFOBK                  : Result:=GetBrush(QPaletteInActive, QPaletteToolTipBase);
      COLOR_HOTLIGHT                : Result:=GetBrush(QPaletteActive,   QPaletteLight);

      // qt does not provide any methods to retrieve titlebar colors
    {$IFNDEF MSWINDOWS}
      COLOR_ACTIVECAPTION           : Result:=GetBrush(QPaletteActive,   QPaletteHighlight);
      COLOR_INACTIVECAPTION         : Result:=GetBrush(QPaletteInActive, QPaletteHighlight);
      COLOR_CAPTIONTEXT             : Result:=GetBrush(QPaletteActive,   QPaletteHighlightedText);
      COLOR_INACTIVECAPTIONTEXT     : Result:=GetBrush(QPaletteInactive, QPaletteHighlightedText);
      COLOR_GRADIENTACTIVECAPTION   : Result:=GetBrush(QPaletteActive,   QPaletteBase);
      COLOR_GRADIENTINACTIVECAPTION : Result:=GetBrush(QPaletteInactive, QPaletteBase);
    {$ELSE}
      COLOR_ACTIVECAPTION           : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_ACTIVECAPTION));
      COLOR_INACTIVECAPTION         : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_INACTIVECAPTION));
      COLOR_CAPTIONTEXT             : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_CAPTIONTEXT));
      COLOR_INACTIVECAPTIONTEXT     : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_INACTIVECAPTIONTEXT));
      COLOR_GRADIENTACTIVECAPTION   : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_GRADIENTACTIVECAPTION));
      COLOR_GRADIENTINACTIVECAPTION : Result:=GetSolidBrush(Windows.GetSysColor(COLOR_GRADIENTINACTIVECAPTION));
    {$ENDIF}
      COLOR_MENU                    : Result:=GetBrush(QPaletteActive,   QPaletteButton, 'QMenu');
      COLOR_MENUTEXT                : Result:=GetBrush(QPaletteActive,   QPaletteButtonText, 'QMenu');
      COLOR_MENUHILIGHT             : Result:=GetBrush(QPaletteDisabled, QPaletteHighlight, 'QMenu');
      COLOR_MENUBAR                 : Result:=GetBrush(QPaletteActive,   QPaletteButton, 'QMenu');
      COLOR_FORM                    : Result:=GetBrush(QPaletteActive,   QPaletteWindow);
    else
      Result:=0;
    end;
    FSysColorBrushes[nIndex] := Result;
  end
  else
    Result := FSysColorBrushes[nIndex];
end;

{------------------------------------------------------------------------------
  Function: GetSystemMetrics
  Params:
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSystemMetrics(nIndex: Integer): Integer;
var
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn(Format('Trace:> [TQtWidgetSet.GetSystemMetrics] %d', [nIndex]));
  {$endif}
  Result := 0;
  case nIndex of
    SM_ARRANGE:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_ARRANGE          ');
        {$endif}
      end;
    SM_CLEANBOOT:
      begin
        {$ifdef VerboseQtWinAPI}
          WriteLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CLEANBOOT          ');
        {$endif}
      end;
    SM_CMONITORS:
      Result := QDesktopWidget_numScreens(QApplication_desktop());
    SM_CMOUSEBUTTONS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER, SM_CYBORDER:
      begin
        // size of frame around controls
        Result := QStyle_pixelMetric(QApplication_style(),
                    QStylePM_DefaultFrameWidth, nil, nil);
      end;
    SM_CXCURSOR:
      begin
        Result := 32; // recomended in docs
      end;
    SM_CYCURSOR:
      begin
        Result := 32; // recomended in docs
      end;
    SM_CXDOUBLECLK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
      end;
    SM_CXDRAG:
      begin
        Result := 2;
      end;
    SM_CYDRAG:
      begin
        Result := 2;
      end;
    SM_CXEDGE:
      begin
        Result := 2;
      end;
    SM_CYEDGE:
      begin
        Result := 2;
      end;
    SM_CXFIXEDFRAME:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHTHUMB:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON,
    SM_CYICON:
      begin
        Result := 32;
      end;
    SM_CXICONSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorWidth, nil, nil);
      end;
    SM_CYMENUSIZE:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_IndicatorHeight, nil, nil);
      end;
    SM_CXMIN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
      end;
    SM_CXSCREEN:
      begin
        QDesktopWidget_screenGeometry(QApplication_desktop(), @R, QDesktopWidget_primaryScreen(QApplication_desktop()));
        Result := R.Right - R.Left;
      end;
    SM_CYSCREEN:
      begin
        QDesktopWidget_screenGeometry(QApplication_desktop(), @R, QDesktopWidget_primaryScreen(QApplication_desktop()));
        Result := R.Bottom - R.Top;
      end;
    SM_CXSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME,
    SM_CYSIZEFRAME:
      begin
        Result := QStyle_pixelMetric(QApplication_style(), QStylePM_MDIFrameWidth, nil, nil);
      end;
    SM_CXSMICON,
    SM_CYSMICON:
      begin
        Result := 16
      end;
    SM_CXSMSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVIRTUALSCREEN:
      begin
        Result := QWidget_width(QApplication_desktop);
      end;
    SM_CYVIRTUALSCREEN:
      begin
        Result := QWidget_height(QApplication_desktop);
      end;
    SM_CXVSCROLL,
    SM_CYVSCROLL,
    SM_CXHSCROLL,
    SM_CYHSCROLL:
      begin
        Result := QStyle_pixelMetric(QApplication_Style, QStylePM_ScrollBarExtent, nil, nil);
      end;
    SM_CYCAPTION:
      begin
        Result := QStyle_pixelMetric(QApplication_Style, QStylePM_TitleBarHeight, nil, nil);
      end;
    SM_CYKANJIWINDOW:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        //DebugLn('Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
  end;
end;  *)

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC     - A device context
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextColor(DC: HDC) : TColorRef;
var
  lFont: TFPCustomFont;
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.SetTextColor]  DC: %x', [DC]));
  {$endif}

  Result := 0;
  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);

  if LazDC.Font <> nil then
    Result := FPColorToTColor(LazDC.Font.FPColor);
end;

{$ifndef CD_UseNativeText}
{------------------------------------------------------------------------------
  Function: GetTextExtentExPoint
  Params: http://msdn.microsoft.com/en-us/library/dd144935%28VS.85%29.aspx
  Returns: True on success
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentExPoint(DC: HDC; Str: PChar; Count,
  MaxWidth: Integer; MaxCount, PartialWidths: PInteger; var Size: Types.TSize
  ): Boolean;
{var
  i: Integer;
  w: Integer;
  AStr: WideString;
  Accu: Integer;  }
begin
//  Result := False;
  Result := inherited GetTextExtentExPoint(DC, Str, Count, MaxWidth,
    MaxCount, PartialWidths, Size);
  {if not IsValidDC(DC) then Exit;
  with TQtDeviceContext(DC) do
  begin
    AStr := GetUtf8String(Str);
    Size.cx := 0;
    Size.cY := Font.Metrics.Height;
    if PartialWidths = nil then
    begin
      if MaxCount <> nil then
      begin
        Size.cx := Font.Metrics.width(@AStr);
        Accu := 0;
        if MaxWidth <= 0 then
          MaxCount^ := 0
        else
          for i := 0 to Count - 1 do
          begin
            W := QFontMetrics_charWidth(Font.Metrics.FHandle, @AStr, i);
            Accu := Accu + W;
            if Accu <= MaxWidth then
              MaxCount^ := i + 1
            else
              break;
          end;
      end;
    end else
    begin
      if MaxCount <> nil then
        MaxCount^ := 0;
      for i := 0 to Count - 1 do
      begin
        w := QFontMetrics_charWidth(Font.Metrics.FHandle, @AStr, i);
        Inc(Size.cx, w);
        if MaxCount <> nil then
        begin
          if Size.cx <= MaxWidth then
          begin
            inc(MaxCount^);
            PartialWidths[i] := Size.cx;
          end else
          begin
            Dec(Size.cx, w);
            break;
          end;
        end else
          PartialWidths[i] := Size.cx;
      end;
    end;
  end;
  Result := True;}
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: Types.TSize): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  ftFont: TFreeTypeFont;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetTextExtentPoint]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  ftFont := TFreeTypeFont(LazDC.ExtraFontData);
  if ftFont = nil then
  begin
    DebugLn('[TCDWidgetSet.GetTextExtentPoint] Error: ExtraFontData not yet created');
    Exit;
  end;
  Size.cx := Round(ftFont.TextWidth(Str));
  Size.cy := Round(ftFont.TextHeight(Str));
  if Size.cy = 0 then Size.cy := LazDC.AssignedFont.Size; // crude aproximation
  if Size.cy = 0 then Size.cy := DefaultFontSize;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  DC     - A device context with a font selected
           TM     - The structure to receive the font information
  Returns: If successfull
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  LazDC: TLazCanvas absolute DC;
  lTestText: string;
  lTestSize: Types.TSize;
  lFont: TFPCustomFont;
  lFTFont: TFreeTypeFont;
  FreeFTFont: Boolean = False;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetTextMetrics]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  FillChar(TM, SizeOf(TM), 0);

  lFont := LazDC.Font;
  lFTFont := TFreeTypeFont(LazDC.ExtraFontData);
  if lFTFont = nil then
  begin
    DebugLn('[TCDWidgetSet.GetTextMetrics] Error: ExtraFontData not yet created');
    Exit;
  end;

  //QtFontMetrics := QtDC.Metrics;
  TM.tmHeight := Round(lFTFont.TextHeight('ŹÇ'));
  TM.tmAscent := Round(lFTFont.Ascent);
  TM.tmDescent := Round(lFTFont.Descent);
  TM.tmInternalLeading := 0;
  TM.tmExternalLeading := 0;// ToDo
  TM.tmAveCharWidth := Round(lFTFont.TextWidth('x'));
  TM.tmMaxCharWidth := Round(lFTFont.TextWidth('M'));

  if lFont.Bold then TM.tmWeight := FW_BOLD
  else TM.tmWeight := FW_NORMAL;

  TM.tmOverhang := 0;
  TM.tmDigitizedAspectX := 0;
  TM.tmDigitizedAspectY := 0;
  TM.tmFirstChar := 'a';
  TM.tmLastChar := 'z';
  TM.tmDefaultChar := 'x';
  TM.tmBreakChar := '?';
  TM.tmItalic := Ord(lFont.Italic);
  TM.tmUnderlined := Ord(lFont.Underline);
  {$IF (FPC_FULLVERSION<=20600) or (FPC_FULLVERSION=20602)}
  TM.tmStruckOut := Ord(lFont.StrikeTrough); //old version with typo
  {$ELSE}
  TM.tmStruckOut := Ord(lFont.StrikeThrough);
  {$ENDIF}

  { Defaults to a TrueType font.
    Note that the meaning of the FIXED_PITCH constant is the opposite of
    the name implies, according to MSDN docs. Just a small inconsistency
    on Windows API that we have to mimic. }
{  if QtDC.font.fixedPitch then
    TM.tmPitchAndFamily := TRUETYPE_FONTTYPE
  else}
    TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;

  TM.tmCharSet := DEFAULT_CHARSET;

  Result := True;

  if FreeFTFont then lFTFont.Free;
end;
{$endif}

(*function TQtWidgetSet.GetViewPortExtEx(DC: HDC; Size: PSize): Integer;
var
  R: TRect;
begin
  if IsValidDC(DC) and (Size <> nil) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    Size^.cx := R.Right - R.Left;
    Size^.cy := R.Bottom - R.Top;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.GetViewPortOrgEx(DC: HDC; P: PPoint): Integer;
var
  R: TRect;
begin
  if IsValidDC(DC) and (P <> nil) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    P^ := R.TopLeft;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.GetWindowExtEx(DC: HDC; Size: PSize): Integer;
var
  R: TRect;
begin
  if IsValidDC(DC) and (Size <> nil) then
  begin
    QPainter_Window(TQtDeviceContext(DC).Widget, @R);
    Size^.cx := R.Right - R.Left;
    Size^.cy := R.Bottom - R.Top;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.GetWindowLong(Handle : hwnd; int: Integer): PtrInt;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetWindowLong] missing implementation ');
  {$endif}
end;*)

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC    -
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[WinAPI GetWindowOrgEx] DC=%s', [dbghex(DC)]));
  {$endif}
  Result := 0;
  if not IsValidDC(DC) then Exit;
  if P = nil then Exit;

  P^.X := LazDC.WindowOrg.X - LazDC.BaseWindowOrg.X;
  P^.Y := LazDC.WindowOrg.Y - LazDC.BaseWindowOrg.Y;
  Result := 1; // any non-zero will do according to MSDN
  {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI GetWindowOrgEx] Result='+dbgs(p^));
  {$endif}
end;


(*{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetWindowRect(Handle: HWND; var ARect: TRect): Integer;
var
  APos: TQtPoint;
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRect]');
  {$endif}

  Result := 0;
  if not IsValidHandle(Handle) then
    exit;
  APos := QtPoint(0,0);
  QWidget_mapToGlobal(TQtWidget(Handle).Widget, @APos, @APos);

  R := TQtWidget(Handle).getFrameGeometry;
  ARect := Bounds(APos.X,APos.Y,R.Right-R.Left,R.Bottom-R.Top);

  Result := -1;
end;*)

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetWindowRelativePosition(Handle: HWND; var Left, Top: integer): boolean;
var
  lObject: TObject;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI GetWindowRelativePosition]');
  {$endif}
  if Handle = 0 then Exit(False);
  lObject := TObject(Handle);
  if lObject is TCDForm then
  begin
    Result := BackendGetWindowRelativePosition(Handle, Left, Top);
    Exit;
  end
  else
    Result := inherited GetWindowRelativePosition(Handle, Left, Top);
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height
 ------------------------------------------------------------------------------}
function TCDWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: integer): boolean;
var
  lObject: TObject;
  lCDWinControl: TCDWinControl;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn(':>[WinAPI GetWindowSize]');
  {$endif}
  if Handle = 0 then Exit(False);
  lObject := TObject(Handle);
  if lObject is TCDForm then
  begin
    // Initial size guessed
    if TCDForm(lObject).Image <> nil then
    begin
      Width := TCDForm(lObject).Image.Width;
      Height := TCDForm(lObject).Image.Height;
    end
    else
    begin
      Width := 0;
      Height := 0;
    end;

    // Now ask the backend
    Result := BackendGetWindowSize(Handle, Width, Height);
  end
  else if lObject is TCDWinControl then
  begin
    lCDWinControl := lObject as TCDWinControl;
    Width := lCDWinControl.WinControl.Width;
    Height := lCDWinControl.WinControl.Height;
    Result := True;
    {$ifdef VerboseCDWinAPI}
      DebugLn(Format(':[WinAPI GetWindowSize] WinControl %s:%s',
        [lCDWinControl.WinControl.Name, lCDWinControl.WinControl.ClassName]));
    {$endif}
  end
  else
    Result := False;

  {$ifdef VerboseCDWinAPI}
    DebugLn(Format(':<[WinAPI GetWindowSize] Result=%d Width=%d Height=%d',
      [PtrInt(Result), Width, Height]));
  {$endif}
end;

(*{------------------------------------------------------------------------------
  Function: GradientFill
  Params: DC - DeviceContext to perform on
          Vertices - array of Points W/Color & Alpha
          NumVertices - Number of Vertices
          Meshes - array of Triangle or Rectangle Meshes,
                   each mesh representing one Gradient Fill
          NumMeshes - Number of Meshes
          Mode - Gradient Type, either Triangle,
                 Vertical Rect, Horizontal Rect

  Returns: true on success

  Performs multiple Gradient Fills, either a Three way Triangle Gradient,
  or a two way Rectangle Gradient, each Vertex point also supports optional
  Alpha/Transparency for more advanced Gradients.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GradientFill(DC: HDC; Vertices: PTriVertex;
  NumVertices : Longint;
  Meshes: Pointer; NumMeshes : Longint; Mode : Longint): boolean;

  function DoFillTriangle: Boolean; inline;
  begin
    Result := (Mode and GRADIENT_FILL_TRIANGLE) = GRADIENT_FILL_TRIANGLE;
  end;

  function DoFillVRect: Boolean; inline;
  begin
    Result := (Mode and GRADIENT_FILL_RECT_V) = GRADIENT_FILL_RECT_V;
  end;

  function VertexToColor(AVertex: tagTRIVERTEX): TQColor;
  var
    TheAlpha: Byte;
  begin
    TheAlpha := AVertex.Alpha shr 8;
    if TheAlpha = 0 then
      TheAlpha := 255;
    with AVertex do
      QColor_fromRgb(@Result, Red shr 8, Green shr 8, Blue shr 8, TheAlpha);
  end;

  function FillTriMesh(Mesh: tagGradientTriangle) : Boolean;
  var
    V1, V2, V3: tagTRIVERTEX;
    C1, C2, C3: TQColor;
    Grad: QConicalGradientH;
    Brush: QBrushH;
    Triangle: QPolygonH;
    R: TRect;
    Painter: QPainterH;
    Rgn: QRegionH;
  begin
    with Mesh do
    begin
      Result :=
        (Vertex1 < Cardinal(NumVertices)) and (Vertex2 >= 0) and
        (Vertex2 < Cardinal(NumVertices)) and (Vertex2 >= 0) and
        (Vertex3 < Cardinal(NumVertices)) and (Vertex3 >= 0);

      if (Vertex1 = Vertex2) or
        (Vertex1 = Vertex3) or
        (Vertex2 = Vertex3) or not Result then
        Exit;

      V1 := Vertices[Vertex1];
      V2 := Vertices[Vertex2];
      V3 := Vertices[Vertex3];

      Painter := TQtDeviceContext(DC).Widget;
      QPainter_save(Painter);
      Triangle := QPolygon_create(3);
      QPolygon_setPoint(Triangle, 0, V1.X, V1.Y);
      QPolygon_setPoint(Triangle, 1, V2.X, V2.Y);
      QPolygon_setPoint(Triangle, 2, V3.X, V3.Y);
      QPolygon_boundingRect(Triangle, @R);

      Dec(R.Bottom);
      Dec(R.Right);

      Rgn := QRegion_create(@R);

      // make our poly clip region , so gradient center is at real center
      QPainter_setClipRegion(Painter, Rgn, QtIntersectClip);

      Grad := QConicalGradient_create(R.Right div 2, R.Bottom div 2, 90);
      C1 := VertexToColor(V1);
      C2 := VertexToColor(V2);
      C3 := VertexToColor(V3);

      QGradient_setColorAt(Grad, 0.0, @C1); // open
      QGradient_setColorAt(Grad, 0.33, @C2); // left corner
      QGradient_setColorAt(Grad, 0.66, @C3); // right corner
      QGradient_setColorAt(Grad, 1.0, @C1); // close


      Brush := QBrush_create(Grad);
      QPainter_setPen(Painter, QtNoPen);
      QPainter_setBrush(Painter, Brush);

      // move center point down, so we remove reflections of C2 and C3
      // TODO: C1 reflection is still visible
      QPainter_setBrushOrigin(Painter, 0, R.Bottom div 5);
      QPainter_drawPolygon(Painter, Triangle);

      //TODO: now me must make it look "softer" because reflection look of
      // first color is ugly.

      QBrush_destroy(Brush);
      QPolygon_destroy(Triangle);
      QGradient_destroy(Grad);
      QRegion_destroy(Rgn);
      QPainter_restore(Painter);

    end;
  end;

  function FillRectMesh(Mesh: tagGradientRect) : boolean;
  var
    TL,BR: tagTRIVERTEX;
    StartColor, EndColor, SwapColor: TQColor;
    Swap: Longint;
    SwapColors: Boolean;
    Grad: QGradientH;
    Brush: QBrushH;
  begin
    with Mesh do
    begin
      Result :=
        (UpperLeft < Cardinal(NumVertices)) and (UpperLeft >= 0) and
        (LowerRight < Cardinal(NumVertices)) and (LowerRight >= 0);
      if (LowerRight = UpperLeft) or not Result then
        Exit;

      TL := Vertices[UpperLeft];
      BR := Vertices[LowerRight];
      SwapColors := (BR.Y < TL.Y) and (BR.X < TL.X);
      if BR.X < TL.X then
      begin
        Swap := BR.X;
        BR.X := TL.X;
        TL.X := Swap;
      end;
      if BR.Y < TL.Y then
      begin
        Swap := BR.Y;
        BR.Y := TL.Y;
        TL.Y := Swap;
      end;
      StartColor := VertexToColor(TL);
      EndColor := VertexToColor(BR);
      if SwapColors then
      begin
        SwapColor := StartColor;
        StartColor := EndColor;
        EndColor := SwapColor;
      end;
      if DoFillVRect then
        Grad := QLinearGradient_create(TL.X, TL.Y, TL.X, BR.Y)
      else
        Grad := QLinearGradient_create(TL.X, TL.Y, BR.X, TL.Y);
      QGradient_setColorAt(Grad, 0, @StartColor);
      QGradient_setColorAt(Grad, 1, @EndColor);
      Brush := QBrush_create(Grad);
      TQtDeviceContext(DC).fillRect(TL.X, TL.Y, BR.X - TL.X, BR.Y - TL.Y, Brush);
      QGradient_destroy(Grad);
      QBrush_destroy(Brush);
    end;
  end;

const
  MeshSize: Array[Boolean] of Integer = (
    SizeOf(tagGradientRect), SizeOf(tagGradientTriangle));
var
  i : Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('***** [WinAPI TQtWidgetSet.GradientFill] ');
  {$endif}

  //Currently Alpha blending is ignored... Ideas anyone?
  Result := (Meshes <> nil) and (NumMeshes >= 1) and (NumVertices >= 2)
            and (Vertices <> nil);
  if Result and DoFillTriangle then
    Result := NumVertices >= 3;
  if Result then
  begin
    Result := False;

    //Sanity Checks For Vertices Size vs. Count
    if MemSize(Vertices) < PtrUInt(SizeOf(tagTRIVERTEX)*NumVertices) then
      exit;

    //Sanity Checks For Meshes Size vs. Count
    if MemSize(Meshes) < PtrUInt(MeshSize[DoFillTriangle]*NumMeshes) then
      exit;

    for I := 0 to NumMeshes - 1 do
    begin
      if DoFillTriangle then
      begin
        if not FillTriMesh(PGradientTriangle(Meshes)[I]) then
          exit;
      end
      else
      begin
        if not FillRectMesh(PGradientRect(Meshes)[I]) then
          exit;
      end;
    end;
    Result := True;
  end;
end;

function TQtWidgetSet.HideCaret(hWnd: HWND): Boolean;
begin
  Result := (hWnd <> 0) and QtCaret.HideCaret(TQtWidget(hWnd));
end;*)

{------------------------------------------------------------------------------
  Procedure: InitializeCriticalSection
  Params: var CritSection: TCriticalSection
  Returns:
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

(*function TQtWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer;
var
  QtDC: TQtDeviceContext absolute dc;
  IntersectRgn, Rgn: QRegionH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI TQtWidgetSet.IntersectClipRect] L ',Left,' T ',Top,' R ',Right,' B ',Bottom);
  {$endif}
  Result := ERROR;
  if not IsValidDC(DC) then exit;

  IntersectRgn := QRegion_create(Left, Top, Right - Left, Bottom - Top);
  try
    if QtDC.getClipping then
    begin
      Rgn := QRegion_create;
      try
        QPainter_clipRegion(QtDC.Widget, Rgn);
        if QRegion_isEmpty(Rgn) then
          QtDC.setClipRegion(IntersectRgn)
        else
          QtDC.setClipRegion(IntersectRgn, QtIntersectClip);
        QtDC.setClipping(True);
        // recreate Rgn
        QRegion_destroy(Rgn);
        Rgn := QRegion_create;
        QPainter_clipRegion(QtDC.Widget, Rgn);
        Result := QtDC.GetRegionType(Rgn);
      finally
        QRegion_destroy(Rgn);
      end;
    end else
    begin
      QtDC.setClipRegion(InterSectRgn);
      QtDC.setClipping(True);
      Result := QtDC.GetRegionType(InterSectRgn);
    end;
  finally
    QRegion_destroy(IntersectRgn);
  end;
end;*)

(*function TCDWidgetSet.IsIconic(Handle: HWND): boolean;
begin
  Result := TCDForm(Handle).LCLForm.FormState = fsMinimized;
end;*)

function TCDWidgetSet.IsWindow(handle: HWND): boolean;
begin
  Result := TObject(Handle) is TCDForm;
end;

function TCDWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  Result := TCDForm(Handle).LCLForm.Enabled;
end;

function TCDWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  Result := TCDForm(Handle).LCLForm.Visible;
end;

(*function TQtWidgetSet.IsZoomed(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).isMaximized;
end;*)

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}
function TCDWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect; bErase: Boolean): Boolean;
var
  lHandle: TObject;
  lControlHandle: TCDWinControl;
  lControl: TWinControl;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI InvalidateRect]');
  {$endif}
  if AHandle = 0 then exit(False);

  lHandle := TObject(AHandle);

  // Invalidate on a child control
  if lHandle is TCDWinControl then
  begin
    lControlHandle := TCDWinControl(lHandle);
    lControlHandle.IncInvalidateCount();
    if lControlHandle.CDControlInjected and (lControlHandle.CDControl <> nil) then
      TCDWinControl(lControlHandle.CDControl.Handle).IncInvalidateCount();
    lControl := lControlHandle.WinControl;
    lControl := Forms.GetParentForm(lControl);
    // Don't use Rect in BackendInvalidateRect unless we really make the full
    // conversion of coordinates to window coordinates. Better invalidate everything
    // then too few. And anyway on each draw we send everything.
    // This fixes changing the selection in TCustomGrid
    Result := BackendInvalidateRect(lControl.Handle, nil, BErase);
  end
  // Invalidate on a form
  else
  begin
    Result := BackendInvalidateRect(AHandle, Rect, BErase);
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: InvalidateRgn
  Params: aHandle:
          Rect:
          bErase:
  Returns: True if invalidate is successfull.
  Invalidates region of widget.

  Felipe: Invalidating a non-rectangular region is unusual and complicated,
  so for now lets just get the bounding rect and invalidate that instead.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.InvalidateRgn(aHandle: HWND; Rgn: HRGN; Erase: Boolean): Boolean;
var
  lLazRegion: TLazRegion absolute Rgn;
  localRect: TRect;
begin
  {$ifdef VerboseCDWinAPI}
    DebugLn('[WinAPI InvalidateRgn]');
  {$endif}
  if aHandle = 0 then Exit(False);
  if Rgn <> 0 then
  begin
    localRect := lLazRegion.GetBoundingRect();
    Result := InvalidateRect(aHandle, @localRect, Erase);
  end
  else
    Result := InvalidateRect(aHandle, nil, Erase);
end;

{------------------------------------------------------------------------------
  Procedure: LeaveCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCDWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.LeaveCriticalsection(ACritSec^);
end;

{------------------------------------------------------------------------------
  Function: LineTo
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TCDWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  PenPos, LastPos: TPoint;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.LineTo] DC=%x X=%d Y=%d', [DC, X, Y]));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then
  begin
    DebugLn('[TCDWidgetSet.LineTo] Invalid DC');
    Exit;
  end;

(*  TQtDeviceContext(DC).getPenPos(@PenPos);
  LastPos := Point(X, Y);
  if TQtDeviceContext(DC).pen.getCosmetic then
    LastPos := TQtDeviceContext(DC).GetLineLastPixelPos(PenPos, LastPos);
  TQtDeviceContext(DC).drawLine(PenPos.X, PenPos.Y, LastPos.X, LastPos.Y);
  MoveToEx(DC, X, Y, nil);*)

  LazDC.LineTo(X, Y);

  Result := True;
end;
(*
function TQtWidgetSet.LPtoDP(DC: HDC; var Points; Count: Integer): BOOL;
var
  P: PPoint;
  QtPoint: TQtPoint;
  Matrix: QTransformH;
  QtDC: TQtDeviceContext;
begin
  Result := False;

  if not IsValidDC(DC) then
    Exit;

  QtDC := TQtDeviceContext(DC);

  Matrix := QPainter_transform(QtDC.Widget);
  P := @Points;
  while Count > 0 do
  begin
    Dec(Count);
    QtPoint.X := P^.X;
    QtPoint.Y := P^.Y;
    QTransform_map(Matrix, PQtPoint(@QtPoint), PQtPoint(@QtPoint));
    P^.X := QtPoint.X;
    P^.Y := QtPoint.Y;
    Inc(P);
  end;

  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI MoveToEx]',
     ' DC:', dbghex(DC),
     ' X:', dbgs(X),
     ' Y:', dbgs(Y));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if (OldPoint <> nil) then OldPoint^ := LazDC.PenPos;

  LazDC.PenPos := Types.Point(X, Y);

  Result := True;
end;

(*function TQtWidgetSet.OffsetRgn(RGN: HRGN; nXOffset, nYOffset: Integer): Integer;
var
  QtRgn: QRegionH;
begin
  Result := ERROR;

  if not IsValidGDIObject(RGN) then
    Exit
  else
    QtRgn := TQtRegion(RGN).FHandle;

  QRegion_translate(QtRgn, nXOffset, nYOffset);

  if QRegion_isEmpty(QtRgn) then
    Result := NULLREGION
  else
  begin
    if TQtRegion(RGN).IsPolyRegion or (TQtRegion(RGN).numRects > 0) then
      Result := COMPLEXREGION
    else
      Result := SIMPLEREGION;
  end;
end;

function TQtWidgetSet.PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.PeekMessage] missing implementation ');
  {$endif}
end;*)

{------------------------------------------------------------------------------
  Function: PolyBezier
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Filled: Boolean;
           Continuous: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
begin
  {$ifdef VerboseCDDrawing}
    WriteLn('[WinAPI PolyBezier] DC: ', dbghex(DC));
  {$endif}
  Result := inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;

{------------------------------------------------------------------------------
  Function: Polygon
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: Boolean): boolean;
var
  LazDC: TLazCanvas absolute DC;
  lPoints: array of TPoint;
  i: Integer;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Polygon] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then Exit(False);

  SetLength(lPoints, NumPts);
  for i := 0 to NumPts-1 do
  begin
    {$ifdef VerboseCDDrawing}
    LCLProc.DbgOut(Format(' P=%d,%d', [Points[i].X, Points[i].Y]));
    {$endif}
    lPoints[i] := Points[i];
  end;

  LazDC.Polygon(lPoints);
  Result := True;

  {$ifdef VerboseCDDrawing}
  DebugLn('');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: Polyline
  Params:  DC: HDC; Points: PPoint; NumPts: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
var
  LazDC: TLazCanvas absolute DC;
  lPoints: array of TPoint;
  i: Integer;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Polygon] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then Exit(False);

  SetLength(lPoints, NumPts);
  for i := 0 to NumPts-1 do
    lPoints[i] := Points[i];

  LazDC.Polyline(lPoints);
  Result := True;
end;

(*function TQtWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): Boolean;
var
  Widget: TQtWidget absolute Handle;
  Event: QLCLMessageEventH;
begin
  Result := False;
  if Handle <> 0 then
  begin
    Event := QLCLMessageEvent_create(QEventLCLMessage, Msg, wParam, lParam, 0);
    QCoreApplication_postEvent(Widget.Widget, Event, 1 {high priority});
    Result := True;
  end;
end;*)

function TCDWidgetSet.PtInRegion(RGN: HRGN; X, Y: Integer): Boolean;
var
  lRegion: TLazRegion absolute RGN;
begin
  Result := False;

  if not IsValidGDIObject(RGN) then Exit;

  Result := lRegion.IsPointInRegion(X, Y);
end;

{------------------------------------------------------------------------------
  Function: Rectangle
  Params:  DC: HDC; X1, Y1, X2, Y2: Integer
  Returns: Nothing

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
  DebugLn(Format(':>[WinAPI Rectangle] DC=%s', [dbghex(DC)]));
  {$endif}

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
    DebugLn(':<[WinAPI Rectangle] Invalid DC!');
    {$endif}
    Exit(False);
  end;

  // ToDo: We can normalize the rectangle, but this is not necessary as
  // TLazCanvas ignores invalid coordinates
{  R := NormalizeRect(Rect(X1, Y1, X2, Y2));
  if IsRectEmpty(R) then Exit(True);}

  LazDC.Rectangle(X1, Y1, X2, Y2);

  Result := True;
end;

function TCDWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
var
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDDrawing}
  Debugln('[WinAPI RectVisible]');
  {$endif}
  Result := True;
  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);
  // as MSDN says only clipping region can play here
{  if QtDC.getClipping then
    Result := QtDC.getClipRegion.containsRect(ARect);}
end;

(*{------------------------------------------------------------------------------
  Function: RedrawWindow
  Params: Wnd:
          lprcUpdate:
          hrgnUpdate:
          flags:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.RedrawWindow(Wnd: HWND; lprcUpdate: PRECT; hrgnUpdate: HRGN; flags: UINT): Boolean;
var
  QtWidget: TQtWidget;
  Region: TQtRegion;
begin
  if not IsValidHandle(Wnd) then
    Exit(False);

  QtWidget := TQtWidget(Wnd);
  if IsValidGDIObject(hrgnUpdate) then
    Region := TQtRegion(hrgnUpdate)
  else
    Region := nil;
  if (lprcUpdate = nil) and (hrgnUpdate = 0) then
  begin
    QtWidget.Update(nil);
    Exit(True);
  end;

  if Region = nil then
    Result := InvalidateRect(Wnd, lprcUpdate, False)
  else
    QtWidget.UpdateRegion(Region.FHandle);

  Result := True;
end;

function TQtWidgetSet.ReleaseCapture: Boolean;
var
  w: TQtWidget;
begin
  w := TQtWidget(GetCapture);
  Result := w <> nil;
  if Result then
  begin
    {$IFDEF MSWINDOWS}
    if w is TQtMainWindow then
      w.releaseMouse()
    else
      windows.ReleaseCapture;
    {$ELSE}
    w.releaseMouse();
    {$ENDIF}
  end;
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI ReleaseCapture] Capture = ', THandle(w));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: ReleaseDC
  Params:     hWnd:       Handle to the window whose DC is to be released.
              hDC:        Handle to the DC to be released.
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ReleaseDC(hWnd: HWND; DC: HDC): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ReleaseDC]',
     ' hWnd: ', dbghex(hWnd),
     ' DC: ', dbghex(DC));
  {$endif}

  Result := 0;

  if IsValidDC(DC) then Exit;

  Result := 1;
end;*)

{------------------------------------------------------------------------------
  Function: RestoreDC: Restore a previously saved DC state
  Params:
    DC: Handle to a DeviceContext
    SavedDC: Index of saved state that needs to be restored
  Returns: True if state was successfuly restored.
-------------------------------------------------------------------------------}
function TCDWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:> [WinAPI RestoreDC] DC=', dbghex(DC),' SavedDC=',SavedDC);
  {$Endif}
  Result := False;
  if not IsValidDC(DC) then Exit;

  LazDC.RestoreState(SavedDC);
  Result := True;
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI RestoreDC]');
  {$Endif}
end;

(*function TQtWidgetSet.RoundRect(DC: hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Result := False;
  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI RoundRect] DC Invalid, result=', result);
    {$Endif}
    Exit;
  end;
  Result := inherited RoundRect(DC, X1, Y1, X2, Y2, RX, RY);
end;*)

{------------------------------------------------------------------------------
  Function: SaveDC: save DC state information to a stack
  Params:  DC
  Returns: The index assigned to the or 0 if DC is not valid
-------------------------------------------------------------------------------}
function TCDWidgetSet.SaveDC(DC: HDC): Integer;
var
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:> [WinAPI SaveDC] DC=', dbghex(DC));
  {$Endif}

  result:=0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI SaveDC] DC Invalid, result=', result);
    {$Endif}
    exit;
  end;

  Result := LazDC.SaveState();

  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI SaveDC] result=', Result);
  {$Endif}
end;

(*{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params:  Handle: HWND; var P: TPoint
  Returns:
-------------------------------------------------------------------------------}
function TQtWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
var
  APoint: TQtPoint;
begin
  Result := 0;
  if IsValidHandle(Handle) then
  begin
    APoint := QtPoint(P.X, P.Y);
    QWidget_mapFromGlobal(TQtWidget(Handle).GetContainerWidget, @APoint, @APoint);
    P := Point(APoint.x, APoint.y);
    Result := 1;
  end;
end;

{------------------------------------------------------------------------------
  Method:  ScrollWindowEx
  Params:  HWnd       - handle of window to scroll
           DX         - horizontal amount to scroll
           DY         - vertical amount to scroll
           PRcScroll  - pointer to scroll rectangle
           PRcClip    - pointer to clip rectangle
           HRgnUpdate - handle of update region
           PRcUpdate  - pointer to update rectangle
           Flags      - scrolling flags

  Returns: True if succesfull

  The ScrollWindowEx function scrolls the content of the specified window's
  client area
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ScrollWindowEx(HWnd: HWND; DX, DY: Integer; PRcScroll,
  PRcClip: PRect; HRgnUpdate: HRGN; PRcUpdate: PRect; Flags: UINT): Boolean;
var
  R: TRect;
  W: TQtWidget;
begin
  Result := False;
  if (HWND = 0) then exit;

  W := TQtWidget(HWND);
  if ((Flags and SW_SCROLLCHILDREN) <> 0) then
    W.scroll(dx, dy, nil)
  else
  if (PrcScroll = nil) then
  begin
    R := W.getClientBounds;
    W.scroll(dx, dy, @R);
  end
  else
    W.scroll(dx, dy, PRcScroll);

  if ((Flags and SW_INVALIDATE) <> 0) then
  begin
    if IsValidGDIObject(HRgnUpdate) then
    begin
      R := TQtRegion(HRgnUpdate).getBoundingRect;
      PRcUpdate := @R;
      W.Update(@R);
    end else
    if PRcClip <> nil then
    begin
      PRcUpdate := PRcClip;
      W.Update(PrcClip);
    end;
  end;

  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: SelectClipRGN
  Params:  DC, RGN
  Returns: longint

  Sets the DeviceContext's ClipRegion. The Return value
  is the new clip regions type, or ERROR.

  The result can be one of the following constants
      Error
      NullRegion
      SimpleRegion
      ComplexRegion
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
begin
  Result := ExtSelectClipRgn(DC, RGN, RGN_COPY);
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: The GDI object of the same type previously associated with the DC

  Changes one of the GDI objects (Font, Brush, etc) of a Device Context;
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  aObject: TObject;
  lFont: TFPCustomFont absolute AObject;
  lPen: TFPCustomPen absolute AObject;
  lBrush: TFPCustomBrush absolute AObject;
  lOrigBrush: TFPCustomBrush;
  {$ifdef VerboseCDDrawing}
  ObjType: string;
  {$endif}
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format(':>[TCDWidgetSet.SelectObject] DC=%s GDIObj=%s',
      [dbghex(DC), dbghex(GDIObj)]));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseCDDrawing}
      DebugLn(':<[TCDWidgetSet.SelectObject] Invalid DC');
    {$endif}

    Exit;
  end;

  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseCDDrawing}
      DebugLn(':<[TCDWidgetSet.SelectObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);

  if aObject is TFPCustomFont then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Font';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).AssignedFont);
    TLazCanvas(DC).AssignFontData(lFont); // := doesn't work and Assign() raises exceptions
    TLazCanvas(DC).AssignedFont := lFont;
    {$ifndef CD_UseNativeText}
    TLazCanvas(DC).ExtraFontData := TLazCDCustomFont(lFont).FTFont;
    {$endif}
  end
  else if aObject is TFPCustomPen then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Pen';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).AssignedPen);
    TLazCanvas(DC).AssignPenData(lPen); // := doesn't work and Assign() raises exceptions
    TLazCanvas(DC).AssignedPen := lPen;
  end
  else if aObject is TFPCustomBrush then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Brush';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).AssignedBrush);
    TLazCanvas(DC).AssignBrushData(lBrush); // := doesn't work and Assign() raises exceptions
    TLazCanvas(DC).AssignedBrush := lBrush;
  end
  else if aObject is TCDBitmap then
  begin
    {$ifdef VerboseCDDrawing}ObjType := 'Bitmap';{$endif}

    Result := HGDIOBJ(TLazCanvas(DC).Image);

    TLazCanvas(DC).Image := TCDBitmap(aObject).Image;
    TLazCanvas(DC).SelectedBitmap := aObject;
  end; (*else
  if AObject is TQtRegion then
  begin
    Result := HGDIOBJ(TQtDeviceContext(DC).getClipRegion);
    SelectClipRGN(DC, HRGN(GDIObj));
  end*);

  {$ifdef VerboseCDDrawing}
    DebugLn(':<[TCDWidgetSet.SelectObject] Result=', dbghex(Result), ' ObjectType=', ObjType);
  {$endif}
end;

(*function TQtWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): LResult;
var
  Widget: TQtWidget absolute HandleWnd;
  Event: QLCLMessageEventH;
begin
  Result := 0;
  if (HandleWnd <> 0) and (Widget.Widget <> nil) then
  begin
    Event := QLCLMessageEvent_create(QEventLCLMessage, Msg, wParam, lParam, 0);
    try
      QCoreApplication_sendEvent(Widget.Widget, Event);
      Result := QLCLMessageEvent_getMsgResult(Event);
    finally
      QLCLMessageEvent_destroy(Event);
    end;
  end;
end;

function TQtWidgetSet.SetActiveWindow(Handle: HWND): HWND;
begin
  Result := GetActiveWindow;

  if Handle <> 0 then
    TQtWidget(Handle).Activate
  else
    Result := 0; // error
end;

{------------------------------------------------------------------------------
  Function: SetBKColor
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetBKColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SetBkColor]',
     ' DC: ', dbghex(DC),
     ' Color: ', dbgs(Color));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SetBkColor] Invalid DC');
    {$endif}

    Exit;
  end;

  Result := TQtDeviceContext(DC).SetBkColor(TColorRef(Color));
end;

{------------------------------------------------------------------------------
  Method:  SetBkMode
  Params:  DC    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetBkMode(DC: HDC; bkMode: Integer): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SetBkMode] DC=', dbghex(DC), ' BkMode=', dbgs(bkMode));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SetBkMode] Invalid DC');
    {$endif}

    Exit;
  end;

  Result := TQtDeviceContext(DC).SetBkMode(bkMode);
end;

function TQtWidgetSet.SetCapture(AHandle: HWND): HWND;
var
  Message: TLMessage;
begin
  Result := GetCapture;
  if Result <> AHandle then
  begin
    if Result <> 0 then
      ReleaseCapture;
    if AHandle <> 0 then
     {$IFDEF MSWINDOWS}
      Windows.SetCapture(AHandle);
     {$ELSE}
      TQtWidget(AHandle).grabMouse();
     {$ENDIF}
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI SetCapture] Capture = ', Result, ' New capture = ', AHandle);
    {$endif}
    if Result <> 0 then
    begin
      Message.Msg := 0;
      FillChar(Message, SizeOf(Message), 0);
      Message.msg := LM_CAPTURECHANGED;
      Message.wParam := 0;
      Message.lParam := Result;
      LCLMessageGlue.DeliverMessage(TQtWidget(AHandle).LCLObject, Message);
    end;
  end;
end;

function TQtWidgetSet.SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := QtCaret.SetCaretPos(X, Y);
end;

function TQtWidgetSet.SetCaretPosEx(Handle: HWnd; X, Y: Integer): Boolean;
begin
  Result := QtCaret.SetCaretPos(X, Y);
end;

function TQtWidgetSet.SetCaretRespondToFocus(handle: HWND;
  ShowHideOnFocus: boolean): Boolean;
begin
  Result := True;
  QtCaret.SetQtCaretRespondToFocus(ShowHideOnFocus);
end;

{------------------------------------------------------------------------------
  Function: SetCursor
  Params: ACursor - HCursor (TQtCursor)
  Returns:
       previous global cursor
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursor(ACursor: HCURSOR): HCURSOR;
begin
  Result := HCURSOR(OverrideCursor);

  if Result = ACursor then
    Exit;

  if Screen.Cursors[crDefault] = ACursor then
    OverrideCursor := nil
  else
    OverrideCursor := TQtCursor(ACursor);
end;

{------------------------------------------------------------------------------
  Function: SetCursorPos
  Params: X:
          Y:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursorPos(X, Y: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetCursorPos]');
  {$endif}

  QCursor_setPos(X, Y);

  Result := True;
end;*)

{------------------------------------------------------------------------------
  Function: SetFocus
  Params: hWnd   - Window handle to be focused
  Returns:

 ------------------------------------------------------------------------------}
function TCDWidgetSet.SetFocus(hWnd: HWND): HWND;
var
  lObject, lOldObject: TCDBaseControl;
  lOldControl: TWinControl;
  lHandle: TCDWinControl;
begin
  {$ifdef VerboseCDFocus}
  DebugLn(Format('[TCDWidgetSet.SetFocus] Handle=%x', [hWnd]));
  {$endif}
  Result := 0;
  // Strangly this breaks the Android Virtual Keyboard =(
  // Remove the ifdef only when we can guarantee that this doesn't break Android Virtual Keyboard
  {$ifndef CD_Android}
  if hwnd = 0 then
  begin
    Result := GetFocus();
    Exit;
  end;
  lObject := TCDBaseControl(hWnd);

  // SetFocus on a child control
  if lObject is TCDWinControl then
  begin
    lHandle := TCDWinControl(lObject);

    // Set focus in the parent window
    //Result := BackendSetFocus(hWnd);

    if lHandle.WinControl = nil then Exit;
    CDSetFocusToControl(lHandle.WinControl, lHandle.CDControl);

    {$ifdef VerboseCDFocus}
    DebugLn(Format(':[TCDWidgetSet.SetFocus] NewFocusedControl=%s NewFocusedIntfControl=%x', [FocusedControl.Name, PtrUInt(FocusedIntfControl)]));
    {$endif}
  end
  // SetFocus on a form
  else
  begin
    Result := BackendSetFocus(hWnd);
  end;
  {$endif}
end;

(*function TQtWidgetSet.GetForegroundWindow: HWND;
var
  W: QWidgetH;
begin
  {$IFDEF HASX11}
  if WindowManagerName = 'metacity' then
    W := X11GetActivewindow
  else
    W := QApplication_activeWindow();
  {$ELSE}
  W := QApplication_activeWindow();
  {$ENDIF}
  Result := HwndFromWidgetH(W);
end;

function TQtWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  Result := False;
  if HWND <> 0 then
  begin
    Result := TQtWidget(HWND).IsActiveWindow;
    TQtWidget(HWnd).Activate;
  end;
end;

function TQtWidgetSet.SetMenu(AWindowHandle: HWND; AMenuHandle: HMENU): Boolean;
var
  AWidget, AMenuWidget: TQtWidget;
  QtMainWindow: TQtMainWindow absolute AWidget;
  QtMenuBar: TQtMenuBar absolute AMenuWidget;
  R, R1: TRect;
begin
  AWidget := TQtWidget(AWindowHandle);
  Result := AWidget is TQtMainWindow;
  if Result then
  begin
    AMenuWidget := TQtWidget(AMenuHandle);
    if AMenuWidget is TQtMenuBar then
    begin
      R := AWidget.LCLObject.ClientRect;
      R1 := QtMainWindow.MenuBar.getGeometry;
      R1.Right := R.Right;
      QtMenuBar.setGeometry(R1);
      QtMainWindow.setMenuBar(QMenuBarH(QtMenuBar.Widget));
    end
    else
      QtMainWindow.setMenuBar(QMenuBarH(QtMainWindow.MenuBar.Widget));
  end;
end;

function TQtWidgetSet.SetParent(hWndChild: HWND; hWndParent: HWND): HWND;
var
  OldVisible: Boolean;
  Flags: QtWindowFlags;
  W: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('[WinApi SetParent] child: ',dbgHex(PtrUInt(hwndChild)),
    ' parent: ',dbgHex(PtrUInt(hWndParent)));
  {$endif}
  Result := 0;
  if not IsValidHandle(hwndChild) then
    exit;
  Result := GetParent(hWndChild);
  if (Result = hwndParent) then
    exit;
  W := TQtWidget(hWndChild);
  OldVisible := W.getVisible;
  Flags := W.windowFlags;
  if IsValidHandle(hWndParent) then
    W.setParent(TQtWidget(hWndParent).GetContainerWidget)
  else
  begin
    W.setParent(nil);
    W.setWindowFlags(Flags);
  end;
  W.setVisible(OldVisible);
end;

function TQtWidgetSet.SetMapMode(DC: HDC; fnMapMode : Integer): Integer;
var
  AWindowExt: TPoint;
  R: TRect;
begin
  if IsValidDC(DC) then
  begin
    if fnMapMode <> TQtDeviceContext(DC).vMapMode then
    begin
      case fnMapMode of
        MM_ANISOTROPIC:; // user's choice
        MM_ISOTROPIC:; // adjusted after each SetViewPortExtEx call (see MSDN for details)
        MM_HIENGLISH: AWindowExt := Point(1000, -1000);
        MM_HIMETRIC: AWindowExt := Point(2540, -2540);
        MM_LOENGLISH: AWindowExt := Point(100, -100);
        MM_LOMETRIC: AWindowExt := Point(254, -254);
        MM_TWIPS: AWindowExt := Point(1440, -1440);
      else
        fnMapMode := MM_TEXT;
      end;
      TQtDeviceContext(DC).vMapMode := fnMapMode;
      QPainter_setViewTransformEnabled(TQtDeviceContext(DC).Widget, fnMapMode <> MM_TEXT);
      if not (fnMapMode in [MM_TEXT, MM_ANISOTROPIC, MM_ISOTROPIC]) then
      begin
        QPainter_Window(TQtDeviceContext(DC).Widget, @R);
        R.BottomRight := AWindowExt;
        QPainter_setWindow(TQtDeviceContext(DC).Widget, @R);
        QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
        R.Right := QtWidgetSet.GetDeviceCaps(DC, LOGPIXELSX);
        R.Bottom := QtWidgetSet.GetDeviceCaps(DC, LOGPIXELSX);
        QPainter_setViewPort(TQtDeviceContext(DC).Widget, @R);
      end;
    end;
    Result := Integer(True);
  end else
    Result := Integer(False);
end;

function TQtWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := (hWnd <> 0) and (QtCaret.ShowCaret(TQtWidget(hWnd)));
end;*)

{------------------------------------------------------------------------------
  Method:  SetProp
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean;
begin
  if Handle<>0 then
  begin
    TCDBaseControl(Handle).Props[str] := Data;
    Result := (TCDBaseControl(Handle).Props[str]=Data);
    {$ifdef VerboseCDWinApi}
    DebugLn('[WinAPI SetProp win=%s str=%s data=%x',[dbgsname(TCDWinControl(Handle)), str, ptrint(data)]);
    {$endif}
  end else
    Result := False;
end;

(*{------------------------------------------------------------------------------
  Function: SetROP2
  Params:  HDC, Raster OP mode
  Returns: Old Raster OP mode

  Please note that the bitwise raster operation modes, denoted with a
  RasterOp prefix, are only natively supported in the X11 and
  raster paint engines.
  This means that the only way to utilize these modes on the Mac is
  via a QImage.
  The RasterOp denoted blend modes are not supported for pens and brushes
  with alpha components. Also, turning on the QPainter::Antialiasing render
  hint will effectively disable the RasterOp modes.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetROP2(DC: HDC; Mode: Integer): Integer;
var
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('TQtWidgetSet.SetROP2() DC ',dbghex(DC),' Mode ',Mode);
  {$endif}
  Result := R2_COPYPEN;
  if not IsValidDC(DC) then
    exit;
  Result := QtDC.Rop2;
  QtDC.Rop2 := Mode;
end;

{------------------------------------------------------------------------------
  Function: SetScrollInfo
  Params:  none
  Returns: The new position value

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
var
  Control: TWinControl;
  ScrollBar: TQtScrollBar;

  function UpdateScrollInfo: Integer;
  var
    iReCountMax: Integer;
    SBUpdatesCount: Integer;
    i: Integer;
    WheelLines: Integer;
  begin
    Result := 0;
    SBUpdatesCount := 0;

    if (ScrollInfo.FMask and SIF_RANGE) <> 0 then
    begin
      inc(SBUpdatesCount);
      ScrollBar.setMinimum(ScrollInfo.nMin);

      // we must recount ScrollBar.Max since invalid value raises AV
      iRecountMax := ScrollInfo.nMax - ScrollInfo.nPage;
      if iRecountMax < ScrollInfo.nMin then
        iRecountMax := ScrollInfo.nMin;

      ScrollBar.setMaximum(iRecountMax);
    end;

    if (ScrollInfo.FMask and SIF_PAGE) <> 0 then
    begin
      // segfaults if we don't check Enabled property
      if ScrollBar.getEnabled then
      begin
        inc(SBUpdatesCount);
        ScrollBar.setPageStep(ScrollInfo.nPage);
        WheelLines := QApplication_wheelScrollLines();
        with Scrollbar do
        begin
          i := Max(1, floor((GetPageStep / WheelLines) / 6));
          setSingleStep(i);
        end;
      end;
    end;

    if (ScrollInfo.FMask and SIF_UPDATEPOLICY) <> 0 then
      ScrollBar.setTracking(ScrollInfo.nTrackPos <> SB_POLICY_DISCONTINUOUS);

    if (ScrollInfo.FMask and SIF_POS) <> 0 then
    begin
      inc(SBUpdatesCount);

      if SBUpdatesCount = 1 then
        ScrollBar.BeginUpdate;
      try
        if not (ScrollBar.getTracking and ScrollBar.getSliderDown) then
        begin
          {do not setValue() if values are equal, since it calls
           signalValueChanged() which sends unneeded LM_SCROLL msgs }
          if (ScrollBar.getValue = ScrollInfo.nPos) then
            SBUpdatesCount := 0;

          if (ScrollInfo.nPos < ScrollBar.getMin) then
            ScrollInfo.nPos := ScrollBar.getMin
          else
          if (ScrollInfo.nPos > ScrollBar.getMax) then
            ScrollInfo.nPos := ScrollBar.getMax;

          if (SBUpdatesCount > 0) then
            ScrollBar.setValue(ScrollInfo.nPos);
        end;
      finally
        if ScrollBar.InUpdate then
          ScrollBar.EndUpdate;
      end;
    end;

    if (ScrollInfo.FMask and SIF_TRACKPOS) <> 0 then
    begin
      ScrollBar.TrackPos := ScrollInfo.nTrackPos;
      // from MSDN: the SetScrollInfo function ignores this member
      // ScrollBar.setSliderPosition(ScrollInfo.nTrackPos);
    end;

    Result := ScrollBar.getValue;
  end;

begin
  // bRedraw is useles with qt

  Result := 0;

  if (Handle = 0) then exit;

  ScrollBar := nil;
  case SBStyle of
    SB_BOTH:
    begin
      {TODO: SB_BOTH fixme }
      //writeln('TODO: ############## SB_BOTH CALLED HERE .... #################');
    end; {SB_BOTH}

    SB_CTL:
    begin
      {HWND is always TScrollBar, but seem that Create ScrollBar should be called here }
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState) or
      (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) then exit;

      ScrollBar := TQtScrollBar(Handle);

      if not Assigned(ScrollBar) then exit;
    end; {SB_CTL}

    SB_HORZ:
    begin
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState) or
         (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) then
        exit;

      if TQtWidget(Handle) is TQtAbstractScrollArea then
      begin
        ScrollBar := TQtAbstractScrollArea(Handle).horizontalScrollBar;
      end else
      begin
        {do not localize !}
        Control := TWinControl(TQtWidget(Handle).LCLObject.FindChildControl(TQtWidget(Handle).LCLObject.Name+'_HSCROLLBAR'));
        if (Control <> nil) and (Control.HandleAllocated) then
          ScrollBar := TQtScrollBar(Control.Handle)
      end;
    end; {SB_HORZ}

    SB_VERT:
    begin
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState) or
        (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) then
        exit;

      if TQtWidget(Handle) is TQtAbstractScrollArea then
      begin
        ScrollBar := TQtAbstractScrollArea(Handle).verticalScrollBar;
      end else
      begin
        {do not localize !}
        Control := TWinControl(TQtWidget(Handle).LCLObject.FindChildControl(TQtWidget(Handle).LCLObject.Name+'_VSCROLLBAR'));
        if (Control <> nil) and (Control.HandleAllocated) then
          ScrollBar := TQtScrollBar(Control.Handle)
      end;
    end; {SB_VERT}

  end;

  if Assigned(ScrollBar) then
    Result := UpdateScrollInfo;
end;*)

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
var
  lFont: TFPCustomFont;
  LazDC: TLazCanvas;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[TCDWidgetSet.SetTextColor]  DC: %x Color: %8x', [DC, Color]));
  {$endif}

  Result := CLR_INVALID;
  if not IsValidDC(DC) then Exit;
  LazDC := TLazCanvas(DC);

  if LazDC.Font <> nil then
    LazDC.Font.FPColor := TColorToFPColor(Color);
end;

(*{------------------------------------------------------------------------------
  function ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
  Params  Handle: HWND; wBar: Integer; bShow: Boolean
  Result
------------------------------------------------------------------------------}
function TQtWidgetSet.ShowScrollBar(Handle: HWND; wBar: Integer; bShow: Boolean): Boolean;
var
  w: TQtWidget;
  ScrollArea: TQtAbstractScrollArea;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ShowScrollBar] Handle: ', dbghex(Handle),' wBar: ',wBar);
  {$endif}

  Result := (Handle <> 0);

  if not Result then exit;

  w := TQtWidget(Handle);

  if w is TQtAbstractScrollArea then
  begin
    ScrollArea := TQtAbstractScrollArea(w);
    case wBar of
      SB_BOTH:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssBoth)
        else
          ScrollArea.setScrollStyle(ssNone);
      end;

      SB_HORZ:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssHorizontal)
        else
          ScrollArea.ScrollBarPolicy[False] := QtScrollBarAlwaysOff;
      end;

      SB_VERT:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssVertical)
        else
          ScrollArea.ScrollBarPolicy[True] := QtScrollBarAlwaysOff;
      end;

      SB_CTL:
      begin
        if bShow then
          ScrollArea.Show
        else
          ScrollArea.Hide;
      end;
    end;

  end else
    Result := False;
end;

function TQtWidgetSet.SetViewPortExtEx(DC: HDC; XExtent, YExtent : Integer; OldSize: PSize): Boolean;
var
  R, RW: TRect;
  Ratio: Single;
begin
  Result := False;
  if IsValidDC(DC) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    if OldSize <> nil then
    begin
      OldSize^.cx := R.Right - R.Left;
      OldSize^.cy := R.Bottom - R.Top;
    end;
    if (XExtent <> R.Right) or (YExtent <> R.Bottom) then
    begin
      case TQtDeviceContext(DC).vMapMode of
        MM_ANISOTROPIC, MM_ISOTROPIC:
        begin
          if TQtDeviceContext(DC).vMapMode = MM_ISOTROPIC then
          begin
            // TK: Is here also an adjustment on Windows if DPIX and DPIY are different?
            QPainter_Window(TQtDeviceContext(DC).Widget, @RW);
            Ratio := RW.Right / RW.Bottom; // no check, programmer cannot put nonsense
            if YExtent * Ratio > XExtent then
              YExtent := RoundToInt(XExtent / Ratio)
            else if YExtent * Ratio < XExtent then
              XExtent := RoundToInt(YExtent * Ratio)
          end;
          QPainter_setViewPort(TQtDeviceContext(DC).Widget, R.Left, R.Top, XExtent, YExtent);
          Result := True;
        end;
      end;
    end;
  end;
end;

function TQtWidgetSet.SetViewPortOrgEx(DC: HDC; NewX, NewY: Integer; OldPoint: PPoint): Boolean;
var
  R: TRect;
begin
  Result := False;
  if IsValidDC(DC) then
  begin
    QPainter_ViewPort(TQtDeviceContext(DC).Widget, @R);
    if OldPoint <> nil then
      OldPoint^ := R.TopLeft;
    if (TQtDeviceContext(DC).vMapMode <> MM_TEXT) and (NewX <> R.Left) or (NewY <> R.Top) then
    begin
      QPainter_setViewPort(TQtDeviceContext(DC).Widget, NewX, NewY, R.Right - R.Left, R.Bottom - R.Top);
      Result := True;
    end;
  end;
end;

function TQtWidgetSet.SetWindowExtEx(DC: HDC; XExtent, YExtent: Integer; OldSize: PSize): Boolean;
var
  R: TRect;
begin
  Result := False;
  if IsValidDC(DC) then
  begin
    QPainter_Window(TQtDeviceContext(DC).Widget, @R);
    if OldSize <> nil then
    begin
      OldSize^.cx := R.Right - R.Left;
      OldSize^.cy := R.Bottom - R.Top;
    end;
    if (XExtent <> R.Right) or (YExtent <> R.Bottom) then
    begin
      case TQtDeviceContext(DC).vMapMode of
        MM_ANISOTROPIC, MM_ISOTROPIC:
        begin
          QPainter_setWindow(TQtDeviceContext(DC).Widget, R.Left, R.Top, XExtent, YExtent);
          Result := True;
        end;
      end;
    end;
  end;
end;*)

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - handle of device context
           NewX  - new x-coordinate of window origin
           NewY  - new y-coordinate of window origin
           Point - record receiving original origin
  Returns: Whether the call was successful

  Sets the window origin of the device context by using the specified coordinates.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean;
var
  P: TPoint;
  LazDC: TLazCanvas absolute DC;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn(Format('[WinAPI SetWindowOrgEx] DC=%x  NewX=%d NewY=%d',
      [DC, NewX, NewY]));
  {$endif}

  Result := False;
  if not IsValidDC(DC) then Exit;

  GetWindowOrgEx(DC, @P);
  if OldPoint <> nil then OldPoint^ := P;

  LazDC.WindowOrg := Types.Point(-NewX, -NewY);
  Result := True;
end;

(*{------------------------------------------------------------------------------
  Method:  SetWindowPos
  Params: HWnd            - handle of window
          HWndInsertAfter - placement-order handle
          X               - horizontal position
          Y               - vertical position
          CX              - width
          CY              - height
          UFlags          - window-positioning flags
  Returns: If the function succeeds

  Changes the size, position, and Z order of a child, pop-up, or top-level
  window.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowPos(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx,
  cy: Integer; uFlags: UINT): Boolean;
var
  DisableUpdates: boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowPos] Handle: ', dbghex(hWnd),
      ' hWndInsertAfter: ',dbghex(hWnd));
  {$endif}
  Result := hWnd <> 0;
  if not Result then
    exit;

  DisableUpdates := (SWP_NOREDRAW and uFlags) <> 0;
  if DisableUpdates then
    TQtWidget(Hwnd).setUpdatesEnabled(False);
  try
    if (SWP_NOMOVE and uFlags) = 0 then
      TQtWidget(Hwnd).move(X, Y);

    if (SWP_NOSIZE and uFlags) = 0 then
      TQtWidget(Hwnd).resize(CX, CY);

    if (SWP_NOZORDER and uFlags) = 0 then
    begin
      case hWndInsertAfter of
        HWND_TOP:
          begin
            TQtWidget(hWnd).raiseWidget;
            if (SWP_NOACTIVATE and uFlags) = 0 then
              TQtWidget(hWnd).Activate;
          end;
        HWND_BOTTOM: TQtWidget(hWnd).lowerWidget;
        {TODO: HWND_TOPMOST ,HWND_NOTOPMOST}
      end;
    end;
  finally
    if DisableUpdates then
      TQtWidget(Hwnd).setUpdatesEnabled(True);
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetWindowRgn
  Params:  hWnd    - handle of the widget
           hRgn    - handle of the region
           bRedraw - ?
  Returns: 0 if the call failed, any other value if it was successful

  Makes the region specifyed in hRgn be the only part of the window which is
  visible.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowRgn(hWnd: HWND;
 hRgn: HRGN; bRedraw: Boolean):longint;
var
  w: TQtWidget;
  r: TQtRegion;
begin
  Result := 0;

  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowRgn] Handle: ', dbghex(hWnd));
  {$endif}

  // Basic checks
  if (hWnd = 0) or (hRgn = 0) then Exit;

  w := TQtWidget(hWnd);
  r := TQtRegion(hRgn);

  // Now set the mask in the widget
  w.setMask(r.FHandle);

  Result := 1;
end;

{------------------------------------------------------------------------------
  function ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;

  nCmdShow:
    SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED
------------------------------------------------------------------------------}
function TQtWidgetSet.ShowWindow(hWnd: HWND; nCmdShow: Integer): Boolean;
var
  Widget: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI}
  WriteLn('[WinAPI ShowWindow] hwnd ',dbgHex(PtrUInt(hWnd)),' nCmdShow ',nCmdShow);
  {$endif}

  Result := False;

  Widget := TQtWidget(hWnd);

  if Widget <> nil then
  begin
    case nCmdShow of
      SW_SHOW: Widget.setVisible(True);
      SW_SHOWNORMAL: Widget.ShowNormal;
      SW_MINIMIZE: Widget.setWindowState(QtWindowMinimized);
      SW_SHOWMINIMIZED: Widget.ShowMinimized;
      SW_SHOWMAXIMIZED: Widget.ShowMaximized;
      SW_SHOWFULLSCREEN: Widget.ShowFullScreen;
      SW_HIDE: Widget.setVisible(False);
    end;
    Result := True;
  end;
end;*)

{------------------------------------------------------------------------------
  Function: StretchBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified raster operation. If needed it
  resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
  If SrcDC contains a mask the pixmap will be copied with this transparency.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; ROp: Cardinal): Boolean;
begin
  Result := StretchMaskBlt(DestDC,X,Y,Width,Height,
                          SrcDC,XSrc,YSrc,SrcWidth,SrcHeight,
                          0,0,0,
                          ROp);
end;

{------------------------------------------------------------------------------
  Function: StretchMaskBlt
  Params:  DestDC:                The destination devicecontext
           X, Y:                  The left/top corner of the destination rectangle
           Width, Height:         The size of the destination rectangle
           SrcDC:                 The source devicecontext
           XSrc, YSrc:            The left/top corner of the source rectangle
           SrcWidth, SrcHeight:   The size of the source rectangle
           Mask:                  The handle of a monochrome bitmap
           XMask, YMask:          The left/top corner of the mask rectangle
           ROp:                   The raster operation to be performed
  Returns: True if succesful

  The StretchMaskBlt function copies a bitmap from a source rectangle into a
  destination rectangle using the specified mask and raster operation. If needed
  it resizes the bitmap to fit the dimensions of the destination rectangle.
  Sizing is done according to the stretching mode currently set in the
  destination device context.
 ------------------------------------------------------------------------------}
function TCDWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcLazDC: TLazCanvas absolute SrcDC;
  DstLazDC: TLazCanvas absolute DestDC;
  BufferImage: TLazIntfImage = nil;
  BufferDC: TLazCanvas = nil;
  FreeBuffer: Boolean;
  SrcRect, DstRect, MaskRect: TRect;
begin
  {$ifdef VerboseCDDrawing}
    DebugLn('[WinAPI StretchMaskBlt]' +
     ' DestDC:' + dbghex(DestDC) +
     ' SrcDC:' + dbghex(SrcDC) +
     ' X:' + dbgs(X) + ' Y:' + dbgs(Y) +
     ' W:' + dbgs(Width) + ' H:', dbgs(Height) +
     ' XSrc:' + dbgs(XSrc) + ' YSrc:' + dbgs(YSrc) +
     ' WSrc:' + dbgs(SrcWidth) + ' HSrc:' + dbgs(SrcHeight));
  {$endif}

  Result := False;

  // Optimization if no stretch is desired
  if (SrcWidth = Width) and (SrcHeight = Height) then
  begin
    DstLazDC.CanvasCopyRect(SrcLazDC, X, Y, XSrc, YSrc, SrcWidth, SrcHeight);
    Exit;
  end;

  // Otherwise do the real stretch

  // Get an interpolation acording to the anti-aliasing option
  {if DstLazDC. .AntiAliasing then
    DstLazDC.Interpolation := TMitchelInterpolation.Create
  else}
    DstLazDC.Interpolation := TFPSharpInterpolation.Create;

  // Copy the source rectangle to a temporary buffer if it is not the entire source
  if (XSrc = 0) and (YSrc = 0) and (SrcWidth = SrcLazDC.Width) and (SrcHeight = SrcLazDC.Height) then
  begin
    BufferDC := SrcLazDC;
    BufferImage := TLazIntfImage(SrcLazDC.Image);
    FreeBuffer := False;
  end
  else
  begin
    UpdateControlLazImageAndCanvas(BufferImage, BufferDC,
      SrcWidth, SrcHeight, clfARGB32);
    BufferDC.CanvasCopyRect(SrcLazDC, 0, 0, XSrc, YSrc, SrcWidth, SrcHeight);
    FreeBuffer := True;
  end;

  // Execute the stretch
  DstLazDC.StretchDraw(X, Y, Width, Height, BufferImage);

  // Free the interpolation
  DstLazDC.Interpolation.Free;
  DstLazDC.Interpolation := nil;

  // Free the buffer
  if FreeBuffer then
  begin
    BufferDC.Free;
    BufferImage.Free;
  end;

  Result := True;
end;

(*{------------------------------------------------------------------------------
  Function: SystemParametersInfo
  Params: uiAction: System-wide parameter to be retrieved or set
          uiParam: Depends on the system parameter being queried or set
          pvParam: Depends on the system parameter being queried or set
          fWinIni:
  Returns: True if the function succeeds
  retrieves or sets the value of one of the system-wide parameters
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SystemParametersInfo(uiAction: DWord; uiParam: DWord; pvParam: Pointer; fWinIni: DWord): LongBool;
begin
  case uiAction of
    SPI_GETWHEELSCROLLLINES: PDword(pvPAram)^ := QApplication_wheelScrollLines;
    SPI_GETWORKAREA: begin
      TRect(pvParam^):=Bounds(GetSystemMetrics(SM_XVIRTUALSCREEN),
                              GetSystemMetrics(SM_YVIRTUALSCREEN),
                              GetSystemMetrics(SM_CXVIRTUALSCREEN),
                              GetSystemMetrics(SM_CYVIRTUALSCREEN));
      Result:=True;
    end;
  else
    Result := False;
  end
end;*)

{------------------------------------------------------------------------------
  Function: TextOut
  Params: DC:
          X:
          Y:
          Str:
          Count:
  Returns:

 ------------------------------------------------------------------------------}
function TCDWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : Pchar; Count: Integer) : Boolean;
begin
  Result:=ExtTextOut(DC, X, Y, 0, nil, Str, Count, nil);
end;

(*{------------------------------------------------------------------------------
  Method:  UpdateWindow
  Params:  Handle
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
 {$ifdef VerboseQtWinAPI}
   WriteLn('[WinAPI UpdateWindow]');
 {$endif}
  Result := False;
  if Handle <> 0 then
  begin
    TQtWidget(Handle).Update;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params:  TPoint
  Returns: The return value is a handle to the window that contains the param
  point.
  If no window exists at the given point, the return value is 0.
  If the point is over a static text control,
  the return value is a handle to the window under the static text control.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.WindowFromPoint(APoint: TPoint): HWND;
var
  Widget: QWidgetH;
begin
  // we use cachedresults instead of calling very expensive widgetAt
  if (FLastWFPResult <> 0) then
  begin
    if not IsValidWidgetAtCachePointer then
      FLastWFPResult := 0
    else
    if (APoint.X = FLastWFPMousePos.X) and (APoint.Y = FLastWFPMousePos.Y) and
      TQtWidget(FLastWFPResult).getVisible and
      TQtWidget(FLastWFPResult).getEnabled then
    begin
      // return from cache
      exit(FLastWFPResult);
    end;
  end;

  Result := 0;
  Widget := QApplication_widgetAt(APoint.x, APoint.y);

  if (Widget = nil) then
  begin
    if (APoint.X = FLastWFPMousePos.X) and (APoint.Y = FLastWFPMousePos.Y) then
    begin
      FLastWFPMousePos := Point(MaxInt, MaxInt);
      FLastWFPResult := 0;
    end;
    exit;
  end;

  // according to MSDN disabled widget shouldn't be in result
  // but win32 returns first enabled and visible parent !
  if not QWidget_isEnabled(Widget) or not QWidget_isVisible(Widget) then
  begin
    while Widget <> nil do
    begin
      Widget := QWidget_parentWidget(Widget);
      if (Widget <> nil) and QWidget_IsVisible(Widget) and
        QWidget_isEnabled(Widget) then
          break;
    end;
    if Widget = nil then
      exit;
  end;

  Result := HwndFromWidgetH(Widget);

  // return from cache if we are same TQtWidget, just update point
  if IsValidWidgetAtCachePointer and (Result = FLastWFPResult) then
  begin
    FLastWFPMousePos := APoint;
    exit(FLastWFPResult);
  end;

  // maybe we are viewport of native QAbstractScrollArea (eg. QTextEdit).
  if (Result = 0) then
  begin
    if QWidget_parentWidget(Widget) <> nil then
    begin
      while (Widget <> nil) do
      begin
        Widget := QWidget_parentWidget(Widget);
        if Widget <> nil then
          Result := HwndFromWidgetH(Widget);
        if Result <> 0 then
          break;
      end;
    end;
  end;

  if (Result <> 0) and
    not (TQtWidget(Result) is TQtMainWindow) then
  begin
    if TQtWidget(Result).getOwner <> nil then
      Result := HWND(TQtWidget(Result).getOwner);
  end else
  begin
    Widget := QApplication_topLevelAt(APoint.x, APoint.y);
    if (Widget <> nil) and QWidget_isEnabled(Widget) then
      Result := HwndFromWidgetH(Widget)
    else
      Result := 0;
  end;

  // add to cache
  FLastWFPResult := Result;
  FLastWFPMousePos := APoint;
end;*)

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
