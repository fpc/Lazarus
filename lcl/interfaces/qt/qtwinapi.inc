{%MainUnit qtint.pp}
{ $Id$ }
{******************************************************************************
  All QT Winapi implementations.
  This are the implementations of the overrides of the QT Interface for the
  methods defined in the
  lcl/include/winapi.inc


  !! Keep alphabetical !!


 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove, no wizard declaration before this line

{------------------------------------------------------------------------------
  Function: Arc
  Params: DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Arc(DC: HDC; Left,Top,Right,Bottom,angle1,angle2 : Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Arc] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC);
  if Result then
    QPainter_drawArc(TQtDeviceContext(DC).Widget, Left, Top, Right, Bottom, Angle1, Angle2);
end;

{------------------------------------------------------------------------------
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
end;

{------------------------------------------------------------------------------
  Function: BeginPaint
  Params:
  Returns:

  This function is Called:
  - Once on every OnPaint event
 ------------------------------------------------------------------------------}
function TQtWidgetSet.BeginPaint(Handle: hWnd; Var PS : TPaintStruct): hdc;
var
  Widget: TQtWidget;
  DC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI BeginPaint] Handle=', dbghex(Handle));
  {$endif}
  Widget := TQtWidget(Handle);
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
  {$endif}
end;

function TQtWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [TQtWidgetSet.BitBlt]');
  {$endif}
  
  Result := StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width,
                       Height, ROP);
                       
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [TQtWidgetSet.BitBlt]');
  {$endif}
end;

function TQtWidgetSet.CallNextHookEx(hHk: HHOOK; ncode : Integer; wParam: WParam; lParam : LParam) : Integer;
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
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method:  ClientToScreen
  Params:  Handle    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ClientToScreen(Handle: HWND; var P: TPoint) : Boolean;
var
  APoint: TQtPoint;
  R: TRect;
begin
  Result := True;
  if Handle <> 0 then
  begin
    APoint.x := P.X;
    APoint.y := P.Y;
    
    QWidget_mapToGlobal(TQtWidget(Handle).Widget, @APoint, @APoint);
    P.X := APoint.x;
    P.Y := APoint.y;
    R := TQtWidget(Handle).getClientBounds;
    inc(P.X, R.Left);
    inc(P.Y, R.Top);
  end;
end;


function TQtWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := Clipboard.FormatToMimeType(FormatID);
end;

function TQtWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  Result := Clipboard.Getdata(ClipboardType, FormatID, Stream);
end;

function TQtWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
begin
  Result := Clipboard.GetFormats(ClipboardType, Count, List);
end;

function TQtWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
begin
  Result := Clipboard.GetOwnerShip(ClipboardType, OnRequestProc, FormatCount, Formats);
end;

function TQtWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := Clipboard.RegisterFormat(AMimeType);
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
function TQtWidgetSet.CombineRgn(Dest, Src1, Src2: HRGN; fnCombineMode: Longint): Longint;
var
  RDest,RSrc1,RSrc2: QRegionH;
begin
  result:=ERROR;

  if not IsValidGDIObject(Dest) or not IsValidGDIObject(Src1) then
    exit
  else begin
    RDest := TQtRegion(Dest).Widget;
    RSrc1 := TQtRegion(Src1).Widget;
  end;
  
  if (fnCombineMode<>RGN_COPY) and not IsValidGDIObject(Src2) then
    exit
  else
    RSrc2 := TQtRegion(Src2).Widget;
  
  case fnCombineMode of
    RGN_AND:
      QRegion_Intersect(RSrc1, RDest, RSrc2);
    RGN_COPY:
      begin
        // union of Src1 with a null region
        RSrc2 := QRegion_Create;
        QRegion_unite(RSrc1, RDest, RSrc2);
        QRegion_Destroy(RSrc2);
      end;
    RGN_DIFF:
      QRegion_Subtract(RSrc1, RDest, RSrc2);
    RGN_OR:
      QRegion_Unite(RSrc1, RDest, RSrc2);
    RGN_XOR:
      QRegion_eor(RSrc1, RDest, RSrc2);
  end;
  
  if QRegion_isEmpty(RDest) then
    result := NULLREGION
  else begin
    // TODO: Evaluate if region is complex
    Result := SIMPLEREGION;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.ComboBoxDropDown
  Params: Handle: HWND; DropDown: boolean
  Returns:

  Shows or hides combobox dropdown list via DropDown parameter.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean;
var
  ComboList: QAbstractItemViewH;
  ComboBox: QComboBoxH;
begin
  {$ifdef VerboseQtWinAPI}
	WriteLn('[WinAPI TQtWidgetSet.ComboBoxDropDown] ');
  {$endif}
  Result := False;
  if TQtWidget(Handle) is TQtComboBox then
  begin
    ComboBox := QComboBoxH(TQtComboBox(Handle).Widget);
    ComboList := QComboBox_view(ComboBox);
    if DropDown <> QWidget_isVisible(ComboList) then
    begin
      if DropDown then QComboBox_showPopup(ComboBox)
      else QComboBox_hidePopup(ComboBox);
    end;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.CreateCompatibleBitmap
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer): HBITMAP;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  	WriteLn('***** [WinAPI TQtWidgetSet.CreateCompatibleBitmap] missing implementation ');
  {$endif}
  Result := 0;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.CreateBitmap
  Params:
  Returns:

  This functions is for TBitmap support.
  Specifically it´s utilized on when a handle for a bitmap is needed
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateBitmap(Width, Height: Integer;
  Planes, BitCount: Longint; BitmapBits: Pointer): HBITMAP;
var
  Format: QImageFormat;
  NewBits: PByte;
  NewBitsSize: PtrUInt;
  ARowStride, RSS: Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI CreateBitmap]',
     ' Width:', dbgs(Width),
     ' Height:', dbgs(Height),
     ' Planes:', dbgs(Planes),
     ' BitCount:', dbgs(BitCount),
     ' BitmapBits: ', dbgs(BitmapBits));
  {$endif}

  // for win32 data is aligned to WORD
  // for qt we must realign data to DWORD

  case BitCount of
    1: Format := QImageFormat_Mono;
    15, 16: Format := QImageFormat_RGB16;
    24: Format := QImageFormat_RGB32;
    32: Format := QImageFormat_ARGB32;
  else
    Format := QImageFormat_ARGB32;
  end;
  
  RSS := GetBytesPerLine(Width, BitCount, rileWordBoundary);
  if BitmapBits <> nil then
  begin
    ARowStride := GetBytesPerLine(Width, BitCount, rileDWordBoundary);
    if not CopyImageData(Width, Height, RSS, BitCount, BitmapBits, Rect(0, 0, Width, Height),
      riloBottomToTop, riloBottomToTop, rileDWordBoundary, NewBits, NewBitsSize) then
    begin
      // this was never tested
      ARowStride := RSS;
      NewBits := AllocMem(RSS * Height);
      Move(BitmapBits^, NewBits^, RSS * Height);
    end;
    Result := HBitmap(TQtImage.Create(NewBits, Width, Height, ARowStride, Format, True));
  end
  else
    Result := HBitmap(TQtImage.Create(nil, Width, Height, Format));

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI CreateBitmap] Bitmap:', dbghex(Result));
  {$endif}
end;


{------------------------------------------------------------------------------
  Function:  CreateBrushIndirect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateBrushIndirect(const LogBrush: TLogBrush): HBRUSH;
var
  QtBrush: TQtBrush;
  Color: TQColor;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn(Format('Trace:> [WinAPI CreateBrushIndirect]  Style: %d, Color: %8x (%s)',
      [LogBrush.lbStyle, LogBrush.lbColor, ColorToString(LogBrush.lbColor)]));
  {$endif}

  result := 0;

  QtBrush := TQtBrush.Create(True);

  try
    case LogBrush.lbStyle of
//      BS_HOLLOW,         // Hollow brush.
      BS_NULL:           // Same as BS_HOLLOW.
      begin
        QtBrush.setStyle(QtNoBrush);
      end;

      BS_SOLID:          // Solid brush.
      begin
        QtBrush.setStyle(QtSolidPattern);
      end;

      BS_HATCHED:        // Hatched brush.
      begin
        case LogBrush.lbHatch of
          HS_BDIAGONAL: QtBrush.setStyle(QtBDiagPattern);
          HS_CROSS: QtBrush.setStyle(QtCrossPattern);
          HS_DIAGCROSS: QtBrush.setStyle(QtDiagCrossPattern);
          HS_FDIAGONAL: QtBrush.setStyle(QtFDiagPattern);
          HS_HORIZONTAL: QtBrush.setStyle(QtHorPattern);
          HS_VERTICAL: QtBrush.setStyle(QtVerPattern);
        else
          RaiseGDBException('invalid lbHatch');
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
        QtBrush.setTextureImage(TQtImage(LogBrush.lbHatch).Handle);
      end;
    else
      WriteLn(Format('Unsupported Style %d',[LogBrush.lbStyle]));
    end;

    {
      Other non-utilized Qt brushes:
        QtDense1Pattern,
        QtDense2Pattern,
        QtDense3Pattern,
        QtDense4Pattern,
        QtDense5Pattern,
        QtDense6Pattern,
        QtDense7Pattern,
        QtLinearGradientPattern,
        QtRadialGradientPattern,
        QtConicalGradientPattern
    }
    
    // set brush color
    Color := QBrush_Color(QtBrush.Widget)^;
    ColorRefToTQColor(ColorToRGB(logBrush.lbColor), Color);
    QBrush_setColor(QtBrush.Widget, @Color);
  except
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI CreateBrushIndirect] Failed');
    {$endif}
  end;

  Result := HBRUSH(QtBrush);

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI CreateBrushIndirect] Result: ', dbghex(Result));
  {$endif}
end;

function TQtWidgetSet.CreateCaret(Handle: HWND; Bitmap: hBitmap; Width, Height: Integer): Boolean;
begin
  Result := (Handle <> 0) and
    QtCaret.CreateCaret(TQtWidget(Handle), Bitmap, Width, Height);
end;

{------------------------------------------------------------------------------
  Function: CreateCompatibleDC
  Params:  DC - handle to memory device context
  Returns: handle to a memory device context

  Creates a memory device context (DC) compatible with the specified device.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateCompatibleDC(DC: HDC): HDC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI CreateCompatibleDC] DC: ', dbghex(DC));
  {$endif}
  Result := HDC(TQtDeviceContext.Create(nil, True));
end;

{------------------------------------------------------------------------------
  Function: CreateCursor
  Params:  ACursorInfo - PIconInfo
  Returns: hCursor (QCursorH)

  Creates a cursor from bitmap and mask.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateCursor(ACursorInfo: PIconInfo): hCursor;
var
  Image: TQtImage;
  Pixmap: QPixmapH;
begin
  Result := 0;
  if IsValidGDIObject(ACursorInfo^.hbmColor) then
  begin
    Image := TQtImage(ACursorInfo^.hbmColor);
    Pixmap := QPixmap_create();
    QPixmap_fromImage(Pixmap, Image.Handle);
    Result := hCursor(QCursor_create(Pixmap, ACursorInfo^.xHotspot, ACursorInfo^.yHotspot));
    QPixmap_destroy(Pixmap);
  end;
end;

{------------------------------------------------------------------------------
  Function: CreateEllipticRgn
  Params:  p1, p2, p3, p4: Integer
  Returns: HRGN
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateEllipticRgn(p1, p2, p3, p4: Integer): HRGN;
var
  QtRegion: TQtRegion;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI CreateEllipticRgn] ');
  {$endif}
  QtRegion := TQtRegion.Create(True, p1, p2, p3, p4, QRegionEllipse);
  Result := HRGN(QtRegion);
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirect
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateFontIndirect(const LogFont: TLogFont): HFONT;
begin
  Result := CreateFontIndirectEx(LogFont, '');
end;

{------------------------------------------------------------------------------
  Function: CreateFontIndirectEx
  Params:  const LogFont: TLogFont
  Returns: HFONT

  Creates a font GDIObject.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateFontIndirectEx(const LogFont: TLogFont; const LongFontName: string): HFONT;
var
  QtFont: TQtFont;
  FamilyName: string;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI CreateFontIndirectEx] FontName: ' + LongFontName);
  {$endif}

  Result := 0;

  QtFont := TQtFont.Create(True);
  try
    // -1 has different meaning - it means that font height was set using setPointSize
    if LogFont.lfHeight <> -1 then
      QtFont.setPixelSize(Abs(LogFont.lfHeight));

    // Some values at available on Qt documentation at a table
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
    begin
      QtFont.setFamily(FamilyName);
    end;

  finally
    Result := HFONT(QtFont);
  end;
end;

{------------------------------------------------------------------------------
  Function:  CreatePenIndirect
  Params:  none
  Returns: HPEN
 ------------------------------------------------------------------------------}

function TQtWidgetSet.CreatePenIndirect(const LogPen: TLogPen): HPEN;
var
  QtPen: TQtPen;
  color: TQColor;
begin
//  Assert(False, 'trace:[TQtWidgetSet.CreatePenIndirect]');
//   writeln('CreatePenIndirect->');
  Result := 0;	
  QtPen := TQtPen.Create(True);
  
  with LogPen do
  begin
    case lopnStyle of
      PS_SOLID: QtPen.setStyle(QtSolidLine);
      PS_DASH: QtPen.setStyle(QtDashLine);
      PS_DOT: QtPen.setStyle(QtDotLine);
      PS_DASHDOT:QtPen.setStyle(QtDashDotLine);
      PS_DASHDOTDOT:QtPen.setStyle(QtDashDotDotLine);
      PS_USERSTYLE: QtPen.setStyle(QtCustomDashLine);
      PS_NULL:QtPen.setStyle(QtNoPen);
    else
      QtPen.setStyle(QtSolidLine);
    end;

    QtPen.setWidth(lopnWidth.X);

    QPen_Color(QtPen.Widget, @Color);
    ColorRefToTQColor(ColorToRGB(lopnColor), Color);
    QPen_setColor(QtPen.Widget, @Color);
  end;

  Result := HPEN(QtPen);
end;

{------------------------------------------------------------------------------
  Function: CreatePolygonRgn
  Params:  none
  Returns: HRGN

 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreatePolygonRgn(Points: PPoint; NumPts: Integer; FillMode: integer): HRGN;
var
  QtRegion: TQtRegion;
  pts: Array of TQtPoint;
  i: Integer;
  p: PPoint;
  Poly: QPolygonH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace: [WinAPI CreatePolygonRgn] ');
  {$endif}
  SetLength(pts, NumPts);
  p := PPoint(@Points);
  for i := 0 to NumPts - 1 do
  begin
    pts[i].X := p^.X;
    pts[i].Y := p^.Y;
    Inc(p);
  end;
  Poly := QPolygon_create(NumPts, @pts[0]);
  try
    {fillmode can be ALTERNATE or WINDING as msdn says}
    if FillMode = ALTERNATE then
			QtRegion := TQtRegion.Create(True, Poly, QtOddEvenFill)
    else
    	QtRegion := TQtRegion.Create(True, Poly, QtWindingFill);
    Result := HRGN(QtRegion);
  finally
    QPolygon_destroy(Poly);
  end;
end;

{------------------------------------------------------------------------------
  Function: CreateRectRgn
  Params:  none
  Returns: HRGN


 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateRectRgn(X1,Y1,X2,Y2 : Integer): HRGN;
var
  QtRegion: TQtRegion;
begin
  QtRegion := TQtRegion.Create(True, X1,Y1,X2,Y2);
  Result := HRGN(QtRegion); 
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace: [WinAPI CreateRectRgn] Result: ', dbghex(Result),
     ' QRegionH: ', dbghex(PtrInt(QtRegion.Widget)));
  {$endif}
end;

{------------------------------------------------------------------------------
  Procedure: DeleteCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.DeleteCriticalSection(var CritSection: TCriticalSection);
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
function TQtWidgetSet.DeleteDC(hDC: HDC): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DeleteDC] Handle: ', dbghex(hDC));
  {$endif}
  
  Result := False;
  if not IsValidDC(hDC) then exit;
  
  TQtDeviceContext(hDC).Free;
end;

{------------------------------------------------------------------------------
  Function: DeleteObject
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TQtWidgetSet.DeleteObject(GDIObject: HGDIOBJ): Boolean;
var
  aObject: TObject;
  APaintEngine: QPaintEngineH;
  APainter: QPainterH;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI DeleteObject] GDIObject: ', dbghex(GDIObject));
    ObjType := 'Unidentifyed';
  {$endif}

  Result := False;

  if GDIObject = 0 then
  begin
    Result := true;

    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI DeleteObject]');
    {$endif}

    Exit;
  end;
  
  if not IsValidGDIObject(GDIObject) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI DeleteObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObject);

  {------------------------------------------------------------------------------
    Font
   ------------------------------------------------------------------------------}
  if aObject is TQtFont then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}
    
//    TQtFont(aObject).Free;
  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}

//    TQtBrush(aObject).Free;
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}
    
    // we must stop paintdevice before destroying

    APaintEngine := QImage_paintEngine(TQtImage(AObject).Handle);
    
    if (APaintEngine <> NiL)
    and QPaintEngine_isActive(APaintEngine) then
    begin
      APainter := QPaintEngine_painter(APaintEngine);
      if APainter <> nil then
        QPainter_end(APainter);
    end ;

  end
  {------------------------------------------------------------------------------
    Region
   ------------------------------------------------------------------------------}
  else if aObject is TQtRegion then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Region';
    {$endif}
    
//    TQtRegion(aObject).Free;
  end
  
  {------------------------------------------------------------------------------
    Pen
   ------------------------------------------------------------------------------}
  else if aObject is TQtPen then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Pen';
    {$endif}

//    TQtRegion(aObject).Free;
  end;

  if AObject is TQtResource then
    if TQtResource(AObject).Owner<>nil then
    begin
      // this is an owned (default) resource, let owner free it
      DebugLn('WARNING: Trying to Free a default resource');
      AObject:=nil;
    end;
  
  if AObject <> nil then
    AObject.Free;
  
  // Find out if we want to release internal GDI object
{    case GDIType of
      gdiBrush:
      gdiBitmap:
      gdiPen:
      gdiRegion:
      gdiPalette:
      else begin
        Result:= false;
        DebugLn('[TGtkWidgetSet.DeleteObject] TODO : Unimplemented GDI type');
        Assert(False, 'Trace:TODO : Unimplemented GDI object in delete object');
  end;}

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI DeleteObject] Result=', dbgs(Result), ' ObjectType=', ObjType);
  {$endif}
end;

function TQtWidgetSet.DestroyCaret(Handle: HWND): Boolean;
begin
  Result := (Handle <> 0) and QtCaret.DestroyCaret;
end;

{------------------------------------------------------------------------------
  Method:  DestroyCursor
  Params:  Handle
  Returns: Result of destroying
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DestroyCursor(Handle: hCursor): Boolean;
begin
  QCursor_destroy(QCursorH(Handle));
  Result := True;
end;

{------------------------------------------------------------------------------
  Method: DrawFocusRect
  Params: DC: HDC; const Rect: TRect
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
var
  StyleOption: QStyleOptionFocusRectH;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[TQtWidgetSet.DrawFocusRect] Handle: ', dbghex(DC));
  {$endif}
  Result := False;

  if not IsValidDC(DC) then exit;

  QtDC := TQtDeviceContext(DC);
  StyleOption := QStyleOptionFocusRect_create;
  try
    QStyleOption_setRect(StyleOption, @Rect);
    QStyle_drawPrimitive(QApplication_style, QStylePE_FrameFocusRect, StyleOption, QtDC.Widget, QtDC.Parent);
    Result := True;
  finally
    QStyleOptionFocusRect_destroy(StyleOption);
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
  ColorDark, ColorLight: TColor;
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
    EdgeQtColor.ColorSpec := 1; // rgb
    EdgeQtColor.Alpha := $FFFF;
    EdgeQtColor.Pad := 0;

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
      if grfFlags and BF_MONO = 0 then
        ColorLeftTop := ColorToRgb(ColorLeftTop);

      APen := TQtPen.Create(True);
      ColorRefToTQColor(ColorLeftTop, EdgeQtColor);
      APen.setColor(EdgeQtColor);
      OldPen := QtDC.setPen(APen);

      if grfFlags and BF_LEFT <> 0 then
        QtDC.DrawLine(X1, Y1, X1, Y2);
      if grfFlags and BF_TOP <> 0 then
        QtDC.DrawLine(X1, Y1, X2, Y1);

      QtDC.setPen(OldPen);
      APen.Free;
      APen := TQtPen.Create(True);

      if grfFlags and BF_MONO = 0 then
        ColorRightBottom := ColorToRgb(ColorRightBottom);

      ColorRefToTQColor(ColorRightBottom, EdgeQtColor);
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
      if grfFlags and BF_MONO = 0 then
        ColorRightBottom := ColorToRgb(ColorRightBottom);

      APen := TQtPen.Create(True);
      ColorRefToTQColor(ColorLeftTop, EdgeQtColor);
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
  if not IsValidDC(DC) then exit;
  
  QtDC := TQtDeviceContext(DC);
  
  ClientRect := Rect;
  Dec(ClientRect.Right, 1);
  Dec(ClientRect.Bottom, 1);
  QPainter_save(QtDC.Widget);
  try
    ColorDark := ColorToRGB(clDark);
    ColorLight := ColorToRGB(clBtnHighlight);
    if grfFlags and BF_FLAT <> 0 then
      ColorLight := clSilver;
    if grfFlags and BF_MONO <> 0 then
    begin
      ColorDark := clBlack;
      ColorLight := clWhite;
    end;
    try
      if Edge and (BDR_SUNKENOUTER or BDR_RAISEDOUTER) <> 0 then
        InternalDrawEdge(True, Rect);
      InflateRect(ClientRect, -1, -1);
      if grfFlags and BF_MONO = 0 then
        ColorDark := ColorToRGB(clMid);
      if Edge and (BDR_SUNKENINNER or BDR_RAISEDINNER) <> 0 then
      begin
        InternalDrawEdge(False, ClientRect);
        InflateRect(ClientRect, -1, -1);
      end;
    finally
    end;

    if grfFlags and BF_MIDDLE <> 0 then
    begin
      Brush := CreateSolidBrush(clButton);
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
    QPainter_restore(QtDC.Widget);
  end;

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
  Matrix: QMatrixH;
  MatrixInv: QMatrixH;
  QtDC: TQtDeviceContext;
  Inverted: Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DPtoLP] ');
  {$endif}

  Result := False;
  
  if not IsValidDC(DC)
  then
    exit;
    
  QtDC := TQtDeviceContext(DC);

  Matrix := QMatrix_create;
  MatrixInv := QMatrix_create;
  QPainter_combinedMatrix(QtDC.Widget, Matrix);
  P := @Points;
  try
    while Count > 0 do
    begin
      Dec(Count);
      Inverted := QMatrix_isInvertible(Matrix);
      QMatrix_inverted(Matrix, MatrixInv, @Inverted);
      QtPoint.X := P^.X;
      QtPoint.Y := P^.Y;
      QMatrix_map(MatrixInv, @QtPoint, @QtPoint);
      P^.X := QtPoint.X;
      P^.Y := QtPoint.Y;
      Inc(P);
    end;
    
    Result := True;
  finally
    QMatrix_destroy(MatrixInv);
    QMatrix_destroy(Matrix);
  end;
end;

{------------------------------------------------------------------------------
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
const
  QtTextSingleLine            = $0100;
  QtTextDontClip              = $0200;
  QtTextExpandTabs            = $0400;
  QtTextShowMnemonic          = $0800;
  QtTextWordWrap              = $1000;
  QtTextWrapAnywhere          = $2000;
  QtTextHideMnemonic          = $8000;
  QtTextDontPrint             = $4000;
  QtTextIncludeTrailingSpaces =	$08000000;
  QtTextJustificationForced   = $10000;
var
  WideStr: WideString;
  R: TRect;
  F: Integer;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DrawText] DC: ', dbghex(DC), ' Str: ', string(Str),
     ' CalcRect: ', dbgs((Flags and DT_CALCRECT) = DT_CALCRECT));
  {$endif}

  Result := 0;

  if not IsValidDC(DC) then
    Exit;
    
  QtDC :=TQtDeviceContext(DC);

  WideStr := GetUtf8String(Str);

  // convert DT flags to QT Flags
  F := 0;
  // horizontal alignment
  if Flags and DT_CENTER <> 0 then
    F := F or  QtAlignHCenter
  else
  if Flags and DT_RIGHT <> 0 then
    F := F or QtAlignRight
  else
    F := F or QtAlignLeft;
  // vertical alignment
  if Flags and DT_VCENTER <> 0 then
    F := F or QtAlignVCenter
  else
  if Flags and DT_BOTTOM <> 0 then
    F := F or QtAlignBottom
  else
    F := F or QtAlignTop;

  // mutually exclusive wordbreak and singleline
  if Flags and DT_WORDBREAK <> 0 then
    F := F or QtTextWordWrap
  else
  if Flags and DT_SINGLELINE <> 0 then
    F := F or QtTextSingleLine;

  if Flags and DT_NOPREFIX = 0 then
    F := F or QtTextShowMnemonic;

  if Flags and DT_NOCLIP = 0 then
    F := F or QtTextDontClip;

  if Flags and DT_EXPANDTABS <> 0 then
    F := F or QtTextExpandTabs;
  
  QtDC.font.Metrics.BoundingRect(@R, @ARect, F, @WideStr);
  
  
  //TODO: result should be different when DT_VCENTER or DT_BOTTOM is set
  Result := R.Bottom - R.Top;

  if (Flags and DT_CALCRECT) = DT_CALCRECT then
  begin
    ARect.Right := ARect.Left + R.Right - R.Left;
    ARect.Bottom := ARect.Top + R.Bottom - R.Top;
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI DrawText] Rect=', dbgs(ARect));
    {$endif}
    Exit;
  end;

  with ARect do
    QtDC.DrawText(left, Top, Right-Left, Bottom-Top, F, @WideStr);
end;

{------------------------------------------------------------------------------
  Method:   Ellipse
  Params:   X1, Y1, X2, Y2
  Returns:  Nothing

  Use Ellipse to draw a filled circle or ellipse.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Ellipse(DC: HDC; x1, y1, x2, y2: Integer): Boolean;
begin
  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawEllipse(x1, y1, X2 - X1, Y2 - Y1);

  Result := True;
end;

function TQtWidgetSet.EnableScrollBar(Wnd: HWND; wSBflags, wArrows: Cardinal): Boolean;
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
  Result := QWidget_isEnabled(TQtWidget(hWnd).Widget);
  TQtWidget(hWnd).setEnabled(bEnable);
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
end;

{------------------------------------------------------------------------------
  Procedure: EnterCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.EnterCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  ACritSec:=System.PRTLCriticalSection(CritSection);
  System.EnterCriticalsection(ACritSec^);
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
  i: Integer;

  function QtGetFontFamilies(var List:TStringList):integer;
  var
    FontDB: QFontDatabaseH;
    StrLst: QStringlistH;
    Str: WideString;
    j: integer;
  begin
    Result := -1;
    FontDB := QFontDatabase_create();
    StrLst := QStringList_create;
    try
      QFontDatabase_families(FontDB, StrLst);
      Result := QStringList_size(StrLst);
      for j := 0 to Result - 1 do
      begin
        QStringList_at(StrLst, @Str, j);
        List.Add(GetUtf8String(Str));
      end;
    finally
      QFontDatabase_destroy(FontDB);
      QStringList_destroy(StrLst);
    end;
  end;
  
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI EnumFontFamiliesEx]');
  {$endif}
  Result := 0;
  
  if (lpLogFont^.lfCharSet = DEFAULT_CHARSET) and
     (lpLogFont^.lfFaceName= '') and
     (lpLogFont^.lfPitchAndFamily = 0) then
  begin
    FontType := 0;
    FontList := TStringList.create;
    try
      if QtGetFontFamilies(FontList) > 0 then
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
   {$note implement}
    Result := 0;
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
  ExRegion: QRegionH;
  QtDC: TQtDeviceContext absolute dc;
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ExcludeClipRect]');
  {$endif}
  
  Result := ERROR;
  
  if not IsValidDC(DC) then Exit;

  ExRegion := QRegion_create(Left, Top, Right - Left, Bottom - Top, QRegionRectangle);
  Region := QRegion_create;
  try
    QPainter_clipRegion(QtDC.Widget, Region);
    QRegion_subtract(Region, Region, ExRegion);
    QtDC.setClipRegion(Region);
    QtDC.setClipping(True);
    
    if QRegion_isEmpty(Region)
    then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(Region, @R);
      if QRegion_contains(Region, PRect(@R))
      then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  finally
    QRegion_destroy(Region);
    QRegion_destroy(ExRegion);
  end;
end;

function TQtWidgetSet.ExtSelectClipRGN(dc: hdc; rgn : hrgn; Mode : Longint) : Integer;
var
  Clip,
  Tmp : hRGN;
  X, Y : Longint;
  DCOrigin: TPoint;
  QtWidget: TQtWidget;
  QtDC: TQtDeviceContext;
begin
  // copied from gtk winapi
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.ExtSelectClipRGN] possible wrong implementation ');
  {$endif}
  If not IsValidDC(DC) then
  begin
    Result := ERROR;
    exit;
  end else
    Result := SIMPLEREGION;
  
  QtDC := TQtDeviceContext(DC);

  
  QtWidget := QtObjectFromWidgetH(TQtDeviceContext(DC).Parent);
  
  if Assigned(QtWidget) and (QtWidget.PaintData.ClipRegion = nil) then
  begin
    // there is no clipping region in the DC
    Case Mode of
      RGN_COPY:
        begin
         // Result := RegionType(PGdiObject(RGN)^.GDIRegionObject);
         // If Result <> ERROR then
          Result := SelectClipRGN(DC, RGN);
        end;
      RGN_OR,
      RGN_XOR,
      RGN_AND,
      RGN_DIFF:
        begin
          // get existing clip
          X := -1;
          Y := -1;
          if QtDC.Parent <> nil then
          begin
            X := QWidget_width(QtDC.Parent);
            Y := QWidget_height(QtDC.Parent);
          end;
          // GDK_Window_Get_Size(Drawable, @X, @Y);
          // DCOrigin := GetDCOffset(TQtDeviceContext(DC));
          GetDeviceSize(DC, DCOrigin);
          
          if (X = -1) and (Y = -1) then
            Clip := CreateRectRGN(-DCOrigin.X,-DCOrigin.Y,DCOrigin.X,DCOrigin.Y)
          else
            Clip := CreateRectRGN(-DCOrigin.X,-DCOrigin.Y,X-DCOrigin.X,Y-DCOrigin.Y);
          // create target clip
          Tmp := CreateEmptyRegion;
          // combine
          Result := CombineRGN(Tmp, Clip, RGN, Mode);
          // commit
          //DebugLn('TGtkWidgetSet.ExtSelectClipRGN B ClipRegValid=',dbgs(ClipRegion),' TmpRGN=',GDKRegionAsString(PGdiObject(Tmp)^.GDIRegionObject));
          SelectClipRGN(DC, Tmp);
          // clean up
          DeleteObject(Clip);
          DeleteObject(Tmp);
        end;
      end;
  end
  else
    Result := Inherited ExtSelectClipRGN(dc, rgn, mode);
end;

{------------------------------------------------------------------------------
  Function: ExtTextOut
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.ExtTextOut(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
var
  WideStr: WideString;
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ExtTextOut]');
  {$endif}

  Result := False;

  if ((Options and (ETO_OPAQUE + ETO_CLIPPED)) <> 0) and (Rect = nil) then
    exit;

  if not IsValidDC(DC) then Exit;

   if ((Options and ETO_OPAQUE) <> 0) then
     QtDC.fillRect(Rect^.Left, Rect^.Top, Rect^.Right - Rect^.Left, Rect^.Bottom - Rect^.Top);

  if Str <> nil then
  begin
    if Count >= 0 then
      WideStr := GetUtf8String(Copy(Str, 1, Count))
    else
      WideStr := GetUtf8String(Str);

    QtDC.drawText(X, Y, @WideStr);
  end;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: FillRect
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH): Boolean;
begin
  result:=false;
  
  {$ifdef VerboseQtWinAPI}
    DebugLn('[WinAPI FillRect Rect=', dbgs(Rect),' Brush=', dbghex(Brush));
  {$endif}

  if not IsValidDC(DC) then
    exit;
  if not IsValidGdiObject(Brush) then
    exit;

  TQTDeviceContext(DC).fillRect(@Rect, TQTBrush(Brush).Widget);

  result := true;
end;

{------------------------------------------------------------------------------
  Function: FillRgn
  Params:  DC: HDC; RegionHnd: HRGN; hbr: HBRUSH
  Returns: Boolean

 ------------------------------------------------------------------------------}
function TQtWidgetSet.FillRgn(DC: HDC; RegionHnd: HRGN; hbr: HBRUSH): Bool;
var
  OldRgn: TQtRegion;
  R: TRect;
  hasClipping: Boolean;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    DebugLn('[WinAPI FillRgn Rgn=', dbgs(RegionHnd),' Brush=', dbghex(hbr));
  {$endif}

  Result := False;
  
  if not IsValidDC(DC) then exit;
  
  QtDC := TQtDeviceContext(DC);
  
  QtDC.save;
  OldRgn := TQtRegion.Create(True);
  try
    hasClipping := QPainter_hasClipping(QtDC.Widget);
    if hasClipping then
      QPainter_clipRegion(QtDC.Widget, OldRgn.Widget);
    if SelectClipRgn(DC, RegionHnd) <> ERROR then
    begin
      QRegion_boundingRect(TQtRegion(RegionHnd).Widget, @R);
      QtDC.fillRect(@R, TQtBrush(hbr).Widget);
      if hasClipping then
        SelectClipRgn(DC, HRGN(OldRgn));
      Result := True;
    end;
  finally
    OldRgn.Free;
    QtDC.restore;
  end;

end;

{------------------------------------------------------------------------------
  Function: Frame
  Params:  none
  Returns: Nothing

  Draws the border of a rectangle.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Frame(DC: HDC; const ARect: TRect): Integer;
begin
  Result := 0;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawRect(ARect.Left, ARect.Top,
   ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: Frame3D
  Params:  none
  Returns: Nothing

  Draws a 3d border in Qt native style.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Frame3d(DC : HDC; var ARect : TRect;
  const FrameWidth : integer; const Style : TBevelCut) : boolean;
var
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    DebugLn('[TQtWidgetSet.Frame3d Rect=', dbgs(ARect));
  {$endif}

  Result := False;
  
  if not IsValidDC(DC) then exit;

  QtDC := TQtDeviceContext(DC);

  case Style of
    bvNone: ;
    bvLowered: QtDC.qDrawWinPanel(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, nil, True, FrameWidth);
    bvRaised: QtDC.qDrawWinPanel(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, nil, False, FrameWidth);
    bvSpace: QtDC.qDrawPlainRect(ARect.Left, ARect.Top,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, nil, FrameWidth);
  end;

  if Style <> bvNone then
    InflateRect(ARect, -FrameWidth, -FrameWidth);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: FrameRect
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.FrameRect(DC: HDC; const ARect: TRect;
  hBr: HBRUSH): Integer;
begin
  Result := 0;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).qDrawPLainRect(ARect.Left, ARect.Top,
   ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);

  Result := 1;
end;

function TQtWidgetSet.GetActiveWindow: HWND;
var
  Widget: QWidgetH;
begin
  Widget := QApplication_activeWindow;
  if Widget <> nil then
    Result := HWND(QtObjectFromWidgetH(Widget))
  else
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

  Image := QImage_create(TQtImage(Bitmap).Handle);
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

function TQtWidgetSet.GetCapture: HWND;
var
  w: QWidgetH;
  Widget: TQtWidget;
begin
  w := QWidget_mouseGrabber();
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
end;

{------------------------------------------------------------------------------
  Function: GetClientBounds
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetClientBounds(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetClientBounds]');
  {$endif}

  ARect := TQtWidget(handle).getClientBounds;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetClientRect
  Params: handle:
          Result:
  Returns: true on success

  Returns the client bounds of a control. The client bounds is the rectangle of
  the inner area of a control, where the child controls are visible. The
  coordinates are relative to the control's left and top.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetClientRect(handle : HWND; var ARect : TRect) : Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetClientRect]');
  {$endif}

  ARect := TQtWidget(handle).getClientBounds;
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  //WriteLn('ClientRect: ', dbgsName(TQtWidget(handle).LCLObject), ' Width = ', ARect.Right, ' Height = ', ARect.Bottom);

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
function TQtWidgetSet.GetClipBox(DC: hDC; lpRect: PRect): Longint;
var
  ARegion: QRegionH;
begin
  Result := SIMPLEREGION;
  If lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);

  If not IsValidDC(DC) then
    Result := ERROR;

  if Result <> ERROR
  then with TQtDeviceContext(DC) do
  begin
    {$ifdef VerboseQtWinAPI}
      Write('TQtWidgetSet.GetClipBox FastClip=',
       ((vClipRect <> nil) and not vClipRectDirty) );
    {$endif}
    
    // the most correct way to get a clipbox if through
    // region.boundingrect, but it's slower.
    
    // TODO: remove "and false" below when vClipRectDirty is implemented
    //       it should be "true" when user set a custom clip rect
    //       and "false" on beginpaint
    if (vClipRect<>nil) and not vClipRectDirty and false then
      lpRect^ := vClipRect^
    else
    if QPainter_HasClipping(Widget) then
    begin
      ARegion := QRegion_Create;
      try
        QPainter_ClipRegion(Widget, ARegion);
        QRegion_boundingRect(ARegion, lpRect);
      finally
        QRegion_destroy(ARegion);
      end;
    end;
    {$ifdef VerboseQtWinAPI}
    WriteLn(' Rect=', dbgs(lprect^));
    {$endif}
  end;
end;

{------------------------------------------------------------------------------
  Function: GetClipRGN
  Params: dc, rgn
  Returns: Integer

  Returns a copy of the current Clipping Region.

  The result can be one of the following constants
     0 = no clipping set
     1 = ok
    -1 = error
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetClipRGN(DC : hDC; RGN : hRGN): Longint;
begin
  {$ifdef VerboseQtWinAPI}
    Write('Trace: [WinAPI GetClipRgn]',
     ' DC: ', dbghex(DC),
     ' RGN: ', dbghex(Rgn));
    if RGN<>0 then
      WriteLn(' QRegionH=', PtrInt(TQtRegion(Rgn).Widget))
    else
      WriteLn(' Rgn=0');
  {$endif}
  // it assumes that clipregion object has been created some other place
  Result := -1;
  if not IsValidDC(DC) then
    exit;
  if rgn=0 then
    exit;
  if not QPainter_HasClipping(TQtDeviceContext(DC).Widget) then
    Result := 0
  else begin
    QPainter_ClipRegion(TQtDeviceContext(DC).Widget, TQtRegion(Rgn).Widget);
    Result := 1;
  end;
end;

{------------------------------------------------------------------------------
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
end;

{------------------------------------------------------------------------------
  Function: GetDC
  Params:  hWnd is any widget.
  Returns: Nothing

  This function is Called:
  - Once on app startup with hWnd = 0
  - Twice for every TLabel on the TCustomLabel.CalcSize function
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDC(hWnd: HWND): HDC;
var
  Widget: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetDC] hWnd: ', dbghex(hWnd));
  {$endif}
  
  Widget := TQtWidget(hWnd);
  
  if Widget <> nil then
  begin
    Result := Widget.Context;
    if Result = 0 then
      Result := HDC(QtDefaultContext);
  end 
  else
  begin
    Result := HDC(QtScreenContext);
  end;
  
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetDC] Result: ', dbghex(Result));
  {$endif}
end;

function TQtWidgetSet.GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetDCOriginRelativeToWindow] missing implementation ');
  {$endif}
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
  w: QWidgetH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceCaps] DC ' + dbghex(DC));
  {$endif}

  Result := 0;
  if not IsValidDC(DC) then exit;
  
  QtDC := TQtDeviceContext(DC);
  
  if QtDC.Parent <> nil
  then
    w := QtDC.Parent
  else
    w := QApplication_desktop;
    
  PaintDevice := QWidget_to_QPaintDevice(w);
  
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
end;

function TQtWidgetSet.GetDIBits(DC: HDC; Bitmap: HBitmap; StartScan, NumScans: UINT; Bits: Pointer; var BitInfo: BitmapInfo; Usage: UINT): Integer;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetDIBits] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: GetFocus
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetFocus: HWND;
var
  WidgetH: QWidgetH;
  Obj: TQtWidget;
begin
  result:=0;
  WidgetH:=QApplication_FocusWidget();
  if WidgetH<>nil then
  begin
    Obj := QtObjectFromWidgetH(WidgetH);
    if Obj<>nil then
      result:=Hwnd(Obj);
    {$ifdef VerboseFocus}
    Write('TQtWidgetSet.GetFocus: WidgetH=',dbghex(ptrint(WidgetH)), ' QtWidget=', dbgsname(Obj));
    if Obj<>nil then
      WriteLn(' LclObject=', dbgsname(Obj.LCLObject))
    else
      WriteLn;
    {$endif}
  end;
end;

function TQtWidgetSet.GetKeyState(nVirtKey: Integer): Smallint;
const
  StateDown    = SmallInt($FF80);
  StateToggled = SmallInt($0001);
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
      if (QApplication_keyboardModifiers and QtMetaModifier) > 0 then
        Result := Result or StateDown;
    VK_SHIFT:
      if (QApplication_keyboardModifiers and QtShiftModifier) > 0 then
        Result := Result or StateDown;
    VK_CONTROL:
      if (QApplication_keyboardModifiers and QtControlModifier) > 0 then
        Result := Result or StateDown;
    else
      DebugLn('TQtWidgetSet.GetKeyState TODO ', DbgSVKCode(Word(nVirtkey)));
  end;
end;

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
var
  aObject: TObject;
  AFont: TQtFont absolute aObject;
  BitmapSection : TDIBSECTION;
  ALogFont: PLogFont absolute Buf;
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
    if BufSize = SizeOf(TLogFont) then
    begin
      Result := BufSize;

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
      ALogFont^.lfFaceName := UTF8Encode(AFont.getFamily);
    end;
  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else
  if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}
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
end;

function TQtWidgetSet.GetParent(Handle : HWND): HWND;
var
  QtWidget: TQtWidget;
  w: QWidgetH;
begin
	{$ifdef VerboseQtWinAPI}
  writeln('Trace:> [WinAPI GetParent] Handle: ' + dbghex(Handle));
  {$endif}
  
  QtWidget := TQtWidget(Handle);
  
  w := QWidget_parentWidget(QtWidget.Widget);
  
  Result := HWND(QtObjectFromWidgetH(w));
  
	{$ifdef VerboseQtWinAPI}
  writeln('Trace:< [WinAPI GetParent] : ' + dbghex(Result));
  {$endif}
end;

function TQtWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
begin
  if Handle<>0 then
    result := TQtWidget(Handle).Props[str]
  else
    result := nil;
end;

function TQtWidgetSet.GetRgnBox(RGN : HRGN; lpRect : PRect) : Longint;
var
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetRgnBox] possible wrong implementation ');
  {$endif}
  Result := SIMPLEREGION;
  If lpRect <> nil then
    lpRect^ := Rect(0,0,0,0);
  If Not IsValidGDIObject(RGN) then
    Result := ERROR
  else
  begin
    Result := TQtRegion(RGN).GetRegionType;
    If lpRect <> nil then
    begin
      QRegion_boundingRect(TQtRegion(RGN).Widget, @R);
      
      with lpRect^ do
      begin
        Left   := R.Left;
        Top    := R.Top;
        Right  := R.Left + R.Right;
        Bottom := R.Top + R.Bottom;
      end;
    end;
  end;
end;

function TQtWidgetSet.GetROP2(DC: HDC): Integer;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetROP2] missing implementation ');
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
    if BarKind = SM_CYVSCROLL then
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
    if BarKind = SM_CYVSCROLL then
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
      ScrollInfo.nPos := QtScrollBar.getValue;
      
    // RANGE
    if (ScrollInfo.fMask and SIF_RANGE) <> 0
    then begin
      ScrollInfo.nMin:= QtScrollBar.getMin;
      ScrollInfo.nMax:= QtScrollBar.getMax;
    end;
    // PAGE
    if (ScrollInfo.fMask and SIF_PAGE) <> 0 then
      ScrollInfo.nPage := QtScrollBar.getPageStep;
      
    // TRACKPOS
    if (ScrollInfo.fMask and SIF_TRACKPOS) <> 0 then
      ScrollInfo.nTrackPos := QtScrollBar.getSliderPosition;

    Result := True;
  end;
end;

function TQtWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetStockObject] Value: ', Value);
  {$endif}

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
    DEFAULT_GUI_FONT, SYSTEM_FONT:
      begin

        If FStockSystemFont <> 0 then
        begin
          DeleteObject(FStockSystemFont);
          FStockSystemFont := 0;
        end;

        If FStockSystemFont = 0 then
          FStockSystemFont := CreateDefaultFont;
        Result := FStockSystemFont;
      end;
      
  {$ifdef VerboseQtWinAPI}
    else
    WriteLn('[WinAPI GetStockObject] UNHANDLED Value: ', Value);
  {$endif}
  end;
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetStockObject] Value: ', Value);
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSysColor(nIndex: Integer): DWORD;

  function GetPaletteColor(Palette: QPaletteH; Group: QPaletteColorGroup; Role: QPaletteColorRole): TColor;
  var
    QColor : PQColor;
  begin
    QColor:=QPalette_color(Palette, Group, Role);
    Result:=(QColor^.r and $00FF) or ((QColor^.g and $00FF) shl 8) or ((QColor^.b and $00FF) shl 16);
  end;
  
  function GetClInfo(Bk: Boolean): TColor;
  var
    APalette: QPaletteH;
  begin
    APalette := QPalette_create();
    QToolTip_palette(APalette);
    if Bk then
      Result := GetPaletteColor(APalette, QPaletteActive, QPaletteWindow)
    else
      Result := GetPaletteColor(APalette, QPaletteActive, QPaletteWindowText);
    QPalette_destroy(APalette);
  end;

  {------------------------------------------------------------------------------
    Function: GetColor
    Params:   A Qt color group and a Qt color role
    Returns:  TColor

  ------------------------------------------------------------------------------}
  function GetColor(Group: QPaletteColorGroup; Role: QPaletteColorRole): TColor;
  var
    Handle : QPaletteH;
  begin
    Handle := QPalette_create;
    QApplication_palette(Handle);

    Result := GetPaletteColor(Handle, Group, Role);

    QPalette_destroy(Handle);
  end;

begin
  if (nIndex < 0) or (nIndex > MAX_SYS_COLORS) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:Unknown lcl system color: [TQtWidgetSet.GetSysColor]');
    {$endif}
    exit;
  end;
    
  case nIndex of
    COLOR_SCROLLBAR               : Result:=GetColor(QPaletteActive,   QPaletteMid);
    COLOR_BACKGROUND              : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_ACTIVECAPTION           : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_INACTIVECAPTION         : Result:=GetColor(QPaletteInActive, QPaletteBase);
    COLOR_MENU                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_WINDOW                  : Result:=GetColor(QPaletteInActive, QPaletteBase);
    COLOR_WINDOWFRAME             : Result:=GetColor(QPaletteActive,   QPaletteShadow);
    COLOR_MENUTEXT                : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
    COLOR_WINDOWTEXT              : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
    COLOR_CAPTIONTEXT             : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_ACTIVEBORDER            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_INACTIVEBORDER          : Result:=GetColor(QPaletteInactive, QPaletteWindow);
    COLOR_APPWORKSPACE            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_HIGHLIGHT               : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_HIGHLIGHTTEXT           : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
    COLOR_BTNFACE                 : Result:=GetColor(QPaletteActive,   QPaletteButton);
    COLOR_BTNSHADOW               : Result:=GetColor(QPaletteActive,   QPaletteDark);
    COLOR_GRAYTEXT                : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_BTNTEXT                 : Result:=GetColor(QPaletteActive,   QPaletteButtonText);
    COLOR_INACTIVECAPTIONTEXT     : Result:=GetColor(QPaletteInactive, QPaletteText);
    COLOR_BTNHIGHLIGHT            : Result:=GetColor(QPaletteActive,   QPaletteLight);
    COLOR_3DDKSHADOW              : Result:=GetColor(QPaletteActive,   QPaletteShadow);
    COLOR_3DLIGHT                 : Result:=GetColor(QPaletteActive,   QPaletteMidlight);
    COLOR_INFOTEXT                : Result:=GetClInfo(False);
    COLOR_INFOBK                  : Result:=GetClInfo(True);
    // PBD: 25 is unassigned in all the docs I can find
    //      if someone finds what this is supposed to be then fill it in
    //      note defaults below, and cl[ColorConst] in graphics
    COLOR_HOTLIGHT                : Result:=GetColor(QPaletteActive,   QPaletteLight);
    COLOR_GRADIENTACTIVECAPTION   : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_GRADIENTINACTIVECAPTION : Result:=GetColor(QPaletteInactive, QPaletteBase);
    COLOR_FORM                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);

    COLOR_clForeground..COLOR_clHighlightedText
                                  : Result:=GetColor(QPaletteActive,   nIndex - COLOR_clForeground);
    COLOR_clNormalForeground..COLOR_clNormalHighlightedText
                                  : Result:=GetColor(QPaletteInactive, nIndex - COLOR_clNormalForeground);
    COLOR_clDisabledForeground..COLOR_clDisabledHighlightedText
                                  : Result:=GetColor(QPaletteDisabled, nIndex - COLOR_clDisabledForeground);
    COLOR_clActiveForeground..COLOR_clActiveHighlightedText
                                  : Result:=GetColor(QPaletteActive,   nIndex - COLOR_clActiveForeground);
  else
    Result:=0;
  end;
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
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXBORDER         ');
      end;
    SM_CYBORDER:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYBORDER         ');
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
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
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
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    SM_CXHTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON,
    SM_CYICON:
      begin
        Result := 32;
      end;
    SM_CXICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMENUSIZE       ');
      end;
    SM_CYMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENUSIZE       ');
      end;
    SM_CXMIN:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
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
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
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
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
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
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        Assert(False, 'Trace:TODO: [TQtWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC     - A device context
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextColor(DC: HDC) : TColorRef;
var
  Color: TQColor;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextColor]');
  {$endif}

  Result := 0;
  
  if IsValidDC(DC) then
  begin
    QtDC := TQtDeviceContext(DC);
    ColorRefToTQColor(QtDC.vTextColor, Color);
    // QColor_setRgb(QColorH(@Color),Red(QtDC.vTextColor),Green(QtDC.vTextColor),Blue(QtDC.vTextColor));
    TQColorToColorRef(Color, Result);
  end;
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  WideStr: WideString;
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextExtentPoint]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  WideStr := GetUtf8String(Str);
  Size.cx := QtDC.font.Metrics.width(@WideStr, Count);
  Size.cy := QtDC.font.Metrics.height;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetTextMetrics
  Params:  DC     - A device context with a font selected
           TM     - The structure to receive the font information
  Returns: If successfull
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextMetrics(DC: HDC; var TM: TTextMetric): Boolean;
var
  QtFontMetrics: TQtFontMetrics;
  FontFamily: WideString;
  QtDC: TQtDeviceContext absolute DC;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextMetrics]');
  {$endif}

  Result := IsValidDC(DC);

  if Result then
  begin
    QtFontMetrics := QtDC.font.Metrics;
    TM.tmHeight := QtFontMetrics.height;
    TM.tmAscent := QtFontMetrics.ascent;
    TM.tmDescent := QtFontMetrics.descent;
    TM.tmInternalLeading := QtFontMetrics.leading;
    TM.tmExternalLeading := 0;
    TM.tmAveCharWidth := QtFontMetrics.charWidth('x', 0);
    TM.tmMaxCharWidth := QtFontMetrics.maxWidth;
    TM.tmWeight := QtDC.font.getWeight;
    TM.tmOverhang := 0;
    TM.tmDigitizedAspectX := 0;
    TM.tmDigitizedAspectY := 0;
    TM.tmFirstChar := 'a';
    TM.tmLastChar := 'z';
    TM.tmDefaultChar := 'x';
    TM.tmBreakChar := '?';
    TM.tmItalic := 0;
    TM.tmUnderlined := 0;
    TM.tmStruckOut := 0;
    
    QtDC.font.family(@FontFamily);
    
    { Defaults to a TrueType font.
      Note that the meaning of the FIXED_PITCH constant is the opposite of
      the name implies, according to MSDN docs. Just a small inconsistency
      on Windows API that we have to mimic. }
    if QtDC.font.fixedPitch then
      TM.tmPitchAndFamily := TRUETYPE_FONTTYPE
    else
      TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;

    TM.tmCharSet := EASTEUROPE_CHARSET;
  end;
end;

function TQtWidgetSet.GetWindowLong(Handle : hwnd; int: Integer): PtrInt;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetWindowLong] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  GetWindowOrgEx
  Params:  DC    -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowOrgEx(dc: hdc; P: PPoint): Integer;
var
  Matrix: QMatrixH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace: > [WinAPI GetWindowOrgEx]');
  {$endif}
  Result := 0;
  if not IsValidDC(DC) and (P<>nil) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace: < [WinAPI GetWindowOrgEx] No valid DC or P is nil');
    {$endif}
    exit;
  end;
    
  Matrix := QPainter_Matrix(TQtDeviceContext(DC).Widget);
  if Matrix <> nil then
  begin
    P^.X := -Trunc(QMatrix_Dx(Matrix));
    P^.Y := -Trunc(QMatrix_Dy(Matrix));
    result := 1;
  end;
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace: < [WinAPI GetWindowOrgEx] Result=', dbgs(p^));
  {$endif}
end;


{------------------------------------------------------------------------------
  Method:  GetWindowRect
  Params:  Handle - handle of window
           Rect   - record for window coordinates
  Returns: if the function succeeds, the return value is nonzero; if the
           function fails, the return value is zero

  Retrieves the dimensions of the bounding rectangle of the specified window.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowRect(Handle: HWND; var ARect: TRect): Integer;
var
  APos: TQtPoint;
  AParent: QWidgetH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRect]');
  {$endif}

  Result := 0;

{ // this code doesnot work sometimes - why? Maybe qt bug?
  // to test bug one can create 4 pannels with 0 sizes and aligned to differnt sides
  // then aligned to left and to right panel will return same value
  
  APos.x := 0;
  APos.y := 0;
  QWidget_mapToGlobal(TQtWidget(Handle).Widget, @APos, @APos);
}
  APos := TQtWidget(Handle).getPos;
  AParent := TQtWidget(Handle).getParent;
  if AParent <> nil then
    QWidget_mapToGlobal(AParent, @APos, @APos);

  ARect.Left := APos.x;
  ARect.Top := APos.y;
  ARect.Bottom := ARect.Top + TQtWidget(Handle).getHeight;
  ARect.Right := ARect.Left + TQtWidget(Handle).getWidth;

  Result := -1;
end;

{------------------------------------------------------------------------------
  Function: GetWindowRelativePosition
  Params:  Handle : HWND;
  Returns: true on success

  returns the current widget Left, Top, relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowRelativePosition(Handle: HWND; var Left, Top: integer): boolean;
var
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRelativePosition]');
  {$endif}

  R := TQtWidget(Handle).getFrameGeometry;
  Left := R.Left;
  Top := R.Top;
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: GetWindowSize
  Params:  Handle : hwnd;
  Returns: true on success

  Returns the current widget Width and Height
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetWindowSize(Handle: hwnd; var Width, Height: integer): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowSize]');
  {$endif}

  Result := False;

  Height := TQtWidget(Handle).getHeight;
  Width := TQtWidget(Handle).getWidth;

  Result := True;

  // Here we should convert top level lcl window coordinaties to qt coord
  // Due to borders and etc
  // ?
end;

{------------------------------------------------------------------------------
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

  function DoFillTriangle : Boolean;
  begin
    Result := (Mode and GRADIENT_FILL_TRIANGLE) = GRADIENT_FILL_TRIANGLE;
  end;

  function DoFillVRect : Boolean;
  begin
    Result := (Mode and GRADIENT_FILL_RECT_V) = GRADIENT_FILL_RECT_V;
  end;

  procedure GetGradientBrush(BeginColor, EndColor : TColorRef; Position,
    TotalSteps : Longint; var GradientBrush : hBrush);
  var
    R1, G1, B1 : Integer;
    R2, G2, B2 : Integer;
    NewBrush : TLogBrush;
  begin
    GetRGBIntValues(BeginColor,R1,G1,B1);
    GetRGBIntValues(EndColor,R2,G2,B2);

    R1 := R1 + (Position*(R2 - R1) div TotalSteps);
    G1 := G1 + (Position*(G2 - G1) div TotalSteps);
    B1 := B1 + (Position*(B2 - B1) div TotalSteps);

    with NewBrush do
    begin
      lbStyle := BS_SOLID;
      lbColor := RGB(R1,G1,B1);
    end;

    if GradientBrush <> 0 then
      LCLIntf.DeleteObject(GradientBrush);
    GradientBrush := LCLIntf.CreateBrushIndirect(NewBrush);
  end;

  function FillTriMesh(Mesh : tagGradientTriangle) : Boolean;
  {var
    V1, V2, V3 : tagTRIVERTEX;
    C1, C2, C3 : TColorRef;
  begin
    With Mesh do begin
      Result := (Vertex1 < NumVertices) and (Vertex2 >= 0) and
        (Vertex2 < NumVertices) and (Vertex2 >= 0) and
        (Vertex3 < NumVertices) and (Vertex3 >= 0);

      If (Vertex1 = Vertex2) or (Vertex1 = Vertex3) or
        (Vertex2 = Vertex3) or not Result
      then
        exit;

      V1 := Vertices[Vertex1];
      V2 := Vertices[Vertex2];
      V3 := Vertices[Vertex3];

      //Check to make sure they are in reasonable positions..

      //then what??
    end;}
  begin
    Result := False;
  end;

  function FillRectMesh(Mesh : tagGradientRect) : boolean;
  var
    TL,BR : tagTRIVERTEX;
    StartColor, EndColor : TColorRef;
    I, Swap : Longint;
    SwapColors : Boolean;
    UseBrush : hBrush;
    Steps, MaxSteps : Longint;
  begin
    with Mesh do
    begin
      Result := (UpperLeft < NumVertices) and (UpperLeft >= 0) and
        (LowerRight < NumVertices) and (LowerRight >= 0);
      if (LowerRight = UpperLeft) or not Result then
        exit;
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
      StartColor := RGB(TL.Red, TL.Green, TL.Blue);
      EndColor := RGB(BR.Red, BR.Green, BR.Blue);
      if SwapColors then
      begin
        Swap := StartColor;
        StartColor := EndColor;
        EndColor := Swap;
      end;
      UseBrush := 0;
      MaxSteps := GetDeviceCaps(DC, BITSPIXEL);
      if MaxSteps >= 4 then
        MaxSteps := Floor(Power(2, MaxSteps))
      else
        MaxSteps := 256;
      if DoFillVRect then
      begin
        Steps := Min(BR.Y - TL.Y, MaxSteps);
        for I := 0 to Steps - 1 do
        begin
          GetGradientBrush(StartColor, EndColor, I, Steps, UseBrush);
            LCLIntf.FillRect(DC, Rect(TL.X, TL.Y + I, BR.X, TL.Y + I + 1),
              UseBrush);
        end;
      end
      else
      begin
        Steps := Min(BR.X - TL.X, MaxSteps);
        for i := 0 to  Steps - 1 do
        begin
          GetGradientBrush(StartColor, EndColor, I, Steps, UseBrush);
          LCLIntf.FillRect(DC, Rect(TL.X + I, TL.Y, TL.X + I + 1, BR.Y),
            UseBrush);
        end;
      end;
      If UseBrush <> 0 then
        LCLIntf.DeleteObject(UseBrush);
    end;
  end;

const
  MeshSize: Array[Boolean] of Integer = (
    SizeOf(tagGradientRect), SizeOf(tagGradientTriangle));
var
  i : Integer;
begin
//  {$ifdef VerboseQtWinAPI}
    WriteLn('***** [WinAPI TQtWidgetSet.GradientFill] ');
//  {$endif}

  //Currently Alpha blending is ignored... Ideas anyone?
  Result := (Meshes <> nil) and (NumMeshes >= 1) and (NumVertices >= 2)
            and (Vertices <> nil);
  if Result and DoFillTriangle then
    Result := NumVertices >= 3;
  if Result then
  begin
    Result := False;

    //Sanity Checks For Vertices Size vs. Count
    if MemSize(Vertices) < PtrInt(SizeOf(tagTRIVERTEX)*NumVertices) then
      exit;

    //Sanity Checks For Meshes Size vs. Count
    if MemSize(Meshes) < PtrInt(MeshSize[DoFillTriangle]*NumMeshes) then
      exit;

    for I := 0 to NumMeshes - 1 do
    begin
      if DoFillTriangle then
      begin
        If Not FillTriMesh(PGradientTriangle(Meshes)[I]) then
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
end;

{------------------------------------------------------------------------------
  Procedure: InitializeCriticalSection
  Params: var CritSection: TCriticalSection
  Returns:
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.InitializeCriticalSection(var CritSection: TCriticalSection);
var
  ACritSec: System.PRTLCriticalSection;
begin
  New(ACritSec);
  System.InitCriticalSection(ACritSec^);
  CritSection:=TCriticalSection(ACritSec);
end;

function TQtWidgetSet.IntersectClipRect(dc: hdc; Left, Top, Right, Bottom: Integer): Integer;
var
  QtDC: TQtDeviceContext absolute dc;
  IntersectRgn, Rgn: QRegionH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('***** [WinAPI TQtWidgetSet.IntersectClipRect] ');
  {$endif}
  Result := ERROR;
  if not IsValidDC(DC) then exit;

  IntersectRgn := QRegion_create(Left, Top, Right - Left, Bottom - Top);
  Rgn := QRegion_create;
  try
    if QtDC.getClipping then
    begin
      QPainter_clipRegion(QtDC.Widget, Rgn);
      if QRegion_isEmpty(Rgn) then
        QtDC.setClipRegion(IntersectRgn)
      else
        QtDC.setClipRegion(IntersectRgn, QtIntersectClip);
    end
    else
    begin
      QtDC.setClipRegion(InterSectRgn);
      QPainter_clipRegion(QtDC.Widget, Rgn);
    end;
    QtDC.setClipping(True);
    Result := QtDC.GetRegionType(Rgn);
  finally
    QRegion_destroy(IntersectRgn);
    QRegion_destroy(Rgn);
  end;
end;

function TQtWidgetSet.IsWindowEnabled(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).getEnabled;
end;

function TQtWidgetSet.IsWindowVisible(Handle: HWND): boolean;
begin
  Result := TQtWidget(Handle).getVisible;
end;

{------------------------------------------------------------------------------
  Function: InvalidateRect
  Params: aHandle:
          Rect:
          bErase:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.InvalidateRect(aHandle: HWND; Rect: pRect; bErase: Boolean): Boolean;
var
  R: TRect;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Invalidate Rect]');
  {$endif}
  
  if Rect <> nil then
  begin
    R := TQtWidget(aHandle).getClientBounds;
    OffsetRect(Rect^, R.Left, R.Top);
    // no need to handle bErase. Qt automatically erase rect on paint event according to docs
    TQtWidget(aHandle).Update(Rect);
  end else
    TQtWidget(aHandle).Update;

  Result := True;
end;

{------------------------------------------------------------------------------
  Procedure: LeaveCriticalSection
  Params:  var CritSection: TCriticalSection
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TQtWidgetSet.LeaveCriticalSection(var CritSection: TCriticalSection);
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
function TQtWidgetSet.LineTo(DC: HDC; X, Y: Integer): Boolean;
var
  PenPos: TPoint;
  CX: Integer;
  CY: Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI LineTo]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).getPenPos(@PenPos);

  CX := X;
  CY := Y;
  
  if (CX <> PenPos.X) and (CX > 0) and (CX > PenPos.X) then
    CX := CX - 1;
  
  if (CY <> PenPos.Y) and (CY > 0) and (CY > PenPos.Y) then
    CY := CY - 1;
  
  TQtDeviceContext(DC).drawLine(PenPos.X, PenPos.Y, CX, CY);

  MoveToEx(DC, CX, CY, nil);
  
  Result := True;
end;


function TQtWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal): integer;
var
  Str: WideString;
  TitleStr: WideString;
  OkStr: WideString;
begin
  Str := GetUtf8String('TQtWidgetSet.MessageBox - not implemented');
  TitleStr := GetUtf8String(lpCaption);
  OkStr := GetUtf8String('Ok');
  Result := QMessageBox_information(TQtWidget(hWnd).Widget, @Str, @TitleStr, @OkStr);
end;

{------------------------------------------------------------------------------
  Function: MoveToEx
  Params:  none
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TQtWidgetSet.MoveToEx(DC: HDC; X, Y: Integer; OldPoint: PPoint): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI MoveToEx]',
     ' DC:', dbghex(DC),
     ' X:', dbgs(X),
     ' Y:', dbgs(Y));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if (OldPoint <> nil) then TQtDeviceContext(DC).getPenPos(OldPoint);

  TQtDeviceContext(DC).setPenPos(X, Y);
  
  Result := True;
end;

function TQtWidgetSet.PeekMessage(var lpMsg : TMsg; Handle : HWND; wMsgFilterMin, wMsgFilterMax,wRemoveMsg : UINT): Boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.PeekMessage] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: PolyBezier
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Filled: Boolean;
           Continuous: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.PolyBezier(DC: HDC; Points: PPoint; NumPts: Integer;
  Filled, Continuous: Boolean): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI PolyBezier] DC: ', dbghex(DC));
  {$endif}
  Result := inherited PolyBezier(DC, Points, NumPts, Filled, Continuous);
end;

{------------------------------------------------------------------------------
  Function: Polygon
  Params:  DC: HDC; Points: PPoint; NumPts: Integer; Winding: Boolean
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Polygon(DC: HDC; Points: PPoint; NumPts: Integer;
  Winding: Boolean): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Polygon] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC);
  if Result then
  begin
    {TODO: discuss with other developers about antialiasing by default}
    // QPainter_setRenderHint(TQtDeviceContext(DC).Widget, QPainterAntialiasing, True);
    if Winding
    then
      QPainter_drawPolygon(TQtDeviceContext(DC).Widget, PQtPoint(Points), NumPts, QtWindingFill)
    else
      QPainter_drawPolygon(TQtDeviceContext(DC).Widget, PQtPoint(Points), NumPts, QtOddEvenFill);
  end;
end;

{------------------------------------------------------------------------------
  Function: Polyline
  Params:  DC: HDC; Points: PPoint; NumPts: Integer
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Polyline(DC: HDC; Points: PPoint; NumPts: Integer): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Polyline] DC: ', dbghex(DC));
  {$endif}
  Result := IsValidDC(DC);
  if Result then
    QPainter_drawPolyline(TQtDeviceContext(DC).Widget, PQtPoint(Points), NumPts);
end;

function TQtWidgetSet.PostMessage(Handle: HWND; Msg: Cardinal; wParam: WParam; lParam: LParam): Boolean;
var
  Widget: TQtWidget absolute Handle;
  Event: QLCLMessageEventH;
begin
  Result := False;
  if Handle <> 0 then
  begin
    Event := QLCLMessageEvent_create(QEventLCLMessage, Msg, wParam, lParam, 0);
    QCoreApplication_postEvent(Widget.Widget, Event);
    Result := True;
  end;
end;
{------------------------------------------------------------------------------
  Function: Rectangle
  Params:  DC: HDC; X1, Y1, X2, Y2: Integer
  Returns: Nothing

  The Rectangle function draws a rectangle. The rectangle is outlined by using
  the current pen and filled by using the current brush.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): Boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI Rectangle] DC: ', dbghex(DC));
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  TQtDeviceContext(DC).drawRect(X1, Y1, X2 - X1 - 1, Y2 - Y1 - 1);
  
  Result := True;
end;

function TQtWidgetSet.RectVisible(dc : hdc; const ARect: TRect) : Boolean;
var
  w: QWidgetH;
  Region: QRegionH;
begin
  {$ifdef VerboseQtWinAPI}
  writeln('[WinAPI RectVisible] ');
  {$endif}
  Result := False;
  if not IsValidDC(DC) then Exit;
  w := TQtDeviceContext(DC).Parent;
  if w <> nil then
  begin
    if QWidget_isVisible(w) then
    begin
      Region := QRegion_create;
      try
        QWidget_visibleRegion(w, Region);
        Result := QRegion_contains(Region, PRect(@ARect));
      finally
        QRegion_destroy(Region);
      end;
    end;
  end;
end;

function TQtWidgetSet.ReleaseCapture: Boolean;
var
  w: TQtWidget;
begin
  w := TQtWidget(GetCapture);
  Result := w <> nil;
  if Result then
    w.releaseMouse();
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
end;


{------------------------------------------------------------------------------
  Function: RestoreDC: Restore a previously saved DC state
  Params:
    DC: Handle to a DeviceContext
    SavedDC: Index of saved state that needs to be restored
  Returns: True if state was successfuly restored.
-------------------------------------------------------------------------------}
function TQtWidgetSet.RestoreDC(DC: HDC; SavedDC: Integer): Boolean;
var
  DCData: PQtDCData;
begin
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:> [WinAPI RestoreDC] DC=', dbghex(DC),' SavedDC=',SavedDC);
  {$Endif}
  // if SavedDC is positive, it represents the wished saved dc instance
  // if SavedDC is negative, it's a relative number from last pushed state
  Result := False;
  if SavedDCList=nil then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI RestoreDC] there is no List yet, result=', result);
    {$Endif}
    exit;
  end;

  if SavedDC<0 then
    SavedDC := SavedDC+SavedDCList.Count;

  // check index
  Result := (SavedDC > 0) and (SavedDC < SavedDCList.Count);
  if Result then
  begin
    Result := true;
    while SavedDC > 0 do
    begin
      DCData := PQtDcData(SavedDCList[SavedDC]);
      SavedDCList.Delete(SavedDC);
      Result := TQtDeviceContext(DC).RestoreDCData(DCData);
      Dec(SavedDC);
    end;
  end;
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI RestoreDC]');
  {$Endif}
end;

function TQtWidgetSet.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
begin
  Result := False;
  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI RoundRect] DC Invalid, result=', result);
    {$Endif}
    exit;
  end;

  TQtDeviceContext(Dc).drawRoundRect(X1, Y1, X2 - X1 - 1, Y2 - Y1 - 1, RX, RY);
  Result := True;
end;
{------------------------------------------------------------------------------
  Function: SaveDC: save DC state information to a stack
  Params:  DC
  Returns: The index assigned to the or 0 if DC is not valid
-------------------------------------------------------------------------------}
function TQtWidgetSet.SaveDC(DC: HDC): Integer;
var
  DCData: PQtDCData;
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

  if SavedDCList=nil then
  begin
    SavedDCList := TList.Create;
    SavedDCList.Add(nil); // start at index 1, 0 is an invalid saved state
  end;
  
  DCData := TQtDeviceContext(DC).CreateDCData;
  Result := 1;
  SavedDCList.Insert(Result, DCData);
  
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI SaveDC] result=', Result);
  {$Endif}
end;

{------------------------------------------------------------------------------
  Function: ScreenToClient
  Params:  Handle: HWND; var P: TPoint
  Returns:
-------------------------------------------------------------------------------}
function TQtWidgetSet.ScreenToClient(Handle : HWND; var P : TPoint) : Integer;
var
  APoint: TQtPoint;
  R: TRect;
begin
  Result := 0;
  if Handle <> 0 then
  begin
    APoint.x := P.X;
    APoint.y := P.Y;
    QWidget_mapFromGlobal(TQtWidget(Handle).Widget, @APoint, @APoint);
    P.X := APoint.x;
    P.Y := APoint.y;
    R := TQtWidget(Handle).getClientBounds;
    dec(P.X, R.Left);
    dec(P.Y, R.Top);
    Result := 1;
  end;
end;

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
function TQtWidgetSet.SelectClipRGN(DC: hDC; RGN: HRGN): Longint;
var
  ARegion: QRegionH;
  EmptyRegion: QRegionH;
  Painter: QPainterH;
  R: TRect;
begin
  Result := ERROR;
  if IsValidDC(DC) then
  begin
    Painter := TQtDeviceContext(DC).Widget;
    if RGN <> 0 then
      QPainter_setClipRegion(Painter, TQtRegion(Rgn).Widget)
    else
    begin
      EmptyRegion := QRegion_create;
      try
        QPainter_setClipRegion(Painter, EmptyRegion, QtNoClip);
      finally
        QRegion_destroy(EmptyRegion);
      end;
    end;
    if QPainter_hasClipping(Painter) then
    begin
      ARegion := QRegion_Create;
      try
        QPainter_ClipRegion(Painter, ARegion);
        if QRegion_isEmpty(ARegion) then
          Result := NULLREGION
        else
        begin
          QRegion_boundingRect(ARegion, @R);
          if QRegion_contains(ARegion, PRect(@R)) then
            Result := SIMPLEREGION
          else
           Result := COMPLEXREGION;
        end;
      finally
        QRegion_Destroy(ARegion);
      end;
    end else
      Result := NULLREGION;
  end;
end;

{------------------------------------------------------------------------------
  Function: SelectObject
  Params:  none
  Returns: The GDI object of the same type previously associated with the DC

  Changes one of the GDI objects (Font, Brush, etc) of a Device Context;
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SelectObject(DC: HDC; GDIObj: HGDIOBJ): HGDIOBJ;
var
  aObject: TObject;
  {$ifdef VerboseQtWinAPI}
    ObjType: string;
  {$endif}
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI SelectObject]',
    ' DC=', dbghex(DC),
    ' GDIObj=', dbghex(GDIObj));
  {$endif}

  Result := 0;
  
  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SelectObject] Invalid DC');
    {$endif}

    Exit;
  end;
  
  if not IsValidGDIObject(GDIObj) then
  begin
    {$ifdef VerboseQtWinAPI}
      WriteLn('Trace:< [WinAPI SelectObject] Invalid GDI Object');
    {$endif}

    Exit;
  end;

  aObject := TObject(GDIObj);
  
  if aObject is TQtFont then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}
    
    Result := HGDIOBJ(TQtDeviceContext(DC).font);
    
    TQtDeviceContext(DC).setFont(TQtFont(aObject));
  end
  else if aObject is TQtPen then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Pen'      ;
    {$endif}
    result := HGDIOBJ(TQtDeviceContext(DC).pen);
    
    TQtDeviceContext(DC).setPen(TQtPen(aObject));
  end
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}
    
    Result := HGDIOBJ(TQtDeviceContext(DC).brush);
    
    TQtDeviceContext(DC).setBrush(TQtBrush(aObject));
  end
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    Result := HGDIOBJ(TQtDeviceContext(DC).vImage);
    
    // TODO: is this also saved in qpainter_save?
    TQtDeviceContext(DC).setImage(TQtImage(aObject));
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI SelectObject] Result=', dbghex(Result), ' ObjectType=', ObjType);
  {$endif}
end;

function TQtWidgetSet.SendMessage(HandleWnd: HWND; Msg: Cardinal;
  WParam: WParam; LParam: LParam): LResult;
var
  Widget: TQtWidget absolute HandleWnd;
  Event: QLCLMessageEventH;
begin
  Result := 0;
  if HandleWnd <> 0 then
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
  
  result := TQtDeviceContext(DC).SetBkColor(Color);
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

  result := TQtDeviceContext(DC).SetBkMode(bkMode);
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
      TQtWidget(AHandle).grabMouse();
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI SetCapture] Capture = ', Result, ' New capture = ', AHandle);
    {$endif}
    if Result <> 0 then
    begin
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
  Params: ACursor - HCursor (QCursorH)
  Returns:
       previous global cursor
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetCursor(ACursor: HCURSOR): HCURSOR;
begin
  Result := hCursor(QApplication_overrideCursor());
  if Result = ACursor then exit;
  if Screen.Cursors[crDefault] = ACursor then
  begin
    QApplication_restoreOverrideCursor();
  end else
  begin
    if Result = 0 then
      QApplication_setOverrideCursor(QCursorH(ACursor)) else
      QApplication_changeOverrideCursor(QCursorH(ACursor));
  end;
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
end;

{------------------------------------------------------------------------------
  Function: SetFocus
  Params: hWnd   - Window handle to be focused
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetFocus(hWnd: HWND): HWND;
begin
  if hwnd<>0 then
  begin
    {$ifdef VerboseFocus}
    WriteLn('*********  TQtWidgetSet.SetFocus INIT focusing ', TQtWidget(hwnd).lclobject.name);
    {$endif}
    result := GetFocus;
    TQtWidget(hWnd).setFocus;
    {$ifdef VerboseFocus}
    DebugLn('********* TQtWidgetSet.SetFocus END was %x now is %x',[result,hwnd]);
    {$endif}
  end;
end;

function TQtWidgetSet.SetForegroundWindow(HWnd: HWND): boolean;
begin
  TQtWidget(HWnd).Activate;
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  SetWindowOrgEx
  Params:  DC    - handle of device context
           NewX  - new x-coordinate of window origin
           NewY  - new y-coordinate of window origin
           Point - record receiving original origin
  Returns: Whether the call was successful

  Sets the window origin of the device context by using the specified coordinates.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetWindowOrgEx(DC : HDC; NewX, NewY : Integer; OldPoint: PPoint) : Boolean;
var
  P: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowOrgEx] DC: ', dbghex(DC), ' NewX: ', dbgs(NewX), ' NewY: ', dbgs(NewY));
  {$endif}

  Result := False;

  if IsValidDC(DC) then
  begin
    GetWindowOrgEx(DC, @P);
    // restore 0, 0
    if (P.X <> 0) or (P.Y <> 0) then
      TQtDeviceContext(DC).translate(P.X, P.Y);
    if OldPoint <> nil then
      OldPoint^ := P;
    TQtDeviceContext(DC).translate(-NewX, -NewY);
  end;

  Result := True;
end;

function TQtWidgetSet.ShowCaret(hWnd: HWND): Boolean;
begin
  Result := (hWnd <> 0) and (QtCaret.ShowCaret(TQtWidget(hWnd)));
end;

{------------------------------------------------------------------------------
  Method:  SetProp
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetProp(Handle: hwnd; Str: PChar; Data: Pointer): Boolean;
begin
  if Handle<>0 then
  begin
    TQtWidget(Handle).Props[str] := Data;
    result:=(TQtWidget(Handle).Props[str]=Data);
    {$ifdef VerboseQT}
    DebugLn('[WinAPI SetProp win=%s str=%s data=%x',[dbgsname(TQtWidget(Handle)), str, ptrint(data)]);
    {$endif}
  end else
    result:=False;
end;


{------------------------------------------------------------------------------
  Function: SetScrollInfo
  Params:  none
  Returns: The old position value


 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetScrollInfo(Handle : HWND; SBStyle : Integer;
  ScrollInfo: TScrollInfo; bRedraw : Boolean): Integer;
var
  Control: TWinControl;
  ScrollBar: TQtScrollBar;

  function UpdateScrollInfo: Integer;
  var
    iReCountMax: Integer;
  begin
    Result := 0;

    if (ScrollInfo.FMask and SIF_RANGE) <> 0 then
    begin
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
        ScrollBar.setPageStep(ScrollInfo.nPage);
    end;

    if (ScrollInfo.FMask and SIF_POS) <> 0 then
    begin
      if (ScrollInfo.nPos < ScrollBar.getMin) then
        ScrollInfo.nPos := ScrollBar.getMin
      else
      if (ScrollInfo.nPos > ScrollBar.getMax) then
        ScrollInfo.nPos := ScrollBar.getMax;

      ScrollBar.setValue(ScrollInfo.nPos);
    end;

    if (ScrollInfo.FMask and SIF_TRACKPOS) <> 0 then
    begin
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
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState)
      or (csDestroying in TQtWidget(Handle).LCLObject.ComponentState)
      then
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
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState)
      or (csDestroying in TQtWidget(Handle).LCLObject.ComponentState)
      then
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
end;

{------------------------------------------------------------------------------
  Method:  SetTextColor
  Params:  Handle -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetTextColor(DC: HDC; Color: TColorRef): TColorRef;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetTextColor] DC: ', dbghex(DC));
  {$endif}
  result := CLR_INVALID;
  if not IsValidDC(DC) then begin
    {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetTextColor] Invalid DC');
    {$endif}
    exit;
  end;
  result := TQtDeviceContext(DC).vTextColor;
  TQtDeviceContext(DC).vTextColor := ColorToRGB(Color); // be sure we get TColorRef
end;

{------------------------------------------------------------------------------
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
          QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH(ScrollArea.Widget), QtScrollBarAlwaysOff);
      end;
      
      SB_VERT:
      begin
        if bShow then
          ScrollArea.setScrollStyle(ssVertical)
        else
          QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH(ScrollArea.Widget), QtScrollBarAlwaysOff);
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
    WriteLn('[WinAPI ShowWindow]');
  {$endif}

  Result := False;
  
  Widget := TQTWidget(hWnd);

  if Widget <> nil then
  begin
    case nCmdShow of
      SW_SHOW: Widget.setVisible(True);
      SW_SHOWNORMAL: Widget.ShowNormal;
      SW_MINIMIZE: Widget.setWindowState(QtWindowMinimized);
      SW_SHOWMINIMIZED: Widget.ShowMinimized;
      SW_SHOWMAXIMIZED: Widget.ShowMaximized;
      SW_HIDE: TQTWidget(hWnd).setVisible(False);
    end;
    Result := True;
  end;
end;

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
function TQtWidgetSet.StretchBlt(DestDC: HDC; X, Y, Width, Height: Integer;
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
function TQtWidgetSet.StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer;
  SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP;
  XMask, YMask: Integer; Rop: DWORD): Boolean;
var
  SrcQDC: TQtDeviceContext absolute SrcDC;
  DstQDC: TQtDeviceContext absolute DestDC;
  SrcRect, DstRect, MaskRect: TRect;
  SrcWidthOrig, SrcHeightOrig: Integer;
  Image, QMask: QImageH;
  SrcMatrix: QMatrixH;
  dx, dy: integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI StretchMaskBlt]',
     ' DestDC:', dbghex(DestDC),
     ' SrcDC:', dbghex(SrcDC),
     ' Image:', dbghex(PtrInt(Image)),
     ' X:', dbgs(X), ' Y:', dbgs(Y),
     ' W:', dbgs(Width), ' H:', dbgs(Height),
     ' XSrc:', dbgs(XSrc), ' YSrc:', dbgs(YSrc),
     ' WSrc:', dbgs(SrcWidth), ' HSrc:', dbgs(SrcHeight));
  {$endif}

  SrcMatrix := QPainter_Matrix(SrcQDC.Widget);
  Image := SrcQDC.vImage.Handle;
  SrcWidthOrig := QImage_width(Image);
  SrcHeightOrig := QImage_height(Image);
  QMatrix_map(SrcMatrix, XSrc, YSrc, @XSrc, @YSrc);
  // our map can have some transformations
  if XSrc < 0 then // we cannot draw from negative coord, so we will draw from zero with shift
  begin
    dx := -XSrc;
    XSrc := 0;
  end
  else
    dx := 0;

  if YSrc < 0 then
  begin
    dy := -YSrc;
    YSrc := 0;
  end
  else
    dy := 0;

  // if passed source Width/Height is more than original then
  // qt skip that, so for that case we should transform size here
  //
  // stretch_factor := Width / SrcWidth;
  // Width := stretch_factor * (SrcWidthOrig - XSrc);
  if (SrcWidthOrig - XSrc < SrcWidth) then
    Width := Width * (SrcWidthOrig - XSrc) div SrcWidth;

  if (SrcHeightOrig - YSrc < SrcHeight) then
    Height := Height * (SrcHeightOrig - YSrc) div SrcHeight;

  if dx <> 0 then // apply shifts
  begin
    inc(X, dx);        // shift destination
    dec(Width, dx);    // substract width
    dec(SrcWidth, dx); // and dont forget about SrcWidth or we will get unneeded stretching
  end;
  if dy <> 0 then
  begin
    inc(Y, dy);
    dec(Height, dy);
    dec(SrcWidth, dy);
  end;
  
  DstRect := Bounds(X, Y, Width, Height);
  SrcRect := Bounds(XSrc, YSrc, SrcWidth, SrcHeight);
  MaskRect := Bounds(XMask, YMask, SrcWidth, SrcHeight);
  // #0011187 - makes painting wrong
  //DstQDC.CorrectCoordinates(DstRect);
  //DstQDC.CorrectCoordinates(SrcRect);
  //DstQDC.CorrectCoordinates(MaskRect);
  if Mask <> 0 then
    QMask := TQtImage(Mask).Handle
  else
    QMask := nil;
  DstQDC.drawImage(@DstRect, Image, @SrcRect, QMask, @MaskRect);

  Result := True;
end;

{------------------------------------------------------------------------------
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
  else
    Result := False;
  end
end;

{------------------------------------------------------------------------------
  Function: TextOut
  Params: DC:
          X:
          Y:
          Str:
          Count:
  Returns:

 ------------------------------------------------------------------------------}
function TQtWidgetSet.TextOut(DC: HDC; X,Y : Integer; Str : PChar; Count: Integer) : Boolean;
var
  WideStr: WideString;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI TextOut]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  if Count >= 0 then
    WideStr := GetUtf8String(Copy(Str, 1, Count))
  else
    WideStr := GetUtf8String(Str);

  TQtDeviceContext(DC).drawText(X, Y, @WideStr);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  UpdateWindow
  Params:  Handle
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.UpdateWindow(Handle: HWND): Boolean;
begin
 {$ifdef VerboseQtWinAPI}
   WriteLn('[WinAPI UpdateWindow]');
 {$endif}
  TQtWidget(Handle).Update;
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  WindowFromPoint
  Params:  Point -
  Returns:
 ------------------------------------------------------------------------------}
function TQtWidgetSet.WindowFromPoint(Point: TPoint): HWND;
var
  Widget: QWidgetH;
  APoint: TQtPoint;
begin
  APoint.x := Point.x;
  APoint.y := Point.y;
  Widget := QApplication_widgetAt(@APoint);
  if Widget <> nil then
  begin
    Widget := QWidget_window(Widget);
    Result := HWND(QtObjectFromWidgetH(Widget));
  end else
    Result := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
