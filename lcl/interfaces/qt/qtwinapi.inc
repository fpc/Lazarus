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

const
  ClipbBoardTypeToQtClipboard: array[TClipboardType] of QClipboardMode =
  (
{ctPrimarySelection  } QClipboardSelection,
{ctSecondarySelection} QClipboardSelection,
{ctClipboard         } QClipboardClipboard
  );

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
  if Result
  then
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
  if Result
  then
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

{  if IsDoubleBuffered then
    Result :=GetDoubleBufferedDC(Handle)
  else}
  
  DC := TQtDeviceContext.Create(Handle, True);
  PS.hdc := HDC(DC);
  
  if Handle<>0 then
  begin
    // if current handle has paintdata information,
    // setup hdc with it
    //DC.DebugClipRect('BeginPaint: Before');
    Widget := TQtWidget(Handle);
    if Widget.PaintData.ClipRegion<>nil then
    begin
      //Write('>>> Setting Paint ClipRegion: ');
      //DebugRegion('PaintData.ClipRegion: ', Widget.PaintData.ClipRegion);
      QPainter_SetClipRegion(DC.Widget, Widget.PaintData.ClipRegion);
    end;
    if Widget.PaintData.ClipRect<>nil then
    begin
      New(DC.vClipRect);
      DC.vClipRect^ := Widget.PaintData.ClipRect^;
    end;
    //DC.DebugClipRect('BeginPaint: After');
    // TODO: ask what is this good for
    if Widget is TQtMainWindow then
      TQtMainWindow(Widget).Canvas := DC;
  end;

  Result := PS.hdc;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI BeginPaint] Result=', dbghex(Result));
  {$endif}
end;

function TQtWidgetSet.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD): Boolean;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.BitBlt] missing implementation ');
  {$endif}
  Result := False;
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
  if FClipBoardFormats.Count > FormatID then
    Result := FClipBoardFormats[FormatID];
end;

function TQtWidgetSet.ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
  	WriteLn('***** [WinAPI TQtWidgetSet.ClipboardGetData] missing implementation ');
  {$endif}
  
 {$ifdef VerboseQtWinAPI_DEBUG_CLIPBOARD}
   writeln('DEBUG TQtWidgetSet.ClipboardGetData() ');
 {$endif}
 
  Result := True;
end;

function TQtWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;
  
var
  QtClipBoard: QClipBoardH;
  QtMimeData: QMimeDataH;
  QtList: QStringListH;
  i: Integer;
  Str: WideString;
begin
  Result := False;
  Count := 0;
  List := nil;

  QtClipBoard := QApplication_clipBoard;
  QtMimeData := QClipboard_mimeData(QtClipBoard, ClipbBoardTypeToQtClipboard[ClipBoardType]);

  QtList := QStringList_create;
  QMimeData_formats(QtMimeData, QtList);
  
  try
    Count := QStringList_size(QtList);
    GetMem(List, Count * SizeOf(TClipboardFormat));
  
    for i := 0 to Count - 1 do
    begin
      QStringList_at(QtList, @Str, i);
      Str := UTF8Encode(Str);
      List[i] := ClipboardRegisterFormat(Str);
    end;
    
    Result := Count > 0;
    
  finally
    QStringList_destroy(QtList);
  end;
end;

function TQtWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean;
var
  ClipBoard: QClipboardH;
  MimeData: QMimeDataH;
  
  procedure PutOnClipBoard;
  var
    MimeType: WideString;
    MimeData: QMimeDataH;
    Data: QByteArrayH;
  begin
    MimeData := QMimeData_create();
    
{    for I := 0 to FormatCount - 1 do
    begin
      MimeType := FClipBoardFormats[Format];
      
      Data := QByteArray_create();
      
      QMimeData_setData(MimeData, @MimeType, Data);
    end;}

    QClipboard_setMimeData(ClipBoard, MimeData, ClipbBoardTypeToQtClipboard[ClipBoardType]);
    QMimeData_destroy(MimeData);
  end;
  
begin
  Result := false;
  if (FormatCount=0) or (OnRequestProc=nil) then
  begin
  { The LCL indicates it doesn't have the clipboard data anymore
    and the interface can't use the OnRequestProc anymore.}
    OnClipBoardRequest := nil;
    Result := true;
  end else
  begin
  { clear OnClipBoardRequest to prevent destroying the LCL clipboard,
    when emptying the clipboard}
    OnClipBoardRequest := nil;
    ClipBoard := QApplication_clipboard();
    if ClipBoard = nil then
    begin
      exit;
    end;
    QClipboard_clear(ClipBoard, ClipbBoardTypeToQtClipboard[ClipBoardType]);
    Result := true;
    OnClipBoardRequest := OnRequestProc;
    PutOnClipBoard;
  end;
end;

function TQtWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := FClipBoardFormats.IndexOf(AMimeType);
  if Result = -1 then
    Result := FClipBoardFormats.Add(AMimeType);
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
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI CreateBitmap]',
     ' Width:', dbgs(Width),
     ' Height:', dbgs(Height),
     ' Planes:', dbgs(Planes),
     ' BitCount:', dbgs(BitCount),
     ' BitmapBits: ', dbgs(BitmapBits));
  {$endif}

  Result := HBitmap(TQtImage.Create(BitmapBits, Width,
   Height, QImageFormat_ARGB32));
   
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI CreateBitmap] Bitmap:', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.CreateBitmapFromRawImage
  Params:
  Returns:

  This functions is for TBitmap support
  
  The memory allocation code was added because it is necessary for
 TBitmap.LoadFromDevice support. For other operations it isnt needed
 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreateBitmapFromRawImage(const RawImage: TRawImage;
  var Bitmap, MaskBitmap: HBitmap; AlwaysCreateMask: boolean): boolean;
var
  NewData: PByte;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI CreateBitmapFromRawImage]',
     ' Width:', dbgs(RawImage.Description.Width),
     ' Height:', dbgs(RawImage.Description.Height),
     ' DataSize: ', dbgs(RawImage.DataSize),
     ' CreateMask: ', dbgs(AlwaysCreateMask));
  {$endif}

  Result := False;
  Bitmap := 0;
  MaskBitmap := 0;

  NewData := GetMem(RawImage.DataSize);

  Move(RawImage.Data^, NewData^, RawImage.DataSize);

  Bitmap := HBitmap(TQtImage.Create(NewData, RawImage.Description.Width,
   RawImage.Description.Height, QImageFormat_ARGB32, True));
  
  Result := True;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI CreateBitmapFromRawImage] Bitmap:', dbghex(Bitmap));
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
      end;
    else
      WriteLn(Format('Unsupported Style %d',[LogBrush.lbStyle]));
    end;

    {   Other non-utilized Qt brushes:

    QtDense1Pattern,
    QtDense2Pattern,
    QtDense3Pattern,
    QtDense4Pattern,
    QtDense5Pattern,
    QtDense6Pattern,
    QtDense7Pattern,
    QtLinearGradientPattern,
    QtRadialGradientPattern,
    QtConicalGradientPattern,
    QtTexturePattern = 24 );}
    
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

  Result := GetDC(0);
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
    if LogFont.lfHeight > 0 then QtFont.setPointSize(LogFont.lfHeight)
    else if LogFont.lfHeight < 0 then QtFont.setPointSize(-1 * LogFont.lfHeight);

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
  Function: CreatePixmapIndirect
  Params:  const Data: Pointer; const TransColor: Longint
  Returns: HBITMAP

 ------------------------------------------------------------------------------}
function TQtWidgetSet.CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.CreatePixmapIndirect] missing implementation ');
  {$endif}
  Result := 0;
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
var
  Painter: QPainterH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI DeleteDC] Handle: ', dbghex(hDC));
  {$endif}
  
  Result := False;
  if not IsValidDC(hDC) then exit;
  

  Painter := TQtDeviceContext(hDC).Widget;
  Result := not QPainter_isActive(Painter);
  
  if not Result then
    QPainter_end(Painter);
  
  if Result then
  begin
    {we should never come here ... endPaint() do the job}
    {$ifdef VerboseQtWinAPI}
      WriteLn('[WinAPI DeleteDC] freeing resources ');
    {$endif}
    TQtDeviceContext(hDC).Free;
  end;
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
      if APainter <> NiL
      then
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
  
  if (AObject <> nil)
  then
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
  Method: DrawFrameControl
  Params: DC: HDC; const Rect : TRect; uType, uState : Cardinal
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawFrameControl(DC: HDC; const Rect : TRect; uType, uState : Cardinal) : Boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.DrawFrameControl] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method: DrawFocusRect
  Params: DC: HDC; const Rect: TRect
  Returns: Boolean
 ------------------------------------------------------------------------------}
function TQtWidgetSet.DrawFocusRect(DC: HDC; const Rect: TRect): boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.DrawFocusRect] missing implementation ');
  {$endif}
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
  Pen: QPenH;
  
procedure InternalDrawEdge(Outer: Boolean; const R: TRect);
var
  X1, Y1, X2, Y2: Integer;
  ColorLeftTop, ColorRightBottom: TColor;
  EdgeQtColor: TQColor;
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
  
		if grfFlags and BF_MONO = 0 then
      ColorLeftTop := ColorToRgb(ColorLeftTop);
    
    QColor_setRgb(QColorH(@EdgeQtColor),Red(ColorLeftTop),Green(ColorLeftTop),Blue(ColorLeftTop));
    Pen := QPainter_pen(QtDC.Widget);
    QPen_setColor(Pen, @EdgeQtColor);

    if grfFlags and BF_LEFT <> 0 then
      QtDC.DrawLine(X1, Y1, X1, Y2);
    if grfFlags and BF_TOP <> 0 then
      QtDC.DrawLine(X1, Y1, X2, Y1);

		if grfFlags and BF_MONO = 0 then
      ColorLeftTop := ColorToRgb(ColorRightBottom);
    
    QColor_setRgb(QColorH(@EdgeQtColor),Red(ColorRightBottom),Green(ColorRightBottom),Blue(ColorRightBottom));
    Pen := QPainter_pen(QtDC.Widget);
    QPen_setColor(Pen, @EdgeQtColor);

    if grfFlags and BF_RIGHT <> 0 then
      QtDC.DrawLine(X2, Y1, X2, Y2);
    if grfFlags and BF_BOTTOM <> 0 then
      QtDC.DrawLine(X1, Y2, X2, Y2);
  end
  else
  begin
		if grfFlags and BF_MONO = 0 then
      ColorLeftTop := ColorToRgb(ColorRightBottom);
    QColor_setRgb(QColorH(@EdgeQtColor),Red(ColorLeftTop),Green(ColorLeftTop),Blue(ColorLeftTop));
    Pen := QPainter_pen(QtDC.Widget);
    QPen_setColor(Pen, @EdgeQtColor);
    if (grfFlags and BF_DIAGONAL_ENDTOPLEFT = BF_DIAGONAL_ENDTOPLEFT) or
       (grfFlags and BF_DIAGONAL_ENDBOTTOMRIGHT = BF_DIAGONAL_ENDBOTTOMRIGHT) then
      QtDC.DrawLine(X1, Y1, X2, Y2)
    else
      QtDC.DrawLine(X1, Y2, X2, Y1);
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
var
  WideStr: WideString;
  QtFontMetrics: TQtFontMetrics;
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

  WideStr := UTF8Decode(Str);

  QtFontMetrics := TQtFontMetrics.Create(QtDC.font.Widget);
  try
    // convert DT flags to QT Flags
    F := 0;
    // horizontal alignment
    if Flags and DT_CENTER <> 0 then
      F := F or  QTAlignHCenter
    else
    if Flags and DT_RIGHT <> 0 then
      F := F or QTAlignRight
    else
      F := F or QTAlignLeft;
    // vertical alignment
    if Flags and DT_VCENTER <> 0 then
      F := F or QTAlignVCenter
    else
    if Flags and DT_BOTTOM <> 0 then
      F := F or QTAlignBottom
    else
      F := F or QTAlignTop;

    // mutually exclusive wordbreak and singleline
    if Flags and DT_WORDBREAK <> 0 then
      F := F or $1000{QTTExtWordWrap}
    else
    if Flags and DT_SINGLELINE <> 0 then
      F := F or $100;{QTTextSingleLine;}

    if Flags and DT_NOPREFIX = 0 then
      F := F or $800;{QTTextShowMnemonic;}
    
    QFontMetrics_BoundingRect(QtFontMetrics.Widget, @R, @ARect, F, @WideStr);
    
    
    //TODO: result should be different when DT_VCENTER or DT_BOTTOM is set
    Result := QtFontMetrics.height;

  finally
    QtFontMetrics.Free;
  end;

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
  Result := True;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.EnableWindow] possible wrong implementation');
  {$endif}
  if TQtWidget(hWnd).LCLObject.ClassName<>'TScrollBar' then
  QWidget_setEnabled(TQtWidget(hWnd).Widget, bEnable);
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

  if IsValidDC(PS.HDC) then
   if (TObject(PS.HDC) is TQtDeviceContext) then
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
  Function: ExcludeClipRect
  Params:  none
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TQtWidgetSet.ExcludeClipRect(dc: hdc; Left, Top, Right, Bottom : Integer) : Integer;
var
  Region: QRegionH;
  ExRegion: QRegionH;
  QtDC: TQtDeviceContext;
  R: TRect;
  R1: PRect;
  X1,Y1,X2,Y2: Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI ExcludeClipRect]');
  {$endif}
  
  Result := ERROR;
  
  if not IsValidDC(DC) then Exit;
  
  QtDC := TQtDeviceContext(DC);
  
  
  X1 := Left;
  Y1 := Top;
  X2 := Right;
  Y2 := Bottom;
  
  QMatrix_map(QPainter_worldMatrix(QtDC.Widget), X1, Y1, @X2, @Y2);

  ExRegion := QRegion_create(X1, Y1, X2 - X1, Y2 - Y1, QRegionRectangle);
  Region := QRegion_create;
  try
    QPainter_clipRegion(QtDC.Widget, Region);
    QRegion_subtract(Region, Region, ExRegion);
    QPainter_setClipRegion(QtDC.Widget, Region);
    QPainter_setClipping(QtDC.Widget, True);
    
    if QRegion_isEmpty(Region)
    then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(Region, @R);
      New(R1);
      R1^ := R;
      if QRegion_contains(Region, R1)
      then
        Result := SIMPLEREGION
      else
        RESULT := COMPLEXREGION;
      Dispose(R1);
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
          // GDK_Window_Get_Size(Drawable, @X, @Y);
          // DCOrigin := GetDCOffset(TQtDeviceContext(DC));
          GetDeviceSize(DC, DCOrigin);
          
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
    WideStr := Str;

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
begin
  {$ifdef VerboseQtWinAPI}
    DebugLn('[WinAPI FillRgn Rgn=', dbgs(RegionHnd),' Brush=', dbghex(hbr));
  {$endif}
  Result := False;
  if not IsValidDC(DC) then exit;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.FillRgn] missing implementation ');
  {$endif}
  {$note implement FillRgn()}
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
begin
  {$note implement}
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.Frame3d] missing implementation ');
  {$endif}
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

  TQtDeviceContext(DC).drawRect(ARect.Left, ARect.Top,
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

function TQtWidgetSet.GetBitmapBits(Bitmap: HBITMAP; Count: Longint;  Bits: Pointer): Longint;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetBitmapBits] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetBitmapRawImageDescription
  Params:  none
  Returns: The handle of the window with focus

  Describes the inner format utilized by Qt + the specific information for this image
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetBitmapRawImageDescription(Bitmap: HBITMAP;
  Desc: PRawImageDescription): Boolean;
var
  BitmapInfo: TDIBSECTION;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetBitmapRawImageDescription]',
     ' Bitmap=', dbghex(Bitmap));
  {$endif}

  Result := true;

  FillStandardDescription(Desc^);

  GetObject(Bitmap, SizeOf(BitmapInfo), @BitmapInfo);

  Desc^.Width := BitmapInfo.dsBm.bmWidth;
  Desc^.Height := BitmapInfo.dsBm.bmHeight;

//  Desc^.BitOrder := riboReversedBits;
//  Desc^.ByteOrder := riboLSBFirst;
//  Desc^.LineOrder := riloTopToBottom;
//  Desc^.ColorCount := 0;                // entries in color palette. Ignore when no palette.
//  Desc^.BitsPerPixel := BitmapInfo.bmBitsPixel;    // bits per pixel. can be greater than Depth.
//  Desc^.LineEnd := rileDWordBoundary;
end;

function TQtWidgetSet.GetCapture: HWND;
var
  w: QWidgetH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetCapture]');
  {$endif}
  w := QWidget_mouseGrabber();
  if w <> nil then
    Result := HWND(QtObjectFromWidgetH(w))
  else
    Result := 0;
end;

function TQtWidgetSet.GetCaretPos(var lpPoint: TPoint): Boolean;
begin
  Result := QtCaret.GetCaretPos(lpPoint);
end;

function TQtWidgetSet.GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean;
begin
  {$note implement}
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetCaretRespondToFocus] missing implementation ');
  {$endif}
  Result := False;
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

  Result:=true;
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
  result := -1;
  if not IsValidDC(DC) then
    exit;
  if rgn=0 then
    exit;
  if not QPainter_HasClipping(TQtDeviceContext(DC).Widget) then
    result := 0
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
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetDC] hWnd: ', dbghex(hWnd));
  {$endif}

  Result := HDC(TQtDeviceContext.Create(hWnd));
  
//  if hWnd <> 0 then TQtCustomForm(hWnd).Canvas := TQtDeviceContext(Result);

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

function TQtWidgetSet.GetDesignerDC(WindowHandle: HWND): HDC;
begin
  Result := HDC(TQtDeviceContext.Create(WindowHandle, False));
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetDesignerDC] missing implementation ');
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
begin
  Result := 0;

  case nVirtKey of
    VK_LSHIFT:   nVirtKey := VK_SHIFT;
    VK_LCONTROL: nVirtKey := VK_CONTROL;
    VK_LMENU:    nVirtKey := VK_MENU;
  end;
  
  case nVirtKey of
    VK_MENU:
      if (QApplication_keyboardModifiers and QtMetaModifier) > 0 then
        Result:=-1;
    VK_SHIFT:
      if (QApplication_keyboardModifiers and QtShiftModifier) > 0 then
        Result:=-1;
    VK_CONTROL:
      if (QApplication_keyboardModifiers and QtControlModifier) > 0 then
        Result:=-1;
    VK_LBUTTON:
      if (QApplication_mouseButtons and QtLeftButton) > 0 then
        Result := -1;
    VK_RBUTTON:
      if (QApplication_mouseButtons and QtRightButton) > 0 then
        Result := -1;
    VK_MBUTTON:
      if (QApplication_mouseButtons and QtMidButton) > 0 then
        Result := -1;
    else
      DebugLn('TQtWidgetSet.GetKeyState TODO ', DbgSVKCode(Word(nVirtkey)));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceRawImageDescription
  Params:  none
  Returns: True if successful

  Describes the standard format utilized by Qt
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceRawImageDescription] DC: ' + dbghex(DC));
  {$endif}
  Result := true;

  FillStandardDescription(Desc^);
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetDeviceSize
  Params:  none
  Returns: True if successful

  Return the size of a device
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetDeviceSize(DC: HDC; var P: TPoint): Boolean;
var
  Size: TSize;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetDeviceSize]');
  {$endif}

  Result := False;

  P.X := 0;
  P.Y := 0;

  if not IsValidDC(DC) then Exit;
  
  if (TObject(DC) is TQtDeviceContext) then
  begin
    if TQtDeviceContext(DC).Parent <> nil then
    begin
      QWidget_size(TQtDeviceContext(DC).Parent, @Size);
    
      P.X := Size.cx;
      P.Y := Size.cy;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetMapMode
  Params:  DC
  Returns: Integer
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetMapMode(DC: HDC): Integer;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetMapMode] DC: ' + dbghex(DC));
  {$endif}
  Result := 0;
  if not IsValidDC(DC) then exit;
  
  {$note implement - wait for LCL}
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetMapMode] missing implementation ');
  {$endif}
  // Result := TQtDeviceContext(DC).MapMode;
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
  Width, Height: Longint;
  BitmapSection : TDIBSECTION;
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
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Font';
    {$endif}

  end
  {------------------------------------------------------------------------------
    Brush
   ------------------------------------------------------------------------------}
  else if aObject is TQtBrush then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Brush';
    {$endif}
  end
  {------------------------------------------------------------------------------
    Image
   ------------------------------------------------------------------------------}
  else if aObject is TQtImage then
  begin
    {$ifdef VerboseQtWinAPI}
      ObjType := 'Image';
    {$endif}

    if Buf = nil then Result := SizeOf(TDIBSECTION)
    else
    begin
      Width := TQtImage(aObject).width;
      Height := TQtImage(aObject).height;
    
      FillChar(BitmapSection, SizeOf(TDIBSECTION), 0);

      {dsBM - BITMAP}
      BitmapSection.dsBm.bmType := $4D42;
      BitmapSection.dsBm.bmWidth := Width;
      BitmapSection.dsBm.bmHeight := Height;
      BitmapSection.dsBm.bmWidthBytes := 0;
      BitmapSection.dsBm.bmPlanes := 1;//Does Bitmap Format support more?
      BitmapSection.dsBm.bmBitsPixel := 1;
      BitmapSection.dsBm.bmBits := nil;

      {dsBmih - BITMAPINFOHEADER}
      BitmapSection.dsBmih.biSize := 40;
      BitmapSection.dsBmih.biWidth := Width;
      BitmapSection.dsBmih.biHeight := Height;
      BitmapSection.dsBmih.biPlanes := BitmapSection.dsBm.bmPlanes;
      BitmapSection.dsBmih.biBitCount := 1;

      BitmapSection.dsBmih.biCompression := 0;
      BitmapSection.dsBmih.biSizeImage := 0;

      BitmapSection.dsBmih.biXPelsPerMeter := 0;
      BitmapSection.dsBmih.biYPelsPerMeter := 0;

      BitmapSection.dsBmih.biClrUsed   := 0;
      BitmapSection.dsBmih.biClrImportant := 0;

{             case GDIBitmapType of
                gbBitmap:
                  If GDIBitmapObject <> nil then begin
                    GDK_WINDOW_GET_SIZE(GDIBitmapObject, @biWidth, @biHeight);
                    NumColors := 2;
                    biBitCount := 1;
                  end;
                gbPixmap:
                  If GDIPixmapObject <> nil then begin
                     biBitCount := word(gdk_drawable_get_depth(GDIPixmapObject));
                     gdk_drawable_get_size(GDIPixmapObject,@biWidth, @biHeight);
                  end;
              end;}

      BitmapSection.dsBmih.biBitCount := 32;

//              biSizeImage := (((biBitCount*biWidth+31) shr 5) shl 2)*biHeight;

//      BitmapSection.dsBmih.biXPelsPerMeter := ;

//      BitmapSection.dsBmih.biYPelsPerMeter := ;

//        BitmapSection.dsBm.bmHeight :=      bmWidth := biWidth;
//              bmHeight := biHeight;
//              bmBitsPixel := biBitCount;

      {dsBitfields: array[0..2] of DWORD;
       dshSection: THandle;
       dsOffset: DWORD;}

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
  Parent: TQtWidget;
begin
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetParent] possible wrong implementation ');
  {$endif}
  QtWidget := TQtWidget(Handle);
  if Assigned(QtWidget.LCLObject.Parent) then
  begin
    Parent := TQtWidget(QtWidget.LCLObject.Parent.Handle);
    Result := HWND(Parent);
  end;
end;

function TQtWidgetSet.GetProp(Handle: hwnd; Str: PChar): Pointer;
begin
  if Handle<>0 then
    result := TQtWidget(Handle).Props[str]
  else
    result := nil;
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromDevice
  Params:  none
  Returns: True if successful

  Important note: LCL requires that a Data and a Mask be allocated, even if not necessary,
 because it will try to free them without checking if they are nil.
  Allocating dummy data wont leak memory, because LCL takes care to free it.
  What is important here is to free all Qt handles utilized.
  
  This function is utilized when the function TBitmap.LoadFromDevice is called
  
  The main use for this function is to get a screenshot. It may have other uses,
 but this is the only one implemented here.
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
var
  IsDesktopDC: Boolean;
  SrcWidth, SrcHeight: Integer;
  WinID: Cardinal;
  desktop: QDesktopWidgetH;
  ScreenSize: TSize;
  PScreenSize: PSize;
  Pixmap: TQtPixmap;
  Image: QImageH;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:> [WinAPI GetRawImageFromDevice] SrcDC: ', dbghex(SrcDC),
     ' SrcWidth: ', dbgs(SrcRect.Right - SrcRect.Left),
     ' SrcHeight: ', dbgs(SrcRect.Bottom - SrcRect.Top));
  {$endif}

  Result := True;

  FillStandardDescription(NewRawImage.Description);

  SrcWidth := SrcRect.Right - SrcRect.Left;
  SrcHeight := SrcRect.Bottom - SrcRect.Top;
  
  IsDesktopDC := True; // Hard-coded, but a real check should be made

  if IsDesktopDC then
  begin
    PScreenSize := @ScreenSize;

    desktop := QApplication_desktop;

    QWidget_size(QDesktopWidget_screen(desktop), PScreenSize);

    WinID := QWidget_winId(QDesktopWidget_screen(desktop));

    Pixmap := TQtPixmap.Create(PScreenSize);
    try
      NewRawImage.Mask := GetMem(1); // Creates a dummy mask

      Pixmap.grabWindow(WinID);
    
      Image := QImage_Create;

      Pixmap.toImage(Image);
    
      NewRawImage.DataSize := QImage_numBytes(Image);

      NewRawImage.Data := GetMem(QImage_numBytes(Image));

      Move(QImage_bits(Image)^, NewRawImage.Data^, QImage_numBytes(Image));
    finally
      Pixmap.Free;
    end;

    // In this case we use the size of the desktop
    
    NewRawImage.Description.Width := QWidget_width(QApplication_desktop);
    NewRawImage.Description.Height := QWidget_height(QApplication_desktop);
  end
  else
  begin
    NewRawImage.Mask := GetMem(1); // Creates a dummy mask

    NewRawImage.Data := GetMem(1); // Creates a dummy data

    NewRawImage.Description.Width := SrcWidth;
    NewRawImage.Description.Height := SrcHeight;
  end;

  {$ifdef VerboseQtWinAPI}
    WriteLn('Trace:< [WinAPI GetRawImageFromDevice]');
  {$endif}
end;

{------------------------------------------------------------------------------
  Method:  TQtWidgetSet.GetRawImageFromBitmap
  Params:  none
  Returns: True if successful

  Creates a raw image from a bitmap
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP;
  const SrcRect: TRect; var NewRawImage: TRawImage): boolean;
var
  Image: TQtImage;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetRawImageFromBitmap] SrcBitmap: ', dbghex(SrcBitmap));
  {$endif}
  Result := false;
  
  Image := TQtImage(SrcBitmap);
  
  FillChar(NewRawImage, SizeOf(NewRawImage), 0);

  FillStandardDescription(NewRawImage.Description);
  
  {
    For now the image is simply entirely copyed and SrcRect is ignored.
    
    In the future this will need to change
  }
{  NewRawImage.Description.Width := SrcRect.Right - SrcRect.Left;
  NewRawImage.Description.Height := SrcRect.Bottom - SrcRect.Top;

  if (NewRawImage.Description.Width <= 0) or (NewRawImage.Description.Height <= 0)
  then begin
    DebugLn('WARNING: TQtWidgetSet.GetRawImageFromBitmap Intersection empty');
    exit;
  end;}

  NewRawImage.Description.Width := Image.width;
  NewRawImage.Description.Height := Image.height;

  try
    NewRawImage.DataSize := Image.numBytes;

    // copy data
    ReAllocMem(NewRawImage.Data, NewRawImage.DataSize);
    if NewRawImage.DataSize > 0 then
     Move(Image.bits()^, NewRawImage.Data^, NewRawImage.DataSize);
  except
    // Free partially copied data
  end;
  
  Result := True;
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
    If lpRect <> nil then begin
      QRegion_boundingRect(TQtRegion(RGN).Widget, @R);
      
      With lpRect^ do
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
begin
  Result := 15;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetScrollBarSize] missing implementation ');
  {$endif}
end;

function TQtWidgetSet.GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean;
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetScrollBarVisible] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: GetScrollInfo
  Params: BarFlag
           SB_CTL Retrieves the parameters for a scroll bar control. The hwnd parameter must be the handle to the scroll bar control.
           SB_HORZ Retrieves the parameters for the window's standard horizontal scroll bar.
           SB_VERT Retrieves the parameters for the window's standard vertical scroll bar.
           
          ScrollInfo returns TScrollInfo structure.
          
  Returns: boolean

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetScrollInfo(Handle: HWND; BarFlag: Integer; Var ScrollInfo: TScrollInfo): Boolean;
var
  FScrollBar: TScrollBar;
  QtScrollBar: TQtScrollBar;
begin
  Result := False;

  if Handle = 0 then exit;
  
  ScrollInfo.nTrackPos := 0;
  ScrollInfo.nPage := 0;
  ScrollInfo.nMax := 0;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPos := 0;
  ScrollInfo.fMask := SIF_UPDATEPOLICY;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  
  if (csDestroying in TQtWidget(Handle).LCLObject.ComponentState) or
   (csFreeNotification in TQtWidget(Handle).LCLObject.ComponentState) then
    exit;

  FScrollBar := nil;
  QtScrollBar := nil;

  if not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomScrollBar) then
  begin
  
    case BarFlag of
      SB_HORZ: QtScrollBar := TQtAbstractScrollArea(Handle).horizontalScrollBar;
      SB_VERT: QtScrollBar := TQtAbstractScrollArea(Handle).verticalScrollBar;
    end;

    if QtScrollBar = nil then exit;

    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMax := QAbstractSlider_maximum(QScrollBarH(QtScrollBar.Widget));
    ScrollInfo.nMin := QAbstractSlider_minimum(QScrollBarH(QtScrollBar.Widget));
    ScrollInfo.nPage := QAbstractSlider_pageStep(QScrollBarH(QtScrollBar.Widget));
    ScrollInfo.nPos := QAbstractSlider_value(QScrollBarH(QtScrollBar.Widget));
    ScrollInfo.fMask := SIF_ALL;
    ScrollInfo.cbSize := SizeOf(ScrollInfo);

    Result := True;
  end
  else
	  FScrollBar := TScrollBar(TQtWidget(Handle).LCLObject);

  if Assigned(FScrollBar) then
  begin

    if (csDestroying in FScrollBar.ComponentState) then exit;

    ScrollInfo.nTrackPos := 0; {TODO: according to msdn this is ignored in SetScrollInfo()}
    ScrollInfo.nPage := FScrollBar.PageSize;
    ScrollInfo.nMax := FScrollBar.Max;
    ScrollInfo.nMin := FScrollBar.Min;
    ScrollInfo.nPos := FScrollBar.Position;
    ScrollInfo.cbSize := SizeOf(ScrollInfo);

    Result := True;
  end;
end;

function TQtWidgetSet.GetStockObject(Value: Integer): THandle;
begin
  Result := 0;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetStockObject] missing implementation ');
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtWidgetSet.GetSysColor
  Params:   index to the syscolors array
  Returns:  RGB value

 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetSysColor(nIndex: Integer): DWORD;

  {------------------------------------------------------------------------------
    Function: GetColor
    Params:   A Qt color group and a Qt color role
    Returns:  TColor

  ------------------------------------------------------------------------------}
  function GetColor(Group: QPaletteColorGroup; Role: QPaletteColorRole): TColor;
  var
    Handle : QPaletteH;
    QColor : PQColor;
  begin
    Handle := QPalette_create;
    QApplication_palette(Handle);

    QColor:=QPalette_color(Handle, Group,Role);
    Result:=(QColor^.r and $00FF) or ((QColor^.g and $00FF) shl 8) or ((QColor^.b and $00FF) shl 16);

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
    COLOR_ACTIVECAPTION           : Result:=GetColor(QPaletteActive,   QPaletteBase);
    COLOR_INACTIVECAPTION         : Result:=GetColor(QPaletteInActive, QPaletteBase);
    COLOR_MENU                    : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_WINDOW                  : Result:=GetColor(QPaletteInActive, QPaletteBase);
    COLOR_WINDOWFRAME             : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_MENUTEXT                : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
    COLOR_WINDOWTEXT              : Result:=GetColor(QPaletteActive,   QPaletteWindowText);
    COLOR_CAPTIONTEXT             : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_ACTIVEBORDER            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_INACTIVEBORDER          : Result:=GetColor(QPaletteInactive, QPaletteWindow);
    COLOR_APPWORKSPACE            : Result:=GetColor(QPaletteActive,   QPaletteWindow);
    COLOR_HIGHLIGHT               : Result:=GetColor(QPaletteActive,   QPaletteHighlight);
    COLOR_HIGHLIGHTTEXT           : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
    COLOR_BTNFACE                 : Result:=GetColor(QPaletteActive,   QPaletteButton);
    COLOR_BTNSHADOW               : Result:=GetColor(QPaletteActive,   QPaletteShadow);
    COLOR_GRAYTEXT                : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_BTNTEXT                 : Result:=GetColor(QPaletteActive,   QPaletteButtonText);
    COLOR_INACTIVECAPTIONTEXT     : Result:=GetColor(QPaletteInactive, QPaletteText);
    COLOR_BTNHIGHLIGHT            : Result:=GetColor(QPaletteActive,   QPaletteHighlightedText);
    COLOR_3DDKSHADOW              : Result:=GetColor(QPaletteActive,   QPaletteMid);
    COLOR_3DLIGHT                 : Result:=GetColor(QPaletteActive,   QPaletteMidlight);
    COLOR_INFOTEXT                : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_INFOBK                  : Result:=GetColor(QPaletteActive,   QPaletteBase);
    // PBD: 25 is unassigned in all the docs I can find
    //      if someone finds what this is supposed to be then fill it in
    //      note defaults below, and cl[ColorConst] in graphics
    COLOR_HOTLIGHT                : Result:=GetColor(QPaletteActive,   QPaletteLight);
    COLOR_GRADIENTACTIVECAPTION   : Result:=GetColor(QPaletteActive,   QPaletteText);
    COLOR_GRADIENTINACTIVECAPTION : Result:=GetColor(QPaletteInactive, QPaletteText);
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
    SM_CMOUSEBUTTONS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CMOUSEBUTTONS    ');
      end;
    SM_CXBORDER:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXBORDER         ');
      end;
    SM_CYBORDER:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYBORDER         ');
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
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXDOUBLECLK      ');
      end;
    SM_CYDOUBLECLK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYDOUBLECLK      ');
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
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXFIXEDFRAME     ');
      end;
    SM_CYFIXEDFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYFIXEDFRAME     ');
      end;
    SM_CXFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXFULLSCREEN     ');
      end;
    SM_CYFULLSCREEN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYFULLSCREEN     ');
      end;
    { Size of the array bitmap on the horizontal scrollbar
      Currently hardcoded, but more research should be made to check if Qt gives this info }
    SM_CXHSCROLL:
      begin
        Result := 15;
      end;
    SM_CYHSCROLL:
      begin
        Result := 15;
      end;
    SM_CXHTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXHTHUMB         ');
      end;
    SM_CXICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXICON           ');
      end;
    SM_CYICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYICON           ');
      end;
    SM_CXICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXICONSPACING    ');
      end;
    SM_CYICONSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYICONSPACING    ');
      end;
    SM_CXMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMAXIMIZED      ');
      end;
    SM_CYMAXIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMAXIMIZED      ');
      end;
    SM_CXMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMAXTRACK       ');
      end;
    SM_CYMAXTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMAXTRACK       ');
      end;
    SM_CXMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMENUCHECK      ');
      end;
    SM_CYMENUCHECK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENUCHECK      ');
      end;
    SM_CXMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMENUSIZE       ');
      end;
    SM_CYMENUSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENUSIZE       ');
      end;
    SM_CXMIN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMIN            ');
      end;
    SM_CYMIN:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMIN            ');
      end;
    SM_CXMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINIMIZED      ');
      end;
    SM_CYMINIMIZED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINIMIZED      ');
      end;
    SM_CXMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINSPACING     ');
      end;
    SM_CYMINSPACING:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINSPACING     ');
      end;
    SM_CXMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXMINTRACK       ');
      end;
    SM_CYMINTRACK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMINTRACK       ');
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
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSIZE           ');
      end;
    SM_CYSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSIZE           ');
      end;
    SM_CXSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSIZEFRAME      ');
      end;
    SM_CYSIZEFRAME:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSIZEFRAME      ');
      end;
    SM_CXSMICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSMICON         ');
      end;
    SM_CYSMICON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMICON         ');
      end;
    SM_CXSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CXSMSIZE         ');
      end;
    SM_CYSMSIZE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMSIZE         ');
      end;
    SM_CXVIRTUALSCREEN:
      begin
        Result := QWidget_width(QApplication_desktop);
      end;
    SM_CYVIRTUALSCREEN:
      begin
        Result := QWidget_height(QApplication_desktop);
      end;
    { Size of the array bitmap on the vertical scrollbar
      Currently hardcoded, but more research should be made to check if Qt gives this info }
    SM_CXVSCROLL:
      begin
        Result := 15;
      end;
    SM_CYVSCROLL:
      begin
        Result := 15;
      end;
    SM_CYCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYCAPTION        ');
      end;
    SM_CYKANJIWINDOW:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYKANJIWINDOW    ');
      end;
    SM_CYMENU:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYMENU           ');
      end;
    SM_CYSMCAPTION:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYSMCAPTION      ');
      end;
    SM_CYVTHUMB:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_CYVTHUMB         ');
      end;
    SM_DBCSENABLED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_DBCSENABLED      ');
      end;
    SM_DEBUG:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_DEBUG            ');
      end;
    SM_MENUDROPALIGNMENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MENUDROPALIGNMENT');
      end;
    SM_MIDEASTENABLED:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MIDEASTENABLED   ');
      end;
    SM_MOUSEPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MOUSEPRESENT     ');
      end;
    SM_MOUSEWHEELPRESENT:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_MOUSEWHEELPRESENT');
      end;
    SM_NETWORK:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_NETWORK          ');
      end;
    SM_PENWINDOWS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_PENWINDOWS       ');
      end;
    SM_SECURE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SECURE           ');
      end;
    SM_SHOWSOUNDS:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SHOWSOUNDS       ');
      end;
    SM_SLOWMACHINE:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SLOWMACHINE      ');
      end;
    SM_SWAPBUTTON:
      begin
        Assert(False, 'Trace:TODO: [TGtkWidgetSet.GetSystemMetrics] --> SM_SWAPBUTTON       ');
      end;
    else Result := 0;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetTextColor
  Params:  DC     - A device context
  Returns: TColorRef

  Gets the Font Color currently assigned to the Device Context
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextColor(DC: HDC) : TColorRef;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextColor]');
  {$endif}

  Result := 0;
  
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.GetTextColor] missing implementation ');
  {$endif}

{  if IsValidDC(DC) then
    with TQtDeviceContext(DC) do
    begin
      Result := CurrentTextColor.ColorRef;
    end;}
end;

{------------------------------------------------------------------------------
  Function: GetTextExtentPoint
  Params:  none
  Returns: Nothing
 ------------------------------------------------------------------------------}
function TQtWidgetSet.GetTextExtentPoint(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): Boolean;
var
  QtFontMetrics: TQtFontMetrics;
  WideStr: WideString;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextExtentPoint]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  QtFontMetrics := TQtFontMetrics.Create(TQtDeviceContext(DC).font.Widget);
  try
    WideStr := Utf8Decode(Str);
    Size.cx := QtFontMetrics.width(@WideStr);
    Size.cy := QtFontMetrics.height;
  finally
    QtFontMetrics.Free;
  end;

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
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetTextMetrics]');
  {$endif}

  Result := IsValidDC(DC);

  if Result then
  begin
    QtFontMetrics := TQtFontMetrics.Create(TQtDeviceContext(DC).font.Widget);
    try
      TM.tmHeight := QtFontMetrics.height;
      TM.tmAscent := QtFontMetrics.ascent;
      TM.tmDescent := QtFontMetrics.descent;
      TM.tmInternalLeading := QtFontMetrics.leading;
      TM.tmExternalLeading := 0;
      TM.tmAveCharWidth := QtFontMetrics.charWidth('x', 0);
      TM.tmMaxCharWidth := QtFontMetrics.maxWidth;
      TM.tmWeight := TQtDeviceContext(DC).font.weight;
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
      
      TQtDeviceContext(DC).font.family(@FontFamily);
      
      { Defaults to a TrueType font.
        Note that the meaning of the FIXED_PITCH constant is the opposite of
        the name implies, according to MSDN docs. Just a small inconsistency
        on Windows API that we have to mimic. }
      if TQtDeviceContext(DC).font.fixedPitch then
        TM.tmPitchAndFamily := TRUETYPE_FONTTYPE
      else TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;

      TM.tmCharSet := EASTEUROPE_CHARSET;
    finally
      QtFontMetrics.Free;
    end;
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
  if Matrix<>nil then
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
function TQtWidgetSet.GetWindowRect(Handle: HWND; Var ARect: TRect): Integer;
var
  APos: TQtPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRect]');
  {$endif}

  Result := 0;

  QWidget_pos(TQtWidget(Handle).Widget, @APos);
  QWidget_mapToGlobal(TQtWidget(Handle).Widget, @APos, @APos);
  ARect.Top := APos.x;
  ARect.Left := APos.y;
  ARect.Bottom := ARect.Top + QWidget_height(TQtWidget(Handle).Widget);
  ARect.Right := ARect.Left + QWidget_width(TQtWidget(Handle).Widget);

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
  //LeftTop:TPoint;
  R: TRect;
  //ParentHandle: THandle;
  //WindowInfo: PWindowInfo;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI GetWindowRelativePosition]');
  {$endif}

  QWidget_geometry(TQtMainWindow(Handle).Widget, @R);
  Left := R.Left;
  Top := R.Top;
  Result := True;
  
{  WindowInfo := GetWindowInfo(Handle);
  if (WindowInfo^.WinControl is TCustomFloatSpinEdit) then
    Handle := Windows.SendMessage(Handle, UDM_GETBUDDY, 0, 0);
  if not Windows.GetWindowRect(Handle,@R) then exit;
  LeftTop.X:=R.Left;
  LeftTop.Y:=R.Top;
  ParentHandle:=Windows.GetParent(Handle);
  if ParentHandle<>0 then
  begin
    if not Windows.ScreenToClient(ParentHandle,@LeftTop) then exit;
    if not GetLCLClientBoundsOffset(ParentHandle, R) then
      exit;
    dec(LeftTop.X, R.Left);
    dec(LeftTop.Y, R.Top);
  end;
  Left:=LeftTop.X;
  Top:=LeftTop.Y;
  Result := True;}
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

  Height := QWidget_height(TQtWidget(Handle).Widget);
  Width := QWidget_width(TQtWidget(Handle).Widget);

  Result := True;

  // Here we should convert top level lcl window coordinaties to qt coord
  // Due to borders and etc
{  Style := Windows.GetWindowLong(Handle, GWL_STYLE);
  ExStyle := Windows.GetWindowLong(Handle, GWL_EXSTYLE);
  if (Style and WS_THICKFRAME) <> 0 then
  begin
    // thick, sizing border
    // add twice, top+bottom border
    Dec(Width, 2*Windows.GetSystemMetrics(SM_CXSIZEFRAME));
    Dec(Height, 2*Windows.GetSystemMetrics(SM_CYSIZEFRAME));
  end else
  if (Style and WS_BORDER) <> 0 then
  begin
    // thin, non-sizing border
    Dec(Width, 2*Windows.GetSystemMetrics(SM_CXFIXEDFRAME));
    Dec(Height, 2*Windows.GetSystemMetrics(SM_CYFIXEDFRAME));
  end;
  if (Style and WS_CAPTION) <> 0 then
    if (ExStyle and WS_EX_TOOLWINDOW) <> 0 then
      Dec(Height, Windows.GetSystemMetrics(SM_CYSMCAPTION))
    else
      Dec(Height, Windows.GetSystemMetrics(SM_CYCAPTION));

  if (WindowInfo^.WinControl is TCustomFloatSpinEdit) then
    AdjustForBuddySize;}
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
  IntersectRgn, Rgn: QRegionH;
  X1,Y1,X2,Y2: Integer;
  QtDC: TQtDeviceContext;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('***** [WinAPI TQtWidgetSet.IntersectClipRect] ');
  {$endif}
  Result := ERROR;
  if not IsValidDC(DC) then exit;
  
  QtDC := TQtDeviceContext(DC);
  
  X1 := Left;
  Y1 := Top;
  X2 := Right;
  Y2 := Bottom;
  QMatrix_map(QPainter_worldMatrix(QtDC.Widget), X1, Y1, @X2, @Y2);

  IntersectRgn := QRegion_create(X1, Y1, X2 - X1, Y2 - Y1);
  Rgn := QRegion_create;
  try
    if QPainter_hasClipping(QtDC.Widget) then
    begin
      QPainter_clipRegion(QtDC.Widget, Rgn);
      if QRegion_isEmpty(Rgn) then
        QRegion_unite(Rgn, Rgn, IntersectRgn)
      else
        QRegion_intersect(Rgn, Rgn, IntersectRgn);
    end
    else
    begin
      QPainter_setClipRegion(QtDC.Widget, InterSectRgn);
      QPainter_clipRegion(QtDC.Widget, Rgn);
    end;
    QPainter_setClipping(QtDC.Widget, True);
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
  end;
  
  TQtWidget(aHandle).Update(Rect);

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
  BrushPos: TPoint;
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI LineTo]');
  {$endif}

  Result := False;

  if not IsValidDC(DC) then Exit;

  //TODO: check this brushorigin stuff, don't smell good.
  //      replace with DC local point.
  TQtDeviceContext(DC).brushOrigin(@BrushPos);

  TQtDeviceContext(DC).drawLine( BrushPos.X, BrushPos.Y, X, Y);
   
  MoveToEx(DC, X, Y, nil);
  
  Result := True;
end;


function TQtWidgetSet.MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: Cardinal): integer;
var
  Str: WideString;
  TitleStr: WideString;
  OkStr: WideString;
begin
  Str := UTF8Decode('TQtWidgetSet.MessageBox - not implemented');
  TitleStr := UTF8Decode(lpCaption);
  OkStr := UTF8Decode('Ok');
  Result := QMessageBox_information(TQtWidget(hWnd).Widget, @Str, @TitleStr, @OkStr);
//  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
//    WriteLn('***** [WinAPI TQtWidgetSet.MessageBox] missing implementation ');
//  {$endif}
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

  if (OldPoint <> nil) then TQtDeviceContext(DC).brushOrigin(OldPoint);

  TQtDeviceContext(DC).setBrushOrigin(X, Y);
  
  Result := True;
end;

function TQtWidgetSet.MoveWindowOrgEx(DC: HDC; dX, dY: Integer): Boolean;
begin
{ note:
  this is not right implementation. MoveWindowOrgEx means shift, but
  here we only set offset
}
  Result := False;

  if IsValidDC(DC) then
    TQtDeviceContext(DC).translate(dX, dY);

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
begin
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.PostMessage] missing implementation ');
  {$endif}
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

  TQtDeviceContext(DC).drawRect(x1, y1, X2 - X1, Y2 - Y1);
  
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
    w.releaseMouse()
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

  if IsValidDC(DC) then TQtDeviceContext(DC).Free;
  
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
  result := (SavedDC>0)and(SavedDC<SavedDCList.Count);
  if result then
  begin

    if SavedDC<>SavedDCList.Count-1 then
      DebugLn('WARNING: poping other than last saved DC from stack');

    result := true;
    while SavedDC>0 do
    begin
      DCData := PQtDcData(SavedDCList[SavedDC]);
      SavedDCList.Delete(SAvedDC);
      result := TQtDeviceContext(DC).RestoreDCData(DCData);
      Dec(SavedDC);
    end;
    
  end;
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI RestoreDC]');
  {$Endif}
end;

function TQtWidgetSet.RoundRect(DC : hDC; X1, Y1, X2, Y2: Integer; RX,RY : Integer): Boolean;
var
  Painter: QPainterH;
begin
  Result := False;
  if not IsValidDC(DC) then
  begin
    {$ifdef VerboseQTWinAPI}
    WriteLn('Trace:< [WinAPI RoundRect] DC Invalid, result=', result);
    {$Endif}
    exit;
  end;

  Painter := TQtDeviceContext(Dc).Widget;
  QPainter_drawRoundRect(Painter, X1, Y1, X2, Y2, RX, RY);
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
  
  DCData := TQtDeviceContext(Dc).CreateDCData;
  result := SavedDCList.Add(DCData);
  
  {$ifdef VerboseQTWinAPI}
  WriteLn('Trace:< [WinAPI SaveDC] result=', result);
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
  Painter: QPainterH;
  R: TRect;
  R1: PRect;
begin
  result := ERROR;
  if IsValidDC(DC) then
  begin
    Painter := TQtDeviceContext(DC).Widget;
    QPainter_setClipRegion(Painter, TQtRegion(Rgn).Widget);
    if QPainter_hasClipping(Painter) then
    begin
      ARegion := QRegion_Create;
      try
        QPainter_ClipRegion(Painter, ARegion);
        if QRegion_isEmpty(ARegion) then
          result := NULLREGION
        else
        begin
          QRegion_boundingRect(ARegion, @R);
          New(R1);
          try
            R1^ := R;
            if QRegion_contains(ARegion, R1)
            then
              result := SIMPLEREGION
            else
              Result := COMPLEXREGION;
          finally
            Dispose(R1);
          end;
        end;
      finally
        QRegion_Destroy(ARegion);
      end;
    end else begin
      result := NULLREGION;
    end;
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
    FillChar(Message, SizeOf(Message), 0);
    Message.msg := LM_CAPTURECHANGED;
    Message.wParam := 0;
    Message.lParam := Result;
    LCLMessageGlue.DeliverMessage(TQtWidget(AHandle).LCLObject, Message);
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
  {$note implement}
  Result := False;
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.SetCaretRespondToFocus] missing implementation ');
  {$endif}
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
  Function: TQtWidgetSet.SetMapMode
  Params: DC: HDC; MapMode: Integer
  Returns: Integer

 ------------------------------------------------------------------------------}
function TQtWidgetSet.SetMapMode(DC: HDC; MapMode: Integer): Integer;
var
  Matrix: QMatrixH;
  MatrixNew: QMatrixH;
  dpi: TSize;
  m11, m22: Double;
  dx, dy: Double;
  QtDC: TQtDeviceContext;

procedure SetM(const Am11, Am22: Double);
begin
  m11 := Am11;
  m22 := Am22;
end;

begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetMapMode] DC: ',dbghex(DC),' MapMode: ',MapMode);
  {$endif}

  Result := 0;
  if not IsValidDC(DC) then exit;
  
  {$note implement - wait for LCL}
  
  {$ifdef VerboseQtWinAPI_MISSING_IMPLEMENTATION}
    WriteLn('***** [WinAPI TQtWidgetSet.SetMapMode] missing implementation ');
  {$endif}
  
  (*
  QtDC := TQtDeviceContext(DC);
  
  dpi.cx := GetDeviceCaps(DC, LOGPIXELSX);
  dpi.cy := GetDeviceCaps(DC, LOGPIXELSY);

  Result := GetMapMode(DC);

  case MapMode of
    MM_TEXT:
      SetM(1, 1);

    MM_LOMETRIC:
      SetM((dpi.cx / 2.54) / 100, -(dpi.cy / 2.54) / 100);
    MM_HIMETRIC:
      SetM((dpi.cx / 2.54) / 1000, -(dpi.cy / 2.54) / 1000);

    MM_LOENGLISH:
      SetM(dpi.cx / 10, -dpi.cy / 10);
    MM_HIENGLISH:
      SetM(dpi.cx / 100, -dpi.cy / 100);

    MM_TWIPS:
      SetM(dpi.cx / 1440, -dpi.cy / 1440);

    MM_ISOTROPIC:

  end;

  Matrix := QPainter_matrix(QtDC.Widget);
  
  if QPainter_worldMatrixEnabled(QtDC.Widget) then
  begin
    Matrix := QPainter_worldMatrix(QtDC.Widget);
    dx := QMatrix_dx(Matrix);
    dy := QMatrix_dy(Matrix);
  end else
  begin
    dx := 0;
    dy := 0;
  end;
  
  MatrixNew := QMatrix_create(m11, 0, 0, m22, dx, dy);
  try
    QPainter_setWorldMatrix(QtDC.Widget, MatrixNew, False);
  finally
    QMatrix_destroy(MatrixNew);
  end;
  
  Result := MapMode;
  *)
  
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
    QWidget_SetFocus(TQtWidget(hWnd).Widget);
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
begin
  {$ifdef VerboseQtWinAPI}
    WriteLn('[WinAPI SetWindowOrgEx] DC: ', dbghex(DC), ' NewX: ', dbgs(NewX), ' NewY: ', dbgs(NewY));
  {$endif}

  Result := False;

  if IsValidDC(DC) then
  begin
    GetWindowOrgEx(DC, OldPoint);
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
  ScrollBar: TScrollBar;
  FScrollInfo: TScrollInfo;
  R: TRect;
  FRepaint: Boolean;

function PrepareScrollInfo: Integer;
var
  iReCountMax: Integer;
begin

  Result := 0;
  
  if not Assigned(ScrollBar) then exit;

  FillChar(FScrollInfo, SizeOf(FScrollInfo), #0);
  FScrollInfo.cbSize := SizeOf(FScrollInfo);
  FScrollInfo.FMask := ScrollInfo.FMask;

  if GetScrollInfo(Handle, SBStyle, FScrollInfo) then
  begin
    {impossible cases}
    if (Integer(ScrollInfo.nPage) > ScrollInfo.nMax) then exit;
  
    if (ScrollInfo.FMask or SIF_RANGE) = ScrollInfo.FMask then
    begin
      FScrollInfo.nMin := ScrollInfo.nMin;
      FScrollInfo.nMax := ScrollInfo.nMax;
      ScrollBar.Min := ScrollInfo.nMin;
      
      {we must recount ScrollBar.Max since invalid value raises AV}
      iRecountMax := FScrollInfo.nMax - ScrollInfo.nPage;
      if iRecountMax < FScrollInfo.nMin then
      iRecountMax := FScrollInfo.nMin;

      ScrollBar.Max := iRecountMax;
      { - (ScrollInfo.nMax div 4 PageStep property)); }
    end;

    if (ScrollInfo.FMask or SIF_PAGE) = ScrollInfo.FMask then
    begin
      FScrollInfo.nPage := ScrollInfo.nPage;
      {segfaults if we don't check Enabled property !}
      if ScrollBar.Enabled  then
        ScrollBar.PageSize := ScrollInfo.nPage;
    end;
    
    if (ScrollInfo.FMask or SIF_POS) = ScrollInfo.FMask then
    begin
      FScrollInfo.nPos := ScrollInfo.nPos;
      if ScrollBar.Position <> ScrollInfo.nPos then
        ScrollBar.Position := ScrollInfo.nPos;
    end;

    if (ScrollInfo.FMask or SIF_TRACKPOS) = ScrollInfo.FMask then
    begin
      FScrollInfo.nTrackPos := ScrollInfo.nTrackPos;
      {TODO: TQtScrollBar(ScrollBar.Handle).setTracking(True); via SB_THUMBTRACK }
    end;

    if (ScrollInfo.FMask or SIF_ALL) = ScrollInfo.FMask then
    begin
      FScrollInfo.nPage := ScrollInfo.nPage;
      FScrollInfo.nPos := ScrollInfo.nPos;
      FScrollInfo.nMin := ScrollInfo.nMin;
      FScrollInfo.nMax := ScrollInfo.nMax;
      ScrollBar.Min := ScrollInfo.nMin;
      ScrollBar.Max := ScrollInfo.nMax;
      {segfaults if we don't check Enabled property !}
      if ScrollBar.Enabled then
        ScrollBar.PageSize := ScrollInfo.nPage;
      if ScrollBar.Position<>ScrollInfo.nPos then
        ScrollBar.Position := ScrollInfo.nPos;
    end;
    
    if (ScrollInfo.FMask or SIF_DISABLENOSCROLL) = ScrollInfo.FMask then
    begin
      {This value is used only when setting a scroll bar''s parameters.
       If the scroll bar's new parameters make the scroll bar unnecessary,
       disable the scroll bar instead of removing it.}

       ScrollBar.Enabled := False;
    end else
    begin
       if not ScrollBar.Enabled then
       begin
         ScrollBar.Enabled := True;
         ScrollBar.Invalidate;
       end;
    end;
    
    ScrollInfo := FScrollInfo;
    Result := FScrollInfo.nPos;
  end;
end;

begin
  Result := 0;
  
  if (Handle = 0) then exit;
  
  FRepaint := False;
  ScrollBar := NiL;
  case SBStyle of
  
    SB_BOTH:
    begin
      {TODO: SB_BOTH fixme }
      //writeln('TODO: ############## SB_BOTH CALLED HERE .... #################');
    end; {SB_BOTH}
    
    SB_CTL:
    begin
      {HWND is always TScrollBar, but seem that Create ScrollBar should be called here }
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState)
      or (csDestroying in TQtWidget(Handle).LCLObject.ComponentState)
      then
        exit;

      ScrollBar := TScrollBar(TQtWidget(Handle).LCLObject);

      if not Assigned(ScrollBar) then exit;

      if not Assigned(ScrollBar.Parent) then
      begin
       ScrollBar := NiL;
       exit; {still creating ... set it to Nil because of PrepareScrollInfo() }
      end;

      FRepaint := bRedraw and not ScrollBar.Visible;
                   
      ScrollBar.Visible := bRedraw;
    end; {SB_CTL}
    
    SB_HORZ:
    begin
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState)
      or (csDestroying in TQtWidget(Handle).LCLObject.ComponentState)
      then
        exit;

      {do not localize !}
      ScrollBar := TScrollBar(TQtWidget(Handle).LCLObject.FindChildControl(TQtWidget(Handle).LCLObject.Name+'_HSCROLLBAR'));

      if not Assigned(ScrollBar)
      and not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomForm) then
      begin
        ScrollBar := TScrollBar.Create(TQtWidget(Handle).LCLObject);
        ScrollBar.Name := TQtWidget(Handle).LCLObject.Name+'_HSCROLLBAR'; {do not localize !}
        ScrollBar.Parent := TQtWidget(Handle).LCLObject;
        ScrollBar.Kind := sbHorizontal;
        R := TQtWidget(Handle).LCLObject.ClientRect;
        {if we have -width then av raises }
        if (R.Right - ScrollBar.Height) >= 0 then
        Scrollbar.Width := R.Right - ScrollBar.Height;
        ScrollBar.Top := R.Bottom - ScrollBar.Height;
        
        if not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomForm) then
        begin
          ScrollBar.Parent := TQtAbstractScrollArea(Handle).LCLObject;
          TQtAbstractScrollArea(Handle).sethorizontalScrollBar(TQtScrollBar(ScrollBar.Handle));
          {TODO: howto find ScrollBar style ?!?}
          TQtAbstractScrollArea(Handle).setScrollStyle(ssAutoHorizontal);
          TQtAbstractScrollArea(Handle).horizontalScrollBar.Show;
        end;
      end;
      if Assigned(ScrollBar) then
      begin
        FRepaint := bRedraw and not ScrollBar.Visible;
        ScrollBar.Visible := bRedraw;
      end;
      
    end; {SB_HORZ}
    
    SB_VERT:
    begin
      if (csReading in TQtWidget(Handle).LCLObject.ComponentState)
      or (csDestroying in TQtWidget(Handle).LCLObject.ComponentState)
      then
        exit;

      {do not localize !}
      ScrollBar := TScrollBar(TQtWidget(Handle).LCLObject.FindChildControl(TQtWidget(Handle).LCLObject.Name+'_VSCROLLBAR'));
                   
      if not Assigned(ScrollBar)
      and not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomForm) then
      begin
        ScrollBar := TScrollBar.Create(TQtWidget(Handle).LCLObject);
        ScrollBar.Name := TQtWidget(Handle).LCLObject.Name+'_VSCROLLBAR';  {do not localize !}
        ScrollBar.Parent := TQtWidget(Handle).LCLObject;
        ScrollBar.Kind := sbVertical;
        R := TQtWidget(Handle).LCLObject.ClientRect;

        Scrollbar.Height := R.Bottom;

        ScrollBar.Top := 0;
        {TODO: Check why BorderWidth is 0 when BorderStyle is eg. bsSingle ?!? }
        ScrollBar.Left := R.Right - ScrollBar.Width;
        if not TQtWidget(Handle).LCLObject.InheritsFrom(TCustomForm) then
        begin
          ScrollBar.Parent := TQtAbstractScrollArea(Handle).LCLObject;
          TQtAbstractScrollArea(Handle).setVerticalScrollBar(TQtScrollBar(ScrollBar.Handle));
          {TODO: howto find ScrollBar style ?!?}
          TQtAbstractScrollArea(Handle).setScrollStyle(ssAutoVertical);
          TQtAbstractScrollArea(Handle).verticalScrollBar.Show;
        end;

      end;
      if Assigned(ScrollBar) then
      begin
        FRepaint := bRedraw and not ScrollBar.Visible;
        ScrollBar.Visible := bRedraw;
      end;

    end; {SB_VERT}
    
  end;
  
  if Assigned(ScrollBar) then
  begin
    if FRepaint then ScrollBar.Invalidate;

    if bRedraw
    then
      Result := PrepareScrollInfo;
  end;

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
  SrcRect, DstRect: TRect;
  Image: QImageH;
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

  DstRect := Bounds(X, Y, Width, Height);

  SrcRect := Bounds(XSrc, YSrc, SrcWidth, SrcHeight);
  
  Image := TQtDeviceContext(SrcDC).vImage;

  TQtDeviceContext(DestDC).CorrectCoordinates(DstRect);

  TQtDeviceContext(DestDC).CorrectCoordinates(SrcRect);

  TQtDeviceContext(DestDC).drawImage(@DstRect, Image, @SrcRect);

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

  WideStr := UTF8Decode(Str);

//  if TQtDeviceContext(DC).isDrawing then TQtDeviceContext(DC).drawText(X, Y, @WideStr)
//  else TQtDeviceContext(DC).AddObject(dcTextOut, @WideStr, X, Y);
  
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




