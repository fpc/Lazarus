unit lmfWMFRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LConvEncoding, LCLIntf, LCLType, Graphics,
  lmf, lmfObj, lmfWMF;

type
  TWMFParamArray = array of word;

  { TlmfWMFReader }

  TlmfWMFReader = class(TlmfReader)
  private
    FImage: TlmfImage;
    FObjTable: TFPList;        // List with WMF objects (pen, brush, ...)
    FErrMsg: TStrings;
    // info from header
    FBBox: TRect;  // in metafile units as specified by UnitsPerInch. NOTE: "logical" units can be different!
    FUnitsPerInch: Integer;
    FHasPlaceableMetaHeader: Boolean;
    // state
    FCurrPen: TPen;
    FCurrBrush: TBrush;
    FCurrFont: TFont;
    FCurrBkColor: TColor;
    FCurrBkMode: Word;
    FCurrTextAlign: Word;
    FCurrTextColor: TColor;
    FCurrPolyFillMode: Word;
    FMapMode: Word;
    FWindowOrigin: TPoint;
    FWindowExtent: TPoint;
    FScalingFactorX: Double;
    FScalingFactorY: Double;
    FPageWidth: Integer;
    FPageHeight: Integer;

    function CreateBrush(const AParams: TWMFParamArray): Integer;
    function CreateFont(const AParams: TWMFParamArray): Integer;
    function CreatePen(const AParams: TWMFParamArray): Integer;
    procedure DeleteObj(const AParams: TWMFParamArray);
    procedure ReadArc(const AParams: TWMFParamArray);
    procedure ReadBkColor(const AParams: TWMFParamArray);
    procedure ReadBkMode(const AParams: TWMFParamArray);
    procedure ReadChord(const AParams: TWMFParamArray);
    function  ReadColor(const AParams: TWMFParamArray; AIndex: Integer): TColor;
    procedure ReadDIBStretchBlt(const AParams: TWMFParamArray);
    procedure ReadEllipse(const AParams: TWMFParamArray);
    procedure ReadExtTextOut(const AParams: TWMFParamArray);
    function ReadImage(const AParams: TWMFParamArray; AIndex: Integer; APicture: TPicture): Boolean;
    procedure ReadLineTo(const AParams: TWMFParamArray);
    procedure ReadMapMode(const AParams: TWMFParamArray);
    procedure ReadMoveTo(const AParams: TWMFParamArray);
    procedure ReadOffsetWindowOrg(const AParams: TWMFParamArray);
    procedure ReadPie(const AParams: TWMFParamArray);
    procedure ReadPolyFillMode(const AParams: TWMFParamArray);
    procedure ReadPolygon(const AParams: TWMFParamArray; Filled: Boolean);
    procedure ReadRectangle(const AParams: TWMFParamArray);
    procedure ReadRoundRect(const AParams: TWMFParamArray);
    procedure ReadStretchDIB(const AParams: TWMFParamArray);
    function ReadString(const AParams: TWMFParamArray; AStartIndex, ALength: Integer): String;
    procedure ReadTextAlign(const AParams: TWMFParamArray);
    procedure ReadTextColor(const AParams: TWMFParamArray);
    procedure ReadTextOut(const AParams: TWMFParamArray);
    procedure ReadWindowExt(const AParams: TWMFParamArray);
    procedure ReadWindowOrg(const AParams: TWMFParamArray);
    procedure SelectObj(const AParams: TWMFParamArray);

  protected
    procedure CalcScalingFactors(out fx, fy: Double);
    procedure LogError(const AMsg: String);
    procedure ReadHeader(AStream: TStream);
    procedure ReadRecords(AStream: TStream);

  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AImage: TlmfImage); override;

    function ScaleX(x: Integer): Integer;
    function ScaleY(y: Integer): Integer;
    function ScaleSizeX(x: Integer): Integer;
    function ScaleSizeY(y: Integer): Integer;

  end;


implementation

uses
  BMPcomn;

const
  INCH2MM = 25.4;      // 1 inch = 25.4 mm
  MM2INCH = 1.0/INCH2MM;
  DEFAULT_SIZE = 100;  // size of image (in mm) if scaling info is not available
  SIZE_OF_WORD = 2;

constructor TlmfWMFReader.Create;
begin
  inherited;
  FErrMsg := TStringList.Create;
  FObjTable := TFPList.Create;
  FCurrPen := TPen.Create;
  with FCurrPen do begin
    Style := psSolid;
    Color := clBlack;
    Width := 1;
  end;
  FCurrBrush := TBrush.Create;
  with FCurrBrush do begin
    Style := bsClear; //Solid;
    Color := clBlack;
  end;
  FCurrFont := TFont.Create;
  with FCurrFont do begin
    Color := clBlack;
    Size := 10;
    Name := 'Arial';
    Orientation := 0;
    Bold := false;
    Italic := False;
    Underline := false;
    StrikeThrough := false;
  end;
  FCurrBkColor := clWhite;
  FCurrTextColor := clBlack;
  FCurrTextAlign := 0;  // Left + Top
  FCurrPolyFillMode := ALTERNATE;
  FMapMode := MM_ANISOTROPIC;
  FUnitsPerInch := 96;
end;

destructor TlmfWMFReader.Destroy;
begin
  FCurrFont.Free;
  FCurrBrush.Free;
  FCurrPen.Free;
  FObjTable.Free;
  FErrMsg.Free;
  inherited;
end;

procedure TlmfWMFReader.CalcScalingFactors(out fx, fy: Double);
var
  screenDpiX, screenDpiY: Integer;
begin
  screenDpiX := ScreenInfo.PixelsPerInchX;
  screenDpiY := ScreenInfo.PixelsPerInchY;

  // Convert to pixels
  case FMapMode of
    MM_TEXT:         // 1 log unit = 1 pixel
      begin
        fx := 1.0;
        fy := 1.0;
      end;
    MM_LOMETRIC:     // 1 log unit = 1/10 mm
      begin
        fx := 0.1 * MM2INCH * screenDpiX;
        fy := 0.1 * MM2INCH * screenDpiY;
      end;
    MM_HIMETRIC:     // 1 log unit = 1/100 mm
      begin
        fx := 0.01 * MM2INCH * screenDpiX;
        fy := 0.01 * MM2INCH * screenDpiY;
      end;
    MM_LOENGLISH:    // 1 log unit = 1/100"
      begin
        fx := 0.1 * screenDpiX;
        fy := 0.1 * screenDpiY;
      end;
    MM_HIENGLISH:    // 1 log unit = 1/1000"
      begin
        fx := 0.01 * screenDpiX;
        fy := 0.01 * screenDpiY;
      end;
    MM_TWIPS:        // 1 log unit = 1 twip = 1/1440 inch
      begin
        fx := 1.0 / 1440 * INCH2MM;
        fy := fx;
      end;
    else
      if (FWindowExtent.X = 0) or (FWindowExtent.Y = 0) then
        exit;
      if FHasPlaceableMetaHeader then begin
        FPageWidth := round((FBBox.Right - FBBox.Left) / FUnitsPerInch * screenDpiX);
        FPageHeight := round((FBBox.Bottom - FBBox.Top) / FUnitsPerInch * screenDpiY);
      end else
      if FWindowExtent.X > FWindowExtent.Y then begin
        FPageWidth := round(DEFAULT_SIZE * MM2INCH * screenDpiX);
        FPageHeight := round(FPageWidth * FWindowExtent.Y / FWindowExtent.X);
      end else begin
        FPageHeight := round(DEFAULT_SIZE * MM2INCH * screenDpiY);
        FPageWidth := round(FPageHeight * FWindowExtent.X / FWindowExtent.Y);
      end;
      fx := FPageWidth / FWindowExtent.X;
      fy := FPageHeight / FWindowExtent.Y;
  end;
                        (*
  // If required convert to mm
  // The nominal fpv units are mm, but the svg reader converts to pixels.
  if FPV_UNIT = fuMM then begin
    fx := fx / screenDpiX * INCH2MM;
    fy := fy / screenDpiY * INCH2MM;
    if FMapMode in [MM_ISOTROPIC, MM_ANISOTROPIC]  then begin
      FPageWidth := FPageWidth / screenDpiX * INCH2MM;
      FPageHeight := FPageHeight / screenDpiY * INCH2MM;
    end;
  end;
  *)
end;

function TlmfWMFReader.CreateBrush(const AParams: TWMFParamArray): Integer;
var
  brushRec: PWMFBrushRecord;
  lmfBrush: TlmfBrush;
begin
  lmfBrush := TlmfBrush.Create(nil);
  brushRec := PWMFBrushRecord(@AParams[0]);

  // Brush style
  case LEToN(brushRec^.Style) of
    BS_SOLID:
      lmfBrush.Brush.Style := bsSolid;
    BS_NULL:
      lmfBrush.Brush.Style := bsClear;
    BS_HATCHED:
      case brushRec^.Hatch of
        HS_HORIZONTAL : lmfBrush.Brush.Style := bsHorizontal;
        HS_VERTICAL   : lmfBrush.Brush.Style := bsVertical;
        HS_FDIAGONAL  : lmfBrush.brush.Style := bsFDiagonal;
        HS_BDIAGONAL  : lmfBrush.Brush.Style := bsBDiagonal;
        HS_CROSS      : lmfBrush.Brush.Style := bsCross;
        HS_DIAGCROSS  : lmfBrush.Brush.Style := bsDiagCross;
      end;
    { --- not supported at the moment ...
    BS_PATTERN = $0003;
    BS_INDEXED = $0004;
    BS_DIBPATTERN = $0005;
    BS_DIBPATTERNPT = $0006;
    BS_PATTERN8X8 = $0007;
    BS_DIBPATTERN8X8 = $0008;
    BS_MONOPATTERN = $0009; }
    else
      lmfBrush.Brush.Style := bsSolid;
  end;

  // Brush color
  lmfBrush.Brush.Color := RGBToColor(brushRec^.ColorRED, brushRec^.ColorGREEN, brushRec^.ColorBLUE);

  // Add to meta file
  FImage.List.InsertComponent(lmfBrush);

  // Add to WMF object list
  Result := FObjTable.Add(lmfBrush);
end;

function TlmfWMFReader.CreateFont(const AParams: TWMFParamArray): Integer;
var
  lmfFont: TlmfFont;
  fontRec: PWMFFontRecord;
  fntName: AnsiString = '';
  idx: Integer;
begin
  lmfFont := TlmfFont.Create(nil);
  fontRec := PWMFFontRecord(@AParams[0]);

  // Get font name
  SetLength(fntName, 32);
  idx := SizeOf(TWMFFontRecord) div SIZE_OF_WORD;
  fntname := StrPas(PChar(@AParams[idx]));   // string is 0-terminated

  lmfFont.Font.Name := ISO_8859_1ToUTF8(fntName);
  lmfFont.Font.Height := round(ScaleSizeY(SmallInt(LEToN(fontRec^.Height))));
  lmfFont.Font.Color := FCurrTextColor;
  lmfFont.Font.Bold := LEToN(fontRec^.Weight) >= 700;
  lmfFont.Font.Italic := fontRec^.Italic <> 0;
  lmfFont.Font.Underline := fontRec^.UnderLine <> 0;
  lmfFont.Font.StrikeThrough := fontRec^.Strikeout <> 0;
  lmfFont.Font.Orientation := LEToN(fontRec^.Escapement);  // Do not use fontRec^.Orientation here!

  // Add to WMF object list
  Result := FObjTable.Add(lmfFont);

  // Add to metafile list
  FImage.List.InsertComponent(lmfFont);
end;

function TlmfWMFReader.CreatePen(const AParams: TWMFParamArray): Integer;
var
  penRec: PWMFPenRecord;
  lmfPen: TlmfPen;
  style: Word;
begin
  lmfPen := TlmfPen.Create(nil);
  penRec := PWMFPenRecord(@AParams[0]);

  // Pen style
  style := LEToN(penRec^.Style);
  case style and $000F of
    PS_DASH       : lmfPen.Pen.Style := psDash;
    PS_DOT        : lmfPen.Pen.Style := psDot;
    PS_DASHDOT    : lmfPen.Pen.Style := psDashDot;
    PS_DASHDOTDOT : lmfPen.Pen.Style := psDashDotDot;
    PS_NULL       : lmfPen.Pen.Style := psClear;
    PS_INSIDEFRAME: lmfPen.Pen.Style := psInsideFrame;
    else            lmfPen.Pen.Style := psSolid;
  end;
  case style and $0F00 of
    PS_ENDCAP_SQUARE: lmfPen.Pen.Endcap := pecSquare;
    PS_ENDCAP_FLAT  : lmfPen.Pen.EndCap := pecFlat;
    else              lmfPen.Pen.EndCap := pecRound;
  end;
  case style and $1000 of
    PS_JOIN_BEVEL   : lmfPen.Pen.JoinStyle := pjsBevel;
    PS_JOIN_MITER   : lmfPen.Pen.JoinStyle := pjsMiter;
    else              lmfPen.Pen.JoinStyle := pjsRound;
  end;

  // Pen width
  lmfPen.Pen.Width := round(ScaleSizeX(LEToN(penRec^.Width)));
  { wp: No - pen.Width=0 means an unscaled 1-px width !
  if penRec^.Width = 0 then
    lmfPen.Pen.Width := 1;
  }

  // Pen color
  lmfPen.Pen.Color := RGBToColor(penRec^.ColorRED, penRec^.ColorGREEN, penRec^.ColorBLUE);

  // Add to WMF object list
  Result := FObjTable.Add(lmfPen);

  // Add to metafile image
  FImage.List.InsertComponent(lmfPen);
end;

procedure TlmfWMFReader.DeleteObj(const AParams: TWMFParamArray);
var
  item: TlmfObject;
  idx: Integer;
begin
  idx := LEToN(AParams[0]);
  if idx < FObjTable.Count then begin
    item := TlmfObject(FObjTable[idx]);
    FObjTable[idx] := nil;
    // Do not delete from list because this will confuse the obj indexes.
    // Only mark the deleted obj item as nil so that the index can be re-used.
    // Also: Do not delete from FImage.
  end;
end;

procedure TlmfWMFReader.LogError(const AMsg: String);
begin
  FErrMsg.Add(AMsg);
end;

procedure TlmfWMFReader.ReadArc(const AParams: TWMFParamArray);
var
  item: TlmfObject;
  arcRec: PWMFArcRecord;
  startPt, endPt: TPoint;
  R: TRect;
begin
  arcRec := PWMFArcRecord(@AParams[0]);
  startPt := Point(
    ScaleX(SmallInt(LEToN(arcRec^.XStartArc))),
    ScaleY(SmallInt(LEToN(arcRec^.YStartArc)))
  );
  endPt := Point(
    ScaleX(SmallInt(LEToN(arcRec^.XEndArc))),
    ScaleY(SmallInt(LEToN(arcRec^.YEndArc)))
  );
  R := Rect(
    ScaleX(SmallInt(LEToN(arcRec^.Left))),
    ScaleY(SmallInt(LEToN(arcRec^.Top))),
    ScaleX(SmallInt(LEToN(arcRec^.Right))),
    ScaleY(SmallInt(LEToN(arcRec^.Bottom)))
  );
  item := TlmfArc.Create(R, startPt, endPt);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadBkColor(const AParams: TWMFParamArray);
begin
  FCurrBkColor := ReadColor(AParams, 0);
end;

procedure TlmfWMFReader.ReadBkMode(const AParams: TWMFParamArray);
begin
  FCurrBkMode := LEToN(AParams[0]);
end;

procedure TlmfWMFReader.ReadChord(const AParams: TWMFParamArray);
var
  item: TlmfObject;
  arcRec: PWMFArcRecord;
  startPt, endPt: TPoint;
  R: TRect;
begin
  arcRec := PWMFArcRecord(@AParams[0]);
  startPt := Point(
    ScaleX(SmallInt(LEToN(arcRec^.XStartArc))),
    ScaleY(SmallInt(LEToN(arcRec^.YStartArc)))
  );
  endPt := Point(
    ScaleX(SmallInt(LEToN(arcRec^.XEndArc))),
    ScaleY(SmallInt(LEToN(arcRec^.YEndArc)))
  );
  R := Rect(
    ScaleX(SmallInt(LEToN(arcRec^.Left))),
    ScaleY(SmallInt(LEToN(arcRec^.Top))),
    ScaleX(SmallInt(LEToN(arcRec^.Right))),
    ScaleY(SmallInt(LEToN(arcRec^.Bottom)))
  );
  item := TlmfChord.Create(R, startPt, endPt);
  FImage.List.InsertComponent(item);
end;

function TlmfWMFReader.ReadColor(const AParams: TWMFParamArray;
  AIndex: Integer): TColor;
var
  colorRec: PWMFColorRecord;
begin
  colorRec := PWMFColorRecord(@AParams[AIndex]);
  Result := RGBToColor(colorRec^.ColorRED, colorRec^.ColorGREEN, colorRec^.ColorBLUE);
end;

// To do: two cases
// (1) ROP = SRC_COPY --> correct as is
// (2) two subsequent records, first with SRC_AND, second with SRC_PAINT; to be combined.
procedure TlmfWMFReader.ReadDIBStretchBlt(const AParams: TWMFParamArray);
var
  dibRec: PWMFDIBStretchBLTRecord;
  lmfPic: TlmfPicture;
  R: TRect;
  w, h: Integer;
begin
  dibRec := PWMFDIBStretchBltRecord(@AParams[0]);
  lmfPic := TlmfPicture.Create(nil);
  try
    w := ScaleSizeX(SmallInt(LEToN(dibRec^.DestWidth)));
    h := ScaleSizeY(SmallInt(LEToN(dibRec^.DestHeight)));
    R.Left := ScaleX(SmallInt(LEToN(dibRec^.DestX)));
    R.Top := ScaleY(SmallInt(LEToN(dibRec^.DestY)));
    R.Right := R.Left + w;
    R.Bottom := R.Top + h;
    // SrcRec not needed ...
    lmfPic.Clip := R;
    if not ReadImage(AParams, SizeOf(TWMFDIBStretchBltRecord) div SIZE_OF_WORD, lmfPic.Picture) then
      exit;
    FImage.List.InsertComponent(lmfPic);
  except
    on E:Exception do begin
      FreeAndNil(lmfPic);
      LogError('Image reading error: ' + E.Message);
    end;
  end;
end;

procedure TlmfWMFReader.ReadEllipse(const AParams: TWMFParamArray);
var
  rectRec: PWMFRectRecord;    // coordinates are SmallInt.
  R: TRect;
  lmfEllipse: TlmfEllipse;
begin
  rectRec := PWMFRectRecord(@AParams[0]);
  R := Rect(
    ScaleX(LEToN(rectRec^.Left)),
    ScaleY(LEToN(rectRec^.Top)),
    ScaleX(LEToN(rectRec^.Right)),
    ScaleY(LEToN(rectRec^.Bottom))
  );

  lmfEllipse := TlmfEllipse.Create(R);
  FImage.List.InsertComponent(lmfEllipse);
end;

procedure TlmfWMFReader.ReadExtTextOut(const AParams: TWMFParamArray);
var
  x, y, len, opts: Integer;
  R: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  txt: String;
  txtStyle: TTextStyle;
  item: TlmfObject;
begin
  y := ScaleY(SmallInt(LEToN(AParams[0])));   // signed int
  x := ScaleX(SmallInt(LEToN(AParams[1])));
  len := SmallInt(LEToN(AParams[2]));
  opts := LEToN(AParams[3]);         // unsigned int
  if opts <> 0 then begin
    R.Bottom := ScaleY(SmallInt(LEToN(AParams[4])));
    R.Right := ScaleX(SmallInt(LEToN(AParams[5])));
    R.Top := ScaleY(SmallInt(LEToN(AParams[6])));
    R.Left := ScaleX(SmallInt(LEToN(AParams[7])));
    txt := ReadString(AParams, 8, len);
  end else
    txt := ReadString(AParams, 4, len);
  // We ignore the Dx fields

  txtStyle := Default(TTextStyle);
  txtStyle.Opaque := opts and ETO_OPAQUE <> 0;
  txtStyle.Clipping := opts and ETO_CLIPPED <> 0;
  txtStyle.RightToLeft := opts and ETO_RTLREADING <> 0;
  case FCurrTextAlign and (TA_TOP or TA_BASELINE or TA_BOTTOM) of
    TA_TOP: txtStyle.Layout := tlTop;
    TA_BASELINE: txtStyle.Layout := tlCenter;
    TA_BOTTOM: txtStyle.Layout := tlBottom;
  end;
  case FCurrTextAlign and (TA_LEFT or TA_CENTER or TA_RIGHT) of
    TA_TOP: txtStyle.Alignment := taLeftJustify;
    TA_CENTER: txtStyle.Alignment := taCenter;
    TA_RIGHT: txtStyle.Alignment := taRightJustify;
  end;

  item := TlmfTextInRect.Create(R, x, y, txt, txtStyle);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadFromStream(AStream: TStream; AImage: TlmfImage);
begin
  FImage := AImage;
  FObjTable.Clear;
  FErrMsg.Clear;

  ReadHeader(AStream);
  ReadRecords(AStream);

  if FErrMsg.Count > 0 then
    raise ElmfReader.Create(FErrMsg.Text);
end;

procedure TlmfWMFReader.ReadHeader(AStream: TStream);
var
  n: Integer;
  buf: packed array[0..31] of byte;
  placeableMetaHdr: TPlaceableMetaHeader absolute buf;  // 22 bytes needed
  wmfHdr: TWMFHeader absolute buf;                      // 18 bytes needed
begin
  AStream.Position := 0;

  // Test if file begins with a placeable metafile header
  FHasPlaceableMetaHeader := false;
  n := AStream.Read(buf, SizeOf(TPlaceableMetaHeader));
  if n <> SizeOf(TPlaceableMetaHeader) then
  begin
    LogError('Error reading the wmf file header.');
    exit;
  end;

  if LEToN(placeableMetaHdr.Key) = WMF_MAGIC_NUMBER then  // yes!
  begin
    FHasPlaceableMetaHeader := true;
    FBBox.Left := LEToN(placeableMetaHdr.Left);
    FBBox.Top := LEToN(placeableMetaHdr.Top);
    FBBox.Right := LEToN(placeableMetaHdr.Right);
    FBBox.Bottom := LEToN(placeableMetaHdr.Bottom);
    FUnitsPerInch := LEToN(placeableMetaHdr.Inch);
  end else
  begin
    // Is it the wmf header?
    if not ((LEToN(wmfHdr.FileType) in [0, 1]) and (LEToN(wmfHdr.HeaderSize) = 9)) then begin
      // No - then it is not a WMF format.
      LogError('This is not a WMF file.');
      exit;
    end;
    // Rewind stream
    AStream.Position := 0;
  end;

  // Read the WMF header
  AStream.ReadBuffer(buf, SizeOf(TWMFHeader));
  // The stream now is at the first metafile record.
end;

function TlmfWMFReader.ReadImage(const AParams: TWMFParamArray;
  AIndex: Integer; APicture: TPicture): Boolean;
var
  bmpCoreHdr: PWMFBitmapCoreHeader = nil;
  bmpInfoHdr: PWMFBitmapInfoHeader = nil;
  hasCoreHdr: Boolean;
  bmpFileHdr: TBitmapFileHeader;
  w, h: Integer;
  memstream: TMemoryStream;
  imgSize: Int64;
  dataSize: Integer;
begin
  Result := false;

  bmpCoreHdr := PWMFBitmapCoreHeader(@AParams[AIndex]);
  bmpInfoHdr := PWMFBitmapInfoHeader(@AParams[AIndex]);
  hasCoreHdr := bmpInfoHdr^.HeaderSize = SizeOf(TWMFBitmapCoreHeader);
  if hasCoreHdr then
    exit;

  w := LEToN(bmpInfoHdr^.Width);
  h := LEToN(bmpInfoHdr^.Height);
  if (w = 0) or (h = 0) then
    exit;

  memStream := TMemoryStream.Create;
  try
    datasize := (Length(AParams) - AIndex) * SIZE_OF_WORD;

    // Put a bitmap file header in front of the bitmap info header and the data
    bmpFileHdr.bfType := BMmagic;
    bmpFileHdr.bfSize := SizeOf(bmpFileHdr) + datasize;
    if bmpInfoHdr^.Compression in [BI_RGB, BI_BITFIELDS{, BI_CMYK}] then
      imgSize := (w + Int64(bmpInfoHdr^.Planes) * bmpInfoHdr^.BitCount + 31) div 32 * abs(h)
    else
      imgSize := bmpInfoHdr^.ImageSize;
    bmpFileHdr.bfOffset := bmpFileHdr.bfSize - imgSize;
    bmpFileHdr.bfReserved := 0;
    // Write the file header to the memory stream
    memstream.WriteBuffer(bmpFileHdr, SizeOf(bmpFileHdr));
    // Now write the DIB to the memory stream
    memstream.WriteBuffer(AParams[AIndex], (Length(AParams) - AIndex) * SIZE_OF_WORD);

    // Read bitmap to image using the standard Picture routines.
    memstream.Position := 0;
    APicture.LoadFromStream(memstream);
    Result := true;

  finally
    memstream.Free;
  end;
end;


procedure TlmfWMFReader.ReadLineTo(const AParams: TWMFParamArray);
var
  item: TlmfObject;
begin
  item := TlmfLineTo.Create(
    ScaleX(LEToN(AParams[1])),
    ScaleY(LEToN(AParams[0]))
  );
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadMapMode(const AParams: TWMFParamArray);
begin
  FMapMode := LEToN(AParams[0]);
  CalcScalingFactors(FScalingFactorX, FScalingFactorY);
end;

procedure TlmfWMFReader.ReadMoveTo(const AParams: TWMFParamArray);
var
  item: TlmfObject;
begin
  item := TlmfMoveTo.Create(
    ScaleX(LEToN(AParams[1])),
    ScaleY(LEToN(AParams[0]))
  );
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadOffsetWindowOrg(const AParams: TWMFParamArray);
begin
  FWindowOrigin.Y := FWindowOrigin.Y + SmallInt(LEToN(AParams[0]));
  FWindowOrigin.X := FWindowOrigin.X + SmallInt(LEToN(AParams[1]));
end;

procedure TlmfWMFReader.ReadPie(const AParams: TWMFParamArray);
var
  item: TlmfObject;
  arcRec: PWMFArcRecord;
  startPt, endPt: TPoint;
  R: TRect;
begin
  arcRec := PWMFArcRecord(@AParams[0]);
  startPt := Point(
    ScaleX(SmallInt(LEToN(arcRec^.XStartArc))),
    ScaleY(SmallInt(LEToN(arcRec^.YStartArc)))
  );
  endPt := Point(
    ScaleX(SmallInt(LEToN(arcRec^.XEndArc))),
    ScaleY(SmallInt(LEToN(arcRec^.YEndArc)))
  );
  R := Rect(
    ScaleX(SmallInt(LEToN(arcRec^.Left))),
    ScaleY(SmallInt(LEToN(arcRec^.Top))),
    ScaleX(SmallInt(LEToN(arcRec^.Right))),
    ScaleY(SmallInt(LEToN(arcRec^.Bottom)))
  );
  item := TlmfPie.Create(R, startPt, endPt);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadPolyFillMode(const AParams: TWMFParamArray);
begin
  FCurrPolyFillMode := LEToN(AParams[0]);
end;

{ AParams[0] ... number of points
  AParams[1] ... x value of 1st point
  AParams[2] ... y value of 1st point
  etc }
procedure TlmfWMFReader.ReadPolygon(const AParams: TWMFParamArray; Filled: Boolean);
const
  EPS = 1E-6;
var
  n: Integer;
  i, j: Integer;
  pts: Array of TPoint = nil;
  item: TlmfObject;
begin
  n := LEToN(AParams[0]);
  SetLength(pts, n);
  j := 1;
  for i:= 0 to n-1 do begin
    pts[i].X := ScaleX(SmallInt(LEToN(AParams[j])));
    pts[i].Y := ScaleY(SmallInt(LEToN(AParams[j+1])));
    inc(j, 2);
  end;
  if Filled then
    item := TlmfPolygon.Create(@pts[0], n, (FCurrPolyFillMode=WINDING))
  else
    item := TlmfPolyLine.Create(@pts[0], n);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadRecords(AStream: TStream);
var
  recordStartPos: Int64;
  wmfRec: TWMFRecord;
  params: TWMFParamArray = nil;
  n: Integer;
begin
  wmfRec := Default(TWMFRecord);
  while AStream.Position < AStream.Size do begin
    // Store the stream position where the current record begins
    recordStartPos := AStream.Position;

    // Read record size and function code
    n := AStream.Read(wmfRec{%H-}, SizeOf(TWMFRecord));
    if n <> SizeOf(TWMFRecord) then
      raise ElmfReader.CreateFmt('Record size error (at offset %d).', [recordStartPos]);

    wmfRec.Size := LEToN(wmfRec.Size);
    wmfRec.Func := LEToN(wmfRec.Func);

   {$IFDEF WMF_DEBUG}
    DebugLn(Format(['[ReadRecords] Record position: %0:d / Record size: %1:d words / Record type: %2:d ($%2:x): %3:s',
      [FRecordStartPos, wmfRec.Size, wmfRec.Func, WMF_GetRecordTypeName(wmfRec.Func)]));
   {$ENDIF}

    // End of file?
    if wmfRec.Func = META_EOF then
      break;

    // Obviously invalid record?
    if wmfRec.Size < 3 then begin
      LogError(Format('Record size error, record at offset %d, function %d', [recordStartPos, wmfRec.Func]));
      exit;
    end;

    // Read record parameters into word array
    SetLength(params, wmfRec.Size - 3);
    n := AStream.Read(params[0], (wmfRec.Size - 3)*SIZE_OF_WORD);
    if n <> (wmfRec.Size - 3)*SIZE_OF_WORD then
      raise ElmfReader.CreateFmt('Record parameter size error, record at offset %d, function %d', [recordStartPos, wmfRec.Func]);

    // Process record, depending on function code
    case wmfRec.Func of
      { *** Bitmap record types *** }
      {
      META_BITBLT:
        ;
      META_DIBBITBLT:
        ;
      META_DIBSTRETCHBLT:
        ;
      META_SETDIBTODEV:
        ;
      META_STRETCHBLT:
        ;
      }
      META_STRETCHDIB:
        ReadStretchDIB(params);
      META_DIBSTRETCHBLT:
        ReadDIBStretchBlt(params);

      { *** Drawing records *** }
      META_ARC:
        ReadArc(params);
      META_CHORD:
        ReadChord(params);
      META_ELLIPSE:
        ReadEllipse(params);
      META_EXTFLOODFILL:
        ;
      META_EXTTEXTOUT:
        ReadExtTextOut(params);
      META_FILLREGION:
        ;
      META_FLOODFILL:
        ;
      META_FRAMEREGION:
        ;
      META_INVERTREGION:
        ;
      META_MOVETO:
        ReadMoveTo(params);
      META_LINETO:
        ReadLineTo(params);
      META_PAINTREGION:
        ;
      META_PATBLT:
        ;
      META_PIE:
        ReadPie(params);
      META_POLYGON:
        ReadPolygon(params, true);
      META_POLYLINE:
        ReadPolygon(params, false);
      {
      META_POLYPOLYGON:
        ReadPolyPolygon(page, params);
        }
      META_RECTANGLE:
        ReadRectangle(params);
      META_ROUNDRECT:
        ReadRoundRect(params);
      META_SETPIXEL:
        ;
      META_TEXTOUT:
        ReadTextOut(params);

      { *** WMF Object records *** }
      META_CREATEBRUSHINDIRECT:
        CreateBrush(params);
      META_CREATEFONTINDIRECT:
        CreateFont(params);
        {
      META_CREATEPALETTE:
        CreatePalette(params);
      META_CREATEPATTERNBRUSH:
        CreatePatternBrush(params);
        }
      META_CREATEPENINDIRECT:
        CreatePen(params);
      {
      META_CREATEREGION:
        CreateRegion(params);
      META_DIBCREATEPATTERNBRUSH:
        DIBCreatePatternBrush(params);
      }
      META_DELETEOBJECT:
        DeleteObj(params);
      {
      META_SELECTCLIPREGION:
        ;
        }
      META_SELECTOBJECT:
        SelectObj(params);
      {
      META_SELECTPALETTE:
        SelectPalette(params[0]);
       }
      { *** State records *** }
      META_ANIMATEPALETTE:
        ;
      META_EXCLUDECLIPRECT:
        ;
      META_INTERSECTCLIPRECT:
        ;
      META_OFFSETCLIPRGN:
        ;
      META_OFFSETVIEWPORTORG:
        ;
      META_OFFSETWiNDOWORG:
        ReadOffsetWindowOrg(params);
      META_REALIZEPALETTE:
        ;
      META_RESIZEPALETTE:
        ;
      META_RESTOREDC:
        ;
      META_SAVEDC:
        ;
      META_SCALEVIEWPORTEXT:
        ;
      META_SCALEWINDOWEXT:
        ;
      META_SETBKCOLOR:
        ReadBkColor(params);
      META_SETBKMODE:
        ReadBkMode(params);
      META_SETLAYOUT:
        ;
      META_SETMAPMODE:
        ReadMapMode(params);
      META_SETMAPPERFLAGS:
        ;
      META_SETPALENTRIES:
        ;
      META_SETPOLYFILLMODE:
        ReadPolyFillMode(params);
      META_SETRELABS:
        ;
      META_SETROP2:
        ;
      META_SETSTRETCHBLTMODE:
        ;
      META_SETTEXTALIGN:
        ReadTextAlign(params);
      META_SETTEXTCHAREXTRA:
        ;
      META_SETTEXTCOLOR:
        ReadTextColor(params);
      META_SETVIEWPORTEXT:
        ;
      META_SETVIEWPORTORG:
        ;
      META_SETWINDOWEXT:
        ReadWindowExt(params);
      META_SETWINDOWORG:
        ReadWindowOrg(params);

    end;
    AStream.Position := recordStartPos + Int64(wmfRec.Size) * SIZE_OF_WORD;
  end;

  if FHasPlaceableMetaHeader then begin
    FImage.Width := FPageWidth;
    FImage.Height := FPageHeight;
  end else begin
    FImage.Width := ScaleSizeX(FWindowExtent.X);
    FImage.Height := ScaleSizeY(FWindowExtent.Y);
  end;
end;

procedure TlmfWMFReader.ReadRectangle(const AParams: TWMFParamArray);
var
  rectRec: PWMFRectRecord;   // coordinates are SmallInt
  lmfItem: TlmfRect;
  R: TRect;
begin
  rectRec := PWMFRectRecord(@AParams[0]);
  R := Rect(
    ScaleX(SmallInt(LEToN(rectRec^.Left))),
    ScaleY(SmallInt(LEToN(rectRec^.Top))),
    ScaleX(SmallInt(LEToN(rectRec^.Right))),
    Scaley(SmallInt(LEToN(rectRec^.Bottom)))
  );
  lmfItem := TlmfRect.Create(R);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfWMFReader.ReadRoundRect(const AParams: TWMFParamArray);
var
  roundRectRec: PWMFRoundRectRecord;   // coordinates are SmallInt
  lmfItem: TlmfObject;
  rx, ry: Word;
  R: TRect;
begin
  roundRectRec := PWMFRoundRectRecord(@AParams[0]);
  R := Rect(
    ScaleX(SmallInt(LEToN(roundRectRec^.Left))),
    ScaleY(SmallInt(LEToN(roundRectRec^.Top))),
    ScaleX(SmallInt(LEToN(roundRectRec^.Right))),
    ScaleY(SmallInt(LEToN(roundRectRec^.Bottom)))
  );
  RX := ScaleX(SmallInt(LEToN(roundRectRec^.RX)));
  RY := ScaleY(SmallInt(LEToN(roundRectRec^.RY)));
  lmfItem := TlmfRoundRect.Create(R, RX, RY);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfWMFReader.ReadStretchDIB(const AParams: TWMFParamArray);
var
  dibRec: PWMFStretchDIBRecord;
  lmfPic: TlmfPicture;
  w, h: Integer;
  R: TRect;
begin
  dibRec := PWMFStretchDIBRecord(@AParams[0]);
  lmfPic := TlmfPicture.Create(nil);
  try
    w := ScaleSizeX(SmallInt(LEToN(dibRec^.DestWidth)));
    h := ScaleSizeY(SmallInt(LEToN(dibRec^.DestHeight)));
    R.Left := ScaleX(SmallInt(LEToN(dibRec^.DestX)));
    R.Top := ScaleY(SmallInt(LEToN(dibRec^.DestY)));
    R.Right := R.Left + w;
    R.Bottom := R.Top + h;
    // SrcRec not needed ...
    lmfPic.Clip := R;
    if not ReadImage(AParams, SizeOf(TWMFStretchDIBRecord) div SIZE_OF_WORD, lmfPic.Picture) then
      exit;
    FImage.List.InsertComponent(lmfPic);
  except
    on E:Exception do begin
      FreeAndNil(lmfPic);
      LogError('Image reading error: ' + E.Message);
    end;
  end;
end;

function TlmfWMFReader.ReadString(const AParams: TWMFParamArray;
  AStartIndex, ALength: Integer): String;
var
  s: ansistring = '';
begin
  SetLength(s, ALength);
  Move(AParams[AStartIndex], s[1], ALength);
  // Note: ALength is the true string length. No need to remove the padding byte added to odd-length strings.
  Result := ISO_8859_1ToUTF8(s);
end;

procedure TlmfWMFReader.ReadTextAlign(const AParams: TWMFParamArray);
begin
  FCurrTextAlign := LEToN(AParams[0]);
end;

procedure TlmfWMFReader.ReadTextColor(const AParams: TWMFParamArray);
begin
  FCurrTextColor := ReadColor(AParams, 0);
end;

procedure TlmfWMFReader.ReadTextOut(const AParams: TWMFParamArray);
var
  x, y, len, i: Integer;
  txt: String;
  item: TlmfText;
begin
  { Record layout:
    word - String length
    even number of bytes - String, no trailing zero, but padded to even length
    smallInt - yStart
    smallInt - xStart }

  len := LEToN(AParams[0]);
  i := 1;
  txt := ReadString(AParams, i, len);
  if txt[Length(txt)] = #0 then SetLength(txt, length(txt)-1);
  inc(i, len div 2);
  y := ScaleX(SmallInt((LEToN(AParams[i]))));      // signed int!
  x := ScaleY(SmallInt(LEToN(AParams[i + 1])));

  item := TlmfText.Create(x, y, txt);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadWindowExt(const AParams: TWMFParamArray);
begin
  FWindowExtent.Y := SmallInt(LEToN(AParams[0]));
  FWindowExtent.X := SmallInt(LEToN(AParams[1]));
  CalcScalingFactors(FScalingFactorX, FScalingFactorY);
end;

procedure TlmfWMFReader.ReadWindowOrg(const AParams: TWMFParamArray);
begin
  FWindowOrigin.Y := SmallInt(LEToN(AParams[0]));
  FWindowOrigin.X := SmallInt(LEToN(AParams[1]));
end;

{ Scale horizontal logical units (x) to millimeters }
function TlmfWMFReader.ScaleX(x: Integer): Integer;
begin
  Result := ScaleSizeX(x - FWindowOrigin.X);
end;

{ Scale vertical logical units (y) to millimeters.        // ???? mm, really?
  Coordinates will be increasing downwards }
function TlmfWMFReader.ScaleY(y: Integer): Integer;
begin
  Result := ScaleSizeY(y - FWindowOrigin.Y);
end;

function TlmfWMFReader.ScaleSizeX(x: Integer): Integer;
begin
  Result := Round(FScalingFactorX * x);
end;

function TlmfWMFReader.ScaleSizeY(y: Integer): Integer;
begin
  Result := Round(FScalingFactorY * y);
end;

procedure TlmfWMFReader.SelectObj(const AParams: TWMFParamArray);
var
  idx: Integer;
  item: TlmfObject;
begin
  idx := LEToN(AParams[0]);
  if (idx < 0) or (idx >= FObjTable.Count) then
    exit;
  item := TlmfObject(FObjTable[idx]);
  if item = nil then
    exit;
  if item is TlmfPen then
    FCurrPen.Assign(TlmfPen(item).Pen)
  else
  if item is TlmfBrush then
    FCurrBrush.Assign(TlmfBrush(item).Brush)
  else
  if item is TlmfFont then
    FCurrFont.Assign(TlmfFont(item).Font)
  else
  {  // to be added, as well as regions and more
  if obj is TFPPalette then
    FCurrPalette := TFPPalette(obj);
    };
end;

end.

