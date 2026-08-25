unit lmfWMFRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage,
  LConvEncoding, LCLIntf, LCLType, GraphType, Graphics, IntfGraphics,
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
    FHasPlaceableMetaHeader: Boolean;
    FLogOrgX, FLogOrgY, FLogWidth, FLogHeight: Integer;
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
    // Bitmap transparency mask
    FMaskBmp: TBitmap;

    function CreateBrush(const AParams: TWMFParamArray): Integer;
    function CreateFont(const AParams: TWMFParamArray): Integer;
    function CreatePen(const AParams: TWMFParamArray): Integer;
    procedure DeleteObj(const AParams: TWMFParamArray);
    procedure MeasureLogExtent;
    procedure ReadArc(const AParams: TWMFParamArray);
    procedure ReadBkColor(const AParams: TWMFParamArray);
    procedure ReadBkMode(const AParams: TWMFParamArray);
    procedure ReadChord(const AParams: TWMFParamArray);
    function  ReadColor(const AParams: TWMFParamArray; AIndex: Integer): TColor;
    procedure ReadDIBStretchBlt(const AParams: TWMFParamArray);
    procedure ReadEllipse(const AParams: TWMFParamArray);
    procedure ReadExtFloodFill(const AParams: TWMFParamArray);
    procedure ReadExtTextOut(const AParams: TWMFParamArray);
    procedure ReadFloodFill(const AParams: TWMFParamArray);
    function ReadImage(const AParams: TWMFParamArray; AIndex: Integer; APicture: TPicture;
      AlwaysLoadImage: Boolean = false): Boolean;
    procedure ReadLineTo(const AParams: TWMFParamArray);
    procedure ReadMapMode(const AParams: TWMFParamArray);
    procedure ReadMoveTo(const AParams: TWMFParamArray);
    procedure ReadOffsetWindowOrg(const AParams: TWMFParamArray);
    procedure ReadPie(const AParams: TWMFParamArray);
    procedure ReadPolyFillMode(const AParams: TWMFParamArray);
    procedure ReadPolygon(const AParams: TWMFParamArray; Filled: Boolean);
    procedure ReadRectangle(const AParams: TWMFParamArray);
    procedure ReadRoundRect(const AParams: TWMFParamArray);
    procedure ReadSetDIBtoDEV(const AParams: TWMFParamArray);
    procedure ReadStretchDIB(const AParams: TWMFParamArray);
    function ReadString(const AParams: TWMFParamArray; AStartIndex, ALength: Integer): String;
    procedure ReadTextAlign(const AParams: TWMFParamArray);
    procedure ReadTextColor(const AParams: TWMFParamArray);
    procedure ReadTextOut(const AParams: TWMFParamArray);
    procedure ReadWindowExt(const AParams: TWMFParamArray);
    procedure ReadWindowOrg(const AParams: TWMFParamArray);
    procedure SelectObj(const AParams: TWMFParamArray);

  protected
    function AddToObjTable(AItem: TlmfObject): Integer;
    procedure DeleteFromObjTable(AIndex: Integer);

    procedure LogError(const AMsg: String);
    procedure ReadHeader(AStream: TStream);
    procedure ReadRecords(AStream: TStream);

  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AImage: TlmfImage); override;

  end;


implementation

uses
  BMPcomn;

const
  SIZE_OF_WORD = 2;

{ TlmfWMFReader}

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
//  FCurrTextColor := clBlack;
  FCurrTextAlign := 0;  // Left + Top
  FCurrPolyFillMode := ALTERNATE;
  FMapMode := MM_ANISOTROPIC;
end;

destructor TlmfWMFReader.Destroy;
begin
  FMaskBmp.Free;
  FCurrFont.Free;
  FCurrBrush.Free;
  FCurrPen.Free;
  FObjTable.Free;
  FErrMsg.Free;
  inherited;
end;

// Occupy the first empty place, or add at end
function TlmfWMFReader.AddToObjTable(AItem: TlmfObject): Integer;
var
  idx: Integer;
  item: TlmfObject;
begin
  for Result := 0 to FObjTable.Count-1 do
    if FObjTable[Result] = nil then
    begin
      FObjTable[Result] := AItem;
      exit;
    end;
  Result := FObjTable.Add(AItem);
end;

function TlmfWMFReader.CreateBrush(const AParams: TWMFParamArray): Integer;
var
  brushRec: PWMFBrushRecord;
  lmfBrush: TlmfBrush;
begin
  lmfBrush := TlmfBrush.Create(nil);
  brushRec := PWMFBrushRecord(@AParams[0]);

  // Brush color (must be set before Style, otherwise style would be reset to solid)
  lmfBrush.Brush.Color := RGBToColor(brushRec^.ColorRED, brushRec^.ColorGREEN, brushRec^.ColorBLUE);

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
        HS_FDIAGONAL  : lmfBrush.Brush.Style := bsFDiagonal;
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

  // Add to meta file
  FImage.List.InsertComponent(lmfBrush);

  // Add to WMF object list
  Result := AddToObjTable(lmfBrush);
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
  lmfFont.Height := abs(round(SmallInt(LEToN(fontRec^.Height))));
  lmfFont.Font.Height := -FImage.ScaleSizeY(lmfFont.Height);
//  lmfFont.Font.Height := round(SmallInt(LEToN(fontRec^.Height)));
  lmfFont.Font.Color := FCurrTextColor;
  lmfFont.Font.Bold := LEToN(fontRec^.Weight) >= 700;
  lmfFont.Font.Italic := fontRec^.Italic <> 0;
  lmfFont.Font.Underline := fontRec^.UnderLine <> 0;
  lmfFont.Font.StrikeThrough := fontRec^.Strikeout <> 0;
  lmfFont.Font.Orientation := LEToN(fontRec^.Escapement);  // Do not use fontRec^.Orientation here!

  // Add to metafile list
  FImage.List.InsertComponent(lmfFont);

  // Add to WMF object list
  Result := AddToObjTable(lmfFont);
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
  lmfPen.Pen.Width := round(LEToN(penRec^.Width));

  // Pen color
  lmfPen.Pen.Color := RGBToColor(penRec^.ColorRED, penRec^.ColorGREEN, penRec^.ColorBLUE);

  // Add to metafile image
  FImage.List.InsertComponent(lmfPen);

  // Add to WMF object list
  Result := AddToObjTable(lmfPen);
end;

procedure TlmfWMFReader.DeleteFromObjTable(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FObjTable.Count) then
  begin
    FObjTable[AIndex] := nil;
    // Do not delete from ObjTable's list because this will confuse the obj indexes.
    // Only mark the deleted obj item as nil so that the index can be re-used.
    // Also: Do not delete from FImage.List.
  end;
end;

procedure TlmfWMFReader.DeleteObj(const AParams: TWMFParamArray);
var
  idx: Integer;
begin
  idx := LEToN(AParams[0]);
  DeleteFromObjTable(idx);
end;

procedure TlmfWMFReader.LogError(const AMsg: String);
begin
  FErrMsg.Add(AMsg);
end;

{ If the wfm has no placeable metaheader, and if it contains no
  META_SETWINDOWEXTENT record, the size of FImage is still zero, which will
  later crash drawing. In this case, iterate over all records and try to
  measure the size of the window.
  TO BE IMPLEMENTED. }
procedure TlmfWMFReader.MeasureLogExtent;
begin
  FLogWidth := 5000;    // just using dummy values so far.
  FLogHeight := 5000;
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
    SmallInt(LEToN(arcRec^.XStartArc)),
    SmallInt(LEToN(arcRec^.YStartArc))
  );
  endPt := Point(
    SmallInt(LEToN(arcRec^.XEndArc)),
    SmallInt(LEToN(arcRec^.YEndArc))
  );
  R := Rect(
    SmallInt(LEToN(arcRec^.Left)),
    SmallInt(LEToN(arcRec^.Top)),
    SmallInt(LEToN(arcRec^.Right)),
    SmallInt(LEToN(arcRec^.Bottom))
  );
  item := TlmfArc.Create(R, startPt, endPt);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadBkColor(const AParams: TWMFParamArray);
var
  item: TlmfBkColor;
begin
  FCurrBkColor := ReadColor(AParams, 0);
  item := TlmfBkColor.Create(FCurrBkColor);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadBkMode(const AParams: TWMFParamArray);
var
  item: TlmfBkMode;
begin
  FCurrBkMode := LEToN(AParams[0]);
  item := TlmfBkMode.Create(FCurrBkMode);
  FImage.List.InsertComponent(item);
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
    SmallInt(LEToN(arcRec^.XStartArc)),
    SmallInt(LEToN(arcRec^.YStartArc))
  );
  endPt := Point(
    SmallInt(LEToN(arcRec^.XEndArc)),
    SmallInt(LEToN(arcRec^.YEndArc))
  );
  R := Rect(
    SmallInt(LEToN(arcRec^.Left)),
    SmallInt(LEToN(arcRec^.Top)),
    SmallInt(LEToN(arcRec^.Right)),
    SmallInt(LEToN(arcRec^.Bottom))
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
    w := SmallInt(LEToN(dibRec^.DestWidth));
    h := SmallInt(LEToN(dibRec^.DestHeight));
    R.Left := SmallInt(LEToN(dibRec^.DestX));
    R.Top := SmallInt(LEToN(dibRec^.DestY));
    R.Right := R.Left + w;
    R.Bottom := R.Top + h;
    // SrcRect not needed, it is derived from the picture ...
    lmfPic.Clip := R;
    if not ReadImage(AParams, SizeOf(TWMFDIBStretchBltRecord) div SIZE_OF_WORD, lmfPic.Picture) then
    begin
      LogError('Streaming error of DIB image.');
      lmfPic.Free;
      exit;
    end;
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
    SmallInt(LEToN(rectRec^.Left)),
    SmallInt(LEToN(rectRec^.Top)),
    SmallInt(LEToN(rectRec^.Right)),
    SmallInt(LEToN(rectRec^.Bottom))
  );

  lmfEllipse := TlmfEllipse.Create(R);
  FImage.List.InsertComponent(lmfEllipse);
end;

procedure TlmfWMFReader.ReadExtFloodFill(const AParams: TWMFParamArray);
var
  x, y: Integer;
  fillStyle: TFillStyle;
  fillColor: TColor;
  lmfFloodFill: TlmfFloodFill;
begin
  fillStyle := TFillStyle(1 - LEToN(AParams[0]));
  fillColor := ReadColor(AParams, 1);
  y := SmallInt(LEToN(AParams[3]));
  x := SmallInt(LEToN(AParams[4]));

  lmfFloodFill := TlmfFloodFill.Create(x, y, fillColor, fillStyle);
  FImage.List.InsertComponent(lmfFloodFill);
end;

procedure TlmfWMFReader.ReadExtTextOut(const AParams: TWMFParamArray);
var
  x, y, len, opts: Integer;
  R: TRect = (Left:0; Top:0; Right:0; Bottom:0);
  txt: String;
  txtStyle: TTextStyle;
  item: TlmfObject;
begin
  y := SmallInt(LEToN(AParams[0]));
  x := SmallInt(LEToN(AParams[1]));
  len := SmallInt(LEToN(AParams[2]));
  opts := LEToN(AParams[3]);
  if opts <> 0 then begin
    R.Bottom := SmallInt(LEToN(AParams[4]));
    R.Right := SmallInt(LEToN(AParams[5]));
    R.Top := SmallInt(LEToN(AParams[6]));
    R.Left := SmallInt(LEToN(AParams[7]));
    txt := ReadString(AParams, 8, len);
  end else
  begin
    txt := ReadString(AParams, 4, len);
    R := Rect(x, y, x, y);
  end;
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

procedure TlmfWMFReader.ReadFloodFill(const AParams: TWMFParamArray);
var
  x, y: Integer;
  fillStyle: TFillStyle;
  fillColor: TColor;
  lmfFloodFill: TlmfFloodFill;
begin
  fillStyle := fsBorder;
  fillColor := ReadColor(AParams, 0);
  y := SmallInt(LEToN(AParams[2]));
  x := SmallInt(LEToN(AParams[3]));

  lmfFloodFill := TlmfFloodFill.Create(x, y, fillColor, fillStyle);
  FImage.List.InsertComponent(lmfFloodFill);
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
    FImage.Width := FBBox.Right - FBBox.Left;
    FImage.Height := FBBox.Bottom - FBBox.Top;
    FImage.LogUnitsPerInch := LEToN(placeableMetaHdr.Inch);
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
  AIndex: Integer; APicture: TPicture; AlwaysLoadImage: Boolean = false): Boolean;
var
  bmpInfoHdr: PBitmapInfoHeader = nil;
  bmpFileHdr: TBitmapFileHeader;
  w, h: Integer;
  memstream: TMemoryStream;
  dataSize: Integer;
  bmp: TBitmap;
  img: TLazIntfImage;
  maskImg: TLazIntfImage;
  x, y: Integer;
begin
  Result := false;

  bmpInfoHdr := PBitmapInfoHeader(@AParams[AIndex]);
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
    bmpFileHdr.bfOffset := SizeOf(bmpFileHdr) + bmpInfoHdr^.Size;
    bmpFileHdr.bfReserved := 0;
    // Write the file header to the memory stream
    memstream.WriteBuffer(bmpFileHdr, SizeOf(bmpFileHdr));
    // Now write the DIB to the memory stream
    memstream.WriteBuffer(AParams[AIndex], (Length(AParams) - AIndex) * SIZE_OF_WORD);
    memstream.Position := 0;
    case PDWord(@AParams[0])^ of
      SRCCOPY:  // There is no mask --> read full image
        APicture.LoadFromStream(memstream);
      SRCAND:  // Extract the mask
        begin
          FMaskBmp := TBitmap.Create;
          FMaskBmp.LoadFromStream(memStream);
          // Will be destroyed when the following record with SRCPAINT is read.
        end;
      SRCPAINT:  // Extract the masked bitmap and combine it with the extracted mask.
        if FMaskBmp <> nil then
        begin
          bmp := TBitmap.Create;
          try
            bmp.LoadFromStream(memStream);
            img := TLazIntfImage.Create(0, 0);
            try
              img.LoadFromBitmap(bmp.Handle, FMaskBmp.Handle);
              maskImg := FMaskBmp.CreateIntfImage;
              try
                for y := 0 to img.Height-1 do
                  for x := 0 to img.Width-1 do
                    img.Masked[x, y] := (maskImg.Colors[x, y] = colWhite);
                APicture.Bitmap.LoadFromIntfImage(img);
                APicture.Bitmap.Transparent := true;
              finally
                maskImg.Free;
              end;
            finally
              img.Free;
            end;
          finally
            bmp.Free;
            FreeAndNil(FMaskBmp);
          end;
        end else
          APicture.LoadFromStream(memstream);

      otherwise
        if AlwaysLoadImage then // in case of META_SetDIBtoDEVRecord:
          APicture.LoadFromStream(memstream);
    end;
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
    SmallInt(LEToN(AParams[1])),
    SmallInt(LEToN(AParams[0]))
  );
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadMapMode(const AParams: TWMFParamArray);
begin
  FMapMode := LEToN(AParams[0]);
end;

procedure TlmfWMFReader.ReadMoveTo(const AParams: TWMFParamArray);
var
  item: TlmfObject;
begin
  item := TlmfMoveTo.Create(
    SmallInt(LEToN(AParams[1])),
    SmallInt(LEToN(AParams[0]))
  );
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadOffsetWindowOrg(const AParams: TWMFParamArray);
begin
  FImage.LogOriginY := FImage.LogOriginY + SmallInt(LEToN(AParams[0]));
  FImage.LogOriginX := FImage.LogOriginX + SmallInt(LEToN(AParams[1]));
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
    SmallInt(LEToN(arcRec^.XStartArc)),
    SmallInt(LEToN(arcRec^.YStartArc))
  );
  endPt := Point(
    SmallInt(LEToN(arcRec^.XEndArc)),
    SmallInt(LEToN(arcRec^.YEndArc))
  );
  R := Rect(
    SmallInt(LEToN(arcRec^.Left)),
    SmallInt(LEToN(arcRec^.Top)),
    SmallInt(LEToN(arcRec^.Right)),
    SmallInt(LEToN(arcRec^.Bottom))
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
    pts[i].X := SmallInt(LEToN(AParams[j]));
    pts[i].Y := SmallInt(LEToN(AParams[j+1]));
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
      META_STRETCHBLT:
        ;
      }
      META_SETDIBTODEV:
        ReadSetDIBtoDEV(params);
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
        ReadExtFloodFill(params);
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

  if (FLogWidth = 0) and (FLogHeight = 0) then
    MeasureLogExtent;

  FImage.SetLogBounds(FLogOrgX, FLogOrgY, FLogWidth, FLogHeight);
end;

procedure TlmfWMFReader.ReadRectangle(const AParams: TWMFParamArray);
var
  rectRec: PWMFRectRecord;   // coordinates are SmallInt
  lmfItem: TlmfRect;
  R: TRect;
begin
  rectRec := PWMFRectRecord(@AParams[0]);
  R := Rect(
    SmallInt(LEToN(rectRec^.Left)),
    SmallInt(LEToN(rectRec^.Top)),
    SmallInt(LEToN(rectRec^.Right)),
    SmallInt(LEToN(rectRec^.Bottom))
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
    SmallInt(LEToN(roundRectRec^.Left)),
    SmallInt(LEToN(roundRectRec^.Top)),
    SmallInt(LEToN(roundRectRec^.Right)),
    SmallInt(LEToN(roundRectRec^.Bottom))
  );
  RX := SmallInt(LEToN(roundRectRec^.RX));
  RY := SmallInt(LEToN(roundRectRec^.RY));
  lmfItem := TlmfRoundRect.Create(R, RX, RY);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfWMFReader.ReadSetDIBtoDEV(const AParams: TWMFParamArray);
var
  rec: PWMFSetDIBtoDEVRecord;
  lmfPic: TlmfPicture;
  w, h: Word;
  R: TRect;
begin
  rec := PWMFSetDIBToDEVRecord(@AParams[0]);
  lmfPic := TlmfPicture.Create(nil);
  try
    w := LEToN(rec^.Width);
    h := LEToN(rec^.Height);
    R.Left := LEToN(rec^.xDIB);
    R.Top := LEToN(rec^.yDIB);
    R.Right := R.Left + w;
    R.Bottom := R.Top + h;
    lmfPic.Clip := R;
    if not ReadImage(AParams, SizeOf(TWMFSetDIBtoDEVRecord) div SIZE_OF_WORD, lmfPic.Picture, true) then
      exit;
    FImage.List.InsertComponent(lmfPic);
  except
    on E:Exception do begin
      FreeAndNil(lmfPic);
      LogError('Image reading error: ' + E.Message);
    end;
  end;
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
    w := SmallInt(LEToN(dibRec^.DestWidth));
    h := SmallInt(LEToN(dibRec^.DestHeight));
    R.Left := SmallInt(LEToN(dibRec^.DestX));
    R.Top := SmallInt(LEToN(dibRec^.DestY));
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
var
  item: TlmfTextColor;
begin
  FCurrTextColor := ReadColor(AParams, 0);
  item := TlmfTextColor.Create(FCurrTextColor);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadTextOut(const AParams: TWMFParamArray);
var
  x, y, len, i: Integer;
  txt: String;
  item: TlmfText;
begin
  { Record layout:
    - word - String length
    - even number of bytes - String, no trailing zero, but padded to even length
    - smallInt - yStart
    - smallInt - xStart }

  len := LEToN(AParams[0]);
  i := 1;
  txt := ReadString(AParams, i, len);
  if txt[Length(txt)] = #0 then SetLength(txt, length(txt)-1);
  inc(i, len div 2);
  y := SmallInt((LEToN(AParams[i])));
  x := SmallInt(LEToN(AParams[i + 1]));

  item := TlmfText.Create(x, y, txt);
  FImage.List.InsertComponent(item);
end;

procedure TlmfWMFReader.ReadWindowExt(const AParams: TWMFParamArray);
begin
  FLogHeight := SmallInt(LEToN(AParams[0]));
  FLogWidth := SmallInt(LEToN(AParams[1]));
end;

procedure TlmfWMFReader.ReadWindowOrg(const AParams: TWMFParamArray);
begin
  FLogOrgY := SmallInt(LEToN(AParams[0]));
  FLogOrgX := SmallInt(LEToN(AParams[1]));
end;

procedure TlmfWMFReader.SelectObj(const AParams: TWMFParamArray);
var
  idx: Integer;
  item, newItem: TlmfObject;
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
  newItem := TlmfSelectObject.Create(item);
  FImage.List.InsertComponent(newItem);
end;

end.

