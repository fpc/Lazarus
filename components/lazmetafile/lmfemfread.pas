unit lmfEMFRead;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, bmpComn, FPImage, Math,
  LCLType, LCLIntf, LazUTF8, Graphics,
  lmf, lmfObj, lmfEMF;

type
  TEMFParamArray = array of byte;

  { TlmfEMFReader }
  TlmfEMFReader = class(TlmfReader)
  private
    FBBox: TRect;
    FBBoxMM: TRect;
    FCurrBrush: TBrush;
    FCurrFont: TFont;
    FCurrPen: TPen;
    FCurrArcDirection: Integer;
    FCurrBkColor: TColor;
    FCurrBkMode: Integer;
    FCurrPath: TlmfPath;
    FCurrTextAlign: Integer;
    FCurrTextColor: TColor;
    FCurrPolyFillMode: Integer;
    FCurrCopyMode: TCopyMode;
  private
    procedure ParamsToArc(const AParams: TEMFParamArray; AIndex: Integer;
      out ABox: TRect; out AStartPt, AEndPt: TPoint);
    procedure ParamsToColor(const AParams: TEMFParamArray; AIndex: Integer;
      out AColor: TColor);
    procedure ParamsToInt(const AParams: TEMFParamArray; AIndex: Integer;
      out AIndexValue: Integer);
    procedure ParamsToPointL(const AParams: TEMFParamArray; AIndex: Integer;
      out APoint: TPoint);
    procedure ParamsToPointS(const AParams: TEMFParamArray; AIndex: Integer;
      out APoint: TPoint);
    procedure ParamsToPointLArray(const AParams: TEMFParamArray; AIndex: Integer;
      out APoints: TPointArray);
    procedure ParamsToPointLArray(const AParams: TEMFParamArray;
      AIndex, ACount: Integer; out APoints: TPointArray);
    procedure ParamsToPointSArray(const AParams: TEMFParamArray; AIndex: Integer;
      out APoints: TPointArray);
    procedure ParamsToPointSArray(const AParams: TEMFParamArray;
      AIndex, ACount: Integer; out APoints: TPointArray);
    procedure ParamsToRect(const AParams: TEMFParamArray; AIndex: Integer;
      out ARect: TRect);
  private
    procedure ReadAbortPath;
    procedure ReadArc(const AParams: TEMFParamArray);
    procedure ReadArcTo(const AParams: TEMFParamArray);
    procedure ReadBeginPath;
    procedure ReadBitBLT(const AParams: TEMFParamArray);
    procedure ReadChord(const AParams: TEMFParamArray);
    procedure ReadCloseFigure;
    procedure ReadCreateBrushIndirect(const AParams: TEMFParamArray);
    procedure ReadCreateDIBPatternBrush(const AParams: TEMFParamArray);  // used also by CreateMonoBrush
    procedure ReadCreatePen(const AParams: TEMFParamArray);
    procedure ReadDeleteObject(const AParams: TEMFParamArray);
    procedure ReadEllipse(const AParams: TEMFParamArray);
    procedure ReadEndPath;
    procedure ReadExtCreateFontIndirect(const AParams: TEMFParamArray);
    procedure ReadExtCreatePen(const AParams: TEMFParamArray);
    procedure ReadExtTextOut(const AParams: TEMFParamArray; ACharMode: Byte);
    procedure ReadFillPath(const AParams: TEMFParamArray);
    procedure ReadFlattenPath;
    procedure ReadGradientFill(const AParams: TEMFParamArray);
    function ReadImage(const AParams: TEMFParamArray;
      AOffsetToHeader, ASizeOfHeader, AOffsetToImageBits, ASizeOfImageBits: DWord): TBitmap;
    procedure ReadLineTo(const AParams: TEMFParamArray);
    procedure ReadMoveToEx(const AParams: TEMFParamArray);
    procedure ReadPie(const AParams: TEMFParamArray);
    procedure ReadPolyBezier(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadPolyBezierTo(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadPolygon(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadPolyLine(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadPolyLineTo(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadPolyPolygon(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadPolyPolyLine(const AParams: TEMFParamArray; SmallPts: Boolean);
    procedure ReadRectangle(const AParams: TEMFParamArray);
    procedure ReadRoundRect(const AParams: TEMFParamArray);
    procedure ReadSelectObject(const AParams: TEMFParamArray);
    procedure ReadSetArcDirection(const AParams: TEMFParamArray);
    procedure ReadSetBkColor(const AParams: TEMFParamArray);
    procedure ReadSetBkMode(const AParams: TEMFParamArray);
    procedure ReadSetPolyFillMode(const AParams: TEMFParamArray);
    procedure ReadSetROP2(const AParams: TEMFParamArray);
    procedure ReadSetTextAlign(const AParams: TEMFParamArray);
    procedure ReadSetTextColor(const AParams: TEMFParamArray);
    procedure ReadStretchBlt(const AParams: TEMFParamArray);
    procedure ReadStretchDIBits(const AParams: TEMFParamArray);
    procedure ReadStrokePath(const AParams: TEMFParamArray);
    procedure ReadStrokeAndFillPath(const AParams: TEMFParamArray);
    procedure ReadWidenPath;
  protected
    procedure AddToObjTable(AIndex: Integer; AItem: TlmfObject);
    procedure DeleteFromObjTable(AIndex: Integer);

    function ReadHeader(AStream: TStream): Boolean;
    procedure ReadRecords(AStream: TStream); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TStockObjects }

procedure StockObjectToPen(AIndex: Byte; APen: TPen);

  procedure SetPen(AStyle: TPenStyle; AColor: TColor);
  begin
    APen.Color := AColor;
    APen.Style := AStyle;
    APen.Width := 0;    // 1 pixel, even when scaled.
  end;

begin
  // Note: These are the LCLIntf declarations which are > 0 (unlike the WinAPI
  // values which are $800000x).
  case AIndex of
    WHITE_PEN: SetPen(psSolid, clWhite);
    BLACK_PEN: SetPen(psSolid, clBlack);
    NULL_PEN: SetPen(psClear, clNone);
    DC_PEN: SetPen(psSolid, clBlack);
  end;
end;

procedure StockObjectToBrush(AIndex: Byte; ABrush: TBrush);

  procedure SetBrush(AStyle: TBrushStyle; AColor: TColor);
  begin
    ABrush.Color := AColor;
    ABrush.Style := AStyle;
  end;

begin
  // Note: These are the LCLIntf declarations which are > 0 (unlike the WinAPI
  // values which are $800000x).
  case AIndex of
    WHITE_BRUSH: SetBrush(bsSolid, clWhite);
    LTGRAY_BRUSH: SetBrush(bsSolid, clLtGray);
    GRAY_BRUSH: SetBrush(bsSolid, clGray);
    DKGRAY_BRUSH: SetBrush(bsSolid, clDkGray);
    BLACK_BRUSH: SetBrush(bsSolid, clBlack);
    NULL_BRUSH: SetBrush(bsClear, clNone);
    DC_BRUSH: SetBrush(bsSolid, clWhite);
  end;
end;

// to be completed...
procedure StockObjectToFont(AIndex: Byte; AFont: TFont);
begin
  case AIndex of
    OEM_FIXED_FONT: ;
    ANSI_FIXED_FONT: ;
    ANSI_VAR_FONT: ;
    SYSTEM_FONT: ;
    DEVICE_DEFAULT_FONT: ;
    SYSTEM_FIXED_FONT: ;
    DEFAULT_GUI_FONT: ;
  end;
end;


{ TlmfEMFReader }

constructor TlmfEMFReader.Create;
begin
  inherited;
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
  FCurrArcDirection := AD_COUNTERCLOCKWISE;
  FCurrCopyMode := SRCCOPY;
end;

destructor TlmfEMFReader.Destroy;
begin
//  FMaskBmp.Free;
  FCurrFont.Free;
  FCurrBrush.Free;
  FCurrPen.Free;
  inherited;
end;

{ AIndex is the index at which the specified item is supposed to be in the
  object list. }
procedure TlmfEMFReader.AddToObjTable(AIndex: Integer; AItem: TlmfObject);
var
  n, i: Integer;
begin
  n := FObjTable.Count;
  if AIndex >= n then
    for i := n to AIndex do
      FObjTable.Add(nil);
  FObjTable[AIndex] := AItem;
end;

procedure TlmfEMFReader.DeleteFromObjTable(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < FObjTable.Count) then
  begin
    FObjTable[AIndex] := nil;
    // Do not delete from ObjTable's list because this will confuse the obj indexes.
    // Only mark the deleted obj item as nil so that the index can be re-used.
    // Also: Do not delete from FImage.List.
  end;
end;

procedure TlmfEMFReader.ParamsToArc(const AParams: TEMFParamArray; AIndex: Integer;
  out ABox: TRect; out AStartPt, AEndPt: TPoint);
begin
  ParamsToRect(AParams, AIndex, ABox);
  ParamsToPointL(AParams, AIndex + 16, AStartPt);
  ParamsToPointL(AParams, AIndex + 24, AEndPt);
end;

procedure TlmfEMFReader.ParamsToColor(const AParams: TEMFParamArray; AIndex: Integer;
  out AColor: TColor);
begin
  // to do: flip bytes for Big Endian...
  AColor := RGBToColor(AParams[AIndex], AParams[AIndex+1], AParams[AIndex+2]);
end;

procedure TlmfEMFReader.ParamsToInt(const AParams: TEMFParamArray; AIndex: Integer;
  out AIndexValue: Integer);
var
  idx: DWord;
begin
  idx := LEToN(PDWord(@AParams[AIndex])^);
  AIndexValue := idx;
end;

procedure TlmfEMFReader.ParamsToPointS(const AParams: TEMFParamArray;
  AIndex: Integer; out APoint: TPoint);
var
  ptRec: PEmfPointSRecord;
begin
  ptRec := PEMFPointSRecord(@AParams[AIndex]);
  APoint := Point(
    LongInt(LEtoN(ptRec^.X)),
    LongInt(LEtoN(ptRec^.Y))
  );
end;

procedure TlmfEMFReader.ParamsToPointL(const AParams: TEMFParamArray;
  AIndex: Integer; out APoint: TPoint);
var
  ptRec: PEmfPointLRecord;
begin
  ptRec := PEMFPointLRecord(@AParams[AIndex]);
  APoint := Point(
    LongInt(LEtoN(ptRec^.X)),
    LongInt(LEtoN(ptRec^.Y))
  );
end;

procedure TlmfEMFReader.ParamsToPointLArray(const AParams: TEMFParamArray;
  AIndex: Integer; out APoints: TPointArray);
var
  i, j, nPts: Integer;
begin
  ParamsToInt(AParams, AIndex, nPts);
  APoints := nil;
  SetLength(APoints, nPts);
  j := AIndex + 4;
  for i := 0 to nPts-1 do
  begin
    ParamsToPointL(AParams, j, APoints[i]);
    inc(j, Sizeof(TEmfPointLRecord));
  end;
end;

procedure TlmfEMFReader.ParamsToPointLArray(const AParams: TEMFParamArray;
  AIndex, ACount: Integer; out APoints: TPointArray);
var
  i, j: Integer;
begin
  APoints := nil;
  SetLength(APoints, ACount);
  j := AIndex;
  for i := 0 to ACount-1 do
  begin
    ParamsToPointL(AParams, j, APoints[i]);
    inc(j, Sizeof(TEmfPointLRecord));
  end;
end;

procedure TlmfEMFReader.ParamsToPointSArray(const AParams: TEMFParamArray;
  AIndex: Integer; out APoints: TPointArray);
var
  i, j, nPts: Integer;
begin
  ParamsToInt(AParams, AIndex, nPts);
  APoints := nil;
  SetLength(APoints, nPts);
  j := AIndex + 4;
  for i := 0 to nPts-1 do
  begin
    ParamsToPointS(AParams, j, APoints[i]);
    inc(j, Sizeof(TEmfPointSRecord));
  end;
end;

procedure TlmfEMFReader.ParamsToPointSArray(const AParams: TEMFParamArray;
  AIndex, ACount: Integer; out APoints: TPointArray);
var
  i, j: Integer;
begin
  APoints := nil;
  SetLength(APoints, ACount);
  j := AIndex;
  for i := 0 to ACount-1 do
  begin
    ParamsToPointS(AParams, j, APoints[i]);
    inc(j, Sizeof(TEmfPointSRecord));
  end;
end;

procedure TlmfEMFReader.ParamsToRect(const AParams: TEMFParamArray;
  AIndex: Integer; out ARect: TRect);
var
  rectRec: PEMFRectLRecord;
begin
  rectRec := PEMFRectLRecord(@AParams[AIndex]);
  ARect := Rect(
    LongInt(LEtoN(rectRec^.Left)),
    LongInt(LEtoN(rectRec^.Top)),
    LongInt(LEtoN(rectRec^.Right)),
    LongInt(LEtoN(rectRec^.Bottom))
  );
end;

procedure TlmfEMFReader.ReadAbortPath;
begin
  if Assigned(FCurrPath) then
    FCurrPath.AbortPath;
end;

procedure TlmfEMFReader.ReadArc(const AParams: TEMFParamArray);
var
  R: TRect;
  startPt, endPt: TPoint;
  lmfItem: TlmfArc;
begin
  ParamsToArc(AParams, 0, R, startPt, endPt);
  case FCurrArcDirection of
    AD_COUNTERCLOCKWISE: lmfItem := TlmfArc.Create(R, startPt, endpt);
    AD_CLOCKWISE: lmfItem := TlmfArc.Create(R, endPt, startPt)
  end;
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadArcTo(const AParams: TEMFParamArray);
var
  R: TRect;
  startPt, endPt, tmp: TPoint;
  lmfItem: TlmfObject;
begin
  ParamsToArc(AParams, 0, R, startPt, endPt);
  if FCurrArcDirection = AD_CLOCKWISE then
  begin
    tmp := startPt;
    startPt := endPt;
    endPt := tmp;
  end;
  lmfItem := TlmfArcTo.Create(R, startPt, endpt);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadBeginPath;
begin
  FCurrPath := TlmfPath.Create(Rect(0, 0, -1, -1));
  FCurrPath.PolyFillMode := FCurrPolyFillMode;
  FImage.List.InsertComponent(FCurrPath);
end;

procedure TlmfEMFReader.ReadBitBLT(const AParams: TEMFParamArray);
var
  bmp: TBitmap;
  offsHeader, sizeHeader: Integer;
  offsBits, sizeBits: Integer;
  bounds: TRect;
  bitBltRasterOp: Integer;
  xSrc, ySrc: Integer;
  destRect: TRect;
  destWidth, destHeight: Integer;
  bkClr: TColor;
  lmfItem: TlmfObject;
  lmfPic: TlmfPicture;
  //rec: PEMFBitBLTRecord;
begin
  ParamsToRect(AParams, 0, bounds);
  ParamsToPointL(AParams, 16, destRect.TopLeft);
  ParamsToInt(AParams, 24, destWidth);
  ParamsToInt(AParams, 28, destHeight);
  ParamsToInt(AParams, 32, bitBltRasterOp);
  ParamsToInt(AParams, 36, xSrc);
  ParamsToInt(AParams, 40, ySrc);
  ParamsToColor(AParams, 68, bkClr);
  ParamsToInt(AParams, 76, offsHeader);
  ParamsToInt(AParams, 80, sizeHeader);
  ParamsToInt(AParams, 84, offsBits);
  ParamsToInt(AParams, 88, sizeBits);

  //rec := PEMFBitBLTRecord(@AParams[0]);

  bmp := ReadImage(AParams, offsHeader-8, sizeHeader, offsBits-8, sizeBits);
  if bmp = nil then
    exit;

  if TCopyMode(bitBltRasterOp) <> FCurrCopyMode then
  begin
    FCurrCopyMode := TCopyMode(bitBltRasterOp);
    lmfItem := TlmfCopyMode.Create(FCurrCopyMode);
    FImage.List.InsertComponent(lmfItem);
  end;

  lmfPic := TlmfPicture.Create(nil);
  destRect.BottomRight := destRect.TopLeft + Point(destWidth, destHeight);
  lmfPic.clip := destRect;
  lmfPic.PixelsPerInch := ScreenInfo.PixelsPerInchX;
  lmfPic.TransparentColor := bkClr;
  lmfPic.Picture.Bitmap.Assign(bmp);
//  lmfPic.SrcRect := Rect(xSrc, ySrc, xSrc + bmp.Width, ySrc + bmp.Height);
  FImage.List.InsertComponent(lmfPic);

  bmp.Free;
end;

procedure TlmfEMFReader.ReadChord(const AParams: TEMFParamArray);
var
  R: TRect;
  startPt, endPt: TPoint;
  lmfItem: TlmfChord;
begin
  ParamsToArc(AParams, 0, R, startPt, endPt);
  case FCurrArcDirection of
    AD_COUNTERCLOCKWISE: lmfItem := TlmfChord.Create(R, startPt, endPt);
    AD_CLOCKWISE: lmfItem := TlmfChord.Create(R, endPt, startpt);
  end;
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadCloseFigure;
begin
  if Assigned(FCurrPath) then
    FCurrPath.ClosePath;
end;

procedure TlmfEMFReader.ReadCreateBrushIndirect(const AParams: TEMFParamArray);
var
  lmfBrush: TlmfBrush;
  brushRec: PEMFBrushRecord;
  idx: integer;
begin
  ParamsToInt(AParams, 0, idx);
  if idx < 0 then
    exit;

  lmfBrush := TlmfBrush.Create(nil);
  brushRec := PEMFBrushRecord(@AParams[4]);

    // Brush color (must be set before Style, otherwise style would be reset to solid)
  lmfBrush.Brush.Color := RGBToColor(brushRec^.ColorRED, brushRec^.ColorGREEN, brushRec^.ColorBLUE);

  // Brush style
  case LEToN(brushRec^.BrushStyle) of
    BS_SOLID:
      lmfBrush.Brush.Style := bsSolid;
    BS_NULL:
      lmfBrush.Brush.Style := bsClear;
    BS_HATCHED:
      case brushRec^.BrushHatch of
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

  // Add to EMF object table
  AddToObjTable(idx, lmfBrush);
end;

procedure TlmfEMFReader.ReadCreateDIBPatternBrush(const AParams: TEMFParamArray);
var
  idx: Integer;
  offsHeader: Integer;
  sizeHeader: Integer;
  offsBits: Integer;
  sizeBits: Integer;
  bmp: TBitmap;
  lmfBrush: TlmfBrush;
begin
  ParamsToInt(AParams, 0, idx);
  if idx < 0 then
    exit;

  ParamsToInt(AParams, 8, offsHeader);
  ParamsToInt(AParams, 12, sizeHeader);
  ParamsToInt(AParams, 16, offsBits);
  ParamsToInt(AParams, 20, sizeBits);
  bmp := ReadImage(AParams, offsHeader-8, sizeHeader, offsBits-8, sizeBits);
  if bmp = nil then
    exit;

  lmfBrush := TlmfBrush.Create(nil);
  lmfBrush.Brush.Style := bsImage;
  lmfBrush.Brush.Bitmap := bmp;
  FImage.List.InsertComponent(lmfBrush);

  // Add to EMF object table
  AddToObjTable(idx, lmfBrush);

end;

procedure TlmfEMFReader.ReadCreatePen(const AParams: TEMFParamArray);
var
  penRec: PEMFLogPenRecord;
  lmfPen: TlmfPen;
  style: Word;
  idx: Integer;
begin
  ParamsToInt(AParams, 0, idx);
  if idx < 0 then   // stock objects
    exit;

  lmfPen := TlmfPen.Create(nil);
  penRec := PEMFLogPenRecord(@AParams[4]);

  // Pen style
  style := LEToN(penRec^.PenStyle);
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

  // Add to EMF object list
  AddToObjTable(idx, lmfPen);
end;

procedure TlmfEMFReader.ReadDeleteObject(const AParams: TEMFParamArray);
var
  idx: Integer;
begin
  ParamsToInt(AParams, 0, idx);
  if (idx >= 0) and (idx < FObjTable.Count) then
  begin
    FObjTable[idx] := nil;
    // Do not delete from ObjTable's list because this will confuse the obj indexes.
    // Only mark the deleted obj item as nil so that the index can be re-used.
    // Also: Do not delete from FImage.List.
  end;
end;

procedure TlmfEMFReader.ReadEllipse(const AParams: TEMFParamArray);
var
  lmfItem: TlmfEllipse;
  R: TRect;
begin
  ParamsToRect(AParams, 0, R);
  lmfItem := TlmfEllipse.Create(R);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadEndPath;
begin
  if Assigned(FCurrPath) then
    FCurrPath.EndPath;
end;

procedure TlmfEMFReader.ReadExtCreateFontIndirect(const AParams: TEMFParamArray);
var
  fontRec: PEMFLogFontRecord;
  lmfFont: TlmfFont;
  idx: Integer;
  wName: WideString;
begin
  ParamsToInt(AParams, 0, idx);
  if idx < 0 then
    exit;

  fontRec := PEMFLogFontRecord(@AParams[4]);
  // Get font name
  wName := PWideChar(fontRec^.FaceName);
  SetLength(wName, StrLen(PWideChar(wName)));

  lmfFont := TlmfFont.Create(nil);
  lmfFont.Font.Name := wName;
  lmfFont.Height := abs(round(LongInt(LEToN(fontRec^.Height))));
//  lmfFont.Font.Height := -FImage.ScaleSizeY(lmfFont.Height);
//  lmfFont.Font.Height := round(SmallInt(LEToN(fontRec^.Height)));
  lmfFont.Font.Color := FCurrTextColor;
  lmfFont.Font.Bold := LEToN(fontRec^.Weight) >= 700;
  lmfFont.Font.Italic := fontRec^.Italic <> 0;
  lmfFont.Font.Underline := fontRec^.UnderLine <> 0;
  lmfFont.Font.StrikeThrough := fontRec^.Strikeout <> 0;
  lmfFont.Font.Orientation := LEToN(fontRec^.Escapement);
  lmfFont.Font.CharSet := fontrec^.CharSet;

  // Add to metafile list
  FImage.List.InsertComponent(lmfFont);

  // Add to EMF object list
  AddToObjTable(idx, lmfFont);
end;


procedure TlmfEMFReader.ReadExtCreatePen(const AParams: TEMFParamArray);
var
  penRec: PEMFLogPenExRecord;
  lmfPen: TlmfPen;
  style: Word;
  idx: Integer;
begin
  ParamsToInt(AParams, 0, idx);
  if idx < 0 then  // this is for stock objects
    exit;

  lmfPen := TlmfPen.Create(nil);
  penRec := PEMFLogPenExRecord(@AParams[20]);

  // Pen style
  style := LEToN(penRec^.PenStyle);
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

  // Add to EMF object list
  AddToObjTable(idx, lmfPen);
end;

{ ACharMode = 0 --> AnsiChars
  ACharMode = 1 --> WideChars
  ACharMode = 2 --> WideChars with high-byte = 0, i.e. String contains 8-bit chars.}
procedure TlmfEMFReader.ReadExtTextOut(const AParams: TEMFParamArray; ACharMode: Byte);
var
  pText: PEMFTextRecord;
  pSmallText: PEMFSmallTextOutRecord;
  len: Integer;
  offsText: DWord;
  opts: DWord;
  x, y, i: Integer;
  R: TRect;
  txt: String = '';
  wTxt: WideString = '';
  txtStyle: TTextStyle;
  item: TlmfObject;
begin
  if ACharMode in [0, 1] then
  begin
    pText := PEMFTextRecord(@AParams[28]);
    x := LEToN(pText^.RefPtX);
    y := LEToN(pText^.RefPtY);
    R.Left := LEtoN(pText^.RectLeft);
    R.Top := LEtoN(pText^.RectTop);
    R.Right := LEtoN(pText^.RectRight);
    R.Bottom := LEtoN(pText^.RectBottom);
    opts := LEToN(pText^.Options);
    len := LEToN(pText^.NumChars);
    offsText := LEToN(pText^.OffsToString) - 8;  // -8 for EMSRecord
  end
  else
  begin
    pSmallText := PEMFSmallTextOutRecord(@AParams[0]);
    x := LEToN(pSmallText^.RefPtX);
    y := LEToN(pSmallText^.RefPtY);
    opts := LEToN(pSmallText^.Options);
    len := LEToN(pSmallText^.NumChars);
    offsText := SizeOf(TEMFSmallTextOutRecord);
    if opts and ETO_NO_RECT = 0 then
    begin
      R.Left := LEtoN(pSmallText^.RectLeft);
      R.Top := LEtoN(pSmallText^.RectTop);
      R.Right := LEtoN(pSmallText^.RectRight);
      R.Bottom := LEtoN(pSmallText^.RectBottom);
    end else
      offsText := offsText - SizeOf(TEMFRectLRecord);
  end;
  case ACharMode of
    0: begin   // AnsiChars
         SetLength(txt, len);
         Move(AParams[offsText], txt[1], len);
         SetLength(txt, StrLen(PChar(txt)));
       end;
    1: begin   // WideChars
         SetLength(wTxt, len);
         Move(AParams[offsText], wTxt[1], len*2);
         txt := wTxt;
       end;
    2: if (opts and ETO_SMALL_CHARS <> 0) then
       begin
         SetLength(txt, len);
         Move(AParams[offsText], txt[1], len);
         SetLength(wTxt, len);
         for i := 1 to len do
           wTxt[i] := WideChar(txt[i]);
         txt := wTxt;
       end else
       begin
         SetLength(wTxt, len);
         Move(AParams[offsText], wTxt[1], len*2);
         txt := wTxt;
       end;
  end;

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
    TA_LEFT: txtStyle.Alignment := taLeftJustify;
    TA_CENTER: txtStyle.Alignment := taCenter;
    TA_RIGHT: txtStyle.Alignment := taRightJustify;
  end;

  item := TlmfTextInRect.Create(R, x, y, txt, txtStyle);
  FImage.List.InsertComponent(item);
end;

procedure TlmfEMFReader.ReadFillPath(const AParams: TEMFParamArray);
var
  R: TRect;
begin
  if Assigned(FCurrPath) then
  begin
    ParamsToRect(AParams, 0, R);
    FCurrPath.Clip := R;
    FCurrPath.FillStrokeMode := fsmFill;
    FCurrPath := nil;
  end;
end;

procedure TlmfEMFReader.ReadFlattenPath;
begin
  if Assigned(FCurrPath) then
    FCurrPath.FlattenPath;
end;

procedure TlmfEMFReader.ReadGradientFill(const AParams: TEMFParamArray);
var
  bounds: TRect;
  nVer: Integer;    // Number of vertixes
  nTri: Integer;    // Number of triangles
  mode: Integer;    // Gradient fill mode: 0=Rect_H, 1=Rect_V, 2=Triangle
  idxVertices: Integer;
  idxMeshes: Integer;
  lmfItem: TlmfMultiGradientFill;
begin
  ParamsToRect(AParams, 0, bounds);
  ParamsToInt(AParams, 16, nVer);
  ParamsToInt(AParams, 20, nTri);
  ParamsToInt(AParams, 24, mode);

  idxVertices := 28;
  idxMeshes := idxVertices + nVer * SizeOf(TTriVertex);

  case mode of
    GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V:
      lmfItem := TlmfMultiGradientFill.Create(
        @AParams[idxVertices], nVer,
        @AParams[idxMeshes], nTri,
        mode
      );
    GRADIENT_FILL_TRIANGLE:
      lmfItem := TlmfMultiGradientFill.Create(
        @AParams[idxVertices], nVer,
        @AParams[idxMeshes], nTri
      );
  end;
  FImage.List.InsertComponent(lmfItem);
end;

function TlmfEMFReader.ReadImage(const AParams: TEMFParamArray;
  AOffsetToHeader, ASizeOfHeader, AOffsetToImageBits, ASizeOfImageBits: DWord): TBitmap;
var
  memstream: TMemoryStream;
  bmpFileHdr: TBitmapFileHeader;
//  bmpCoreHdr: PBitmapCoreHeader;
  bmpInfoHdr: PBitmapInfoHeader;
  hasCoreHdr: Boolean;
  compression: DWord;
  w, h: Integer;
begin
//  bmpCoreHdr := PBitmapCoreHeader(@FBuffer[AOffsetToHeader]);
  bmpInfoHdr := PBitmapInfoHeader(@AParams[AOffsetToHeader]);
  hasCoreHdr := ASizeOfHeader = SizeOf(TBitmapCoreHeader);
  if hasCoreHdr then
    exit(nil);

  w := bmpInfoHdr^.biWidth;
  h := bmpInfoHdr^.biHeight;
  if (w = 0) or (h = 0) then
    exit(nil);

  compression := bmpInfoHdr^.biCompression;
  if compression in [BI_JPEG, BI_PNG] then  // Not implemented in TBitmap
    exit(nil);

  memstream := TMemoryStream.Create;
  try
    // Put a bitmap file header before the buffer header and the image bits
    bmpFileHdr.bfType := BMmagic;
    bmpFileHdr.bfSize:= SizeOf(bmpFileHdr) + ASizeOfHeader + ASizeOfImageBits;
    bmpFileHdr.bfOffBits := bmpFileHdr.bfSize - ASizeOfImageBits;
    bmpFileHdr.bfReserved1 := 0;
    bmpFileHdr.bfReserved2 := 0;
    memstream.WriteBuffer(bmpFileHdr, SizeOf(bmpFileHdr));
    memstream.WriteBuffer(AParams[AOffsetToHeader], ASizeOfHeader);
    memstream.WriteBuffer(AParams[AOffsetToImageBits], ASizeOfImageBits);
    memstream.Position := 0;
    Result := TBitmap.Create;
    Result.SetSize(abs(w), abs(h));
    {
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(0, 0, w, h);
    }
    Result.LoadFromStream(memStream);
  finally
    memstream.Free;
  end;
end;

function TlmfEMFReader.ReadHeader(AStream: TStream): Boolean;
var
  hdr: TEnhancedMetaHeader;
  n: Int64;
begin
  Result := false;
  hdr := Default(TEnhancedMetaHeader);

  n := AStream.Read(hdr, SizeOf(hdr));
  if n < SizeOf(hdr) then
  begin
    LogError('Header size error');
    exit;
  end;

  if (LEToN(hdr.Signature) <> $464D4520) or (LEtoN(hdr.Version) <> $00010000) then
  begin
    LogError('Incorrect header');
    exit;
  end;

  // Bounding box in logical units
  FBBox.Left := LEToN(hdr.BoundsLeft);
  FBBox.Top := LEToN(hdr.BoundsTop);
  FBBox.Right := LEToN(hdr.BoundsRight);
  FBBox.Bottom := LEToN(hdr.BoundsBottom);

  // in 0.01 mm
  FBBoxMM.Left := LEToN(hdr.FrameLeft);
  FBBoxMM.Top := LEToN(hdr.FrameTop);
  FBBoxMM.Right := LEToN(hdr.FrameRight);
  FBBoxMM.Bottom := LEToN(hdr.FrameBottom);

  FImage.Width := FBBox.Right - FBBox.Left;
  FImage.Height := FBBox.Bottom - FBBox.Top;
  FImage.LogUnitsPerInch := TWIPS_PER_INCH;
//  FImage.LogUnitsPerInch := round((FBBox.Right - FBBox.Left) / (FBBoxMM.Right - FBBoxMM.Left) * 2540);

  Result := true;
end;

procedure TlmfEMFReader.ReadLineTo(const AParams: TEMFParamArray);
var
  pt: TPoint;
  lmfItem: TlmfLineTo;
begin
  ParamsToPointL(AParams, 0, pt);
  if Assigned(FCurrPath) then
    FCurrPath.AddLineTo(pt)
  else
  begin
    lmfItem := TlmfLineTo.Create(pt.X, pt.Y);
    FImage.List.InsertComponent(lmfItem);
  end;
end;

procedure TlmfEMFReader.ReadMoveToEx(const AParams: TEMFParamArray);
var
  pt: TPoint;
  lmfItem: TlmfMoveTo;
begin
  ParamsToPointL(AParams, 0, pt);
  if Assigned(FCurrPath) then
    FCurrPath.AddMoveTo(pt)
  else
  begin
    lmfItem := TlmfMoveTo.Create(pt.X, pt.Y);
    FImage.List.InsertComponent(lmfItem);
  end;
end;

procedure TlmfEMFReader.ReadPie(const AParams: TEMFParamArray);
var
  R: TRect;
  startPt, endPt: TPoint;
  lmfItem: TlmfPie;
begin
  ParamsToArc(AParams, 0, R, startPt, endPt);
  case FCurrArcDirection of
    AD_COUNTERCLOCKWISE: lmfItem := TlmfPie.Create(R, startPt, endPt);
    AD_CLOCKWISE: lmfItem := TlmfPie.Create(R, endPt, startpt);
  end;
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadPolyBezier(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  Bounds: TRect;
  lmfItem: TlmfPolyBezier;
  pts: TPointArray = nil;
begin
  ParamsToRect(AParams, 0, Bounds);
  if SmallPts then
    ParamsToPointSArray(AParams, 16, pts)
  else
    ParamsToPointLArray(AParams, 16, pts);
  if Assigned(FCurrPath) then
    FCurrPath.AddPolyBezier(@pts[0], Length(pts))
  else
  begin
    lmfItem := TlmfPolyBezier.Create(@pts[0], Length(pts));
    lmfItem.StartsAtPenPos := false;  // first point is included in pts
    FImage.List.InsertComponent(lmfItem);
  end;
end;

procedure TlmfEMFReader.ReadPolyBezierTo(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  Bounds: TRect;
  lmfItem: TlmfPolyBezier;
  pts: TPointArray = nil;
begin
  ParamsToRect(AParams, 0, Bounds);
  if SmallPts then
    ParamsToPointSArray(AParams, 16, pts)
  else
    ParamsToPointLArray(AParams, 16, pts);
  if Assigned(FCurrPath) then
    FCurrPath.AddPolyBezierTo(@pts[0], Length(pts))
  else
  begin
    lmfItem := TlmfPolyBezier.Create(@pts[0], Length(pts));
    lmfItem.StartsAtPenPos := true;  // first point is NOT included in pts, must be read from Canvas.PenPos
    FImage.List.InsertComponent(lmfItem);
  end;
end;

procedure TlmfEMFReader.ReadPolygon(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  Bounds: TRect;
  lmfItem: TlmfPolygon;
  pts: TPointArray = nil;
begin
  ParamsToRect(AParams, 0, Bounds);
  if SmallPts then
    ParamsToPointSArray(AParams, 16, pts)
  else
    ParamsToPointLArray(AParams, 16, pts);
  if Assigned(FCurrPath) then
    FCurrPath.AddPolygon(@pts[0], Length(pts))
  else
  begin
    lmfItem := TlmfPolygon.Create(@pts[0], Length(pts), FCurrPolyFillMode=WINDING);
    FImage.List.InsertComponent(lmfItem);
  end;
end;

procedure TlmfEMFReader.ReadPolyLine(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  Bounds: TRect;
  lmfItem: TlmfPolyLine;
  pts: TPointArray = nil;
begin
  ParamsToRect(AParams, 0, Bounds);
  if SmallPts then
    ParamsToPointSArray(AParams, 16, pts)
  else
    ParamsToPointLArray(AParams, 16, pts);
  if Assigned(FCurrPath) then
    FCurrPath.AddPolyLine(@pts[0], Length(pts))
  else
  begin
    lmfItem := TlmfPolyline.Create(@pts[0], Length(pts));
    FImage.List.InsertComponent(lmfItem);
  end;
end;

procedure TlmfEMFReader.ReadPolyLineTo(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  Bounds: TRect;
  lmfItem: TlmfObject;
  pts: TPointArray = nil;
  lastPt: TPoint;
begin
  ParamsToRect(AParams, 0, Bounds);
  if SmallPts then
    ParamsToPointSArray(AParams, 16, pts)
  else
    ParamsToPointLArray(AParams, 16, pts);
  lastPt := pts[High(pts)];
  if Assigned(FCurrPath) then
  begin
    FCurrPath.AddPolyLineTo(@pts[0], Length(pts));
//    FCurrPath.AddMoveTo(lastPt);
  end else
  begin
    lmfItem := TlmfPolyline.Create(@pts[0], Length(pts));
    FImage.List.InsertComponent(lmfItem);
    lmfItem := TlmfMoveTo.Create(lastPt.X, lastPt.Y);
    FImage.List.InsertComponent(lmfItem);
  end;
end;

// To do: this does not take care of polygons with holes...
procedure TlmfEMFReader.ReadPolyPolygon(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  lmfPolygon: TlmfPolygon;
  nPolygons: Integer;
  totalPts: Integer;
  ptsPerPolygon: Array of DWord = nil;
  pts: TPointArray = nil;
  i, j: Integer;
begin
  ParamsToInt(AParams, 16, nPolygons);
  ParamsToInt(AParams, 20, totalPts);
  SetLength(ptsPerPolygon, nPolygons);
  Move(AParams[24], ptsPerPolygon[0], nPolygons * SizeOf(DWord));

  j := 24 + nPolygons * SizeOf(DWord);
  for i := 0 to nPolygons-1 do
  begin
    if SmallPts then
      ParamsToPointSArray(AParams, j, ptsPerPolygon[i], pts)
    else
      ParamsToPointLArray(AParams, j, ptsPerPolygon[i], pts);
    lmfPolygon := TlmfPolygon.Create(@pts[0], ptsPerPolygon[i]);
    fImage.List.InsertComponent(lmfPolygon);
    if SmallPts then
      inc(j, ptsPerPolygon[i] * SizeOf(TEMFPointSRecord))
    else
      inc(j, ptsPerPolygon[i] * SizeOf(TEMFPointLRecord));
  end;
end;

procedure TlmfEMFReader.ReadPolyPolyLine(const AParams: TEMFParamArray; SmallPts: Boolean);
var
  lmfPolyLine: TlmfPolyLine;
  nPolyLines: Integer;
  totalPts: Integer;
  ptsPerPolyLine: Array of DWord = nil;
  pts: TPointArray = nil;
  i, j: Integer;
begin
  ParamsToInt(AParams, 16, nPolyLines);
  ParamsToInt(AParams, 20, totalPts);
  SetLength(ptsPerPolyLine, nPolyLines);
  Move(AParams[24], ptsPerPolyLine[0], nPolyLines * SizeOf(DWord));

  j := 24 + nPolyLines * SizeOf(DWord);
  for i := 0 to nPolyLines-1 do
  begin
    if SmallPts then
      ParamsToPointSArray(AParams, j, ptsPerPolyLine[i], pts)
    else
      ParamsToPointLArray(AParams, j, ptsPerPolyLine[i], pts);
    lmfPolyLine := TlmfPolyLine.Create(@pts[0], ptsPerPolyLine[i]);
    fImage.List.InsertComponent(lmfPolyLine);
    if SmallPts then
      inc(j, ptsPerPolyLine[i] * SizeOf(TEMFPointSRecord))
    else
      inc(j, ptsPerPolyLine[i] * SizeOf(TEMFPointLRecord));
  end;
(*
  SetLength(pts, totalPts);
  if SmallPts then
    Move(AParams[24 + nPolyLines * SizeOf(Word)], pts[0], totalPts * SizeOf(TPoint))
  else
    Move(AParams[24 + nPolyLines * SizeOf(DWord)], pts[0], totalPts * SizeOf(TPoint));

  j := 0;
  for i := 0 to nPolyLines-1 do
  begin
    lmfPolyLine := TlmfPolyLine.Create(@pts[j], ptsPerPolyLine[i]);
    lmfPolyLine.StartsAtPenPos := false;
    fImage.List.InsertComponent(lmfPolyLine);
    inc(j, ptsPerPolyLine[i]);
  end;
  *)
end;

procedure TlmfEMFReader.ReadRecords(AStream: TStream);
var
  recordStartPos: Int64;
  emfRec: TEMFRecord;
  params: TEMFParamArray = nil;
  n: Integer;
begin
  if not ReadHeader(AStream) then
    exit;

  emfRec := Default(TEMFRecord);
  AStream.Position := 0;
  while AStream.Position < AStream.Size do begin
    // Store the stream position where the current record begins
    recordStartPos := AStream.Position;

    // Read record size and function code
    n := AStream.Read(emfRec, SizeOf(TEMFRecord));
    if n <> SizeOf(TEMFRecord) then
      raise ElmfReader.CreateFmt('Record size error (at offset %d).', [recordStartPos]);

    emfRec.Func := LEToN(emfRec.Func);
    emfRec.Size := LEToN(emfRec.Size);

    // End of file?
    if emfRec.Func = EMR_EOF then
      break;

    // Obviously invalid record?
    if emfRec.Size < 8 then begin
      LogError(Format('Record size error, record at offset %d, function %d', [recordStartPos, emfRec.Func]));
      exit;
    end;

    // Read record parameters into byte array; Func and Size are not included.
    SetLength(params, emfRec.Size - 8);
    n := AStream.Read(params[0], (emfRec.Size - 8));
    if n <> (emfRec.Size - 8) then
      raise ElmfReader.CreateFmt('Record parameter size error, record at offset %d, function %d', [recordStartPos, emfRec.Func]);

    // Process record, depending on function code
    case emfRec.Func of
      EMR_ABORTPATH:
        ReadAbortPath;
      EMR_ARC:
        ReadArc(params);
      EMR_ARCTO:
        ReadArcTo(params);
      EMR_BEGINPATH:
        ReadBeginPath;
      EMR_BITBLT:
        ReadBitBLT(params);
      EMR_CHORD:
        ReadChord(params);
      EMR_CLOSEFIGURE:
        ReadCloseFigure;
      EMR_CREATEBRUSHINDIRECT:
        ReadCreateBrushIndirect(params);
      EMR_CREATEPEN:
        ReadCreatePen(params);
      EMR_CREATEDIBPATTERNBRUSHPT:
        ReadCreateDIBPatternBrush(params);
      EMR_CREATEMONOBRUSH:
        ReadCreateDIBPatternBrush(params);
      EMR_ELLIPSE:
        ReadEllipse(params);
      EMR_ENDPATH:
        ReadEndPath;
      EMR_EXTCREATEFONTINDIRECTW:
        ReadExtCreateFontIndirect(params);
      EMR_EXTCREATEPEN:
        ReadExtCreatePen(params);
      EMR_EXTTEXTOUTA:
        ReadExtTextOut(params, 0);
      EMR_EXTTEXTOUTW:
        ReadExtTextOut(params, 1);
      EMR_FLATTENPATH:
        ReadFlattenPath;
      EMR_FILLPATH:
        ReadFillPath(params);
      EMR_GRADIENTFILL:
        ReadGradientFill(params);
      EMR_HEADER:
        ; // Nothing to do, has already been handled.
      EMR_LINETO:
        ReadLineTo(params);
      EMR_MOVETOEX:
        ReadMoveToEx(params);
      EMR_PIE:
        ReadPie(params);
      EMR_POLYBEZIER:
        ReadPolyBezier(params, false);
      EMR_POLYBEZIER16:
        ReadPolyBezier(params, true);
      EMR_POLYBEZIERTO:
        ReadPolyBezierTo(params, false);
      EMR_POLYBEZIERTO16:
        ReadPolyBezierTo(params, true);
      EMR_POLYGON:
        ReadPolygon(params, false);
      EMR_POLYGON16:
        ReadPolygon(params, true);
      EMR_POLYLINE:
        ReadPolyLine(params, false);
      EMR_POLYLINE16:
        ReadPolyLine(params, true);
      EMR_POLYLINETO:
        ReadPolyLineTo(params, false);
      EMR_POLYLINETO16:
        ReadPolyLineTo(params, true);
      EMR_POLYPOLYGON:
        ReadPolyPolygon(params, false);
      EMR_POLYPOLYGON16:
        ReadPolyPolygon(params, true);
      EMR_POLYPOLYLINE:
        ReadPolyPolyLine(params, false);
      EMR_POLYPOLYLINE16:
        ReadPolyPolyLine(params, true);
      EMR_RECTANGLE:
        ReadRectangle(params);
      EMR_ROUNDRECT:
        ReadRoundRect(params);
      EMR_SELECTOBJECT:
        ReadSelectObject(params);
      EMR_SETARCDIRECTION:
        ReadSetArcDirection(params);
      EMR_SETBKCOLOR:
        ReadSetBkColor(params);
      EMR_SETBKMODE:
        ReadSetBkMode(params);
      EMR_SETPOLYFILLMODE:
        ReadSetPolyFillMode(params);
      EMR_SETROP2:
        ReadSetROP2(params);
      EMR_SETTEXTALIGN:
        ReadSetTextAlign(params);
      EMR_SETTEXTCOLOR:
        ReadSetTextColor(params);
      EMR_SMALLTEXTOUT:
        ReadExtTextOut(params, 2);
      EMR_STRETCHBLT:
        ReadStretchBLT(params);
      EMR_STRETCHDIBITS:
        ReadStretchDIBits(params);
      EMR_STROKEANDFILLPATH:
        ReadStrokeAndFillPath(params);
      EMR_STROKEPATH:
        ReadStrokePath(params);
      EMR_WIDENPATH:
        ReadWidenPath;
    end;
    AStream.Position := recordStartPos + Int64(emfRec.Size);
  end;
                                                           (*
  if (FLogWidth = 0) and (FLogHeight = 0) then
    MeasureLogExtent;

  FImage.SetLogBounds(FLogOrgX, FLogOrgY, FLogWidth, FLogHeight);
  *)
  FImage.SetLogBounds(FBBox.Left, FBBox.Top, FBBox.Right-FBBox.Left, FBBox.Bottom-FBBox.Top);
end;

procedure TlmfEMFReader.ReadRectangle(const AParams: TEMFParamArray);
var
  lmfItem: TlmfRect;
  R: TRect;
begin
  ParamsToRect(AParams, 0, R);
  lmfItem := TlmfRect.Create(R);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadRoundRect(const AParams: TEMFParamArray);
var
  lmfItem: TlmfRoundRect;
  R: TRect;
  P: TPoint;
begin
  ParamsToRect(AParams, 0, R);
  ParamsToPointL(AParams, 16, P);  // Rx, Ry
  lmfItem := TlmfRoundRect.Create(R, P.X, P.Y);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadSelectObject(const AParams: TEMFParamArray);
var
  idx: Integer;
  stockIdx: Word;
  item, newItem: TlmfObject;
  pen: TPen;
  brush: TBrush;
  font: TFont;
begin
  ParamsToInt(AParams, 0, idx);
  if DWord(idx) >= $80000000 then  // Stock object
  begin
    item := nil;
    stockIdx := DWord(idx) - $80000000;
    case stockIdx of
      WHITE_BRUSH, LTGRAY_BRUSH, GRAY_BRUSH, DKGRAY_BRUSH, BLACK_BRUSH,
      NULL_BRUSH, DC_BRUSH:
        begin
          brush := TBrush.Create;
          try
            StockObjectToBrush(stockIdx, brush);
            FCurrBrush.Assign(brush);
            item := TlmfBrush.Create(nil);
            TlmfBrush(item).Brush.Assign(brush);
            FImage.List.InsertComponent(item);
          finally
            brush.Free;
          end;
        end;
      WHITE_PEN, BLACK_PEN, NULL_PEN, DC_PEN:
        begin
          pen := TPen.Create;
          try
            StockObjectToPen(stockIdx, pen);
            FCurrPen.Assign(pen);
            item := TlmfPen.Create(nil);
            TlmfPen(item).Pen.Assign(pen);
            FImage.List.InsertComponent(item);
          finally
            pen.Free;
          end
        end;
      OEM_FIXED_FONT, ANSI_FIXED_FONT, ANSI_VAR_FONT, SYSTEM_FONT,
      DEVICE_DEFAULT_FONT, SYSTEM_FIXED_FONT, DEFAULT_GUI_FONT:
        begin
          font := TFont.Create;
          try
            StockObjectToFont(stockIdx, font);
            FCurrFont.Assign(font);
            item := TlmfFont.Create(nil);
            TlmfFont(item).Font.Assign(font);
            FImage.List.InsertComponent(item);
          finally
            font.Free;
          end;
        end;
      DEFAULT_PALETTE:
        ;  // to do...
    end;
  end else
  begin
    if (idx >= FObjTable.Count) then
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
      ;  // others to be added
  end;

  if item <> nil then
  begin
    newItem := TlmfSelectObject.Create(item);
    FImage.List.InsertComponent(newItem);
  end;
end;

procedure TlmfEMFReader.ReadSetArcDirection(const AParams: TEMFParamArray);
begin
  ParamsToInt(AParams, 0, FCurrArcDirection);
end;

procedure TlmfEMFReader.ReadSetBkColor(const AParams: TEMFParamArray);
var
  item: TlmfBkColor;
begin
  ParamsToColor(AParams, 0, FCurrBkColor);
  item := TlmfBkColor.Create(FCurrBkColor);
  FImage.List.InsertComponent(item);
end;

procedure TlmfEMFReader.ReadSetBkMode(const AParams: TEMFParamArray);
var
  item: TlmfBkMode;
begin
  ParamsToInt(AParams, 0, FCurrBkMode);
  item := TlmfBkMode.Create(FCurrBkMode);
  FImage.List.InsertComponent(item);
end;

procedure TlmfEMFReader.ReadSetPolyFillMode(const AParams: TEMFParamArray);
begin
  ParamsToInt(AParams, 0, FCurrPolyFillMode);
end;

procedure TlmfEMFReader.ReadSetROP2(const AParams: TEMFParamArray);
var
  rop2Mode: Integer;
  penMode: TPenMode;
  lmfItem: TlmfObject;
begin
  ParamsToInt(AParams, 0, rop2Mode);
  case rop2Mode of
    R2_BLACK       : penMode := pmBlack;
    R2_NOTMERGEPEN : penMode := pmNotMerge;
    R2_MASKNOTPEN  : penMode := pmMaskNotPen;
    R2_NOTCOPYPEN  : penMode := pmNotCopy;
    R2_MASKPENNOT  : penMode := pmMaskPenNot;
    R2_NOT         : penMode := pmNot;
    R2_XORPEN      : penMode := pmXor;
    R2_NOTMASKPEN  : penMode := pmNotMask;
    R2_MASKPEN     : penMode := pmMask;
    R2_NOTXORPEN   : penMode := pmNotXor;
    R2_NOP         : penMode := pmNOP;
    R2_MERGENOTPEN : penMode := pmMergeNotPen;
    R2_COPYPEN     : penMode := pmCopy;
    R2_MERGEPENNOT : penMode := pmMergePenNot;
    R2_MERGEPEN    : penMode := pmMerge;
    R2_WHITE       : penMode := pmWhite;
  end;
  lmfItem := TlmfPenMode.Create(penMode);
  FImage.List.InsertComponent(lmfItem);
end;

procedure TlmfEMFReader.ReadSetTextAlign(const AParams: TEMFParamArray);
begin
  ParamsToInt(AParams, 0, FCurrTextAlign);
end;

procedure TlmfEMFReader.ReadSetTextColor(const AParams: TEMFParamArray);
var
  item: TlmfTextColor;
begin
  ParamsToColor(AParams, 0, FCurrTextColor);
  item := TlmfTextColor.Create(FCurrTextColor);
  FImage.List.InsertComponent(item);
end;

procedure TlmfEMFReader.ReadStretchBlt(const AParams: TEMFParamArray);
var
  bmp: TBitmap;
  offsHeader, sizeHeader: Integer;
  offsBits, sizeBits: Integer;
  bitBltRasterOp: Integer;
  bounds: TRect;
  srcRect: TRect;
  destRect: TRect;
  w, h: Integer;
  bkClr: TColor;
  lmfItem: TlmfObject;
  lmfPic: TlmfPicture;
begin
  ParamsToRect(AParams, 0, bounds);
  ParamsToPointL(AParams, 16, destRect.TopLeft);
  ParamsToInt(AParams, 24, w);
  ParamsToInt(AParams, 28, h);
  destRect.BottomRight := destRect.TopLeft + Point(w, h);
  ParamsToInt(AParams, 32, bitBltRasterOp);
  ParamsToPointL(AParams, 36, srcRect.TopLeft);
  ParamsToColor(AParams, 68, bkClr);
  ParamsToInt(AParams, 76, offsHeader);
  ParamsToInt(AParams, 80, sizeHeader);
  ParamsToInt(AParams, 84, offsBits);
  ParamsToInt(AParams, 88, sizeBits);
  ParamsToInt(AParams, 92, w);
  ParamsToInt(AParams, 96, h);
  srcRect.BottomRight := srcRect.TopLeft + Point(w, h);

  bmp := ReadImage(AParams, offsHeader-8, sizeHeader, offsBits-8, sizeBits);
  if bmp = nil then
    exit;

  if TCopyMode(bitBltRasterOp) <> FCurrCopyMode then
  begin
    FCurrCopyMode := TCopyMode(bitBltRasterOp);
    lmfItem := TlmfCopyMode.Create(FCurrCopyMode);
    FImage.List.InsertComponent(lmfItem);
  end;

  lmfPic := TlmfPicture.Create(nil);
  lmfPic.clip := destRect;
  lmfPic.PixelsPerInch := ScreenInfo.PixelsPerInchX;
  lmfPic.TransparentColor := bkClr;
  lmfPic.SrcRect := srcRect;
  lmfPic.Picture.Bitmap.Assign(bmp);
  FImage.List.InsertComponent(lmfPic);

  bmp.Free;
end;

procedure TlmfEMFReader.ReadStretchDIBits(const AParams: TEMFParamArray);
var
  bmp: TBitmap;
  offsHeader, sizeHeader: Integer;
  offsBits, sizeBits: Integer;
  bitBltRasterOp: Integer;
  bounds: TRect;
  srcRect: TRect;
  srcSize: TSize;
  destPt: TPoint;
  destWidth, destHeight: Integer;
  lmfPic: TlmfPicture;
  lmfItem: TlmfObject;
  {%H-}idx: Integer;
begin
  ParamsToRect(AParams, 0, bounds);
  ParamsToPointL(AParams, 16, destPt);
  ParamsToPointL(AParams, 24, srcRect.TopLeft);
  ParamsToPointL(AParams, 32, TPoint(srcSize));
  srcRect.Right := srcRect.Left + srcSize.CX;
  srcRect.Bottom := srcRect.Top + srcSize.CY;
  ParamsToInt(AParams, 40, offsHeader);
  ParamsToInt(AParams, 44, sizeHeader);
  ParamsToInt(AParams, 48, offsBits);
  ParamsToInt(AParams, 52, sizeBits);
  ParamsToInt(AParams, 60, bitBltRasterOp);
  ParamsToInt(AParams, 64, destWidth);
  ParamsToInt(AParams, 68, destHeight);

  bmp := ReadImage(AParams, offsHeader-8, sizeHeader, offsBits-8, sizeBits);
  if bmp = nil then
    exit;

  if TCopyMode(bitBltRasterOp) <> FCurrCopyMode then
  begin
    FCurrCopyMode := TCopyMode(bitBltRasterOp);
    lmfItem := TlmfCopyMode.Create(FCurrCopyMode);
    FImage.List.InsertComponent(lmfItem);
  end;

  lmfPic := TlmfPicture.Create(nil);
  lmfPic.clip := Rect(destPt.X, destPt.Y, destPt.X + destWidth, destPt.Y + destHeight);
  lmfPic.SrcRect := srcRect;
  lmfPic.PixelsPerInch := ScreenInfo.PixelsPerInchX;
  lmfPic.Picture.Bitmap.Assign(bmp);
  FImage.List.InsertComponent(lmfPic);

  idx := FImage.List.ComponentCount-1;

  bmp.Free;
end;

procedure TlmfEMFReader.ReadStrokePath(const AParams: TEMFParamArray);
var
  R: TRect;
begin
  if Assigned(FCurrPath) then
  begin
    ParamsToRect(AParams, 0, R);
    FCurrPath.Clip := R;
    FCurrPath.FillStrokeMode := fsmStroke;
    FCurrPath := nil;
  end;
end;

procedure TlmfEMFReader.ReadStrokeAndFillPath(const AParams: TEMFParamArray);
var
  R: TRect;
begin
  if Assigned(FCurrPath) then
  begin
    ParamsToRect(AParams, 0, R);
    FCurrPath.Clip := R;
    FCurrPath.FillStrokeMode := fsmFillStroke;
    FCurrPath := nil;
  end;
end;

procedure TlmfEMFReader.ReadWidenPath;
begin
  if Assigned(FCurrPath) then
    FCurrPath.WidenPath;
end;

end.

