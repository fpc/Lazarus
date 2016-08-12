{ A fpvectorial reader for wmf files.

  Documentation used:
  - http://msdn.microsoft.com/en-us/library/cc250370.aspx
  - http://wvware.sourceforge.net/caolan/ora-wmf.html
  - http://www.symantec.com/avcenter/reference/inside.the.windows.meta.file.format.pdf

  These functions are not supported:
  - see the empty case items in "TWMFVectorialReader.ReadRecords"

  Issues:
  - fpvectorial polygon fill has the issue to always fill holes.

  Author: Werner Pamler
}

{.$DEFINE WMF_DEBUG}

unit wmfvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FPImage, FPCanvas,
  fpvectorial;

type
  TParamArray = array of word;

  TWMFObjList = class(TFPList)
  public
    function Add(AData: Pointer): Integer;
  end;

  { TvWMFVectorialReader }

  TvWMFVectorialReader = class(TvCustomVectorialReader)
  private
    // list for WMF Objects
    FObjList: TWMFObjList;
    // info from header
    FBBox: TRect;  // in metafile units as specified by UnitsPerInch. NOTE: "logical" units can be different!
    FUnitsPerInch: Integer;
    FHasPlaceableMetaHeader: Boolean;
    // state
    FCurrPen: TvPen;
    FCurrBrush: TvBrush;
    FCurrFont: TvFont;
    FCurrPalette: TFPPalette;
    FCurrTextColor: TFPColor;
    FCurrTextAlign: Word;
    FCurrPolyFillMode: Word;
    FCurrRawFontHeight: Integer;
    FBkColor: TFPColor;
    FMapMode: Word;
    FWindowOrigin: TPoint;
    FWindowExtent: TPoint;
    FRecordStartPos: Int64;
    FScalingFactorX: Double;
    FScalingFactorY: Double;
    FPageWidth: Double;
    FPageHeight: Double;
    FErrMsg: TStrings;

    procedure ClearObjList;

    function CreateBrush(const AParams: TParamArray): Integer;
    function CreateFont(const AParams: TParamArray): Integer;
    function CreatePalette(const AParams: TParamArray): Integer;
    function CreatePen(const AParams: TParamArray): Integer;
    function CreateRegion(const AParams: TParamArray): Integer;
    procedure DeleteObj(const AParams: TParamArray);
    procedure ReadArc(APage: TvVectorialpage; const AParams: TParamArray);
    procedure ReadBkColor(const AParams: TParamArray);
    procedure ReadChord(APage: TvVectorialpage; const AParams: TParamArray);
    function ReadColor(const AParams: TParamArray; AIndex: Integer): TFPColor;
    procedure ReadExtTextOut(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadEllipse(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadLine(APage: TvVectorialPage; P1X, P1Y, P2X, P2Y: SmallInt);
    procedure ReadMapMode(const AParams: TParamArray);
    procedure ReadOffsetWindowOrg(const AParams: TParamArray);
    procedure ReadPie(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadPolyFillMode(const AValue: Word);
    procedure ReadPolygon(APage: TvVectorialPage; const AParams: TParamArray;
      AFilled: boolean);
    procedure ReadPolyPolygon(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadRectangle(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadRoundRect(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadStretchDIB(AStream: TStream; APage: TvVectorialPage;
      const AParams: TParamArray);
    function ReadString(const AParams: TParamArray;
      AStartIndex, ALength: Integer): String;
    procedure ReadTextAlign(const AParams: TParamArray);
    procedure ReadTextColor(const AParams: TParamArray);
    procedure ReadTextOut(APage: TvVectorialPage; const AParams: TParamArray);
    procedure ReadWindowExt(const AParams: TParamArray);
    procedure ReadWindowOrg(const AParams: TParamArray);
    procedure SelectObj(const AIndex: Integer);
    procedure SelectPalette(const AIndex: Integer);

  protected
    procedure ReadHeader(AStream: TStream);
    procedure ReadRecords(AStream: TStream; AData: TvVectorialDocument);

    procedure LogError(AMsg: String);

    procedure CalcScalingFactors(out fx, fy: Double);
    function ScaleX(x: Integer): Double;
    function ScaleY(y: Integer): Double;
    function ScaleSizeX(x: Integer): Double;
    function ScaleSizeY(y: Integer): Double;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;


implementation

uses
  FPReadBMP, BMPcomn,
  LConvEncoding, Math,
  fpvUtils, fpvWMF;

const
  INCH = 25.4;  // 1 inch = 25.4 mm
  DEFAULT_SIZE = 100;  // size of image if scaling info is not available

type
  TWMFFont = class
    Font: TvFont;
    RawHeight: Integer;
  end;

  TWMFBrush = class
    Brush: TvBrush;
  end;

  TWMFPen = class
    Pen: TvPen;
    RawPenWidth: Integer;
  end;

  TWMFPalette = class
    // not used, just needed as a filler in the ObjList
  end;

  TWMFRegion = class
    // not used, just needed as a filler in the ObjList
  end;


{ TWMFObjList }

function TWMFObjList.Add(AData: Pointer): Integer;
var
  i: Integer;
begin
  // Fill empty items first
  for i := 0 to Count-1 do
    if Items[i] = nil then begin
      Items[i] := AData;
      Result := i;
      exit;
    end;
  Result := inherited Add(AData);
end;


{ TvWMFVectorialReader }

constructor TvWMFVectorialReader.Create;
begin
  inherited;
  FErrMsg := TStringList.Create;
  FObjList := TWMFObjList.Create;
  with FCurrPen do begin
    Style := psSolid;
    Color := colBlack;
    Width := 1;
  end;
  with FCurrBrush do begin
    Style := bsSolid;
    Color := colBlack;
  end;
  with FCurrFont do begin
    Color := colBlack;
    Size := 10;
    Name := 'Arial';
    Orientation := 0;
    Bold := false;
    Italic := False;
    Underline := false;
    StrikeThrough := false;
  end;
  FCurrTextColor := colBlack;
  FCurrTextAlign := 0;  // Left + Top
  FCurrPolyFillMode := ALTERNATE;
  FBkColor := colWhite;
  FMapMode := MM_ANISOTROPIC;
  FUnitsPerInch := 96;
end;

destructor TvWMFVectorialReader.Destroy;
begin
  ClearObjList;
  FObjList.Free;
  FErrMsg.Free;
  inherited;
end;

procedure TvWMFVectorialReader.ClearObjList;
var
  i: Integer;
begin
  for i:=0 to FObjList.Count-1 do
    TObject(FObjList[i]).Free;
  FObjList.Clear;
end;

function TvWMFVectorialReader.CreateBrush(const AParams: TParamArray): Integer;
var
  brushRec: PWMFBrushRecord;
  wmfBrush: TWMFBrush;
begin
  wmfBrush := TWMFBrush.Create;
  brushRec := PWMFBrushRecord(@AParams[0]);

  // brush style
  case brushRec^.Style of
    BS_SOLID:
      wmfBrush.Brush.Style := bsSolid;
    BS_NULL:
      wmfBrush.brush.Style := bsClear;
    BS_HATCHED:
      case brushRec^.Hatch of
        HS_HORIZONTAL : wmfBrush.brush.Style := bsHorizontal;
        HS_VERTICAL   : wmfBrush.brush.Style := bsVertical;
        HS_FDIAGONAL  : wmfBrush.brush.Style := bsFDiagonal;
        HS_BDIAGONAL  : wmfBrush.brush.Style := bsBDiagonal;
        HS_CROSS      : wmfBrush.brush.Style := bsCross;
        HS_DIAGCROSS  : wmfBrush.brush.Style := bsDiagCross;
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
      wmfBrush.brush.Style := bsSolid;
  end;

  // brush color
  wmfBrush.brush.Color.Red := brushRec^.ColorRED shl 8;
  wmfBrush.brush.Color.Green := brushRec^.ColorGREEN shl 8;
  wmfBrush.brush.Color.Blue := brushRec^.ColorBLUE shl 8;

  // add to WMF object list
  Result := FObjList.Add(wmfBrush);
end;

function TvWMFVectorialReader.CreateFont(const AParams: TParamArray): Integer;
var
  wmfFont: TWMFFont;
  fontRec: PWMFFontRecord;
begin
  wmfFont := TWMFFont.Create;
  fontRec := PWMFFontRecord(@AParams[0]);

  wmfFont.Font.Name := ISO_8859_1ToUTF8(fontRec^.FaceName);
  wmfFont.Font.Size := round(ScaleSizeY(fontRec^.Height));
  wmfFont.Font.Color := colBlack;  // to be replaced by FCurrTextColor
  wmfFont.Font.Bold := fontRec^.Weight >= 700;
  wmfFont.Font.Italic := fontRec^.Italic <> 0; //Escapement <> 0;
  wmfFont.Font.Underline := fontRec^.UnderLine <> 0;
  wmfFont.Font.StrikeThrough := fontRec^.Strikeout <> 0;
  wmfFont.Font.Orientation := fontRec^.Orientation div 10;
  wmfFont.RawHeight := fontRec^.Height;

  // add to WMF object list
  Result := FObjList.Add(wmfFont);
end;

// to do: implement read palette
function TvWMFVectorialReader.CreatePalette(const AParams: TParamArray): Integer;
var
  pal: TFPPalette;
  col: TFPColor;
  colRec: PWMFPaletteColorRecord;
  i, n: Integer;
begin
  // start := AParams[0];
  n := AParams[1];
  pal := TFPPalette.Create(n);
  for i:=0 to n-1 do begin
    colRec := PWMFPaletteColorRecord(@AParams[2 + i*4]);
    col.Red := colRec^.ColorRED shl 8;
    col.Green := colRec^.ColorGREEN shl 8;
    col.Blue := colRec^.ColorBLUE shl 8;
    pal.Add(col);
  end;
  Result := FObjList.Add(pal);
end;

function TvWMFVectorialReader.CreatePen(const AParams: TParamArray): Integer;
var
  penRec: PWMFPenRecord;
  wmfPen: TWMFPen;
begin
  wmfPen := TWMFPen.Create;
  penRec := PWMFPenRecord(@AParams[0]);

  // pen style
  case penRec^.Style and $000F of
    PS_DASH       : wmfPen.pen.Style := psDash;
    PS_DOT        : wmfPen.pen.Style := psDot;
    PS_DASHDOT    : wmfPen.pen.Style := psDashDot;
    PS_DASHDOTDOT : wmfPen.pen.Style := psDashDotDot;
    PS_NULL       : wmfPen.pen.Style := psClear;
    PS_INSIDEFRAME: wmfPen.pen.Style := psInsideFrame;
    else            wmfPen.pen.Style := psSolid;
  end;
  { -- this is not yet supported by fpvectorial
  case penRec^.Style and $0F00 of
    PS_ENDCAP_SQUARE: wmfPen.pen.Endcap := pseSquare;
    PS_ENDCAP_FLAT  : wmfPen.pen.EndCap := pseFlat;
    else              wmfPen.pen.EndCap := pseRound;
  end;
  case penRec^.Style and $1000 of
    PS_JOIN_BEVEL   : wmfPen.pen.JoinStyle := pjsBevel;
    PS_JOIN_MITER   : wmfPen.pen.JoinStyle := pjsMiter;
    else              wmfPen.pen.JoinStyle := pjsRound;
  end; }

  // pen width
  wmfPen.pen.Width := round(ScaleSizeX(penRec^.Width));
  if penRec^.Width = 0 then
    wmfPen.pen.Width := 1;

  if wmfPen.pen.Style = psClear
    then wmfPen.RawPenWidth := 0
    else wmfPen.RawPenWidth := penRec^.Width;

  // pen color
  wmfPen.pen.Color.Red := penRec^.ColorRED shl 8;
  wmfPen.pen.Color.Green := penRec^.ColorGREEN shl 8;
  wmfPen.pen.Color.Blue := penRec^.ColorBLUE shl 8;

  // Add to WMF object list
  Result := FObjList.Add(wmfPen);
end;

// todo: implement region
function TvWMFVectorialReader.CreateRegion(const AParams: TParamArray): Integer;
var
  wmfReg: TWMFRegion;
begin
  wmfReg := TWMFRegion.Create;
  Result := FObjList.Add(wmfReg);
end;

procedure TvWMFVectorialReader.DeleteObj(const AParams: TParamArray);
var
  obj: TObject;
  idx: Integer;
begin
  idx := AParams[0];
  if idx < FObjList.Count then begin
    obj := TObject(FObjList[idx]);
    TObject(obj).Free;
    FObjList[idx] := nil;
    // Do not delete from list because this will confuse the obj indexes.
  end;
end;

procedure TvWMFVectorialReader.LogError(AMsg: String);
begin
  FErrMsg.Add(AMsg);
end;

procedure TvWMFVectorialReader.ReadArc(APage: TvVectorialPage;
  const AParams: TParamArray);
var
  path: TPath;
  arcRec: PWMFArcRecord;
begin
  arcRec := PWMFArcRecord(@AParams[0]);

  APage.StartPath(ScaleX(arcRec^.XStartArc), ScaleY(arcRec^.YStartArc));
  APage.AddEllipticalArcWithCenterToPath(
    ScaleX(arcRec^.Right - arcRec^.Left) / 2,
    ScaleY(abs(arcRec^.Bottom - arcRec^.Top)) / 2,
    0.0,
    ScaleX(arcRec^.XEndArc),
    ScaleY(arcrec^.YEndArc),
    ScaleX(arcRec^.Left + arcRec^.Right) / 2,
    ScaleY(abs(arcRec^.Top + arcRec^.Bottom)) / 2,
    false
  );
  path := APage.EndPath;
  path.Pen := FCurrPen;
end;

procedure TvWMFVectorialReader.ReadBkColor(const AParams: TParamArray);
begin
  FBkColor := ReadColor(AParams, 0);
end;

procedure TvWMFVectorialReader.ReadChord(APage: TvVectorialPage;
  const AParams: TParamArray);
var
  path: TPath;
  arcRec: PWMFArcRecord;
  p1, p2: T3dPoint;
begin
  arcRec := PWMFArcRecord(@AParams[0]);

  p1 := Make3DPoint(ScaleX(arcRec^.XStartArc), ScaleY(arcRec^.YStartArc));
  p2 := Make3DPoint(ScaleX(arcRec^.XEndArc), ScaleY(arcRec^.YEndArc));

  APage.StartPath(p1.x, p1.y);
  APage.AddEllipticalArcWithCenterToPath(
    ScaleX(arcRec^.Right - arcRec^.Left) / 2,
    ScaleY(abs(arcRec^.Bottom - arcRec^.Top)) / 2,
    0.0,
    p2.x,
    p2.y,
    ScaleX(arcRec^.Left + arcRec^.Right) / 2,
    ScaleY(abs(arcRec^.Top + arcRec^.Bottom)) / 2,
    false
  );
  APage.AddLineToPath(p1.x, p1.y);
  path := APage.EndPath;
  path.Pen := FCurrPen;
  path.Brush := FCurrBrush;
end;

function TvWMFVectorialReader.ReadColor(const AParams: TParamArray;
  AIndex: Integer): TFPColor;
var
  colorRec: PWMFColorRecord;
begin
  colorRec := PWMFColorRecord(@AParams[AIndex]);
  Result.Red := colorRec^.ColorRED shl 8;
  Result.Green := colorRec^.ColorGREEN shl 8;
  Result.Blue := colorRec^.ColorBLUE shl 8;
  Result.Alpha := alphaOpaque;
end;

procedure TvWMFVectorialReader.ReadExtTextOut(APage: TvVectorialPage;
  const AParams: TParamArray);
var
  x, y, len, opts: Integer;
  offs: TPoint;
  R: TRect;
  s: String;
  txt: TvText;
begin
  y := SmallInt(AParams[0]);   // signed int
  x := SmallInt(AParams[1]);
  len := SmallInt(AParams[2]);
  opts := AParams[3];         // unsigned int
  if opts <> 0 then begin
    R.Bottom := SmallInt(AParams[4]);
    R.Right := SmallInt(AParams[5]);
    R.Top := SmallInt(AParams[6]);
    R.Left := SmallInt(AParams[7]);
    s := ReadString(AParams, 8, len);
  end else
    s := ReadString(AParams, 4, len);
  // We ignore the Dx fields

  // Correct text position which is at baseline in case of fpvectorial, but
  // may be different depending on bits in the CurrTextAlign value.
  case FCurrTextAlign and (TA_BOTTOM + TA_BASELINE) of
    0 :
      { In this case, the text should be top-aligned, but fpvectorial draws
        the text at the baseline --> move it down (and take care of text
        rotation!) }
      offs := Rotate2DPoint(Point(0, FCurrRawFontHeight), Point(0, 0), DegToRad(FCurrFont.Orientation));
    TA_BASELINE:
      { This is the way how fpvectorial draws the text --> nothing to do }
      offs := Point(0, 0);
    TA_BOTTOM:
      { Unfortunately we don't know the descender of the font here. Lets
      assume it to be 1/5th of the font size. Move text up. }
      offs := Rotate2DPoint(Point(0, -FCurrRawFontHeight div 5), Point(0, 0), DegToRad(FCurrFont.Orientation));
  end;

  // Pass text to fpvectorial
  txt := APage.AddText(ScaleX(x + offs.X), ScaleY(y + offs.Y), s);
  // Select the font
  txt.Font := FCurrFont;
  // Set horizontal text alignment.
  case FCurrTextAlign and (TA_RIGHT + TA_CENTER) of
    TA_RIGHT  : txt.TextAnchor := vtaEnd;
    TA_CENTER : txt.TextAnchor := vtaMiddle;
    else        txt.TextAnchor := vtaStart;
  end;

  // to do: draw text background (if opts and ETO_OPAQUE <> 0 )
  // to do: take care of clipping (if opts and ETO_CLIPPED <> 0)
end;

procedure TvWMFVectorialReader.ReadEllipse(APage: TvVectorialPage;
  const AParams: TParamArray);
var
  rectRec: PWMFRectRecord;    // coordinates are SmallInt.
  ellipse: TvEllipse;
begin
  rectRec := PWMFRectRecord(@AParams[0]);

  ellipse := TvEllipse.Create(APage);
  ellipse.X := (ScaleX(rectRec^.Left) + ScaleX(rectRec^.Right)) / 2;
  ellipse.Y := (ScaleY(rectRec^.Top) + ScaleY(rectRec^.Bottom)) / 2;
  ellipse.HorzHalfAxis := abs(ScaleX(rectRec^.Right - rectRec^.Left) / 2);
  ellipse.VertHalfAxis := abs(ScaleSizeY(rectRec^.Bottom - rectRec^.Top) / 2);

  ellipse.Pen := FCurrPen;
  ellipse.Brush := FCurrBrush;

  APage.AddEntity(ellipse);
end;

procedure TvWMFVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  ClearObjList;
  FErrMsg.Clear;

  ReadHeader(AStream);
  ReadRecords(AStream, AData);

  if FErrMsg.Count > 0 then
    raise Exception.Create(FErrMsg.Text);
end;

procedure TvWMFVectorialReader.ReadHeader(AStream: TStream);
var
  buf: array[0..80] of byte;
  placeableMetaHdr: TPlaceableMetaHeader absolute buf;
  wmfHdr: TWMFHeader absolute buf;
begin
  AStream.Position := 0;

  // Test if file begins with a placeable meta file header
  FHasPlaceableMetaHeader := false;
  AStream.ReadBuffer(buf, SizeOf(TPlaceableMetaHeader));
  if placeableMetaHdr.Key = $9AC6CDD7 then begin  // yes!
    FHasPlaceableMetaHeader := true;
    FBBox.Left := placeableMetaHdr.Left;
    FBBox.Top := placeableMetaHdr.Top;
    FBBox.Right := placeableMetaHdr.Right;
    FBBox.Bottom := placeableMetaHdr.Bottom;
    FUnitsPerInch := placeableMetaHdr.Inch;
  end else
  begin
    // Is it the wmf header?
    if not ((wmfHdr.FileType in [0, 1]) and (wmfHdr.HeaderSize = 9)) then begin
      // No - then it is not a wmf format.
      LogError('This is not a WMF file.');
      exit;
    end;
    // Rewind stream
    AStream.Position := 0;
  end;

  // Read the wmf header
  AStream.ReadBuffer(buf, SizeOf(TWMFHeader));
//  FNumObj := wmfHdr.NumOfObjects;
//  FMaxRecordSize := wmfHdr.MaxRecordSize;  // words
end;

procedure TvWMFVectorialReader.ReadLine(APage: TvVectorialPage;
  P1X, P1Y, P2X, P2Y: SmallInt);
var
  path: TPath;
begin
  APage.StartPath(ScaleX(P1X), ScaleY(P1Y));
  APage.AddLineToPath(ScaleX(P2X), ScaleY(P2Y));
  path := APage.EndPath;
  path.Pen := FCurrPen;
end;

procedure TvWMFVectorialReader.ReadMapMode(const AParams: TParamArray);
begin
  FMapMode := AParams[0];
  CalcScalingFactors(FScalingFactorX, FScalingFactorY);
end;

procedure TvWMFVectorialReader.ReadOffsetWindowOrg(const AParams: TParamArray);
begin
  FWindowOrigin.Y := FWindowOrigin.Y + SmallInt(AParams[0]);
  FWindowOrigin.X := FWindowOrigin.X + SmallInt(AParams[1]);
end;

procedure TvWMFVectorialReader.ReadPie(APage: TvVectorialpage;
  const AParams: TParamArray);
var
  path: TPath;
  arcRec: PWMFArcRecord;
  p1, p2, ctr: T3dPoint;
begin
  arcRec := PWMFArcRecord(@AParams[0]);

  p1 := Make3DPoint(ScaleX(arcRec^.XStartArc), ScaleY(arcRec^.YStartArc));
  p2 := Make3DPoint(ScaleX(arcRec^.XEndArc), ScaleY(arcRec^.YEndArc));
  ctr := Make3DPoint(ScaleX(arcRec^.Left + arcRec^.Right)/2, ScaleY(arcRec^.Top + arcRec^.Bottom)/2);

  APage.StartPath(p1.x, p1.y);
  APage.AddEllipticalArcWithCenterToPath(
    ScaleX(arcRec^.Right - arcRec^.Left) / 2,
    ScaleY(abs(arcRec^.Bottom - arcRec^.Top)) / 2,
    0.0,
    p2.x, p2.y,
    ctr.x, ctr.y,
    false
  );
  APage.AddLineToPath(ctr.x, ctr.y);
  APage.AddLineToPath(p1.x, p1.y);
  path := APage.EndPath;
  path.Pen := FCurrPen;
  path.Brush := FCurrBrush;
end;

procedure TvWMFVectorialReader.ReadPolyFillMode(const AValue: Word);
begin
  FCurrPolyFillMode := AValue;
end;

{ AParams[0] ... number of points
  AParams[1] ... x value of 1st point
  AParams[2] ... y value of 1st point
  etc }
procedure TvWMFVectorialReader.ReadPolygon(APage: TvVectorialPage;
  const AParams: TParamArray; AFilled: boolean);
var
  n: Integer;
  i, j: Integer;
  poly: TvPolygon;
  pts: Array of T3DPoint;
begin
  n := AParams[0];
  SetLength(pts, n);
  j := 1;
  for i:= 0 to n-1 do begin
    pts[i] := Make3DPoint(ScaleX(SmallInt(AParams[j])), ScaleY(SmallInt(AParams[j+1])));
    inc(j, 2);
  end;

  poly := TvPolygon.Create(APage);
  poly.Points := pts;
  poly.Pen := FCurrPen;
  if AFilled then
    poly.Brush := FCurrBrush
  else
    poly.Brush.Style := bsClear;
  case FCurrPolyFillMode of
    ALTERNATE : poly.WindingRule := vcmEvenOddRule;
    WINDING   : poly.WindingRule := vcmNonZeroWindingRule;
  end;

  APage.AddEntity(poly);
end;

procedure TvWMFVectorialReader.ReadPolyPolygon(APage: TvVectorialPage;
  const AParams: TParamArray);
var
  nPoly: Integer;
  nPts: array of Integer;
  pts: array of T3DPoint;
  i, j, k: Integer;
  path: TPath;
  P: T3DPoint;
begin
  k := 0;
  nPoly := AParams[k];
  inc(k);
  SetLength(nPts, nPoly);
  for i:=0 to nPoly-1 do begin
    nPts[i] := AParams[k];
    inc(k);
  end;

  APage.StartPath;
  for j := 0 to nPoly-1 do begin
    P := Make3DPoint(ScaleX(SmallInt(AParams[k])), ScaleY(SmallInt(AParams[k+1])));
    inc(k, 2);
    APage.AddMoveToPath(P.X, P.Y);
    for i := 1 to nPts[j]-1 do begin
      P := Make3DPoint(ScaleX(SmallInt(AParams[k])), ScaleY(SmallInt(AParams[k+1])));
      inc(k, 2);
      APage.AddLineToPath(P.X, P.Y);
    end;
  end;
  path := APage.EndPath;
  path.Pen := FCurrPen;
  path.Brush := FCurrBrush;
  case FCurrPolyFillMode of
    ALTERNATE : path.WindingRule := vcmEvenOddRule;
    WINDING   : path.WindingRule := vcmNonZeroWindingRule;
  end;

  // No need to add path to page explicity
end;

procedure TvWMFVectorialReader.ReadRecords(AStream: TStream; AData: TvVectorialDocument);
var
  wmfRec: TWMFRecord;
  params: TParamArray;
  page: TvVectorialPage;
  prevX, prevY: Word;
begin
  AData.AddPage(true);
  page := AData.GetPageAsVectorial(0);

  while AStream.Position < AStream.Size do begin
    // Store the stream position where the current record begins
    FRecordStartPos := AStream.Position;

    // Read record size and function code
    AStream.ReadBuffer(wmfRec, SizeOf(wmfRec));

   {$IFDEF WMF_DEBUG}
    writeLn(Format('Record position: %0:d / Record size: %1:d words / Record type: %2:d ($%2:x): %3:s',
      [FRecordStartPos, wmfRec.Size, wmfRec.Func, WMF_GetRecordTypeName(wmfRec.Func)]));
   {$ENDIF}

    // End of file?
    if wmfRec.Func = META_EOF then
      break;

    // Obviously invalid record?
    if wmfRec.Size < 3 then begin
      LogError(Format('Record size error at position %d', [FRecordStartPos]));
      exit;
    end;

    // Read parameters
    SetLength(params, wmfRec.Size - 3);
    AStream.ReadBuffer(params[0], (wmfRec.Size - 3)*SizeOf(word));

    // Process record, depending on function code
    case wmfRec.Func of
      { *** Bitmap record types *** }
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
      META_STRETCHDIB:
        ReadStretchDIB(AStream, page, params);

      { *** Drawing records *** }
      META_ARC:
        ReadArc(page, params);
      META_CHORD:
        ReadChord(page, params);
      META_ELLIPSE:
        ReadEllipse(page, params);
      META_EXTFLOODFILL:
        ;
      META_EXTTEXTOUT:
        ReadExtTextOut(page, params);
      META_FILLREGION:
        ;
      META_FLOODFILL:
        ;
      META_FRAMEREGION:
        ;
      META_INVERTREGION:
        ;
      META_MOVETO:
        begin
          prevX := params[1];
          prevY := params[0];
        end;
      META_LINETO:
        begin
          ReadLine(page, prevX, prevY, params[1], params[0]);
          prevX := params[1];
          prevY := params[0];
        end;
      META_PAINTREGION:
        ;
      META_PATBLT:
        ;
      META_PIE:
        ReadPie(page, params);
      META_POLYGON:
        ReadPolygon(page, params, true);
      META_POLYLINE:
        ReadPolygon(page, params, false);
      META_POLYPOLYGON:
        ReadPolyPolygon(page, params);
      META_RECTANGLE:
        ReadRectangle(page, params);
      META_ROUNDRECT:
        ReadRectangle(page, params);
      META_SETPIXEL:
        ;
      META_TEXTOUT:
        ReadTextOut(page, params);

      { *** WMF Object records *** }
      META_CREATEBRUSHINDIRECT:
        CreateBrush(params);
      META_CREATEFONTINDIRECT:
        CreateFont(params);
      META_CREATEPALETTE:
        CreatePalette(params);
      META_CREATEPATTERNBRUSH:
        FObjList.Add(nil);  // i.e. not supported ATM       /// !!! TO DO
      META_CREATEPENINDIRECT:
        CreatePen(params);
      META_CREATEREGION:
        CreateRegion(params);
      META_DIBCREATEPATTERNBRUSH:
        FObjList.Add(nil);  // i.e. not supported ATM
      META_DELETEOBJECT:
        DeleteObj(params);
      META_SELECTCLIPREGION:
        ;
      META_SELECTOBJECT:
        SelectObj(params[0]);
      META_SELECTPALETTE:
        SelectPalette(params[0]);

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
        ;
      META_SETLAYOUT:
        ;
      META_SETMAPMODE:
        ReadMapMode(params);
      META_SETMAPPERFLAGS:
        ;
      META_SETPALENTRIES:
        ;
      META_SETPOLYFILLMODE:
        ReadPolyFillMode(params[0]);
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

      { *** ESCAPE records *** }
      // None of them implemented
    end;

    AStream.Position := FRecordStartPos + wmfRec.Size*SizeOf(word);
  end;

  if FHasPlaceableMetaHeader then begin
    page.Width := FPageWidth;
    page.Height := FPageHeight;
  end else begin
    page.Width := ScaleSizeX(FWindowExtent.X);
    page.Height := ScaleSizeY(FWindowExtent.Y);
  end;
  AData.Width := page.Width;
  AData.Height := page.Height;

  SetLength(params, 0);
end;

procedure TvWMFVectorialReader.ReadRectangle(APage: TvVectorialPage;
  const AParams: TParamArray);
// To do: not tested, having not test file
var
  rectRec: PWMFRectRecord;   // coordinates are SmallInt
  poly: TvPolygon;
  pts: array of T3DPoint;
begin
  rectRec := PWMFRectRecord(@AParams[0]);

  SetLength(pts, 5);
  pts[0] := Make3DPoint(ScaleX(rectRec^.Left), ScaleY(rectRec^.Top));
  pts[1] := Make3DPoint(ScaleX(rectRec^.Right), ScaleY(rectRec^.Top));
  pts[2] := Make3DPoint(ScaleX(rectRec^.Right), ScaleY(rectRec^.Bottom));
  pts[3] := Make3DPoint(ScaleX(rectRec^.Left), ScaleY(rectRec^.Bottom));
  pts[4] := pts[0];

  poly := TvPolygon.Create(APage);
  poly.Points := pts;
  poly.Pen := FCurrPen;
  poly.Brush := FCurrBrush;
  APage.AddEntity(poly);
end;

procedure TvWMFVectorialReader.ReadRoundRect(APage: TvVectorialPage;
  const AParams: TParamArray);
// To do: not tested, having no test file
var
  rectRec: PWMFRectRecord;   // coordinates are SmallInt
  rx, ry: SmallInt;
  rect: TvRectangle;
begin
  ry := AParams[0];
  rx := AParams[1];

  rectRec := PWMFRectRecord(@AParams[4]);

  rect := TvRectangle.Create(APage);
  rect.X := ScaleX(rectRec^.Left);
  rect.Y := ScaleY(rectRec^.Top);
  rect.CX := ScaleSizeX(rectRec^.Right - rectRec^.Left);
  rect.CY := ScaleSizeY(rectRec^.Bottom - rectRec^.Top);
  rect.RX := ScaleSizeX(rx);
  rect.RY := ScaleSizeY(ry);
  rect.Pen := FCurrPen;
  rect.Brush := FCurrBrush;

  APage.AddEntity(rect);
end;

{ Tested: embedded bmp, png and jpeg in Inkscape, saved as wmf.
  Other tests are missing due to lack of well-defined test files. }
procedure TvWMFVectorialReader.ReadStretchDIB(AStream: TStream;
  APage: TvVectorialPage; const AParams: TParamArray);
var
  rasterImg: TvRasterImage = nil;
  memImg: TFPMemoryImage = nil;
  reader: TFPCustomImageReader;
  dibRec: PWMFStretchDIBRecord;
  bmpCoreHdr: PWMFBitmapCoreHeader;
  bmpInfoHdr: PWMFBitmapInfoHeader;
  hasCoreHdr: Boolean;
  bmpFileHdr: TBitmapFileHeader;
  w, h: Integer;
  memstream: TMemoryStream;
  savedPos: Int64;
  datasize: Int64;
  imgSize: Int64;
begin
  // Store the current stream position.
  savedPos := AStream.Position;

  dibRec := PWMFStretchDIBRecord(@AParams[0]);
  bmpCoreHdr := PWMFBitmapCoreHeader(@AParams[SizeOf(TWMFStretchDIBRecord) div SizeOf(word)]);
  bmpInfoHdr := PWMFBitmapInfoHeader(@AParams[SizeOf(TWMFStretchDIBRecord) div SizeOf(word)]);
  hasCoreHdr := bmpInfoHdr^.HeaderSize = SizeOf(TWMFBitmapCoreHeader);

  if hasCoreHdr then begin
    w := bmpCoreHdr^.Width;
    h := bmpCoreHdr^.Height;
    // Not implemented due to lack of test files.
  end else begin
    w := bmpInfoHdr^.Width;
    h := bmpInfoHdr^.Height;
    if (w = 0) or (h = 0) then
      exit;
    memStream := TMemoryStream.Create;
    try
      datasize := Length(AParams) * SizeOf(word) - SizeOf(TWMFStretchDIBRecord);
      // Put a bitmap file header before the bitmap info header and the data
      bmpFileHdr.bfType := BMmagic;
      bmpFileHdr.bfSize:= SizeOf(bmpFileHdr) + datasize;
      if bmpInfoHdr^.Compression in [BI_RGB, BI_BITFIELDS, BI_CMYK] then
        imgSize := (w + bmpInfoHdr^.Planes * bmpInfoHdr^.BitCount + 31) div 32 * abs(h)
      else
        imgSize := bmpInfoHdr^.ImageSize;
      bmpFileHdr.bfOffset := bmpFileHdr.bfSize - imgSize;
      bmpFileHdr.bfReserved := 0;
      memstream.WriteBuffer(bmpFileHdr, SizeOf(bmpFileHdr));
      AStream.Position := FRecordStartPos + 3*SizeOf(Word) + SizeOf(TWMFStretchDIBRecord);
      memstream.CopyFrom(AStream, Length(AParams) * SizeOf(Word) - SizeOf(TWMFStretchDIBRecord));
      memstream.Position := 0;
      try
        // Read bitmap
        memImg := TFPMemoryImage.Create(w, h);
        reader := TFPReaderBMP.Create;
        try
          memImg.LoadfromStream(memStream, reader);
        finally
          reader.Free;
        end;
        // Pass bitmap to fpvectorial
        rasterImg := TvRasterImage.Create(APage);
        rasterImg.RasterImage := memImg;
        rasterImg.x := ScaleX(dibRec^.DestX);
        rasterImg.y := ScaleY(dibRec^.DestY);
        rasterImg.Width := ScaleX(dibRec^.DestWidth);
        rasterImg.Height := ScaleY(dibRec^.DestHeight);
        APage.AddEntity(rasterImg);
      except
        on E:Exception do begin
          memImg.Free;
          rasterImg.Free;
          LogError('Image reading error: ' + E.Message);
          exit;
        end;
      end;
    finally
      memstream.Free;
    end;
  end;
  // Restore original stream position
  AStream.Position := savedPos;
end;

function TvWMFVectorialReader.ReadString(const AParams: TParamArray;
  AStartIndex, ALength: Integer): String;
var
  s: ansistring;
  i, j: Integer;
begin
  SetLength(s, ALength);
  i := AStartIndex;
  j := 1;
  while j < ALength do begin
    Move(AParams[i], s[j], SizeOf(Word));
    inc(i);
    inc(j, 2);
  end;
  if odd(ALength) then SetLength(s, ALength-1);
  Result := ISO_8859_1ToUTF8(s);
end;

procedure TvWMFVectorialReader.ReadTextAlign(const AParams: TParamArray);
begin
  FCurrTextAlign := AParams[0];
end;

procedure TvWMFVectorialReader.ReadTextColor(const AParams: TParamArray);
begin
  FCurrTextColor := ReadColor(AParams, 0);
end;

procedure TvWMFVectorialReader.ReadTextOut(APage: TvVectorialPage;
  const AParams: TParamArray);
var
  x, y, len, i: Integer;
  s: String;
  txt: TvText;
  offs: TPoint;
begin
  len := AParams[0];
  i := 1;
  s := ReadString(AParams, i, len);
  inc(i, len);
  if odd(len) then inc(i);
  y := SmallInt(AParams[i]);      // signed int!
  x := SmallInt(AParams[i + 1]);

  // Correct text position which is at baseline in case of fpvectorial, but
  // may be different depending on bits in the CurrTextAlign value.
  case FCurrTextAlign and (TA_BOTTOM + TA_BASELINE) of
    0 :
      { In this case, the text should be top-aligned, but fpvectorial draws it
        at the baseline --> move it down (and respect text rotation!) }
      offs := Rotate2DPoint(Point(0, FCurrRawFontHeight), Point(0, 0), DegToRad(FCurrFont.Orientation));
    TA_BASELINE:
      { This is the way how fpvectorial draws the text --> nothing to do }
      offs := Point(0, 0);
    TA_BOTTOM:
      { Unfortunately we don't know the descender of the font here. Lets
        assume it to be 1/5th of the font size. Move text up. }
      offs := Rotate2DPoint(Point(0, -FCurrRawFontHeight div 5), Point(0, 0), DegToRad(FCurrFont.Orientation));
  end;

  // Pass the text to fpvectorial
  txt := APage.AddText(x + offs.x, y + offs.y, s);
  // Select the font
  txt.Font := FCurrFont;
  // Set horizontal text alignment.
  case FCurrTextAlign and (TA_RIGHT + TA_CENTER) of
    TA_RIGHT  : txt.TextAnchor := vtaEnd;
    TA_CENTER : txt.TextAnchor := vtaMiddle;
    else        txt.TextAnchor := vtaStart;
  end;
end;

procedure TvWMFVectorialReader.ReadWindowExt(const AParams: TParamArray);
begin
  FWindowExtent.Y := SmallInt(AParams[0]);   // signed int
  FWindowExtent.X := SmallInt(AParams[1]);
  CalcScalingFactors(FScalingFactorX, FScalingFactorY);
end;

procedure TvWMFVectorialReader.ReadWindowOrg(const AParams: TParamArray);
begin
  FWindowOrigin.Y := SmallInt(AParams[0]);   // signed int, probably not relevant here.
  FWindowOrigin.X := SmallInt(AParams[1]);
end;

procedure TvWMFVectorialReader.CalcScalingFactors(out fx, fy: Double);
begin
  case FMapMode of
    MM_TEXT:         // 1 log unit = 1 pixel
      begin
        fx := ScreenDpiX * INCH;
        fy := ScreenDpiY * INCH;
      end;
    MM_LOMETRIC:     // 1 log unit = 1/10 mm
      begin
        fx := 0.1;
        fy := 0.1;
      end;
    MM_HIMETRIC:     // 1 log unit = 1/100 mm
      begin
        fx := 0.01;
        fy := 0.01;
      end;
    MM_LOENGLISH:    // 1 log unit = 1/100"
      begin
        fx := 0.01 * INCH;
        fy := fx;
      end;
    MM_HIENGLISH:    // 1 log unit = 1/1000"
      begin
        fx := 0.001 * INCH;
        fy := fx;
      end;
    MM_TWIPS:        // 1 log unit = 1 twip
      begin
        fx := 1.0 / 1440 * INCH;
        fy := fx;
      end;
    else
      if (FWindowExtent.X = 0) or (FWindowExtent.Y = 0) then
        exit;
      if FHasPlaceableMetaHeader then begin
        FPageWidth := (FBBox.Right - FBBox.Left) * INCH / FUnitsPerInch;
        FPageHeight := (FBBox.Bottom - FBBox.Top) * INCH / FUnitsPerInch;
      end else
      if FWindowExtent.X > FWindowExtent.Y then begin
        FPageWidth := DEFAULT_SIZE;
        FPageHeight := DEFAULT_SIZE * FWindowExtent.Y / FWindowExtent.X;
      end else begin
        FPageHeight := DEFAULT_SIZE;
        FPageWidth := DEFAULT_SIZE * FWindowExtent.X / FWindowExtent.Y;
      end;
      fx := FPageWidth / FWindowExtent.X;
      fy := FPageHeight / FWindowExtent.Y;
  end;
end;

{ Scale horizontal logical units (x) to millimeters }
function TvWMFVectorialReader.ScaleX(x: Integer): Double;
begin
  Result := ScaleSizeX(x - FWindowOrigin.X);
end;

{ Scale vertical logical units (y) to millimeters.
  Coordinates will be increasing downwards, like in SVG }
function TvWMFVectorialReader.ScaleY(y: Integer): Double;
begin
  Result := ScaleSizeY(y - FWindowOrigin.Y);    // there is probably an issue with y direction
end;

function TvWMFVectorialReader.ScaleSizeX(x: Integer): Double;
begin
  Result := FScalingFactorX * x;
end;

function TvWMFVectorialReader.ScaleSizeY(y: Integer): Double;
begin
  Result := FScalingFactorY * y;
end;

procedure TvWMFVectorialReader.SelectObj(const AIndex: Integer);
var
  obj: TObject;
begin
  obj := TObject(FObjList[AIndex]);
  if obj = nil then
    exit;
  if obj is TWMFPen then begin
    FCurrPen := TWMFPen(obj).Pen;
  end else
  if obj is TWMFBrush then
    FCurrBrush := TWMFBrush(obj).Brush
  else
  if obj is TWMFFont then begin
    FCurrFont := TWMFFont(obj).Font;
    FCurrRawFontHeight := TWMFFont(obj).RawHeight;
  end else
  if obj is TFPPalette then
    FCurrPalette := TFPPalette(obj);
end;

procedure TvWMFVectorialReader.SelectPalette(const AIndex: Integer);
begin
  SelectObj(AIndex);
end;


initialization
  RegisterVectorialReader(TvWMFVectorialReader, vfWindowsMetafileWMF);

end.

