{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADrawerSVG;

{$H+}

interface

uses
  Graphics, Classes, FPImage, FPCanvas, EasyLazFreeType,
  TAFonts, TAChartUtils, TADrawUtils, TAGraph;

type
  TSVGDrawer = class(TBasicDrawer, IChartDrawer)
  strict private
    FAntialiasingMode: TChartAntialiasingMode;
    FBrushColor: TFPColor;
    FBrushStyle: TFPBrushStyle;
    FClippingPathId: Integer;
    FFont: TFreeTypeFont;
    FFontHeight: Integer;  // Height of text in pixels
    FFontOrientation: Integer;  // angle*10 (i.e. 90° --> 900, >0 if ccs.
    FFontColor: TFPColor;
    FPatterns: TStrings;
    FPen: TFPCustomPen;
    FPrevPos: TPoint;
    FStream: TStream;

    function OpacityStr: String;
    function PointsToStr(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer): String;

    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);

    function StyleFill: String;
    function StyleStroke: String;

    procedure WriteFmt(const AFormat: String; AParams: array of const);
    procedure WriteStr(const AString: String);
  strict protected
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;

  public
    constructor Create(AStream: TStream; AWriteDocType: Boolean);
    destructor Destroy; override;
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    procedure DrawingBegin(const ABoundingBox: TRect); override;
    procedure DrawingEnd; override;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetFontAngle: Double; override;
    function GetFontColor: TFPColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TChartFontStyles; override;
    function GetPenColor: TChartColor;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer); override;
    procedure MoveTo(AX, AY: Integer); override;
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure PutImage(AX, AY: Integer; AImage: TFPCustomImage); override;
    procedure PutPixel(AX, AY: Integer; AColor: TChartColor); override;
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure ResetFont;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenColor(AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
  end;


  { TSVGChartHelper }

  TSVGChartHelper = class helper for TChart
    procedure SaveToSVGFile(const AFileName: String);
  end;


implementation

uses
  Base64, FPWritePNG, Math, SysUtils, TAGeometry;

const
  RECT_FMT =
    '<rect x="%d" y="%d" width="%d" height="%d" style="%s"/>';

function EscapeXML(const AText: String): String;
var
  ch: Char;
begin
  Result := '';
  for ch in AText do
    case ch of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '"': Result := Result + '&quot;';
      '''':Result := Result + '&apos;';
      '&': Result := Result + '&amp;';
      else Result := Result + ch;
    end;
end;

function ColorToHex(AColor: TFPColor): String;
begin
  if AColor = colBlack then
    Result := 'black'
  else if AColor = colWhite then
    Result := 'white'
  else
    with AColor do
      Result := Format('#%.2x%.2x%.2x', [red shr 8, green shr 8, blue shr 8]);
end;

function DP2S(AValue: TDoublePoint): String;
begin
  Result := Format('%g,%g', [AValue.X, AValue.Y], DefSeparatorSettings);
end;

function F2S(AValue: Double): String;
begin
  Result := FloatToStr(AValue, DefSeparatorSettings);
end;

function SVGGetFontOrientationFunc(AFont: TFPCustomFont): Integer;
begin
  if AFont is TFont then
    Result := TFont(AFont).Orientation
  else
    Result := AFont.Orientation;
end;

function SVGChartColorToFPColor(AChartColor: TChartColor): TFPColor;
begin
  Result := ChartColorToFPColor(ColorToRGB(AChartColor));
end;


{ TSVGDrawer }

procedure TSVGDrawer.AddToFontOrientation(ADelta: Integer);
begin
  FFontOrientation += ADelta;
end;

procedure TSVGDrawer.ClippingStart(const AClipRect: TRect);
begin
  FClippingPathId += 1;
  WriteFmt('<clipPath id="clip%d">', [FClippingPathId]);
  with AClipRect do
    WriteFmt(RECT_FMT, [Left, Top, Right - Left, Bottom - Top, '']);
  WriteStr('</clipPath>');
  ClippingStart;
end;

procedure TSVGDrawer.ClippingStart;
begin
  WriteFmt('<g clip-path="url(#clip%d)">', [FClippingPathId]);
end;

procedure TSVGDrawer.ClippingStop;
begin
  WriteStr('</g>');
end;

constructor TSVGDrawer.Create(AStream: TStream; AWriteDocType: Boolean);
begin
  inherited Create;
  InitFonts;
  FStream := AStream;
  FPatterns := TStringList.Create;
  FPen := TFPCustomPen.Create;
  FGetFontOrientationFunc := @SVGGetFontOrientationFunc;
  FChartColorToFPColorFunc := @SVGChartColorToFPColor;
  if AWriteDocType then begin
    WriteStr('<?xml version="1.0"?>');
    WriteStr('<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.0//EN"');
    WriteStr('"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">');
  end;
end;

destructor TSVGDrawer.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FPatterns);
  FreeAndNil(FPen);
  inherited Destroy;
end;

procedure TSVGDrawer.DrawingBegin(const ABoundingBox: TRect);
begin
  FAntialiasingMode := amDontCare;
  with ABoundingBox do
    WriteFmt(
      '<svg ' +
      'xmlns="http://www.w3.org/2000/svg" ' +
      'xmlns:xlink="http://www.w3.org/1999/xlink" ' +
      'width="%dpx" height="%dpx" viewBox="%d %d %d %d">',
      [Right - Left, Bottom - Top, Left, Top, Right, Bottom]);
  FClippingPathId := 0;
end;

procedure TSVGDrawer.DrawingEnd;
var
  i: Integer;
begin
  if FAntialiasingMode <> amDontCare then
    WriteStr('</g>');
  if FPatterns.Count > 0 then begin
    WriteStr('<defs>');
    for i := 0 to FPatterns.Count - 1 do
      WriteFmt(
        '<pattern id="bs%d" width="8" height="8" patternUnits="userSpaceOnUse">' +
        '%s</pattern>',
        [i, FPatterns[i]]);
    WriteStr('</defs>');
    FPatterns.Clear;
  end;
  WriteStr('</svg>');
end;

procedure TSVGDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
var
  e: TEllipse;
begin
  e.InitBoundingBox(AX1, AY1, AX2, AY2);
  WriteFmt(
    '<ellipse cx="%g" cy="%g" rx="%g" ry="%g" style="%s"/>',
    [e.FC.X, e.FC.Y, e.FR.X, e.FR.Y, StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(RECT_FMT, [AX1, AY1, AX2 - AX1, AY2 - AY1, StyleFill]);
end;

function TSVGDrawer.GetBrushColor: TChartColor;
begin
  Result := FPColorToChartColor(FBrushColor);
end;

function TSVGDrawer.GetFontAngle: Double;
begin
  Result := OrientToRad(FFontOrientation);
end;

function TSVGDrawer.GetFontColor: TFPColor;
begin
  Result := FFontColor;
end;

function TSVGDrawer.GetFontName: String;
begin
  Result := FFont.Family;
end;

function TSVGDrawer.GetFontSize: Integer;
begin
  Result := Round(FFont.SizeInPoints);
end;

function TSVGDrawer.GetFontStyle: TChartFontStyles;
begin
  Result := [];
  if ftsBold in FFont.Style then Include(Result, cfsBold);
  if ftsItalic in FFont.Style then Include(Result, cfsItalic);
  if FFont.UnderlineDecoration then Include(Result, cfsUnderline);
  if FFont.StrikeoutDecoration then Include(Result, cfsStrikeout);
end;

function TSVGDrawer.GetPenColor: TChartColor;
begin
  Result := FPColorToChartColor(FPen.FPColor);
end;

procedure TSVGDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(
    '<line x1="%d" y1="%d" x2="%d" y2="%d" style="%s"/>',
    [AX1, AY1, AX2, AY2, StyleStroke]);
end;

procedure TSVGDrawer.Line(const AP1, AP2: TPoint);
begin
  Line(AP1.X, AP1.Y, AP2.X, AP2.Y);
end;

procedure TSVGDrawer.LineTo(AX, AY: Integer);
begin
  Line(FPrevPos.X, FPrevPos.Y, AX, AY);
  FPrevPos := Point(AX, AY);
end;

procedure TSVGDrawer.MoveTo(AX, AY: Integer);
begin
  FPrevPos := Point(AX, AY);
end;

function TSVGDrawer.OpacityStr: String;
begin
  if FTransparency = 0 then
    Result := ''
  else
    Result := F2S((255 - FTransparency) / 256);
end;

function TSVGDrawer.PointsToStr(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer): String;
var
  i: Integer;
begin
  if ANumPts < 0 then
    ANumPts := Length(APoints) - AStartIndex;
  Result := '';
  for i := 0 to ANumPts - 1 do
    with APoints[i + AStartIndex] do
      Result += Format('%d %d ', [X, Y]);
end;

procedure TSVGDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  WriteFmt(
    '<polygon points="%s" style="%s"/>',
    [PointsToStr(APoints, AStartIndex, ANumPts), StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  WriteFmt(
    '<polyline points="%s" style="fill: none; %s"/>',
    [PointsToStr(APoints, AStartIndex, ANumPts), StyleStroke]);
end;

procedure TSVGDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  FPen.FPColor := FChartColorToFPColorFunc(ColorOrMono(AColor));
  FPen.Style := psSolid;
  FPen.Width := 1;
end;

procedure TSVGDrawer.PutImage(AX, AY: Integer; AImage: TFPCustomImage);
var
  s: TStringStream = nil;
  w: TFPWriterPNG = nil;
  b: TBase64EncodingStream = nil;
begin
  s := TStringStream.Create('');
  b := TBase64EncodingStream.Create(s);
  w := TFPWriterPNG.Create;
  try
    w.Indexed := false;
    w.UseAlpha := true;
    AImage.SaveToStream(b, w);
    b.Flush;
    WriteFmt(
      '<image x="%d" y="%d" width="%d" height="%d" ' +
      'xlink:href="data:image/png;base64,%s"/>',
      [AX, AY, AImage.Width, AImage.Height, s.DataString]);
  finally
    w.Free;
    s.Free;
    b.Free;
  end;
end;

procedure TSVGDrawer.PutPixel(AX, AY: Integer; AColor: TChartColor);
var
  stroke: String;
begin
  stroke := 'stroke:'+ColorToHex(FChartColorToFPColorFunc(ColorOrMono(AColor))) + ';stroke-width:1;';
  WriteFmt(RECT_FMT, [AX, AY, 1, 1, stroke]);
end;

procedure TSVGDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer; AStartAngle16Deg, AAngleLength16Deg: Integer);
var
  e: TEllipse;
  p1, p2: TDoublePoint;
begin
  e.InitBoundingBox(AX1, AY1, AX2, AY2);
  p1 := e.GetPoint(Deg16ToRad(AStartAngle16Deg));
  p2 := e.GetPoint(Deg16ToRad(AStartAngle16Deg + AAngleLength16Deg));
  WriteFmt(
    '<path d="M%s L%s A%s 0 0,0 %s Z" style="%s"/>',
    [DP2S(e.FC), DP2S(p1), DP2S(e.FR), DP2S(p2), StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  WriteFmt(
    RECT_FMT, [AX1, AY1, AX2 - AX1, AY2 - AY1, StyleFill + StyleStroke]);
end;

procedure TSVGDrawer.Rectangle(const ARect: TRect);
begin
  with ARect do
    Rectangle(Left, Top, Right, Bottom);
end;

procedure TSVGDrawer.ResetFont;
begin
  FFontOrientation := 0;
end;

procedure TSVGDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
const
  AM_TO_CSS: array [amOn .. amOff] of String =
    ('geometricPrecision', 'crispEdges');
begin
  if FAntialiasingMode = AValue then exit;
  if FAntialiasingMode <> amDontCare then
    WriteStr('</g>');
  FAntialiasingMode := AValue;
  if FAntialiasingMode <> amDontCare then
    WriteFmt('<g style="shape-rendering: %s">',[AM_TO_CSS[FAntialiasingMode]]);
end;

procedure TSVGDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  if ABrush is TBrush then
    FBrushColor := FChartColorToFPColorFunc(ColorOrMono(TBrush(ABrush).Color))
  else
    FBrushColor := FPColorOrMono(ABrush.FPColor);
  FBrushStyle := ABrush.Style;
end;

procedure TSVGDrawer.SetBrushColor(AColor: TChartColor);
begin
  FBrushColor := FChartColorToFPColorFunc(ColorOrMono(AColor));
end;

procedure TSVGDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  FBrushColor := FChartColorToFPColorFunc(ColorOrMono(AColor));
  FBrushStyle := AStyle;
end;

procedure TSVGDrawer.SetFont(AFont: TFPCustomFont);
var
  style: TFreeTypeStyles;
  fn: String;
begin
  style := [];
  if AFont.Bold then Include(style, ftsBold);
  if AFont.Italic then Include(style, ftsItalic);

  // create a new font if not yet loaded
  if (FFont = nil) or (FFont.Family <> AFont.Name) or(FFont.Style <> style) then
  begin
    FreeAndNil(FFont);
    if SameText(AFont.Name, 'default') then
      fn := 'Arial'   // FIXME: Find font in FontCollection!
    else
      fn := AFont.Name;
    FFont := LoadFont(fn, style);
    if FFont = nil then
      raise Exception.CreateFmt('Font "%s" not found."', [AFont.Name]);
  end;

  // Set the requested font attributes
  FFont.SizeInPoints := IfThen(AFont.Size = 0, DEFAULT_FONT_SIZE, AFont.Size);
  FFont.UnderlineDecoration := AFont.Underline;
  FFont.StrikeoutDecoration := AFont.StrikeThrough;
  FFont.Hinted := true;
  FFont.Quality := grqHighQuality;

  if FMonochromeColor <> clTAColor then
    FFontColor := FChartColorToFPColorFunc(FMonochromeColor)
  else
    FFontColor := AFont.FPColor;
  FFontOrientation := FGetFontOrientationFunc(AFont);
  FFontHeight := round(FFont.TextHeight('Tg'));
end;

procedure TSVGDrawer.SetPen(APen: TFPCustomPen);
begin
  if APen is TPen then
    FPen.FPColor := FChartColorToFPColorFunc(ColorOrMono(TPen(APen).Color))
  else
    FPen.FPColor := FPColorOrMono(APen.FPColor);
  FPen.Style := APen.Style;
  FPen.Width := APen.Width;
end;

procedure TSVGDrawer.SetPenColor(AColor: TChartColor);
begin
  FPen.FPColor := FChartColorToFPColorFunc(ColorOrMono(AColor));
end;

procedure TSVGDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  FPen.FPColor := FChartColorToFPColorFunc(ColorOrMono(AColor));
  FPen.Style := AStyle;
end;

function TSVGDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result.X := Round(FFont.TextWidth(AText));
  Result.Y := FFontHeight;
end;

type
  TFreeTypeFontOpener = class(TFreeTypeFont);

procedure TSVGDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);
var
  p: TPoint;
  stext: String;
  sstyle: String;
  dy: Integer;
  phi: Double;
begin
  dy := round(TFreeTypeFontOpener(FFont).GetAscent);
  phi := OrientToRad(FFontOrientation);
  p := RotatePoint(Point(0, dy), -phi) + Point(AX, AY);
  stext := Format('x="%d" y="%d"', [p.X, p.Y]);
  if FFontOrientation <> 0 then
    stext := stext + Format(' transform="rotate(%g,%d,%d)"',
      [-FFontOrientation*0.1, p.X, p.Y], DefSeparatorSettings);

  sstyle := Format('fill:%s; font-family:''%s''; font-size:%dpt;',
    [ColorToHex(GetFontColor), GetFontName, round(FFont.SizeInPoints)]);
  if (ftsBold in FFont.Style) then
    sstyle := sstyle + ' font-weight:bold;';
  if (ftsItalic in FFont.Style) then
    sstyle := sstyle + ' font-style:oblique;';
  if FFont.UnderlineDecoration and FFont.StrikeoutDecoration then
    sstyle := sstyle + ' text-decoration:underline,line-through;'
  else if FFont.UnderlineDecoration then
    sstyle := sstyle + ' text-deocration:underline;'
  else if FFont.StrikeoutDecoration then
    sstyle := sstyle + ' text-decoration:line-through;';
  if OpacityStr <> '' then
    sstyle := sstyle + OpacityStr + ';';

  WriteFmt('<text %s style="%s">%s</text>', [stext, sstyle, EscapeXML(AText)]);
end;

function TSVGDrawer.StyleFill: String;

  function AddPattern(APattern: String): String;
  var
    i: Integer;
  begin
    i := FPatterns.IndexOf(APattern);
    if i < 0 then
      i := FPatterns.Add(APattern);
    Result := Format('url(#bs%d)', [i]);
  end;

const
  PATTERNS: array [TFPBrushStyle] of String = (
    '', '',
    'M0,4 h8',              // bsHorizontal
    'M4,0 v8',              // bsVertical
    'M0,0 l8,8',            // bsFDiagonal
    'M0,8 l8,-8',           // bsBDiagonal
    'M0,4 h8 M4,0 v8',      // bsCross
    'M0,0 l8,8 M0,8 l8,-8', // bsDiagCross
    '', '');
var
  fill: String;
begin
  case FBrushStyle of
    bsClear: exit('fill: none;');
    bsHorizontal..bsDiagCross:
      fill := AddPattern(Format(
        '<path d="%s" stroke="%s"/>',
        [PATTERNS[FBrushStyle], ColorToHex(FBrushColor)]));
    else
      fill := ColorToHex(FBrushColor);
  end;
  Result :=
    Format('fill:%s;', [fill]) + FormatIfNotEmpty('fill-opacity:%s;', OpacityStr);
end;

function TSVGDrawer.StyleStroke: String;
const
  PEN_DASHARRAY: array [TFPPenStyle] of String =
    ('', '2,2', '1,1', '2,1,1,1', '2,1,1,1,1,1', '', '', '');
begin
  if FPen.Style = psClear then
    exit('stroke: none');
  Result := 'stroke:' + ColorToHex(FPen.FPColor) + ';';
  if FPen.Width <> 1 then
    Result += 'stroke-width:' + IntToStr(FPen.Width) + ';';
  Result +=
    FormatIfNotEmpty('stroke-dasharray:%s;', PEN_DASHARRAY[FPen.Style]) +
    FormatIfNotEmpty('stroke-opacity:%s;', OpacityStr);
end;

procedure TSVGDrawer.WriteFmt(const AFormat: String; AParams: array of const);
begin
  WriteStr(Format(AFormat, AParams, DefSeparatorSettings));
end;

procedure TSVGDrawer.WriteStr(const AString: String);
var
  le: String = LineEnding;
begin
  FStream.WriteBuffer(AString[1], Length(AString));
  FStream.WriteBuffer(le[1], Length(le));
end;


{ TSVGChartHelper }

procedure TSVGChartHelper.SaveToSVGFile(const AFileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    Draw(TSVGDrawer.Create(fs, true), Rect(0, 0, Width, Height));
  finally
    fs.Free;
  end;
end;


end.

