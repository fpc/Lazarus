{
Reads an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit svgvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  fpimage, fpcanvas, laz2_xmlread, laz2_dom, fgl,
  fpvectorial, fpvutils, lazutf8, TypInfo;

type
  TSVGTokenType = (
    // moves
    sttMoveTo, sttRelativeMoveTo,
    // Close Path
    sttClosePath,
    // lines
    sttLineTo, sttRelativeLineTo,
    sttHorzLineTo, sttRelativeHorzLineTo, sttVertLineTo, sttRelativeVertLineTo,
    // cubic beziers
    sttBezierTo, sttRelativeBezierTo, sttSmoothBezierTo, sttRelativeSmoothBezierTo,
    // quadratic beziers
    sttQuadraticBezierTo, sttRelativeQuadraticBezierTo,
    // Elliptic curves
    sttEllipticArcTo, sttRelativeEllipticArcTo,
    // numbers
    sttFloatValue);

  TSVGToken = class
    TokenType: TSVGTokenType;
    Value: Float;
  end;

  TSVGTokenList = specialize TFPGList<TSVGToken>;

  { TSVGPathTokenizer }

  TSVGPathTokenizer = class
  public
    FPointSeparator, FCommaSeparator: TFormatSettings;
    Tokens: TSVGTokenList;
    constructor Create;
    Destructor Destroy; override;
    procedure AddToken(AStr: string);
    procedure TokenizePathString(AStr: string);
    function DebugOutTokensAsString: string;
  end;

  { TvSVGVectorialReader }

  TvSVGVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    FSVGPathTokenizer: TSVGPathTokenizer;
    FLayerStylesKeys, FLayerStylesValues: TFPList; // of TStringList;
    function ReadSVGColor(AValue: string): TFPColor;
    procedure ReadSVGStyle(AValue: string; ADestEntity: TvEntityWithPen; AUseFillAsPen: Boolean = False);
    procedure ReadSVGPenStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPen);
    procedure ReadSVGBrushStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPenAndBrush);
    procedure ReadSVGFontStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPenBrushAndFont);
    function IsAttributeFromStyle(AStr: string): Boolean;
    procedure ApplyLayerStyles(ADestEntity: TvEntity);
    //
    procedure ReadEntityFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadCircleFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadEllipseFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadLayerFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadLineFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadPathFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadPathFromString(AStr: string; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadPointsFromString(AStr: string; AData: TvVectorialPage; ADoc: TvVectorialDocument; AClosePath: Boolean);
    procedure ReadPolyFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadRectFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadTextFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadUseFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    function  StringWithUnitToFloat(AStr: string): Double;
    procedure ConvertSVGCoordinatesToFPVCoordinates(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double);
    procedure ConvertSVGDeltaToFPVDelta(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double);
  public
    { General reading methods }
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

const
  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 0.2822; // DPI 90 = 1 / 90 inches per pixel
  FLOAT_PIXELS_PER_MILIMETER = 3.5433; // DPI 90 = 1 / 90 inches per pixel

{ TSVGPathTokenizer }

constructor TSVGPathTokenizer.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator

  Tokens := TSVGTokenList.Create;
end;

destructor TSVGPathTokenizer.Destroy;
begin
  Tokens.Free;

  inherited Destroy;
end;

procedure TSVGPathTokenizer.AddToken(AStr: string);
var
  lToken: TSVGToken;
  lStr: string;
begin
  lToken := TSVGToken.Create;

  lStr := Trim(AStr);
  if lStr = '' then Exit;

  // Moves
  if lStr[1] = 'M' then lToken.TokenType := sttMoveTo
  else if lStr[1] = 'm' then lToken.TokenType := sttRelativeMoveTo
  // Close Path
  else if lStr[1] = 'Z' then lToken.TokenType := sttClosePath
  else if lStr[1] = 'z' then lToken.TokenType := sttClosePath
  // Lines
  else if lStr[1] = 'L' then lToken.TokenType := sttLineTo
  else if lStr[1] = 'l' then lToken.TokenType := sttRelativeLineTo
  else if lStr[1] = 'H' then lToken.TokenType := sttHorzLineTo
  else if lStr[1] = 'h' then lToken.TokenType := sttRelativeHorzLineTo
  else if lStr[1] = 'V' then lToken.TokenType := sttVertLineTo
  else if lStr[1] = 'v' then lToken.TokenType := sttRelativeVertLineTo
  // cubic Bézier curve commands
  else if lStr[1] = 'C' then lToken.TokenType := sttBezierTo
  else if lStr[1] = 'c' then lToken.TokenType := sttRelativeBezierTo
  else if lStr[1] = 'S' then lToken.TokenType := sttSmoothBezierTo
  else if lStr[1] = 's' then lToken.TokenType := sttRelativeSmoothBezierTo
  // quadratic beziers
  else if lStr[1] = 'Q' then lToken.TokenType := sttQuadraticBezierTo
  else if lStr[1] = 'q' then lToken.TokenType := sttRelativeQuadraticBezierTo
  // Elliptic curves
  else if lStr[1] = 'A' then lToken.TokenType := sttEllipticArcTo
  else if lStr[1] = 'a' then lToken.TokenType := sttRelativeEllipticArcTo
  else
  begin
    lToken.TokenType := sttFloatValue;
    lToken.Value := StrToFloat(AStr, FPointSeparator);
  end;

  // Sometimes we get a command glued to a value, for example M150
  if (lToken.TokenType <> sttFloatValue) and (Length(lStr) > 1) then
  begin
    Tokens.Add(lToken);
    lToken.TokenType := sttFloatValue;
    lStr := Copy(AStr, 2, Length(AStr));
    lToken.Value := StrToFloat(lStr, FPointSeparator);
  end;

  Tokens.Add(lToken);
end;

procedure TSVGPathTokenizer.TokenizePathString(AStr: string);
const
  Str_Space: Char = ' ';
  Str_Comma: Char = ',';
var
  i: Integer;
  lTmpStr: string = '';
  lState: Integer;
  lFirstTmpStrChar, lCurChar: Char;
begin
  lState := 0;

  i := 1;
  while i <= Length(AStr) do
  begin
    case lState of
    0: // Adding to the tmp string
    begin
      lCurChar := AStr[i];
      if lCurChar = Str_Space then
      begin
        lState := 1;
        AddToken(lTmpStr);
        lTmpStr := '';
      end
      else if lCurChar = Str_Comma then
      begin
        AddToken(lTmpStr);
        lTmpStr := '';
      end
      else
      begin
        // Check for a break, from letter to number
        if (Length(lTmpStr) >= 1) then
        begin
          lFirstTmpStrChar := lTmpStr[1];
          if ((lFirstTmpStrChar in ['a'..'z', 'A'..'Z']) and not (lCurChar  in ['a'..'z', 'A'..'Z'])) or
             (not (lFirstTmpStrChar in ['a'..'z', 'A'..'Z']) and (lCurChar  in ['a'..'z', 'A'..'Z'])) then
          begin
            AddToken(lTmpStr);
            lTmpStr := '';
            Continue;
          end;
        end;

        lTmpStr := lTmpStr + lCurChar;
      end;

      Inc(i);
    end;
    1: // Removing spaces
    begin
      if AStr[i] <> Str_Space then lState := 0
      else Inc(i);
    end;
    end;
  end;

  // If there is a token still to be added, add it now
  if (lState = 0) and (lTmpStr <> '') then AddToken(lTmpStr);
end;

function TSVGPathTokenizer.DebugOutTokensAsString: string;
var
  i: Integer;
begin
  for i := 0 to Tokens.Count-1 do
    Result := Result + GetEnumName(TypeInfo(TSVGTokenType), integer(Tokens.Items[i].TokenType))
      + Format('(%f) ', [Tokens.Items[i].Value]);
end;

{ Example of a supported SVG image:

<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!-- Created with fpVectorial (http://wiki.lazarus.freepascal.org/fpvectorial) -->

<svg
  xmlns:dc="http://purl.org/dc/elements/1.1/"
  xmlns:cc="http://creativecommons.org/ns#"
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  xmlns:svg="http://www.w3.org/2000/svg"
  xmlns="http://www.w3.org/2000/svg"
  xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
  width="100mm"
  height="100mm"
  id="svg2"
  version="1.1"
  sodipodi:docname="New document 1">
  <g id="layer1">
  <path
    style="fill:none;stroke:#000000;stroke-width:10px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    d="m 0,283.486888731396 l 106.307583274274,-35.4358610914245 "
  id="path0" />
  <path
    style="fill:none;stroke:#000000;stroke-width:10px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    d="m 0,354.358610914245 l 354.358610914245,0 l 0,-354.358610914245 l -354.358610914245,0 l 0,354.358610914245 "
  id="path1" />
  <path
    style="fill:none;stroke:#000000;stroke-width:10px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1"
    d="m 0,354.358610914245 l 35.4358610914245,-35.4358610914245 c 0,-35.4358610914246 35.4358610914245,-35.4358610914246 35.4358610914245,0 l 35.4358610914245,35.4358610914245 "
  id="path2" />
  </g>
</svg>
}

{ TvSVGVectorialReader }

function TvSVGVectorialReader.ReadSVGColor(AValue: string): TFPColor;
var
  lValue, lStr: string;
  lStrings: TStringList;
  i: Integer;
begin
  Result := colBlack;
  lValue := Trim(LowerCase(AValue));

  // Support for rgb(255,255,0)
  if (Length(lValue) > 3) and (Copy(lValue, 0, 3) = 'rgb') then
  begin
    lStrings := TStringList.Create;
    try
      lStr := Copy(lValue, 5, Length(lValue)-5);
      lStrings.Delimiter := ',';
      lStrings.DelimitedText := lStr;
      if lStrings.Count = 3 then
      begin
        Result.Red := StrToInt(lStrings.Strings[0]) * $101;
        Result.Blue := StrToInt(lStrings.Strings[1]) * $101;
        Result.Green := StrToInt(lStrings.Strings[2]) * $101;
      end
      else
        raise Exception.Create(Format('[TvSVGVectorialReader.ReadSVGColor] An unexpected number of channels was found: %d', [lStrings.Count]));
    finally
      lStrings.Free;
    end;
    Exit;
  end;

  // Support for RGB hex
  // ex: #0000ff
  // Another wierd valid variant: #000
  if (Length(lValue) > 1) and (lValue[1] = '#') then
  begin
    lStr := Copy(lValue, 2, 2);
    Result.Red := StrToInt('$'+lStr)*$101;
    lStr := Copy(lValue, 4, 2);
    if lStr = '' then Result.Green := 0
    else Result.Green := StrToInt('$'+lStr)*$101;
    lStr := Copy(lValue, 6, 2);
    if lStr = '' then Result.Blue := 0
    else Result.Blue := StrToInt('$'+lStr)*$101;
    Exit;
  end;

  // Support for named colors
  // List here: http://www.december.com/html/spec/colorsvghex.html
  case lValue of
  'black':   Result := colBlack;
  'navy':    Result.Blue := $8080;
  'darkblue':Result.Blue := $8B8B;
  'mediumblue':Result.Blue := $CDCD;
  'blue':    Result := colBlue;
  'darkgreen':Result.Green := $6464;
  'green':   Result.Green := $8080;
  'teal':
  begin
    Result.Green := $8080;
    Result.Blue := $8080;
  end;
  'darkcyan':
  begin
    Result.Green := $8B8B;
    Result.Blue := $8B8B;
  end;
  'deepskyblue':
  begin
    Result.Green := $BFBF;
    Result.Blue := $FFFF;
  end;
  'darkturquoise':
  begin
    Result.Green := $CECE;
    Result.Blue := $D1D1;
  end;
  'mediumspringgreen':
  begin
    Result.Green := $FAFA;
    Result.Blue := $9A9A;
  end;
  'lime': Result := colGreen;
  'springgreen':
  begin
    Result.Green := $FFFF;
    Result.Blue := $7F7F;
  end;
  'cyan':   Result := colCyan;
  'aqua':   Result := colCyan;
  'midnightblue':
  begin
    Result.Red := $1919;
    Result.Green := $1919;
    Result.Blue := $7070;
  end;
  'dodgerblue':
  begin
    Result.Red := $1E1E;
    Result.Green := $9090;
    Result.Blue := $FFFF;
  end;
  'lightseagreen':
  begin
    Result.Red := $2020;
    Result.Green := $B2B2;
    Result.Blue := $AAAA;
  end;
  'forestgreen':
  begin
    Result.Red := $2222;
    Result.Green := $8B8B;
    Result.Blue := $2222;
  end;
  'seagreen':
  begin
    Result.Red := $2E2E;
    Result.Green := $8B8B;
    Result.Blue := $5757;
  end;
  'darkslategray', 'darkslategrey':
  begin
    Result.Red := $2F2F;
    Result.Green := $4F4F;
    Result.Blue := $4F4F;
  end;
  'limegreen':
  begin
    Result.Red := $3232;
    Result.Green := $CDCD;
    Result.Blue := $3232;
  end;
  'mediumseagreen':
  begin
    Result.Red := $3C3C;
    Result.Green := $CBCB;
    Result.Blue := $7171;
  end;
  'turquoise':
  begin
    Result.Red := $4040;
    Result.Green := $E0E0;
    Result.Blue := $D0D0;
  end;
  'royalblue':
  begin
    Result.Red := $4141;
    Result.Green := $6969;
    Result.Blue := $E1E1;
  end;
  'steelblue':
  begin
    Result.Red := $4646;
    Result.Green := $8282;
    Result.Blue := $B4B4;
  end;
  'darkslateblue':
  begin
    Result.Red := $4848;
    Result.Green := $3D3D;
    Result.Blue := $8B8B;
  end;
  'mediumturquoise':
  begin
    Result.Red := $4848;
    Result.Green := $D1D1;
    Result.Blue := $CCCC;
  end;
{
indigo #4B0082
 	darkolivegreen #556B2F		cadetblue #5F9EA0
cornflowerblue #6495ED
 	mediumaquamarine #66CDAA		dimgrey #696969
dimgray #696969
 	slateblue #6A5ACD		olivedrab #6B8E23
slategrey #708090
 	slategray #708090		lightslategray(Hex3) #778899
lightslategrey(Hex3) #778899
 	mediumslateblue #7B68EE		lawngreen #7CFC00
chartreuse #7FFF00
}
  'aquamarine':
  begin
    Result.Red := $7F7F;
    Result.Green := $FFFF;
    Result.Blue := $D4D4;
  end;
  'maroon': Result.Red := $8080;
  'purple': Result := colPurple;
  'olive':  Result := colOlive;
  'gray', 'grey': Result := colGray;
  'skyblue':
  begin
    Result.Red := $8787;
    Result.Green := $CECE;
    Result.Blue := $EBEB;
  end;
  'lightskyblue':
  begin
    Result.Red := $8787;
    Result.Green := $CECE;
    Result.Blue := $FAFA;
  end;
  'blueviolet':
  begin
    Result.Red := $8A8A;
    Result.Green := $2B2B;
    Result.Blue := $E2E2;
  end;
  'darkred': Result.Red := $8B8B;
  'darkmagenta':
  begin
    Result.Red := $8B8B;
    Result.Blue := $8B8B;
  end;
{
saddlebrown #8B4513
 	darkseagreen #8FBC8F		lightgreen #90EE90
mediumpurple #9370DB
 	darkviolet #9400D3		palegreen #98FB98
darkorchid #9932CC
 	yellowgreen #9ACD32		sienna #A0522D
brown #A52A2A
 	darkgray #A9A9A9		darkgrey #A9A9A9
lightblue #ADD8E6
 	greenyellow #ADFF2F		paleturquoise #AFEEEE
lightsteelblue #B0C4DE
 	powderblue #B0E0E6		firebrick #B22222
darkgoldenrod #B8860B
 	mediumorchid #BA55D3		rosybrown #BC8F8F
darkkhaki #BDB76B
}
  'silver': Result := colSilver;
  'mediumvioletred':
  begin
    Result.Red := $C7C7;
    Result.Green := $1515;
    Result.Blue := $8585;
  end;
  'indianred':
  begin
    Result.Red := $CDCD;
    Result.Green := $5C5C;
    Result.Blue := $5C5C;
  end;
  'peru':
  begin
    Result.Red := $CDCD;
    Result.Green := $8585;
    Result.Blue := $3F3F;
  end;
  'chocolate':
  begin
    Result.Red := $D2D2;
    Result.Green := $6969;
    Result.Blue := $1E1E;
  end;
{
tan #D2B48C
 	lightgray #D3D3D3		lightgrey #D3D3D3
thistle #D8BFD8
 	orchid #DA70D6		goldenrod #DAA520
palevioletred #DB7093
 	crimson #DC143C		gainsboro #DCDCDC
plum #DDA0DD
 	burlywood #DEB887		lightcyan #E0FFFF
lavender #E6E6FA
}
  'darksalmon':
  begin
    Result.Red := $E9E9;
    Result.Green := $9696;
    Result.Blue := $7A7A;
  end;
  'violet':
  begin
    Result.Red := $EEEE;
    Result.Green := $8282;
    Result.Blue := $EEEE;
  end;
  'palegoldenrod':
  begin
    Result.Red := $EEEE;
    Result.Green := $E8E8;
    Result.Blue := $AAAA;
  end;
  'lightcoral':
  begin
    Result.Red := $F0F0;
    Result.Green := $8080;
    Result.Blue := $8080;
  end;
  'khaki':
  begin
    Result.Red := $F0F0;
    Result.Green := $E6E6;
    Result.Blue := $8C8C;
  end;
  'aliceblue':
  begin
    Result.Red := $F0F0;
    Result.Green := $F8F8;
    Result.Blue := $FFFF;
  end;
  'honeydew':
  begin
    Result.Red := $F0F0;
    Result.Green := $FFFF;
    Result.Blue := $F0F0;
  end;
  'azure':
  begin
    Result.Red := $F0F0;
    Result.Green := $FFFF;
    Result.Blue := $FFFF;
  end;
  'sandybrown':
  begin
    Result.Red := $F4F4;
    Result.Green := $A4A4;
    Result.Blue := $6060;
  end;
{
 	wheat #F5DEB3		beige #F5F5DC
whitesmoke #F5F5F5
 	mintcream #F5FFFA		ghostwhite #F8F8FF
salmon #FA8072
 	antiquewhite #FAEBD7		linen #FAF0E6
lightgoldenrodyellow #FAFAD2
 	oldlace #FDF5E6
}
  'red':   Result := colRed;
  'fuchsia':   Result := colFuchsia;
  'magenta':   Result := colMagenta;
{	deeppink #FF1493
orangered #FF4500
 	tomato #FF6347		hotpink #FF69B4
coral #FF7F50
 	darkorange #FF8C00		lightsalmon #FFA07A
orange #FFA500
 	lightpink #FFB6C1		pink #FFC0CB
gold #FFD700
 	peachpuff #FFDAB9		navajowhite #FFDEAD
moccasin #FFE4B5
 	bisque #FFE4C4		mistyrose #FFE4E1
blanchedalmond #FFEBCD
 	papayawhip #FFEFD5		lavenderblush #FFF0F5
seashell #FFF5EE
 	cornsilk #FFF8DC		lemonchiffon #FFFACD
floralwhite #FFFAF0
}
  'snow':
  begin
    Result.Red := $FFFF;
    Result.Green := $FAFA;
    Result.Blue := $FAFA;
  end;
  'yellow': Result := colYellow;
  'lightyellow':
  begin
    Result.Red := $FFFF;
    Result.Green := $FEFE;
  end;
  'ivory':
  begin
    Result.Red := $FFFF;
    Result.Green := $FFFF;
    Result.Blue := $F0F0;
  end;
  'white': Result := colWhite;
  end;
end;

// style="fill:none;stroke:black;stroke-width:3"
procedure TvSVGVectorialReader.ReadSVGStyle(AValue: string;
  ADestEntity: TvEntityWithPen; AUseFillAsPen: Boolean = False);
var
  lStr, lStyleKeyStr, lStyleValueStr: String;
  lStrings: TStringList;
  i, lPosEqual: Integer;
begin
  if AValue = '' then Exit;

  // Now split using ";" separator
  lStrings := TStringList.Create;
  try
    lStrings.Delimiter := ';';
    lStrings.DelimitedText := LowerCase(AValue);
    for i := 0 to lStrings.Count-1 do
    begin
      lStr := lStrings.Strings[i];
      lPosEqual := Pos(':', lStr);
      lStyleKeyStr := Copy(lStr, 0, lPosEqual-1);
      lStyleValueStr := Copy(lStr, lPosEqual+1, Length(lStr));
      ReadSVGPenStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity);
      if AUseFillAsPen and (lStyleKeyStr = 'fill') then
        ReadSVGPenStyleWithKeyAndValue('stroke', lStyleValueStr, ADestEntity)
      else if ADestEntity is TvText then
        ReadSVGFontStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvText)
      else if ADestEntity is TvEntityWithPenAndBrush then
        ReadSVGBrushStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvEntityWithPenAndBrush);
    end;
  finally
    lStrings.Free;
  end;
end;

procedure TvSVGVectorialReader.ReadSVGPenStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPen);
var
  OldAlpha: Word;
begin
  if AKey = 'stroke' then
  begin
    // We store and restore the old alpha to support the "-opacity" element
    OldAlpha := ADestEntity.Pen.Color.Alpha;
    if ADestEntity.Pen.Style = psClear then ADestEntity.Pen.Style := psSolid;

    if AValue = 'none'  then ADestEntity.Pen.Style := fpcanvas.psClear
    else
    begin
      ADestEntity.Pen.Color := ReadSVGColor(AValue);
      ADestEntity.Pen.Color.Alpha := OldAlpha;
    end;
  end
  else if AKey = 'stroke-width' then
    ADestEntity.Pen.Width := Round(StringWithUnitToFloat(AValue))
  else if AKey = 'stroke-opacity' then
    ADestEntity.Pen.Color.Alpha := StrToInt(AValue)*$101
  else if AKey = 'stroke-linecap' then
  begin
    {case LowerCase(AValue) of
    'butt':
    'round':
    'square': ADestEntity.Pen;
    end;}
  end;
end;

procedure TvSVGVectorialReader.ReadSVGBrushStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPenAndBrush);
var
  OldAlpha: Word;
begin
  if AKey = 'fill' then
  begin
    // We store and restore the old alpha to support the "-opacity" element
    OldAlpha := ADestEntity.Brush.Color.Alpha;
    if ADestEntity.Brush.Style = bsClear then ADestEntity.Brush.Style := bsSolid;

    if AValue = 'none'  then ADestEntity.Brush.Style := fpcanvas.bsClear
    else
    begin
      ADestEntity.Brush.Color := ReadSVGColor(AValue);
      ADestEntity.Brush.Color.Alpha := OldAlpha;
    end;
  end
  else if AKey = 'fill-opacity' then
    ADestEntity.Brush.Color.Alpha := StrToInt(AValue)*$101;
end;

procedure TvSVGVectorialReader.ReadSVGFontStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPenBrushAndFont);
begin
  // SVG text uses "fill" to indicate the pen color of the text, very unintuitive as
  // "fill" is usually for brush in other elements
  if AKey = 'fill' then
    ADestEntity.Font.Color := ReadSVGColor(AValue)
  else if AKey = 'fill-opacity' then
    ADestEntity.Font.Color.Alpha := StrToInt(AValue)*$101
  else if AKey = 'font-size' then
    ADestEntity.Font.Size := Round(StringWithUnitToFloat(AValue))
  else if AKey = 'font-family' then
    ADestEntity.Font.Name := AValue
  else if AKey = 'font-weight' then
  begin
    case LowerCase(AValue) of
    'bold': ADestEntity.Font.Bold := True;
    end;
  end;
end;

function TvSVGVectorialReader.IsAttributeFromStyle(AStr: string): Boolean;
begin
  Result := (AStr = 'stroke') or (AStr = 'stroke-width') or
    (AStr = 'stroke-dasharray') or (AStr = 'stroke-opacity') or
    (AStr = 'stroke-linecap') or
    // brush
    (AStr = 'fill') or (AStr = 'fill-opacity') or
    // font
    (AStr = 'font-size') or (AStr = 'fill-family') or
    (AStr = 'font-weight');
end;

procedure TvSVGVectorialReader.ApplyLayerStyles(ADestEntity: TvEntity);
var
  lStringsKeys, lStringsValues: TStringList;
  i, j: Integer;
begin
  for i := 0 to FLayerStylesKeys.Count-1 do
  begin
    lStringsKeys := TStringList(FLayerStylesKeys.Items[i]);
    lStringsValues := TStringList(FLayerStylesValues.Items[i]);
    for j := 0 to lStringsKeys.Count-1 do
    begin
      if ADestEntity is TvEntityWithPen then
        ReadSVGPenStyleWithKeyAndValue(lStringsKeys.Strings[j], lStringsValues.Strings[j], ADestEntity as TvEntityWithPen);
      if ADestEntity is TvEntityWithPenAndBrush then
        ReadSVGBrushStyleWithKeyAndValue(lStringsKeys.Strings[j], lStringsValues.Strings[j], ADestEntity as TvEntityWithPenAndBrush);
      if ADestEntity is TvEntityWithPenBrushAndFont then
        ReadSVGFontStyleWithKeyAndValue(lStringsKeys.Strings[j], lStringsValues.Strings[j], ADestEntity as TvEntityWithPenBrushAndFont);
    end;
  end;
end;

procedure TvSVGVectorialReader.ReadEntityFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lEntityName: DOMString;
begin
  lEntityName := LowerCase(ANode.NodeName);
  case lEntityName of
    'circle': ReadCircleFromNode(ANode, AData, ADoc);
    'ellipse': ReadEllipseFromNode(ANode, AData, ADoc);
    'g': ReadLayerFromNode(ANode, AData, ADoc);
    'line': ReadLineFromNode(ANode, AData, ADoc);
    'path': ReadPathFromNode(ANode, AData, ADoc);
    'polygon', 'polyline': ReadPolyFromNode(ANode, AData, ADoc);
    'rect': ReadRectFromNode(ANode, AData, ADoc);
    'text': ReadTextFromNode(ANode, AData, ADoc);
    'use': ReadUseFromNode(ANode, AData, ADoc);
  end;
end;

procedure TvSVGVectorialReader.ReadCircleFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  cx, cy, cr, dtmp: double;
  lCircle: TvCircle;
  i: Integer;
  lNodeName: DOMString;
begin
  cx := 0.0;
  cy := 0.0;
  cr := 0.0;

  lCircle := TvCircle.Create;
  // SVG entities start without any pen drawing, but with a black brush
  lCircle.Pen.Style := psClear;
  lCircle.Brush.Style := bsSolid;
  lCircle.Brush.Color := colBlack;

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if  lNodeName = 'cx' then
      cx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'cy' then
      cy := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'r' then
      cr := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'id' then
      lCircle.Name := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lCircle)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lCircle);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lCircle);
    end;
  end;

  ConvertSVGDeltaToFPVDelta(
        AData, cx, cy, lCircle.X, lCircle.Y);
  ConvertSVGDeltaToFPVDelta(
        AData, cr, 0, lCircle.Radius, dtmp);

  AData.AddEntity(lCircle);
end;

procedure TvSVGVectorialReader.ReadEllipseFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  cx, cy, crx, cry: double;
  lEllipse: TvEllipse;
  i: Integer;
  lNodeName: DOMString;
begin
  cx := 0.0;
  cy := 0.0;
  crx := 0.0;
  cry := 0.0;

  lEllipse := TvEllipse.Create;
  // SVG entities start without any pen drawing, but with a black brush
  lEllipse.Pen.Style := psClear;
  lEllipse.Brush.Style := bsSolid;
  lEllipse.Brush.Color := colBlack;

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if  lNodeName = 'cx' then
      cx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'cy' then
      cy := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'rx' then
      crx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'ry' then
      cry := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'id' then
      lEllipse.Name := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lEllipse)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lEllipse);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lEllipse);
    end;
  end;

  ConvertSVGDeltaToFPVDelta(
        AData, cx, cy, lEllipse.X, lEllipse.Y);
  ConvertSVGDeltaToFPVDelta(
        AData, crx, cry, lEllipse.HorzHalfAxis, lEllipse.VertHalfAxis);

  AData.AddEntity(lEllipse);
end;

procedure TvSVGVectorialReader.ReadLayerFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lNodeName: DOMString;
  lLayerName: string = '';
  lCurNode, lLayerNameNode: TDOMNode;
  lLayer, lParentLayer: TvLayer;
  i: Integer;
  lLayerStyleKeys, lLayerStyleValues: TStringList;
begin
  // Store the style of this layer in the list
  lLayerStyleKeys := TStringList.Create;
  lLayerStyleValues := TStringList.Create;
  FLayerStylesKeys.Add(lLayerStyleKeys);
  FLayerStylesValues.Add(lLayerStyleValues);

  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'id' then
      lLayerName := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      lLayerStyleKeys.Add(lNodeName);
      lLayerStyleValues.Add(UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue));
    end;
  end;

  lParentLayer := AData.GetCurrentLayer();
  lLayer := AData.AddLayerAndSetAsCurrent(lLayerName);

  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    ReadEntityFromNode(lCurNode, AData, ADoc);
    lCurNode := lCurNode.NextSibling;
  end;

  // Now remove the style from this layer
  FLayerStylesKeys.Remove(lLayerStyleKeys);
  lLayerStyleKeys.Free;
  FLayerStylesValues.Remove(lLayerStyleValues);
  lLayerStyleValues.Free;

  // Set the current layer to the parent node,
  // or else items read next will be put as children of this layer
  AData.SetCurrentLayer(lParentLayer);
end;

procedure TvSVGVectorialReader.ReadLineFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  x1, y1, x2, y2: double;
  vx1, vy1, vx2, vy2: double;
  i: Integer;
  lNodeName: DOMString;
  lPath: TPath;
  lStyleStr, lStrokeStr, lStrokeWidthStr: DOMString;
begin
  x1 := 0.0;
  y1 := 0.0;
  x2 := 0.0;
  y2 := 0.0;

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if  lNodeName = 'x1' then
      x1 := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'y1' then
      y1 := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'x2' then
      x2 := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'y2' then
      y2 := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      lStyleStr := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'stroke' then
      lStrokeStr := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'stroke-width' then
      lStrokeWidthStr := ANode.Attributes.Item[i].NodeValue;
  end;

  ConvertSVGCoordinatesToFPVCoordinates(
        AData, x1, y1, vx1, vy1);
  ConvertSVGCoordinatesToFPVCoordinates(
        AData, x2, y2, vx2, vy2);

  AData.StartPath();
  AData.AddMoveToPath(vx1, vy1);
  AData.AddLineToPath(vx2, vy2);
  lPath := AData.EndPath();
  // Add the pen/brush
  ReadSVGStyle(lStyleStr, lPath);
  ReadSVGStyle(lStrokeStr, lPath);
  ReadSVGStyle(lStrokeWidthStr, lPath);
end;

procedure TvSVGVectorialReader.ReadPathFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lNodeName, lDStr: WideString;
  i: Integer;
  lPath: TPath;
begin
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'd' then
      lDStr := ANode.Attributes.Item[i].NodeValue;
  end;

  AData.StartPath();
  ReadPathFromString(UTF8Encode(lDStr), AData, ADoc);
  lPath := AData.EndPath();
  // Add default SVG pen/brush
  lPath.Pen.Style := psClear;
  lPath.Brush.Color := colBlack;
  lPath.Brush.Style := bsSolid;
  // Apply the layer style
  ApplyLayerStyles(lPath);
  // Add the pen/brush/name
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'id' then
      lPath.Name := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
    end;
  end;
end;

// Documentation: http://www.w3.org/TR/SVG/paths.html
procedure TvSVGVectorialReader.ReadPathFromString(AStr: string;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i: Integer;
  X, Y, X2, Y2, X3, Y3: Double;
  CurX, CurY: Double;
  lCurTokenType, lLastCommandToken: TSVGTokenType;
  lDebugStr: String;
begin
  FSVGPathTokenizer.Tokens.Clear;
  FSVGPathTokenizer.TokenizePathString(AStr);
  //lDebugStr := FSVGPathTokenizer.DebugOutTokensAsString();
  CurX := 0;
  CurY := 0;
  lLastCommandToken := sttFloatValue;

  i := 0;
  while i < FSVGPathTokenizer.Tokens.Count do
  begin
    lCurTokenType := FSVGPathTokenizer.Tokens.Items[i].TokenType;
    if not (lCurTokenType = sttFloatValue) then lLastCommandToken := lCurTokenType;
    // --------------
    // Moves
    // --------------
    if lCurTokenType in [sttMoveTo, sttRelativeMoveTo] then
    begin
      X := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);

      // take care of relative or absolute
      if lCurTokenType = sttRelativeMoveTo then
      begin
        CurX := CurX + X;
        CurY := CurY + Y;
      end
      else
      begin
        CurX := X;
        CurY := Y;
      end;
      AData.AddMoveToPath(CurX, CurY);

      Inc(i, 3);
    end
    // --------------
    // Lines which appear without a command token and after a MoveTo
    // --------------
    else if (lCurTokenType = sttFloatValue) and (lLastCommandToken in [sttMoveTo, sttRelativeMoveTo]) then
    begin
      X := FSVGPathTokenizer.Tokens.Items[i].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      CurX := X;
      CurY := Y;
      AData.AddLineToPath(CurX, CurY);

      Inc(i, 2);
    end
    // --------------
    // Close Path
    // --------------
    else if lCurTokenType = sttClosePath then
    begin
      // Get the first point
      AData.GetTmpPathStartPos(X, Y);

      // And repeat it
      CurX := X;
      CurY := Y;
      AData.AddLineToPath(CurX, CurY);

      Inc(i, 3);
    end
    // --------------
    // Lines
    // --------------
    else if lCurTokenType in [sttLineTo, sttRelativeLineTo, sttHorzLineTo,
      sttRelativeHorzLineTo, sttVertLineTo, sttRelativeVertLineTo] then
    begin
      X := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      if not (lCurTokenType in [sttHorzLineTo, sttRelativeHorzLineTo, sttVertLineTo, sttRelativeVertLineTo]) then
        Y := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      // horizontal and vertical line corrections
      if lCurTokenType in [sttHorzLineTo, sttRelativeHorzLineTo] then
        Y := 0
      else if lCurTokenType in [sttVertLineTo, sttRelativeVertLineTo] then
      begin
        Y := X;
        X := 0;
      end;

      // "l" LineTo uses relative coordenates in SVG
      if lCurTokenType in [sttRelativeLineTo, sttRelativeHorzLineTo, sttRelativeVertLineTo] then
      begin
        CurX := CurX + X;
        CurY := CurY + Y;
      end
      else
      begin
        CurX := X;
        CurY := Y;
      end;
      AData.AddLineToPath(CurX, CurY);

      if not (lCurTokenType in [sttHorzLineTo, sttRelativeHorzLineTo, sttVertLineTo, sttRelativeVertLineTo]) then
        Inc(i, 3)
      else Inc(i, 2);
    end
    // --------------
    // Cubic Bezier
    // --------------
    else if lCurTokenType in [sttBezierTo, sttRelativeBezierTo,
      sttSmoothBezierTo, sttRelativeSmoothBezierTo] then
    begin
      if lCurTokenType in [sttBezierTo, sttRelativeBezierTo] then
      begin
        X2 := FSVGPathTokenizer.Tokens.Items[i+1].Value;
        Y2 := FSVGPathTokenizer.Tokens.Items[i+2].Value;
        X3 := FSVGPathTokenizer.Tokens.Items[i+3].Value;
        Y3 := FSVGPathTokenizer.Tokens.Items[i+4].Value;
        X := FSVGPathTokenizer.Tokens.Items[i+5].Value;
        Y := FSVGPathTokenizer.Tokens.Items[i+6].Value;
      end
      else
      begin
        X2 := CurX;
        Y2 := CurY;
        X3 := FSVGPathTokenizer.Tokens.Items[i+1].Value;
        Y3 := FSVGPathTokenizer.Tokens.Items[i+2].Value;
        X := FSVGPathTokenizer.Tokens.Items[i+3].Value;
        Y := FSVGPathTokenizer.Tokens.Items[i+4].Value;
      end;

      ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
      ConvertSVGDeltaToFPVDelta(AData, X3, Y3, X3, Y3);
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      if lCurTokenType = sttRelativeBezierTo then
      begin
        AData.AddBezierToPath(X2 + CurX, Y2 + CurY, X3 + CurX, Y3 + CurY, X + CurX, Y + CurY);
        CurX := CurX + X;
        CurY := CurY + Y;
      end
      else
      begin
        AData.AddBezierToPath(X2, Y2, X3, Y3, X, Y);
        CurX := X;
        CurY := Y;
      end;

      if lCurTokenType in [sttBezierTo, sttRelativeBezierTo] then
        Inc(i, 7)
      else Inc(i, 5);
    end
    // --------------
    // Quadratic Bezier
    // --------------
    else if lCurTokenType in [sttQuadraticBezierTo, sttRelativeQuadraticBezierTo] then
    begin
      X2 := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y2 := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      X := FSVGPathTokenizer.Tokens.Items[i+3].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+4].Value;

      ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      if lCurTokenType = sttRelativeQuadraticBezierTo then
      begin
        AData.AddBezierToPath(X2 + CurX, Y2 + CurY, X2 + CurX, Y2 + CurY, X + CurX, Y + CurY);
        CurX := CurX + X;
        CurY := CurY + Y;
      end
      else
      begin
        AData.AddBezierToPath(X2, Y2, X2, Y2, X, Y);
        CurX := X;
        CurY := Y;
      end;

      Inc(i, 5);
    end
    // --------------
    // Elliptical arcs
    // --------------
    else if lCurTokenType in [sttEllipticArcTo, sttRelativeEllipticArcTo] then
    begin
      {X2 := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y2 := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      X := FSVGPathTokenizer.Tokens.Items[i+3].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+4].Value;

      ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      if lCurTokenType = sttRelativeQuadraticBezierTo then
      begin
        AData.AddBezierToPath(X2 + CurX, Y2 + CurY, X2 + CurX, Y2 + CurY, X + CurX, Y + CurY);
        CurX := CurX + X;
        CurY := CurY + Y;
      end
      else
      begin
        AData.AddBezierToPath(X2, Y2, X2, Y2, X, Y);
        CurX := X;
        CurY := Y;
      end; }

      Inc(i, 8);
    end
    else
    begin
      Inc(i);
    end;
  end;
end;

procedure TvSVGVectorialReader.ReadPointsFromString(AStr: string;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AClosePath: Boolean);
var
  i: Integer;
  X, Y: Double;
  FirstPtX, FirstPtY, CurX, CurY: Double;
begin
  FSVGPathTokenizer.Tokens.Clear;
  FSVGPathTokenizer.TokenizePathString(AStr);
  CurX := 0;
  CurY := 0;

  if FSVGPathTokenizer.Tokens.Count <= 2 then
    raise Exception.Create('[TvSVGVectorialReader.ReadPointsFromString] There are too few points in the element');

  // The first point
  CurX := FSVGPathTokenizer.Tokens.Items[0].Value;
  CurY := FSVGPathTokenizer.Tokens.Items[1].Value;
  ConvertSVGCoordinatesToFPVCoordinates(AData, CurX, CurY, CurX, CurY);
  FirstPtX := CurX;
  FirstPtY := CurY;
  AData.AddMoveToPath(CurX, CurY);

  // Now all other points
  i := 2;
  while i < FSVGPathTokenizer.Tokens.Count do
  begin
    X := FSVGPathTokenizer.Tokens.Items[i].Value;
    Y := FSVGPathTokenizer.Tokens.Items[i+1].Value;
    ConvertSVGDeltaToFPVDelta(AData, X, Y, CurX, CurY);
    AData.AddLineToPath(CurX, CurY);

    Inc(i, 2);
  end;

  // and if we want, close the path
  if AClosePath then
    AData.AddLineToPath(FirstPtX, FirstPtY);
end;

// polygon and polyline are very similar
procedure TvSVGVectorialReader.ReadPolyFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lPointsStr: string = '';
  i: Integer;
  lNodeName: DOMString;
  lPath: TPath;
  lIsPolygon: Boolean = False;
begin
  lIsPolygon := LowerCase(ANode.NodeName) = 'polygon';

  // first get the points
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if  lNodeName = 'points' then
      lPointsStr := ANode.Attributes.Item[i].NodeValue;
  end;

  AData.StartPath();
  ReadPointsFromString(lPointsStr, AData, ADoc, lIsPolygon);
  lPath := AData.EndPath();

  // now read the other attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'id' then
      lPath.Name := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
    end;
  end;
end;

//          <rect width="90" height="90" stroke="green" stroke-width="3" fill="yellow" filter="url(#f1)" />
procedure TvSVGVectorialReader.ReadRectFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lx, ly, cx, cy, lrx, lry: double;
  lRect: TvRectangle;
  i: Integer;
  lNodeName: DOMString;
begin
  lx := 0.0;
  ly := 0.0;
  cx := 0.0;
  cy := 0.0;
  lrx := 0.0;
  lry := 0.0;

  lRect := TvRectangle.Create;
  // SVG entities start without any pen drawing, but with a black brush
  lRect.Pen.Style := psClear;
  lRect.Brush.Style := bsSolid;
  lRect.Brush.Color := colBlack;

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if  lNodeName = 'x' then
      lx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'y' then
      ly := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'rx' then
      lrx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'ry' then
      lry := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'width' then
      cx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'height' then
      cy := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'id' then
      lRect.Name := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lRect)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lRect);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lRect);
    end;
  end;

  ConvertSVGDeltaToFPVDelta(
        AData, lx, ly, lRect.X, lRect.Y);
  ConvertSVGDeltaToFPVDelta(
        AData, cx, cy, lRect.CX, lRect.CY);
  ConvertSVGDeltaToFPVDelta(
        AData, lrx, lry, lRect.RX, lRect.RY);
  lRect.RX := Abs(lRect.RX) * 2;
  lRect.RY := Abs(lRect.RY) * 2;

  AData.AddEntity(lRect);
end;

procedure TvSVGVectorialReader.ReadTextFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lTextStr: string = '';
  lx, ly: double;
  lText: TvText;
  i: Integer;
  lNodeName: DOMString;
  lCurNode: TDOMNode;
begin
  lx := 0.0;
  ly := 0.0;

  lText := TvText.Create;

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if  lNodeName = 'x' then
      lx := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'y' then
      ly := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'id' then
      lText.Name := UTF16ToUTF8(ANode.Attributes.Item[i].NodeValue)
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lText, True)
    else if IsAttributeFromStyle(lNodeName) then
      ReadSVGFontStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lText);
  end;

  // The text contents are inside as a child text, not as a attribute
  // ex:   <text x="0" y="15" fill="red" transform="rotate(30 20,40)">I love SVG</text>
  if Anode.FirstChild <> nil then
    lTextStr := Anode.FirstChild.NodeValue;
  // Add the first line
  lText.Value.Add(lTextStr);

  // Set the coordinates
  ConvertSVGDeltaToFPVDelta(
        AData, lx, ly, lText.X, lText.Y);

  // Now add other lines, which appear as <tspan ...>another line</tspan>
  // Example:
  // <text x="10" y="20" style="fill:red;">Several lines:
  //   <tspan x="10" y="45">First line</tspan>
  //   <tspan x="10" y="70">Second line</tspan>
  // </text>
  // These other lines can be positioned, so they need to appear as independent TvText elements
{  lCurNode := Anode.FirstChild;
  while lCurNode <> nil do
  begin
    lNodeName := LowerCase(lCurNode.NodeName);
    if lNodeName <> 'tspan' then Continue;
    ReadTextFromNode(lCurNode, AData, ADoc);

    lCurNode := lCurNode.NextSibling;
  end;}

  // Finalization
  AData.AddEntity(lText);
end;

procedure TvSVGVectorialReader.ReadUseFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
begin

end;

function TvSVGVectorialReader.StringWithUnitToFloat(AStr: string): Double;
var
  UnitStr, ValueStr: string;
  Len: Integer;
  LastChar: Char;
begin
  if AStr = '' then Exit(0.0);

  // Check the unit
  Len := Length(AStr);
  UnitStr := Copy(AStr, Len-1, 2);
  LastChar := AStr[Len];
  if UnitStr = 'mm' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToFloat(ValueStr, FPointSeparator);
  end
  else if UnitStr = 'cm' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToFloat(ValueStr, FPointSeparator) * 10;
  end
  else if UnitStr = 'px' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToFloat(ValueStr, FPointSeparator);
  end
  else if LastChar = '%' then
  begin
    ValueStr := Copy(AStr, 1, Len-1);
    Result := StrToInt(ValueStr);
  end
  else // If there is no unit, just use StrToFloat
  begin
    Result := StrToFloat(AStr, FPointSeparator);
  end;
end;

procedure TvSVGVectorialReader.ConvertSVGCoordinatesToFPVCoordinates(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double;
  var ADestX,ADestY: Double);
begin
  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := AData.Height - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
end;

procedure TvSVGVectorialReader.ConvertSVGDeltaToFPVDelta(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: Double);
begin
  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
end;

constructor TvSVGVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator

  FSVGPathTokenizer := TSVGPathTokenizer.Create;
  FLayerStylesKeys := TFPList.Create;
  FLayerStylesValues := TFPList.Create;
end;

destructor TvSVGVectorialReader.Destroy;
begin
  FLayerStylesKeys.Free;
  FLayerStylesValues.Free;
  FSVGPathTokenizer.Free;

  inherited Destroy;
end;

procedure TvSVGVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument;
  lCurNode: TDOMNode;
  lPage: TvVectorialPage;
begin
  try
    // Read in xml file from the stream
    ReadXMLFile(Doc, AStream);

    // Read the properties of the <svg> tag
    AData.Width := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('width'));
    AData.Height := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('height'));

    // Now process the elements
    lCurNode := Doc.DocumentElement.FirstChild;
    lPage := AData.AddPage();
    lPage.Width := AData.Width;
    lPage.Height := AData.Height;
    while Assigned(lCurNode) do
    begin
      ReadEntityFromNode(lCurNode, lPage, AData);
      lCurNode := lCurNode.NextSibling;
    end;
  finally
    // finally, free the document
    Doc.Free;
  end;
end;

initialization

  RegisterVectorialReader(TvSVGVectorialReader, vfSVG);

end.

