{
Reads an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit svgvectorialreader;

{$mode objfpc}{$H+}
{$define SVG_MERGE_LAYER_STYLES}

interface

uses
  Classes, SysUtils, math, contnrs,
  fpimage, fpcanvas, laz2_xmlread, laz2_dom, fgl,
  // image data formats
  fpreadpng,
  fpvectorial, fpvutils, lazutf8, TypInfo;

type
  TDoubleArray = array of Double;

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
    StrValue: string; // filled only by TokenizeFunctions
  end;

  TSVGTokenList = specialize TFPGList<TSVGToken>;

  { TSVGObjectStack }

  TSVGObjectStack = class(TObjectStack)
  public
    function GetList: TList;
  end;

  { TSVGTextSpanStyle }

  TSVGTextSpanStyle = class(TvStyle)
  public
    PositionSet: Boolean;
    X, Y: Double;
    function GenerateDebugTree(ADestRoutine: TvDebugAddItemProc; APageItem: Pointer): Pointer; override;
  end;

  { TSVGPathTokenizer }

  TSVGPathTokenizer = class
  public
    FPointSeparator, FCommaSeparator: TFormatSettings;
    Tokens: TSVGTokenList;
    ExtraDebugStr: string;
    constructor Create;
    Destructor Destroy; override;
    procedure AddToken(AStr: string);
    procedure TokenizePathString(AStr: string);
    procedure TokenizeFunctions(AStr: string);
    function DebugOutTokensAsString: string;
  end;

  TSVGCoordinateKind = (sckUnknown, sckX, sckY, sckXDelta, sckYDelta, sckXSize, sckYSize);

  TSVGUnit = (suPX, suMM, suPT {Points});

  { TvSVGVectorialReader }

  TvSVGVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    FSVGPathTokenizer: TSVGPathTokenizer;
    FLayerStylesKeys, FLayerStylesValues: TFPList; // of TStringList;
    // View box adjustment
    ViewBoxAdjustment: Boolean;
    ViewBox_Left, ViewBox_Top, ViewBox_Width, ViewBox_Height, Page_Width, Page_Height: Double;
    // Defs section
    FBrushDefs: TFPList; // of TvEntityWithPenAndBrush;
    // debug symbols
    FPathNumber: Integer;
    function ReadSVGColor(AValue: string): TFPColor;
    function ReadSVGStyle(AValue: string; ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil; AUseFillAsPen: Boolean = False): TvSetPenBrushAndFontElements;
    function ReadSVGStyleToStyleLists(AValue: string; AStyleKeys, AStyleValues: TStringList): TvSetPenBrushAndFontElements;
    function ReadSVGPenStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPen): TvSetPenBrushAndFontElements;
    function ReadSVGBrushStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPenAndBrush): TvSetPenBrushAndFontElements;
    function ReadSVGFontStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPenBrushAndFont; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
    function ReadSVGGeneralStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntity): TvSetPenBrushAndFontElements;
    function IsAttributeFromStyle(AStr: string): Boolean;
    procedure ApplyLayerStyles(ADestEntity: TvEntity);
    function ReadSpaceSeparatedFloats(AInput: string; AOtherSeparators: string): TDoubleArray;
    procedure ReadSVGTransformationMatrix(AMatrix: string; out AA, AB, AC, AD, AE, AF: Double);
    //
    function GetTextContentFromNode(ANode: TDOMNode): string;
    procedure ReadDefsFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    //
    function ReadEntityFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadCircleFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadEllipseFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadFrameFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadFrameTextFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadImageFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    procedure ReadLayerFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    function ReadLineFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadPathFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    procedure ReadPathFromString(AStr: string; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadNextPathCommand(ACurTokenType: TSVGTokenType; var i: Integer; var CurX, CurY: Double; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadPointsFromString(AStr: string; AData: TvVectorialPage; ADoc: TvVectorialDocument; AClosePath: Boolean);
    function ReadPolyFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadRectFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadSymbolFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadTextFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadUseFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    //
    function  StringWithUnitToFloat(AStr: string; ACoordKind: TSVGCoordinateKind = sckUnknown;
      ADefaultUnit: TSVGUnit = suPX; ATargetUnit: TSVGUnit = suPX): Double;
    function  StringFloatZeroToOneToWord(AStr: string): Word;
    procedure ConvertSVGCoordinatesToFPVCoordinates(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double;
      ADoViewBoxAdjust: Boolean = True);
    procedure ConvertSVGDeltaToFPVDelta(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double;
      ADoViewBoxAdjust: Boolean = True);
    procedure ConvertSVGSizeToFPVSize(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double;
      ADoViewBoxAdjust: Boolean = True);
    procedure AutoDetectDocSize(var ALeft, ATop, ARight, ABottom: Double; ABaseNode: TDOMNode);
    function SVGColorValueStrToWord(AStr: string): Word;
  public
    { General reading methods }
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
    procedure ReadFromXML(Doc: TXMLDocument; AData: TvVectorialDocument); override;
    class function ReadSpaceSeparatedStrings(AInput: string; AOtherSeparators: string): TStringList;
  end;

implementation

const
  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 1; //0.2822; // DPI 90 = 1 / 90 inches per pixel => Actually I changed the value! Because otherwise it looks ugly!
  FLOAT_PIXELS_PER_MILIMETER = 1 / FLOAT_MILIMETERS_PER_PIXEL; // DPI 90 = 1 / 90 inches per pixel

  FLOAT_POINTS_PER_PIXEL = 0.75; // For conversion
  FLOAT_PIXEL_PER_POINT = 1 / FLOAT_POINTS_PER_PIXEL; // For conversion

{ TSVGTextSpanStyle }

function TSVGTextSpanStyle.GenerateDebugTree(ADestRoutine: TvDebugAddItemProc;
  APageItem: Pointer): Pointer;
begin
  FExtraDebugStr := '';
  if PositionSet then
    FExtraDebugStr := FExtraDebugStr + Format(' X=%f Y=%f', [X, Y]);
  Result:=inherited GenerateDebugTree(ADestRoutine, APageItem);
end;

{ TSVGObjectStack }

function TSVGObjectStack.GetList: TList;
begin
  Result := List;
end;

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
    try
      lToken.Value := StrToFloat(AStr, FPointSeparator);
    except
      on MyException: Exception do
      begin
        MyException.Message := MyException.Message + ExtraDebugStr;
        raise MyException;
      end;
    end;
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
  ListOfCommandLetters: set of Char = ['a'..'d', 'f'..'z', 'A'..'D', 'F'..'Z'];
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
        // But don't forget that we need to support 3.799e-4 !!
        // So e is not a valid letter for commands here
        if (Length(lTmpStr) >= 1) then
        begin
          lFirstTmpStrChar := lTmpStr[1];
          if ((lFirstTmpStrChar in ListOfCommandLetters) and not (lCurChar  in ListOfCommandLetters)) or
             (not (lFirstTmpStrChar in ListOfCommandLetters) and (lCurChar  in ListOfCommandLetters)) then
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

procedure TSVGPathTokenizer.TokenizeFunctions(AStr: string);
const
  Str_Space: Char = ' ';
  Str_Start_Params: Char = '(';
  Str_End_Params: Char = ')';
  ListOfCommandLetters: set of Char = ['a'..'d', 'f'..'z', 'A'..'D', 'F'..'Z'];
var
  i: Integer;
  lTmpStr: string = '';
  lState: Integer;
  lFirstTmpStrChar, lCurChar: Char;
  lToken: TSVGToken;
begin
  lState := 0;

  i := 1;
  while i <= Length(AStr) do
  begin
    case lState of
    0: // Adding to the tmp string
    begin
      lCurChar := AStr[i];
      if lCurChar in [Str_Start_Params, Str_End_Params] then
      begin
        lState := 1;
        // Add the token
        lToken := TSVGToken.Create;
        lToken.StrValue := lTmpStr;
        Tokens.Add(lToken);
        //
        lTmpStr := '';
      end
      else
      begin
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
  Result := '';
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
  HasAlphaChannel: Boolean = False;
begin
  Result := colBlack;
  lValue := Trim(LowerCase(AValue));

  // Support for rgb(255,255,0)
  if (Length(lValue) > 3) and (Copy(lValue, 0, 3) = 'rgb') then
  begin
    lStrings := TStringList.Create;
    try
      HasAlphaChannel := Copy(lValue, 0, 4) = 'rgba';
      if HasAlphaChannel then lStr := Copy(lValue, 6, Length(lValue)-6)
      else lStr := Copy(lValue, 5, Length(lValue)-5);
      lStrings.Delimiter := ',';
      lStrings.StrictDelimiter := True;
      lStrings.DelimitedText := lStr;
      if lStrings.Count in [3, 4] then
      begin
        Result.Red := SVGColorValueStrToWord(lStrings.Strings[0]);
        Result.Green := SVGColorValueStrToWord(lStrings.Strings[1]);
        Result.Blue := SVGColorValueStrToWord(lStrings.Strings[2]);
        if (lStrings.Count = 4) and HasAlphaChannel then
          Result.alpha := StringFloatZeroToOneToWord(lStrings.Strings[3]);
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
function TvSVGVectorialReader.ReadSVGStyle(AValue: string;
  ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil;
  AUseFillAsPen: Boolean = False): TvSetPenBrushAndFontElements;
var
  lStr, lStyleKeyStr, lStyleValueStr: String;
  lStrings: TStringList;
  i, lPosEqual: Integer;
begin
  Result := [];
  if AValue = '' then Exit;

  // Now split using ";" separator
  lStrings := TStringList.Create;
  try
    lStrings.Delimiter := ';';
    lStrings.StrictDelimiter := True;
    lStrings.DelimitedText := LowerCase(AValue);
    for i := 0 to lStrings.Count-1 do
    begin
      lStr := lStrings.Strings[i];
      lPosEqual := Pos(':', lStr);
      lStyleKeyStr := Copy(lStr, 0, lPosEqual-1);
      lStyleKeyStr := Trim(lStyleKeyStr);
      lStyleValueStr := Copy(lStr, lPosEqual+1, Length(lStr));
      lStyleValueStr := Trim(lStyleValueStr);
      if ADestEntity <> nil then
      begin
        ReadSVGPenStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity);
        ReadSVGGeneralStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity);
        if AUseFillAsPen and (lStyleKeyStr = 'fill') then
          Result := Result + ReadSVGPenStyleWithKeyAndValue('stroke', lStyleValueStr, ADestEntity)
        else if ADestEntity is TvText then
          Result := Result + ReadSVGFontStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvText)
        else if ADestEntity is TvEntityWithPenAndBrush then
          Result := Result + ReadSVGBrushStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvEntityWithPenAndBrush);
      end;
      if ADestStyle <> nil then
      begin
        {ReadSVGPenStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity);
        ReadSVGGeneralStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity);
        Result := Result + ReadSVGPenStyleWithKeyAndValue('stroke', lStyleValueStr, ADestEntity)}
        Result := Result + ReadSVGFontStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, nil, ADestStyle);
        //Result := Result + ReadSVGBrushStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvEntityWithPenAndBrush);
      end;
    end;
  finally
    lStrings.Free;
  end;
end;

// style="fill:none;stroke:black;stroke-width:3"
function TvSVGVectorialReader.ReadSVGStyleToStyleLists(AValue: string;
  AStyleKeys, AStyleValues: TStringList): TvSetPenBrushAndFontElements;
var
  lStr, lStyleKeyStr, lStyleValueStr: String;
  lStrings: TStringList;
  i, lPosEqual: Integer;
begin
  Result := [];
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
      AStyleKeys.Add(lStyleKeyStr);
      AStyleValues.Add(lStyleValueStr);
    end;
  finally
    lStrings.Free;
  end;
end;

function TvSVGVectorialReader.ReadSVGPenStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPen): TvSetPenBrushAndFontElements;
var
  OldAlpha: Word;
begin
  Result := [];
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
    Result := Result + [spbfPenColor, spbfPenStyle];
  end
  else if AKey = 'stroke-width' then
  begin
    ADestEntity.Pen.Width := Round(StringWithUnitToFloat(AValue, sckXSize));
    Result := Result + [spbfPenWidth];
  end
  else if AKey = 'stroke-opacity' then
  begin
    ADestEntity.Pen.Color.Alpha := Round(StrToFloat(AValue)*$FFFF);
  end
  else if AKey = 'stroke-linecap' then
  begin
    {case LowerCase(AValue) of
    'butt':
    'round':
    'square': ADestEntity.Pen;
    end;}
  end;
end;

function TvSVGVectorialReader.ReadSVGBrushStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPenAndBrush): TvSetPenBrushAndFontElements;
var
  OldAlpha: Word;
  Len: Integer;
  lDefName: String;
  i: Integer;
  lCurBrush: TvEntityWithPenAndBrush;
begin
  Result := [];
  if AKey = 'fill' then
  begin
    // Support for fill="url(#grad2)"
    lDefName := Trim(AValue);
    if Copy(lDefName, 0, 3) = 'url' then
    begin
      lDefName := StringReplace(AValue, 'url(#', '', []);
      lDefName := StringReplace(lDefName, ')', '', []);
      if lDefName = '' then Exit;

      for i := 0 to FBrushDefs.Count-1 do
      begin
        lCurBrush := TvEntityWithPenAndBrush(FBrushDefs.Items[i]);
        if lCurBrush.Name = lDefName then
        begin
          ADestEntity.Brush := lCurBrush.Brush;
          Exit;
        end;
      end;
      Exit;
    end;

    // We store and restore the old alpha to support the "-opacity" element
    OldAlpha := ADestEntity.Brush.Color.Alpha;
    if ADestEntity.Brush.Style = bsClear then ADestEntity.Brush.Style := bsSolid;

    if AValue = 'none'  then ADestEntity.Brush.Style := fpcanvas.bsClear
    else
    begin
      ADestEntity.Brush.Color := ReadSVGColor(AValue);
      ADestEntity.Brush.Color.Alpha := OldAlpha;
    end;

    Result := Result + [spbfBrushColor, spbfBrushStyle];
  end
  else if AKey = 'fill-opacity' then
    ADestEntity.Brush.Color.Alpha := StringFloatZeroToOneToWord(AValue)
  // For linear gradient => stop-color:rgb(255,255,0);stop-opacity:1
  else if AKey = 'stop-color' then
  begin
    Len := Length(ADestEntity.Brush.Gradient_colors);
    SetLength(ADestEntity.Brush.Gradient_colors, Len+1);
    ADestEntity.Brush.Gradient_colors[Len] := ReadSVGColor(AValue);
  end;
end;

function TvSVGVectorialReader.ReadSVGFontStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPenBrushAndFont; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
var
  lLowerValue: String;
begin
  Result := [];
  lLowerValue := LowerCase(AValue);
  // SVG text uses "fill" to indicate the pen color of the text, very unintuitive as
  // "fill" is usually for brush in other elements
  if AKey = 'fill' then
  begin
    if ADestEntity <> nil then ADestEntity.Font.Color := ReadSVGColor(AValue);
    if ADestStyle <> nil then ADestStyle.Font.Color := ReadSVGColor(AValue);
    Result := Result + [spbfFontColor];
  end
  // But sometimes SVG also uses stroke! Oh no...
  else if AKey = 'stroke' then
  begin
    if ADestEntity <> nil then ADestEntity.Font.Color := ReadSVGColor(AValue);
    if lLowerValue <> 'none' then // sometimes we get a fill value, but a stroke=none after it...
    begin
      if ADestStyle <> nil then ADestStyle.Font.Color := ReadSVGColor(AValue);
      Result := Result + [spbfFontColor];
    end;
    Result := Result + [spbfFontColor];
  end
  else if AKey = 'fill-opacity' then
  begin
    if ADestEntity <> nil then ADestEntity.Font.Color.Alpha := StrToInt(AValue)*$101;
    if ADestStyle <> nil then ADestStyle.Font.Color.Alpha := StrToInt(AValue)*$101;
    Result := Result + [spbfFontColor];
  end
  else if AKey = 'font-size' then
  begin
    if ADestEntity <> nil then ADestEntity.Font.Size := Round(StringWithUnitToFloat(AValue, sckXSize, suPX, suPT));
    if ADestStyle <> nil then ADestStyle.Font.Size := Round(StringWithUnitToFloat(AValue, sckXSize, suPX, suPT));
    Result := Result + [spbfFontSize];
  end
  else if AKey = 'font-family' then
  begin
    if ADestEntity <> nil then ADestEntity.Font.Name := AValue;
    if ADestStyle <> nil then ADestStyle.Font.Name := AValue;
    Result := Result + [spbfFontName];
  end
  else if AKey = 'font-weight' then
  begin
    case lLowerValue of
    'bold', '700', '800', '900':
    begin
      if ADestEntity <> nil then ADestEntity.Font.Bold := True;
      if ADestStyle <> nil then ADestStyle.Font.Bold := True;
    end;
    else
      if ADestEntity <> nil then ADestEntity.Font.Bold := False;
      if ADestStyle <> nil then ADestStyle.Font.Bold := False;
    end;
    Result := Result + [spbfFontBold];
  end
  // Other text attributes, non-font ones
  else if AKey = 'text-anchor' then
  begin
    // Adjust according to the text-anchor, if necessary
    case lLowerValue of
    'start':
    begin
      if ADestEntity <> nil then ADestEntity.TextAnchor := vtaStart;
      if ADestStyle <> nil then ADestStyle.TextAnchor := vtaStart;
    end;
    'middle':
    begin
      if ADestEntity <> nil then ADestEntity.TextAnchor := vtaMiddle;
      if ADestStyle <> nil then ADestStyle.TextAnchor := vtaMiddle;
    end;
    'end':
    begin
      if ADestEntity <> nil then ADestEntity.TextAnchor := vtaEnd;
      if ADestStyle <> nil then ADestStyle.TextAnchor := vtaEnd;
    end;
    end;
    Result := Result + [spbfTextAnchor];
  end;
  if ADestStyle <> nil then
    ADestStyle.SetElements := ADestStyle.SetElements + Result;
end;

function TvSVGVectorialReader.ReadSVGGeneralStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntity): TvSetPenBrushAndFontElements;
var
  // transform
  MA, MB, MC, MD, ME, MF: Double;
  lMTranslateX, lMTranslateY, lMScaleX, lMScaleY, lMSkewX, lMSkewY, lMRotate: Double;
  lTokenizer: TSVGPathTokenizer;
  i: Integer;
  lFunctionName, lParamStr: string;
var
  lMatrixElements: array of Double;
begin
  // Examples:
  // transform="matrix(0.860815 0 -0 1.07602 339.302 489.171)"
  // transform="scale(0.24) translate(0, 35)"
  // transform="rotate(90)"
  if AKey = 'transform' then
  begin
    lTokenizer := TSVGPathTokenizer.Create;
    try
      lTokenizer.TokenizeFunctions(AValue);

      i := 0;
      while i < lTokenizer.Tokens.Count-1 do
      begin
        lFunctionName := Trim(lTokenizer.Tokens.Items[i].StrValue);
        lParamStr := lTokenizer.Tokens.Items[i+1].StrValue;
        lMatrixElements := ReadSpaceSeparatedFloats(lParamStr, ',');

        if lFunctionName = 'matrix' then
        begin
          ReadSVGTransformationMatrix(lParamStr, MA, MB, MC, MD, ME, MF);

          ConvertTransformationMatrixToOperations(MA, MB, MC, MD, ME, MF,
            lMTranslateX, lMTranslateY, lMScaleX, lMScaleY, lMSkewX, lMSkewY, lMRotate);

          ConvertSVGDeltaToFPVDelta(nil,
            lMTranslateX, lMTranslateY,
            lMTranslateX, lMTranslateY);

          ADestEntity.Move(lMTranslateX, lMTranslateY);
          ADestEntity.Scale(lMScaleX, lMScaleY);
        end
        else if lFunctionName = 'scale' then
        begin
          ;
        end
        else if lFunctionName = 'translate' then
        begin
          ConvertSVGDeltaToFPVDelta(nil,
            lMatrixElements[0], lMatrixElements[1],
            lMatrixElements[0], lMatrixElements[1]);
          ADestEntity.Move(lMatrixElements[0], lMatrixElements[1]);
        end
        else if lFunctionName = 'rotate' then
        begin
          ADestEntity.Rotate(lMatrixElements[0], Make3DPoint(0, 0, 0));
        end;

        Inc(i, 2);
      end;
    finally
      lTokenizer.Free;
    end;
  end;
end;

function TvSVGVectorialReader.IsAttributeFromStyle(AStr: string): Boolean;
begin
  Result :=
    // pen
    (AStr = 'stroke') or (AStr = 'stroke-width') or
    (AStr = 'stroke-dasharray') or (AStr = 'stroke-opacity') or
    (AStr = 'stroke-linecap') or
    // general
    (AStr = 'transform') or
    // brush
    (AStr = 'fill') or (AStr = 'fill-opacity') or
    // font
    (AStr = 'font-size') or (AStr = 'fill-family') or
    (AStr = 'font-weight') or (AStr = 'text-anchor');
end;

procedure TvSVGVectorialReader.ApplyLayerStyles(ADestEntity: TvEntity);
var
  lStringsKeys, lStringsValues: TStringList;
  i, j: Integer;
  lCurKey, lCurValue: String;
begin
  for i := 0 to FLayerStylesKeys.Count-1 do
  begin
    lStringsKeys := TStringList(FLayerStylesKeys.Items[i]);
    lStringsValues := TStringList(FLayerStylesValues.Items[i]);
    for j := 0 to lStringsKeys.Count-1 do
    begin
      lCurKey := lStringsKeys.Strings[j];
      lCurValue := lStringsValues.Strings[j];
      if ADestEntity is TvEntityWithPen then
        ReadSVGPenStyleWithKeyAndValue(lCurKey, lCurValue, ADestEntity as TvEntityWithPen);
      // Unfortunately SVG uses 'fill' for the text color =/ so we need to hack
      // our way out of this ambiguity with the brush fill
      if (ADestEntity is TvEntityWithPenAndBrush) and (not (ADestEntity is TvText)) then
        ReadSVGBrushStyleWithKeyAndValue(lCurKey, lCurValue, ADestEntity as TvEntityWithPenAndBrush);
      if ADestEntity is TvEntityWithPenBrushAndFont then
        ReadSVGFontStyleWithKeyAndValue(lCurKey, lCurValue, ADestEntity as TvEntityWithPenBrushAndFont);
      // transform
      ReadSVGGeneralStyleWithKeyAndValue(lCurKey, lCurValue, ADestEntity);
    end;
  end;
end;

function TvSVGVectorialReader.ReadSpaceSeparatedFloats(AInput: string;
  AOtherSeparators: string): TDoubleArray;
var
  lStrings: TStringList;
  lInputStr: string;
  lMatrixElements: array of Double;
  i: Integer;
begin
  lStrings := TStringList.Create;
  try
    lStrings.Delimiter := ' ';
    // now other separator too
    lInputStr := AInput;
    for i := 1 to Length(AOtherSeparators) do
    begin
      lInputStr := StringReplace(lInputStr, AOtherSeparators[i], ' ', [rfReplaceAll]);
    end;
    //
    lStrings.DelimitedText := lInputStr;
    SetLength(lMatrixElements, lStrings.Count);
    for i := 0 to lStrings.Count-1 do
    begin
      lMatrixElements[i] := StringWithUnitToFloat(lStrings.Strings[i]);
    end;

    Result := lMatrixElements;
  finally
    lStrings.Free;
  end;
end;

class function TvSVGVectorialReader.ReadSpaceSeparatedStrings(AInput: string;
  AOtherSeparators: string): TStringList;
var
  i: Integer;
  lInputStr: String;
begin
  Result := TStringList.Create;
  Result.Delimiter := ' ';
  // now other separator too
  lInputStr := AInput;
  for i := 1 to Length(AOtherSeparators) do
  begin
    lInputStr := StringReplace(lInputStr, AOtherSeparators[i], ' ', [rfReplaceAll]);
  end;
  //
  Result.DelimitedText := lInputStr;
end;

// transform="matrix(0.860815 0 -0 1.07602 354.095 482.177)"=>matrix(a, b, c, d, e, f)
// Looks like that some Apps separate the matrix elements with spaces
// But Inkscape uses commas! transform="matrix(1.2,0,0,1.2,9.48633,-2.25781)"
// See http://apike.ca/prog_svg_transform.html
procedure TvSVGVectorialReader.ReadSVGTransformationMatrix(
  AMatrix: string; out AA, AB, AC, AD, AE, AF: Double);
var
  lMatrixElements: array of Double;
begin
  lMatrixElements := ReadSpaceSeparatedFloats(AMatrix, ',');

  AA := lMatrixElements[0];
  AB := lMatrixElements[1];
  AC := lMatrixElements[2];
  AD := lMatrixElements[3];
  AE := lMatrixElements[4];
  AF := lMatrixElements[5];
end;

function TvSVGVectorialReader.GetTextContentFromNode(ANode: TDOMNode): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to ANode.ChildNodes.Count-1 do
  begin
    if ANode.ChildNodes.Item[i] is TDOMText then
      Result := ANode.ChildNodes.Item[i].NodeValue;
  end;
end;

procedure TvSVGVectorialReader.ReadDefsFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lEntityName: DOMString;
  lBlock: TvBlock;
  lPreviousLayer: TvEntityWithSubEntities;
  lAttrName, lAttrValue, lNodeName: DOMString;
  lLayerName: String;
  i: Integer;
  lCurNode, lCurSubNode: TDOMNode;
  lBrushEntity: TvEntityWithPenAndBrush;
  lCurEntity: TvEntity;
  lOffset: Double;
  x1, x2, y1, y2: string;
begin
  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lEntityName := LowerCase(lCurNode.NodeName);
    case lEntityName of
      'radialgradient':
      begin
        lBrushEntity := TvEntityWithPenAndBrush.Create(nil);
        lBrushEntity.Brush.Kind := bkRadialGradient;

        // <radialGradient id="grad1" cx="50%" cy="50%" r="50%" fx="50%" fy="50%">
        for i := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lAttrName := lCurNode.Attributes.Item[i].NodeName;
          lAttrValue := lCurNode.Attributes.Item[i].NodeValue;
          if lAttrName = 'id' then
          begin
            lBrushEntity.Name := lAttrValue;
          end;
          {else if lAttrName = 'cx' then
          begin
            lLayerName := lCurNode.Attributes.Item[i].NodeValue;
            lBlock.Name := lLayerName;
          end;
          }
        end;

{        Gradient_cx, Gradient_cy, Gradient_r, Gradient_fx, Gradient_fy: Double;
        Gradient_cx_Unit, Gradient_cy_Unit, Gradient_r_Unit, Gradient_fx_Unit, Gradient_fy_Unit: TvCoordinateUnit;
        Gradient_colors: array of TFPColor;}

        //  <stop offset="0%" style="stop-color:rgb(255,255,255); stop-opacity:0" />
        //  <stop offset="100%" style="stop-color:rgb(0,0,255);stop-opacity:1" />
        //</radialGradient>}
        lCurSubNode := lCurNode.FirstChild;
        while Assigned(lCurSubNode) do
        begin
          {lNodeName := LowerCase(lCurSubNode.NodeName);

          for i := 0 to lCurSubNode.Attributes.Length - 1 do
          begin
            lAttrName := lCurSubNode.Attributes.Item[i].NodeName;
            lAttrValue := lCurSubNode.Attributes.Item[i].NodeValue;
            if lAttrName = 'offset' then
            begin
              //lOffset := lAttrName;
            end;
            else if lAttrName = 'cx' then
            begin
              lLayerName := lCurNode.Attributes.Item[i].NodeValue;
              lBlock.Name := lLayerName;
            end;
          end;}

          lCurSubNode := lCurSubNode.NextSibling;
        end;

        FBrushDefs.Add(lBrushEntity);
      end;
      {
      <linearGradient id="grad1" x1="0%" y1="0%" x2="0%" y2="100%">
        <stop offset="0%" style="stop-color:rgb(255,255,0);stop-opacity:1" />
        <stop offset="100%" style="stop-color:rgb(255,0,0);stop-opacity:1" />
      </linearGradient>
      }
      'lineargradient':
      begin
        lBrushEntity := TvEntityWithPenAndBrush.Create(nil);

        // <linearGradient id="grad1" x1="0%" y1="0%" x2="0%" y2="100%">
        for i := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lAttrName := lCurNode.Attributes.Item[i].NodeName;
          lAttrValue := lCurNode.Attributes.Item[i].NodeValue;
          if lAttrName = 'id' then
            lBrushEntity.Name := lAttrValue
          else if lAttrName = 'x1' then
            x1 := lAttrValue
          else if lAttrName = 'x2' then
            x2 := lAttrValue
          else if lAttrName = 'y1' then
            y1 := lAttrValue
          else if lAttrName = 'y2' then
            y2 := lAttrValue;
        end;
        if x2 = x1 then lBrushEntity.Brush.Kind := bkVerticalGradient
        else lBrushEntity.Brush.Kind := bkHorizontalGradient;

        // <stop offset="0%" style="stop-color:rgb(255,255,0);stop-opacity:1" />
        // <stop offset="100%" style="stop-color:rgb(255,0,0);stop-opacity:1" />
        lCurSubNode := lCurNode.FirstChild;
        while Assigned(lCurSubNode) do
        begin
          lNodeName := LowerCase(lCurSubNode.NodeName);

          for i := 0 to lCurSubNode.Attributes.Length - 1 do
          begin
            lAttrName := lCurSubNode.Attributes.Item[i].NodeName;
            lAttrValue := lCurSubNode.Attributes.Item[i].NodeValue;
            if lAttrName = 'offset' then
            begin
              lOffset := StringWithUnitToFloat(lAttrValue);
            end
            else if lAttrName = 'style' then
              ReadSVGStyle(lAttrValue, lBrushEntity);
          end;

          lCurSubNode := lCurSubNode.NextSibling;
        end;

        FBrushDefs.Add(lBrushEntity);
      end;
      // Sometime entities are also put in the defs
      'circle', 'ellipse', 'g', 'line', 'path',
      'polygon', 'polyline', 'rect', 'text', 'use', 'symbol':
      begin
        lLayerName := '';
        lBlock := TvBlock.Create(nil);

        // pre-load attribute reader, to get the block name
        for i := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lAttrName := lCurNode.Attributes.Item[i].NodeName;
          if lAttrName = 'id' then
          begin
            lLayerName := lCurNode.Attributes.Item[i].NodeValue;
            lBlock.Name := lLayerName;
          end;
        end;

        // Skip g without id, create instead items for his sub-items
        if (lEntityName = 'g') and (lLayerName = '') then
        begin
          lCurSubNode := lCurNode.FirstChild;
          while Assigned(lCurSubNode) do
          begin
            lCurEntity := ReadEntityFromNode(lCurSubNode, AData, ADoc);
            if lCurEntity <> nil then
              AData.AddEntity(lCurEntity);
            lCurSubNode := lCurSubNode.NextSibling;
          end;
          lBlock.Free;
          lCurNode := lCurNode.NextSibling;
          Continue;
        end;

        lPreviousLayer := AData.GetCurrentLayer();
        AData.AddEntity(lBlock);
        AData.SetCurrentLayer(lBlock);
        //
        lCurEntity := ReadEntityFromNode(lCurNode, AData, ADoc);
        if lCurEntity <> nil then
          AData.AddEntity(lCurEntity);
        //
        AData.SetCurrentLayer(lPreviousLayer);
      end;
    end;

    lCurNode := lCurNode.NextSibling;
  end;
end;

function TvSVGVectorialReader.ReadEntityFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lEntityName: DOMString;
begin
  Result := nil;
  lEntityName := LowerCase(ANode.NodeName);
  case lEntityName of
    'circle': Result := ReadCircleFromNode(ANode, AData, ADoc);
    'ellipse': Result := ReadEllipseFromNode(ANode, AData, ADoc);
    'frame': Result := ReadFrameFromNode(ANode, AData, ADoc);
    'g': ReadLayerFromNode(ANode, AData, ADoc);
    'image': Result := ReadImageFromNode(ANode, AData, ADoc);
    'line': Result := ReadLineFromNode(ANode, AData, ADoc);
    'path': Result := ReadPathFromNode(ANode, AData, ADoc);
    'polygon', 'polyline': Result := ReadPolyFromNode(ANode, AData, ADoc);
    'rect': Result := ReadRectFromNode(ANode, AData, ADoc);
    'symbol': Result := ReadSymbolFromNode(ANode, AData, ADoc);
    'text': Result := ReadTextFromNode(ANode, AData, ADoc);
    'use': Result := ReadUseFromNode(ANode, AData, ADoc);
  end;
end;

function TvSVGVectorialReader.ReadCircleFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  cx, cy, cr, dtmp: double;
  lCircle: TvCircle;
  i: Integer;
  lNodeName, lNodeValue: DOMString;
begin
  cx := 0.0;
  cy := 0.0;
  cr := 0.0;

  lCircle := TvCircle.Create(nil);
  // SVG entities start without any pen drawing, but with a black brush
  lCircle.Pen.Style := psClear;
  lCircle.Brush.Style := bsSolid;
  lCircle.Brush.Color := colBlack;
  // Apply the layer style
  ApplyLayerStyles(lCircle);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'cx' then
      cx := StringWithUnitToFloat(lNodeValue, sckX, suPX, suMM)
    else if lNodeName = 'cy' then
      cy := StringWithUnitToFloat(lNodeValue, sckY, suPX, suMM)
    else if lNodeName = 'r' then
      cr := StringWithUnitToFloat(lNodeValue, sckXSize, suPX, suMM)
    else if lNodeName = 'id' then
      lCircle.Name := lNodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(lNodeValue, lCircle)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lCircle);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lCircle);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, lNodeValue, lCircle);
    end;
  end;

  lCircle.X := lCircle.X + cx;
  lCircle.Y := lCircle.Y + cy;
  lCircle.Radius := lCircle.Radius + cr;

  Result := lCircle;
end;

function TvSVGVectorialReader.ReadEllipseFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  cx, cy, crx, cry: double;
  lEllipse: TvEllipse;
  i: Integer;
  lNodeName, lNodeValue: DOMString;
begin
  cx := 0.0;
  cy := 0.0;
  crx := 0.0;
  cry := 0.0;

  lEllipse := TvEllipse.Create(nil);
  // SVG entities start without any pen drawing, but with a black brush
  lEllipse.Pen.Style := psClear;
  lEllipse.Brush.Style := bsSolid;
  lEllipse.Brush.Color := colBlack;
  // Apply the layer style
  ApplyLayerStyles(lEllipse);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if  lNodeName = 'cx' then
      cx := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'cy' then
      cy := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'rx' then
      crx := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'ry' then
      cry := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'id' then
      lEllipse.Name := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(lNodeValue, lEllipse)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lEllipse);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lEllipse);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, lNodeValue, lEllipse);
    end;
  end;

  ConvertSVGCoordinatesToFPVCoordinates(
        AData, cx, cy, lEllipse.X, lEllipse.Y);
  ConvertSVGDeltaToFPVDelta(
        AData, crx, cry, lEllipse.HorzHalfAxis, lEllipse.VertHalfAxis);

  Result := lEllipse;
end;

{
<draw:frame draw:style-name="gr5" draw:layer="layout" svg:width="9.024cm" svg:height="0.963cm"
    draw:transform="rotate (-1.58737695468884) translate (2.3cm 1.197cm)">
   <draw:text-box>
      <text:p>Jump opposite arm and leg up</text:p>
   </draw:text-box>
</draw:frame>

<draw:frame draw:style-name="gr5" draw:layer="layout" svg:width="15.07cm" svg:height="1.115cm" svg:x="2.6cm" svg:y="26.9cm">
   <draw:text-box>
      <text:p>
         <text:span text:style-name="T1">Back muscle movement</text:span>
         <text:span text:style-name="T2">opposite</text:span>
         <text:span text:style-name="T3">arm</text:span>
         and
         <text:span text:style-name="T3">leg</text:span>
         up
      </text:p>
   </draw:text-box>
</draw:frame>
}
function TvSVGVectorialReader.ReadFrameFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lTextStr: string = '';
  lx, ly: double;
  lText: TvText;
  i: Integer;
  lNodeName, lNodeValue, lSubNodeName, lSubNodeValue: DOMString;
  lCurNode, lCurSubNode: TDOMNode;
begin
  lx := 0.0;
  ly := 0.0;
  Result := nil;

  lText := nil;//TvText.Create(nil);

  // Apply the layer style
  ApplyLayerStyles(lText);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if  lNodeName = 'svg:x' then
      lx := lx + StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'svg:y' then
      ly := ly + StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'draw:style-name' then
      ReadSVGStyle(lNodeValue, lText);
  end;

  // Get the text contents
  lCurNode := Anode.FirstChild;
  while lCurNode <> nil do
  begin
    lNodeName := LowerCase(lCurNode.NodeName);
    if lNodeName <> 'draw:text-box' then Continue;

    lCurSubNode := lCurNode.FirstChild;
    while lCurSubNode <> nil do
    begin
      lSubNodeName := LowerCase(lCurSubNode.NodeName);
      if lSubNodeName <> 'draw:text-box' then Continue;

      lText := ReadFrameTextFromNode(lCurNode, AData, ADoc) as TvText;
      Break;

      lCurSubNode := lCurSubNode.NextSibling;
    end;
    if lText <> nil then Break;

    lCurNode := lCurNode.NextSibling;
  end;

  if lText = nil then Exit;

  // Set the coordinates
  ConvertSVGCoordinatesToFPVCoordinates(
        AData, lx, ly, lText.X, lText.Y);

  // Finalization
  Result := lText;
end;

{
   <text:p>Jump opposite arm and leg up</text:p>

   <text:p>
      <text:span text:style-name="T1">Back muscle movement</text:span>
      <text:span text:style-name="T2">opposite</text:span>
      <text:span text:style-name="T3">arm</text:span>
      and
      <text:span text:style-name="T3">leg</text:span>
      up
   </text:p>
}
function TvSVGVectorialReader.ReadFrameTextFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lTextStr: string = '';
  lx, ly: double;
  lText: TvText;
  //i: Integer;
  //lNodeName, lNodeValue: DOMString;
  //lCurNode: TDOMNode;
begin
  lx := 0.0;
  ly := 0.0;

  lText := TvText.Create(nil);

  // Apply the layer style
  ApplyLayerStyles(lText);

  {// read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if  lNodeName = 'x' then
      lx := lx + StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'y' then
      ly := ly + StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'id' then
      lText.Name := lNodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(lNodeValue, lText);
  end;}

  // The text contents are inside as a child text, not as a attribute
  // ex:   <text x="0" y="15" fill="red" transform="rotate(30 20,40)">I love SVG</text>
  if Anode.FirstChild <> nil then
    lTextStr := Anode.FirstChild.NodeValue;
  // Add the first line
  lText.Value.Add(lTextStr);

  // Recover the position if there was a transformation matrix
  //lx := lx + lText.X;
  //ly := ly + lText.Y;

  // Set the coordinates
  ConvertSVGCoordinatesToFPVCoordinates(
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
  Result := lText;
end;

// <image width="92.5" x="0" y="0" height="76.0429"
//  xlink:href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAKMAAACGCAYAA
//  ACxDToF......" clip-path="url(#Clip0)" transform="matrix(1 0 0 1 0 0)"/>
function TvSVGVectorialReader.ReadImageFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lImage: TvRasterImage;
  lx, ly, lw, lh: Double;
  lTransformX, lTransformY, lTransformW, lTransformH: Double;
  i: Integer;
  lNodeName, lNodeValue: DOMString;
  lImageDataParts: TStringList;
  lImageDataBase64: string;
  lImageData: array of Byte;
  lImageDataStream: TMemoryStream;
  lImageReader: TFPCustomImageReader;
begin
  lImage := TvRasterImage.Create(nil);
  lx := 0;
  ly := 0;
  lw := 0;
  lh := 0;
  lImage.Width := 1000;
  lImage.Height := 1000;

  // Apply the layer style
  //ApplyLayerStyles(lEllipse);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if  lNodeName = 'x' then
      lx := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'y' then
      ly := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'width' then
      lw := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'height' then
      lh := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'xlink:href' then
    begin
      lImageDataParts := ReadSpaceSeparatedStrings(lNodeValue, ':;,');
      try
        if (lImageDataParts.Strings[0] = 'data') and
           (lImageDataParts.Strings[1] = 'image/png') and
           (lImageDataParts.Strings[2] = 'base64') then
        begin
          lImageReader := TFPReaderPNG.Create;
          lImageDataStream := TMemoryStream.Create;
          try
            lImageDataBase64 := lImageDataParts.Strings[3];
            DecodeBase64(lImageDataBase64, lImageDataStream);
            lImageDataStream.Position := 0;
            lImage.CreateRGB888Image(10, 10);
            lImage.RasterImage.LoadFromStream(lImageDataStream, lImageReader);
          finally
            lImageDataStream.Free;
            lImageReader.Free;
          end;
        end
        else
          raise Exception.Create('[TvSVGVectorialReader.ReadImageFromNode] Unimplemented image format');
      finally
        lImageDataParts.Free;
      end;
    end
    else if lNodeName = 'id' then
      lImage.Name := lNodeValue
{    else if lNodeName = 'style' then
      ReadSVGStyle(lNodeValue, lImage)}
    else if IsAttributeFromStyle(lNodeName) then
    begin
      //ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lImage);
      //ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lImage);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, lNodeValue, lImage);
    end;
  end;

  // Record the transform data
  lTransformX := lImage.X;
  lTransformY := lImage.Y;
  lTransformW := lImage.Width / 1000;
  lTransformH := lImage.Height / 1000;

  ConvertSVGCoordinatesToFPVCoordinates(
        AData, lx, ly, lImage.X, lImage.Y);
  ConvertSVGDeltaToFPVDelta(
        AData, lw, lh, lImage.Width, lImage.Height);

  // And re-apply the transform data
  lImage.X := lImage.X + lTransformX;
  lImage.Y := lImage.Y + lTransformY;
  lImage.Width := lImage.Width * lTransformW;
  lImage.Height := lImage.Height * lTransformH;
  // Strange hack: No idea why it works ... but helped wmtboc
  lImage.X := lImage.X - lImage.Width;
  lImage.Y := lImage.Y + lImage.Height / 2;

  // Apply the layer style
  ApplyLayerStyles(lImage);

  Result := lImage;
end;

procedure TvSVGVectorialReader.ReadLayerFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lNodeName, lNodeValue: DOMString;
  lLayerName: string = '';
  lCurNode, lLayerNameNode: TDOMNode;
  lLayer: TvLayer;
  lParentLayer: TvEntityWithSubEntities;
  i: Integer;
  {$ifdef SVG_MERGE_LAYER_STYLES}
  lLayerStyleKeys, lLayerStyleValues: TStringList;
  lCurEntity: TvEntity;
  {$endif}
begin
  // Store the style of this layer in the list
  {$ifdef SVG_MERGE_LAYER_STYLES}
  lLayerStyleKeys := TStringList.Create;
  lLayerStyleValues := TStringList.Create;
  FLayerStylesKeys.Add(lLayerStyleKeys);
  FLayerStylesValues.Add(lLayerStyleValues);
  {$endif}

  // first attribute reader, there is a second one
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'id' then
      lLayerName := ANode.Attributes.Item[i].NodeValue;
  end;

  lParentLayer := AData.GetCurrentLayer();
  lLayer := AData.AddLayerAndSetAsCurrent(lLayerName);

  // attribute reading again after getting the object
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'style' then
    begin
      {$ifdef SVG_MERGE_LAYER_STYLES}
      ReadSVGStyleToStyleLists(lNodeValue, lLayerStyleKeys, lLayerStyleValues);
      {$else}
      lLayer.SetPenBrushAndFontElements += ReadSVGStyle(lNodeValue, lLayer)
      {$endif}
    end
    else if IsAttributeFromStyle(lNodeName) then
    begin
      {$ifdef SVG_MERGE_LAYER_STYLES}
      lLayerStyleKeys.Add(lNodeName);
      lLayerStyleValues.Add(lNodeValue);
      {$else}
      lLayer.SetPenBrushAndFontElements += ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lLayer);
      lLayer.SetPenBrushAndFontElements += ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lLayer);
      lLayer.SetPenBrushAndFontElements += ReadSVGFontStyleWithKeyAndValue(lNodeName, lNodeValue, lLayer);
      {$endif}
    end;
  end;

  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lCurEntity := ReadEntityFromNode(lCurNode, AData, ADoc);
    if lCurEntity <> nil then
      AData.AddEntity(lCurEntity);
    lCurNode := lCurNode.NextSibling;
  end;

  {$ifdef SVG_MERGE_LAYER_STYLES}
  // Now remove the style from this layer
  FLayerStylesKeys.Remove(lLayerStyleKeys);
  lLayerStyleKeys.Free;
  FLayerStylesValues.Remove(lLayerStyleValues);
  lLayerStyleValues.Free;
  {$endif}

  // Set the current layer to the parent node,
  // or else items read next will be put as children of this layer
  AData.SetCurrentLayer(lParentLayer);
end;

function TvSVGVectorialReader.ReadLineFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  x1, y1, x2, y2: double;
  vx1, vy1, vx2, vy2: double;
  i: Integer;
  lNodeName: DOMString;
  lPath: TPath;
  lStyleStr: DOMString;
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
      y2 := StringWithUnitToFloat(ANode.Attributes.Item[i].NodeValue);
  end;

  ConvertSVGCoordinatesToFPVCoordinates(
        AData, x1, y1, vx1, vy1);
  ConvertSVGCoordinatesToFPVCoordinates(
        AData, x2, y2, vx2, vy2);

  AData.StartPath();
  AData.AddMoveToPath(vx1, vy1);
  AData.AddLineToPath(vx2, vy2);
  lPath := AData.EndPath(True);

  // Apply the layer style
  ApplyLayerStyles(lPath);

  // Add the entity styles
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
    end;
  end;
  //
  Result := lPath;
end;

function TvSVGVectorialReader.ReadPathFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
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
  Inc(FPathNumber);
  FSVGPathTokenizer.ExtraDebugStr := Format(' [TvSVGVectorialReader.ReadPathFromNode] path#(1-based)=%d', [FPathNumber]);
  ReadPathFromString(UTF8Encode(lDStr), AData, ADoc);
  FSVGPathTokenizer.ExtraDebugStr := '';
  lPath := AData.EndPath(True);
  Result := lPath;
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
      lPath.Name := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
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
  lTmpTokenType: TSVGTokenType;
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
    if not (lCurTokenType = sttFloatValue) then
    begin
      lLastCommandToken := lCurTokenType;
      ReadNextPathCommand(lCurTokenType, i, CurX, CurY, AData, ADoc);
    end
    // In this case we are getting a command without a starting letter
    // It is a copy of the last one, or something related to it
    else
    begin
      lTmpTokenType := lLastCommandToken;
      if lLastCommandToken = sttMoveTo then lTmpTokenType := sttLineTo;
      if lLastCommandToken = sttRelativeMoveTo then lTmpTokenType := sttRelativeLineTo;
      // For bezier I checked that a sttBezierTo upon repetition expects a sttBezierTo
      Dec(i);// because there is no command token in this command
      ReadNextPathCommand(lTmpTokenType, i, CurX, CurY, AData, ADoc);
    end;
  end;
end;

procedure TvSVGVectorialReader.ReadNextPathCommand(ACurTokenType: TSVGTokenType;
  var i: Integer; var CurX, CurY: Double; AData: TvVectorialPage;
  ADoc: TvVectorialDocument);
var
  X, Y, X2, Y2, X3, Y3, XQ, YQ: Double;
  LargeArcFlag, SweepFlag, LeftmostEllipse, ClockwiseArc: Boolean;
  lCurTokenType: TSVGTokenType;
  lDebugStr: String;
  lToken5Before, lToken7Before: TSVGTokenType;
  lCorrectPreviousToken: Boolean;
begin
  lCurTokenType := ACurTokenType;
  // --------------
  // Moves
  // --------------
  if lCurTokenType in [sttMoveTo, sttRelativeMoveTo] then
  begin
    X := FSVGPathTokenizer.Tokens.Items[i+1].Value;
    Y := FSVGPathTokenizer.Tokens.Items[i+2].Value;

    // take care of relative or absolute
    // Idiotism in SVG: If the path starts with a relative move to,
    // the coordinates are absolute =o source: http://www.w3.org/TR/SVG/paths.html#PathDataMovetoCommands
    if (lCurTokenType = sttRelativeMoveTo) and (i > 0) then
    begin
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
      CurX := CurX + X;
      CurY := CurY + Y;
    end
    else
    begin
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
      CurX := X;
      CurY := Y;
    end;
    AData.AddMoveToPath(CurX, CurY);

    Inc(i, 3);
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

    // "l" LineTo uses relative coordenates in SVG
    if lCurTokenType in [sttRelativeLineTo, sttRelativeHorzLineTo, sttRelativeVertLineTo] then
    begin
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
      CurX := CurX + X;
      CurY := CurY + Y;
    end
    else
    begin
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
      CurX := X;
      CurY := Y;
    end;

    // horizontal and vertical line corrections
    if lCurTokenType in [sttHorzLineTo, sttRelativeHorzLineTo] then
      Y := 0
    else if lCurTokenType in [sttVertLineTo, sttRelativeVertLineTo] then
    begin
      Y := X;
      X := 0;
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
      // Calculation found here: http://stackoverflow.com/questions/5287559/calculating-control-points-for-a-shorthand-smooth-svg-path-bezier-curve
      // Description here: http://www.w3.org/TR/SVG/paths.html#PathDataCurveCommands
      //
      // M X0, Y0 C X1, Y1 X2, Y2 X3, Y3 S X4, Y4 X5, Y5
      // Missing control points for S is:
      // XR = 2*X3 - X2 and
      // YR = 2*Y3 - Y2
      if i >= 7 then
      begin
        lToken5Before := FSVGPathTokenizer.Tokens.Items[i-5].TokenType;
        lToken7Before := FSVGPathTokenizer.Tokens.Items[i-7].TokenType;
        lCorrectPreviousToken := lToken5Before in [sttSmoothBezierTo, sttRelativeSmoothBezierTo];
        lCorrectPreviousToken := lCorrectPreviousToken or
          (lToken7Before in [sttBezierTo, sttRelativeBezierTo]);
      end;
      if (i >= 7) and (lCorrectPreviousToken) then
      begin
        if lCurTokenType = sttRelativeSmoothBezierTo then
        begin
          X2 := FSVGPathTokenizer.Tokens.Items[i-2].Value - FSVGPathTokenizer.Tokens.Items[i-4].Value;
          Y2 := FSVGPathTokenizer.Tokens.Items[i-1].Value - FSVGPathTokenizer.Tokens.Items[i-3].Value;
        end
        else
        begin
          X2 := 2*FSVGPathTokenizer.Tokens.Items[i-2].Value - FSVGPathTokenizer.Tokens.Items[i-4].Value;
          Y2 := 2*FSVGPathTokenizer.Tokens.Items[i-1].Value - FSVGPathTokenizer.Tokens.Items[i-3].Value;
        end;
      end;
      // Now the non-missing items
      X3 := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y3 := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      X := FSVGPathTokenizer.Tokens.Items[i+3].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+4].Value;
    end;

    // Careful that absolute coordinates require using ConvertSVGCoordinatesToFPVCoordinates
    if lCurTokenType in [sttRelativeBezierTo, sttRelativeSmoothBezierTo] then
    begin
      ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
      ConvertSVGDeltaToFPVDelta(AData, X3, Y3, X3, Y3);
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
    end
    else
    begin
      ConvertSVGCoordinatesToFPVCoordinates(AData, X2, Y2, X2, Y2);
      ConvertSVGCoordinatesToFPVCoordinates(AData, X3, Y3, X3, Y3);
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
    end;

    // Covers the case where there is no valid first control point in smooth bezier
    // The code is here to be after the conversions
    if (lCurTokenType in [sttSmoothBezierTo, sttRelativeSmoothBezierTo]) and
      ((i < 7) or (not lCorrectPreviousToken)) then
    begin
      if lCurTokenType = sttRelativeSmoothBezierTo then
      begin
        X2 := CurX;
        Y2 := CurY;
      end
      else
      begin
        X2 := 0;
        Y2 := 0;
      end;
    end;

    // The final step
    if lCurTokenType in [sttRelativeBezierTo, sttRelativeSmoothBezierTo] then
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
    XQ := FSVGPathTokenizer.Tokens.Items[i+1].Value;
    YQ := FSVGPathTokenizer.Tokens.Items[i+2].Value;
    X := FSVGPathTokenizer.Tokens.Items[i+3].Value;
    Y := FSVGPathTokenizer.Tokens.Items[i+4].Value;

    // Careful that absolute coordinates require using ConvertSVGCoordinatesToFPVCoordinates
    if lCurTokenType in [sttRelativeQuadraticBezierTo] then
    begin
      ConvertSVGDeltaToFPVDelta(AData, XQ, YQ, XQ, YQ);
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      XQ := XQ + CurX;
      YQ := YQ + CurY;
      X := X + CurX;
      Y := Y + CurY;
    end
    else
    begin
      ConvertSVGCoordinatesToFPVCoordinates(AData, XQ, YQ, XQ, YQ);
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
    end;

    // Convert quadratic to cubic bezier
    // CP1 = QP0 + 2/3 *(QP1-QP0)
    // CP2 = QP2 + 2/3 *(QP1-QP2)
    // See http://stackoverflow.com/questions/3162645/convert-a-quadratic-bezier-to-a-cubic
    // Just CP1=CP2=QP1 does not work! See svg/w3c/path2.svg
    X2 := CurX + (2/3) * (XQ-CurX);
    Y2 := CurY + (2/3) * (YQ-CurY);
    //
    X3 := X + (2/3) * (XQ-X);
    Y3 := Y + (2/3) * (YQ-Y);

    AData.AddBezierToPath(X2, Y2, X3, Y3, X, Y);
    CurX := X;
    CurY := Y;

    Inc(i, 5);
  end
  // --------------
  // Elliptical arcs
  // See http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands
  // (rx ry x-axis-rotation large-arc-flag sweep-flag x y)+
  // --------------
  else if lCurTokenType in [sttEllipticArcTo, sttRelativeEllipticArcTo] then
  begin
    X2 := FSVGPathTokenizer.Tokens.Items[i+1].Value; // RX
    Y2 := FSVGPathTokenizer.Tokens.Items[i+2].Value; // RY
    X3 := FSVGPathTokenizer.Tokens.Items[i+3].Value; // RotationX
    X3 := X3 * Pi / 180; // degrees to radians conversion
    LargeArcFlag := Round(FSVGPathTokenizer.Tokens.Items[i+4].Value) = 1;
    SweepFlag := Round(FSVGPathTokenizer.Tokens.Items[i+5].Value) = 1;
    X := FSVGPathTokenizer.Tokens.Items[i+6].Value;  // X
    Y := FSVGPathTokenizer.Tokens.Items[i+7].Value;  // Y

    // non-coordinate values
    ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);

    // Careful that absolute coordinates require using ConvertSVGCoordinatesToFPVCoordinates
    if lCurTokenType in [sttRelativeEllipticArcTo] then
    begin
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
    end
    else
    begin
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
    end;

    // Convert SVG flags to fpvectorial flags
    LeftmostEllipse := (LargeArcFlag and (not SweepFlag))
      or ((not LargeArcFlag) and SweepFlag);
    ClockwiseArc := SweepFlag;

    if lCurTokenType = sttRelativeEllipticArcTo then
    begin
      AData.AddEllipticalArcToPath(X2, Y2, X3, X + CurX, Y + CurY, LeftmostEllipse, ClockwiseArc);
      CurX := CurX + X;
      CurY := CurY + Y;
    end
    else
    begin
      AData.AddEllipticalArcToPath(X2, Y2, X3, X, Y, LeftmostEllipse, ClockwiseArc);
      CurX := X;
      CurY := Y;
    end;

    Inc(i, 8);
  end
  else
  begin
    Inc(i);
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
    ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, CurX, CurY);
    AData.AddLineToPath(CurX, CurY);

    Inc(i, 2);
  end;

  // and if we want, close the path
  if AClosePath then
    AData.AddLineToPath(FirstPtX, FirstPtY);
end;

// polygon and polyline are very similar
function TvSVGVectorialReader.ReadPolyFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
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
  lPath := AData.EndPath(True);
  Result := lPath;

  // Apply the layer style
  ApplyLayerStyles(lPath);

  // now read the other attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'id' then
      lPath.Name := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lPath);
    end;
  end;
end;

//          <rect width="90" height="90" stroke="green" stroke-width="3" fill="yellow" filter="url(#f1)" />
function TvSVGVectorialReader.ReadRectFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
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

  lRect := TvRectangle.Create(nil);
  // SVG entities start without any pen drawing, but with a black brush
  lRect.Pen.Style := psClear;
  lRect.Brush.Style := bsSolid;
  lRect.Brush.Color := colBlack;
  // Apply the layer style
  ApplyLayerStyles(lRect);

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
      lRect.Name := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(ANode.Attributes.Item[i].NodeValue, lRect)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lRect);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lRect);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, ANode.Attributes.Item[i].NodeValue, lRect);
    end;
  end;

  ConvertSVGCoordinatesToFPVCoordinates(
        AData, lx, ly, lRect.X, lRect.Y);
  ConvertSVGSizeToFPVSize(
        AData, cx, cy, lRect.CX, lRect.CY);
  ConvertSVGSizeToFPVSize(
        AData, lrx, lry, lRect.RX, lRect.RY);
  lRect.RX := Abs(lRect.RX) * 2;
  lRect.RY := Abs(lRect.RY) * 2;

  Result := lRect;
end;

function TvSVGVectorialReader.ReadSymbolFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lBlock: TvBlock;
  lLayerName: string;
  lPreviousLayer: TvEntityWithSubEntities;
  lCurEntity: TvEntity;
  i: Integer;
  lAttrName: DOMString;
  lCurNode: TDOMNode;
begin
  lBlock := TvBlock.Create(nil);

  // pre-load attribute reader, to get the block name
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lAttrName := ANode.Attributes.Item[i].NodeName;
    if lAttrName = 'id' then
    begin
      lLayerName := ANode.Attributes.Item[i].NodeValue;
      lBlock.Name := lLayerName;
    end;
  end;

  lPreviousLayer := AData.GetCurrentLayer();
  AData.AddEntity(lBlock);
  AData.SetCurrentLayer(lBlock);
  //
  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lCurEntity := ReadEntityFromNode(lCurNode, AData, ADoc);
    if lCurEntity <> nil then
      AData.AddEntity(lCurEntity);
    lCurNode := lCurNode.NextSibling;
  end;
  //
  AData.SetCurrentLayer(lPreviousLayer);
end;

{
 <text x="0" y="15" fill="red" transform="rotate(30 20,40)">I love SVG</text>

 Example with nested tspan:

 <text class="TextShape">
   <tspan class="TextParagraph" font-family="Times New Roman, serif" font-size="917px" font-weight="400">
     <tspan class="TextPosition" x="3650" y="11251">
       <tspan fill="rgb(0,0,0)" stroke="none">This </tspan>
       <tspan fill="rgb(197,0,11)" stroke="none">size</tspan>
       <tspan fill="rgb(0,0,0)" stroke="none"> is bigger.</tspan>
     </tspan>
   </tspan>
 </text>
}
function TvSVGVectorialReader.ReadTextFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lx, ly: double;
  lName: string;
  lParagraph: TvParagraph;
  lTextSpanStack: TSVGObjectStack; // of TSVGTextSpanStyle
  lCurStyle: TSVGTextSpanStyle;
  i: Integer;
  lNodeName, lNodeValue, lXLink: DOMString;
  lCurObject: TObject;

  procedure ApplyStackStylesToText(ADest: TvText);
  var
    j: Integer;
  begin
    for j := 0 to lTextSpanStack.GetList().Count-1 do
    begin
      lCurStyle := TSVGTextSpanStyle(lTextSpanStack.GetList().Items[j]);
      lCurStyle.ApplyIntoEntity(ADest);
      if lCurStyle.PositionSet then
      begin
        ADest.X := ADest.X + lCurStyle.X;
        ADest.Y := ADest.Y + lCurStyle.Y;
      end;
    end;
  end;

  procedure ReadTextSpans(ACurNode: TDOMNode);
  var
    j: Integer;
    lCurNode: TDOMNode;
    lTextStr: string;
    lText: TvText;
    lCText: TvCurvedText;
    lInsertedEntity, lInsertedSubEntity: TvEntity;
  begin
    lCurNode := ACurNode.FirstChild;
    while lCurNode <> nil do
    begin
      lNodeName := LowerCase(lCurNode.NodeName);
      lNodeValue := lCurNode.NodeValue;

      if lNodeName = 'tspan' then
      begin
        lCurStyle := TSVGTextSpanStyle.Create;
        lTextSpanStack.Push(lCurStyle);

        // read the attributes
        for j := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lNodeName := lCurNode.Attributes.Item[j].NodeName;
          lNodeValue := lCurNode.Attributes.Item[j].NodeValue;
          lNodeName := LowerCase(lNodeName);
          if lNodeName = 'x' then
          begin
            lCurStyle.PositionSet := True;
            lCurStyle.X := StringWithUnitToFloat(lNodeValue, sckX) - lParagraph.X;
          end
          else if lNodeName = 'y' then
          begin
            lCurStyle.PositionSet := True;
            lCurStyle.Y := StringWithUnitToFloat(lNodeValue, sckY) - lParagraph.Y;
          end
          //else if lNodeName = 'id' then
          //  lText.Name := lNodeValue
          //else if lNodeName = 'style' then
          //  ReadSVGStyle(lNodeValue, lParagraph)
          else if IsAttributeFromStyle(lNodeName) then
          begin
            ReadSVGFontStyleWithKeyAndValue(lNodeName, lNodeValue, nil, lCurStyle);
            //ReadSVGGeneralStyleWithKeyAndValue(lNodeName, lNodeValue, lText);
          end;
        end;

        // Recursion
        ReadTextSpans(lCurNode);

        // Get rid of the current style
        lCurObject := lTextSpanStack.Pop();
        if lCurObject <> nil then lCurObject.Free;
      end
      else if lNodeName = 'textpath' then
      begin
        lTextStr := GetTextContentFromNode(lCurNode);
        lTextStr := RemoveLineEndingsAndTrim(lTextStr);
        lTextStr := Trim(lTextStr);

        lCText := lParagraph.AddCurvedText(lTextStr);

        lCText.Font.Size := 10;
        lCText.Name := lName;

        // read the attributes
        for j := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lNodeName := lCurNode.Attributes.Item[j].NodeName;
          lNodeValue := lCurNode.Attributes.Item[j].NodeValue;
          lNodeName := LowerCase(lNodeName);
          if lNodeName = 'xlink:href' then
          begin
            lXLink := lNodeValue;
            lXLink := Copy(lXLink, 2, Length(lXLink));
            if lXLink = '' then Continue; // nothing to insert, so give up
            lInsertedEntity := AData.FindEntityWithNameAndType(lXLink, TvEntity, True);
            if lInsertedEntity = nil then Continue; // nothing to insert, give up!
            if lInsertedEntity is TvBlock then
            begin
              lInsertedSubEntity := TvBlock(lInsertedEntity).GetFirstEntity();
              if not (lInsertedSubEntity is TPath) then Continue;
              // Add the curvature
              lCText.Path := TPath(lInsertedSubEntity as TPath);
            end;
          end;
        end;

        // Apply the layer style
        ApplyLayerStyles(lCText);

        // Apply the layer style
        ApplyStackStylesToText(lCText);
      end
      else
      begin
        lText := lParagraph.AddText(lNodeValue);

        lText.Font.Size := 10;
        lText.Name := lName;
        // Apply the layer style
        ApplyLayerStyles(lText);

        // Apply the layer style
        ApplyStackStylesToText(lText);
      end;


      lCurNode := lCurNode.NextSibling;
    end;
  end;

begin
  lx := 0.0;
  ly := 0.0;

  // The text contents are inside as a child text, not as a attribute
  // ex:   <text x="0" y="15" fill="red" transform="rotate(30 20,40)">I love SVG</text>
  // For simple text, but much more complex structures appear if there are
  // text spans

  lParagraph := TvParagraph.Create(AData);
  lTextSpanStack := TSVGObjectStack.Create;
  lCurStyle := TSVGTextSpanStyle.Create;
  lTextSpanStack.Push(lCurStyle);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if  lNodeName = 'x' then
      lx := lx + StringWithUnitToFloat(lNodeValue, sckX, suPX, suMM)
    else if lNodeName = 'y' then
      ly := ly + StringWithUnitToFloat(lNodeValue, sckY, suPX, suMM)
    else if lNodeName = 'id' then
    begin
      lName := lNodeValue;
      lParagraph.Name := lName;
    end
    else if lNodeName = 'style' then
      ReadSVGStyle(lNodeValue, nil, lCurStyle)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGFontStyleWithKeyAndValue(lNodeName, lNodeValue, nil, lCurStyle);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, lNodeValue, lParagraph);
    end;
  end;

  // Takes into account a possible transformation matrix
  lx := lx + lParagraph.X;
  ly := ly + lParagraph.Y;

  // Set the coordinates -> Don't use ConvertSVGCoordinatesToFPVCoordinates
  // because StringWithUnitToFloat(..sckX..) already makes all conversions necessary
  lParagraph.X := lx;
  lParagraph.Y := ly;

  // Now add other lines, which appear as <tspan ...>another line</tspan>
  // Example:
  // <text x="10" y="20" style="fill:red;">Several lines:
  //   <tspan x="10" y="45">First line</tspan>
  //   <tspan x="10" y="70">Second line</tspan>
  // </text>
  // These other lines can be positioned, so they need to appear as independent TvText elements
  ReadTextSpans(ANode);

  // Finalization
  lCurObject := lTextSpanStack.Pop();
  if lCurObject <> nil then lCurObject.Free;
  lTextSpanStack.Free;
  Result := lParagraph;
end;

// <use xlink:href="#svgbar" transform="rotate(45)"/>
// It might use another entity or use something from Defs
function TvSVGVectorialReader.ReadUseFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lInsert: TvInsert;
  lXLink: DOMString = '';
  lInsertedEntity: TvEntity;
  i: Integer;
  lx, ly: Double;
  lNodeName, lNodeValue: DOMString;
begin
  Result := nil;
  lx := 0.0;
  ly := 0.0;

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'xlink:href' then
    begin
      lXLink := lNodeValue;
      lXLink := Copy(lXLink, 2, Length(lXLink));
    end
    else if lNodeName = 'x' then
      lx := StringWithUnitToFloat(lNodeValue, sckX)
    else if lNodeName = 'y' then
      ly := StringWithUnitToFloat(lNodeValue, sckY);
  end;

  if lXLink = '' then Exit; // nothing to insert, so give up

  lInsertedEntity := AData.FindEntityWithNameAndType(lXLink, TvEntity, True);
  if lInsertedEntity = nil then Exit; // nothing to insert, give up!

  lInsert := TvInsert.Create(nil);
  lInsert.InsertEntity := lInsertedEntity;
  lInsert.x := lx;
  lInsert.y := ly;

  // Apply the styles
  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'style' then
    begin
      ReadSVGStyle(lNodeValue, lInsert);
    end
    else if IsAttributeFromStyle(lNodeName) then
    begin
      lInsert.SetElements += ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
      lInsert.SetElements += ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
      lInsert.SetElements += ReadSVGFontStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
      ReadSVGGeneralStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
    end;
  end;

  // We need to add this hack here, otherwise the Height is added twice
  // to inserted items: Once in the Insert and yet another time in the
  // coordinates of the inserted item!
  lInsert.Y := lInsert.Y - AData.Height;

  Result := lInsert;
end;

function TvSVGVectorialReader.StringWithUnitToFloat(AStr: string;
  ACoordKind: TSVGCoordinateKind = sckUnknown; ADefaultUnit: TSVGUnit = suPX;
  ATargetUnit: TSVGUnit = suPX): Double;
var
  UnitStr, ValueStr: string;
  Len: Integer;
  LastChar: Char;
  ViewPortApplied: Boolean = False;

  procedure DoViewBoxAdjust();
  begin
    if ViewBoxAdjustment then
    begin
      case ACoordKind of
        sckX:      Result := (Result - ViewBox_Left) * Page_Width / ViewBox_Width;
        sckXDelta,
        sckXSize:  Result := Result * Page_Width / ViewBox_Width;
        sckY:      Result := Page_Height - (Result - ViewBox_Top) * Page_Height / ViewBox_Height;
        sckYDelta: Result := - (Result - ViewBox_Top) * Page_Height / ViewBox_Height;
        sckYSize:  Result := Result * Page_Height / ViewBox_Height;
      end;
      ViewPortApplied := True;
    end
    else
    begin
      case ACoordKind of
        sckY:      Result := Page_Height - Result;
        sckYDelta: Result := - Result;
      end;
    end;
  end;

  procedure DoProcessMM_End();
  begin
    if ATargetUnit = suPX then
      Result := Result / FLOAT_MILIMETERS_PER_PIXEL;
    DoViewBoxAdjust();
  end;

  procedure DoProcessPX();
  begin
    Result := StrToFloat(ValueStr, FPointSeparator);
    case ATargetUnit of
    suMM: Result := Result * FLOAT_MILIMETERS_PER_PIXEL;
    suPT: Result := Result * FLOAT_POINTS_PER_PIXEL;
    end;
    DoViewBoxAdjust();
  end;

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
    DoProcessMM_End();
  end
  else if UnitStr = 'cm' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToFloat(ValueStr, FPointSeparator);
    Result := Result * 10;
    DoProcessMM_End();
  end
  else if UnitStr = 'px' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    DoProcessPX();
  end
  else if LastChar = '%' then
  begin
    ValueStr := Copy(AStr, 1, Len-1);
    Result := StrToInt(ValueStr);
  end
  else if UnitStr = 'pt' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToFloat(ValueStr, FPointSeparator);
  end
  // Now process default values
  else if ADefaultUnit = suMM then
  begin
    ValueStr := AStr;
    Result := StrToFloat(ValueStr, FPointSeparator);
    DoProcessMM_End();
  end
  else if ADefaultUnit = suPX then
  begin
    ValueStr := AStr;
    DoProcessPX();
  end
  else // If there is no unit and no matching default, just use StrToFloat
  begin
    Result := StrToFloat(AStr, FPointSeparator);
    DoViewBoxAdjust();
  end;
end;

function TvSVGVectorialReader.StringFloatZeroToOneToWord(AStr: string): Word;
begin
  Result := Round(StrToFloat(AStr, FPointSeparator) * $FFFF);
end;

procedure TvSVGVectorialReader.ConvertSVGCoordinatesToFPVCoordinates(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double;
  var ADestX,ADestY: Double; ADoViewBoxAdjust: Boolean = True);
begin
  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := AData.Height - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
  if ViewBoxAdjustment and ADoViewBoxAdjust then
  begin
    ADestX := (ASrcX - ViewBox_Left) * Page_Width / ViewBox_Width;
    ADestY := AData.Height - (ASrcY - ViewBox_Top) * Page_Height / ViewBox_Height;
  end;
end;

procedure TvSVGVectorialReader.ConvertSVGDeltaToFPVDelta(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: Double; ADoViewBoxAdjust: Boolean = True);
begin
  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
  if ViewBoxAdjustment and ADoViewBoxAdjust then
  begin
    ADestX := ASrcX * Page_Width / ViewBox_Width;
    ADestY := - ASrcY * Page_Height / ViewBox_Height;
  end;
end;

procedure TvSVGVectorialReader.ConvertSVGSizeToFPVSize(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: Double; ADoViewBoxAdjust: Boolean = True);
begin
  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
  if ViewBoxAdjustment and ADoViewBoxAdjust then
  begin
    ADestX := ASrcX * Page_Width / ViewBox_Width;
    ADestY := ASrcY * Page_Height / ViewBox_Height;
  end;
end;

procedure TvSVGVectorialReader.AutoDetectDocSize(var ALeft, ATop, ARight, ABottom: Double;
  ABaseNode: TDOMNode);
var
  i: Integer;
  lCurNode: TDOMNode;
  lx, ly, lcx, lcy: Double;
  lNodeName, lNodeValue: DomString;
begin
  lx := 0;
  ly := 0;
  lcx := 0;
  lcy := 0;
  // read the attributes
  if ABaseNode.Attributes <> nil then
  begin
    for i := 0 to ABaseNode.Attributes.Length - 1 do
    begin
      lNodeName := ABaseNode.Attributes.Item[i].NodeName;
      lNodeValue := ABaseNode.Attributes.Item[i].NodeValue;
      if (lNodeName = 'x') or (lNodeName = 'cx') then
        lx := lx + StringWithUnitToFloat(lNodeValue)
      else if (lNodeName = 'y') or (lNodeName = 'cy') then
        ly := ly + StringWithUnitToFloat(lNodeValue)
      else if lNodeName = 'width' then
        lcx := StringWithUnitToFloat(lNodeValue)
      else if lNodeName = 'height' then
        lcy := StringWithUnitToFloat(lNodeValue);
    end;
  end;

  // Borders guessing
  if lx < ALeft then ALeft := lx;
  if ly < ATop then ATop := ly;
  if lx + lcx > ARight then ARight := lx + lcx;
  if ly + lcy > ABottom then ABottom := ly + lcy;

  // iterate through children
  lCurNode := ABaseNode.FirstChild;
  while Assigned(lCurNode) do
  begin
    AutoDetectDocSize(ALeft, ATop, ARight, ABottom, lCurNode);
    lCurNode := lCurNode.NextSibling;
  end;
end;

// From 0..255 to 0..$FFFF
// From 0%..100% to 0..$FFFF
function TvSVGVectorialReader.SVGColorValueStrToWord(AStr: string): Word;
var
  lStr: string;
begin
  lStr := Trim(AStr);
  if lStr[Length(lStr)] = '%' then
  begin
    lStr := Copy(lStr, 1, Length(lStr)-1);
    Result := StrToInt(lStr) * $FFFF div 100;
  end
  else Result := StrToInt(lStr) * $101;
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
  FBrushDefs := TFPList.Create;
end;

destructor TvSVGVectorialReader.Destroy;
begin
  FLayerStylesKeys.Free;
  FLayerStylesValues.Free;
  FBrushDefs.Free;
  FSVGPathTokenizer.Free;

  inherited Destroy;
end;

procedure TvSVGVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument = nil;
begin
  try
    // Read in xml file from the stream
    ReadXMLFile(Doc, AStream);
    ReadFromXML(Doc, AData);
  finally
    // finally, free the document
    Doc.Free;
  end;
end;

procedure TvSVGVectorialReader.ReadFromXML(Doc: TXMLDocument;
  AData: TvVectorialDocument);
var
  lCurNode: TDOMNode;
  lPage: TvVectorialPage;
  {$ifdef SVG_MERGE_LAYER_STYLES}
  lLayerStyleKeys, lLayerStyleValues: TStringList;
  {$endif}
  lNodeName, lNodeValue: DOMString;
  ANode: TDOMElement;
  i: Integer;
  lCurEntity: TvEntity;
  lViewBox: TDoubleArray;
  lStr: string;
  lDocNeedsSizeAutoDetection: Boolean = True;
  lx, ly, lx2, ly2: Double;
begin
  FPathNumber := 0;
  ViewBoxAdjustment := False;

  // ----------------
  // Read the properties of the <svg> tag
  // ----------------
  AData.Width := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('width'), sckX, suPX, suMM);
  lStr := Doc.DocumentElement.GetAttribute('height');
  if lStr <> '' then
  begin
    lDocNeedsSizeAutoDetection := False;
    AData.Height := StringWithUnitToFloat(lStr, sckX, suPX, suMM);
  end;
  Page_Width := AData.Width;
  Page_Height := AData.Height;

  {$ifdef SVG_MERGE_LAYER_STYLES}
  FLayerStylesKeys.Clear;
  FLayerStylesValues.Clear;
  lLayerStyleKeys := TStringList.Create;
  lLayerStyleValues := TStringList.Create;
  FLayerStylesKeys.Add(lLayerStyleKeys);
  FLayerStylesValues.Add(lLayerStyleValues);
  {$endif}
  ANode := Doc.DocumentElement;
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'viewBox' then
    begin
      lViewBox := ReadSpaceSeparatedFloats(lNodeValue, '');
      if lDocNeedsSizeAutoDetection then // Has only ViewBox
      begin
        lDocNeedsSizeAutoDetection := False;
        AData.Width := lViewBox[2] - lViewBox[0];
        AData.Height := lViewBox[3] - lViewBox[1];
        ViewBox_Left := 0;
        ViewBox_Top := 0;
        ViewBox_Width := 0;
        ViewBox_Height := 0;
      end
      else // Has both viewBox and width/height!
      begin
        ViewBox_Left := lViewBox[0];
        ViewBox_Top := lViewBox[1];
        ViewBox_Width := lViewBox[2];
        ViewBox_Height := lViewBox[3];
        ViewBoxAdjustment := True;
      end;
    end
    else if lNodeName = 'style' then
    begin
      {$ifdef SVG_MERGE_LAYER_STYLES}
      ReadSVGStyleToStyleLists(lNodeValue, lLayerStyleKeys, lLayerStyleValues);
      {$endif}
    end
    else if IsAttributeFromStyle(lNodeName) then
    begin
      {$ifdef SVG_MERGE_LAYER_STYLES}
      lLayerStyleKeys.Add(lNodeName);
      lLayerStyleValues.Add(lNodeValue);
      {$endif}
    end;
  end;

  // Auto-detect the document size of necessary
  if lDocNeedsSizeAutoDetection then
  begin
    lx := 0;
    ly := 0;
    lx2 := 0;
    ly2 := 0;
    lCurNode := Doc.DocumentElement.FirstChild;
    while Assigned(lCurNode) do
    begin
      AutoDetectDocSize(lx, ly, lx2, ly2, lCurNode);
      lCurNode := lCurNode.NextSibling;
    end;
    AData.Width := lx2 - lx;
    AData.Height := ly2 - ly;
  end;

  // Make sure the latest page size is syncronized with auto-detected
  // or ViewBox-only obtained size
  Page_Width := AData.Width;
  Page_Height := AData.Height;

  // ----------------
  // Now process the elements
  // ----------------
  lCurNode := Doc.DocumentElement.FirstChild;
  lPage := AData.AddPage();
  lPage.Width := AData.Width;
  lPage.Height := AData.Height;
  while Assigned(lCurNode) do
  begin
    lNodeName := lCurNode.NodeName;
    if lNodeName = 'defs' then
    begin
      ReadDefsFromNode(lCurNode, lPage, AData);
    end
    else
    begin
      lCurEntity := ReadEntityFromNode(lCurNode, lPage, AData);
      if lCurEntity <> nil then
        lPage.AddEntity(lCurEntity);
    end;
    lCurNode := lCurNode.NextSibling;
  end;

  // ----------------
  // Remove the memory of the styles
  // ----------------
  {$ifdef SVG_MERGE_LAYER_STYLES}
  // Now remove the style from this layer
  FLayerStylesKeys.Remove(lLayerStyleKeys);
  lLayerStyleKeys.Free;
  FLayerStylesValues.Remove(lLayerStyleValues);
  lLayerStyleValues.Free;
  {$endif}
end;

initialization

  RegisterVectorialReader(TvSVGVectorialReader, vfSVG);

end.

