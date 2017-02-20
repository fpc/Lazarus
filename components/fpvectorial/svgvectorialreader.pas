{
Reads an SVG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

SVG Coordinates vs FPVectorial coordinates:

SVG by default has [0, 0] at the top-left and coordinates grow downwards and to the right
Text is drawn upwards (towards negative Y)
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

  TvSVGVectorialReader = class;

  { TSVG_CSS_Style }

  TSVG_CSS_Style = class(TvStyle)
  public
    CSSName, CSSData: string;
    function MatchesClass(AClassName: string): Boolean;
    procedure ParseCSSData(AReader: TvSVGVectorialReader);
  end;

  { TSVGPathTokenizer }

  TSVGPathTokenizer = class
  protected
    Tokens: TSVGTokenList;
  public
    FPointSeparator, FCommaSeparator: TFormatSettings;
    ExtraDebugStr: string;
    constructor Create;
    destructor Destroy; override;
    procedure AddToken(AStr: string);
    procedure ClearTokens;
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
    FCSSDefs: TFPList; // of TSVG_CSS_Style;
    // debug symbols
    FPathNumber: Integer;
    // Path support for multiple polygons
    FPathStart: T2DPoint;
    // BrushDefs functions
    function FindBrushDef_WithName(AName: string): TvEntityWithPenAndBrush;
    //
    function ReadSVGColor(AValue: string): TFPColor;
    function ReadSVGGradientColorStyle(AValue: String): TFPColor;
    function ReadSVGStyle(AData: TvVectorialPage; AValue: string;
      ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil;
      AUseFillAsPen: Boolean = False): TvSetPenBrushAndFontElements;
    function ReadSVGStyleToStyleLists(AValue: string; AStyleKeys, AStyleValues: TStringList): TvSetPenBrushAndFontElements;
    function ReadSVGPenStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
    function ReadSVGBrushStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPenAndBrush; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
    function ReadSVGFontStyleWithKeyAndValue(AKey, AValue: string; ADestEntity: TvEntityWithPenBrushAndFont; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
    procedure ReadSVGGeneralStyleWithKeyAndValue(AData: TvVectorialPage;
        AKey, AValue: string; ADestEntity: TvEntity);
    function IsAttributeFromStyle(AStr: string): Boolean;
    procedure ApplyLayerStyles(AData: TvVectorialPage; ADestEntity: TvEntity);
    function ReadSpaceSeparatedFloats(AInput: string; AOtherSeparators: string): TDoubleArray;
    procedure ReadSVGTransformationMatrix(AMatrix: string; out AA, AB, AC, AD, AE, AF: Double);
    procedure ApplyCSSClass(AData: TvVectorialPage; AValue: string;
      ADestEntity: TvEntityWithPen);
    function IsEntityStyleField(AFieldName: string): Boolean;
    function ReadEntityStyleField(AData: TvVectorialPage; AFieldName, AFieldValue: string; ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil;
      AUseFillAsPen: Boolean = False): TvSetPenBrushAndFontElements;
    //
    function GetTextContentFromNode(ANode: TDOMNode): string;
    procedure ReadDefs_LinearGradient(ADest: TvEntityWithPenAndBrush; ANode: TDOMNode; AData: TvVectorialPage);
    procedure ReadDefs_CSS(ANode: TDOMNode);
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
    procedure ReadNextPathCommand(ACurTokenType: TSVGTokenType; var i: Integer; var AIsFirstPathMove: Boolean; var CurX, CurY: Double; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadPointsFromString(AStr: string; AData: TvVectorialPage; ADoc: TvVectorialDocument; AClosePath: Boolean);
    function ReadPolyFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadRectFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadSymbolFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadTextFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    function ReadUseFromNode(ANode: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
    //
    procedure StringToPenPattern(const AStr: String; var APen: TvPen);
    function  StringWithUnitToFloat(AStr: string; ACoordKind: TSVGCoordinateKind = sckUnknown;
      ADefaultUnit: TSVGUnit = suPX; ATargetUnit: TSVGUnit = suPX): Double;
    function  StringFloatZeroToOneToWord(AStr: string): Word;
    function  StringWithPercentToFloat(AStr: String): Double;

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

  FLOAT_MILLIMETERS_PER_PIXEL = 1; //0.2822; // DPI 90 = 1 / 90 inches per pixel => Actually I changed the value! Because otherwise it looks ugly!
  FLOAT_PIXELS_PER_MILIMETER = 1 / FLOAT_MILLIMETERS_PER_PIXEL; // DPI 90 = 1 / 90 inches per pixel

  FLOAT_POINTS_PER_PIXEL = 0.75; // For conversion
  FLOAT_PIXEL_PER_POINT = 1 / FLOAT_POINTS_PER_PIXEL; // For conversion

{ TSVG_CSS_Style }

function TSVG_CSS_Style.MatchesClass(AClassName: string): Boolean;
var
  lNameModified: string;
begin
  lNameModified := '.' + AClassName;
  Result := (lNameModified = CSSName);
end;

{
<style type="text/css">
 <![CDATA[
  .strr3 {stroke:#C7C7A2;stroke-width:0.793701}
  .str1 {stroke:#8C8C60;stroke-width:1.70079}
  .strr2 {stroke:#636347;stroke-width:2.26772}
  .str0 {stroke:#2B2A29;stroke-width:3.9685}
  .fil1 {fill:none}
  .fil0 {fill:#EEEED4}
 ]]>
}
procedure TSVG_CSS_Style.ParseCSSData(AReader: TvSVGVectorialReader);
begin
  SetElements += AReader.ReadSVGStyle(nil, CSSData, nil, Self, False);
end;

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
  ClearTokens;
  Tokens.Free;
  inherited Destroy;
end;

procedure TSVGPathTokenizer.AddToken(AStr: string);
var
  lToken: TSVGToken;
  lStr: string;
begin
//  lToken := TSVGToken.Create;

  lStr := Trim(AStr);
  if lStr = '' then Exit;

  lToken := TSVGToken.Create;
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

procedure TSVGPathTokenizer.ClearTokens;
var
  i: Integer;
begin
  for i := Tokens.Count-1 downto 0 do
    Tokens[i].Free;
  Tokens.Clear;
end;

procedure TSVGPathTokenizer.TokenizePathString(AStr: string);
const
  Str_Space: Char = ' ';
  Str_Comma: Char = ',';
  Str_Plus: Char = '+';
  Str_Minus: Char = '-';
  ListOfCommandLetters: set of Char = ['a'..'d', 'f'..'z', 'A'..'D', 'F'..'Z'];
var
  i: Integer;
  lTmpStr: string = '';
  lState: Integer;
  lFirstTmpStrChar, lCurChar, lPrevChar: Char;
begin
  lState := 0;

  i := 1;
  while i <= Length(AStr) do
  begin
    case lState of
    0: // Adding to the tmp string
    begin
      if i > 0 then lPrevChar := AStr[i-1];
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
      else if (lCurChar in [Str_Plus, Str_Minus]) and (lPrevChar in ['0'..'9']) then
      begin
        AddToken(lTmpStr);
        lTmpStr := lCurChar;
      end
      // Check for a break, from letter to number
      // Note: But don't forget that we need to support 3.799e-4 !!
      // So e is not a valid letter for commands here
      // Note 2: Letters don't need space between them as in "zm"
      else if (lCurChar in ListOfCommandLetters) then
      begin
        if Length(lTmpStr) > 0 then
        begin
          AddToken(lTmpStr);
        end;
        AddToken(lCurChar);
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

function TvSVGVectorialReader.FindBrushDef_WithName(AName: string): TvEntityWithPenAndBrush;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FBrushDefs.Count-1 do
  begin
    Result := TvEntityWithPenAndBrush(FBrushDefs.Items[i]);
    if Result.Name = AName then
    begin
      Exit;
    end;
  end;
  Result := nil;
end;

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
  case lValue[1] of
    'a': case lValue of
           'aliceblue'           : Result := FPColor($F0F0, $F8F8, $FFFF);
           'antiquewhite'        : Result := FPColor($FAFA, $EBEB, $D7D7);
           'aqua'                : Result := colCyan;
           'aquamarine'          : Result := FPColor($7F7F, $FFFF, $D4D4);
           'azure'               : Result := FPColor($F0F0, $FFFF, $FFFF);
         end;

    'b': case lValue of
           'beige'               : Result := FPColor($F5F5, $F5F5, $DCDC);
           'bisque'              : Result := FPColor($FFFF, $E4E4, $C4C4);
           'black'               : Result := colBlack;
           'blanchedalmond'      : Result := FPColor($FFFF, $EBEB, $CDCD);
           'blue'                : Result := colBlue;
           'blueviolet'          : Result := FPColor($8A8A, $2B2B, $E2E2);
           'brown'               : Result := FPColor($A5A5, $2A2A, $2A2A);
           'burlywood'           : Result := FPColor($DEDE, $B8B8, $8787);
         end;

    'c': case lValue of
           'cadetblue'           : Result := FPColor($5F5F, $9E9E, $A0A0);
           'chartreuse'          : Result := FPColor($7F7F, $FFFF, $0000);
           'chocolate'           : Result := FPColor($D2D2, $6969, $1E1E);
           'coral'               : Result := FPColor($FFFF, $7F7F, $5050);
           'cornflowerblue'      : Result := FPColor($6464, $9595, $EDED);
           'cornsilk'            : Result := FPColor($FFFF, $F8F8, $DCDC);
           'crimson'             : Result := FPColor($DCDC, $1414, $3C3C);
           'cyan'                : Result := colCyan;
         end;

    'd': case lValue of
           'darkblue'            : Result.Blue := $8B8B;
           'darkcyan'            : Result := FPColor($0000, $8B8B, $8B8B);
           'darkgoldenrod'       : Result := FPColor($B8B8, $8686, $0B0B);
           'darkgray',
           'darkgrey'            : Result := FPColor($A9A9, $A9A9, $A9A9);
           'darkgreen'           : Result.Green := $6464;
           'darkkhaki'           : Result := FPColor($BDBD, $B7B7, $6B6B);
           'darkmagenta'         : Result := FPColor($8B8B, $0000, $8B8B);
           'darkolivegreen'      : Result := FPColor($5555, $6B6B, $2F2F);
           'darkorange'          : Result := FPColor($FFFF, $8C8C, $0000);
           'darkorchid'          : Result := FPColor($9999, $3232, $CCCC);
           'darkred'             : Result.Red := $8B8B;
           'darksalmon'          : Result := FPColor($E9E9, $9696, $7A7A);
           'darkseagreen'        : Result := FPColor($8F8F, $BCBC, $8F8F);
           'darkslateblue'       : Result := FPColor($4848, $3D3D, $8B8B);
           'darkslategray',
           'darkslategrey'       : Result := FPColor($2F2F, $4F4F, $4F4F);
           'darkturquoise'       : Result := FPColor($0000, $CECE, $D1D1);
           'darkviolet'          : Result := FPColor($9494, $0000, $D3D3);
           'deeppink'            : Result := FPColor($FFFF, $1414, $9393);
           'deepskyblue'         : Result := FPColor($0000, $BFBF, $FFFF);
           'dimgray',
           'dimgrey'             : Result := FPColor($6969, $6969, $6969);
           'dodgerblue'          : Result := FPColor($1E1E, $9090, $FFFF);
         end;

    'f': case lValue of
           'firebrick'           : Result := FPColor($B2B2, $2222, $2222);
           'floralwhite'         : Result := FPColor($FFFF, $FAFA, $F0F0);
           'forestgreen'         : Result := FPColor($2222, $8B8B, $2222);
           'fuchsia'             : Result := colFuchsia;
         end;

    'g': case lValue of
           'gainsboro'           : Result := FPColor($DCDC, $DCDC, $DCDC);
           'ghostwhite'          : Result := FPColor($F8F8, $F8F8, $FFFF);
           'gold'                : Result := FPColor($FFFF, $D7D7, $0000);
           'goldenrod'           : Result := FPColor($DADA, $A5A5, $2020);
           'gray', 'grey'        : Result := colGray;
           'green'               : Result.Green := $8080;
           'greenyellow'         : Result := FPColor($ADAD, $FFFF, $2F2F);
         end;

    'h': case lValue of
           'honeydew'            : Result := FPColor($F0F0, $FFFF, $F0F0);
           'hotpink'             : Result := FPColor($FFFF, $6969, $B4B4);
         end;

    'i': case lValue of
           'indianred'           : Result := FPColor($CDCD, $5C5C, $5C5C);
           'indigo'              : Result := FPColor($4B4B, $0000, $8282);
           'ivory'               : Result := FPColor($FFFF, $FFFF, $F0F0);
         end;

    'k': case lValue of
           'khaki'               : Result := FPColor($F0F0, $E6E6, $8C8C);
         end;

    'l': case lValue of
           'lavender'            : Result := FPColor($E6E6, $E6E6, $FAFA);
           'lavenderblush'       : Result := FPColor($FFFF, $F0F0, $F5F5);
           'lawngreen'           : Result := FPColor($7C7C, $FCFE, $0000);
           'lemonchiffon'        : Result := FPColor($FFFF, $FAFA, $CDCD);
           'lightblue'           : Result := FPColor($ADAD, $D8D8, $E6E6);
           'lightcoral'          : Result := FPColor($F0F0, $8080, $8080);
           'lightcyan'           : Result := FPColor($E0E0, $FFFF, $FFFF);
           'lightgoldenrodyellow': Result := FPColor($FAFA, $FAFA, $D2D2);
           'lightgray',
           'lightgrey'           : Result := FPColor($D3D3, $D3D3, $D3D3);
           'lightgreen'          : Result := FPColor($9090, $EEEE, $9090);
           'lightpink'           : Result := FPColor($FFFF, $B6B6, $C1C1);
           'lightsalmon'         : Result := FPColor($FFFF, $A0A0, $7A7A);
           'lightseagreen'       : Result := FPColor($2020, $B2B2, $AAAA);
           'lightskyblue'        : Result := FPColor($8787, $CECE, $FAFA);
           'lightslategray',
           'lightslategrey'      : Result := FPColor($7777, $8888, $9999);
           'lightsteelblue'      : Result := FPColor($B0B0, $C4C4, $DEDE);
           'lightyellow'         : Result := FPColor($FFFF, $FEFE, $0000);
           'lime'                : Result := colGreen;
           'limegreen'           : Result := FPColor($3232, $CDCD, $3232);
           'linen'               : Result := FPColor($FAFA, $F0F0, $E6E6);
         end;

    'm': case lValue of
           'magenta'             : Result := colMagenta;
           'maroon'              : Result.Red := $8080;
           'mediumaquamarine'    : Result := FPColor($6666, $CDCD, $AAAA);
           'mediumblue'          : Result.Blue := $CDCD;
           'mediumorchid'        : Result := FPColor($BABA, $5555, $D3D3);
           'mediumpurple'        : Result := FPColor($9393, $7070, $DBDB);
           'mediumseagreen'      : Result := FPColor($3C3C, $CBCB, $7171);
           'mediumslateblue'     : Result := FPColor($7B7B, $6868, $EEEE);
           'mediumspringgreen'   : Result := FPColor($0000, $FAFA, $9A9A);
           'mediumturquoise'     : Result := FPColor($4848, $D1D1, $CCCC);
           'mediumvioletred'     : Result := FPColor($C7C7, $1515, $8585);
           'midnightblue'        : Result := FPColor($1919, $1919, $7070);
           'mintcream'           : Result := FPColor($F5F5, $FFFF, $FAFA);
           'mistyrose'           : Result := FPColor($FFFF, $E4E4, $E1E1);
           'moccasin'            : Result := FPColor($FFFF, $E4E4, $B5B5);
         end;

    'n': case lValue of
           'navajowhite'         : Result := FPColor($FFFF, $DEDE, $ADAD);
           'navy'                : Result.Blue := $8080;
         end;

    'o': case lValue of
           'oldlace'             : Result := FPColor($FDFD, $F5F5, $E6E6);
           'olive'               : Result := colOlive;
           'olivedrab'           : Result := FPColor($6B6B, $8E8E, $2323);
           'orange'              : Result := FPColor($FFFF, $A5A5, $0000);
           'orangered'           : Result := FPColor($FFFF, $4545, $0000);
           'orchid'              : Result := FPColor($DADA, $7070, $D6D6);
         end;

    'p': case lValue of
           'palegreen'           : Result := FPColor($9898, $FBFB, $9898);
           'palegoldenrod'       : Result := FPColor($EEEE, $E8E8, $AAAA);
           'paleturquoise'       : Result := FPColor($AFAF, $EEEE, $EEEE);
           'palevioletred'       : Result := FPColor($DBDB, $7070, $9393);
           'papayawhip'          : Result := FPColor($FFFF, $EFEF, $D5D5);
           'peachpuff'           : Result := FPColor($FFFF, $DADA, $B9B9);
           'peru'                : Result := FPColor($CDCD, $8585, $3F3F);
           'pink'                : Result := FPColor($FFFF, $C0C0, $CBCB);
           'plum'                : Result := FPColor($DDDD, $A0A0, $DDDD);
           'powderblue'          : Result := FPColor($B0B0, $E0E0, $E6E6);
           'purple'              : Result := colPurple;
         end;

    'r': case lValue of
           'red'                 : Result := colRed;
           'rosybrown'           : Result := FPColor($BCBC, $8F8F, $8F8F);
           'royalblue'           : Result := FPColor($4141, $6969, $E1E1);
         end;

    's': case lValue of
           'saddlebrown'         : Result := FPColor($8B8B, $4545, $1313);
           'salmon'              : Result := FPColor($FAFA, $8080, $7272);
           'sandybrown'          : Result := FPColor($F4F4, $A4A4, $6060);
           'seagreen'            : Result := FPColor($2E2E, $8B8B, $5757);
           'seashell'            : Result := FPColor($FFFF, $F5F5, $EEEE);
           'sienna'              : Result := FPColor($A0A0, $5252, $2D2D);
           'silver'              : Result := colSilver;
           'skyblue'             : Result := FPColor($8787, $CECE, $EBEB);
           'slateblue'           : Result := FPCOlor($6A6A, $5A5A, $CDCD);
           'slategray',
           'slategrey'           : Result := FPColor($7070, $8080, $9090);
           'snow'                : Result := FPColor($FFFF, $FAFA, $FAFA);
           'springgreen'         : Result := FPColor($0000, $FFFF, $7F7F);
           'steelblue'           : Result := FPColor($4646, $8282, $B4B4);
         end;

    't': case lValue of
           'tan'                 : Result := FPColor($D2D2, $B4B4, $8C8C);
           'teal'                : Result := FPColor($0000, $8080, $8080);
           'thistle'             : Result := FPColor($D8D8, $BFBF, $D8D8);
           'tomato'              : Result := FPColor($FFFF, $6363, $4747);
           'turquoise'           : Result := FPColor($4040, $E0E0, $D0D0);
         end;

    'v': case lValue of
           'violet'              : Result := FPColor($EEEE, $8282, $EEEE);
         end;

    'w': case lValue of
           'wheat'               : Result := FPColor($F5F5, $DEDE, $B3B3);
           'white'               : Result := colWhite;
           'whitesmoke'          : Result := FPColor($F5F5, $F5F5, $F5F5);
         end;

    'y': case lValue of
           'yellow'              : Result := colYellow;
           'yellowgreen'         : Result := FPColor($9A9A, $CDCD, $3232);
         end;
  end;
end;

// style="stop-color:rgb(255,255,10);stop-opacity:1.0"
function TvSVGVectorialReader.ReadSVGGradientColorStyle(AValue: String): TFPColor;
var
  lStr, lStyleKeyStr, lStyleValueStr: String;
  lStrings: TStringList;
  i: Integer;
  p: Integer;
begin
  Result := colBlack;
  if AValue = '' then Exit;
  lStrings := TStringList.Create;
  try
    lStrings.Delimiter := ';';
    lStrings.StrictDelimiter := True;
    lStrings.DelimitedText := LowerCase(AValue);
    for i := 0 to lStrings.Count-1 do
    begin
      lStr := lStrings.Strings[i];
      p := Pos(':', lStr);
      lStyleKeyStr := Trim(Copy(lStr, 1, p-1));
      lStyleValueStr := Trim(Copy(lStr, p+1, MaxInt));
      if lStyleKeyStr = 'stop-color' then
        Result := ReadSVGColor(lStyleValueStr)
      else if lStyleKeyStr = 'stop-opacity' then
        Result.Alpha := Round(StrToFloat(lStyleValueStr, FPointSeparator)*$FFFF);
    end;
  finally
    lStrings.Free;
  end;
end;

// style="fill:none;stroke:black;stroke-width:3"
function TvSVGVectorialReader.ReadSVGStyle(AData: TvVectorialPage; AValue: string;
  ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil;
  AUseFillAsPen: Boolean = False): TvSetPenBrushAndFontElements;
var
  lStr, lStyleKeyStr, lStyleValueStr: String;
  lStrings: TStringList;
  i: Integer;
begin
  Result := [];
  if AValue = '' then Exit;

  // Now split using ";" separator
  lStrings := TStringList.Create;
  try
    lStrings.Delimiter := ';';
    lStrings.StrictDelimiter := True;
    lStrings.DelimitedText := AValue;
    for i := 0 to lStrings.Count-1 do
    begin
      lStr := lStrings.Strings[i];
      SeparateStringInTwo(lStr, ':', lStyleKeyStr, lStyleValueStr);
      lStyleKeyStr := LowerCase(Trim(lStyleKeyStr));
      lStyleValueStr := Trim(lStyleValueStr);
      if ADestEntity <> nil then
      begin
        ReadSVGPenStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity);
        ReadSVGGeneralStyleWithKeyAndValue(AData, lStyleKeyStr, lStyleValueStr, ADestEntity);
        if AUseFillAsPen and (lStyleKeyStr = 'fill') then
          Result := Result + ReadSVGPenStyleWithKeyAndValue('stroke', lStyleValueStr, ADestEntity)
        else if ADestEntity is TvText then
          Result := Result + ReadSVGFontStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvText)
        else if ADestEntity is TvEntityWithPenAndBrush then
          Result := Result + ReadSVGBrushStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, ADestEntity as TvEntityWithPenAndBrush);
      end;
      if ADestStyle <> nil then
      begin
        Result += ReadSVGPenStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, nil, ADestStyle);
        //Result += ReadSVGGeneralStyleWithKeyAndValue(AData, lStyleKeyStr, lStyleValueStr, nil, ADestStyle);
        if AUseFillAsPen and (lStyleKeyStr = 'fill') then
          Result += ReadSVGPenStyleWithKeyAndValue('stroke', lStyleValueStr, nil, ADestStyle);
        Result += ReadSVGFontStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, nil, ADestStyle);
        Result += ReadSVGBrushStyleWithKeyAndValue(lStyleKeyStr, lStyleValueStr, nil, ADestStyle);
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
  i: Integer;
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
      SeparateStringInTwo(lStr, ':', lStyleKeyStr, lStyleValueStr);
      AStyleKeys.Add(lStyleKeyStr);
      AStyleValues.Add(lStyleValueStr);
    end;
  finally
    lStrings.Free;
  end;
end;

function TvSVGVectorialReader.ReadSVGPenStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPen; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
var
  OldAlpha: Word;
  lValueInt: Int64;
begin
  Result := [];
  if AKey = 'stroke' then
  begin
    // We store and restore the old alpha to support the "-opacity" element
    if ADestEntity <> nil then
    begin
      OldAlpha := ADestEntity.Pen.Color.Alpha;
      if ADestEntity.Pen.Style = psClear then ADestEntity.Pen.Style := psSolid;
    end;
    if ADestStyle <> nil then
    begin
      OldAlpha := ADestStyle.Pen.Color.Alpha;
      if ADestStyle.Pen.Style = psClear then ADestStyle.Pen.Style := psSolid;
    end;

    if AValue = 'none'  then
    begin
      if ADestEntity <> nil then ADestEntity.Pen.Style := fpcanvas.psClear;
      if ADestStyle <> nil then ADestStyle.Pen.Style := fpcanvas.psClear;
    end
    else
    begin
      if ADestEntity <> nil then
      begin
        ADestEntity.Pen.Color := ReadSVGColor(AValue);
        ADestEntity.Pen.Color.Alpha := OldAlpha;
      end;
      if ADestStyle <> nil then
      begin
        ADestStyle.Pen.Color := ReadSVGColor(AValue);
        ADestStyle.Pen.Color.Alpha := OldAlpha;
      end;
    end;
    Result := Result + [spbfPenColor, spbfPenStyle];
  end
  else if AKey = 'stroke-width' then
  begin
    lValueInt := Round(StringWithUnitToFloat(AValue, sckXSize));
    if ADestEntity <> nil then ADestEntity.Pen.Width := lValueInt;
    if ADestStyle <> nil then ADestStyle.Pen.Width := lValueInt;
    Result := Result + [spbfPenWidth];
  end
  else if AKey = 'stroke-opacity' then
  begin
    lValueInt := Round(StrToFloat(AValue, FPointSeparator)*$FFFF);
    if ADestEntity <> nil then ADestEntity.Pen.Color.Alpha := lValueInt;
    if ADestStyle <> nil then ADestStyle.Pen.Color.Alpha := lValueInt;
  end
  else if AKey = 'stroke-linecap' then
  begin
    {case LowerCase(AValue) of
    'butt':
    'round':
    'square': ADestEntity.Pen;
    end;}
  end
  else if AKey = 'stroke-dasharray' then
    StringToPenPattern(AValue, ADestEntity.Pen);
end;

function TvSVGVectorialReader.ReadSVGBrushStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPenAndBrush; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
var
  OldAlpha: Word;
  Len: Integer;
  lDefName: String;
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

      lCurBrush := FindBrushDef_WithName(lDefName);
      if lCurBrush <> nil then
      begin
        ADestEntity.Brush := lCurBrush.Brush;
        Exit;
      end;
      Exit;
    end;

    // We store and restore the old alpha to support the "-opacity" element
    if ADestEntity <> nil then
    begin
      OldAlpha := ADestEntity.Brush.Color.Alpha;
      if ADestEntity.Brush.Style = bsClear then ADestEntity.Brush.Style := bsSolid;
    end;
    if ADestStyle <> nil then
    begin
      OldAlpha := ADestStyle.Brush.Color.Alpha;
      if ADestStyle.Brush.Style = bsClear then ADestStyle.Brush.Style := bsSolid;
    end;

    if AValue = 'none'  then
    begin
      if ADestEntity <> nil then ADestEntity.Brush.Style := fpcanvas.bsClear;
      if ADestStyle <> nil then ADestStyle.Brush.Style := fpcanvas.bsClear;
    end
    else
    begin
      if ADestEntity <> nil then
      begin
        ADestEntity.Brush.Color := ReadSVGColor(AValue);
        ADestEntity.Brush.Color.Alpha := OldAlpha;
      end;
      if ADestStyle <> nil then
      begin
        ADestStyle.Brush.Color := ReadSVGColor(AValue);
        ADestStyle.Brush.Color.Alpha := OldAlpha;
      end;
    end;

    Result := Result + [spbfBrushColor, spbfBrushStyle];
  end
  else if AKey = 'fill-rule' then
  begin
    if AValue = 'evenodd' then
      ADestEntity.WindingRule := vcmEvenOddRule else
    if AValue = 'nonzero' then
      ADestEntity.WindingRule  := vcmNonzeroWindingRule;  // to do: "inherit" missing here
  end
  else if AKey = 'fill-opacity' then
    ADestEntity.Brush.Color.Alpha := StringFloatZeroToOneToWord(AValue)
  // For linear gradient => stop-color:rgb(255,255,0);stop-opacity:1
  else if AKey = 'stop-color' then
  begin
    Len := Length(ADestEntity.Brush.Gradient_colors);
    SetLength(ADestEntity.Brush.Gradient_colors, Len+1);
    ADestEntity.Brush.Gradient_colors[Len].Color := ReadSVGColor(AValue);
  end;
end;

function TvSVGVectorialReader.ReadSVGFontStyleWithKeyAndValue(AKey,
  AValue: string; ADestEntity: TvEntityWithPenBrushAndFont; ADestStyle: TvStyle = nil): TvSetPenBrushAndFontElements;
var
  lLowerValue: String;
  p: Integer;
  fntName: String;
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
    if ADestEntity <> nil then ADestEntity.Font.Size := Round(StringWithUnitToFloat(AValue, sckXSize, suPT, suPT));
    if ADestStyle <> nil then ADestStyle.Font.Size := Round(StringWithUnitToFloat(AValue, sckXSize, suPT, suPT));
    Result := Result + [spbfFontSize];
  end
  else if AKey = 'font-family' then
  begin
    // Extract the font name
    // To do: Check if font name exists in system. Use replacement fonts which
    // may follow after the comma.
    p := pos(',', AValue);
    if p = 0 then
      fntName := AValue else
      fntName := trim(Copy(AValue, 1, p-1));
    if ADestEntity <> nil then ADestEntity.Font.Name := fntName;
    if ADestStyle <> nil then ADestStyle.Font.Name := fntName;
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

procedure TvSVGVectorialReader.ReadSVGGeneralStyleWithKeyAndValue(AData: TvVectorialPage;
  AKey, AValue: string; ADestEntity: TvEntity);
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
          lMRotate := -DegToRad(lMatrixElements[0]);
            // "-" because of orientation of svg coordinate system
          lMTranslateX := 0;
          lMTranslateY := 0;
          if Length(lMatrixElements) > 1 then
            lMTranslateX := lMatrixElements[1];
          if Length(lMatrixElements) > 2 then
            lMTranslateY := lMatrixElements[2];
          ConvertSVGCoordinatesToFPVCoordinates(AData,
            lMTranslateX, lMTranslateY, lMTranslateX, lMTranslateY);
          ADestEntity.Rotate(lMRotate, Make3DPoint(lMTranslateX, lMTranslateY));
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
    (AStr = 'font-size') or (AStr = 'font-family') or
    (AStr = 'font-weight') or (AStr = 'text-anchor');
end;

procedure TvSVGVectorialReader.ApplyLayerStyles(AData: TvVectorialPage;
  ADestEntity: TvEntity);
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
      ReadSVGGeneralStyleWithKeyAndValue(AData, lCurKey, lCurValue, ADestEntity);
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

procedure TvSVGVectorialReader.ApplyCSSClass(AData: TvVectorialPage;
  AValue: string; ADestEntity: TvEntityWithPen);
var
  i, j: Integer;
  lCurStyle: TSVG_CSS_Style;
  lAllClasses: T10Strings;
begin
  lAllClasses := SeparateString(AValue, ' ');
  for i := 0 to High(lAllClasses)-1 do
  begin
    if lAllClasses[i] = '' then Break;
    for j := 0 to FCSSDefs.Count-1 do
    begin
      lCurStyle := TSVG_CSS_Style(FCSSDefs.Items[j]);
      if lCurStyle.MatchesClass(lAllClasses[i]) then
      begin
        lCurStyle.ApplyIntoEntity(ADestEntity);
        Break;
      end;
    end;
  end;
end;

function TvSVGVectorialReader.IsEntityStyleField(AFieldName: string): Boolean;
begin
  Result := (AFieldName = 'style') or (AFieldName = 'class') or
    IsAttributeFromStyle(AFieldName);
end;

function TvSVGVectorialReader.ReadEntityStyleField(AData: TvVectorialPage;
  AFieldName, AFieldValue: string; ADestEntity: TvEntityWithPen;
  ADestStyle: TvStyle; AUseFillAsPen: Boolean): TvSetPenBrushAndFontElements;
begin
  case AFieldName of
  'style': Result := ReadSVGStyle(AData, AFieldValue, ADestEntity, ADestStyle,
             AUseFillAsPen);
  'class': ApplyCSSClass(AData, AFieldValue, ADestEntity);
  else
    ReadSVGPenStyleWithKeyAndValue(AFieldName, AFieldValue, ADestEntity);
    if ADestEntity is TvEntityWithPenAndBrush then
      ReadSVGBrushStyleWithKeyAndValue(AFieldName, AFieldValue, TvEntityWithPenAndBrush(ADestEntity));
    ReadSVGGeneralStyleWithKeyAndValue(AData, AFieldName, AFieldValue, ADestEntity);
  end;
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

// <linearGradient id="grad1" x1="0%" y1="0%" x2="0%" y2="100%">
procedure TvSVGVectorialReader.ReadDefs_LinearGradient(ADest: TvEntityWithPenAndBrush;
  ANode: TDOMNode; AData: TvVectorialPage);
var
  lAttrName, lAttrValue, lNodeName: DOMString;
  i, len: Integer;
  lCurSubNode: TDOMNode;
  lBrushEntity, lCurBrush: TvEntityWithPenAndBrush;
  lGradientColor: TvGradientColor;
  x1, y1, x2, y2: Double;
begin
  x1 := 0;
  x2 := 0;
  y1 := 0;
  y2 := 0;
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lAttrName := lowercase(ANode.Attributes.Item[i].NodeName);
    lAttrValue := ANode.Attributes.Item[i].NodeValue;
    if lAttrName = 'id' then
      ADest.Name := lAttrValue
    else
    if lAttrName = 'x1' then
    begin
      if lAttrValue[Length(lAttrValue)] = '%' then
        Include(ADest.Brush.Gradient_flags, gfRelStartX);
      x1 := StringWithPercentToFloat(lAttrValue);
    end else
    if lAttrName = 'x2' then
    begin
      if lAttrValue[Length(lAttrValue)] = '%' then
        Include(ADest.Brush.Gradient_flags, gfRelEndX);
      x2 := StringWithPercentToFloat(lAttrValue);
    end else
    if lAttrName = 'y1' then
    begin
      if lAttrValue[Length(lAttrValue)] = '%' then
        Include(ADest.Brush.Gradient_flags, gfRelStartY);
      y1 := StringWithPercentToFloat(lAttrValue);
    end else
    if lAttrName = 'y2' then
    begin
      if lAttrValue[Length(lAttrValue)] = '%' then
        Include(ADest.Brush.Gradient_flags, gfRelEndY);
      y2 := StringWithPercentToFloat(lAttrValue);
    end else
    if lAttrName = 'gradientunits' then
    begin
      lAttrValue := LowerCase(lAttrValue);
      if lAttrValue = 'userspaceonuse' then
        Include(ADest.Brush.Gradient_flags, gfRelToUserSpace)
      else if lAttrValue = 'objectboundingbox' then
        Exclude(ADest.Brush.Gradient_flags, gfRelToUserSpace);
    end;
  end;
  ADest.Brush.Gradient_start.X := x1;
  ADest.Brush.Gradient_end.X := x2;
  ADest.Brush.Gradient_start.Y := y1;
  ADest.Brush.Gradient_end.Y := y2;
  ConvertSVGCoordinatesToFPVCoordinates(AData, x1, y1, x1, y1);
  ConvertSVGCoordinatesToFPVCoordinates(AData, x2, y2, x2, y2);
  if not (gfRelStartX in ADest.Brush.Gradient_flags) then
    ADest.Brush.Gradient_start.X := x1;
  if not (gfRelEndX in ADest.Brush.Gradient_flags) then
    ADest.Brush.Gradient_end.X := x2;
  if not (gfRelStartY in ADest.Brush.Gradient_flags) then
    ADest.Brush.Gradient_start.Y := y1;
  if not (gfRelEndY in ADest.Brush.Gradient_flags) then
    ADest.Brush.Gradient_end.Y := y2;
  if (ADest.Brush.Gradient_start.X = 0) and
     (ADest.Brush.Gradient_start.Y = 0) and
     (ADest.Brush.Gradient_end.X = 0) and
     (ADest.Brush.Gradient_end.Y = 0) then
  begin
    ADest.Brush.Gradient_start.X := 0.0;
    ADest.Brush.Gradient_start.Y := 0.0;
    ADest.Brush.Gradient_end.X := 1.0;
    ADest.Brush.Gradient_end.Y := 1.0;
  end;
  if ADest.Brush.Gradient_start.X = ADest.Brush.Gradient_end.X then
    ADest.Brush.Kind := bkVerticalGradient
  else if ADest.Brush.Gradient_start.Y = ADest.Brush.Gradient_end.Y then
    ADest.Brush.Kind := bkHorizontalGradient
  else
    ADest.Brush.Kind := bkOtherLinearGradient;

  // <stop offset="0%" style="stop-color:rgb(255,255,0);stop-opacity:1" />
  // <stop offset="100%" style="stop-color:rgb(255,0,0);stop-opacity:1" />
  lCurSubNode := ANode.FirstChild;
  while Assigned(lCurSubNode) do
  begin
    lNodeName := LowerCase(lCurSubNode.NodeName);
    if lNodeName = 'stop' then begin
      for i := 0 to lCurSubNode.Attributes.Length - 1 do
      begin
        lAttrName := lCurSubNode.Attributes.Item[i].NodeName;
        lAttrValue := lCurSubNode.Attributes.Item[i].NodeValue;
        if lAttrName = 'offset' then
          lGradientColor.Position := StringWithPercentToFloat(lAttrValue)
          // use as fraction 0..1
        else if lAttrName = 'style' then
          lGradientColor.Color := ReadSVGGradientColorStyle(lAttrValue)
        else if lAttrName = 'stop-color' then
          lGradientColor.Color := ReadSVGColor(lAttrValue);
      end;
      len := Length(ADest.Brush.Gradient_colors);
      SetLength(ADest.Brush.Gradient_colors, Len+1);
      ADest.Brush.Gradient_colors[len] := lGradientColor;
    end;
    lCurSubNode := lCurSubNode.NextSibling;
  end;
end;

procedure TvSVGVectorialReader.ReadDefs_CSS(ANode: TDOMNode);
var
  lContent, lCurName, lCurData: string;
  lCurChar: AnsiChar;
  i, lParserState: Integer;
  lCurStyle: TSVG_CSS_Style;
begin
  lContent := Trim(ANode.TextContent);
  lContent := StringReplace(lContent, '<![CDATA[', '', []);
  lContent := StringReplace(lContent, ']]>', '', []);
  lContent := Trim(lContent);
  lParserState := 0;
  for i := 1 to Length(lContent) do
  begin
    lCurChar := lContent[i];
    case lParserState of
    0: // filling class name
    begin
      if lCurChar = '{' then lParserState := 1
      else lCurName += lCurChar;
    end;
    1: // filling class data
    begin
      if lCurChar = '}' then
      begin
        lCurStyle := TSVG_CSS_Style.Create;
        lCurStyle.CSSName := Trim(lCurName);
        lCurStyle.CSSData := Trim(lCurData);
        FCSSDefs.Add(lCurStyle);
        lCurStyle.ParseCSSData(Self);
        lParserState := 0;
        lCurName := '';
        lCurData := '';
      end
      else lCurData += lCurChar;
    end;
    end;
  end;
end;

procedure TvSVGVectorialReader.ReadDefsFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lEntityName: DOMString;
  lBlock: TvBlock;
  lPreviousLayer: TvEntityWithSubEntities;
  lAttrName, lAttrValue, lNodeName, lEntityValue: DOMString;
  lLayerName: String;
  i, len: Integer;
  lCurNode, lCurSubNode: TDOMNode;
  lBrushEntity, lCurBrush: TvEntityWithPenAndBrush;
  lCurEntity: TvEntity;
  lAttrValue_Double: Double;
begin
  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lEntityName := LowerCase(lCurNode.NodeName);
    lEntityValue := lCurNode.NodeValue;
    case lEntityName of
      'radialgradient':
      begin
        lBrushEntity := TvEntityWithPenAndBrush.Create(nil);

        // First copy everything we can from any xlink:href
        for i := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lAttrName := LowerCase(lCurNode.Attributes.Item[i].NodeName);
          lAttrValue := lCurNode.Attributes.Item[i].NodeValue;
          if lAttrName = 'xlink:href' then
          begin
            lAttrValue := StringReplace(Trim(lAttrValue), '#', '', []);
            lCurBrush := FindBrushDef_WithName(lAttrValue);
            if lCurBrush <> nil then
              lBrushEntity.Brush := lCurBrush.Brush;
          end;
        end;

        // Now linear gradient properties
        ReadDefs_LinearGradient(lBrushEntity, lCurNode, AData);

        // Now process our own properties

        lBrushEntity.Brush.Kind := bkRadialGradient;

        // <radialGradient id="grad1" cx="50%" cy="50%" r="50%" fx="50%" fy="50%">
        // or
        // <linearGradient id="linearGradient6038">
        // <stop style="stop-color:#ffffff;stop-opacity:1;" offset="0" id="stop6040" />
        // <stop style="stop-color:#000000;stop-opacity:0;" offset="1" id="stop6042" />
        // </linearGradient>
        // <radialGradient inkscape:collect="always" xlink:href="#linearGradient6038"
        //  id="radialGradient3333" gradientUnits="userSpaceOnUse"
        //  gradientTransform="matrix(0.05999054,1.120093,-1.0249935,0.05489716,995.9708,109.4759)"
        //  cx="406.62762" cy="567.12799" fx="406.62762" fy="567.12799" r="37.15749" />
        for i := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lAttrName := lCurNode.Attributes.Item[i].NodeName;
          lAttrValue := lCurNode.Attributes.Item[i].NodeValue;
          case lAttrName of
          'cx', 'cy', 'fx', 'fy', 'r':
            lAttrValue_Double := StringWithUnitToFloat(lAttrValue, sckUnknown, suMM, suMM);
          end;

          case lAttrName of
          'id': lBrushEntity.Name := lAttrValue;
          'cx': lBrushEntity.Brush.Gradient_cx := lAttrValue_Double;
          'cy': lBrushEntity.Brush.Gradient_cy := lAttrValue_Double;
          'r':  lBrushEntity.Brush.Gradient_r  := lAttrValue_Double;
          'fx': lBrushEntity.Brush.Gradient_fx := lAttrValue_Double;
          'fy': lBrushEntity.Brush.Gradient_fy := lAttrValue_Double;
          end;
          //lBrushEntity.Gradient_cx_Unit, Gradient_cy_Unit, Gradient_r_Unit, Gradient_fx_Unit, Gradient_fy_Unit
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

        ReadDefs_LinearGradient(lBrushEntity, lCurNode, AData);

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
      // CSS styles
      'style':
      begin
        for i := 0 to lCurNode.Attributes.Length - 1 do
        begin
          lAttrName := LowerCase(lCurNode.Attributes.Item[i].NodeName);
          lAttrValue := lCurNode.Attributes.Item[i].NodeValue;
          if (lAttrName = 'type') and (lAttrValue = 'text/css') then
          begin
            ReadDefs_CSS(lCurNode);
          end;
        end;
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
    'defs': ReadDefsFromNode(ANode, AData, ADoc);
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
  cx, cy, cr, tmp: double;
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
  ApplyLayerStyles(AData, lCircle);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'cx' then
      cx := StringWithUnitToFloat(lNodeValue) //, sckX, suPX, suMM)
    else if lNodeName = 'cy' then
      cy := StringWithUnitToFloat(lNodeValue) //, sckY, suPX, suMM)
    else if lNodeName = 'r' then
      cr := StringWithUnitToFloat(lNodeValue) //, sckXSize, suPX, suMM)
    else if lNodeName = 'id' then
      lCircle.Name := lNodeValue;
  end;

  ConvertSVGCoordinatesToFPVCoordinates(AData, cx, cy, cx, cy);
  ConvertSVGSizeToFPVSize(AData, cr, cr, lCircle.Radius, tmp);
  lCircle.X := lCircle.X + cx;
  lCircle.Y := lCircle.Y + cy;

  // Make sure that transformations are read after geometry and position
  // of cirlce is known.
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'style' then
      ReadSVGStyle(AData, lNodeValue, lCircle)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lCircle);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lCircle);
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName, lNodeValue, lCircle);
    end;
  end;

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
  ApplyLayerStyles(AData, lEllipse);

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
      lEllipse.Name := ANode.Attributes.Item[i].NodeValue;
  end;

  ConvertSVGCoordinatesToFPVCoordinates(AData, cx, cy, lEllipse.X, lEllipse.Y);
  ConvertSVGSizeToFPVSize(AData, crx, cry, lEllipse.HorzHalfAxis, lEllipse.VertHalfAxis);

  // Make sure that transformations are read after geometry and position
  // of ellipse is known.
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'style' then
      ReadSVGStyle(AData, lNodeValue, lEllipse)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lEllipse);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lEllipse);
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName, lNodeValue, lEllipse);
    end;
  end;

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
  ApplyLayerStyles(AData, lText);

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
      ReadSVGStyle(AData, lNodeValue, lText);
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
  ApplyLayerStyles(AData, lText);

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
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName, lNodeValue, lImage);
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
  ApplyLayerStyles(AData, lImage);

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

  // Add default SVG pen/brush
  lPath.Pen.Style := psClear;

  // Apply the layer style
  ApplyLayerStyles(AData, lPath);

  // Add the entity styles
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'style' then
      ReadSVGStyle(AData, ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName,
        ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName,
        ANode.Attributes.Item[i].NodeValue, lPath);
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
  lPath.Brush.Style := bsClear;
  // Apply the layer style
  ApplyLayerStyles(AData, lPath);
  // Add the pen/brush/name
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'id' then
      lPath.Name := ANode.Attributes.Item[i].NodeValue
    else if lNodeName = 'style' then
      ReadSVGStyle(AData, ANode.Attributes.Item[i].NodeValue, lPath)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName,
        ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName,
        ANode.Attributes.Item[i].NodeValue, lPath);
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName,
        ANode.Attributes.Item[i].NodeValue, lPath);
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
  lIsFirstPathMove: Boolean;
begin
  FSVGPathTokenizer.ClearTokens;
  FSVGPathTokenizer.TokenizePathString(AStr);
  //lDebugStr := FSVGPathTokenizer.DebugOutTokensAsString();
  CurX := 0;
  CurY := 0;
  lIsFirstPathMove := true;
  lLastCommandToken := sttFloatValue;

  i := 0;
  while i < FSVGPathTokenizer.Tokens.Count do
  begin
    lCurTokenType := FSVGPathTokenizer.Tokens.Items[i].TokenType;
    if not (lCurTokenType = sttFloatValue) then
    begin
      lLastCommandToken := lCurTokenType;
      ReadNextPathCommand(lCurTokenType, i, lIsFirstPathMove, CurX, CurY, AData, ADoc);
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
      ReadNextPathCommand(lTmpTokenType, i, lIsFirstPathMove, CurX, CurY, AData, ADoc);
    end;
  end;
end;

procedure TvSVGVectorialReader.ReadNextPathCommand(ACurTokenType: TSVGTokenType;
  var i: Integer; var AIsFirstPathMove: Boolean; var CurX, CurY: Double; AData: TvVectorialPage;
  ADoc: TvVectorialDocument);
var
  X, Y, X2, Y2, X3, Y3, XQ, YQ, Xnew, Ynew, cx, cy, phi, tmp: Double;
  LargeArcFlag, SweepFlag, LeftmostEllipse, ClockwiseArc: Boolean;
  lCurTokenType: TSVGTokenType;
  lDebugStr: String;
  lToken5Before, lToken7Before: TSVGTokenType;
  lCorrectPreviousToken: Boolean;
  lPrevRelative, lCurRelative: Boolean;
begin
  lCurTokenType := ACurTokenType;
  // --------------
  // Moves
  // --------------
  if lCurTokenType in [sttMoveTo, sttRelativeMoveTo] then
  begin
    // The point at which the polygon starts.
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
    // Since there may be several subpolygons we must store the start point
    // to close the subpolygon correctly later.
    if AIsFirstPathMove then
    begin
      FPathStart.X := CurX;
      FPathStart.Y := CurY;
      AIsFirstPathMove := false;
    end;

    Inc(i, 3);
  end
  // --------------
  // Close Path
  // --------------
  else if lCurTokenType = sttClosePath then
  begin
    // Repeat the first point of the subpolygon
    CurX := FPathStart.X;
    CurY := FPathStart.Y;
    AData.AddLineToPath(CurX, CurY);

    Inc(i, 1);
  end
  // --------------
  // Lines
  // --------------
  else if lCurTokenType in [sttLineTo, sttRelativeLineTo, sttHorzLineTo,
    sttRelativeHorzLineTo, sttVertLineTo, sttRelativeVertLineTo] then
  begin
    if lCurTokenType in [sttLineTo, sttRelativeLineTo] then
    begin
      X := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      if lCurTokenType = sttLineTo then
        ConvertSVGCoordinatesToFPVCoordinates(AData, X,Y, CurX,CurY)
      else
      begin
        ConvertSVGDeltaToFPVDelta(AData, X,Y, X,Y);
        CurX := CurX + X;
        CurY := CurY + Y;
      end;
      inc(i, 3);
    end else
    if lCurTokenType in [sttHorzLineTo, sttVertLineTo] then
    begin
      tmp := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      ConvertSVGCoordinatesToFPVCoordinates(AData, tmp, tmp, X, Y);
      if lCurTokenType = sttHorzLineTo then
        CurX := X else
        CurY := Y;
      inc(i, 2);
    end else
    if lCurTokenType in [sttRelativeHorzLineTo, sttRelativeVertLineTo] then
    begin
      tmp := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      ConvertSVGDeltaToFPVDelta(AData, tmp, tmp, X, Y);
      if lCurTokenType = sttRelativeHorzLineTo then
        CurX := CurX + X else
        CurY := CurY + Y;
      inc(i, 2);
    end;
    AData.AddLineToPath(CurX, CurY);
  end
  // --------------
  // Cubic Bezier
  // --------------
  else if lCurTokenType in [sttBezierTo, sttRelativeBezierTo,
    sttSmoothBezierTo, sttRelativeSmoothBezierTo] then
  begin
    lPrevRelative := false;
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
        lPrevRelative := (lToken5Before = sttRelativeSmoothBezierTo) or (lToken7Before = sttRelativeBezierTo);
      end;
      if (i >= 7) and (lCorrectPreviousToken) then
      begin
        if (lCurTokenType = sttRelativeSmoothBezierTo) or lPrevRelative then
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
    lCurRelative := lCurTokenType in [sttRelativeBezierTo, sttRelativeSmoothBezierTo];
    if lPrevRelative then
    begin
      ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
      if lCurRelative then
      begin
        ConvertSVGDeltaToFPVDelta(AData, X3, Y3, X3, Y3);
        ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
      end else
      begin
        ConvertSVGCoordinatesToFPVCoordinates(AData, X3, Y3, X3, Y3);
        ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
      end;
    end else
    begin
      if lCurRelative then
      begin
        ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
        ConvertSVGDeltaToFPVDelta(AData, X3, Y3, X3, Y3);
        ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
      end else
      begin
        ConvertSVGCoordinatesToFPVCoordinates(AData, X2, Y2, X2, Y2);
        ConvertSVGCoordinatesToFPVCoordinates(AData, X3, Y3, X3, Y3);
        ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
      end;
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
      if lPrevRelative then
        AData.AddBezierToPath(X2 + CurX, Y2 + CurY, X3, Y3, X, Y) else
        AData.AddBezierToPath(X2, Y2, X3, Y3, X, Y);
      CurX := X;
      CurY := Y;
    end;

    if lCurTokenType in [sttBezierTo, sttRelativeBezierTo] then
      Inc(i, 7) else
      Inc(i, 5);
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
    phi := FSVGPathTokenizer.Tokens.Items[i+3].Value; // RotationX
    phi := DegToRad(phi);  // degrees to radians conversion
    LargeArcFlag := Round(FSVGPathTokenizer.Tokens.Items[i+4].Value) = 1;
    SweepFlag := Round(FSVGPathTokenizer.Tokens.Items[i+5].Value) = 1;
    X := FSVGPathTokenizer.Tokens.Items[i+6].Value;  // X
    Y := FSVGPathTokenizer.Tokens.Items[i+7].Value;  // Y

    {
    if lCurTokenType = sttRelativeEllipticArcTo then
    begin
      Xnew := CurX + X;
      Ynew := CurY + Y;
    end else
    begin
      Xnew := CurX;
      Ynew := CurY;
    end;

    CalcEllipseCenter(CurX, CurY, Xnew, Ynew, X2, Y2, phi, LargeArcFlag, SweepFlag, cx, cy, tmp);
    ConvertSVGCoordinatesToFPVCoordinates(AData, cx, cy, cx, cy);
     }
    // non-coordinate values (radii)
    ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
    if X2 < 0 then X2 := -X2;
    if Y2 < 0 then Y2 := -Y2;

    // Careful that absolute coordinates require using ConvertSVGCoordinatesToFPVCoordinates
    if lCurTokenType in [sttRelativeEllipticArcTo] then
    begin
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);
      Xnew := CurX + X;
      Ynew := CurY + Y;
    end else
    begin
      ConvertSVGCoordinatesToFPVCoordinates(AData, X, Y, X, Y);
      Xnew := X;
      Ynew := Y;
    end;

    // in svg the y axis increases downward, in fpv upward. Therefore, angles
    // change their sign!
    if vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags then
    begin
      phi := -phi;
      SweepFlag := not SweepFlag;  // i.e. "clockwise" turns into "counter-clockwise"!
    end;

    if CalcEllipseCenter(CurX, CurY, Xnew, Ynew, X2, Y2, phi, LargeArcFlag, SweepFlag, cx, cy, tmp) then
      AData.AddEllipticalArcWithCenterToPath(X2*tmp, Y2*tmp, phi, Xnew, Ynew, cx, cy, SweepFlag)
    else
      // Use a straight segment in case of no solution existing for the ellipse center
      AData.AddLineToPath(Xnew, Ynew);

    CurX := Xnew;
    CurY := Ynew;
    {
    // Convert SVG flags to fpvectorial flags
    LeftmostEllipse := (LargeArcFlag and (not SweepFlag))
      or ((not LargeArcFlag) and SweepFlag);
    if (Y > CurY) or ((Y = CurY) and (X > CurX)) then
      LeftMostEllipse := not LeftMostEllipse;
      // if Y = CurY then "LeftMost" is to be understood as "TopMost"
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
     }

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
  FSVGPathTokenizer.ClearTokens;
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
  lNodeName, lNodeValue: DOMString;
  lPath: TPath;
  lIsPolygon: Boolean = False;
begin
  lIsPolygon := LowerCase(ANode.NodeName) = 'polygon';

  // first get the points
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'points' then
      lPointsStr := ANode.Attributes.Item[i].NodeValue;
  end;

  AData.StartPath();
  ReadPointsFromString(lPointsStr, AData, ADoc, lIsPolygon);
  lPath := AData.EndPath(True);
  Result := lPath;

  // Add default SVG pen/brush
  lPath.Pen.Style := psClear;
  lPath.Brush.Style := bsClear;

  // Apply the layer style
  ApplyLayerStyles(AData, lPath);

  // now read the other attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if lNodeName = 'id' then
      lPath.Name := lNodeValue
    else if IsEntityStyleField(lNodeName) then
      ReadEntityStyleField(AData, lNodeName, lNodeValue, lPath);
  end;
end;

// <rect width="90" height="90" stroke="green" stroke-width="3" fill="yellow" filter="url(#f1)" />
function TvSVGVectorialReader.ReadRectFromNode(ANode: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument): TvEntity;
var
  lx, ly, cx, cy, lrx, lry: double;
  lRect: TvRectangle;
  i: Integer;
  lNodeName: DOMString;
  lNodeValue: String;
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
  ApplyLayerStyles(AData, lRect);

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if  lNodeName = 'x' then
      lx := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'y' then
      ly := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'rx' then
      lrx := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'ry' then
      lry := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'width' then
      cx := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'height' then
      cy := StringWithUnitToFloat(lNodeValue)
    else if lNodeName = 'id' then
      lRect.Name := lNodeValue;
  end;

  ConvertSVGCoordinatesToFPVCoordinates(AData, lx, ly, lRect.X, lRect.Y);
  ConvertSVGSizeToFPVSize(AData, cx, cy, lRect.CX, lRect.CY);
  ConvertSVGSizeToFPVSize(AData, lrx, lry, lRect.RX, lRect.RY);
  lRect.RX := Abs(lRect.RX) * 2;
  lRect.RY := Abs(lRect.RY) * 2;

  // Make sure that transformations are read after geometry and position
  // of rectangle is known.
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    if lNodeName = 'style' then
      ReadSVGStyle(AData, lNodeValue, lRect)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGPenStyleWithKeyAndValue(lNodeName,
        ANode.Attributes.Item[i].NodeValue, lRect);
      ReadSVGBrushStyleWithKeyAndValue(lNodeName,
        ANode.Attributes.Item[i].NodeValue, lRect);
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName,
        ANode.Attributes.Item[i].NodeValue, lRect);
    end;
  end;

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
    i,j: Integer;
    lCurNode: TDOMNode;
    lTextStr: string;
    lText: TvText;
    lCText: TvCurvedText;
    lInsertedEntity, lInsertedSubEntity: TvEntity;
    s: String;
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
        ApplyLayerStyles(AData, lCText);

        // Apply the layer style
        ApplyStackStylesToText(lCText);
      end
      else
      if lNodeValue <> '' then
      begin
        lText := lParagraph.AddText(lNodeValue);
        lText.Font.Size := 10;
        lText.Name := lName;
        // Apply the layer style
        ApplyLayerStyles(AData, lText);
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
  lParagraph.YPos_NeedsAdjustment_DelFirstLineBodyHeight := True;
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
      ReadSVGStyle(AData, lNodeValue, nil, lCurStyle)
    else if IsAttributeFromStyle(lNodeName) then
    begin
      ReadSVGFontStyleWithKeyAndValue(lNodeName, lNodeValue, nil, lCurStyle);
  //    ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName, lNodeValue, lParagraph);
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

  // read the attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
    if IsAttributeFromStyle(lNodeName) then
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName, lNodeValue, lParagraph);
  end;

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
      ReadSVGStyle(AData, lNodeValue, lInsert);
    end
    else if IsAttributeFromStyle(lNodeName) then
    begin
      lInsert.SetElements += ReadSVGPenStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
      lInsert.SetElements += ReadSVGBrushStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
      lInsert.SetElements += ReadSVGFontStyleWithKeyAndValue(lNodeName, lNodeValue, lInsert);
      ReadSVGGeneralStyleWithKeyAndValue(AData, lNodeName, lNodeValue, lInsert);
    end;
  end;

  // We need to add this hack here, otherwise the Height is added twice
  // to inserted items: Once in the Insert and yet another time in the
  // coordinates of the inserted item!
  if vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags then
    lInsert.Y := lInsert.Y - AData.Height;

  Result := lInsert;
end;

procedure TvSVGVectorialReader.StringToPenPattern(const AStr: String;
  var APen: TvPen);
var
  float_patt: TDoubleArray;
  patt: array of LongWord;
  i: Integer;
begin
  if AStr = 'none' then
    exit;

  float_patt := ReadSpaceSeparatedFloats(AStr, ',');
  if Length(float_patt) < 2 then
    exit;

  SetLength(patt, Length(float_patt));
  for i:=0 to High(patt) do
  begin
    if float_patt[i] < 0 then
      raise Exception.CreateFmt('Incorrect value in stroke-dasharray "%s"', [AStr]);
    patt[i] := round(float_patt[i]);
  end;

  case Length(patt) of
    2: if patt[1] = 5 then
         case patt[0] of
           3: begin APen.Style := psDot;  exit; end;     // stroke-dasharray: 3, 5
           9: begin APen.Style := psDash; exit; end;     // stroke-dasharray: 9, 5
         end;
    4: if (patt[0] = 9) and (patt[1] = 5) and (patt[2] = 3) and (patt[3] = 5) then
       begin                             // stroke-dasharray: 9, 5, 3, 5
         APen.Style := psDashDot;
         exit;
       end;
    6: if (patt[0] = 9) and (patt[1] = 5) and (patt[2] = 3) and (patt[3] = 5) and
          (patt[4] = 3) and (patt[5] = 5)
       then begin                       // stroke-dasharray: 9, 5, 3, 5, 3, 5
         APen.Style := psDashDotDot;
         exit;
       end;
  end;
  APen.Style := psPattern;
  APen.Pattern := patt;
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
      end;
      if not (vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags) then
      begin
        case ACoordKind of
          sckY:      Result := (Result - ViewBox_Top) * Page_Height / ViewBox_Height;
          sckYDelta,
          sckYSize:  Result := Result * Page_Height / ViewBox_Height;
        end;
      end
      else
      begin
        case ACoordKind of
          sckY:      Result := Page_Height - (Result - ViewBox_Top) * Page_Height / ViewBox_Height;
          sckYDelta: Result := - (Result - ViewBox_Top) * Page_Height / ViewBox_Height;
          sckYSize:  Result := Result * Page_Height / ViewBox_Height;
        end;
      end;
      ViewPortApplied := True;
    end
    else
    begin
      if vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags then
      begin
        case ACoordKind of
          sckY:      Result := Page_Height - Result;
          sckYDelta: Result := - Result;
        end;
      end;
    end;
  end;

  procedure DoProcessMM_End();
  begin
    if ATargetUnit = suPX then
      Result := Result / FLOAT_MILLIMETERS_PER_PIXEL;
    DoViewBoxAdjust();
  end;

  procedure DoProcessPX();
  begin
    Result := StrToFloat(ValueStr, FPointSeparator);
    case ATargetUnit of
    suMM: Result := Result * FLOAT_MILLIMETERS_PER_PIXEL;
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
  else if UnitStr = 'in' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToFloat(ValueStr, FPointSeparator);
    Result := Result * 25.4;
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

{@@ Converts a number string to a floating-point number. If the string has a
    % character at its end then it is removed, and the numerical value is
    divided by 100. }
function TvSVGVectorialReader.StringWithPercentToFloat(AStr: String): Double;
begin
  if AStr[Length(AStr)] = '%' then
  begin
    Delete(AStr, Length(AStr), 1);
    Result := 0.01 * StrToFloat(trim(AStr), FPointSeparator);
  end else
    Result := StrToFloat(AStr, FPointSeparator);
end;

procedure TvSVGVectorialReader.ConvertSVGCoordinatesToFPVCoordinates(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double;
  var ADestX,ADestY: Double; ADoViewBoxAdjust: Boolean = True);
begin
  ADestX := ASrcX * FLOAT_MILLIMETERS_PER_PIXEL;
  if ViewBoxAdjustment and ADoViewBoxAdjust then
  begin
    ADestX := (ASrcX - ViewBox_Left) * Page_Width / ViewBox_Width;
  end;

  if not (vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags) then
  begin
    ADestY := ASrcY * FLOAT_MILLIMETERS_PER_PIXEL;
    if ViewBoxAdjustment and ADoViewBoxAdjust then
    begin
      ADestY := (ASrcY - ViewBox_Top) * Page_Height / ViewBox_Height;
    end;
  end
  else
  begin
    ADestY := AData.Height - ASrcY * FLOAT_MILLIMETERS_PER_PIXEL;
    if ViewBoxAdjustment and ADoViewBoxAdjust then
    begin
      ADestY := AData.Height - (ASrcY - ViewBox_Top) * Page_Height / ViewBox_Height;
    end;
  end;
end;

procedure TvSVGVectorialReader.ConvertSVGDeltaToFPVDelta(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: Double; ADoViewBoxAdjust: Boolean = True);
begin
  ADestX := ASrcX * FLOAT_MILLIMETERS_PER_PIXEL;
  if ViewBoxAdjustment and ADoViewBoxAdjust then
  begin
    ADestX := ASrcX * Page_Width / ViewBox_Width;
  end;

  if not (vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags) then
  begin
    ADestY := ASrcY * FLOAT_MILLIMETERS_PER_PIXEL;
    if ViewBoxAdjustment and ADoViewBoxAdjust then
    begin
      ADestY := ASrcY * Page_Height / ViewBox_Height;
    end;
  end
  else
  begin
    ADestY := -ASrcY * FLOAT_MILLIMETERS_PER_PIXEL;  // fpc y coords grow upward, svg downward
    if ViewBoxAdjustment and ADoViewBoxAdjust then
    begin
      ADestY := -ASrcY * Page_Height / ViewBox_Height;
    end;
  end;
end;

procedure TvSVGVectorialReader.ConvertSVGSizeToFPVSize(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: Double; ADoViewBoxAdjust: Boolean = True);
begin
  ADestX := ASrcX * FLOAT_MILLIMETERS_PER_PIXEL;
  ADestY := ASrcY * FLOAT_MILLIMETERS_PER_PIXEL;
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
  FCSSDefs := TFPList.Create;
end;

destructor TvSVGVectorialReader.Destroy;
var
  i: Integer;
begin
  FSVGPathTokenizer.Free;
  FLayerStylesKeys.Free;
  FLayerStylesValues.Free;
  for i:=FBrushDefs.Count-1 downto 0 do TObject(FBrushDefs[i]).Free;
  FBrushDefs.Free;
  for i:=FCSSDefs.Count-1 downto 0 do TObject(FCSSDefs[i]).Free;
  FCSSDefs.Free;

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

  // Make sure the latest page size is synchronized with auto-detected
  // or ViewBox-only obtained size
  Page_Width := AData.Width;
  Page_Height := AData.Height;

  // ----------------
  // Now process the elements
  // ----------------
  lCurNode := Doc.DocumentElement.FirstChild;
  lPage := AData.AddPage(not (vrfSVG_UseBottomLeftCoords in Settings.VecReaderFlags));
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

