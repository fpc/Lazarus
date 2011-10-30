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
  xmlread, dom, fgl,
  fpvectorial, fpvutils;

type
  TSVGTokenType = (sttMoveTo, sttLineTo, sttBezierTo, sttFloatValue);

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
  end;

  { TvSVGVectorialReader }

  TvSVGVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    FSVGPathTokenizer: TSVGPathTokenizer;
    procedure ReadPathFromNode(APath: TDOMNode; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadPathFromString(AStr: string; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    function  StringWithUnitToFloat(AStr: string): Single;
    procedure ConvertSVGCoordinatesToFPVCoordinates(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Float; var ADestX, ADestY: Float);
    procedure ConvertSVGDeltaToFPVDelta(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Float; var ADestX, ADestY: Float);
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
begin
  lToken := TSVGToken.Create;

  if AStr = 'm' then lToken.TokenType := sttMoveTo
  else if AStr = 'l' then lToken.TokenType := sttLineTo
  else if AStr = 'c' then lToken.TokenType := sttBezierTo
  else
  begin
    lToken.TokenType := sttFloatValue;
    lToken.Value := StrToFloat(AStr, FPointSeparator);
  end;

  Tokens.Add(lToken);
end;

procedure TSVGPathTokenizer.TokenizePathString(AStr: string);
const
  Str_Space: Char = ' ';
  Str_Comma: Char = ',';
var
  i: Integer;
  lTmpStr: string;
  lState: Integer;
  lCurChar: Char;
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
        lTmpStr := lTmpStr + lCurChar;

      Inc(i);
    end;
    1: // Removing spaces
    begin
      if AStr[i] <> Str_Space then lState := 0
      else Inc(i);
    end;
    end;
  end;
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

procedure TvSVGVectorialReader.ReadPathFromNode(APath: TDOMNode;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  lNodeName, lStyleStr, lDStr: WideString;
  i: Integer;
begin
  for i := 0 to APath.Attributes.Length - 1 do
  begin
    lNodeName := APath.Attributes.Item[i].NodeName;
    if  lNodeName = 'style' then
      lStyleStr := APath.Attributes.Item[i].NodeValue
    else if lNodeName = 'd' then
      lDStr := APath.Attributes.Item[i].NodeValue
  end;

  AData.StartPath();
  ReadPathFromString(UTF8Encode(lDStr), AData, ADoc);
  AData.EndPath();
end;

procedure TvSVGVectorialReader.ReadPathFromString(AStr: string;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i: Integer;
  X, Y, X2, Y2, X3, Y3: Float;
  CurX, CurY: Float;
begin
  FSVGPathTokenizer.Tokens.Clear;
  FSVGPathTokenizer.TokenizePathString(AStr);
  CurX := 0;
  CurY := 0;

  i := 0;
  while i < FSVGPathTokenizer.Tokens.Count do
  begin
    if FSVGPathTokenizer.Tokens.Items[i].TokenType = sttMoveTo then
    begin
      CurX := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      CurY := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      ConvertSVGCoordinatesToFPVCoordinates(AData, CurX, CurY, CurX, CurY);

      AData.AddMoveToPath(CurX, CurY);

      Inc(i, 3);
    end
    else if FSVGPathTokenizer.Tokens.Items[i].TokenType = sttLineTo then
    begin
      X := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      // LineTo uses relative coordenates in SVG
      CurX := CurX + X;
      CurY := CurY + Y;

      AData.AddLineToPath(CurX, CurY);

      Inc(i, 3);
    end
    else if FSVGPathTokenizer.Tokens.Items[i].TokenType = sttBezierTo then
    begin
      X2 := FSVGPathTokenizer.Tokens.Items[i+1].Value;
      Y2 := FSVGPathTokenizer.Tokens.Items[i+2].Value;
      X3 := FSVGPathTokenizer.Tokens.Items[i+3].Value;
      Y3 := FSVGPathTokenizer.Tokens.Items[i+4].Value;
      X := FSVGPathTokenizer.Tokens.Items[i+5].Value;
      Y := FSVGPathTokenizer.Tokens.Items[i+6].Value;

      ConvertSVGDeltaToFPVDelta(AData, X2, Y2, X2, Y2);
      ConvertSVGDeltaToFPVDelta(AData, X3, Y3, X3, Y3);
      ConvertSVGDeltaToFPVDelta(AData, X, Y, X, Y);

      AData.AddBezierToPath(X2 + CurX, Y2 + CurY, X3 + CurX, Y3 + CurY, X + CurX, Y + CurY);

      // BezierTo uses relative coordenates in SVG
      CurX := CurX + X;
      CurY := CurY + Y;

      Inc(i, 7);
    end
    else
    begin
      Inc(i);
    end;
  end;
end;

function TvSVGVectorialReader.StringWithUnitToFloat(AStr: string): Single;
var
  UnitStr, ValueStr: string;
  Len: Integer;
begin
  // Check the unit
  Len := Length(AStr);
  UnitStr := Copy(AStr, Len-1, 2);
  if UnitStr = 'mm' then
  begin
    ValueStr := Copy(AStr, 1, Len-2);
    Result := StrToInt(ValueStr);
  end;
end;

procedure TvSVGVectorialReader.ConvertSVGCoordinatesToFPVCoordinates(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Float;
  var ADestX,ADestY: Float);
begin
  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
  ADestY := AData.Height - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
end;

procedure TvSVGVectorialReader.ConvertSVGDeltaToFPVDelta(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Float; var ADestX,
  ADestY: Float);
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
end;

destructor TvSVGVectorialReader.Destroy;
begin
  FSVGPathTokenizer.Free;

  inherited Destroy;
end;

procedure TvSVGVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument;
  lFirstLayer, lCurNode: TDOMNode;
  lPage: TvVectorialPage;
begin
  try
    // Read in xml file from the stream
    ReadXMLFile(Doc, AStream);

    // Read the properties of the <svg> tag
    AData.Width := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('width'));
    AData.Height := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('height'));

    // Now process the elements inside the first layer
    lFirstLayer := Doc.DocumentElement.FirstChild;
    lCurNode := lFirstLayer.FirstChild;
    lPage := AData.AddPage();
    lPage.Width := AData.Width;
    lPage.Height := AData.Height;
    while Assigned(lCurNode) do
    begin
      ReadPathFromNode(lCurNode, lPage, AData);
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

