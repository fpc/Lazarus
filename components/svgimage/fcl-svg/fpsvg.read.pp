{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    XML to SVG tree, and the attribute value grammars.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.read;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.StrUtils, System.Hash.Base64,
     System.ZLib.Zstream, Xml.Dom, Xml.Read, fpsvg.types, fpsvg.dom,
     fpsvg.path;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, strutils, base64, zstream, dom, xmlread,
     fpsvg.types, fpsvg.dom, fpsvg.path;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGRead = class(ESVGError);

  TSVGReadOption = (roKeepForeign, roKeepComments, roPreserveSpace,
                    roExternalEntities);
  TSVGReadOptions = set of TSVGReadOption;

  { Parses the length grammar and length-valued attributes. }
  TSVGLengthReadHelper = record helper for TSVGLength
    // Parses a number followed by an optional unit.
    function TryParse(const aText: TSVGString): Boolean;
    // Parses aText. Keeps the current value when the text is malformed.
    procedure ParseDef(const aText: TSVGString);
    // Reads a length-valued attribute. Keeps the current value when the
    // attribute is absent.
    procedure ReadAttribute(aElement: TSVGElement; const aName: TSVGString);
  end;

  { Parses the transform list grammar. }
  TSVGMatrixReadHelper = record helper for TSVGMatrix
    // Parses a transform list into the one matrix it composes to.
    function TryParse(const aText: TSVGString): Boolean;
    // Reads the transform attribute. Gives the identity when it is absent or malformed.
    procedure ReadAttribute(aElement: TSVGElement);
    // Reads a named transform-valued attribute, such as gradientTransform.
    procedure ReadAttributeNamed(aElement: TSVGElement; const aName: TSVGString);
  end;

  { Parses the viewBox grammar. }
  TSVGRectReadHelper = record helper for TSVGRect
    // Parses "x y width height". A negative width or height is rejected.
    function TryParseViewBox(const aText: TSVGString): Boolean;
    // Reads the viewBox attribute. False when it is absent or malformed.
    function ReadViewBoxAttribute(aElement: TSVGElement): Boolean;
  end;

  { Parses the preserveAspectRatio grammar. }
  TSVGPreserveAspectRatioReadHelper = record helper for TSVGPreserveAspectRatio
    // Parses an alignment keyword, such as xMidYMid, optionally followed by meet or slice.
    function TryParse(const aText: TSVGString): Boolean;
    // Reads the attribute. When it is absent, the drawing is scaled to fit inside the viewport and centred there.
    procedure ReadAttribute(aElement: TSVGElement);
  end;

  { Builds an SVG tree from XML. }
  TSVGReader = class(TObject)
  private
    FOptions: TSVGReadOptions;
    function BuildElement(aSource: TDOMElement): TSVGElement;
    procedure BuildChildren(aSource: TDOMElement; aTarget: TSVGElement);
    procedure CopyAttributes(aSource: TDOMElement; aTarget: TSVGElement);
  public
    // Reads a document from a file.
    function ReadFromFile(const aFileName: String): TSVGDocument;
    // Reads a document from a stream.
    function ReadFromStream(aStream: TStream): TSVGDocument;
    { Reads a document from SVG source held in a string. The text is
      taken byte for byte, the encoding being the one the document itself
      declares. }
    function ReadFromString(const aText: RawByteString): TSVGDocument;
    // Converts an XML document that is already parsed. The caller keeps
    // ownership of it.
    function ReadFromXML(aXML: TXMLDocument): TSVGDocument;
    // Options that change how a document is read.
    property Options: TSVGReadOptions read FOptions write FOptions;
  end;

// Reads an SVG document from a file.
function ReadSVGFile(const aFileName: String): TSVGDocument;
// Reads an SVG document from a stream.
function ReadSVGStream(aStream: TStream): TSVGDocument;
{ Reads an SVG document from SVG source held in a string. The text is
  taken byte for byte, the encoding being the one the document itself
  declares. }
function ReadSVGString(const aText: RawByteString): TSVGDocument;
// True when a reference has the content inline instead of pointing to a
// file.
function SVGIsDataURI(const aHRef: String): Boolean;
// True when a data URI contains an SVG document, according to the media
// type it declares. A URI that declares no media type contains text,
// which is not SVG.
function SVGDataURIHoldsSVG(const aHRef: String): Boolean;
// Reads the document inside a data URI, gzipped or not.
// Nil when it has no document that can be read.
function ReadSVGDataURI(const aHRef: String): TSVGDocument;

// Parses a bare number, rejecting trailing characters.
function TryStrToSVGNumber(const aText: TSVGString; out aValue: Double): Boolean;
// Parses a list of numbers separated by whitespace or commas.
function TryStrToSVGNumberList(const aText: TSVGString;  out aValues: TSVGDoubleArray): Boolean;
// Parses a list of lengths, such as the x, y, dx and dy of a text element.
// Each value may have a unit: 2em is a coordinate as well as 32.
function TryStrToSVGLengthList(const aText: TSVGString; out aValues: TSVGLengthArray): Boolean;
// Parses a list of coordinate pairs into points.
// A lone coordinate at the end, and anything that is not a number, end the list at that position.
// SVG 1.1 draws a polyline or a polygon up to the first coordinate it cannot read.
// False when there is not one whole pair.
function TryStrToSVGPointList(const aText: TSVGString; out aPoints: TSVGPointArray): Boolean;
// Reads href. The plain attribute takes precedence over the xlink one.
function SVGHRefOf(aElement: TSVGElement): TSVGString;
// True when the element is a shape that converts to a path.
function IsSVGShapeElement(aElement: TSVGElement): Boolean;
// True when the element holds text, where a run of spaces is content.
function IsSVGTextContentElement(aElement: TSVGElement): Boolean;
// True when the doctype declares an entity whose body is another file.
function SVGDeclaresExternalEntity(const aText: RawByteString): Boolean;
// Builds the path of a shape element, replacing the contents of aPath.
// False when the element is not a shape, or its geometry is malformed.
function BuildSVGShapePath(aElement: TSVGElement; aPath: TSVGPath;
  const aContext: TSVGLengthContext): Boolean;

// The view a view element declares, under its id.
function SVGViewOf(aElement: TSVGElement): TSVGView;
// The views a document declares, in document order.
function SVGViewsOf(aDocument: TSVGDocument): TSVGViewArray;
// The view of that name, False when the document declares no such view.
function SVGFindView(aDocument: TSVGDocument; const aName: String;
  out aView: TSVGView): Boolean;
// The view a fragment identifier selects: a view element by name, or an svgView(...) specification.
// False when it selects neither.
// transform(), zoomAndPan() and viewTarget() are read past and dropped.
function SVGViewOfFragment(aDocument: TSVGDocument; const aFragment: String;
  out aView: TSVGView): Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

const
  UnitNames: array[TSVGLengthUnit] of String =
    ('', '%', 'em', 'ex', 'px', 'cm', 'mm', 'in', 'pt', 'pc');
  AlignNames: array[TSVGAspectAlign] of String =
    ('none', 'xMinYMin', 'xMidYMin', 'xMaxYMin',
     'xMinYMid', 'xMidYMid', 'xMaxYMid',
     'xMinYMax', 'xMidYMax', 'xMaxYMax');

function TryStrToSVGNumber(const aText: TSVGString; out aValue: Double): Boolean;

var
  lScanner: TSVGScanner;

begin
  lScanner := TSVGScanner.Create(aText);
  lScanner.SkipWSP;
  Result := lScanner.ScanNumber(aValue);
  if not Result then
    Exit;
  lScanner.SkipWSP;
  Result := lScanner.AtEnd;
end;


function TryStrToSVGNumberList(const aText: TSVGString;
  out aValues: TSVGDoubleArray): Boolean;

var
  lScanner: TSVGScanner;
  lCount: Integer;
  lValue: Double;

begin
  SetLength(aValues, 8);
  lCount := 0;
  lScanner := TSVGScanner.Create(aText);
  lScanner.SkipWSP;
  while not lScanner.AtEnd do
    begin
    if not lScanner.ScanNumber(lValue) then
      begin
      aValues := nil;
      Exit(False);
      end;
    if lCount = Length(aValues) then
      SetLength(aValues, lCount * 2);
    aValues[lCount] := lValue;
    Inc(lCount);
    lScanner.SkipWSPComma;
    end;
  SetLength(aValues, lCount);
  Result := True;
end;


function TryStrToSVGLengthList(const aText: TSVGString;
  out aValues: TSVGLengthArray): Boolean;

var
  lScanner: TSVGScanner;
  lCount: Integer;
  lValue: Double;
  lSuffix: String;
  lUnit: TSVGLengthUnit;
  U: TSVGLengthUnit;
  lKnown: Boolean;

begin
  SetLength(aValues, 8);
  lCount := 0;
  lScanner := TSVGScanner.Create(aText);
  lScanner.SkipWSP;
  while not lScanner.AtEnd do
    begin
    if not lScanner.ScanNumber(lValue) then
      begin
      aValues := nil;
      Exit(False);
      end;
    lUnit := luNumber;
    if lScanner.SkipChar('%') then
      lUnit := luPercent
    else
      begin
      lSuffix := lScanner.ScanName;
      if lSuffix <> '' then
        begin
        lKnown := False;
        for U := Low(TSVGLengthUnit) to High(TSVGLengthUnit) do
          if SameText(lSuffix, UnitNames[U]) then
            begin
            lUnit := U;
            lKnown := True;
            Break;
            end;
        if not lKnown then
          begin
          aValues := nil;
          Exit(False);
          end;
        end;
      end;
    if lCount = Length(aValues) then
      SetLength(aValues, lCount * 2);
    aValues[lCount] := TSVGLength.Create(lValue, lUnit);
    Inc(lCount);
    lScanner.SkipWSPComma;
    end;
  SetLength(aValues, lCount);
  Result := True;
end;


function TryStrToSVGPointList(const aText: TSVGString;
  out aPoints: TSVGPointArray): Boolean;

var
  lScanner: TSVGScanner;
  lCount: Integer;
  lX, lY: Double;

begin
  SetLength(aPoints, 4);
  lCount := 0;
  lScanner := TSVGScanner.Create(aText);
  lScanner.SkipWSP;
  while not lScanner.AtEnd do
    begin
    if not lScanner.ScanNumber(lX) then
      Break;
    lScanner.SkipWSPComma;
    if not lScanner.ScanNumber(lY) then
      Break;
    if lCount = Length(aPoints) then
      SetLength(aPoints, lCount * 2);
    aPoints[lCount] := TSVGPoint.Create(lX, lY);
    Inc(lCount);
    lScanner.SkipWSPComma;
    end;
  SetLength(aPoints, lCount);
  Result := lCount > 0;
end;


function SVGHRefOf(aElement: TSVGElement): TSVGString;

begin
  if aElement = nil then
    Result := ''
  else
    Result := aElement.Attributes['href'];
end;


function IsSVGTextContentElement(aElement: TSVGElement): Boolean;

begin
  Result := (aElement is TSVGTextElement) or (aElement is TSVGTSpanElement)
         or (aElement is TSVGTextPathElement);
end;


// The value of a pseudo-attribute of a processing instruction,
// empty when there is none of that name.
function SVGPseudoAttribute(const aData, aName: String): String;

var
  lAt, lStop: Integer;
  lQuote: AnsiChar;

begin
  Result := '';
  lAt := 1;
  while lAt <= Length(aData) do
    begin
    lAt := PosEx(aName, aData, lAt);
    if lAt = 0 then
      Exit;
    if (lAt > 1) and not (aData[lAt - 1] in [' ', #9, #10, #13]) then
      begin
      Inc(lAt, Length(aName));
      Continue;
      end;
    lStop := lAt + Length(aName);
    while (lStop <= Length(aData)) and (aData[lStop] in [' ', #9, #10, #13]) do
      Inc(lStop);
    if (lStop > Length(aData)) or (aData[lStop] <> '=') then
      begin
      lAt := lStop;
      Continue;
      end;
    Inc(lStop);
    while (lStop <= Length(aData)) and (aData[lStop] in [' ', #9, #10, #13]) do
      Inc(lStop);
    if (lStop > Length(aData)) or not (aData[lStop] in ['''', '"']) then
      Exit;
    lQuote := aData[lStop];
    Inc(lStop);
    lAt := lStop;
    while (lStop <= Length(aData)) and (aData[lStop] <> lQuote) do
      Inc(lStop);
    Result := Copy(aData, lAt, lStop - lAt);
    Exit;
    end;
end;


// Records the stylesheets referenced by the xml-stylesheet instructions of a document. 
// An alternate sheet is left out, and so is one whose type is neither empty nor CSS.
procedure SVGCollectStyleSheetHRefs(aXML: TXMLDocument; aDocument: TSVGDocument);

var
  I: Integer;
  lNode: TDOMNode;
  lData, lType: String;

begin
  for I := 0 to aXML.ChildNodes.Count - 1 do
    begin
    lNode := aXML.ChildNodes[I];
    if not (lNode is TDOMProcessingInstruction) then
      Continue;
    if not SameText(UTF8Encode(TDOMProcessingInstruction(lNode).Target),
       'xml-stylesheet') then
      Continue;
    lData := UTF8Encode(TDOMProcessingInstruction(lNode).Data);
    if SameText(SVGPseudoAttribute(lData, 'alternate'), 'yes') then
      Continue;
    lType := Trim(SVGPseudoAttribute(lData, 'type'));
    if (lType <> '') and not SameText(lType, 'text/css') then
      Continue;
    aDocument.AddStyleSheetHRef(SVGPseudoAttribute(lData, 'href'));
    end;
end;


function SVGDeclaresExternalEntity(const aText: RawByteString): Boolean;

var
  lSubset, lDeclaration: String;
  lStart, lStop, lEnd: Integer;

begin
  Result := False;
  // Only the internal subset is searched.
  lStart := Pos('<!DOCTYPE', aText);
  if lStart = 0 then
    Exit;
  lStart := PosEx('[', aText, lStart);
  if lStart = 0 then
    Exit;
  lEnd := PosEx(']', aText, lStart);
  if lEnd = 0 then
    lEnd := Length(aText);
  lSubset := Copy(aText, lStart, lEnd - lStart);
  lStart := Pos('<!ENTITY', lSubset);
  while lStart > 0 do
    begin
    lStop := PosEx('>', lSubset, lStart);
    if lStop = 0 then
      lStop := Length(lSubset) + 1;
    lDeclaration := Copy(lSubset, lStart, lStop - lStart);
    if (Pos('SYSTEM', lDeclaration) > 0)
       or (Pos('PUBLIC', lDeclaration) > 0) then
      Exit(True);
    lStart := PosEx('<!ENTITY', lSubset, lStop);
    end;
end;


function IsSVGShapeElement(aElement: TSVGElement): Boolean;

begin
  Result := (aElement is TSVGRectElement) or (aElement is TSVGCircleElement)
         or (aElement is TSVGEllipseElement) or (aElement is TSVGLineElement)
         or (aElement is TSVGPolylineElement) or (aElement is TSVGPolygonElement)
         or (aElement is TSVGPathElement);
end;


// Resolves a length-valued attribute of a shape along the given axis.
function ShapeLength(aElement: TSVGElement; const aName: TSVGString;
  const aContext: TSVGLengthContext; aAxis: TSVGLengthAxis;
  aDefault: Double): Double;

var
  lLength: TSVGLength;

begin
  if not aElement.HasAttribute(aName) then
    Exit(aDefault);
  lLength := TSVGLength.Create(aDefault, luNumber);
  lLength.ParseDef(aElement.Attributes[aName]);
  Result := aContext.Resolve(lLength, aAxis);
end;


// Builds the path of a rect, applying the default rules for rx and ry.
function BuildRectPath(aElement: TSVGElement; aPath: TSVGPath;
  const aContext: TSVGLengthContext): Boolean;

var
  lWidth, lHeight, lRX, lRY: Double;

begin
  lWidth := ShapeLength(aElement, 'width', aContext, laHorizontal, 0);
  lHeight := ShapeLength(aElement, 'height', aContext, laVertical, 0);
  Result := (lWidth > 0) and (lHeight > 0);
  if not Result then
    Exit;
  lRX := ShapeLength(aElement, 'rx', aContext, laHorizontal, -1);
  lRY := ShapeLength(aElement, 'ry', aContext, laVertical, -1);
  aPath.AddRect(ShapeLength(aElement, 'x', aContext, laHorizontal, 0),
    ShapeLength(aElement, 'y', aContext, laVertical, 0),
    lWidth, lHeight, lRX, lRY);
end;


// Builds the path of a polyline or polygon from its points attribute.
function BuildPolyPath(aElement: TSVGElement; aPath: TSVGPath;
  aClose: Boolean): Boolean;

var
  lPoints: TSVGPointArray;

begin
  Result := TryStrToSVGPointList(aElement.Attributes['points'], lPoints)
        and (Length(lPoints) > 0);
  if Result then
    aPath.AddPolygon(lPoints, aClose);
end;


// Builds the path of a circle. Its single radius is resolved along the diagonal.
function BuildCirclePath(aElement: TSVGElement; aPath: TSVGPath;
  const aContext: TSVGLengthContext): Boolean;

var
  lRadius: Double;

begin
  lRadius := ShapeLength(aElement, 'r', aContext, laDiagonal, 0);
  Result := lRadius > 0;
  if Result then
    aPath.AddCircle(ShapeLength(aElement, 'cx', aContext, laHorizontal, 0),
      ShapeLength(aElement, 'cy', aContext, laVertical, 0), lRadius);
end;


// Builds the path of an ellipse. Both radii must be positive.
function BuildEllipsePath(aElement: TSVGElement; aPath: TSVGPath;
  const aContext: TSVGLengthContext): Boolean;

var
  lRX, lRY: Double;

begin
  lRX := ShapeLength(aElement, 'rx', aContext, laHorizontal, 0);
  lRY := ShapeLength(aElement, 'ry', aContext, laVertical, 0);
  Result := (lRX > 0) and (lRY > 0);
  if Result then
    aPath.AddEllipse(ShapeLength(aElement, 'cx', aContext, laHorizontal, 0),
      ShapeLength(aElement, 'cy', aContext, laVertical, 0), lRX, lRY);
end;


// Builds the path of a line, which is always two points.
function BuildLinePath(aElement: TSVGElement; aPath: TSVGPath;
  const aContext: TSVGLengthContext): Boolean;

begin
  aPath.AddLine(ShapeLength(aElement, 'x1', aContext, laHorizontal, 0),
    ShapeLength(aElement, 'y1', aContext, laVertical, 0),
    ShapeLength(aElement, 'x2', aContext, laHorizontal, 0),
    ShapeLength(aElement, 'y2', aContext, laVertical, 0));
  Result := True;
end;


function BuildSVGShapePath(aElement: TSVGElement; aPath: TSVGPath;
  const aContext: TSVGLengthContext): Boolean;

begin
  if (aElement = nil) or (aPath = nil) then
    Exit(False);
  aPath.Clear;
  if aElement is TSVGRectElement then
    Result := BuildRectPath(aElement, aPath, aContext)
  else if aElement is TSVGCircleElement then
    Result := BuildCirclePath(aElement, aPath, aContext)
  else if aElement is TSVGEllipseElement then
    Result := BuildEllipsePath(aElement, aPath, aContext)
  else if aElement is TSVGLineElement then
    Result := BuildLinePath(aElement, aPath, aContext)
  else if aElement is TSVGPolylineElement then
    Result := BuildPolyPath(aElement, aPath, False)
  else if aElement is TSVGPolygonElement then
    Result := BuildPolyPath(aElement, aPath, True)
  else if aElement is TSVGPathElement then
    Result := aPath.TryParse(aElement.Attributes['d'])
  else
    Result := False;
end;


{ TSVGLengthReadHelper }

function TSVGLengthReadHelper.TryParse(const aText: TSVGString): Boolean;

var
  lScanner: TSVGScanner;
  lValue: Double;
  lSuffix: String;
  U: TSVGLengthUnit;

begin
  lScanner := TSVGScanner.Create(aText);
  lScanner.SkipWSP;
  if not lScanner.ScanNumber(lValue) then
    Exit(False);
  lSuffix := Trim(Copy(aText, lScanner.Pos, Length(aText)));
  for U := Low(TSVGLengthUnit) to High(TSVGLengthUnit) do
    if SameText(lSuffix, UnitNames[U]) then
      begin
      Self := TSVGLength.Create(lValue, U);
      Exit(True);
      end;
  Result := False;
end;


procedure TSVGLengthReadHelper.ParseDef(const aText: TSVGString);

var
  lParsed: TSVGLength;

begin
  if lParsed.TryParse(aText) then
    Self := lParsed;
end;


procedure TSVGLengthReadHelper.ReadAttribute(aElement: TSVGElement;
  const aName: TSVGString);

begin
  if (aElement <> nil) and aElement.HasAttribute(aName) then
    ParseDef(aElement.Attributes[aName]);
end;


{ TSVGMatrixReadHelper }

function ScanTransformArguments(var aScanner: TSVGScanner;
  out aValues: TSVGDoubleArray): Boolean;

var
  lCount: Integer;
  lValue: Double;

begin
  aValues := nil;
  aScanner.SkipWSP;
  if not aScanner.SkipChar('(') then
    Exit(False);
  SetLength(aValues, 6);
  lCount := 0;
  aScanner.SkipWSPComma;
  while not aScanner.AtEnd and (aScanner.Current <> ')') do
    begin
    if not aScanner.ScanNumber(lValue) then
      begin
      aValues := nil;
      Exit(False);
      end;
    if lCount = Length(aValues) then
      SetLength(aValues, lCount * 2);
    aValues[lCount] := lValue;
    Inc(lCount);
    aScanner.SkipWSPComma;
    end;
  if not aScanner.SkipChar(')') then
    begin
    aValues := nil;
    Exit(False);
    end;
  SetLength(aValues, lCount);
  Result := True;
end;


function BuildTransform(const aName: TSVGString; const aValues: TSVGDoubleArray;
  out aMatrix: TSVGMatrix): Boolean;

begin
  aMatrix := TSVGMatrix.Identity;
  Result := True;
  if aName = 'matrix' then
    begin
    if Length(aValues) <> 6 then
      Exit(False);
    aMatrix := TSVGMatrix.Create(aValues[0], aValues[1], aValues[2],
      aValues[3], aValues[4], aValues[5]);
    end
  else if aName = 'translate' then
    case Length(aValues) of
      1: aMatrix := TSVGMatrix.Translation(aValues[0], 0);
      2: aMatrix := TSVGMatrix.Translation(aValues[0], aValues[1]);
    else
      Exit(False);
    end
  else if aName = 'scale' then
    case Length(aValues) of
      1: aMatrix := TSVGMatrix.Scaling(aValues[0], aValues[0]);
      2: aMatrix := TSVGMatrix.Scaling(aValues[0], aValues[1]);
    else
      Exit(False);
    end
  else if aName = 'rotate' then
    case Length(aValues) of
      1: aMatrix := TSVGMatrix.Rotation(aValues[0]);
      3: aMatrix := TSVGMatrix.RotationAbout(aValues[0], aValues[1], aValues[2]);
    else
      Exit(False);
    end
  else if aName = 'skewX' then
    begin
    if Length(aValues) <> 1 then
      Exit(False);
    aMatrix := TSVGMatrix.SkewingX(aValues[0]);
    end
  else if aName = 'skewY' then
    begin
    if Length(aValues) <> 1 then
      Exit(False);
    aMatrix := TSVGMatrix.SkewingY(aValues[0]);
    end
  else
    Result := False;
end;


function TSVGMatrixReadHelper.TryParse(const aText: TSVGString): Boolean;

var
  lScanner: TSVGScanner;
  lName: String;
  lValues: TSVGDoubleArray;
  lTerm, lAccumulated: TSVGMatrix;

begin
  lAccumulated := TSVGMatrix.Identity;
  lScanner := TSVGScanner.Create(aText);
  lScanner.SkipWSPComma;
  while not lScanner.AtEnd do
    begin
    lName := lScanner.ScanName;
    if lName = '' then
      Exit(False);
    if not ScanTransformArguments(lScanner, lValues) then
      Exit(False);
    if not BuildTransform(lName, lValues, lTerm) then
      Exit(False);
    lAccumulated := lTerm.Compose(lAccumulated);
    lScanner.SkipWSPComma;
    end;
  Self := lAccumulated;
  Result := True;
end;


procedure TSVGMatrixReadHelper.ReadAttribute(aElement: TSVGElement);

begin
  ReadAttributeNamed(aElement, 'transform');
end;


procedure TSVGMatrixReadHelper.ReadAttributeNamed(aElement: TSVGElement;
  const aName: TSVGString);

begin
  if (aElement = nil) or not TryParse(aElement.Attributes[aName]) then
    Self := TSVGMatrix.Identity;
end;


{ TSVGRectReadHelper }

function TSVGRectReadHelper.TryParseViewBox(const aText: TSVGString): Boolean;

var
  lValues: TSVGDoubleArray;

begin
  Result := TryStrToSVGNumberList(aText, lValues) and (Length(lValues) = 4)
        and (lValues[2] >= 0) and (lValues[3] >= 0);
  if Result then
    Self := TSVGRect.CreateSize(lValues[0], lValues[1], lValues[2], lValues[3])
  else
    Self := TSVGRect.Empty;
end;


function TSVGRectReadHelper.ReadViewBoxAttribute(aElement: TSVGElement): Boolean;

begin
  Self := TSVGRect.Empty;
  Result := (aElement <> nil) and aElement.HasAttribute('viewBox')
        and TryParseViewBox(aElement.Attributes['viewBox']);
end;


{ TSVGPreserveAspectRatioReadHelper }

function TSVGPreserveAspectRatioReadHelper.TryParse(const aText: TSVGString): Boolean;

var
  lParts: TStringList;
  A: TSVGAspectAlign;
  I: Integer;

begin
  Self := TSVGPreserveAspectRatio.Default;
  lParts := TStringList.Create;
  try
    lParts.Delimiter := ' ';
    lParts.StrictDelimiter := False;
    lParts.DelimitedText := Trim(aText);
    I := 0;
    if (lParts.Count > 0) and SameText(lParts[0], 'defer') then
      Inc(I);
    if I >= lParts.Count then
      Exit(False);
    Result := False;
    for A := Low(TSVGAspectAlign) to High(TSVGAspectAlign) do
      if lParts[I] = AlignNames[A] then
        begin
        Align := A;
        Result := True;
        Break;
        end;
    if not Result then
      Exit;
    Inc(I);
    if I < lParts.Count then
      begin
      if lParts[I] = 'meet' then
        MeetOrSlice := msMeet
      else if lParts[I] = 'slice' then
        MeetOrSlice := msSlice
      else
        Exit(False);
      Inc(I);
      end;
    Result := I >= lParts.Count;
  finally
    lParts.Free;
  end;
end;


procedure TSVGPreserveAspectRatioReadHelper.ReadAttribute(aElement: TSVGElement);

begin
  if (aElement = nil)
     or not TryParse(aElement.Attributes['preserveAspectRatio']) then
    Self := TSVGPreserveAspectRatio.Default;
end;


{ TSVGReader }

procedure TSVGReader.CopyAttributes(aSource: TDOMElement; aTarget: TSVGElement);

var
  I: Integer;
  lAttr: TDOMNode;
  lName: String;

begin
  if aSource.Attributes = nil then
    Exit;
  for I := 0 to aSource.Attributes.Length - 1 do
    begin
    lAttr := aSource.Attributes[I];
    lName := UTF8Encode(lAttr.NodeName);
    if (lName = 'xmlns') or (Copy(lName, 1, 6) = 'xmlns:') then
      Continue;
    if UTF8Encode(lAttr.NamespaceURI) = XLinkNamespace then
      lName := UTF8Encode(lAttr.LocalName);
    aTarget.Attributes[lName] := UTF8Encode(lAttr.NodeValue);
    end;
end;


function TSVGReader.BuildElement(aSource: TDOMElement): TSVGElement;

var
  lNamespace, lLocalName: String;

begin
  lNamespace := UTF8Encode(aSource.NamespaceURI);
  lLocalName := UTF8Encode(aSource.LocalName);
  if lLocalName = '' then
    lLocalName := UTF8Encode(aSource.NodeName);
  if (lNamespace <> '') and (lNamespace <> SVGNamespace) then
    begin
    if not (roKeepForeign in FOptions) then
      Exit(nil);
    Result := TSVGForeignElement.Create(lLocalName);
    TSVGForeignElement(Result).Namespace := lNamespace;
    end
  else
    Result := CreateSVGElement(lLocalName);
  CopyAttributes(aSource, Result);
end;


procedure TSVGReader.BuildChildren(aSource: TDOMElement; aTarget: TSVGElement);

var
  lChild: TDOMNode;
  lElement: TSVGElement;
  lText: String;

begin
  lChild := aSource.FirstChild;
  while lChild <> nil do
    begin
    if lChild.NodeType = ELEMENT_NODE then
      begin
      lElement := BuildElement(TDOMElement(lChild));
      if lElement <> nil then
        begin
        aTarget.AppendChild(lElement);
        BuildChildren(TDOMElement(lChild), lElement);
        end;
      end
    else if lChild.NodeType in [TEXT_NODE, CDATA_SECTION_NODE] then
      begin
      lText := UTF8Encode(lChild.NodeValue);
      // Inside text, a run of spaces separates words. It is kept even when
      // there is nothing else between two elements.
      if (roPreserveSpace in FOptions) or (Trim(lText) <> '')
         or IsSVGTextContentElement(aTarget) then
        aTarget.AppendChild(TSVGTextNode.Create(lText));
      end;
    lChild := lChild.NextSibling;
    end;
end;


function TSVGReader.ReadFromXML(aXML: TXMLDocument): TSVGDocument;

var
  lRoot: TSVGElement;

begin
  if aXML = nil then
    raise ESVGRead.Create(SErrNoXMLDocument);
  if aXML.DocumentElement = nil then
    raise ESVGRead.Create(SErrNoRootElement);
  lRoot := BuildElement(aXML.DocumentElement);
  if lRoot = nil then
    raise ESVGRead.Create(SErrRootNotSVG);
  Result := TSVGDocument.Create;
  try
    Result.Root := lRoot;
    BuildChildren(aXML.DocumentElement, Result.Root);
    SVGCollectStyleSheetHRefs(aXML, Result);
  except
    Result.Free;
    raise;
  end;
end;


// The bytes a gzip member has, or the source unchanged when it is not gzip or cannot be inflated.
// RFC 1952 puts a fixed header of ten bytes in front of the deflate data, followed by whatever the flags indicate.
function SVGInflated(const aRaw: RawByteString): RawByteString;

const
  HasExtra = $04;
  HasName = $08;
  HasComment = $10;
  HasCRC = $02;

var
  lFlags: Byte;
  lAt, lRead: Integer;
  lSource: TMemoryStream;
  lPlain: TMemoryStream;
  lZip: Tdecompressionstream;
  lBuffer: array[0..8191] of Byte;

  procedure SkipTerminated;
  begin
    while (lAt <= Length(aRaw)) and (aRaw[lAt] <> #0) do
      Inc(lAt);
    Inc(lAt);
  end;

begin
  Result := aRaw;
  if (Length(aRaw) < 18) or (aRaw[1] <> #$1F) or (aRaw[2] <> #$8B)
     or (aRaw[3] <> #8) then
    Exit;
  lFlags := Ord(aRaw[4]);
  lAt := 11;
  if (lFlags and HasExtra) <> 0 then
    lAt := lAt + 2 + Ord(aRaw[lAt]) + Ord(aRaw[lAt + 1]) * 256;
  if (lFlags and HasName) <> 0 then
    SkipTerminated;
  if (lFlags and HasComment) <> 0 then
    SkipTerminated;
  if (lFlags and HasCRC) <> 0 then
    lAt := lAt + 2;
  if lAt > Length(aRaw) then
    Exit;
  lSource := nil;
  lPlain := nil;
  lZip := nil;
  try
    try
      lSource := TMemoryStream.Create;
      TMemoryStream(lSource).Write(aRaw[lAt], Length(aRaw) - lAt + 1);
      lSource.Position := 0;
      lPlain := TMemoryStream.Create;
      // The header is removed already, leaving the deflate data and the
      // checksum after it.
      lZip := Tdecompressionstream.Create(lSource, True);
      repeat
        lRead := lZip.Read(lBuffer, SizeOf(lBuffer));
        if lRead > 0 then
          lPlain.Write(lBuffer, lRead);
      until lRead <= 0;
      SetLength(Result, lPlain.Size);
      if lPlain.Size > 0 then
        Move(lPlain.Memory^, Result[1], lPlain.Size);
    except
      Result := aRaw;
    end;
  finally
    lZip.Free;
    lPlain.Free;
    lSource.Free;
  end;
end;


function SVGIsDataURI(const aHRef: String): Boolean;

begin
  Result := SameText(Copy(Trim(aHRef), 1, 5), 'data:');
end;


// The part a data URI declares before the comma: the media type and its
// parameters, lower case and without spaces.
function SVGDataURIHead(const aHRef: String): String;

var
  lAt: Integer;

begin
  Result := '';
  if not SVGIsDataURI(aHRef) then
    Exit;
  Result := Copy(Trim(aHRef), 6, MaxInt);
  lAt := Pos(',', Result);
  if lAt = 0 then
    Exit('');
  Result := LowerCase(StringReplace(Copy(Result, 1, lAt - 1), ' ', '',
    [rfReplaceAll]));
end;


function SVGDataURIHoldsSVG(const aHRef: String): Boolean;

var
  lHead: String;

begin
  lHead := SVGDataURIHead(aHRef);
  Result := (Pos('image/svg+xml', lHead) = 1)
    or (Pos('text/xml+svg', lHead) = 1);
end;


// The bytes base64 text stands for, empty when it decodes to nothing.
// The decoding runs over bytes: the call that takes a string would read
// them as text of the RTL and lose them.
function DecodedBase64(const aText: RawByteString): RawByteString;

var
  lSource, lPlain: TMemoryStream;
  lDecoder: TBase64DecodingStream;
  lBuffer: array[0..4095] of Byte;
  lRead: Integer;

begin
  Result := '';
  if aText = '' then
    Exit;
  lSource := TMemoryStream.Create;
  lPlain := nil;
  lDecoder := nil;
  try
    try
      lSource.Write(aText[1], Length(aText));
      lSource.Position := 0;
      lPlain := TMemoryStream.Create;
      lDecoder := TBase64DecodingStream.Create(lSource);
      repeat
        lRead := lDecoder.Read(lBuffer, SizeOf(lBuffer));
        if lRead > 0 then
          lPlain.Write(lBuffer, lRead);
      until lRead <= 0;
      SetLength(Result, lPlain.Size);
      if lPlain.Size > 0 then
        Move(lPlain.Memory^, Result[1], lPlain.Size);
    except
      Result := '';
    end;
  finally
    lDecoder.Free;
    lPlain.Free;
    lSource.Free;
  end;
end;


// The bytes of a data URI, decoded from base64, or from percent escapes
// when it is not base64.
function SVGDataURIBody(const aHRef: String): RawByteString;

var
  lText: RawByteString;
  lAt, I: Integer;
  lCode: Integer;

begin
  Result := '';
  lText := Trim(aHRef);
  lAt := Pos(',', lText);
  if lAt = 0 then
    Exit;
  Result := Copy(lText, lAt + 1, MaxInt);
  if Pos(';base64', SVGDataURIHead(aHRef)) > 0 then
    begin
    Result := StringReplace(Result, #13, '', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '', [rfReplaceAll]);
    Result := StringReplace(Result, ' ', '', [rfReplaceAll]);
    Result := StringReplace(Result, #9, '', [rfReplaceAll]);
    Result := DecodedBase64(Result);
    Exit;
    end;
  lText := Result;
  Result := '';
  I := 1;
  while I <= Length(lText) do
    begin
    if (lText[I] = '%') and (I + 2 <= Length(lText))
       and TryStrToInt('$' + Copy(lText, I + 1, 2), lCode) then
      begin
      Result := Result + Chr(lCode);
      Inc(I, 3);
      end
    else
      begin
      Result := Result + lText[I];
      Inc(I);
      end;
    end;
end;


function ReadSVGDataURI(const aHRef: String): TSVGDocument;

var
  lBody: RawByteString;

begin
  Result := nil;
  if not SVGDataURIHoldsSVG(aHRef) then
    Exit;
  lBody := SVGInflated(SVGDataURIBody(aHRef));
  if lBody = '' then
    Exit;
  try
    Result := ReadSVGString(lBody);
  except
    FreeAndNil(Result);
  end;
end;


function TSVGReader.ReadFromStream(aStream: TStream): TSVGDocument;

var
  lXML: TXMLDocument;
  lParser: TDOMParser;
  lSource: TXMLInputSource;
  lText: TMemoryStream;
  lRaw, lPlain: RawByteString;

begin
  lXML := nil;
  lSource := nil;
  lParser := nil;
  // The source is held as bytes from here on: it is text in whatever
  // encoding the document declares, which the parser reads for itself.
  lText := TMemoryStream.Create;
  try
    lText.CopyFrom(aStream, 0);
    SetLength(lRaw, lText.Size);
    if lText.Size > 0 then
      Move(lText.Memory^, lRaw[1], lText.Size);
    // A document written out with gzip, an svgz file, is inflated before
    // anything reads it.
    lPlain := SVGInflated(lRaw);
    if lPlain <> lRaw then
      begin
      lText.Size := 0;
      if lPlain <> '' then
        lText.Write(lPlain[1], Length(lPlain));
      end;
    if not (roExternalEntities in FOptions)
       and SVGDeclaresExternalEntity(lPlain) then
      raise ESVGRead.Create(SErrExternalEntity);
    lText.Position := 0;
    lParser := TDOMParser.Create;
    lParser.Options.Namespaces := True;
    // SVG collapses whitespace by its own rules, so it must arrive here
    // intact.
    lParser.Options.PreserveWhitespace := True;
    // An entity reference is markup and has no node in the tree.
    // Expanding it puts the elements in its place.
    lParser.Options.ExpandEntities := True;
    lSource := TXMLInputSource.Create(lText);
    lParser.Parse(lSource, lXML);
    Result := ReadFromXML(lXML);
  finally
    lXML.Free;
    lSource.Free;
    lParser.Free;
    lText.Free;
  end;
end;


function TSVGReader.ReadFromFile(const aFileName: String): TSVGDocument;

var
  lStream: TFileStream;

begin
  lStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := ReadFromStream(lStream);
    Result.BaseURI := aFileName;
  finally
    lStream.Free;
  end;
end;


function TSVGReader.ReadFromString(const aText: RawByteString): TSVGDocument;

var
  lStream: TMemoryStream;

begin
  lStream := TMemoryStream.Create;
  try
    if aText <> '' then
      lStream.Write(aText[1], Length(aText));
    lStream.Position := 0;
    Result := ReadFromStream(lStream);
  finally
    lStream.Free;
  end;
end;


function ReadSVGFile(const aFileName: String): TSVGDocument;

var
  lReader: TSVGReader;

begin
  lReader := TSVGReader.Create;
  try
    Result := lReader.ReadFromFile(aFileName);
  finally
    lReader.Free;
  end;
end;


function ReadSVGStream(aStream: TStream): TSVGDocument;

var
  lReader: TSVGReader;

begin
  lReader := TSVGReader.Create;
  try
    Result := lReader.ReadFromStream(aStream);
  finally
    lReader.Free;
  end;
end;


function ReadSVGString(const aText: RawByteString): TSVGDocument;

var
  lReader: TSVGReader;

begin
  lReader := TSVGReader.Create;
  try
    Result := lReader.ReadFromString(aText);
  finally
    lReader.Free;
  end;
end;


function SVGViewOf(aElement: TSVGElement): TSVGView;

begin
  Result := TSVGView.None;
  if aElement = nil then
    Exit;
  Result.Name := aElement.ID;
  Result.HasViewBox := Result.ViewBox.ReadViewBoxAttribute(aElement)
                   and not Result.ViewBox.IsEmpty;
  Result.HasRatio := aElement.HasAttribute('preserveAspectRatio');
  if Result.HasRatio then
    Result.Ratio.ReadAttribute(aElement);
end;


// Appends the views of a subtree, in document order.
procedure CollectViews(aElement: TSVGElement; var aViews: TSVGViewArray);

var
  I: Integer;

begin
  if aElement is TSVGViewElement then
    begin
    SetLength(aViews, Length(aViews) + 1);
    aViews[High(aViews)] := SVGViewOf(aElement);
    end;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      CollectViews(TSVGElement(aElement[I]), aViews);
end;


function SVGViewsOf(aDocument: TSVGDocument): TSVGViewArray;

begin
  Result := nil;
  if (aDocument = nil) or (aDocument.Root = nil) then
    Exit;
  CollectViews(aDocument.Root, Result);
end;


function SVGFindView(aDocument: TSVGDocument; const aName: String;
  out aView: TSVGView): Boolean;

var
  lElement: TSVGElement;

begin
  aView := TSVGView.None;
  Result := False;
  if (aDocument = nil) or (Trim(aName) = '') then
    Exit;
  lElement := aDocument.ElementByID(Trim(aName));
  // Only a view element changes the frame. A fragment identifier that
  // refers to any other element leaves the drawing unchanged.
  Result := lElement is TSVGViewElement;
  if Result then
    aView := SVGViewOf(lElement);
end;


// The text of a URL fragment with its percent escapes decoded. In
// svgView(viewBox(...)%3Btransform(...)) the escape is the semicolon
// between the two parts.
function UnescapeFragment(const aText: String): String;

var
  I, lValue, lCode: Integer;

begin
  Result := '';
  I := 1;
  while I <= Length(aText) do
    begin
    if (aText[I] = '%') and (I + 2 <= Length(aText)) then
      begin
      Val('$' + Copy(aText, I + 1, 2), lValue, lCode);
      if lCode = 0 then
        begin
        Result := Result + AnsiChar(lValue);
        Inc(I, 3);
        Continue;
        end;
      end;
    Result := Result + aText[I];
    Inc(I);
    end;
end;


// The text between the brackets of name(...) inside an svgView list, and
// an empty string when the list holds no such part.
function ViewSpecPart(const aSpec, aName: String): String;

var
  lAt, lStop, lDepth: Integer;

begin
  Result := '';
  lAt := Pos(LowerCase(aName) + '(', LowerCase(aSpec));
  if lAt = 0 then
    Exit;
  lAt := lAt + Length(aName) + 1;
  lDepth := 1;
  lStop := lAt;
  while (lStop <= Length(aSpec)) and (lDepth > 0) do
    begin
    if aSpec[lStop] = '(' then
      Inc(lDepth)
    else if aSpec[lStop] = ')' then
      Dec(lDepth);
    if lDepth > 0 then
      Inc(lStop);
    end;
  if lDepth <> 0 then
    Exit;
  Result := Trim(Copy(aSpec, lAt, lStop - lAt));
end;


function SVGViewOfFragment(aDocument: TSVGDocument; const aFragment: String;
  out aView: TSVGView): Boolean;

var
  lText, lPart: String;
  lNumbers: TSVGDoubleArray;
  lMatrix: TSVGMatrix;

begin
  aView := TSVGView.None;
  Result := False;
  lText := Trim(aFragment);
  if (lText <> '') and (lText[1] = '#') then
    lText := Copy(lText, 2, Length(lText));
  lText := Trim(UnescapeFragment(lText));
  if lText = '' then
    Exit;
  if not SameText(Copy(lText, 1, 8), 'svgView(') then
    Exit(SVGFindView(aDocument, lText, aView));
  lText := Copy(lText, 9, Length(lText));
  if (lText <> '') and (lText[Length(lText)] = ')') then
    lText := Copy(lText, 1, Length(lText) - 1);
  lPart := ViewSpecPart(lText, 'viewBox');
  if (lPart <> '') and TryStrToSVGNumberList(lPart, lNumbers)
     and (Length(lNumbers) = 4) and (lNumbers[2] > 0) and (lNumbers[3] > 0) then
    begin
    aView.ViewBox := TSVGRect.CreateSize(lNumbers[0], lNumbers[1],
      lNumbers[2], lNumbers[3]);
    aView.HasViewBox := True;
    end;
  lPart := ViewSpecPart(lText, 'preserveAspectRatio');
  if lPart <> '' then
    aView.HasRatio := aView.Ratio.TryParse(lPart);
  lPart := ViewSpecPart(lText, 'transform');
  if (lPart <> '') and lMatrix.TryParse(lPart) then
    begin
    aView.Transform := lMatrix;
    aView.HasTransform := True;
    end;
  Result := aView.HasViewBox or aView.HasRatio or aView.HasTransform;
end;


end.
