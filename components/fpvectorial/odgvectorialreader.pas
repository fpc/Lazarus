{
Reads an ODG Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

An OpenDocument document is a compressed ZIP file with the following files inside:

content.xml     - Actual contents
meta.xml        - Authoring data
settings.xml    - User persistent viewing information, such as zoom, cursor position, etc.
styles.xml      - Styles, which are the only way to do formatting
mimetype        - application/vnd.oasis.opendocument.spreadsheet
META-INF\manifest.xml  - Describes the other files in the archive

Specifications obtained from:

http://docs.oasis-open.org/office/v1.1/OS/OpenDocument-v1.1.pdf

AUTHORS: Felipe Monteiro de Carvalho
}
unit odgvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  zipper, {NOTE: fpszipper is the latest zipper.pp Change to standard zipper when FPC 2.8 is released}
  xmlread, DOM, AVL_Tree,
  fpimage, fpcanvas, fgl,
  fpvectorial, fpvutils, lazutf8;

type
{  TSVGTokenType = (
    // moves
    sttMoveTo, sttRelativeMoveTo,
    // Close Path
    sttClosePath,
    // lines
    sttLineTo, sttRelativeLineTo,
    sttHorzLineTo, sttRelativeHorzLineTo, sttVertLineTo, sttRelativeVertLineTo,
    // cubic beziers
    sttBezierTo, sttRelativeBezierTo,
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
  end;      }

  { TvODGVectorialReader }

  TvODGVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    //FSVGPathTokenizer: TSVGPathTokenizer;
    function GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
    procedure ConvertODGCoordinatesToFPVCoordinates(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double);
    procedure ConvertODGDeltaToFPVDelta(
      const AData: TvVectorialPage;
      const ASrcX, ASrcY: Double; var ADestX, ADestY: Double);
  public
    { General reading methods }
    constructor Create; override;
    Destructor Destroy; override;
    //procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); override;
  end;

implementation

const
  { OpenDocument general XML constants }
  XML_HEADER           = '<?xml version="1.0" encoding="utf-8" ?>';

  { OpenDocument Directory structure constants }
  OPENDOC_PATH_CONTENT   = 'content.xml';
  OPENDOC_PATH_META      = 'meta.xml';
  OPENDOC_PATH_SETTINGS  = 'settings.xml';
  OPENDOC_PATH_STYLES    = 'styles.xml';
  OPENDOC_PATH_MIMETYPE  = 'mimetype';
  OPENDOC_PATH_METAINF = 'META-INF' + '/';
  OPENDOC_PATH_METAINF_MANIFEST = 'META-INF' + '/' + 'manifest.xml';

  { OpenDocument schemas constants }
  SCHEMAS_XMLNS_OFFICE   = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
  SCHEMAS_XMLNS_DCTERMS  = 'http://purl.org/dc/terms/';
  SCHEMAS_XMLNS_META     = 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
  SCHEMAS_XMLNS          = 'http://schemas.openxmlformats.org/officeDocument/2006/extended-properties';
  SCHEMAS_XMLNS_CONFIG   = 'urn:oasis:names:tc:opendocument:xmlns:config:1.0';
  SCHEMAS_XMLNS_OOO      = 'http://openoffice.org/2004/office';
  SCHEMAS_XMLNS_MANIFEST = 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
  SCHEMAS_XMLNS_FO       = 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0';
  SCHEMAS_XMLNS_STYLE    = 'urn:oasis:names:tc:opendocument:xmlns:style:1.0';
  SCHEMAS_XMLNS_SVG      = 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0';
  SCHEMAS_XMLNS_TABLE    = 'urn:oasis:names:tc:opendocument:xmlns:table:1.0';
  SCHEMAS_XMLNS_TEXT     = 'urn:oasis:names:tc:opendocument:xmlns:text:1.0';
  SCHEMAS_XMLNS_V        = 'urn:schemas-microsoft-com:vml';
  SCHEMAS_XMLNS_NUMBER   = 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0';
  SCHEMAS_XMLNS_CHART    = 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0';
  SCHEMAS_XMLNS_DR3D     = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  SCHEMAS_XMLNS_MATH     = 'http://www.w3.org/1998/Math/MathML';
  SCHEMAS_XMLNS_FORM     = 'urn:oasis:names:tc:opendocument:xmlns:form:1.0';
  SCHEMAS_XMLNS_SCRIPT   = 'urn:oasis:names:tc:opendocument:xmlns:script:1.0';
  SCHEMAS_XMLNS_OOOW     = 'http://openoffice.org/2004/writer';
  SCHEMAS_XMLNS_OOOC     = 'http://openoffice.org/2004/calc';
  SCHEMAS_XMLNS_DOM      = 'http://www.w3.org/2001/xml-events';
  SCHEMAS_XMLNS_XFORMS   = 'http://www.w3.org/2002/xforms';
  SCHEMAS_XMLNS_XSD      = 'http://www.w3.org/2001/XMLSchema';
  SCHEMAS_XMLNS_XSI      = 'http://www.w3.org/2001/XMLSchema-instance';

{ TSVGPathTokenizer }

{constructor TSVGPathTokenizer.Create;
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
end;}

function TvODGVectorialReader.GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
var
  i : integer;
  Found : Boolean;
begin
  Found:=false;
  i:=0;
  Result:='';
  while not Found and (i<ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName=AAttrName then begin
      Found:=true;
      Result:=ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;

procedure TvODGVectorialReader.ConvertODGCoordinatesToFPVCoordinates(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double;
  var ADestX,ADestY: Double);
begin
//  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
//  ADestY := AData.Height - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
end;

procedure TvODGVectorialReader.ConvertODGDeltaToFPVDelta(
  const AData: TvVectorialPage; const ASrcX, ASrcY: Double; var ADestX,
  ADestY: Double);
begin
//  ADestX := ASrcX * FLOAT_MILIMETERS_PER_PIXEL;
//  ADestY := - ASrcY * FLOAT_MILIMETERS_PER_PIXEL;
end;

constructor TvODGVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator

//  FSVGPathTokenizer := TSVGPathTokenizer.Create;
end;

destructor TvODGVectorialReader.Destroy;
begin
//  FSVGPathTokenizer.Free;

  inherited Destroy;
end;

{procedure TvODGVectorialReader.ReadFromStream(AStream: TStream;
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
end;}

procedure TvODGVectorialReader.ReadFromFile(AFileName: string; AData: TvVectorialDocument);
var
  Col, Row : integer;
  FilePath : string;
  UnZip : TUnZipper;
  FileList : TStringList;
  Doc : TXMLDocument;
  BodyNode, SpreadSheetNode, TableNode, RowNode, CellNode : TDOMNode;
  ParamRowsRepeated, ParamColsRepeated, ParamValueType, ParamFormula : string;
  RowsCount, ColsCount : integer;
begin
{  //unzip content.xml into AFileName path
  FilePath:=GetTempDir(false);
  UnZip:=TUnZipper.Create;
  UnZip.OutputPath:=FilePath;
  FileList:=TStringList.Create;
  FileList.Add('content.xml');
  try
    Unzip.UnZipFiles(AFileName,FileList);
  finally
    FreeAndNil(FileList);
    FreeAndNil(UnZip);
  end; //try

  Doc:=nil;
  try
    //process the xml file
    ReadXMLFile(Doc,FilePath+'content.xml');
    DeleteFile(FilePath+'content.xml');

    BodyNode:= Doc.DocumentElement.FindNode('office:body');
    if not Assigned(BodyNode) then Exit;

    SpreadSheetNode:=BodyNode.FindNode('office:spreadsheet');
    if not Assigned(SpreadSheetNode) then Exit;

    //process each table (sheet)
    TableNode:=SpreadSheetNode.FindNode('table:table');
    while Assigned(TableNode) do
    begin
      FWorkSheet:=aData.AddWorksheet(GetAttrValue(TableNode,'table:name'));
      Row:=0;

      //process each row inside the sheet
      RowNode:=TableNode.FindNode('table:table-row');
      while Assigned(RowNode) do begin

        Col:=0;

        ParamRowsRepeated:=GetAttrValue(RowNode,'table:number-rows-repeated');
        if ParamRowsRepeated='' then ParamRowsRepeated:='1';

        //process each cell of the row
        CellNode:=RowNode.FindNode('table:table-cell');
        while Assigned(CellNode) do
        begin
          ParamColsRepeated:=GetAttrValue(CellNode,'table:number-columns-repeated');
          if ParamColsRepeated='' then ParamColsRepeated:='1';

          //select this cell value's type
          ParamValueType:=GetAttrValue(CellNode,'office:value-type');
          ParamFormula:=GetAttrValue(CellNode,'table:formula');
          for RowsCount:=0 to StrToInt(ParamRowsRepeated)-1 do begin
            for ColsCount:=0 to StrToInt(ParamColsRepeated)-1 do begin
              if ParamValueType='string' then
                ReadLabel(Row+RowsCount,Col+ColsCount,CellNode)
              else if ParamFormula<>'' then
                ReadFormula(Row+RowsCount,Col+ColsCount,CellNode)
              else if ParamValueType='float' then
                ReadNumber(Row+RowsCount,Col+ColsCount,CellNode)
              else if ParamValueType='date' then
                ReadDate(Row+RowsCount,Col+ColsCount,CellNode);
            end; //for ColsCount
          end; //for RowsCount

          Inc(Col,ColsCount+1);
          CellNode:=CellNode.NextSibling;
        end; //while Assigned(CellNode)

        Inc(Row,RowsCount+1);
        RowNode:=RowNode.NextSibling;
      end; // while Assigned(RowNode)

      TableNode:=TableNode.NextSibling;
    end; //while Assigned(TableNode)
  finally
    Doc.Free;
  end;}
end;

initialization

  RegisterVectorialReader(TvODGVectorialReader, vfODG);

end.

