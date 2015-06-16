{
Reads a HTML Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit htmlvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, contnrs,
  fpimage, fpcanvas, laz2_xmlread, laz2_dom, fgl,
  // image data formats
  fpreadpng,
  // HTML can contain SVG
  svgvectorialreader,
  fpvectorial, fpvutils, lazutf8, TypInfo;

type
  { TvHTMLVectorialReader }

  TvHTMLVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    //
    function GetTextContentFromNode(ANode: TDOMNode): string;
    //
    function ReadEntityFromNode(ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
    function ReadHeaderFromNode(ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
    procedure ReadParagraphFromNode(ADest: TvParagraph; ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument);
    function ReadSVGFromNode(ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
    function ReadMathFromNode(ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
    function ReadTableFromNode(ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
    function ReadTableRowNode(ATable: TvTable; ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
    function ReadUListFromNode(ANode: TDOMNode; AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
  public
    { General reading methods }
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
    procedure ReadFromXML(Doc: TXMLDocument; AData: TvVectorialDocument);
    class function IsSupportedRasterImage(AFileName: string): Boolean;
  end;

implementation

const
  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 5*0.2822; // DPI 90 = 1 / 90 inches per pixel => Actually I changed the value by this factor! Because otherwise it looks ugly!
  FLOAT_PIXELS_PER_MILIMETER = 1 / FLOAT_MILIMETERS_PER_PIXEL; // DPI 90 = 1 / 90 inches per pixel

{ TvHTMLVectorialReader }

function TvHTMLVectorialReader.GetTextContentFromNode(ANode: TDOMNode): string;
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

// if something is returned, it will be added to the base document
// if nothing is returned, either nothing was written, or it was already added
function TvHTMLVectorialReader.ReadEntityFromNode(ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  lEntityName: DOMString;
  lPara: TvParagraph;
begin
  Result := nil;
  lEntityName := LowerCase(ANode.NodeName);
  case lEntityName of
    'h1', 'h2', 'h3', 'h4', 'h5', 'h6': Result := ReadHeaderFromNode(ANode, AData, ADoc);
    'p':
    begin
      lPara := AData.AddParagraph();
      ReadParagraphFromNode(lPara, ANode, AData, ADoc);
      Result := nil;
    end;
    'svg': Result := ReadSVGFromNode(ANode, AData, ADoc);
    'math': Result := ReadMathFromNode(ANode, AData, ADoc);
    'table': Result := ReadTableFromNode(ANode, AData, ADoc);
    'br':
    begin
      AData.AddParagraph().AddText(LineEnding);
      Result := nil;
    end;
    'ul': Result := ReadUListFromNode(ANode, AData, ADoc);
  end;
end;

function TvHTMLVectorialReader.ReadHeaderFromNode(ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  CurParagraph: TvParagraph;
  lText: TvText;
  lTextStr: string;
  lHeaderType: DOMString;
begin
  Result := nil;
  CurParagraph := AData.AddParagraph();
  CurParagraph.Style := ADoc.StyleTextBody;
  lTextStr := ANode.FirstChild.NodeValue;
  lText := CurParagraph.AddText(lTextStr);
  lHeaderType := LowerCase(ANode.NodeName);
  case lHeaderType of
    'h1': lText.Style := ADoc.StyleHeading1;
    'h2': lText.Style := ADoc.StyleHeading2;
    'h3': lText.Style := ADoc.StyleHeading3;
    'h4': lText.Style := ADoc.StyleHeading4;
    'h5': lText.Style := ADoc.StyleHeading5;
    'h6': lText.Style := ADoc.StyleHeading6;
  end;
end;

procedure TvHTMLVectorialReader.ReadParagraphFromNode(ADest: TvParagraph; ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument);
var
  lText: TvText = nil;
  lTextStr: string = '';
  lCurNode: TDOMNode;
  lNodeName, lNodeValue, lAttrName, lAttrValue: DOMString;
  lCurAttr: TDOMNode;
  lRasterImage: TvRasterImage;
  lEmbVecImg: TvEmbeddedVectorialDoc = nil;
  i: Integer;
  lWidth, lHeight: Double;
  lAltText: string;
  // xlink:href
  lx, ly, lw, lh: Double;
  lImageDataParts: TStringList;
  lImageDataBase64: string;
  lImageData: array of Byte;
  lImageDataStream: TMemoryStream;
  lImageReader: TFPCustomImageReader;

  procedure TextMerging();
  begin
    if lTextStr <> '' then
    begin
      if lText = nil then
        lText := ADest.AddText(lTextStr)
      else
        lText.Value.Add(lTextStr);
      lTextStr := '';
    end;
  end;

begin
  ADest.Style := ADoc.StyleTextBody;

  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lNodeName := LowerCase(lCurNode.NodeName);
    lNodeValue := lCurNode.NodeValue;

    if (lCurNode is TDOMText) then
    begin
      lTextStr += RemoveLineEndingsAndTrim(lNodeValue);
      lCurNode := lCurNode.NextSibling;
      Continue;
    end;

    // text merging
    TextMerging();
    // reset text merging
    if lNodeName <> 'br' then
      lText := nil;

    case lNodeName of
    // <image width="100" height="100" xlink:href="data:image/png;base64,UgAAA....QSK5CYII="/>
    // <img src="images/noimage.gif" width="100" height="100" alt="No image" />
    'img', 'image':
    begin
      lRasterImage := nil;
      lEmbVecImg := nil;
      lWidth := -1;
      lHeight := -1;
      lAltText := '';

      for i := 0 to lCurNode.Attributes.Length - 1 do
      begin
        lCurAttr := lCurNode.Attributes.Item[i];
        lAttrName := lCurAttr.NodeName;
        lAttrValue := lCurAttr.NodeValue;

        case lAttrName of
        'alt':
        begin
          lAltText := lAttrValue;
        end;
        'src':
        begin
          lAttrValue := lCurAttr.NodeValue;
          lAttrValue := ExtractFilePath(FFilename) + lAttrValue;

          if TvHTMLVectorialReader.IsSupportedRasterImage(lAttrValue) then
          begin
            if not FileExists(lAttrValue) then Continue;

            lRasterImage := ADest.AddRasterImage();
            lRasterImage.CreateImageFromFile(lAttrValue);
          end
          else if TvVectorialDocument.GetFormatFromExtension(lAttrValue, False) <> vfUnknown then
          begin
            lEmbVecImg := ADest.AddEmbeddedVectorialDoc();
            lEmbVecImg.Document.ReadFromFile(lAttrValue);
          end;
        end;
        'xlink:href':
        begin
          lRasterImage := ADest.AddRasterImage();
          lImageDataParts := TvSVGVectorialReader.ReadSpaceSeparatedStrings(lNodeValue, ':;,');
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
                lRasterImage.CreateRGB888Image(10, 10);
                lRasterImage.RasterImage.LoadFromStream(lImageDataStream, lImageReader);
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
        end;
        'width':
        begin
          lWidth := StrToInt(lAttrValue);
        end;
        'height':
        begin
          lHeight := StrToInt(lAttrValue);
        end;
        end;
      end;

      if lRasterImage <> nil then
      begin
        if lWidth <= 0 then
          lWidth := lRasterImage.RasterImage.Width;
        if lHeight <= 0 then
          lHeight := lRasterImage.RasterImage.Height;

        lRasterImage.Width := lWidth;
        lRasterImage.Height := lHeight;
        lRasterImage.AltText := lAltText;
      end
      else if (lEmbVecImg <> nil) and (lWidth > 0) and (lHeight > 0) then
      begin
        lEmbVecImg.Width := lWidth;
        lEmbVecImg.Height := lHeight;
      end;
    end;
    end;

    lCurNode := lCurNode.NextSibling;
  end;

  TextMerging();
end;

function TvHTMLVectorialReader.ReadSVGFromNode(ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  CurSVG: TvEmbeddedVectorialDoc;
  lText: TvText;
  lDoc: TXMLDocument;
  lImportedNode: TDOMNode;
begin
  Result := nil;
  CurSVG := AData.AddEmbeddedVectorialDoc();
  lDoc := TXMLDocument.Create;
  try
    lImportedNode := lDoc.ImportNode(ANode, True);
    lDoc.AppendChild(lImportedNode);
    CurSVG.Document.ReadFromXML(lDoc, vfSVG);
  finally
    lDoc.Free;
  end;
end;

function TvHTMLVectorialReader.ReadMathFromNode(ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  CurSVG: TvEmbeddedVectorialDoc;
  lText: TvText;
  lDoc: TXMLDocument;
  lImportedNode: TDOMNode;
begin
  Result := nil;
  CurSVG := AData.AddEmbeddedVectorialDoc();
  lDoc := TXMLDocument.Create;
  try
    lImportedNode := lDoc.ImportNode(ANode, True);
    lDoc.AppendChild(lImportedNode);
    CurSVG.Document.ReadFromXML(lDoc, vfMathML);
  finally
    lDoc.Free;
  end;
end;

function TvHTMLVectorialReader.ReadTableFromNode(ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  CurTable: TvTable;
  lCurNode, lCurSubnode: TDOMNode;
  lNodeName, lNodeValue: DOMString;
  CurRow: TvTableRow;
  Caption_Cell: TvTableCell;
  CurCellPara: TvParagraph;
  // attributes
  i, lBorderNr: Integer;
  lAttrName, lAttrValue: DOMString;

  procedure SetBorderLineType(AType: TvTableBorderType);
  begin
    CurTable.Borders.Left.LineType := AType;
    CurTable.Borders.Right.LineType := AType;
    CurTable.Borders.Top.LineType := AType;
    CurTable.Borders.Bottom.LineType := AType;
    CurTable.Borders.InsideHoriz.LineType := AType;
    CurTable.Borders.InsideVert.LineType := AType;
  end;

begin
  Result := nil;
  CurTable := AData.AddTable();
  CurTable.CellSpacingLeft := 3;
  CurTable.CellSpacingTop := 2;

  // Default to no border without "border" attribute
  SetBorderLineType(tbtNone);

  // table attributes
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lAttrName := ANode.Attributes.Item[i].NodeName;
    lAttrValue := ANode.Attributes.Item[i].NodeValue;

    case lAttrName of
    'border':
    begin
      lBorderNr := StrToInt(lAttrValue);

      SetBorderLineType(tbtSingle);
      CurTable.Borders.Left.Width := lBorderNr;
      CurTable.Borders.Right.Width := lBorderNr;
      CurTable.Borders.Top.Width := lBorderNr;
      CurTable.Borders.Bottom.Width := lBorderNr;
      CurTable.Borders.InsideHoriz.Width := lBorderNr;
      CurTable.Borders.InsideVert.Width := lBorderNr;

      if lBorderNr = 0 then
        SetBorderLineType(tbtNone);
    end;
    end;
  end;

  // table child nodes
  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lNodeName := lCurNode.NodeName;
    lNodeValue := lCurNode.NodeValue;
    case lNodeName of
    'caption':
    begin
      CurRow := CurTable.AddRow();
      Caption_Cell := CurRow.AddCell();
      {Caption_Cell.Borders.Left.LineType := tbtNone;
      Caption_Cell.Borders.Top.LineType := tbtNone;
      Caption_Cell.Borders.Right.LineType := tbtNone;
      Caption_Cell.Borders.Bottom.LineType := tbtNone;}
      CurCellPara := Caption_Cell.AddParagraph();
      CurCellPara.Style := ADoc.StyleTextBodyCentralized;
      CurCellPara.AddText(GetTextContentFromNode(lCurNode));
    end;
    'tbody':
    begin
      lCurSubnode := lCurNode.FirstChild;
      while Assigned(lCurSubnode) do
      begin
        ReadTableRowNode(CurTable, lCurSubnode, AData, ADoc);

        lCurSubnode := lCurSubnode.NextSibling;
      end;
    end;
    end;

    lCurNode := lCurNode.NextSibling;
  end;

  // the caption spans all columns
  Caption_Cell.SpannedCols := CurTable.GetColCount();
end;

function TvHTMLVectorialReader.ReadTableRowNode(ATable: TvTable; ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  lCurNode: TDOMNode;
  lNodeName, lNodeValue: DOMString;
  CurRow: TvTableRow;
  CurCell: TvTableCell;
  CurCellPara: TvParagraph;
begin
  Result := nil;
  CurRow := ATable.AddRow();

  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lNodeName := lCurNode.NodeName;
    lNodeValue := lCurNode.NodeValue;
    case lNodeName of
    'th':
    begin
      CurCell := CurRow.AddCell();
      CurCellPara := CurCell.AddParagraph();
      CurCellPara.Style := ADoc.StyleTextBodyBold;
      CurCellPara.AddText(GetTextContentFromNode(lCurNode));
    end;
    'td':
    begin
      CurCell := CurRow.AddCell();

      CurCellPara := CurCell.AddParagraph();
      Self.ReadParagraphFromNode(CurCellPara, lCurNode, AData, ADoc);
    end;
    end;

    lCurNode := lCurNode.NextSibling;
  end;
end;

function TvHTMLVectorialReader.ReadUListFromNode(ANode: TDOMNode;
  AData: TvTextPageSequence; ADoc: TvVectorialDocument): TvEntity;
var
  lCurNode: TDOMNode;
  lNodeName, lNodeValue: DOMString;
  lNodeText: string;
  //
  lList: TvList;
  lCurPara: TvParagraph;
begin
  Result := nil;
  lList := AData.AddList();

  lCurNode := ANode.FirstChild;
  while Assigned(lCurNode) do
  begin
    lNodeName := lCurNode.NodeName;
    lNodeValue := lCurNode.NodeValue;
    case lNodeName of
    'li':
    begin
      lNodeText := GetTextContentFromNode(lCurNode);
      lCurPara := lList.AddParagraph(lNodeText);
    end;
    end;

    lCurNode := lCurNode.NextSibling;
  end;
end;

constructor TvHTMLVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
end;

destructor TvHTMLVectorialReader.Destroy;
begin
  inherited Destroy;
end;

procedure TvHTMLVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument = nil;
  lStream: TMemoryStream;
  lTmp: String;
begin
  lStream := TMemoryStream.Create();
  try
    // Remove the <!DOCTYPE line
    if Pos('<!DOCTYPE', AStrings.Strings[0]) <> 0 then
      AStrings.Delete(0);
    // Create a header
    AStrings.Insert(0, '<?xml version="1.0"?>');
    lTmp := AStrings.Text;
    // Flush it back to a stream
    AStrings.SaveToStream(lStream);
    lStream.Position := 0;
    // HTML is not XML, but might be compatible enough... a dedicated reader will be complex, but eventually necessary
    ReadXMLFile(Doc, lStream);
    lStream.Free; // Release as soon as unnecessary
    lStream := nil;
    //
    ReadFromXML(Doc, AData);
  finally
    Doc.Free;
    lStream.Free;
  end;
end;

procedure TvHTMLVectorialReader.ReadFromXML(Doc: TXMLDocument;
  AData: TvVectorialDocument);
var
  lCurNode, lCurSubnode: TDOMNode;
  lPage: TvTextPageSequence;
  lNodeName, lNodeValue: DOMString;
  ANode: TDOMElement;
  i: Integer;
  lCurEntity: TvEntity;
begin
  {ANode := Doc.DocumentElement;
  for i := 0 to ANode.Attributes.Length - 1 do
  begin
    lNodeName := ANode.Attributes.Item[i].NodeName;
    lNodeValue := ANode.Attributes.Item[i].NodeValue;
  end;}

  AData.AddStandardTextDocumentStyles(vfHTML);

  // ----------------
  // Now process the elements
  // ----------------
  lCurNode := Doc.DocumentElement.FirstChild;
  lPage := AData.AddTextPageSequence();
  //lPage.Width := AData.Width;
  //lPage.Height := AData.Height;
  while Assigned(lCurNode) do
  begin
    lNodeName := lCurNode.NodeName;
    if lNodeName = 'body' then
    begin
      lCurSubnode := lCurNode.FirstChild;
      while Assigned(lCurSubnode) do
      begin
        lCurEntity := ReadEntityFromNode(lCurSubnode, lPage, AData);
        if lCurEntity <> nil then
          lPage.AddEntity(lCurEntity);

        lCurSubnode := lCurSubnode.NextSibling;
      end;
    end;

    lCurNode := lCurNode.NextSibling;
  end;
end;

class function TvHTMLVectorialReader.IsSupportedRasterImage(AFileName: string): Boolean;
var
  lExt: string;
begin
  Result := False;
  lExt := LowerCase(ExtractFileExt(AFileName));
  case lExt of
  '.png', '.jpg', '.jpeg', '.bmp', '.xpm':
    Result := True
  end;
end;

initialization

  RegisterVectorialReader(TvHTMLVectorialReader, vfHTML);

end.

