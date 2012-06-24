{
Reads a MathML Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit mathmlvectorialreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  laz2_xmlread, laz2_dom,
  fpvectorial, fpvutils;

type
  { TvMathMLVectorialReader }

  TvMathMLVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator, FCommaSeparator: TFormatSettings;
    function StringToFloat(AStr: string): Single;
  public
    { General reading methods }
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFormulaFromNode(ACurNode: TDOMNode; APage: TvVectorialPage; var AFormula: TvFormula);
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

{ TvMathMLVectorialReader }

function TvMathMLVectorialReader.StringToFloat(AStr: string): Single;
begin
  Result := StrToInt(AStr);
end;

constructor TvMathMLVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator
end;

destructor TvMathMLVectorialReader.Destroy;
begin
  inherited Destroy;
end;

procedure TvMathMLVectorialReader.ReadFormulaFromNode(ACurNode: TDOMNode;
  APage: TvVectorialPage; var AFormula: TvFormula);
var
  lCurNode: TDOMNode;
  lStr: DOMString;
  lFormula, lFormulaBottom: TvFormula;
  lFormElem: TvFormulaElement;
  lMFracRow: TDOMNode;
  lSubNodeNameStr: DOMString;
begin
  // Now process the elements inside the first layer
  lCurNode := ACurNode.FirstChild;
  while Assigned(lCurNode) do
  begin
    // mi - variables
    // Examples:
    // <mi>x</mi>
    lStr := lCurNode.NodeName;
    if lStr = 'mi' then
    begin
      AFormula.AddElementWithKindAndText(fekVariable, lCurNode.FirstChild.NodeValue);
    end
    // <mo>=</mo>
    else if lStr = 'mo' then
    begin
      lSubNodeNameStr := lCurNode.FirstChild.NodeValue;
      // equal
      if lSubNodeNameStr = '=' then
        AFormula.AddElementWithKind(fekSubtraction)
      // minus
      else if (lSubNodeNameStr = '&#x2212;') or (lSubNodeNameStr = #$22#$12) then
        AFormula.AddElementWithKind(fekEqual)
      // &InvisibleTimes;
      else if (lSubNodeNameStr = '&#x2062;') or (lSubNodeNameStr = #$20#$62) then
        AFormula.AddElementWithKind(fekMultiplication)
      // &PlusMinus;
      else if (lSubNodeNameStr = '&#x00B1;') or (lSubNodeNameStr = #$00#$B1) then
        AFormula.AddElementWithKind(fekPlusMinus)
      //
      else
        AFormula.AddElementWithKindAndText(fekVariable, lSubNodeNameStr);
    end
    //
    else if lStr = 'mfrac' then
    begin
      // Top line
      lMFracRow := lCurNode.FirstChild;
      lStr := lMFracRow.NodeName;
      if lStr = 'mrow' then
      begin
        lFormula := TvFormula.Create;
        ReadFormulaFromNode(lMFracRow, APage, lFormula);
      end
      else
        raise Exception.Create(Format('[TvMathMLVectorialReader.ReadFormulaFromNode] Error reading mfrac: expected mrow, got %s', [lStr]));
      // Bottom line
      lMFracRow := lMFracRow.NextSibling;
      lStr := lMFracRow.NodeName;
      if lStr = 'mrow' then
      begin
        lFormulaBottom := TvFormula.Create;
        ReadFormulaFromNode(lMFracRow, APage, lFormulaBottom);
      end
      else
        raise Exception.Create(Format('[TvMathMLVectorialReader.ReadFormulaFromNode] Error reading mfrac: expected mrow, got %s', [lStr]));
      // Now add both formulas into our element
      lFormElem := AFormula.AddElementWithKind(fekFraction);
      lFormElem.Formula := lFormula;
      lFormElem.BottomFormula := lFormulaBottom;
    end
    else if lStr = 'msqrt' then
    begin

    end;

    lCurNode := lCurNode.NextSibling;
  end;
end;

procedure TvMathMLVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument;
  lFirstLayer, lCurNode: TDOMNode;
  lPage: TvVectorialPage;
  lFormula: TvFormula;
  lStr: DOMString;
begin
  try
    // Read in xml file from the stream
    ReadXMLFile(Doc, AStream);

    {// Read the properties of the <svg> tag
    AData.Width := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('width'));
    AData.Height := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('height'));}

    // Now process the elements inside the first layer
    lFirstLayer := Doc.DocumentElement;
    lCurNode := lFirstLayer.FirstChild;
    lPage := AData.AddPage();
    lPage.Width := AData.Width;
    lPage.Height := AData.Height;
    while Assigned(lCurNode) do
    begin
      lStr := lCurNode.NodeName;
      if lStr = 'mrow' then
      begin
        lFormula := TvFormula.Create;
        ReadFormulaFromNode(lCurNode, lPage, lFormula);
        lPage.AddEntity(lFormula);
      end
      else
        raise Exception.Create(Format('[TvMathMLVectorialReader.ReadFromStream] Expected mrow, got %s', [lStr]));

      lCurNode := lCurNode.NextSibling;
    end;
  finally
    // finally, free the document
    Doc.Free;
  end;
end;

initialization

  RegisterVectorialReader(TvMathMLVectorialReader, vfMathML);

end.

