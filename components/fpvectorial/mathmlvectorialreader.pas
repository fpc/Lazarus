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
    procedure AddNodeToFormula(ANode: TDOMNode; APage: TvVectorialPage; var AFormula: TvFormula);
    procedure ReadFormulaFromNodeChildren(ACurNode: TDOMNode; APage: TvVectorialPage; var AFormula: TvFormula);
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

procedure TvMathMLVectorialReader.AddNodeToFormula(ANode: TDOMNode;
  APage: TvVectorialPage; var AFormula: TvFormula);
var
  lNodeName, lNodeText, lSubNodeName: DOMString;
  lFormula, lFormulaBottom: TvFormula;
  lFormElem: TvFormulaElement;
  lMFracRow: TDOMNode;
  lSubNodeNameStr: DOMString;
  lNodeTextLen: Integer;
begin
  lNodeName := ANode.NodeName;
  if ANode.FirstChild <> nil then
    lNodeText := ANode.FirstChild.NodeValue;
  // mi - variables
  // Examples:
  // <mi>x</mi>
  if lNodeName = 'mi' then
  begin
    AFormula.AddElementWithKindAndText(fekVariable, lNodeText);
  end
  // mn - numbers
  // Examples:
  // <mn>4</mn>
  else if lNodeName = 'mn' then
  begin
    AFormula.AddElementWithKindAndText(fekVariable, lNodeText);
  end
  // <mo>=</mo>
  else if lNodeName = 'mo' then
  begin
    lNodeTextLen := Length(lNodeText);
    // equal
    if lNodeText = '=' then
      AFormula.AddElementWithKind(fekEqual)
    // minus
    else if (lNodeText = '&#x2212;') or (lNodeText = #$E2#$88#$92) or (lNodeText = '-') then
      AFormula.AddElementWithKind(fekSubtraction)
    // &InvisibleTimes;
    else if (lNodeText = '&#x2062;') or (lNodeText = #$E2#$81#$A2) then
      AFormula.AddElementWithKind(fekMultiplication)
    // &PlusMinus;
    else if (lNodeText = '&#x00B1;') or (lNodeText = #$C2#$B1) or (lNodeText = '±') then
      AFormula.AddElementWithKind(fekPlusMinus)
    //
    else
      AFormula.AddElementWithKindAndText(fekVariable, lNodeText);
  end
  // Fraction
  // should contain two sets of: <mrow>...elements...</mrow>
  else if lNodeName = 'mfrac' then
  begin
    // Top line
    lMFracRow := ANode.FirstChild;
    lSubNodeName := lMFracRow.NodeName;
    if lSubNodeName = 'mrow' then
    begin
      lFormula := TvFormula.Create;
      ReadFormulaFromNodeChildren(lMFracRow, APage, lFormula);
    end
    else
      raise Exception.Create(Format('[TvMathMLVectorialReader.ReadFormulaFromNode] Error reading mfrac: expected mrow, got %s', [lSubNodeName]));    // Bottom line
    lMFracRow := lMFracRow.NextSibling;
    lSubNodeName := lMFracRow.NodeName;
    if lSubNodeName = 'mrow' then
    begin
      lFormulaBottom := TvFormula.Create;
      ReadFormulaFromNodeChildren(lMFracRow, APage, lFormulaBottom);
    end
    else
      raise Exception.Create(Format('[TvMathMLVectorialReader.ReadFormulaFromNode] Error reading mfrac: expected mrow, got %s', [lSubNodeName]));
    // Now add both formulas into our element
    lFormElem := AFormula.AddElementWithKind(fekFraction);
    lFormElem.Formula := lFormula;
    lFormElem.AdjacentFormula := lFormulaBottom;
  end
  // Square Root
  // might contain 1 set of: <mrow>...elements...</mrow>
  // or just: ...elements...
  else if lNodeName = 'msqrt' then
  begin
    lFormula := TvFormula.Create;

    lMFracRow := ANode.FirstChild;
    lSubNodeName := lMFracRow.NodeName;
    if lSubNodeName = 'mrow' then
      ReadFormulaFromNodeChildren(lMFracRow, APage, lFormula)
    else
      ReadFormulaFromNodeChildren(ANode, APage, lFormula);

    lFormElem := AFormula.AddElementWithKind(fekRoot);
    lFormElem.Formula := lFormula;
  end
  // msup - Power
  // Example: b^2
  //<msup>
  //  <mi>b</mi>
  //  <mn>2</mn>
  //</msup>
  else if lNodeName = 'msup' then
  begin
    lFormElem := AFormula.AddElementWithKind(fekPower);

    // First read the bottom element
    lMFracRow := ANode.FirstChild;
    AddNodeToFormula(lMFracRow, APage, lFormElem.Formula);

    // Now the top element
    lMFracRow := lMFracRow.NextSibling;
    AddNodeToFormula(lMFracRow, APage, lFormElem.AdjacentFormula);
  end
  { msub - Subscript
    Example: Xi
  <msub>
    <mrow>
      <mi>x</mi>
    </mrow>
    <mrow>
      <mi>i</mi>
    </mrow>
  </msub>
  }
  else if lNodeName = 'msub' then
  begin
    lFormElem := AFormula.AddElementWithKind(fekSubscript);

    // First read the main element
    lMFracRow := ANode.FirstChild;
    AddNodeToFormula(lMFracRow, APage, lFormElem.Formula);

    // Now the subscripted element
    lMFracRow := lMFracRow.NextSibling;
    AddNodeToFormula(lMFracRow, APage, lFormElem.AdjacentFormula);
  end
  // mrow and msrow are horizontal formulas
  // <msrow> <mo>+</mo> <none/> <mn>33</mn> </msrow>
  else if (lNodeName = 'mrow') or (lNodeName = 'msrow') then
  begin
    lFormElem := AFormula.AddElementWithKind(fekFormula);

    lFormula := TvFormula.Create;
    // Read all elements
    ReadFormulaFromNodeChildren(ANode, APage, lFormula);
    lFormElem.Formula := lFormula;
  end
  // msline is a horizontal line
  // <msline/>
  else if lNodeName = 'msline' then
  begin
    lFormElem := AFormula.AddElementWithKind(fekHorizontalLine);
  end
  // mstyle can be ignored
  else if lNodeName = 'mstyle' then
  begin
    ReadFormulaFromNodeChildren(ANode, APage, AFormula);
  end
  { Somatory

  <munderover>
    <mrow>
      <mo>&#x2211;</mo>
    </mrow>
    <mrow>
      <mi>i</mi>
      <mo>=</mo>
      <mn>1</mn>
    </mrow>
    <mrow>
      <mi>N</mi>
    </mrow>
  </munderover>
  }
  else if lNodeName = 'munderover' then
  begin
    lFormElem := AFormula.AddElementWithKind(fekSummation);

    // The first element is just the symbol, ignore it
    lMFracRow := ANode.FirstChild;

    // Read the bottom element
    lMFracRow := lMFracRow.NextSibling;
    AddNodeToFormula(lMFracRow, APage, lFormElem.Formula);

    // Now the top element
    lMFracRow := lMFracRow.NextSibling;
    AddNodeToFormula(lMFracRow, APage, lFormElem.AdjacentFormula);
  end;
end;

procedure TvMathMLVectorialReader.ReadFormulaFromNodeChildren(ACurNode: TDOMNode;
  APage: TvVectorialPage; var AFormula: TvFormula);
var
  lCurNode: TDOMNode;
begin
  // Now process the elements inside the first layer
  lCurNode := ACurNode.FirstChild;
  while Assigned(lCurNode) do
  begin
    AddNodeToFormula(lCurNode, APage, AFormula);

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
        ReadFormulaFromNodeChildren(lCurNode, lPage, lFormula);
        lPage.AddEntity(lFormula);
      end
      else if lStr = 'mstack' then
      begin
        lFormula := TvVerticalFormulaStack.Create;
        ReadFormulaFromNodeChildren(lCurNode, lPage, lFormula);
        lPage.AddEntity(lFormula);
      end
      else
        raise Exception.Create(Format('[TvMathMLVectorialReader.ReadFromStream] Expected mrow or mstack, got %s', [lStr]));

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

