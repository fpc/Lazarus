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
  xmlread, dom,
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
begin

end;

procedure TvMathMLVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  Doc: TXMLDocument;
  lFirstLayer, lCurNode: TDOMNode;
  lPage: TvVectorialPage;
  lFormula: TvFormula;
begin
  try
    // Read in xml file from the stream
    ReadXMLFile(Doc, AStream);

    {// Read the properties of the <svg> tag
    AData.Width := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('width'));
    AData.Height := StringWithUnitToFloat(Doc.DocumentElement.GetAttribute('height'));}

    // Now process the elements inside the first layer
    lFirstLayer := Doc.DocumentElement.FirstChild;
    lCurNode := lFirstLayer.FirstChild;
    lPage := AData.AddPage();
    lPage.Width := AData.Width;
    lPage.Height := AData.Height;
    while Assigned(lCurNode) do
    begin
      if lCurNode.ToString = 'row' then
      begin
        lFormula := TvFormula.Create;
        ReadFormulaFromNode(lCurNode, lPage, lFormula);
        lPage.AddEntity(lFormula);
      end;

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

