unit TestHighlightXml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TestBase, SynHighlighterXML;

type

  { THighlightXml }

  THighlightXml = class(TTestBase)
  published
    procedure TestXml;
  end; 

implementation

procedure THighlightXml.TestXml;
  function TestText: TStringArray;
  begin
    SetLength(Result, 4);
    Result[0] := '<qwe>';
    Result[1] := '<abc> a </abc>';
    Result[2] := '</qwe>';
    Result[3] := '';
  end;
var
  hl: TSynXMLSyn;
begin
  ReCreateEdit;
  hl := TSynXMLSyn.Create(nil);
  SynEdit.Highlighter := hl;
  SetLines(TestText);

  SynEdit.Highlighter := nil;
  hl.free;
end;



initialization

  RegisterTest(THighlightXml); 
end.

