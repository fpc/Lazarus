unit TestHighlightXml;

{$mode objfpc}{$H+}

interface

uses
  testregistry, TestBase, TestHighlightFoldBase, SynHighlighterXML,
  SynEditHighlighterFoldBase;

type

  { THighlightXml }

  THighlightXml = class(TTestBaseHighlighterFoldBase)
  protected
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
  published
    procedure TestXml;
  end; 

implementation

function THighlightXml.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynXMLSyn.Create(nil);
end;

procedure THighlightXml.TestXml;
  function TestText: TStringArray;
  begin
    SetLength(Result, 4);
    Result[0] := '<qwe>';
    Result[1] := '<abc> a </abc>';
    Result[2] := '</qwe>';
    Result[3] := '';
  end;
begin
  ReCreateEdit;
  SetLines(TestText);

  CheckFoldOpenCounts('simple', [1,0,0]);
  CheckFoldLengths   ('simple', [ExpVLine(0,[2])]);
  CheckFoldEndLines  ('simple', [ExpVLine(0,[2])]);

  SetLines(['<a>', '<b><c>', '</c>', '', '</b></a>', '']);
  CheckFoldOpenCounts('nested', [1,2,0,0,0]);
  CheckFoldLengths   ('nested', [ExpVLine(0, [4]),  ExpVLine(1, [3,1]) ]);
  CheckFoldEndLines  ('nested', [ExpVLine(0, [4]),  ExpVLine(1, [4,2]) ]);

  // c is not closed, and ended by b
  SetLines(['<a>', '<b><c>', '', '', '</b></a>', '']);
  CheckFoldOpenCounts('bad nested', [1,2,0,0,0]);
  CheckFoldLengths   ('bad nested', [ExpVLine(0, [4]),  ExpVLine(1, [3,3]) ]);
  CheckFoldEndLines  ('bad nested', [ExpVLine(0, [4]),  ExpVLine(1, [4,4]) ]);

  // a is not closed
  SetLines(['<a>', '<b><c>', '</c>', '', '</b>', '']);
  CheckFoldOpenCounts('open end', [1,2,0,0,0]);
  CheckFoldLengths   ('open end', [ExpVLine(0, [4]),  ExpVLine(1, [3,1]) ]);
  CheckFoldEndLines  ('open end', [ExpVLine(0, [4]),  ExpVLine(1, [4,2]) ]);

  // a is not closed
  SetLines(['<a>', '']);
  CheckFoldOpenCounts('open end (one line)', [0]);
  //CheckFoldLengths   ('open end (one line)', [ExpVLine(0, [0]) ]);
  //CheckFoldEndLines  ('open end (one line)', [ExpVLine(0, [0]) ]);

end;



initialization

  RegisterTest(THighlightXml); 
end.

