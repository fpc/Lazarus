unit TestHighlightXml;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, testregistry, TestBase, TestHighlightFoldBase, SynHighlighterXML,
  SynEditHighlighterFoldBase;

type

  { THighlightXml }

  THighlightXml = class(TTestBaseHighlighterFoldBase)
  protected
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    function TestTextFold1: TStringArray;
  published
    procedure TestXml;
    procedure TestFold;
  end;

implementation

function THighlightXml.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynXMLSyn.Create(nil);
end;

function THighlightXml.TestTextFold1: TStringArray;
begin
  SetLength(Result, 16);

  Result[0] := '<code>';
  Result[1] := '<font  size=3 face="Courier New">';
  Result[2] := '<span style="color: black;">';
  Result[3] := '</span></font>';
  Result[4] := '</code>';

  Result[5] := '<code><div><span></span>';
  Result[6] := '<font>';
  Result[7] := '<span><p><p></p><div><div></div>';
  Result[8] := '</div><p></p></p></span></font></div>';
  Result[9] := '</code>';

  Result[10] := '<code><div><span></span>';
  Result[11] := '<font>';
  Result[12] := '<span><p><p></p><div><div></div>';
  Result[13] := '</div><p></p></p></span></font></div><div>';  // close and open line
  Result[14] := '</div></code>';
  Result[15] := '';
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

procedure THighlightXml.TestFold;
begin
  ReCreateEdit;
  SetLines(TestTextFold1);

  CheckFoldOpenCounts('f1', [1,1,1,0,0,  2,1,3,0,0, 2,1,3,1,0]);
  CheckFoldLengths   ('f1', [
    ExpVLine(0,[4]),
    ExpVLine(1,[2]),
    ExpVLine(2,[1]),

    ExpVLine(5,[4,3]),
    ExpVLine(6,[2]),
    ExpVLine(7,[1,1,1]),

    ExpVLine(10,[4,3-1]),
    ExpVLine(11,[2-1]),
    //ExpVLine(12,[1-1,1-1,1-1]),
    ExpVLine(13,[1])

  ]);
  CheckFoldEndLines  ('simple', [
    ExpVLine(0,[4]),
    ExpVLine(1,[3]),
    ExpVLine(2,[3])
  ]);

end;



initialization

  RegisterTest(THighlightXml); 
end.

