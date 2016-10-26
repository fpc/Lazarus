unit TestMarkupFoldColoring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, testregistry, TestBase, TestHighlightPas, Forms,
  LCLProc, SynEdit, SynHighlighterPas, SynEditMarkupFoldColoring,
  SynEditMiscClasses, SynEditMarkup, SynEditHighlighterFoldBase;

type

  { TTestMarkupFoldColoring }

  TTestMarkupFoldColoring = class(TTestBaseHighlighterPas)
  private
    Markup: TSynEditMarkupFoldColors;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce;
    function TestText1: TStringArray;
    procedure EnableOutlines(AEnbledTypes: TPascalCodeFoldBlockTypes);
  published
    procedure TestColors;
  end;

implementation

{ TTestMarkupFoldColoring }

procedure TTestMarkupFoldColoring.SetUp;
begin
  Markup := nil;
  inherited SetUp;
end;

procedure TTestMarkupFoldColoring.TearDown;
begin
  FreeAndNil(Markup);
  inherited TearDown;
end;

procedure TTestMarkupFoldColoring.ReCreateEdit;
begin
  inherited ReCreateEdit;
  Markup := TSynEditMarkupFoldColors.Create(SynEdit);
  Markup.Lines := SynEdit.TextBuffer;
  //Markup.Highlighter := SynEdit.Highlighter;
end;

function TTestMarkupFoldColoring.TestText1: TStringArray;
begin
  SetLength(Result, 20);
  Result[ 0] := 'program Foo;';
  Result[ 1] := '';
  Result[ 2] := 'procedure a;';
  Result[ 3] := 'var';
  Result[ 4] := '  a: boolean;';
  Result[ 5] := '  procedure inner;';
  Result[ 6] := '    writeln()';
  Result[ 7] := '  end;';
  Result[ 8] := '';
  Result[ 9] := 'begin';
  Result[10] := '  if a then begin';
  Result[11] := '    writeln()';
  Result[12] := '  end;';
  Result[13] := '';
  Result[14] := 'end;';
  Result[15] := '';
  Result[16] := 'begin';
  Result[17] := '';
  Result[18] := 'end.';
  Result[19] := '';
end;

procedure TTestMarkupFoldColoring.EnableOutlines(AEnbledTypes: TPascalCodeFoldBlockTypes);
var
  i: TPascalCodeFoldBlockType;
begin
  for i := low(TPascalCodeFoldBlockType) to high(TPascalCodeFoldBlockType) do begin
    PasHighLighter.FoldConfig[ord(i)].Enabled := i in AEnbledTypes;
    if (i in AEnbledTypes) then
      PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes + [fmOutline]
    else
      PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes - [fmOutline]
  end;
end;


procedure TTestMarkupFoldColoring.TestColors;
  procedure TestNextAtPos(ALine, APos, AExp: Integer; Amsg: String);
  var
    col: TLazSynDisplayTokenBound;
    rtl: TLazSynDisplayRtlInfo;
    nextP, nextL: Integer;
  begin
    rtl.IsRtl := False;
    // text has no special char log=phys
    col.Logical := APos;
    col.Physical := APos;
    col.Offset := 0;
    Markup.GetNextMarkupColAfterRowCol(ALine, col, rtl, nextP, nextL);
    AssertEquals(BaseTestName + Amsg, AExp, Max(nextP, nextL));
  end;
begin
  ReCreateEdit;

  PushBaseName('All folds');

  SetLines(TestText1);
  EnableFolds([cfbtBeginEnd.. cfbtNone], [cfbtSlashComment]);
  EnableOutlines([cfbtBeginEnd.. cfbtNone]);

  PushBaseName('Line 7');
    Markup.PrepareMarkupForRow(7); // writeln // inner
    TestNextAtPos(7, 0, 1, '1 markup at 1');
    TestNextAtPos(7, 1, 3, '2 markup at 3');
    TestNextAtPos(7, 3,-1, '3 markup none');
  PopBaseName;

  PushBaseName('Line 11 (if then)');
    Markup.PrepareMarkupForRow(11); // if them
    TestNextAtPos(11, 0, 1, '1 markup at 1');
    TestNextAtPos(11, 1, 3, '2 markup at 3');
    TestNextAtPos(11, 3, 8, '3 markup at 8 (then)');
    // ....
  PopBaseName;

  PopBaseName;
end;

initialization

  RegisterTest(TTestMarkupFoldColoring);
end.

