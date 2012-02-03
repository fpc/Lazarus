unit TestHighlighterLfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, Forms, LCLProc, TestHighlightFoldBase,
  SynEdit, SynEditTypes, SynHighlighterLFM, SynEditHighlighterFoldBase;

type

  { TTestBaseHighlighterLem }

  TTestBaseHighlighterLem = class(TTestBaseHighlighterFoldBase)
  protected
    function LfmHighLighter: TSynLFMSyn;
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    procedure EnableFolds(AEnbledTypes: TLfmCodeFoldBlockTypes;
                          AHideTypes: TLfmCodeFoldBlockTypes = [];
                          ANoFoldTypes: TLfmCodeFoldBlockTypes = []
                         );
    //procedure DebugFoldInfo(ALineIdx: Integer; AFilter: TSynFoldActions; Group: Integer=0);
    //procedure DebugFoldInfo(AFilter: TSynFoldActions; Group: Integer=0);
    //function FoldActionsToString(AFoldActions: TSynFoldActions): String;
  end;

  { TTestHighlighterLfm }

  TTestHighlighterLfm = class(TTestBaseHighlighterLem)
  protected
    function TestTextFoldInfo1: TStringArray;

    procedure CheckTokensForLine(Name: String; LineIdx: Integer; ExpTokens: Array of TtkTokenKind);
  published
    procedure TestFoldInfo;
  end;

implementation

{ TTestBaseHighlighterLem }

function TTestBaseHighlighterLem.LfmHighLighter: TSynLFMSyn;
begin
  Result := TSynLFMSyn(FTheHighLighter);
end;

function TTestBaseHighlighterLem.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynLFMSyn.Create(nil);
end;

procedure TTestBaseHighlighterLem.EnableFolds(AEnbledTypes: TLfmCodeFoldBlockTypes;
  AHideTypes: TLfmCodeFoldBlockTypes; ANoFoldTypes: TLfmCodeFoldBlockTypes);
var
  i: TLfmCodeFoldBlockType;
begin
  for i := low(TLfmCodeFoldBlockType) to high(TLfmCodeFoldBlockType) do begin
    LfmHighLighter.FoldConfig[ord(i)].Enabled := i in AEnbledTypes;
    if (i in ANoFoldTypes) then
      LfmHighLighter.FoldConfig[ord(i)].Modes := []
    else
      LfmHighLighter.FoldConfig[ord(i)].Modes := [fmFold];
    if i in AHideTypes then
      LfmHighLighter.FoldConfig[ord(i)].Modes := LfmHighLighter.FoldConfig[ord(i)].Modes + [fmHide]
  end;
end;

function TTestHighlighterLfm.TestTextFoldInfo1: TStringArray;
begin
  SetLength(Result, 11);
  Result[0] := 'object BreakPointGroupDlg: TBreakPointGroupDlg';
  Result[1] := '  Left = 431';
  Result[2] := '  Height = 225';
  Result[3] := '  object ButtonPanel1: TButtonPanel';
  Result[4] := '    Left = 6';
  Result[5] := '  end';
  Result[6] := '  object Label1: TLabel';
  Result[7] := '    Left = 0';
  Result[8] := '  end';
  Result[9] := 'end';
  Result[10] := '';


end;

procedure TTestHighlighterLfm.CheckTokensForLine(Name: String; LineIdx: Integer;
  ExpTokens: array of TtkTokenKind);
var
  c: Integer;
begin
  LfmHighLighter.StartAtLineIndex(LineIdx);
  c := 0;
  while not LfmHighLighter.GetEol do begin
    //DebugLn([LfmHighLighter.GetToken,' (',PasHighLighter.GetTokenID ,') at ', PasHighLighter.GetTokenPos]);
    AssertEquals(Name + 'TokenId Line='+IntToStr(LineIdx)+' pos='+IntToStr(c),  ord(ExpTokens[c]), ord(LfmHighLighter.GetTokenID));
    LfmHighLighter.Next;
    inc(c);
    if c >= length(ExpTokens) then
      break;
  end;
  AssertEquals(Name+ 'TokenId Line='+IntToStr(LineIdx)+'  amount of tokens', length(ExpTokens), c );
end;

procedure TTestHighlighterLfm.TestFoldInfo;
begin
  ReCreateEdit;

  //  DebugFoldInfo([]);

  {%region}
  SetLines(TestTextFoldInfo1);
  EnableFolds([cfbtLfmObject..cfbtLfmNone]);
  PushBaseName('Text 1 all folds');

  EnableFolds([cfbtLfmObject..cfbtLfmNone], [cfbtLfmNone]);
  AssertEquals('Len 0', 9, LfmHighLighter.FoldLineLength(0,0));
  //AssertEquals('Len 1', 0, LfmHighLighter.FoldLineLength(1,0));
  AssertEquals('Len 3', 2, LfmHighLighter.FoldLineLength(3,0));
  //AssertEquals('Len 4', 0, LfmHighLighter.FoldLineLength(4,0));
  //AssertEquals('Len 5', 0, LfmHighLighter.FoldLineLength(5,0));
  AssertEquals('Len 6', 2, LfmHighLighter.FoldLineLength(6,0));

  CheckFoldOpenCounts('', [1, 0, 0, 1, 0, 0, 1, 0, 0, 0]);

  {%endregion}

end;



initialization

  RegisterTest(TTestHighlighterLfm);
end.

