unit TestHighlightPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, Forms, LCLProc, TestHighlightFoldBase,
  SynEdit, SynEditTypes, SynHighlighterPas, SynEditHighlighterFoldBase;

type

  // used by Fold / MarkupWord

  { TTestBaseHighlighterPas }

  TTestBaseHighlighterPas = class(TTestBaseHighlighterFoldBase)
  protected
    function PasHighLighter: TSynPasSyn;
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    procedure EnableFolds(AEnbledTypes: TPascalCodeFoldBlockTypes;
                          AHideTypes: TPascalCodeFoldBlockTypes = [];
                          ANoFoldTypes: TPascalCodeFoldBlockTypes = []
                         );
    procedure DebugFoldInfo(ALineIdx: Integer; AFilter: TSynFoldActions; Group: Integer=0);
    procedure DebugFoldInfo(AFilter: TSynFoldActions; Group: Integer=0);
    function FoldActionsToString(AFoldActions: TSynFoldActions): String;
    Procedure CheckPasFoldNodeInfo(AName: String; nd: TSynFoldNodeInfo;
      ALine: TLineIdx; AColumn, AAllColIndex: integer; LogXStart, LogXEnd,
      FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
      FoldType, FoldTypeCompatible: TPascalCodeFoldBlockType; FoldGroup: Integer;
      FoldAction: TSynFoldActions);
    Procedure CheckPasFoldNodeInfo(AName: String; nd: TSynFoldNodeInfo;
      ALine: TLineIdx; AColumn: integer; LogXStart, LogXEnd,
      FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
      FoldType, FoldTypeCompatible: TPascalCodeFoldBlockType; FoldGroup: Integer;
      FoldAction: TSynFoldActions);
  end;

  { TTestHighlighterPas }

  TTestHighlighterPas = class(TTestBaseHighlighterPas)
  protected
    function TestTextFoldInfo1: TStringArray;
    function TestTextFoldInfo2: TStringArray;
    function TestTextFoldInfo3: TStringArray;
    function TestTextFoldInfo4(AIfCol: Integer): TStringArray;
    function TestTextFoldInfo5: TStringArray;

    procedure CheckTokensForLine(Name: String; LineIdx: Integer; ExpTokens: Array of TtkTokenKind);
  published
    procedure TestFoldInfo;
    procedure TestExtendedKeywordsAndStrings;
    procedure TestContextForProcModifiers;
    procedure TestContextForProperties;
    procedure TestContextForProcedure;
    procedure TestContextForDeprecated;
    procedure TestContextForClassModifier; // Sealed abstract
    procedure TestContextForClassHelper;
    procedure TestContextForTypeHelper;
    procedure TestContextForClassFunction; // in class,object,record
    procedure TestContextForRecordHelper;
    procedure TestContextForStatic;
    procedure TestCaretAsString;
    procedure TestFoldNodeInfo;
  end;

implementation

{ TTestBaseHighlighterPas }

function TTestBaseHighlighterPas.PasHighLighter: TSynPasSyn;
begin
  Result := TSynPasSyn(FTheHighLighter);
end;

function TTestBaseHighlighterPas.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynPasSyn.Create(nil);
end;

procedure TTestBaseHighlighterPas.EnableFolds(AEnbledTypes: TPascalCodeFoldBlockTypes;
  AHideTypes: TPascalCodeFoldBlockTypes; ANoFoldTypes: TPascalCodeFoldBlockTypes);
var
  i: TPascalCodeFoldBlockType;
begin
  for i := low(TPascalCodeFoldBlockType) to high(TPascalCodeFoldBlockType) do begin
    PasHighLighter.FoldConfig[ord(i)].Enabled := i in AEnbledTypes;
    if (i in ANoFoldTypes) then
      PasHighLighter.FoldConfig[ord(i)].Modes := []
    else
      PasHighLighter.FoldConfig[ord(i)].Modes := [fmFold];
    if i in AHideTypes then
      PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes + [fmHide];

    PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes +
      PasHighLighter.FoldConfig[ord(i)].SupportedModes * [fmMarkup];
  end;
end;

procedure TTestBaseHighlighterPas.DebugFoldInfo(ALineIdx: Integer;
  AFilter: TSynFoldActions; Group: Integer=0);
var
  i, c: LongInt;
  n: TSynFoldNodeInfo;
  l: TLazSynFoldNodeInfoList;
begin
  l := PasHighLighter.FoldNodeInfo[ALineIdx];
  c := PasHighLighter.FoldNodeInfo[ALineIdx].CountEx(AFilter, Group);
  l.ClearFilter;
  l.ActionFilter := AFilter;
  l.GroupFilter := Group;
  debugln(['### Foldinfo Line: ', ALineIdx,
           ' Cnt=', l.Count, ' CntEx=', c,
           '   PasMinLvl=', PasHighLighter.FoldBlockMinLevel(ALineIdx,1),
           ' EndLvl=',PasHighLighter.FoldBlockEndLevel(ALineIdx,1),
           //' Nestcnt=',PasHighLighter.FoldNestCount(ALineIdx,1),
            ' : ', copy(SynEdit.Lines[ALineIdx],1,40)]);
  debugln('Idx: LogXStart End  FldLvlStart End  NestLvlStart End  FldType FldTypeCompat FldGroup FldAction');
  for i := 0 to c-1 do begin
    n := l.NodeInfoEx(i, AFilter, Group);
    if sfaInvalid in n.FoldAction then
      debugln(Format('%3d %9d %3d  %11d %3d  %12d %3d  %7d %13d %8d %s',
                     [i, 0,0,  0,0, 0,0, 0, 0, 0, FoldActionsToString(n.FoldAction)]))
    else
      debugln(Format('%3d %9d %3d  %11d %3d  %12d %3d  %7d %13d %8d %s   // %s',
                     [i, n.LogXStart, n.LogXEnd,
                      n.FoldLvlStart, n.FoldLvlEnd,  n.NestLvlStart, n.NestLvlEnd,
                      PtrUInt(n.FoldType), PtrUInt(n.FoldTypeCompatible), n.FoldGroup,
                      FoldActionsToString(n.FoldAction),
                      copy(SynEdit.Lines[ALineIdx],n.LogXStart, n.LogXEnd-n.LogXStart+1)
                     ]));
  end;
end;

procedure TTestBaseHighlighterPas.DebugFoldInfo(AFilter: TSynFoldActions;
  Group: Integer=0);
var
  i: Integer;
begin
  for i := 0 to SynEdit.Lines.Count - 1 do
    DebugFoldInfo(i, AFilter, Group);
end;

function TTestBaseHighlighterPas.FoldActionsToString(
  AFoldActions: TSynFoldActions): String;
var
  s: string;
  i: TSynFoldAction;
begin
  Result:='';
  for i := low(TSynFoldAction) to high(TSynFoldAction) do
    if i in AFoldActions then begin
      WriteStr(s, i);
      Result := Result + s + ',';
    end;
  if Result <> '' then SetLength(Result, Length(Result)-1);
end;

procedure TTestBaseHighlighterPas.CheckPasFoldNodeInfo(AName: String; nd: TSynFoldNodeInfo;
  ALine: TLineIdx; AColumn, AAllColIndex: integer; LogXStart, LogXEnd, FoldLvlStart,
  FoldLvlEnd, NestLvlStart, NestLvlEnd: Integer; FoldType,
  FoldTypeCompatible: TPascalCodeFoldBlockType; FoldGroup: Integer;
  FoldAction: TSynFoldActions);
begin
  AName := BaseTestName + AName;
  AssertEquals(Format('%s (%d/%d) LineIndex',    [AName, ALine, AColumn]), ALine, nd.LineIndex);
  AssertEquals(Format('%s (%d/%d) NodeIndex',    [AName, ALine, AColumn]), AColumn, nd.NodeIndex);
  if AAllColIndex >= 0 then
  AssertEquals(Format('%s (%d/%d) NodeIndex',    [AName, ALine, AColumn]), AAllColIndex, nd.AllNodeIndex);
  if not(sfaInvalid in nd.FoldAction) then begin
    AssertEquals(Format('%s (%d/%d) LogXStart',    [AName, ALine, AColumn]), LogXStart, nd.LogXStart);
    AssertEquals(Format('%s (%d/%d) LogXEnd',      [AName, ALine, AColumn]), LogXEnd, nd.LogXEnd);
    if FoldLvlStart >= 0 then
    AssertEquals(Format('%s (%d/%d) FoldLvlStart', [AName, ALine, AColumn]), FoldLvlStart, nd.FoldLvlStart);
    if FoldLvlEnd >= 0 then
    AssertEquals(Format('%s (%d/%d) FoldLvlEnd',   [AName, ALine, AColumn]), FoldLvlEnd, nd.FoldLvlEnd);
    AssertEquals(Format('%s (%d/%d) NestLvlStart', [AName, ALine, AColumn]), NestLvlStart, nd.NestLvlStart);
    AssertEquals(Format('%s (%d/%d) NestLvlEnd',   [AName, ALine, AColumn]), NestLvlEnd, nd.NestLvlEnd);
    AssertEquals(Format('%s (%d/%d) FoldType',     [AName, ALine, AColumn]), PtrInt(FoldType), PtrInt(nd.FoldType));
    AssertEquals(Format('%s (%d/%d) FoldTypeCompatible', [AName, ALine, AColumn]),
                       PtrInt(FoldTypeCompatible), PtrInt(nd.FoldTypeCompatible));
    AssertEquals(Format('%s (%d/%d) FoldGroup:',   [AName, ALine, AColumn]), FoldGroup, nd.FoldGroup);
  end;
  AssertEquals(Format('%s (%d/%d) FoldAction',   [AName, ALine, AColumn]),
    FoldActionsToString(FoldAction),
    FoldActionsToString(nd.FoldAction - [sfaOutline..sfaOutlineNoLine]));
end;

procedure TTestBaseHighlighterPas.CheckPasFoldNodeInfo(AName: String; nd: TSynFoldNodeInfo;
  ALine: TLineIdx; AColumn: integer; LogXStart, LogXEnd, FoldLvlStart, FoldLvlEnd,
  NestLvlStart, NestLvlEnd: Integer; FoldType, FoldTypeCompatible: TPascalCodeFoldBlockType;
  FoldGroup: Integer; FoldAction: TSynFoldActions);
begin
  CheckPasFoldNodeInfo(AName, nd, ALine, AColumn, -1, LogXStart, LogXEnd,
    FoldLvlStart, FoldLvlEnd, NestLvlStart, NestLvlEnd, FoldType, FoldTypeCompatible,
    FoldGroup, FoldAction);
end;

  { TTestHighlighterPas }

function TTestHighlighterPas.TestTextFoldInfo1: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a;';
  Result[2] := '{$IFDEF A}';
  Result[3] := 'begin';
  Result[4] := '{$ENDIF}';
  Result[5] := '  {$IFDEF B} with a do begin {$ENDIF}';
  Result[6] := '    writeln()';
  Result[7] := '  end;';
  Result[8] := 'end;';
  Result[9] := 'begin';
  Result[10]:= 'end.';
  Result[11]:= '//';
  Result[12]:= '';
end;

function TTestHighlighterPas.TestTextFoldInfo2: TStringArray;
begin
  // mix folds and same-line-closing
  SetLength(Result, 9);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a;';
  Result[2] := '{$IFDEF A} begin {$IFDEF B} repeat a; {$ENDIF} until b; {$IFDEF c} try {$ELSE} //x';
  Result[3] := '  //foo';
  Result[4] := '  finally repeat x; {$ENDIF C} until y;';
  Result[5] := '  repeat m; until n; end; {$ENDIF A} // end finally';
  Result[6] := 'end';
  Result[7] := 'begin end.';
  Result[8] := '';

end;

function TTestHighlighterPas.TestTextFoldInfo3: TStringArray;
begin
  SetLength(Result, 12);
  Result[0] := 'Unit Foo;';
  Result[1] := 'Interface';
  Result[2] := 'type a=Integer;';
  Result[3] := 'var';
  Result[4] := '  b:Integer';
  Result[5] := 'const';
  Result[6] := '  c = 1;';
  Result[7] := '  d = 2; {$IFDEF A}';
  Result[8] := 'Implementation';
  Result[9] := '//';
  Result[10]:= 'end.';
  Result[11]:= '';
end;

function TTestHighlighterPas.TestTextFoldInfo4(AIfCol: Integer): TStringArray;
begin
  // various mixed of pascal and ifdef blocks => actually a test for pascal highlighter
  SetLength(Result, 8);
  Result[0] := 'program p;';
  Result[1] := 'procedure A;';
  case AIfCol of
    0: Result[2] := '{$IFDEF} begin  with a do begin';
    1: Result[2] := 'begin {$IFDEF} with a do begin';
    2: Result[2] := 'begin  with a do begin {$IFDEF}';
  end;
  Result[3] := '  end; // 2';
  Result[4] := 'end; // 1';
  Result[5] := '{$ENDIF}';
  Result[6] := '//';
  Result[7] := ''; // program fold is open end

end;

function TTestHighlighterPas.TestTextFoldInfo5: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := 'Unit Foo;';
  Result[1] := 'Interface';
  Result[2] := 'type';
  Result[3] := 'TFoo<T: class> = class(TBar<T>)';
  Result[4] := 'class procedure Proc;';
  Result[5] := 'end;';
  Result[6] := 'TFoo = record';
  Result[7] := 'class procedure Proc;';
  Result[8] := 'end;';
  Result[9] := 'Implementation';
  Result[10] := '//';
  Result[11] := 'end.';
  Result[12] := '';
end;

procedure TTestHighlighterPas.CheckTokensForLine(Name: String; LineIdx: Integer;
  ExpTokens: array of TtkTokenKind);
var
  c: Integer;
begin
  PasHighLighter.StartAtLineIndex(LineIdx);
  c := 0;
  while not PasHighLighter.GetEol do begin
    //DebugLn([PasHighLighter.GetToken,' (',PasHighLighter.GetTokenID ,') at ', PasHighLighter.GetTokenPos]);
    AssertEquals(Name + 'TokenId Line='+IntToStr(LineIdx)+' pos='+IntToStr(c),  ord(ExpTokens[c]), ord(PasHighLighter.GetTokenID));
    PasHighLighter.Next;
    inc(c);
    if c >= length(ExpTokens) then
      break;
  end;
  AssertEquals(Name+ 'TokenId Line='+IntToStr(LineIdx)+'  amount of tokens', length(ExpTokens), c );
end;

procedure TTestHighlighterPas.TestFoldInfo;
begin
  ReCreateEdit;

  //  DebugFoldInfo([]);

  {%region}
  SetLines(TestTextFoldInfo1);
  EnableFolds([cfbtBeginEnd..cfbtNone]);
  PushBaseName('Text 1 all folds');

  AssertEquals('Len Prog',  10, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',   7, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len IF A',   2, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len Begin',  5, PasHighLighter.FoldLineLength(3,0));
  AssertEquals('Len if beg', 2, PasHighLighter.FoldLineLength(5,0));
  AssertEquals('Len PrgBeg', 1, PasHighLighter.FoldLineLength(9,0));

  AssertEquals('Len invalid', -1, PasHighLighter.FoldLineLength(4,0)); // endif
  AssertEquals('Len // (no hide)', -1, PasHighLighter.FoldLineLength(11,0));

  //                       Pg pc $I bg $E be w  e  e be  e  //
  CheckFoldOpenCounts('', [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]);

  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  AssertEquals('Len // (with hide)', 0, PasHighLighter.FoldLineLength(11,0));

  //                       Pg pc $I bg $E be w  e  e be  e  //
  CheckFoldOpenCounts('', [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]); // TODO: does not include the //
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold], // includes the //
                          [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold, sfaFoldFold],
                          [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaOneLineOpen, sfaFold, sfaFoldHide],
                          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
  {%endregion}

  {%region}
  SetLines(TestTextFoldInfo2);
  EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtRepeat], [cfbtSlashComment]);
  PopPushBaseName('Text 2 all folds except repeat');

  AssertEquals('Len Prog',    7, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',    5, PasHighLighter.FoldLineLength(1,0));

  AssertEquals('Len IFDEF A', 3, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len Begin',   4, PasHighLighter.FoldLineLength(2,1));
  AssertEquals('Len Try',     3, PasHighLighter.FoldLineLength(2,2));
  AssertEquals('Len ELSE C',  2, PasHighLighter.FoldLineLength(2,3));

  AssertEquals('Len //',      0, PasHighLighter.FoldLineLength(3,0));
  AssertEquals('Finally',     1, PasHighLighter.FoldLineLength(4,0));

  AssertEquals('Len invalid begin end', -1, PasHighLighter.FoldLineLength(7,0));

  //                       Pg pc 4  // fi e  e  be-
  CheckFoldOpenCounts('', [1, 1, 4, 0, 1, 0, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 4, 1, 1, 0, 0, 0]);
  {%endregion}

  {%region}
  SetLines(TestTextFoldInfo3);
  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  PushBaseName('Text 3 (end-last-line)');

  AssertEquals('Len Unit',     10, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Intf',      6, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len type(non)',-1, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len var',       1, PasHighLighter.FoldLineLength(3,0));
  AssertEquals('Len const',     2, PasHighLighter.FoldLineLength(5,0));
  AssertEquals('Len Impl',      1, PasHighLighter.FoldLineLength(8,0));
  AssertEquals('Len //',        0, PasHighLighter.FoldLineLength(9,0));

  //                       Un If ty va -  co -  $  Im // e
  CheckFoldOpenCounts('', [1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0]);
  CheckFoldInfoCounts('', [sfaCloseFold, sfaFold, sfaLastLineClose],
                          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
  CheckFoldInfoCounts('', [sfaLastLineClose],
                          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]);
  {%endregion}

  {%region}
  SetLines(TestTextFoldInfo4(0));
  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  PushBaseName('Text 4 (mixed group) 0');

  AssertEquals('Len Prog',   6, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',   3, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len IF',     3, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len beg 1',  2, PasHighLighter.FoldLineLength(2,1));
  AssertEquals('Len beg 2',  1, PasHighLighter.FoldLineLength(2,2));
  AssertEquals('Len //',     0, PasHighLighter.FoldLineLength(6,0));

  //                       Pg Pc 3  e  e  $e //
  CheckFoldOpenCounts('', [1, 1, 3, 0, 0, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 3, 0, 0, 0, 1]);
  CheckFoldInfoCounts('', [sfaCloseFold, sfaFold, sfaLastLineClose],
                          [0, 0, 0, 0, 0, 0, 2]);
  CheckFoldInfoCounts('', [sfaLastLineClose],
                          [0, 0, 0, 0, 0, 0, 2]);


  SetLines(TestTextFoldInfo4(1));
  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  PushBaseName('Text 4 (mixed group) 1');

  AssertEquals('Len Prog',   6, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',   3, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len IF',     3, PasHighLighter.FoldLineLength(2,1));
  AssertEquals('Len beg 1',  2, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len beg 2',  1, PasHighLighter.FoldLineLength(2,2));
  AssertEquals('Len //',     0, PasHighLighter.FoldLineLength(6,0));

  //                       Pg Pc 3  e  e  $e //
  CheckFoldOpenCounts('', [1, 1, 3, 0, 0, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 3, 0, 0, 0, 1]);


  SetLines(TestTextFoldInfo4(2));
  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  PushBaseName('Text 4 (mixed group) 1');

  AssertEquals('Len Prog',   6, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',   3, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len IF',     3, PasHighLighter.FoldLineLength(2,2));
  AssertEquals('Len beg 1',  2, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len beg 2',  1, PasHighLighter.FoldLineLength(2,1));
  AssertEquals('Len //',     0, PasHighLighter.FoldLineLength(6,0));

  //                       Pg Pc 3  e  e  $e //
  CheckFoldOpenCounts('', [1, 1, 3, 0, 0, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 3, 0, 0, 0, 1]);
  {%endregion}

  {%region}
  SetLines(TestTextFoldInfo5);
  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  PushBaseName('Text 5 (class in generic type)');

  AssertEquals('Len Unit',     11, PasHighLighter.FoldLineLength( 0,0));
  AssertEquals('Len Intf',      7, PasHighLighter.FoldLineLength( 1,0));
  AssertEquals('Len type',      6, PasHighLighter.FoldLineLength( 2,0));
  AssertEquals('Len class',     2, PasHighLighter.FoldLineLength( 3,0));
  AssertEquals('Len record',    2, PasHighLighter.FoldLineLength( 6,0));
  AssertEquals('Len Impl',      1, PasHighLighter.FoldLineLength( 9,0));
  AssertEquals('Len //',        0, PasHighLighter.FoldLineLength(10,0));

  //                       un in ty cl pr en re pr en im // en
  CheckFoldOpenCounts('', [1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpenFold, sfaFold],
                          [1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 0]);
  CheckFoldInfoCounts('', [sfaCloseFold, sfaFold, sfaLastLineClose],
                          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  CheckFoldInfoCounts('', [sfaLastLineClose],
                          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
  {%endregion}
end;

procedure TTestHighlighterPas.TestExtendedKeywordsAndStrings;
begin
  ReCreateEdit;
  SetLines
    ([ 'Program A;',
       'var',
       '  Foo1: String;',
       '  Foo2: AnsiString;',
       '  Foo3: WideString;',
       '  Foo4: Shortstring;',
       '  Foo5: Integer;',
       '',
       'Procedure b;',
       'begin',
       ' while Foo1 <> '''' do',
       ' continue;',
       ' exit;',
       'end',
       '',
       'begin',
       'end',
       ''
    ]);

  PushBaseName('spsmDefault');
  PasHighLighter.StringKeywordMode := spsmDefault;
  CheckTokensForLine('String', 2, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkKey, tkSymbol  ]);
  CheckTokensForLine('ansi',   3, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkKey, tkSymbol  ]);
  CheckTokensForLine('wide',   4, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkKey, tkSymbol  ]);
  CheckTokensForLine('short',  5, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkKey, tkSymbol  ]);
  CheckTokensForLine('int',    6, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]); // selftest

  PopPushBaseName('spsmStringOnly');
  PasHighLighter.StringKeywordMode := spsmStringOnly;
  CheckTokensForLine('String', 2, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkKey, tkSymbol  ]);
  CheckTokensForLine('ansi',   3, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('wide',   4, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('short',  5, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('int',    6, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]); // selftest

  PopPushBaseName('spsmNone');
  PasHighLighter.StringKeywordMode := spsmNone;
  CheckTokensForLine('String', 2, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('ansi',   3, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('wide',   4, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('short',  5, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('int',    6, [tkSpace, tkIdentifier, tkSymbol, tkSpace,   tkIdentifier, tkSymbol  ]); // selftest


  PopPushBaseName('False');
  PasHighLighter.ExtendedKeywordsMode := False;
  CheckTokensForLine('continue',  11, [tkSpace, tkIdentifier, tkSymbol ]);
  CheckTokensForLine('exit',      12, [tkSpace, tkIdentifier, tkSymbol ]);

  PopPushBaseName('True');
  PasHighLighter.ExtendedKeywordsMode := True;
  CheckTokensForLine('continue',  11, [tkSpace, tkKey, tkSymbol ]);
  CheckTokensForLine('exit',      12, [tkSpace, tkKey, tkSymbol ]);

end;

procedure TTestHighlighterPas.TestContextForProcModifiers;
begin
  {%region message modifier for procedure}
    ReCreateEdit;
    SetLines
      ([ 'Unit A; interface',
         'type TFoo=class',
           'message: message;',
           'Procedure message(message: message); message 100;',
           'property message: message read message;',
         'end;',
         'var',
         '  message: message;',
         'Procedure message(message: message);'
      ]);
  CheckTokensForLine('class field',  2,
    [ tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol ]);
  CheckTokensForLine('class, proc message',  3,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol,        // "Procedure",  " ", "message", "("
      tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "message",, ":", " ", "message"
      tkSymbol, tkSymbol, tkSpace,                   // ")", ";", " "
      tkKey, // "message" as key
      tkSpace, tkNumber, tkSymbol
    ]);
  CheckTokensForLine('property',  4,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey, tkSpace, tkIdentifier ]);

  CheckTokensForLine('var',  7,
    [ tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol ]);
  CheckTokensForLine('procedure',  8,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol,        // "Procedure",  " ", "message", "("
      tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "message",, ":", " ", "message"
      tkSymbol, tkSymbol                             // ")", ";"
    ]);

  {%endregion}
end;

procedure TTestHighlighterPas.TestContextForProperties;
begin
  {%region property and index}
    ReCreateEdit;
    SetLines
      ([ 'Unit A; interface',
         'type TFoo = class',
         'property Index[Index: Integer]: Integer read GetIndex write SetIndex Index 3;',
         ''
      ]);
  CheckTokensForLine('property with index',  2,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol,        // "property",  " ", "Index", "["
      tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "Index",, ":", " ", "Integer"
      tkSymbol, tkSymbol, tkSpace, tkIdentifier,     // "]", ":", " ", "Integer"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'read', " ", "GetIndex"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'write', " ", "SetIndex"
      tkSpace, tkKey, tkSpace, tkNumber,             // '" ", "INDEX" (key), " ", "3"
      tkSymbol
    ]);

    SetLines
      ([ 'Unit A; interface',
         'type TFoo = class',
         'property AnIndex[Index: Index]: Index read Index write Index Index 3;',
         ''
      ]);
  CheckTokensForLine('property with index 2',  2,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol,        // "property",  " ", "AnIndex", "["
      tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "Index",, ":", " ", "Index"
      tkSymbol, tkSymbol, tkSpace, tkIdentifier,     // "]", ":", " ", "Index"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'read', " ", "Index"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'write', " ", "Index"
      tkSpace, tkKey, tkSpace, tkNumber,             // '" ", "INDEX" (key), " ", "3"
      tkSymbol
    ]);

    SetLines
      ([ 'Unit A; interface',
         'type',
         'Index = Integer;',
         'Foo = Index;',
         '',
         'var',
         'Foo, Index: Index;',
         'Index: Index;',
         ''
      ]);
  CheckTokensForLine('index outside property',  2,
    [tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, tkSymbol]);
  CheckTokensForLine('index outside property',  3,
    [tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, tkSymbol]);
  CheckTokensForLine('index outside property',  6,
    [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol]);
  CheckTokensForLine('index outside property',  7,
    [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol]);

  {%endregion}

  {%region property and read/write}
    ReCreateEdit;
    SetLines
      ([ 'Unit A; interface',
         'type TFoo = class',
         'property read[read: read]: read read read write read;',
         ''
      ]);
  CheckTokensForLine('property "read"',  2,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol,        // "property",  " ", "read", "["
      tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "read",, ":", " ", "read"
      tkSymbol, tkSymbol, tkSpace, tkIdentifier,     // "]", ":", " ", "read"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'READ', " ", "read"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'write', " ", "read"
      tkSymbol
    ]);


    ReCreateEdit;
    SetLines
      ([ 'Unit A; interface',
         'type TFoo = class',
         'property write[write: write]: write read write write write;',
         ''
      ]);
  CheckTokensForLine('property "write"',  2,
    [ tkKey, tkSpace, tkIdentifier, tkSymbol,        // "property",  " ", "write", "["
      tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "write",, ":", " ", "write"
      tkSymbol, tkSymbol, tkSpace, tkIdentifier,     // "]", ":", " ", "write"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'read', " ", "write"
      tkSpace, tkKey, tkSpace, tkIdentifier,      // " ", 'write', " ", "write"
      tkSymbol
    ]);
  {%endregion}
end;

procedure TTestHighlighterPas.TestContextForProcedure;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A;',
       'interface',
       '',
       'var',
       '  Foo: Procedure of object;', // no folding // do not end var block
       '',
       'type',
       '  TBar: ',
       '    Function(): Boolean;',  // no folding // do not end type block
       '',
       'Procedure a;', // no folding in interface
       '',
       'implementation',
       '',
       'var',
       '  Foo2: Procedure of object;', // no folding // do not end var block
       '',
       'type',
       '  TBar2: ',
       '    Function(): Boolean;',  // no folding // do not end type block
       '',
       'Procedure a;', // fold
       'var',
       '  Foo3: Procedure of object;', // no folding // do not end var block
       '',
       'begin end;',
       '',
       'end.',
       ''
    ]);
  EnableFolds([cfbtBeginEnd..cfbtNone]);
  CheckFoldOpenCounts('', [ 1, 1, 0, 1 {var}, 0, 0, 1 {type}, 0, 0, 0, 0 {Proc}, 0,
                            1 {impl}, 0, 1 {var}, 0, 0, 1 {type}, 0, 0, 0,
                            1 {proc}, 1 {var}, 0, 0, 0, 0, 0
                     ]);
  AssertEquals('Len var 1 ',   2, PasHighLighter.FoldLineLength(3, 0));
  AssertEquals('Len type 1 ',  3, PasHighLighter.FoldLineLength(6, 0));
  AssertEquals('Len var 2 ',   2, PasHighLighter.FoldLineLength(14, 0));
  AssertEquals('Len type 2 ',  3, PasHighLighter.FoldLineLength(17, 0));
  AssertEquals('Len var 3 ',   2, PasHighLighter.FoldLineLength(22, 0));

end;

procedure TTestHighlighterPas.TestContextForDeprecated;
  procedure SubTest(s: String);

    procedure SubTest2(struct: String);
    begin
      SetLines
        ([  'Unit A; interface',
            'type',
            'TFoo='+struct,
               s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
              'foo, '+s+', bar: Integer '+s+';',
              'procedure '+s+'('+s+': '+s+'); '+s+';',
            'end',
            ''
        ]);

      CheckTokensForLine('member in class', 3,
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
      CheckTokensForLine('multi member in class', 4,
        [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
        tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure in class', 5,
        [tkKey, tkSpace, tkIdentifier, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
         tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkKey {the one and only}, tkSymbol
        ]);


      if struct = 'record' then
        exit;

      SetLines
        ([  'Unit A; interface',
            'type',
            'TFoo='+struct,
               s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
              'foo, '+s+', bar: Integer '+s+';',
              'procedure '+s+'('+s+': '+s+'); '+s+';',
            'private',
               s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
              'foo, '+s+', bar: Integer '+s+';',
              'property '+s+': '+s+' read '+s+'; '+s+';',
              'procedure '+s+'('+s+': '+s+'); '+s+';',
            'end',
            ''
        ]);

      CheckTokensForLine('member in class', 3,
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
      CheckTokensForLine('multi member in class', 4,
        [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
        tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure in class', 5,
        [tkKey, tkSpace, tkIdentifier, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
         tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkKey {the one and only}, tkSymbol
        ]);

      CheckTokensForLine('member in class-sect', 7,
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
      CheckTokensForLine('multi member in class-sect', 8,
        [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
        tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
      CheckTokensForLine('property in class-sect', 9,
        [tkKey, tkSpace, tkIdentifier, tkSymbol { : }, tkSpace, tkIdentifier, tkSpace,
         tkKey { read }, tkSpace, tkIdentifier, tkSymbol { ; }, tkSpace, tkKey {the one and only}, tkSymbol
        ]);
      CheckTokensForLine('procedure in class-sect', 10,
        [tkKey, tkSpace, tkIdentifier, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
         tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkKey {the one and only}, tkSymbol
        ]);

    end;

  begin
    PushBaseName('test for '+s);
    ReCreateEdit;
    SetLines
      ([  'Unit A; interface',
          'var',
           s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
          'foo, '+s+', bar: Integer '+s+';',
          'type',
          s+' = '+s+' '+s+';',   // nameDEPRECATED = typeDEPRECATED deprecated;
          'procedure '+s+'('+s+': '+s+'); '+s+';',
          ''
      ]);
    CheckTokensForLine('var', 2,
      [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
    CheckTokensForLine('multi var', 3,
      [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
      tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
    CheckTokensForLine('type', 5,
      [tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
    CheckTokensForLine('procedure', 6,
      [tkKey, tkSpace, tkIdentifier, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
       tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkKey {the one and only}, tkSymbol
      ]);


    PushBaseName('class');
    SubTest2('class');
    PopPushBaseName('object');
    SubTest2('object');
    PopPushBaseName('record');
    SubTest2('record');
    PopBaseName;


    SetLines
      ([  'Program a',
          'procedure '+s+'('+s+': '+s+');',
          'var',
             s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
            'foo, '+s+', bar: Integer '+s+';',
          'begin end;',
          ''
      ]);

    CheckTokensForLine('procedure in implement', 1,
      [tkKey, tkSpace, tkIdentifier, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
       tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol
      ]);
    CheckTokensForLine('var in procedure', 3,
      [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);
    CheckTokensForLine('multi var in procedure', 4,
      [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
      tkSpace, tkIdentifier, tkSpace, tkKey {the one and only}, tkSymbol]);



    // after class declaration
    SetLines
      ([  'Unit A; interface',
          'type',
          'TFoo=class',
            'foo: Integer;',
          'end '+s+';',
          ''
      ]);
    CheckTokensForLine('after class declaration', 4,
      [tkKey, tkSpace, tkKey, tkSymbol]);

    PopBaseName;
  end;
begin
  SubTest('deprecated');
  SubTest('unimplemented');
  SubTest('experimental');
  SubTest('platform');
end;

procedure TTestHighlighterPas.TestContextForClassModifier;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = class sealed abstract',
         'a, sealed, abstract: Integer;',
         'procedure Foo; abstract;',
        'end;',
       ''
    ]);

  CheckTokensForLine('class declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace,
      tkKey {sealed}, tkSpace, tkKey {abstract}
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);
  CheckTokensForLine('procedure in class "',  4,
    [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);



  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = class {} sealed abstract',
         'a, sealed, abstract: Integer;',
         'procedure Foo; abstract;',
        'end;',
       ''
    ]);

  CheckTokensForLine('class declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace, tkComment, tkSpace,
      tkKey {sealed}, tkSpace, tkKey {abstract}
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);
  CheckTokensForLine('procedure in class "',  4,
    [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);



  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = class {}',
       ' sealed abstract',
         'a, sealed, abstract: Integer;',
         'procedure Foo; abstract;',
        'end;',
       ''
    ]);

  CheckTokensForLine('class declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace, tkComment
    ]);
  CheckTokensForLine('class declaration"',  3,
    [ tkSpace, tkKey {sealed}, tkSpace, tkKey {abstract}
    ]);
  CheckTokensForLine('var in class "',  4,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);
  CheckTokensForLine('procedure in class "',  5,
    [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);




  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = class(sealed) sealed abstract',
         'helper, sealed, abstract: Integer;',
         'procedure Foo; abstract;',
        'end;',
       ''
    ]);

  CheckTokensForLine('class declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSymbol, tkIdentifier, tkSymbol, tkSpace,
      tkKey {sealed}, tkSpace,
      tkKey {abstract}
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);
  CheckTokensForLine('procedure in class "',  4,
    [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);


end;

procedure TTestHighlighterPas.TestContextForClassHelper;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = class helper for TBar',
         'helper, sealed, abstract: Integer;',
         'procedure Foo; abstract;',
        'end;',
       ''
    ]);
  CheckTokensForLine('class declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace, tkKey {helper}, tkSpace, tkKey {for},
      tkSpace, tkIdentifier
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);
  CheckTokensForLine('procedure in class "',  4,
    [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);


  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = class helper(helper) for helper',
         'helper, sealed, abstract: Integer;',
         'procedure Foo; abstract;',
        'end;',
       ''
    ]);
  CheckTokensForLine('class declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace, tkKey {helper}, tkSymbol, tkIdentifier, tkSymbol,
      tkSpace, tkKey {for},
      tkSpace, tkIdentifier
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);
  CheckTokensForLine('procedure in class "',  4,
    [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);

end;

procedure TTestHighlighterPas.TestContextForTypeHelper;
  procedure DoChecks;
  begin
    CheckTokensForLine('not a helper',  2,
      [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
        tkKey {type}, tkSpace, tkIdentifier {helper}, tkSpace, tkKey {for}, tkSpace, tkIdentifier, tkSymbol
      ]);
    AssertEquals('not a helper / no fold', 0, PasHighLighter.FoldOpenCount(2));

    CheckTokensForLine('helper',  5,
      [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
        tkKey {type}, tkSpace, tkKey {helper}, tkSpace, tkKey {for}, tkSpace, tkIdentifier
      ]);
    CheckTokensForLine('procedure in helper',  6,
      [ tkKey, tkSpace, tkIdentifier,  tkSymbol, tkSpace, tkKey,  tkSymbol ]);
    CheckTokensForLine('uniq type',  8,
      [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
        tkKey {type}, tkSpace, tkIdentifier, tkSymbol
      ]);
    AssertEquals('uniq type / no fold', 0, PasHighLighter.FoldOpenCount(8));

    CheckTokensForLine('not a helper, switched off',  11,
      [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
        tkKey {type}, tkSpace, tkIdentifier {helper}, tkSpace, tkKey {for}, tkSpace, tkIdentifier, tkSymbol
      ]);
    AssertEquals('not a helper, switched off / no fold', 0, PasHighLighter.FoldOpenCount(11));
  end;

begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A; {$mode objfpc} interface',
       'type',
       'helper = type helper for helper;',
       'type',
       '{$modeswitch typehelpers}',
       'helper = type helper for helper',
         'procedure Foo; static;',
        'end;',
       'helper = type integer;',
       'type',
       '{$modeswitch typehelpers-}',
       'helper = type helper for helper;',
       '{$modeswitch typehelpers}',
       ''
    ]);

  DoChecks;
  SynEdit.TestTypeText(1, 2, ' ');
  DoChecks; // modeswitch on rescan

  PasHighLighter.FoldConfig[ord(cfbtClass)].Enabled := False;
  DoChecks;

end;

procedure TTestHighlighterPas.TestContextForClassFunction;
var
  i: Integer;
const
  t: array[0..2] of string = ('class', 'object', 'record');
begin
  for i:= 0 to 2 do begin
  ReCreateEdit;
  EnableFolds([cfbtBeginEnd..cfbtNone]);

  SetLines
    ([ 'Unit A; interface',
       'type',
       'TFoo = '+t[i],
         'class function f1: boolean;',
         'class procedure p1(v: boolean);',
        'end;',
       ''
    ]);
  //   unit/iface,  type,  record, -,-
  CheckFoldOpenCounts('', [2, 1, 1, 0, 0]);
  CheckTokensForLine('class function',  3,
    [ tkKey, tkSpace, tkKey, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol  ]);
  CheckTokensForLine('class procedure',  4,
    [ tkKey, tkSpace, tkKey, tkSpace,  tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSymbol  ]);

  end;
end;

procedure TTestHighlighterPas.TestContextForRecordHelper;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface {$mode delphi}',
       'type',
       'TFoo = record helper for TBar',
         'helper, sealed, abstract: Integer;',
        'end;',
       ''
    ]);
  CheckTokensForLine('record declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace, tkKey {helper}, tkSpace, tkKey {for},
      tkSpace, tkIdentifier
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);


  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface  {$mode delphi}',
       'type',
       'TFoo = record helper(helper) for helper',
         'helper, sealed, abstract: Integer;',
        'end;',
       ''
    ]);
  CheckTokensForLine('record declaration"',  2,
    [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
      tkKey {class}, tkSpace, tkKey {helper}, tkSymbol, tkIdentifier, tkSymbol,
      tkSpace, tkKey {for},
      tkSpace, tkIdentifier
    ]);
  CheckTokensForLine('var in class "',  3,
    [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
      tkSpace, tkIdentifier, tkSymbol
    ]);

end;

procedure TTestHighlighterPas.TestContextForStatic;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',
       'type',
       'static=class end;',
       'TFoo=class(static)',
       '  Ffoo,static: static; static;',
       '  static: static; static;',  // static as var-name can be first in list, IF previous was static modifier
       '  function static(static:static): static; static;',
       '  property static[static:static]: static read static write static;',
       'public',
       '  Ffoo,static: static; static;',
       '  static: static; static;',  // static as var-name can be first in list, IF previous was static modifier
       '  function static(static:static): static; static;',
       '  property static[static:static]: static read static write static;',
       'end;',
       ''
    ]);

  CheckTokensForLine('static = class',      2,
                     [tkIdentifier, tkSymbol, tkKey, tkSpace, tkKey, tkSymbol]);
  CheckTokensForLine('Tfoo=class(static)',  3,
  [tkIdentifier, tkSymbol, tkKey,tkSymbol, tkIdentifier, tkSymbol]);
  CheckTokensForLine('fields',              4,
                     [tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkKey, tkSymbol]);
  CheckTokensForLine('fields 2',            5,
                     [tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkKey, tkSymbol]);
  CheckTokensForLine('function',            6,
                     [tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                      tkSymbol, tkSpace, tkIdentifier, tkSymbol, // : #32 static ;
                      tkSpace, tkKey, tkSymbol                   // #32 static ;
                     ]);
  CheckTokensForLine('property',            7,
                     [tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                      tkSymbol, tkSpace, tkIdentifier,           // : #32 static
                      tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 read static
                      tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 write static
                      tkSymbol                   // ;
                     ]);
  CheckTokensForLine('pup fields',          9,
                     [tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkKey, tkSymbol]);
  CheckTokensForLine('pup fields 2',       10,
                     [tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkKey, tkSymbol]);
  CheckTokensForLine('pup function',       11,
                     [tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                      tkSymbol, tkSpace, tkIdentifier, tkSymbol, // : #32 static ;
                      tkSpace, tkKey, tkSymbol                   // #32 static ;
                     ]);
  CheckTokensForLine('pup property',       12,
                     [tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                      tkSymbol, tkSpace, tkIdentifier,           // : #32 static
                      tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 read static
                      tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 write static
                      tkSymbol                   // ;
                     ]);
end;

procedure TTestHighlighterPas.TestCaretAsString;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',  // 0
       'var',
         'a:char=^o;',
         'a:somestring=^o^c;',
         'b:^char=nil;',
       'type',
         'c=^char;',         // 6
         'c=type ^char;',         // 6
       'implementation',
       'function x(f:^char=^k^c):^v;', // actually the compiler does not allow ^ as pointer for result
       'var',
         'a:char=^o;',       // 11
         'b:^char=nil;',     // 12
       'type',
         'c=^char;',
       'begin',
         'i:=^f;',
         'x:=GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});', // 17
         'c:=p^;',
         'c:=p ^;',
         'c:=p(**)^;',
         'c:=p{} ^;',     // 21
         'i:=f(1)^;',     // 22
         'i:=f[1]^;',
         'i:=f^^;',
         'c:=p^+^i''e''^a#13^x;',
         'c:=x=^a and ^a=k and(^a^a=z);',
       'end;',
       ''
    ]);

  CheckTokensForLine('a:char=^o;',   2,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkString, tkSymbol]);
  CheckTokensForLine('a:char=^o^c;',   3,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkString, tkString, tkSymbol]);
  CheckTokensForLine('b:^char=nil;',   4,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol, tkKey, tkSymbol]);
  CheckTokensForLine('c=^char;',   6,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol]);
  CheckTokensForLine('c=type ^char;',   7,
                     [tkIdentifier, tkSymbol, tkKey, tkSpace, tkSymbol, tkIdentifier, tkSymbol]);

  CheckTokensForLine('function x(f:^char=^k):^v;',   9,
                     [tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier,  // function x(f
                      tkSymbol, tkSymbol, tkIdentifier, tkSymbol, tkString,  tkString,  // :^char=^k
                      tkSymbol, tkSymbol, tkSymbol, tkIdentifier, tkSymbol]);          // ):^v;
  CheckTokensForLine('LOCAL a:char=^o;',   11,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkString, tkSymbol]);
  CheckTokensForLine('LOCAL b:^char=nil;',   12,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol, tkKey, tkSymbol]);
  CheckTokensForLine('LOCAL c=^char;',   14,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol]);
  CheckTokensForLine('i:=^f',   16,
                     [tkIdentifier, tkSymbol, tkString, tkSymbol]);

  CheckTokensForLine('x:=GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});',   17,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,    // x:=GetTypeData(
                      tkIdentifier, tkSymbol, tkSymbol, tkIdentifier,    // PropInfo^.PropType
                      tkDirective, tkSymbol, tkDirective, tkSymbol, tkSymbol]);  // {$IFNDEF FPC}^{$ENDIF});

  CheckTokensForLine('c:=p^;',   18,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p ^;',   19,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSpace, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p(**)^;',   20,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkComment, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p{} ^;',   21,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkComment, tkSpace, tkSymbol, tkSymbol]);

  CheckTokensForLine('c:=p(1)^;',   22,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkNumber, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p[1]^;',   23,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkNumber, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p^^;',   24,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSymbol, tkSymbol]);

  CheckTokensForLine('c:=p^+^i''e''^a#13^x;',   25,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSymbol, // c:=p^+
                      tkString, tkString, tkString, tkString, tkString, tkSymbol  // ^i'e'^a#13^x;
                     ]);
  CheckTokensForLine('c:=x=^a and ^a=k and(^a^a=z);',   26,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkString, tkSpace, // c:=x=^a
                      tkKey, tkSpace, tkString, tkSymbol, tkIdentifier, tkSpace, // and ^a=k
                      tkKey, tkSymbol, tkString, tkString, tkSymbol, tkIdentifier,  // and(^a^a=z
                      tkSymbol, tkSymbol                                            // );'
                     ]);

end;

procedure TTestHighlighterPas.TestFoldNodeInfo;
  Procedure CheckNode(ALine: TLineIdx; AFilter: TSynFoldActions; AFoldGroup: Integer;
    AColumn: integer;
    AAllColIndex, LogXStart, LogXEnd,  FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
    FoldType, FoldTypeCompatible: TPascalCodeFoldBlockType; FoldGroup: Integer;
    FoldAction: TSynFoldActions);
  var
    nd: TSynFoldNodeInfo;
    l: TLazSynFoldNodeInfoList;
  begin
    l := PasHighLighter.FoldNodeInfo[ALine];

    // use NodeInfoEx
    nd := PasHighLighter.FoldNodeInfo[ALine].NodeInfoEx(AColumn, AFilter, AFoldGroup);
    CheckPasFoldNodeInfo('', nd, ALine, AColumn, AAllColIndex, LogXStart, LogXEnd, FoldLvlStart,
      FoldLvlEnd, NestLvlStart, NestLvlEnd, FoldType, FoldTypeCompatible,
      FoldGroup, FoldAction);

    // use filter
    l.ClearFilter;
    l.ActionFilter := AFilter;
    l.GroupFilter := AFoldGroup;
    nd := PasHighLighter.FoldNodeInfo[ALine].Item[AColumn];
    CheckPasFoldNodeInfo('', nd, ALine, AColumn, AAllColIndex, LogXStart, LogXEnd, FoldLvlStart,
      FoldLvlEnd, NestLvlStart, NestLvlEnd, FoldType, FoldTypeCompatible,
      FoldGroup, FoldAction);
  end;
  Procedure CheckNode(ALine: TLineIdx; AFilter: TSynFoldActions; AFoldGroup: Integer;
    AColumn: integer;
    LogXStart, LogXEnd,  FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
    FoldType, FoldTypeCompatible: TPascalCodeFoldBlockType; FoldGroup: Integer;
    FoldAction: TSynFoldActions);
  begin
    CheckNode(ALine, AFilter, AFoldGroup, AColumn, -1, LogXStart, LogXEnd,
      FoldLvlStart, FoldLvlEnd, NestLvlStart, NestLvlEnd, FoldType, FoldTypeCompatible,
      FoldGroup, FoldAction);
  end;
begin
  ReCreateEdit;
  PushBaseName('');
  //  // +(\d+) +(\d+) +(\d+) +(\d+) +(\d+) +(\d+) +(\d+) +(\d+) +(\d+) +(\d+) +(\S+)
  // CheckNode( 0, [], 0,   $1,   $2, $3,   $4, $5,   $6, $7,   $8, $9,  $10, [$11]);

  //Line, [filter], group,   Idx,   LogXStart End   FldLvlStart End   NestLvlStart End   FldType FldTypeCompat   FldGroup  [FldAction]

  {%region TEXT 1}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 1, 1, 1, 3, 0, 1, 2, 1, 2, 2]);

      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [], 0,   0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [], 0,   0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   {$ifdef a}                                                   # pasminlvl=2 endlvl=2
      CheckNode( 2, [], 0,   0,   0,   1, 7,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 3:   begin                                                        # pasminlvl=2 endlvl=3
      CheckNode( 3, [], 0,   0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:   {$endif}                                                     # pasminlvl=3 endlvl=3
      CheckNode( 4, [], 0,   0,   0,   1, 7,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 5:     {$ifdef b} if a then begin {$endif}                        zz# pasminlvl=3 endlvl=4
      CheckNode( 5, [], 0,   0,   0,   3, 9,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 5, [], 0,   1,   1,   23, 28,   3, 4,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 5, [], 0,   2,   2,   30, 36,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);
      // Line 6:       writeln()                                                # pasminlvl=4 endlvl=4
      // Line 7:     end;                                                       # pasminlvl=3 endlvl=3
      CheckNode( 7, [], 0,   0,   0,   2, 5,   4, 3,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 8:   end;                                                         # pasminlvl=1 endlvl=1
      CheckNode( 8, [], 0,   0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 8, [], 0,   1,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 9:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 9, [], 0,   0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [], 0,   0,   0,   0, 3,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode(10, [], 0,   1,   1,   0, 3,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0
      CheckNode(11, [], 0,   0,   0,   0, 2,   0, 1,   0, 1,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode(11, [], 0,   1,   1,   2, 2,   1, 0,   1, 0,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose,sfaLastLineClose, sfaSingleLine]);
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], []}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [] grp=1}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [], grp=1');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([],1);

      CheckFoldInfoCounts('', [], 1, [1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 2]);

      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [], 1,   0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [], 1,   0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   {$ifdef a}                                                   # pasminlvl=2 endlvl=2
      // Line 3:   begin                                                        # pasminlvl=2 endlvl=3
      CheckNode( 3, [], 1,   0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:   {$endif}                                                     # pasminlvl=3 endlvl=3
      // Line 5:     {$ifdef b} if a then begin {$endif}                                                    # pasminlvl=3 endlvl=4
      CheckNode( 5, [], 1,   0,   1,   23, 28,   3, 4,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:       writeln()                                                # pasminlvl=4 endlvl=4
      // Line 7:     end;                                                       # pasminlvl=3 endlvl=3
      CheckNode( 7, [], 1,   0,   0,   2, 5,   4, 3,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 8:   end;                                                         # pasminlvl=1 endlvl=1
      CheckNode( 8, [], 1,   0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 8, [], 1,   1,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 9:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 9, [], 1,   0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [], 1,   0,   0,   0, 3,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode(10, [], 1,   1,   1,   0, 3,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0
      CheckNode(11, [], 1,   0,   0,   0, 2,   0, 1,   0, 1,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode(11, [], 1,   1,   1,   2, 2,   1, 0,   1, 0,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose,sfaLastLineClose, sfaSingleLine]);
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [] grp=1}

    {%region TEXT 1 -- [cfbtBeginEnd,cfbtIfDef], [] grp=1}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd,cfbtIfDef], [], grp=4');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd,cfbtIfDef]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([],4);

      CheckFoldInfoCounts('', [], 1, [1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 2]);

      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=0
      CheckNode( 0, [], 1,   0,   0, 7,   0, 0,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaMultiLine]);   // program
      // Line 1:   procedure a;                                                 # pasminlvl=0 endlvl=0
      CheckNode( 1, [], 1,   0,   0, 9,   1, 1,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaMultiLine]);   // procedure
      // Line 2:   {$ifdef a}                                                   # pasminlvl=0 endlvl=0
      // Line 3:   begin                                                        # pasminlvl=0 endlvl=0
      CheckNode( 3, [], 1,   0,   0, 5,   2, 2,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaMultiLine]);   // begin
      // Line 4:   {$endif}                                                     # pasminlvl=0 endlvl=0
      // Line 5:     {$ifdef b} if a then begin {$endif}                                                    # pasminlvl=0 endlvl=1
      CheckNode( 5, [], 1,   0,   23, 28,   0, 1,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);   //  begin
      // Line 6:       writeln()                                                # pasminlvl=1 endlvl=1
      // Line 7:     end;                                                       # pasminlvl=0 endlvl=0
      CheckNode( 7, [], 1,   0,   2, 5,   1, 0,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);   //  end
      // Line 8:   end;                                                         # pasminlvl=0 endlvl=0
      CheckNode( 8, [], 1,   0,   0, 3,   3, 3,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold, sfaMultiLine]);   // end;
      CheckNode( 8, [], 1,   1,   0, 3,   2, 2,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold, sfaMultiLine]);   // end;
      // Line 9:   begin                                                        # pasminlvl=0 endlvl=1
      CheckNode( 9, [], 1,   0,   0, 5,   0, 1,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);   // begin
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [], 1,   0,   0, 3,   1, 0,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);   // end.
      CheckNode(10, [], 1,   1,   0, 3,   1, 1,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold, sfaMultiLine]);   // end.
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0
      CheckNode(11, [], 1,   0,   0, 2,   0, 0,   0, 1,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaMultiLine]);   // //
      CheckNode(11, [], 1,   1,   2, 2,   1, 1,   1, 0,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaLastLineClose, sfaMultiLine]);   // /
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [] grp=4}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaFold, sfaMultiLine]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [sfaFold, sfaMultiLine], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([sfaFold, sfaMultiLine]);

      CheckFoldInfoCounts('', [sfaFold, sfaMultiLine], 0, [1, 1, 1, 1, 1, 1, 0, 1, 2, 1, 2, 0]);

      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [sfafold, sfaMultiLine], 0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [sfafold, sfaMultiLine], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   {$ifdef a}                                                   # pasminlvl=2 endlvl=2
      CheckNode( 2, [sfafold, sfaMultiLine], 0,   0,   1, 7,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 3:   begin                                                        # pasminlvl=2 endlvl=3
      CheckNode( 3, [sfafold, sfaMultiLine], 0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:   {$endif}                                                     # pasminlvl=3 endlvl=3
      CheckNode( 4, [sfafold, sfaMultiLine], 0,   0,   1, 7,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold,sfaMultiLine]);
      // Line 5:     {$ifdef b} if a then begin {$endif}                                                    # pasminlvl=3 endlvl=4
      CheckNode( 5, [sfafold, sfaMultiLine], 0,   0,   23, 28,   3, 4,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:       writeln()                                                # pasminlvl=4 endlvl=4
      // Line 7:     end;                                                       # pasminlvl=3 endlvl=3
      CheckNode( 7, [sfafold, sfaMultiLine], 0,   0,   2, 5,   4, 3,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 8:   end;                                                         # pasminlvl=1 endlvl=1
      CheckNode( 8, [sfafold, sfaMultiLine], 0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 8, [sfafold, sfaMultiLine], 0,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 9:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 9, [sfafold, sfaMultiLine], 0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [sfafold, sfaMultiLine], 0,   0,   0, 3,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode(10, [sfafold, sfaMultiLine], 0,   1,   0, 3,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0

    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaFold, sfaMultiLine]}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup, sfaMultiLine]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone]-cfbtIfDef, [sfaMarkup, sfaMultiLine], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtIfDef]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([sfaMarkup, sfaMultiLine]);

      CheckFoldInfoCounts('', [sfaMarkup, sfaMultiLine], 0, [1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 0]);
      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [sfamarkup, sfaMultiLine], 0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [sfamarkup, sfaMultiLine], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   {$ifdef a}                                                   # pasminlvl=2 endlvl=2
// TODO add check for IFDEF
      // Line 3:   begin                                                        # pasminlvl=2 endlvl=3
      CheckNode( 3, [sfamarkup, sfaMultiLine], 0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:   {$endif}                                                     # pasminlvl=3 endlvl=3
      // Line 5:     {$ifdef b} if a then begin {$endif}                                                    # pasminlvl=3 endlvl=4
// TODO add chek for IFDEF
      CheckNode( 5, [sfamarkup, sfaMultiLine], 0,   0,   23, 28,   3, 4,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:       writeln()                                                # pasminlvl=4 endlvl=4
      // Line 7:     end;                                                       # pasminlvl=3 endlvl=3
      CheckNode( 7, [sfamarkup, sfaMultiLine], 0,   0,   2, 5,   4, 3,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 8:   end;                                                         # pasminlvl=1 endlvl=1
      CheckNode( 8, [sfamarkup, sfaMultiLine], 0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 8, [sfamarkup, sfaMultiLine], 0,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 9:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 9, [sfamarkup, sfaMultiLine], 0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [sfamarkup, sfaMultiLine], 0,   0,   0, 3,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode(10, [sfamarkup, sfaMultiLine], 0,   1,   0, 3,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0

    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup, sfaMultiLine]}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup, sfaMultiLine]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup, sfaMultiLine], cfbtIfDef 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], [], [cfbtIfDef]);
      //DebugFoldInfo([sfaMarkup, sfaMultiLine]);

      CheckFoldInfoCounts('', [sfaMarkup, sfaMultiLine], 0, [1, 1, 1, 1, 1, 3, 0, 1, 2, 1, 2, 0]);
      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [sfamarkup, sfaMultiLine], 0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [sfamarkup, sfaMultiLine], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   {$ifdef a}                                                   # pasminlvl=2 endlvl=2
// TODO add chek for IFDEF
      // Line 3:   begin                                                        # pasminlvl=2 endlvl=3
      CheckNode( 3, [sfamarkup, sfaMultiLine], 0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:   {$endif}                                                     # pasminlvl=3 endlvl=3
      // Line 5:     {$ifdef b} if a then begin {$endif}                                                    # pasminlvl=3 endlvl=4
// TODO add chek for IFDEF
      CheckNode( 5, [sfamarkup, sfaMultiLine], 0,   1,   23, 28,   3, 4,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:       writeln()                                                # pasminlvl=4 endlvl=4
      // Line 7:     end;                                                       # pasminlvl=3 endlvl=3
      CheckNode( 7, [sfamarkup, sfaMultiLine], 0,   0,   2, 5,   4, 3,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 8:   end;                                                         # pasminlvl=1 endlvl=1
      CheckNode( 8, [sfamarkup, sfaMultiLine], 0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 8, [sfamarkup, sfaMultiLine], 0,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 9:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 9, [sfamarkup, sfaMultiLine], 0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [sfamarkup, sfaMultiLine], 0,   0,   0, 3,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode(10, [sfamarkup, sfaMultiLine], 0,   1,   0, 3,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0

    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup, sfaMultiLine]}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtProcedure]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], [cfbtSlashComment]);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 1, 1, 1, 3, 0, 1, 2, 1, 2, 2, 0]);

      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=1
      CheckNode( 1, [], 0,   0,   0, 9,   1, 1,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaMultiLine]);
      // Line 2:   {$ifdef a}                                                   # pasminlvl=1 endlvl=1
      CheckNode( 2, [], 0,   0,   1, 7,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 3:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 3, [], 0,   0,   0, 5,   1, 2,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:   {$endif}                                                     # pasminlvl=2 endlvl=2
      CheckNode( 4, [], 0,   0,   1, 7,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 5:     {$ifdef b} if a then begin {$endif}                                                    # pasminlvl=2 endlvl=3
      CheckNode( 5, [], 0,   0,   3, 9,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 5, [], 0,   1,   23, 28,   2, 3,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 5, [], 0,   2,   30, 36,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);
      // Line 6:       writeln()                                                # pasminlvl=3 endlvl=3
      // Line 7:     end;                                                       # pasminlvl=2 endlvl=2
      CheckNode( 7, [], 0,   0,   2, 5,   3, 2,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 8:   end;                                                         # pasminlvl=1 endlvl=1
      CheckNode( 8, [], 0,   0,   0, 3,   2, 1,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 8, [], 0,   1,   0, 3,   2, 2,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold, sfaMultiLine]);
      // Line 9:   begin                                                        # pasminlvl=1 endlvl=2
      CheckNode( 9, [], 0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [], 0,   0,   0, 3,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode(10, [], 0,   1,   0, 3,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 11:   //                                                          # pasminlvl=0 endlvl=0
      CheckNode(11, [], 0,   0,   0, 2,   0, 1,   0, 1,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldHide,sfaOneLineOpen, sfaSingleLine]);
      CheckNode(11, [], 0,   1,   2, 2,   1, 0,   1, 0,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold,sfaOneLineClose,sfaLastLineClose, sfaSingleLine]);
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment]}

  {%endregion TEXT 1}


  {%region TEXT 2}

    {%region TEXT 2 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 2 -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo2);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 10, 2, 4, 5, 2, 3]);

      // Line 0:   program foo;                                                 # pasminlvl=0 endlvl=1
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   {$ifdef a} begin {$ifdef b} repeat a; {$endif} until b; {$ifdef c} try {$else} //x                                                     zz# pasminlvl=2 endlvl=4
      CheckNode( 2, [], 0,   0,   1, 7,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);            // {$IFDEF A}
      CheckNode( 2, [], 0,   1,   11, 16,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);  // begin
      CheckNode( 2, [], 0,   2,   18, 24,   1, 2,   1, 2,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);           // {$IFDEF B}
      CheckNode( 2, [], 0,   3,   28, 34,   3, 4,   3, 4,
                                  cfbtRepeat, cfbtRepeat,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]); // repeat a;
      CheckNode( 2, [], 0,   4,   39, 45,   2, 1,   2, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);                      // {$ENDIF}
      CheckNode( 2, [], 0,   5,   47, 52,   4, 3,   4, 3,
                                  cfbtRepeat, cfbtRepeat,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);            // until b;
      CheckNode( 2, [], 0,   6,   57, 63,   1, 2,   1, 2,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);           // {$IFDEF c}
      CheckNode( 2, [], 0,   7,   67, 70,   3, 4,   3, 4,
                                  cfbtTry, cfbtTry,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);// try
      CheckNode( 2, [], 0,   8,   72, 77,   2, 1,   2, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine, sfaCloseAndOpen]);                      // {$ELSE}
      CheckNode( 2, [], 0,   9,   72, 77,   1, 2,   1, 2,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaMarkup,sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine, sfaCloseAndOpen]);          // {$ELSE}
      // Line 3:     //foo                                                      # pasminlvl=4 endlvl=4
      CheckNode( 3, [], 0,   0,   2, 4,   4, 5,   4, 5,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 3, [], 0,   1,   7, 7,   5, 4,   5, 4,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose, sfaCloseForNextLine, sfaSingleLine]);
      // Line 4:     finally repeat x; {$endif c} until y;                                      # pasminlvl=4 endlvl=5
      CheckNode( 4, [], 0,   0,   2, 9,   4, 5,   4, 5,
                                  cfbtExcept, cfbtExcept,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 4, [], 0,   1,   10, 16,   5, 6,   5, 6,
                                  cfbtRepeat, cfbtRepeat,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 4, [], 0,   2,   21, 27,   2, 1,   2, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 4, [], 0,   3,   31, 36,   6, 5,   6, 5,
                                  cfbtRepeat, cfbtRepeat,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);
      // Line 5:     repeat m; until n; end; {$endif a} //                                      # pasminlvl=3 endlvl=3
      CheckNode( 5, [], 0,   0,   2, 8,   5, 6,   5, 6,
                                  cfbtRepeat, cfbtRepeat,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 5, [], 0,   1,   12, 17,   6, 5,   6, 5,
                                  cfbtRepeat, cfbtRepeat,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);
      CheckNode( 5, [], 0,   2,   21, 24,   5, 4,   5, 4,
                                  cfbtExcept, cfbtExcept,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 5, [], 0,   3,   21, 24,   4, 3,   4, 3,
                                  cfbtTry, cfbtTry,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 5, [], 0,   4,   27, 33,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:   end                                                          # pasminlvl=1 endlvl=1
      CheckNode( 6, [], 0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 6, [], 0,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 7:   begin end.                                                   # pasminlvl=0 endlvl=0
      CheckNode( 7, [], 0,   0,   0, 5,   1, 2,   1, 2,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaMarkup,sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 7, [], 0,   1,   6, 9,   2, 1,   2, 1,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaMarkup,sfaOneLineClose, sfaSingleLine]);
      CheckNode( 7, [], 0,   2,   6, 9,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
    {%endregion TEXT 2 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 2}


  {%region TEXT 3}

    {%region TEXT 3 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 3 -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo3);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 2, 1, 1, 1, 0, 3, 1, 3, 2]);

      // Line 0:   unit foo;                                                    # pasminlvl=0 endlvl=1
      CheckNode( 0, [], 0,   0,   0, 4,   0, 1,   0, 1,
                                  cfbtUnit, cfbtUnit,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   interface                                                    # pasminlvl=1 endlvl=2
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   type a=integer;                                              # pasminlvl=2 endlvl=2
      CheckNode( 2, [], 0,   0,   0, 4,   2, 3,   2, 3,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 2, [], 0,   1,   15, 15,   3, 2,   3, 2,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose, sfaCloseForNextLine, sfaSingleLine]);
      // Line 3:   var                                                          # pasminlvl=2 endlvl=3
      CheckNode( 3, [], 0,   0,   0, 3,   2, 3,   2, 3,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:     b:integer                                                  # pasminlvl=2 endlvl=2
      CheckNode( 4, [], 0,   0,   11, 11,   3, 2,   3, 2,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaCloseForNextLine, sfaMultiLine]);
      // Line 5:   const                                                        # pasminlvl=2 endlvl=3
      CheckNode( 5, [], 0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:     c = 1;                                                     # pasminlvl=3 endlvl=3
      // Line 7:     d = 2; {$ifdef a}                                          # pasminlvl=1 endlvl=1
      CheckNode( 7, [], 0,   0,   10, 16,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 7, [], 0,   1,   19, 19,   3, 2,   3, 2,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaCloseForNextLine, sfaMultiLine]);
      CheckNode( 7, [], 0,   2,   19, 19,   2, 1,   2, 1,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaCloseForNextLine, sfaMultiLine]);
      // Line 8:   implementation                                               # pasminlvl=1 endlvl=2
      CheckNode( 8, [], 0,   0,   0, 14,   1, 2,   1, 2,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 9:   //, unit-section                                             # pasminlvl=1 endlvl=1
      CheckNode( 9, [], 0,   0,   0, 2,   2, 3,   2, 3,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 9, [], 0,   1,   2, 2,   3, 2,   3, 2,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose, sfaCloseForNextLine, sfaSingleLine]);
      CheckNode( 9, [], 0,   2,   2, 2,   2, 1,   2, 1,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaCloseForNextLine, sfaMultiLine]);
      // Line 10:   end.                                                        # pasminlvl=0 endlvl=0
      CheckNode(10, [], 0,   0,   0, 3,   1, 0,   1, 0,
                                  cfbtUnit, cfbtUnit,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaMultiLine]);
      CheckNode(10, [], 0,   1,   4, 4,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold,sfaLastLineClose, sfaMultiLine]);
    {%endregion TEXT 3 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 3}


  {%region TEXT 4}

    {%region TEXT 4 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 4(1) -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo4(1));
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1,3, 1, 2, 1, 3]);

      // Line 0:   program p;                                                   # pasminlvl=0 endlvl=1
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 1:   procedure a;                                                 # pasminlvl=1 endlvl=2
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 2:   begin {$ifdef} if a then begin                                             # pasminlvl=2 endlvl=4
      CheckNode( 2, [], 0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 2, [], 0,   1,   7, 13,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 2, [], 0,   2,   25, 30,   3, 4,   3, 4,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 3:     end; // 2                                                  # pasminlvl=3 endlvl=3
      CheckNode( 3, [], 0,   0,   2, 5,   4, 3,   4, 3,
                                  cfbtBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 4:   end; // 1                                                    # pasminlvl=1 endlvl=1
      CheckNode( 4, [], 0,   0,   0, 3,   3, 2,   3, 2,
                                  cfbtTopBeginEnd, cfbtBeginEnd,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      CheckNode( 4, [], 0,   1,   0, 3,   2, 1,   2, 1,
                                  cfbtProcedure, cfbtProcedure,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold, sfaMultiLine]);
      // Line 5:   {$endif}                                                     # pasminlvl=1 endlvl=1
      CheckNode( 5, [], 0,   0,   1, 7,   1, 0,   1, 0,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaClose, sfaCloseFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:   //                                                           # pasminlvl=1 endlvl=0
      CheckNode( 6, [], 0,   0,   0, 2,   1, 2,   1, 2,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 6, [], 0,   1,   2, 2,   2, 1,   2, 1,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose,sfaLastLineClose, sfaSingleLine]);
      CheckNode( 6, [], 0,   2,   2, 2,   1, 0,   1, 0,
                                  cfbtProgram, cfbtProgram,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold,sfaLastLineClose, sfaMultiLine]);
    {%endregion TEXT 4 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 4}

  {%region TEXT 5}

    {%region TEXT 5 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 5 -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo5);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtForDo,cfbtWhileDo,cfbtWithDo], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 1, 1, 0, 1, 1, 0, 3, 1, 3, 1]);

      // Line 0:   unit foo;
      CheckNode( 0, [], 0,   0,   0, 4,   0, 1,   0, 1,
                                  cfbtUnit, cfbtUnit,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaFold, sfaFoldFold, sfaMultiLine]);
      // Line 1:   interface
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold, sfaFold, sfaFoldFold, sfaMultiLine]);
      // Line 2:   type
      CheckNode( 2, [], 0,   0,   0, 4,   2, 3,   2, 3,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaFold, sfaFoldFold, sfaMultiLine, sfaOpenFold]);
      // Line 3:   TFoo<T: class> = class(TBar<T>)
      CheckNode( 3, [], 0,   0,   17, 22,   3, 4,   3, 4,
                                  cfbtClass, cfbtClass,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaFold, sfaFoldFold, sfaMultiLine, sfaMarkup, sfaOpenFold]);
      // Line 4:   class procedure Proc;
      CheckNode( 4, [], 0,   -1,    0, 0,   4, 4,   4, 4,
                                  cfbtNone, cfbtNone,  FOLDGROUP_PASCAL,
                                  [sfaInvalid]);
      // Line 5:   end.
      CheckNode( 5, [], 0,   0,    0, 3,   4, 3,   4, 3,
                                  cfbtClass, cfbtClass,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaFold, sfaMultiLine, sfaMarkup, sfaCloseFold]);
      CheckNode( 6, [], 0,   0,     7, 13,   3, 4,   3, 4,
                                  cfbtRecord, cfbtRecord,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaFold, sfaFoldFold, sfaMultiLine, sfaMarkup, sfaOpenFold]);
      // Line 7:   class procedure Proc;
      CheckNode( 7, [], 0,   -1,    0, 0,   4, 4,   4, 4,
                                  cfbtNone, cfbtNone,  FOLDGROUP_PASCAL,
                                  [sfaInvalid]);
      // Line 8:   end.
      CheckNode( 8, [], 0,   0,    0, 3,   4, 3,   4, 3,
                                  cfbtRecord, cfbtRecord,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaFold, sfaMultiLine, sfaMarkup, sfaCloseFold]);
      CheckNode( 8, [], 0,   1,    4, 4,   3, 2,   3, 2,
                                  cfbtVarType, cfbtVarType,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaFold, sfaMultiLine, sfaCloseForNextLine, sfaCloseFold]);
      CheckNode( 8, [], 0,   2,    4, 4,   2, 1,   2, 1,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaFold, sfaMultiLine, sfaCloseForNextLine, sfaCloseFold]);
      // Line 9:   implementation
      CheckNode( 9, [], 0,   0,   0, 14,   1, 2,   1, 2,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold, sfaFoldFold, sfaMultiLine]);
      // Line 10:   //, unit-section
      CheckNode(10, [], 0,   0,   0, 2,   2, 3,   2, 3,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode(10, [], 0,   1,   2, 2,   3, 2,   3, 2,
                                  cfbtSlashComment, cfbtSlashComment,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose, sfaCloseForNextLine, sfaSingleLine]);
      CheckNode(10, [], 0,   2,   2, 2,   2, 1,   2, 1,
                                  cfbtUnitSection, cfbtUnitSection,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaCloseForNextLine, sfaMultiLine]);
      // Line 11:   end.
      CheckNode(11, [], 0,   0,   0, 3,   1, 0,   1, 0,
                                  cfbtUnit, cfbtUnit,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaMultiLine]);
    {%endregion TEXT 5 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 5}
end;

initialization

  RegisterTest(TTestHighlighterPas);
end.

