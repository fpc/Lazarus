unit TestHighlightPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, Forms, LCLProc,
  SynEdit, SynEditTypes, SynHighlighterPas, SynEditHighlighterFoldBase;

type

  // used by Fold / MarkupWord

  { TTestBaseHighlighterPas }

  TTestBaseHighlighterPas = class(TTestBase)
  protected
    PasHighLighter: TSynPasSyn;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce;
    procedure EnableFolds(AEnbledTypes: TPascalCodeFoldBlockTypes;
                          AHideTypes: TPascalCodeFoldBlockTypes = [];
                          ANoFoldTypes: TPascalCodeFoldBlockTypes = []
                         );
    procedure DebugFoldInfo(ALineIdx: Integer; AFilter: TSynFoldActions; Group: Integer=0);
    procedure DebugFoldInfo(AFilter: TSynFoldActions; Group: Integer=0);
    function FoldActionsToString(AFoldActions: TSynFoldActions): String;
  end;

  { TTestHighlighterPas }

  TTestHighlighterPas = class(TTestBaseHighlighterPas)
  protected
    function TestTextFoldInfo1: TStringArray;
    function TestTextFoldInfo2: TStringArray;
    function TestTextFoldInfo3: TStringArray;
    function TestTextFoldInfo4(AIfCol: Integer): TStringArray;

    procedure CheckFoldOpenCounts(Name: String; Expected: Array of Integer);
    procedure CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions; Expected: Array of Integer);
    procedure CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions; Group: Integer; Expected: Array of Integer);
    procedure CheckTokensForLine(Name: String; LineIdx: Integer; ExpTokens: Array of TtkTokenKind);

    function FoldActionsToString(AFoldActions: TSynFoldActions): String;
  published
    procedure TestFoldInfo;
    procedure TestExtendedKeywordsAndStrings;
    procedure TestContextForProcModifiers;
    procedure TestContextForProperties;
    procedure TestContextForProcedure;
    procedure TestContextForDeprecated;
    procedure TestContextForClassModifier; // Sealed abstract
    procedure TestContextForStatic;
    procedure TestFoldNodeInfo;
  end;

implementation

{ TTestBaseHighlighterPas }

procedure TTestBaseHighlighterPas.SetUp;
begin
  PasHighLighter := nil;
  inherited SetUp;
end;

procedure TTestBaseHighlighterPas.TearDown;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(PasHighLighter);
  inherited TearDown;
end;

procedure TTestBaseHighlighterPas.ReCreateEdit;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(PasHighLighter);
  inherited ReCreateEdit;
  PasHighLighter := TSynPasSyn.Create(nil);
  SynEdit.Highlighter := PasHighLighter;
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
      PasHighLighter.FoldConfig[ord(i)].Modes := PasHighLighter.FoldConfig[ord(i)].Modes + [fmHide]
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
           '   PasMinLvl=',PasHighLighter.MinimumPasFoldLevel(ALineIdx,1),
           ' EndLvl=',PasHighLighter.EndPasFoldLevel(ALineIdx,1),
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

  { TTestHighlighterPas }

function TTestHighlighterPas.TestTextFoldInfo1: TStringArray;
begin
  SetLength(Result, 13);
  Result[0] := 'program Foo;';
  Result[1] := 'procedure a;';
  Result[2] := '{$IFDEF A}';
  Result[3] := 'begin';
  Result[4] := '{$ENDIF}';
  Result[5] := '  {$IFDEF B} if a then begin {$ENDIF}';
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
    0: Result[2] := '{$IFDEF} begin  if a then begin';
    1: Result[2] := 'begin {$IFDEF} if a then begin';
    2: Result[2] := 'begin  if a then begin {$IFDEF}';
  end;
  Result[3] := '  end; // 2';
  Result[4] := 'end; // 1';
  Result[5] := '{$ENDIF}';
  Result[6] := '//';
  Result[7] := ''; // program fold is open end

end;

procedure TTestHighlighterPas.CheckFoldOpenCounts(Name: String; Expected: array of Integer);
var
  i: Integer;
begin
  for i := 0 to high(Expected) do
    AssertEquals(Name + 'OpenCount Line='+IntToStr(i),  Expected[i], PasHighLighter.FoldOpenCount(i));
end;

procedure TTestHighlighterPas.CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions;
  Expected: array of Integer);
begin
  CheckFoldInfoCounts(Name, Filter, 0, Expected);
end;

procedure TTestHighlighterPas.CheckFoldInfoCounts(Name: String;
  Filter: TSynFoldActions; Group: Integer; Expected: array of Integer);
var
  i: Integer;
  l: TLazSynFoldNodeInfoList;
begin
  for i := 0 to high(Expected) do begin
    l := PasHighLighter.FoldNodeInfo[i];
    AssertEquals(Name + 'InfoCount(Ex) Line='+IntToStr(i),
                 Expected[i],
                 l.CountEx(Filter, Group));
    l.ClearFilter;
    l.ActionFilter := Filter;
    l.GroupFilter := Group;
    AssertEquals(Name + 'InfoCount Line='+IntToStr(i),
                 Expected[i],
                 PasHighLighter.FoldNodeInfo[i].Count);
  end;
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

function TTestHighlighterPas.FoldActionsToString(AFoldActions: TSynFoldActions
  ): String;
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
  CheckFoldInfoCounts('', [sfaOpen, sfaFold],
                          [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]);

  EnableFolds([cfbtBeginEnd..cfbtNone], [cfbtSlashComment]);
  AssertEquals('Len // (with hide)', 0, PasHighLighter.FoldLineLength(11,0));

  //                       Pg pc $I bg $E be w  e  e be  e  //
  CheckFoldOpenCounts('', [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]); // TODO: does not include the //
  CheckFoldInfoCounts('', [sfaOpen, sfaFold], // includes the //
                          [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 1]);
  CheckFoldInfoCounts('', [sfaOpen, sfaFold, sfaFoldFold],
                          [1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0]);
  CheckFoldInfoCounts('', [sfaOpen, sfaOneLineOpen, sfaFold, sfaFoldHide],
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
  CheckFoldInfoCounts('', [sfaOpen, sfaFold],
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
  CheckFoldInfoCounts('', [sfaOpen, sfaFold],
                          [1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0]);
  CheckFoldInfoCounts('', [sfaClose, sfaFold, sfaLastLineClose],
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
  CheckFoldInfoCounts('', [sfaOpen, sfaFold],
                          [1, 1, 3, 0, 0, 0, 1]);
  CheckFoldInfoCounts('', [sfaClose, sfaFold, sfaLastLineClose],
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
  CheckFoldInfoCounts('', [sfaOpen, sfaFold],
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
  CheckFoldInfoCounts('', [sfaOpen, sfaFold],
                          [1, 1, 3, 0, 0, 0, 1]);
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


    SetLines
      ([  'Unit A; interface',
          'type',
          'TFoo=class',
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

procedure TTestHighlighterPas.TestFoldNodeInfo;
  Procedure CheckNode(ALine: TLineIdx; AFilter: TSynFoldActions; AFoldGroup: Integer;
    AColumn: integer;
    LogXStart, LogXEnd,  FoldLvlStart, FoldLvlEnd,  NestLvlStart, NestLvlEnd: Integer;
    FoldType: integer;  FoldTypeCompatible: integer; FoldGroup: Integer;
    FoldAction: TSynFoldActions);
    procedure CheckInfoNode(nd: TSynFoldNodeInfo);
    begin
      AssertEquals(Format('%s (%d/%d) LineIndex',    [BaseTestName, ALine, AColumn]), ALine, nd.LineIndex);
      AssertEquals(Format('%s (%d/%d) NodeIndex',    [BaseTestName, ALine, AColumn]), AColumn, nd.NodeIndex);
      if not(sfaInvalid in nd.FoldAction) then begin
        AssertEquals(Format('%s (%d/%d) LogXStart',    [BaseTestName, ALine, AColumn]), LogXStart, nd.LogXStart);
        AssertEquals(Format('%s (%d/%d) LogXEnd',      [BaseTestName, ALine, AColumn]), LogXEnd, nd.LogXEnd);
        AssertEquals(Format('%s (%d/%d) FoldLvlStart', [BaseTestName, ALine, AColumn]), FoldLvlStart, nd.FoldLvlStart);
        AssertEquals(Format('%s (%d/%d) FoldLvlEnd',   [BaseTestName, ALine, AColumn]), FoldLvlEnd, nd.FoldLvlEnd);
        AssertEquals(Format('%s (%d/%d) NestLvlStart', [BaseTestName, ALine, AColumn]), NestLvlStart, nd.NestLvlStart);
        AssertEquals(Format('%s (%d/%d) NestLvlEnd',   [BaseTestName, ALine, AColumn]), NestLvlEnd, nd.NestLvlEnd);
        AssertEquals(Format('%s (%d/%d) FoldType',     [BaseTestName, ALine, AColumn]), PtrUInt(FoldType), PtrUInt(nd.FoldType));
        AssertEquals(Format('%s (%d/%d) FoldTypeCompatible', [BaseTestName, ALine, AColumn]), PtrUInt(FoldTypeCompatible), PtrUInt(nd.FoldTypeCompatible));
        AssertEquals(Format('%s (%d/%d) FoldGroup:',   [BaseTestName, ALine, AColumn]), FoldGroup, nd.FoldGroup);
      end;
      AssertEquals(Format('%s (%d/%d) FoldAction',   [BaseTestName, ALine, AColumn]), FoldActionsToString(FoldAction), FoldActionsToString(nd.FoldAction));
    end;
  var
    nd: TSynFoldNodeInfo;
    l: TLazSynFoldNodeInfoList;
  begin
    l := PasHighLighter.FoldNodeInfo[ALine];

    // use NodeInfoEx
    nd := PasHighLighter.FoldNodeInfo[ALine].NodeInfoEx(AColumn, AFilter, AFoldGroup);
    CheckInfoNode(nd);

    // use filter
    l.ClearFilter;
    l.ActionFilter := AFilter;
    l.GroupFilter := AFoldGroup;
    nd := PasHighLighter.FoldNodeInfo[ALine].Item[AColumn];
    CheckInfoNode(nd);
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
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 1, 1, 1, 3, 0, 1, 2, 1, 2, 2]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program Foo;
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : procedure a;
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,   3, 3,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=2 : {$IFDEF A}
      CheckNode( 2, [], 0,   0,   2, 7,   0, 1,   0, 1,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 3   PasMinLvl=2 EndLvl=3 : begin
      CheckNode( 3, [], 0,   0,   0, 5,   2, 3,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 4   PasMinLvl=3 EndLvl=3 : {$ENDIF}
      CheckNode( 4, [], 0,   0,   2, 7,   1, 0,   1, 0,   18, 18,  3, [sfaClose,sfaFold]);
      //### Foldinfo Line: 5   PasMinLvl=3 EndLvl=4 :   {$IFDEF B} if a then begin {$ENDIF}
      CheckNode( 5, [], 0,   0,   4, 9,   0, 1,   0, 1,   18, 18,  3, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 5, [], 0,   1,   23, 28,   3, 4,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      CheckNode( 5, [], 0,   2,   31, 36,   1, 0,   1, 0,   18, 18,  3, [sfaOneLineClose]);
      //### Foldinfo Line: 6   PasMinLvl=4 EndLvl=4 :     writeln()
      //### Foldinfo Line: 7   PasMinLvl=3 EndLvl=3 :   end;
      CheckNode( 7, [], 0,   0,   2, 5,   4, 3,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 8   PasMinLvl=1 EndLvl=1 : end;
      CheckNode( 8, [], 0,   0,   0, 3,   3, 2,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 8, [], 0,   1,   0, 3,   2, 1,   2, 1,   3, 3,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 9   PasMinLvl=1 EndLvl=2 : begin
      CheckNode( 9, [], 0,   0,   0, 5,   1, 2,   1, 2,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [], 0,   0,   0, 3,   2, 1,   2, 1,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode(10, [], 0,   1,   0, 3,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 11   PasMinLvl=0 EndLvl=0 : //
      CheckNode(11, [], 0,   0,   0, 2,   0, 1,   0, 1,   22, 22,  1, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode(11, [], 0,   1,   2, 2,   1, 0,   1, 0,   22, 22,  1, [sfaOneLineClose,sfaLastLineClose]);
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], []}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [] grp=1}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [], grp=1');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      DebugFoldInfo([],1);

      CheckFoldInfoCounts('', [], 1, [1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 2]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program Foo;
      CheckNode( 0, [], 1,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : procedure a;
      CheckNode( 1, [], 1,   0,   0, 9,   1, 2,   1, 2,   3, 3,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=2 : {$IFDEF A}
      //### Foldinfo Line: 3   PasMinLvl=2 EndLvl=3 : begin
      CheckNode( 3, [], 1,   0,   0, 5,   2, 3,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 4   PasMinLvl=3 EndLvl=3 : {$ENDIF}
      //### Foldinfo Line: 5   PasMinLvl=3 EndLvl=4 :   {$IFDEF B} if a then begin {$ENDIF}
      CheckNode( 5, [], 1,   0,   23, 28,   3, 4,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 6   PasMinLvl=4 EndLvl=4 :     writeln()
      //### Foldinfo Line: 7   PasMinLvl=3 EndLvl=3 :   end;
      CheckNode( 7, [], 1,   0,   2, 5,   4, 3,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 8   PasMinLvl=1 EndLvl=1 : end;
      CheckNode( 8, [], 1,   0,   0, 3,   3, 2,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 8, [], 1,   1,   0, 3,   2, 1,   2, 1,   3, 3,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 9   PasMinLvl=1 EndLvl=2 : begin
      CheckNode( 9, [], 1,   0,   0, 5,   1, 2,   1, 2,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [], 1,   0,   0, 3,   2, 1,   2, 1,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode(10, [], 1,   1,   0, 3,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 11   PasMinLvl=0 EndLvl=0 : //
      CheckNode(11, [], 1,   0,   0, 2,   0, 1,   0, 1,   22, 22,  1, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode(11, [], 1,   1,   2, 2,   1, 0,   1, 0,   22, 22,  1, [sfaOneLineClose,sfaLastLineClose]);
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [] grp=1}

    {%region TEXT 1 -- [cfbtBeginEnd,cfbtIfDef], [] grp=4}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd,cfbtIfDef], [], grp=4');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd,cfbtIfDef], []);
      //DebugFoldInfo([],4);

      CheckFoldInfoCounts('', [], 4, [1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 2]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=0 : program Foo;
      CheckNode( 0, [], 4,   0,   0, 7,   0, 0,   0, 1,   10, 10,  4, [sfaOpen,sfaMarkup]);   // program
      //### Foldinfo Line: 1   PasMinLvl=0 EndLvl=0 : procedure a;
      CheckNode( 1, [], 4,   0,   0, 9,   1, 1,   1, 2,   3, 3,  4, [sfaOpen,sfaMarkup]);   // procedure
      //### Foldinfo Line: 2   PasMinLvl=0 EndLvl=0 : {$IFDEF A}
      //### Foldinfo Line: 3   PasMinLvl=0 EndLvl=0 : begin
      CheckNode( 3, [], 4,   0,   0, 5,   2, 2,   2, 3,   1, 0,  4, [sfaOpen,sfaMarkup]);   // begin
      //### Foldinfo Line: 4   PasMinLvl=0 EndLvl=0 : {$ENDIF}
      //### Foldinfo Line: 5   PasMinLvl=0 EndLvl=1 :   {$IFDEF B} if a then begin {$ENDIF}
      CheckNode( 5, [], 4,   0,   23, 28,   0, 1,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);   //  begin
      //### Foldinfo Line: 6   PasMinLvl=1 EndLvl=1 :     writeln()
      //### Foldinfo Line: 7   PasMinLvl=0 EndLvl=0 :   end;
      CheckNode( 7, [], 4,   0,   2, 5,   1, 0,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);   //  end
      //### Foldinfo Line: 8   PasMinLvl=0 EndLvl=0 : end;
      CheckNode( 8, [], 4,   0,   0, 3,   3, 3,   3, 2,   1, 0,  4, [sfaClose,sfaMarkup]);   // end;
      CheckNode( 8, [], 4,   1,   0, 3,   2, 2,   2, 1,   3, 3,  4, [sfaClose,sfaMarkup]);   // end;
      //### Foldinfo Line: 9   PasMinLvl=0 EndLvl=1 : begin
      CheckNode( 9, [], 4,   0,   0, 5,   0, 1,   1, 2,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);   // begin
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [], 4,   0,   0, 3,   1, 0,   2, 1,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);   // end.
      CheckNode(10, [], 4,   1,   0, 3,   1, 1,   1, 0,   10, 10,  4, [sfaClose,sfaMarkup]);   // end.
      //### Foldinfo Line: 11   PasMinLvl=0 EndLvl=0 : //
      CheckNode(11, [], 4,   0,   0, 2,   0, 0,   0, 1,   22, 22,  4, [sfaOpen]);   // //
      CheckNode(11, [], 4,   1,   2, 2,   1, 1,   1, 0,   22, 22,  4, [sfaClose,sfaLastLineClose]);   // /
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [] grp=4}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaFold]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [sfaFold], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      //DebugFoldInfo([sfaFold]);

      CheckFoldInfoCounts('', [sfaFold], 0, [1, 1, 1, 1, 1, 1, 0, 1, 2, 1, 2, 0]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program Foo;
      CheckNode( 0, [sfaFold], 0,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : procedure a;
      CheckNode( 1, [sfaFold], 0,   0,   0, 9,   1, 2,   1, 2,   3, 3,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=2 : {$IFDEF A}
      CheckNode( 2, [sfaFold], 0,   0,   2, 7,   0, 1,   0, 1,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 3   PasMinLvl=2 EndLvl=3 : begin
      CheckNode( 3, [sfaFold], 0,   0,   0, 5,   2, 3,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 4   PasMinLvl=3 EndLvl=3 : {$ENDIF}
      CheckNode( 4, [sfaFold], 0,   0,   2, 7,   1, 0,   1, 0,   18, 18,  3, [sfaClose,sfaFold]);
      //### Foldinfo Line: 5   PasMinLvl=3 EndLvl=4 :   {$IFDEF B} if a then begin {$ENDIF}
      CheckNode( 5, [sfaFold], 0,   0,   23, 28,   3, 4,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 6   PasMinLvl=4 EndLvl=4 :     writeln()
      //### Foldinfo Line: 7   PasMinLvl=3 EndLvl=3 :   end;
      CheckNode( 7, [sfaFold], 0,   0,   2, 5,   4, 3,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 8   PasMinLvl=1 EndLvl=1 : end;
      CheckNode( 8, [sfaFold], 0,   0,   0, 3,   3, 2,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 8, [sfaFold], 0,   1,   0, 3,   2, 1,   2, 1,   3, 3,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 9   PasMinLvl=1 EndLvl=2 : begin
      CheckNode( 9, [sfaFold], 0,   0,   0, 5,   1, 2,   1, 2,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [sfaFold], 0,   0,   0, 3,   2, 1,   2, 1,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode(10, [sfaFold], 0,   1,   0, 3,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 11   PasMinLvl=0 EndLvl=0 : //

    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaFold]}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      //DebugFoldInfo([sfaMarkup]);

      CheckFoldInfoCounts('', [sfaMarkup], 0, [1, 1, 0, 1, 0, 1, 0, 1, 2, 1, 2, 0]);
      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program Foo;
      CheckNode( 0, [sfaMarkup], 0,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : procedure a;
      CheckNode( 1, [sfaMarkup], 0,   0,   0, 9,   1, 2,   1, 2,   3, 3,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=2 : {$IFDEF A}
      //### Foldinfo Line: 3   PasMinLvl=2 EndLvl=3 : begin
      CheckNode( 3, [sfaMarkup], 0,   0,   0, 5,   2, 3,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 4   PasMinLvl=3 EndLvl=3 : {$ENDIF}
      //### Foldinfo Line: 5   PasMinLvl=3 EndLvl=4 :   {$IFDEF B} if a then begin {$ENDIF}
      CheckNode( 5, [sfaMarkup], 0,   0,   23, 28,   3, 4,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 6   PasMinLvl=4 EndLvl=4 :     writeln()
      //### Foldinfo Line: 7   PasMinLvl=3 EndLvl=3 :   end;
      CheckNode( 7, [sfaMarkup], 0,   0,   2, 5,   4, 3,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 8   PasMinLvl=1 EndLvl=1 : end;
      CheckNode( 8, [sfaMarkup], 0,   0,   0, 3,   3, 2,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 8, [sfaMarkup], 0,   1,   0, 3,   2, 1,   2, 1,   3, 3,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 9   PasMinLvl=1 EndLvl=2 : begin
      CheckNode( 9, [sfaMarkup], 0,   0,   0, 5,   1, 2,   1, 2,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [sfaMarkup], 0,   0,   0, 3,   2, 1,   2, 1,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode(10, [sfaMarkup], 0,   1,   0, 3,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 11   PasMinLvl=0 EndLvl=0 : //

    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone], [sfaMarkup]}

    {%region TEXT 1 -- [cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment]}
      PopPushBaseName('Text 1 -- [cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment], 0');
      SetLines(TestTextFoldInfo1);
      EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment]);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 1, 1, 1, 3, 0, 1, 2, 1, 2, 2, 0]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program Foo;
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=1 : procedure a;
      CheckNode( 1, [], 0,   0,   0, 9,   1, 1,   1, 2,   3, 3,  4, [sfaOpen,sfaMarkup]);
      //### Foldinfo Line: 2   PasMinLvl=1 EndLvl=1 : {$IFDEF A}
      CheckNode( 2, [], 0,   0,   2, 7,   0, 1,   0, 1,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 3   PasMinLvl=1 EndLvl=2 : begin
      CheckNode( 3, [], 0,   0,   0, 5,   1, 2,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 4   PasMinLvl=2 EndLvl=2 : {$ENDIF}
      CheckNode( 4, [], 0,   0,   2, 7,   1, 0,   1, 0,   18, 18,  3, [sfaClose,sfaFold]);
      //### Foldinfo Line: 5   PasMinLvl=2 EndLvl=3 :   {$IFDEF B} if a then begin {$ENDIF}
      CheckNode( 5, [], 0,   0,   4, 9,   0, 1,   0, 1,   18, 18,  3, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 5, [], 0,   1,   23, 28,   2, 3,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      CheckNode( 5, [], 0,   2,   31, 36,   1, 0,   1, 0,   18, 18,  3, [sfaOneLineClose]);
      //### Foldinfo Line: 6   PasMinLvl=3 EndLvl=3 :     writeln()
      //### Foldinfo Line: 7   PasMinLvl=2 EndLvl=2 :   end;
      CheckNode( 7, [], 0,   0,   2, 5,   3, 2,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 8   PasMinLvl=1 EndLvl=1 : end;
      CheckNode( 8, [], 0,   0,   0, 3,   2, 1,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 8, [], 0,   1,   0, 3,   2, 2,   2, 1,   3, 3,  4, [sfaClose,sfaMarkup]);
      //### Foldinfo Line: 9   PasMinLvl=1 EndLvl=2 : begin
      CheckNode( 9, [], 0,   0,   0, 5,   1, 2,   1, 2,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [], 0,   0,   0, 3,   2, 1,   2, 1,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode(10, [], 0,   1,   0, 3,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 11   PasMinLvl=0 EndLvl=0 : //
      CheckNode(11, [], 0,   0,   0, 2,   0, 1,   0, 1,   22, 22,  1, [sfaOpen,sfaFold,sfaFoldHide,sfaOneLineOpen]);
      CheckNode(11, [], 0,   1,   2, 2,   1, 0,   1, 0,   22, 22,  1, [sfaClose,sfaFold,sfaOneLineClose,sfaLastLineClose]);
    {%endregion TEXT 1 -- [cfbtBeginEnd..cfbtNone]-[cfbtProcedure], [cfbtSlashComment]}

  {%endregion TEXT 1}


  {%region TEXT 2}

    {%region TEXT 2 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 2 -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo2);
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 10, 2, 4, 5, 2, 3]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program Foo;
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : procedure a;
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,   3, 3,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=4 : {$IFDEF A} begin {$IFDEF B} repeat a; {$ENDIF} until b; {$IFDEF c} try {$ELSE} //x
      CheckNode( 2, [], 0,   0,   2, 7,   0, 1,   0, 1,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);            // {$IFDEF A}
      CheckNode( 2, [], 0,   1,   11, 16,   2, 3,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);  // begin
      CheckNode( 2, [], 0,   2,   19, 24,   1, 2,   1, 2,   18, 18,  3, [sfaFoldFold,sfaOneLineOpen]);           // {$IFDEF B}
      CheckNode( 2, [], 0,   3,   28, 34,   3, 4,   3, 4,   15, 15,  1, [sfaMarkup,sfaFoldFold,sfaOneLineOpen]); // repeat a;
      CheckNode( 2, [], 0,   4,   40, 45,   2, 1,   2, 1,   18, 18,  3, [sfaOneLineClose]);                      // {$ENDIF}
      CheckNode( 2, [], 0,   5,   47, 52,   4, 3,   4, 3,   15, 15,  1, [sfaMarkup,sfaOneLineClose]);            // until b;
      CheckNode( 2, [], 0,   6,   58, 63,   1, 2,   1, 2,   18, 18,  3, [sfaFoldFold,sfaOneLineOpen]);           // {$IFDEF c}
      CheckNode( 2, [], 0,   7,   67, 70,   3, 4,   3, 4,   13, 13,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);// try
      CheckNode( 2, [], 0,   8,   73, 77,   2, 1,   2, 1,   18, 18,  3, [sfaOneLineClose]);                      // {$ELSE}
      CheckNode( 2, [], 0,   9,   73, 77,   1, 2,   1, 2,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);          // //x
      //### Foldinfo Line: 3   PasMinLvl=4 EndLvl=4 :   //foo
      CheckNode( 3, [], 0,   0,   2, 4,   4, 5,   4, 5,   22, 22,  1, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 3, [], 0,   1,   7, 7,   5, 4,   5, 4,   22, 22,  1, [sfaOneLineClose]);
      //### Foldinfo Line: 4   PasMinLvl=4 EndLvl=5 :   finally repeat x; {$ENDIF C} until y;
      CheckNode( 4, [], 0,   0,   2, 9,   4, 5,   4, 5,   14, 14,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      CheckNode( 4, [], 0,   1,   10, 16,   5, 6,   5, 6,   15, 15,  1, [sfaMarkup,sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 4, [], 0,   2,   22, 27,   2, 1,   2, 1,   18, 18,  3, [sfaClose,sfaFold]);
      CheckNode( 4, [], 0,   3,   31, 36,   6, 5,   6, 5,   15, 15,  1, [sfaMarkup,sfaOneLineClose]);
      //### Foldinfo Line: 5   PasMinLvl=3 EndLvl=3 :   repeat m; until n; end; {$ENDIF A} //
      CheckNode( 5, [], 0,   0,   2, 8,   5, 6,   5, 6,   15, 15,  1, [sfaMarkup,sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 5, [], 0,   1,   12, 17,   6, 5,   6, 5,   15, 15,  1, [sfaMarkup,sfaOneLineClose]);
      CheckNode( 5, [], 0,   2,   21, 24,   5, 4,   5, 4,   14, 14,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 5, [], 0,   3,   21, 24,   4, 3,   4, 3,   13, 13,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 5, [], 0,   4,   28, 33,   1, 0,   1, 0,   18, 18,  3, [sfaClose,sfaFold]);
      //### Foldinfo Line: 6   PasMinLvl=1 EndLvl=1 : end
      CheckNode( 6, [], 0,   0,   0, 3,   3, 2,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 6, [], 0,   1,   0, 3,   2, 1,   2, 1,   3, 3,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 7   PasMinLvl=0 EndLvl=0 : begin end.
      CheckNode( 7, [], 0,   0,   0, 5,   1, 2,   1, 2,   0, 0,  1, [sfaMarkup,sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 7, [], 0,   1,   6, 9,   2, 1,   2, 1,   0, 0,  1, [sfaMarkup,sfaOneLineClose]);
      CheckNode( 7, [], 0,   2,   6, 9,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaMarkup,sfaFold]);
    {%endregion TEXT 2 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 2}


  {%region TEXT 3}

    {%region TEXT 3 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 3 -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo3);
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1, 2, 1, 1, 1, 0, 3, 1, 3, 2]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : Unit Foo;
      CheckNode( 0, [], 0,   0,   0, 4,   0, 1,   0, 1,   11, 11,  1, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : Interface
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,   9, 9,  1, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=2 : type a=Integer;
      CheckNode( 2, [], 0,   0,   0, 4,   2, 3,   2, 3,   5, 5,  1, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 2, [], 0,   1,   15, 15,   3, 2,   3, 2,   5, 5,  1, [sfaOneLineClose]);
      //### Foldinfo Line: 3   PasMinLvl=2 EndLvl=3 : var
      CheckNode( 3, [], 0,   0,   0, 3,   2, 3,   2, 3,   5, 5,  1, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 4   PasMinLvl=2 EndLvl=2 :   b:Integer
      CheckNode( 4, [], 0,   0,   11, 11,   3, 2,   3, 2,   5, 5,  1, [sfaClose,sfaFold]);
      //### Foldinfo Line: 5   PasMinLvl=2 EndLvl=3 : const
      CheckNode( 5, [], 0,   0,   0, 5,   2, 3,   2, 3,   5, 5,  1, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 6   PasMinLvl=3 EndLvl=3 :   c = 1;
      //### Foldinfo Line: 7   PasMinLvl=1 EndLvl=1 :   d = 2; {$IFDEF A}
      CheckNode( 7, [], 0,   0,   11, 16,   0, 1,   0, 1,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);
      CheckNode( 7, [], 0,   1,   19, 19,   3, 2,   3, 2,   5, 5,  1, [sfaClose,sfaFold]);
      CheckNode( 7, [], 0,   2,   19, 19,   2, 1,   2, 1,   9, 9,  1, [sfaClose,sfaFold]);
      //### Foldinfo Line: 8   PasMinLvl=1 EndLvl=2 : Implementation
      CheckNode( 8, [], 0,   0,   0, 14,   1, 2,   1, 2,   9, 9,  1, [sfaOpen,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 9   PasMinLvl=1 EndLvl=1 : //
      CheckNode( 9, [], 0,   0,   0, 2,   2, 3,   2, 3,   22, 22,  1, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 9, [], 0,   1,   2, 2,   3, 2,   3, 2,   22, 22,  1, [sfaOneLineClose]);
      CheckNode( 9, [], 0,   2,   2, 2,   2, 1,   2, 1,   9, 9,  1, [sfaClose,sfaFold]);
      //### Foldinfo Line: 10   PasMinLvl=0 EndLvl=0 : end.
      CheckNode(10, [], 0,   0,   0, 3,   1, 0,   1, 0,   11, 11,  1, [sfaClose,sfaFold]);
      CheckNode(10, [], 0,   1,   4, 4,   1, 0,   1, 0,   18, 18,  3, [sfaClose,sfaFold,sfaLastLineClose]);
    {%endregion TEXT 3 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 3}


  {%region TEXT 4}

    {%region TEXT 4 -- [cfbtBeginEnd..cfbtNone], []}
      PopPushBaseName('Text 4(1) -- [cfbtBeginEnd..cfbtNone], [], 0');
      SetLines(TestTextFoldInfo4(1));
      EnableFolds([cfbtBeginEnd..cfbtNone], []);
      //DebugFoldInfo([]);

      CheckFoldInfoCounts('', [], 0, [1, 1,3, 1, 2, 1, 3]);

      //### Foldinfo Line: 0   PasMinLvl=0 EndLvl=1 : program p;
      CheckNode( 0, [], 0,   0,   0, 7,   0, 1,   0, 1,   10, 10,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 1   PasMinLvl=1 EndLvl=2 : procedure A;
      CheckNode( 1, [], 0,   0,   0, 9,   1, 2,   1, 2,   3, 3,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 2   PasMinLvl=2 EndLvl=4 : begin {$IFDEF} if a then begin
      CheckNode( 2, [], 0,   0,   0, 5,   2, 3,   2, 3,   1, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      CheckNode( 2, [], 0,   1,   8, 13,   0, 1,   0, 1,   18, 18,  3, [sfaOpen,sfaFold,sfaFoldFold]);
      CheckNode( 2, [], 0,   2,   25, 30,   3, 4,   3, 4,   0, 0,  1, [sfaOpen,sfaMarkup,sfaFold,sfaFoldFold]);
      //### Foldinfo Line: 3   PasMinLvl=3 EndLvl=3 :   end; // 2
      CheckNode( 3, [], 0,   0,   2, 5,   4, 3,   4, 3,   0, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 4   PasMinLvl=1 EndLvl=1 : end; // 1
      CheckNode( 4, [], 0,   0,   0, 3,   3, 2,   3, 2,   1, 0,  1, [sfaClose,sfaMarkup,sfaFold]);
      CheckNode( 4, [], 0,   1,   0, 3,   2, 1,   2, 1,   3, 3,  1, [sfaClose,sfaMarkup,sfaFold]);
      //### Foldinfo Line: 5   PasMinLvl=1 EndLvl=1 : {$ENDIF}
      CheckNode( 5, [], 0,   0,   2, 7,   1, 0,   1, 0,   18, 18,  3, [sfaClose,sfaFold]);
      //### Foldinfo Line: 6   PasMinLvl=1 EndLvl=0 : //
      CheckNode( 6, [], 0,   0,   0, 2,   1, 2,   1, 2,   22, 22,  1, [sfaFoldFold,sfaOneLineOpen]);
      CheckNode( 6, [], 0,   1,   2, 2,   2, 1,   2, 1,   22, 22,  1, [sfaOneLineClose,sfaLastLineClose]);
      CheckNode( 6, [], 0,   2,   2, 2,   1, 0,   1, 0,   10, 10,  1, [sfaClose,sfaFold,sfaLastLineClose]);
    {%endregion TEXT 4 -- [cfbtBeginEnd..cfbtNone], []}

  {%region TEXT 4}

end;

initialization

  RegisterTest(TTestHighlighterPas); 
end.


