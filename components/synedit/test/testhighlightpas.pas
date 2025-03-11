unit TestHighlightPas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, TestBase, Forms, LazLoggerBase,
  TestHighlightFoldBase, SynEdit, SynEditTypes, SynHighlighterPas,
  SynEditHighlighterFoldBase, SynEditHighlighter;

type

  // used by Fold / MarkupWord

  { TTestBaseHighlighterPas }

  TTestBaseHighlighterPas = class(TTestBaseHighlighterFoldBase)
  protected
    FKeepAllModifierAttribs: boolean;
    function PasHighLighter: TSynPasSyn;
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    procedure InitTighLighterAttr; override;
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
    FAttrProcName: TSynHighlighterAttributesModifier;
    FCaseLabelAttri: TSynHighlighterAttributesModifier;
    procedure ReCreateEdit; override;

    function TestTextFoldInfo1: TStringArray;
    function TestTextFoldInfo2: TStringArray;
    function TestTextFoldInfo3: TStringArray;
    function TestTextFoldInfo4(AIfCol: Integer): TStringArray;
    function TestTextFoldInfo5: TStringArray;
    procedure CheckTokensForLine(Name: String; LineIdx: Integer; ExpTokens: Array of TExpTokenInfo); reintroduce;

  published
    procedure TestFoldInfo;
    procedure TestExtendedKeywordsAndStrings;
    procedure TestRaiseAt;
    procedure TestContextForProcModifiers;
    procedure TestContextForProcModifiers2;
    procedure TestContextForProcModifiersName;
    procedure TestContextForVarModifiers;
    procedure TestContextForProperties;
    procedure TestContextForProcedure;
    procedure TestContextForProcedureNameAttr;
    procedure TestContextForInterface;
    procedure TestContextForDeprecated;
    procedure TestContextForClassObjRecHelp;
    procedure TestContextForClassSection;
    procedure TestContextForClassModifier; // Sealed abstract
    procedure TestContextForClassOf;
    procedure TestContextForClassProcModifier; // virtual override final reintroduce
    procedure TestContextForClassHelper;
    procedure TestContextForTypeHelper;
    procedure TestContextForClassFunction; // in class,object,record
    procedure TestContextForRecordHelper;
    procedure TestContextForRecordCase;
    procedure TestContextForStatic;
    procedure TestCaseLabel;
    procedure TestModifierAttributesForProcedure;
    procedure TestModifierAttributesForProperty;
    procedure TestModifierAttributesForVarConstType;
    procedure TestModifierAttributesForLabel;
    procedure TestCaretAsString;
    procedure TestFoldNodeInfo;
  end;

implementation

const
  TK_Comma   = tkSymbol;
  TK_Semi    = tkSymbol;
  TK_Dot     = tkSymbol;
  TK_Colon   = tkSymbol;
  TK_Equal   = tkSymbol;
  TK_Bracket = tkSymbol;

type
  TNoMergedTokenAttriIndicator = (PlainAttr);

operator := (a: TtkTokenKind) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpKind := ord(a);
  result.Flags := [etiKind];
end;

operator + (a: TtkTokenKind; b: TSynHighlighterAttributes) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpKind := ord(a);
  result.ExpAttr := b;
  result.Flags := [etiKind, etiAttr];
end;

{ TTestBaseHighlighterPas }

function TTestBaseHighlighterPas.PasHighLighter: TSynPasSyn;
begin
  Result := TSynPasSyn(FTheHighLighter);
end;

function TTestBaseHighlighterPas.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynPasSyn.Create(nil);
end;

procedure TTestBaseHighlighterPas.InitTighLighterAttr;
begin
  inherited InitTighLighterAttr;

  if FKeepAllModifierAttribs then exit;

  PasHighLighter.PropertyNameAttr.Clear;
  PasHighLighter.ProcedureHeaderParamAttr.Clear;
  PasHighLighter.ProcedureHeaderTypeAttr.Clear;
  PasHighLighter.ProcedureHeaderValueAttr.Clear;
  PasHighLighter.ProcedureHeaderResultAttr.Clear;
  PasHighLighter.DeclarationVarConstNameAttr.Clear;
  PasHighLighter.DeclarationTypeNameAttr.Clear;
  PasHighLighter.DeclarationTypeAttr.Clear;
  PasHighLighter.DeclarationValueAttr.Clear;
  PasHighLighter.GotoLabelAttr.Clear;
  PasHighLighter.StructMemberAttr.Clear;
end;

procedure TTestBaseHighlighterPas.EnableFolds(AEnbledTypes: TPascalCodeFoldBlockTypes;
  AHideTypes: TPascalCodeFoldBlockTypes; ANoFoldTypes: TPascalCodeFoldBlockTypes);
var
  i: TPascalCodeFoldBlockType;
begin
  PasHighLighter.BeginUpdate;
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
  PasHighLighter.EndUpdate;
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

procedure TTestHighlighterPas.ReCreateEdit;
begin
  inherited ReCreateEdit;
  FAttrProcName := PasHighLighter.ProcedureHeaderName;
  FCaseLabelAttri := PasHighLighter.CaseLabelAttri;
end;

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

procedure TTestHighlighterPas.CheckTokensForLine(Name: String;
  LineIdx: Integer; ExpTokens: array of TExpTokenInfo);
var
  i: Integer;
begin
  for i := low(ExpTokens) to high(ExpTokens) do begin
    if ExpTokens[i].Flags * [etiAttr, etiKind] = [etiKind] then begin
      case TtkTokenKind(ExpTokens[i].ExpKind) of
        tkIdentifier: ExpTokens[i].ExpAttr := PasHighLighter.IdentifierAttri;
        tkKey:        ExpTokens[i].ExpAttr := PasHighLighter.KeywordAttribute;
        tkModifier:   ExpTokens[i].ExpAttr := PasHighLighter.ModifierAttri;
        tkSymbol:     ExpTokens[i].ExpAttr := PasHighLighter.SymbolAttri;
        tkString:     ExpTokens[i].ExpAttr := PasHighLighter.StringAttri;
        tkNumber:     ExpTokens[i].ExpAttr := PasHighLighter.NumberAttri;
        tkSpace:      ExpTokens[i].ExpAttr := PasHighLighter.SpaceAttri;
        tkComment:    ExpTokens[i].ExpAttr := PasHighLighter.CommentAttri;
        else          ExpTokens[i].ExpAttr := nil;
      end;
      if ExpTokens[i].ExpAttr <> nil then
        ExpTokens[i].Flags := ExpTokens[i].Flags + [etiAttr];
    end;
  end;
  inherited CheckTokensForLine(Name, LineIdx, ExpTokens);
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
  SetLines(TestTextFoldInfo1);
  EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtBeginEnd]);
  PopPushBaseName('Text 1 all folds, except cfbtBeginEnd');

  AssertEquals('Len Prog',  10, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',   7, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len IF A',   2, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len Begin',  5, PasHighLighter.FoldLineLength(3,0));
  AssertEquals('Len if beg (not avail)', -1, PasHighLighter.FoldLineLength(5,0));
  //AssertEquals('Len PrgBeg', 1, PasHighLighter.FoldLineLength(9,0));

  AssertEquals('Len invalid', -1, PasHighLighter.FoldLineLength(4,0)); // endif
  AssertEquals('Len // (no hide)', -1, PasHighLighter.FoldLineLength(11,0));

  //                       Pg pc $I bg $E be w  e  e be  e  //
  CheckFoldOpenCounts('', [1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0]);
  {%endregion}

  {%region}
  SetLines(TestTextFoldInfo1);
  EnableFolds([cfbtBeginEnd..cfbtNone]-[cfbtBeginEnd,cfbtTopBeginEnd]);
  PopPushBaseName('Text 1 all folds, except cfbtBeginEnd,cfbtTopBeginEnd');

  AssertEquals('Len Prog',  10, PasHighLighter.FoldLineLength(0,0));
  AssertEquals('Len Proc',   7, PasHighLighter.FoldLineLength(1,0));
  AssertEquals('Len IF A',   2, PasHighLighter.FoldLineLength(2,0));
  AssertEquals('Len Begin (not avail)',  -1, PasHighLighter.FoldLineLength(3,0));
  AssertEquals('Len if beg (not avail)', -1, PasHighLighter.FoldLineLength(5,0));
  //AssertEquals('Len PrgBeg', 1, PasHighLighter.FoldLineLength(9,0));

  AssertEquals('Len // (no hide)', -1, PasHighLighter.FoldLineLength(11,0));

  //                       Pg pc $I bg $E be w  e  e be  e  //
  CheckFoldOpenCounts('', [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
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

procedure TTestHighlighterPas.TestRaiseAt;
begin
  ReCreateEdit;
  SetLines
    ([ 'program A;',
       'begin',
       '  raise foo at 1;',  // 2
       '  raise at AT at;',
       '  raise at + at AT at.at;',  // 4
       '  raise at.at at AT + at;',
       '  raise at(at, at) AT at + at;', // 6
       '  raise at(at, at).at AT at + at;',
       '  raise at(at, at) + at AT at + at;',
       '',
       'end.'
    ]);

    CheckTokensForLine('foo at 1',  2,
      [ tkSpace, tkKey, tkSpace,  tkIdentifier, tkSpace,
        tkKey{at}, tkSpace, tkNumber, TK_Semi ]);
    CheckTokensForLine('at AT at',  3,
      [ tkSpace, tkKey, tkSpace,  tkIdentifier, tkSpace,
        tkKey{at}, tkSpace, tkIdentifier, TK_Semi ]);
    CheckTokensForLine('at + at AT at.at',  4,
      [ tkSpace, tkKey, tkSpace,  tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, tkSpace,
        tkKey{at}, tkSpace, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi ]);
    CheckTokensForLine('at.at AT at + at',  5,
      [ tkSpace, tkKey, tkSpace,  tkIdentifier, TK_Dot, tkIdentifier, tkSpace,
        tkKey{at}, tkSpace, tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, TK_Semi ]);
    CheckTokensForLine('at(at, at) AT at + at',  6,
      [ tkSpace, tkKey, tkSpace, tkIdentifier,TK_Bracket, tkIdentifier, TK_Comma, tkSpace, tkIdentifier, TK_Bracket, tkSpace,
        tkKey{at}, tkSpace, tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, TK_Semi ]);
    CheckTokensForLine('at(at, at).at AT at + at',  7,
      [ tkSpace, tkKey, tkSpace, tkIdentifier,TK_Bracket, tkIdentifier, TK_Comma, tkSpace, tkIdentifier, TK_Bracket, TK_Dot, tkIdentifier, tkSpace,
        tkKey{at}, tkSpace, tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, TK_Semi ]);
    CheckTokensForLine('at(at, at) + at AT at + at',  8,
      [ tkSpace, tkKey, tkSpace, tkIdentifier,TK_Bracket, tkIdentifier, TK_Comma, tkSpace, tkIdentifier, TK_Bracket, tkSpace, tkSymbol, tkSpace, tkIdentifier, tkSpace,
        tkKey{at}, tkSpace, tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, TK_Semi ]);

end;

procedure TTestHighlighterPas.TestContextForProcModifiers;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $10-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    if (i and $08) = 0 then AFolds := AFolds - [cfbtClassSection];

    {%region message modifier for procedure}
      ReCreateEdit;
      EnableFolds(AFolds);
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
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol,        // "Procedure",  " ", "message", "("
        tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "message",, ":", " ", "message"
        tkSymbol, tkSymbol, tkSpace,                   // ")", ";", " "
        tkModifier, // "message" as key
        tkSpace, tkNumber, tkSymbol
      ]);
    CheckTokensForLine('property',  4,
      [ tkKey, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkKey, tkSpace, tkIdentifier ]);

    CheckTokensForLine('var',  7,
      [ tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol ]);
    CheckTokensForLine('procedure',  8,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol,        // "Procedure",  " ", "message", "("
        tkIdentifier, tkSymbol, tkSpace, tkIdentifier, // "message",, ":", " ", "message"
        tkSymbol, tkSymbol                             // ")", ";"
      ]);

    {%endregion}
  end;
end;

procedure TTestHighlighterPas.TestContextForProcModifiers2;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i, j: Integer;
  n: String;
begin
  ReCreateEdit;
  for i := 0 to $10-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    if (i and $08) = 0 then AFolds := AFolds - [cfbtProcedure];

    EnableFolds(AFolds);
    SetLines
      ([ 'Unit A; interface',
         'type',
         'cdecl=function(cdecl:cdecl):cdecl;cdecl;',
         'type',
         'Stdcall=class(cdecl)',
         'function Stdcall(Stdcall:Stdcall):Stdcall;Stdcall;deprecated;',
         'property cdecl:cdecl read cdecl;',
         'end;',
         '',
         'cdecl=record',
         'function cdecl(cdecl:cdecl):cdecl;cdecl;deprecated;',
         'end;',
         '',
         'var',
         'Stdcall:function(cdecl:cdecl):cdecl;cdecl;',
         'var',
         'cdecl:cdecl;',
         '',
         'function Stdcall(cdecl:cdecl):cdecl;cdecl;',
         'var',
         'cdecl:cdecl deprecated;',
         'function Stdcall(cdecl:cdecl):cdecl;cdecl;deprecated;',
         ''
      ]);

    CheckTokensForLine('type cdecl',  2,
      [ tkIdentifier, TK_Equal, tkKey,  // cdecl=function
        TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket,  // (cdecl:cdecl)
        TK_Colon, tkIdentifier, TK_Semi, tkModifier, TK_Semi // :cdecl;cdecl;
      ]);

    CheckTokensForLine('StdCall=class',  4,
      [ tkIdentifier, TK_Equal, tkKey,  // Stdcall=class
        TK_Bracket, tkIdentifier, TK_Bracket  // (cdecl)
      ]);

    CheckTokensForLine('function/method cdecl',  5,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  // function cdecl
        TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket,  // (cdecl:cdecl)
        TK_Colon, tkIdentifier, TK_Semi, tkModifier, TK_Semi, // :cdecl;cdecl;
        tkModifier, TK_Semi //deprecated;
      ]);

    CheckTokensForLine('property',  6,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkKey, tkSpace, tkIdentifier, TK_Semi ]);

    CheckTokensForLine('StdCall=record',  9,
      [ tkIdentifier, TK_Equal, tkKey  // Stdcall=record
      ]);

    CheckTokensForLine('funciton in recorld',  10,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  // function cdecl
        TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket,  // (cdecl:cdecl)
        TK_Colon, tkIdentifier, TK_Semi, tkModifier, TK_Semi, // :cdecl;cdecl;
        tkModifier, TK_Semi //deprecated;
      ]);

    CheckTokensForLine('var cdecl function',  14,
      [ tkIdentifier, TK_Equal, tkKey,  // Stdcall:function
        TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket,  // (cdecl:cdecl)
        TK_Colon, tkIdentifier, TK_Semi, tkModifier, TK_Semi // :cdecl;cdecl;
      ]);

    CheckTokensForLine('var cdecl:cdecl',  16,
      [ tkIdentifier, TK_Colon, tkIdentifier, TK_Semi  //cdecl:cdecl;
      ]);

    CheckTokensForLine('function',  18,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  // function StdCall
        TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket,  // (cdecl:cdecl)
        TK_Colon, tkIdentifier, TK_Semi, tkModifier, TK_Semi // :cdecl;cdecl;
      ]);

    CheckTokensForLine('var cdecl deprecated:cdecl',  20,
      [ tkIdentifier, TK_Colon, tkIdentifier,   //cdecl:cdecl
        tkSpace, tkModifier, TK_Semi //deprecated;
      ]);

    CheckTokensForLine('function deprecated',  21,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  // function StdCall
        TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket,  // (cdecl:cdecl)
        TK_Colon, tkIdentifier,TK_Semi, // :cdecl;
        tkModifier, TK_Semi, // cdecl;
        tkModifier, TK_Semi //deprecated;
      ]);


    for j := 0 to 6 do begin
      case j of
        0: n := 'overload';
        1: n := 'assembler';
        2: n := 'alias';
        3: n := 'inline';
        4: n := 'weakexternal';
        5: n := 'compilerproc';
        6: n := 'forward';
      end;

      SetLines
      ([ 'Unit A; interface',
         'var',
         n+':function:'+n+';'+n+':'+n+';',
         'type',
         n+'=function:'+n+';'+n+':'+n+';',
         'function '+n+':'+n+';'+n+';',
         'type',
         n+'=class('+n+') public',
         n+':function:'+n+';'+n+':'+n+';',
         'function '+n+':'+n+';'+n+';',
         'end;',
         ''
      ]);

       CheckTokensForLine(n+':function:'+n+';'+n+':'+n+';',2,
          [ tkIdentifier, TK_Colon, tkKey, TK_Colon, tkIdentifier, TK_Semi,
            tkIdentifier, TK_Colon, tkIdentifier, TK_Semi
          ]);
       CheckTokensForLine(n+'=function:'+n+';'+n+':'+n+';',4,
          [ tkIdentifier, TK_Colon, tkKey, TK_Colon, tkIdentifier, TK_Semi,
            tkIdentifier, TK_Colon, tkIdentifier, TK_Semi
          ]);
       CheckTokensForLine('function '+n+':'+n+';'+n+';',5,
          [ tkKey, tkSpace, tkIdentifier+FAttrProcName, TK_Colon, tkIdentifier, TK_Semi,
            tkModifier, TK_Semi
          ]);

       CheckTokensForLine(n+':function:'+n+';'+n+':'+n+';',8,
          [ tkIdentifier, TK_Colon, tkKey, TK_Colon, tkIdentifier, TK_Semi,
            tkIdentifier, TK_Colon, tkIdentifier, TK_Semi
          ]);
       if j = 6 then
         continue;
       CheckTokensForLine('function '+n+':'+n+';'+n+';',9,
          [ tkKey, tkSpace, tkIdentifier+FAttrProcName, TK_Colon, tkIdentifier, TK_Semi,
            tkModifier, TK_Semi
          ]);

    end;

  end;
end;

procedure TTestHighlighterPas.TestContextForProcModifiersName;
var
  p: TSynHighlighterAttributesModifier;
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  ReCreateEdit;
  p := FAttrProcName;
  SetLines
    ([ 'Unit A; interface',
       'procedure name; external ''name'' name ''name'';',
       'procedure name; public name ''name'';',
       '  begin end;',
       'function name: name; external ''name'' name ''name'';',
       'function name: name; public name ''name'';',
       '  begin end;',
       '',
       'type TFoo = class ',
       'procedure name; public name: name;',  // just a public field
       'function name: name; public name: name;',  // just a public field
       'end;',
       ''
        ]);

  for i := 0 to $3F do begin
    AFolds := [];
    if (i and $20) = 0 then AFolds := [cfbtBeginEnd..cfbtNone] - [cfbtUnitSection, cfbtProcedure, cfbtVarBlock, cfbtClass, cfbtClassSection];
    if (i and $01) = 0 then AFolds := AFolds + [cfbtUnitSection];
    if (i and $02) = 0 then AFolds := AFolds + [cfbtProcedure];
    if (i and $04) = 0 then AFolds := AFolds + [cfbtVarBlock];
    if (i and $08) = 0 then AFolds := AFolds + [cfbtClass];
    if (i and $10) = 0 then AFolds := AFolds + [cfbtClassSection];

    EnableFolds(AFolds);

    CheckTokensForLine('procedure name; external ''name'' name ''name'';', 1,
      [tkKey, tkSpace, tkIdentifier+p, TK_Semi, tkSpace,
       tkModifier, tkSpace, tkString, tkSpace, tkModifier, tkSpace, tkString, TK_Semi]);
    CheckTokensForLine('procedure name; public name ''name'';', 2,
      [tkKey, tkSpace, tkIdentifier+p, TK_Semi, tkSpace,
       tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi]);

    CheckTokensForLine('function name: name; external ''name'' name ''name'';', 4,
      [tkKey, tkSpace, tkIdentifier+p, TK_Colon, tkSpace, tkIdentifier, TK_Semi, tkSpace,
       tkModifier, tkSpace, tkString, tkSpace, tkModifier, tkSpace, tkString, TK_Semi]);
    CheckTokensForLine('function name: name; public name ''name'';', 5,
      [tkKey, tkSpace, tkIdentifier+p, TK_Colon, tkSpace, tkIdentifier, TK_Semi, tkSpace,
       tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi]);

    CheckTokensForLine('CLASS: procedure name; public name: name;', 9,
      [tkKey, tkSpace, tkIdentifier+p, TK_Semi, tkSpace,
       tkKey, tkSpace, tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
    CheckTokensForLine('CLASS: function name: name; public name: name;', 10,
      [tkKey, tkSpace, tkIdentifier+p, TK_Colon, tkSpace, tkIdentifier, TK_Semi, tkSpace,
       tkKey, tkSpace, tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);

  end;
end;

procedure TTestHighlighterPas.TestContextForVarModifiers;
var
  n: String;
  AFolds: TPascalCodeFoldBlockTypes;
  i, j: Integer;
begin
  ReCreateEdit;
  for i := 0 to 7 do begin
    case i of
      0: n := 'name';
      1: n := 'public';
      2: n := 'external';
      3: n := 'export';
      4: n := 'cvar';
      5: n := 'deprecated';
      6: n := 'default';
      7: n := 'absolute';
    end;

    SetLines
      ([ 'Unit A; interface',
         '',
         'var ',
         // Line 3:
         n+':'+n+'; public;',
         n+':'+n+'; public name ''name'';',
         n+':'+n+'; external;',
         n+':'+n+'; external ''name'';',
         n+':'+n+'; external name ''name'';',
         n+':'+n+'; external ''name'' name ''name'';',
         n+':'+n+'; export;',
         n+':'+n+'; export name ''name'';',
         '',
         '',
         // Line 13:
         n+':'+n+';cvar; public;',
         n+':'+n+';cvar; public name ''name'';',
         n+':'+n+';cvar; external;',
         n+':'+n+';cvar; external ''name'';',
         '',//n+':'+n+';cvar; external name ''name'';',
         '',//n+':'+n+';cvar; external ''name'' name ''name'';',
         n+':'+n+';cvar; export;',
         n+':'+n+';cvar; export name ''name'';',
         n+':'+n+';cvar; cvar: cvar; name: name; var',                         // just another variable
         '',
         // Line 23:
         n+':'+n+'=1; public;',
         n+':'+n+'=1; public name ''name'';',
         '',//n+':'+n+'=1; external;',
         '',//n+':'+n+'=1; external ''name'';',
         '',//n+':'+n+'=1; external name ''name'';',
         '',//n+':'+n+'=1; external ''name'' name ''name'';',
         n+':'+n+'=1; export;',
         n+':'+n+'=1; export name ''name'';',
         '',
         '',
         // Line 33:
         n+':'+n+' deprecated; public;',
         n+':'+n+' deprecated; public name ''name'';',
         n+':'+n+' deprecated; external;',
         n+':'+n+' deprecated; external ''name'';',
         n+':'+n+' deprecated; external name ''name'';',
         n+':'+n+' deprecated; external ''name'' name ''name'';',
         n+':'+n+' deprecated; export;',
         n+':'+n+' deprecated; export name ''name'';',
         '',
         '',
         // Line 43:
         n+':'+n+' absolute '+n+';',
         '',
         '',
         '',
         'type',
         // Line 48:
         n+'='+n+'; '+n+'='+n+';',  // just another type
         'const',
         // Line 50:
         n+'='+n+'; '+n+'='+n+';',  // just another const
         n+':'+n+'='+n+'; cvar;',   // key CVAR
         n+':'+n+'='+n+'; cvar; public;',   // key CVAR
         n+':'+n+'='+n+'; public;',   // key public
         n+':'+n+'='+n+'; public name ''name'';',   // key public name
         '',
         '',
         '',
         '',
         // NOT for "public"
         'type TFoo = class ',
         // Line 60:
         n+':'+n+'; '+n+':'+n+'; public private',  // just another field
         n+':'+n+'; public '+n+':'+n+'; public private',  // just another public field
         'var '+n+':'+n+'; public '+n+':'+n+'; public private',  // just another public field
         'type '+n+':'+n+'; '+n+':'+n+'; public private',  // just another type
         'const '+n+':'+n+'='+n+'; '+n+':'+n+'='+n+'; public private',  // just another const
         '',
         'end;',
         ''
          ]);

    for j := 0 to $1F do begin
      AFolds := [];
      if (j and $10) = 0 then AFolds := [cfbtBeginEnd..cfbtNone] - [cfbtUnitSection, cfbtVarBlock, cfbtClass, cfbtClassSection];
      if (j and $01) = 0 then AFolds := AFolds + [cfbtUnitSection];
      if (j and $02) = 0 then AFolds := AFolds + [cfbtVarBlock];
      if (j and $04) = 0 then AFolds := AFolds + [cfbtClass];
      if (j and $08) = 0 then AFolds := AFolds + [cfbtClassSection];
      EnableFolds(AFolds);

      CheckTokensForLine(n+':'+n+'; public;', 3,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; public name ''name'';', 4,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; external;', 5,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; external ''name'';', 6,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; external name ''name'';', 7,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; external ''name'' name ''name'';', 8,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkString, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; export;', 9,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'; export name ''name'';', 10,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);

      CheckTokensForLine(n+':'+n+';cvar; public;', 13,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+';cvar; public name ''name'';', 14,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+';cvar; external;', 15,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+';cvar; external ''name'';', 16,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkString, TK_Semi
       ]);
//      CheckTokensForLine(n+':'+n+';cvar; external name ''name'';', 17,
//        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
//       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,     tkModifier {cvar}, TK_Semi, tkSpace,
//       ]);
//      CheckTokensForLine(n+':'+n+';cvar; external ''name'' name ''name'';', 18,
//       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,     tkModifier {cvar}, TK_Semi, tkSpace,
//        tkModifier, tkSpace, tkString, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
//       ]);
      CheckTokensForLine(n+':'+n+';cvar; export;', 19,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+';cvar; export name ''name'';', 20,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      // just another var:
      CheckTokensForLine(n+':'+n+';cvar; cvar: cvar; name: name; var', 21,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,      tkModifier {cvar}, TK_Semi, tkSpace,
        tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Semi, tkSpace,
        tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Semi,
        tkSpace, tkkey
       ]);

      CheckTokensForLine(n+':'+n+'=1; public;', 23,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'=1; public name ''name'';', 24,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
//      CheckTokensForLine(n+':'+n+'=1; external;', 25,
//       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
//        tkModifier, TK_Semi
//       ]);
//      CheckTokensForLine(n+':'+n+'=1; external ''name'';', 26,
//       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
//        tkModifier, tkSpace, tkString, TK_Semi
//       ]);
//      CheckTokensForLine(n+':'+n+'=1; external name ''name'';', 27,
//       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
//        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
//       ]);
//      CheckTokensForLine(n+':'+n+'=1; external ''name'' name ''name'';', 28,
//       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
//        tkModifier, tkSpace, tkString, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
//       ]);
      CheckTokensForLine(n+':'+n+'=1; export;', 29,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'=1; export name ''name'';', 30,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkNumber, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);

      CheckTokensForLine(n+':'+n+' deprecated; public;', 33,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; public name ''name'';', 34,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; external;', 35,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; external ''name'';', 36,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; external name ''name'';', 37,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; external ''name'' name ''name'';', 38,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkString, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; export;', 39,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+' deprecated; export name ''name'';', 40,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{depr}, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);

      CheckTokensForLine(n+':'+n+' absolute '+n+';', 43,
       [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier{absolute}, tkSpace, tkIdentifier, TK_Semi ]);

       //TYPE / just another type
      CheckTokensForLine(n+'='+n+'; '+n+'='+n+';', 48,
       [tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkIdentifier, TK_Equal, tkIdentifier, TK_Semi]);

      // const
      CheckTokensForLine(n+'='+n+'; '+n+'='+n+';', 50,   // just another const
       [tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkIdentifier, TK_Equal, tkIdentifier, TK_Semi]);
      CheckTokensForLine(n+':'+n+'='+n+'; cvar;', 51,    // key CVAR
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);

      if copy(n,1,6) = 'public' then
        continue;
      // NOT for "public"
      CheckTokensForLine(n+':'+n+'='+n+'; cvar; public;', 52,   // key CVAR
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'='+n+'; public;', 53,    // key public
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, TK_Semi
       ]);
      CheckTokensForLine(n+':'+n+'='+n+'; public name ''name'';', 54,   // key public name
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkModifier, tkSpace, tkModifier, tkSpace, tkString, TK_Semi
       ]);

      // NOT for "public"
      CheckTokensForLine('#CLASS#'+ n+':'+n+'; '+n+':'+n+'; public private', 60,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
        tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace, tkKey {private}
       ]);
      CheckTokensForLine('#CLASS#'+n+':'+n+'; public '+n+':'+n+'; public private', 61,
       [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace,
        tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace, tkKey {private}
       ]);
      CheckTokensForLine('#CLASS#'+'var '+n+':'+n+'; public '+n+':'+n+'; public private', 62,
       [tkKey{var}, tkSpace,
        tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace,
        tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace, tkKey {private}
       ]);
      CheckTokensForLine('#CLASS#'+'type '+n+'='+n+'; '+n+'='+n+'; public private', 63,
       [tkKey{type}, tkSpace,
        tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace, tkKey {private}]);
      CheckTokensForLine('#CLASS#'+'const '+n+':'+n+'='+n+'; '+n+':'+n+'='+n+'; public private',  64,   // just another const
       [tkKey{const}, tkSpace,
        tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace,
        tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkIdentifier, TK_Semi, tkSpace, tkKey {public}, tkSpace, tkKey {private}]);

    end;
  end;
end;

procedure TTestHighlighterPas.TestContextForProperties;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $10-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    if (i and $08) = 0 then AFolds := AFolds - [cfbtClassSection];

    {%region property and index}
      ReCreateEdit;
      EnableFolds(AFolds);
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
      EnableFolds(AFolds);
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
      EnableFolds(AFolds);
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

    {%region property and default}
      ReCreateEdit;
      EnableFolds(AFolds);
      SetLines
        ([ 'Unit A; interface',
           'type TFoo = class',
           'default,default:default;',
           'private type',
           'default=integer;',
           'private',
           'a: default;',
           'default:default.default;',
   {8}     'function default(default:default):default;',
   {9}     'function default(default:default.default):default.default;',
  {10}     'property default[default:default]:default read default write default; default;',
  {11}     'property default:default read default default default;',
  {12}     'property default:default index default read default default default-default+default;',
           // property could read a field inside an embedded record
  {13}     'property default:default.default index {C} default.default read {C} default.default {C} default default.default * default.default;',
  {14}     'property default: default index not default.default read default default -default.default;',
  {15}     'property default: default.default index specialize default<default, default>.default read default default default.default;',
  {16}     'property default: specialize default<default, default> index specialize default<default, default>.default read default default specialize default<default, default>.default;',
           ''
        ]);

    CheckTokensForLine('FIELD: default,default:default;',  2,
      [ tkIdentifier, TK_Comma, tkIdentifier,    // default , default
        TK_Colon, tkIdentifier, TK_Semi                   // : default;
      ]);

    CheckTokensForLine('TYPE: default=integer;',  4,
      [ tkIdentifier, TK_Equal, tkIdentifier, TK_Semi // default = integer ;
      ]);

    CheckTokensForLine('FIELD: default:default.default;',  7,
      [ tkIdentifier, TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi   // default : default . default ;
      ]);

    CheckTokensForLine('function default(default:default):default;',  8,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,                         // function default
        TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,  // ( default : default )
        TK_Colon, tkIdentifier, TK_Semi                       // : default;
      ]);

    CheckTokensForLine('function default(default:default.default):default.default;',  9,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,                           // function default
        TK_Bracket, tkIdentifier, TK_Colon,                     // ( default :
        tkIdentifier, TK_Dot, tkIdentifier, TK_Bracket,         // default . default )
        TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi  // : default . default;
      ]);

    CheckTokensForLine('property default[default:default]:default read default write default; default;',  10,
      [ tkKey, tkSpace, tkIdentifier, TK_Bracket, tkIdentifier,     // property default[default
        TK_Colon, tkIdentifier, TK_Bracket, TK_Colon, tkIdentifier, // :default]:default
        tkSpace, tkKey, tkSpace, tkIdentifier,  // read default
        tkSpace, tkKey, tkSpace, tkIdentifier,  // write default
        TK_Semi, tkSpace, tkModifier, TK_Semi // ; default;
      ]);

    CheckTokensForLine('property default:default read default default default;',  11,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkIdentifier,   //property default:default
        tkSpace, tkKey, tkSpace, tkIdentifier, // read default
        tkSpace, tkKey, tkSpace, tkIdentifier, TK_Semi  // default default;
      ]);

    CheckTokensForLine('property default:default index default read default default default-default+default;',  12,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkIdentifier,  // property default:default
        tkSpace, tkKey, tkSpace, tkIdentifier, // index default
        tkSpace, tkKey, tkSpace, tkIdentifier, // read default
        tkSpace, tkKey, tkSpace, tkIdentifier, // default default
        tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, TK_Semi // -default+default;
      ]);

    CheckTokensForLine('property default:default.default index {C} default.default read {C} default.default {C} default default.default * default.default;',  13,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkIdentifier, TK_Dot, tkIdentifier,  // property default:default.default
        tkSpace, tkKey, tkSpace, tkComment, tkSpace, // index (C}
        tkIdentifier, TK_Dot, tkIdentifier, tkSpace, // default.default
        tkKey, tkSpace, tkComment, tkSpace,          // read (C}
        tkIdentifier, TK_Dot, tkIdentifier, tkSpace, // default.default
        tkComment, tkSpace, // (C}
        tkKey, tkSpace, tkIdentifier, TK_Dot, tkIdentifier, tkSpace,  // default default.default
        tkSymbol, tkSpace, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi // * default.default;
      ]);

    CheckTokensForLine('property default: default index not default.default read default default -default.default;',  14,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkSpace, tkIdentifier, tkSpace,  // property default:default
        tkKey, tkSpace, tkKey, tkSpace, tkIdentifier, TK_Dot, tkIdentifier, tkSpace, // index not default.default
        tkKey, tkSpace, tkIdentifier, tkSpace,  //read default
        tkKey, tkSpace, tkSymbol, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi // default -default.default;
      ]);

    CheckTokensForLine('property default: default.default index specialize default<default, default>.default read default default default.default;',  15,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Dot, tkIdentifier, tkSpace,  // property default: default.default
        tkKey, tkSpace, tkKey, tkSpace, // index specialize
        tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol,  //default<default, default>
        tkSymbol, tkIdentifier, tkSpace,  // .default
        tkKey, tkSpace, tkIdentifier, tkSpace,  // read default
        tkKey, tkSpace, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi  // default default.default;
      ]);

    CheckTokensForLine('property default: specialize default<default, default> index specialize default<default, default>.default read default default specialize default<default, default>.default;',  16,
      [ tkKey, tkSpace, tkIdentifier, TK_Colon, tkSpace,  // property default:
        tkKey, tkSpace, //specialize
        tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace,  //default<default, default>
        tkKey, tkSpace, tkKey, tkSpace,  // index specialize
        tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol,  // default<default, default>
        tkSymbol, tkIdentifier, tkSpace, // .default
        tkKey, tkSpace, tkIdentifier, tkSpace, // read default
        tkKey, tkSpace, tkKey, tkSpace, // default specialize
        tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, // default<default, default>
        tkSymbol, tkIdentifier, TK_Semi // .default;
      ]);






    {%endregion}
  end;
end;

procedure TTestHighlighterPas.TestContextForProcedure;
var
  AtP, AtI, AtK: TSynHighlighterAttributes;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  ReCreateEdit;
  AtP := PasHighLighter.ProcedureHeaderName;
  AtI := PasHighLighter.IdentifierAttri;
  AtK := PasHighLighter.KeywordAttribute;

  SetLines
    ([ 'Unit A;',
       'interface',
       '',
       'type',
       '  IBar = interface',
{5}    '     procedure p1;',
       '     procedure p2;',
       '  end;',
       '',
       'var',
{10}   '  Foo: Procedure of object;', // no folding // do not end var block
       '',
       'type',
       '  TBar= ',
       '    Function(): Boolean;',  // no folding // do not end type block
{15}   '',
       'Procedure a;', // no folding in interface
       '',
       'implementation',
       '',
{20}   'var',
       '  Foo2: Procedure of object;', // no folding // do not end var block
       '',
       'type',
       '  TBar2: ',
{25}   '    Function(): Boolean;',  // no folding // do not end type block
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

  for i := 0 to $20-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    if (i and $08) = 0 then AFolds := AFolds - [cfbtClassSection];
    if (i and $10) = 0 then AFolds := AFolds - [cfbtProcedure];
    EnableFolds(AFolds);
    if i = $20-1 then begin
      // fold depth / only for all folds enabled
      CheckFoldOpenCounts('', [ 1, 1, 0,
                                1 {type}, 1, 0, 0, 0, 0,
                                1 {var}, 0, 0, 1 {type}, 0, 0, 0,
                                0 {Proc}, 0,
                                1 {impl}, 0, 1 {var}, 0, 0, 1 {type}, 0, 0, 0,
                                1 {proc}, 1 {var}, 0, 0, 0, 0, 0
                         ]);
      AssertEquals('Len var 1 ',   2, PasHighLighter.FoldLineLength(9, 0));
      AssertEquals('Len type 1 ',  3, PasHighLighter.FoldLineLength(12, 0));
      AssertEquals('Len var 2 ',   2, PasHighLighter.FoldLineLength(20, 0));
      AssertEquals('Len type 2 ',  3, PasHighLighter.FoldLineLength(23, 0));
      AssertEquals('Len var 3 ',   2, PasHighLighter.FoldLineLength(28, 0));
    end;

    CheckTokensForLine('IBar.p1',   5, [ tkSpace, tkKey + AtK, tkSpace, tkIdentifier + AtP, tkSymbol ]);
    CheckTokensForLine('IBar.p2',   6, [ tkSpace, tkKey + AtK, tkSpace, tkIdentifier + AtP, tkSymbol ]);
    CheckTokensForLine('foo p of', 10, [ tkSpace, tkIdentifier, tkSymbol, tkSpace,
      tkKey + AtK, tkSpace, tkKey + AtK {of}, tkSpace, tkKey, tkSymbol
      ]);
    CheckTokensForLine('TBar',     14, [ tkSpace, tkKey + AtK, tkSymbol, tkSymbol, tkSymbol,
      tkSpace, tkIdentifier + AtI, tkSymbol
      ]);

  end;
end;

procedure TTestHighlighterPas.TestContextForProcedureNameAttr;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  ReCreateEdit;
  SetLines
    ([ 'Unit A;',
{ 1}   'interface',
{  }   '',
{  }   'TFoo=class',
{ 4}   'Procedure Bar;',
{ 5}   'function Bar:boolean;',
{ 6}   'function Bar(a:t.x):t.x;',
{  }   'end',
{  }   '',
{ 9}   'Procedure Bar;',
{10}   'function Bar:boolean;',
{11}   'function Bar(a:t.x):t.x;',
{  }   '',
{  }   'implementation',
{14}   'Procedure TFoo.Bar;',
{  }   'begin end;',
{16}   'function TFoo.Bar:boolean;',
{  }   'begin end;',
{18}   'function TFoo.Bar(a:t.x):t.x;',
{  }   'begin end;',
{20}   'Procedure Bar;',
{  }   'begin end;',
{22}   'function Bar:boolean;',
{  }   'begin end;',
{24}   'function Bar(a:t.x):t.x;',
{  }   'begin end;',
{  }   '',
{  }   'var',
{28}   'p1:procedure deprecated;', // deprecated is not the name
{29}   'p2:procedure(a:int64) deprecated;',
{30}   'f1:function:int64 deprecated;',
{  }   '',
{  }   'end.',
       ''
    ]);

  for i := 0 to $20-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    if (i and $08) = 0 then AFolds := AFolds - [cfbtClassSection];
    if (i and $10) = 0 then AFolds := AFolds - [cfbtProcedure];
    EnableFolds(AFolds);

    CheckTokensForLine('procedure Bar in class',  4,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, TK_Semi     // Procedure Bar;
      ]);

    CheckTokensForLine('function Bar:boolean in class',  5,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,      // function Bar
        TK_Colon, tkIdentifier, TK_Semi  //: boolean;
      ]);

    CheckTokensForLine('function Bar(a:t.x):boolean in class',  6,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,      // function Bar
        TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Bracket,  // (a:t.x)
        TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi  // :t.x;
      ]);


    CheckTokensForLine('procedure Bar',  9,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, TK_Semi     // Procedure Bar;
      ]);

    CheckTokensForLine('function Bar:boolean',  10,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,      // function Bar
        TK_Colon, tkIdentifier, TK_Semi  //: boolean;
      ]);

    CheckTokensForLine('function Bar(a:t.x):boolean', 11,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,      // function Bar
        TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Bracket,  // (a:t.x)
        TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi  // :t.x;
      ]);


    CheckTokensForLine('procedure TFoo.Bar', 14,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, TK_Dot + FAttrProcName, tkIdentifier + FAttrProcName, TK_Semi     // Procedure TFoo.Bar;
      ]);

    CheckTokensForLine('function TFoo.Bar:boolean', 16,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, TK_Dot + FAttrProcName, tkIdentifier + FAttrProcName,      // function TFoo.Bar
        TK_Colon, tkIdentifier, TK_Semi  //: boolean;
      ]);

    CheckTokensForLine('function TFoo.Bar(a:t.x):boolean', 18,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, TK_Dot + FAttrProcName, tkIdentifier + FAttrProcName,      // function TFoo.Bar
        TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Bracket,  // (a:t.x)
        TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi  // :t.x;
      ]);



    CheckTokensForLine('procedure Bar', 20,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName, TK_Semi     // Procedure TFoo.Bar;
      ]);

    CheckTokensForLine('function Bar:boolean', 22,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,      // function TFoo.Bar
        TK_Colon, tkIdentifier, TK_Semi  //: boolean;
      ]);

    CheckTokensForLine('function Bar(a:t.x):boolean', 24,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,      // function TFoo.Bar
        TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Bracket,  // (a:t.x)
        TK_Colon, tkIdentifier, TK_Dot, tkIdentifier, TK_Semi  // :t.x;
      ]);



    CheckTokensForLine('var p1:procedure deprecated;', 28,
      [ tkIdentifier, TK_Colon, tkKey, tkSpace, tkModifier, TK_Semi     // p1:procedure deprecated;
      ]);

    CheckTokensForLine('var p2:procedure(a:int64) deprecated;', 29,
      [ tkIdentifier, TK_Colon, tkKey,   // p2:procedure
      TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,    // (a:int64)
      tkSpace, tkModifier, TK_Semi  //deprecated;
      ]);

    CheckTokensForLine('var f1:function:int64 deprecated;', 30,
      [ tkIdentifier, TK_Colon, tkKey,   // f1:function
      TK_Colon, tkIdentifier, tkSpace, tkModifier, TK_Semi    // :int64 deprecated;
      ]);

  end;
end;

procedure TTestHighlighterPas.TestContextForInterface;
var
  AtP, AtI, AtK: TSynHighlighterAttributes;
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  ReCreateEdit;
  AtK := PasHighLighter.KeywordAttribute;

  SetLines
    ([ 'Unit A;',
       'interface',
       '',
       'type',
       '  IBar = interface',
       '     procedure p1;',
       '     procedure p2;',
       '  end;',
       '',
       'var',
       '  IBar2: interface', // not allowed "anonymous class"
       '     procedure p1;',
       '     procedure p2;',
       '',
       'implementation',
       ''
    ]);

  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    EnableFolds(AFolds);
    if i = $08-1 then begin
      CheckFoldOpenCounts('', [ 1, 1, 0,
                                1 {type}, 1, 0, 0, 0, 0,
                                1 {var},  0, 0, 0, 0, 0
                                // implementation
                         ]);
      AssertEquals('Len type ',  5, PasHighLighter.FoldLineLength(3, 0));
      AssertEquals('Len intf ',  3, PasHighLighter.FoldLineLength(4, 0));
      AssertEquals('Len var  ',  1, PasHighLighter.FoldLineLength(9, 0)); // ends at next procedure
    end;

    CheckTokensForLine('unit "interface"',  1,
      [ tkKey + AtK ]);
    CheckTokensForLine('type "interface"',  4,
      [ tkSpace, tkIdentifier, tkSpace, tkSymbol, tkSpace, tkKey + AtK ]);
    CheckTokensForLine('var "interface"',  10,
      [ tkSpace, tkIdentifier, tkSymbol, tkSpace, tkKey + AtK ]); // not allowed, still a keyword
  end;
end;

procedure TTestHighlighterPas.TestContextForDeprecated;
  procedure SubTest(s: String;
    AEnbledTypes: TPascalCodeFoldBlockTypes;
    AHideTypes: TPascalCodeFoldBlockTypes = [];
    ANoFoldTypes: TPascalCodeFoldBlockTypes = []);

    procedure SubTest2(struct: String);
    begin
      SetLines
        ([  'Unit A; interface {$ModeSwitch nestedprocvars}',
            'type',
            'TFoo='+struct,
               s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
               s+': array of '+s+' '+s+';',  // nameDEPRECATED=array of typeDEPRECATED deprecated;
               s+': array [1..2] of '+s+' '+s+';',  // nameDEPRECATED=array of typeDEPRECATED deprecated;
               s+': set of '+s+' '+s+';',    // nameDEPRECATED=set of typeDEPRECATED deprecated;
               s+': class of '+s+' '+s+';',  // nameDEPRECATED=class of typeDEPRECATED deprecated;
               s+': procedure '+s+';',
               s+': procedure of object '+s+';',
               s+': procedure(a:'+s+') '+s+';',
               s+': procedure(a:'+s+') of object '+s+';',
               s+': function:'+s+' '+s+';',
               s+': function:'+s+' of object '+s+';',
               s+': function(a:'+s+'):'+s+' '+s+';',
               s+': function(a:'+s+'):'+s+' of object '+s+';',
               s+': record end '+s+';',  // nameDEPRECATED=packed record deprecated;
               s+': packed record end '+s+';',  // nameDEPRECATED=packed record deprecated;
              'foo, '+s+', bar: Integer '+s+';',
              'procedure '+s+'('+s+': '+s+'); '+s+';',
            'end',
            ''
        ]);
        //TODO: is nested

      CheckTokensForLine('member in class', 3,
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('array of', 4,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('array [1..2] of', 5,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkSymbol, tkNumber, tkSymbol, tkNumber, tkSymbol,
         tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('set of', 6,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('class of', 7,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure ', 8,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure of object ', 9,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure(a:s) ', 10,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
         TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
         tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure(a:s) of object ', 11,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
         TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
         tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function', 12,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, TK_Colon, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function of object ', 13,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, TK_Colon, tkIdentifier, tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function(a:s)', 14,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
          TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
          TK_Colon, tkIdentifier,
          tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function(a:s) of object ', 15,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
          TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
          TK_Colon, tkIdentifier,
          tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('record end', 16,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"end"}, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('packed record end', 17,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey, tkSpace, tkKey {"end"}, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('multi member in class', 18,
        [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
        tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);

      if copy(struct, 1,11) = 'record case' then // procedure not allowed in record-case
        exit;
      CheckTokensForLine('procedure in class', 19,
        [tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
         tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkModifier {the one and only}, tkSymbol
        ]);

      if struct = 'byte;var' then
        struct := 'byte;';

      // NOT in record-case / no type allowed
      SetLines
        ([  'Unit A; interface {$modeswitch advancedrecords}{$ModeSwitch nestedprocvars}',
            'type',
            'TFoo='+struct+' type',
               s+'= '+s+' '+s+';',  // nameDEPRECATED= typeDEPRECATED deprecated;
               s+'= array of '+s+' '+s+';',  // nameDEPRECATED=array of typeDEPRECATED deprecated;
               s+'= array [1..2] of '+s+' '+s+';',  // nameDEPRECATED=array of typeDEPRECATED deprecated;
               s+'= set of '+s+' '+s+';',    // nameDEPRECATED=set of typeDEPRECATED deprecated;
               s+'= class of '+s+' '+s+';',  // nameDEPRECATED=class of typeDEPRECATED deprecated;
               s+'= procedure '+s+';',
               s+'= procedure of object '+s+';',
               s+'= procedure(a:'+s+') '+s+';',
               s+'= procedure(a:'+s+') of object '+s+';',
               s+'= function:'+s+' '+s+';',
               s+'= function:'+s+' of object '+s+';',
               s+'= function(a:'+s+'):'+s+' '+s+';',
               s+'= function(a:'+s+'):'+s+' of object '+s+';',
               s+'= record end '+s+';',  // nameDEPRECATED=packed record deprecated;
               s+'= packed record end '+s+';',  // nameDEPRECATED=packed record deprecated;
            'end',
            ''
        ]);

      CheckTokensForLine('member in class', 3,
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('array of', 4,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('array [1..2] of', 5,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkSymbol, tkNumber, tkSymbol, tkNumber, tkSymbol,
         tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('set of', 6,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('class of', 7,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure ', 8,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure of object ', 9,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure(a:s) ', 10,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
         TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
         tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure(a:s) of object ', 11,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
         TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
         tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function', 12,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, TK_Colon, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function of object ', 13,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, TK_Colon, tkIdentifier, tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function(a:s)', 14,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
          TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
          TK_Colon, tkIdentifier,
          tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('function(a:s) of object ', 15,
        [tkIdentifier, tkSymbol, tkSpace, tkKey,
          TK_Bracket, tkIdentifier, TK_Colon, tkIdentifier, TK_Bracket,
          TK_Colon, tkIdentifier,
          tkSpace, tkKey {"of"}, tkSpace, tkKey, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('record end', 16,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey {"end"}, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('packed record end', 17,
        [tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkKey, tkSpace, tkKey {"end"}, tkSpace, tkModifier {the one and only}, tkSymbol]);

      if (struct = 'record') or (struct = 'byte;') then
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
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('multi member in class', 4,
        [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
        tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('procedure in class', 5,
        [tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
         tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkModifier {the one and only}, tkSymbol
        ]);

      CheckTokensForLine('member in class-sect', 7,
        [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('multi member in class-sect', 8,
        [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
        tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
      CheckTokensForLine('property in class-sect', 9,
        [tkKey, tkSpace, tkIdentifier, tkSymbol { : }, tkSpace, tkIdentifier, tkSpace,
         tkKey { read }, tkSpace, tkIdentifier, tkSymbol { ; }, tkSpace, tkModifier {the one and only}, tkSymbol
        ]);
      CheckTokensForLine('procedure in class-sect', 10,
        [tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
         tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkModifier {the one and only}, tkSymbol
        ]);

    end;

  begin
    PushBaseName('test for '+s);
    ReCreateEdit;
    EnableFolds(AEnbledTypes, AHideTypes, ANoFoldTypes);
    SetLines
      ([  'Unit A; interface',
          'var',
           s+': '+s+' '+s+';',  // nameDEPRECATED: typeDEPRECATED deprecated;
          'foo, '+s+', bar: Integer '+s+';',
          'type',
          s+' = '+s+' '+s+';',   // nameDEPRECATED = typeDEPRECATED deprecated;
          s+' =type '+s+' '+s+';',   // nameDEPRECATED = type typeDEPRECATED deprecated;
          'procedure '+s+'('+s+': '+s+'); '+s+';',
          'var',
          s+':procedure '+s+';',
          '',
          'type tfoo = class',
          // 12
          'procedure bar; message 1; '+s+';',
          'procedure bar; message A; '+s+';',
          'procedure bar; message ''x''; '+s+';',
          'procedure bar; message #01; '+s+';',
          'end;',
          ''
      ]);
    CheckTokensForLine('var', 2,
      [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
    CheckTokensForLine('multi var', 3,
      [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
      tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
    CheckTokensForLine('type', 5,
      [tkIdentifier, tkSpace, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
    CheckTokensForLine('type', 6,
      [tkIdentifier, tkSpace, tkSymbol, tkKey, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
    CheckTokensForLine('procedure', 7,
      [tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
       tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol, tkSpace, tkModifier {the one and only}, tkSymbol
      ]);
    CheckTokensForLine('var a:procedure DEPRECATED;', 9,
      [tkIdentifier, TK_Colon, tkKey, tkSpace, tkModifier {the one and only}, TK_Semi]);

    CheckTokensForLine('procedure bar; message 1;  DEPRECATED;', 12,
      [tkKey, tkSpace, tkIdentifier+FAttrProcName, TK_Semi,
       tkSpace, tkModifier, tkSpace, tkNumber, TK_Semi, tkSpace, tkModifier, TK_Semi]);
    CheckTokensForLine('procedure bar; message A;  DEPRECATED;', 13,
      [tkKey, tkSpace, tkIdentifier+FAttrProcName, TK_Semi,
       tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
    CheckTokensForLine('procedure bar; message ''X'';  DEPRECATED;', 14,
      [tkKey, tkSpace, tkIdentifier+FAttrProcName, TK_Semi,
       tkSpace, tkModifier, tkSpace, tkString, TK_Semi, tkSpace, tkModifier, TK_Semi]);
    CheckTokensForLine('procedure bar; message ''X'';  DEPRECATED;', 15,
      [tkKey, tkSpace, tkIdentifier+FAttrProcName, TK_Semi,
       tkSpace, tkModifier, tkSpace, tkString, TK_Semi, tkSpace, tkModifier, TK_Semi]);


    PushBaseName('class');
    SubTest2('class');
    PushBaseName('class public');
    SubTest2('class public');
    PopPushBaseName('object');
    SubTest2('object');
    PopPushBaseName('object public');
    SubTest2('object public');
    PopPushBaseName('record');
    SubTest2('record');
    PopPushBaseName('record public');
    SubTest2('record public');

    PopPushBaseName('record case integer of 1:(');
    SubTest2('record case integer of 1:(');
    PopPushBaseName('record case integer of 1:( B:record case integer of 2:(');
    SubTest2('record case integer of 1:( B:record case integer of 2:(');
    PopPushBaseName('var/type');
    SubTest2('byte;var');
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
      [tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol { ( }, tkIdentifier, tkSymbol { : },
       tkSpace, tkIdentifier, tkSymbol { ) }, tkSymbol
      ]);
    CheckTokensForLine('var in procedure', 3,
      [tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);
    CheckTokensForLine('multi var in procedure', 4,
      [tkIdentifier, tkSymbol,   tkSpace, tkIdentifier, tkSymbol,   tkSpace,tkIdentifier, tkSymbol, // ... ":"
      tkSpace, tkIdentifier, tkSpace, tkModifier {the one and only}, tkSymbol]);



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
      [tkKey, tkSpace, tkModifier , tkSymbol]);


    // after unit declaration
    SetLines
      ([  'Unit A nonkey;', // check wrong word - must not be key
          'interface uses foo;',
          ''
      ]);
    CheckTokensForLine('dummy word after unit', 0,
      [tkKey, tkSpace, tkIdentifier, tkSpace, tkIdentifier, tkSymbol]);

    SetLines
      ([  'Unit A;'+s+';', // must not be key
          'interface uses foo;',
          ''
      ]);
    CheckTokensForLine('after unit, but after semicolon', 0,
      [tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol]);

    SetLines
      ([  'Unit A '+s+';',
          'interface uses foo;',
          ''
      ]);
    CheckTokensForLine('after unit', 0,
      [tkKey, tkSpace, tkIdentifier, tkSpace, tkModifier, tkSymbol]);
    CheckTokensForLine('after unit - next line', 1,
      [tkKey, tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol]);

    SetLines
      ([  'Unit A.B '+s+';',
          'interface uses foo;',
          ''
      ]);
    CheckTokensForLine('after dotted unit', 0,
      [tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSpace, tkModifier, tkSymbol]);
    CheckTokensForLine('after unit - next line', 1,
      [tkKey, tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol]);


    PopBaseName;
  end;

var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $40-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtVarBlock, cfbtLocalVarBlock];
    if (i and $08) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    if (i and $10) = 0 then AFolds := AFolds - [cfbtClassSection];
    if (i and $20) = 0 then AFolds := AFolds - [cfbtProcedure];
    //if (i and $40) = 0 then AFolds := AFolds - [cfbtAnonymousProcedure];
    SubTest('deprecated'   , AFolds);
    SubTest('unimplemented', AFolds);
    SubTest('experimental' , AFolds);
    SubTest('platform'     , AFolds);
  end;
  SubTest('deprecated'   , []);
  SubTest('unimplemented', []);
  SubTest('experimental' , []);
  SubTest('platform'     , []);
end;

procedure TTestHighlighterPas.TestContextForClassObjRecHelp;
var
  i0, i1, i2, i3, i4: Integer;
  s0, s1, s2: String;
begin
  ReCreateEdit;
  EnableFolds([cfbtClass, cfbtRecord], []);

  for i0 := 0 to 9 do
  for i1 := 0 to 5 do
  for i2 := 0 to 1 do
  for i3 := 0 to 1 do
  for i4 := 0 to 3 do
  begin
    if (i1 = 3) and (i2 = 1) then // type type helper
      continue;

    case i0 of
      0: s0 := '';
      1: s0 := 'TSome = class type';
      2: s0 := 'TSome = class public type';
      3: s0 := 'TSome = class var a: integer; type';
      4: s0 := 'TSome = object type';
      5: s0 := 'TSome = object public type';
      6: s0 := 'TSome = object var a: integer; type';
      7: s0 := 'TSome = record type';
      8: s0 := 'TSome = record public type';
      9: s0 := 'TSome = record var a: integer; type';
// TODO: nested in record-case
      //10: s0 := 'TSome = record case integer of 1: ( a: record b: integer; type'; // BracketNestLevel inside record-case
    end;

    case i4 of
      0: s1 := 'TFoo = ';
      1: s1 := 'TFoo=';
      2: s1 := 'generic TFoo<A> = ';
      3: s1 := 'generic TFoo<A>=';
    end;

    case i1 of
      0: s2 := 'class';
      1: s2 := 'record';
      2: s2 := 'object';
      3: s2 := 'type helper for integer';
      4: s2 := 'class helper for TFoo';
      5: s2 := 'record helper for TBar';
    end;
    if i2 = 1 then s2 := 'type '+s2;
    if i3 = 1 then s2 := ' '+s2; // leading space

    SetLines
      ([ 'Unit A; interface {$modeswitch advancedrecords}{$modeswitch typehelpers}',  // 0
         'type',
         s0,
         '',
         s1,
         s2,  // 5
         'public',
         'end;',
         ''
      ]);

    AssertEquals(1, FTheHighLighter.FoldOpenCount(5));  // fold opens for class/record/...
    AssertEquals(7, FTheHighLighter.FoldEndLine(5, 0));  // fold end for class/record/...
  end;



  for i2 := 0 to 1 do
  for i3 := 0 to 1 do
  for i4 := 0 to 1 do
  begin

    case i4 of
      0: s1 := 'TFoo : ';
      1: s1 := 'TFoo:';
    end;

    s2 := 'record';
    if i2 = 1 then s2 := 'type '+s2;
    if i3 = 1 then s2 := ' '+s2; // leading space
    SetLines
      ([ 'Unit A; interface {$modeswitch advancedrecords}{$modeswitch typehelpers}',  // 0
         'var',
         '',
         '',
         s1,
         s2,  // 5
         'public',
         'end;',
         ''
      ]);

    AssertEquals(1, FTheHighLighter.FoldOpenCount(5));  // fold opens for class/record/...
    AssertEquals(7, FTheHighLighter.FoldEndLine(5, 0));  // fold end for class/record/...
  end;
end;

procedure TTestHighlighterPas.TestContextForClassSection;
var
  ty, rc, rc1, hlp, lead1, lead2, cm, s1, s2, v, v_t: string;
  strict1, strict2: Boolean;
  cmod, sp1, sp2: Integer;
begin
  ReCreateEdit;
  for ty in ['     ', 'type '] do
  for rc1 in ['class ', 'object', 'record'] do
  for hlp in ['                 ', ' helper for c    ', ' helper(b) for c '] do
  for lead1 in ['', '  '] do
  for lead2 in ['', '  '] do
  for cm  in ['                ', ' sealed abstract', ' sealed         ', ' abstract       '] do
  for s1 in ['strict private  ', 'strict protected', 'private          ', 'protected        ', 'public           ', 'published        '] do
  for s2 in ['strict private  ', 'strict protected', 'private          ', 'protected        ', 'public           ', 'published        '] do
  for v  in ['private          ', 'protected        ', 'public           ', 'published        '] do
  begin
    rc := rc1;
    sp1 := ord(tkSpace);
    if lead1 = '' then sp1 := TK_SKIP;
    sp2 := ord(tkSpace);
    if lead2 = '' then sp2 := TK_SKIP;

    strict1 := s1[1] = 's';
    strict2 := s2[1] = 's';
    cmod := 0;
    if cm[2] <> ' ' then
    case cm[9] of
      ' ': cmod := 1; // sealed
      'a': cmod := 2; // sealed abstract
      't': cmod := 1; // abstract
    end;

    if (hlp[2]<>' ') and
       ( (ty[1] <> ' ') or (cm[2] <> ' ') )
    then
      continue;

    if (hlp[2]<>' ') and (rc = 'object') then // no "object helper" // make it "type helper"
      rc := 'type';

    if (rc <> 'class ') and ( (cmod <> 0) or strict1 or  strict2 )
    then
      continue;

    //ReCreateEdit;
    v_t := trim(v);
    SetLines
      ([ 'Unit A; interface {$mode objfpc} {$modeswitch typehelpers} {$modeswitch advancedrecords}',  // 0
         'type',
         'TFoo='+ty+rc+hlp+cm ,  // 2  class sealed abstract
         lead1+trim(s1),
         lead2+trim(s2),
           'a,'+v_t+':'+v_t+';', // 5
         lead1+trim(s2),
         lead2+trim(s1),
           'function '+v_t+'('+v_t+':'+v_t+';'+v_t+','+v_t+':'+v_t+'):'+v_t+';', // 8
         lead1+trim(s1),
         lead2+trim(s2), //10
         'end;',
         lead1+v_t+'='+v_t+';', // 12
         lead2+v_t+'='+v_t+';',
         'var',
         lead1+v_t+':'+v_t+';', // 15
         lead2+v_t+':'+v_t+';',
         ''
      ]);

    if hlp[2]=' ' then begin // not a helper
      if ty[1] = ' ' then begin
        case cmod of
          0: CheckTokensForLine('TFoo=class',  2, [ tkIdentifier, tkSymbol, tkSpace, tkKey ]);
          1: CheckTokensForLine('TFoo=class',  2, [ tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkModifier, tkSpace ]);
          2: CheckTokensForLine('TFoo=class',  2, [ tkIdentifier, tkSymbol, tkSpace, tkKey, tkSpace, tkModifier, tkSpace, tkModifier]);
        end;
      end
      else begin
        case cmod of
          0: CheckTokensForLine('TFoo=class',  2, [ tkIdentifier, tkSymbol, tkKey, tkSpace, tkKey ]);
          1: CheckTokensForLine('TFoo=class',  2, [ tkIdentifier, tkSymbol, tkKey, tkSpace, tkKey, tkSpace, tkModifier, tkSpace ]);
          2: CheckTokensForLine('TFoo=class',  2, [ tkIdentifier, tkSymbol, tkKey, tkSpace, tkKey, tkSpace, tkModifier, tkSpace, tkModifier]);
        end;
      end;
    end;

    case strict1 of
      False: CheckTokensForLine('public',          3, [ sp1, tkKey ]);
      True:  CheckTokensForLine('strict private',  3, [ sp1, tkKey, tkSpace, tkKey ]);
    end;
    case strict2 of
      False: CheckTokensForLine('public',          4, [ sp2, tkKey ]);
      True:  CheckTokensForLine('strict private',  4, [ sp2, tkKey, tkSpace, tkKey ]);
    end;

    CheckTokensForLine('a,public:public;',  5,
      [ tkIdentifier {a}, tkSymbol{,}, tkIdentifier {public}, tkSymbol{:},
        tkIdentifier, tkSymbol{;} ]);

    case strict2 of
      False: CheckTokensForLine('public',          6, [ sp1, tkKey ]);
      True:  CheckTokensForLine('strict private',  6, [ sp1, tkKey, tkSpace, tkKey ]);
    end;
    case strict1 of
      False: CheckTokensForLine('public',          7, [ sp2, tkKey ]);
      True:  CheckTokensForLine('strict private',  7, [ sp2, tkKey, tkSpace, tkKey ]);
    end;


    CheckTokensForLine('function ...',  8,
      [ tkKey {function}, tkSpace, tkIdentifier + FAttrProcName{public},tkSymbol{(},
        tkIdentifier {public}, tkSymbol{:}, tkIdentifier, tkSymbol{;},
        tkIdentifier {public}, tkSymbol{,}, tkIdentifier {public}, tkSymbol{:}, tkIdentifier, tkSymbol{;},
        tkSymbol{)}, tkIdentifier, tkSymbol{;}
      ]);

    case strict1 of
      False: CheckTokensForLine('public',          9, [ sp1, tkKey ]);
      True:  CheckTokensForLine('strict private',  9, [ sp1, tkKey, tkSpace, tkKey ]);
    end;
    case strict2 of
      False: CheckTokensForLine('public',         10, [ sp2, tkKey ]);
      True:  CheckTokensForLine('strict private', 10, [ sp2, tkKey, tkSpace, tkKey ]);
    end;

    CheckTokensForLine('end',          11, [ tkKey, tkSymbol ]);

    CheckTokensForLine('public=public;',  12,
      [ sp1, tkIdentifier {public}, tkSymbol{=}, tkIdentifier, tkSymbol{;} ]);
    CheckTokensForLine('public=public;',  13,
      [ sp2, tkIdentifier {public}, tkSymbol{=}, tkIdentifier, tkSymbol{;} ]);

    CheckTokensForLine('var',          14, [ tkKey ]);

    CheckTokensForLine('public:public;',  15,
      [ sp1, tkIdentifier {public}, tkSymbol{:}, tkIdentifier, tkSymbol{;} ]);
    // public would be modifier
    if trim(v) <> 'public' then
      CheckTokensForLine('public:public;',  16,
        [ sp2, tkIdentifier {public}, tkSymbol{:}, tkIdentifier, tkSymbol{;} ]);

  end;
end;

procedure TTestHighlighterPas.TestContextForClassModifier;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];

    ReCreateEdit;
    EnableFolds(AFolds);
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
        tkModifier {sealed}, tkSpace, tkModifier {abstract}
      ]);
    CheckTokensForLine('var in class "',  3,
      [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
        tkSpace, tkIdentifier, tkSymbol
      ]);
    CheckTokensForLine('procedure in class "',  4,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);



    ReCreateEdit;
    EnableFolds(AFolds);
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
        tkModifier {sealed}, tkSpace, tkModifier {abstract}
      ]);
    CheckTokensForLine('var in class "',  3,
      [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
        tkSpace, tkIdentifier, tkSymbol
      ]);
    CheckTokensForLine('procedure in class "',  4,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);



    ReCreateEdit;
    EnableFolds(AFolds);
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
      [ tkSpace, tkModifier {sealed}, tkSpace, tkModifier {abstract}
      ]);
    CheckTokensForLine('var in class "',  4,
      [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
        tkSpace, tkIdentifier, tkSymbol
      ]);
    CheckTokensForLine('procedure in class "',  5,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);




    ReCreateEdit;
    EnableFolds(AFolds);
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
        tkModifier {sealed}, tkSpace,
        tkModifier {abstract}
      ]);
    CheckTokensForLine('var in class "',  3,
      [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
        tkSpace, tkIdentifier, tkSymbol
      ]);
    CheckTokensForLine('procedure in class "',  4,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);

  end;
end;

procedure TTestHighlighterPas.TestContextForClassOf;
  procedure SetClassOfText(s: String; s2: String = '');
  begin
    if s2 = '' then s2 := 'FInt1: String;';
    SetLines
      ([ 'Unit A; interface',
         'type',
         s,                    // 2
         s2,                   // 3
         'FInt: String;',      // 4
         'private',            // 5
         'procedure Foo; abstract;',
          'end;',
         ''
      ]);
  end;
  procedure CheckClassOfField(ALine: Integer);
  begin
    CheckTokensForLine('Fint: integer; '+IntToStr(ALine),  ALine, [ tkIdentifier, TK_Colon, tkSpace, tkKey, TK_Semi ]);
    AssertEquals('no Fold-OpenCount '+IntToStr(ALine), 0, FTheHighLighter.FoldOpenCount(ALine));
  end;
  procedure CheckClassOfFold(ALine: Integer; AFold: Boolean);
  begin
    AssertEquals('Fold-OpenCount 2', 1, FTheHighLighter.FoldOpenCount(2)); // currently always

    if AFold then begin
      CheckTokensForLine('private '+IntToStr(ALine),  ALine, [ tkKey ]);
      AssertEquals('Fold-OpenCount '+IntToStr(ALine), 1, FTheHighLighter.FoldOpenCount(ALine));
    end
    else begin
      CheckTokensForLine('private '+IntToStr(ALine),  ALine, [ tkIdentifier ]);
      AssertEquals('no Fold-OpenCount '+IntToStr(ALine), 0, FTheHighLighter.FoldOpenCount(ALine));
    end;
  end;
begin
  ReCreateEdit;
  EnableFolds([cfbtBeginEnd..cfbtNone]);

  SetClassOfText('TFoo=class');
  CheckClassOfField(3);
  CheckClassOfFold(5, True);

  SetClassOfText('TFoo=class(TFoo)');
  CheckClassOfField(3);
  CheckClassOfFold(5, True);

  SetClassOfText('TFoo=class()');
  CheckClassOfField(3);
  CheckClassOfFold(5, True);

  SetClassOfText('TFoo=class()', 'private'); // incomplete
  CheckClassOfFold(3, True);
  CheckClassOfField(4);
  CheckClassOfFold(5, True);


  SetClassOfText('TFoo=class of');
  CheckClassOfField(3);
  CheckClassOfFold(5, False);

  SetClassOfText('TFoo=class {bar} of');
  CheckClassOfField(3);
  CheckClassOfFold(5, False);

  SetClassOfText('TFoo=class of', 'private');
  CheckClassOfFold(3, False);
  CheckClassOfField(4);
  CheckClassOfFold(5, False);


  SetClassOfText('TFoo = class sealed (TBar) of');
  CheckClassOfField(3);
  CheckClassOfFold(5, True);

  SetClassOfText('TFoo = class sealed of');
  CheckClassOfField(3);
  CheckClassOfFold(5, True);
end;

procedure TTestHighlighterPas.TestContextForClassProcModifier;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i, j: Integer;
  n: String;
  h: TSynHighlighterAttributesModifier;
begin
  ReCreateEdit;
  h := FAttrProcName;
  for i := 0 to 8 do begin
    case i of
      0: n := 'virtual';
      1: n := 'dynamic';
      2: n := 'override';
      3: n := 'abstract';
      4: n := 'final';
      5: n := 'reintroduce';
      6: n := 'message';
      7: n := 'platform';
      8: n := 'overload';
      9: n := 'enumerator'; // "enumerator current" or "enumerator MoveNext"
    end;

    SetLines
      ([ 'Unit A; interface {$mode delphi}',
         'type',
         'TFoo = class public',
         // 3
         n+':'+n+';'+n+':'+n+';', // 2 fields
         'public',
         // 5
         n+':'+n+' deprecated;'+n+','+n+':'+n+';', // 3 fields
         'public',
         // 7
         n+':procedure;'+n+':'+n+';', //
         n+':procedure deprecated;'+n+':'+n+';', //
         '',
         '',
         // 11
        'procedure '+n+';'+n+';',
        'procedure '+n+';deprecated; '+n+';',  // deprecated before virtual: ONLY mode delphi
        'procedure '+n+'; '+n+'; deprecated;',
        'procedure '+n+';overload; '+n+';',
        'procedure '+n+'; '+n+'; overload;',
         '',
         // 17
        'procedure '+n+'; override; final;',
        'procedure '+n+'; virtual; final;',
        'procedure '+n+'; reintroduce; virtual;',
        'procedure '+n+'; reintroduce; virtual; final;',
        'procedure '+n+'; overload; reintroduce; virtual; final;',
        'procedure '+n+'; reintroduce; virtual; final; overload;',
        'procedure '+n+'; reintroduce; virtual; final; deprecated;',
        '',
        // 25
        'procedure '+n+'; message A; '+n+';',
        'procedure '+n+'; message 1; '+n+';',
        'procedure '+n+'; message ''x''; '+n+';',
        'procedure '+n+'; message #01; '+n+';',
        'procedure '+n+'; message '+n+'; '+n+';',
        // 30
        'procedure '+n+'; '+n+'; message A;',
        'procedure '+n+'; '+n+'; message 1;',
        'procedure '+n+'; '+n+'; message ''x'';',
        'procedure '+n+'; '+n+'; message '+n+';',
        // 34
        'procedure '+n+'; override; message A;final;',
        'procedure '+n+'; override; message 1;final;',
        'procedure '+n+'; override; message ''x'';final;',
        'procedure '+n+'; override; message '+n+';final;',
        // 38
        'procedure '+n+'('+n+':'+n+');'+n+';',
        'function '+n+':'+n+';'+n+';',
        'function '+n+'('+n+':'+n+'):'+n+';'+n+';',
        // 41
        'function '+n+'('+n+':'+n+'):'+n+';enumerator MoveNext;'+n+';',
        'property '+n+':'+n+' read '+n+';enumerator Current;deprecated;',
        'end;',
         ''
      ]);

    for j := 0 to $0F do begin
      AFolds := [];
      if (j and $08) = 0 then AFolds := [cfbtBeginEnd..cfbtNone] - [cfbtClass, cfbtClassSection, cfbtProcedure];
      if (j and $01) = 0 then AFolds := AFolds + [cfbtClass];
      if (j and $02) = 0 then AFolds := AFolds + [cfbtClassSection];
      if (j and $04) = 0 then AFolds := AFolds + [cfbtProcedure];

      EnableFolds(AFolds);

        CheckTokensForLine(n+':'+n+';'+n+':'+n+';', 3,
          [tkIdentifier, TK_Colon, tkIdentifier, TK_Semi,
           tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
        CheckTokensForLine( 'public', 4, [tkKey]);
        // 5
        CheckTokensForLine( n+':'+n+' deprecated;'+n+','+n+':'+n+';', 5,
          [tkIdentifier, TK_Colon, tkIdentifier, tkSpace, tkModifier, TK_Semi,
           tkIdentifier, TK_Comma, tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
        CheckTokensForLine( 'public', 6,  [tkKey]);
        // 7
        CheckTokensForLine( n+':procedure;'+n+':'+n+';', 7,
          [tkIdentifier, TK_Colon, tkKey, TK_Semi,
           tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
        CheckTokensForLine( n+':procedure deprecated;'+n+':'+n+';', 8,
          [tkIdentifier, TK_Colon, tkKey, tkSpace, tkModifier, TK_Semi,
           tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
        // 11
        CheckTokensForLine('procedure '+n+';'+n+';', 11,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkModifier, TK_Semi]);
              // deprecated before virtual: ONLY mode delphi
        CheckTokensForLine('procedure '+n+';deprecated; '+n+';', 12,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; '+n+'; deprecated;', 13,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+';overload; '+n+';', 14,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; '+n+'; overload;', 15,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        // 17
        CheckTokensForLine('procedure '+n+'; override; final;', 17,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; virtual; final;', 18,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; reintroduce; virtual;', 19,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; reintroduce; virtual; final;', 20,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; overload; reintroduce; virtual; final;', 21,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; reintroduce; virtual; final; overload;', 22,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; reintroduce; virtual; final; deprecated;', 23,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, TK_Semi, tkSpace, tkModifier, TK_Semi]);
        // 25
        CheckTokensForLine('procedure '+n+'; message A; '+n+';',25,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi,
           tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; message 1; '+n+';',26,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkNumber, TK_Semi,
           tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; message ''x''; '+n+';',27,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkString, TK_Semi,
           tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; message #01; '+n+';',28,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkString, TK_Semi,
           tkSpace, tkModifier, TK_Semi]);
        CheckTokensForLine('procedure '+n+'; message '+n+'; '+n+';',29,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi,
           tkSpace, tkModifier, TK_Semi]);
        // 30
        CheckTokensForLine('procedure '+n+'; '+n+'; message A;',30,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi
          ]);
        CheckTokensForLine('procedure '+n+'; '+n+'; message 1;',31,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkNumber, TK_Semi
          ]);
        CheckTokensForLine('procedure '+n+'; '+n+'; message ''x'';',32,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkString, TK_Semi
          ]);
        CheckTokensForLine('procedure '+n+'; '+n+'; message '+n+';',33,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi
          ]);
        // 34
        CheckTokensForLine('procedure '+n+'; override; message A;final;',34,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi,
           tkModifier, TK_Semi
          ]);
        CheckTokensForLine('procedure '+n+'; override; message 1;final;',35,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkNumber, TK_Semi,
           tkModifier, TK_Semi
          ]);
        CheckTokensForLine('procedure '+n+'; override; message ''x'';final;',36,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkString, TK_Semi,
           tkModifier, TK_Semi
          ]);
        CheckTokensForLine('procedure '+n+'; override; message '+n+';final;',37,
          [tkKey, tkSpace, tkIdentifier+h, TK_Semi,
           tkSpace, tkModifier, TK_Semi,
           tkSpace, tkModifier, tkSpace, tkIdentifier, TK_Semi,
           tkModifier, TK_Semi
          ]);
        // 38
        CheckTokensForLine('procedure '+n+'('+n+':'+n+');'+n+';',38,
          [tkKey, tkSpace, tkIdentifier+h,
           TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket, TK_Semi,
           tkModifier, TK_Semi
          ]);
        CheckTokensForLine('function '+n+':'+n+';'+n+';',39,
          [tkKey, tkSpace, tkIdentifier+h, TK_Colon,
           tkIdentifier, TK_Semi,
           tkModifier, TK_Semi
          ]);
        CheckTokensForLine('function '+n+'('+n+':'+n+'):'+n+';'+n+';',40,
          [tkKey, tkSpace, tkIdentifier+h,
           TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket, TK_Colon,
           tkIdentifier, TK_Semi,
           tkModifier, TK_Semi
          ]);
        // 41
        CheckTokensForLine('function '+n+'('+n+':'+n+'):'+n+';enumerator MoveNext;'+n+';',41,
          [tkKey, tkSpace, tkIdentifier+h,
           TK_Bracket, tkIdentifier, TK_Comma, tkIdentifier, TK_Bracket, TK_Colon, tkIdentifier, TK_Semi,
           tkModifier, tkSpace, tkIdentifier, TK_Semi,
           tkModifier, TK_Semi
          ]);
        CheckTokensForLine('property '+n+':'+n+' read '+n+';enumerator Current;deprecated;',42,
          [tkKey, tkSpace, tkIdentifier,
           TK_Colon, tkIdentifier, tkSpace, tkKey, tkSpace, tkIdentifier, TK_Semi,
           tkModifier, tkSpace, tkIdentifier, TK_Semi,
           tkModifier, TK_Semi
          ]);
    end;
  end;
end;

procedure TTestHighlighterPas.TestContextForClassHelper;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];

    ReCreateEdit;
    EnableFolds(AFolds);
    SetLines
      ([ 'Unit A; interface',
         'type',
         'TFoo = class helper for TBar',
           'helper, sealed, abstract, public: Integer;',
           'procedure Foo; abstract;',
          'end;',
         'TFoo = class helper for TBar',
         'protected',
         'end;',
         ''
      ]);
    CheckTokensForLine('class declaration"',  2,
      [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
        tkKey {class}, tkSpace, tkKey {helper}, tkSpace, tkKey {for},
        tkSpace, tkIdentifier
      ]);
    CheckTokensForLine('var in class "',  3,
      [ tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSpace,
        tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol,
        tkSpace, tkIdentifier, tkSymbol
      ]);
    CheckTokensForLine('procedure in class "',  4,
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);
    CheckTokensForLine('end',  5,
      [ tkKey, tkSymbol ]);
    CheckTokensForLine('class declaration"',  6,
      [ tkIdentifier, tkSpace, tkSymbol, tkSpace,
        tkKey {class}, tkSpace, tkKey {helper}, tkSpace, tkKey {for},
        tkSpace, tkIdentifier
      ]);
    CheckTokensForLine('class section',  7,
      [ tkKey ]);


    ReCreateEdit;
    EnableFolds(AFolds);
    SetLines
      ([ 'Unit A; interface',
         'type',
         'TFoo = class helper(helper) for helper',
           'helper, sealed, abstract: Integer;',
           'procedure Foo; abstract;',
          'end;',
         'TFoo = class helper(helper) for helper',
         'protected',
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
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);
    CheckTokensForLine('class section',  7,
      [ tkKey ]);
  end;
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
      [ tkKey, tkSpace, tkIdentifier + FAttrProcName,  tkSymbol, tkSpace, tkModifier,  tkSymbol ]);
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

    CheckTokensForLine('class section',  14,
      [ tkKey ]);
    CheckTokensForLine('NOT class section',  18,
      [ tkIdentifier ]);
  end;

var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];

    ReCreateEdit;
    EnableFolds(AFolds);
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
         'helper = type helper for helper',
         'protected',
         'end;',
         '{$modeswitch typehelpers-}',
         'helper = type helper for helper',
         'protected',
         '{$modeswitch typehelpers}',
         ''
      ]);

    DoChecks;
    SynEdit.TestTypeText(1, 2, ' ');
    DoChecks; // modeswitch on rescan

    PasHighLighter.FoldConfig[ord(cfbtClass)].Enabled := False;
    DoChecks;
  end;
end;

procedure TTestHighlighterPas.TestContextForClassFunction;
var
  i, j: Integer;
  AFolds: TPascalCodeFoldBlockTypes;
const
  t: array[0..2] of string = ('class', 'object', 'record');
begin
  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];

    for j:= 0 to 2 do begin
      ReCreateEdit;
      EnableFolds(AFolds);

      SetLines
        ([ 'Unit A; interface',
           'type',
           'TFoo = '+t[j],
             'class function f1: boolean;',
             'class procedure p1(v: boolean);',
            'end;',
           ''
        ]);
      //   unit/iface,  type,  record, -,-
      if i = $08-1 then
        CheckFoldOpenCounts('', [2, 1, 1, 0, 0]);

      CheckTokensForLine('class function',  3,
        [ tkKey, tkSpace, tkKey, tkSpace,  tkIdentifier + FAttrProcName, tkSymbol, tkSpace,  tkIdentifier, tkSymbol  ]);
      CheckTokensForLine('class procedure',  4,
        [ tkKey, tkSpace, tkKey, tkSpace,  tkIdentifier + FAttrProcName, tkSymbol, tkIdentifier, tkSymbol, tkSpace,  tkIdentifier, tkSymbol, tkSymbol  ]);

    end;
  end;
end;

procedure TTestHighlighterPas.TestContextForRecordHelper;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];


    ReCreateEdit;
    EnableFolds(AFolds);
    SetLines
      ([ 'Unit A; interface {$mode delphi}',
         'type',
         'TFoo = record helper for TBar',
           'helper, sealed, abstract: Integer;',
          'end;',
         'TFoo = record helper for TBar',
          'protected;',
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
    CheckTokensForLine('class section',  6,
      [ tkKey ]);


    ReCreateEdit;
    EnableFolds(AFolds);
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
end;

procedure TTestHighlighterPas.TestContextForRecordCase;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
begin
  for i := 0 to $1F do begin
    AFolds := [];
    if (i and $10) = 0 then AFolds := [cfbtBeginEnd..cfbtNone] - [cfbtVarBlock, cfbtRecord, cfbtRecordCase, cfbtRecordCaseSection];
    if (i and $01) = 0 then AFolds := AFolds + [cfbtVarBlock];
    if (i and $02) = 0 then AFolds := AFolds + [cfbtRecord];
    if (i and $04) = 0 then AFolds := AFolds + [cfbtRecordCase];
    if (i and $08) = 0 then AFolds := AFolds + [cfbtRecordCaseSection];

    ReCreateEdit;
    SetLines
      ([ 'Unit A; interface',
         'type',
         '  TFoo = record',
         '  A:byte;',
         '  case integer of',
         '  1: (',
         '    B: record',
         '    case integer of',
         '    3: (',
         '    );',
         '    4: (',
         '      C: packed record',
         '      A:byte;',
         '      case integer of',
         '      5: (',
         '      B: record end;',
         '      );',
         '      6: (',
         '        X:byte;',
         '        case integer of',
         '        8: (',
         '        );',
         '        9: (',
         '        );',
         '      );',
         '      end;',
         '    );',
         '    end;',
         '  );',
         '  2: (',
         '  );',
         '  end;',
         '',
         'var',
         ''
      ]);

    EnableFolds(AFolds);

    CheckTokensForLine('  TFoo = record',         2, [tkSpace, tkIdentifier, tkSpace, TK_Equal, tkSpace, tkKey]);
    CheckTokensForLine('  A:byte;',               3, [tkSpace, tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
    CheckTokensForLine('  case integer of',       4, [tkSpace, tkKey, tkSpace, tkIdentifier, tkSpace, tkKey]);
    CheckTokensForLine('  1: (',                  5, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('    B: record',           6, [tkSpace, tkIdentifier, TK_Colon, tkSpace, tkKey]);
    CheckTokensForLine('    case integer of',     7, [tkSpace, tkKey, tkSpace, tkIdentifier, tkSpace, tkKey]);
    CheckTokensForLine('    3: (',                8, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('    );',                  9, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('    4: (',               10, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('      C: packed record', 11, [tkSpace, tkIdentifier, TK_Colon, tkSpace, tkKey, tkSpace, tkKey]);
    CheckTokensForLine('      A:byte;',          12, [tkSpace, tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
    CheckTokensForLine('      case integer of',  13, [tkSpace, tkKey, tkSpace, tkIdentifier, tkSpace, tkKey]);
    CheckTokensForLine('      5: (',             14, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('      B: record end;',   15, [tkSpace, tkIdentifier, TK_Colon, tkSpace, tkKey, tkSpace, tkKey, TK_Semi]);
    CheckTokensForLine('      );',               16, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('      6: (',             17, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('        X:byte;',        18, [tkSpace, tkIdentifier, TK_Colon, tkIdentifier, TK_Semi]);
    CheckTokensForLine('        case integer of',19, [tkSpace, tkKey, tkSpace, tkIdentifier, tkSpace, tkKey]);
    CheckTokensForLine('        8: (',           20, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('        );',             21, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('        9: (',           22, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('        );',             23, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('      );',               24, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('      end;',             25, [tkSpace, tkKey, TK_Semi]);
    CheckTokensForLine('    );',                 26, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('    end;',               27, [tkSpace, tkKey, TK_Semi]);
    CheckTokensForLine('  );',                   28, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('  2: (',                 29, [tkSpace, tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, TK_Bracket]);
    CheckTokensForLine('  );',                   30, [tkSpace, TK_Bracket, TK_Semi]);
    CheckTokensForLine('  end;',                 31, [tkSpace, tkKey, TK_Semi]);


    if cfbtVarBlock in AFolds then
      AssertEquals('Fold-Len type   (1) ',   31, PasHighLighter.FoldLineLength(1, 0));
    if cfbtRecord in AFolds then
      AssertEquals('Fold-Len record (2) ',   29, PasHighLighter.FoldLineLength(2, 0));
    if cfbtRecordCase in AFolds then begin
      AssertEquals('Fold-Len case   (4) ',   27, PasHighLighter.FoldLineLength( 4, 0));
      AssertEquals('Fold-Len case   (7) ',   20, PasHighLighter.FoldLineLength( 7, 0));
      AssertEquals('Fold-Len case  (13) ',   12, PasHighLighter.FoldLineLength(13, 0));
      AssertEquals('Fold-Len case  (19) ',    5, PasHighLighter.FoldLineLength(19, 0)); // closed by ")" of surrounding case-section
    end;
    if cfbtRecordCaseSection in AFolds then begin
      AssertEquals('Fold-Len section  (5) ',  23, PasHighLighter.FoldLineLength( 5, 0));
      AssertEquals('Fold-Len section ( 8) ',   1, PasHighLighter.FoldLineLength( 8, 0));
      AssertEquals('Fold-Len section (10) ',  16, PasHighLighter.FoldLineLength(10, 0));
      AssertEquals('Fold-Len section (14) ',   2, PasHighLighter.FoldLineLength(14, 0));
      AssertEquals('Fold-Len section (17) ',   7, PasHighLighter.FoldLineLength(17, 0));
      AssertEquals('Fold-Len section (20) ',   1, PasHighLighter.FoldLineLength(20, 0));
      AssertEquals('Fold-Len section (22) ',   1, PasHighLighter.FoldLineLength(22, 0));
      AssertEquals('Fold-Len section (29) ',   1, PasHighLighter.FoldLineLength(29, 0));
    end;



  end;
end;

procedure TTestHighlighterPas.TestContextForStatic;
var
  AFolds: TPascalCodeFoldBlockTypes;
  i: Integer;
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

  for i := 0 to $08-1 do begin
    AFolds := [cfbtBeginEnd..cfbtNone];
    if (i and $01) = 0 then AFolds := AFolds - [cfbtProgram, cfbtUnit];
    if (i and $02) = 0 then AFolds := AFolds - [cfbtUnitSection];
    if (i and $04) = 0 then AFolds := AFolds - [cfbtClass, cfbtRecord];
    EnableFolds(AFolds);

    CheckTokensForLine('static = class',      2,
                       [tkIdentifier, tkSymbol, tkKey, tkSpace, tkKey, tkSymbol]);
    CheckTokensForLine('Tfoo=class(static)',  3,
    [tkIdentifier, tkSymbol, tkKey,tkSymbol, tkIdentifier, tkSymbol]);
    CheckTokensForLine('fields',              4,
                       [tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkModifier, tkSymbol]);
    CheckTokensForLine('fields 2',            5,
                       [tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkModifier, tkSymbol]);
    CheckTokensForLine('function',            6,
                       [tkSpace, tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                        tkSymbol, tkSpace, tkIdentifier, tkSymbol, // : #32 static ;
                        tkSpace, tkModifier, tkSymbol                   // #32 static ;
                       ]);
    CheckTokensForLine('property',            7,
                       [tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                        tkSymbol, tkSpace, tkIdentifier,           // : #32 static
                        tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 read static
                        tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 write static
                        tkSymbol                   // ;
                       ]);
    CheckTokensForLine('pup fields',          9,
                       [tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkModifier, tkSymbol]);
    CheckTokensForLine('pup fields 2',       10,
                       [tkSpace, tkIdentifier, tkSymbol, tkSpace, tkIdentifier, tkSymbol, tkSpace, tkModifier, tkSymbol]);
    CheckTokensForLine('pup function',       11,
                       [tkSpace, tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                        tkSymbol, tkSpace, tkIdentifier, tkSymbol, // : #32 static ;
                        tkSpace, tkModifier, tkSymbol                   // #32 static ;
                       ]);
    CheckTokensForLine('pup property',       12,
                       [tkSpace, tkKey, tkSpace, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,
                        tkSymbol, tkSpace, tkIdentifier,           // : #32 static
                        tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 read static
                        tkSpace, tkKey, tkSpace, tkIdentifier,     // #32 write static
                        tkSymbol                   // ;
                       ]);
  end;
end;

procedure TTestHighlighterPas.TestCaseLabel;
begin
  ReCreateEdit;
  SetLines
    ([ 'program a; begin',  // 0
         'case b of',
           '1: foo;',    // 2
           'bar: bar;',
           'else foo;',
         'end;',
         'case b of',
           '''123'': bar;',  // 7
           'bar: bar;',
           'otherwise foo;',
         'end;',
       'end;',
       ''
    ]);

  PasHighLighter.CaseLabelAttriMatchesElseOtherwise := True;
  CheckTokensForLine('1: foo;',  2, [tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('bar: bar;',  3, [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('else foo;',  4, [tkKey+FCaseLabelAttri, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('end;',  5, [tkKey, TK_Semi]);

  CheckTokensForLine('''123'': foo;',  7, [tkString+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('bar: bar;',  8, [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('else foo;',  9, [tkKey+FCaseLabelAttri, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('end;',  10, [tkKey, TK_Semi]);


  PasHighLighter.CaseLabelAttriMatchesElseOtherwise := False;
  CheckTokensForLine('1: foo;',  2, [tkNumber+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('bar: bar;',  3, [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('else foo;',  4, [tkKey, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('end;',  5, [tkKey, TK_Semi]);

  CheckTokensForLine('''123'': foo;',  7, [tkString+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('bar: bar;',  8, [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('else foo;',  9, [tkKey, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('end;',  10, [tkKey, TK_Semi]);


  FCaseLabelAttri.Clear;
  FCaseLabelAttri := nil;
  PasHighLighter.CaseLabelAttriMatchesElseOtherwise := True;
  CheckTokensForLine('1: foo;',  2, [tkNumber, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('bar: bar;',  3, [tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('else foo;',  4, [tkKey, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('end;',  5, [tkKey, TK_Semi]);

  CheckTokensForLine('''123'': foo;',  7, [tkString, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('bar: bar;',  8, [tkIdentifier, TK_Colon, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('else foo;',  9, [tkKey, tkSpace, tkIdentifier, TK_Semi]);
  CheckTokensForLine('end;',  10, [tkKey, TK_Semi]);

end;

procedure TTestHighlighterPas.TestModifierAttributesForProcedure;
var
  ProcName, ProcParam, ProcType, ProcVal, ProcRes: TSynHighlighterAttributesModifier;
begin
  FKeepAllModifierAttribs := True;
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',  // 0
       'function Foo: integer;',
       'function Foo: string;',
       'function Foo(var a:byte;b, b2:string;c:array of boolean): integer;',
       'function Foo(d:word=2-x;e:boolean=(1=y*2);f:qword=default(qword); g:MySet=[a1..a2]): integer;',
       ''
    ]);

  ProcName  := PasHighLighter.ProcedureHeaderName;
  ProcParam := PasHighLighter.ProcedureHeaderParamAttr;
  ProcType  := PasHighLighter.ProcedureHeaderTypeAttr;
  ProcVal  := PasHighLighter.ProcedureHeaderValueAttr;
  ProcRes   := PasHighLighter.ProcedureHeaderResultAttr;

  PasHighLighter.DeclaredTypeAttributeMode := tamIdentifierOnly;
  PasHighLighter.DeclaredValueAttributeMode := tamIdentifierOnly;
  PasHighLighter.DeclaredValueAttributeMachesStringNum := False;

  CheckTokensForLine('2: function Foo: integer',  1,
    [tkKey, tkSpace, tkIdentifier+ProcName, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Semi]);

  CheckTokensForLine('3: function Foo: string',  2,
    [tkKey, tkSpace, tkIdentifier+ProcName, TK_Colon, tkSpace,
     tkKey, TK_Semi]);

  CheckTokensForLine('4: function Foo(var a:byte;b, b2:string;c:array of boolean): integer;',  3,
    [tkKey, tkSpace, tkIdentifier+ProcName, TK_Bracket,
     tkKey, tkSpace, tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Comma, tkSpace, tkIdentifier+ProcParam,  TK_Colon, tkKey, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkKey, tkSpace, tkKey, tkSpace, tkIdentifier+ProcType,
     TK_Bracket, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Semi]);

  CheckTokensForLine('4: function Foo(d:word=2-x;e:boolean=(1=y*2);f:qword=default(qword); g:MySet=[a1..a2]): integer;',  4,
    [tkKey, tkSpace, tkIdentifier+ProcName, TK_Bracket,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType,
       TK_Equal, tkNumber, tkSymbol, tkIdentifier+ProcVal, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType,
       TK_Equal, TK_Bracket, tkNumber, tkSymbol, tkIdentifier+ProcVal, tkSymbol, tkNumber, TK_Bracket, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType,
       TK_Equal, tkIdentifier+ProcVal, TK_Bracket, tkIdentifier+ProcVal, TK_Bracket, TK_Semi,
     tkSpace,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType,
       TK_Equal, TK_Bracket, tkIdentifier+ProcVal, tkSymbol, tkIdentifier+ProcVal, TK_Bracket,
     TK_Bracket, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Semi]);


  //PropName.Clear;
  //PasHighLighter.DeclaredTypeAttributeMode := tamIdentifierOnly;
  //PasHighLighter.DeclaredValueAttributeMode := tamIdentifierOnly;
  //PasHighLighter.DeclaredValueAttributeMachesStringNum := False;


end;

procedure TTestHighlighterPas.TestModifierAttributesForProperty;
var
  PropName, ProcParam, ProcType, ProcRes: TSynHighlighterAttributesModifier;
begin
  FKeepAllModifierAttribs := True;
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',  // 0
       'property Foo: integer read Foo;',
       'property Foo[a:byte;b:string;c:unit2.word]: unit2.integer read Foo;',
       ''
    ]);

  PropName  := PasHighLighter.PropertyNameAttr;
  ProcParam := PasHighLighter.ProcedureHeaderParamAttr;
  ProcType  := PasHighLighter.ProcedureHeaderTypeAttr;
  ProcRes   := PasHighLighter.ProcedureHeaderResultAttr;

  PasHighLighter.DeclaredTypeAttributeMode := tamIdentifierOnly;
  PasHighLighter.DeclaredValueAttributeMode := tamIdentifierOnly;
  PasHighLighter.DeclaredValueAttributeMachesStringNum := False;

  CheckTokensForLine('1: unit a: interface;',  0,
    [tkKey, tkSpace, tkIdentifier, TK_Semi, tkSpace, tkKey]);

  CheckTokensForLine('2:property Foo: integer read Foo;',  1,
    [tkKey, tkSpace, tkIdentifier+PropName, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, tkSpace,
     tkKey, tkSpace, tkIdentifier, TK_Semi]);

  CheckTokensForLine('3:property Foo[a:byte;b:string;c:unit2.word]: unit2.integer read Foo;',  2,
    [tkKey, tkSpace, tkIdentifier+PropName, TK_Bracket,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkKey, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Dot, tkIdentifier+ProcType,
     TK_Bracket, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Dot, tkIdentifier+ProcRes, tkSpace,
     tkKey, tkSpace, tkIdentifier, TK_Semi]);

  PasHighLighter.DeclaredTypeAttributeMode := tamPredefinedNames;
  CheckTokensForLine('3:property Foo[a:byte;b;string;c:unit2.word]: unit2.integer read Foo;',  2,
    [tkKey, tkSpace, tkIdentifier+PropName, TK_Bracket,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkKey+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Dot, tkIdentifier+ProcType,
     TK_Bracket, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Dot, tkIdentifier+ProcRes, tkSpace,
     tkKey, tkSpace, tkIdentifier, TK_Semi]);

  PasHighLighter.DeclaredTypeAttributeMode := tamKeywordsAndSymbols;
  CheckTokensForLine('3:property Foo[a:byte;b;string;c:unit2.word]: unit2.integer read Foo;',  2,
    [tkKey, tkSpace, tkIdentifier+PropName, TK_Bracket,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkKey+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Dot+ProcType, tkIdentifier+ProcType,
     TK_Bracket, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Dot+ProcRes, tkIdentifier+ProcRes, tkSpace+ProcRes{for space....},
     tkKey, tkSpace, tkIdentifier, TK_Semi]);



  PropName.Clear;
  PasHighLighter.DeclaredTypeAttributeMode := tamIdentifierOnly;
  PasHighLighter.DeclaredValueAttributeMode := tamIdentifierOnly;
  PasHighLighter.DeclaredValueAttributeMachesStringNum := False;

  CheckTokensForLine('2:property Foo: integer read Foo;',  1,
    [tkKey, tkSpace, tkIdentifier, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, tkSpace,
     tkKey, tkSpace, tkIdentifier, TK_Semi]);

  CheckTokensForLine('3:property Foo[a:byte;b;string;c:unit2.word]: unit2.integer read Foo;',  2,
    [tkKey, tkSpace, tkIdentifier, TK_Bracket,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkKey, TK_Semi,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Dot, tkIdentifier+ProcType,
     TK_Bracket, TK_Colon, tkSpace,
     tkIdentifier+ProcRes, TK_Dot, tkIdentifier+ProcRes, tkSpace,
     tkKey, tkSpace, tkIdentifier, TK_Semi]);

end;

procedure TTestHighlighterPas.TestModifierAttributesForVarConstType;
var
  DeclVarName, DeclTypeName, DeclType, DeclVal, ProcName,
    ProcParam, ProcType, ProcVal, ProcRes: TSynHighlighterAttributesModifier;
  i: Integer;
begin
  FKeepAllModifierAttribs := True;
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',  // 0
       'var',
         'Foo: word deprecated;',  //2
         'Foo: word = val deprecated;',
       'type',
         'Foo= word deprecated;',
       'const',            // 6
         'x:function (a:word; b:byte): integer = nil;',            // 7
       'type',
         'x=function (): integer deprecated;',             //9
         'x=function (a:word): integer deprecated;',
         'x=function (a:word=b): integer deprecated;',
         'x=function (a:word; b:byte): integer deprecated;',
       'var',
         'a:record',  // 14
           'b:byte;',
           'c:array of word;',
         'end;',
       ''
    ]);

  DeclVarName := PasHighLighter.DeclarationVarConstNameAttr;
  DeclTypeName := PasHighLighter.DeclarationTypeNameAttr;
  DeclType  := PasHighLighter.DeclarationTypeAttr;
  DeclVal := PasHighLighter.DeclarationValueAttr;

  ProcName  := PasHighLighter.ProcedureHeaderName;
  ProcParam := PasHighLighter.ProcedureHeaderParamAttr;
  ProcType  := PasHighLighter.ProcedureHeaderTypeAttr;
  ProcVal  := PasHighLighter.ProcedureHeaderValueAttr;
  ProcRes   := PasHighLighter.ProcedureHeaderResultAttr;

  PasHighLighter.DeclaredTypeAttributeMode := tamKeywords;
  PasHighLighter.DeclaredValueAttributeMode := tamKeywords;
  // inside the function, use the proc-attr
  CheckTokensForLine('8:x:function (a:word; b:byte): integer = nil;', 7,
    [tkIdentifier+DeclVarName, TK_Colon, tkKey+DeclType, tkSpace, TK_Bracket,
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi, tkSpace, // a:word
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, // b:byte
     TK_Bracket, TK_Colon, tkSpace,  // ):
     tkIdentifier+ProcRes, tkSpace,  // integer
     TK_Equal, tkSpace, tkKey+DeclVal, TK_Semi
    ]);
  //type
  CheckTokensForLine('10:x=function (a:word): integer deprecated;', 9,
    [tkIdentifier+DeclTypeName, TK_Colon, tkKey+DeclType, tkSpace, TK_Bracket, // x=function (
     TK_Bracket, TK_Colon, tkSpace,  // ):
     tkIdentifier+ProcRes, tkSpace,  // integer
     tkModifier, TK_Semi  // deprecated
    ]);
  CheckTokensForLine('11:x=function (a:word): integer deprecated;', 10,
    [tkIdentifier+DeclTypeName, TK_Colon, tkKey+DeclType, tkSpace, TK_Bracket, // x=function (
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType,   // a:word
     TK_Bracket, TK_Colon, tkSpace,  // ):
     tkIdentifier+ProcRes, tkSpace,  // integer
     tkModifier, TK_Semi  // deprecated
    ]);
  CheckTokensForLine('12:x=function (a:word=b): integer deprecated;', 11,
    [tkIdentifier+DeclTypeName, TK_Colon, tkKey+DeclType, tkSpace, TK_Bracket, // x=function (
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType,   // a:word
     TK_Equal, tkIdentifier+ProcVal, // =b
     TK_Bracket, TK_Colon, tkSpace,  // ):
     tkIdentifier+ProcRes, tkSpace,  // integer
     tkModifier, TK_Semi  // deprecated
    ]);
  CheckTokensForLine('13:x=function (a:word; b:byte): integer deprecated;', 12,
    [tkIdentifier+DeclTypeName, TK_Colon, tkKey+DeclType, tkSpace, TK_Bracket, // x=function (
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, TK_Semi, tkSpace, // a:word
     tkIdentifier+ProcParam, TK_Colon, tkIdentifier+ProcType, // b:byte
     TK_Bracket, TK_Colon, tkSpace,  // ):
     tkIdentifier+ProcRes, tkSpace,  // integer
     tkModifier, TK_Semi  // deprecated
    ]);

  // inside the record, use decl-attr according to record
// TODO: record and end are not DeclType, because cfbtVarConstTypeExt is missing
  //CheckTokensForLine('15: a:record', 14,
  //  [tkIdentifier+DeclVarName, TK_Colon,tkKey+DeclType]);
  CheckTokensForLine('16: b:byte', 15,
    [tkIdentifier+DeclVarName, TK_Colon,tkIdentifier+DeclType]);
  CheckTokensForLine('17: c:array of word', 16,
    [tkIdentifier+DeclVarName, TK_Colon,tkKey+DeclType, tkSpace,
     tkKey+DeclType, tkSpace, tkIdentifier+DeclType, TK_Semi]);
  //CheckTokensForLine('18: end', 17,
  //  [tkKey+DeclType, TK_Semi]);

  for i := 0 to 1 do begin
    case i of
      0: begin
        PasHighLighter.DeclaredTypeAttributeMode := tamIdentifierOnly;
        PasHighLighter.DeclaredValueAttributeMode := tamIdentifierOnly;
        PasHighLighter.DeclaredValueAttributeMachesStringNum := False;
      end;
      1: begin
        PasHighLighter.DeclaredTypeAttributeMode := tamKeywords;
        PasHighLighter.DeclaredValueAttributeMode := tamKeywords;
        PasHighLighter.DeclaredValueAttributeMachesStringNum := True;
      end;
    end;

    CheckTokensForLine('3: Foo: word; deprecated;',  2,
      [tkIdentifier+DeclVarName, TK_Colon, tkSpace, tkIdentifier+DeclType,
       tkSpace, tkModifier, TK_Semi]);

    CheckTokensForLine('4: Foo: word = val; deprecated;',  3,
      [tkIdentifier+DeclVarName, TK_Colon, tkSpace, tkIdentifier+DeclType,
       tkSpace, TK_Equal, tkSpace, tkIdentifier+DeclVal,
       tkSpace, tkModifier, TK_Semi]);

    CheckTokensForLine('6: Foo= word; deprecated;', 5,
      [tkIdentifier+DeclTypeName, TK_Equal, tkSpace, tkIdentifier+DeclType,
       tkSpace, tkModifier, TK_Semi]);

  end;
end;

procedure TTestHighlighterPas.TestModifierAttributesForLabel;
var
  GotoLbl: TSynHighlighterAttributes;
begin
  FKeepAllModifierAttribs := True;
  ReCreateEdit;
  SetLines
    ([ 'Unit A; interface',  // 0
       'label lbl1,',
       'lbl2',
       ', lbl3 ;',
       'procedure foo;',  // 4
       'label lbl4;',
       'begin',
       'lbl1:',      // 7
       '  lbl2:',
       'foo:=1;',
       'lbl3:',
       'if true then',  // 11
       'lbl3:',
         'case x of',
           'abc: ;',
           'def: lbla: lblb: {} lblc: i:=1;', // 15
           'xyz: ;',
           'else  ;',
           '; lbl:  ;',   // 18
         'end;',
       'end;',
       'repeat',  // 21
       'lbl:',
       'until false;',
       ''
    ]);

  GotoLbl  := PasHighLighter.GotoLabelAttr;

  CheckTokensForLine('2: label lbl1,', 1,
    [tkKey, tkSpace, tkIdentifier+GotoLbl, TK_Comma]);

  CheckTokensForLine('3: lbl2', 2,
    [tkIdentifier+GotoLbl]);

  CheckTokensForLine('4: , lbl3 ;', 3,
    [TK_Comma, tkSpace, tkIdentifier+GotoLbl, tkSpace, TK_Semi]);

  CheckTokensForLine('6: label lbl4;', 5,
    [tkKey, tkSpace, tkIdentifier+GotoLbl, TK_Semi]);

  CheckTokensForLine('8: lbl1:', 7,
    [tkIdentifier+GotoLbl, TK_Colon]);

  CheckTokensForLine('9:   lbl2:', 8,
    [tkSpace, tkIdentifier+GotoLbl, TK_Colon]);

  CheckTokensForLine('10: foo:=1;', 9,
    [tkIdentifier, tkSymbol, tkNumber, TK_Semi]);

  CheckTokensForLine('11: lbl3:', 10,
    [tkIdentifier+GotoLbl, TK_Colon]);

  CheckTokensForLine('13: lbl3:', 12,
    [tkIdentifier+GotoLbl, TK_Colon]);

  CheckTokensForLine('15: abc: ;', 14,
    [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace, TK_Semi]);

  CheckTokensForLine('16: def: lbla: lblb: {} lblc: i:=1;', 15,
    [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace,
     tkIdentifier+GotoLbl, TK_Colon, tkSpace,
     tkIdentifier+GotoLbl, TK_Colon, tkSpace, tkComment, tkSpace,
     tkIdentifier+GotoLbl, TK_Colon, tkSpace,
     tkIdentifier, tkSymbol, tkNumber, TK_Semi]);

  CheckTokensForLine('17: xyz: ;', 16,
    [tkIdentifier+FCaseLabelAttri, TK_Colon, tkSpace, TK_Semi]);

  CheckTokensForLine('18: else  ;', 17,
    [tkKey+FCaseLabelAttri, tkSpace, TK_Semi]);

  CheckTokensForLine('19: ; lbl:  ;', 18,
    [TK_Semi, tkSpace, tkIdentifier+GotoLbl, TK_Colon, tkSpace, TK_Semi]);

  CheckTokensForLine('23: lbl:', 22,
    [tkIdentifier+GotoLbl, TK_Colon]);


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
         'c:record A:char; B:^char; end=(a:^c;b:nil);',   // 5
       'type',
         'c=^char;',         // 7
         'c=type ^char;',         // 8
         'f=procedure(a:char=^c);',
       'const',
         'f:procedure(a:char=^c) =nil;',
         'd:record A:char; B:^char; end=(a:^c;b:nil);',  //12
       'implementation',
       'function x(f:^char=^k^c):^v;', // actually the compiler does not allow ^ as pointer for result
       'var',
         'a:char=^o;',       // 16
         'b:^char=nil;',     // 17
       'type',  // 18
         'c=^char;',
       'begin',
         'i:=^f;',
         'x:=GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});', // 18
         'c:=p^;',
         'c:=p ^;',
         'c:=p(**)^;',
         'c:=p{} ^;',     // 26
         'i:=f(1)^;',     // 27
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
  CheckTokensForLine('c:record A:char; B:^char; end=(a:^c;b:nil);',   5,
                     [tkIdentifier, tkSymbol, tkKey, tkSpace,
                        tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
                        tkIdentifier, TK_Colon, tkSymbol, tkIdentifier, TK_Semi, tkSpace, tkKey,
                        TK_Equal, TK_Bracket,
                        tkIdentifier, TK_Colon, tkString, TK_Semi, tkIdentifier, TK_Colon, tkKey,
                        TK_Bracket, TK_Semi
                        ]);

  CheckTokensForLine('c=^char;',   7,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol]);
  CheckTokensForLine('c=type ^char;',   8,
                     [tkIdentifier, tkSymbol, tkKey, tkSpace, tkSymbol, tkIdentifier, tkSymbol]);
  CheckTokensForLine('f=procedure(a:char=^c);',   9,
                     [tkIdentifier, tkSymbol, tkKey, TK_Bracket,
                      tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkString, TK_Bracket, TK_Semi]);

  CheckTokensForLine('f:procedure(a:char=^c) =nil;',   11,
                     [tkIdentifier, tkSymbol, tkKey, TK_Bracket,
                      tkIdentifier, TK_Colon, tkIdentifier, TK_Equal, tkString, TK_Bracket, tkSpace,
                      TK_Equal, tkKey, TK_Semi]);

  CheckTokensForLine('CONST d:record A:char; B:^char; end=(a:^c;b:nil);',   12,
                     [tkIdentifier, tkSymbol, tkKey, tkSpace,
                        tkIdentifier, TK_Colon, tkIdentifier, TK_Semi, tkSpace,
                        tkIdentifier, TK_Colon, tkSymbol, tkIdentifier, TK_Semi, tkSpace, tkKey,
                        TK_Equal, TK_Bracket,
                        tkIdentifier, TK_Colon, tkString, TK_Semi, tkIdentifier, TK_Colon, tkKey,
                        TK_Bracket, TK_Semi
                        ]);

  CheckTokensForLine('function x(f:^char=^k):^v;',   14,
                     [tkKey, tkSpace, tkIdentifier + FAttrProcName, tkSymbol, tkIdentifier,  // function x(f
                      tkSymbol, tkSymbol, tkIdentifier, tkSymbol, tkString,  tkString,  // :^char=^k
                      tkSymbol, tkSymbol, tkSymbol, tkIdentifier, tkSymbol]);          // ):^v;
  CheckTokensForLine('LOCAL a:char=^o;',   16,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkString, tkSymbol]);
  CheckTokensForLine('LOCAL b:^char=nil;',   17,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol, tkKey, tkSymbol]);
  CheckTokensForLine('LOCAL c=^char;',   19,
                     [tkIdentifier, tkSymbol, tkSymbol, tkIdentifier, tkSymbol]);
  CheckTokensForLine('i:=^f',   21,
                     [tkIdentifier, tkSymbol, tkString, tkSymbol]);

  CheckTokensForLine('x:=GetTypeData(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF});',   22,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol,    // x:=GetTypeData(
                      tkIdentifier, tkSymbol, tkSymbol, tkIdentifier,    // PropInfo^.PropType
                      tkDirective, tkSymbol, tkDirective, tkSymbol, tkSymbol]);  // {$IFNDEF FPC}^{$ENDIF});

  CheckTokensForLine('c:=p^;',   23,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p ^;',   24,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSpace, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p(**)^;',   25,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkComment, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p{} ^;',   26,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkComment, tkSpace, tkSymbol, tkSymbol]);

  CheckTokensForLine('c:=p(1)^;',   27,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkNumber, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p[1]^;',   28,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkNumber, tkSymbol, tkSymbol]);
  CheckTokensForLine('c:=p^^;',   29,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSymbol, tkSymbol]);

  CheckTokensForLine('c:=p^+^i''e''^a#13^x;',   30,
                     [tkIdentifier, tkSymbol, tkIdentifier, tkSymbol, tkSymbol, // c:=p^+
                      tkString, tkString, tkString, tkString, tkString, tkSymbol  // ^i'e'^a#13^x;
                     ]);
  CheckTokensForLine('c:=x=^a and ^a=k and(^a^a=z);',   31,
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
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOneLineOpen, sfaSingleLine]);
      CheckNode( 2, [], 0,   1,   15, 15,   3, 2,   3, 2,
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaOneLineClose, sfaCloseForNextLine, sfaSingleLine]);
      // Line 3:   var                                                          # pasminlvl=2 endlvl=3
      CheckNode( 3, [], 0,   0,   0, 3,   2, 3,   2, 3,
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 4:     b:integer                                                  # pasminlvl=2 endlvl=2
      CheckNode( 4, [], 0,   0,   11, 11,   3, 2,   3, 2,
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
                                  [sfaClose, sfaCloseFold,sfaFold, sfaCloseForNextLine, sfaMultiLine]);
      // Line 5:   const                                                        # pasminlvl=2 endlvl=3
      CheckNode( 5, [], 0,   0,   0, 5,   2, 3,   2, 3,
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
                                  [sfaOpen, sfaOpenFold,sfaFold,sfaFoldFold, sfaMultiLine]);
      // Line 6:     c = 1;                                                     # pasminlvl=3 endlvl=3
      // Line 7:     d = 2; {$ifdef a}                                          # pasminlvl=1 endlvl=1
      CheckNode( 7, [], 0,   0,   10, 16,   0, 1,   0, 1,
                                  cfbtIfDef, cfbtIfDef,  FOLDGROUP_IFDEF,
                                  [sfaOpen, sfaOpenFold,sfaMarkup,sfaFold,sfaFoldFold, sfaMultiLine]);
      CheckNode( 7, [], 0,   1,   19, 19,   3, 2,   3, 2,
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
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
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
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
                                  cfbtVarBlock, cfbtVarBlock,  FOLDGROUP_PASCAL,
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

