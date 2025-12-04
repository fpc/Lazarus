unit TestHighlightFoldBase;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, TestBase, SynEdit, SynEditHighlighterFoldBase, SynEditHighlighter,
  SynEditMiscClasses, LazLoggerBase, LazEditTextAttributes;

type

  // used by Fold / MarkupWord

  TTestExpValuesForLine = record
    Line: integer;
    Exp: Array of integer;
  end;

  function ExpVLine(ALine: Integer; AExp: Array of integer): TTestExpValuesForLine;

const
  TK_SKIP = -1;
type

  TExpTokenInfo = record
    ExpKind: Integer;
    ExpAttr: TLazEditTextAttribute;
    Flags: set of (etiKind, etiAttr);
  end;

operator := (a: Integer) : TExpTokenInfo;
operator := (a: TLazEditTextAttribute) : TExpTokenInfo;
operator + (a: Integer; b: TLazEditTextAttribute) : TExpTokenInfo;

type

  { TTestBaseHighlighterFoldBase }

  TTestBaseHighlighterFoldBase = class(TTestBase)
  protected
    FTheHighLighter: TSynCustomFoldHighlighter;
    function CreateTheHighLighter: TSynCustomFoldHighlighter; virtual; abstract;
    procedure InitTighLighterAttr; virtual;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ReCreateEdit; reintroduce; virtual;

    procedure CheckFoldOpenCounts(Name: String; Expected: Array of Integer);
    procedure CheckFoldLengths(Name: String; Expected: Array of TTestExpValuesForLine);
    procedure CheckFoldEndLines(Name: String; Expected: Array of TTestExpValuesForLine);

    procedure CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions; Expected: Array of Integer);
    procedure CheckFoldInfoCounts(Name: String; Filter: TSynFoldActions; Group: Integer; Expected: Array of Integer);

    procedure CheckTokensForLine(Name: String; LineIdx: Integer; ExpTokens: Array of TExpTokenInfo);

    function FoldActionsToString(AFoldActions: TSynFoldActions): String;
  end;


implementation

operator := (a: Integer) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpKind := a;
  result.Flags := [etiKind];
end;

operator := (a: TLazEditTextAttribute) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpAttr := a;
  result.Flags := [etiAttr];
end;

operator + (a: Integer; b: TLazEditTextAttribute) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpKind := a;
  result.ExpAttr := b;
  result.Flags := [etiKind, etiAttr];
end;

function ExpVLine(ALine: Integer; AExp: array of integer): TTestExpValuesForLine;
var
  i: Integer;
begin
  Result.Line := ALine;
  SetLength(Result.Exp, Length(AExp));
  for i := low(AExp) to high(AExp) do
    Result.Exp[i] := AExp[i];
end;

{ TTestBaseHighlighterFoldBase }

procedure TTestBaseHighlighterFoldBase.InitTighLighterAttr;
var
  i: Integer;
begin
  for i := 0 to FTheHighLighter.AttrCount - 1 do begin
    //DebugLn(['# ', i, ' ', FTheHighLighter.Attribute[i].StoredName]);
    FTheHighLighter.Attribute[i].Foreground := 10000 + i; // unique foreground colors
    FTheHighLighter.Attribute[i].Foreground := 10000 + i; // unique foreground colors
    if FTheHighLighter.Attribute[i] is TLazEditTextAttributeModifier then begin
      TLazEditTextAttributeModifier(FTheHighLighter.Attribute[i]).ForeAlpha := 0;
      TLazEditTextAttributeModifier(FTheHighLighter.Attribute[i]).ForePriority := 100+i;
    end;
  end;
end;

procedure TTestBaseHighlighterFoldBase.SetUp;
begin
  FTheHighLighter := nil;
  inherited SetUp;
end;

procedure TTestBaseHighlighterFoldBase.TearDown;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(FTheHighLighter);
  inherited TearDown;
end;

procedure TTestBaseHighlighterFoldBase.ReCreateEdit;
begin
  if Assigned(SynEdit) then
    SynEdit.Highlighter := nil;
  FreeAndNil(FTheHighLighter);
  inherited ReCreateEdit;
  FTheHighLighter := CreateTheHighLighter;
  InitTighLighterAttr;
  SynEdit.Highlighter := FTheHighLighter;
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldOpenCounts(Name: String;
  Expected: array of Integer);
var
  i: Integer;
begin
  for i := 0 to high(Expected) do
    AssertEquals('%s OpenCount Line OLD%d=', [Name, i], Expected[i], FTheHighLighter.FoldOpenCount(i));

  for i := 0 to high(Expected) do
    AssertEquals('%s OpenCount Line=%d', [Name, i],  Expected[i], FTheHighLighter.FoldBlockOpeningCount(i));
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldLengths(Name: String;
  Expected: array of TTestExpValuesForLine);
var
  i, j: Integer;
begin
  for i := 0 to high(Expected) do
    for j := 0 to high(Expected[i].Exp) do
      AssertEquals('%s FoldLength Line=%d idx=%d', [Name, Expected[i].Line, j],
        Expected[i].Exp[j], FTheHighLighter.FoldLineLength(Expected[i].Line, j));
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldEndLines(Name: String;
  Expected: array of TTestExpValuesForLine);
var
  i, j: Integer;
begin
  for i := 0 to high(Expected) do
    for j := 0 to high(Expected[i].Exp) do
      AssertEquals('%s FoldEnd Line=%d idx=%d', [Name, Expected[i].Line, j],
        Expected[i].Exp[j], FTheHighLighter.FoldEndLine(Expected[i].Line, j));
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldInfoCounts(Name: String;
  Filter: TSynFoldActions; Expected: array of Integer);
begin
  CheckFoldInfoCounts(Name, Filter, 0, Expected);
end;

procedure TTestBaseHighlighterFoldBase.CheckFoldInfoCounts(Name: String;
  Filter: TSynFoldActions; Group: Integer; Expected: array of Integer);
var
  i: Integer;
  l: TLazSynFoldNodeInfoList;
begin
  for i := 0 to high(Expected) do begin
    l := FTheHighLighter.FoldNodeInfo[i];
    AssertEquals('%s InfoCount(Ex) Line=%d', [Name, i],
                 Expected[i],
                 l.CountEx(Filter, Group));
    l.ClearFilter;
    l.ActionFilter := Filter;
    l.GroupFilter := Group;
    AssertEquals('%s InfoCount Line=%d', [Name, i],
                 Expected[i],
                 FTheHighLighter.FoldNodeInfo[i].Count);
  end;
end;

procedure TTestBaseHighlighterFoldBase.CheckTokensForLine(Name: String;
  LineIdx: Integer; ExpTokens: array of TExpTokenInfo);

  function AttrVal(a: TLazCustomEditTextAttribute): Integer;
  begin
    if a = nil then exit(-1);
    if a is TLazEditTextAttributeMergeResult then
      TLazEditTextAttributeMergeResult(a).FinishMerge;
    Result := a.Foreground; // compare the color
  end;
var
  c: Integer;
  e: TExpTokenInfo;
  GotAttr: TLazCustomEditTextAttribute;
begin
  FTheHighLighter.StartAtLineIndex(LineIdx);
  c := 0;
  while not FTheHighLighter.GetEol do begin
    e := ExpTokens[c];
    if e.ExpKind = TK_SKIP then begin
      inc(c);
      continue;
    end;
    //DebugLn([FTheHighLighter.GetToken,' (',FTheHighLighter.GetTokenKind ,') at ', FTheHighLighter.GetTokenPos]);

    if etiKind in e.Flags then
      AssertEquals('%s ASSERT token-kind @ TokenId Line=%d pos=%d Src=%s @ %d', [Name, LineIdx, c, FTheHighLighter.GetToken, FTheHighLighter.GetTokenPos],
        e.ExpKind, FTheHighLighter.GetTokenKind);

    GotAttr := FTheHighLighter.GetTokenAttributeEx;
    if etiAttr in e.Flags then
      AssertEquals('%s Attr @ TokenId Line=%d pos=%d Src=%s @ %d', [Name, LineIdx, c, FTheHighLighter.GetToken, FTheHighLighter.GetTokenPos],
        AttrVal(e.ExpAttr), AttrVal(GotAttr))
    else
      AssertTrue('%s Attr is NOT modifier @ TokenId Line=%d pos=%d Src=%s @ %d', [Name, LineIdx, c, FTheHighLighter.GetToken, FTheHighLighter.GetTokenPos],
        (GotAttr=nil) or (not (GotAttr is TSynHighlighterAttributesModifier)) );

    FTheHighLighter.Next;
    inc(c);
    if c >= length(ExpTokens) then
      break;
  end;
  AssertEquals('%s TokenId Line=%d  amount of tokens', [Name, LineIdx], length(ExpTokens), c );
end;

function TTestBaseHighlighterFoldBase.FoldActionsToString(AFoldActions: TSynFoldActions): String;
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

end.

