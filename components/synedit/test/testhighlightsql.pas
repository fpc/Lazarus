unit TestHighlightSql;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TestHighlightFoldBase, SynHighlighterSQL, SynEditHighlighterFoldBase,
  LazEditTextAttributes, testregistry;

type

  { TTestBaseHighlighterSql }

  TTestBaseHighlighterSql = class(TTestBaseHighlighterFoldBase)
  protected
    FKeepAllModifierAttribs: boolean;
    FKeepGenericModifierAttribs: boolean;
    function SqlHighLighter: TSynSQLSyn;
    function CreateTheHighLighter: TSynCustomFoldHighlighter; override;
    procedure InitTighLighterAttr; override;
    procedure EnableFolds(AEnbledTypes: TSqlCodeFoldBlockTypes;
                          AHideTypes: TSqlCodeFoldBlockTypes = [];
                          ANoFoldTypes: TSqlCodeFoldBlockTypes = []
                         );
  end;

  { TTestHighlighterSql }

  TTestHighlighterSql = class(TTestBaseHighlighterSql)
  published
    procedure TestUpdateSet;
  end;

implementation

const // short versions of token kind
  _        = tkSpace;
  _K       = tkKey;
  _I       = tkIdentifier;
  _D       = tkNumber; // Digits
  _Comma   = tkSymbol;
  _Semi    = tkSymbol;
  _Dot     = tkSymbol;
  _Eq      = tkSymbol;


operator := (a: TtkTokenKind) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpKind := ord(a);
  result.Flags := [etiKind];
end;

operator + (a: TtkTokenKind; b: TLazEditTextAttribute) : TExpTokenInfo;
begin
  result := default(TExpTokenInfo);
  result.ExpKind := ord(a);
  result.ExpAttr := b;
  result.Flags := [etiKind, etiAttr];
end;


{ TTestBaseHighlighterSql }

function TTestBaseHighlighterSql.SqlHighLighter: TSynSQLSyn;
begin
  Result := TSynSQLSyn(FTheHighLighter);
end;

function TTestBaseHighlighterSql.CreateTheHighLighter: TSynCustomFoldHighlighter;
begin
  Result := TSynSQLSyn.Create(nil);
end;

procedure TTestBaseHighlighterSql.InitTighLighterAttr;
begin
  inherited InitTighLighterAttr;

  SqlHighLighter.ClientKeywordAttri.Clear;
  SqlHighLighter.CharSetAttri.Clear;
  SqlHighLighter.CollationAttri.Clear;
end;

procedure TTestBaseHighlighterSql.EnableFolds(AEnbledTypes: TSqlCodeFoldBlockTypes;
  AHideTypes: TSqlCodeFoldBlockTypes; ANoFoldTypes: TSqlCodeFoldBlockTypes);
var
  i: TSqlCodeFoldBlockType;
begin
  SqlHighLighter.BeginUpdate;
  for i := low(TSqlCodeFoldBlockType) to high(TSqlCodeFoldBlockType) do begin
    SqlHighLighter.FoldConfig[ord(i)].Enabled := i in AEnbledTypes;
    if (i in ANoFoldTypes) then
      SqlHighLighter.FoldConfig[ord(i)].Modes := []
    else
      SqlHighLighter.FoldConfig[ord(i)].Modes := [fmFold];
    if i in AHideTypes then
      SqlHighLighter.FoldConfig[ord(i)].Modes := SqlHighLighter.FoldConfig[ord(i)].Modes + [fmHide];

    SqlHighLighter.FoldConfig[ord(i)].Modes := SqlHighLighter.FoldConfig[ord(i)].Modes +
      SqlHighLighter.FoldConfig[ord(i)].SupportedModes * [fmMarkup];
  end;
  SqlHighLighter.EndUpdate;
end;

{ TTestHighlighterSql }

procedure TTestHighlighterSql.TestUpdateSet;
begin
  ReCreateEdit;
  SqlHighLighter.SQLDialect := sqlMySQL5;
  SetLines(['update foo set bar = 1;']); // line 0

  CheckTokensForLine('1: update foo set bar = 1;', 0,
    [_K, _, _I {maybe table}, _, _K {set}, _, _I, _, _Eq, _, _D, _Semi ]);
end;

initialization

  RegisterTest(TTestHighlighterSql);
end.

