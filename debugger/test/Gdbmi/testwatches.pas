unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  TestBase, Debugger, GDBMIDebugger, LCLProc, SynRegExpr;

type

  { TTestWatch }

  TTestWatch = class(TBaseWatch)
  private
    FHasMultiValue: Boolean;
    FHasValue: Boolean;
    FMaster: TDBGWatch;
    FValue: String;
    FTypeInfo: TDBGType;
  protected
    procedure DoChanged; override;
    function GetTypeInfo: TDBGType; override;
  public
    constructor Create(AOwner: TBaseWatches; AMaster: TDBGWatch);
    property Master: TDBGWatch read FMaster;
    property HasMultiValue: Boolean read FHasMultiValue;
    property HasValue: Boolean read FHasValue;
    property Value: String read FValue;
  end;

  { TTestWatches }

  TTestWatches = class(TGDBTestCase)
  private
    FWatches: TBaseWatches;
  public
    procedure DebugInteract(dbg: TGDBMIDebugger);

  published
    procedure TestWatches;
  end;


implementation

const
  RNoPreQuote  = '(^|[^''])'; // No open qoute (Either at start, or other char)
  RNoPostQuote = '($|[^''])'; // No close qoute (Either at end, or other char)

type
  TWatchExpectationFlag = (fnoDwrf, fnoStabs, fTpMtch);
  TWatchExpectationFlags = set of TWatchExpectationFlag;
  TWatchExpectation = record
    Exp:  string;
    Mtch: string;
    Kind: TDBGSymbolKind;
    TpNm: string;
    Flgs: TWatchExpectationFlags;
  end;

const
  Match_Pointer    = '(0x|\$)[0-9A-F]+';
  Match_PasPointer = '\$[0-9A-F]+';
  Match_ArgTRec  = 'record TREC .+ valint = -1.+valfoo'; // record TREC {  VALINT = -1,  VALFOO = $0}
  Match_ArgTRec1 = 'record TREC .+ valint = 1.+valfoo'; // record TREC {  VALINT = 1,  VALFOO = $xxx}
  Match_ArgTRec2 = 'record TREC .+ valint = 2.+valfoo'; // record TREC {  VALINT = 2,  VALFOO = $xxx}
  Match_ArgTNewRec  = 'record TNEWREC .+ valint = 3.+valfoo'; // record TREC {  VALINT = 3,  VALFOO = $0}

  Match_ArgTFoo = '<TFoo> = \{.+ValueInt = -1';
  Match_ArgTFoo1 = '<TFoo> = \{.+ValueInt = 31';
  // Todo: Dwarf fails with dereferenced var pointer types

  ExpectBrk1NoneNil: Array [1..55] of TWatchExpectation = (
    { records }

    (Exp: 'ArgTRec';       Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgTRec';      Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'ArgPRec';       Mtch: '^PRec\('+Match_PasPointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'VArgPRec';      Mtch: '^PRec\('+Match_PasPointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'ArgPRec^';       Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgPRec^';      Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPRec';      Mtch: '^PPRec\('+Match_PasPointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'VArgPPRec';     Mtch: '^PPRec\('+Match_PasPointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'ArgPPRec^';      Mtch: '^PRec\('+Match_PasPointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: []),
    (Exp: 'VArgPPRec^';     Mtch: '^PRec\('+Match_PasPointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPRec^^';     Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgPPRec^^';    Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgTNewRec';    Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'TNewRec'; Flgs: []),
    (Exp: 'VArgTNewRec';   Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'TNewRec'; Flgs: []),

    (Exp: 'VarTRec';       Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarPRec';       Mtch: '^PRec\('+Match_PasPointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'VarPRec^';       Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarPPRec';      Mtch: '^PPRec\('+Match_PasPointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'VarPPRec^';      Mtch: '^PRec\('+Match_PasPointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: []),
    (Exp: 'VarPPRec^^';     Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarTNewRec';    Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'TNewRec'; Flgs: []),

    (Exp: 'PVarTRec';      Mtch: '^(\^T|P)Rec\('+Match_PasPointer;     Kind: skPointer;     TpNm: '^(\^T|P)Rec$'; Flgs: [fTpMtch]), // TODO: stabs returns PRec
    (Exp: 'PVarTRec^';      Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'PVarTNewRec';   Mtch: '^\^TNewRec\('+Match_PasPointer;    Kind: skPointer;     TpNm: '^TNewRec'; Flgs: []),
    (Exp: 'PVarTNewRec^';   Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'TNewRec'; Flgs: []),

      { Classes }

    (Exp: 'ArgTFoo';      Mtch: Match_ArgTFoo;                     Kind: skClass;      TpNm: 'TFoo'; Flgs: []),
    (Exp: 'VArgTFoo';     Mtch: Match_ArgTFoo;                     Kind: skClass;      TpNm: 'TFoo'; Flgs: []),
    (Exp: 'ArgPFoo';      Mtch: 'PFoo\('+Match_PasPointer;          Kind: skPointer;      TpNm: 'PFoo'; Flgs: []),
    (Exp: 'VArgPFoo';     Mtch: 'PFoo\('+Match_PasPointer;          Kind: skPointer;      TpNm: 'PFoo'; Flgs: []),
    (Exp: 'ArgPFoo^';      Mtch: Match_ArgTFoo1;                     Kind: skClass;      TpNm: 'TFoo'; Flgs: []),
    (Exp: 'VArgPFoo^';     Mtch: Match_ArgTFoo1;                     Kind: skClass;      TpNm: 'TFoo'; Flgs: [fnoDwrf]),

    (Exp: 'ArgTFoo=ArgTFoo';      Mtch: 'True';                     Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    (Exp: 'VArgTFoo=VArgTFoo';    Mtch: 'True';                     Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    (Exp: 'ArgTFoo=VArgTFoo';     Mtch: 'True';                     Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    (Exp: 'ArgTFoo=ArgPFoo';      Mtch: 'False';                    Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    (Exp: 'ArgPFoo=ArgPPFoo^';    Mtch: 'True';                     Kind: skSimple;      TpNm: 'bool'; Flgs: []),

    (Exp: '@ArgTFoo';     Mtch: '(P|\^T)Foo\('+Match_PasPointer;    Kind: skPointer;     TpNm: '(P|\^T)Foo'; Flgs: [fTpMtch]),

    (*

    (Exp: 'ArgPPFoo';      Mtch: '';      Kind: sk;      TpNm: 'PPFoo'; Flgs: []),
    (Exp: 'VArgPPFoo';      Mtch: '';      Kind: sk;      TpNm: 'PPFoo'; Flgs: []),
    (Exp: 'ArgTSamePFoo';      Mtch: '';      Kind: sk;      TpNm: 'TSamePFoo'; Flgs: []),
    (Exp: 'VArgTSamePFoo';      Mtch: '';      Kind: sk;      TpNm: 'TSamePFoo'; Flgs: []),
    (Exp: 'ArgTNewPFoo';      Mtch: '';      Kind: sk;      TpNm: 'TNewPFoo'; Flgs: []),
    (Exp: 'VArgTNewPFoo';      Mtch: '';      Kind: sk;      TpNm: 'TNewPFoo'; Flgs: []),

    (Exp: 'ArgTSameFoo';      Mtch: '';      Kind: sk;      TpNm: 'TSameFoo'; Flgs: []),
    (Exp: 'VArgTSameFoo';      Mtch: '';      Kind: sk;      TpNm: 'TSameFoo'; Flgs: []),
    (Exp: 'ArgTNewFoo';      Mtch: '';      Kind: sk;      TpNm: 'TNewFoo'; Flgs: []),
    (Exp: 'VArgTNewFoo';      Mtch: '';      Kind: sk;      TpNm: 'TNewFoo'; Flgs: []),
    (Exp: 'ArgPNewFoo';      Mtch: '';      Kind: sk;      TpNm: 'PNewFoo'; Flgs: []),
    (Exp: 'VArgPNewFoo';      Mtch: '';      Kind: sk;      TpNm: 'PNewFoo'; Flgs: []),

      { ClassesTyps }
    (Exp: 'ArgTFooClass';      Mtch: '';      Kind: sk;      TpNm: 'TFooClass'; Flgs: []),
    (Exp: 'VArgTFooClass';      Mtch: '';      Kind: sk;      TpNm: 'TFooClass'; Flgs: []),
    (Exp: 'ArgPFooClass';      Mtch: '';      Kind: sk;      TpNm: 'PFooClass'; Flgs: []),
    (Exp: 'VArgPFooClass';      Mtch: '';      Kind: sk;      TpNm: 'PFooClass'; Flgs: []),
    (Exp: 'ArgPPFooClass';      Mtch: '';      Kind: sk;      TpNm: 'PPFooClass'; Flgs: []),
    (Exp: 'VArgPPFooClass';      Mtch: '';      Kind: sk;      TpNm: 'PPFooClass'; Flgs: []),
    (Exp: 'ArgTNewFooClass';      Mtch: '';      Kind: sk;      TpNm: 'TNewFooClass'; Flgs: []),
    (Exp: 'VArgTNewFooClass';      Mtch: '';      Kind: sk;      TpNm: 'TNewFooClass'; Flgs: []),
    (Exp: 'ArgPNewFooClass';      Mtch: '';      Kind: sk;      TpNm: 'PNewFooClass'; Flgs: []),
    (Exp: 'VArgPNewFooClass';      Mtch: '';      Kind: sk;      TpNm: 'PNewFooClass'; Flgs: []),
    *)

      { strings }
    (Exp: 'ArgTMyAnsiString';      Mtch: '''ansi''';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyAnsiString';     Mtch: '''ansi''';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyAnsiString';      Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'VArgPMyAnsiString';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'ArgPMyAnsiString^';      Mtch: '''ansi''';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyAnsiString^';     Mtch: '''ansi''';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch, fnoDwrf]),
    (Exp: 'ArgPPMyAnsiString';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PPMyAnsiString'; Flgs: []),
    (Exp: 'VArgPPMyAnsiString';    Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PPMyAnsiString'; Flgs: []),
    (Exp: 'ArgPPMyAnsiString^';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'VArgPPMyAnsiString^';    Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPMyAnsiString^^';    Mtch: '''ansi''';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPPMyAnsiString^^';   Mtch: '''ansi''';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch, fnoDwrf]),

    (*
    (Exp: 'ArgTNewAnsiString';      Mtch: '';      Kind: sk;      TpNm: 'TNewAnsiString'; Flgs: []),
    (Exp: 'VArgTNewAnsiString';     Mtch: '';      Kind: sk;      TpNm: 'TNewAnsiString'; Flgs: []),
    (Exp: 'ArgPNewAnsiString';      Mtch: '';      Kind: sk;      TpNm: 'PNewAnsiString'; Flgs: []),
    (Exp: 'VArgPNewAnsiString';     Mtch: '';      Kind: sk;      TpNm: 'PNewAnsiString'; Flgs: []),
    *)

    (Exp: 'ArgTMyShortString';      Mtch: '''short''';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyShortString';     Mtch: '''short''';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyShortString';      Mtch: Match_Pointer;      Kind: skPointer;     TpNm: 'P(My)?ShortString'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyShortString';     Mtch: Match_Pointer;      Kind: skPointer;     TpNm: 'P(My)?ShortString'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyShortString^';      Mtch: '''short''';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyShortString^';     Mtch: '''short''';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch, fnoDwrf])//,

    (*
    (Exp: 'ArgPPMyShortString';      Mtch: '';      Kind: sk;      TpNm: 'PPMyShortString'; Flgs: []),
    (Exp: 'VArgPPMyShortString';      Mtch: '';      Kind: sk;      TpNm: 'PPMyShortString'; Flgs: []),
    (Exp: 'ArgTNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'TNewhortString'; Flgs: []),
    (Exp: 'VArgTNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'TNewhortString'; Flgs: []),
    (Exp: 'ArgPNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'PNewhortString'; Flgs: []),
    (Exp: 'VArgPNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'PNewhortString'; Flgs: []),

    (Exp: 'ArgTMyWideString';      Mtch: '';      Kind: sk;      TpNm: 'TMyWideString'; Flgs: []),
    (Exp: 'VArgTMyWideString';      Mtch: '';      Kind: sk;      TpNm: 'TMyWideString'; Flgs: []),
    (Exp: 'ArgPMyWideString';      Mtch: '';      Kind: sk;      TpNm: 'PMyWideString'; Flgs: []),
    (Exp: 'VArgPMyWideString';      Mtch: '';      Kind: sk;      TpNm: 'PMyWideString'; Flgs: []),
    (Exp: 'ArgPPMyWideString';      Mtch: '';      Kind: sk;      TpNm: 'PPMyWideString'; Flgs: []),
    (Exp: 'VArgPPMyWideString';      Mtch: '';      Kind: sk;      TpNm: 'PPMyWideString'; Flgs: []),

    (Exp: 'ArgTNewWideString';      Mtch: '';      Kind: sk;      TpNm: 'TNewWideString'; Flgs: []),
    (Exp: 'VArgTNewWideString';      Mtch: '';      Kind: sk;      TpNm: 'TNewWideString'; Flgs: []),
    (Exp: 'ArgPNewWideString';      Mtch: '';      Kind: sk;      TpNm: 'PNewWideString'; Flgs: []),
    (Exp: 'VArgPNewWideString';      Mtch: '';      Kind: sk;      TpNm: 'PNewWideString'; Flgs: []),

    (Exp: 'ArgTMyString10';      Mtch: '';      Kind: sk;      TpNm: 'TMyString10'; Flgs: []),
    (Exp: 'VArgTMyString10';      Mtch: '';      Kind: sk;      TpNm: 'TMyString10'; Flgs: []),
    (Exp: 'ArgPMyString10';      Mtch: '';      Kind: sk;      TpNm: 'PMyString10'; Flgs: []),
    (Exp: 'VArgPMyString10';      Mtch: '';      Kind: sk;      TpNm: 'PMyString10'; Flgs: []),
    (Exp: 'ArgPPMyString10';      Mtch: '';      Kind: sk;      TpNm: 'PPMyString10'; Flgs: []),
    (Exp: 'VArgPPMyString10';      Mtch: '';      Kind: sk;      TpNm: 'PPMyString10'; Flgs: []),

      { simple }

    (Exp: 'ArgByte';      Mtch: '';      Kind: sk;      TpNm: 'Byte'; Flgs: []),
    (Exp: 'VArgByte';      Mtch: '';      Kind: sk;      TpNm: 'Byte'; Flgs: []),
    (Exp: 'ArgWord';      Mtch: '';      Kind: sk;      TpNm: 'Word'; Flgs: []),
    (Exp: 'VArgWord';      Mtch: '';      Kind: sk;      TpNm: 'Word'; Flgs: []),
    (Exp: 'ArgLongWord';      Mtch: '';      Kind: sk;      TpNm: 'LongWord'; Flgs: []),
    (Exp: 'VArgLongWord';      Mtch: '';      Kind: sk;      TpNm: 'LongWord'; Flgs: []),
    (Exp: 'ArgQWord';      Mtch: '';      Kind: sk;      TpNm: 'QWord'; Flgs: []),
    (Exp: 'VArgQWord';      Mtch: '';      Kind: sk;      TpNm: 'QWord'; Flgs: []),

    (Exp: 'ArgShortInt';      Mtch: '';      Kind: sk;      TpNm: 'ShortInt'; Flgs: []),
    (Exp: 'VArgShortInt';      Mtch: '';      Kind: sk;      TpNm: 'ShortInt'; Flgs: []),
    (Exp: 'ArgSmallInt';      Mtch: '';      Kind: sk;      TpNm: 'SmallInt'; Flgs: []),
    (Exp: 'VArgSmallInt';      Mtch: '';      Kind: sk;      TpNm: 'SmallInt'; Flgs: []),
    (Exp: 'ArgInt';      Mtch: '';      Kind: sk;      TpNm: 'Integer'; Flgs: []),
    (Exp: 'VArgInt';      Mtch: '';      Kind: sk;      TpNm: 'Integer'; Flgs: []),
    (Exp: 'ArgInt64';      Mtch: '';      Kind: sk;      TpNm: 'Int64'; Flgs: []),
    (Exp: 'VArgInt64';      Mtch: '';      Kind: sk;      TpNm: 'Int64'; Flgs: []),

    (Exp: 'ArgPByte';      Mtch: '';      Kind: sk;      TpNm: 'PByte'; Flgs: []),
    (Exp: 'VArgPByte';      Mtch: '';      Kind: sk;      TpNm: 'PByte'; Flgs: []),
    (Exp: 'ArgPWord';      Mtch: '';      Kind: sk;      TpNm: 'PWord'; Flgs: []),
    (Exp: 'VArgPWord';      Mtch: '';      Kind: sk;      TpNm: 'PWord'; Flgs: []),
    (Exp: 'ArgPLongWord';      Mtch: '';      Kind: sk;      TpNm: 'PLongWord'; Flgs: []),
    (Exp: 'VArgPLongWord';      Mtch: '';      Kind: sk;      TpNm: 'PLongWord'; Flgs: []),
    (Exp: 'ArgPQWord';      Mtch: '';      Kind: sk;      TpNm: 'PQWord'; Flgs: []),
    (Exp: 'VArgPQWord';      Mtch: '';      Kind: sk;      TpNm: 'PQWord'; Flgs: []),

    (Exp: 'ArgPShortInt';      Mtch: '';      Kind: sk;      TpNm: 'PShortInt'; Flgs: []),
    (Exp: 'VArgPShortInt';      Mtch: '';      Kind: sk;      TpNm: 'PShortInt'; Flgs: []),
    (Exp: 'ArgPSmallInt';      Mtch: '';      Kind: sk;      TpNm: 'PSmallInt'; Flgs: []),
    (Exp: 'VArgPSmallInt';      Mtch: '';      Kind: sk;      TpNm: 'PSmallInt'; Flgs: []),
    (Exp: 'ArgPInt';      Mtch: '';      Kind: sk;      TpNm: 'PInteger'; Flgs: []),
    (Exp: 'VArgPInt';      Mtch: '';      Kind: sk;      TpNm: 'PInteger'; Flgs: []),
    (Exp: 'ArgPInt64';      Mtch: '';      Kind: sk;      TpNm: 'PInt64'; Flgs: []),
    (Exp: 'VArgPInt64';      Mtch: '';      Kind: sk;      TpNm: 'PInt64'; Flgs: []),

    (Exp: 'ArgPointer';      Mtch: '';      Kind: sk;      TpNm: 'Pointer'; Flgs: []),
    (Exp: 'VArgPointer';      Mtch: '';      Kind: sk;      TpNm: 'Pointer'; Flgs: []),
    (Exp: 'ArgPPointer';      Mtch: '';      Kind: sk;      TpNm: 'PPointer'; Flgs: []),
    (Exp: 'VArgPPointer';      Mtch: '';      Kind: sk;      TpNm: 'PPointer'; Flgs: []),

    (Exp: 'ArgDouble';      Mtch: '';      Kind: sk;      TpNm: 'Double'; Flgs: []),
    (Exp: 'VArgDouble';      Mtch: '';      Kind: sk;      TpNm: 'Double'; Flgs: []),
    (Exp: 'ArgExtended';      Mtch: '';      Kind: sk;      TpNm: 'Extended'; Flgs: []),
    (Exp: 'VArgExtended';      Mtch: '';      Kind: sk;      TpNm: 'Extended'; Flgs: []),
*)

  );





{ TTestWatch }

procedure TTestWatch.DoChanged;
begin
  if FMaster = nil then exit;;
  if FMaster.Valid = vsValid then begin
    if FHasValue and (FValue <> FMaster.Value) then begin
      FHasMultiValue := True;
      FValue := FValue + LineEnding + FMaster.Value;
    end
    else
      FValue := FMaster.Value;
    FHasValue := True;

    FTypeInfo := Master.TypeInfo;
  end;
end;

function TTestWatch.GetTypeInfo: TDBGType;
begin
  Result := FTypeInfo;
end;

constructor TTestWatch.Create(AOwner: TBaseWatches; AMaster: TDBGWatch);
begin
  inherited Create(AOwner);
  Expression := AMaster.Expression;
  FMaster := AMaster;
  FMaster.Slave := Self;
  FMaster.Enabled := True;
end;

{ TTestWatches }

procedure TTestWatches.DebugInteract(dbg: TGDBMIDebugger);
var s: string;
begin
  readln(s);
  while s <> '' do begin
    dbg.TestCmd(s);
    readln(s);
  end;
end;

procedure TTestWatches.TestWatches;
var FailText: String;

  procedure TestWatch(Name: String; Data: TWatchExpectation);
  const KindName: array [TDBGSymbolKind] of string =
     ('skClass', 'skRecord', 'skEnum', 'skSet', 'skProcedure', 'skFunction', 'skSimple', 'skPointer', 'skVariant');
  var
    AWatch: TTestWatch;
    rx: TRegExpr;
    s: String;
  begin
    rx := nil;
    if (fnoDwrf in Data.Flgs) and (SymbolType = stDwarf) then exit;
    if (fnoStabs in Data.Flgs) and (SymbolType = stStabs) then exit;

    Name := Name + Data.Exp;
    AWatch := TTestWatch(FWatches.Find(Data.Exp));
    try
      AWatch.Master.Value; // trigger read
      AssertTrue  (Name+ ' (HasValue)',   AWatch.HasValue);
      AssertFalse (Name+ ' (One Value)',  AWatch.HasMultiValue);

      s := AWatch.Value;
      rx := TRegExpr.Create;
      rx.ModifierI := true;
      rx.Expression := Data.Mtch;
      if Data.Mtch <> ''
      then AssertTrue(Name + ' Matches "'+Data.Mtch + '", but was "' + s + '"', rx.Exec(s));
    except
      on e: Exception do
        FailText := FailText + LineEnding + e.Message;
    end;
    try
      if Data.TpNm <> '' then begin;
        AssertTrue(Name + ' has typeinfo',  AWatch.TypeInfo <> nil);
        AssertEquals(Name + ' kind',  KindName[Data.Kind], KindName[AWatch.TypeInfo.Kind]);
        if fTpMtch  in Data.Flgs
        then begin
          FreeAndNil(rx);
          s := AWatch.TypeInfo.TypeName;
          rx := TRegExpr.Create;
          rx.ModifierI := true;
          rx.Expression := Data.TpNm;
          AssertTrue(Name + ' TypeName matches '+Data.TpNm+' but was '+AWatch.TypeInfo.TypeName,  rx.Exec(s))
        end
        else AssertEquals(Name + ' TypeName',  LowerCase(Data.TpNm), LowerCase(AWatch.TypeInfo.TypeName));
      end;
    except
      on e: Exception do
        FailText := FailText + LineEnding + e.Message;
    end;
    FreeAndNil(rx);
  end;

var
  TestExeName: string;
  dbg: TGDBMIDebugger;
  i: Integer;
begin
  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);

  try
    FWatches := TBaseWatches.Create(TBaseWatch);
    dbg := TGDBMIDebugger.Create(DebuggerInfo.ExeName);

    (* Add breakpoints *)
    //with dbg.BreakPoints.Add('WatchesPrg.pas', 44) do begin
    //  InitialEnabled := True;
    //  Enabled := True;
    //end;
    with dbg.BreakPoints.Add('WatchesPrg.pas', 395) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    (* Create all watches *)
    for i := low(ExpectBrk1NoneNil) to high(ExpectBrk1NoneNil) do
      TTestWatch.Create(FWatches, dbg.Watches.Add(ExpectBrk1NoneNil[i].Exp));


    (* Start debugging *)
    dbg.Init;
    if dbg.State = dsError then
      Fail(' Failed Init');

    dbg.WorkingDir := AppDir;
    dbg.FileName   := TestExeName;
    dbg.Arguments := '';
    dbg.ShowConsole := True;

    dbg.Run;

    (* Hit first breakpoint: Test *)
    for i := low(ExpectBrk1NoneNil) to high(ExpectBrk1NoneNil) do
    TestWatch('Brk1 ', ExpectBrk1NoneNil[i]);

    dbg.Run;

	//DebugInteract(dbg);

    dbg.Stop;
  finally
    dbg.Free;
    FreeAndNil(FWatches);

    //debugln(FailText)
    if FailText <> '' then fail(FailText);
  end;
end;



initialization

  RegisterDbgTest(TTestWatches);
end.

