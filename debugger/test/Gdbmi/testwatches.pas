unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  TestBase, Debugger, GDBMIDebugger, LCLProc, SynRegExpr, Forms, StdCtrls, Controls;

const
  BREAK_LINE_FOOFUNC = 113;
  RUN_TEST_ONLY = -1; // -1 run all

(*  TODO:
  - procedure of object currently is skRecord
  - proc/func under stabs => just happen too match because the function they point too..

  - TREC vs TNewRec ("type TnewRec = type TRec;")
    for quick eval TRec is fine, but in other modes TNewRec may be needed (requires an extra "whatis" under dwarf)

  - widestring in gdb 6.7.5
  - FooObject = BarObject (dwarf 3)
*)

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
    procedure DoDbgOutput(Sender: TObject; const AText: String);
  public
    procedure DebugInteract(dbg: TGDBMIDebugger);

  published
    procedure TestWatches;
  end;

implementation

var
  DbgForm: TForm;
  DbgMemo: TMemo;
  DbgLog: Boolean;

const
  RNoPreQuote  = '(^|[^''])'; // No open qoute (Either at start, or other char)
  RNoPostQuote = '($|[^''])'; // No close qoute (Either at end, or other char)

type
  TWatchExpectationFlag =
    (fnoDwrf,      // no dwarf at all
     fnoDwrfNoSet, // no dwarf2 (-gw) without set
     //fnoDwrfSet,   // no dwarf2 with set // no dwarf3
     fnoDwrf2, fnoDwrf3,
     fnoStabs, // no stabs
     fTpMtch
    );
  TWatchExpectationFlags = set of TWatchExpectationFlag;
  TWatchExpectation = record
    Exp:  string;
    Mtch: string;
    Kind: TDBGSymbolKind;
    TpNm: string;
    Flgs: TWatchExpectationFlags;
  end;

const
  Match_Pointer = '\$[0-9A-F]+';
  Match_ArgTRec  = 'record TREC .+ valint = -1.+valfoo'; // record TREC {  VALINT = -1,  VALFOO = $0}
  Match_ArgTRec1 = 'record TREC .+ valint = 1.+valfoo'; // record TREC {  VALINT = 1,  VALFOO = $xxx}
  Match_ArgTRec2 = 'record TREC .+ valint = 2.+valfoo'; // record TREC {  VALINT = 2,  VALFOO = $xxx}
  // TODO: TNewRec
  Match_ArgTNewRec  = 'record T(NEW)?REC .+ valint = 3.+valfoo'; // record TREC {  VALINT = 3,  VALFOO = $0}

  {%region    * Classes * }
  Match_ArgTFoo = '<TFoo> = \{.*<TObject>.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*<TObject>.+ValueInt = 31';
  {%ebdregion    * Classes * }
  // Todo: Dwarf fails with dereferenced var pointer types

  ExpectBrk1NoneNil: Array [1..116] of TWatchExpectation = (
    {%region    * records * }

    (Exp: 'ArgTRec';       Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgTRec';      Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'ArgPRec';       Mtch: '^PRec\('+Match_Pointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'VArgPRec';      Mtch: '^PRec\('+Match_Pointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'ArgPRec^';       Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgPRec^';      Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPRec';      Mtch: '^PPRec\('+Match_Pointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'VArgPPRec';     Mtch: '^PPRec\('+Match_Pointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'ArgPPRec^';      Mtch: '^PRec\('+Match_Pointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: []),
    (Exp: 'VArgPPRec^';     Mtch: '^PRec\('+Match_Pointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPRec^^';     Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgPPRec^^';    Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgTNewRec';    Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),
    (Exp: 'VArgTNewRec';   Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),

    (Exp: 'VarTRec';       Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarPRec';       Mtch: '^PRec\('+Match_Pointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'VarPRec^';       Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarPPRec';      Mtch: '^PPRec\('+Match_Pointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'VarPPRec^';      Mtch: '^PRec\('+Match_Pointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: []),
    (Exp: 'VarPPRec^^';     Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarTNewRec';    Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),

    (Exp: 'PVarTRec';      Mtch: '^(\^T|P)Rec\('+Match_Pointer;     Kind: skPointer;     TpNm: '^(\^T|P)Rec$'; Flgs: [fTpMtch]), // TODO: stabs returns PRec
    (Exp: 'PVarTRec^';      Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'PVarTNewRec';   Mtch: '^\^TNewRec\('+Match_Pointer;    Kind: skPointer;     TpNm: '\^T(New)?Rec'; Flgs: [fTpMtch]),
    (Exp: 'PVarTNewRec^';   Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),
    {%endregion    * records * }

// @ArgTRec @VArgTRec  @ArgTRec^ @VArgTRec^

    {%region    * Classes * }

    (Exp: 'ArgTFoo';    Mtch: Match_ArgTFoo;                 Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: '@ArgTFoo';   Mtch: '(P|\^T)Foo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^T)Foo';  Flgs: [fTpMtch]),
    // Only with brackets...
    (Exp: '(@ArgTFoo)^'; Mtch: Match_ArgTFoo;                Kind: skClass;   TpNm: 'TFoo';  Flgs: []),

    (Exp: 'ArgPFoo';    Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: []),
    (Exp: 'ArgPFoo^';   Mtch: Match_ArgTFoo1;                Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: '@ArgPFoo';   Mtch: '(P|\^)PFoo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^)PFoo';  Flgs: [fTpMtch]),

    (Exp: 'ArgPPFoo';   Mtch: 'PPFoo\('+Match_Pointer;       Kind: skPointer; TpNm: 'PPFoo'; Flgs: []),
    (Exp: 'ArgPPFoo^';  Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: []),
    (Exp: '@ArgPPFoo';  Mtch: '\^PPFoo\('+Match_Pointer;      Kind: skPointer; TpNm: '^PPFoo'; Flgs: []),
    (Exp: 'ArgPPFoo^^'; Mtch: Match_ArgTFoo1;                Kind: skClass;   TpNm: 'TFoo';  Flgs: []),


    (Exp: 'VArgTFoo';   Mtch: Match_ArgTFoo;                 Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: '@VArgTFoo';  Mtch: '(P|\^T)Foo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^T)Foo';  Flgs: [fTpMtch]),
    (Exp: '(@VArgTFoo)^'; Mtch: Match_ArgTFoo;                 Kind: skClass;   TpNm: 'TFoo';  Flgs: []),

    (Exp: 'VArgPFoo';   Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: []),
    (Exp: 'VArgPFoo^' ; Mtch: Match_ArgTFoo1;                Kind: skClass;   TpNm: 'TFoo';  Flgs: [fnoDwrf]),
    (Exp: '@VArgPFoo';  Mtch: '(P|\^)PFoo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^)PFoo';  Flgs: [fTpMtch]),

    (Exp: 'VArgPPFoo';  Mtch: 'PPFoo\('+Match_Pointer;       Kind: skPointer; TpNm: 'PPFoo'; Flgs: []),
    (Exp: 'VArgPPFoo^'; Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: [fnoDwrf]),
    (Exp: '@VArgPPFoo'; Mtch: '\^PPFoo\('+Match_Pointer;      Kind: skPointer; TpNm: '^PPFoo'; Flgs: []),
    (Exp: 'VArgPPFoo^^'; Mtch: Match_ArgTFoo1;               Kind: skClass;   TpNm: 'TFoo';  Flgs: [fnoDwrf]),


    (Exp: 'ArgTFoo.ValueInt';     Mtch: '^-11$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    (Exp: 'ArgPFoo^.ValueInt';    Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    // GDB automatically derefs the pointer
    //(Exp: 'ArgPFoo.ValueInt';     Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),
    (Exp: 'ArgPPFoo^^.ValueInt';  Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    //(Exp: 'ArgPPFoo.ValueInt';     Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),

    (Exp: 'VArgTFoo.ValueInt';    Mtch: '^-11$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    (Exp: 'VArgPFoo^.ValueInt';   Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    //(Exp: 'VArgPFoo.ValueInt';    Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),
    (Exp: 'VArgPPFoo^^.ValueInt'; Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    //(Exp: 'VArgPPFoo.ValueInt';    Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),

    (*

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

    { Compare Objects }
    // TODO: not working in Dwarf3
    (Exp: 'ArgTFoo=ArgTFoo';      Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'not(ArgTFoo=ArgTFoo)'; Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'VArgTFoo=VArgTFoo';    Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgTFoo=VArgTFoo';     Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgTFoo=ArgPFoo';      Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgTFoo=ArgPFoo^';     Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgPFoo=ArgPPFoo^';    Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),

    //(Exp: 'ArgTFoo<>ArgTFoo';     Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    //(Exp: 'ArgTFoo<>ArgPFoo^';    Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),

    (Exp: 'ArgTFoo=0';          Mtch: 'False';          Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'not(ArgTFoo=0)';     Mtch: 'True';           Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    //(Exp: 'ArgTFoo<>0';         Mtch: 'True';           Kind: skSimple;      TpNm: 'bool'; Flgs: []),

    //(Exp: 'ArgTFoo=nil';          Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    //(Exp: 'not(ArgTFoo=nil)';     Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    //(Exp: 'ArgTFoo<>nil';         Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    {%endendregion    * Classes * }

    {%region    * Strings * }
      { strings }
    (Exp: 'ArgTMyAnsiString';      Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyAnsiString';     Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyAnsiString';      Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'VArgPMyAnsiString';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'ArgPMyAnsiString^';      Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyAnsiString^';     Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch, fnoDwrf]),
    (Exp: 'ArgPPMyAnsiString';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PPMyAnsiString'; Flgs: []),
    (Exp: 'VArgPPMyAnsiString';    Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PPMyAnsiString'; Flgs: []),
    (Exp: 'ArgPPMyAnsiString^';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'VArgPPMyAnsiString^';    Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPMyAnsiString^^';    Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPPMyAnsiString^^';   Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch, fnoDwrf]),

    (*
    (Exp: 'ArgTNewAnsiString';      Mtch: '';      Kind: sk;      TpNm: 'TNewAnsiString'; Flgs: []),
    (Exp: 'VArgTNewAnsiString';     Mtch: '';      Kind: sk;      TpNm: 'TNewAnsiString'; Flgs: []),
    (Exp: 'ArgPNewAnsiString';      Mtch: '';      Kind: sk;      TpNm: 'PNewAnsiString'; Flgs: []),
    (Exp: 'VArgPNewAnsiString';     Mtch: '';      Kind: sk;      TpNm: 'PNewAnsiString'; Flgs: []),
    *)

    (Exp: 'ArgTMyShortString';      Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyShortString';     Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyShortString';      Mtch: Match_Pointer;      Kind: skPointer;     TpNm: 'P(My)?ShortString'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyShortString';     Mtch: Match_Pointer;      Kind: skPointer;     TpNm: 'P(My)?ShortString'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyShortString^';      Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyShortString^';     Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch, fnoDwrf]),

    (*
    (Exp: 'ArgPPMyShortString';      Mtch: '';      Kind: sk;      TpNm: 'PPMyShortString'; Flgs: []),
    (Exp: 'VArgPPMyShortString';      Mtch: '';      Kind: sk;      TpNm: 'PPMyShortString'; Flgs: []),
    (Exp: 'ArgTNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'TNewhortString'; Flgs: []),
    (Exp: 'VArgTNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'TNewhortString'; Flgs: []),
    (Exp: 'ArgPNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'PNewhortString'; Flgs: []),
    (Exp: 'VArgPNewhortString';      Mtch: '';      Kind: sk;      TpNm: 'PNewhortString'; Flgs: []),
*)

    // gdb 6.7.5 does not show the text
    (Exp: 'ArgTMyWideString';      Mtch: '(''wide''$)|(widestring\(\$.*\))';      Kind: skPointer;      TpNm: '^(TMy)?WideString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyWideString';     Mtch: '(''wide''$)|(widestring\(\$.*\))';      Kind: skPointer;      TpNm: '^(TMy)?WideString$'; Flgs: [fTpMtch]),
    (*
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
*)
    {%endregion    * Strings * }

    {%region    * Simple * }

    (Exp: 'ArgByte';      Mtch: '^25$';      Kind: skSimple;      TpNm: 'Byte'; Flgs: []),
    (Exp: 'VArgByte';     Mtch: '^25$';      Kind: skSimple;      TpNm: 'Byte'; Flgs: []),
    (Exp: 'ArgWord';      Mtch: '^26$';      Kind: skSimple;      TpNm: 'Word'; Flgs: []),
    (Exp: 'VArgWord';     Mtch: '^26$';      Kind: skSimple;      TpNm: 'Word'; Flgs: []),
    (Exp: 'ArgLongWord';  Mtch: '^27$';      Kind: skSimple;      TpNm: 'LongWord'; Flgs: []),
    (Exp: 'VArgLongWord'; Mtch: '^27$';      Kind: skSimple;      TpNm: 'LongWord'; Flgs: []),
    (Exp: 'ArgQWord';     Mtch: '^28$';      Kind: skSimple;      TpNm: 'QWord'; Flgs: []),
    (Exp: 'VArgQWord';    Mtch: '^28$';      Kind: skSimple;      TpNm: 'QWord'; Flgs: []),

    (Exp: 'ArgShortInt';  Mtch: '^35$';      Kind: skSimple;      TpNm: 'ShortInt'; Flgs: []),
    (Exp: 'VArgShortInt'; Mtch: '^35$';      Kind: skSimple;      TpNm: 'ShortInt'; Flgs: []),
    (Exp: 'ArgSmallInt';  Mtch: '^36$';      Kind: skSimple;      TpNm: 'SmallInt'; Flgs: []),
    (Exp: 'VArgSmallInt'; Mtch: '^36$';      Kind: skSimple;      TpNm: 'SmallInt'; Flgs: []),
    (Exp: 'ArgInt';       Mtch: '^37$';      Kind: skSimple;      TpNm: 'Integer|LongInt'; Flgs: [fTpMtch]),
    (Exp: 'VArgInt';      Mtch: '^37$';      Kind: skSimple;      TpNm: 'Integer|LongInt'; Flgs: [fTpMtch]),
    (Exp: 'ArgInt64';     Mtch: '^38$';      Kind: skSimple;      TpNm: 'Int64'; Flgs: []),
    (Exp: 'VArgInt64';    Mtch: '^38$';      Kind: skSimple;      TpNm: 'Int64'; Flgs: []),

    (Exp: 'ArgPointer';      Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'Pointer'; Flgs: []),
    (Exp: 'VArgPointer';     Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'Pointer'; Flgs: []),
    (*
    (Exp: 'ArgPPointer';      Mtch: '';      Kind: sk;      TpNm: 'PPointer'; Flgs: []),
    (Exp: 'VArgPPointer';      Mtch: '';      Kind: sk;      TpNm: 'PPointer'; Flgs: []),
*)

    (Exp: 'ArgDouble';       Mtch: '1\.123';      Kind: skSimple;      TpNm: 'Double'; Flgs: []),
    (Exp: 'VArgDouble';      Mtch: '1\.123';      Kind: skSimple;      TpNm: 'Double'; Flgs: []),
    (Exp: 'ArgExtended';     Mtch: '2\.345';      Kind: skSimple;      TpNm: 'Extended'; Flgs: []),
    (Exp: 'VArgExtended';    Mtch: '2\.345';      Kind: skSimple;      TpNm: 'Extended'; Flgs: []),

    (*
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
*)
    {%endregion    * Simple * }

    {%region    * Enum/Set * }

    (Exp: 'ArgEnum';       Mtch: '^Two$';                      Kind: skEnum;       TpNm: 'TEnum'; Flgs: []),
    (Exp: 'ArgEnumSet';    Mtch: '^\[Two(, ?|\.\.)Three\]$';   Kind: skSet;        TpNm: 'TEnumSet'; Flgs: [fnoDwrfNoSet]),
    (Exp: 'ArgSet';        Mtch: '^\[Alpha(, ?|\.\.)Beta\]$';  Kind: skSet;        TpNm: 'TSet'; Flgs: [fnoDwrfNoSet]),

    (Exp: 'VarEnumA';      Mtch: '^e3$';                       Kind: skEnum;       TpNm: ''; Flgs: []),
    // maybe typename = "set of TEnum"
    (Exp: 'VarEnumSetA';   Mtch: '^\[Three\]$';                Kind: skSet;        TpNm: ''; Flgs: [fnoDwrfNoSet]),
    (Exp: 'VarSetA';       Mtch: '^\[s2\]$';                   Kind: skSet;        TpNm: ''; Flgs: [fnoDwrfNoSet]),
    {%endregion    * Enum/Set * }

    {%region    * Variant * }

    (Exp: 'ArgVariantInt';       Mtch: '^5$';         Kind: skVariant;       TpNm: 'Variant'; Flgs: []),
    (Exp: 'ArgVariantString';    Mtch: '^''v''$';     Kind: skVariant;       TpNm: 'Variant'; Flgs: []),

    (Exp: 'VArgVariantInt';       Mtch: '^5$';         Kind: skVariant;       TpNm: 'Variant'; Flgs: []),
    (Exp: 'VArgVariantString';    Mtch: '^''v''$';     Kind: skVariant;       TpNm: 'Variant'; Flgs: []),
    {%endregion    * Variant * }

    {%region    * procedure/function/method * }

    (Exp: 'ArgProcedure';       Mtch: 'procedure';           Kind: skProcedure;       TpNm: 'TProcedure'; Flgs: []),
    (Exp: 'ArgFunction';        Mtch: 'function';            Kind: skFunction;        TpNm: 'TFunction';  Flgs: []),
(*
// normal procedure on stabs / recodr on dwarf => maybe the data itself may reveal some ?
    (Exp: 'ArgObjProcedure';    Mtch: 'procedure.*of object|record.*procedure.*self =';
                                                             Kind: skRecord;          TpNm: 'TObjProcedure'; Flgs: []),
    (Exp: 'ArgObjFunction';     Mtch: 'function.*of object|record.*function.*self =';
                                                             Kind: skRecord;          TpNm: 'TObjFunction';  Flgs: []),

*)
// doesn't work, ptype returns empty in dwarf => maybe via whatis
//    (Exp: 'VArgProcedure';       Mtch: 'procedure';           Kind: skProcedure;       TpNm: 'TProcedure'; Flgs: []),
//    (Exp: 'VArgFunction';        Mtch: 'function';            Kind: skFunction;        TpNm: 'TFunction';  Flgs: []),
(*
    (Exp: 'VArgObjProcedure';    Mtch: 'procedure.*of object|record.*procedure.*self =';
                                                              Kind: skRecord;          TpNm: 'TObjProcedure'; Flgs: []),
    (Exp: 'VArgObjFunction';     Mtch: 'function.*of object|record.*function.*self =';
                                                              Kind: skRecord;          TpNm: 'TObjFunction';  Flgs: []),
*)

    (Exp: 'VarProcedureA';       Mtch: 'procedure';           Kind: skProcedure;       TpNm: 'Procedure'; Flgs: []),
    (Exp: 'VarFunctionA';        Mtch: 'function';            Kind: skFunction;        TpNm: 'Function';  Flgs: [])//,
(*
    (Exp: 'VarObjProcedureA';    Mtch: 'procedure.*of object|record.*procedure.*self =';
                                                              Kind: skRecord;          TpNm: 'Procedure'; Flgs: []),
    (Exp: 'VarObjFunctionA';     Mtch: 'function.*of object|record.*function.*self =';
                                                              Kind: skRecord;          TpNm: 'Function';  Flgs: [])//,
*)
    {%endregion    * procedure/function/method * }

  );





{ TTestWatch }

procedure TTestWatch.DoChanged;
var
  v: String;
begin
  if FMaster = nil then exit;
  if (FMaster.Valid = vsValid) then begin
    DbgLog := True;
    v := FMaster.Value;
    if v <> '<evaluating>' then begin // TODO: need better check
      if FHasValue and (FValue <> v) then begin
        FHasMultiValue := True;
        FValue := FValue + LineEnding + v;
      end
      else
        FValue := v;
      FHasValue := True;

      FTypeInfo := Master.TypeInfo;
    end;
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

procedure TTestWatches.DoDbgOutput(Sender: TObject; const AText: String);
begin
  if DbgLog then
    DbgMemo.Lines.Add(AText);
end;

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

  function SkipTest(const Data: TWatchExpectation): Boolean;
  begin
    Result := True;
    if (fnoDwrf in Data.Flgs)      and (SymbolType in [stDwarf, stDwarfSet, stDwarf3]) then exit;
    if (fnoDwrfNoSet in Data.Flgs) and (SymbolType in [stDwarf]) then exit;
    //if (fnoDwrfSet   in Data.Flgs) and (SymbolType in [stDwarfSet, stDwarf3]) then exit;
    if (fnoDwrf2 in Data.Flgs)     and (SymbolType in [stDwarf, stDwarfSet]) then exit;
    if (fnoDwrf3 in Data.Flgs)     and (SymbolType in [stDwarf3]) then exit;

    if (fnoStabs in Data.Flgs) and (SymbolType = stStabs) then exit;
    Result := False;
  end;

  procedure TestWatch(Name: String; Data: TWatchExpectation);
  const KindName: array [TDBGSymbolKind] of string =
     ('skClass', 'skRecord', 'skEnum', 'skSet', 'skProcedure', 'skFunction', 'skSimple', 'skPointer', 'skVariant');
  var
    AWatch: TTestWatch;
    rx: TRegExpr;
    s: String;
  begin
    rx := nil;

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

    if RUN_TEST_ONLY >= 0 then begin
      dbg.OnDbgOutput  := @DoDbgOutput;
      DbgLog := False;
      if DbgForm = nil then begin
        DbgForm := TForm.Create(Application);
        DbgMemo := TMemo.Create(DbgForm);
        DbgMemo.Parent := DbgForm;
        DbgMemo.Align := alClient;
        DbgForm.Show;
      end;
      DbgMemo.Lines.Add('');
      DbgMemo.Lines.Add(' *** ' + Parent.TestSuiteName + ' ' + Parent.TestName + ' ' + TestSuiteName+' '+TestName);
      DbgMemo.Lines.Add('');
    end;

    (* Add breakpoints *)
    //with dbg.BreakPoints.Add('WatchesPrg.pas', 44) do begin
    //  InitialEnabled := True;
    //  Enabled := True;
    //end;
    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    (* Create all watches *)
    if RUN_TEST_ONLY >= 0 then
      TTestWatch.Create(FWatches, dbg.Watches.Add(ExpectBrk1NoneNil[RUN_TEST_ONLY].Exp))
    else
      for i := low(ExpectBrk1NoneNil) to high(ExpectBrk1NoneNil) do begin
        if not SkipTest(ExpectBrk1NoneNil[i]) then
          TTestWatch.Create(FWatches, dbg.Watches.Add(ExpectBrk1NoneNil[i].Exp));
      end;


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
    if RUN_TEST_ONLY >= 0 then
      TestWatch('Brk1 ', ExpectBrk1NoneNil[RUN_TEST_ONLY])
    else
      for i := low(ExpectBrk1NoneNil) to high(ExpectBrk1NoneNil) do begin
        if not SkipTest(ExpectBrk1NoneNil[i]) then
          TestWatch('Brk1 '+IntToStr(i)+' ', ExpectBrk1NoneNil[i]);
      end;

    dbg.Run;

	//DebugInteract(dbg);

    dbg.Stop;
  finally
    dbg.Free;
    FreeAndNil(FWatches);

    if (DbgMemo <> nil) and (FailText <> '') then DbgMemo.Lines.Add(FailText);
    //debugln(FailText)
    if FailText <> '' then fail(FailText);
  end;
end;



initialization

  RegisterDbgTest(TTestWatches);
end.

