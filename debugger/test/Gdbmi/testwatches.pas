unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc, SynRegExpr, Forms, StdCtrls, Controls;

const
  BREAK_LINE_FOOFUNC = 113;
  RUN_GDB_TEST_ONLY = -1; // -1 run all
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
  { TTestWatches }

  TTestWatches = class(TGDBTestCase)
  private
    FWatches: TcurrentWatches;
    FDbgOutPut: String;
    FDbgOutPutEnable: Boolean;
    procedure DoDbgOutput(Sender: TObject; const AText: String); override;
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
    Fmt: TWatchDisplayFormat;

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
  // _vptr$TOBJECt on older gdb e.g. mac 6.3.50
  Match_ArgTFoo = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = 31';
  {%ebdregion    * Classes * }
  // Todo: Dwarf fails with dereferenced var pointer types

  // direct commands to gdb, to check assumptions
  // only Exp and Mtch
  ExpectGdbBrk1NoneNil: Array [1..4] of TWatchExpectation = (
    (Exp: 'ptype ArgTFoo'; Fmt: wdfDefault; Mtch: 'type = \^TFoo = class : PUBLIC TObject';  Kind: skClass; TpNm: '';  Flgs: []),
    (Exp: 'ptype ArgTFoo^'; Fmt: wdfDefault; Mtch: 'type = TFoo = class : PUBLIC TObject';  Kind: skClass; TpNm: '';  Flgs: []),

    (Exp: '-data-evaluate-expression sizeof(ArgTFoo)'; Fmt: wdfDefault; Mtch: 'value="(4|8)"|(parse|syntax) error in expression';  Kind: skClass; TpNm: '';  Flgs: []),
    (Exp: '-data-evaluate-expression sizeof(ArgTFoo^)'; Fmt: wdfDefault; Mtch: 'value="\d\d+"|(parse|syntax) error in expression';  Kind: skClass; TpNm: '';  Flgs: [])//,
  );

  (* *****     *****     *****     *****     *****     *****     ***** *)

  ExpectBrk1NoneNil: Array [1..133] of TWatchExpectation = (
    {%region    * records * }

    (Exp: 'ArgTRec';       Fmt: wdfDefault;  Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgTRec';      Fmt: wdfDefault;  Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'ArgPRec';       Fmt: wdfDefault;  Mtch: '^PRec\('+Match_Pointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'VArgPRec';      Fmt: wdfDefault;  Mtch: '^PRec\('+Match_Pointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'ArgPRec^';       Fmt: wdfDefault;  Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgPRec^';      Fmt: wdfDefault;  Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPRec';      Fmt: wdfDefault;  Mtch: '^PPRec\('+Match_Pointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'VArgPPRec';     Fmt: wdfDefault;  Mtch: '^PPRec\('+Match_Pointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'ArgPPRec^';      Fmt: wdfDefault;  Mtch: '^PRec\('+Match_Pointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: []),
    (Exp: 'VArgPPRec^';     Fmt: wdfDefault;  Mtch: '^PRec\('+Match_Pointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPRec^^';     Fmt: wdfDefault;  Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VArgPPRec^^';    Fmt: wdfDefault;  Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: [fnoDwrf]),
    (Exp: 'ArgTNewRec';    Fmt: wdfDefault;  Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),
    (Exp: 'VArgTNewRec';   Fmt: wdfDefault;  Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),

    (Exp: 'VarTRec';       Fmt: wdfDefault;  Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarPRec';       Fmt: wdfDefault;  Mtch: '^PRec\('+Match_Pointer;      Kind: skPointer;     TpNm: 'PRec'; Flgs: []),
    (Exp: 'VarPRec^';       Fmt: wdfDefault;  Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarPPRec';      Fmt: wdfDefault;  Mtch: '^PPRec\('+Match_Pointer;     Kind: skPointer;     TpNm: 'PPRec'; Flgs: []),
    (Exp: 'VarPPRec^';      Fmt: wdfDefault;  Mtch: '^PRec\('+Match_Pointer;     Kind: skPointer;      TpNm: 'PRec'; Flgs: []),
    (Exp: 'VarPPRec^^';     Fmt: wdfDefault;  Mtch: Match_ArgTRec1;                  Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'VarTNewRec';    Fmt: wdfDefault;  Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),

    (Exp: 'PVarTRec';      Fmt: wdfDefault;  Mtch: '^(\^T|P)Rec\('+Match_Pointer;     Kind: skPointer;     TpNm: '^(\^T|P)Rec$'; Flgs: [fTpMtch]), // TODO: stabs returns PRec
    (Exp: 'PVarTRec^';      Fmt: wdfDefault;  Mtch: Match_ArgTRec;                   Kind: skRecord;      TpNm: 'TRec'; Flgs: []),
    (Exp: 'PVarTNewRec';   Fmt: wdfDefault;  Mtch: '^\^TNewRec\('+Match_Pointer;    Kind: skPointer;     TpNm: '\^T(New)?Rec'; Flgs: [fTpMtch]),
    (Exp: 'PVarTNewRec^';   Fmt: wdfDefault;  Mtch: Match_ArgTNewRec;                Kind: skRecord;      TpNm: 'T(New)?Rec'; Flgs: [fTpMtch]),
    {%endregion    * records * }

// @ArgTRec @VArgTRec  @ArgTRec^ @VArgTRec^

    {%region    * Classes * }

    (Exp: 'ArgTFoo';    Fmt: wdfDefault;  Mtch: Match_ArgTFoo;                 Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: '@ArgTFoo';   Fmt: wdfDefault;  Mtch: '(P|\^T)Foo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^T)Foo';  Flgs: [fTpMtch]),
    // Only with brackets...
    (Exp: '(@ArgTFoo)^'; Fmt: wdfDefault;  Mtch: Match_ArgTFoo;                Kind: skClass;   TpNm: 'TFoo';  Flgs: []),

    (Exp: 'ArgPFoo';    Fmt: wdfDefault;  Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: []),
    (Exp: 'ArgPFoo^';   Fmt: wdfDefault;  Mtch: Match_ArgTFoo1;                Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: '@ArgPFoo';   Fmt: wdfDefault;  Mtch: '(P|\^)PFoo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^)PFoo';  Flgs: [fTpMtch]),

    (Exp: 'ArgPPFoo';   Fmt: wdfDefault;  Mtch: 'PPFoo\('+Match_Pointer;       Kind: skPointer; TpNm: 'PPFoo'; Flgs: []),
    (Exp: 'ArgPPFoo^';  Fmt: wdfDefault;  Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: []),
    (Exp: '@ArgPPFoo';  Fmt: wdfDefault;  Mtch: '\^PPFoo\('+Match_Pointer;      Kind: skPointer; TpNm: '^PPFoo'; Flgs: []),
    (Exp: 'ArgPPFoo^^'; Fmt: wdfDefault;  Mtch: Match_ArgTFoo1;                Kind: skClass;   TpNm: 'TFoo';  Flgs: []),


    (Exp: 'VArgTFoo';   Fmt: wdfDefault;  Mtch: Match_ArgTFoo;                 Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: '@VArgTFoo';  Fmt: wdfDefault;  Mtch: '(P|\^T)Foo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^T)Foo';  Flgs: [fTpMtch]),
    (Exp: '(@VArgTFoo)^'; Fmt: wdfDefault;  Mtch: Match_ArgTFoo;                 Kind: skClass;   TpNm: 'TFoo';  Flgs: []),

    (Exp: 'VArgPFoo';   Fmt: wdfDefault;  Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: []),
    (Exp: 'VArgPFoo^' ; Fmt: wdfDefault;  Mtch: Match_ArgTFoo1;                Kind: skClass;   TpNm: 'TFoo';  Flgs: [fnoDwrf]),
    (Exp: '@VArgPFoo';  Fmt: wdfDefault;  Mtch: '(P|\^)PFoo\('+Match_Pointer;  Kind: skPointer; TpNm: '(P|\^)PFoo';  Flgs: [fTpMtch]),

    (Exp: 'VArgPPFoo';  Fmt: wdfDefault;  Mtch: 'PPFoo\('+Match_Pointer;       Kind: skPointer; TpNm: 'PPFoo'; Flgs: []),
    (Exp: 'VArgPPFoo^'; Fmt: wdfDefault;  Mtch: 'PFoo\('+Match_Pointer;        Kind: skPointer; TpNm: 'PFoo';  Flgs: [fnoDwrf]),
    (Exp: '@VArgPPFoo'; Fmt: wdfDefault;  Mtch: '\^PPFoo\('+Match_Pointer;      Kind: skPointer; TpNm: '^PPFoo'; Flgs: []),
    (Exp: 'VArgPPFoo^^'; Fmt: wdfDefault;  Mtch: Match_ArgTFoo1;               Kind: skClass;   TpNm: 'TFoo';  Flgs: [fnoDwrf]),


    (Exp: 'ArgTFoo.ValueInt';     Fmt: wdfDefault;  Mtch: '^-11$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    (Exp: 'ArgPFoo^.ValueInt';    Fmt: wdfDefault;  Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    // GDB automatically derefs the pointer
    //(Exp: 'ArgPFoo.ValueInt';     Fmt: wdfDefault;  Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),
    (Exp: 'ArgPPFoo^^.ValueInt';  Fmt: wdfDefault;  Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    //(Exp: 'ArgPPFoo.ValueInt';     Fmt: wdfDefault;  Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),

    (Exp: 'VArgTFoo.ValueInt';    Fmt: wdfDefault;  Mtch: '^-11$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    (Exp: 'VArgPFoo^.ValueInt';   Fmt: wdfDefault;  Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    //(Exp: 'VArgPFoo.ValueInt';    Fmt: wdfDefault;  Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),
    (Exp: 'VArgPPFoo^^.ValueInt'; Fmt: wdfDefault;  Mtch: '^31$';             Kind: skSimple;   TpNm: 'Integer|LongInt';  Flgs: [fTpMtch]),
    //(Exp: 'VArgPPFoo.ValueInt';    Fmt: wdfDefault;  Mtch: 'error';            Kind: skSimple;   TpNm: '';  Flgs: []),


    (Exp: 'ArgTFoo';    Fmt: wdfPointer;  Mtch: Match_Pointer;                           Kind: skClass;   TpNm: 'TFoo';  Flgs: []),
    (Exp: 'ArgTFoo';    Fmt: wdfMemDump;  Mtch: ':.*?6D 65 6D 20 6F 66 20 54 46 6F 6F';  Kind: skClass;   TpNm: 'TFoo';  Flgs: []),

    (*

    (Exp: 'ArgTSamePFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TSamePFoo'; Flgs: []),
    (Exp: 'VArgTSamePFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TSamePFoo'; Flgs: []),
    (Exp: 'ArgTNewPFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewPFoo'; Flgs: []),
    (Exp: 'VArgTNewPFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewPFoo'; Flgs: []),

    (Exp: 'ArgTSameFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TSameFoo'; Flgs: []),
    (Exp: 'VArgTSameFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TSameFoo'; Flgs: []),
    (Exp: 'ArgTNewFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewFoo'; Flgs: []),
    (Exp: 'VArgTNewFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewFoo'; Flgs: []),
    (Exp: 'ArgPNewFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewFoo'; Flgs: []),
    (Exp: 'VArgPNewFoo';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewFoo'; Flgs: []),

      { ClassesTyps }
    (Exp: 'ArgTFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TFooClass'; Flgs: []),
    (Exp: 'VArgTFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TFooClass'; Flgs: []),
    (Exp: 'ArgPFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PFooClass'; Flgs: []),
    (Exp: 'VArgPFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PFooClass'; Flgs: []),
    (Exp: 'ArgPPFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPFooClass'; Flgs: []),
    (Exp: 'VArgPPFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPFooClass'; Flgs: []),
    (Exp: 'ArgTNewFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewFooClass'; Flgs: []),
    (Exp: 'VArgTNewFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewFooClass'; Flgs: []),
    (Exp: 'ArgPNewFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewFooClass'; Flgs: []),
    (Exp: 'VArgPNewFooClass';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewFooClass'; Flgs: []),
    *)

    { Compare Objects }
    // TODO: not working in Dwarf3
    (Exp: 'ArgTFoo=ArgTFoo';      Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'not(ArgTFoo=ArgTFoo)'; Fmt: wdfDefault;  Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'VArgTFoo=VArgTFoo';    Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgTFoo=VArgTFoo';     Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgTFoo=ArgPFoo';      Fmt: wdfDefault;  Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgTFoo=ArgPFoo^';     Fmt: wdfDefault;  Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'ArgPFoo=ArgPPFoo^';    Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),

    (Exp: '@ArgTFoo=PVarTFoo';    Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: '@VArgTFoo=PVarTFoo';   Fmt: wdfDefault;  Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),

    //(Exp: 'ArgTFoo<>ArgTFoo';     Fmt: wdfDefault;  Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    //(Exp: 'ArgTFoo<>ArgPFoo^';    Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),

    (Exp: 'ArgTFoo=0';          Fmt: wdfDefault;  Mtch: 'False';          Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    (Exp: 'not(ArgTFoo=0)';     Fmt: wdfDefault;  Mtch: 'True';           Kind: skSimple;      TpNm: 'bool'; Flgs: [fnoDwrf3]),
    //(Exp: 'ArgTFoo<>0';         Fmt: wdfDefault;  Mtch: 'True';           Kind: skSimple;      TpNm: 'bool'; Flgs: []),

    //(Exp: 'ArgTFoo=nil';          Fmt: wdfDefault;  Mtch: 'False';        Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    //(Exp: 'not(ArgTFoo=nil)';     Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    //(Exp: 'ArgTFoo<>nil';         Fmt: wdfDefault;  Mtch: 'True';         Kind: skSimple;      TpNm: 'bool'; Flgs: []),
    {%endregion    * Classes * }

    {%region    * Strings * }
      { strings }
    (Exp: 'ArgTMyAnsiString';      Fmt: wdfDefault;  Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyAnsiString';     Fmt: wdfDefault;  Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyAnsiString';      Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'VArgPMyAnsiString';     Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'ArgPMyAnsiString^';      Fmt: wdfDefault;  Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyAnsiString^';     Fmt: wdfDefault;  Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch, fnoDwrf]),
    (Exp: 'ArgPPMyAnsiString';     Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PPMyAnsiString'; Flgs: []),
    (Exp: 'VArgPPMyAnsiString';    Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PPMyAnsiString'; Flgs: []),
    (Exp: 'ArgPPMyAnsiString^';     Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: []),
    (Exp: 'VArgPPMyAnsiString^';    Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'PMyAnsiString'; Flgs: [fnoDwrf]),
    (Exp: 'ArgPPMyAnsiString^^';    Fmt: wdfDefault;  Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPPMyAnsiString^^';   Fmt: wdfDefault;  Mtch: '''ansi''$';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch, fnoDwrf]),

    (*
    (Exp: 'ArgTNewAnsiString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewAnsiString'; Flgs: []),
    (Exp: 'VArgTNewAnsiString';     Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewAnsiString'; Flgs: []),
    (Exp: 'ArgPNewAnsiString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewAnsiString'; Flgs: []),
    (Exp: 'VArgPNewAnsiString';     Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewAnsiString'; Flgs: []),
    *)

    (Exp: 'ArgTMyShortString';      Fmt: wdfDefault;  Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyShortString';     Fmt: wdfDefault;  Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyShortString';      Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;     TpNm: 'P(My)?ShortString'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyShortString';     Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;     TpNm: 'P(My)?ShortString'; Flgs: [fTpMtch]),
    (Exp: 'ArgPMyShortString^';      Fmt: wdfDefault;  Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgPMyShortString^';     Fmt: wdfDefault;  Mtch: '''short''$';        Kind: skSimple;      TpNm: '^(TMy)?ShortString$'; Flgs: [fTpMtch, fnoDwrf]),

    (*
    (Exp: 'ArgPPMyShortString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPMyShortString'; Flgs: []),
    (Exp: 'VArgPPMyShortString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPMyShortString'; Flgs: []),
    (Exp: 'ArgTNewhortString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewhortString'; Flgs: []),
    (Exp: 'VArgTNewhortString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewhortString'; Flgs: []),
    (Exp: 'ArgPNewhortString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewhortString'; Flgs: []),
    (Exp: 'VArgPNewhortString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewhortString'; Flgs: []),
*)

    // gdb 6.7.5 does not show the text
    (Exp: 'ArgTMyWideString';      Fmt: wdfDefault;  Mtch: '(''wide''$)|(widestring\(\$.*\))';      Kind: skPointer;      TpNm: '^(TMy)?WideString$'; Flgs: [fTpMtch]),
    (Exp: 'VArgTMyWideString';     Fmt: wdfDefault;  Mtch: '(''wide''$)|(widestring\(\$.*\))';      Kind: skPointer;      TpNm: '^(TMy)?WideString$'; Flgs: [fTpMtch]),
    (*
    (Exp: 'ArgPMyWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PMyWideString'; Flgs: []),
    (Exp: 'VArgPMyWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PMyWideString'; Flgs: []),
    (Exp: 'ArgPPMyWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPMyWideString'; Flgs: []),
    (Exp: 'VArgPPMyWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPMyWideString'; Flgs: []),

    (Exp: 'ArgTNewWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewWideString'; Flgs: []),
    (Exp: 'VArgTNewWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TNewWideString'; Flgs: []),
    (Exp: 'ArgPNewWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewWideString'; Flgs: []),
    (Exp: 'VArgPNewWideString';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PNewWideString'; Flgs: []),

    (Exp: 'ArgTMyString10';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TMyString10'; Flgs: []),
    (Exp: 'VArgTMyString10';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'TMyString10'; Flgs: []),
    (Exp: 'ArgPMyString10';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PMyString10'; Flgs: []),
    (Exp: 'VArgPMyString10';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PMyString10'; Flgs: []),
    (Exp: 'ArgPPMyString10';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPMyString10'; Flgs: []),
    (Exp: 'VArgPPMyString10';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPMyString10'; Flgs: []),
*)


    (Exp: 'ArgTMyAnsiString';      Fmt: wdfMemDump;  Mtch: ': 61 6E 73 69 00';         Kind: skPointer;      TpNm: '^(TMy)?AnsiString$'; Flgs: [fTpMtch]),

    {%endregion    * Strings * }

    {%region    * Simple * }

    (Exp: 'ArgByte';      Fmt: wdfDefault;  Mtch: '^25$';      Kind: skSimple;      TpNm: 'Byte'; Flgs: []),
    (Exp: 'VArgByte';     Fmt: wdfDefault;  Mtch: '^25$';      Kind: skSimple;      TpNm: 'Byte'; Flgs: []),
    (Exp: 'ArgWord';      Fmt: wdfDefault;  Mtch: '^26$';      Kind: skSimple;      TpNm: 'Word'; Flgs: []),
    (Exp: 'VArgWord';     Fmt: wdfDefault;  Mtch: '^26$';      Kind: skSimple;      TpNm: 'Word'; Flgs: []),
    (Exp: 'ArgLongWord';  Fmt: wdfDefault;  Mtch: '^27$';      Kind: skSimple;      TpNm: 'LongWord'; Flgs: []),
    (Exp: 'VArgLongWord'; Fmt: wdfDefault;  Mtch: '^27$';      Kind: skSimple;      TpNm: 'LongWord'; Flgs: []),
    (Exp: 'ArgQWord';     Fmt: wdfDefault;  Mtch: '^28$';      Kind: skSimple;      TpNm: 'QWord'; Flgs: []),
    (Exp: 'VArgQWord';    Fmt: wdfDefault;  Mtch: '^28$';      Kind: skSimple;      TpNm: 'QWord'; Flgs: []),

    (Exp: 'ArgShortInt';  Fmt: wdfDefault;  Mtch: '^35$';      Kind: skSimple;      TpNm: 'ShortInt'; Flgs: []),
    (Exp: 'VArgShortInt'; Fmt: wdfDefault;  Mtch: '^35$';      Kind: skSimple;      TpNm: 'ShortInt'; Flgs: []),
    (Exp: 'ArgSmallInt';  Fmt: wdfDefault;  Mtch: '^36$';      Kind: skSimple;      TpNm: 'SmallInt'; Flgs: []),
    (Exp: 'VArgSmallInt'; Fmt: wdfDefault;  Mtch: '^36$';      Kind: skSimple;      TpNm: 'SmallInt'; Flgs: []),
    (Exp: 'ArgInt';       Fmt: wdfDefault;  Mtch: '^37$';      Kind: skSimple;      TpNm: 'Integer|LongInt'; Flgs: [fTpMtch]),
    (Exp: 'VArgInt';      Fmt: wdfDefault;  Mtch: '^37$';      Kind: skSimple;      TpNm: 'Integer|LongInt'; Flgs: [fTpMtch]),
    (Exp: 'ArgInt64';     Fmt: wdfDefault;  Mtch: '^38$';      Kind: skSimple;      TpNm: 'Int64'; Flgs: []),
    (Exp: 'VArgInt64';    Fmt: wdfDefault;  Mtch: '^38$';      Kind: skSimple;      TpNm: 'Int64'; Flgs: []),

    (Exp: 'ArgPointer';      Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'Pointer'; Flgs: []),
    (Exp: 'VArgPointer';     Fmt: wdfDefault;  Mtch: Match_Pointer;      Kind: skPointer;      TpNm: 'Pointer'; Flgs: []),
    (*
    (Exp: 'ArgPPointer';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPointer'; Flgs: []),
    (Exp: 'VArgPPointer';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PPointer'; Flgs: []),
*)

    (Exp: 'ArgDouble';       Fmt: wdfDefault;  Mtch: '1\.123';      Kind: skSimple;      TpNm: 'Double'; Flgs: []),
    (Exp: 'VArgDouble';      Fmt: wdfDefault;  Mtch: '1\.123';      Kind: skSimple;      TpNm: 'Double'; Flgs: []),
    (Exp: 'ArgExtended';     Fmt: wdfDefault;  Mtch: '2\.345';      Kind: skSimple;      TpNm: 'Extended'; Flgs: []),
    (Exp: 'VArgExtended';    Fmt: wdfDefault;  Mtch: '2\.345';      Kind: skSimple;      TpNm: 'Extended'; Flgs: []),

    (*
    (Exp: 'ArgPByte';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PByte'; Flgs: []),
    (Exp: 'VArgPByte';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PByte'; Flgs: []),
    (Exp: 'ArgPWord';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PWord'; Flgs: []),
    (Exp: 'VArgPWord';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PWord'; Flgs: []),
    (Exp: 'ArgPLongWord';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PLongWord'; Flgs: []),
    (Exp: 'VArgPLongWord';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PLongWord'; Flgs: []),
    (Exp: 'ArgPQWord';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PQWord'; Flgs: []),
    (Exp: 'VArgPQWord';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PQWord'; Flgs: []),

    (Exp: 'ArgPShortInt';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PShortInt'; Flgs: []),
    (Exp: 'VArgPShortInt';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PShortInt'; Flgs: []),
    (Exp: 'ArgPSmallInt';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PSmallInt'; Flgs: []),
    (Exp: 'VArgPSmallInt';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PSmallInt'; Flgs: []),
    (Exp: 'ArgPInt';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PInteger'; Flgs: []),
    (Exp: 'VArgPInt';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PInteger'; Flgs: []),
    (Exp: 'ArgPInt64';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PInt64'; Flgs: []),
    (Exp: 'VArgPInt64';      Fmt: wdfDefault;  Mtch: '';      Kind: sk;      TpNm: 'PInt64'; Flgs: []),
*)
    {%endregion    * Simple * }

    {%region    * Enum/Set * }

    (Exp: 'ArgEnum';       Fmt: wdfDefault;  Mtch: '^Two$';                      Kind: skEnum;       TpNm: 'TEnum'; Flgs: []),
    (Exp: 'ArgEnumSet';    Fmt: wdfDefault;  Mtch: '^\[Two(, ?|\.\.)Three\]$';   Kind: skSet;        TpNm: 'TEnumSet'; Flgs: [fnoDwrfNoSet]),
    (Exp: 'ArgSet';        Fmt: wdfDefault;  Mtch: '^\[Alpha(, ?|\.\.)Beta\]$';  Kind: skSet;        TpNm: 'TSet'; Flgs: [fnoDwrfNoSet]),

    (Exp: 'VarEnumA';      Fmt: wdfDefault;  Mtch: '^e3$';                       Kind: skEnum;       TpNm: ''; Flgs: []),
    // maybe typename = "set of TEnum"
    (Exp: 'VarEnumSetA';   Fmt: wdfDefault;  Mtch: '^\[Three\]$';                Kind: skSet;        TpNm: ''; Flgs: [fnoDwrfNoSet]),
    (Exp: 'VarSetA';       Fmt: wdfDefault;  Mtch: '^\[s2\]$';                   Kind: skSet;        TpNm: ''; Flgs: [fnoDwrfNoSet]),
    {%endregion    * Enum/Set * }

    {%region    * Array * }
    //TODO: DynArray, decide what to display
    // TODO {} fixup array => replace with []
    (Exp: 'VarDynIntArray';       Fmt: wdfDefault;      Mtch: Match_Pointer+'|\{\}|0,[\s\r\n]+2';
                                  Kind: skSimple;       TpNm: 'TDynIntArray';
                                  Flgs: []) ,
//TODO add () around list
    (Exp: 'VarStatIntArray';      Fmt: wdfDefault;      Mtch: '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18';
                                  Kind: skSimple;       TpNm: 'TStatIntArray';
                                  Flgs: []) ,
    (Exp: 'VarPDynIntArray';      Fmt: wdfDefault;      Mtch: Match_Pointer;
                                  Kind: skPointer;      TpNm: 'PDynIntArray';
                                  Flgs: []) ,
    (Exp: 'VarPStatIntArray';     Fmt: wdfDefault;      Mtch: Match_Pointer;
                                  Kind: skPointer;      TpNm: 'PStatIntArray';
                                  Flgs: []) ,
    (Exp: 'VarDynIntArrayA';      Fmt: wdfDefault;      Mtch: Match_Pointer+'|\{\}|0,[\s\r\n]+2';
                                  Kind: skSimple;       TpNm: '';
                                  Flgs: []) ,
    (Exp: 'VarStatIntArrayA';     Fmt: wdfDefault;      Mtch: '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18';
                                  Kind: skSimple;       TpNm: '';
                                  Flgs: []) ,

    (Exp: 'VarDynIntArray[1]';    Fmt: wdfDefault;      Mtch: '2';
                                  Kind: skSimple;       TpNm: 'Integer|LongInt';
                                  Flgs: [fTpMtch]) ,
    (Exp: 'VarStatIntArray[6]';   Fmt: wdfDefault;      Mtch: '12';
                                  Kind: skSimple;       TpNm: 'Integer|LongInt';
                                  Flgs: [fTpMtch]) ,
    (Exp: 'VarPDynIntArray^[1]';  Fmt: wdfDefault;      Mtch: '2';
                                  Kind: skSimple;       TpNm: 'Integer|LongInt';
                                  Flgs: [fTpMtch]) ,
    (Exp: 'VarPStatIntArray^[6]'; Fmt: wdfDefault;      Mtch: '12';
                                  Kind: skSimple;       TpNm: 'Integer|LongInt';
                                  Flgs: [fTpMtch]) ,
    (Exp: 'VarDynIntArrayA[1]';   Fmt: wdfDefault;      Mtch: '2';
                                  Kind: skSimple;       TpNm: 'Integer|LongInt';
                                  Flgs: [fTpMtch]) ,
    (Exp: 'VarStatIntArrayA[6]';  Fmt: wdfDefault;      Mtch: '12';
                                  Kind: skSimple;       TpNm: 'Integer|LongInt';
                                  Flgs: [fTpMtch]),
    {%endregion    * Array * }

    {%region    * Variant * }

    (Exp: 'ArgVariantInt';       Fmt: wdfDefault;  Mtch: '^5$';         Kind: skVariant;       TpNm: 'Variant'; Flgs: []),
    (Exp: 'ArgVariantString';    Fmt: wdfDefault;  Mtch: '^''v''$';     Kind: skVariant;       TpNm: 'Variant'; Flgs: []),

    (Exp: 'VArgVariantInt';       Fmt: wdfDefault;  Mtch: '^5$';         Kind: skVariant;       TpNm: 'Variant'; Flgs: []),
    (Exp: 'VArgVariantString';    Fmt: wdfDefault;  Mtch: '^''v''$';     Kind: skVariant;       TpNm: 'Variant'; Flgs: []),
    {%endregion    * Variant * }

    {%region    * procedure/function/method * }

    (Exp: 'ArgProcedure';       Fmt: wdfDefault;  Mtch: 'procedure';           Kind: skProcedure;       TpNm: 'TProcedure'; Flgs: []),
    (Exp: 'ArgFunction';        Fmt: wdfDefault;  Mtch: 'function';            Kind: skFunction;        TpNm: 'TFunction';  Flgs: []),
(*
// normal procedure on stabs / recodr on dwarf => maybe the data itself may reveal some ?
    (Exp: 'ArgObjProcedure';    Fmt: wdfDefault;  Mtch: 'procedure.*of object|record.*procedure.*self =';
                                                             Kind: skRecord;          TpNm: 'TObjProcedure'; Flgs: []),
    (Exp: 'ArgObjFunction';     Fmt: wdfDefault;  Mtch: 'function.*of object|record.*function.*self =';
                                                             Kind: skRecord;          TpNm: 'TObjFunction';  Flgs: []),

*)
// doesn't work, ptype returns empty in dwarf => maybe via whatis
//    (Exp: 'VArgProcedure';       Fmt: wdfDefault;  Mtch: 'procedure';           Kind: skProcedure;       TpNm: 'TProcedure'; Flgs: []),
//    (Exp: 'VArgFunction';        Fmt: wdfDefault;  Mtch: 'function';            Kind: skFunction;        TpNm: 'TFunction';  Flgs: []),
(*
    (Exp: 'VArgObjProcedure';    Fmt: wdfDefault;  Mtch: 'procedure.*of object|record.*procedure.*self =';
                                                              Kind: skRecord;          TpNm: 'TObjProcedure'; Flgs: []),
    (Exp: 'VArgObjFunction';     Fmt: wdfDefault;  Mtch: 'function.*of object|record.*function.*self =';
                                                              Kind: skRecord;          TpNm: 'TObjFunction';  Flgs: []),
*)

    (Exp: 'VarProcedureA';       Fmt: wdfDefault;  Mtch: 'procedure';           Kind: skProcedure;       TpNm: 'Procedure'; Flgs: []),
    (Exp: 'VarFunctionA';        Fmt: wdfDefault;  Mtch: 'function';            Kind: skFunction;        TpNm: 'Function';  Flgs: [])//,
(*
    (Exp: 'VarObjProcedureA';    Fmt: wdfDefault;  Mtch: 'procedure.*of object|record.*procedure.*self =';
                                                              Kind: skRecord;          TpNm: 'Procedure'; Flgs: []),
    (Exp: 'VarObjFunctionA';     Fmt: wdfDefault;  Mtch: 'function.*of object|record.*function.*self =';
                                                              Kind: skRecord;          TpNm: 'Function';  Flgs: [])//,
*)
    {%endregion    * procedure/function/method * }

  );




{ TTestWatches }

procedure TTestWatches.DoDbgOutput(Sender: TObject; const AText: String);
begin
  inherited DoDbgOutput(Sender, AText);
  if FDbgOutPutEnable then
    FDbgOutPut := FDbgOutPut + AText;
  if DbgLog and (DbgMemo <> nil) then
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

  procedure TestWatch(Name: String; AWatch: TCurrentWatch; Data: TWatchExpectation; WatchValue: String = '');
  const KindName: array [TDBGSymbolKind] of string =
     ('skClass', 'skRecord', 'skEnum', 'skSet', 'skProcedure', 'skFunction', 'skSimple', 'skPointer', 'skVariant');
  var
    rx: TRegExpr;
    s: String;
    flag: Boolean;
    WV: TWatchValue;
  begin
    rx := nil;

    Name := Name + ' ' + Data.Exp + ' (' + TWatchDisplayFormatNames[Data.Fmt] + ')';
    flag := AWatch <> nil;
    if flag then begin;
      WV := AWatch.Values[1, 0];// trigger read
      flag := flag and TestTrue  (Name+ ' (HasValue)',   WV.Validity = ddsValid);
      //flag := flag and TestFalse (Name+ ' (One Value)',  AWatch.HasMultiValue);
      s := WV.Value;
    end
    else
      s := WatchValue;

    if flag then begin
      rx := TRegExpr.Create;
      rx.ModifierI := true;
      rx.Expression := Data.Mtch;
      if Data.Mtch <> ''
      then TestTrue(Name + ' Matches "'+Data.Mtch + '", but was "' + s + '"', rx.Exec(s));
    end;

    flag := (AWatch <> nil) and (Data.TpNm <> '');
    if flag then flag := TestTrue(Name + ' has typeinfo',  WV.TypeInfo <> nil);
    if flag then flag := TestEquals(Name + ' kind',  KindName[Data.Kind], KindName[WV.TypeInfo.Kind]);
    if flag then begin
      if fTpMtch  in Data.Flgs
      then begin
        FreeAndNil(rx);
        s := WV.TypeInfo.TypeName;
        rx := TRegExpr.Create;
        rx.ModifierI := true;
        rx.Expression := Data.TpNm;
        TestTrue(Name + ' TypeName matches '+Data.TpNm+' but was '+WV.TypeInfo.TypeName,  rx.Exec(s))
      end
      else TestEquals(Name + ' TypeName',  LowerCase(Data.TpNm), LowerCase(WV.TypeInfo.TypeName));
    end;
    FreeAndNil(rx);
  end;

var
  TestExeName: string;
  dbg: TGDBMIDebugger;
  i: Integer;
  WList: Array of TCurrentWatch;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch')] then exit;

  ClearTestErrors;
  try
    TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);
  except
    on e: Exception do Fail('Compile error: ' + e.Message);
  end;

  try
    dbg := StartGDB(AppDir, TestExeName);
    FWatches := Watches.CurrentWatches;

    if (RUN_TEST_ONLY >= 0) or (RUN_GDB_TEST_ONLY >= 0) then begin
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
    SetLength(WList, high(ExpectBrk1NoneNil)+1);
    if RUN_TEST_ONLY >= 0 then begin
      i := RUN_TEST_ONLY;
      WList[i] := TCurrentWatch.Create(FWatches);
      WList[i].Expression := ExpectBrk1NoneNil[i].Exp;
      WList[i].DisplayFormat := ExpectBrk1NoneNil[i].Fmt;
      WList[i].enabled := True;
    end
    else
      for i := low(ExpectBrk1NoneNil) to high(ExpectBrk1NoneNil) do begin
        if not SkipTest(ExpectBrk1NoneNil[i]) then begin
          WList[i] := TCurrentWatch.Create(FWatches);
          WList[i].Expression := ExpectBrk1NoneNil[i].Exp;
          WList[i].DisplayFormat := ExpectBrk1NoneNil[i].Fmt;
          WList[i].enabled := True;
        end;
      end;


    (* Start debugging *)
    if dbg.State = dsError then
      Fail(' Failed Init');

    dbg.ShowConsole := True;

    dbg.Run;

    (* Hit first breakpoint: Test *)
    FDbgOutPutEnable := True;
    if (RUN_TEST_ONLY < 0) or (RUN_GDB_TEST_ONLY >= 0) then begin
      if RUN_GDB_TEST_ONLY >= 0 then begin
        i := RUN_GDB_TEST_ONLY;
        FDbgOutPut := '';
        dbg.TestCmd(ExpectGdbBrk1NoneNil[i].Exp);
        TestWatch('Brk1 Direct Gdb '+IntToStr(i)+' ', nil, ExpectGdbBrk1NoneNil[i], FDbgOutPut);
      end
      else
        for i := low(ExpectGdbBrk1NoneNil) to high(ExpectGdbBrk1NoneNil) do begin
          if not SkipTest(ExpectGdbBrk1NoneNil[i]) then begin
            FDbgOutPut := '';
            dbg.TestCmd(ExpectGdbBrk1NoneNil[i].Exp);
            TestWatch('Brk1 Direct Gdb '+IntToStr(i)+' ', nil, ExpectGdbBrk1NoneNil[i], FDbgOutPut);
          end;
        end;
    end;
    FDbgOutPutEnable := False;

    if (RUN_GDB_TEST_ONLY < 0) or (RUN_TEST_ONLY >= 0) then begin
      if RUN_TEST_ONLY >= 0 then begin
        i := RUN_TEST_ONLY;
        TestWatch('Brk1 ', WList[i], ExpectBrk1NoneNil[i]);
      end
      else
        for i := low(ExpectBrk1NoneNil) to high(ExpectBrk1NoneNil) do begin
          if not SkipTest(ExpectBrk1NoneNil[i]) then
            TestWatch('Brk1 '+IntToStr(i)+' ', WList[i], ExpectBrk1NoneNil[i]);
        end;
    end;

    dbg.Run;

	//DebugInteract(dbg);

    dbg.Stop;
  finally
    CleanGdb;
    dbg.Free;

    if (DbgMemo <> nil) and (TestErrors <> '') then DbgMemo.Lines.Add(TestErrors);
    //debugln(FailText)
    AssertTestErrors;
  end;
end;



initialization

  RegisterDbgTest(TTestWatches);
end.

