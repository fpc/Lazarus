unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc, SynRegExpr, Forms, StdCtrls, Controls;

const
  BREAK_LINE_FOOFUNC      = 160;
  BREAK_LINE_FOOFUNC_NEST = 136;
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

var
  // direct commands to gdb, to check assumptions  // only Exp and Mtch
  ExpectGdbBrk1NoneNil: Array of TWatchExpectation;
  // Watches
  ExpectBrk1NoneNil: Array of TWatchExpectation;

const
  Match_Pointer = '\$[0-9A-F]+';
  M_Int = 'Integer|LongInt';

  {%region    * Classes * }
  // _vptr$TOBJECt on older gdb e.g. mac 6.3.50
  Match_ArgTFoo = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = 31';
  {%ebdregion    * Classes * }
  // Todo: Dwarf fails with dereferenced var pointer types

procedure InitializeExpectGdbBrk1NoneNil;
  procedure Add(AnExp:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags
  );
  begin
    SetLength(ExpectGdbBrk1NoneNil, Length(ExpectGdbBrk1NoneNil)+1);
    with ExpectGdbBrk1NoneNil[Length(ExpectGdbBrk1NoneNil)-1] do begin
      Exp  := AnExp;
      Fmt  := AFmt;
      Mtch := AMtch;
      Kind := AKind;
      TpNm := ATpNm;
      Flgs := AFlgs;
    end;
  end;
begin
  Add('ptype ArgTFoo',  wdfDefault, 'type = \^TFoo = class : PUBLIC TObject', skClass, '', []);
  Add('ptype ArgTFoo^', wdfDefault, 'type = TFoo = class : PUBLIC TObject',   skClass, '', []);

  Add('-data-evaluate-expression sizeof(ArgTFoo)',  wdfDefault, 'value="(4|8)"|(parse|syntax) error in expression', skClass, '',  []);
  Add('-data-evaluate-expression sizeof(ArgTFoo^)', wdfDefault, 'value="\d\d+"|(parse|syntax) error in expression', skClass, '',  []);
end;

procedure InitializeExpectBrk1NoneNil;
  procedure Add(AnExp:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
    AFlgs: TWatchExpectationFlags = []
  );
  begin
    SetLength(ExpectBrk1NoneNil, Length(ExpectBrk1NoneNil)+1);
    with ExpectBrk1NoneNil[Length(ExpectBrk1NoneNil)-1] do begin
      Exp  := AnExp;
      Fmt  := AFmt;
      Mtch := AMtch;
      Kind := AKind;
      TpNm := ATpNm;
      Flgs := AFlgs;
    end;
  end;

  function MatchPointer(TypeName: String=''): String;
  begin
    if TypeName = ''
    then Result := '\$[0-9A-F]+'
    else Result := TypeName+'\(\$[0-9A-F]+';
  end;
  function MatchRecord(TypeName: String; AContent: String = ''): String;
  begin
    Result := 'record '+TypeName+' .+'+AContent;
  end;
  function MatchRecord(TypeName: String; AValInt: integer; AValFoo: String = ''): String;
  begin
    Result := 'record '+TypeName+' .+ valint = '+IntToStr(AValInt);
    If AValFoo <> '' then Result := Result + ',.* valfoo = '+AValFoo;
  end;
begin
  {%region    * records * }
  // Foo(var XXX: PRecord); DWARF has problems with the implicit pointer for "var"

  // param to FooFunc
  Add('ArgTRec',         wdfDefault,  MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,    'TRec', []);
  Add('ArgPRec',         wdfDefault,  MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  Add('ArgPRec^',         wdfDefault, MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
  Add('ArgPPRec',        wdfDefault,  MatchPointer('^PPRec'),                 skPointer,   'PPRec', []);
  Add('ArgPPRec^',        wdfDefault, MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  Add('ArgPPRec^^',       wdfDefault, MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
  Add('ArgTNewRec',      wdfDefault,  MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,    'T(New)?Rec', [fTpMtch]);
  Add('ArgTRec.ValInt',  wdfDefault,  '-1',                                   skSimple,    M_Int, [fTpMtch]);
  Add('ArgPRec^.ValInt', wdfDefault,  '1',                                    skSimple,    M_Int, [fTpMtch]);
  Add('ArgPPRec^^.ValInt',wdfDefault, '1',                                    skSimple,    M_Int, [fTpMtch]);
  Add('ArgPRec^.ValFoo',  wdfDefault, '<TFoo> = \{',                          skClass,     'TFoo', []);

  // VAR param to FooFunc
  Add('VArgTRec',         wdfDefault,  MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,    'TRec', []);
  Add('VArgPRec',         wdfDefault,  MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  Add('VArgPRec^',         wdfDefault, MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', [fnoDwrf]);
  Add('VArgPPRec',        wdfDefault,  MatchPointer('^PPRec'),                 skPointer,   'PPRec', []);
  Add('VArgPPRec^',        wdfDefault, MatchPointer('^PRec'),                  skPointer,   'PRec', [fnoDwrf]);
  Add('VArgPPRec^^',       wdfDefault, MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', [fnoDwrf]);
  Add('VArgTNewRec',      wdfDefault,  MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,    'T(New)?Rec', [fTpMtch]);
  Add('VArgTRec.ValInt',  wdfDefault,  '-1',                                   skSimple,    M_Int, [fTpMtch]);
  Add('VArgPRec^.ValInt', wdfDefault,  '1',                                    skSimple,    M_Int, [fTpMtch]);
  Add('VArgPPRec^^.ValInt',wdfDefault, '1',                                    skSimple,    M_Int, [fTpMtch]);
  Add('VArgPRec^.ValFoo',  wdfDefault, '<TFoo> = \{',                          skClass,     'TFoo', []);

  // LOCAL var (global type)
  Add('VarTRec',         wdfDefault,  MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,    'TRec', []);
  Add('VarPRec',         wdfDefault,  MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  Add('VarPRec^',         wdfDefault, MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
  Add('VarPPRec',        wdfDefault,  MatchPointer('^PPRec'),                 skPointer,   'PPRec', []);
  Add('VarPPRec^',        wdfDefault, MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  Add('VarPPRec^^',       wdfDefault, MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
  Add('VarTNewRec',      wdfDefault,  MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,    'T(New)?Rec', [fTpMtch]);
  Add('VarTRec.ValInt',  wdfDefault,  '-1',                                   skSimple,    M_Int, [fTpMtch]);

  // LOCAL var (reference (^) to global type)
  Add('PVarTRec',      wdfDefault,  MatchPointer('^(\^T|P)Rec'),            skPointer,  '^(\^T|P)Rec$', [fTpMtch]); // TODO: stabs returns PRec
  Add('PVarTRec^',     wdfDefault,  MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,   'TRec', []);
  Add('PVarTNewRec',   wdfDefault,  MatchPointer('^\^TNewRec'),             skPointer,  '\^T(New)?Rec', [fTpMtch]);
  Add('PVarTNewRec^',  wdfDefault,  MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,   'T(New)?Rec',   [fTpMtch]);

  // LOCAL var (local type)
  Add('VarRecA',       wdfDefault,  MatchRecord('', 'val = 1'),             skRecord,    '', []);
  {%endregion    * records * }

  // @ArgTRec @VArgTRec  @ArgTRec^ @VArgTRec^

  {%region    * Classes * }

  Add('ArgTFoo',    wdfDefault,  Match_ArgTFoo,                 skClass,   'TFoo',  []);
  Add('@ArgTFoo',   wdfDefault,  '(P|\^T)Foo\('+Match_Pointer,  skPointer, '(P|\^T)Foo',  [fTpMtch]);
  // Only with brackets...
  Add('(@ArgTFoo)^', wdfDefault,  Match_ArgTFoo,                skClass,   'TFoo',  []);

  Add('ArgPFoo',    wdfDefault,  'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
  Add('ArgPFoo^',   wdfDefault,  Match_ArgTFoo1,                skClass,   'TFoo',  []);
  Add('@ArgPFoo',   wdfDefault,  '(P|\^)PFoo\('+Match_Pointer,  skPointer, '(P|\^)PFoo',  [fTpMtch]);

  Add('ArgPPFoo',   wdfDefault,  'PPFoo\('+Match_Pointer,       skPointer, 'PPFoo', []);
  Add('ArgPPFoo^',  wdfDefault,  'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
  Add('@ArgPPFoo',  wdfDefault,  '\^PPFoo\('+Match_Pointer,      skPointer, '^PPFoo', []);
  Add('ArgPPFoo^^', wdfDefault,  Match_ArgTFoo1,                skClass,   'TFoo',  []);


  Add('VArgTFoo',   wdfDefault,  Match_ArgTFoo,                 skClass,   'TFoo',  []);
  Add('@VArgTFoo',  wdfDefault,  '(P|\^T)Foo\('+Match_Pointer,  skPointer, '(P|\^T)Foo',  [fTpMtch]);
  Add('(@VArgTFoo)^', wdfDefault,  Match_ArgTFoo,                 skClass,   'TFoo',  []);

  Add('VArgPFoo',   wdfDefault,  'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
  Add('VArgPFoo^' , wdfDefault,  Match_ArgTFoo1,                skClass,   'TFoo',  [fnoDwrf]);
  Add('@VArgPFoo',  wdfDefault,  '(P|\^)PFoo\('+Match_Pointer,  skPointer, '(P|\^)PFoo',  [fTpMtch]);

  Add('VArgPPFoo',  wdfDefault,  'PPFoo\('+Match_Pointer,       skPointer, 'PPFoo', []);
  Add('VArgPPFoo^', wdfDefault,  'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  [fnoDwrf]);
  Add('@VArgPPFoo', wdfDefault,  '\^PPFoo\('+Match_Pointer,      skPointer, '^PPFoo', []);
  Add('VArgPPFoo^^', wdfDefault,  Match_ArgTFoo1,               skClass,   'TFoo',  [fnoDwrf]);


  Add('ArgTFoo.ValueInt',     wdfDefault,  '^-11$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  Add('ArgPFoo^.ValueInt',    wdfDefault,  '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  // GDB automatically derefs the pointer
  //Add('ArgPFoo.ValueInt',     wdfDefault,  'error',            skSimple,   '',  []);
  Add('ArgPPFoo^^.ValueInt',  wdfDefault,  '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  //Add('ArgPPFoo.ValueInt',     wdfDefault,  'error',            skSimple,   '',  []);

  Add('VArgTFoo.ValueInt',    wdfDefault,  '^-11$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  Add('VArgPFoo^.ValueInt',   wdfDefault,  '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  //Add('VArgPFoo.ValueInt',    wdfDefault,  'error',            skSimple,   '',  []);
  Add('VArgPPFoo^^.ValueInt', wdfDefault,  '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  //Add('VArgPPFoo.ValueInt',    wdfDefault,  'error',            skSimple,   '',  []);


  Add('ArgTFoo',    wdfPointer,  Match_Pointer,                           skClass,   'TFoo',  []);
  Add('ArgTFoo',    wdfMemDump,  ':.*?6D 65 6D 20 6F 66 20 54 46 6F 6F',  skClass,   'TFoo',  []);

  (*

  Add('ArgTSamePFoo',      wdfDefault,  '',      sk,      'TSamePFoo', []);
  Add('VArgTSamePFoo',      wdfDefault,  '',      sk,      'TSamePFoo', []);
  Add('ArgTNewPFoo',      wdfDefault,  '',      sk,      'TNewPFoo', []);
  Add('VArgTNewPFoo',      wdfDefault,  '',      sk,      'TNewPFoo', []);

  Add('ArgTSameFoo',      wdfDefault,  '',      sk,      'TSameFoo', []);
  Add('VArgTSameFoo',      wdfDefault,  '',      sk,      'TSameFoo', []);
  Add('ArgTNewFoo',      wdfDefault,  '',      sk,      'TNewFoo', []);
  Add('VArgTNewFoo',      wdfDefault,  '',      sk,      'TNewFoo', []);
  Add('ArgPNewFoo',      wdfDefault,  '',      sk,      'PNewFoo', []);
  Add('VArgPNewFoo',      wdfDefault,  '',      sk,      'PNewFoo', []);

    { ClassesTyps }
  Add('ArgTFooClass',      wdfDefault,  '',      sk,      'TFooClass', []);
  Add('VArgTFooClass',      wdfDefault,  '',      sk,      'TFooClass', []);
  Add('ArgPFooClass',      wdfDefault,  '',      sk,      'PFooClass', []);
  Add('VArgPFooClass',      wdfDefault,  '',      sk,      'PFooClass', []);
  Add('ArgPPFooClass',      wdfDefault,  '',      sk,      'PPFooClass', []);
  Add('VArgPPFooClass',      wdfDefault,  '',      sk,      'PPFooClass', []);
  Add('ArgTNewFooClass',      wdfDefault,  '',      sk,      'TNewFooClass', []);
  Add('VArgTNewFooClass',      wdfDefault,  '',      sk,      'TNewFooClass', []);
  Add('ArgPNewFooClass',      wdfDefault,  '',      sk,      'PNewFooClass', []);
  Add('VArgPNewFooClass',      wdfDefault,  '',      sk,      'PNewFooClass', []);
  *)

  { Compare Objects }
  // TODO: not working in Dwarf3
  Add('ArgTFoo=ArgTFoo',      wdfDefault,  'True',         skSimple,      'bool', [fnoDwrf3]);
  Add('not(ArgTFoo=ArgTFoo)', wdfDefault,  'False',        skSimple,      'bool', [fnoDwrf3]);
  Add('VArgTFoo=VArgTFoo',    wdfDefault,  'True',         skSimple,      'bool', [fnoDwrf3]);
  Add('ArgTFoo=VArgTFoo',     wdfDefault,  'True',         skSimple,      'bool', [fnoDwrf3]);
  Add('ArgTFoo=ArgPFoo',      wdfDefault,  'False',        skSimple,      'bool', [fnoDwrf3]);
  Add('ArgTFoo=ArgPFoo^',     wdfDefault,  'False',        skSimple,      'bool', [fnoDwrf3]);
  Add('ArgPFoo=ArgPPFoo^',    wdfDefault,  'True',         skSimple,      'bool', [fnoDwrf3]);

  Add('@ArgTFoo=PVarTFoo',    wdfDefault,  'True',         skSimple,      'bool', [fnoDwrf3]);
  Add('@VArgTFoo=PVarTFoo',   wdfDefault,  'False',        skSimple,      'bool', [fnoDwrf3]);

  //Add('ArgTFoo<>ArgTFoo',     wdfDefault,  'False',        skSimple,      'bool', [fnoDwrf3]);
  //Add('ArgTFoo<>ArgPFoo^',    wdfDefault,  'True',         skSimple,      'bool', [fnoDwrf3]);

  Add('ArgTFoo=0',          wdfDefault,  'False',          skSimple,      'bool', [fnoDwrf3]);
  Add('not(ArgTFoo=0)',     wdfDefault,  'True',           skSimple,      'bool', [fnoDwrf3]);
  //Add('ArgTFoo<>0',         wdfDefault,  'True',           skSimple,      'bool', []);

  //Add('ArgTFoo=nil',          wdfDefault,  'False',        skSimple,      'bool', []);
  //Add('not(ArgTFoo=nil)',     wdfDefault,  'True',         skSimple,      'bool', []);
  //Add('ArgTFoo<>nil',         wdfDefault,  'True',         skSimple,      'bool', []);
  {%endregion    * Classes * }

  {%region    * Strings * }
    { strings }
  Add('ArgTMyAnsiString',      wdfDefault,  '''ansi''$',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch]);
  Add('VArgTMyAnsiString',     wdfDefault,  '''ansi''$',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch]);
  Add('ArgPMyAnsiString',      wdfDefault,  Match_Pointer,      skPointer,      'PMyAnsiString', []);
  Add('VArgPMyAnsiString',     wdfDefault,  Match_Pointer,      skPointer,      'PMyAnsiString', []);
  Add('ArgPMyAnsiString^',      wdfDefault,  '''ansi''$',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch]);
  Add('VArgPMyAnsiString^',     wdfDefault,  '''ansi''$',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch, fnoDwrf]);
  Add('ArgPPMyAnsiString',     wdfDefault,  Match_Pointer,      skPointer,      'PPMyAnsiString', []);
  Add('VArgPPMyAnsiString',    wdfDefault,  Match_Pointer,      skPointer,      'PPMyAnsiString', []);
  Add('ArgPPMyAnsiString^',     wdfDefault,  Match_Pointer,      skPointer,      'PMyAnsiString', []);
  Add('VArgPPMyAnsiString^',    wdfDefault,  Match_Pointer,      skPointer,      'PMyAnsiString', [fnoDwrf]);
  Add('ArgPPMyAnsiString^^',    wdfDefault,  '''ansi''$',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch]);
  Add('VArgPPMyAnsiString^^',   wdfDefault,  '''ansi''$',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch, fnoDwrf]);

  (*
  Add('ArgTNewAnsiString',      wdfDefault,  '',      sk,      'TNewAnsiString', []);
  Add('VArgTNewAnsiString',     wdfDefault,  '',      sk,      'TNewAnsiString', []);
  Add('ArgPNewAnsiString',      wdfDefault,  '',      sk,      'PNewAnsiString', []);
  Add('VArgPNewAnsiString',     wdfDefault,  '',      sk,      'PNewAnsiString', []);
  *)

  Add('ArgTMyShortString',      wdfDefault,  '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
  Add('VArgTMyShortString',     wdfDefault,  '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
  Add('ArgPMyShortString',      wdfDefault,  Match_Pointer,      skPointer,     'P(My)?ShortString', [fTpMtch]);
  Add('VArgPMyShortString',     wdfDefault,  Match_Pointer,      skPointer,     'P(My)?ShortString', [fTpMtch]);
  Add('ArgPMyShortString^',      wdfDefault,  '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
  Add('VArgPMyShortString^',     wdfDefault,  '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch, fnoDwrf]);

  (*
  Add('ArgPPMyShortString',      wdfDefault,  '',      sk,      'PPMyShortString', []);
  Add('VArgPPMyShortString',      wdfDefault,  '',      sk,      'PPMyShortString', []);
  Add('ArgTNewhortString',      wdfDefault,  '',      sk,      'TNewhortString', []);
  Add('VArgTNewhortString',      wdfDefault,  '',      sk,      'TNewhortString', []);
  Add('ArgPNewhortString',      wdfDefault,  '',      sk,      'PNewhortString', []);
  Add('VArgPNewhortString',      wdfDefault,  '',      sk,      'PNewhortString', []);
  *)

  // gdb 6.7.5 does not show the text
  Add('ArgTMyWideString',      wdfDefault,  '(''wide''$)|(widestring\(\$.*\))',      skPointer,      '^(TMy)?WideString$', [fTpMtch]);
  Add('VArgTMyWideString',     wdfDefault,  '(''wide''$)|(widestring\(\$.*\))',      skPointer,      '^(TMy)?WideString$', [fTpMtch]);
  (*
  Add('ArgPMyWideString',      wdfDefault,  '',      sk,      'PMyWideString', []);
  Add('VArgPMyWideString',      wdfDefault,  '',      sk,      'PMyWideString', []);
  Add('ArgPPMyWideString',      wdfDefault,  '',      sk,      'PPMyWideString', []);
  Add('VArgPPMyWideString',      wdfDefault,  '',      sk,      'PPMyWideString', []);

  Add('ArgTNewWideString',      wdfDefault,  '',      sk,      'TNewWideString', []);
  Add('VArgTNewWideString',      wdfDefault,  '',      sk,      'TNewWideString', []);
  Add('ArgPNewWideString',      wdfDefault,  '',      sk,      'PNewWideString', []);
  Add('VArgPNewWideString',      wdfDefault,  '',      sk,      'PNewWideString', []);

  Add('ArgTMyString10',      wdfDefault,  '',      sk,      'TMyString10', []);
  Add('VArgTMyString10',      wdfDefault,  '',      sk,      'TMyString10', []);
  Add('ArgPMyString10',      wdfDefault,  '',      sk,      'PMyString10', []);
  Add('VArgPMyString10',      wdfDefault,  '',      sk,      'PMyString10', []);
  Add('ArgPPMyString10',      wdfDefault,  '',      sk,      'PPMyString10', []);
  Add('VArgPPMyString10',      wdfDefault,  '',      sk,      'PPMyString10', []);
  *)


  Add('ArgTMyAnsiString',      wdfMemDump,  ': 61 6E 73 69 00',         skPointer,      '^(TMy)?AnsiString$', [fTpMtch]);

  {%endregion    * Strings * }

  {%region    * Simple * }

  Add('ArgByte',      wdfDefault,  '^25$',      skSimple,      'Byte', []);
  Add('VArgByte',     wdfDefault,  '^25$',      skSimple,      'Byte', []);
  Add('ArgWord',      wdfDefault,  '^26$',      skSimple,      'Word', []);
  Add('VArgWord',     wdfDefault,  '^26$',      skSimple,      'Word', []);
  Add('ArgLongWord',  wdfDefault,  '^27$',      skSimple,      'LongWord', []);
  Add('VArgLongWord', wdfDefault,  '^27$',      skSimple,      'LongWord', []);
  Add('ArgQWord',     wdfDefault,  '^28$',      skSimple,      'QWord', []);
  Add('VArgQWord',    wdfDefault,  '^28$',      skSimple,      'QWord', []);

  Add('ArgShortInt',  wdfDefault,  '^35$',      skSimple,      'ShortInt', []);
  Add('VArgShortInt', wdfDefault,  '^35$',      skSimple,      'ShortInt', []);
  Add('ArgSmallInt',  wdfDefault,  '^36$',      skSimple,      'SmallInt', []);
  Add('VArgSmallInt', wdfDefault,  '^36$',      skSimple,      'SmallInt', []);
  Add('ArgInt',       wdfDefault,  '^37$',      skSimple,      'Integer|LongInt', [fTpMtch]);
  Add('VArgInt',      wdfDefault,  '^37$',      skSimple,      'Integer|LongInt', [fTpMtch]);
  Add('ArgInt64',     wdfDefault,  '^38$',      skSimple,      'Int64', []);
  Add('VArgInt64',    wdfDefault,  '^38$',      skSimple,      'Int64', []);

  Add('ArgPointer',      wdfDefault,  Match_Pointer,      skPointer,      'Pointer', []);
  Add('VArgPointer',     wdfDefault,  Match_Pointer,      skPointer,      'Pointer', []);
  (*
  Add('ArgPPointer',      wdfDefault,  '',      sk,      'PPointer', []);
  Add('VArgPPointer',      wdfDefault,  '',      sk,      'PPointer', []);
  *)

  Add('ArgDouble',       wdfDefault,  '1\.123',      skSimple,      'Double', []);
  Add('VArgDouble',      wdfDefault,  '1\.123',      skSimple,      'Double', []);
  Add('ArgExtended',     wdfDefault,  '2\.345',      skSimple,      'Extended', []);
  Add('VArgExtended',    wdfDefault,  '2\.345',      skSimple,      'Extended', []);

  (*
  Add('ArgPByte',      wdfDefault,  '',      sk,      'PByte', []);
  Add('VArgPByte',      wdfDefault,  '',      sk,      'PByte', []);
  Add('ArgPWord',      wdfDefault,  '',      sk,      'PWord', []);
  Add('VArgPWord',      wdfDefault,  '',      sk,      'PWord', []);
  Add('ArgPLongWord',      wdfDefault,  '',      sk,      'PLongWord', []);
  Add('VArgPLongWord',      wdfDefault,  '',      sk,      'PLongWord', []);
  Add('ArgPQWord',      wdfDefault,  '',      sk,      'PQWord', []);
  Add('VArgPQWord',      wdfDefault,  '',      sk,      'PQWord', []);

  Add('ArgPShortInt',      wdfDefault,  '',      sk,      'PShortInt', []);
  Add('VArgPShortInt',      wdfDefault,  '',      sk,      'PShortInt', []);
  Add('ArgPSmallInt',      wdfDefault,  '',      sk,      'PSmallInt', []);
  Add('VArgPSmallInt',      wdfDefault,  '',      sk,      'PSmallInt', []);
  Add('ArgPInt',      wdfDefault,  '',      sk,      'PInteger', []);
  Add('VArgPInt',      wdfDefault,  '',      sk,      'PInteger', []);
  Add('ArgPInt64',      wdfDefault,  '',      sk,      'PInt64', []);
  Add('VArgPInt64',      wdfDefault,  '',      sk,      'PInt64', []);
  *)
  {%endregion    * Simple * }

  {%region    * Enum/Set * }

  Add('ArgEnum',       wdfDefault,  '^Two$',                      skEnum,       'TEnum', []);
  Add('ArgEnumSet',    wdfDefault,  '^\[Two(, ?|\.\.)Three\]$',   skSet,        'TEnumSet', [fnoDwrfNoSet]);
  Add('ArgSet',        wdfDefault,  '^\[Alpha(, ?|\.\.)Beta\]$',  skSet,        'TSet', [fnoDwrfNoSet]);

  Add('VarEnumA',      wdfDefault,  '^e3$',                       skEnum,       '', []);
  // maybe typename = "set of TEnum"
  Add('VarEnumSetA',   wdfDefault,  '^\[Three\]$',                skSet,        '', [fnoDwrfNoSet]);
  Add('VarSetA',       wdfDefault,  '^\[s2\]$',                   skSet,        '', [fnoDwrfNoSet]);
  {%endregion    * Enum/Set * }

  {%region    * Array * }
  //TODO: DynArray, decide what to display
  // TODO {} fixup array => replace with []
  Add('VarDynIntArray',       wdfDefault,      Match_Pointer+'|\{\}|0,[\s\r\n]+2',
                                skSimple,       'TDynIntArray',
                                []);
  //TODO add () around list
  Add('VarStatIntArray',      wdfDefault,      '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18',
                                skSimple,       'TStatIntArray',
                                []);
  Add('VarPDynIntArray',      wdfDefault,      Match_Pointer,
                                skPointer,      'PDynIntArray',
                                []);
  Add('VarPStatIntArray',     wdfDefault,      Match_Pointer,
                                skPointer,      'PStatIntArray',
                                []);
  Add('VarDynIntArrayA',      wdfDefault,      Match_Pointer+'|\{\}|0,[\s\r\n]+2',
                                skSimple,       '',
                                []);
  Add('VarStatIntArrayA',     wdfDefault,      '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18',
                                skSimple,       '',
                                []);

  Add('VarDynIntArray[1]',    wdfDefault,      '2',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  Add('VarStatIntArray[6]',   wdfDefault,      '12',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  Add('VarPDynIntArray^[1]',  wdfDefault,      '2',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  Add('VarPStatIntArray^[6]', wdfDefault,      '12',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  Add('VarDynIntArrayA[1]',   wdfDefault,      '2',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  Add('VarStatIntArrayA[6]',  wdfDefault,      '12',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  {%endregion    * Array * }

  {%region    * Variant * }

  Add('ArgVariantInt',       wdfDefault,  '^5$',         skVariant,       'Variant', []);
  Add('ArgVariantString',    wdfDefault,  '^''v''$',     skVariant,       'Variant', []);

  Add('VArgVariantInt',       wdfDefault,  '^5$',         skVariant,       'Variant', []);
  Add('VArgVariantString',    wdfDefault,  '^''v''$',     skVariant,       'Variant', []);
  {%endregion    * Variant * }

  {%region    * procedure/function/method * }

  Add('ArgProcedure',       wdfDefault,  'procedure',           skProcedure,       'TProcedure', []);
  Add('ArgFunction',        wdfDefault,  'function',            skFunction,        'TFunction',  []);
  (*
  // normal procedure on stabs / recodr on dwarf => maybe the data itself may reveal some ?
  Add('ArgObjProcedure',    wdfDefault,  'procedure.*of object|record.*procedure.*self =',
                                                           skRecord,          'TObjProcedure', []);
  Add('ArgObjFunction',     wdfDefault,  'function.*of object|record.*function.*self =',
                                                           skRecord,          'TObjFunction',  []);

  *)
  // doesn't work, ptype returns empty in dwarf => maybe via whatis
  //    Add('VArgProcedure',       wdfDefault,  'procedure',           skProcedure,       'TProcedure', []);
  //    Add('VArgFunction',        wdfDefault,  'function',            skFunction,        'TFunction',  []);
  (*
  Add('VArgObjProcedure',    wdfDefault,  'procedure.*of object|record.*procedure.*self =',
                                                            skRecord,          'TObjProcedure', []);
  Add('VArgObjFunction',     wdfDefault,  'function.*of object|record.*function.*self =',
                                                            skRecord,          'TObjFunction',  []);
  *)

  Add('VarProcedureA',       wdfDefault,  'procedure',           skProcedure,       'Procedure', []);
  Add('VarFunctionA',        wdfDefault,  'function',            skFunction,        'Function',  []);
  (*
  Add('VarObjProcedureA',    wdfDefault,  'procedure.*of object|record.*procedure.*self =',
                                                            skRecord,          'Procedure', []);
  Add('VarObjFunctionA',     wdfDefault,  'function.*of object|record.*function.*self =',
                                                            skRecord,          'Function',  []);
  *)
  {%endregion    * procedure/function/method * }

end;

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
      s := WV.Value;
      flag := flag and TestTrue  (Name+ ' (HasValue)',   WV.Validity = ddsValid);
      //flag := flag and TestFalse (Name+ ' (One Value)',  AWatch.HasMultiValue);
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
    dbg.Free;
    CleanGdb;

    if (DbgMemo <> nil) and (TestErrors <> '') then DbgMemo.Lines.Add(TestErrors);
    //debugln(FailText)
    AssertTestErrors;
  end;
end;



initialization

  InitializeExpectBrk1NoneNil;
  InitializeExpectGdbBrk1NoneNil;
  RegisterDbgTest(TTestWatches);
end.

