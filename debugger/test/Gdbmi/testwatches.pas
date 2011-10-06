unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc, SynRegExpr, Forms, StdCtrls, Controls;

const
  BREAK_LINE_FOOFUNC      = 230;
  BREAK_LINE_FOOFUNC_NEST = 206;
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

  TWatchExpectationFlag =
    (fnoDwrf,      // no dwarf at all
     fnoDwrfNoSet, // no dwarf2 (-gw) without set
     //fnoDwrfSet,   // no dwarf2 with set // no dwarf3
     fTstSkip,
     fTpMtch
    );
  TWatchExpectationFlags = set of TWatchExpectationFlag;

  PWatchExpectation= ^TWatchExpectation;
  TWatchExpectationResult = record
    ExpMatch: string;
    ExpKind: TDBGSymbolKind;
    ExpTypeName: string;
    Flgs: TWatchExpectationFlags;
  end;

  TWatchExpectation = record
    TestName: String;
    Expression:  string;
    DspFormat: TWatchDisplayFormat;
    StackFrame: Integer;
    Result: Array [TSymbolType] of TWatchExpectationResult;
  end;
  TWatchExpectationArray = array of TWatchExpectation;


  { TTestWatches }

  TTestWatches = class(TGDBTestCase)
  private
    FWatches: TcurrentWatches;

    ExpectBreakFooGdb: TWatchExpectationArray;    // direct commands to gdb, to check assumptions  // only Exp and Mtch
    ExpectBreakSubFoo: TWatchExpectationArray;    // Watches, evaluated in SubFoo (nested)
    ExpectBreakFoo: TWatchExpectationArray;       // Watches, evaluated in Foo

    FDbgOutPut: String;
    FDbgOutPutEnable: Boolean;
    FDoStatIntArray: Boolean;

    procedure DoDbgOutput(Sender: TObject; const AText: String); override;
    procedure ClearAllTestArrays;
    function  HasTestArraysData: Boolean;
    function AddTo(var ExpArray: TWatchExpectationArray;
      AnExpr:  string; AFmt: TWatchDisplayFormat;
      AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
      AFlgs: TWatchExpectationFlags = [];
      AStackFrame: Integer = 0
    ): PWatchExpectation;
    function AddTo(var ExpArray: TWatchExpectationArray; ATestName: String;
      AnExpr:  string; AFmt: TWatchDisplayFormat;
      AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
      AFlgs: TWatchExpectationFlags = [];
      AStackFrame: Integer = 0
    ): PWatchExpectation;
    procedure UpdRes(AWatchExp: PWatchExpectation;
      ASymbolType: TSymbolType;
      AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
      AFlgs: TWatchExpectationFlags = []
    );

    procedure AddExpectBreakFooGdb;
    procedure AddExpectBreakFooAll;
    procedure AddExpectBreakFooMixInfo;
    //procedure AddExpectBreakSubFoo;
    procedure AddExpectBreakFooAndSubFoo;  // check for caching issues
    procedure RunTestWatches(NamePreFix: String;
                             TestExeName, ExtraOpts: String;
                             UsedUnits: array of TUsesDir
                            );
  public
    procedure DebugInteract(dbg: TGDBMIDebugger);
  published
    procedure TestWatches;
  end;

implementation

const
  RNoPreQuote  = '(^|[^''])'; // No open qoute (Either at start, or other char)
  RNoPostQuote = '($|[^''])'; // No close qoute (Either at end, or other char)
  Match_Pointer = '\$[0-9A-F]+';
  M_Int = 'Integer|LongInt';

  {%region    * Classes * }
  // _vptr$TOBJECt on older gdb e.g. mac 6.3.50
  Match_ArgTFoo = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = -11';
  Match_ArgTFoo1 = '<TFoo> = \{.*(<|vptr\$)TObject>?.+ValueInt = 31';
  {%ebdregion    * Classes * }
  // Todo: Dwarf fails with dereferenced var pointer types

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

function MatchClass(TypeName: String; AContent: String = ''): String;
begin
  Result := '<'+TypeName+'> = \{.*(vptr\$|<TObject>).+'+AContent;
end;

function MatchClassNil(TypeName: String): String;
begin
  Result := '<'+TypeName+'> = nil';
end;

{ TTestWatches }

procedure TTestWatches.ClearAllTestArrays;
begin
  SetLength(ExpectBreakFooGdb, 0);
  SetLength(ExpectBreakSubFoo, 0);
  SetLength(ExpectBreakFoo, 0);
end;

function TTestWatches.HasTestArraysData: Boolean;
begin
  Result := (Length(ExpectBreakFooGdb) > 0) or
            (Length(ExpectBreakSubFoo) > 0) or
            (Length(ExpectBreakFoo) >0 );

end;

function TTestWatches.AddTo(var ExpArray: TWatchExpectationArray; AnExpr: string;
  AFmt: TWatchDisplayFormat; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags; AStackFrame: Integer = 0): PWatchExpectation;
var
  i: TSymbolType;
begin
  SetLength(ExpArray, Length(ExpArray)+1);
  with ExpArray[Length(ExpArray)-1] do begin
    TestName     := AnExpr;
    Expression   := AnExpr;
    DspFormat    := AFmt;
    for i := low(TSymbolType) to high(TSymbolType) do begin
      Result[i].ExpMatch     := AMtch;
      Result[i].ExpKind      := AKind;
      Result[i].ExpTypeName  := ATpNm;
      Result[i].Flgs         := AFlgs;
      if ( (fnoDwrf in AFlgs) and (i in [stDwarf, stDwarfSet, stDwarf3]) ) or
         ( (fnoDwrfNoSet in AFlgs) and (i in [stDwarf]) )
      then Result[i].Flgs := Result[i].Flgs + [fTstSkip];
    end;
    StackFrame   := AStackFrame;
  end;
  Result := @ExpArray[Length(ExpArray)-1];
end;

function TTestWatches.AddTo(var ExpArray: TWatchExpectationArray; ATestName: String;
  AnExpr: string; AFmt: TWatchDisplayFormat; AMtch: string; AKind: TDBGSymbolKind;
  ATpNm: string; AFlgs: TWatchExpectationFlags; AStackFrame: Integer): PWatchExpectation;
var
  i: TSymbolType;
begin
  SetLength(ExpArray, Length(ExpArray)+1);
  with ExpArray[Length(ExpArray)-1] do begin
    TestName     := ATestName;
    Expression   := AnExpr;
    DspFormat    := AFmt;
    for i := low(TSymbolType) to high(TSymbolType) do begin
      Result[i].ExpMatch     := AMtch;
      Result[i].ExpKind      := AKind;
      Result[i].ExpTypeName  := ATpNm;
      Result[i].Flgs         := AFlgs;
      if ( (fnoDwrf in AFlgs) and (i in [stDwarf, stDwarfSet, stDwarf3]) ) or
         ( (fnoDwrfNoSet in AFlgs) and (i in [stDwarf]) )
      then Result[i].Flgs := Result[i].Flgs + [fTstSkip];
    end;
    StackFrame   := AStackFrame;
  end;
  Result := @ExpArray[Length(ExpArray)-1];
end;

procedure TTestWatches.UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpMatch     := AMtch;
    Result[ASymbolType].ExpKind      := AKind;
    Result[ASymbolType].ExpTypeName  := ATpNm;
    Result[ASymbolType].Flgs         := AFlgs;
  end;
end;

procedure TTestWatches.AddExpectBreakFooGdb;
  function Add(AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
  begin
    Result := AddTo(ExpectBreakFooGdb,AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs );
  end;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.Gdb')] then exit;
  Add('ptype ArgTFoo',  wdfDefault, 'type = \^TFoo = class : PUBLIC TObject', skClass, '', []);
  Add('ptype ArgTFoo^', wdfDefault, 'type = TFoo = class : PUBLIC TObject',   skClass, '', []);

  Add('-data-evaluate-expression sizeof(ArgTFoo)',  wdfDefault, 'value="(4|8)"|(parse|syntax) error in expression', skClass, '',  []);
  Add('-data-evaluate-expression sizeof(ArgTFoo^)', wdfDefault, 'value="\d\d+"|(parse|syntax) error in expression', skClass, '',  []);

  if RUN_GDB_TEST_ONLY > 0 then begin
    ExpectBreakFooGdb[0] := ExpectBreakFooGdb[abs(RUN_GDB_TEST_ONLY)];
    SetLength(ExpectBreakFooGdb, 1);
  end;
end;

procedure TTestWatches.AddExpectBreakFooAll;
  function Add(AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
  begin
    Result := AddTo(ExpectBreakFoo, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs )
  end;
var
  r: PWatchExpectation;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.All')] then exit;

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
  Add('ArgTFoo=ArgTFoo',      wdfDefault,  'True',         skSimple,      'bool', []);
  Add('not(ArgTFoo=ArgTFoo)', wdfDefault,  'False',        skSimple,      'bool', []);
  Add('VArgTFoo=VArgTFoo',    wdfDefault,  'True',         skSimple,      'bool', []);
  Add('ArgTFoo=VArgTFoo',     wdfDefault,  'True',         skSimple,      'bool', []);
  Add('ArgTFoo=ArgPFoo',      wdfDefault,  'False',        skSimple,      'bool', []);
  Add('ArgTFoo=ArgPFoo^',     wdfDefault,  'False',        skSimple,      'bool', []);
  Add('ArgPFoo=ArgPPFoo^',    wdfDefault,  'True',         skSimple,      'bool', []);

  Add('@ArgTFoo=PVarTFoo',    wdfDefault,  'True',         skSimple,      'bool', []);
  Add('@VArgTFoo=PVarTFoo',   wdfDefault,  'False',        skSimple,      'bool', []);

  //Add('ArgTFoo<>ArgTFoo',     wdfDefault,  'False',        skSimple,      'bool', []);
  //Add('ArgTFoo<>ArgPFoo^',    wdfDefault,  'True',         skSimple,      'bool', []);

  Add('ArgTFoo=0',          wdfDefault,  'False',          skSimple,      'bool', []);
  Add('not(ArgTFoo=0)',     wdfDefault,  'True',           skSimple,      'bool', []);
  //Add('ArgTFoo<>0',         wdfDefault,  'True',           skSimple,      'bool', []);

  //Add('ArgTFoo=nil',          wdfDefault,  'False',        skSimple,      'bool', []);
  //Add('not(ArgTFoo=nil)',     wdfDefault,  'True',         skSimple,      'bool', []);
  //Add('ArgTFoo<>nil',         wdfDefault,  'True',         skSimple,      'bool', []);

  {%endregion    * Classes * }

  {%region    * Strings * }
    { strings }
    // todo: some skPOINTER should be skSimple
  // ArgAnsiString
  r:=Add('ArgAnsiString',         wdfDefault,  '''Ansi''$',        skPOINTER,   'AnsiString', []);
                         UpdRes(r, stDwarf3,   '''Ansi''$',        skSimple,    'AnsiString', []);
  r:=Add('VArgAnsiString',        wdfDefault,  '''Ansi 2''$',      skPOINTER,   'AnsiString', []);
                         UpdRes(r, stDwarf3,   '''Ansi 2''$',      skSimple,    'AnsiString', []);
  r:=Add('ArgTMyAnsiString',      wdfDefault,  '''MyAnsi''$',      skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''MyAnsi''$',      skSimple,   '^(TMy)?AnsiString$', [fTpMtch]);
  r:=Add('VArgTMyAnsiString',     wdfDefault,  '''MyAnsi 2''$',    skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''MyAnsi 2''$',    skSimple,   '^(TMy)?AnsiString$', [fTpMtch]);
  r:=Add('ArgPMyAnsiString',      wdfDefault,  Match_Pointer,      skPointer,   'PMyAnsiString', []);
                         UpdRes(r, stStabs,    MatchPointer,       skPointer,   '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=Add('VArgPMyAnsiString',     wdfDefault,  Match_Pointer,      skPointer,   'PMyAnsiString', []);
                         UpdRes(r, stStabs,    MatchPointer,       skPointer,   '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=Add('ArgPMyAnsiString^',     wdfDefault,  '''MyAnsi P''$',    skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''MyAnsi P''$',    skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
  r:=Add('VArgPMyAnsiString^',    wdfDefault,  '''MyAnsi P2''$',   skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch, fnoDwrf]);
                         UpdRes(r, stDwarf3,   '''MyAnsi P2''$',    skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
  r:=Add('ArgPPMyAnsiString',     wdfDefault,  Match_Pointer,      skPointer,   'PPMyAnsiString', []);
  r:=Add('VArgPPMyAnsiString',    wdfDefault,  Match_Pointer,      skPointer,   'PPMyAnsiString', []);
  r:=Add('ArgPPMyAnsiString^',    wdfDefault,  Match_Pointer,      skPointer,   'PMyAnsiString', []);
                         UpdRes(r, stStabs,    MatchPointer,       skPointer,   '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=Add('VArgPPMyAnsiString^',   wdfDefault,  Match_Pointer,      skPointer,   'PMyAnsiString', [fnoDwrf]);
                         UpdRes(r, stStabs,    MatchPointer,       skPointer,   '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=Add('ArgPPMyAnsiString^^',   wdfDefault,  '''MyAnsi P''$',    skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''MyAnsi P''$',    skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);
  r:=Add('VArgPPMyAnsiString^^',  wdfDefault,  '''MyAnsi P2''$',   skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch, fnoDwrf]);
                         UpdRes(r, stDwarf3,   '''MyAnsi P2''$',   skPOINTER,   '^(TMy)?AnsiString$', [fTpMtch]);


  r:=Add('ArgTNewAnsiString',     wdfDefault,  '''NewAnsi''$',     skPOINTER,   'TNewAnsiString', []);
                         UpdRes(r, stStabs,    '''NewAnsi''$',     skPOINTER,   '(TNew)?AnsiString', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''NewAnsi''$',     skSimple,    'TNewAnsiString', []);
  r:=Add('VArgTNewAnsiString',    wdfDefault,  '''NewAnsi 2''$',   skPOINTER,   'TNewAnsiString', []);
                         UpdRes(r, stStabs,    '''NewAnsi 2''$',   skPOINTER,   '(TNew)?AnsiString', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''NewAnsi 2''$',   skSimple,    'TNewAnsiString', []);
  r:=Add('ArgPNewAnsiString',     wdfDefault,  MatchPointer,       skPointer,   'PNewAnsiString', []);
                         UpdRes(r, stStabs,    MatchPointer,       skPointer,   '(\^|PNew|P)AnsiString|PPChar', [fTpMtch]);
  r:=Add('VArgPNewAnsiString',    wdfDefault,  MatchPointer,       skPointer,   'PNewAnsiString', []);
                         UpdRes(r, stStabs,    MatchPointer,       skPointer,   '(\^|PNew|P)AnsiString|PPChar', [fTpMtch]);
  r:=Add('ArgPNewAnsiString^',    wdfDefault,  '''NewAnsi P''',    skPOINTER,   'TNewAnsiString', []);
                         UpdRes(r, stStabs,    '''NewAnsi P''$',   skPOINTER,   '(TNew)?AnsiString', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''NewAnsi''$',     skSimple,    'TNewAnsiString', []);
  r:=Add('VArgPNewAnsiString^',   wdfDefault,  '''NewAnsi P2''',   skPOINTER,   'TNewAnsiString', [fnoDwrf]);
                         UpdRes(r, stStabs,    '''NewAnsi P2''$',  skPOINTER,   '(TNew)?AnsiString', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''NewAnsi 2''$',   skSimple,    'TNewAnsiString', []);


  // typecasts
  r:=Add('AnsiString(ArgTMyAnsiString)',   wdfDefault,
                                               '''MyAnsi''$',      skPOINTER,   'AnsiString|^char', [fTpMtch]);
                         UpdRes(r, stDwarf3,   '''MyAnsi''$',      skSimple,    'AnsiString', []);

  r:=Add('PChar(ArgTMyAnsiString)',   wdfDefault,
                                               '''MyAnsi''$',      skPOINTER,   '(\^|p)char', [fTpMtch]);
                         UpdRes(r, stStabs,    '''MyAnsi''$',      skPOINTER,   'pchar|AnsiString', [fTpMtch]);
                         //UpdRes(r, stDwarf3,   '''MyAnsi''$',      skSimple,    'AnsiString', []);

  // accessing len/refcount
  r:=Add('^longint(ArgTMyAnsiString)[-1]',   wdfDefault,
                                               '6',      skSimple,   'longint', []);

  // accessing char
  // TODO: only works with dwarf 3
  r:=Add('ArgTMyAnsiString[1]',    wdfDefault,  '.',      skSimple,   'char', []);
                         UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=Add('VArgTMyAnsiString[1]',   wdfDefault,  '.',      skSimple,   'char', []);
                         UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=Add('ArgPMyAnsiString^[1]',   wdfDefault,  '.',      skSimple,   'char', []);
                         UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=Add('VArgPMyAnsiString^[1]',  wdfDefault,  '.',      skSimple,   'char', [fnoDwrf]);
                         UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);


  //r:=Add('ArgTNewAnsiString',     wdfDefault,  '''NewAnsi''$',     skPOINTER,   '(TNew)?AnsiString', []);
  //                       UpdRes(r, stDwarf3,   '''NewAnsi''$',     skSimple,    '(TNew)?AnsiString', [fTpMtch]);
  //r:=Add('VArgTNewAnsiString',    wdfDefault,  '''NewAnsi 2''$',   skPOINTER,   '(TNew)?AnsiString', []);
  //                       UpdRes(r, stDwarf3,   '''NewAnsi 2''$',   skSimple,    '(TNew)?AnsiString', [fTpMtch]);
  //r:=Add('ArgPNewAnsiString',     wdfDefault,  MatchPointer,       skPointer,   '(\^|PNew|P)AnsiString', []);
  //r:=Add('VArgPNewAnsiString',    wdfDefault,  MatchPointer,       skPointer,   '(\^|PNew|P)AnsiString', []);
  //r:=Add('ArgPNewAnsiString^',    wdfDefault,  '''NewAnsi P''',    skPOINTER,   '(TNew)?AnsiString', []);
  //                       UpdRes(r, stDwarf3,   '''NewAnsi''$',     skSimple,    '(TNew)?AnsiString', [fTpMtch]);
  //r:=Add('VArgPNewAnsiString^',   wdfDefault,  '''NewAnsi P2''',   skPOINTER,   '(TNew)?AnsiString', []);
  //                       UpdRes(r, stDwarf3,   '''NewAnsi 2''$',   skSimple,    '(TNew)?AnsiString', [fTpMtch]);



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


  Add('ArgTMyAnsiString',      wdfMemDump,  ': 4d 79 41 6e 73 69 00',      skPOINTER,      '^(TMy)?AnsiString$', [fTpMtch]);

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
  Add('ArgExtended',     wdfDefault,  '2\.345',      skSimple,      'Extended|double', [fTpMtch]);
  Add('VArgExtended',    wdfDefault,  '2\.345',      skSimple,      'Extended|double', [fTpMtch]);

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
  if FDoStatIntArray then
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
  if FDoStatIntArray then
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

  if RUN_TEST_ONLY > 0 then begin
    ExpectBreakFoo[0] := ExpectBreakFoo[abs(RUN_TEST_ONLY)];
    SetLength(ExpectBreakFoo, 1);
  end;
end;

procedure TTestWatches.AddExpectBreakFooMixInfo;
  procedure Add(AName, AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags);
  begin
    AddTo(ExpectBreakFoo, AName, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs )
  end;
  procedure AddTC(AVar, ATCast:  string; AExpClass: String = ''; AFlgs: TWatchExpectationFlags = [];
                  AIntMember: String = ''; AIntValue: integer = 0);
  begin
    if AExpClass = '' then AExpClass := ATCast;
    If ATCast <> ''
    then Add('',ATCast+'('+AVar+')', wdfDefault, MatchClass(AExpClass, ''), skClass, AExpClass, AFlgs)
    else Add('',AVar,                wdfDefault, MatchClass(AExpClass, ''), skClass, AExpClass, AFlgs);
    if AIntMember <> '' then
      Add('', ATCast+'('+AVar+').'+AIntMember,  wdfDefault, IntToStr(AIntValue), skSimple, M_Int, [fTpMtch]);
  end;
  procedure AddTCN(AVar, ATCast:  string; AExpClass: String = ''; AFlgs: TWatchExpectationFlags = []);
  begin
    if AExpClass = '' then AExpClass := ATCast;
    If ATCast <> ''
    then Add('',ATCast+'('+AVar+')', wdfDefault, MatchClassNil(AExpClass), skClass, AExpClass, AFlgs)
    else Add('',AVar,                wdfDefault, MatchClassNil(AExpClass), skClass, AExpClass, AFlgs);
  end;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.Mix')] then exit;

  // Type Casting objects with mixed symbol type
  AddTC('VarOTestTCast', '', 'TObject');
  AddTC('VarOTestTCast', 'TObject', '');
  AddTC('VarOTestTCast', 'TClassTCast', '', [], 'b', 0);
  AddTC('VarOTestTCast', 'TClassTCast3', 'TClassTCast(3)?', [fTpMtch], 'b', 0);

  AddTC('VarOTestTCastObj', '', 'TObject');
  AddTC('VarOTestTCastObj', 'TObject', '');
  AddTC('VarOTestTCastObj', 'TClassTCastObject', '');

  AddTC('VarOTestTCastComp', '', 'TObject');
  AddTC('VarOTestTCastComp', 'TObject', '');
  AddTC('VarOTestTCastComp', 'TComponent', '');
  AddTC('VarOTestTCastComp', 'TClassTCastComponent', '', [], 'b', 0);

  AddTC('VarOTestTCast2', '', 'TObject');
  AddTC('VarOTestTCast2', 'TObject', '');
  AddTC('VarOTestTCast2', 'TClassTCast', '', [], 'b', 0);
  AddTC('VarOTestTCast2', 'TClassTCast2', '', [], 'b', 0);

  AddTC('VarOTestTCastUW1', '', 'TObject');
  AddTC('VarOTestTCastUW1', 'TObject', '');
  AddTC('VarOTestTCastUW1', 'TClassUW1Base', '');
  AddTC('VarOTestTCastUW1', 'TClassTCastUW1', '');

  AddTC('VarOTestTCastUW1Obj', '', 'TObject');
  AddTC('VarOTestTCastUW1Obj', 'TObject', '');
  AddTC('VarOTestTCastUW1Obj', 'TClassUW1BaseObject', '');
  AddTC('VarOTestTCastUW1Obj', 'TClassTCastUW1Object', '');

  AddTC('VarOTestTCastUW1Comp', '', 'TObject');
  AddTC('VarOTestTCastUW1Comp', 'TObject', '');
  AddTC('VarOTestTCastUW1Comp', 'TComponent', '');
  AddTC('VarOTestTCastUW1Comp', 'TClassUW1BaseComponent', '');
  AddTC('VarOTestTCastUW1Comp', 'TClassTCastUW1Component', '');


  AddTC('VarCTestTCastComp', '', 'TComponent');
  AddTC('VarCTestTCastComp', 'TObject', '');
  AddTC('VarCTestTCastComp', 'TComponent', '');
  AddTC('VarCTestTCastComp', 'TClassTCast', '');

  AddTC('VarCTestTCastUW1Comp', '', 'TComponent');
  AddTC('VarCTestTCastUW1Comp', 'TObject', '');
  AddTC('VarCTestTCastUW1Comp', 'TComponent', '');
  AddTC('VarCTestTCastUW1Comp', 'TClassUW1BaseComponent', '');
  AddTC('VarCTestTCastUW1Comp', 'TClassTCastUW1Component', '');


  AddTC('VarTestTCast', '', 'TClassTCast');
  AddTC('VarTestTCast', 'TObject', '');
  AddTC('VarTestTCast', 'TClassTCast', '');
  AddTC('VarTestTCast', 'TClassTCast3', 'TClassTCast(3)?', [fTpMtch]);

  AddTC('VarTestTCastObj', '', 'TClassTCastObject');
  AddTC('VarTestTCastObj', 'TObject', '');
  AddTC('VarTestTCastObj', 'TClassTCastObject', '');

  AddTC('VarTestTCastComp', '', 'TClassTCastComponent');
  AddTC('VarTestTCastComp', 'TObject', '');
  AddTC('VarTestTCastComp', 'TComponent', '');
  AddTC('VarTestTCastComp', 'TClassTCastComponent', '');

  AddTC('VarTestTCast2', '', 'TClassTCast2');
  AddTC('VarTestTCast2', 'TObject', '');
  AddTC('VarTestTCast2', 'TClassTCast', '');
  AddTC('VarTestTCast2', 'TClassTCast2', '');

  AddTC('VarTestTCast3', '', 'TClassTCast(3)?', [fTpMtch]);
  AddTC('VarTestTCast3', 'TObject', '');
  AddTC('VarTestTCast3', 'TClassTCast', '');

  AddTC('VarTestTCastUW1', '', 'TClassTCastUW1');
  AddTC('VarTestTCastUW1', 'TObject', '');
  AddTC('VarTestTCastUW1', 'TClassUW1Base', '');
  AddTC('VarTestTCastUW1', 'TClassTCastUW1', '');

  AddTC('VarTestTCastUW1Obj', '', 'TClassTCastUW1Object');
  AddTC('VarTestTCastUW1Obj', 'TObject', '');
  AddTC('VarTestTCastUW1Obj', 'TClassUW1BaseObject', '');
  AddTC('VarTestTCastUW1Obj', 'TClassTCastUW1Object', '');

  AddTC('VarTestTCastUW1Comp', '', 'TClassTCastUW1Component');
  AddTC('VarTestTCastUW1Comp', 'TObject', '');
  AddTC('VarTestTCastUW1Comp', 'TComponent', '');
  AddTC('VarTestTCastUW1Comp', 'TClassUW1BaseComponent', '');
  AddTC('VarTestTCastUW1Comp', 'TClassTCastUW1Component', '');



  //AddTCN('VarNOTestTCast', '', 'TObject');
  //AddTCN('VarNOTestTCast', 'TObject', '');
  //AddTCN('VarNOTestTCast', 'TClassTCast', '');
  //AddTCN('VarNOTestTCast', 'TClassTCast3', 'TClassTCast(3)?', [fTpMtch]);


  // MIXED symbol info types
  if FDoStatIntArray then
  Add('', 'VarStatIntArray',  wdfDefault,     '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18',
                                skSimple,       'TStatIntArray',
                                []);
end;

procedure TTestWatches.AddExpectBreakFooAndSubFoo;
  procedure AddF(AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
    AStackFrame: Integer=0);
  begin
    AddTo(ExpectBreakFoo, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs, AStackFrame)
  end;
  procedure AddS(AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
    AStackFrame: Integer=0);
  begin
    AddTo(ExpectBreakSubFoo, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs, AStackFrame)
  end;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.Cache')] then exit;
  AddS('VarCacheTest1', wdfDefault, MatchRecord('TCacheTest', 'CTVal = 101'),
       skRecord, 'TCacheTest',  []);
  AddF('VarCacheTest1', wdfDefault, '<TCacheTest(Type)?> = \{.*(<|vptr\$)TObject>?.+CTVal = 201',
       skClass, 'TCacheTest(Type)?',  [fTpMtch]);

  AddS('VarCacheTest2', wdfDefault, '102',  skSimple, M_Int,  [fTpMtch], 0);
  AddS('VarCacheTest2', wdfDefault, '202',  skSimple, M_Int,  [fTpMtch], 1);
end;

procedure TTestWatches.DoDbgOutput(Sender: TObject; const AText: String);
begin
  inherited DoDbgOutput(Sender, AText);
  if FDbgOutPutEnable then
    FDbgOutPut := FDbgOutPut + AText;
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

procedure TTestWatches.RunTestWatches(NamePreFix: String; TestExeName, ExtraOpts: String;
  UsedUnits: array of TUsesDir);

var
  dbg: TGDBMIDebugger;

  function SkipTest(const Data: TWatchExpectation): Boolean;
  begin
    Result := True;
    if fTstSkip in Data.Result[SymbolType].Flgs then exit;
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
    Stack: Integer;
    n: String;
    DataRes: TWatchExpectationResult;
  begin
    if not TestTrue('Dbg did NOT enter dsError', dbg.State <> dsError) then exit;
    rx := nil;
    Stack := Data.StackFrame;
    DataRes := Data.Result[SymbolType];

    n := Data.TestName;
    if n = '' then n := Data.Expression + ' (' + TWatchDisplayFormatNames[Data.DspFormat] + ')';
    Name := Name + ' ' + n;
    flag := AWatch <> nil;
    if flag then begin;
      WV := AWatch.Values[1, Stack];// trigger read
      s := WV.Value;
      flag := flag and TestTrue  (Name+ ' (HasValue)',   WV.Validity = ddsValid);
      //flag := flag and TestFalse (Name+ ' (One Value)',  AWatch.HasMultiValue);
    end
    else
      s := WatchValue;

    if not TestTrue('Dbg did NOT enter dsError', dbg.State <> dsError) then exit;

    //if flag then begin
      rx := TRegExpr.Create;
      rx.ModifierI := true;
      rx.Expression := DataRes.ExpMatch;
      if DataRes.ExpMatch <> ''
      then TestTrue(Name + ' Matches "'+DataRes.ExpMatch + '", but was "' + s + '"', rx.Exec(s));
    //end;

    flag := (AWatch <> nil) and (DataRes.ExpTypeName <> '');
    if flag then flag := TestTrue(Name + ' has typeinfo',  WV.TypeInfo <> nil);
    if flag then flag := TestEquals(Name + ' kind',  KindName[DataRes.ExpKind], KindName[WV.TypeInfo.Kind]);
    if flag then begin
      if fTpMtch  in DataRes.Flgs
      then begin
        FreeAndNil(rx);
        s := WV.TypeInfo.TypeName;
        rx := TRegExpr.Create;
        rx.ModifierI := true;
        rx.Expression := DataRes.ExpTypeName;
        TestTrue(Name + ' TypeName matches '+DataRes.ExpTypeName+' but was '+WV.TypeInfo.TypeName,  rx.Exec(s))
       end
      else TestEquals(Name + ' TypeName',  LowerCase(DataRes.ExpTypeName), LowerCase(WV.TypeInfo.TypeName));
    end;
    FreeAndNil(rx);
  end;

var
  i, Only: Integer;
  OnlyName: String;
  WList, WListSub: Array of TCurrentWatch;

begin
  TestBaseName := NamePreFix;
  if not HasTestArraysData then exit;
  Only := StrToIntDef(TestControlForm.EdOnlyWatch.Text, -1);
  if Only < 0
  then OnlyName := TestControlForm.EdOnlyWatch.Text;


  try
    TestCompile(AppDir + 'WatchesPrg.pas', TestExeName, UsedUnits, '', ExtraOpts);
  except
    on e: Exception do begin
      TestTrue('Compile error: ' + e.Message, False);
      exit;
    end;
  end;

  try
    dbg := StartGDB(AppDir, TestExeName);
    FWatches := Watches.CurrentWatches;

    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC_NEST) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    if dbg.State = dsError then
      Fail(' Failed Init');

    (* Create all watches *)
    SetLength(WList, length(ExpectBreakFoo));
    for i := low(ExpectBreakFoo) to high(ExpectBreakFoo) do begin
      if ((Only >=0) and (Only <> i)) or ((OnlyName<>'') and (OnlyName<>ExpectBreakFoo[i].TestName)) then continue;
      if not SkipTest(ExpectBreakFoo[i]) then begin
        WList[i] := TCurrentWatch.Create(FWatches);
        WList[i].Expression := ExpectBreakFoo[i].Expression;
        WList[i].DisplayFormat := ExpectBreakFoo[i].DspFormat;
        WList[i].enabled := True;
      end;
    end;
    SetLength(WListSub, length(ExpectBreakSubFoo));
    for i := low(ExpectBreakSubFoo) to high(ExpectBreakSubFoo) do begin
      if ((Only >=0) and (Only <> i)) or ((OnlyName<>'') and (OnlyName<>ExpectBreakSubFoo[i].TestName)) then continue;
      if not SkipTest(ExpectBreakSubFoo[i]) then begin
        WListSub[i] := TCurrentWatch.Create(FWatches);
        WListSub[i].Expression := ExpectBreakSubFoo[i].Expression;
        WListSub[i].DisplayFormat := ExpectBreakSubFoo[i].DspFormat;
        WListSub[i].enabled := True;
      end;
    end;

    (* Start debugging *)
    dbg.ShowConsole := True;
    dbg.Run;
    if TestTrue('State=Pause', dbg.State = dsPause)
    then begin
      (* Hit first breakpoint: NESTED SubFoo -- (1st loop) Called with none nil data *)
      for i := low(ExpectBreakSubFoo) to high(ExpectBreakSubFoo) do begin
        if ((Only >=0) and (Only <> i)) or ((OnlyName<>'') and (OnlyName<>ExpectBreakSubFoo[i].TestName)) then continue;
        if not SkipTest(ExpectBreakSubFoo[i]) then
          TestWatch('Brk1 '+IntToStr(i)+' ', WListSub[i], ExpectBreakSubFoo[i]);
      end;

      dbg.Run;
    end;

    if TestTrue('State=Pause', dbg.State = dsPause)
    then begin
      (* Hit 2nd breakpoint: Foo -- (1st loop) Called with none nil data *)

      FDbgOutPutEnable := True;
      for i := low(ExpectBreakFooGdb) to high(ExpectBreakFooGdb) do begin
        if (Only >=0) and (Only <> i) then continue;
        if not SkipTest(ExpectBreakFooGdb[i]) then begin
          FDbgOutPut := '';
          dbg.TestCmd(ExpectBreakFooGdb[i].Expression);
          TestWatch('Brk1 Direct Gdb '+IntToStr(i)+' ', nil, ExpectBreakFooGdb[i], FDbgOutPut);
        end;
      end;
      FDbgOutPutEnable := False;

      for i := low(ExpectBreakFoo) to high(ExpectBreakFoo) do begin
        if ((Only >=0) and (Only <> i)) or ((OnlyName<>'') and (OnlyName<>ExpectBreakFoo[i].TestName)) then continue;
        if not SkipTest(ExpectBreakFoo[i]) then
          TestWatch('Brk1 '+IntToStr(i)+' ', WList[i], ExpectBreakFoo[i]);
      end;

      dbg.Run;
    end;

    // TODO: 2nd round, with NIL data
	//DebugInteract(dbg);

    dbg.Stop;
  except
    on e: Exception do begin
      TestTrue('Error: ' + e.Message, False);
      exit;
    end;
  end;
  dbg.Done;
  CleanGdb;
  dbg.Free;
end;

procedure TTestWatches.TestWatches;
var
  TestExeName: string;
  UsedUnits: TUsesDir;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch')] then exit;

  FDoStatIntArray := TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.Unstable')];
  // GDB 7.0 with fpc 2.4.x has issues with "array of int"
  FDoStatIntArray := FDoStatIntArray and
                     not ((pos('2.4.', CompilerInfo.Name) > 0) and (DebuggerInfo.Version = 70000));

  ClearTestErrors;

  ClearAllTestArrays;
  AddExpectBreakFooGdb;
  AddExpectBreakFooAll;
  //AddExpectBreakFooMixInfo;
  AddExpectBreakFooAndSubFoo;
  RunTestWatches('', TestExeName,  '', []);

  if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.Mix')]
  then begin

    ClearAllTestArrays;
    AddExpectBreakFooMixInfo;
    with UsedUnits do begin
      DirName:= AppDir + 'u1' + DirectorySeparator + 'unitw1.pas';
      ExeId:= '';
      SymbolType:= stNone;
      ExtraOpts:= '';
      NamePostFix:= ''
    end;
    RunTestWatches('unitw1=none', TestExeName,  '-dUSE_W1', [UsedUnits]);

    if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestWatch.Mix.All')]
    then begin
      if (stStabs in CompilerInfo.SymbolTypes) and (stStabs in DebuggerInfo.SymbolTypes)
      then begin
        ClearAllTestArrays;
        AddExpectBreakFooMixInfo;
        with UsedUnits do begin
          DirName:= AppDir + 'u1' + DirectorySeparator + 'unitw1.pas';
          ExeId:= '';
          SymbolType:= stStabs;
          ExtraOpts:= '';
          NamePostFix:= ''
        end;
        RunTestWatches('unitw1=stabs', TestExeName,  '-dUSE_W1', [UsedUnits]);
      end;

      if (stDwarf in CompilerInfo.SymbolTypes) and (stDwarf in DebuggerInfo.SymbolTypes)
      then begin
        ClearAllTestArrays;
        AddExpectBreakFooMixInfo;
        with UsedUnits do begin
          DirName:= AppDir + 'u1' + DirectorySeparator + 'unitw1.pas';
          ExeId:= '';
          SymbolType:= stDwarf;
          ExtraOpts:= '';
          NamePostFix:= ''
        end;
        RunTestWatches('unitw1=dwarf', TestExeName,  '-dUSE_W1', [UsedUnits]);
      end;

      if (stDwarf3 in CompilerInfo.SymbolTypes) and (stDwarf3 in DebuggerInfo.SymbolTypes)
      then begin
        ClearAllTestArrays;
        AddExpectBreakFooMixInfo;
        with UsedUnits do begin
          DirName:= AppDir + 'u1' + DirectorySeparator + 'unitw1.pas';
          ExeId:= '';
          SymbolType:= stDwarf3;
          ExtraOpts:= '';
          NamePostFix:= ''
        end;
        RunTestWatches('unitw1=dwarf_3', TestExeName,  '-dUSE_W1', [UsedUnits]);
      end;
    end;

  end;

  AssertTestErrors;
end;



initialization

  RegisterDbgTest(TTestWatches);
end.

