unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc, SynRegExpr, Forms, StdCtrls, Controls;

const
  BREAK_LINE_FOOFUNC_NEST  = 206;
  BREAK_LINE_FOOFUNC       = 230;
  BREAK_LINE_FOOFUNC_ARRAY = 254;
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
    (IgnDwrf,            // ignore error for dwarf at all
     IgnDwrf2,           // ignore error for dwarf 2
     IgnDwrf2IfNoSet,    // ignore error for dwarf2 (-gw) without set
     IgnDwrf3,           // ignore error for dwarf 3
     IgnStabs,
     //IgnDwrfSet,   // no dwarf2 with set // no dwarf3

     IgnData,           // Ignore the data part
     IgnDataDw,         // Ignore the data part, if dwarf
     IgnDataDw2,        // Ignore the data part, if dwarf 2
     IgnDataDw3,        // Ignore the data part, if dwarf 3
     IgnDataSt,         // Ignore the data part, if Stabs

     IgnKind,           // Ignore skSimple, ....
     IgnKindDw,
     IgnKindDw2,
     IgnKindDw3,
     IgnKindSt,

     IgnKindPtr,           // Ignore skSimple, ONLY if got kind=skPointer
     IgnKindPtrDw,
     IgnKindPtrDw2,
     IgnKindPtrDw3,
     IgnKindPtrSt,

     IgnTpName,           // Ignore the typename
     IgnTpNameDw,
     IgnTpNameDw2,
     IgnTpNameDw3,
     IgnTpNameSt,

     fTstSkip,       // Do not run test
     fTstSkipDwarf3,
     fTpMtch
    );
  TWatchExpectationFlags = set of TWatchExpectationFlag;

const
  WatchExpFlagMask: array[TSymbolType] of TWatchExpectationFlags
  = ( {stNone}     [],
      {stStabs}    [IgnStabs,
                    IgnData,    IgnDataSt,
                    IgnKind,    IgnKindSt,
                    IgnKindPtr, IgnKindPtrSt,
                    IgnTpName,  IgnTpNameSt
                   ],
      {stDwarf}    [IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet,
                    IgnData,    IgnDataDw, IgnDataDw2,
                    IgnKind,    IgnKindDw, IgnKindDw2,
                    IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw2,
                    IgnTpName,  IgnTpNameDw, IgnTpNameDw2
                   ],
      {stDwarfSet} [IgnDwrf, IgnDwrf2,
                    IgnData,    IgnDataDw, IgnDataDw2,
                    IgnKind,    IgnKindDw, IgnKindDw2,
                    IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw2,
                    IgnTpName,  IgnTpNameDw, IgnTpNameDw2
                   ],
      {stDwarf3}   [IgnDwrf, IgnDwrf3,
                    IgnData,    IgnDataDw, IgnDataDw3,
                    IgnKind,    IgnKindDw, IgnKindDw3,
                    IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw3,
                    IgnTpName,  IgnTpNameDw, IgnTpNameDw3
                   ]
    );

  WatchExpFlagSIgnAll     = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3];
  WatchExpFlagSIgnData    = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnData,    IgnDataDw, IgnDataDw2, IgnDataDw3, IgnDataSt];
  WatchExpFlagSIgnKind    = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnKind,    IgnKindDw, IgnKindDw2, IgnKindDw3, IgnKindSt];
  WatchExpFlagSIgnKindPtr = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnKindPtr, IgnKindPtrDw, IgnKindPtrDw2, IgnKindPtrDw3, IgnKindPtrSt];
  WatchExpFlagSIgnTpName  = [IgnStabs, IgnDwrf, IgnDwrf2, IgnDwrf2IfNoSet, IgnDwrf3,  IgnTpName,  IgnTpNameDw, IgnTpNameDw2, IgnTpNameDw3, IgnTpNameSt];

type

  PWatchExpectation= ^TWatchExpectation;
  TWatchExpectationResult = record
    ExpMatch: string;
    ExpKind: TDBGSymbolKind;
    ExpTypeName: string;
    Flgs: TWatchExpectationFlags;
    MinGdb, MinFpc: Integer;
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
    ExpectBreakFooArray: TWatchExpectationArray;       // Watches, evaluated in Foo_Array

    FCurrentExpArray: ^TWatchExpectationArray; // currently added to

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
      AStackFrame: Integer = 0;
      AMinGdb: Integer = 0; AMinFpc: Integer = 0
    ): PWatchExpectation;
    function AddTo(var ExpArray: TWatchExpectationArray; ATestName: String;
      AnExpr:  string; AFmt: TWatchDisplayFormat;
      AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
      AFlgs: TWatchExpectationFlags = [];
      AStackFrame: Integer = 0;
      AMinGdb: Integer = 0; AMinFpc: Integer = 0
    ): PWatchExpectation;

    // using FCurrentExpArray
    function Add(AnExpr:  string; AFmt: TWatchDisplayFormat; AMtch: string;
                 AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
    function AddFmtDef        (AnExpr, AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddStringFmtDef  (AnExpr, AMtch, ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddShortStrFmtDef(AnExpr, AMtch: string; ATpNm: string = 'ShortString'; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddCharFmtDef    (AnExpr, AMtch: string; ATpNm: string = 'Char'; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddPointerFmtDef (AnExpr,        ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddPointerFmtDef (AnExpr, AMtch, ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
    function AddPointerFmtDefRaw(AnExpr, AMtch, ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;


    procedure UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
      AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
      AMinGdb: Integer; AMinFpc: Integer
    );
    procedure UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
      AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags = []
    );
    procedure UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
      AMtch: string; AKind: TDBGSymbolKind
    );
    procedure UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
      AKind: TDBGSymbolKind
    );
    procedure UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
      ATpNm: string; AFlgs: TWatchExpectationFlags
    );

    procedure UpdRes(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes;
      ATpNm: string; AFlgs: TWatchExpectationFlags
    );
    procedure UpdResMinGdb(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType; AMinGdb: Integer);
    procedure UpdResMinFpc(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType; AMinFpc: Integer);

    procedure AddExpectBreakFooGdb;
    procedure AddExpectBreakFooAll;
    procedure AddExpectBreakFooArray;
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
  SetLength(ExpectBreakFooArray, 0);
end;

function TTestWatches.HasTestArraysData: Boolean;
begin
  Result := (Length(ExpectBreakFooGdb) > 0) or
            (Length(ExpectBreakSubFoo) > 0) or
            (Length(ExpectBreakFoo) > 0) or
            (Length(ExpectBreakFooArray) >0 );

end;

function TTestWatches.AddTo(var ExpArray: TWatchExpectationArray; AnExpr: string;
  AFmt: TWatchDisplayFormat; AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags; AStackFrame: Integer = 0; AMinGdb: Integer = 0;
  AMinFpc: Integer = 0): PWatchExpectation;
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
      Result[i].MinGdb := AMinGdb;
      Result[i].MinFpc := AMinFpc;
    end;
    StackFrame   := AStackFrame;
  end;
  Result := @ExpArray[Length(ExpArray)-1];
end;

function TTestWatches.AddTo(var ExpArray: TWatchExpectationArray; ATestName: String;
  AnExpr: string; AFmt: TWatchDisplayFormat; AMtch: string; AKind: TDBGSymbolKind;
  ATpNm: string; AFlgs: TWatchExpectationFlags; AStackFrame: Integer; AMinGdb: Integer = 0;
  AMinFpc: Integer = 0): PWatchExpectation;
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
      Result[i].MinGdb := AMinGdb;
      Result[i].MinFpc := AMinFpc;
    end;
    StackFrame   := AStackFrame;
  end;
  Result := @ExpArray[Length(ExpArray)-1];
end;

function TTestWatches.Add(AnExpr: string; AFmt: TWatchDisplayFormat; AMtch: string;
  AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddTo(FCurrentExpArray^, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddFmtDef(AnExpr, AMtch: string; AKind: TDBGSymbolKind; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := Add(AnExpr, wdfDefault, AMtch, AKind, ATpNm, AFlgs );
end;

function TTestWatches.AddStringFmtDef(AnExpr, AMtch, ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  // TODO, encoding of special chars
  // String should be skSimple
  // but the IDE only gets that with Dwarf-3
  // might be prefixed, with address
  Result := AddFmtDef(AnExpr, '''' + AMtch + '''$', skPOINTER, ATpNm, AFlgs );
  UpdRes(Result, stDwarf3,           skSimple);
end;

function TTestWatches.AddShortStrFmtDef(AnExpr, AMtch: string; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  // TODO, encoding of special chars
  // shortstring
  // might be prefixed, with address
  Result := AddFmtDef(AnExpr, '''' + AMtch + '''$', skSimple, ATpNm, AFlgs );
end;

function TTestWatches.AddCharFmtDef(AnExpr, AMtch: string; ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  // TODO, encoding of special chars
  // might be prefixed, with address
  Result := AddFmtDef(AnExpr, '''' + AMtch + '''$', skSimple, ATpNm, AFlgs );
end;

function TTestWatches.AddPointerFmtDef(AnExpr, ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, MatchPointer(ATpNm), skPointer, ATpNm, AFlgs );
end;

function TTestWatches.AddPointerFmtDef(AnExpr, AMtch, ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, MatchPointer(AMtch), skPointer, ATpNm, AFlgs );
end;

function TTestWatches.AddPointerFmtDefRaw(AnExpr, AMtch, ATpNm: string;
  AFlgs: TWatchExpectationFlags): PWatchExpectation;
begin
  Result := AddFmtDef(AnExpr, AMtch, skPointer, ATpNm, AFlgs );
end;

procedure TTestWatches.UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
  AMinGdb: Integer; AMinFpc: Integer);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpMatch     := AMtch;
    Result[ASymbolType].ExpKind      := AKind;
    Result[ASymbolType].ExpTypeName  := ATpNm;
    Result[ASymbolType].Flgs         := AFlgs;
    Result[ASymbolType].MinGdb := AMinGdb;
    Result[ASymbolType].MinFpc := AMinFpc;
  end;
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

procedure TTestWatches.UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMtch: string; AKind: TDBGSymbolKind);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpMatch     := AMtch;
    Result[ASymbolType].ExpKind      := AKind;
  end;
end;

procedure TTestWatches.UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AKind: TDBGSymbolKind);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpKind      := AKind;
  end;
end;

procedure TTestWatches.UpdRes(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  ATpNm: string; AFlgs: TWatchExpectationFlags);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].ExpTypeName  := ATpNm;
    Result[ASymbolType].Flgs         := AFlgs;
  end;
end;

procedure TTestWatches.UpdRes(AWatchExp: PWatchExpectation; ASymbolTypes: TSymbolTypes;
  ATpNm: string; AFlgs: TWatchExpectationFlags);
var
  i: TSymbolType;
begin
  for i := low(TSymbolType) to high(TSymbolType) do
    if i in ASymbolTypes then
      UpdRes(AWatchExp, i, ATpNm, AFlgs);
end;

procedure TTestWatches.UpdResMinGdb(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMinGdb: Integer);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].MinGdb := AMinGdb;
  end;
end;

procedure TTestWatches.UpdResMinFpc(AWatchExp: PWatchExpectation; ASymbolType: TSymbolType;
  AMinFpc: Integer);
begin
  with AWatchExp^ do begin
    Result[ASymbolType].MinFpc := AMinFpc;
  end;
end;

procedure TTestWatches.AddExpectBreakFooGdb;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.Gdb')] then exit;
  FCurrentExpArray := @ExpectBreakFooGdb;

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
var
  r: PWatchExpectation;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.All')] then exit;
  FCurrentExpArray := @ExpectBreakFoo;

  {%region    * records * }
  // Foo(var XXX: PRecord); DWARF has problems with the implicit pointer for "var"

  // param to FooFunc
  AddFmtDef('ArgTRec',           MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,    'TRec', []);
  AddFmtDef('ArgPRec',           MatchPointer('PRec'),                   skPointer,   'PRec', []);
  AddFmtDef('ArgPRec^',          MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
  AddFmtDef('ArgPPRec',          MatchPointer('PPRec'),                  skPointer,   'PPRec', []);
  AddFmtDef('ArgPPRec^',         MatchPointer('PRec'),                   skPointer,   'PRec', []);
  AddFmtDef('ArgPPRec^^',        MatchRecord('TREC', 2, '.'),            skRecord,    'TRec', []);
  AddFmtDef('ArgTNewRec',        MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,    'T(New)?Rec', [fTpMtch]);
  AddFmtDef('ArgTRec.ValInt',    '-1',                                   skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgPRec^.ValInt',   '1',                                    skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgPPRec^^.ValInt', '2',                                    skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgPRec^.ValFoo',   MatchClass('TFoo'),                     skClass,     'TFoo', []);

  AddFmtDef('ArgTRecSelf',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelf.ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelf.ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTRecSelf.ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', []);
  AddFmtDef('ArgTRecSelf.ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //AddFmtDef('ArgTRecSelf',       MatchRecord('TRecSelf', 'valint = 100'),skSimple,    'TRecSelf', []);
  //AddFmtDef('ArgTRecSelf',       MatchRecord('TRecSelf', 'valint = 100'),skSimple,    'TRecSelf', []);

  AddFmtDef('ArgTRecSelfDArray',  '.',                                   skSimple,    'TRecSelfDArray', []);
  //ArgTRecSelf
  AddFmtDef('ArgTRecSelfDArray[0]',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelfDArray[0].ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelfDArray[0].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTRecSelfDArray[0].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTRecSelfDArray[0].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('ArgTRecSelfDArray[1]',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelfDArray[1].ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelfDArray[1].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTRecSelfDArray[1].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTRecSelfDArray[1].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  AddFmtDef('ArgTRecSelfS0Array',  '.',                                   skSimple,    'TRecSelfS0Array', []);
  //ArgTRecSelf
  AddFmtDef('ArgTRecSelfS0Array[0]',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelfS0Array[0].ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelfS0Array[0].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTRecSelfS0Array[0].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTRecSelfS0Array[0].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('ArgTRecSelfS0Array[1]',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelfS0Array[1].ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelfS0Array[1].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTRecSelfS0Array[1].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTRecSelfS0Array[1].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  AddFmtDef('ArgTRecSelfS3Array',  '.',                                   skSimple,    'TRecSelfS3Array', []);
  //ArgTRecSelf
  AddFmtDef('ArgTRecSelfS3Array[3]',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelfS3Array[3].ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelfS3Array[3].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTRecSelfS3Array[3].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTRecSelfS3Array[3].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('ArgTRecSelfS3Array[4]',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('ArgTRecSelfS3Array[4].ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('ArgTRecSelfS3Array[4].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', [IgnDwrf3]);
  AddFmtDef('ArgTRecSelfS3Array[4].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', []);
  AddFmtDef('ArgTRecSelfS3Array[4].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  AddFmtDef('ArgTPRecSelfDArray',  '.',                                   skSimple,    'TPRecSelfDArray', []);
  //ArgTRecSelf
  AddFmtDef('ArgTPRecSelfDArray[0]^',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', [IgnDwrf3]);
  AddFmtDef('ArgTPRecSelfDArray[0]^.ValInt',   '100',                               skSimple,    M_Int, [fTpMtch, IgnDwrf3]);
  AddFmtDef('ArgTPRecSelfDArray[0]^.ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTPRecSelfDArray[0]^.ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTPRecSelfDArray[0]^.ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('ArgTPRecSelfDArray[1]^',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', [IgnDwrf3]);
  AddFmtDef('ArgTPRecSelfDArray[1]^.ValInt',   '102',                               skSimple,    M_Int, [fTpMtch, IgnDwrf3]);
  AddFmtDef('ArgTPRecSelfDArray[1]^.ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('ArgTPRecSelfDArray[1]^.ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('ArgTPRecSelfDArray[1]^.ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);



  // VAR param to FooFunc
  r := AddFmtDef('VArgTRec',           MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,    'TRec', []);
  r := AddFmtDef('VArgPRec',           MatchPointer('PRec'),                   skPointer,   'PRec', []);
  r := AddFmtDef('VArgPRec^',          MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  r := AddFmtDef('VArgPPRec',          MatchPointer('PPRec'),                  skPointer,   'PPRec', []);
  r := AddFmtDef('VArgPPRec^',         MatchPointer('PRec'),                   skPointer,   'PRec', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  r := AddFmtDef('VArgPPRec^^',        MatchRecord('TREC', 2, '.'),            skRecord,    'TRec', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  AddFmtDef('VArgTNewRec',        MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,    'T(New)?Rec', [fTpMtch]);
  AddFmtDef('VArgTRec.ValInt',    '-1',                                   skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgPRec^.ValInt',   '1',                                    skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgPPRec^^.ValInt', '2',                                    skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgPRec^.ValFoo',   MatchClass('TFoo'),                      skClass,     'TFoo', []);

  AddFmtDef('VArgTRecSelf',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelf.ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelf.ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelf.ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', []);
  AddFmtDef('VArgTRecSelf.ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  r := AddFmtDef('VArgTRecSelfDArray',  '.',                                   skSimple,    'TRecSelfDArray', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  //ArgTRecSelf
  r := AddFmtDef('VArgTRecSelfDArray[0]',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelfDArray[0].ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelfDArray[0].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelfDArray[0].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTRecSelfDArray[0].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  r := AddFmtDef('VArgTRecSelfDArray[1]',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelfDArray[1].ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelfDArray[1].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelfDArray[1].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTRecSelfDArray[1].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  AddFmtDef('VArgTRecSelfS0Array',  '.',                                   skSimple,    'TRecSelfS0Array', []);
  //ArgTRecSelf
  AddFmtDef('VArgTRecSelfS0Array[0]',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelfS0Array[0].ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelfS0Array[0].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelfS0Array[0].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTRecSelfS0Array[0].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('VArgTRecSelfS0Array[1]',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelfS0Array[1].ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelfS0Array[1].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelfS0Array[1].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTRecSelfS0Array[1].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  AddFmtDef('VArgTRecSelfS3Array',  '.',                                   skSimple,    'TRecSelfS3Array', []);
  //ArgTRecSelf
  AddFmtDef('VArgTRecSelfS3Array[3]',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelfS3Array[3].ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelfS3Array[3].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelfS3Array[3].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTRecSelfS3Array[3].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('VArgTRecSelfS3Array[4]',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', []);
  AddFmtDef('VArgTRecSelfS3Array[4].ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTRecSelfS3Array[4].ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTRecSelfS3Array[4].ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTRecSelfS3Array[4].ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);

  r := AddFmtDef('VArgTPRecSelfDArray',  '.',                                   skSimple,    'TPRecSelfDArray', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  //ArgTRecSelf
  AddFmtDef('VArgTPRecSelfDArray[0]^',       MatchRecord('TRecSelf', 'valint = 100'),skRecord,    'TRecSelf', [IgnDwrf3]);
  AddFmtDef('VArgTPRecSelfDArray[0]^.ValInt',   '100',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTPRecSelfDArray[0]^.ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTPRecSelfDArray[0]^.ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTPRecSelfDArray[0]^.ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);
  //VArgTRecSelf
  AddFmtDef('VArgTPRecSelfDArray[1]^',       MatchRecord('TRecSelf', 'valint = 102'),skRecord,    'TRecSelf', [IgnDwrf3]);
  AddFmtDef('VArgTPRecSelfDArray[1]^.ValInt',   '102',                               skSimple,    M_Int, [fTpMtch]);
  AddFmtDef('VArgTPRecSelfDArray[1]^.ValPrec',  MatchPointer('PRec'),                skPointer,   'PRec', []);
  AddFmtDef('VArgTPRecSelfDArray[1]^.ValPrec^', MatchRecord('TRec', 1),              skRecord,    'TRec', [IgnDwrf3]);
  AddFmtDef('VArgTPRecSelfDArray[1]^.ValPrec^.ValInt',  '1',                         skSimple,    M_Int, [fTpMtch]);


  // LOCAL var (global type)
  AddFmtDef('VarTRec',           MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,    'TRec', []);
  AddFmtDef('VarPRec',           MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  AddFmtDef('VarPRec^',          MatchRecord('TREC', 1, '.'),            skRecord,    'TRec', []);
  AddFmtDef('VarPPRec',          MatchPointer('^PPRec'),                 skPointer,   'PPRec', []);
  AddFmtDef('VarPPRec^',         MatchPointer('^PRec'),                  skPointer,   'PRec', []);
  AddFmtDef('VarPPRec^^',        MatchRecord('TREC', 2, '.'),            skRecord,    'TRec', []);
  AddFmtDef('VarTNewRec',        MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,    'T(New)?Rec', [fTpMtch]);
  AddFmtDef('VarTRec.ValInt',    '-1',                                   skSimple,    M_Int, [fTpMtch]);

  // LOCAL var (reference (^) to global type)
  AddFmtDef('PVarTRec',        MatchPointer('^(\^T|P)Rec'),            skPointer,  '^(\^T|P)Rec$', [fTpMtch]); // TODO: stabs returns PRec
  AddFmtDef('PVarTRec^',       MatchRecord('TREC', -1, '(\$0|nil)'),   skRecord,   'TRec', []);
  AddFmtDef('PVarTNewRec',     MatchPointer('^\^TNewRec'),             skPointer,  '\^T(New)?Rec', [fTpMtch]);
  AddFmtDef('PVarTNewRec^',    MatchRecord('T(NEW)?REC', 3, '.'),      skRecord,   'T(New)?Rec',   [fTpMtch]);

  // LOCAL var (local type)
  AddFmtDef('VarRecA',         MatchRecord('', 'val = 1'),             skRecord,    '', []);
  {%endregion    * records * }

  // @ArgTRec @VArgTRec  @ArgTRec^ @VArgTRec^

  {%region    * Classes * }

  AddFmtDef('ArgTFoo',      Match_ArgTFoo,                 skClass,   'TFoo',  []);
  AddFmtDef('@ArgTFoo',     '(P|\^T)Foo\('+Match_Pointer,  skPointer, '(P|\^T)Foo',  [fTpMtch]);
  // Only with brackets...
  AddFmtDef('(@ArgTFoo)^',   Match_ArgTFoo,                skClass,   'TFoo',  []);

  AddFmtDef('ArgPFoo',      'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
  AddFmtDef('ArgPFoo^',     Match_ArgTFoo1,                skClass,   'TFoo',  []);
  AddFmtDef('@ArgPFoo',     '(P|\^)PFoo\('+Match_Pointer,  skPointer, '(P|\^)PFoo',  [fTpMtch]);

  AddFmtDef('ArgPPFoo',     'PPFoo\('+Match_Pointer,       skPointer, 'PPFoo', []);
  AddFmtDef('ArgPPFoo^',    'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
  AddFmtDef('@ArgPPFoo',    '\^PPFoo\('+Match_Pointer,      skPointer, '^PPFoo', []);
  AddFmtDef('ArgPPFoo^^',   Match_ArgTFoo1,                skClass,   'TFoo',  []);


  AddFmtDef('VArgTFoo',     Match_ArgTFoo,                 skClass,   'TFoo',  []);
  AddFmtDef('@VArgTFoo',    '(P|\^T)Foo\('+Match_Pointer,  skPointer, '(P|\^T)Foo',  [fTpMtch]);
  AddFmtDef('(@VArgTFoo)^',   Match_ArgTFoo,                 skClass,   'TFoo',  []);

  r := AddFmtDef('VArgPFoo',     'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
  r := AddFmtDef('VArgPFoo^' ,   Match_ArgTFoo1,                skClass,   'TFoo',  []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  r := AddFmtDef('@VArgPFoo',    '(P|\^)PFoo\('+Match_Pointer,  skPointer, '(P|\^)PFoo',  [fTpMtch]);

  r := AddFmtDef('VArgPPFoo',    'PPFoo\('+Match_Pointer,       skPointer, 'PPFoo', []);
  r := AddFmtDef('VArgPPFoo^',   'PFoo\('+Match_Pointer,        skPointer, 'PFoo',  []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  r := AddFmtDef('@VArgPPFoo',   '\^PPFoo\('+Match_Pointer,      skPointer, '^PPFoo', []);
  r := AddFmtDef('VArgPPFoo^^',   Match_ArgTFoo1,               skClass,   'TFoo',  []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);


  AddFmtDef('ArgTFoo.ValueInt',       '^-11$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  AddFmtDef('ArgPFoo^.ValueInt',      '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  // GDB automatically derefs the pointer
  //AddFmtDef('ArgPFoo.ValueInt',       'error',            skSimple,   '',  []);
  AddFmtDef('ArgPPFoo^^.ValueInt',    '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  //AddFmtDef('ArgPPFoo.ValueInt',       'error',            skSimple,   '',  []);

  AddFmtDef('VArgTFoo.ValueInt',      '^-11$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  AddFmtDef('VArgPFoo^.ValueInt',     '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  //AddFmtDef('VArgPFoo.ValueInt',      'error',            skSimple,   '',  []);
  AddFmtDef('VArgPPFoo^^.ValueInt',   '^31$',             skSimple,   'Integer|LongInt',  [fTpMtch]);
  //AddFmtDef('VArgPPFoo.ValueInt',      'error',            skSimple,   '',  []);


  Add('ArgTFoo',    wdfPointer,  Match_Pointer,                           skClass,   'TFoo',  []);
  Add('ArgTFoo',    wdfMemDump,  ':.*?6D 65 6D 20 6F 66 20 54 46 6F 6F',  skClass,   'TFoo',  []);

  (*

  AddFmtDef('ArgTSamePFoo',        '',      sk,      'TSamePFoo', []);
  AddFmtDef('VArgTSamePFoo',        '',      sk,      'TSamePFoo', []);
  AddFmtDef('ArgTNewPFoo',        '',      sk,      'TNewPFoo', []);
  AddFmtDef('VArgTNewPFoo',        '',      sk,      'TNewPFoo', []);

  AddFmtDef('ArgTSameFoo',        '',      sk,      'TSameFoo', []);
  AddFmtDef('VArgTSameFoo',        '',      sk,      'TSameFoo', []);
  AddFmtDef('ArgTNewFoo',        '',      sk,      'TNewFoo', []);
  AddFmtDef('VArgTNewFoo',        '',      sk,      'TNewFoo', []);
  AddFmtDef('ArgPNewFoo',        '',      sk,      'PNewFoo', []);
  AddFmtDef('VArgPNewFoo',        '',      sk,      'PNewFoo', []);

    { ClassesTyps }
  AddFmtDef('ArgTFooClass',        '',      sk,      'TFooClass', []);
  AddFmtDef('VArgTFooClass',        '',      sk,      'TFooClass', []);
  AddFmtDef('ArgPFooClass',        '',      sk,      'PFooClass', []);
  AddFmtDef('VArgPFooClass',        '',      sk,      'PFooClass', []);
  AddFmtDef('ArgPPFooClass',        '',      sk,      'PPFooClass', []);
  AddFmtDef('VArgPPFooClass',        '',      sk,      'PPFooClass', []);
  AddFmtDef('ArgTNewFooClass',        '',      sk,      'TNewFooClass', []);
  AddFmtDef('VArgTNewFooClass',        '',      sk,      'TNewFooClass', []);
  AddFmtDef('ArgPNewFooClass',        '',      sk,      'PNewFooClass', []);
  AddFmtDef('VArgPNewFooClass',        '',      sk,      'PNewFooClass', []);
  *)

  { Compare Objects }
  // TODO: not working in Dwarf3
  AddFmtDef('ArgTFoo=ArgTFoo',        'True',         skSimple,      'bool', []);
  AddFmtDef('not(ArgTFoo=ArgTFoo)',   'False',        skSimple,      'bool', []);
  AddFmtDef('VArgTFoo=VArgTFoo',      'True',         skSimple,      'bool', []);
  AddFmtDef('ArgTFoo=VArgTFoo',       'True',         skSimple,      'bool', []);
  AddFmtDef('ArgTFoo=ArgPFoo',        'False',        skSimple,      'bool', []);
  AddFmtDef('ArgTFoo=ArgPFoo^',       'False',        skSimple,      'bool', []);
  AddFmtDef('ArgPFoo=ArgPPFoo^',      'True',         skSimple,      'bool', []);

  AddFmtDef('@ArgTFoo=PVarTFoo',      'True',         skSimple,      'bool', []);
  AddFmtDef('@VArgTFoo=PVarTFoo',     'False',        skSimple,      'bool', []);

  //AddFmtDef('ArgTFoo<>ArgTFoo',       'False',        skSimple,      'bool', []);
  //AddFmtDef('ArgTFoo<>ArgPFoo^',      'True',         skSimple,      'bool', []);

  AddFmtDef('ArgTFoo=0',            'False',          skSimple,      'bool', []);
  AddFmtDef('not(ArgTFoo=0)',       'True',           skSimple,      'bool', []);
  //AddFmtDef('ArgTFoo<>0',           'True',           skSimple,      'bool', []);

  //AddFmtDef('ArgTFoo=nil',            'False',        skSimple,      'bool', []);
  //AddFmtDef('not(ArgTFoo=nil)',       'True',         skSimple,      'bool', []);
  //AddFmtDef('ArgTFoo<>nil',           'True',         skSimple,      'bool', []);

  {%endregion    * Classes * }

  {%region    * Strings * }
    { strings }
    // todo: some skPOINTER should be skSimple
  // ArgAnsiString
  r:=AddStringFmtDef('ArgAnsiString',          'Ansi',         'AnsiString', []);
  r:=AddStringFmtDef('VArgAnsiString',         'Ansi 2',       'AnsiString', []);
  r:=AddStringFmtDef('ArgTMyAnsiString',       'MyAnsi',       '^(TMy)?AnsiString$', [fTpMtch]);
  r:=AddStringFmtDef('VArgTMyAnsiString',      'MyAnsi 2',     '^(TMy)?AnsiString$', [fTpMtch]);

  r:=AddFmtDef('ArgPMyAnsiString',     MatchPointer, skPointer,     'PMyAnsiString', []);
     UpdRes(r, stStabs,                                             '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=AddFmtDef('VArgPMyAnsiString',    MatchPointer,  skPointer,    'PMyAnsiString', []);
     UpdRes(r, stStabs,                                             '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=AddStringFmtDef('ArgPMyAnsiString^',      'MyAnsi P',     '^(TMy)?AnsiString$', [fTpMtch]);
  r:=AddStringFmtDef('VArgPMyAnsiString^',     'MyAnsi P2',    '^(TMy)?AnsiString$', [fTpMtch]);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);

  r:=AddFmtDef('ArgPPMyAnsiString',    MatchPointer,  skPointer,    'PPMyAnsiString', []);
  r:=AddFmtDef('VArgPPMyAnsiString',   MatchPointer,  skPointer,    'PPMyAnsiString', []);
  r:=AddFmtDef('ArgPPMyAnsiString^',   MatchPointer,  skPointer,    'PMyAnsiString', []);
     UpdRes(r, stStabs,                                             '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=AddFmtDef('VArgPPMyAnsiString^',  MatchPointer,  skPointer,    'PMyAnsiString', []);
     UpdRes(r, stStabs,                                             '^(PMyAnsiString|PPChar)$', [fTpMtch]);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
  r:=AddStringFmtDef('ArgPPMyAnsiString^^',    'MyAnsi P',     '^(TMy)?AnsiString$', [fTpMtch]);
  r:=AddStringFmtDef('VArgPPMyAnsiString^^',   'MyAnsi P2',    '^(TMy)?AnsiString$', [fTpMtch]);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);


  r:=AddStringFmtDef('ArgTNewAnsiString',      'NewAnsi',      'TNewAnsiString', []);
     UpdRes(r, stStabs,                                             '(TNew)?AnsiString', [fTpMtch]);
  r:=AddStringFmtDef('VArgTNewAnsiString',     'NewAnsi 2',    'TNewAnsiString', []);
     UpdRes(r, stStabs,                                             '(TNew)?AnsiString', [fTpMtch]);

  r:=AddFmtDef('ArgPNewAnsiString',    MatchPointer,  skPointer,    'PNewAnsiString', []);
                    UpdRes(r, stStabs,                              '(\^|PNew|P)AnsiString|PPChar', [fTpMtch]);
  r:=AddFmtDef('VArgPNewAnsiString',   MatchPointer,  skPointer,    'PNewAnsiString', []);
                    UpdRes(r, stStabs,                              '(\^|PNew|P)AnsiString|PPChar', [fTpMtch]);
  r:=AddStringFmtDef('ArgPNewAnsiString^',      'NewAnsi P',   'TNewAnsiString', []);
     UpdRes(r, stStabs,                                             '(TNew)?AnsiString', [fTpMtch]);
  r:=AddStringFmtDef('VArgPNewAnsiString^',     'NewAnsi P2',  'TNewAnsiString', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
     UpdRes(r, stStabs,                                             '(TNew)?AnsiString', [fTpMtch]);


  // typecasts
  r:=AddStringFmtDef('AnsiString(ArgTMyAnsiString)',   'MyAnsi',      'AnsiString|\^char', [fTpMtch]);
     UpdRes(r, stDwarf3,                                                   'AnsiString', []);
  r:=AddStringFmtDef('AnsiString(VArgTMyAnsiString)',  'MyAnsi 2',    'AnsiString|\^char', [fTpMtch]);
     UpdRes(r, stDwarf3,                                                   'AnsiString', []);

  r:=AddFmtDef('PMyAnsiString(ArgPMyAnsiString)',     MatchPointer, skPointer,   '^(\^|PMy)AnsiString$', [fTpMtch]);
     UpdRes(r, stStabs,                                                          '^(PMyAnsiString|PPChar)$', [fTpMtch]);
  r:=AddFmtDef('PMyAnsiString(VArgPMyAnsiString)',    MatchPointer,  skPointer,  '^(\^|PMy)AnsiString$', [fTpMtch]);
     UpdRes(r, stStabs,                                                          '^(PMyAnsiString|PPChar)$', [fTpMtch]);
// TODO,, IDE derefs with dwarf3
  r:=AddFmtDef('^AnsiString(ArgPMyAnsiString)',       MatchPointer, skPointer,   '^(\^AnsiString|\^\^char)', [fTpMtch]);
     UpdRes(r, stStabs,                                                          '^(\^AnsiString|PPChar)$', [fTpMtch]);
  r:=AddFmtDef('^AnsiString(VArgPMyAnsiString)',      MatchPointer,  skPointer,  '^(\^AnsiString|\^\^char)', [fTpMtch]);
     UpdRes(r, stStabs,                                                          '^(\^AnsiString|PPChar)$', [fTpMtch]);

  r:=AddStringFmtDef('AnsiString(ArgPMyAnsiString^)',  'MyAnsi P',     '^((TMy)?AnsiString|\^char)$', [fTpMtch]);
  r:=AddStringFmtDef('AnsiString(VArgPMyAnsiString^)', 'MyAnsi P2',    '^((TMy)?AnsiString|\^char)$', [fTpMtch]);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);  // ^char => gdb 6.7.5 with dwarf
  r:=AddStringFmtDef('PMyAnsiString(ArgPMyAnsiString)^',  'MyAnsi P',  '^(TMy)?AnsiString$', [fTpMtch]);
  r:=AddStringFmtDef('PMyAnsiString(VArgPMyAnsiString)^', 'MyAnsi P2', '^(TMy)?AnsiString$', [fTpMtch]);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);


  r:=AddFmtDef('PChar(ArgTMyAnsiString)',
                                               '''MyAnsi''$',      skPOINTER,   '(\^|p)char', [fTpMtch]);
                    UpdRes(r, stStabs,    '''MyAnsi''$',      skPOINTER,   'pchar|AnsiString', [fTpMtch]);
                         //UpdRes(r, stDwarf3,   '''MyAnsi''$',      skSimple,    'AnsiString', []);

  // accessing len/refcount
  r:=AddFmtDef('^longint(ArgTMyAnsiString)[-1]',
                                               '6',      skSimple,   'longint', []);
  r:=AddFmtDef('^longint(VArgTMyAnsiString)[-1]',
                                               '8',      skSimple,   'longint', []);

  // accessing char
  // TODO: only works with dwarf 3
  r:=AddFmtDef('ArgTMyAnsiString[1]',      '.',      skSimple,   'char', []);
                    UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=AddFmtDef('VArgTMyAnsiString[1]',     '.',      skSimple,   'char', []);
                    UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=AddFmtDef('ArgPMyAnsiString^[1]',     '.',      skSimple,   'char', []);
                    UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=AddFmtDef('VArgPMyAnsiString^[1]',    '.',      skSimple,   'char', []);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);
     UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=AddFmtDef('AnsiString(ArgTMyAnsiString)[1]',      '.',      skSimple,   'char', []);
                    UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);
  r:=AddFmtDef('AnsiString(VArgTMyAnsiString)[1]',     '.',      skSimple,   'char', []);
                    UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);

  // accessing char, after typecast
  r:=AddFmtDef('AnsiString(ArgTMyAnsiString)[1]',      '.',      skSimple,   'char', []);
                    UpdRes(r, stDwarf3,    '''M''$', skSimple,   'char', []);


  // string in array
  r:=AddStringFmtDef('ArgTMyAnsiStringDArray[0]',   'DArray1 Str0',         'AnsiString', []);
  r:=AddStringFmtDef('ArgTMyAnsiStringDArray[1]',   'DArray1 Str1',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyAnsiStringDArray[0]',  'DArray2 Str0',         'AnsiString', []);
  if (DebuggerInfo.Version > 0) and (DebuggerInfo.Version < 070000) then UpdRes(r, stDwarf2All, '^(\^Char|AnsiString)$', [fTpMtch]);
  r:=AddStringFmtDef('VArgTMyAnsiStringDArray[1]',  'DArray2 Str1',         'AnsiString', []);
  if (DebuggerInfo.Version > 0) and (DebuggerInfo.Version < 070000) then UpdRes(r, stDwarf2All, '^(\^Char|AnsiString)$', [fTpMtch]);


  r:=AddCharFmtDef('ArgTMyAnsiStringDArray[0][1]',  'D', 'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyAnsiStringDArray[0][12]', '0',   'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyAnsiStringDArray[1][1]',  'D',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyAnsiStringDArray[1][12]', '1',    'char', [IgnDwrf2, IgnStabs]);

  r:=AddCharFmtDef('VArgTMyAnsiStringDArray[0][1]',   'D',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyAnsiStringDArray[0][12]',  '0',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyAnsiStringDArray[1][1]',   'D',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyAnsiStringDArray[1][12]',  '1',    'char', [IgnDwrf2, IgnStabs]);

  r:=AddStringFmtDef('ArgTMyAnsiStringSArray[3]',   'SArray1 Str3',         'AnsiString', []);
  r:=AddStringFmtDef('ArgTMyAnsiStringSArray[4]',   'SArray1 Str4',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyAnsiStringSArray[3]',  'SArray2 Str3',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyAnsiStringSArray[4]',  'SArray2 Str4',         'AnsiString', []);

  r:=AddCharFmtDef('ArgTMyAnsiStringSArray[3][1]',   'S',    'char', [IgnDataDw, IgnDataSt]);
  r:=AddCharFmtDef('ArgTMyAnsiStringSArray[3][12]',  '0',    'char', [IgnDataDw, IgnDataSt]);
  r:=AddCharFmtDef('ArgTMyAnsiStringSArray[4][1]',   'S',    'char', [IgnDataDw, IgnDataSt]);
  r:=AddCharFmtDef('ArgTMyAnsiStringSArray[4][12]',  '1',    'char', [IgnDataDw, IgnDataSt]);

  // string in array // no typename for array
  r:=AddStringFmtDef('GlobAMyAnsiStringDArray[0]',   'ADArray1 Str0',         'AnsiString', []);
  r:=AddStringFmtDef('GlobAMyAnsiStringDArray[1]',   'ADArray1 Str1',         'AnsiString', []);

  r:=AddCharFmtDef('GlobAMyAnsiStringDArray[0][1]',   'A',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('GlobAMyAnsiStringDArray[0][13]',  '0',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('GlobAMyAnsiStringDArray[1][1]',   'A',    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('GlobAMyAnsiStringDArray[1][13]',  '1',    'char', [IgnDwrf2, IgnStabs]);

  // PAnsiString in array
  r:=AddPointerFmtDefRaw('ArgTMyPAnsiStringDArray[0]',   MatchPointer(),     '^(\^|P)(AnsiString|PChar)$', [fTpMtch]);
  r:=AddPointerFmtDefRaw('ArgTMyPAnsiStringDArray[1]',   MatchPointer(),     '^(\^|P)(AnsiString|PChar)$', [fTpMtch]);
  r:=AddStringFmtDef('ArgTMyPAnsiStringDArray[0]^',   'DArray1 Str0',         'AnsiString', []);
  r:=AddStringFmtDef('ArgTMyPAnsiStringDArray[1]^',   'DArray1 Str1',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyPAnsiStringDArray[0]^',  'DArray2 Str0',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyPAnsiStringDArray[1]^',  'DArray2 Str1',         'AnsiString', []);


  r:=AddCharFmtDef('ArgTMyPAnsiStringDArray[0][1]^',   'D'  ,    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyPAnsiStringDArray[0][12]^',  '0'  ,    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyPAnsiStringDArray[1][1]^',   'D'  ,    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyPAnsiStringDArray[1][12]^',  '1'  ,    'char', [IgnDwrf2, IgnStabs]);

  r:=AddCharFmtDef('VArgTMyPAnsiStringDArray[0]^[1]',  'D'  ,    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyPAnsiStringDArray[0]^[12]', '0'  ,    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyPAnsiStringDArray[1]^[1]',  'D'  ,    'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyPAnsiStringDArray[1]^[12]', '1'  ,    'char', [IgnDwrf2, IgnStabs]);

  r:=AddStringFmtDef('ArgTMyPAnsiStringSArray[3]^',   'SArray1 Str3',         'AnsiString', []);
  r:=AddStringFmtDef('ArgTMyPAnsiStringSArray[4]^',   'SArray1 Str4',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyPAnsiStringSArray[3]^',  'SArray2 Str3',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTMyPAnsiStringSArray[4]^',  'SArray2 Str4',         'AnsiString', []);

  r:=AddCharFmtDef('ArgTMyPAnsiStringSArray[3]^[1]',   'S',    'char', [IgnDataDw, IgnDataSt]);
  r:=AddCharFmtDef('ArgTMyPAnsiStringSArray[3]^[12]',  '0',    'char', [IgnDataDw, IgnDataSt]);
  r:=AddCharFmtDef('ArgTMyPAnsiStringSArray[4]^[1]',   'S',    'char', [IgnDataDw, IgnDataSt]);
  r:=AddCharFmtDef('ArgTMyPAnsiStringSArray[4]^[12]',  '1',    'char', [IgnDataDw, IgnDataSt]);


  // string in obj
  r:=AddStringFmtDef('ArgTStringHolderObj.FTMyAnsiString',   'Obj1 MyAnsi',         'AnsiString', []);
  r:=AddStringFmtDef('VArgTStringHolderObj.FTMyAnsiString',  'Obj2 MyAnsi',         'AnsiString', []);

  r:=AddFmtDef('ArgTStringHolderObj.FTMyAnsiString[1]',   '.$',  skSimple,    'char', []);
     UpdRes(r, stDwarf3,                         '''O''$', skSimple);
  r:=AddFmtDef('VArgTStringHolderObj.FTMyAnsiString[1]',   '.$',  skSimple,    'char', []);
     UpdRes(r, stDwarf3,                         '''O''$', skSimple);

  // string in rec
  r:=AddStringFmtDef('ArgTStringHolderRec.FTMyAnsiString',   'Rec1 MyAnsi',         'AnsiString', [fTstSkipDwarf3]);
  r:=AddStringFmtDef('VArgTStringHolderRec.FTMyAnsiString',  'Rec2 MyAnsi',         'AnsiString', [fTstSkipDwarf3]);

  r:=AddFmtDef('ArgTStringHolderRec.FTMyAnsiString[1]',   '.$',  skSimple,    'char', [fTstSkipDwarf3]);
     UpdRes(r, stDwarf3,                         '''R''$', skSimple);
  r:=AddFmtDef('VArgTStringHolderRec.FTMyAnsiString[1]',   '.$',  skSimple,    'char', [fTstSkipDwarf3]);
     UpdRes(r, stDwarf3,                         '''R''$', skSimple);


  //r:=AddFmtDef('ArgTNewAnsiString',       '''NewAnsi''$',     skPOINTER,   '(TNew)?AnsiString', []);
  //                  UpdRes(r, stDwarf3,   '''NewAnsi''$',     skSimple,    '(TNew)?AnsiString', [fTpMtch]);
  //r:=AddFmtDef('VArgTNewAnsiString',      '''NewAnsi 2''$',   skPOINTER,   '(TNew)?AnsiString', []);
  //                  UpdRes(r, stDwarf3,   '''NewAnsi 2''$',   skSimple,    '(TNew)?AnsiString', [fTpMtch]);
  //r:=AddFmtDef('ArgPNewAnsiString',       MatchPointer,       skPointer,   '(\^|PNew|P)AnsiString', []);
  //r:=AddFmtDef('VArgPNewAnsiString',      MatchPointer,       skPointer,   '(\^|PNew|P)AnsiString', []);
  //r:=AddFmtDef('ArgPNewAnsiString^',      '''NewAnsi P''',    skPOINTER,   '(TNew)?AnsiString', []);
  //                  UpdRes(r, stDwarf3,   '''NewAnsi''$',     skSimple,    '(TNew)?AnsiString', [fTpMtch]);
  //r:=AddFmtDef('VArgPNewAnsiString^',     '''NewAnsi P2''',   skPOINTER,   '(TNew)?AnsiString', []);
  //                  UpdRes(r, stDwarf3,   '''NewAnsi 2''$',   skSimple,    '(TNew)?AnsiString', [fTpMtch]);



  AddFmtDef('ArgTMyShortString',        '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
  AddFmtDef('VArgTMyShortString',       '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
  AddFmtDef('ArgPMyShortString',        Match_Pointer,      skPointer,     'P(My)?ShortString', [fTpMtch]);
  AddFmtDef('VArgPMyShortString',       Match_Pointer,      skPointer,     'P(My)?ShortString', [fTpMtch]);
  AddFmtDef('ArgPMyShortString^',        '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
  r := AddFmtDef('VArgPMyShortString^',       '''short''$',        skSimple,      '^(TMy)?ShortString$', [fTpMtch]);
     UpdResMinFpc(r, stDwarf, 020600); UpdResMinFpc(r, stDwarfSet, 020600);

  // string in array
  r:=AddShortStrFmtDef('ArgTMyShortStringDArray[0]',   'DArray1 Short0',         'ShortString', []);
  r:=AddShortStrFmtDef('ArgTMyShortStringDArray[1]',   'DArray1 Short1',         'ShortString', []);
  r:=AddShortStrFmtDef('VArgTMyShortStringDArray[0]',  'DArray2 Short0',         'ShortString', []);
     UpdResMinFpc(r, stDwarf, 020600);
     UpdResMinFpc(r, stDwarfSet, 020600);
  r:=AddShortStrFmtDef('VArgTMyShortStringDArray[1]',  'DArray2 Short1',         'ShortString', []);
     UpdResMinFpc(r, stDwarf, 020600);
     UpdResMinFpc(r, stDwarfSet, 020600);


  r:=AddCharFmtDef('ArgTMyShortStringDArray[0][1]',   'D',      'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyShortStringDArray[0][14]',  '0',      'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyShortStringDArray[1][1]',   'D',      'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('ArgTMyShortStringDArray[1][14]',  '1',      'char', [IgnDwrf2, IgnStabs]);

  r:=AddCharFmtDef('VArgTMyShortStringDArray[0][1]',   'D',      'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyShortStringDArray[0][14]',  '0',      'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyShortStringDArray[1][1]',   'D',      'char', [IgnDwrf2, IgnStabs]);
  r:=AddCharFmtDef('VArgTMyShortStringDArray[1][14]',  '1',      'char', [IgnDwrf2, IgnStabs]);

  r:=AddShortStrFmtDef('ArgTMyShortStringSArray[3]',   'SArray1 Short3',         'ShortString', []);
  r:=AddShortStrFmtDef('ArgTMyShortStringSArray[4]',   'SArray1 Short4',         'ShortString', []);
  r:=AddShortStrFmtDef('VArgTMyShortStringSArray[3]',  'SArray2 Short3',         'ShortString', []);
  r:=AddShortStrFmtDef('VArgTMyShortStringSArray[4]',  'SArray2 Short4',         'ShortString', []);

  r:=AddCharFmtDef('ArgTMyShortStringSArray[3][1]',   'S',      'char', [IgnDwrf2]);
  r:=AddCharFmtDef('ArgTMyShortStringSArray[3][14]',  '3',      'char', [IgnDwrf2]);
  r:=AddCharFmtDef('ArgTMyShortStringSArray[4][1]',   'S',      'char', [IgnDwrf2]);
  r:=AddCharFmtDef('ArgTMyShortStringSArray[4][14]',  '4',      'char', [IgnDwrf2]);

  // string in obj
  r:=AddFmtDef('ArgTStringHolderObj.FTMyShortString',   '''Obj1 Short''$',  skSimple,     '^(TMy)?ShortString$', [fTpMtch, IgnDwrf3]);
  r:=AddFmtDef('VArgTStringHolderObj.FTMyShortString',  '''Obj2 Short''$',  skSimple,     '^(TMy)?ShortString$', [fTpMtch, IgnDwrf3]);

  // string in rec
  r:=AddFmtDef('ArgTStringHolderRec.FTMyShortString',   '''Rec1 Short''$',  skSimple,     '^(TMy)?ShortString$', [fTpMtch, IgnDwrf3]);
  r:=AddFmtDef('VArgTStringHolderRec.FTMyShortString',  '''Rec2 Short''$',  skSimple,     '^(TMy)?ShortString$', [fTpMtch, IgnDwrf3]);


  (*
  AddFmtDef('ArgPPMyShortString',        '',      sk,      'PPMyShortString', []);
  AddFmtDef('VArgPPMyShortString',        '',      sk,      'PPMyShortString', []);
  AddFmtDef('ArgTNewhortString',        '',      sk,      'TNewhortString', []);
  AddFmtDef('VArgTNewhortString',        '',      sk,      'TNewhortString', []);
  AddFmtDef('ArgPNewhortString',        '',      sk,      'PNewhortString', []);
  AddFmtDef('VArgPNewhortString',        '',      sk,      'PNewhortString', []);
  *)

  // gdb 6.7.5 does not show the text
  AddFmtDef('ArgTMyWideString',        '(''wide''$)|(widestring\(\$.*\))',      skPointer,      '^(TMy)?WideString$', [fTpMtch]);
  AddFmtDef('VArgTMyWideString',       '(''wide''$)|(widestring\(\$.*\))',      skPointer,      '^(TMy)?WideString$', [fTpMtch]);
  (*
  AddFmtDef('ArgPMyWideString',        '',      sk,      'PMyWideString', []);
  AddFmtDef('VArgPMyWideString',        '',      sk,      'PMyWideString', []);
  AddFmtDef('ArgPPMyWideString',        '',      sk,      'PPMyWideString', []);
  AddFmtDef('VArgPPMyWideString',        '',      sk,      'PPMyWideString', []);

  AddFmtDef('ArgTNewWideString',        '',      sk,      'TNewWideString', []);
  AddFmtDef('VArgTNewWideString',        '',      sk,      'TNewWideString', []);
  AddFmtDef('ArgPNewWideString',        '',      sk,      'PNewWideString', []);
  AddFmtDef('VArgPNewWideString',        '',      sk,      'PNewWideString', []);

  AddFmtDef('ArgTMyString10',        '',      sk,      'TMyString10', []);
  AddFmtDef('VArgTMyString10',        '',      sk,      'TMyString10', []);
  AddFmtDef('ArgPMyString10',        '',      sk,      'PMyString10', []);
  AddFmtDef('VArgPMyString10',        '',      sk,      'PMyString10', []);
  AddFmtDef('ArgPPMyString10',        '',      sk,      'PPMyString10', []);
  AddFmtDef('VArgPPMyString10',        '',      sk,      'PPMyString10', []);
  *)


  Add('ArgTMyAnsiString',      wdfMemDump,  ': 4d 79 41 6e 73 69 00',      skPOINTER,      '^(TMy)?AnsiString$', [fTpMtch]);

  {%endregion    * Strings * }

  {%region    * Simple * }

  AddFmtDef('ArgByte',        '^25$',      skSimple,      'Byte', []);
  AddFmtDef('VArgByte',       '^25$',      skSimple,      'Byte', []);
  AddFmtDef('ArgWord',        '^26$',      skSimple,      'Word', []);
  AddFmtDef('VArgWord',       '^26$',      skSimple,      'Word', []);
  AddFmtDef('ArgLongWord',    '^27$',      skSimple,      'LongWord', []);
  AddFmtDef('VArgLongWord',   '^27$',      skSimple,      'LongWord', []);
  AddFmtDef('ArgQWord',       '^28$',      skSimple,      'QWord', []);
  AddFmtDef('VArgQWord',      '^28$',      skSimple,      'QWord', []);

  AddFmtDef('ArgShortInt',    '^35$',      skSimple,      'ShortInt', []);
  AddFmtDef('VArgShortInt',   '^35$',      skSimple,      'ShortInt', []);
  AddFmtDef('ArgSmallInt',    '^36$',      skSimple,      'SmallInt', []);
  AddFmtDef('VArgSmallInt',   '^36$',      skSimple,      'SmallInt', []);
  AddFmtDef('ArgInt',         '^37$',      skSimple,      'Integer|LongInt', [fTpMtch]);
  AddFmtDef('VArgInt',        '^37$',      skSimple,      'Integer|LongInt', [fTpMtch]);
  AddFmtDef('ArgInt64',       '^38$',      skSimple,      'Int64', []);
  AddFmtDef('VArgInt64',      '^38$',      skSimple,      'Int64', []);

  AddFmtDef('ArgPointer',        Match_Pointer,      skPointer,      'Pointer', []);
  AddFmtDef('VArgPointer',       Match_Pointer,      skPointer,      'Pointer', []);
  (*
  AddFmtDef('ArgPPointer',        '',      sk,      'PPointer', []);
  AddFmtDef('VArgPPointer',        '',      sk,      'PPointer', []);
  *)

  AddFmtDef('ArgDouble',         '1\.123',      skSimple,      'Double', []);
  AddFmtDef('VArgDouble',        '1\.123',      skSimple,      'Double', []);
  AddFmtDef('ArgExtended',       '2\.345',      skSimple,      'Extended|double', [fTpMtch]);
  AddFmtDef('VArgExtended',      '2\.345',      skSimple,      'Extended|double', [fTpMtch]);

  (*
  AddFmtDef('ArgPByte',        '',      sk,      'PByte', []);
  AddFmtDef('VArgPByte',        '',      sk,      'PByte', []);
  AddFmtDef('ArgPWord',        '',      sk,      'PWord', []);
  AddFmtDef('VArgPWord',        '',      sk,      'PWord', []);
  AddFmtDef('ArgPLongWord',        '',      sk,      'PLongWord', []);
  AddFmtDef('VArgPLongWord',        '',      sk,      'PLongWord', []);
  AddFmtDef('ArgPQWord',        '',      sk,      'PQWord', []);
  AddFmtDef('VArgPQWord',        '',      sk,      'PQWord', []);

  AddFmtDef('ArgPShortInt',        '',      sk,      'PShortInt', []);
  AddFmtDef('VArgPShortInt',        '',      sk,      'PShortInt', []);
  AddFmtDef('ArgPSmallInt',        '',      sk,      'PSmallInt', []);
  AddFmtDef('VArgPSmallInt',        '',      sk,      'PSmallInt', []);
  AddFmtDef('ArgPInt',        '',      sk,      'PInteger', []);
  AddFmtDef('VArgPInt',        '',      sk,      'PInteger', []);
  AddFmtDef('ArgPInt64',        '',      sk,      'PInt64', []);
  AddFmtDef('VArgPInt64',        '',      sk,      'PInt64', []);
  *)
  {%endregion    * Simple * }

  {%region    * Enum/Set * }

  AddFmtDef('ArgEnum',         '^Two$',                      skEnum,       'TEnum', []);
  AddFmtDef('ArgEnumSet',      '^\[Two(, ?|\.\.)Three\]$',   skSet,        'TEnumSet', [IgnDwrf2IfNoSet]);
  AddFmtDef('ArgSet',          '^\[Alpha(, ?|\.\.)Beta\]$',  skSet,        'TSet', [IgnDwrf2IfNoSet]);

  AddFmtDef('VarEnumA',        '^e3$',                       skEnum,       '', []);
  // maybe typename = "set of TEnum"
  AddFmtDef('VarEnumSetA',     '^\[Three\]$',                skSet,        '', [IgnDwrf2IfNoSet]);
  AddFmtDef('VarSetA',         '^\[s2\]$',                   skSet,        '', [IgnDwrf2IfNoSet]);
  {%endregion    * Enum/Set * }

  {%region    * Variant * }

  AddFmtDef('ArgVariantInt',         '^5$',         skVariant,       'Variant', []);
  AddFmtDef('ArgVariantString',      '^''v''$',     skVariant,       'Variant', []);

  AddFmtDef('VArgVariantInt',         '^5$',         skVariant,       'Variant', []);
  AddFmtDef('VArgVariantString',      '^''v''$',     skVariant,       'Variant', []);
  {%endregion    * Variant * }

  {%region    * procedure/function/method * }

  AddFmtDef('ArgProcedure',         'procedure',           skProcedure,       'TProcedure', []);
  AddFmtDef('ArgFunction',          'function',            skFunction,        'TFunction',  []);
  (*
  // normal procedure on stabs / recodr on dwarf => maybe the data itself may reveal some ?
  AddFmtDef('ArgObjProcedure',      'procedure.*of object|record.*procedure.*self =',
                                                           skRecord,          'TObjProcedure', []);
  AddFmtDef('ArgObjFunction',       'function.*of object|record.*function.*self =',
                                                           skRecord,          'TObjFunction',  []);

  *)
  // doesn't work, ptype returns empty in dwarf => maybe via whatis
  //    AddFmtDef('VArgProcedure',         'procedure',           skProcedure,       'TProcedure', []);
  //    AddFmtDef('VArgFunction',          'function',            skFunction,        'TFunction',  []);
  (*
  AddFmtDef('VArgObjProcedure',      'procedure.*of object|record.*procedure.*self =',
                                                            skRecord,          'TObjProcedure', []);
  AddFmtDef('VArgObjFunction',       'function.*of object|record.*function.*self =',
                                                            skRecord,          'TObjFunction',  []);
  *)

  AddFmtDef('VarProcedureA',         'procedure',           skProcedure,       'Procedure', []);
  AddFmtDef('VarFunctionA',          'function',            skFunction,        'Function',  []);
  (*
  AddFmtDef('VarObjProcedureA',      'procedure.*of object|record.*procedure.*self =',
                                                            skRecord,          'Procedure', []);
  AddFmtDef('VarObjFunctionA',       'function.*of object|record.*function.*self =',
                                                            skRecord,          'Function',  []);
  *)
  {%endregion    * procedure/function/method * }

  if RUN_TEST_ONLY > 0 then begin
    ExpectBreakFoo[0] := ExpectBreakFoo[abs(RUN_TEST_ONLY)];
    SetLength(ExpectBreakFoo, 1);
  end;
end;

procedure TTestWatches.AddExpectBreakFooArray;

  function AddRecForArrFmtDef  (AnExpr: string; ARecSuffix, AValue: Integer;  AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
  begin
    case ARecSuffix of
      1: Result := Add(AnExpr, wdfDefault, MatchRecord('TRecForArray1', ' a = '+IntToStr(AValue)), skRecord, 'TRecForArray1', AFlgs );
      2: Result := Add(AnExpr, wdfDefault, MatchRecord('TRecForArray2', ' c = '+IntToStr(AValue)), skRecord, 'TRecForArray2', AFlgs );
      3: Result := Add(AnExpr, wdfDefault, MatchRecord('TRecForArray3', ' a = '+IntToStr(AValue)), skRecord, 'TRecForArray3', AFlgs );
      4: Result := Add(AnExpr, wdfDefault, MatchRecord('TRecForArray4', ' c = '+IntToStr(AValue)), skRecord, 'TRecForArray4', AFlgs );
    end;
  end;

  function AddArrayFmtDef  (AnExpr, AMtch, ATpNm: string; AFlgs: TWatchExpectationFlags=[]): PWatchExpectation;
  begin
    Result := Add(AnExpr, wdfDefault, AMtch, skSimple, ATpNm, AFlgs );
  end;

var
  r: PWatchExpectation;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.All')] then exit;
  FCurrentExpArray := @ExpectBreakFooArray;

  {%region    * Array * }
  //TODO: DynArray, decide what to display
  // TODO {} fixup array => replace with []
  AddFmtDef('VarDynIntArray',             Match_Pointer+'|\{\}|0,[\s\r\n]+2',
                                skSimple,       'TDynIntArray',
                                []);
  //TODO add () around list
  if FDoStatIntArray then
  AddFmtDef('VarStatIntArray',            '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18',
                                skSimple,       'TStatIntArray',
                                []);
  AddFmtDef('VarPDynIntArray',            Match_Pointer,
                                skPointer,      'PDynIntArray',
                                []);
  AddFmtDef('VarPStatIntArray',           Match_Pointer,
                                skPointer,      'PStatIntArray',
                                []);
  AddFmtDef('VarDynIntArrayA',            Match_Pointer+'|\{\}|0,[\s\r\n]+2',
                                skSimple,       '',
                                []);
  if FDoStatIntArray then
  AddFmtDef('VarStatIntArrayA',           '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18',
                                skSimple,       '',
                                []);

  AddFmtDef('VarDynIntArray[1]',          '2',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  AddFmtDef('VarStatIntArray[6]',         '12',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  AddFmtDef('VarPDynIntArray^[1]',        '2',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  AddFmtDef('VarPStatIntArray^[6]',       '12',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  AddFmtDef('VarDynIntArrayA[1]',         '2',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  AddFmtDef('VarStatIntArrayA[6]',        '12',
                                skSimple,       'Integer|LongInt',
                                [fTpMtch]);
  {%endregion    * Array * }

  r := AddArrayFmtDef('ArgTDynArrayTRec1', '.', 'TDynArrayTRec1', []);
  r := AddRecForArrFmtDef('ArgTDynArrayTRec1[0]', 3, 90, []);
  r := AddRecForArrFmtDef('ArgTDynArrayTRec1[1]', 3, 91, []);

  r := AddArrayFmtDef('ArgTDynArrayPRec1', '.', 'TDynArrayPRec1', []);
  r := AddPointerFmtDef  ('ArgTDynArrayPRec1[0]', '\^TRecForArray3', '^TRecForArray3', []);
  r := AddRecForArrFmtDef('ArgTDynArrayPRec1[0]^', 3, 90, [IgnDwrf, IgnStabs]);
  r := AddPointerFmtDef  ('ArgTDynArrayPRec1[1]', '\^TRecForArray3', '^TRecForArray3', []);
  r := AddRecForArrFmtDef('ArgTDynArrayPRec1[1]^', 3, 91, [IgnDwrf, IgnStabs]);

  r := AddArrayFmtDef('ArgTDynDynArrayTRec1', '.', 'TDynDynArrayTRec1', []);
  r := AddArrayFmtDef('ArgTDynDynArrayTRec1[0]', '.', 'xxx', [IgnDwrf, IgnStabs]);
  r := AddRecForArrFmtDef('ArgTDynArrayTRec1[0][0]', 1, 80, [IgnDwrf, IgnStabs]);
  r := AddRecForArrFmtDef('ArgTDynArrayTRec1[0][1]', 1, 81, [IgnDwrf, IgnStabs]);
  r := AddArrayFmtDef('ArgTDynDynArrayTRec1[1]', '.', 'xxx', [IgnDwrf, IgnStabs]);
  r := AddRecForArrFmtDef('ArgTDynArrayTRec1[1][0]', 1, 85, [IgnDwrf, IgnStabs]);
  r := AddRecForArrFmtDef('ArgTDynArrayTRec1[1][1]', 1, 86, [IgnDwrf, IgnStabs]);


  r := AddArrayFmtDef('VArgTDynArrayTRec1', '.', 'TDynArrayTRec1', []);
  r := AddRecForArrFmtDef('VArgTDynArrayTRec1[0]', 3, 90, []);
  r := AddRecForArrFmtDef('VArgTDynArrayTRec1[1]', 3, 91, []);

  r := AddArrayFmtDef('VArgTDynArrayPRec1', '.', 'TDynArrayPRec1', []);
  r := AddPointerFmtDef  ('VArgTDynArrayPRec1[0]', '\^TRecForArray3', '^TRecForArray3', []);
  r := AddRecForArrFmtDef('VArgTDynArrayPRec1[0]^', 3, 90, [IgnDwrf, IgnStabs]);
  r := AddPointerFmtDef  ('VArgTDynArrayPRec1[1]', '\^TRecForArray3', '^TRecForArray3', []);
  r := AddRecForArrFmtDef('VArgTDynArrayPRec1[1]^', 3, 91, [IgnDwrf, IgnStabs]);
end;

procedure TTestWatches.AddExpectBreakFooMixInfo;
  procedure AddTC(AVar, ATCast:  string; AExpClass: String = ''; AFlgs: TWatchExpectationFlags = [];
                  AIntMember: String = ''; AIntValue: integer = 0);
  begin
    if AExpClass = '' then AExpClass := ATCast;
    If ATCast <> ''
    then Add(ATCast+'('+AVar+')', wdfDefault, MatchClass(AExpClass, ''), skClass, AExpClass, AFlgs)
    else Add(AVar,                wdfDefault, MatchClass(AExpClass, ''), skClass, AExpClass, AFlgs);
    if AIntMember <> '' then
      Add(ATCast+'('+AVar+').'+AIntMember,  wdfDefault, IntToStr(AIntValue), skSimple, M_Int, [fTpMtch]);
  end;
  procedure AddTCN(AVar, ATCast:  string; AExpClass: String = ''; AFlgs: TWatchExpectationFlags = []);
  begin
    if AExpClass = '' then AExpClass := ATCast;
    If ATCast <> ''
    then Add(ATCast+'('+AVar+')', wdfDefault, MatchClassNil(AExpClass), skClass, AExpClass, AFlgs)
    else Add(AVar,                wdfDefault, MatchClassNil(AExpClass), skClass, AExpClass, AFlgs);
  end;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.Mix')] then exit;
  FCurrentExpArray := @ExpectBreakFoo;

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
  Add('VarStatIntArray',  wdfDefault,     '10,[\s\r\n]+12,[\s\r\n]+14,[\s\r\n]+16,[\s\r\n]+18',
                                skSimple,       'TStatIntArray',
                                []);
end;

procedure TTestWatches.AddExpectBreakFooAndSubFoo;
  procedure AddF(AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
    AStackFrame: Integer=0);
  begin
    FCurrentExpArray := @ExpectBreakFoo;
    AddTo(ExpectBreakFoo, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs, AStackFrame)
  end;
  procedure AddS(AnExpr:  string; AFmt: TWatchDisplayFormat;
    AMtch: string; AKind: TDBGSymbolKind; ATpNm: string; AFlgs: TWatchExpectationFlags;
    AStackFrame: Integer=0);
  begin
    FCurrentExpArray := @ExpectBreakSubFoo;
    AddTo(ExpectBreakSubFoo, AnExpr, AFmt, AMtch, AKind, ATpNm, AFlgs, AStackFrame)
  end;
begin
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.Cache')] then exit;
//  FCurrentExpArray := @ExpectBreakSubFoo;

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
  Only: Integer;
  OnlyName, OnlyNamePart: String;

  function SkipTest(const Data: TWatchExpectation): Boolean;
  begin
    Result := True;
    if Data.Result[SymbolType].Flgs * [fTstSkip, fTstSkipDwarf3] <> [] then exit;
    Result := False;
  end;

  function MatchOnly(const Data: TWatchExpectation; Idx: Integer): Boolean;
  begin
    Result := True;
    if ((Only >=0) and (Only <> Idx)) or
       ((OnlyName<>'') and (OnlyName <> Data.TestName)) or
       ((OnlyNamePart<>'') and (pos(OnlyNamePart, Data.TestName)<1))
    then Result := False;
  end;

  procedure TestWatch(Name: String; AWatch: TCurrentWatch; Data: TWatchExpectation; WatchValue: String = '');
  const KindName: array [TDBGSymbolKind] of string =
     ('skClass', 'skRecord', 'skEnum', 'skSet', 'skProcedure', 'skFunction', 'skSimple', 'skPointer', 'skVariant');
  var
    rx: TRegExpr;
    s: String;
    flag, IsValid, HasTpInfo, f2: Boolean;
    WV: TWatchValue;
    Stack: Integer;
    n: String;
    DataRes: TWatchExpectationResult;
    IgnoreFlags: TWatchExpectationFlags;
    IgnoreAll, IgnoreData, IgnoreKind, IgnoreKindPtr, IgnoreTpName: boolean;
    IgnoreText: String;
  begin
    if not TestTrue('Dbg did NOT enter dsError', dbg.State <> dsError) then exit;
    rx := nil;
    Stack := Data.StackFrame;
    DataRes := Data.Result[SymbolType];
    IgnoreFlags := DataRes.Flgs * WatchExpFlagMask[SymbolType];
    IgnoreAll     := IgnoreFlags * WatchExpFlagSIgnAll <> [];
    IgnoreData    := IgnoreFlags * WatchExpFlagSIgnData <> [];
    IgnoreKind    := IgnoreFlags * WatchExpFlagSIgnKind <> [];
    IgnoreKindPtr := IgnoreFlags * WatchExpFlagSIgnKindPtr <> [];
    IgnoreTpName  := IgnoreFlags * WatchExpFlagSIgnTpName <> [];

    // Get Value
    n := Data.TestName;
    LogToFile('###### ' + n + '######' +LineEnding);
    if n = '' then n := Data.Expression + ' (' + TWatchDisplayFormatNames[Data.DspFormat] + ')';
    Name := Name + ' ' + n;
    flag := AWatch <> nil; // test for typeinfo/kind  // Awatch=nil > direct gdb command
    IsValid := True;
    HasTpInfo := True;
    if flag then begin;
      WV := AWatch.Values[1, Stack];// trigger read
      s := WV.Value;
      IsValid := WV.Validity = ddsValid;
      HasTpInfo := IsValid and (WV.TypeInfo <> nil);
//      flag := flag and IsValid;
    end
    else
      s := WatchValue;

    if not TestTrue('Dbg did NOT enter dsError', dbg.State <> dsError) then exit;

    // Check Data
    f2 := True;
    IgnoreText := '';    if IgnoreData then IgnoreText := 'Ignored by flag';
    if IsValid then begin
      rx := TRegExpr.Create;
      rx.ModifierI := true;
      rx.Expression := DataRes.ExpMatch;
      if DataRes.ExpMatch <> ''
      then f2 := TestTrue(Name + ' Matches "'+DataRes.ExpMatch + '", but was "' + s + '"', rx.Exec(s), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
      FreeAndNil(rx);
    end else begin
       f2 := TestTrue(Name + ' Matches "'+DataRes.ExpMatch + '", but STATE was <'+dbgs(WV.Validity)+'> Val="'+s+'"', False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
       //exit; // failed Data, do not list others as potential unexpected success
    end;

    if (not f2) and IgnoreAll then exit; // failed Data, do not list others as potential unexpected success

    // TypeInfo checks ?
    if (not flag) or (DataRes.ExpTypeName = '') then exit;

    // Check TypeInfo
    s:='';
    if HasTpInfo then s := KindName[WV.TypeInfo.Kind];
    IgnoreText := '';    if IgnoreKind then IgnoreText := 'Ignored by flag';
    if IsValid and HasTpInfo then begin
      if (not IgnoreKind) and IgnoreKindPtr and (WV.TypeInfo.Kind = skPointer) then IgnoreText := 'Ignored by flag (Kind may be Ptr)';
      f2 := TestEquals(Name + ' Kind',  KindName[DataRes.ExpKind], KindName[WV.TypeInfo.Kind], DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    end else begin
      f2 := TestTrue(Name + ' Kind is "'+KindName[DataRes.ExpKind]+'", failed: STATE was <'+dbgs(WV.Validity)+'>, HasTypeInfo='+dbgs(HasTpInfo)+' Val="'+s+'"', False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    end;

    if (not f2) and IgnoreAll then exit; // failed Data, do not list others as potential unexpected success

    // Check TypeName
    IgnoreText := '';    if IgnoreTpName then IgnoreText := 'Ignored by flag';
    if IsValid and HasTpInfo then begin
      s:='';
      if HasTpInfo then s := WV.TypeInfo.TypeName;
      if fTpMtch  in DataRes.Flgs
      then begin
        rx := TRegExpr.Create;
        rx.ModifierI := true;
        rx.Expression := DataRes.ExpTypeName;
        TestTrue(Name + ' TypeName matches '+DataRes.ExpTypeName+' but was '+s,  rx.Exec(s), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
        FreeAndNil(rx);
       end
       else TestEquals(Name + ' TypeName',  LowerCase(DataRes.ExpTypeName), LowerCase(s), DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    end else begin
        TestTrue(Name + ' TypeName matches '+DataRes.ExpTypeName+' but STATE was <'+dbgs(WV.Validity)+'> HasTypeInfo='+dbgs(HasTpInfo)+' Val="'+s+'"',  False, DataRes.MinGdb, DataRes.MinFpc, IgnoreText);
    end;

  end;

var
  i: Integer;
  WList, WListSub, WListArray: Array of TCurrentWatch;

begin
  TestBaseName := NamePreFix;
  if not HasTestArraysData then exit;
  Only := StrToIntDef(TestControlForm.EdOnlyWatch.Text, -1);
  OnlyNamePart := '';OnlyName := '';
  if Only < 0
  then begin
    OnlyName := TestControlForm.EdOnlyWatch.Text;
    if (OnlyName <> '') and (OnlyName[1]='*') then begin
      OnlyNamePart := copy(OnlyName, 2, length(OnlyName));
      OnlyName := '';
    end;
  end;


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
    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC_ARRAY) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    if dbg.State = dsError then
      Fail(' Failed Init');

    (* Create all watches *)
    SetLength(WList, length(ExpectBreakFoo));
    for i := low(ExpectBreakFoo) to high(ExpectBreakFoo) do begin
      if not MatchOnly(ExpectBreakFoo[i], i) then continue;
      if not SkipTest(ExpectBreakFoo[i]) then begin
        WList[i] := TCurrentWatch.Create(FWatches);
        WList[i].Expression := ExpectBreakFoo[i].Expression;
        WList[i].DisplayFormat := ExpectBreakFoo[i].DspFormat;
        WList[i].enabled := True;
      end;
    end;

    SetLength(WListSub, length(ExpectBreakSubFoo));
    for i := low(ExpectBreakSubFoo) to high(ExpectBreakSubFoo) do begin
      if not MatchOnly(ExpectBreakSubFoo[i], i) then continue;
      if not SkipTest(ExpectBreakSubFoo[i]) then begin
        WListSub[i] := TCurrentWatch.Create(FWatches);
        WListSub[i].Expression := ExpectBreakSubFoo[i].Expression;
        WListSub[i].DisplayFormat := ExpectBreakSubFoo[i].DspFormat;
        WListSub[i].enabled := True;
      end;
    end;

    SetLength(WListArray, length(ExpectBreakFooArray));
    for i := low(ExpectBreakFooArray) to high(ExpectBreakFooArray) do begin
      if not MatchOnly(ExpectBreakFooArray[i], i) then continue;
      if not SkipTest(ExpectBreakFooArray[i]) then begin
        WListArray[i] := TCurrentWatch.Create(FWatches);
        WListArray[i].Expression := ExpectBreakFooArray[i].Expression;
        WListArray[i].DisplayFormat := ExpectBreakFooArray[i].DspFormat;
        WListArray[i].enabled := True;
      end;
    end;

    (* Start debugging *)
    dbg.ShowConsole := True;
    dbg.Run;

    if TestTrue('State=Pause', dbg.State = dsPause)
    then begin
      (* Hit first breakpoint: BREAK_LINE_FOOFUNC_NEST SubFoo -- (1st loop) Called with none nil data *)

      for i := low(ExpectBreakSubFoo) to high(ExpectBreakSubFoo) do begin
        if not MatchOnly(ExpectBreakSubFoo[i], i) then continue;
        if not SkipTest(ExpectBreakSubFoo[i]) then
          TestWatch('Brk1 '+IntToStr(i)+' ', WListSub[i], ExpectBreakSubFoo[i]);
      end;

      dbg.Run;
    end
    else TestTrue('Hit BREAK_LINE_FOOFUNC_NEST', False);

    if TestTrue('State=Pause', dbg.State = dsPause)
    then begin
      (* Hit 2nd breakpoint: BREAK_LINE_FOOFUNC Foo -- (1st loop) Called with none nil data *)

      FDbgOutPutEnable := True;
      for i := low(ExpectBreakFooGdb) to high(ExpectBreakFooGdb) do begin
        if not MatchOnly(ExpectBreakFooGdb[i], i) then continue;
        if not SkipTest(ExpectBreakFooGdb[i]) then begin
          FDbgOutPut := '';
          dbg.TestCmd(ExpectBreakFooGdb[i].Expression);
          TestWatch('Brk1 Direct Gdb '+IntToStr(i)+' ', nil, ExpectBreakFooGdb[i], FDbgOutPut);
        end;
      end;
      FDbgOutPutEnable := False;

      for i := low(ExpectBreakFoo) to high(ExpectBreakFoo) do begin
        if not MatchOnly(ExpectBreakFoo[i], i) then continue;
        if not SkipTest(ExpectBreakFoo[i]) then
          TestWatch('Brk1 '+IntToStr(i)+' ', WList[i], ExpectBreakFoo[i]);
      end;

      dbg.Run;
    end
    else TestTrue('Hit BREAK_LINE_FOOFUNC', False);

    if TestTrue('State=Pause', dbg.State = dsPause)
    then begin
      (* Hit 2nd breakpoint: BREAK_LINE_FOOFUNC_ARRAY SubFoo_Watches -- (1st loop) Called with none nil data *)

      for i := low(ExpectBreakFooArray) to high(ExpectBreakFooArray) do begin
        if not MatchOnly(ExpectBreakFooArray[i], i) then continue;
        if not SkipTest(ExpectBreakFooArray[i]) then
          TestWatch('Brk1 '+IntToStr(i)+' ', WListArray[i], ExpectBreakFooArray[i]);
      end;

      dbg.Run;
    end
    else TestTrue('Hit BREAK_LINE_FOOFUNC_ARRAY', False);


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

  FDoStatIntArray := TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.Unstable')];
  // GDB 7.0 with fpc 2.4.x has issues with "array of int"
  FDoStatIntArray := FDoStatIntArray and
                     not ((pos('2.4.', CompilerInfo.Name) > 0) and (DebuggerInfo.Version = 70000));

  ClearTestErrors;

  ClearAllTestArrays;
  AddExpectBreakFooGdb;
  AddExpectBreakFooAll;
  AddExpectBreakFooArray;
  //AddExpectBreakFooMixInfo;
  AddExpectBreakFooAndSubFoo;
  RunTestWatches('', TestExeName,  '', []);

  if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestWatch.Mix')]
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

    if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('    TTestWatch.Mix.All')]
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

