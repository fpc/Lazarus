unit TestDisAss;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, LCLProc,
  DbgIntfBaseTypes, DbgIntfDebuggerBase, GDBMIDebugger, TestBase, maps,
  TTestDebuggerClasses;

type
  TTestDisAssRegion = record
    FirstAddr, LastAddr: TDBGPtr;
    FirstOutAddr, LastOutAddr: TDBGPtr; // continue output to last_out addr
    FuncName: String;
    FileName: String;
    BaseLine, InstrPerLine: Integer;
    InstrLen: Integer
  end;

  { TTestBrkGDBMIDebugger }

  TTestBrkGDBMIDebugger = class(TGDBMIDebugger)
  protected
    FTestCmdLine: String;
    procedure SendCmdLn(const ACommand: String); override;  overload;
    function  ReadLine(const APeek: Boolean; ATimeOut: Integer = - 1): String; override; overload;
    function CreateDebugProcess(const AOptions: String): Boolean; override;
    function GetDebugProcessRunning: Boolean; override;
  protected
    function TestRespodDisass(AStartAddr, AEndAddr: TDBGPtr; AWithSrc: Boolean): String;
    function TestMemDump(AStartAddr: TDBGPtr; AWSize, ARowCnt, AColCnt: Integer): String;
  public
    TestDisAssRegions: array of TTestDisAssRegion;
    TestDefaultRegion: TTestDisAssRegion;
    TestFailMemDump: Boolean;
    TestIsFailed: Boolean;
    procedure TestSetState(const AValue: TDBGState);
  end;

  { TTestDisAss }

  TTestDisAss = class(TTestCase)
  protected
    FCallStack: TTestCallStackMonitor;
    FExceptions: TBaseExceptions;
    //FSignals: TBaseSignals;
    //FBreakPoints: TIDEBreakPoints;
    //FBreakPointGroups: TIDEBreakPointGroups;
    FLocals: TLocalsMonitor;
    FLineInfo: TBaseLineInfo;
    FWatches: TTestWatchesMonitor;
    FThreads: TThreadsMonitor;
    FRegisters: TRegistersMonitor;
  published
    procedure RangeMap;
    procedure Disassemble;
  end;

implementation

{ TTestBrkGDBMIDebugger }

procedure TTestBrkGDBMIDebugger.SendCmdLn(const ACommand: String);
begin
  debugln(['############', ACommand]);
  FTestCmdLine := ACommand;
end;

function TTestBrkGDBMIDebugger.ReadLine(const APeek: Boolean; ATimeOut: Integer): String;
  procedure SkipSpaces(var pos: Integer);
  begin
    while (pos <= length(FTestCmdLine)) and (FTestCmdLine[pos] = ' ') do inc(pos);
  end;
  function CheckString(var pos: Integer; const txt: String; ASkipSpaces: Boolean=True): Boolean;
  begin
    Result := copy(FTestCmdLine, pos, length(txt)) = txt;
    if Result then inc(pos, length(txt));
    if Result and ASkipSpaces then SkipSpaces(pos);
  end;
  function GetNum(var pos: Integer; out num: Integer; ASkipSpaces: Boolean=True): Boolean;
  var p1: Integer;
  begin
    p1 := pos;
    if not (FTestCmdLine[pos] in ['0'..'9']) then exit(False);
    while (pos <= length(FTestCmdLine)) and (FTestCmdLine[pos] in ['0'..'9']) do inc(pos);
    if (pos <= length(FTestCmdLine)) and not (FTestCmdLine[pos] in [' ']) then exit(False);
    num := StrToIntDef(copy(FTestCmdLine, p1, pos-p1), -1);
    if ASkipSpaces then SkipSpaces(pos);
    Result := True;
  end;


var
  i,j : Integer;
  StartAddr, EndAddr, WSize, RowCnt, ColCnt: Integer;
  WithSrc: Boolean;
begin
  if FTestCmdLine = '' then exit('(gdb) ');

  i := 1;
  if CheckString(i, '-data-disassemble ')
  then if CheckString(i, '-s')
  then if GetNum(i, StartAddr)
  then if CheckString(i, '-e')
  then if GetNum(i, EndAddr)
  then begin
    WithSrc := False;
    if CheckString(i, '--')
    then begin
      WithSrc := GetNum(i, j);
      WithSrc := j = 1;
    end;
debugln([StartAddr]);
debugln([EndAddr]);
    FTestCmdLine := '';
    Result := TestRespodDisass(StartAddr, EndAddr, WithSrc);
    debugln(['###>>>',Result,'###<<<']);
    exit;
  end;

  // -data-read-memory 65608 x 1 1 389
  i:=1;
  if CheckString(i, '-data-read-memory ')
  then if GetNum(i, StartAddr)
  then if CheckString(i, 'x') // only hex supported
  then if GetNum(i, WSize) // word-size: 1=byte
  then if GetNum(i, RowCnt) // rows
  then if GetNum(i, ColCnt) // col
  then begin
    FTestCmdLine := '';
    if TestFailMemDump then exit('(gdb) ');
    Result := TestMemDump(StartAddr, WSize, RowCnt, ColCnt);
    debugln(['###>>>',Result,'###<<<']);
    exit;
  end;


  TestIsFailed := True; // unknow command
  FTestCmdLine := '';
  Result := '';
end;

function TTestBrkGDBMIDebugger.CreateDebugProcess(const AOptions: String): Boolean;
begin
  Result := True;
end;

function TTestBrkGDBMIDebugger.GetDebugProcessRunning: Boolean;
begin
  Result := True;
end;

function TTestBrkGDBMIDebugger.TestRespodDisass(AStartAddr, AEndAddr: TDBGPtr;
  AWithSrc: Boolean): String;

  function GetDisAssRegion(AnAddr: TDBGPtr): TTestDisAssRegion;
  var
    i: Integer;
  begin
    Result := TestDefaultRegion;
    for i := 0 to high(TestDisAssRegions) do
      if (AnAddr >= TestDisAssRegions[i].FirstAddr) and
         (AnAddr <= TestDisAssRegions[i].LastAddr)
      then begin
        Result := (TestDisAssRegions[i]);
        break;
      end;
    if Result.InstrLen < 1 then Result.InstrLen := 4;
    if Result.InstrPerLine < 1 then Result.InstrPerLine := 8;
  end;

  function GetMIInstr(AnAddr: TDBGPtr; ARgn: TTestDisAssRegion): String;
  begin
    Result := Format('{address="0x%x",', [AnAddr]);
    if ARgn.FuncName <> '' then
      Result := Result + Format('func-name="%s",offset="%d",', [ARgn.FuncName, AnAddr -ARgn.FirstAddr]);
    Result := Result + 'inst="nop"}'
  end;

var
  CurAddr: TDBGPtr;
  Rgn: TTestDisAssRegion;
  CurLine, lc: Integer;
begin
  Result := '';
  Rgn := GetDisAssRegion(AStartAddr);
  if (Rgn.FileName = '') then AWithSrc := False;
  if Rgn.FirstOutAddr <> 0 then AStartAddr := Rgn.FirstOutAddr;
  if (Rgn.LastOutAddr <> 0) and (AEndAddr < Rgn.LastOutAddr) then AEndAddr := Rgn.LastOutAddr;

  CurAddr := AStartAddr;
  CurLine := Rgn.BaseLine + (CurAddr - Rgn.FirstAddr) div Rgn.InstrPerLine;
  Result := '^done,asm_insns=[';
  lc := 0;
  if AWithSrc then begin
    Result := Result + Format('src_and_asm_line={line="%d",file="%s",line_asm_insn=[ ',
                              [CurLine, Rgn.FileName]);
  end;

  while CurAddr <= AEndAddr do begin;
    if AWithSrc and (lc >= Rgn.InstrPerLine) then begin
      Result := Result + ']}';
      Result := Result + Format(',src_and_asm_line={line="%d",file="%s",line_asm_insn=[ ',
                                [CurLine, Rgn.FileName]);
      lc := 0;
    end;
    inc(lc);

    if CurAddr > Rgn.LastOutAddr then begin
      Rgn := GetDisAssRegion(CurAddr);
    end;
    if (Result <> '') and not (Result[length(Result)] in ['{', '[']) then Result := Result + ',';;
    Result := Result + GetMIInstr(CurAddr, Rgn);
    CurAddr := CurAddr + Rgn.InstrLen;
  end;

  if AWithSrc then
    Result := Result + ']}';
  Result := Result + ']';//+LineEnding;
end;

function TTestBrkGDBMIDebugger.TestMemDump(AStartAddr: TDBGPtr; AWSize, ARowCnt,
  AColCnt: Integer): String;
var
  i: Integer;
begin
  // only supports single row
  Result := Format('^done,'
                  +'addr="0x%x",nr-bytes="%d",total-bytes="%d",'
                  +'next-row="0x%x",prev-row="0x%x",'
                  +'next-page="0x%x",prev-page="0x%x",'
                  +'memory=[{addr="0x%x",data=["0x00"', // first entry
                  [AStartAddr, AColCnt, AColCnt,
                   AStartAddr+AColCnt, AStartAddr-AColCnt,
                   AStartAddr+AColCnt, AStartAddr-AColCnt,
                   AStartAddr
                  ]);
  for i := 2 to AColCnt do
    Result := Result + ',"0x00"';
  Result := Result + ']}]';//+LineEnding;
end;

procedure TTestBrkGDBMIDebugger.TestSetState(const AValue: TDBGState);
begin
  SetState(AValue);
end;

procedure TTestDisAss.RangeMap;
var Errors: String;

  function AddRangeItem(ARange: TDBGDisassemblerEntryRange; AnAddr: TDBGPtr): TDisassemblerEntry;
  begin
    Result.Addr := AnAddr;
    ARange.Append(@Result);
  end;

  function CreateRange(AFirst, ALast, AItemEnd: TDBGPtr; AList: Array of TDBGPtr): TDBGDisassemblerEntryRange;
  var
    i: Integer;
  begin
    Result := TDBGDisassemblerEntryRange.Create;
    Result.RangeStartAddr   := AFirst;
    Result.RangeEndAddr     := ALast;
    Result.LastEntryEndAddr := AItemEnd;
    for i := low(AList) to high(AList) do
      AddRangeItem(Result, AList[i]);
  end;

  procedure TestValue(AName: String; AExp, AGot: String); overload;
  begin
    if AExp <> AGot then Errors := Format('%s%s: ExP: %s Got %s%s', [Errors, AName, AExp, AGot, LineEnding]);;
  end;
  procedure TestValue(AName: String; AExp, AGot: TDBGPtr); overload;
  begin
    if AExp <> AGot then Errors := Format('%s%s: ExP: %d Got %d%s', [Errors, AName, AExp, AGot, LineEnding]);;
  end;
  procedure TestRange(AName: String; ARange: TDBGDisassemblerEntryRange; AFirst, ALast, AItemEnd: TDBGPtr; AList: Array of TDBGPtr);
  var
    i: Integer;
  begin
    try
      if ARange = nil then begin
        Errors := Format('%s%s: No Range found%s', [Errors, AName, LineEnding]);
      end;
      TestValue(AName + 'RangeStartAddr',   ARange.RangeStartAddr, AFirst);
      TestValue(AName + 'RangeEndAddr',     ARange.RangeEndAddr, ALast);
      TestValue(AName + 'LastEntryEndAddr', ARange.LastEntryEndAddr, AItemEnd);
      TestValue(AName + 'Count',            ARange.Count, high(AList)-low(AList)+1);
        for i := 0 to ARange.Count-1 do debugln([i,': ',ARange.EntriesPtr[i]^.Addr]);
      for i := low(AList) to high(AList) do
        TestValue(AName + 'Item '+IntToStr(i), ARange.EntriesPtr[i]^.Addr, AList[i]);
    except
      on e: exception do Errors := Format('%s%s: Exception: %s %s%s', [Errors, AName, e.ClassName, e.Message, LineEnding]);
    end;
  end;

var
  EntryRanges: TDBGDisassemblerEntryMap;
  Range: TDBGDisassemblerEntryRange;
begin
  //TDBGDisassemblerEntryMapIterator
  EntryRanges := TDBGDisassemblerEntryMap.Create(itu8, SizeOf(TDBGDisassemblerEntryRange));

  EntryRanges.AddRange(CreateRange(100, 120, 120, [100, 108, 116]));
  TestValue('Map Count', 1, EntryRanges.Count);
  Range := EntryRanges.GetRangeForAddr(100, False);
  TestRange('R1', Range, 100, 120, 120, [100, 108, 116]);

  EntryRanges.AddRange(CreateRange(120, 140, 140, [120, 128, 136]));
  TestValue('Map Count (merged to end)', 1, EntryRanges.Count);
  Range := EntryRanges.GetRangeForAddr(100, False);
  TestRange('R1 (merged to end)', Range, 100, 140, 140, [100, 108, 116, 120, 128, 136]);

  EntryRanges.AddRange(CreateRange(80, 100, 100, [80, 90]));
  TestValue('Map Count (merged to start)', 1, EntryRanges.Count);
  Range := EntryRanges.GetRangeForAddr(100, False);
  TestRange('R1 (merged to end)', Range, 80, 140, 140, [80, 90, 100, 108, 116, 120, 128, 136]);

  FreeAndNil(EntryRanges);

  AssertEquals('', Errors);
end;

procedure TTestDisAss.Disassemble;
var
  IdeDisAss: TBaseDisassembler;
  Gdb: TTestBrkGDBMIDebugger;

  procedure Init;
  begin
    FreeAndNil(IdeDisAss);
    FreeAndNil(Gdb);
    Gdb := TTestBrkGDBMIDebugger.Create('');
    //IdeDisAss := TBaseDisassembler.Create;
    //IdeDisAss.Master := Gdb.Disassembler;
    IdeDisAss := Gdb.Disassembler;

    FWatches := TTestWatchesMonitor.Create;
    FThreads := TThreadsMonitor.Create;
    FExceptions := TBaseExceptions.Create(TBaseException);
    //FSignals := TBaseSignals.Create(TBaseSignal);
    FLocals := TLocalsMonitor.Create;
    FLineInfo := TBaseLineInfo.Create;
    FCallStack := TTestCallStackMonitor.Create;
    FRegisters := TRegistersMonitor.Create;

    //TManagedBreakpoints(FBreakpoints).Master := FDebugger.BreakPoints;
    FWatches.Supplier := Gdb.Watches;
    FThreads.Supplier := Gdb.Threads;
    FLocals.Supplier := Gdb.Locals;
    //FLineInfo.Master := Gdb.LineInfo;
    FCallStack.Supplier := Gdb.CallStack;
    Gdb.Exceptions := FExceptions;
    //FSignals.Master := Gdb.Signals;
    FRegisters.Supplier := Gdb.Registers;

    Gdb.TestSetState(dsPause);
    Gdb.TestIsFailed := False;;
    Gdb.TestFailMemDump := False;

  end;
  procedure CleanGdb;
  begin
    FreeAndNil(Gdb);
    FWatches.Supplier := nil;
    FThreads.Supplier := nil;
    //FLocals.Master := nil;
    //FLineInfo.Master := nil;
    FCallStack.Supplier := nil;
    //FExceptions.Master := nil;
    //FSignals.Master := nil;
    //FRegisters.Master := nil;

    FreeAndNil(FWatches);
    FreeAndNil(FThreads);
    //FreeAndNil(FBreakPoints);
    //FreeAndNil(FBreakPointGroups);
    FreeAndNil(FCallStack);
    FreeAndNil(FExceptions);
    //FreeAndNil(FSignals);
    FreeAndNil(FLocals);
    FreeAndNil(FLineInfo);
    FreeAndNil(FRegisters);
  end;

  procedure Test(Name: String;Addr: TDBGPtr; MinBefore, MinAfter: Integer);
  var
    t: String;
    a, b: TDBGPtr;
    i: Integer;
  begin
    t := '';
    if Gdb.TestIsFailed then t := t + ' - Unknown Command';
    if IdeDisAss.BaseAddr <> Addr then t := t + Format(' - BaseAddr, Exp %x, got %x', [Addr, IdeDisAss.BaseAddr]);
    if IdeDisAss.CountBefore < MinBefore then t := t + Format(' - CountBefore, Exp %d, got %d', [MinBefore, IdeDisAss.CountBefore]);
    if IdeDisAss.CountAfter < MinAfter then t := t + Format(' - CountAfter, Exp %d, got %d', [MinAfter, IdeDisAss.CountAfter]);

    if IdeDisAss.Entries[0].Addr <> Addr then t := t + Format(' - Entries[0].Addr, Exp %x, got %x', [Addr, IdeDisAss.Entries[0].Addr]);

    a := IdeDisAss.Entries[-IdeDisAss.CountBefore].Addr;
    for i := -IdeDisAss.CountBefore + 1 to IdeDisAss.CountAfter - 1 do begin
      b := IdeDisAss.Entries[i].Addr;
      if b <= a then t := t + Format(' - Entries[%d].Addr went back, Exp greater %x, got %x', [i, a, b]);
      a := b;
    end;;

    if t <> '' then t := Name+LineEnding+t;
    AssertEquals('', t);
  end;

  procedure TestSrc(Name: String; StartAddr, EndAddr: TDBGPtr);
  var
    t: String;
    b: TDBGPtr;
    i: Integer;
    itm: TDisassemblerEntry;
  begin
    t := '';
    for i := -IdeDisAss.CountBefore to IdeDisAss.CountAfter - 1 do begin
      itm := IdeDisAss.Entries[i];
      b := itm.Addr;
      if (b >= StartAddr) and (b <= EndAddr) and
         not(itm.SrcFileName <> '')
      then t := t + Format(' - Entries[%d] Addr(%x) has no source', [i, b]);
    end;;

    if t <> '' then t := Name+LineEnding+t;
    AssertEquals('', t);
  end;

begin
  Gdb := nil; IdeDisAss := nil;

  {%region NO SOURCE}
    //{%region simple block}
    //Init;
    //IdeDisAss.PrepareRange($20100, 10, 20);
    //Test('no src, no offset, 1 block', $20100, 10, 19);
    //{%endregion}

    //{%region multi simple block}
    //Init;
    //SetLength(Gdb.TestDisAssRegions, 2);
    //with Gdb.TestDisAssRegions[0] do begin
    //  FirstAddr := $30100-184; LastAddr := $30100-25; InstrLen := 8;
    //  FuncName := 'abc';
    //end;
    //with Gdb.TestDisAssRegions[1] do begin
    //  FirstAddr := $30100-24; LastAddr := $30100+200; InstrLen := 8;
    //  FuncName := 'def';
    //end;
    //IdeDisAss.PrepareRange($30100, 10, 20);
    //Test('no src, multi block', $30100, 10, 19);
    //{%endregion}

    {%region multi simple block, overlap}
    // The first block disassembles 4 bytes into the 2nd
    // The 2nd block must be re-done
    Init;
    SetLength(Gdb.TestDisAssRegions, 2);
    with Gdb.TestDisAssRegions[0] do begin
      FirstAddr := $30100-188; LastAddr := $30100-20; InstrLen := 16;
      FuncName := 'abc';
    end;
    with Gdb.TestDisAssRegions[1] do begin
      FirstAddr := $30100-24; LastAddr := $30100+200; InstrLen := 8;
      FuncName := 'def';
    end;
    IdeDisAss.PrepareRange($30100, 10, 20);
    Test('no src, multi block, overlap of 4', $30100, 10, 19);
    CleanGdb;
    {%endregion}
  {%endregion NO SOURCE}


    if false then begin
  {%region simple block, src}
  Init;
  SetLength(Gdb.TestDisAssRegions, 1);
  with Gdb.TestDisAssRegions[0] do begin
    FirstAddr := $30100-400; LastAddr := $30100+400; InstrLen := 8;
    FuncName := 'abc';       FileName := 'foo.pp';   InstrPerLine := 5;
  end;
  IdeDisAss.PrepareRange($30100, 10, 20);
  Test('src, 1 block', $30100, 10, 19);
  TestSrc('src, 1 block', $30100-400, $30100+400);
  CleanGdb;
  {%endregion}

  {%region 2 block, part src}
  Init;
  SetLength(Gdb.TestDisAssRegions, 2);
  with Gdb.TestDisAssRegions[0] do begin
    FirstAddr := $30100-400; LastAddr := $30100-9; InstrLen := 8;
    FuncName := 'abc';
  end;
  with Gdb.TestDisAssRegions[1] do begin
    FirstAddr := $30100-8; LastAddr := $30100+400; InstrLen := 8;
    FuncName := 'def';       FileName := 'foo.pp';   InstrPerLine := 5;
  end;
  IdeDisAss.PrepareRange($30100, 10, 20);
  Test('part-src, 1 block', $30100, 10, 19);
  TestSrc('part-src, 1 block', $30100-8, $30100+400);
  CleanGdb;
  {%endregion}




  {%region fail mem dump}
  Init;
  Gdb.TestFailMemDump := True;
  IdeDisAss.PrepareRange($10100, 10, 20);
  // just enough, if it din't crash => go error state.
  CleanGdb;
  {%endregion}
end;//xxxxxxxxxxxx
  //FreeAndNil(IdeDisAss);
  FreeAndNil(Gdb);
end;



initialization

  RegisterTest(TTestDisAss); 
end.

