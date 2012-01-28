unit TestBreakPoint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc, TestWatches;

type

  { TTestBrkGDBMIDebugger }

  TTestBrkGDBMIDebugger = class(TGDBMIDebugger)
  public
    procedure TestInterruptTarget;
  end;


  { TTestBreakPoint }

  TTestBreakPoint = class(TGDBTestCase)
  private
    FCurLine: Integer;
    FCurFile: string;
    FBrkErr: TDBGBreakPoint;
  protected
    function DoGetFeedBack(Sender: TObject; const AText, AInfo: String;
      AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
    function GdbClass: TGDBMIDebuggerClass; override;
    procedure DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
  published
    // Due to a linker error breakpoints can point to invalid addresses
    procedure TestStartMethod;
    procedure TestBadAddrBreakpoint;
    procedure TestInteruptWhilePaused;
  end;

implementation

procedure TTestBrkGDBMIDebugger.TestInterruptTarget;
begin
  InterruptTarget;
end;

{ TTestBrkGDBMIDebugger }


{ TTestBreakPoint }

procedure   TTestBreakPoint.DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
begin
  FCurFile := ALocation.SrcFile;
  FCurLine := ALocation.SrcLine;
end;

procedure TTestBreakPoint.TestStartMethod;
var
  dbg: TGDBMIDebugger;
  TestExeName, s: string;
  i: TGDBMIDebuggerStartBreak;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestBreakPoint')] then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestBreakPoint.StartMethod')] then exit;

  ClearTestErrors;
  FBrkErr := nil;
  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);

  for i := Low(TGDBMIDebuggerStartBreak) to high(TGDBMIDebuggerStartBreak) do begin
    WriteStr(s, i);

    try
      dbg := StartGDB(AppDir, TestExeName);
      dbg.OnCurrent  := @DoCurrent;
      TGDBMIDebuggerProperties(dbg.GetProperties).InternalStartBreak := i;
      with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
        InitialEnabled := True;
        Enabled := True;
      end;

      dbg.Run;
      TestTrue(s+' not in error state 1', dbg.State <> dsError);
	  TestTrue(s+' at break', FCurLine = BREAK_LINE_FOOFUNC);
    finally
      dbg.Done;
      CleanGdb;
      dbg.Free;
    end;
  end;

  AssertTestErrors;
end;

function   TTestBreakPoint.DoGetFeedBack(Sender: TObject; const AText, AInfo: String;
  AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
begin
  Result := frOk;
  ReleaseRefAndNil(FBrkErr);
end;

function   TTestBreakPoint.GdbClass: TGDBMIDebuggerClass;
begin
  Result := TTestBrkGDBMIDebugger;
end;

procedure   TTestBreakPoint.TestBadAddrBreakpoint;
var
  TestExeName: string;
  dbg: TTestBrkGDBMIDebugger;
  i: LongInt;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestBreakPoint')] then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestBreakPoint.BadAddr')] then exit;
  ClearTestErrors;
  FBrkErr := nil;

  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);
  try
    dbg := TTestBrkGDBMIDebugger(StartGDB(AppDir, TestExeName));
    dbg.OnCurrent  := @DoCurrent;
    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.OnFeedback  := @DoGetFeedBack;

    dbg.Run;
    // hit breakpoint
    FBrkErr := dbg.BreakPoints.Add(TDBGPtr(200));
    with FBrkErr do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    TestTrue('not in error state 1', dbg.State <> dsError);

    i := FCurLine;
    dbg.StepOver;
    TestTrue('not in error state 2', dbg.State <> dsError);
    //TestTrue('gone next line 2', i <> FCurLine);

    i := FCurLine;
    dbg.StepOver;
    TestTrue('not in error state 3', dbg.State <> dsError);
    //TestTrue('gone next line 3', i <> FCurLine);

    i := FCurLine;
    dbg.StepOver;
    TestTrue('not in error state 4', dbg.State <> dsError);
    //TestTrue('gone next line 4', i <> FCurLine);

  finally
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;
  AssertTestErrors;

end;

procedure   TTestBreakPoint.TestInteruptWhilePaused;
var
  TestExeName, Err, IgnoreRes: string;
  dbg: TTestBrkGDBMIDebugger;
  i, m: LongInt;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestBreakPoint')] then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('  TTestBreakPoint.BadInterrupt')] then exit;

  (* Trigger a InterruptTarget while paused.
     Test if the app can continue, and reach it normal exit somehow (even if multiply interupts must be skipped)
  *)

  ClearTestErrors;
  FBrkErr := nil;

  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName, '_wsleep', ' -dWITH_SLEEP ');

  try
    LogToFile(LineEnding+'######################  with pause -- 1 break  ########################'+LineEnding+LineEnding);
    Err := '';
    dbg := TTestBrkGDBMIDebugger(StartGDB(AppDir, TestExeName));
    dbg.OnCurrent  := @DoCurrent;
    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.OnFeedback  := @DoGetFeedBack;

    dbg.Run;
    // at main break
    if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
    then Err := Err + 'Never reached breakpoint to start with';
    if dbg.State <> dsPause
    then Err := Err + 'Never entered dsPause to start with';
    //dbg.StepOver;
    //dbg.StepOver;

    LogToFile('##### INTERRUPT #####');
    dbg.TestInterruptTarget;
    dbg.Run;
    // at main break
    if dbg.State = dsError
    then Err := Err + 'Enterred dsError after 1st exec-continue';
    if dbg.State = dsStop
    then Err := Err + 'Enterred dsStop after 1st exec-continue';

    // try to skip to next break
    if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
    then dbg.Run;
    if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
    then dbg.Run;

    if dbg.State = dsError
    then Err := Err + 'Enterred dsError before reaching break the 2nd time';
    if dbg.State = dsStop
    then Err := Err + 'Enterred dsStop before reaching break the 2nd time';
    if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
    then Err := Err + 'Did not reached breakpoint for the 2nd time';


    dbg.Run;
    if (dbg.State = dsPause)
    then dbg.Run; // got the break really late
    if (dbg.State = dsPause)
    then dbg.Run; // got the break really late

    if dbg.State <> dsStop
    then Err := Err + 'Never reached final stop';
  finally
    TestEquals('Passed pause run', '', Err);
    dbg.Done;
    CleanGdb;
    dbg.Free;
  end;


  if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('    TTestBreakPoint.BadInterrupt.All')] then begin
    try
      LogToFile(LineEnding+'######################  with pause -- 2 breaks  ########################'+LineEnding+LineEnding);
      Err := '';
      dbg := TTestBrkGDBMIDebugger(StartGDB(AppDir, TestExeName));
      dbg.OnCurrent  := @DoCurrent;
      with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
        InitialEnabled := True;
        Enabled := True;
      end;
      with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC_NEST) do begin
        InitialEnabled := True;
        Enabled := True;
      end;

      dbg.OnFeedback  := @DoGetFeedBack;

      dbg.Run;
      // at nested break
      dbg.Run;
      // at main break
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
      then Err := Err + 'Never reached breakpoint to start with';
      if dbg.State <> dsPause
      then Err := Err + 'Never entered dsPause to start with';
      //dbg.StepOver;
      //dbg.StepOver;

      LogToFile('##### INTERRUPT #####');
      dbg.TestInterruptTarget;
      dbg.Run;
      // at main break
      if dbg.State = dsError
      then Err := Err + 'Enterred dsError after 1st exec-continue';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop after 1st exec-continue';

      // try to skip to next break
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC_NEST)
      then dbg.Run;
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC_NEST)
      then dbg.Run;

      if dbg.State = dsError
      then Err := Err + 'Enterred dsError before reaching nest break the 2nd time';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop before reaching nest break the 2nd time';
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC_NEST
      then Err := Err + 'Did not reached best breakpoint for the 2nd time';


      dbg.Run;
      // try to skip to next break
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
      then dbg.Run;
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
      then dbg.Run;

      if dbg.State = dsError
      then Err := Err + 'Enterred dsError before reaching break the 2nd time';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop before reaching break the 2nd time';
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
      then Err := Err + 'Did not reached breakpoint for the 2nd time';


      dbg.Run;
      if (dbg.State = dsPause)
      then dbg.Run; // got the break really late
      if (dbg.State = dsPause)
      then dbg.Run; // got the break really late

      if dbg.State <> dsStop
      then Err := Err + 'Never reached final stop';
    finally
      TestEquals('Passed pause run 2 breaks', '', Err);
      dbg.Done;
      CleanGdb;
      dbg.Free;
    end;
  end;

  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);


  m := 1;
  if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('    TTestBreakPoint.BadInterrupt.All')]
  then m := 5;  // run extra tests of Passed none-pause run

  Err := '';
  for i := 1 to m do begin
    try
      LogToFile(LineEnding+'######################  withOUT pause -- NO stepping  ########################'+LineEnding+LineEnding);
      dbg := TTestBrkGDBMIDebugger(StartGDB(AppDir, TestExeName));
      dbg.OnCurrent  := @DoCurrent;
      with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
        InitialEnabled := True;
        Enabled := True;
      end;

      dbg.OnFeedback  := @DoGetFeedBack;

      dbg.Run;
      // at main break
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
      then Err := Err + 'Never reached breakpoint to start with';
      if dbg.State <> dsPause
      then Err := Err + 'Never entered dsPause to start with';
      //dbg.StepOver;
      //dbg.StepOver;

      LogToFile('##### INTERRUPT #####');
      dbg.TestInterruptTarget;
      dbg.Run;
      // at main break
      if dbg.State = dsError
      then Err := Err + 'Enterred dsError after 1st exec-continue';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop after 1st exec-continue';

      // try to skip to next break
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
      then dbg.Run;
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
      then dbg.Run;

      if dbg.State = dsError
      then Err := Err + 'Enterred dsError before reaching break the 2nd time';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop before reaching break the 2nd time';
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
      then Err := Err + 'Did not reached breakpoint for the 2nd time';


      dbg.Run;
      if (dbg.State = dsPause)
      then dbg.Run; // got the break really late
      if (dbg.State = dsPause)
      then dbg.Run; // got the break really late

      if dbg.State <> dsStop
      then Err := Err + 'Never reached final stop';
    finally
      dbg.Done;
      CleanGdb;
      dbg.Free;
    end;
  end;
  IgnoreRes := '';
  case DebuggerInfo.Version of
    0..069999: IgnoreRes:= 'all gdb 6.x may or may not fail';
    070000: IgnoreRes:= 'gdb 7.0.0 may or may not fail';
    // 7.0.50 seems to always pass
    // 7.1.x seems to always pass
    // 7.2.x seems to always pass
    070300..070399: IgnoreRes:= 'gdb 7.3.x may or may not fail';
  end;
  TestEquals('Passed none-pause run', '', Err, 0, IgnoreRes);


  if TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('    TTestBreakPoint.BadInterrupt.All')] then begin

    try
      LogToFile(LineEnding+'######################  withOUT pause -- with stepping  ########################'+LineEnding+LineEnding);
      Err := '';
      dbg := TTestBrkGDBMIDebugger(StartGDB(AppDir, TestExeName));
      dbg.OnCurrent  := @DoCurrent;
      with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
        InitialEnabled := True;
        Enabled := True;
      end;

      dbg.OnFeedback  := @DoGetFeedBack;

      dbg.Run;
      // at main break
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
      then Err := Err + 'Never reached breakpoint to start with';
      if dbg.State <> dsPause
      then Err := Err + 'Never entered dsPause to start with';
      dbg.StepOver;
      dbg.StepOver;

      LogToFile('##### INTERRUPT #####');
      dbg.TestInterruptTarget;
      dbg.Run;
      // at main break
      if dbg.State = dsError
      then Err := Err + 'Enterred dsError after 1st exec-continue';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop after 1st exec-continue';

      // try to skip to next break
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
      then dbg.Run;
      if (dbg.State = dsPause) and (dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC)
      then dbg.Run;

      if dbg.State = dsError
      then Err := Err + 'Enterred dsError before reaching break the 2nd time';
      if dbg.State = dsStop
      then Err := Err + 'Enterred dsStop before reaching break the 2nd time';
      if dbg.GetLocation.SrcLine <> BREAK_LINE_FOOFUNC
      then Err := Err + 'Did not reached breakpoint for the 2nd time';


      dbg.Run;
      if (dbg.State = dsPause)
      then dbg.Run; // got the break really late
      if (dbg.State = dsPause)
      then dbg.Run; // got the break really late

      if dbg.State <> dsStop
      then Err := Err + 'Never reached final stop';
    finally
      TestEquals('Passed none-pause run with steps', '', Err);
      dbg.Done;
      CleanGdb;
      dbg.Free;
    end;
  end;

  AssertTestErrors;
end;

initialization

  RegisterDbgTest(TTestBreakPoint);
end.

