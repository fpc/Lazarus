unit TestBreakPoint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testutils, testregistry, TestGDBMIControl,
  TestBase, Debugger, GDBMIDebugger, LCLProc, TestWatches;

type

  { TTestBrkGDBMIDebugger }

  TTestBrkGDBMIDebugger = class(TGDBMIDebugger)
  protected
    FReplaceBreak: Boolean;
    procedure SendCmdLn(const ACommand: String); override;
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
    procedure TestBadAddrBreakpoint;
  end;

implementation

{ TTestBrkGDBMIDebugger }

procedure TTestBrkGDBMIDebugger.SendCmdLn(const ACommand: String);
begin
  if FReplaceBreak and (copy(ACommand,1,13) = '-break-insert')
  then inherited SendCmdLn('-break-insert *200')
  else inherited SendCmdLn(ACommand);
end;



{ TTestBreakPoint }

procedure TTestBreakPoint.DoCurrent(Sender: TObject; const ALocation: TDBGLocationRec);
begin
  FCurFile := ALocation.SrcFile;
  FCurLine := ALocation.SrcLine;
end;

function TTestBreakPoint.DoGetFeedBack(Sender: TObject; const AText, AInfo: String;
  AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
begin
  Result := frOk;
  FreeAndNil(FBrkErr);
end;

function TTestBreakPoint.GdbClass: TGDBMIDebuggerClass;
begin
  Result := TTestBrkGDBMIDebugger;
end;

procedure TTestBreakPoint.TestBadAddrBreakpoint;
var
  TestExeName: string;
  dbg: TTestBrkGDBMIDebugger;
  i: LongInt;
begin
  if SkipTest then exit;
  if not TestControlForm.CheckListBox1.Checked[TestControlForm.CheckListBox1.Items.IndexOf('TTestBreakPoint')] then exit;
  ClearTestErrors;
  FBrkErr := nil;

  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);
  try
    dbg := TTestBrkGDBMIDebugger(StartGDB(AppDir, TestExeName));
    dbg.FReplaceBreak := False;
    dbg.OnCurrent  := @DoCurrent;
    with dbg.BreakPoints.Add('WatchesPrg.pas', BREAK_LINE_FOOFUNC) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    dbg.OnFeedback  := @DoGetFeedBack;

    dbg.Run;
    // hit breakpoint
    dbg.FReplaceBreak := True;
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

initialization

  RegisterDbgTest(TTestBreakPoint);
end.

