unit TestException;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpcunit, testutils, testregistry,
  TestBase, Debugger, GDBMIDebugger, LCLProc;

type

  { TTestExceptionOne }

  TTestExceptionOne = class(TGDBTestCase)
  private
    FGotExceptCount: Integer;
    FGotExceptClass: String;
    FGotExceptMsg: String;
    FGotExceptType: TDBGExceptionType;

    procedure DoDebuggerException(Sender: TObject; const AExceptionType: TDBGExceptionType;
      const AExceptionClass: String; const AExceptionText: String; out AContinue: Boolean);
  published
    procedure TestException;
  end; 




implementation

procedure TTestExceptionOne.DoDebuggerException(Sender: TObject;
  const AExceptionType: TDBGExceptionType; const AExceptionClass: String;
  const AExceptionText: String; out AContinue: Boolean);
begin
  inc(FGotExceptCount);
  FGotExceptClass := AExceptionClass;
  FGotExceptMsg   := AExceptionText;
  FGotExceptType  := AExceptionType;
  AContinue := False;
end;

procedure TTestExceptionOne.TestException;
var
  TestExeName: string;
  dbg: TGDBMIDebugger;
begin
  FGotExceptCount := 0;

  TestCompile(AppDir + 'ExceptPrg.pas', TestExeName);

  try
    dbg := TGDBMIDebugger.Create(DebuggerInfo.ExeName);
    //dbg.OnBreakPointHit := @DebuggerBreakPointHit;
    //dbg.OnState         := @DebuggerChangeState;
    //dbg.OnCurrent       := @DebuggerCurrentLine;
    //dbg.OnDbgOutput     := @DebuggerOutput;
    //dbg.OnDbgEvent      := @DebuggerEvent;
    dbg.OnException      := @DoDebuggerException;

    dbg.Init;
    if dbg.State = dsError then
      Fail(' Failed Init');
    //dbg.Environment

    dbg.WorkingDir := AppDir;
    dbg.FileName   := TestExeName;
    dbg.Arguments := '';
    dbg.ShowConsole := True;

    dbg.Run;
    dbg.Stop;
  finally
    dbg.Free;
  end;

  AssertEquals(' Got 1 exception', 1, FGotExceptCount);
  AssertEquals(' Got class', 'Exception', FGotExceptClass);
  AssertEquals(' Got msg',   'foo', FGotExceptMsg);
end;

initialization
  RegisterDbgTest(TTestExceptionOne);

end.

