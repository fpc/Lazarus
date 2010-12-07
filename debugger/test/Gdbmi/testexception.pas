unit TestException;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CompileHelpers,
  Debugger, GDBMIDebugger;

type

  { TTestException }

  TTestException = class(TTestCase)
  private
    FGotExceptCount: Integer;
    FGotExceptClass: String;
    FGotExceptMsg: String;
    FGotExceptType: TDBGExceptionType;

    procedure DoDebuggerException(Sender: TObject; const AExceptionType: TDBGExceptionType;
      const AExceptionClass: String; const AExceptionText: String; out AContinue: Boolean);
  protected
    procedure SetUp; override; 
    procedure TearDown; override; 
  published
    procedure TestException1;
  end; 

implementation

procedure TTestException.DoDebuggerException(Sender: TObject;
  const AExceptionType: TDBGExceptionType; const AExceptionClass: String;
  const AExceptionText: String; out AContinue: Boolean);
begin
  inc(FGotExceptCount);
  FGotExceptClass := AExceptionClass;
  FGotExceptMsg   := AExceptionText;
  FGotExceptType  := AExceptionType;
  AContinue := False;
end;

procedure TTestException.TestException1;
var
  AppDir: String;

  procedure TestCompileWith(const Name, TestExeName, Fpc, Opts: string);
  var
    ErrMsg: String;
  begin
    ErrMsg := TestCompile(AppDir + 'ExceptPrg.pas', Opts, TestExeName, Fpc);
    if ErrMsg <> '' then
      Fail(Name + ' Compilation Failed '+ErrMsg);
  end;

  procedure TestEceptWith(const Name, TestExeName, Gdb: string);
  var
    dbg: TGDBMIDebugger;
    AppDir: String;
  begin
    FGotExceptCount := 0;

    dbg := TGDBMIDebugger.Create(Gdb);
    try
      //dbg.OnBreakPointHit := @DebuggerBreakPointHit;
      //dbg.OnState         := @DebuggerChangeState;
      //dbg.OnCurrent       := @DebuggerCurrentLine;
      //dbg.OnDbgOutput     := @DebuggerOutput;
      //dbg.OnDbgEvent      := @DebuggerEvent;
      dbg.OnException      := @DoDebuggerException;

      dbg.Init;
      if dbg.State = dsError then
        Fail(Name + ' Failed Init');
      //dbg.Environment

      dbg.WorkingDir := AppDir;
      dbg.FileName   := TestExeName;
      dbg.Arguments := '';
      dbg.ShowConsole := True;

      dbg.Run;
      dbg.Stop;
    finally
      dbg.Release;
    end;

    AssertEquals(Name + ' Got 1 exception', 1, FGotExceptCount);
    AssertEquals(Name + ' Got class', 'Exception', FGotExceptClass);
    AssertEquals(Name + ' Got msg',   'foo', FGotExceptMsg);
  end;

var
  FpcList, GdbList: TStringList;
  i, j: Integer;
  TestExeName: string;

begin
  AppDir := ExtractFilePath(Paramstr(0)) + DirectorySeparator+ 'TestApps' + DirectorySeparator;

  FpcList := GetCompilers;
  GdbList := GetDebuggers;
  AssertTrue('Has Compilers', FpcList.Count > 0);
  AssertTrue('Has Debuggers', GdbList.Count > 0);

  for i := 0 to FpcList.Count - 1 do begin
    TestExeName := AppDir + 'lib/ExceptPrg.exe';
    AssertFalse('exe doesn''t exist yet', FileExists(TestExeName));
    try
      TestCompileWith('-gw', TestExeName, FpcList[i], '-gw');
      for j := 0 to GdbList.Count - 1 do begin
        TestEceptWith('-gw', TestExeName, GdbList[j]);
      end;

      DeleteFile(TestExeName);
      AssertFalse('exe doesn''t exist yet', FileExists(TestExeName));
      TestCompileWith('-gs', TestExeName, FpcList[i], '-gs');
      for j := 0 to GdbList.Count - 1 do begin
        TestEceptWith('-gs', TestExeName, GdbList[j]);
      end;

// gw3: msg does not work yet
      //DeleteFile(TestExeName);
      //AssertFalse('exe doesn''t exist yet', FileExists(TestExeName));
      //TestCompileWith('-gw3', TestExeName, FpcList[i], '-gw3');
      //for j := 0 to GdbList.Count - 1 do begin
      //  TestEceptWith('-gw3', TestExeName, GdbList[j]);
      //end;

    finally
      DeleteFile(TestExeName);
    end;
  end;


  FreeAndNil(FpcList);
  FreeAndNil(GdbList);
end;

procedure TTestException.SetUp;
begin
//
end; 

procedure TTestException.TearDown; 
begin
//
end;

initialization

  RegisterTest(TTestException); 
end.

