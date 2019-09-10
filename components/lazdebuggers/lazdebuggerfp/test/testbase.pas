unit TestBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazLogger, DbgIntfDebuggerBase,
  TestDbgConfig, TTestDbgExecuteables, TestDbgTestSuites, TestDbgControl,
  FpDebugDebugger, Dialogs, Forms,
  FpDbgDwarfFreePascal;

implementation

type

  { TTestFpDebugDebugger }

  TTestFpDebugDebugger = class(TTestDbgDebugger)
  protected
    procedure DoBetweenWaitForFinish; override;
  public
    function StartDebugger(AppDir, TestExeName: String): Boolean;
      override;
    procedure CleanAfterTestDone; override;
  end;


procedure BuildTestSuites;
var
  FpcList, GdbList: TBaseList;
  DbgInfo: TExternalExeInfo;
begin
  FpcList := TBaseList(LoadConfig(ConfDir + 'fpclist.txt'));

  DbgInfo.Name := 'FpDebug';
  DbgInfo.CpuBitTypes := [cpu32,cpu64];
  {$IFDEF WIN64}
  DbgInfo.CpuBitTypes := [cpu32,cpu64];
  {$ENDIF}
  {$IFDEF WIN32} // Windows can not cross debug
  DbgInfo.CpuBitTypes := [cpu32];
  {$ENDIF}
  DbgInfo.SymbolTypes := [stDwarf, stDwarfSet, stDwarf3, stDwarf4];
  GdbList := TBaseList.Create;
  GdbList.Add(DbgInfo);

  CreateCompilerList(FpcList, TTestDbgCompiler);
  CreateDebuggerList(GdbList, TTestFpDebugDebugger);

  CreateTestSuites(TestDbgCompilerList, TestDbgDebuggerList, TDBGTestsuite);

  TestControlRegisterCompilers(FpcList);
  TestControlRegisterDebuggers(GdbList);
  FpcList.Free;
  GdbList.Free;
end;

function CheckAppDir(AppDir: string): Boolean;
begin
  Result := DirectoryExistsUTF8(AppDir + 'TestApps') and
    DirectoryExistsUTF8(AppDir + 'TestApps' + DirectorySeparator + 'lib');
end;

function AppDirStripAppBundle(AppDir: string): String;
var
  p: LongInt;
begin
  Result := AppDir;
  p := pos('.app' + DirectorySeparator, AppDir);
  while (p > 1) and (AppDir[p-1] <> DirectorySeparator) do
    dec(p);
  if p > 1 then
    Result := Copy(AppDir, 1, p - 1);
end;

{ TTestFpDebugDebugger }

procedure TTestFpDebugDebugger.DoBetweenWaitForFinish;
begin
  CheckSynchronize(25);
  Application.ProcessMessages;
  inherited DoBetweenWaitForFinish;
end;

function TTestFpDebugDebugger.StartDebugger(AppDir, TestExeName: String
  ): Boolean;
begin
  Result := False;
  FLazDebugger := TFpDebugDebugger.Create('');
  //FLazDebugger.OnDbgOutput  := @InternalDbgOutPut;
  //FLazDebugger.OnFeedback := @InternalFeedBack;
  //FLazDebugger.OnDbgEvent:=@InternalDbgEvent;

  InitDebuggerMonitors(FLazDebugger);

  FLazDebugger.Init;
  if FLazDebugger.State = dsError then begin
    FreeAndNil(FLazDebugger);
    Exit;
  end;

  FLazDebugger.WorkingDir := AppDir;
  FLazDebugger.FileName   := TestExeName;
  FLazDebugger.Arguments := '';
  //FLazDebugger.ShowConsole := True;
  Result := True;
end;

procedure TTestFpDebugDebugger.CleanAfterTestDone;
begin
  if FLazDebugger = nil then exit;
  try
    FLazDebugger.Release;
    FLazDebugger := nil;
    ClearDebuggerMonitors;
  except
  end;
end;

initialization
  DebugLogger.FindOrRegisterLogGroup('DBG_CMD_ECHO' , True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBG_VERBOSE'  , True  )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS', True )^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('DBG_DISASSEMBLER', True  )^.Enabled := True;

  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_ERRORS', True);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_SEARCH', True)^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_WARNINGS', True)^.Enabled := True;
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_VERBOSE', True);
  DebugLogger.FindOrRegisterLogGroup('FPDBG_DWARF_DATA_WARNINGS', True);


  AppDir := AppendPathDelim(ExtractFilePath(ParamStr(0)));
  if  not(CheckAppDir(AppDir))
  then begin
    AppDir := AppDirStripAppBundle(AppDir);
    if  not(CheckAppDir(AppDir))
    then
      with TSelectDirectoryDialog.Create(nil) do begin
        if Execute then AppDir := AppendPathDelim(FileName);
        Free;
      end;
  end;
  ConfDir := AppDir;
  AppDir := AppendPathDelim(AppDir + 'testapps');

  if DirectoryExistsUTF8(ConfDir+'logs') then
    TestControlSetLogPath(ConfDir+'logs'+DirectorySeparator)
  else if DirectoryExistsUTF8(ConfDir+'log') then
    TestControlSetLogPath(ConfDir+'log'+DirectorySeparator)
  else
    TestControlSetLogPath(ConfDir);

  BuildTestSuites;

finalization

end.

