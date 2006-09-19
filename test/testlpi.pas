unit TestLpi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, process,
  FileUtil;

type

  { TLpiTest }

  TLpiTest= class(TTestCase)
  private
    FPath: string;
    procedure RunScript;
  public
    constructor Create(const APath: string; const ATestName: string); overload;
    class function Suite(const APath: string): TTestSuite;
    class function ExamplesSuite: TTestSuite;
  published
    procedure TestCompile;
    procedure TestRun;
  end; 

var
  Compiler: string;

implementation

var
  LazarusDir: string;
  ExamplesDir: string;
  ScriptEngine: string;
  
procedure InitDirectories;
begin
  LazarusDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../');
  ExamplesDir := LazarusDir + 'examples' + PathDelim;
  ScriptEngine := 'C:\Program Files\AutoHotkey\AutoHotKey.exe';
end;

function GetScriptFileName(const LpiFileName: string): string;
begin
  Result := AppendPathDelim(ProgramDirectory) +
              ExtractFileNameOnly(LpiFileName) +'.ahk';
end;

procedure TLpiTest.RunScript;
var
  ScriptProcess : TProcess;
begin
  ScriptProcess := TProcess.Create(nil);
  try
    ScriptProcess.CommandLine := ScriptEngine + ' ' + GetScriptFileName(FPath);
    ScriptProcess.Execute;
    ScriptProcess.WaitOnExit;
  finally
    ScriptProcess.Free;
  end;
end;

constructor TLpiTest.Create(const APath: string; const ATestName: string);
begin
  inherited CreateWithName(ATestName);
  FPath := APath;
end;

class function TLpiTest.Suite(const APath: string): TTestSuite;
var
  AhkFileName: String;
begin
  Result := TTestSuite.Create(APath);
  Result.AddTest(TLpiTest.Create(APath, 'TestCompile'));
{$IFDEF win32}
  AhkFileName := GetScriptFileName(APath);
  if FileExists(AhkFileName) then
    Result.AddTest(TLpiTest.Create(APath, 'TestRun'));
{$ELSE}
  {$NOTE scripting is only available on windows}
{$ENDIF}
end;

class function TLpiTest.ExamplesSuite: TTestSuite;
var
  SearchMask: String;
  FileInfo: TSearchRec;
begin
  Result := TTestSuite.Create('Examples');
  SearchMask := ExamplesDir + '*.lpi';
  if FindFirst(SearchMask,faAnyFile,FileInfo)=0 then begin
    repeat
      if RightStr(FileInfo.Name,4)='.lpi' then
        Result.AddTest(Suite(ExamplesDir + FileInfo.Name));
    until FindNext(FileInfo)<>0;
  end;
  FindClose(FileInfo);
end;

procedure TLpiTest.TestCompile;
var
  LazBuildPath: string;
  LazBuild: TProcess;
begin
  LazBuildPath := LazarusDir + 'lazbuild' + GetExeExt;
  AssertTrue(LazBuildPath + ' does not exist', FileExists(LazBuildPath));
  LazBuild := TProcess.Create(nil);
  try
    {$IFDEF win32}
    LazBuild.Options := [poNewConsole];
    {$ENDIF}
    LazBuild.ShowWindow := swoHIDE;
    LazBuild.CommandLine := LazBuildPath;
    if Compiler<>'' then
      LazBuild.CommandLine := LazBuild.CommandLine + ' --compiler='+Compiler;
    LazBuild.CommandLine := LazBuild.CommandLine + ' ' + FPath;
    LazBuild.CurrentDirectory := ExtractFileDir(FPath);
    LazBuild.Execute;
    LazBuild.WaitOnExit;
    AssertEquals('Compilation failed: ExitCode', 0, LazBuild.ExitStatus);
  finally
    LazBuild.Free;
  end;
end;

procedure TLpiTest.TestRun;
var
  TestProcess : TProcess;
  ExeName: string;
begin
  ExeName := ChangeFileExt(FPath, GetExeExt);
  AssertTrue(ExeName + 'does not exist.', FileExists(ExeName));
  TestProcess := TProcess.Create(nil);
  try
    TestProcess.CommandLine := ExeName;
    TestProcess.Execute;
    RunScript;
    TestProcess.WaitOnExit;
  finally
    TestProcess.Free;
  end;
end;

initialization
  InitDirectories;
  GetTestRegistry.AddTest(TLpiTest.ExamplesSuite);
end.

