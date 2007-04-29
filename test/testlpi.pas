{ $Id$}
{ Copyright (C) 2006 Vincent Snijders

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit TestLpi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, process, FileUtil,
  TestGlobals;

type

  { TLpiTest }

  TLpiTest= class(TTestCase)
  private
    FPath: string;
    procedure RunScript;
  public
    constructor Create(const APath: string; const ATestName: string); overload;
    class function CreateProjectSuite(const aName, APath: string): TTestSuite;
    class function CreateSuiteFromDirectory(const AName, ABasePath: string): TTestSuite;
  published
    procedure TestCompile;
    procedure TestRun;
  end; 

implementation

var
  LazarusDir: string;
  ExamplesDir: string;
  CTExamplesDir: string;
  LCLTestDir: string;
  ScriptEngine: string;
  
procedure InitDirectories;
begin
  LazarusDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../');
  ExamplesDir := LazarusDir + 'examples' + PathDelim;
  CTExamplesDir := SetDirSeparators(LazarusDir + 'components/codetools/examples/');
  LCLTestDir := LazarusDir + 'lcl' + PathDelim + 'tests' + PathDelim;
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
  AssertTrue('ScriptEngine "' + ScriptEngine + '" does not exist.',
    FileExists(ScriptEngine));
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

class function TLpiTest.CreateProjectSuite(const AName,
  APath: string): TTestSuite;
var
  AhkFileName: String;
begin
  Result := TTestSuite.Create(AName);
  Result.AddTest(TLpiTest.Create(APath, 'TestCompile'));
{$IFDEF win32}
  AhkFileName := GetScriptFileName(APath);
  if FileExists(AhkFileName) then
    Result.AddTest(TLpiTest.Create(APath, 'TestRun'));
{$ELSE}
  {$NOTE scripting is only available on win32}
{$ENDIF}
end;

class function TLpiTest.CreateSuiteFromDirectory(const AName,
  ABasePath: string): TTestSuite;

  procedure SearchDirectory(const ADirectory: string);
  var
    RelativePath: string;
    SearchMask: string;
    FileInfo: TSearchRec;
  begin
    SearchMask := ABasePath+ADirectory + '*';
    if FindFirst(SearchMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // skip special directory entries
        if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
        
        RelativePath := ADirectory+ FileInfo.Name;
        if RightStr(FileInfo.Name,4)='.lpi' then
          Result.AddTest(CreateProjectSuite(RelativePath, ABasePath+RelativePath))
        else if (FileInfo.Attr and faDirectory=faDirectory) then
          SearchDirectory(AppendPathDelim(RelativePath));
      until FindNext(FileInfo)<>0;
    end;
    FindClose(FileInfo);
  end;
begin
  Result := TTestSuite.Create(AName);
  SearchDirectory('')
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
    {$IFDEF windows}
    LazBuild.Options := [poNewConsole];
    {$ELSE}
    LazBuild.Options := [poNoConsole];
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
  GetTestRegistry.AddTest(
    TLpiTest.CreateSuiteFromDirectory('Examples', ExamplesDir));
  GetTestRegistry.AddTest(
    TLpiTest.CreateSuiteFromDirectory('Codetools Examples', CTExamplesDir));
  GetTestRegistry.AddTest(
    TLpiTest.CreateSuiteFromDirectory('LCL test', LCLTestDir));
end.

