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
  Classes, SysUtils, strutils, fpcunit, testregistry, process, UTF8Process,
  InterfaceBase, FileUtil,
  TestGlobals;

type

  { TLpkTest }

  TLpkTest = class(TTestCase)
  private
    FPath: string;
  public
    constructor Create(const APath: string; const ATestName: string); overload;
    class function GetExtension: string; virtual;
    class function CreateSuiteFromFile(const aName, APath: string): TTestSuite; virtual;
    class function CreateSuiteFromDirectory(const AName, ABasePath: string): TTestSuite;
  published
    procedure TestCompile;
  end;

  { TLpiTest }

  TLpiTest= class(TLpkTest)
  private
    procedure RunScript;
  public
    class function GetExtension: string; override;
    class function CreateSuiteFromFile(const aName, APath: string): TTestSuite; override;
  published
    procedure TestRun;
  end; 
  
implementation

var
  LazarusDir: string;
  ComponentsDir: String;
  ExamplesDir: string;
  CTExamplesDir: string;
  LCLTestDir: string;
  ScriptEngine: string;
  
procedure InitDirectories;
begin
  LazarusDir := ExpandFileNameUTF8(ExtractFilePath(ParamStrUTF8(0)) + '../');
  ComponentsDir := SetDirSeparators(LazarusDir + 'components/');
  ExamplesDir := LazarusDir + 'examples' + PathDelim;
  CTExamplesDir := SetDirSeparators(ComponentsDir + 'codetools/examples/');
  LCLTestDir := LazarusDir + 'lcl' + PathDelim + 'tests' + PathDelim;
  ScriptEngine := 'C:\Program Files\AutoHotkey\AutoHotKey.exe';
end;

function GetScriptFileName(const LpiFileName: string): string;
begin
  Result := AppendPathDelim(ProgramDirectory) +
              ExtractFileNameOnly(LpiFileName) +'.ahk';
end;

constructor TLpkTest.Create(const APath: string; const ATestName: string);
begin
  inherited CreateWithName(ATestName);
  FPath := APath;
end;

class function TLpkTest.GetExtension: string;
begin
  Result := '.lpk';
end;

class function TLpkTest.CreateSuiteFromDirectory(const AName,
  ABasePath: string): TTestSuite;

  procedure SearchDirectory(const ADirectory: string);
  var
    RelativePath: string;
    SearchMask: string;
    FileInfo: TSearchRec;
  begin
    SearchMask := ABasePath+ADirectory + '*';
    if FindFirstUTF8(SearchMask,faAnyFile,FileInfo)=0 then begin
      repeat
        // skip special directory entries
        if (FileInfo.Name='.') or (FileInfo.Name='..') then continue;
        
        RelativePath := ADirectory+ FileInfo.Name;
        if RightStr(FileInfo.Name,4)=GetExtension then
          Result.AddTest(CreateSuiteFromFile(RelativePath, ABasePath+RelativePath))
        else if (FileInfo.Attr and faDirectory=faDirectory) then
          SearchDirectory(AppendPathDelim(RelativePath));
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;
  
begin
  Result := TTestSuite.Create(AName);
  SearchDirectory('')
end;

class function TLpkTest.CreateSuiteFromFile(const AName,
  APath: string): TTestSuite;
begin
  Result := TTestSuite.Create(AnsiReplaceStr(AName, DirectorySeparator, '/'));
  Result.AddTest(Create(APath, 'TestCompile'));
end;

procedure TLpkTest.TestCompile;
var
  LazBuildPath: string;
  LazBuild: TProcessUTF8;
  OutputLines: TStrings;
  CmdLine: string;
begin
  LazBuildPath := LazarusDir + 'lazbuild' + GetExeExt;
  AssertTrue(LazBuildPath + ' does not exist', FileExistsUTF8(LazBuildPath));
  LazBuild := TProcessUTF8.Create(nil);
  OutputLines := nil;
  try
    {$IFDEF windows}
    LazBuild.Options := [poNewConsole, poUsePipes];
    {$ELSE}
    LazBuild.Options := [poNoConsole, poUsePipes];
    {$ENDIF}
    LazBuild.ShowWindow := swoHIDE;
    CmdLine:=LazBuildPath;
    if Compiler<>'' then
      CmdLine:=Cmdline + ' --compiler='+Compiler;
    if PrimaryConfigPath<>'' then
      CmdLine := CmdLine + ' --pcp='+PrimaryConfigPath;
    Cmdline := Cmdline + ' --ws=' + LCLPlatformDirNames[WidgetSet.LCLPlatform];
    Cmdline := Cmdline + ' -B ' + FPath;
    LazBuild.CommandLine := CmdLine;
    LazBuild.CurrentDirectory := ExtractFileDir(FPath);
    LazBuild.Execute;
    OutputLines := ReadOutput(LazBuild);
    if LazBuild.Running then begin
      LazBuild.Terminate(99);
    end;
    if LazBuild.ExitStatus<>0 then
      Fail(format('Compilation failed: ExitCode=%d%s%s',
        [LazBuild.ExitStatus, LineEnding, AnsiToUtf8(OutputLines.Text)]));
  finally
    LazBuild.Free;
    OutputLines.Free;
  end;
end;

class function TLpiTest.GetExtension: string;
begin
  Result := '.lpi';
end;

class function TLpiTest.CreateSuiteFromFile(const AName,
  APath: string): TTestSuite;
{$IFDEF win32}
var
  AhkFileName: String;
{$ENDIF}
begin
  Result := inherited CreateSuiteFromFile(AName, APath);
{$IFDEF win32}
  AhkFileName := GetScriptFileName(APath);
  if FileExistsUTF8(AhkFileName) then
    Result.AddTest(TLpiTest.Create(APath, 'TestRun'));
{$ELSE}
  {$NOTE scripting is only available on win32}
{$ENDIF}
end;

procedure TLpiTest.RunScript;
var
  ScriptProcess : TProcessUTF8;
begin
  AssertTrue('ScriptEngine "' + ScriptEngine + '" does not exist.',
    FileExistsUTF8(ScriptEngine));
  ScriptProcess := TProcessUTF8.Create(nil);
  try
    ScriptProcess.CommandLine := ScriptEngine + ' ' + GetScriptFileName(FPath);
    ScriptProcess.Execute;
    ScriptProcess.WaitOnExit;
  finally
    ScriptProcess.Free;
  end;
end;

procedure TLpiTest.TestRun;
var
  TestProcess : TProcessUTF8;
  ExeName: string;
begin
  ExeName := ChangeFileExt(FPath, GetExeExt);
  AssertTrue(ExeName + 'does not exist.', FileExistsUTF8(ExeName));
  TestProcess := TProcessUTF8.Create(nil);
  try
    TestProcess.CommandLine := ExeName;
    TestProcess.Execute;
    RunScript;
    TestProcess.WaitOnExit;
  finally
    TestProcess.Free;
  end;
end;

procedure InitializeTestSuites;
var
  ATestSuite: TTestSuite;
begin
  // Create testsuite for projects
  ATestSuite := TTestSuite.Create('Projects');
  ATestSuite.AddTest(
    TLpiTest.CreateSuiteFromDirectory('Examples', ExamplesDir));
  ATestSuite.AddTest(
    TLpiTest.CreateSuiteFromDirectory('Codetools Examples', CTExamplesDir));
  ATestSuite.AddTest(
    TLpiTest.CreateSuiteFromDirectory('LCL test', LCLTestDir));
  GetTestRegistry.AddTest(ATestSuite);
  
  // Create testsuite for packages
  ATestSuite := TTestSuite.Create('Packages');
  ATestSuite.AddTest(
    TLpkTest.CreateSuiteFromDirectory('Components', ComponentsDir));
  ATestSuite.AddTest(
    TLpkTest.CreateSuiteFromDirectory('Examples', ExamplesDir));
  GetTestRegistry.AddTest(ATestSuite);
end;

initialization
  InitDirectories;
  InitializeTestSuites;
end.

