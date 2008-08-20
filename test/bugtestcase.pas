{ $Id$}
{ Copyright (C) 2007 Vincent Snijders

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
unit BugTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, process, fileutil, fpcunit, testregistry,
  TestGlobals;

type

  { TBugTestCase }

  TBugTestCase= class(TTestCase)
  private
    FPath: string;
    FProjectFile: string;
  public
    constructor Create(APath, ATestName: string); reintroduce;
    class function CreateSuite(Path: string) : TTestSuite;
  published
    procedure Compile;
    procedure RunTestApp;
    procedure CompareExpectations;
    procedure HeaptrcLog;
  end; 
  
implementation

var
 // a global variable to pass information between tests is not nice, but it works
  RunOutput: TStringList;
  
function FindProjectFile(APath: string):string;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(AppendPathDelim(APath)+'*.lpi', faAnyFile, SearchRec)=0 then begin
    repeat
      if ExtractFileExt(SearchRec.Name)='.lpi' then
        Result := SearchRec.Name;
    until (Length(Result)>0) or (FindNext(SearchRec)<>0);
  end;
  FindClose(SearchRec);
  if length(Result)>0 then
    Result := AppendPathDelim(APath) + Result;
end;

procedure TBugTestCase.Compile;
var
  LazBuildPath: string;
  LazBuild: TProcess;
  LazarusDir: String;
begin
  AssertTrue('Project file '+ FProjectFile + ' does not exist',
    FileExists(FProjectFile));
  LazarusDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../');
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
    LazBuild.CommandLine := LazBuild.CommandLine + ' ' + FProjectFile;
    LazBuild.CurrentDirectory := FPath;
    LazBuild.Execute;
    LazBuild.WaitOnExit;
    AssertEquals('Compilation failed: ExitCode', 0, LazBuild.ExitStatus);
  finally
    LazBuild.Free;
  end;
end;

procedure TBugTestCase.RunTestApp;
var
  TestProcess : TProcess;
  ExeName: string;
  OutputLines: TStringList;
begin
  ExeName := ChangeFileExt(FProjectFile, GetExeExt);
  AssertTrue(ExeName + 'does not exist.', FileExists(ExeName));
  TestProcess := TProcess.Create(nil);
  try
    TestProcess.CommandLine := ExeName + ' --runtest';
    TestProcess.Options := [poUsePipes];
    TestProcess.Execute;
    try
      OutputLines := ReadOutput(TestProcess);
      try
        RunOutput.Assign(OutputLines);
      finally
        OutputLines.Free;
      end;
      AssertFalse('TestProcess did not auto-terminate', TestProcess.Running);
    finally
      TestProcess.Terminate(0);
    end;
  finally
    TestProcess.Free;
  end;
end;

procedure TBugTestCase.CompareExpectations;
var
  ExpectedFileName: string;
  ExpectedLines: TStrings;
  ActualLines: TStrings;
  MinLineCount: integer;
  i: integer;
begin
  ExpectedFileName := AppendPathDelim(FPath) + 'expected.txt';
  AssertTrue('File missing: '+ExpectedFileName, FileExists(ExpectedFileName));
  ExpectedLines := nil;
  ActualLines := nil;
  try
    ExpectedLines := TStringList.Create;
    ExpectedLines.LoadFromFile(ExpectedFileName);
    ActualLines  := TStringList.Create;
    ActualLines.Assign(RunOutput);
    MinLineCount := min(ExpectedLines.Count, ActualLines.Count);
    for i := 0 to MinLineCount - 1 do begin
      AssertEquals('Output difference on line '+IntToStr(i+1),
        ExpectedLines[i], ActualLines[i]);
    end;
    AssertEquals('Difference in line count',
      ExpectedLines.Count, ActualLines.Count);
  finally
    ExpectedLines.Free;
    ActualLines.Free;
  end;
end;

procedure TBugTestCase.HeaptrcLog;
begin

end;

constructor TBugTestCase.Create(APath, ATestName: string);
begin
  CreateWithName(ATestName);
  FPath := APath;
  FProjectFile := FindProjectFile(FPath);
end;

class function TBugTestCase.CreateSuite(Path: string): TTestSuite;
var
  Directory: string;
begin
  Directory := ExtractFileName(Path);
  Result := TTestSuite.Create(Directory);
  Result.AddTest(Create(Path, 'Compile'));
  Result.AddTest(Create(Path, 'RunTestApp'));
  Result.AddTest(Create(Path, 'CompareExpectations'));
  Result.AddTest(Create(Path, 'HeaptrcLog'));
end;

procedure GatherTests;
var
  ProgPath: string;
  SearchRec: TSearchRec;
begin
  ProgPath := ExtractFilePath(ParamStr(0)) + 'bugs' + pathdelim;
  if FindFirst(ProgPath+'*', faAnyFile, SearchRec)=0 then
    repeat
      if (SearchRec.Attr and (faDirectory + faHidden)=faDirectory) and
         (SearchRec.Name<>'.') and (SearchRec.Name<>'..') and
         (SearchRec.Name<>'.svn')
      then
        BugsTestSuite.AddTest(
          TBugTestCase.CreateSuite(ProgPath+SearchRec.Name));
    until FindNext(SearchRec)<>0;
  FindClose(SearchRec);
end;

initialization
  GatherTests;
  RunOutput := TStringList.Create;
  
finalization
  FreeAndNil(RunOutput);
end.

