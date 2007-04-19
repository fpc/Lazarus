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

const
  // Maximal number of bytes read from stream
  READ_BYTES = 2048;
  // Maximal run time for a test program
  TIME_OUT = 30;

var
  BufferedOutput: TMemoryStream; // a global variable is not nice, but it works.
  
procedure ReadOutput(AProcess:TProcess);
var
  BytesRead: Integer;
  n: Integer;
  EndTime: TDateTime;
begin
  BytesRead := 0;
  BufferedOutput.Clear;
  EndTime := Now + TIME_OUT / (24 * 60 * 60);
  while AProcess.Running and (Now<EndTime) do
  begin
    // make sure we have room
    BufferedOutput.SetSize(BytesRead + READ_BYTES);

    // try reading it
    {$IFNDEF VER2_0}
    if AProcess.Output.NumBytesAvailable>0 then begin
      n := AProcess.Output.Read((BufferedOutput.Memory + BytesRead)^, READ_BYTES);
      Inc(BytesRead, n)
    end
    else
      // no data, wait 100 ms
      Sleep(100);
    {$ELSE}
    n := AProcess.Output.Read((BufferedOutput.Memory + BytesRead)^, READ_BYTES);
    if n>0 then
      Inc(BytesRead, n)
    else
      // no data, wait 100 ms
      Sleep(100);
    {$ENDIF}
  end;
  // read last part
  repeat
    // make sure we have room
    BufferedOutput.SetSize(BytesRead + READ_BYTES);
    // try reading it
    {$IFNDEF VER2_0}
    if AProcess.Output.NumBytesAvailable>0 then begin
      n := AProcess.Output.Read((BufferedOutput.Memory + BytesRead)^, READ_BYTES);
      Inc(BytesRead, n);
    end
    else
      n := 0;
    {$ELSE}
    n := AProcess.Output.Read((BufferedOutput.Memory + BytesRead)^, READ_BYTES);
    if n>0 then
      Inc(BytesRead, n);
    {$ENDIF}
  until n <= 0;
  BufferedOutput.SetSize(BytesRead);
end;

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
begin
  ExeName := ChangeFileExt(FProjectFile, GetExeExt);
  AssertTrue(ExeName + 'does not exist.', FileExists(ExeName));
  TestProcess := TProcess.Create(nil);
  try
    TestProcess.CommandLine := ExeName + ' --runtest';
    TestProcess.Options := [poUsePipes];
    TestProcess.Execute;
    try
      ReadOutput(TestProcess);
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
    BufferedOutput.Position := 0;
    ActualLines  := TStringList.Create;
    ActualLines.LoadFromStream(BufferedOutput);
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
  BufferedOutput := TMemoryStream.Create;
  
finalization
  FreeAndNil(BufferedOutput);
end.

