unit BugTestCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, process, fileutil, fpcunit, testutils, testregistry;

type

  { TBugTestCase }

  TBugTestCase= class(TTestCase)
  private
    FPath: string;
    FProjectFile: string;
  protected
    procedure SetUp; override; 
    procedure TearDown; override;
  public
    constructor Create(APath, ATestName: string); reintroduce;
    class function CreateSuite(Path: string) : TTestSuite;
  published
    procedure Compile;
    procedure RunTestApp;
    procedure CompareExpectations;
    procedure HeaptrcLog;
  end; 
  
var
  Compiler: string;

implementation

var
  BufferedOutput: TMemoryStream; // a global variable is not nice, but it works.
  
const
  READ_BYTES = 2048;

procedure ReadOutput(AProcess:TProcess);
var
  BytesRead: Integer;
  n: Integer;
begin
  BytesRead := 0;
  BufferedOutput.Clear;
  while AProcess.Running do
  begin
    // make sure we have room
    BufferedOutput.SetSize(BytesRead + READ_BYTES);

    // try reading it
    n := AProcess.Output.Read((BufferedOutput.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n)
    else
      // no data, wait 100 ms
      Sleep(100);
  end;
  // read last part
  repeat
    // make sure we have room
    BufferedOutput.SetSize(BytesRead + READ_BYTES);
    // try reading it
    n := AProcess.Output.Read((BufferedOutput.Memory + BytesRead)^, READ_BYTES);
    if n > 0 then
      Inc(BytesRead, n);
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
  LazarusDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../../');
  LazBuildPath := LazarusDir + 'lazbuild' + GetExeExt;
  AssertTrue(LazBuildPath + ' does not exist', FileExists(LazBuildPath));
  LazBuild := TProcess.Create(nil);
  try
    {$IFDEF win32}
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
    TestProcess.CommandLine := ExeName;
    TestProcess.Options := [poUsePipes];
    TestProcess.Execute;
    //RunScript;
    ReadOutput(TestProcess);
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
  ExpectedFileName := AppendPathDelim(FPath) + 'Expected.txt';
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

procedure TBugTestCase.SetUp; 
begin
end; 

procedure TBugTestCase.TearDown; 
begin
end;

constructor TBugTestCase.Create(APath, ATestName: string);
begin
  CreateWithName(ATestName);
  FPath := APath;
  FProjectFile := FindProjectFile(FPath);
  writeln(FProjectFile);
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
  ProgPath := ExtractFilePath(ParamStr(0));
  if FindFirst(ProgPath+'*', faAnyFile, SearchRec)=0 then
    repeat
      if (SearchRec.Attr=faDirectory) and
         (SearchRec.Name<>'.') and (SearchRec.Name<>'..')
      then
        GetTestRegistry.AddTest(
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

