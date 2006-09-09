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
  public
    constructor Create(const APath: string; const ATestName: string); overload;
    class function Suite(const APath: string): TTestSuite;
    class function ExamplesSuite: TTestSuite;
  published
    procedure TestCompile;
    procedure TestRun;
  end; 

implementation

var
  LazarusDir: string;
  ExamplesDir: string;
  
procedure InitDirectories;
begin
  LazarusDir := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../');
  ExamplesDir := LazarusDir + 'examples' + PathDelim;
end;

constructor TLpiTest.Create(const APath: string; const ATestName: string);
begin
  inherited CreateWithName(ATestName);
  FPath := APath;
end;

class function TLpiTest.Suite(const APath: string): TTestSuite;
begin
  Result := TTestSuite.Create(APath);
  Result.AddTest(TLpiTest.Create(APath, 'TestCompile'));
end;

class function TLpiTest.ExamplesSuite: TTestSuite;
var
  SearchMask: String;
  FileInfo: TSearchRec;
begin
  Result := TTestSuite.Create('Examples');
  SearchMask := ExamplesDir + '*.lpi';
  writeln(stdout, SearchMask+LineEnding);
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
    LazBuild.Options := [poNewConsole];
    LazBuild.ShowWindow := swoHIDE;
    LazBuild.CommandLine := LazBuildPath + ' ' + FPath;
    LazBuild.CurrentDirectory := ExtractFileDir(FPath);
    LazBuild.Execute;
    LazBuild.WaitOnExit;
    AssertEquals('Compilation failed: ExitCode', 0, LazBuild.ExitStatus);
  finally
    LazBuild.Free;
  end;
end;

procedure TLpiTest.TestRun;
begin

end;

initialization
  InitDirectories;
  GetTestRegistry.AddTest(TLpiTest.ExamplesSuite);
end.

