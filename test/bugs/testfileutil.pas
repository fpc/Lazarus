unit testfileutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals,
  FileUtil;

type

  { TTestFileUtil }

  TTestFileUtil= class(TTestCase)
  published
    procedure TestFileIsExecutable;
  end; 

implementation

{ TTestFileUtil }

procedure TTestFileUtil.TestFileIsExecutable;
  procedure DoTest(const AFileName: string; Expected: boolean);
  begin
    AssertEquals(AFileName, Expected, FileIsExecutable(AFileName));
  end;
begin
  DoTest(ParamStr(0),true);
  // a directory is not an executable file
  DoTest(ExtractFileDir(ParamStr(0)), false);
end;

initialization
  // Maybe this test case should be moved to another testsuite, e.g. lcl test
  AddToBugsTestSuite(TTestSuite.Create(TTestFileUtil, 'TestFileUtil'));
end.

