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
    procedure TestExtractFileNameWithoutExt;
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

procedure TTestFileUtil.TestExtractFileNameWithoutExt;
var
  DirName : string;
  procedure DoTest(const FileName, Expected: string);
  begin
    AssertEquals(FileName, Expected, ExtractFileNameWithoutExt(FileName));
  end;
begin
  DoTest('test.pas', 'test');
  DoTest('test.pas.bak', 'test');
  DirName := AppendPathDelim('testdir');
  DoTest(DirName + 'test.pas', DirName + 'test');
  DoTest(DirName + 'test.pas.bak', DirName + 'test');
end;

initialization
  // Maybe this test case should be moved to another testsuite, e.g. lcl test
  AddToBugsTestSuite(TTestSuite.Create(TTestFileUtil, 'TestFileUtil'));
end.

