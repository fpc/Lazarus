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
    procedure TestTrimFileName;
    procedure TestFileCopyPreserveTime;
  end;

implementation

{ TTestFileUtil }

procedure TTestFileUtil.TestFileIsExecutable;
  procedure DoTest(const AFileName: string; Expected: boolean);
  begin
    AssertEquals(AFileName, Expected, FileIsExecutable(AFileName));
  end;
begin
  DoTest(ParamStrUTF8(0),true);
  // a directory is not an executable file
  DoTest(ExtractFileDir(ParamStrUTF8(0)), false);
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
  DoTest('test.pas.bak', 'test.pas');
  DirName := AppendPathDelim('testdir');
  DoTest(DirName + 'test.pas', DirName + 'test');
  DoTest(DirName + 'test.pas.bak', DirName + 'test.pas');
end;

procedure TTestFileUtil.TestTrimFileName;
  procedure DoTest(AFileName, Expected: string);
  begin
    DoDirSeparators(AFileName);
    DoDirSeparators(Expected);
    AssertEquals(AFileName, Expected, TrimFilename(AFileName));
  end;
begin
{$ifdef windows}
  DoTest('c:\LazarusDir\..\dir\','c:\dir\');
{$endif}
  DoTest('$(LazarusDir)\..\dir\','$(LazarusDir)\..\dir\');
end;

procedure TTestFileUtil.TestFileCopyPreserveTime;
// test for issue 11912 and 12317
var
  File1, File2: string;
  Result: boolean;
  procedure CreateTestFile(FileName: string);
  var
    f: text;
  begin
    assign(f, Utf8ToAnsi(FileName));
    rewrite(f);
    writeln(f, 'TestFileCopyPreserveTime');
    close(f);
    FileSetDateUTF8(FileName, 960054793);
  end;

begin
  File1 := AnsiToUtf8(SysUtils.GetTempFileName);
  CreateTestFile(File1);
  File2 :=  AnsiToUtf8(SysUtils.GetTempFileName);
  try
    Result := CopyFile(File1,File2,true);
    AssertTrue('File copy failed', Result);
    AssertTrue('Copied file does not exist', FileExistsUTF8(File2));
    AssertEquals('Time not preserved', FileAgeUTF8(File1), FileAgeUTF8(File2));
  finally
    if FileExistsUTF8(File1) then
      DeleteFileUTF8(File1);
    if FileExistsUTF8(File2) then
      DeleteFileUTF8(File2);
  end;
end;

initialization
  // Maybe this test case should be moved to another testsuite, e.g. lcl test
  AddToBugsTestSuite(TTestSuite.Create(TTestFileUtil, 'TestFileUtil'));
end.

