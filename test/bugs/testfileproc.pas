unit TestFileProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals,
  FileProcs;

type

  { TTestFileUtil }

  { TTestFileProc }

  TTestFileProc= class(TTestCase)
  published
    procedure TestFileIsExecutable;
    procedure TestTrimFileName;
    procedure TestCreateRelativePath;
  end;

implementation

{ TTestFileProc }

procedure TTestFileProc.TestFileIsExecutable;
  procedure DoTest(const AFileName: string; Expected: boolean);
  begin
    AssertEquals(AFileName, Expected, FileIsExecutable(AFileName));
  end;
begin
  DoTest(ParamStrUTF8(0),true);
  // a directory is not an executable file
  DoTest(ExtractFileDir(ParamStrUTF8(0)), false);
end;

procedure TTestFileProc.TestTrimFileName;
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
  DoTest(' a ','a');
end;

procedure TTestFileProc.TestCreateRelativePath;

  procedure DoTest(Filename, BaseDirectory, Expected: string;
    UsePointDirectory: boolean = false);
  begin
    DoDirSeparators(Filename);
    DoDirSeparators(BaseDirectory);
    DoDirSeparators(Expected);
    AssertEquals('CreateRelativePath(File='+Filename+',Base='+BaseDirectory+')',
      Expected,
      CreateRelativePath(Filename,BaseDirectory,UsePointDirectory));
  end;

begin
  DoTest('/a','/a','');
  DoTest('/a','/a/','');
  DoTest('/a/b','/a/b','');
  DoTest('/a/b','/a/b/','');
  DoTest('/a','/a/','');
  DoTest('/a','','/a');
  DoTest('/a/b','/a','b');
  DoTest('/a/b','/a/','b');
  DoTest('/a/b','/a//','b');
  DoTest('/a','/a/b','../');
  DoTest('/a','/a/b/','../');
  DoTest('/a','/a/b//','../');
  DoTest('/a/','/a/b','../');
  DoTest('/a','/a/b/c','../../');
  DoTest('/a','/a/b//c','../../');
  DoTest('/a','/a//b/c','../../');
  DoTest('/a','/a//b/c/','../../');
  DoTest('/a','/b','/a');
end;

initialization
  // TODO: Maybe this test case should be moved to another testsuite, e.g. codetools test
  AddToBugsTestSuite(TTestSuite.Create(TTestFileProc, 'TestFileProc'));
end.

