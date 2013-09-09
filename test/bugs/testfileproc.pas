{
 Test all with:
     ./runtests --format=plain --suite=TTestFileProc

 Test specific with:
     ./runtests --format=plain --suite=TestFileIsExecutable
     ./runtests --format=plain --suite=TestTrimFileName
     ./runtests --format=plain --suite=TestCreateRelativePath
}
unit TestFileProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, LazFileUtils, FileProcs;

type

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
    ForcePathDelims(AFileName);
    ForcePathDelims(Expected);
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
    ForcePathDelims(Filename);
    ForcePathDelims(BaseDirectory);
    ForcePathDelims(Expected);
    AssertEquals('CreateRelativePath(File='+Filename+',Base='+BaseDirectory+')',
      Expected,
      CreateRelativePath(Filename,BaseDirectory,UsePointDirectory));
  end;

begin
  DoTest('/a','/a','');
  DoTest('/a','/a','.',true);
  DoTest('/a','/a/','');
  DoTest('/a/b','/a/b','');
  DoTest('/a/b','/a/b/','');
  DoTest('/a','/a/','');
  DoTest('/a','','/a');
  DoTest('/a/b','/a','b');
  DoTest('/a/b','/a/','b');
  DoTest('/a/b','/a//','b');
  DoTest('/a','/a/b','..');
  DoTest('/a','/a/b/','..');
  DoTest('/a','/a/b//','..');
  DoTest('/a/','/a/b','..');
  DoTest('/a','/a/b/c','../..');
  DoTest('/a','/a/b//c','../..');
  DoTest('/a','/a//b/c','../..');
  DoTest('/a','/a//b/c/','../..');
  DoTest('/a','/b','/a');
  DoTest('~/bin','/','~/bin');
  DoTest('$(HOME)/bin','/','$(HOME)/bin');
end;

initialization
  // TODO: Maybe this test case should be moved to another testsuite, e.g. codetools test
  AddToBugsTestSuite(TTestSuite.Create(TTestFileProc, 'TestFileProc'));
end.

