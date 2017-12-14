{
 Test all with:
     ./runtests --format=plain --suite=TTestLazFileUtils

 Test specific with:
     ./runtests --format=plain --suite=TTestLazFileUtils.TestResolveDots
}
unit TestLazFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, LazLogger, LazFileUtils, LazUTF8;

type

  { TTestLazFileUtils }

  TTestLazFileUtils = class(TTestCase)
  published
    procedure TestResolveDots;
    procedure TestFileIsExecutable;
    procedure TestTrimFileName;
    procedure TestCreateRelativePath;
  end;

implementation

{ TTestLazFileUtils }

procedure TTestLazFileUtils.TestResolveDots;

  procedure t(AFilename, Expected: string; SetPathDelims: boolean = true);
  var
    Actual: String;
  begin
    if SetPathDelims then begin
      ForcePathDelims(Expected);
      {$IFDEF Windows}
      // test once with / and once with \
      Actual:=ResolveDots(AFilename);
      AssertEquals('TTestLazFileUtils.TestResolveDots File='''+AFilename+'''',Expected,Actual);
      {$ENDIF}
      ForcePathDelims(AFileName);
    end;
    Actual:=ResolveDots(AFilename);
    AssertEquals('TTestLazFileUtils.TestResolveDots File='''+AFilename+'''',Expected,Actual);

    // check that a resolved file can no further be resolved
    Actual:=ResolveDots(Actual);
    if AFilename<>Expected then
      AssertEquals('TTestLazFileUtils.TestResolveDots File='''+Actual+'''',Expected,Actual);
  end;

begin
  t('a','a');
  t(' ',' ');
  t('.','.');
  t('./','.');
  t('.//','.');
  t('..','..');
  t('bla/..','.');
  t('bla//..','.');
  t('foo/bla//..','foo');
  t('bla/../','.');
  t('bla//..//','.');
  t('foo/../bar','bar');
  t('foo//..//bar','bar');
  t('bla/../.','.');
  t('bla//..//.','.');
  t('foo/bar/../too','foo/too');
  t('foo//bar//..//too','foo/too');
  t('./.z','.z');
  t('.//.z','.z');
  t('foo/.z','foo/.z');
  t('foo//.z','foo/.z');
  t('./.','.');
  t('.//.','.');
  t('././.','.');
  t('.//.//.','.');
  t('foo/bar/./../too','foo/too');
  t('foo//bar//.//..//too','foo/too');
  {$IFDEF Unix}
  t('/.','/');
  t('//.','/');
  t('/..','/');
  t('//..','/');
  t('/./..','/');
  t('//.//..','/');
  {$ENDIF}
  {$IFDEF Windows}
  t('C:\.','C:\');
  t('C:.\.','C:.');
  t('C:..','C:..');
  t('C:\..','C:\');
  t('C:foo\bla\..','C:foo');
  t('C:foo\bla\\..','C:foo');
  t('C:\foo\bla\..','C:\foo');
  t('C:\\foo\\bla\\..','C:\foo');
  t('\\','\\');
  t('\\..','\\');
  t('\\?\','\\?\');
  t('\\?\\\\','\\?\\\\');
  {$ENDIF}
end;

procedure TTestLazFileUtils.TestFileIsExecutable;
  procedure DoTest(const AFileName: string; Expected: boolean);
  begin
    AssertEquals(AFileName, Expected, FileIsExecutable(AFileName));
  end;
begin
  DoTest(ParamStrUTF8(0),true);
  // a directory is not an executable file
  DoTest(ExtractFileDir(ParamStrUTF8(0)), false);
end;

procedure TTestLazFileUtils.TestTrimFileName;
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
  DoTest('a ','a');
  DoTest('.','.');
  DoTest('a/','a/');
  DoTest('a/.','a/');
  DoTest('./a','a');
  DoTest('././a','a');
  DoTest('a/..','.');
  DoTest('a/b/..','a/');
  DoTest('a/../b','b');
  DoTest('a/b/../c','a/c');
  DoTest('a/b/../../c','c');
  DoTest('a/./b','a/b');
  DoTest('a/.//b','a/b');
  DoTest('a//b','a/b');
  DoTest('a//./b','a/b');
end;

procedure TTestLazFileUtils.TestCreateRelativePath;

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
  {$IFDEF MSWindows}
  DoTest('D:\a\b\c.pas','D:\a\d\','..\b\c.pas');
  {$ENDIF}
end;

initialization
  AddToLazUtilsTestSuite(TTestLazFileUtils);
end.

