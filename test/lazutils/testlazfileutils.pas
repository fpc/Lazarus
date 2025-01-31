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
  t('foo/bla//..','foo/');
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
  t('/bla/..','/');
  t('/..','/');
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
  DoTest('c:\..','c:\'); //yes Windows actually treats it as that.
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
  DoTest('a/b/c/../..','a/');
  DoTest('a/./b','a/b');
  DoTest('a/.//b','a/b');
  DoTest('a//b','a/b');
  DoTest('a//./b','a/b');
  DoTest('/a/b/../..','/');
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

  // testing an absolute path and its relative variant
  procedure DoTest2(Filename, BaseDirectory, Expected: string;
    UsePointDirectory: boolean = false);
  begin
    // absolute paths
    DoTest(Filename, BaseDirectory, Expected, UsePointDirectory);
    // relative paths
    if Filename = Expected then
      Delete(Expected, 1, 1);
    Delete(Filename, 1, 1);
    Delete(BaseDirectory, 1, 1);
    DoTest(Filename, BaseDirectory, Expected);
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

  // single period in file
  DoTest2('/dir/file.'    , '/dir', 'file.'    );
  DoTest2('/dir/.file'    , '/dir', '.file'    );
  DoTest2('/dir/.file.'   , '/dir', '.file.'   );
  DoTest2('/dir/file.name', '/dir', 'file.name');
  // single period in directory
  DoTest2('/dir/dir2./file.txt'    , '/dir', 'dir2./file.txt'    );
  DoTest2('/dir/.dir2/file.txt'    , '/dir', '.dir2/file.txt'    );
  DoTest2('/dir/.dir2./file.txt'   , '/dir', '.dir2./file.txt'   );
  DoTest2('/dir/dir2.name/file.txt', '/dir', 'dir2.name/file.txt');
  // double period in file
  DoTest2('/dir/file..'    , '/dir', 'file..'    );
  DoTest2('/dir/..file'    , '/dir', '..file'    );
  DoTest2('/dir/..file..'  , '/dir', '..file..'  );
  DoTest2('/dir/file..name', '/dir', 'file..name');
  // double period in directory
  DoTest2('/dir/dir2../file.txt'    , '/dir', 'dir2../file.txt'    );
  DoTest2('/dir/..dir2/file.txt'    , '/dir', '..dir2/file.txt'    );
  DoTest2('/dir/..dir2../file.txt'  , '/dir', '..dir2../file.txt'  );
  DoTest2('/dir/dir2..name/file.txt', '/dir', 'dir2..name/file.txt');
  // triple period (no special purpose)
  DoTest2('/dir/dir/...' , '/dir', 'dir/...');
  DoTest2('/dir/.../file', '/dir', '.../file');
  // illegal input must return the original value
  DoTest2('/dir/../file', '/dir'       , '/dir/../file');
  DoTest2('/dir/..'     , '/dir'       , '/dir/..'     );
  DoTest2('/dir/file'   , '/dir/../dir', '/dir/file'   );
  DoTest2('/dir'        , '/dir/..'    , '/dir'        );
  DoTest2('/../dir/file', '/../dir'    , '/../dir/file');
  DoTest2('/dir/file'   , '/../'       , '/dir/file'   );

  {$IFDEF MSWindows}
  DoTest('D:\a\b\c.pas','D:\a\d\','..\b\c.pas');
  // different delimiters
  DoTest('D:\dir../file', 'D:\dir..', 'file');
  DoTest('D:\dir\../file', 'D:\dir', 'D:\dir\../file');
  {$ENDIF}
end;

initialization
  AddToLazUtilsTestSuite(TTestLazFileUtils);
end.

