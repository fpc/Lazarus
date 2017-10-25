unit TestLazFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, LazLogger, LazFileUtils;

type

  { TTestLazFileUtils }

  TTestLazFileUtils = class(TTestCase)
  published
    procedure TestResolveDots;
  end;

implementation

{ TTestLazFileUtils }

procedure TTestLazFileUtils.TestResolveDots;

  procedure t(AFilename, Expected: string; SetPathDelims: boolean = true);
  var
    Actual: String;
  begin
    if SetPathDelims then begin
      ForcePathDelims(AFileName);
      ForcePathDelims(Expected);
    end;
    Actual:=ResolveDots(AFilename);
    AssertEquals('TTestLazFileUtils.TestResolveDots File='''+AFilename+'''',Expected,Actual);
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
end;

initialization
  AddToLazUtilsTestSuite(TTestLazFileUtils);
end.

