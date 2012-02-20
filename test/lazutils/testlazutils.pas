{
 Test all with:
     ./runtests --format=plain --suite=TTestLazUtils

 Test specific with:
     ./runtests --format=plain --suite=TestReplaceSubstring
}
unit TestLazUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, LazLogger;

type

  { TTestLazUtils }

  TTestLazUtils = class(TTestCase)
  public
  published
    procedure TestReplaceSubstring;
  end;

implementation

{ TTestLazUTF8 }

procedure TTestLazUtils.TestReplaceSubstring;
begin
  AssertEquals('empty string','',ReplaceSubstring('',1,1,''));
  AssertEquals('empty string insert a','a',ReplaceSubstring('',1,1,'a'));
  AssertEquals('empty string negative startpos','a',ReplaceSubstring('',-1,1,'a'));
  AssertEquals('empty string count too big','a',ReplaceSubstring('',-1,10,'a'));
  AssertEquals('empty string beyond length','a',ReplaceSubstring('',10,10,'a'));
  AssertEquals('whole','a',ReplaceSubstring('a',1,1,'a'));
  AssertEquals('whole','b',ReplaceSubstring('a',1,1,'b'));
  AssertEquals('whole','abc',ReplaceSubstring('a',1,1,'abc'));
  AssertEquals('first char','abcbc',ReplaceSubstring('abc',1,1,'abc'));
  AssertEquals('last char single','aba',ReplaceSubstring('abc',3,1,'a'));
  AssertEquals('last char multi','ababc',ReplaceSubstring('abc',3,1,'abc'));
  AssertEquals('middle char same','abc',ReplaceSubstring('abc',2,1,'b'));
  AssertEquals('middle char single','adc',ReplaceSubstring('abc',2,1,'d'));
  AssertEquals('middle char multi','acdec',ReplaceSubstring('abc',2,1,'cde'));
  AssertEquals('last multi','adef',ReplaceSubstring('abc',2,2,'def'));
end;

initialization
  AddToLazUtilsTestSuite(TTestLazUtils);

end.

