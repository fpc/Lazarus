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

  function r(const s: string; StartPos, Count: SizeInt;
             const Insertion: string): string;
  begin
    Result:=s;
    ReplaceSubstring(Result,StartPos,Count,Insertion);
  end;

begin
  AssertEquals('empty string','',r('',1,1,''));
  AssertEquals('empty string insert a','a',r('',1,1,'a'));
  AssertEquals('empty string negative startpos','a',r('',-1,1,'a'));
  AssertEquals('empty string count too big','a',r('',-1,10,'a'));
  AssertEquals('empty string beyond length','a',r('',10,10,'a'));
  AssertEquals('whole','a',r('a',1,1,'a'));
  AssertEquals('whole','b',r('a',1,1,'b'));
  AssertEquals('whole','abc',r('a',1,1,'abc'));
  AssertEquals('first char','abcbc',r('abc',1,1,'abc'));
  AssertEquals('last char single','aba',r('abc',3,1,'a'));
  AssertEquals('last char multi','ababc',r('abc',3,1,'abc'));
  AssertEquals('middle char same','abc',r('abc',2,1,'b'));
  AssertEquals('middle char single','adc',r('abc',2,1,'d'));
  AssertEquals('middle char multi','acdec',r('abc',2,1,'cde'));
  AssertEquals('last multi','adef',r('abc',2,2,'def'));
end;

initialization
  AddToLazUtilsTestSuite(TTestLazUtils);

end.

