{
 Test all with:
     ./runtests --format=plain --suite=TTestLazXML

 Test specific with:
     ./runtests --format=plain --suite=TestStrToXMLValue
     ./runtests --format=plain --suite=TestXMLValueToStr
}
unit TestLazXML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, laz2_DOM;

type

  { TTestLazXML }

  TTestLazXML = class(TTestCase)
  public
  published
    procedure TestStrToXMLValue;
    procedure TestXMLValueToStr;
  end;

implementation

{ TTestLazXML }

procedure TTestLazXML.TestStrToXMLValue;
begin
  AssertEquals('Empty string','',StrToXMLValue(''));
  AssertEquals('Short string','a',StrToXMLValue('a'));
  AssertEquals('String with #0','',StrToXMLValue(#0));
  AssertEquals('String with &','&amp;',StrToXMLValue('&'));
  AssertEquals('String with <','&lt;',StrToXMLValue('<'));
  AssertEquals('String with >','&gt;',StrToXMLValue('>'));
  AssertEquals('String with ''','&apos;',StrToXMLValue(''''));
  AssertEquals('String with "','&quot;',StrToXMLValue('"'));
  AssertEquals('String mix 1','&lt;a&gt;&quot;',StrToXMLValue('<a>'#0'"'));
  AssertEquals('String mix 2','abc',StrToXMLValue('abc'));
end;

procedure TTestLazXML.TestXMLValueToStr;
begin
  AssertEquals('Empty string','',XMLValueToStr(''));
  AssertEquals('Short string','a',XMLValueToStr('a'));
  AssertEquals('Short string',#0,XMLValueToStr(#0));
  AssertEquals('String with &','&',XMLValueToStr('&amp;'));
  AssertEquals('String with <','<',XMLValueToStr('&lt;'));
  AssertEquals('String with >','>',XMLValueToStr('&gt;'));
  AssertEquals('String with ''','''',XMLValueToStr('&apos;'));
  AssertEquals('String with "','"',XMLValueToStr('&quot;'));
  AssertEquals('String mix 1','<a>"',XMLValueToStr('&lt;a&gt;&quot;'));
  AssertEquals('String mix 2','abc',XMLValueToStr('abc'));
end;

initialization
  AddToLazUtilsTestSuite(TTestLazXML);

end.

