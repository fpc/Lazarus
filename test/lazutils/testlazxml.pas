{
 Test all with:
     ./runtests --format=plain --suite=TTestLazXML

 Test specific with:
     ./runtests --format=plain --suite=TestStrToXMLValue
     ./runtests --format=plain --suite=TestXMLValueToStr
     ./runtests --format=plain --suite=TestTranslateUTF8Chars
}
unit TestLazXML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testglobals, laz2_DOM, laz2_xmlutils;

type

  { TTestLazXML }

  TTestLazXML = class(TTestCase)
  public
  published
    procedure TestStrToXMLValue;
    procedure TestXMLValueToStr;
    procedure TestTranslateUTF8Chars;
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
  AssertEquals('Short string a','a',XMLValueToStr('a'));
  AssertEquals('Short string #0',#0,XMLValueToStr(#0));
  AssertEquals('Short string abc','abc',XMLValueToStr('abc'));
  AssertEquals('String with &','&',XMLValueToStr('&amp;'));
  AssertEquals('String with <','<',XMLValueToStr('&lt;'));
  AssertEquals('String with >','>',XMLValueToStr('&gt;'));
  AssertEquals('String with ''','''',XMLValueToStr('&apos;'));
  AssertEquals('String with "','"',XMLValueToStr('&quot;'));
  AssertEquals('String mix <a>"','<a>"',XMLValueToStr('&lt;a&gt;&quot;'));
end;

procedure TTestLazXML.TestTranslateUTF8Chars;

  procedure T(Title, s, SrcChars, DstChars, Expected: string);
  var
    h: String;
  begin
    h:=s;
    TranslateUTF8Chars(h,SrcChars,DstChars);
    if h=Expected then exit;
    AssertEquals(Title+': s="'+s+'" SrcChars="'+SrcChars+'" DstChars="'+DstChars+'"',Expected,h);
  end;

begin
  T('empty','','','','');
  T('nop','a','b','b','a');
  T('a to b','a','a','b','b');
  T('switch a,b','abaa','ab','ba','babb');
  T('delete a','a','a','','');
  T('delete a','aba','a','','b');
end;

initialization
  AddToLazUtilsTestSuite(TTestLazXML);

end.

