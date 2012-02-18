{
 Test all with:
     ./runtests --format=plain --suite=TTestLazXML

 Test specific with:
     ./runtests --format=plain --suite=TestStrToXMLValue
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
  AssertEquals('String mix 1','abc',StrToXMLValue('abc'));
end;

initialization
  AddToLazUtilsTestSuite(TTestLazXML);

end.

