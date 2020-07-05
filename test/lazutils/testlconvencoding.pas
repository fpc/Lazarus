{
 Test all with:
     ./runtests --format=plain --suite=TTestLConvEncoding

 Test specific with:
     ./runtests --format=plain --suite=Test_CP_UTF8_CP
}
unit TestLConvEncoding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, LConvEncoding, LazLogger, testglobals, FileProcs,
  LazUTF8;

type

  { TTestLConvEncoding }

  TTestLConvEncoding = class(TTestCase)
  public
  published
    procedure Test_CP_UTF8_CP;
  end;

implementation

{ TTestLConvEncoding }

procedure TTestLConvEncoding.Test_CP_UTF8_CP;

  procedure Test(CodePageName: string);
  var
    c: Char;
    AsUTF8, Back: string;
    l: integer;
    Encoded: boolean;
  begin
    for c:=#1 to High(Char) do begin
      AsUTF8:=ConvertEncodingToUTF8(c,CodePageName,Encoded);
      if AsUTF8='' then
        AssertEquals('CodePage '+CodePageName+' to UTF8 creates empty string for character #'+IntToStr(ord(c)),true,false);
      Back:=ConvertEncodingFromUTF8(AsUTF8,CodePageName,Encoded);
      if Back<>c then
        AssertEquals('CodePage '+CodePageName+' ('+IntToStr(ord(c))+') to UTF8 ('+dbgs(UTF8CodepointToUnicode(PChar(AsUTF8),l))+') and back differ for character #'+IntToStr(ord(c)),DbgStr(c),dbgstr(Back));
    end;
  end;

begin
  Test(EncodingCPIso1);
  Test(EncodingCPIso2);
  Test(EncodingCPIso15);
  Test(EncodingCP437);
  Test(EncodingCP850);
  Test(EncodingCP852);
  Test(EncodingCP866);
  Test(EncodingCP874);
  Test(EncodingCP1250);
  Test(EncodingCP1251);
  Test(EncodingCP1252);
  Test(EncodingCP1253);
  Test(EncodingCP1254);
  Test(EncodingCP1255);
  Test(EncodingCP1256);
  Test(EncodingCP1257);
  Test(EncodingCP1258);
  Test(EncodingCPKOI8R);
  Test(EncodingCPKOI8U);
  Test(EncodingCPKOI8RU);
end;

initialization
  AddToLazUtilsTestSuite(TTestLConvEncoding);

end.

