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
  Classes, SysUtils, fpcunit, LConvEncoding, LazLogger, testglobals, LCLProc;

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

  procedure Test(CodePageName: string; const CP2UTF8,UTF82CP: TConvertEncodingFunction);
  var
    c: Char;
    AsUTF8, Back: string;
    l: integer;
  begin
    for c:=#1 to High(Char) do begin
      AsUTF8:=CP2UTF8(c);
      if AsUTF8='' then
        AssertEquals('CodePage '+CodePageName+' to UTF8 creates empty string for character #'+IntToStr(ord(c)),true,false);
      Back:=UTF82CP(AsUTF8);
      if Back<>c then
        AssertEquals('CodePage '+CodePageName+' ('+IntToStr(ord(c))+') to UTF8 ('+dbgs(UTF8CharacterToUnicode(PChar(AsUTF8),l))+') and back differ for character #'+IntToStr(ord(c)),DbgStr(c),dbgstr(Back));
    end;
  end;

begin
  Test('ISO_8859_1',@ISO_8859_1ToUTF8,@UTF8ToISO_8859_1);
  Test('ISO_8859_2',@ISO_8859_2ToUTF8,@UTF8ToISO_8859_2);
  Test('ISO_8859_15',@ISO_8859_15ToUTF8,@UTF8ToISO_8859_15);
  Test('437',@CP437ToUTF8,@UTF8ToCP437);
  Test('850',@CP850ToUTF8,@UTF8ToCP850);
  Test('852',@CP852ToUTF8,@UTF8ToCP852);
  Test('866',@CP866ToUTF8,@UTF8ToCP866);
  Test('874',@CP874ToUTF8,@UTF8ToCP874);
  Test('1250',@CP1250ToUTF8,@UTF8ToCP1250);
  Test('1251',@CP1251ToUTF8,@UTF8ToCP1251);
  Test('1252',@CP1252ToUTF8,@UTF8ToCP1252);
  Test('1253',@CP1253ToUTF8,@UTF8ToCP1253);
  Test('1254',@CP1254ToUTF8,@UTF8ToCP1254);
  Test('1255',@CP1255ToUTF8,@UTF8ToCP1255);
  Test('1256',@CP1256ToUTF8,@UTF8ToCP1256);
  Test('1257',@CP1257ToUTF8,@UTF8ToCP1257);
  Test('1258',@CP1258ToUTF8,@UTF8ToCP1258);
end;

initialization
  AddToLazUtilsTestSuite(TTestLConvEncoding);

end.

