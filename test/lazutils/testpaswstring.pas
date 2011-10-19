program testpaswstring;

{$mode objfpc}{$H+}

uses
  sysutils, Classes, paswstring, cwstring;

procedure WriteStringHex(Str: utf8string);
var
  StrOut: utf8string;
  i: Integer;
begin
  StrOut := '';
  for i := 1 to Length(Str) do
  begin
    StrOut := StrOut + IntToHex(Byte(Str[i]), 2) + ' ';
  end;
  Write(StrOut);
end;

procedure AssertAnsiStringCompareOperation(AMsg, AStr1, AStr2: ansistring; AResult: PtrInt; AExpects: Boolean);
begin
  Write(AMsg, ' ', AStr1, ' <=> ', AStr2);

  if (AResult = 0) then Write(' Result: Equal')
  else Write(' Result: Not Equal');

  if (AResult <> 0) xor AExpects then WriteLn(' Correct')
  else WriteLn(' !Error!');
end;

procedure AssertWideStringCompareOperation(AMsg, AStr1, AStr2: widestring; AResult: PtrInt; AExpects: Boolean);
begin
  AssertAnsiStringCompareOperation(AMsg, AStr1, AStr2, AResult, AExpects);
end;

procedure AssertUnicodeStringCompareOperation(AMsg, AStr1, AStr2: unicodestring; AResult: PtrInt; AExpects: Boolean);
begin
  AssertAnsiStringCompareOperation(AMsg, AStr1, AStr2, AResult, AExpects);
end;

procedure AssertAnsiStringOperation(AMsg, AStr1, AStr2, AStrExpected2: ansistring);
begin
  Write(AMsg, ' ', AStr1, ' => ', AStr2);
  if SysUtils.AnsiCompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AStrExpected2, ' !Error!');
    WriteLn();
{    Write('Got      Len=', Length(AStr2), ' Str=');
    WriteStringHex(AStr2);
    WriteLn('');
    Write('Expected Len=', Length(AStrExpected2), ' Str=');
    WriteStringHex(AStrExpected2);
    WriteLn();
    Write('Orig     Len=', Length(AStr1), ' Str=');
    WriteStringHex(AStr1);
    WriteLn('');}
  end;
  WriteLn();
end;

procedure AssertUTF8StringOperation(AMsg, AStr1, AStr2, AStrExpected2: utf8string);
begin
{  Write(AMsg, ' ', AStr1, ' => ', AStr2);
  if UTF8CompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AStrExpected2, ' !Error!');
    WriteLn();
    Write('Got      Len=', Length(AStr2), ' Str=');
    WriteStringHex(AStr2);
    WriteLn('');
    Write('Expected Len=', Length(AStrExpected2), ' Str=');
    WriteStringHex(AStrExpected2);
    WriteLn();
    Write('Orig     Len=', Length(AStr1), ' Str=');
    WriteStringHex(AStr1);
    WriteLn('');
  end;
  WriteLn();}
end;

procedure AssertWideStringOperation(AMsg: ansistring; AStr1, AStr2, AStrExpected2: widestring);
var
  AnsiStr1, AnsiStr2, AnsiExpected2: ansistring;
begin
  AnsiStr1 := AStr1;
  AnsiStr2 := AStr2;
  AnsiExpected2 := AStrExpected2;
  Write(AMsg, ' ', AnsiStr1, ' => ', AnsiStr2);
  if SysUtils.WideCompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AnsiExpected2, ' !Error!');
    WriteLn();
//    Write('Got      Len=', Length(AStr2), ' Str=');
//    WriteStringHex(AStr2);
//    WriteLn('');
//    Write('Expected Len=', Length(AStrExpected2), ' Str=');
//    WriteStringHex(AStrExpected2);
//    WriteLn();
//    Write('Orig     Len=', Length(AStr1), ' Str=');
//    WriteStringHex(AStr1);
//    WriteLn('');
  end;
  WriteLn();
end;

procedure AssertUnicodeStringOperation(AMsg, AStr1, AStr2, AStrExpected2: unicodestring);
begin
  Write(AMsg, ' ', AStr1, ' => ', AStr2);
  if SysUtils.UnicodeCompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AStrExpected2, ' !Error!');
    WriteLn();
//    Write('Got      Len=', Length(AStr2), ' Str=');
//    WriteStringHex(AStr2);
//    WriteLn('');
//    Write('Expected Len=', Length(AStrExpected2), ' Str=');
//    WriteStringHex(AStrExpected2);
//    WriteLn();
//    Write('Orig     Len=', Length(AStr1), ' Str=');
//    WriteStringHex(AStr1);
//    WriteLn('');
  end;
  WriteLn();
end;

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
end;

{  AssertStringOperationUTF8UpperCase('Latin 01C0 UTF8UpperCase', '', 'ǀǁǂǃǄǅǆǇǈǉǊǋǌǍǎǏ', 'ǀǁǂǃǄǄǄǇǇǇǊǊǊǍǍǏ');
  AssertStringOperationUTF8UpperCase('Latin 01D0 UTF8UpperCase', '', 'ǐǑǒǓǔǕǖǗǘǙǚǛǜǝǞǟ', 'ǏǑǑǓǓǕǕǗǗǙǙǛǛƎǞǞ');
  AssertStringOperationUTF8UpperCase('Latin 01E0 UTF8UpperCase', '', 'ǠǡǢǣǤǥǦǧǨǩǪǫǬǭǮǯ', 'ǠǠǢǢǤǤǦǦǨǨǪǪǬǬǮǮ');
  AssertStringOperationUTF8UpperCase('Unicode 03A0 UTF8UpperCase', '', 'ΠΡΣΤΥΦΧΨΩΪΫάέήί', 'ΠΡΣΤΥΦΧΨΩΪΫΆΈΉΊ');
  AssertStringOperationUTF8UpperCase('Unicode 03C0 UTF8UpperCase', '', 'πρςστυφχψωϊϋόύώϏ', 'ΠΡΣΣΤΥΦΧΨΩΪΫΌΎΏϏ');
  AssertStringOperationUTF8UpperCase('Unicode 0410 UTF8UpperCase', '', 'АБВГДЕЖЗИЙКЛМНОП', 'АБВГДЕЖЗИЙКЛМНОП');
  AssertStringOperationUTF8UpperCase('Unicode 0420 UTF8UpperCase', '', 'РСТУФХЦЧШЩЪЫЬЭЮЯ', 'РСТУФХЦЧШЩЪЫЬЭЮЯ');
  AssertStringOperationUTF8UpperCase('Unicode 0430 UTF8UpperCase', '', 'абвгдежзийклмноп', 'АБВГДЕЖЗИЙКЛМНОП');
  AssertStringOperationUTF8LowerCase('ASCII UTF8LowerCase', '', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ', 'abcdefghijklmnopqrstuwvxyz');
  AssertStringOperationUTF8LowerCase('Latin 00C0 UTF8LowerCase', '', 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ', 'àáâãäåæçèéêëìíîï');
  AssertStringOperationUTF8LowerCase('Latin 00E0 UTF8LowerCase', '', 'àáâãäåæçèéêëìíîï', 'àáâãäåæçèéêëìíîï');
  AssertStringOperationUTF8LowerCase('Unicode 03A0 UTF8LowerCase', '', 'ΠΡΣΤΥΦΧΨΩΪΫάέήί', 'πρστυφχψωϊϋάέήί');
  AssertStringOperationUTF8LowerCase('Unicode 03C0 UTF8LowerCase', '', 'πρςστυφχψωϊϋόύώϏ', 'πρςστυφχψωϊϋόύώϗ');
  AssertStringOperationUTF8LowerCase('Unicode 0410 UTF8LowerCase', '', 'АБВГДЕЖЗИЙКЛМНОП', 'абвгдежзийклмноп');
  AssertStringOperationUTF8LowerCase('Unicode 0420 UTF8LowerCase', '', 'РСТУФХЦЧШЩЪЫЬЭЮЯ', 'рстуфхцчшщъыьэюя');
  AssertStringOperationUTF8LowerCase('Unicode 0430 UTF8LowerCase', '', 'абвгдежзийклмноп', 'абвгдежзийклмноп');}

var
  AnsiStr, AnsiStr2: ansistring;
  UTF8Str, UTF8Str2: utf8string;
  WideStr, WideStr2: widestring;
  UnicodeStr, UnicodeStr2: unicodestring;
begin
  WriteLn('======= CompareStr =======');

  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  AnsiStr2 := AnsiStr;
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareStr 1', AnsiStr, AnsiStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), True);
  //
  UniqueString(AnsiStr2);
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareStr 2', AnsiStr, AnsiStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), True);
  //
  AnsiStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareStr 3', AnsiStr, AnsiStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), False);
  //
  WideStr := 'abcdefghijklmnopqrstuwvxyz';
  WideStr2 := WideStr;
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 1', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), True);
  //
  UniqueString(AnsiStr2);
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 1', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), True);
  //
  WideStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 1', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), False);
  //
  //UnicodeStr := 'abcdefghijklmnopqrstuwvxyz';
  //AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareStr', UnicodeStr, SysUtils.UnicodeCompareStr(UnicodeStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  WriteLn('======= CompareText =======');
  WriteLn('======= UpperCase =======');
  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  AssertAnsiStringOperation('SysUtils.AnsiUpperCase', AnsiStr, SysUtils.AnsiUpperCase(AnsiStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  WideStr := 'abcdefghijklmnopqrstuwvxyz';
  AssertWideStringOperation('SysUtils.WideUpperCase', WideStr, SysUtils.WideUpperCase(WideStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  UnicodeStr := 'abcdefghijklmnopqrstuwvxyz';
  AssertUnicodeStringOperation('SysUtils.UnicodeUpperCase', UnicodeStr, SysUtils.UnicodeUpperCase(UnicodeStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  WriteLn('======= LowerCase =======');
  //TestUTF8LowerCase();
  {$ifdef Windows}
  WriteLn('Please press enter to continue');
  readln;
  {$endif}
end.

