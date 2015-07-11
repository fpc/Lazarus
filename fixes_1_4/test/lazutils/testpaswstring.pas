program testpaswstring;

{$mode objfpc}{$H+}

uses
  sysutils, Classes, paswstring, lazutf8;

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
  Write(AMsg, ' '{, AStr1, ' <=> ', AStr2});

  if (AResult = 0) then Write(' Result:     Equal')
  else Write(' Result: Not Equal');

  if (AResult <> 0) xor AExpects then WriteLn(' Correct')
  else WriteLn(' !Error!');
end;

procedure AssertWideStringCompareOperation(AMsg: ansistring; AStr1, AStr2: widestring; AResult: PtrInt; AExpects: Boolean);
begin
  AssertAnsiStringCompareOperation(AMsg, '', '', AResult, AExpects);
end;

procedure AssertUnicodeStringCompareOperation(AMsg: ansistring; AStr1, AStr2: unicodestring; AResult: PtrInt; AExpects: Boolean);
begin
  AssertAnsiStringCompareOperation(AMsg, '', '', AResult, AExpects);
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
begin
  Write(AMsg, ' '{, AnsiStr1, ' => ', AnsiStr2});
  if SysUtils.WideCompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' !Error!');
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
  Write(AMsg, ' '{, AStr1, ' => ', AStr2});
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

procedure TestCompareStr;
begin
  WriteLn('======= CompareStr =======');

  // For AnsiString we can only compare ASCII reliably

  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  AnsiStr2 := AnsiStr;
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareStr 1', AnsiStr, AnsiStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), True);
  //
  UniqueString(AnsiStr2);
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareStr 2', AnsiStr, AnsiStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), True);
  //
  AnsiStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareStr 3', AnsiStr, AnsiStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), False);

  // No UTF-8 routines in the RTL yet

  // Now WideString

  WideStr := 'abcdefghijklmnopqrstuwvxyz';
  WideStr2 := WideStr;
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 1', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), True);
  //
  UniqueString(AnsiStr2);
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 2', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), True);
  //
  WideStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 3', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), False);
  //
  WideStr := 'АБВГДЕЖЗИЙКЛМНОП';
  WideStr2 := WideStr;
  UniqueString(AnsiStr2);
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 4', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), True);
  //
  WideStr2 := 'абвгдежзийклмноп';
  AssertWideStringCompareOperation('SysUtils.WideCompareStr 5', WideStr, WideStr2, SysUtils.WideCompareStr(WideStr, WideStr2), False);

  // UnicodeString

  UnicodeStr := 'abcdefghijklmnopqrstuwvxyz';
  UnicodeStr2 := UnicodeStr;
  AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareStr 1', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareStr(UnicodeStr, UnicodeStr2), True);
  //
  UniqueString(UnicodeStr2);
  AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareStr 2', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareStr(UnicodeStr, UnicodeStr2), True);
  //
  UnicodeStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareStr 3', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareStr(UnicodeStr, UnicodeStr2), False);
  //
  UnicodeStr := 'АБВГДЕЖЗИЙКЛМНОП';
  UnicodeStr2 := UnicodeStr;
  UniqueString(UnicodeStr2);
  AssertWideStringCompareOperation('SysUtils.UnicodeCompareStr 4', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareStr(UnicodeStr, UnicodeStr2), True);
  //
  UnicodeStr2 := 'абвгдежзийклмноп';
  AssertWideStringCompareOperation('SysUtils.UnicodeCompareStr 5', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareStr(UnicodeStr, UnicodeStr2), False);
end;

procedure TestCompareText;
begin
  WriteLn('======= CompareText =======');

  // For AnsiString we can only compare ASCII reliably

  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  AnsiStr2 := AnsiStr;
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareText 1', AnsiStr, AnsiStr2, SysUtils.AnsiCompareText(AnsiStr, AnsiStr2), True);
  //
  UniqueString(AnsiStr2);
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareText 2', AnsiStr, AnsiStr2, SysUtils.AnsiCompareText(AnsiStr, AnsiStr2), True);
  //
  AnsiStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareText 3', AnsiStr, AnsiStr2, SysUtils.AnsiCompareText(AnsiStr, AnsiStr2), True);
  //
  AnsiStr2 := 'ZZZZZ';
  AssertAnsiStringCompareOperation('SysUtils.AnsiCompareText 4', AnsiStr, AnsiStr2, SysUtils.AnsiCompareText(AnsiStr, AnsiStr2), False);

  // No UTF-8 routines in the RTL yet

  // Now WideString

  WideStr := 'abcdefghijklmnopqrstuwvxyz';
  WideStr2 := WideStr;
  AssertWideStringCompareOperation('SysUtils.WideCompareText 1', WideStr, WideStr2, SysUtils.WideCompareText(WideStr, WideStr2), True);
  //
  UniqueString(AnsiStr2);
  AssertWideStringCompareOperation('SysUtils.WideCompareText 2', WideStr, WideStr2, SysUtils.WideCompareText(WideStr, WideStr2), True);
  //
  WideStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertWideStringCompareOperation('SysUtils.WideCompareText 3', WideStr, WideStr2, SysUtils.WideCompareText(WideStr, WideStr2), True);
  //
  WideStr2 := 'абвгдежзийклмноп';
  AssertWideStringCompareOperation('SysUtils.WideCompareText 4', WideStr, WideStr2, SysUtils.WideCompareText(WideStr, WideStr2), False);
  //
  WideStr := UTF8ToUTF16('АБВГДЕЖЗИЙКЛМНОП');
  WideStr2 := WideStr;
  UniqueString(AnsiStr2);
  AssertWideStringCompareOperation('SysUtils.WideCompareText 5', WideStr, WideStr2, SysUtils.WideCompareText(WideStr, WideStr2), True);
  //
  WideStr2 := UTF8ToUTF16('абвгдежзийклмноп');
  AssertWideStringCompareOperation('SysUtils.WideCompareText 6', WideStr, WideStr2, SysUtils.WideCompareText(WideStr, WideStr2), True);

  // UnicodeString

  UnicodeStr := 'abcdefghijklmnopqrstuwvxyz';
  UnicodeStr2 := UnicodeStr;
  AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareText 1', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareText(UnicodeStr, UnicodeStr2), True);
  //
  UniqueString(UnicodeStr2);
  AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareText 2', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareText(UnicodeStr, UnicodeStr2), True);
  //
  UnicodeStr2 := 'ABCDEFGHIJKLMNOPQRSTUWVXYZ';
  AssertUnicodeStringCompareOperation('SysUtils.UnicodeCompareText 3', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareText(UnicodeStr, UnicodeStr2), True);
  //
  UnicodeStr2 := 'абвгдежзийклмноп';
  AssertWideStringCompareOperation('SysUtils.UnicodeCompareText 4', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareText(UnicodeStr, UnicodeStr2), False);
  //
  UnicodeStr := UTF8ToUTF16('АБВГДЕЖЗИЙКЛМНОП');
  UnicodeStr2 := UnicodeStr;
  UniqueString(UnicodeStr2);
  AssertWideStringCompareOperation('SysUtils.UnicodeCompareText 5', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareText(UnicodeStr, UnicodeStr2), True);
  //
  UnicodeStr2 := UTF8ToUTF16('абвгдежзийклмноп');
  AssertWideStringCompareOperation('SysUtils.UnicodeCompareText 6', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareText(UnicodeStr, UnicodeStr2), True);
end;

procedure TestAssignment();
begin
  WriteLn('======= String Assignment =======');
  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  UnicodeStr := AnsiStr;
  UnicodeStr2 := UTF8ToUTF16(AnsiStr);
  AssertUnicodeStringCompareOperation('UnicodeStr := AnsiStr 1', UnicodeStr, UnicodeStr2, SysUtils.UnicodeCompareStr(UnicodeStr, UnicodeStr2), True);
  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  UnicodeStr := UTF8ToUTF16(ansistr);
  AnsiStr2 := UnicodeStr;
  AssertUnicodeStringCompareOperation('AnsiStr := UnicodeStr 1', UnicodeStr, UnicodeStr2, SysUtils.AnsiCompareStr(AnsiStr, AnsiStr2), True);
end;

procedure TestCompareUpperCase();
begin
  WriteLn('======= UpperCase =======');
  AnsiStr := 'abcdefghijklmnopqrstuwvxyz';
  AssertAnsiStringOperation('SysUtils.AnsiUpperCase', AnsiStr, SysUtils.AnsiUpperCase(AnsiStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  WideStr := 'abcdefghijklmnopqrstuwvxyz';
  AssertWideStringOperation('SysUtils.WideUpperCase', WideStr, SysUtils.WideUpperCase(WideStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  UnicodeStr := 'abcdefghijklmnopqrstuwvxyz';
  AssertUnicodeStringOperation('SysUtils.UnicodeUpperCase', UnicodeStr, SysUtils.UnicodeUpperCase(UnicodeStr), 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
end;

begin
  TestCompareStr();
  TestCompareText();
  TestAssignment();
  TestCompareUpperCase();
  {$ifdef Windows}
  WriteLn('Please press enter to continue');
  readln;
  {$endif}
end.

