program TestUnicode;

{$mode objfpc}{$H+}

uses
  sysutils, lazutf8;

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

procedure AssertStringOperation(AMsg, AStr1, AStr2, AStrExpected2: utf8string);
begin
  Write(AMsg, ' ', AStr1, ' => ', AStr2);
  if UTF8CompareStr(AStr2, AStrExpected2) <> 0 then
  begin
    Write(' Expected ', AStrExpected2, ' !Error!');
    WriteLn();
    Write('Got      Len=', Length(AStr2),' ');
    WriteStringHex(AStr2);
    WriteLn('');
    Write('Expected Len=', Length(AStrExpected2),' ');
    WriteStringHex(AStrExpected2);
    WriteLn();
  end;
  WriteLn();
end;

procedure AssertStringOperationUTF8UpperCase(AMsg, ALocale, AStr1, AStrExpected2: utf8string);
begin
  AssertStringOperation(AMsg, AStr1, UTF8UpperCase(AStr1, ALocale), AStrExpected2);
end;

procedure AssertStringOperationUTF8LowerCase(AMsg, ALocale, AStr1, AStrExpected2: utf8string);
begin
  AssertStringOperation(AMsg, AStr1, UTF8LowerCase(AStr1, ALocale), AStrExpected2);
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

procedure TestUTF8UpperCase;
var
  lStartTime, lTimeDiff: TDateTime;
  Str: UTF8String;
  i: Integer;
begin
  // ASCII
  AssertStringOperationUTF8UpperCase('ASCII UTF8UpperCase', '', 'abcdefghijklmnopqrstuwvxyz', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ');
  // Latin
  AssertStringOperationUTF8UpperCase('Portuguese UTF8UpperCase 1', '', 'Ç/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'Ç/Ç Ã/Ã Õ/Õ Á/Á É/É Í/Í Ó/Ó Ú/Ú Ü/Ü À/À Â/Â Ê/Ê Î/Î Ô/Ô Û/Û');
  AssertStringOperationUTF8UpperCase('French UTF8UpperCase 1', '', 'À/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'À/À Â/Â Æ Ç/Ç É/É È/È Ê/Ê Ë/Ë Î/Î Ï/Ï Ô/Ô Œ Ù/Ù Û/Û Ü/Ü Ÿ/Ÿ');
  AssertStringOperationUTF8UpperCase('Polish UTF8UpperCase 1', '', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  AssertStringOperationUTF8UpperCase('Polish UTF8UpperCase 2', '', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  AssertStringOperationUTF8UpperCase('German UTF8UpperCase 1', '', 'Ä/ä,Ö/ö,Ü/ü,ß', 'Ä/Ä,Ö/Ö,Ü/Ü,SS');
  // Turkish
  AssertStringOperationUTF8UpperCase('Turkish UTF8UpperCase 1', 'tu', 'abcçdefgğhııijklmnoöprsştuüvyz', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ');
  AssertStringOperationUTF8UpperCase('Turkish UTF8UpperCase 2', 'tu', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ');
  // Cyrillic
  AssertStringOperationUTF8UpperCase('Russian UTF8UpperCase 1', '', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ');
  AssertStringOperationUTF8UpperCase('Russian UTF8UpperCase 2', '', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ');
  // What shouldnt change
  AssertStringOperationUTF8UpperCase('Chinese UTF8UpperCase 1', '', '名字叫嘉英，嘉陵江的嘉，英國的英', '名字叫嘉英，嘉陵江的嘉，英國的英');

  // Performance test
  lStartTime := Now;
  for i := 0 to 9999 do
  begin
    Str := UTF8UpperCase('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
    Str := Str + UTF8UpperCase('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
    Str := Str + UTF8UpperCase('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
  end;
  lTimeDiff := Now - lStartTime;
  WriteLn('UpperCase Performance test took: ', DateTimeToMilliseconds(lTimeDiff), ' ms');
end;

procedure TestUTF8LowerCase;
var
  i: Integer;
  lStartTime, lTimeDiff: TDateTime;
  Str: UTF8String;
begin
  // ASCII
  AssertStringOperationUTF8LowerCase('ASCII UTF8LowerCase', '', 'ABCDEFGHIJKLMNOPQRSTUWVXYZ', 'abcdefghijklmnopqrstuwvxyz');
  // Latin
  AssertStringOperationUTF8LowerCase('Portuguese UTF8LowerCase 1', '', 'Ç/ç Ã/ã Õ/õ Á/á É/é Í/í Ó/ó Ú/ú Ü/ü À/à Â/â Ê/ê Î/î Ô/ô Û/û', 'ç/ç ã/ã õ/õ á/á é/é í/í ó/ó ú/ú ü/ü à/à â/â ê/ê î/î ô/ô û/û');
  AssertStringOperationUTF8LowerCase('French UTF8LowerCase 1', '', 'À/à Â/â æ Ç/ç É/é È/è Ê/ê Ë/ë Î/î Ï/ï Ô/ô œ Ù/ù Û/û Ü/ü Ÿ/ÿ', 'à/à â/â æ ç/ç é/é è/è ê/ê ë/ë î/î ï/ï ô/ô œ ù/ù û/û ü/ü ÿ/ÿ');
  AssertStringOperationUTF8LowerCase('Polish UTF8LowerCase 1', '', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertStringOperationUTF8LowerCase('Polish UTF8LowerCase 2', '', 'AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ', 'aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
  AssertStringOperationUTF8LowerCase('German UTF8LowerCase 1', '', 'Ä/ä,Ö/ö,Ü/ü,ß', 'ä/ä,ö/ö,ü/ü,ß');
  // Turkish
  AssertStringOperationUTF8LowerCase('Turkish UTF8LowerCase 1', 'tu', 'abcçdefgğhııijklmnoöprsştuüvyz', 'abcçdefgğhııijklmnoöprsştuüvyz');
  AssertStringOperationUTF8LowerCase('Turkish UTF8LowerCase 2', 'tu', 'ABCÇDEFGĞHIIİJKLMNOÖPRSŞTUÜVYZ', 'abcçdefgğhııijklmnoöprsştuüvyz');
  // Cyrillic
  AssertStringOperationUTF8LowerCase('Russian UTF8LowerCase 1', '', 'АБВЕЁЖЗКЛМНОПРДЙГ СУФХЦЧШЩЪЫЬЭЮЯИТ', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  AssertStringOperationUTF8LowerCase('Russian UTF8LowerCase 2', '', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит', 'абвеёжзклмнопрдйг суфхцчшщъыьэюяит');
  // What shouldnt change
  AssertStringOperationUTF8LowerCase('Chinese UTF8LowerCase 1', '', '名字叫嘉英，嘉陵江的嘉，英國的英', '名字叫嘉英，嘉陵江的嘉，英國的英');

  // Performance test
  lStartTime := Now;
  for i := 0 to 99999 do
  begin
    //Str := UTF8LowerCaseMattias('abcdefghijklmnopqrstuwvxyz');
    //Str := UTF8LowerCaseMattias('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
    //Str := UTF8LowerCaseMattias('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
    //Str := UTF8LowerCaseMattias('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
    //Str := UTF8LowerCaseMattias('АБВЕЁЖЗКЛМНОПРДЙГ');
    Str := UTF8LowerCaseMattias('名字叫嘉英，嘉陵江的嘉，英國的英');
  end;
  lTimeDiff := Now - lStartTime;
  WriteLn('Mattias LowerCase Performance test took: ', DateTimeToMilliseconds(lTimeDiff), ' ms');

  lStartTime := Now;
  for i := 0 to 99999 do
  begin
    //Str := UTF8LowerCase('abcdefghijklmnopqrstuwvxyz');
    //Str := UTF8LowerCase('ABCDEFGHIJKLMNOPQRSTUWVXYZ');
    //Str := UTF8LowerCase('aąbcćdeęfghijklłmnńoóprsśtuwyzźż');
    //Str := UTF8LowerCase('AĄBCĆDEĘFGHIJKLŁMNŃOÓPRSŚTUWYZŹŻ');
    //Str := UTF8LowerCase('АБВЕЁЖЗКЛМНОПРДЙГ');
    Str := UTF8LowerCase('名字叫嘉英，嘉陵江的嘉，英國的英');
  end;
  lTimeDiff := Now - lStartTime;
  WriteLn('LowerCase Performance test took: ', DateTimeToMilliseconds(lTimeDiff), ' ms');
end;

begin
  WriteLn('======= UpperCase =======');
  TestUTF8UpperCase();
  WriteLn('======= LowerCase =======');
  TestUTF8LowerCase();
end.

