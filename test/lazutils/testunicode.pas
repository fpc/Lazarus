program TestUnicode;

{$mode objfpc}{$H+}

uses
  lazutf8;

procedure AssertStringOperation(AMsg, AStr1, AStr2, AStrExpected2: utf8string);
begin
  Write(AMsg, ' ', AStr1, ' => ', AStr2, ' Expected ', AStrExpected2);
  if UTF8CompareStr(AStr1, AStr2) <> 0 then Write(' !Error!');
  WriteLn();
end;

begin
  // Turkish
  AssertStringOperation('Turkish UTF8UpperCase 1', 'abcçdefgğhıijklmnoöprsştuüvyz', UTF8UpperCase('abcçdefgğhıijklmnoöprsştuüvyz'), 'ABCÇDEFGĞHIİJKLMNOÖPRSŞTUÜVYZ');
  AssertStringOperation('Turkish UTF8UpperCase 2', 'ABCÇDEFGĞHIİJKLMNOÖPRSŞTUÜVYZ', UTF8UpperCase('ABCÇDEFGĞHIİJKLMNOÖPRSŞTUÜVYZ'), 'ABCÇDEFGĞHIİJKLMNOÖPRSŞTUÜVYZ');
end.

