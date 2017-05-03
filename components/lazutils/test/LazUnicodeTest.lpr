{
  This is a test program for LazUnicode unit.
  Works with UTF-8 and UTF-16 encodings by switching UseUTF16 define.
  Works also in Delphi where String=UnicodeString. Just rename this file as *.dpr.
   For Delphi you must copy units LazUnicode and LazUTF16, both part of LazUtils package.
}
program LazUnicodeTest;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}       // Mostly for Delphi

// For testing the UTF16 version.
{$IF DEFINED(FPC) and DEFINED(UseUTF16)}
{$ModeSwitch UnicodeStrings}   // Sets also FPC_UNICODESTRINGS.
{$ENDIF}

{$IF DEFINED(FPC_UNICODESTRINGS) or not DEFINED(FPC)}
 {$DEFINE ReallyUseUTF16}       // FPC with UTF-16 or Delphi
{$ENDIF}

uses
  LazUnicode;

const
  Eyes = '👀';
  Thai = 'ฃ';   // No idea what it means.
  WineGlass = '🍷';
  Heart = '💓';  // or '♡';
  // Accents in combining codepoints. Last one has 2 consecutive combining marks.
  Combining = 'ÓÓỐỐỚỚÒÒỒỒỎỎỔỔỞỞỌỌBあC'#$CC#$81#$CC#$B2;
  //ArEnStr1 = 'مAرBحCبDاE';

var
  s_UTF8: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  s, ch: String;
  //CodeUnit: Char;
  i: NativeInt;
  cpIter: TCodePointEnumerator;
  ucIter: TUnicodeCharacterEnumerator;
begin
  s_UTF8 := 'Pöö ' + Eyes + Thai + '. Have ' + WineGlass + ' for FPC' + Heart + 'Lazarus';
  s := s_UTF8;                              // Converts encoding when needed.
  {$IFDEF ReallyUseUTF16}
  writeln('Using UnicodeString + UTF-16');
  {$ELSE}
  writeln('Using AnsiString + UTF-8');
  {$ENDIF}
  writeln('Sizeof(Char) = ', Sizeof(Char));
  writeln('Testing with string: ', s);

  writeln('');
  ch := CodePointCopy(s, 14, 1);       // Should return the wine glass.
  writeln('Testing CodePointCopy. SubString = "', ch + '"');

  i := CodePointLength(s);             // Should return 30.
  writeln('Testing CodePointLength. Result = ', i);

  // Constant must be assigned to AnsiString when using the UTF-8 system.
  s_UTF8 := WineGlass;
  i := CodePointPos(s_UTF8, s);        // Should return 14.
  writeln('Testing CodePointPos. Result = ', i);
  s_UTF8 := '☐';
  i := CodePointPos(s_UTF8, s);        // Should return 0.
  writeln('Testing CodePointPos for non-existent char. Result = ', i);

  // Use CodePoint enumerator explicitly
  writeln('');
  writeln('*** Using CodePoint iterator explicitly: ***');
  cpIter := TCodePointEnumerator.Create(s);
  while cpIter.MoveNext do
    writeln('ch=', cpIter.Current, '  has ', cpIter.CurrentCodeUnitCount, ' codeunits.');
  cpIter.Free;

  {$IFDEF FPC}
  // Use for-in loop for CodePoints.
  writeln('');
  writeln('*** Using for-in loop for CodePoints : ***');
  for ch in s do
    writeln('ch=',ch);
  {$ENDIF}

  // Use for-in loop for codeunits using a Char variable still works.
  {    Uncomment to test.
  writeln('');
  writeln('*** Using for-in loop for codeunits: ***');
  for CodeUnit in s do
    writeln('CodeUnit=',CodeUnit);        // The output makes no sense obviously.
  }

  s_UTF8 := Combining;
  s := s_UTF8;                              // Converts encoding when needed.
  writeln('');
  writeln('Testing with string: ', s);

  // Use UnicodeCharacter enumerator explicitly
  writeln('');
  writeln('*** Using UnicodeCharacter iterator explicitly: ***');
  ucIter := TUnicodeCharacterEnumerator.Create(s);
  while ucIter.MoveNext do
    writeln('ch=', ucIter.Current, '  has ', ucIter.CurrentCodePointCount, ' codepoints and ', ucIter.CurrentCodeUnitCount, ' codeunits.');
  ucIter.Free;

end.

