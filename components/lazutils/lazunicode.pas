{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  This unit provides encoding agnostic unicode string manipulation functions and
  an enumerator. It works transparently with UTF-8 and UTF-16 encodings,
  thus allowing one source code to work between :
  1. Lazarus with its UTF-8 solution.
  2. Future FPC and Lazarus with Delphi compatible UTF-16 solution.
  3. Delphi, where String = UnicodeString.
}
unit LazUnicode;

{$IFDEF FPC}
 {$mode objfpc}{$H+}{$inline on}
{$ENDIF}

// For testing the UTF16 version.
{$IF DEFINED(FPC) and DEFINED(UseUTF16)}
 {$ModeSwitch UnicodeStrings}   // Sets also FPC_UNICODESTRINGS.
{$ENDIF}

{$IF DEFINED(FPC_UNICODESTRINGS) or not DEFINED(FPC)}
 {$DEFINE ReallyUseUTF16}       // FPC with UTF-16 or Delphi
{$ENDIF}

interface

uses
  Classes, SysUtils
  {$IFDEF ReallyUseUTF16}
   ,character, LazUTF16
  {$ENDIF}
  {$IFDEF FPC}
   ,LazUTF8
  {$ENDIF}
  ;

  // Helper functions for codepoints. They change behavior depending on ModeSwitch.
  function CodePointCopy(const s: string; StartCharIndex, CharCount: NativeInt): string; inline;
  function CodePointLength(const s: string): NativeInt; inline;
  function CodePointPos(const SearchForText, SearchInText: string; StartPos: NativeInt = 1): NativeInt; inline;
  function CodePointSize(p: PChar): integer; inline;
  function IsCombining(const AChar: PChar): Boolean; {$IFDEF FPC}inline;{$ENDIF}

  function UnicodeToWinCP(const s: string): AnsiString;
  function WinCPToUnicode(const s: AnsiString): string;

  function StringOfCodePoint(ACodePoint: String; N: Integer): String;

type
  // Base class for CodePoint and Character enumerators.

  { TUnicodeEnumeratorBase }

  TUnicodeEnumeratorBase = class
  private
    fSrcPos, fEndPos: PChar;        // Pointers to source string.
    // Preset variables for different codepoint/character lengths.
    // Current will be assigned to one of them.
    fCurOne, fCurTwo, fCurThree, fCurLong: String;
    fCurrent: String;               // Current separated codepoint/character.
    fCurrentCodeUnitCount: Integer; // Number of CodeUnits (Pascal Char) in Current.
    procedure UpdateCurrent(aCount: integer);
  public
    constructor Create(const A: String);
    property Current: String read fCurrent;
    property CurrentCodeUnitCount: Integer read fCurrentCodeUnitCount;
  end;

  { TCodePointEnumerator }

  // Traverse Unicode codepoints. Uses UTF-8 or UTF-16 depending on $ModeSwitch.
  TCodePointEnumerator = class(TUnicodeEnumeratorBase)
  public
    function MoveNext: Boolean;
  end;

  { TUnicodeCharacterEnumerator }

  // Traverse Unicode (user perceived) characters, including accented characters
  //  with combined codepoints. Uses UTF-8 or UTF-16 depending on $ModeSwitch.
  TUnicodeCharacterEnumerator = class(TUnicodeEnumeratorBase)
  private
    fCurrentCodePointCount: Integer; // Number of CodePoints in Current.
  public
    property CurrentCodePointCount: Integer read fCurrentCodePointCount;
    function MoveNext: Boolean;
  end;

  {$IFDEF FPC}
  // Enumerator for CodePoints could be used for the for-in loop.
  //operator Enumerator(A: String): TCodePointEnumerator;

  // This enumerator combines diacritical marks.
  // It is used by default although there are more rules for combining codepoints.
  // Diacritical marks cover rules for most western languages.
  operator Enumerator(A: String): TUnicodeCharacterEnumerator;
  {$ENDIF}

implementation

{$IFDEF ReallyUseUTF16}

function UTF16IsCombining(const AChar: PWideChar): Boolean;
var
  ch: WideChar;
begin
  ch := AChar[0];
  Result :=           // Combining Diacritical Marks (belongs to previos char)
    ( (ch >= #$300) and (ch <= #$36F) ) or    // 0300-036F
    ( (ch >= #$610) and (ch <= #$61A) ) or    // Arabic 0610..061A
    ( (ch >= #$64B) and (ch <= #$65F) ) or    // Arabic 064B..065F
    (  ch = #$670) or                         // Arabic 0670
    ( (ch >= #$6D6) and (ch <= #$6DC) ) or    // Arabic 06D6..06DC
    ( (ch >= #$6DF) and (ch <= #$6E4) ) or    // Arabic 06DF..06E4
    ( (ch >= #$6E7) and (ch <= #$6E8) ) or    // Arabic 06E7..06E8
    ( (ch >= #$6EA) and (ch <= #$6ED) ) or    // Arabic 06EA..06ED
    ( (ch >= #$8E4) and (ch <= #$8FE) ) or    // Arabic 08E4..08FE
    ( (ch >= #$1DC0) and (ch <= #$1DFF) ) or  // Combining Diacritical Marks Supplement 1DC0-1DFF
    ( (ch >= #$20D0) and (ch <= #$20FF) ) or  // Combining Diacritical Marks for Symbols 20D0-20FF
    ( (ch >= #$FE20) and (ch <= #$FE2F) );    // Combining half Marks FE20-FE2F
end;

{$ELSE}

function UTF8IsCombining(const AChar: PChar): Boolean;
begin
  Result :=
   ( (AChar[0] = #$CC) ) or                   // Combining Diacritical Marks (belongs to previos char) 0300-036F
   ( (AChar[0] = #$CD) and (AChar[1] in [#$80..#$AF]) ) or                        // Combining Diacritical Marks
   ( (AChar[0] = #$D8) and (AChar[1] in [#$90..#$9A]) ) or                        // Arabic 0610 (d890)..061A (d89a)
   ( (AChar[0] = #$D9) and (AChar[1] in [#$8b..#$9f, #$B0]) ) or                  // Arabic 064B (d98b)..065F (d99f) // 0670 (d9b0)

   ( (AChar[0] = #$DB) and (AChar[1] in [#$96..#$9C, #$9F..#$A4, #$A7..#$A8, #$AA..#$AD]) ) or // Arabic 06D6 (db96)..  .. ..06ED (dbad)
   ( (AChar[0] = #$E0) and (AChar[1] = #$A3) and (AChar[2] in [#$A4..#$BE]) ) or  // Arabic 08E4 (e0a3a4) ..08FE (e0a3be)

   ( (AChar[0] = #$E1) and (AChar[1] = #$B7) ) or                                 // Combining Diacritical Marks Supplement 1DC0-1DFF (e1b780)
   ( (AChar[0] = #$E2) and (AChar[1] = #$83) and (AChar[2] in [#$90..#$FF]) ) or  // Combining Diacritical Marks for Symbols 20D0-20FF
   ( (AChar[0] = #$EF) and (AChar[1] = #$B8) and (AChar[2] in [#$A0..#$AF]) );    // Combining half Marks FE20-FE2F
end;

{$ENDIF}

//---

function CodePointCopy(const s: string; StartCharIndex, CharCount: NativeInt): string;
// Copy CharCount CodePoints from s, starting from StartCharIndex'th CodePoints.
begin
  {$IFDEF ReallyUseUTF16}
  Result := UTF16Copy(s, StartCharIndex, CharCount);
  {$ELSE}
  Result := UTF8Copy(s, StartCharIndex, CharCount);
  {$ENDIF}
end;

function CodePointLength(const s: string): NativeInt;
// Number of CodePoints in s.
begin
  {$IFDEF ReallyUseUTF16}
  Result := UTF16Length(s);
  {$ELSE}
  Result := UTF8LengthFast(s);
  {$ENDIF}
end;

function CodePointPos(const SearchForText, SearchInText: string; StartPos: NativeInt = 1): NativeInt;
// Position of SearchForText in CodePoints.
begin
  {$IFDEF ReallyUseUTF16}
  Result := UTF16Pos(SearchForText, SearchInText, StartPos);
  {$ELSE}
  Result := UTF8Pos(SearchForText, SearchInText, StartPos);
  {$ENDIF}
end;

function CodePointSize(p: PChar): integer;
// Returns the number of CodeUnits in one CodePoint pointed by p.
begin
  {$IFDEF ReallyUseUTF16}
  if TCharacter.IsHighSurrogate(p^) then
    Result := 2
  else
    Result := 1
  {$ELSE}
  Result := UTF8CodepointSizeFast(p);
  {$ENDIF}
end;

function IsCombining(const AChar: PChar): Boolean;
// Note: there are many more rules for combining codepoints.
//  The diacritical marks here are only a subset.
begin
  {$IFDEF ReallyUseUTF16}
  Result := UTF16IsCombining(AChar);
  {$ELSE}
  Result := UTF8IsCombining(AChar);
  {$ENDIF}
end;

function UnicodeToWinCP(const s: string): AnsiString;
// Convert s to Windows system codepage. The Unicode encoding of s depends on mode.
begin
  {$IFDEF ReallyUseUTF16}
  {$IFDEF FPC}
   // ToDo: Don't convert through UTF-8.
   Result := UTF8ToWinCP(UTF16ToUTF8(s));
   {$ELSE}
   Result := s; // s is UnicodeString in Delphi. Conversion may be lossy.
   {$ENDIF}
  {$ELSE}
   Result := UTF8ToWinCP(s);
  {$ENDIF}
end;

function WinCPToUnicode(const s: AnsiString): string;
// Convert Windows system codepage s to Unicode (encoding depends on mode).
begin
  {$IFDEF ReallyUseUTF16}
  {$IFDEF FPC}
   // ToDo: Don't convert through UTF-8.
   Result := UTF8ToUTF16(WinCPToUTF8(s));
   {$ELSE}
   Result := s; // Result is UnicodeString in Delphi.
   {$ENDIF}
  {$ELSE}
   Result := WinCPToUTF8(s);
  {$ENDIF}
end;

function StringOfCodePoint(ACodePoint: String; N: Integer): String;
// Like StringOfChar
{$IFDEF ReallyUseUTF16}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF ReallyUseUTF16}
  Result := '';
  for i := 1 to N do
    Result := Result + ACodePoint;
  {$ELSE}
  Result := Utf8StringOfChar(ACodePoint, N);
  {$ENDIF}
end;

{ TUnicodeEnumeratorBase }

constructor TUnicodeEnumeratorBase.Create(const A: String);
begin
  fSrcPos := PChar(A); // Note: if A='' then PChar(A) returns a pointer to a #0 string
  fEndPos := fSrcPos + length(A);
  SetLength(fCurOne, 1); // Space for the most common codepoint/character lengths.
  SetLength(fCurTwo, 2);
  SetLength(fCurThree, 3);
end;

procedure TUnicodeEnumeratorBase.UpdateCurrent(aCount: integer);
// Copy the needed bytes to fCurrent which then holds a codepoint or "character".
begin
  fCurrentCodeUnitCount := aCount;
  Assert(aCount<>0, 'TUnicodeEnumeratorBase.UpdateCurrent: aCount=0.');
  case aCount of
    1: fCurrent := fCurOne; // Assignment does not copy but reference count changes.
    2: fCurrent := fCurTwo;
    3: fCurrent := fCurThree;
    else begin
      SetLength(fCurLong, aCount);
      fCurrent := fCurLong;
    end;
  end;
  Move(fSrcPos^, fCurrent[1], aCount*SizeOf(Char));
  Assert(Length(fCurrent)=aCount, 'TUnicodeEnumeratorBase.UpdateCurrent: Length(fCurrent)<>aCount.');
  inc(fSrcPos, aCount);
end;

{ TCodePointEnumerator }

function TCodePointEnumerator.MoveNext: Boolean;
begin
  Result := fSrcPos < fEndPos;
  if Result then
    UpdateCurrent(CodePointSize(fSrcPos));
end;

{ TUnicodeCharacterEnumerator }

function TUnicodeCharacterEnumerator.MoveNext: Boolean;
var
  NextCP: PChar;
  NextCUCount: Integer;
begin
  Result := fSrcPos < fEndPos;
  if Result then
  begin
    fCurrentCodePointCount := 0;
    NextCP := fSrcPos;
    repeat
      NextCUCount := CodePointSize(NextCP);
      Inc(NextCP, NextCUCount);       // Prepare for combining diacritical marks.
      Inc(fCurrentCodePointCount);
    until not IsCombining(NextCP);
    UpdateCurrent(NextCP - fSrcPos);  // Pointer arithmetics.
  end;
end;

//---
// Enumerator
//---
{$IFDEF FPC}
operator Enumerator(A: String): TUnicodeCharacterEnumerator;
begin
  Result := TUnicodeCharacterEnumerator.Create(A);
end;
{$ENDIF}

end.

