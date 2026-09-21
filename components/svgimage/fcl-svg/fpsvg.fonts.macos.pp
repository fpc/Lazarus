{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Finds the font file covering a character, using Core Text.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.fonts.macos;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

{$IFNDEF DARWIN}
{$FATAL This unit builds for macOS only. Use fpsvg.fonts.support for the coverage source of the platform being built for.}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGCoreText = class(ESVGError);

  { Asks Core Text which face it would fall back to for a character, and
    returns the file of that face. }
  TSVGCoreTextCoverage = class(TObject, ISVGFontCoverage)
  private
    FAvailable: Boolean;
    FQueries: Integer;
  public
    constructor Create;
    function CoverFor(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): String;
    // Always True: Core Text is part of macOS.
    property Available: Boolean read FAvailable;
    // Number of characters looked up.
    property Queries: Integer read FQueries;
  end;

// True when this build can ask the system which face covers a character.
function SVGCoreTextAvailable: Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses MacOsApi.MacTypes, MacOsApi.CFBase, MacOsApi.CFString,
     MacOsApi.CFURL, MacOsApi.CGBase, MacOsApi.CTFont,
     MacOsApi.CTFontDescriptor;
{$ELSE FPC_DOTTEDUNITS}
uses MacTypes, CFBase, CFString, CFURL, CGBase, CTFont, CTFontDescriptor;
{$ENDIF FPC_DOTTEDUNITS}

// The univint headers declare the calls but link nothing, so a program
// using this unit would otherwise need the frameworks on its command
// line.
{$linkframework CoreText}
{$linkframework CoreFoundation}

const
  { Core Text needs a size, and the answer does not depend on it. The
    lookup uses a size that every face accepts. }
  LookupSize = 12;

function SVGCoreTextAvailable: Boolean;

begin
  Result := True;
end;


// The first family of a comma separated list, unquoted.
function FirstFamily(const aList: TSVGString): TSVGString;

var
  lComma: Integer;

begin
  Result := Trim(aList);
  lComma := Pos(',', Result);
  if lComma > 0 then
    Result := Trim(Copy(Result, 1, lComma - 1));
  if Length(Result) >= 2 then
    if ((Result[1] = '"') and (Result[Length(Result)] = '"'))
       or ((Result[1] = '''') and (Result[Length(Result)] = '''')) then
      Result := Copy(Result, 2, Length(Result) - 2);
end;


{ TSVGCoreTextCoverage }

constructor TSVGCoreTextCoverage.Create;

begin
  inherited Create;
  FAvailable := SVGCoreTextAvailable;
end;


function TSVGCoreTextCoverage.CoverFor(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): String;

var
  lChars: array[0..1] of UniChar;
  lCount: CFIndex;
  lText, lName: CFStringRef;
  lBase, lCover: CTFontRef;
  lURL: CFTypeRef;
  lRange: CFRange;
  lFamily: RawByteString;
  lPath: array[0..1023] of AnsiChar;

begin
  Result := '';
  if not FAvailable then
    Exit;
  Inc(FQueries);
  // Core Text takes text, not a code point. A code point outside the basic
  // plane is passed as the surrogate pair that spells it.
  if aCodePoint > $FFFF then
    begin
    lChars[0] := UniChar($D800 + ((aCodePoint - $10000) shr 10));
    lChars[1] := UniChar($DC00 + ((aCodePoint - $10000) and $3FF));
    lCount := 2;
    end
  else
    begin
    lChars[0] := UniChar(aCodePoint);
    lCount := 1;
    end;
  lText := CFStringCreateWithCharacters(nil, @lChars[0], lCount);
  if lText = nil then
    Exit;
  lName := nil;
  lBase := nil;
  lCover := nil;
  lURL := nil;
  try
    // Core Text is handed the family as UTF-8, which is what the text of
    // a document already is.
    lFamily := FirstFamily(aRequest.Families);
    if lFamily <> '' then
      begin
      lName := CFStringCreateWithCString(nil, PAnsiChar(lFamily),
        kCFStringEncodingUTF8);
      if lName <> nil then
        lBase := CTFontCreateWithName(lName, LookupSize, nil);
      end;
    // Core Text answers even without a family to start from. It then falls
    // back from the system font.
    lRange.location := 0;
    lRange.length := lCount;
    lCover := CTFontCreateForString(lBase, lText, lRange);
    if lCover = nil then
      Exit;
    lURL := CTFontCopyAttribute(lCover, kCTFontURLAttribute);
    if lURL = nil then
      Exit;
    if CFURLGetFileSystemRepresentation(CFURLRef(lURL), True, @lPath[0],
                                        SizeOf(lPath)) then
      Result := SVGFileNameOfBytes(PAnsiChar(@lPath[0]));
  finally
    if lURL <> nil then
      CFRelease(lURL);
    if lCover <> nil then
      CFRelease(lCover);
    if lBase <> nil then
      CFRelease(lBase);
    if lName <> nil then
      CFRelease(lName);
    CFRelease(lText);
  end;
end;


end.
