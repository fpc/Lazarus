{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Finds the font file covering a character, using the GDI font tables.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.fonts.windows;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

{$IFNDEF WINDOWS}
{$FATAL This unit builds for Windows only. Use fpsvg.fonts.support for the coverage source of the platform being built for.}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGWindowsFonts = class(ESVGError);

  { Finds the file that holds a glyph for a character, using what Windows provides.
    It reads the font-link table of the family first, and then asks every
    installed face which ranges it covers. }
  TSVGWindowsCoverage = class(TObject, ISVGFontCoverage)
  private
    FAvailable: Boolean;
    FQueries: Integer;
    FFontPath: String;
    FFaceFiles: TStringList;
    FLoaded: Boolean;
    // Reads the face to file table that Windows keeps in the registry.
    procedure LoadFaceFiles;
    // The file that a face name is installed as, empty when unknown.
    function FileOfFace(const aFace: String): String;
    // The files that the font-link table offers for a family, one per
    // line.
    function LinkedFiles(const aFamily: String): String;
    // The first installed face that covers the code point, empty when none
    // does. This enumerates every family, so it is tried last.
    function SearchFaces(aCodePoint: Cardinal): String;
  public
    constructor Create;
    destructor Destroy; override;
    function CoverFor(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): String;
    // Always True: the GDI font tables are part of Windows.
    property Available: Boolean read FAvailable;
    // Number of characters looked up.
    property Queries: Integer read FQueries;
    // The directory that the installed fonts live in.
    property FontPath: String read FFontPath;
  end;

// True when this build can ask the system which face covers a character.
function SVGWindowsFontsAvailable: Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses WinApi.Windows, System.Registry;
{$ELSE FPC_DOTTEDUNITS}
uses windows, registry;
{$ENDIF FPC_DOTTEDUNITS}

const
  { GetFontUnicodeRanges and the record it fills are the only part the RTL
    does not declare. They are copied here, rather than adding a dependency
    on winunits-jedi for one call. }
  GdiError = DWORD($FFFFFFFF);
  FontsKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\Fonts';
  LinkKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontLink\SystemLink';

type
  WCRANGE = record
    wcLow : WCHAR;
    cGlyphs : Word;
  end;
  PWCRANGE = ^WCRANGE;

  GLYPHSET = record
    cbThis : DWORD;
    flAccel : DWORD;
    cGlyphsSupported : DWORD;
    cRanges : DWORD;
    ranges : array[0..0] of WCRANGE;
  end;
  PGLYPHSET = ^GLYPHSET;

function GetFontUnicodeRanges(DC: HDC; lpgs: PGLYPHSET): DWORD; stdcall;
  external 'gdi32' name 'GetFontUnicodeRanges';

function SVGWindowsFontsAvailable: Boolean;

begin
  Result := True;
end;


// The first family in a comma separated list, unquoted.
function FirstFamily(const aList: String): String;

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


// True when the given face covers the code point. GDI is asked
// directly.
function FaceCovers(const aFace: String; aCodePoint: Cardinal): Boolean;

var
  lDC: HDC;
  lFont, lOld: HFONT;
  lLogFont: LOGFONTW;
  lSize: DWORD;
  lBuffer: array of Byte;
  lSet: PGLYPHSET;
  lRange: PWCRANGE;
  I: Integer;
  lWide: UnicodeString;

begin
  Result := False;
  // GDI reports its ranges in wide characters, so a code point above the
  // basic plane cannot be answered this way.
  if aCodePoint > $FFFF then
    Exit;
  lDC := CreateCompatibleDC(0);
  if lDC = 0 then
    Exit;
  lFont := 0;
  try
    FillChar(lLogFont, SizeOf(lLogFont), 0);
    lLogFont.lfCharSet := DEFAULT_CHARSET;
    lWide := UnicodeString(aFace);
    if Length(lWide) >= LF_FACESIZE then
      SetLength(lWide, LF_FACESIZE - 1);
    Move(PWideChar(lWide)^, lLogFont.lfFaceName[0],
      Length(lWide) * SizeOf(WideChar));
    lFont := CreateFontIndirectW(@lLogFont);
    if lFont = 0 then
      Exit;
    lOld := SelectObject(lDC, lFont);
    try
      lSize := GetFontUnicodeRanges(lDC, nil);
      if (lSize = 0) or (lSize = GdiError) then
        Exit;
      SetLength(lBuffer, lSize);
      lSet := PGLYPHSET(@lBuffer[0]);
      lSet^.cbThis := lSize;
      if GetFontUnicodeRanges(lDC, lSet) = 0 then
        Exit;
      lRange := @lSet^.ranges[0];
      for I := 0 to lSet^.cRanges - 1 do
        begin
        if (aCodePoint >= Cardinal(lRange^.wcLow))
           and (aCodePoint < Cardinal(lRange^.wcLow) + lRange^.cGlyphs) then
          Exit(True);
        Inc(lRange);
        end;
    finally
      SelectObject(lDC, lOld);
    end;
  finally
    if lFont <> 0 then
      DeleteObject(lFont);
    DeleteDC(lDC);
  end;
end;


function EnumFamily(var aLogFont: ENUMLOGFONTEXW;
  var aMetric: NEWTEXTMETRICEXW; aType: longint; aData: LPARAM): longint;
  stdcall;

var
  lName: String;
  lFamilies: TStringList;

begin
  Result := 1;
  lFamilies := TStringList(PtrUInt(aData));
  lName := UTF8Encode(UnicodeString(
    PWideChar(@aLogFont.elfLogFont.lfFaceName[0])));
  // A family whose name starts with an at sign is the vertical writing
  // form of another. It draws the same characters turned on their side.
  if (lName <> '') and (lName[1] <> '@')
     and (lFamilies.IndexOf(lName) < 0) then
    lFamilies.Add(lName);
end;


{ TSVGWindowsCoverage }

constructor TSVGWindowsCoverage.Create;

var
  lPath: array[0..MAX_PATH] of WideChar;

begin
  inherited Create;
  FAvailable := SVGWindowsFontsAvailable;
  FFaceFiles := TStringList.Create;
  FFaceFiles.CaseSensitive := False;
  if GetWindowsDirectoryW(@lPath[0], MAX_PATH) > 0 then
    FFontPath := IncludeTrailingPathDelimiter(
      String(UnicodeString(PWideChar(@lPath[0])))) + 'Fonts';
end;


destructor TSVGWindowsCoverage.Destroy;

begin
  FreeAndNil(FFaceFiles);
  inherited Destroy;
end;


procedure TSVGWindowsCoverage.LoadFaceFiles;

var
  lRegistry: TRegistry;
  lNames: TStringList;
  I, lBracket: Integer;
  lFace, lFile: String;

begin
  if FLoaded then
    Exit;
  FLoaded := True;
  lRegistry := TRegistry.Create;
  lNames := TStringList.Create;
  try
    lRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if not lRegistry.OpenKeyReadOnly(FontsKey) then
      Exit;
    lRegistry.GetValueNames(lNames);
    for I := 0 to lNames.Count - 1 do
      begin
      lFile := lRegistry.ReadString(lNames[I]);
      if lFile = '' then
        Continue;
      // The value is called "Family (TrueType)". Several families may
      // share one file: that is a font collection.
      lFace := lNames[I];
      lBracket := Pos(' (', lFace);
      if lBracket > 0 then
        lFace := Trim(Copy(lFace, 1, lBracket - 1));
      if lFace <> '' then
        FFaceFiles.Values[lFace] := lFile;
      end;
  finally
    lNames.Free;
    lRegistry.Free;
  end;
end;


function TSVGWindowsCoverage.FileOfFace(const aFace: String): String;

begin
  LoadFaceFiles;
  Result := FFaceFiles.Values[aFace];
  if Result = '' then
    Exit;
  if (ExtractFileDrive(Result) = '') and (FFontPath <> '') then
    Result := IncludeTrailingPathDelimiter(FFontPath) + Result;
end;


function TSVGWindowsCoverage.LinkedFiles(const aFamily: String): String;

var
  lRegistry: TRegistry;
  lLinks: TStringList;
  I, lComma: Integer;
  lEntry: String;

begin
  Result := '';
  if aFamily = '' then
    Exit;
  lRegistry := TRegistry.Create;
  lLinks := TStringList.Create;
  try
    lRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if not lRegistry.OpenKeyReadOnly(LinkKey) then
      Exit;
    if not lRegistry.ValueExists(aFamily) then
      Exit;
    // The value is a list of "file,face" lines, in the order Windows falls
    // back through them.
    lRegistry.ReadStringList(aFamily, lLinks);
    for I := 0 to lLinks.Count - 1 do
      begin
      lEntry := Trim(lLinks[I]);
      if lEntry = '' then
        Continue;
      lComma := Pos(',', lEntry);
      if lComma > 0 then
        lEntry := Trim(Copy(lEntry, 1, lComma - 1));
      if lEntry = '' then
        Continue;
      if (ExtractFileDrive(lEntry) = '') and (FFontPath <> '') then
        lEntry := IncludeTrailingPathDelimiter(FFontPath) + lEntry;
      Result := Result + lEntry + LineEnding;
      end;
  finally
    lLinks.Free;
    lRegistry.Free;
  end;
end;


function TSVGWindowsCoverage.SearchFaces(aCodePoint: Cardinal): String;

var
  lDC: HDC;
  lLogFont: LOGFONTW;
  I: Integer;
  lFile: String;
  lFamilies: TStringList;

begin
  Result := '';
  lDC := CreateCompatibleDC(0);
  if lDC = 0 then
    Exit;
  lFamilies := TStringList.Create;
  try
    FillChar(lLogFont, SizeOf(lLogFont), 0);
    lLogFont.lfCharSet := DEFAULT_CHARSET;
    EnumFontFamiliesExW(lDC, @lLogFont, @EnumFamily,
      LPARAM(PtrUInt(lFamilies)), 0);
    for I := 0 to lFamilies.Count - 1 do
      begin
      lFile := FileOfFace(lFamilies[I]);
      if lFile = '' then
        Continue;
      if FaceCovers(lFamilies[I], aCodePoint) then
        Exit(lFile);
      end;
  finally
    lFamilies.Free;
    DeleteDC(lDC);
  end;
end;


function TSVGWindowsCoverage.CoverFor(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): String;

var
  lLinked: TStringList;
  I: Integer;
  lFamily: String;

begin
  Result := '';
  if not FAvailable then
    Exit;
  Inc(FQueries);
  lFamily := FirstFamily(aRequest.Families);
  // The fallbacks Windows itself uses for this family come first: it is a
  // short list chosen to match it.
  lLinked := TStringList.Create;
  try
    lLinked.Text := LinkedFiles(lFamily);
    for I := 0 to lLinked.Count - 1 do
      if FileExists(lLinked[I]) then
        Exit(lLinked[I]);
  finally
    lLinked.Free;
  end;
  Result := SearchFaces(aCodePoint);
end;


end.
