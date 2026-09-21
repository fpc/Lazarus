{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Finds the font file covering a character, using fontconfig.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.fonts.unix;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.DynLibs, System.SysUtils, System.CTypes, Api.Libfontconfig,
     fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses dynlibs, sysutils, ctypes, libfontconfig, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGFontConfig = class(ESVGError);

  { Asks fontconfig which file holds a glyph for a character.
    Creating one never raises: without the library it finds nothing,
    and the program goes on drawing everything the requested faces do cover. }
  TSVGFontConfigCoverage = class(TObject, ISVGFontCoverage)
  private
    FAvailable: Boolean;
    FConfig: PFcConfig;
    FQueries: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function CoverFor(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): String;
    // True when the fontconfig library loaded and had a configuration.
    property Available: Boolean read FAvailable;
    // Number of characters looked up.
    property Queries: Integer read FQueries;
  end;

// True when the fontconfig library can be loaded on this system.
function SVGFontConfigAvailable: Boolean;

implementation

const
  FontConfigSlantRoman = 0;

var
  Loaded: Boolean = False;
  LoadTried: Boolean = False;

// Loads the library once. Reports whether it can be used.
function LoadOnce: Boolean;

begin
  if not LoadTried then
    begin
    LoadTried := True;
    Loaded := LoadFontConfigLib(DefaultLibName, False) > 0;
    end;
  Result := Loaded;
end;


function SVGFontConfigAvailable: Boolean;

begin
  Result := LoadOnce;
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


// The fontconfig weight nearest to a CSS weight.
function FontConfigWeight(aWeight: Integer): Integer;

begin
  if aWeight >= 900 then
    Result := FC_WEIGHT_BLACK
  else if aWeight >= 700 then
    Result := FC_WEIGHT_BOLD
  else if aWeight >= 600 then
    Result := FC_WEIGHT_DEMIBOLD
  else if aWeight >= 500 then
    Result := FC_WEIGHT_MEDIUM
  else if aWeight >= 400 then
    Result := FC_WEIGHT_REGULAR
  else if aWeight >= 300 then
    Result := FC_WEIGHT_LIGHT
  else
    Result := FC_WEIGHT_THIN;
end;


{ TSVGFontConfigCoverage }

constructor TSVGFontConfigCoverage.Create;

begin
  inherited Create;
  FAvailable := LoadOnce;
  if not FAvailable then
    Exit;
  FConfig := FcInitLoadConfigAndFonts();
  FAvailable := FConfig <> nil;
end;


destructor TSVGFontConfigCoverage.Destroy;

begin
  // The configuration belongs to fontconfig. It keeps one per process and
  // gives the same one to every caller.
  FConfig := nil;
  inherited Destroy;
end;


function TSVGFontConfigCoverage.CoverFor(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): String;

var
  lSet: PFcCharSet;
  lPattern, lMatch: PFcPattern;
  lResult: TFcResult;
  lFile: PFcChar8;
  lFamily: TSVGString;
  lBytes: RawByteString;
  lSlant: Integer;

begin
  Result := '';
  if not FAvailable then
    Exit;
  Inc(FQueries);
  lSet := FcCharSetCreate();
  lPattern := FcPatternCreate();
  try
    if (lSet = nil) or (lPattern = nil) then
      Exit;
    FcCharSetAddChar(lSet, aCodePoint);
    FcPatternAddCharSet(lPattern, pcchar(PAnsiChar(FC_CHARSET)), lSet);
    // The family the element requested is added as well, so a similar
    // face wins among the ones holding the character.
    lFamily := FirstFamily(aRequest.Families);
    if lFamily <> '' then
      begin
      // fontconfig reads and writes its strings in UTF-8.
      lBytes := lFamily;
      FcPatternAddString(lPattern, pcchar(PAnsiChar(FC_FAMILY)),
        PFcChar8(PAnsiChar(lBytes)));
      end;
    FcPatternAddInteger(lPattern, pcchar(PAnsiChar(FC_WEIGHT)),
      FontConfigWeight(aRequest.Weight));
    if aRequest.Style = fnNormal then
      lSlant := FontConfigSlantRoman
    else
      lSlant := FC_SLANT_ITALIC;
    FcPatternAddInteger(lPattern, pcchar(PAnsiChar(FC_SLANT)), lSlant);
    FcConfigSubstitute(FConfig, lPattern, FcMatchPattern);
    FcDefaultSubstitute(lPattern);
    lMatch := FcFontMatch(FConfig, lPattern, @lResult);
    if lMatch = nil then
      Exit;
    try
      lFile := nil;
      if FcPatternGetString(lMatch, pcchar(PAnsiChar(FC_FILE)), 0,
                            @lFile) = FcResultMatch then
        Result := SVGFileNameOfBytes(PAnsiChar(lFile));
    finally
      FcPatternDestroy(lMatch);
    end;
  finally
    if lPattern <> nil then
      FcPatternDestroy(lPattern);
    if lSet <> nil then
      FcCharSetDestroy(lSet);
  end;
end;


end.
