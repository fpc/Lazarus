{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Selects the font provider of the platform being built for.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.fonts.provider;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

{$IF defined(DARWIN) and not defined(SVGFREETYPE)}
  {$DEFINE SVGCORETEXT}
{$ENDIF}
{$IF defined(WINDOWS) and not defined(SVGFREETYPE)}
  {$DEFINE SVGGDI}
{$ENDIF}
{$IF not defined(SVGCORETEXT) and not defined(SVGGDI)}
  {$DEFINE SVGFREETYPEFONTS}
{$ENDIF}

interface

uses fpsvg.types
{$IFDEF SVGCORETEXT}
  , fpsvg.coretext
{$ENDIF}
{$IFDEF SVGGDI}
  , fpsvg.gdi
{$ENDIF}
{$IFDEF SVGFREETYPEFONTS}
  , fpsvg.freetype
{$ENDIF}
  ;

type
{$IFDEF SVGCORETEXT}
  TSVGPlatformFontProvider = TSVGCoreTextProvider;
{$ENDIF}
{$IFDEF SVGGDI}
  TSVGPlatformFontProvider = TSVGGDIProvider;
{$ENDIF}
{$IFDEF SVGFREETYPEFONTS}
  TSVGPlatformFontProvider = TSVGFreeTypeProvider;
{$ENDIF}

// The engine the provider draws on: Core Text, GDI or freetype.
function SVGPlatformFontEngine: String;
{ The font provider of this platform, created on first use and owned by
  this unit. Core Text on macOS, the GDI font calls on Windows, freetype
  elsewhere; define SVGFREETYPE to use freetype on all three. A freetype
  provider is given the system font directories and the coverage source of
  fpsvg.fonts.support. Nil when no face can be resolved at all. }
function SVGPlatformFontProvider: ISVGFontProvider;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils
{$ELSE FPC_DOTTEDUNITS}
uses sysutils
{$ENDIF FPC_DOTTEDUNITS}
{$IFDEF SVGFREETYPEFONTS}
  , fpsvg.fonts.support
{$ENDIF}
  ;

var
  GInstance: TSVGPlatformFontProvider = nil;
  GProvider: ISVGFontProvider = nil;
  GCreated: Boolean = False;


function SVGPlatformFontEngine: String;

begin
  {$IFDEF SVGCORETEXT}
  Result := 'Core Text';
  {$ENDIF}
  {$IFDEF SVGGDI}
  Result := 'GDI';
  {$ENDIF}
  {$IFDEF SVGFREETYPEFONTS}
  Result := 'freetype';
  {$ENDIF}
end;


function SVGPlatformFontProvider: ISVGFontProvider;

begin
  if not GCreated then
    begin
    GCreated := True;
    GInstance := TSVGPlatformFontProvider.Create;
    if GInstance.Available then
      begin
      {$IFDEF SVGFREETYPEFONTS}
      GInstance.AddSystemFonts;
      GInstance.Coverage := SVGPlatformCoverage;
      {$ENDIF}
      GProvider := GInstance;
      end
    else
      FreeAndNil(GInstance);
    end;
  Result := GProvider;
end;


finalization
  GProvider := nil;
  GInstance.Free;
end.
