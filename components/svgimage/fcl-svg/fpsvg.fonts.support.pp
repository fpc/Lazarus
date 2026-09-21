{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Selects the font coverage source of the platform being built for.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.fonts.support;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

uses fpsvg.types
{$IF defined(DARWIN)}
  , fpsvg.fonts.macos
{$ELSEIF defined(WINDOWS)}
  , fpsvg.fonts.windows
{$ELSEIF defined(UNIX)}
  , fpsvg.fonts.unix
{$ENDIF}
  ;

{$IF defined(DARWIN)}
type
  TSVGPlatformCoverage = TSVGCoreTextCoverage;
{$ELSEIF defined(WINDOWS)}
type
  TSVGPlatformCoverage = TSVGWindowsCoverage;
{$ELSEIF defined(UNIX)}
type
  TSVGPlatformCoverage = TSVGFontConfigCoverage;
{$ENDIF}

// True when this build has a coverage source for its platform.
function SVGHasPlatformCoverage: Boolean;
// The system the coverage source asks: Core Text, the GDI font tables or
// fontconfig. Empty when this build has none.
function SVGPlatformCoverageName: String;
// The coverage source of this platform, created on first use and owned by
// this unit. Nil when the build has none, or when the system it needs
// cannot answer.
function SVGPlatformCoverage: ISVGFontCoverage;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils;
{$ENDIF FPC_DOTTEDUNITS}

{$IF defined(DARWIN) or defined(WINDOWS) or defined(UNIX)}
  {$DEFINE HASPLATFORMCOVERAGE}
{$ENDIF}

{$IFDEF HASPLATFORMCOVERAGE}
var
  GInstance: TSVGPlatformCoverage = nil;
  GCoverage: ISVGFontCoverage = nil;
  GCreated: Boolean = False;
{$ENDIF}

function SVGHasPlatformCoverage: Boolean;

begin
  {$IFDEF HASPLATFORMCOVERAGE}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;


function SVGPlatformCoverageName: String;

begin
  {$IF defined(DARWIN)}
  Result := 'Core Text';
  {$ELSEIF defined(WINDOWS)}
  Result := 'the GDI font tables';
  {$ELSEIF defined(UNIX)}
  Result := 'fontconfig';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;


function SVGPlatformCoverage: ISVGFontCoverage;

begin
  Result := nil;
  {$IFDEF HASPLATFORMCOVERAGE}
  if not GCreated then
    begin
    GCreated := True;
    GInstance := TSVGPlatformCoverage.Create;
    if GInstance.Available then
      GCoverage := GInstance
    else
      FreeAndNil(GInstance);
    end;
  Result := GCoverage;
  {$ENDIF}
end;


{$IFDEF HASPLATFORMCOVERAGE}
finalization
  GCoverage := nil;
  GInstance.Free;
{$ENDIF}
end.
