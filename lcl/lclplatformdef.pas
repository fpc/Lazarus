{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Contains the non-GUI dependent parts of LCL Platform definition.
}

unit LCLPlatformDef;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TLCLPlatform = (
    lpGtk,
    lpGtk2,
    lpGtk3,
    lpWin32,
    lpWinCE,
    lpCarbon,
    lpQT,
    lpQt5,
    lpfpGUI,
    lpNoGUI,
    lpCocoa,
    lpCustomDrawn,
    lpMUI
    );

  TLCLPlatforms = set of TLCLPlatform;

  function DirNameToLCLPlatform(const ADirName: string): TLCLPlatform;
  function GetBuildLCLWidgetType: TLCLPlatform;

const
  LCLPlatformDirNames: array[TLCLPlatform] of string = (
    'gtk',
    'gtk2',
    'gtk3',
    'win32',
    'wince',
    'carbon',
    'qt',
    'qt5',
    'fpgui',
    'nogui',
    'cocoa',
    'customdrawn',
    'mui'
    );

  LCLPlatformDisplayNames: array[TLCLPlatform] of string = (
    'gtk (deprecated)',
    'gtk2',
    'gtk3 (alpha)',
    'win32/win64',
    'wince',
    'carbon',
    'qt',
    'qt5',
    'fpGUI (alpha)',
    'NoGUI',
    'cocoa (alpha)',
    'customdrawn (alpha)',
    'MUI'
    );

var
  // set by lazbuild.lpr and used by GetDefaultLCLWidgetType
  BuildLCLWidgetType: TLCLPlatform =
    {$IFDEF MSWindows}{$DEFINE WidgetSetDefined}
    lpWin32;
    {$ENDIF}
    {$IFDEF darwin}{$DEFINE WidgetSetDefined}
    lpCarbon;
    {$ENDIF}
    {$IFDEF HASAMIGA}{$DEFINE WidgetSetDefined}
    lpMUI;
    {$ENDIF}
    {$IFNDEF WidgetSetDefined}
    lpGtk2;
    {$ENDIF}


implementation

function DirNameToLCLPlatform(const ADirName: string): TLCLPlatform;
begin
  for Result:=Low(TLCLPlatform) to High(TLCLPlatform) do
    if CompareText(ADirName,LCLPlatformDirNames[Result])=0 then exit;
  Result:=lpGtk2;
end;

function GetBuildLCLWidgetType: TLCLPlatform;
begin
  Result:=BuildLCLWidgetType;
end;

end.

