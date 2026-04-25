{
 /***************************************************************************
                               lazversion.pas
                             -------------------
                         Version numbers for Lazarus

 ***************************************************************************/

 *****************************************************************************
  This file is part of the LazUtils package

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazVersion;

{$mode objfpc}{$H+}

interface

uses sysutils;

type
  // Also cmd line programs may need the LCL Widgetset of an application.
  TLCLPlatform = (
    lpGtk,
    lpGtk2,
    lpGtk3,
    lpWin32,
    lpWinCE,
    lpCarbon,
    lpQT,
    lpQt5,
    lpQt6,
    lpfpGUI,
    lpNoGUI,
    lpCocoa,
    lpCustomDrawn,
    lpMUI
    );

  TLCLWidgetTypeEvent = function(): TLCLPlatform;
  TLCLWidgetTypeNameEvent = function(): string;

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
    'qt6',
    'fpgui',
    'nogui',
    'cocoa',
    'customdrawn',
    'mui'
    );

  laz_major = 4;
  laz_minor = 99;
  laz_release = 0;
  laz_patch = 0;
  laz_fullversion = ((laz_major *  100 + laz_minor) * 100 + laz_release) * 100 + laz_patch;
  laz_version = '4.99.0.0';

var
  OnLCLWidgetType: TLCLWidgetTypeEvent;  // Set by LCL
  OnLCLWidgetTypeName: TLCLWidgetTypeNameEvent;

function DirNameToLCLPlatform(const ADirName: string): TLCLPlatform;
function GetLCLWidgetType: TLCLPlatform;
function GetLCLWidgetTypeName: string;
function GetBuildLCLWidgetType: TLCLPlatform;


implementation

const
  // Used by GetDefaultLCLWidgetType in InterfaceBase.
  BuildLCLWidgetType: TLCLPlatform =
    {$IFDEF MSWindows}{$DEFINE WidgetSetDefined}
    lpWin32;
    {$ENDIF}
    {$IFDEF darwin}{$DEFINE WidgetSetDefined}
      {$IFDEF CPUPOWERPC}
      lpCarbon;
      {$ELSE}
      lpCocoa;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF HASAMIGA}{$DEFINE WidgetSetDefined}
    lpMUI;
    {$ENDIF}
    {$IFNDEF WidgetSetDefined}
    lpGtk2;
    {$ENDIF}

function DirNameToLCLPlatform(const ADirName: string): TLCLPlatform;
begin
  for Result:=Low(TLCLPlatform) to High(TLCLPlatform) do
    if CompareText(ADirName,LCLPlatformDirNames[Result])=0 then exit;
  Result:=lpGtk2;
end;

function GetLCLWidgetType: TLCLPlatform;
begin
  if Assigned(OnLCLWidgetType) then
    Result := OnLCLWidgetType()
  else
    Result := lpGtk2;
end;

function GetLCLWidgetTypeName: string;
begin
  if Assigned(OnLCLWidgetTypeName) then
    Result := OnLCLWidgetTypeName()
  else
    Result := '';
end;

function GetBuildLCLWidgetType: TLCLPlatform;
begin
  Result:=BuildLCLWidgetType;
end;

end.

