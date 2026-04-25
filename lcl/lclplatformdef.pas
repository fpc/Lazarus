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
  SysUtils,
  LazVersion;

type
  TLCLPlatform = LazVersion.TLCLPlatform;
  TLCLPlatforms = set of TLCLPlatform;

const
  lpGtk    = LazVersion.lpGtk;
  lpGtk2   = LazVersion.lpGtk2;
  lpGtk3   = LazVersion.lpGtk3;
  lpWin32  = LazVersion.lpWin32;
  lpWinCE  = LazVersion.lpWinCE;
  lpCarbon = LazVersion.lpCarbon;
  lpQT     = LazVersion.lpQT;
  lpQt5    = LazVersion.lpQt5;
  lpQt6    = LazVersion.lpQt6;
  lpfpGUI  = LazVersion.lpfpGUI;
  lpNoGUI  = LazVersion.lpNoGUI;
  lpCocoa  = LazVersion.lpCocoa;
  lpCustomDrawn = LazVersion.lpCustomDrawn;
  lpMUI    = LazVersion.lpMUI;

  LCLPlatformDisplayNames: array[TLCLPlatform] of string = (
    'gtk (deprecated)',
    'gtk2',
    'gtk3',
    'win32/win64',
    'wince',
    'carbon',
    'qt',
    'qt5',
    'qt6',
    'fpGUI (alpha)',
    'NoGUI',
    'cocoa',
    'customdrawn (alpha)',
    'MUI'
    );

function DirNameToDisplayName(const ADirName: string): String;
function DisplayNameToDirName(const ADisplayName: String): String;
function GetBuildLCLWidgetType: TLCLPlatform;


implementation

function DirNameToDisplayName(const ADirName: string): String;
begin
  Result:=LCLPlatformDisplayNames[DirNameToLCLPlatform(ADirName)];
end;

function DisplayNameToDirName(const ADisplayName: String): String;
var
  PlatForm: TLCLPlatform;
begin
  for PlatForm:=Low(TLCLPlatform) to High(TLCLPlatform) do
    if CompareText(ADisplayName,LCLPlatformDisplayNames[PlatForm])=0 then
      exit(LCLPlatformDirNames[PlatForm]);
  Result:='gtk2';
end;

function GetBuildLCLWidgetType: TLCLPlatform;
begin
  Result:=LazVersion.GetBuildLCLWidgetType;
end;

end.

