unit reConstsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

resourcestring
  sResourceExplorer = 'Resource explorer';
  sSaveResource     = 'Save resource...';
  sAbout            = 'About...';
  sExit             = 'Exit';
  sOpen             = 'Open...';
  sFile             = 'File';
  sHelp             = 'Help';
  sStrings          = 'Strings';
  sImage            = 'Image';
  sLicense          = 'This program is free software under GNU GPL 2 license, see COPYING file';


  sLCLVersion          = 'LCL Version: ';
  sBuildDate           = 'Build date : ';
  sFpcVersion          = 'FPC version : ';
  sTargetCPU           = 'Target CPU : ';
  sTargetOS            = 'Target OS : ';
  sGTKWidgetSet        = 'GTK widget set';
  sGTK2WidgetSet       = 'GTK 2 widget set';
  sWin32_64WidgetSet   = 'Win32/Win64 widget set';
  sWinCEWidgetSet      = 'WinCE widget set';
  sCarbonWidgetSet     = 'Carbon widget set';
  sQTWidgetSet         = 'QT widget set';
  sFpGUIWidgetSet      = 'FpGUI widget set';
  sOtherGUIWidgetSet   = 'Other gui';


function LCLVersionStr: string;
implementation
uses gettext, translations, LCLVersion, InterfaceBase;

function LCLVersionStr: string;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:Result:=sGTKWidgetSet;
    lpGtk2:Result:=sGTK2WidgetSet;
    lpWin32:Result:=sWin32_64WidgetSet;
    lpWinCE:Result:=sWinCEWidgetSet;
    lpCarbon:Result:=sCarbonWidgetSet;
    lpQT:Result:=sQTWidgetSet;
    lpfpGUI:Result:=sFpGUIWidgetSet;
  else
    Result:=sOtherGUIWidgetSet;
  end;
end;

procedure TranslateResStrings;
var
  Lang, FallbackLang: String;
begin
  GetLanguageIDs(Lang,FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('reConstsUnit','languages'+DirectorySeparator+'resexplorer.%s.po', Lang,FallbackLang);
end;

initialization
  TranslateResStrings;
end.

