unit reConstsUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils, Translations, LCLPlatformDef, InterfaceBase;

resourcestring
  sResourceExplorer = 'Resource explorer';
  sSaveResource     = 'Save resource ...';
  sAbout            = 'About ...';
  sExit             = 'Exit';
  sOpen             = 'Open ...';
  sFile             = 'File';
  sHelp             = 'Help';
  sStrings          = 'Strings';
  sImage            = 'Image';
  sBinary           = 'Binary view';
  sLicense          = 'This program is free software under GNU GPL 2 license, see COPYING file';
  sWidth            = 'Width:';
  sHeight           = 'Height:';
  sPixelformat      = 'Pixel format:';
  sImageIndex       = 'Image index:';
  sIndexOfCount     = '%d of %d';
  sNextImage        = 'Next image';
  sPrevImage        = 'Previous image';

  sLCLVersion          = 'LCL Version: ';
  sBuildDate           = 'Build date: ';
  sFpcVersion          = 'FPC version: ';
  sTargetCPU           = 'Target CPU: ';
  sTargetOS            = 'Target OS: ';
  sWidgetSet           = '%s widget set';
  sOtherGUIWidgetSet   = 'Other GUI';
  sAllFilesExcutableFilesExeExeDLLDllDll = 'All files (*.*)|*.*|Excutable files (*.exe)|*.exe|DLL (*.dll)|*.dll';
  sAllFilesSharedLibSoSo = 'All files (*)|*|Shared libraries (*.so)|*.so';
  sResources = 'Resources';


function LCLVersionStr: string;

implementation

function LCLVersionStr: string;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:Result:=Format(sWidgetSet, ['GTK']);
    lpGtk2:Result:=Format(sWidgetSet, ['GTK2']);
    lpGtk3: Result:=Format(sWidgetSet, ['GTK3']);
    lpWin32:Result:=Format(sWidgetSet, ['Win32/Win64']);
    lpWinCE:Result:=Format(sWidgetSet, ['WinCE']);
    lpCarbon:Result:=Format(sWidgetSet, ['Carbon']);
    lpCocoa: Result:=Format(sWidgetSet, ['Cocoa']);
    lpQT:Result:=Format(sWidgetSet, ['Qt']);
    lpQT5: Result:=Format(sWidgetSet, ['Qt5']);
    lpQT6: Result:=Format(sWidgetSet, ['Qt6']);
    lpfpGUI: Result:=Format(sWidgetSet, ['FpGUI']);
  else
    Result:=sOtherGUIWidgetSet;
  end;
end;

procedure TranslateResStrings;
var
  LangID: TLanguageID;
begin
  LangID := GetLanguageID;
  TranslateUnitResourceStrings('reConstsUnit','languages'+DirectorySeparator+'resexplorer.%s.po', LangID.LanguageID, LangID.LanguageCode);
end;

initialization
  TranslateResStrings;
end.

