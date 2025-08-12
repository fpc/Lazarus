unit reConstsUnit;

{$mode objfpc}{$H+}

interface

uses Translations, LCLPlatformDef, InterfaceBase;

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
  sGTKWidgetSet        = 'GTK widget set';
  sGTK2WidgetSet       = 'GTK2 widget set';
  sGTK3WidgetSet       = 'GTK3 widget set';
  sWin32_64WidgetSet   = 'Win32/Win64 widget set';
  sWinCEWidgetSet      = 'WinCE widget set';
  sCarbonWidgetSet     = 'Carbon widget set';
  sCocoaWidgetSet      = 'Cocoa widget set';
  sQTWidgetSet         = 'QT widget set';
  sQT5WidgetSet        = 'QT5 widget set';
  sQT6WidgetSet        = 'QT6 widget set';
  sFpGUIWidgetSet      = 'FpGUI widget set';
  sOtherGUIWidgetSet   = 'Other gui';
  sAllFilesExcutableFilesExeExeDLLDllDll = 'All files (*.*)|*.*|Excutable files (*.exe)|*.exe|DLL (*.dll)|*.dll';
  sAllFilesSharedLibSoSo = 'All files (*)|*|Shared libraries (*.so)|*.so';
  sResources = 'Resources';


function LCLVersionStr: string;

implementation

function LCLVersionStr: string;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:Result:=sGTKWidgetSet;
    lpGtk2:Result:=sGTK2WidgetSet;
    lpGtk3: Result := sGTK3WidgetSet;
    lpWin32:Result:=sWin32_64WidgetSet;
    lpWinCE:Result:=sWinCEWidgetSet;
    lpCarbon:Result:=sCarbonWidgetSet;
    lpCocoa: Result := sCocoaWidgetSet;
    lpQT:Result:=sQTWidgetSet;
    lpQT5: Result := sQT5WidgetSet;
    lpQT6: Result := sQT6WidgetSet;
    lpfpGUI: Result := sFpGUIWidgetSet;
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

