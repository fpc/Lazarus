unit JcfIdeRegister;

{ AFS 7 Jan 2K
  JEDI Code Format IDE plugin registration }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfIdeRegister, released May 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele, Juergen Kehrel

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL")
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  { rtl }
  SysUtils, Classes,
  { lcl }
  LCLType, LazFileUtils, LazUTF8;

procedure Register;

implementation

{$R jcfsettings.res}

uses
  { lazarus }
  LazIDEIntf, MenuIntf, IdeCommands,
  { local }
  JcfIdeMain, JcfRegistrySettings;

const
  FORMAT_MENU_NAME         = 'jcfJEDICodeFormat';
  FORMAT_SELECTION_NAME    = 'jcfSelectionText';
  FORMAT_CURRENT_NAME      = 'jcfCurrentEditorWindow';
  FORMAT_PROJECT_MENU_NAME = 'jcfAllFilesinProject';
  FORMAT_OPEN_MENU_NAME    = 'jcfAllOpenWindows';
  //FORMAT_REG_SETTINGS_MENU_NAME = 'jcfRegistrySettings';
  FORMAT_SETTINGS_MENU_NAME = 'jcfFormatSettings';
  FORMAT_ABOUT_MENU_NAME   = 'jcfAbout';
  FORMAT_CATEGORY_IDECMD_NAME = 'jcfFormat';
  FORMAT_MENU_SECTION1     = 'jcfSection1';
  FORMAT_MENU_SECTION2     = 'jcfSection2';

resourcestring
  FORMAT_MENU             = 'JEDI Code &Format';
  FORMAT_SELECTION_MENU   = 'Selection';
  FORMAT_CURRENT_MENU     = '&Current Editor Window';
  FORMAT_SELECTION_IDECMD = 'Format code in Selection';
  FORMAT_CURRENT_IDECMD   = 'Format code in current editor window';
  FORMAT_PROJECT_MENU     = '&All Files in Project';
  FORMAT_OPEN_MENU        = 'All &Open Windows';
  //FORMAT_REG_SETTINGS_MENU = '&Registry Settings';
  FORMAT_SETTINGS_MENU    = '&Format Settings';
  FORMAT_ABOUT_MENU       = '&About';
  FORMAT_CATEGORY_IDECMD  = 'JEDI Code Format';

const
  DefaultJCFOptsFile = 'jcfsettings.cfg';

var
  lcJCFIDE: TJcfIdeMain;
  JCFOptsFile: String;

function IDEGetDefaultSettingsFileName: String;
begin
  Result := JCFOptsFile;
end;

procedure SetLazarusDefaultFileName;
var
  S: TFileStream;
  R: TResourceStream;
begin
  JCFOptsFile := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + DefaultJCFOptsFile;
  LazarusIDE.CopySecondaryConfigFile(DefaultJCFOptsFile);
  if not FileExistsUTF8(JCFOptsFile) then
  begin
    // create default
    R := TResourceStream.Create(HInstance, PChar('JCFSettings'), PChar(RT_RCDATA));
    S := TFileStream.Create(UTF8ToSys(JCFOptsFile), fmCreate);
    try
      S.CopyFrom(R, R.Size);
    finally
      S.Free;
      R.Free;
    end;
  end;
end;

procedure Register;
var
  Cat: TIDECommandCategory;
  fcMainMenu, SubSection: TIDEMenuSection;
  KeySelect, KeyUnit: TIDEShortCut;
  CmdSelect, CmdUnit: TIDECommand;
begin
  SetLazarusDefaultFileName;
  GetDefaultSettingsFileName := IDEGetDefaultSettingsFileName;

  Cat := IDECommandList.CreateCategory(nil, FORMAT_CATEGORY_IDECMD_NAME,
    FORMAT_CATEGORY_IDECMD, IDECmdScopeSrcEditOnly);

  fcMainMenu := RegisterIDESubMenu(itmSourceTools, FORMAT_MENU_NAME, FORMAT_MENU);

  KeySelect := IDEShortCut(VK_UNKNOWN, []);
  // We are running out of free shortcut combinations. Ctrl+Shift+Alt+D is free.
  //KeySelect := IDEShortCut(VK_D, [ssShift,ssAlt,SSctrl]);
  CmdSelect := RegisterIDECommand(Cat, FORMAT_SELECTION_NAME, FORMAT_SELECTION_IDECMD,
    KeySelect, lcJCFIDE.DoFormatSelection);
  RegisterIDEMenuCommand(fcMainMenu, FORMAT_SELECTION_NAME, FORMAT_SELECTION_MENU,
    lcJCFIDE.DoFormatSelection, nil, CmdSelect);

  // Ctrl + D
  KeyUnit := IDEShortCut(VK_D, [SSctrl]);
  CmdUnit := RegisterIDECommand(Cat, FORMAT_CURRENT_NAME, FORMAT_CURRENT_IDECMD,
    KeyUnit, lcJCFIDE.DoFormatCurrentIDEWindow);
  RegisterIDEMenuCommand(fcMainMenu, FORMAT_CURRENT_NAME, FORMAT_CURRENT_MENU,
    lcJCFIDE.DoFormatCurrentIDEWindow, nil, CmdUnit);

  RegisterIDEMenuCommand(fcMainMenu, FORMAT_PROJECT_MENU_NAME, FORMAT_PROJECT_MENU,
    lcJCFIDE.DoFormatProject);

  RegisterIDEMenuCommand(fcMainMenu, FORMAT_OPEN_MENU_NAME, FORMAT_OPEN_MENU,
    lcJCFIDE.DoFormatOpen);

  // settings
  SubSection := RegisterIDEMenuSection(fcMainMenu, FORMAT_MENU_SECTION1);
  //RegisterIDEMenuCommand(SubSection, FORMAT_REG_SETTINGS_MENU_NAME, FORMAT_REG_SETTINGS_MENU,
  //  lcJCFIDE.DoRegistrySettings);
  RegisterIDEMenuCommand(SubSection, FORMAT_SETTINGS_MENU_NAME, FORMAT_SETTINGS_MENU,
    lcJCFIDE.DoFormatSettings);

  // about
  SubSection := RegisterIDEMenuSection(fcMainMenu, FORMAT_MENU_SECTION2);
  RegisterIDEMenuCommand(fcMainMenu, FORMAT_ABOUT_MENU_NAME, FORMAT_ABOUT_MENU,
    lcJCFIDE.DoAbout);
end;


initialization
  lcJCFIDE := TJcfIdeMain.Create;

finalization
  FreeAndNil(lcJCFIDE);
end.

