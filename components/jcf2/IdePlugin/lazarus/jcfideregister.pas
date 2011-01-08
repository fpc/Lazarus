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
  LCLType, FileUtil, LResources;

procedure Register;

implementation

uses
  { lazarus }
  LazIDEIntf, MenuIntf, IdeCommands,
  { local }
  JcfIdeMain, JcfRegistrySettings;

const
  FORMAT_MENU_NAME     = 'jcfJEDICodeFormat';
  FORMAT_CURRENT_NAME  = 'jcfCurrentEditorWindow';
  FORMAT_PROJECT_MENU_NAME  = 'jcfAllFilesinProject';
  FORMAT_OPEN_MENU_NAME     = 'jcfAllOpenWindows';
  FORMAT_REG_SETTINGS_MENU_NAME = 'jcfRegistrySettings';
  FORMAT_SETTINGS_MENU_NAME = 'jcfFormatSettings';
  FORMAT_ABOUT_MENU_NAME = 'jcfAbout';
  FORMAT_CATEGORY_IDECMD_NAME = 'jcfFormat';
  FORMAT_MENU_SECTION1 = 'jcfSection1';
  FORMAT_MENU_SECTION2 = 'jcfSection2';

resourcestring
  FORMAT_MENU     = 'JEDI Code &Format';
  FORMAT_CURRENT_MENU  = '&Current Editor Window';
  FORMAT_CURRENT_IDECMD  = 'Format code in current editor window';
  FORMAT_PROJECT_MENU  = '&All Files in Project';
  FORMAT_OPEN_MENU     = 'All &Open Windows';
  FORMAT_REG_SETTINGS_MENU = '&Registry Settings';
  FORMAT_SETTINGS_MENU = '&Format Settings';
  FORMAT_ABOUT_MENU = '&About';
  FORMAT_CATEGORY_IDECMD = 'JEDI Code Format';

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
  R: TLResource;
  S: TFileStream;
begin
  JCFOptsFile := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + DefaultJCFOptsFile;
  LazarusIDE.CopySecondaryConfigFile(DefaultJCFOptsFile);
  if not FileExistsUTF8(JCFOptsFile) then
  begin
    // create default
    R := LazarusResources.Find('JCFSettings', 'CFG');
    if (R <> nil) and (R.Value <> '') then
    begin
      S := TFileStream.Create(UTF8ToSys(JCFOptsFile), fmCreate);
      try
        S.Write(R.Value[1], Length(R.Value));
      finally
        S.Free;
      end;
    end;
  end;
end;

procedure Register;
var
  Cat: TIDECommandCategory;
  Key: TIDEShortCut;
  fcMainMenu, SubSection: TIDEMenuSection;
  CmdFormatFile: TIDECommand;
begin
  SetLazarusDefaultFileName;
  GetDefaultSettingsFileName := IDEGetDefaultSettingsFileName;

  Cat := IDECommandList.CreateCategory(nil, FORMAT_CATEGORY_IDECMD_NAME,
    FORMAT_CATEGORY_IDECMD, IDECmdScopeSrcEditOnly);
  // Ctrl + D ?
  Key := IDEShortCut(VK_D, [SSctrl], VK_UNKNOWN, []);
  CmdFormatFile := RegisterIDECommand(Cat, FORMAT_CURRENT_NAME, FORMAT_CURRENT_IDECMD, Key,
    lcJCFIDE.DoFormatCurrentIDEWindow);

  fcMainMenu := RegisterIDESubMenu(itmSecondaryTools, FORMAT_MENU_NAME, FORMAT_MENU);

  RegisterIDEMenuCommand(fcMainMenu, FORMAT_CURRENT_NAME, FORMAT_CURRENT_MENU,
    lcJCFIDE.DoFormatCurrentIDEWindow, nil, CmdFormatFile);

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
  {$I jcfsettings.lpi}
  lcJCFIDE := TJcfIdeMain.Create;

finalization
  FreeAndNil(lcJCFIDE);
end.

