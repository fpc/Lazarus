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
  SysUtils, Classes,
  // LCL
  LCLType,
  // LazUtils
  LazFileUtils,
  // IdeIntf
  LazIDEIntf, MenuIntf, IdeCommands,
  // local
  JcfIdeMain, JcfRegistrySettings, JcfUIConsts,
  IDEOptionsIntf, IDEOptEditorIntf;

type
  TIDEFormattingSettings = class(TAbstractIDEEnvironmentOptions)
  public
    constructor Create(const pbReadRegFile: boolean);
    destructor Destroy; override;
    class function GetGroupCaption: String; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite({%H-}Restore: boolean); override;
  end;

procedure Register;

implementation

uses
  JcfSettings, JcfUiTools, JcfUiToolsGui;

{$R jcfsettings.res}



const
  FORMAT_MENU_NAME         = 'jcfJEDICodeFormat';
  FORMAT_CURRENT_NAME      = 'jcfCurrentEditorWindow';
  FORMAT_PROJECT_MENU_NAME = 'jcfAllFilesinProject';
  FORMAT_OPEN_MENU_NAME    = 'jcfAllOpenWindows';
  FORMAT_SETTINGS_MENU_NAME = 'jcfFormatSettings';
  FORMAT_ABOUT_MENU_NAME   = 'jcfAbout';
  FORMAT_CATEGORY_IDECMD_NAME = 'jcfFormat';
  FORMAT_MENU_SECTION1     = 'jcfSection1';
  FORMAT_MENU_SECTION2     = 'jcfSection2';

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
  if not FileExists(JCFOptsFile) then
  begin
    // create default
    R := TResourceStream.Create(HInstance, PChar('JCFSettings'), PChar(RT_RCDATA));
    S := TFileStream.Create(JCFOptsFile, fmCreate);
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
  Key: TIDEShortCut;
  Cmd: TIDECommand;
begin
  SetJcfUiClass(TJcfUIGUI.Create); //<< Don't erase, must be the first called function.
  SetLazarusDefaultFileName;
  GetDefaultSettingsFileName := IDEGetDefaultSettingsFileName;

  Cat := IDECommandList.CreateCategory(nil, FORMAT_CATEGORY_IDECMD_NAME,
    FORMAT_CATEGORY_IDECMD, IDECmdScopeSrcEditOnly);

  fcMainMenu := RegisterIDESubMenu(itmSourceTools, FORMAT_MENU_NAME, FORMAT_MENU);

  // Ctrl + D
  Key := IDEShortCut(VK_D, [SSctrl]);
  Cmd := RegisterIDECommand(Cat, FORMAT_CURRENT_NAME, FORMAT_CURRENT_IDECMD,
    Key, lcJCFIDE.DoFormatCurrentIDEWindow);
  RegisterIDEMenuCommand(fcMainMenu, FORMAT_CURRENT_NAME, FORMAT_CURRENT_MENU,
    lcJCFIDE.DoFormatCurrentIDEWindow, nil, Cmd);

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

var
  // a module var
  mcIDEFormattingSettings: TIDEFormattingSettings = nil;

constructor TIDEFormattingSettings.Create(const pbReadRegFile: boolean);
begin
  inherited Create();
  FormattingSettings;  //create JCF FormattingSetting if not exitst.
end;

destructor TIDEFormattingSettings.Destroy;
begin
  if FormattingSettings.WriteOnExit then
    FormattingSettings.Write;
  inherited;
end;

function IDEFormattingSettings: TIDEFormattingSettings;
begin
  if mcIDEFormattingSettings = nil then
    mcIDEFormattingSettings := TIDEFormattingSettings.Create(true);
  Result := mcIDEFormattingSettings;
end;

class function TIDEFormattingSettings.GetGroupCaption: String;
begin
  Result := lisJCFFormatSettings;
end;

class function TIDEFormattingSettings.GetInstance: TAbstractIDEOptions;
begin
  Result := IDEFormattingSettings;
end;

procedure TIDEFormattingSettings.DoAfterWrite(Restore: boolean);
begin
  { settings are now in need of saving }
  FormattingSettings.Dirty := True;
  { check consistency of settings }
  FormattingSettings.MakeConsistent;
  { save to file }
  FormattingSettings.Write;
end;


initialization
  lcJCFIDE := TJcfIdeMain.Create;
  // IDEFormattingSettings;
  JCFOptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(JCFOptionsGroup, TIDEFormattingSettings);

finalization
  FreeAndNil(lcJCFIDE);
  FreeAndNil(mcIDEFormattingSettings);
end.

