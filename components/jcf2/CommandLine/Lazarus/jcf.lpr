program JCF;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JCF, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele. 

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
uses
  SysUtils, ConvertTypes, FileConverter, SettingsStream,
  JcfSettings, JcfRegistrySettings, WarnImbalancedComment,
  JcfMiscFunctions, JcfStringUtils, StatusMessageReceiver,
  JcfUiTools, JcfUiToolsNoGUI, CommandLineReturnCode, CommandLineConstants;

const
  CONFIG_FILE_NAME = 'jcfsettings.cfg';

var
  feReturnCode: TJcfCommandLineReturnCode;
  fbCmdLineShowHelp: boolean = false;
  fbQuietFail: boolean = false;

  fbCmdLineObfuscate: boolean;

  fbHasSourceMode:     boolean;
  feCmdLineSourceMode: TSourceMode;

  fbHasBackupMode:     boolean;
  feCmdLineBackupMode: TBackupMode;

  fbYesAll: boolean;

  fbHasNamedConfigFile: boolean;
  fsConfigFileName:     string;

  lcStatus:  TStatusMesssageReceiver;

  function StripParamPrefix(const ps: string): string;
  begin
    Result := ps;

    if StrLeft(Result, 1) = '/' then
      Result := StrRestOf(Result, 2);
    if StrLeft(ps, 1) = '\' then
      Result := StrRestOf(Result, 2);
    if StrLeft(Result, 1) = '-' then
      Result := StrRestOf(Result, 2);
  end;

  procedure ParseCommandLine;
  var
    liLoop: integer;
    lsOpt:  string;
    lsPath: string;
  begin
    fbCmdLineShowHelp := (ParamCount = 0);
    fbQuietFail := False;
    fbCmdLineObfuscate := False;
    fbHasSourceMode := False;
    fbHasBackupMode := False;
    fbYesAll := False;
    fbHasNamedConfigFile := False;
    fsConfigFileName := '';
    lsPath := '';

    for liLoop := 1 to ParamCount do
    begin
    { look for something that is not a -/\ param }
      lsOpt := ParamStr(liLoop);

      if (StrLeft(lsOpt, 1) <> '-') and {$IFNDEF UNIX} (StrLeft(lsOpt, 1) <> '/') and {$ENDIF}
        (StrLeft(lsOpt, 1) <> '\') and (StrLeft(lsOpt, 1) <> '?') then
      begin
      // must be a path
        lsPath := StrTrimQuotes(lsOpt);
        continue;
      end;

      lsOpt := StripParamPrefix(lsOpt);

      if lsOpt = '?' then
      begin
        fbCmdLineShowHelp := True;
        break;
      end
      else if AnsiSameText(lsOpt, 'obfuscate') then
      begin
        fbCmdLineObfuscate := True;
      end
      else if AnsiSameText(lsOpt, 'clarify') then
      begin
        fbCmdLineObfuscate := False;
      end

      else if AnsiSameText(lsOpt, 'inplace') then
      begin
        fbHasBackupMode     := True;
        feCmdLineBackupMode := cmInPlace;
      end
      else if AnsiSameText(lsOpt, 'out') then
      begin
        fbHasBackupMode     := True;
        feCmdLineBackupMode := cmSeparateOutput;
      end
      else if AnsiSameText(lsOpt, 'backup') then
      begin
        fbHasBackupMode     := True;
        feCmdLineBackupMode := cmInPlaceWithBackup;
      end

      else if AnsiSameText(lsOpt, 'f') then
      begin
        fbHasSourceMode     := True;
        feCmdLineSourceMode := fmSingleFile;
      end
      else if AnsiSameText(lsOpt, 'd') then
      begin
        fbHasSourceMode     := True;
        feCmdLineSourceMode := fmDirectory;
      end
      else if AnsiSameText(lsOpt, 'r') then
      begin
        fbHasSourceMode     := True;
        feCmdLineSourceMode := fmDirectoryRecursive;
      end
      else if AnsiSameText(lsOpt, 'y') then
      begin
        fbYesAll := True;
      end
      else if StrFind('config', lsOpt) = 1 then
      begin
        fbHasNamedConfigFile := True;
        fsConfigFileName     := StrAfter('=', lsOpt);
      end
      else
      begin
        WriteLn('Unknown option ' + StrDoubleQuote(lsOpt));
        WriteLn;
        fbCmdLineShowHelp := True;
        break;
      end;
    end; // for loop

    if lsPath = '' then
    begin
      WriteLn('No path found');
      WriteLn;
      fbCmdLineShowHelp := True;
      feReturnCode := rcNoPathFound;
    end;

  { read settings from file? }
    if fbHasNamedConfigFile and (fsConfigFileName <> '') then
    begin
      if FileExists(fsConfigFileName) then
      begin
        FormatSettingsFromFile(fsConfigFileName);
      end
      else
      begin
        WriteLn('Named config file ' + fsConfigFileName + ' was not found');
        WriteLn;
        fbQuietFail := True;
        feReturnCode := rcConfigFileNotFound;
      end;
    end
    else
    begin  // read default file configuration
      fsConfigFileName:=CONFIG_FILE_NAME;
      if FileExists(fsConfigFileName) then
      begin
        FormatSettingsFromFile(fsConfigFileName);
      end
      else
      begin
        WriteLn('Named config file ' + fsConfigFileName + ' was not found');
        WriteLn;
        fbQuietFail := True;
        feReturnCode := rcConfigFileNotFound;
      end;
    end;

    { must have read from registry or file }
    if (not FormattingSettings.HasRead) and (not fbQuietFail) then
    begin
        WriteLn('No settings to read');
        WriteLn;
        fbQuietFail := True;
        if feReturnCode = rcSuccess then
        begin
          feReturnCode := rcSettingsNotRead;
        end;
    end;

  { write to settings }
    if fbHasSourceMode then
      GetRegSettings.SourceMode := feCmdLineSourceMode;
    if fbHasBackupMode then
      GetRegSettings.BackupMode := feCmdLineBackupMode;

    if not fbCmdLineShowHelp then
    begin
      if GetRegSettings.SourceMode = fmSingleFile then
      begin
        if not FileExists(lsPath) then
        begin
          WriteLn('File ' + StrDoubleQuote(lsPath) + ' not found');
          fbQuietFail := True;
          feReturnCode := rcFileNotFound;
        end;
      end
      else
      begin
        if not DirectoryExists(lsPath) then
        begin
          WriteLn('Directory ' + StrDoubleQuote(lsPath) + ' not found');
          fbQuietFail := True;
          feReturnCode := rcDirectoryNotFound;
        end;
      end;
    end;

    GetRegSettings.Input := lsPath;
    FormattingSettings.Obfuscate.Enabled := fbCmdLineObfuscate;
  end;

  procedure ConvertFiles;
  var
    lcConvert: TFileConverter;
  begin
    lcConvert := TFileConverter.Create;
    try
      lcConvert.OnStatusMessage := lcStatus.OnReceiveStatusMessage;
      // use command line settings
      lcConvert.YesAll := fbYesAll;
      lcConvert.GuiMessages := False;
      lcConvert.SourceMode := GetRegSettings.SourceMode;
      lcConvert.BackupMode := GetRegSettings.BackupMode;
      lcConvert.Input := GetRegSettings.Input;
      // do it!
      lcConvert.Convert;

      if lcConvert.ConvertError then
      begin
        feReturnCode := rcConvertError;
      end;

    finally
      lcConvert.Free;
    end;
  end;

  // we want lowercase filename the same as Lazarus ide.
  function CmdLineDefGetDefaultSettingsFileName: string;
  begin
    Result := IncludeTrailingPathDelimiter(GetApplicationFolder) + CONFIG_FILE_NAME;
  end;


{ main program starts here }
begin
  SetJcfUiClass(TJcfUINoGUI.Create);  // must be the first action.
  GetDefaultSettingsFileName := CmdLineDefGetDefaultSettingsFileName;
  feReturnCode := rcSuccess;
  { read registry }
  GetRegSettings.ReadAll;

  lcStatus := TStatusMesssageReceiver.Create;

  ParseCommandLine;

  if fbCmdLineShowHelp then
     fbQuietFail:=false;

  { format setttings will be altered by the command line.
    Do not persist these changes
    do this after parsing the command line }
  FormattingSettings.WriteOnExit := False;

  if fbQuietFail then
  begin
    // do nothing
  end
  else if fbCmdLineShowHelp then
  begin
    WriteLn(ABOUT_COMMANDLINE);
  end
  else
  begin
    ConvertFiles;
  end;

  // keep for debuginng in the lazarus ide.
  //Write('Press enter to end');
  //ReadLn;

  FreeAndNil(lcStatus);

  HaltOnError(feReturnCode);
end.
