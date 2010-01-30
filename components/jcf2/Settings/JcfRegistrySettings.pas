unit JcfRegistrySettings;

{ AFS 2 Jan 2002
  Store Gui state in registry

  This is not the format options file, that lives in a file so that it can be shared
  This registry file is intended to
   - tell you where the format options file is
   - other GUI config settings that should not be shared, ie
   - logging
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfRegistrySettings, released May 2003.
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

interface

uses
  { delphi }
  Registry, Classes,
  { local }
  ConvertTypes;

type
  TLogLevel = (eLogErrorsOnly, eLogFiles, eLogTokens);
  TLogPlace = (eLogTempDir, eLogAppDIr, eLogSpecifiedDir);
  TFormatFileWriteOption = (eAlwaysWrite, eQuietFail, eNeverWrite);


  TJCFRegistrySettings = class(TObject)
  private
    fcReg:     TRegIniFile;
    fbHasRead: boolean;

    { general settings }
    fShowParseTreeOption: TShowParseTreeOption;

    { ui settings }
    fsFormatConfigFileName:      string;
    fbFormatConfigNameSpecified: boolean;

    fsLastSettingsPage:      string;
    feFormatFileWriteOption: TFormatFileWriteOption;

    {notepad settings }
    fsInputDir:  string;
    fsOutputDir: string;

    { MRU files settings }
    fiMRUMaxItems: integer;

    { log settings }
    feLogLevel: TLogLevel;
    feLogPlace: TLogPlace;
    fsSpecifiedDirectory: string;
    fbViewLogAfterRun: boolean;
    fbLogTime:  boolean;
    fbLogStats: boolean;

    { file settings }
    feBackupMode:      TBackupMode;
    feSourceMode:      TSourceMode;
    fsBackupExtension, fsOutputExtension: string;
    fsInput:           string;
    fcExclusionsFiles: TStringList;
    fcExclusionsDirs:  TStringList;
    fbCheckMultiByteChars: boolean;

    { this is ref not owned }
    fcMRUFiles: TStrings;

    fbEditorIntegration: boolean;
    fbFormatBeforeSave:  boolean;
    fbFormatAfterLoad:   boolean;

    procedure ReadMRUFiles;
    procedure WriteMRUFiles;

    procedure ReadStrings(const psSection, psKey: string; pcStrings: TStrings);
    procedure WriteStrings(const psSection, psKey: string; pcStrings: TStrings);
    function GetBackupExtension: string;
    function GetOutputExtension: string;

  public
    constructor Create;
    destructor Destroy; override;

    function CanClearMRU: boolean;
    procedure ClearMRU;

    procedure ReadAll;
    procedure WriteAll;

    property HasRead: boolean Read fbHasRead;

    { general settings }
    property FormatConfigFileName: string Read fsFormatConfigFileName
      Write fsFormatConfigFileName;
    property FormatConfigNameSpecified: boolean Read fbFormatConfigNameSpecified;

    property FormatFileWriteOption: TFormatFileWriteOption
      Read feFormatFileWriteOption Write feFormatFileWriteOption;

    { ui settings }
    property ShowParseTreeOption: TShowParseTreeOption
      Read fShowParseTreeOption Write fShowParseTreeOption;
    property LastSettingsPage: string Read fsLastSettingsPage Write fsLastSettingsPage;

    { notepad settings }
    property InputDir: string Read fsInputDir Write fsInputDir;
    property OutputDir: string Read fsOutputDir Write fsOutputDir;

    { MRU files settings }
    property MRUMaxItems: integer Read fiMRUMaxItems Write fiMRUMaxItems;
    property MRUFiles: TStrings Read fcMRUFiles Write fcMRUFiles;

    { log settings }
    function LogDirectory: string;
    function LogFileName: string;

    property LogLevel: TLogLevel Read feLogLevel Write feLogLevel;
    property LogPlace: TLogPlace Read feLogPlace Write feLogPlace;
    property SpecifiedDirectory: string Read fsSpecifiedDirectory
      Write fsSpecifiedDirectory;

    property ViewLogAfterRun: boolean Read fbViewLogAfterRun Write fbViewLogAfterRun;
    property LogTime: boolean Read fbLogTime Write fbLogTime;
    property LogStats: boolean Read fbLogStats Write fbLogStats;

    procedure ViewLog;

    { files settings }
    property BackupMode: TBackupMode Read feBackupMode Write feBackupMode;
    property SourceMode: TSourceMode Read feSourceMode Write feSourceMode;
    property BackupExtension: string Read GetBackupExtension Write fsBackupExtension;
    property OutputExtension: string Read GetOutputExtension Write fsOutputExtension;
    property CheckMultiByteChars: boolean Read fbCheckMultiByteChars
      Write fbCheckMultiByteChars;

    function GetOutputFileName(const psIn: string): string; overload;
    function GetOutputFileName(const psIn: string; peMode: TBackupMode): string;
      overload;

    function Output: string;

    function FileIsExcluded(const psFile: string): boolean;
    function DirIsExcluded(const psDir: string): boolean;

    property Input: string Read fsInput Write fsInput;

    property ExclusionsFiles: TStringList Read fcExclusionsFiles;
    property ExclusionsDirs: TStringList Read fcExclusionsDirs;

    { IDE integration settings }
    property EditorIntegration: boolean Read fbEditorIntegration
      Write fbEditorIntegration;
    property FormatBeforeSave: boolean Read fbFormatBeforeSave Write fbFormatBeforeSave;
    property FormatAfterLoad: boolean Read fbFormatAfterLoad Write fbFormatAfterLoad;
  end;

function GetRegSettings: TJCFRegistrySettings;

var
  GetDefaultSettingsFileName: function: String;

implementation

uses
  { delphi }
  {$ifndef fpc}Windows,{$endif} SysUtils, Dialogs,
  { jcf }
  JcfStringUtils, JcfSystemUtils, JcfMiscFunctions;

const
  REG_GENERAL_SECTION = 'General';
  REG_NOTEPAD_SECTION = 'NotepadSettings';
  REG_MRU_FILES_SECTION = 'MRUFiles';
  REG_LOG_SECTION = 'Log';
  REG_UI_SECTION  = 'UI';
  REG_FILES_SECTION = 'Files';
  REG_IDE_SECTION = 'IDE';

  REG_LOG_LEVEL = 'LogLevel';
  REG_LOG_PLACE = 'LogPlace';
  REG_SPECIFIED_DIRECTORY = 'SpecifiedDirectory';
  REG_VIEW_LOG_AFTER_RUN = 'ViewLogAfterRun';
  REG_LOG_TIME  = 'LogTime';
  REG_LOG_STATS = 'LogStats';

  REG_LAST_SETTINGS_PAGE = 'LastSettingsPage';

  REG_INPUT = 'Input';

  REG_BACKUP_MODE = 'BackupMode';
  REG_SOURCE_MODE = 'SourceMode';
  REG_BACKUP_EXT  = 'BackupExt';
  REG_OUTPUT_EXT  = 'OutputExt';

  REG_EXCLUSIONS_FILES = 'ExclusionsFiles';
  REG_EXCLUSIONS_DIRS  = 'ExclusionsDirs';

  REG_CHECK_MULTIBYTE_CHARS = 'CheckMultiByteChars';

  REG_EDITOR_INTEGRATION = 'EditorIntegration';
  REG_FORMAT_BEFORE_SAVE = 'FormatBeforeSave';
  REG_FORMAT_AFTER_LOAD  = 'FormatAfterLoad';

{
  file-based settings,  ie
  - read from the settings file if it exists, else use the registry
  - always write to the file
 }
function DefGetDefaultSettingsFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetApplicationFolder) + 'JCFSettings.cfg';
end;

constructor TJCFRegistrySettings.Create;
var
  Registry: TRegistry;
begin
  inherited;

  // Move old registry content to new registry location if applicable
  Registry := TRegistry.Create;
  try
    Registry.Access  := KEY_ALL_ACCESS;
    Registry.RootKey := HKEY_CURRENT_USER;
    if not (Registry.OpenKey(REG_ROOT_KEY, False)) then
    begin
      if (Registry.OpenKey(OLD_REG_ROOT_KEY, False)) then
      begin
        Registry.MoveKey(OLD_REG_ROOT_KEY, REG_ROOT_KEY, True);
        Registry.CloseKey;
        if (Registry.OpenKey('\Software\Jedi', False)) then
        begin
          if not Registry.HasSubKeys then
          begin
            Registry.CloseKey;
            if (Registry.OpenKey('\Software', False)) then
            begin
              Registry.DeleteKey('Jedi');
              Registry.CloseKey;
            end;
          end
          else
            Registry.CloseKey;
        end;
      end;
    end
    else
      Registry.CloseKey;
  finally
    Registry.Free;
  end;

  // New registry location
  fcReg := TRegIniFile.Create(REG_ROOT_KEY);

  fcExclusionsFiles := TStringList.Create;
  fcExclusionsDirs  := TStringList.Create;
end;

destructor TJCFRegistrySettings.Destroy;
begin
  FreeAndNil(fcExclusionsFiles);
  FreeAndNil(fcExclusionsDirs);

  FreeAndNil(fcReg);
  inherited;
end;

procedure TJCFRegistrySettings.ReadStrings(const psSection, psKey: string;
  pcStrings: TStrings);
var
  lsKey, lsValue: string;
  liCount: integer;
begin
  Assert(pcStrings <> nil);
  pcStrings.Clear;

  liCount := 0;

  while True do
  begin
    lsKey := psKey + IntToStr(liCount);
    lsValue := fcReg.ReadString(psSection, lsKey, '');
    if lsValue = '' then
      break // done
    else
      pcStrings.Add(lsValue);

    Inc(liCount);
  end;
end;


procedure TJCFRegistrySettings.WriteStrings(const psSection, psKey: string;
  pcStrings: TStrings);
var
  lsKey:  string;
  liLoop: integer;
begin
  Assert(pcStrings <> nil);

  for liLoop := 0 to pcStrings.Count - 1 do
  begin
    lsKey := psKey + IntToStr(liLoop);
    fcReg.WriteString(psSection, lsKey, pcStrings.Strings[liLoop]);
  end;

  // null-terminate the list
  lsKey := psKey + IntToStr(pcStrings.Count);
  fcReg.WriteString(psSection, lsKey, '');
end;


procedure TJCFRegistrySettings.ReadMRUFiles;
var
  lcItems: TStringList;
  liLoop:  integer;
begin
  if fcMRUFiles = nil then
    exit;

  fcMRUFiles.Clear;

  lcItems := TStringList.Create;
  try
    ReadStrings(REG_MRU_FILES_SECTION, 'MRUFile', lcItems);

    { add them in reverse order to work around a bug in TJvMRUManager
      where the order reverses every time it is read/written
    }
    for liLoop := lcItems.Count - 1 downto 0 do
    begin
      fcMRUFiles.Add(lcItems.Strings[liLoop]);
    end;

  finally
    lcItems.Free;
  end;
end;


procedure TJCFRegistrySettings.WriteMRUFiles;
begin
  if fcMRUFiles = nil then
    exit;

  WriteStrings(REG_MRU_FILES_SECTION, 'MRUFile', fcMRUFiles);
end;

procedure TJCFRegistrySettings.ReadAll;
begin
  { general section }
  fsFormatConfigFileName := fcReg.ReadString(REG_GENERAL_SECTION,
    'FormatConfigFileName', '');
  // was a config file specified
  fbFormatConfigNameSpecified := (fsFormatConfigFileName <> '');

  if not fbFormatConfigNameSpecified then
  begin
    fsFormatConfigFileName := GetDefaultSettingsFileName;
  end;

  feFormatFileWriteOption := TFormatFileWriteOption(
    fcReg.ReadInteger(REG_GENERAL_SECTION, 'FormatFileWriteOption', Ord(eAlwaysWrite)));

  {notepad settings }
  InputDir  := fcReg.ReadString(REG_NOTEPAD_SECTION, 'InputDir', '');
  OutputDir := fcReg.ReadString(REG_NOTEPAD_SECTION, 'OutputDir', '');

  { MRU section }
  MRUMaxItems := fcReg.ReadInteger(REG_NOTEPAD_SECTION, 'MRUMaxItems', 6);
  ReadMRUFiles;

  { log section }
  feLogLevel := TLogLevel(fcReg.ReadInteger(REG_LOG_SECTION, REG_LOG_LEVEL,
    Ord(eLogFiles)));
  feLogPlace := TLogPlace(fcReg.ReadInteger(REG_LOG_SECTION, REG_LOG_PLACE,
    Ord(eLogTempDir)));
  fsSpecifiedDirectory := fcReg.ReadString(REG_LOG_SECTION,
    REG_SPECIFIED_DIRECTORY, 'c:\');
  fbViewLogAfterRun := fcReg.ReadBool(REG_LOG_SECTION, REG_VIEW_LOG_AFTER_RUN, False);
  fbLogTime  := fcReg.ReadBool(REG_LOG_SECTION, REG_LOG_TIME, False);
  fbLogStats := fcReg.ReadBool(REG_LOG_SECTION, REG_LOG_STATS, False);

  { ui }
  fsLastSettingsPage  := fcReg.ReadString(REG_UI_SECTION, REG_LAST_SETTINGS_PAGE, '');
  ShowParseTreeOption := TShowParseTreeOption(
    fcReg.ReadInteger(REG_UI_SECTION, 'ParseTreeOption', Ord(eShowOnError)));

  { files}
  feBackupMode := TBackupMode(fcReg.ReadInteger(REG_FILES_SECTION,
    REG_BACKUP_MODE, Ord(cmSeparateOutput)));
  feSourceMode := TSourceMode(fcReg.ReadInteger(REG_FILES_SECTION,
    REG_SOURCE_MODE, Ord(fmSingleFile)));
  fsInput := fcReg.ReadString(REG_FILES_SECTION, REG_INPUT, '');

  fsBackupExtension := fcReg.ReadString(REG_FILES_SECTION, REG_BACKUP_EXT, 'bak');
  fsOutputExtension := fcReg.ReadString(REG_FILES_SECTION, REG_OUTPUT_EXT, 'out');

  ReadStrings(REG_FILES_SECTION, REG_EXCLUSIONS_FILES, fcExclusionsFiles);
  ReadStrings(REG_FILES_SECTION, REG_EXCLUSIONS_DIRS, fcExclusionsDirs);

  fbCheckMultiByteChars := fcReg.ReadBool(REG_FILES_SECTION,
    REG_CHECK_MULTIBYTE_CHARS, False);

  { IDE }
  fbEditorIntegration := fcReg.ReadBool(REG_IDE_SECTION, REG_EDITOR_INTEGRATION, False);
  fbFormatBeforeSave := fcReg.ReadBool(REG_IDE_SECTION, REG_FORMAT_BEFORE_SAVE, False);
  fbFormatAfterLoad := fcReg.ReadBool(REG_IDE_SECTION, REG_FORMAT_AFTER_LOAD, False);

  fbHasRead := True;
end;


procedure TJCFRegistrySettings.WriteAll;
begin
  { general section }
  fcReg.WriteString(REG_GENERAL_SECTION, 'FormatConfigFileName', fsFormatConfigFileName);
  fcReg.WriteInteger(REG_GENERAL_SECTION, 'FormatFileWriteOption',
    Ord(feFormatFileWriteOption));

  { notepad section }
  fcReg.WriteString(REG_NOTEPAD_SECTION, 'InputDir', InputDir);
  fcReg.WriteString(REG_NOTEPAD_SECTION, 'OutputDir', OutputDir);

  { mru section}
  fcReg.WriteInteger(REG_MRU_FILES_SECTION, 'MRUMaxItems', MRUMaxItems);
  WriteMRUFiles;

  { log section }
  fcReg.WriteInteger(REG_LOG_SECTION, REG_LOG_LEVEL, Ord(feLogLevel));
  fcReg.WriteInteger(REG_LOG_SECTION, REG_LOG_PLACE, Ord(feLogPlace));
  fcReg.WriteString(REG_LOG_SECTION, REG_SPECIFIED_DIRECTORY, fsSpecifiedDirectory);
  fcReg.WriteBool(REG_LOG_SECTION, REG_VIEW_LOG_AFTER_RUN, fbViewLogAfterRun);
  fcReg.WriteBool(REG_LOG_SECTION, REG_LOG_TIME, fbLogTime);
  fcReg.WriteBool(REG_LOG_SECTION, REG_LOG_STATS, fbLogStats);

  { ui section }
  fcReg.WriteString(REG_UI_SECTION, REG_LAST_SETTINGS_PAGE, fsLastSettingsPage);
  fcReg.WriteInteger(REG_UI_SECTION, 'ParseTreeOption', Ord(ShowParseTreeOption));

  { files section }
  fcReg.WriteInteger(REG_FILES_SECTION, REG_BACKUP_MODE, Ord(feBackupMode));
  fcReg.WriteInteger(REG_FILES_SECTION, REG_SOURCE_MODE, Ord(feSourceMode));

  fcReg.WriteString(REG_FILES_SECTION, REG_INPUT, fsInput);
  fcReg.WriteString(REG_FILES_SECTION, REG_BACKUP_EXT, fsBackupExtension);
  fcReg.WriteString(REG_FILES_SECTION, REG_OUTPUT_EXT, fsOutputExtension);

  WriteStrings(REG_FILES_SECTION, REG_EXCLUSIONS_FILES, fcExclusionsFiles);
  WriteStrings(REG_FILES_SECTION, REG_EXCLUSIONS_DIRS, fcExclusionsDirs);

  fcReg.WriteBool(REG_FILES_SECTION, REG_CHECK_MULTIBYTE_CHARS, fbCheckMultiByteChars);

  { IDE }
  fcReg.WriteBool(REG_IDE_SECTION, REG_EDITOR_INTEGRATION, fbEditorIntegration);
  fcReg.WriteBool(REG_IDE_SECTION, REG_FORMAT_BEFORE_SAVE, fbFormatBeforeSave);
  fcReg.WriteBool(REG_IDE_SECTION, REG_FORMAT_AFTER_LOAD, fbFormatAfterLoad);
end;

function TJCFRegistrySettings.CanClearMRU: boolean;
begin
  Result := (MRUFiles <> nil) and (MRUFiles.Count > 0);
end;

procedure TJCFRegistrySettings.ClearMRU;
begin
  if MRUFiles <> nil then
    MRUFiles.Clear;
end;


function TJCFRegistrySettings.LogDirectory: string;
begin
  case feLogPlace of
    eLogTempDir:
      Result := GetWindowsTempFolder;
    eLogAppDir:
      Result := ExtractFileDir(ParamStr(0));
    eLogSpecifiedDir:
      Result := fsSpecifiedDirectory;
  end;

  Result := IncludeTrailingPathDelimiter(Result);
end;

function TJCFRegistrySettings.LogFileName: string;
begin
  Result := LogDirectory + 'JediCodeFormat.log';
end;

procedure TJCFRegistrySettings.ViewLog;
var
  lsFile: string;
begin
  lsFile := LogFileName;

  if FileExists(lsFile) then
  begin
    ShellExecEx('notepad.exe ', lsFile);
  end
  else
    ShowMessage('No log file found at ' + lsFile);
end;

function TJCFRegistrySettings.DirIsExcluded(const psDir: string): boolean;
var
  liPos: integer;
  lsBareDir: string;
begin
  { exact match  }
  Result := (fcExclusionsDirs.IndexOf(psDir) >= 0);

  if not Result then
  begin
    liPos := StrLastPos('/', psDir);
    if liPos > 0 then
    begin
      lsBareDir := StrRestOf(psDir, liPos + 1);
      Result := (fcExclusionsDirs.IndexOf(lsBareDir) >= 0);
    end;
  end;
end;

function TJCFRegistrySettings.FileIsExcluded(const psFile: string): boolean;
var
  lsBareFile: string;
begin
  { exact match  }
  Result := (fcExclusionsFiles.IndexOf(psFile) >= 0);

  if not Result then
  begin
    // no exact match? then extract the bare file name - no path or ext
    lsBareFile := PathExtractFileNameNoExt(psFile);
    Result := (fcExclusionsFiles.IndexOf(lsBareFile) >= 0);
  end;
end;

function TJCFRegistrySettings.GetOutputFileName(const psIn: string;
  peMode: TBackupMode): string;
var
  lsExt: string;
  liMainFileNameLength: integer;
begin
  if PathExtractFileNameNoExt(psIn) = '' then
  begin
    Result := '';
    exit;
  end;

  if (peMode = cmInPlace) then
  begin
    Result := '';
  end
  else if peMode in [cmInPlaceWithBackup, cmSeparateOutput] then
  begin
    lsExt  := ExtractFileExt(psIn);
    liMainFileNameLength := Length(psIn) - Length(lsExt);
    Result := StrLeft(psIn, liMainFileNameLength);

    if peMode = cmInPlaceWithBackup then
      Result := Result + '.' + BackupExtension
    else
      Result := Result + '.' + OutputExtension;
  end
  else
    raise Exception.Create('TCodeFormatSettings.Output: bad backup mode ');
end;

function TJCFRegistrySettings.GetOutputFileName(const psIn: string): string;
begin
  // use the currently selected mode
  Result := GetOutputFileName(psIn, BackupMode);
end;

function TJCFRegistrySettings.Output: string;
begin
  Result := GetOutputFileName(Input);
end;

function TJCFRegistrySettings.GetBackupExtension: string;
begin
  if fsBackupExtension = '' then
    Result := 'bak'
  else
    Result := fsBackupExtension;
end;

function TJCFRegistrySettings.GetOutputExtension: string;
begin
  if fsOutputExtension = '' then
    Result := 'out'
  else
    Result := fsOutputExtension;
end;

{--------------------------------------------
  singleton accessor }

var
  mcRegistrySettings: TJCFRegistrySettings = nil;

function GetRegSettings: TJCFRegistrySettings;
begin
  if mcRegistrySettings = nil then
  begin
    mcRegistrySettings := TJCFRegistrySettings.Create;
    mcRegistrySettings.ReadAll;
  end;

  Result := mcRegistrySettings;
end;

initialization
  GetDefaultSettingsFileName := DefGetDefaultSettingsFileName;

finalization
  FreeAndNil(mcRegistrySettings);
end.
