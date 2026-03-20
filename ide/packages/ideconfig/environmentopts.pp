{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    This unit defines a class to store the options in a xml file.

}
unit EnvironmentOpts;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
{$ifdef Windows}
  ShlObj,
{$endif}
  Classes, SysUtils, Contnrs, System.UITypes,
  // LazUtils
  LazFileUtils, FileUtil, LazFileCache, LazConfigStorage, LazUTF8, LazStringUtils,
  Laz2_XMLCfg, Laz2_DOM,
  // CodeTools
  FileProcs, SourceChanger, CodeCompletionTool,
  // BuildIntf
  ProjectIntf, IDEOptionsIntf, IDEExternToolIntf,
  // IdeConfig
  IDEOptionDefs, RecentListProcs, SearchPathProcs, LazConf, TransferMacros,
  ModeMatrixOpts;

const
  EnvOptsVersion: integer = 112;
  // 107 added Lazarus version.
  // 108 added LastCalledByLazarusFullPath.
  // 109 changed paths for desktop settings, supporting multiple desktops.
  // 110 changed BackupType to string instead of integer.
  // 111 refactored code dealing with options, split into many units,
  //     removed LCL dependency from EnvironmentOpts.
  // 112 Changed Window (sub) options: Added IDETitleBarCustomText
  {$IFDEF Windows}
  DefaultMakefilename = '$Path($(CompPath))make.exe';
  {$ELSE}
    {$IFDEF FreeBSD}
    DefaultMakefilename = 'gmake';
    {$ELSE}
    DefaultMakefilename = 'make';
    {$ENDIF}
  {$ENDIF}
  RestoreProjectClosed = '-';
  DefaultMaxRecentOpenFiles = 10;
  DefaultMaxRecentProjectFiles = 5;
  DefaultMaxRecentPackageFiles = 10;
  DefaultAutoSaveIntervalInSecs = 300;
  DefaultDropDownCount = 8;

  //----------------------------------------------------------------------------
  
type
  TParseString = record
    UnparsedValue: string;
    ParsedValue: string;
    ParseStamp: integer;
    Parsing: boolean;
  end;

  { Backup }
type
  TBackupType = (
     bakNone,             // no backup files
     bakSymbolInFront,    // .~pp
     bakSymbolBehind,     // .pp~
     bakCounter,          // .pp;1
     bakUserDefinedAddExt,// .pp.xxx
     bakSameName          // .pp  only available if backuping into subdirectory
   );

  TBackupInfo = record
    BackupType: TBackupType;
    AdditionalExtension:string;  // for bakUserDefinedAddExt
    MaxCounter: integer;         // for bakCounter
    SubDirectory: string;
  end;
const
  // Important: When changing any of these values increase EnvOptsVersion
  //            and add code to read old options
  DefaultBackupTypeProject = bakSameName;
  DefaultBackupTypeOther = bakUserDefinedAddExt;
  DefaultBackupAddExt = 'bak';
  DefaultBackupMaxCounter = 9;
  DefaultBackupSubDirectory = 'backup';
  DefaultStarDirectoryExcludes = '.*;'+DefaultBackupSubDirectory;

{ Naming }

type
  TPascalExtType = (petNone, petPAS, petPP, petP);

const
  PascalExtension: array[TPascalExtType] of string = ('', '.pas', '.pp', '.p');


  { Ambiguous files }
type
  TAmbiguousFileAction = (
      afaAsk,
      afaAutoDelete,
      afaAutoRename,
      afaWarnOnCompile,
      afaIgnore
    );
  TAmbiguousFileActions = set of TAmbiguousFileAction;
  
const
  AmbiguousFileActionNames: array[TAmbiguousFileAction] of string = (
      'Ask',
      'AutoDelete',
      'AutoRename',
      'WarnOnCompile',
      'Ignore'
    );

type
  TCharCaseFileAction = (
      ccfaAsk, // before saving as non lowercase, ask
      ccfaAutoRename, // auto lowercase
      ccfaIgnore // don't ask, save whatever case
    );
  TCharCaseFileActions = set of TCharCaseFileAction;

const
  CharCaseFileActionNames: array[TCharCaseFileAction] of string = (
      'Ask',
      'AutoRename',
      'Ignore'
    );

type
  TUnitRenameReferencesAction = (
    urraAlways, // update references in other files
    urraAsk,    // scan, then ask, then update
    urraNever   // don't scan, don't ask, don't update
    );
  TUnitRenameReferencesActions = set of TUnitRenameReferencesAction;

const
  UnitRenameReferencesActionNames: array[TUnitRenameReferencesAction] of string = (
      'Always',
      'Ask',
      'Never'
    );

type
  TIDEMultipleInstancesOption = (
    mioAlwaysStartNew,
    mioOpenFilesInRunning,
    mioForceSingleInstance
    );
const
  IDEMultipleInstancesOptionNames: array[TIDEMultipleInstancesOption] of string = (
    'AlwaysStartNew',      // mioAlwaysStartNew
    'OpenFilesInRunning',  // mioOpenFilesInRunning
    'ForceSingleInstance'  // mioForceSingleInstance
    );
  DefaultIDEMultipleInstancesOption = mioOpenFilesInRunning;

  { External Tools - the user menu items in the Tools menu }
type
  TBaseExternalUserTools = class
  public
    constructor Create; virtual; abstract;
    function Load(Config: TConfigStorage; const Path: string): TModalResult; virtual; abstract;
    function Save(Config: TConfigStorage; const Path: string): TModalResult; virtual; abstract;
  end;
  TExternalUserToolsClass = class of TBaseExternalUserTools;
var
  ExternalUserToolsClass: TExternalUserToolsClass; // set by ExtToolEditDlg to TExternalUserTools

type
  TEnvOptParseType = (
    eopLazarusDirectory,
    eopCompilerFilename,
    eopFPCSourceDirectory,
    eopTestBuildDirectory,
    eopMakeFilename,
    eopFPDocPaths,
    eopCompilerMessagesFilename,
    eopDebuggerFilename,
    eopDebuggerSearchPath,
    eopFppkgConfigFile
    );
  TEnvOptParseTypes = set of TEnvOptParseType;

type
  TEnvironmentOptions = class;

  TLastOpenPackagesList = class(TStringList)
  public
    function Remove(const aString: string): Boolean;
    constructor Create;
  end;

  TUseUnitDlgOptions = record
    AllUnits: Boolean;
    AddToImplementation: Boolean;
  end;

  { TIDESubOptions }

  TIDESubOptions = class(TPersistent)
  private
    FXMLCfg: TRttiXMLConfig;
    FTopPath: string;
    FVersion: integer;
  public
    procedure ReadFromXml(OnlyDesktop: boolean); virtual; abstract;
    procedure WriteToXml(OnlyDesktop: boolean); virtual; abstract;
    procedure InitConfig; virtual;
    procedure Assign(Source: TPersistent); override;
  public
    property XMLCfg: TRttiXMLConfig read FXMLCfg;
    property TopPath: String read FTopPath;
    property Version: integer read FVersion;
  end;
  TIDESubOptionsClass = class of TIDESubOptions;

  { TEnvironmentOptions - class for storing environment options }

  TEnvironmentOptions = class(TIDEEnvironmentOptions)
  private
    FFppkgCheck: boolean;
    fRegisteredSubConfig: TObjectList;
    // config file
    FFilename: string;
    FFileAge: longint;
    FFileVersion: integer;
    FFileHasChangedOnDisk: boolean;
    FMaxExtToolsInParallel: integer;
    FOldLazarusVersion: string;
    FStarDirectoryExcludes: string;
    FWheelSelectTab: boolean;
    FXMLCfg: TRttiXMLConfig;
    FConfigStore: TXMLOptionsStorage;
    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    FLastSavedProjectFile: string;
    FLastOpenPackages: TLastOpenPackagesList;//list of filenames with open packages
    // comboboxes
    FDropDownCount: Integer;
    // messages view
    FMsgViewShowFPCMsgLinesCompiled: Boolean;
    // project inspector
    FProjInspSortAlphabetically: boolean;
    FProjInspShowDirHierarchy: boolean;
    // package editor
    FPackageEditorSortAlphabetically: boolean;
    FPackageEditorShowDirHierarchy: boolean;
    FPackageEditorShowProps: boolean;
    // procedure list
    FProcedureListFilterStart: boolean;
    FAskSaveSessionOnly: boolean;
    FCheckDiskChangesWithLoading: boolean;
    // compiler + lazarus files
    FParseValues: array[TEnvOptParseType] of TParseString;
    FLazarusDirHistory: TStringList;
    FCompilerFileHistory: TStringList;
    FFPCSourceDirHistory: TStringList;
    FMakeFileHistory: TStringList;
    FTestBuildDirHistory: TStringList;
    FCompilerMessagesFileHistory: TStringList;
    FManyBuildModesSelection: TStringList;
    FBuildMatrixOptions: TBuildMatrixOptions;
    FIsGlobalMode: TStrToBoolEvent;
    // Clean build project dialog
    FCleanBuildProjOut: Boolean;
    FCleanBuildProjSrc: Boolean;
    FCleanBuildPkgOut: Boolean;
    FCleanBuildPkgSrc: Boolean;
    // Primary-config verification
    FLastCalledByLazarusFullPath: String;

    // recent files and directories
    FRecentOpenFiles: TStringList;
    FMaxRecentOpenFiles: integer;
    FRecentProjectFiles: TStringList;
    FMaxRecentProjectFiles: integer;
    FRecentPackageFiles: TStringList;
    FMaxRecentPackageFiles: integer;
    FOpenLastProjectAtStart: boolean;
    FNewProjectTemplateAtStart: string;
    FMultipleInstances: TIDEMultipleInstancesOption;
    // Prevent repopulating Recent project files menu with example projects if it was already cleared up.
    FAlreadyPopulatedRecentFiles: Boolean;
    //other recent settings
    FLastEventMethodCCResult: TCodeCreationDlgResult;
    FLastVariableCCResult: TCodeCreationDlgResult;
    FUseUnitDlgOptions: TUseUnitDlgOptions;
    // backup
    FBackupInfoProjectFiles: TBackupInfo;
    FBackupInfoOtherFiles: TBackupInfo;
    // external tools
    fExternalUserTools: TBaseExternalUserTools; // see ExtToolEditDlg.TExternalUserTools
    // naming conventions
    fPascalFileExtension: TPascalExtType;
    fCharcaseFileAction: TCharCaseFileAction;
    fAmbiguousFileAction: TAmbiguousFileAction;
    FUnitRenameReferencesAction: TUnitRenameReferencesAction;
    FAskForFilenameOnNewFile: boolean;
    FLowercaseDefaultFilename: boolean;
    // language ID (see LazarusTranslations in translations.pas)
    fLanguageID: string;
    // 'new items'
    FNewFormTemplate: string;
    FNewUnitTemplate: string;
    FFileDialogFilter: string;

    FFppkgConfigFileHistory: TStringList;

    function GetCompilerFilename: string;
    function GetCompilerMessagesFilename: string;
    function GetDebuggerSearchPath: string;
    function GetFPCSourceDirectory: string;
    function GetFPDocPaths: string;
    function GetLazarusDirectory: string;
    function GetMakeFilename: string;
    function GetSubConfig(Index: Integer): TIDESubOptions;
    function GetTestBuildDirectory: string;
    function GetFppkgConfigFile: string;
    procedure LoadNonDesktop(Path: String);
    procedure SaveNonDesktop(Path: String);
    procedure SetCompilerFilename(const AValue: string);
    procedure SetCompilerMessagesFilename(AValue: string);
    procedure SetDebuggerSearchPath(const AValue: string);
    procedure SetFPDocPaths(const AValue: string);
    procedure SetMakeFilename(const AValue: string);
    procedure SetFPCSourceDirectory(const AValue: string);
    procedure SetLazarusDirectory(const AValue: string);
    procedure SetFppkgConfigFile(AValue: string);
    procedure SetParseValue(o: TEnvOptParseType; const NewValue: string);

    procedure SetFileName(const NewFilename: string);
    function FileHasChangedOnDisk: boolean;
    procedure InitXMLCfg(CleanConfig: boolean);
    procedure FileUpdated;
    procedure SetTestBuildDirectory(const AValue: string);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
    // SubConfig
    function GetSubConfigObj(ASubConfigClass: TIDESubOptionsClass): TIDESubOptions;
    procedure RegisterSubConfig(ASubConfig: TIDESubOptions; APath: String; ALoadConf: Boolean = False);
    procedure UnRegisterSubConfig(ASubConfig: TIDESubOptions);
    function SubConfigCount: integer;
    property SubConfig[Index: Integer]: TIDESubOptions read GetSubConfig;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(OnlyDesktop: boolean);
    procedure Save(OnlyDesktop: boolean);
    property IsGlobalMode: TStrToBoolEvent read FIsGlobalMode write FIsGlobalMode;
    property Filename: string read FFilename write SetFilename;
    function GetDefaultConfigFilename: string;
    procedure CreateConfig;
    property FileVersion: integer read FFileVersion;
    property OldLazarusVersion: string read FOldLazarusVersion;

    function GetParsedLazarusDirectory: string; override;
    function GetParsedTestBuildDirectory: string;
    function GetParsedCompilerFilename: string; override;
    function GetParsedFPCSourceDirectory(FPCVer: string = ''): string;
    function GetParsedMakeFilename: string;
    function GetParsedCompilerMessagesFilename: string;
    function GetParsedFPDocPaths: string;
    function GetParsedDebuggerFilename(AProjectDbgFileName: String): string;
    function GetParsedDebuggerSearchPath: string;
    function GetParsedFppkgConfig: string; override;
    function GetParsedValue(o: TEnvOptParseType; AUnparsedValue: String = ''): string;

    // for the debugger config
    property XMLCfg: TRttiXMLConfig read FXMLCfg;
    // auto save
    // ask even if only project session needs saving
    property AskSaveSessionOnly: boolean read FAskSaveSessionOnly write FAskSaveSessionOnly;
    property AutoSaveEditorFiles: boolean read FAutoSaveEditorFiles write FAutoSaveEditorFiles;
    property AutoSaveProject: boolean read FAutoSaveProject write FAutoSaveProject;
    property AutoSaveIntervalInSecs: integer read FAutoSaveIntervalInSecs write FAutoSaveIntervalInSecs;
       
    // project inspector
    property ProjInspSortAlphabetically: boolean read FProjInspSortAlphabetically
                                                write FProjInspSortAlphabetically;
    property ProjInspShowDirHierarchy: boolean read FProjInspShowDirHierarchy
                                              write FProjInspShowDirHierarchy;
    // package editor
    property PackageEditorSortAlphabetically: boolean read FPackageEditorSortAlphabetically
                                                     write FPackageEditorSortAlphabetically;
    property PackageEditorShowDirHierarchy: boolean read FPackageEditorShowDirHierarchy
                                                   write FPackageEditorShowDirHierarchy;
    property PackageEditorShowProps: boolean read FPackageEditorShowProps
                                            write FPackageEditorShowProps;
    // procedure list
    property ProcedureListFilterStart: boolean read FProcedureListFilterStart
                                              write FProcedureListFilterStart;
    property CheckDiskChangesWithLoading: boolean read FCheckDiskChangesWithLoading
                                                 write FCheckDiskChangesWithLoading;
    // files
    property LazarusDirectory: string read GetLazarusDirectory write SetLazarusDirectory;
    property LazarusDirHistory: TStringList read FLazarusDirHistory write FLazarusDirHistory;
    property CompilerFilename: string read GetCompilerFilename write SetCompilerFilename;
    property CompilerFileHistory: TStringList read FCompilerFileHistory write FCompilerFileHistory;
    property FPCSourceDirectory: string read GetFPCSourceDirectory write SetFPCSourceDirectory;
    property FPCSourceDirHistory: TStringList read FFPCSourceDirHistory;
    property MakeFilename: string read GetMakeFilename write SetMakeFilename;
    property MakeFileHistory: TStringList read FMakeFileHistory;
    property DebuggerSearchPath: string read GetDebuggerSearchPath write SetDebuggerSearchPath;
    property FppkgCheck: boolean read FFppkgCheck write FFppkgCheck;
    property FppkgConfigFile: string read GetFppkgConfigFile write SetFppkgConfigFile;
    property FppkgConfigFileHistory: TStringList read FFppkgConfigFileHistory write FFppkgConfigFileHistory;
    property TestBuildDirectory: string read GetTestBuildDirectory write SetTestBuildDirectory;
    property TestBuildDirHistory: TStringList read FTestBuildDirHistory;
    property CompilerMessagesFilename: string read GetCompilerMessagesFilename
              write SetCompilerMessagesFilename; // non English translation file
    property CompilerMessagesFileHistory: TStringList read FCompilerMessagesFileHistory;
    // ToDo: Remove this from trunk after Lazarus 2.2.0 is out. Now for backwards compatibility.
    property ManyBuildModesSelection: TStringList read FManyBuildModesSelection;
    // global excludes for * and ** in unit paths
    property StarDirectoryExcludes: string read FStarDirectoryExcludes write FStarDirectoryExcludes;

    // Primary-config verification
    property LastCalledByLazarusFullPath: String read FLastCalledByLazarusFullPath write FLastCalledByLazarusFullPath;

    // global build options
    property BuildMatrixOptions: TBuildMatrixOptions read FBuildMatrixOptions;

    // Clean build project dialog
    property CleanBuildProjOut: Boolean read FCleanBuildProjOut write FCleanBuildProjOut;
    property CleanBuildProjSrc: Boolean read FCleanBuildProjSrc write FCleanBuildProjSrc;
    property CleanBuildPkgOut: Boolean read FCleanBuildPkgOut write FCleanBuildPkgOut;
    property CleanBuildPkgSrc: Boolean read FCleanBuildPkgSrc write FCleanBuildPkgSrc;

    // recent files and directories
    property RecentOpenFiles: TStringList read FRecentOpenFiles;
    property MaxRecentOpenFiles: integer read FMaxRecentOpenFiles
                                         write FMaxRecentOpenFiles;
    procedure AddToRecentOpenFiles(const AFilename: string); override;
    procedure RemoveFromRecentOpenFiles(const AFilename: string); override;
    Procedure GetRecentFiles(aType: TIDERecentHandler; aList : TStrings); override;
    property RecentProjectFiles: TStringList read FRecentProjectFiles;
    property MaxRecentProjectFiles: integer read FMaxRecentProjectFiles
                                            write FMaxRecentProjectFiles;
    procedure AddToRecentProjectFiles(const AFilename: string); override;
    procedure RemoveFromRecentProjectFiles(const AFilename: string); override;
    property RecentPackageFiles: TStringList read FRecentPackageFiles;
    property MaxRecentPackageFiles: integer read FMaxRecentPackageFiles
                                         write FMaxRecentPackageFiles;
    procedure AddToRecentPackageFiles(const AFilename: string); override;
    procedure RemoveFromRecentPackageFiles(const AFilename: string); override;
    property LastSavedProjectFile: string read FLastSavedProjectFile
                     write FLastSavedProjectFile; { if empty then create new project,
                                                    if '-' then do not load/create any project }
    property LastOpenPackages: TLastOpenPackagesList read FLastOpenPackages;
    property OpenLastProjectAtStart: boolean read FOpenLastProjectAtStart
                                             write FOpenLastProjectAtStart;
    property NewProjectTemplateAtStart: string read FNewProjectTemplateAtStart
                                               write FNewProjectTemplateAtStart;
    property MultipleInstances: TIDEMultipleInstancesOption read FMultipleInstances
                                                           write FMultipleInstances;
    property AlreadyPopulatedRecentFiles: Boolean read FAlreadyPopulatedRecentFiles
                                                 write FAlreadyPopulatedRecentFiles;
    // other recent settings
    property LastEventMethodCCResult: TCodeCreationDlgResult
      read FLastEventMethodCCResult write FLastEventMethodCCResult;
    property LastVariableCCResult: TCodeCreationDlgResult
      read FLastVariableCCResult write FLastVariableCCResult;
    property UseUnitDlgOptions: TUseUnitDlgOptions
      read FUseUnitDlgOptions write FUseUnitDlgOptions;
    // backup
    property BackupInfoProjectFiles: TBackupInfo read FBackupInfoProjectFiles
                                                 write FBackupInfoProjectFiles;
    property BackupInfoOtherFiles: TBackupInfo read FBackupInfoOtherFiles
                                               write FBackupInfoOtherFiles;
    // external tools
    property ExternalToolMenuItems: TBaseExternalUserTools read fExternalUserTools;
    property MaxExtToolsInParallel: integer read FMaxExtToolsInParallel
                                            write FMaxExtToolsInParallel; // 0=automatic
    // naming conventions
    property PascalFileExtension: TPascalExtType read fPascalFileExtension
                                                 write fPascalFileExtension;
    property AmbiguousFileAction: TAmbiguousFileAction read fAmbiguousFileAction
                                                     write fAmbiguousFileAction;
    property CharcaseFileAction: TCharCaseFileAction read fCharcaseFileAction
                                                     write fCharcaseFileAction;
    property UnitRenameReferencesAction: TUnitRenameReferencesAction
                                              read FUnitRenameReferencesAction
                                              write FUnitRenameReferencesAction;
    property AskForFilenameOnNewFile: boolean read FAskForFilenameOnNewFile
                                              write FAskForFilenameOnNewFile;
    property LowercaseDefaultFilename: boolean read FLowercaseDefaultFilename
                                               write FLowercaseDefaultFilename;
    // fpdoc
    property FPDocPaths: string read GetFPDocPaths write SetFPDocPaths;
    // language
    property LanguageID: string read fLanguageID write fLanguageID;
    // comboboxes
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;
    // messages view
    property MsgViewShowFPCMsgLinesCompiled: Boolean read FMsgViewShowFPCMsgLinesCompiled
                                                    write FMsgViewShowFPCMsgLinesCompiled;
    // default template for each 'new item' category: Name=Path, Value=TemplateName
    property NewFormTemplate: string read FNewFormTemplate write FNewFormTemplate;
    property NewUnitTemplate: string read FNewUnitTemplate write FNewUnitTemplate;

    // file filters
    property FileDialogFilter: string read FFileDialogFilter write FFileDialogFilter;

  published
    property WheelSelectTab: boolean read FWheelSelectTab write FWheelSelectTab;
  end;

var
  OverrideFPCVer: string = '';
  GroupEnvironmentI18NCaption: PAnsiString = nil;
  EnvironmentOptions: TEnvironmentOptions = nil;

function PascalExtToType(const Ext: string): TPascalExtType;
function AmbiguousFileActionNameToType(const Action: string): TAmbiguousFileAction;
function CharCaseFileActionNameToType(const Action: string): TCharCaseFileAction;
function UnitRenameReferencesActionNameToType(const Action: string): TUnitRenameReferencesAction;
function StrToIDEMultipleInstancesOption(const s: string): TIDEMultipleInstancesOption;
function BackupTypeToName(b: TBackupType): string;
function NameToBackupType(const s: string): TBackupType;

const
  DefaultMsgViewFocus = {$IFDEF Windows}true{$ELSE}false{$ENDIF};
  MaxComboBoxCount: integer = 20;
  EnvOptsConfFileName = 'environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';

  EnvOptParseTypeNames: array[TEnvOptParseType] of string = (
    'LazarusDir', // eopLazarusDirectory
    'CompPath', // eopCompilerFilename
    'FPCSrcDir', // eopFPCSourceDirectory
    'TestDir', // eopTestBuildDirectory
    'Make', // eopMakeFilename
    'FPDocPath', // eopFPDocPaths
    'CompMsgFile', // eopCompilerMessagesFilename
    'Debugger', // eopDebuggerFilename
    'DebugPath', // eopDebuggerSearchPath
    'FppkgConfig' // eopFppkgConfigFile
  );

function dbgs(o: TEnvOptParseType): string; overload;
function dbgs(u: TMessageLineUrgency): string; overload;

implementation

function PascalExtToType(const Ext: string): TPascalExtType;
begin
  if Ext<>'' then
    for Result:=Low(TPascalExtType) to High(TPascalExtType) do
      if CompareFilenames(Ext,PascalExtension[Result])=0 then exit;
  Result:=petNone;
end;

function AmbiguousFileActionNameToType(
  const Action: string): TAmbiguousFileAction;
begin
  for Result:=Low(TAmbiguousFileAction) to High(TAmbiguousFileAction) do
    if CompareText(AmbiguousFileActionNames[Result],Action)=0 then
      exit;
  Result:=afaAsk;
end;

function CharCaseFileActionNameToType(
  const Action: string): TCharCaseFileAction;
begin
  for Result:=Low(TCharCaseFileAction) to High(TCharCaseFileAction) do
    if CompareText(CharCaseFileActionNames[Result],Action)=0 then
      exit;
  Result:=ccfaAutoRename;
end;

function UnitRenameReferencesActionNameToType(const Action: string
  ): TUnitRenameReferencesAction;
begin
  for Result:=Low(TUnitRenameReferencesAction) to High(TUnitRenameReferencesAction) do
    if CompareText(UnitRenameReferencesActionNames[Result],Action)=0 then
      exit;
  Result:=urraAsk;
end;

function StrToIDEMultipleInstancesOption(const s: string): TIDEMultipleInstancesOption;
begin
  for Result in TIDEMultipleInstancesOption do
    if CompareText(s,IDEMultipleInstancesOptionNames[Result])=0 then exit;
  Result:=DefaultIDEMultipleInstancesOption;
end;

function BackupTypeToName(b: TBackupType): string;
begin
  Str(b,Result);
  Delete(Result,1,length('bak'));
end;

function NameToBackupType(const s: string): TBackupType;
var
  b: TBackupType;
begin
  for b in TBackupType do
    if CompareText(s,BackupTypeToName(b))=0 then exit(b);
  Result:=bakNone;
end;

function dbgs(o: TEnvOptParseType): string;
begin
  Result:=EnvOptParseTypeNames[o];
end;

function dbgs(u: TMessageLineUrgency): string;
begin
  WriteStr(Result, u);
end;

{ TIDESubOptions }

procedure TIDESubOptions.InitConfig;
begin
  ;
end;

procedure TIDESubOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  FTopPath := TIDESubOptions(Source).FTopPath;
end;

{ TLastOpenPackagesList }

constructor TLastOpenPackagesList.Create;
begin
  inherited Create;
  Sorted:=true;
  Duplicates:=dupIgnore;
end;

function TLastOpenPackagesList.Remove(const aString: string): Boolean;
var
  xIndex: Integer;
begin
  xIndex := IndexOf(aString);
  Result := xIndex >= 0;
  if Result then
    Delete(xIndex);
end;

{ TEnvironmentOptions }

constructor TEnvironmentOptions.Create;
var
  o: TEnvOptParseType;
begin
  inherited Create;
  fRegisteredSubConfig := TObjectList.Create(False);
  for o:=low(FParseValues) to high(FParseValues) do
    FParseValues[o].ParseStamp:=CTInvalidChangeStamp;

  FFilename:='';
  // language
  LanguageID:='';
  // auto save
  FAskSaveSessionOnly:=false;
  FAutoSaveEditorFiles:=true;
  FAutoSaveProject:=true;
  FAutoSaveIntervalInSecs:=DefaultAutoSaveIntervalInSecs;
  FLastSavedProjectFile:='';
  FLastOpenPackages:=TLastOpenPackagesList.Create;
  // project inspector
  FProjInspSortAlphabetically:=false;
  FProjInspShowDirHierarchy:=false;
  // package editor
  FPackageEditorSortAlphabetically:=false;
  FPackageEditorShowDirHierarchy:=false;
  FPackageEditorShowProps:=true;
  // procedure list
  FProcedureListFilterStart:=false;
  FCheckDiskChangesWithLoading:=false;
  // comboboxes
  FDropDownCount:=DefaultDropDownCount;
  // files
  LazarusDirectory:='';
  FLazarusDirHistory:=TStringList.Create;
  CompilerFilename:='';
  FCompilerFileHistory:=TStringList.Create;
  FPCSourceDirectory:='';
  FFPCSourceDirHistory:=TStringList.Create;
  MakeFilename:=DefaultMakefilename;
  FMakeFileHistory:=TStringList.Create;
  FppkgCheck:=false;
  FppkgConfigFile:='';
  FFppkgConfigFileHistory:=TStringList.Create;
  TestBuildDirectory:=GetDefaultTestBuildDirectory;
  FTestBuildDirHistory:=TStringList.Create;
  CompilerMessagesFilename:='';
  FCompilerMessagesFileHistory:=TStringList.Create;
  FManyBuildModesSelection:=TStringList.Create;
  FStarDirectoryExcludes:=DefaultStarDirectoryExcludes;

  // recent files and directories
  FRecentOpenFiles:=TStringList.Create;
  FMaxRecentOpenFiles:=DefaultMaxRecentOpenFiles;
  FRecentProjectFiles:=TStringList.Create;
  FMaxRecentProjectFiles:=DefaultMaxRecentProjectFiles;
  FRecentPackageFiles:=TStringList.Create;
  FMaxRecentPackageFiles:=DefaultMaxRecentPackageFiles;
  FOpenLastProjectAtStart:=true;
  FMultipleInstances:=DefaultIDEMultipleInstancesOption;

  // other recent settings
  FLastEventMethodCCResult.ClassSection:=icsPublic;
  FLastVariableCCResult.ClassSection:=icsPrivate;
  FLastVariableCCResult.Location:=cclLocal;

  // backup
  with FBackupInfoProjectFiles do begin
    BackupType:=DefaultBackupTypeProject;
    AdditionalExtension:=DefaultBackupAddExt;  // for bakUserDefinedAddExt
    MaxCounter:=DefaultBackupMaxCounter;       // for bakCounter
    SubDirectory:=DefaultBackupSubDirectory;
  end;
  with FBackupInfoOtherFiles do begin
    BackupType:=DefaultBackupTypeOther;
    AdditionalExtension:=DefaultBackupAddExt;  // for bakUserDefinedAddExt
    MaxCounter:=DefaultBackupMaxCounter;       // for bakCounter
    SubDirectory:=DefaultBackupSubDirectory;
  end;
  
  // external tools
  if Assigned(ExternalUserToolsClass) then
    fExternalUserTools:=ExternalUserToolsClass.Create;
  FMaxExtToolsInParallel:=0;

  // naming
  fPascalFileExtension:=petPAS;
  fCharcaseFileAction:=ccfaAutoRename;
  FUnitRenameReferencesAction:=urraAsk;
  FAskForFilenameOnNewFile:=false;
  FLowercaseDefaultFilename:=true;

  // global build options
  FBuildMatrixOptions:=TBuildMatrixOptions.Create;
end;

destructor TEnvironmentOptions.Destroy;
begin
  FreeAndNil(fRegisteredSubConfig);
  //FreeAndNil(FDesktops);
  //FreeAndNil(FDesktop);
  //FreeAndNil(FLastDesktopBeforeDebug);
  FreeAndNil(FBuildMatrixOptions);
  FreeAndNil(fExternalUserTools);
  FreeAndNil(FRecentOpenFiles);
  FreeAndNil(FRecentProjectFiles);
  FreeAndNil(FRecentPackageFiles);
  FreeAndNil(FLazarusDirHistory);
  FreeAndNil(FCompilerFileHistory);
  FreeAndNil(FFPCSourceDirHistory);
  FreeAndNil(FMakeFileHistory);
  FreeAndNil(FManyBuildModesSelection);
  FreeAndNil(FTestBuildDirHistory);
  FreeAndNil(FCompilerMessagesFileHistory);
  FreeAndNil(FConfigStore);
  FreeAndNil(FXMLCfg);
  FreeAndNil(FLastOpenPackages);
  FreeAndNil(FFppkgConfigFileHistory);
  inherited Destroy;
end;

class function TEnvironmentOptions.GetGroupCaption: string;
begin
  Result := '';
  if GroupEnvironmentI18NCaption <> nil then
    Result := GroupEnvironmentI18NCaption^;
end;

class function TEnvironmentOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := EnvironmentOptions;
end;

procedure TEnvironmentOptions.DoAfterWrite(Restore: boolean);
begin
  // Note! Data is saved when the IDE is closed.
  //if not Restore then
  //  Save(False);
  inherited DoAfterWrite(Restore);
end;

function TEnvironmentOptions.GetSubConfigObj(ASubConfigClass: TIDESubOptionsClass): TIDESubOptions;
var
  i: Integer;
begin
  i := fRegisteredSubConfig.FindInstanceOf(ASubConfigClass, True, 0);
  if i >= 0 then
    Result := SubConfig[i]
  else
    Result := Nil;
end;

procedure TEnvironmentOptions.RegisterSubConfig(ASubConfig: TIDESubOptions; APath: String;
  ALoadConf: Boolean);
//var
//  i: Integer;
begin
  //if (APath = '') or (APath = '/') then
  //  raise Exception.Create('Empty SubConfig path');
  if APath[Length(APath)] <> '/' then
    APath := APath + '/';
  if APath[1] = '/' then
    delete(APath,1 ,1);
  //for i := 0 to SubConfigCount - 1 do
  //  if SubConfig[i].FPath = APath then
  //    raise Exception.Create('Duplicate SubConfig path');
  fRegisteredSubConfig.Add(ASubConfig);
  ASubConfig.FTopPath := APath;
  ASubConfig.FXMLCfg := FXMLCfg;
  if ALoadConf then begin
    ASubConfig.InitConfig;
    ASubConfig.FVersion:=FFileVersion;
    ASubConfig.ReadFromXml(False);
  end;
end;

procedure TEnvironmentOptions.UnRegisterSubConfig(ASubConfig: TIDESubOptions);
begin
  fRegisteredSubConfig.Remove(ASubConfig);
end;

function TEnvironmentOptions.SubConfigCount: integer;
begin
  Result := fRegisteredSubConfig.Count;
end;

procedure TEnvironmentOptions.CreateConfig;
var
  ConfFileName: string;
begin
  ConfFileName:=GetDefaultConfigFilename;
  CopySecondaryConfigFile(EnvOptsConfFileName);
  if (not FileExistsUTF8(ConfFileName)) then begin
    //DebugLn('Note: environment config file not found - using defaults');
  end;
  Filename:=ConfFilename;
end;

function TEnvironmentOptions.GetParsedLazarusDirectory: string;
begin
  Result:=GetParsedValue(eopLazarusDirectory);
end;

procedure TEnvironmentOptions.SetFileName(const NewFilename: string);
begin
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
  FFileHasChangedOnDisk:=true;
end;

procedure TEnvironmentOptions.LoadNonDesktop(Path: String);

  procedure LoadBackupInfo(var BackupInfo: TBackupInfo; const Path:string;
    DefaultBackupType: TBackupType);
  var i:integer;
  begin
    with BackupInfo do begin
      if FFileVersion>=110 then begin
        BackupType:=NameToBackupType(FXMLCfg.GetValue(Path+'Type',BackupTypeToName(DefaultBackupType)));
      end else begin
        // 109 and less:
        i:=FXMLCfg.GetValue(Path+'Type',5);
        case i of
         0:BackupType:=bakNone;
         1:BackupType:=bakSymbolInFront;
         2:BackupType:=bakSymbolBehind;
         3:BackupType:=bakCounter;
         4:BackupType:=bakSameName;
        else
          BackupType:=bakUserDefinedAddExt;
        end;
      end;
      AdditionalExtension:=FXMLCfg.GetValue(Path+'AdditionalExtension',DefaultBackupAddExt);
      MaxCounter:=FXMLCfg.GetValue(Path+'MaxCounter',9); // DefaultBackupMaxCounter
      if FFileVersion<101 then
        SubDirectory:=''
      else
        SubDirectory:=FXMLCfg.GetValue(Path+'SubDirectory','backup'); // DefaultBackupSubDirectory;
    end;
  end;

begin
  // files
  LazarusDirectory:=FXMLCfg.GetValue(Path+'LazarusDirectory/Value',LazarusDirectory);
  LoadRecentList(FXMLCfg,FLazarusDirHistory,Path+'LazarusDirectory/History/',rltFile);
  if FLazarusDirHistory.Count=0 then
    FLazarusDirHistory.Add(ProgramDirectoryWithBundle);
  CompilerFilename:=TrimFilename(FXMLCfg.GetValue(
                        Path+'CompilerFilename/Value',CompilerFilename));
  LoadRecentList(FXMLCfg,FCompilerFileHistory,Path+'CompilerFilename/History/',rltFile);
  FPCSourceDirectory:=FXMLCfg.GetValue(Path+'FPCSourceDirectory/Value',FPCSourceDirectory);
  LoadRecentList(FXMLCfg,FFPCSourceDirHistory,Path+'FPCSourceDirectory/History/',rltFile);
  MakeFilename:=TrimFilename(FXMLCfg.GetValue(Path+'MakeFilename/Value',MakeFilename));
  LoadRecentList(FXMLCfg,FMakeFileHistory,Path+'MakeFilename/History/',rltFile);
  if FMakeFileHistory.Count=0 then
    GetDefaultMakeFilenames(FMakeFileHistory);
  FppkgCheck:=FXMLCfg.GetValue(Path+'FppkgCheck/Value',false);
  FppkgConfigFile:=FXMLCfg.GetValue(Path+'FppkgConfigFile/Value',FppkgConfigFile);
  LoadRecentList(FXMLCfg,FFppkgConfigFileHistory,Path+'FppkgConfigFile/History/',rltFile);

  TestBuildDirectory:=FXMLCfg.GetValue(Path+'TestBuildDirectory/Value',TestBuildDirectory);
  LoadRecentList(FXMLCfg,FTestBuildDirHistory,Path+'TestBuildDirectory/History/',rltFile);
  if FTestBuildDirHistory.Count=0 then
    GetDefaultTestBuildDirs(FTestBuildDirHistory);
  CompilerMessagesFilename:=FXMLCfg.GetValue(Path+'CompilerMessagesFilename/Value',CompilerMessagesFilename);
  LoadRecentList(FXMLCfg,FCompilerMessagesFileHistory,Path+'CompilerMessagesFilename/History/',rltFile);
  LoadRecentList(FXMLCfg,FManyBuildModesSelection,Path+'ManyBuildModesSelection/',rltCaseInsensitive);
  StarDirectoryExcludes:=FXMLCfg.GetValue(Path+'StarDirExcludes/Value',DefaultStarDirectoryExcludes);

  // Primary-config verification
  FLastCalledByLazarusFullPath:=FXMLCfg.GetValue(Path+'LastCalledByLazarusFullPath/Value','');

  // global build options, additions and overrides
  FConfigStore.AppendBasePath('BuildMatrix');
  FBuildMatrixOptions.LoadFromConfig(FConfigStore);
  FConfigStore.UndoAppendBasePath;

  // Clean build project dialog
  FCleanBuildProjOut:=FXMLCfg.GetValue(Path+'CleanBuild/ProjOut',true);
  FCleanBuildProjSrc:=FXMLCfg.GetValue(Path+'CleanBuild/ProjSrc',true);
  FCleanBuildPkgOut:=FXMLCfg.GetValue(Path+'CleanBuild/PkgOut',true);
  FCleanBuildPkgSrc:=FXMLCfg.GetValue(Path+'CleanBuild/PkgSrc',true);

  // backup
  LoadBackupInfo(FBackupInfoProjectFiles,Path+'BackupProjectFiles/',DefaultBackupTypeProject);
  LoadBackupInfo(FBackupInfoOtherFiles,Path+'BackupOtherFiles/',DefaultBackupTypeOther);

  DebuggerSearchPath:=FXMLCfg.GetValue(Path+'DebuggerSearchPath/Value','');
end;

procedure TEnvironmentOptions.Load(OnlyDesktop: boolean);

  procedure LoadPascalFileExt(const Path: string);
  begin
    fPascalFileExtension:=PascalExtToType(FXMLCfg.GetValue(
      Path+'Naming/PascalFileExtension',PascalExtension[petPAS]));
    if fPascalFileExtension=petNone then
      fPascalFileExtension:=petPAS;
  end;

  procedure LoadCCResult(var CCResult: TCodeCreationDlgResult; const Path: string;
    const DefaultClassSection: TInsertClassSection);
  begin
    CCResult.ClassSection:=InsertClassSectionNameToSection(FXMLCfg.GetValue(
      Path+'/ClassSection',InsertClassSectionNames[DefaultClassSection]));
    CCResult.Location:=CreateCodeLocationNameToLocation(FXMLCfg.GetValue(
      Path+'/Location',CreateCodeLocationNames[cclLocal]));
  end;

var
  Path, CurPath: String;
  i: Integer;
  xFileName: String;
  SubCfg: TIDESubOptions;
begin
  try
    InitXMLCfg(false);
    // ToDo: Get rid of EnvironmentOptions/ path. The whole file is about
    //  environment options. Many section are not under it any more.
    Path:='EnvironmentOptions/';
    FFileVersion:=FXMLCfg.GetValue(Path+'Version/Value',EnvOptsVersion);
    FOldLazarusVersion:=FXMLCfg.GetValue(Path+'Version/Lazarus','');
    //if FOldLazarusVersion='' then begin
      // 108 added LastCalledByLazarusFullPath
      // 107 added Lazarus version
      // 1.1     r36507  106
      // 0.9.31  r28811  106
      // 0.9.29  r21344  106
      // 0.9.27  r16725  106
      // 0.9.25  r12751  106
      // 0.9.23  r10809  106
    //end;

    // language
    fLanguageID:=FXMLCfg.GetValue(Path+'Language/ID','');

    FXMLCfg.ReadObject(Path, Self);

    // auto save
    FAskSaveSessionOnly:=FXMLCfg.GetValue(Path+'AutoSave/AskSaveSessionOnly',false);
    FAutoSaveEditorFiles:=FXMLCfg.GetValue(Path+'AutoSave/EditorFiles',true);
    FAutoSaveProject:=FXMLCfg.GetValue(Path+'AutoSave/Project',true);
    FAutoSaveIntervalInSecs:=FXMLCfg.GetValue(Path+'AutoSave/IntervalInSecs',DefaultAutoSaveIntervalInSecs);
    FLastSavedProjectFile:=FXMLCfg.GetValue(Path+'AutoSave/LastSavedProjectFile','');
    FOpenLastProjectAtStart:=FXMLCfg.GetValue(Path+'AutoSave/OpenLastProjectAtStart',true);
    FLastOpenPackages.Clear;
    if FOpenLastProjectAtStart then
    begin
      i := 1;
      repeat
        xFileName := FXMLCfg.GetValue(Path+'AutoSave/LastOpenPackages/Package'+IntToStr(i), '');
        if FileExistsCached(xFileName) then
          FLastOpenPackages.Add(xFileName);
        Inc(i);
      until xFileName='';
    end;

    // project inspector
    FProjInspSortAlphabetically:=FXMLCfg.GetValue(Path+'ProjInspSortAlphabetically/Value',false);
    FProjInspShowDirHierarchy:=FXMLCfg.GetValue(Path+'ProjInspShowDirHierarchy/Value',false);
    // package editor
    FPackageEditorSortAlphabetically:=FXMLCfg.GetValue(Path+'PackageEditorSortAlphabetically/Value',false);
    FPackageEditorShowDirHierarchy:=FXMLCfg.GetValue(Path+'PackageEditorShowDirHierarchy/Value',false);
    FPackageEditorShowProps:=FXMLCfg.GetValue(Path+'PackageEditorShowPropsPanel/Value',true);
    // procedure list
    FProcedureListFilterStart:=FXMLCfg.GetValue(Path+'ProcedureListFilterStart/Value',false);
    FCheckDiskChangesWithLoading:=FXMLCfg.GetValue(Path+'CheckDiskChangesWithLoading/Value',false);
    // comboboxes
    FDropDownCount:=FXMLCfg.GetValue(Path+'ComboBoxes/DropDownCount',DefaultDropDownCount);
    // messages view
    FMsgViewShowFPCMsgLinesCompiled:=XMLCfg.GetValue(Path+'MsgView/FPCMsg/ShowLinesCompiled',false);

    // recent files and directories
    FMaxRecentOpenFiles:=FXMLCfg.GetValue(Path+'Recent/OpenFiles/Max',DefaultMaxRecentOpenFiles);
    LoadRecentList(FXMLCfg,FRecentOpenFiles,Path+'Recent/OpenFiles/',rltFile);
    FMaxRecentProjectFiles:=FXMLCfg.GetValue(Path+'Recent/ProjectFiles/Max',DefaultMaxRecentProjectFiles);
    LoadRecentList(FXMLCfg,FRecentProjectFiles,Path+'Recent/ProjectFiles/',rltFile);
    FMaxRecentPackageFiles:=FXMLCfg.GetValue(Path+'Recent/PackageFiles/Max',DefaultMaxRecentPackageFiles);
    LoadRecentList(FXMLCfg,FRecentPackageFiles,Path+'Recent/PackageFiles/',rltFile);
    FNewProjectTemplateAtStart:=FXMLCfg.GetValue(Path+'NewProjectTemplateAtStart/Value','Application');
    FMultipleInstances:=StrToIDEMultipleInstancesOption(FXMLCfg.GetValue(Path+'MultipleInstances/Value',''));
    FAlreadyPopulatedRecentFiles := FXMLCfg.GetValue(Path+'Recent/AlreadyPopulated', false);

    // other recent settings
    LoadCCResult(FLastEventMethodCCResult, Path+'Recent/EventMethodCCResult', icsPublic);
    LoadCCResult(FLastVariableCCResult, Path+'Recent/VariableCCResult', icsPrivate);

    FUseUnitDlgOptions.AllUnits:=FXMLCfg.GetValue(Path+'Recent/UseUnitDlg/AllUnits',False);
    FUseUnitDlgOptions.AddToImplementation:=FXMLCfg.GetValue(Path+'Recent/UseUnitDlg/AddToImplementation',False);

    // external tools
    if Assigned(fExternalUserTools) then
      fExternalUserTools.Load(FConfigStore,Path+'ExternalTools/');
    FMaxExtToolsInParallel:=FXMLCfg.GetValue(Path+'ExternalTools/MaxInParallel',0);

    // naming
    LoadPascalFileExt(Path+'');
    if FFileVersion>=103 then begin
      fCharcaseFileAction:=CharCaseFileActionNameToType(FXMLCfg.GetValue(
        Path+'CharcaseFileAction/Value',''));
    end else begin
      if FXMLCfg.GetValue(Path+'PascalFileAskLowerCase/Value',true) then
        fCharcaseFileAction:=ccfaAsk
      else if FXMLCfg.GetValue(Path+'PascalFileAutoLowerCase/Value',false)
      then
        fCharcaseFileAction:=ccfaAutoRename
      else
        fCharcaseFileAction:=ccfaIgnore;
    end;
    if FFileVersion>=104 then
      CurPath:=Path+'AmbiguousFileAction/Value'
    else
      CurPath:=Path+'AmbigiousFileAction/Value';
    fAmbiguousFileAction:=AmbiguousFileActionNameToType(FXMLCfg.GetValue(
      CurPath,AmbiguousFileActionNames[fAmbiguousFileAction]));
    FUnitRenameReferencesAction:=UnitRenameReferencesActionNameToType(FXMLCfg.GetValue(
      Path+'UnitRenameReferencesAction/Value',UnitRenameReferencesActionNames[urraAsk]));
    FAskForFilenameOnNewFile:=FXMLCfg.GetValue(Path+'AskForFilenameOnNewFile/Value',false);
    FLowercaseDefaultFilename:=FXMLCfg.GetValue(Path+'LowercaseDefaultFilename/Value',true);

    // fpdoc
    FPDocPaths := FXMLCfg.GetValue(Path+'LazDoc/Paths','');
    if FFileVersion<=105 then
      FPDocPaths:=LineBreaksToDelimiter(FPDocPaths,';');

    // 'new items'
    FNewUnitTemplate:=FXMLCfg.GetValue(Path+'New/UnitTemplate/Value',FileDescNamePascalUnit);
    FNewFormTemplate:=FXMLCfg.GetValue(Path+'New/FormTemplate/Value',FileDescNameLCLForm);

    if not OnlyDesktop then
      LoadNonDesktop(Path);

    for i := 0 to SubConfigCount - 1 do begin
      SubCfg:=SubConfig[i];
      SubCfg.FVersion:=FFileVersion;
      SubCfg.ReadFromXml(OnlyDesktop);
    end;

    FileUpdated;
  except
    on E: Exception do
      DebugLn('[TEnvironmentOptions.Load]  error reading "',FFilename,'": '+E.Message);
  end;
end;

procedure TEnvironmentOptions.SaveNonDesktop(Path: String);

  procedure SaveBackupInfo(var BackupInfo: TBackupInfo; Path:string;
    DefaultBackupType: TBackupType);
  begin
    with BackupInfo do begin
      FXMLCfg.SetDeleteValue(Path+'Type',BackupTypeToName(BackupType),BackupTypeToName(DefaultBackupType));
      FXMLCfg.SetDeleteValue(Path+'AdditionalExtension',AdditionalExtension,DefaultBackupAddExt);
      FXMLCfg.SetDeleteValue(Path+'MaxCounter',MaxCounter,DefaultBackupMaxCounter);
      FXMLCfg.SetDeleteValue(Path+'SubDirectory',SubDirectory,DefaultBackupSubDirectory);
    end;
  end;

var
  BaseDir, CurLazDir: String;
begin
  // files
  CurLazDir:=ChompPathDelim(LazarusDirectory);
  if not GlobalMacroListClass.StrHasMacros(CurLazDir) then begin
    BaseDir:=ExtractFilePath(ChompPathDelim(GetPrimaryConfigPath));
    if PathIsInPath(CurLazDir,BaseDir) then begin
      // the pcp directory is in the lazarus directory
      // or the lazarus directory is a sibling or a sub dir of a sibling of the pcp
      // examples:
      //   pcp=C:\Lazarus\config, lazdir=C:\Lazarus => store '..'
      //   pcp=/home/user/.lazarus, lazdir=/home/user/freepascal/lazarus => store ../freepascal/lazarus
      CurLazDir:=CreateRelativePath(CurLazDir,GetPrimaryConfigPath);
    end;
    FXMLCfg.SetValue(Path+'LazarusDirectory/Value',CurLazDir); // always store, no SetDeleteValue
  end;
  SaveRecentList(FXMLCfg,FLazarusDirHistory,Path+'LazarusDirectory/History/');
  FXMLCfg.SetDeleteValue(Path+'CompilerFilename/Value',CompilerFilename,'');
  SaveRecentList(FXMLCfg,FCompilerFileHistory,Path+'CompilerFilename/History/');
  FXMLCfg.SetDeleteValue(Path+'FPCSourceDirectory/Value',FPCSourceDirectory,'');
  SaveRecentList(FXMLCfg,FFPCSourceDirHistory,Path+'FPCSourceDirectory/History/');
  FXMLCfg.SetDeleteValue(Path+'MakeFilename/Value',MakeFilename,DefaultMakefilename);
  SaveRecentList(FXMLCfg,FMakeFileHistory,Path+'MakeFilename/History/');
  FXMLCfg.SetDeleteValue(Path+'TestBuildDirectory/Value',TestBuildDirectory,'');
  SaveRecentList(FXMLCfg,FTestBuildDirHistory,Path+'TestBuildDirectory/History/');
  FXMLCfg.SetDeleteValue(Path+'CompilerMessagesFilename/Value',CompilerMessagesFilename,'');
  SaveRecentList(FXMLCfg,FCompilerMessagesFileHistory,Path+'CompilerMessagesFilename/History/');
  FXMLCfg.SetDeleteValue(Path+'FppkgCheck/Value',FppkgCheck,false);
  FXMLCfg.SetDeleteValue(Path+'FppkgConfigFile/Value',FppkgConfigFile,'');
  SaveRecentList(FXMLCfg,FFppkgConfigFileHistory,Path+'FppkgConfigFile/History/');
  // Note: ManyBuildModesSelection is not stored here any more. Moved to project settings.
  FXMLCfg.SetDeleteValue(Path+'StarDirExcludes/Value',StarDirectoryExcludes,DefaultStarDirectoryExcludes);

  // Primary-config verification
  FXMLCfg.SetDeleteValue(Path+'LastCalledByLazarusFullPath/Value',FLastCalledByLazarusFullPath,'');

  // global build options
  FConfigStore.AppendBasePath('BuildMatrix');
  FBuildMatrixOptions.SaveToConfig(FConfigStore,IsGlobalMode);
  FConfigStore.UndoAppendBasePath;

  // Clean build project dialog
  FXMLCfg.SetDeleteValue(Path+'CleanBuild/ProjOut',FCleanBuildProjOut,true);
  FXMLCfg.SetDeleteValue(Path+'CleanBuild/ProjSrc',FCleanBuildProjSrc,true);
  FXMLCfg.SetDeleteValue(Path+'CleanBuild/PkgOut',FCleanBuildPkgOut,true);
  FXMLCfg.SetDeleteValue(Path+'CleanBuild/PkgSrc',FCleanBuildPkgSrc,true);

  // backup
  SaveBackupInfo(FBackupInfoProjectFiles,Path+'BackupProjectFiles/',DefaultBackupTypeProject);
  SaveBackupInfo(FBackupInfoOtherFiles,Path+'BackupOtherFiles/',DefaultBackupTypeOther);

  FXMLCfg.SetDeleteValue(Path+'DebuggerSearchPath/Value',DebuggerSearchPath,'');
end;

procedure TEnvironmentOptions.Save(OnlyDesktop: boolean);
  procedure SaveCCResult(const CCResult: TCodeCreationDlgResult; const Path: string;
    const DefaultClassSection: TInsertClassSection);
  begin
    FXMLCfg.SetDeleteValue(Path+'/ClassSection',
      InsertClassSectionNames[CCResult.ClassSection],
      InsertClassSectionNames[DefaultClassSection]);
    FXMLCfg.SetDeleteValue(Path+'/Location',
      CreateCodeLocationNames[CCResult.Location],
      CreateCodeLocationNames[cclLocal]);
  end;

var
  Path: String;
  i: Integer;
  SubCfg: TIDESubOptions;
begin
  try
    // Don't discard any loaded XML
    // It is not consistent to normally keep it,
    // yet discard it if the file changed on disk (especially since the changed file will be overwritten anyway)
    if (FXMLCfg=nil) then
      InitXMLCfg(true)
    else
      FXMLCfg.Filename := FFilename;
    // ToDo: Get rid of EnvironmentOptions/ path. The whole file is about
    //  environment options. Many section are not under it any more.
    Path:='EnvironmentOptions/';

    FXMLCfg.SetValue(Path+'Version/Value',EnvOptsVersion);
    FXMLCfg.SetValue(Path+'Version/Lazarus',LazarusVersionStr);

    // language
    FXMLCfg.SetDeleteValue(Path+'Language/ID',LanguageID,'');

    // auto save
    FXMLCfg.SetDeleteValue(Path+'AutoSave/AskSaveSessionOnly',FAskSaveSessionOnly,false);
    FXMLCfg.SetDeleteValue(Path+'AutoSave/EditorFiles',FAutoSaveEditorFiles,true);
    FXMLCfg.SetDeleteValue(Path+'AutoSave/Project',FAutoSaveProject,true);
    FXMLCfg.SetDeleteValue(Path+'AutoSave/IntervalInSecs',FAutoSaveIntervalInSecs,DefaultAutoSaveIntervalInSecs);
    FXMLCfg.SetDeleteValue(Path+'AutoSave/LastSavedProjectFile',FLastSavedProjectFile,'');
    FXMLCfg.SetDeleteValue(Path+'AutoSave/OpenLastProjectAtStart',FOpenLastProjectAtStart,true);
    FXMLCfg.DeletePath(Path+'AutoSave/LastOpenPackages/');
    if FOpenLastProjectAtStart then
      for i := 0 to FLastOpenPackages.Count-1 do
        FXMLCfg.SetValue(Path+'AutoSave/LastOpenPackages/Package'+IntToStr(i+1), FLastOpenPackages[i]);

    // project inspector
    FXMLCfg.SetDeleteValue(Path+'ProjInspSortAlphabetically/Value',FProjInspSortAlphabetically,false);
    FXMLCfg.SetDeleteValue(Path+'ProjInspShowDirHierarchy/Value',FProjInspShowDirHierarchy,false);
    // package editor
    FXMLCfg.SetDeleteValue(Path+'PackageEditorSortAlphabetically/Value',FPackageEditorSortAlphabetically,false);
    FXMLCfg.SetDeleteValue(Path+'PackageEditorShowDirHierarchy/Value',FPackageEditorShowDirHierarchy,false);
    FXMLCfg.SetDeleteValue(Path+'PackageEditorShowPropsPanel/Value',FPackageEditorShowProps,true);
    // procedure list
    FXMLCfg.SetDeleteValue(Path+'ProcedureListFilterStart/Value',FProcedureListFilterStart,false);
    FXMLCfg.SetDeleteValue(Path+'CheckDiskChangesWithLoading/Value',FCheckDiskChangesWithLoading,false);
    // comboboxes
    FXMLCfg.SetDeleteValue(Path+'ComboBoxes/DropDownCount',FDropDownCount,DefaultDropDownCount);
    // messages view
    XMLCfg.SetDeleteValue(Path+'MsgView/FPCMsg/ShowLinesCompiled',FMsgViewShowFPCMsgLinesCompiled,false);

    // recent files and directories
    FXMLCfg.SetDeleteValue(Path+'Recent/OpenFiles/Max',FMaxRecentOpenFiles,DefaultMaxRecentOpenFiles);
    SaveRecentList(FXMLCfg,FRecentOpenFiles,Path+'Recent/OpenFiles/',FMaxRecentOpenFiles);
    FXMLCfg.SetDeleteValue(Path+'Recent/ProjectFiles/Max',FMaxRecentProjectFiles,DefaultMaxRecentProjectFiles);
    SaveRecentList(FXMLCfg,FRecentProjectFiles,Path+'Recent/ProjectFiles/',FMaxRecentProjectFiles);
    FXMLCfg.SetDeleteValue(Path+'Recent/PackageFiles/Max',FMaxRecentPackageFiles,DefaultMaxRecentPackageFiles);
    SaveRecentList(FXMLCfg,FRecentPackageFiles,Path+'Recent/PackageFiles/',FMaxRecentPackageFiles);
    FXMLCfg.SetDeleteValue(Path+'NewProjectTemplateAtStart/Value',FNewProjectTemplateAtStart,'Application');
    FXMLCfg.SetDeleteValue(Path+'MultipleInstances/Value',
        IDEMultipleInstancesOptionNames[FMultipleInstances],
        IDEMultipleInstancesOptionNames[DefaultIDEMultipleInstancesOption]);
    FXMLCfg.SetDeleteValue(Path+'Recent/AlreadyPopulated', FAlreadyPopulatedRecentFiles, false);

    // other recent settings
    SaveCCResult(FLastEventMethodCCResult, Path+'Recent/EventMethodCCResult', icsPublic);
    SaveCCResult(FLastVariableCCResult, Path+'Recent/VariableCCResult', icsPrivate);

    FXMLCfg.SetDeleteValue(Path+'Recent/UseUnitDlg/AllUnits',FUseUnitDlgOptions.AllUnits,False);
    FXMLCfg.SetDeleteValue(Path+'Recent/UseUnitDlg/AddToImplementation',FUseUnitDlgOptions.AddToImplementation,False);

    // external tools
    if Assigned(fExternalUserTools) then
      fExternalUserTools.Save(FConfigStore,Path+'ExternalTools/');
    FXMLCfg.SetDeleteValue(Path+'ExternalTools/MaxInParallel',FMaxExtToolsInParallel,0);

    // naming
    FXMLCfg.SetDeleteValue(Path+'Naming/PascalFileExtension',
                             PascalExtension[fPascalFileExtension],'.pas');
    FXMLCfg.SetDeleteValue(Path+'CharcaseFileAction/Value',
                             CharCaseFileActionNames[fCharcaseFileAction],
                             CharCaseFileActionNames[ccfaAutoRename]);
    FXMLCfg.SetDeleteValue(Path+'AmbiguousFileAction/Value',
      AmbiguousFileActionNames[fAmbiguousFileAction],
      AmbiguousFileActionNames[afaAsk]);
    FXMLCfg.SetDeleteValue(Path+'AskForFilenameOnNewFile/Value',FAskForFilenameOnNewFile,false);
    FXMLCfg.SetDeleteValue(Path+'LowercaseDefaultFilename/Value',FLowercaseDefaultFilename,true);

    // fpdoc
    FXMLCfg.SetDeleteValue(Path+'LazDoc/Paths',FPDocPaths,'');

    // 'new items'
    FXMLCfg.SetDeleteValue(Path+'New/UnitTemplate/Value',FNewUnitTemplate,FileDescNamePascalUnit);
    FXMLCfg.SetDeleteValue(Path+'New/FormTemplate/Value',FNewFormTemplate,FileDescNameLCLForm);

    if not OnlyDesktop then
      SaveNonDesktop(Path);

    for i := 0 to SubConfigCount - 1 do begin
      SubCfg:=SubConfig[i];
      SubCfg.WriteToXml(OnlyDesktop);
    end;

    FXMLCfg.WriteObject(Path, Self);
    FXMLCfg.Flush;
    FileUpdated;
  except
    on E: Exception do begin
      DebugLn('[TEnvironmentOptions.Save]  error writing "',Filename,'": ',E.Message);
    end;
  end;
end;

function TEnvironmentOptions.GetDefaultConfigFilename: string;
begin
  Result:=TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+EnvOptsConfFileName);
end;

procedure TEnvironmentOptions.AddToRecentOpenFiles(const AFilename: string);
var
  Allow: Boolean;
begin
  Allow := True;
  DoAddToRecentOpenFiles(AFilename, Allow);
  if Allow then
    AddToRecentList(AFilename,FRecentOpenFiles,FMaxRecentOpenFiles,rltFile);
end;

procedure TEnvironmentOptions.AddToRecentPackageFiles(const AFilename: string);
var
  Allow: Boolean;
begin
  Allow := True;
  DoAddToRecentPackageFiles(AFilename, Allow);
  if Allow then
    AddToRecentList(AFilename,FRecentPackageFiles,FMaxRecentPackageFiles,rltFile);
end;

procedure TEnvironmentOptions.RemoveFromRecentOpenFiles(const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentOpenFiles,rltFile);
end;

procedure TEnvironmentOptions.GetRecentFiles(aType: TIDERecentHandler;
  aList: TStrings);
begin
  case aType of
    irhProjectFiles : aList.Assign(FRecentProjectFiles);
    irhPackageFiles : aList.Assign(FRecentPackageFiles);
    irhOpenFiles : aList.Assign(FRecentOpenFiles);
  end;
end;

procedure TEnvironmentOptions.RemoveFromRecentPackageFiles(const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentPackageFiles,rltFile);
end;

procedure TEnvironmentOptions.AddToRecentProjectFiles(const AFilename: string);
var
  Allow: Boolean;
begin
  Allow := True;
  DoAddToRecentProjectFiles(AFilename, Allow);
  if Allow then
    AddToRecentList(AFilename,FRecentProjectFiles,FMaxRecentProjectFiles,rltFile);
  {$ifdef Windows}
  SHAddToRecentDocs(SHARD_PATHW, PWideChar(UTF8ToUTF16(AFileName)));
  {$endif}
end;

procedure TEnvironmentOptions.RemoveFromRecentProjectFiles(const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentProjectFiles,rltFile);
end;

function TEnvironmentOptions.GetParsedTestBuildDirectory: string;
begin
  Result:=GetParsedValue(eopTestBuildDirectory);
end;

function TEnvironmentOptions.GetParsedFPCSourceDirectory(FPCVer: string): string;
var
  s: String;
begin
  if (FPCVer<>'') and (Pos('$(',FPCSourceDirectory)>0) then begin
    s:='$(FPCVer)';
    GlobalMacroList.SubstituteStr(s);
    if s<>FPCVer then begin
      // override macro FPCVer
      OverrideFPCVer:=FPCVer;
      IncreaseCompilerParseStamp;
      try
        Result:=GetParsedValue(eopFPCSourceDirectory);
        //debugln(['TEnvironmentOptions.GetParsedFPCSourceDirectory FPCVer=',FPCVer,' FPCSrcDir=',Result]);
      finally
        OverrideFPCVer:='';
        IncreaseCompilerParseStamp;
      end;
      exit;
    end;
  end;
  Result:=GetParsedValue(eopFPCSourceDirectory);
end;

function TEnvironmentOptions.GetParsedMakeFilename: string;
begin
  Result:=GetParsedValue(eopMakeFilename);
end;

function TEnvironmentOptions.GetParsedCompilerMessagesFilename: string;
begin
  Result:=GetParsedValue(eopCompilerMessagesFilename);
end;

function TEnvironmentOptions.GetParsedFPDocPaths: string;
begin
  Result:=GetParsedValue(eopFPDocPaths);
end;

function TEnvironmentOptions.GetParsedDebuggerFilename(
  AProjectDbgFileName: String): string;
begin
  if FParseValues[eopDebuggerFilename].UnparsedValue <> AProjectDbgFileName then
    SetParseValue(eopDebuggerFilename,UTF8Trim(AProjectDbgFileName));

  Result:=GetParsedValue(eopDebuggerFilename);
end;

function TEnvironmentOptions.GetParsedDebuggerSearchPath: string;
begin
  Result:=GetParsedValue(eopDebuggerSearchPath);
end;

function TEnvironmentOptions.GetParsedFppkgConfig: string;
begin
  Result:=GetParsedValue(eopFppkgConfigFile);
end;

function TEnvironmentOptions.GetParsedValue(o: TEnvOptParseType;
  AUnparsedValue: String): string;
type
  PParseString = ^TParseString;
var
  SpacePos: SizeInt;
  CurParams: String;
  TempValue: TParseString;
  VP: PParseString;
begin
  if AUnparsedValue <> '' then begin
    TempValue.UnparsedValue := AUnparsedValue;
    TempValue.ParseStamp := CTInvalidChangeStamp;
    TempValue.Parsing := False;
    VP := @TempValue;
  end
  else
    VP := @FParseValues[o];

  with VP^ do begin
    if (ParseStamp<>CompilerParseStamp)
    or (CompilerParseStamp=CTInvalidChangeStamp) then begin
      if Parsing then begin
        debugln(['TEnvironmentOptions.GetParsedValue circular macro dependency: ',dbgs(o)]);
        exit('circularmacroerror');
      end;
      Parsing:=true;
      try
        ParsedValue:=UnparsedValue;
        if (ParsedValue='') and (o=eopCompilerMessagesFilename) then
          ParsedValue:=GetForcedPathDelims('$(FPCSrcDir)/compiler/msg/errore.msg');

        if not GlobalMacroList.SubstituteStr(ParsedValue) then begin
          debugln(['TEnvironmentOptions.GetParsedValue failed for ',dbgs(o),' Value="',UnparsedValue,'"']);
        end;
        ParseStamp:=CompilerParseStamp;

        case o of
        eopLazarusDirectory:
          // lazarus directory
          begin
            ParsedValue:=TrimAndExpandDirectory(ParsedValue,GetPrimaryConfigPath);
            if ParsedValue='' then
              ParsedValue:=TrimFilename(AppendPathDelim(GetCurrentDirUTF8));
          end;
        eopFPCSourceDirectory,eopTestBuildDirectory:
          // directory
          begin
            ParsedValue:=TrimAndExpandDirectory(ParsedValue,GetParsedLazarusDirectory);
            if ParsedValue='' then
              ParsedValue:=GetParsedLazarusDirectory;
          end;
        eopCompilerMessagesFilename:
          // data file
          begin
            ParsedValue:=TrimAndExpandFilename(ParsedValue,GetParsedLazarusDirectory);
            if (UnparsedValue='') and (not FileExistsCached(ParsedValue)) then
            begin
              // the default errore.msg file does not exist in the fpc sources
              // => use the fallback of the codetools
              ParsedValue:=AppendPathDelim(GetParsedLazarusDirectory)
                +GetForcedPathDelims('components/codetools/fpc.errore.msg');
            end;
          end;
        eopFPDocPaths,eopDebuggerSearchPath:
          // search path
          ParsedValue:=TrimSearchPath(ParsedValue,GetParsedLazarusDirectory,true);
        eopCompilerFilename,eopMakeFilename,eopDebuggerFilename,eopFppkgConfigFile:
          // program
          begin
            ParsedValue:=Trim(ParsedValue);
            CurParams:='';
            if (o in [eopDebuggerFilename]) then begin
              // program + params
              // examples:
              //   gdb -v
              //   "C:\public folder\gdb"
              SpacePos:=1;
              while (SpacePos<=length(ParsedValue)) do begin
                if ParsedValue[SpacePos]='"' then begin
                  System.Delete(ParsedValue,1,1); // delete startng "
                  while (SpacePos<=length(ParsedValue))
                  and (ParsedValue[SpacePos]<>'"') do
                    inc(SpacePos);
                  if SpacePos<=length(ParsedValue) then
                    System.Delete(ParsedValue,1,1); // delete ending "
                end else if ParsedValue[SpacePos]=' ' then
                  break
                else
                  inc(SpacePos);
              end;
              CurParams:=copy(ParsedValue,SpacePos,length(ParsedValue));
              system.Delete(ParsedValue,SpacePos,length(ParsedValue));
            end;
            // program
            ParsedValue:=TrimFilename(ParsedValue);
            if (ParsedValue<>'') and (not FilenameIsAbsolute(ParsedValue)) then
            begin
              if (ExtractFilePath(ParsedValue)='')
              and (not FileExistsCached(GetParsedLazarusDirectory+ParsedValue)) then
                ParsedValue:=FindDefaultExecutablePath(ParsedValue)
              else
                ParsedValue:=TrimFilename(GetParsedLazarusDirectory+ParsedValue);
            end;
            // append parameters
            if CurParams<>'' then begin
              if System.Pos(' ',ParsedValue)>0 then
                ParsedValue:='"'+ParsedValue+'"';
              ParsedValue+=CurParams;
            end;
          end;
        end;
      finally
        Parsing:=false;
      end;
    end;
    Result:=ParsedValue;
  end;
end;

function TEnvironmentOptions.GetParsedCompilerFilename: string;
begin
  Result:=GetParsedValue(eopCompilerFilename);
end;

function TEnvironmentOptions.FileHasChangedOnDisk: boolean;
begin
  Result:=FFileHasChangedOnDisk
      or ((FFilename<>'') and (FFileAge<>0) and (FileAgeCached(FFilename)<>FFileAge));
  FFileHasChangedOnDisk:=Result;
end;

procedure TEnvironmentOptions.InitXMLCfg(CleanConfig: boolean);
var
  i: Integer;
  SubCfg: TIDESubOptions;
begin
  if FileHasChangedOnDisk or (FXMLCfg=nil) then begin
    FreeAndNil(FConfigStore);
    FreeAndNil(FXMLCfg);
    if CleanConfig then
      FXMLCfg:=TRttiXMLConfig.CreateClean(Filename)
    else
      FXMLCfg:=TRttiXMLConfig.Create(Filename);
    //SubConfig
    for i := 0 to SubConfigCount - 1 do begin
      SubCfg:=SubConfig[i];
      SubCfg.FXMLCfg:=FXMLCfg;
      SubCfg.InitConfig;
    end;
    FConfigStore:=TXMLOptionsStorage.Create(FXMLCfg);
  end;
end;

procedure TEnvironmentOptions.FileUpdated;
begin
  FFileHasChangedOnDisk:=false;
  if FFilename<>'' then
    FFileAge:=FileAgeCached(FFilename)
  else
    FFileAge:=0;
end;

procedure TEnvironmentOptions.SetTestBuildDirectory(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=AppendPathDelim(TrimFilename(AValue));
  SetParseValue(eopTestBuildDirectory,NewValue);
end;

procedure TEnvironmentOptions.SetLazarusDirectory(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=AppendPathDelim(TrimFilename(AValue));
  SetParseValue(eopLazarusDirectory,NewValue);
end;

procedure TEnvironmentOptions.SetParseValue(o: TEnvOptParseType;
  const NewValue: string);
begin
  with FParseValues[o] do begin
    if UnparsedValue=NewValue then exit;
    UnparsedValue:=NewValue;
    ParseStamp:=CTInvalidChangeStamp;
    IncreaseCompilerParseStamp;
  end;
end;

procedure TEnvironmentOptions.SetFPCSourceDirectory(const AValue: string);
begin
  SetParseValue(eopFPCSourceDirectory,AValue);
end;

procedure TEnvironmentOptions.SetCompilerFilename(const AValue: string);
begin
  SetParseValue(eopCompilerFilename,TrimFilename(AValue));
end;

procedure TEnvironmentOptions.SetCompilerMessagesFilename(AValue: string);
begin
  SetParseValue(eopCompilerMessagesFilename,TrimFilename(AValue));
end;

function TEnvironmentOptions.GetDebuggerSearchPath: string;
begin
  Result:=FParseValues[eopDebuggerSearchPath].UnparsedValue;
end;

function TEnvironmentOptions.GetCompilerFilename: string;
begin
  Result:=FParseValues[eopCompilerFilename].UnparsedValue;
end;

function TEnvironmentOptions.GetCompilerMessagesFilename: string;
begin
  Result:=FParseValues[eopCompilerMessagesFilename].UnparsedValue;
end;

function TEnvironmentOptions.GetFPCSourceDirectory: string;
begin
  Result:=FParseValues[eopFPCSourceDirectory].UnparsedValue;
end;

function TEnvironmentOptions.GetFPDocPaths: string;
begin
  Result:=FParseValues[eopFPDocPaths].UnparsedValue;
end;

function TEnvironmentOptions.GetLazarusDirectory: string;
begin
  Result:=FParseValues[eopLazarusDirectory].UnparsedValue;
end;

function TEnvironmentOptions.GetFppkgConfigFile: string;
begin
  Result:=FParseValues[eopFppkgConfigFile].UnparsedValue;
end;

function TEnvironmentOptions.GetMakeFilename: string;
begin
  Result:=FParseValues[eopMakeFilename].UnparsedValue;
end;

function TEnvironmentOptions.GetSubConfig(Index: Integer): TIDESubOptions;
begin
  Result := TIDESubOptions(fRegisteredSubConfig[Index]);
end;

function TEnvironmentOptions.GetTestBuildDirectory: string;
begin
  Result:=FParseValues[eopTestBuildDirectory].UnparsedValue;
end;

procedure TEnvironmentOptions.SetDebuggerSearchPath(const AValue: string);
begin
  SetParseValue(eopDebuggerSearchPath,TrimSearchPath(AValue,''));
end;

procedure TEnvironmentOptions.SetFPDocPaths(const AValue: string);
begin
  SetParseValue(eopFPDocPaths,TrimSearchPath(AValue,''));
end;

procedure TEnvironmentOptions.SetMakeFilename(const AValue: string);
begin
  SetParseValue(eopMakeFilename,TrimFilename(AValue));
end;

procedure TEnvironmentOptions.SetFppkgConfigFile(AValue: string);
begin
  SetParseValue(eopFppkgConfigFile,UTF8Trim(AValue));
end;

end.

