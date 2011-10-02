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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, TypInfo, Graphics, Controls, Forms, LCLProc, FileProcs, Dialogs,
  Laz_XMLCfg, LazConfigStorage,
  // IDEIntf
  ProjectIntf, ObjectInspector, IDEWindowIntf, IDEOptionsIntf,
  CompOptsIntf, IDEExternToolIntf,
  // IDE
  IDEProcs, LazarusIDEStrConsts, IDETranslations, LazConf,
  IDEOptionDefs, TransferMacros, Debugger;

const
  EnvOptsVersion: integer = 106;

  //----------------------------------------------------------------------------
  
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
  
  
  { Debugging }

type
  TDebuggerEventLogColor = record
    Foreground: TColor;
    Background: TColor;
  end;

const
  DebuggerDefaultColors: array[TDBGEventType] of TDebuggerEventLogColor = (
{ etDefault              } (Foreground: clWindowText; Background: clWindow),
{ etBreakpointEvaluation } (Foreground: $8080FF;      Background: clWindow),
{ etBreakpointHit        } (Foreground: clRed;        Background: clWindow),
{ etBreakpointMessage    } (Foreground: $0000D9;      Background: clWindow),
{ etBreakpointStackDump  } (Foreground: $2080FF;      Background: clWindow),
{ etExceptionRaised      } (Foreground: clTeal;       Background: clWindow),
{ etModuleLoad           } (Foreground: clBlue;       Background: clWindow),
{ etModuleUnload         } (Foreground: clBlue;       Background: clWindow),
{ etOutputDebugString    } (Foreground: clNavy;       Background: clWindow),
{ etProcessExit          } (Foreground: clGray;       Background: clWindow),
{ etProcessStart         } (Foreground: clGray;       Background: clWindow),
{ etThreadExit           } (Foreground: clMaroon;     Background: clWindow),
{ etThreadStart          } (Foreground: clMaroon;     Background: clWindow),
{ etWindowsMessagePosted } (Foreground: clWhite;      Background: clGray),
{ etWindowsMessageSent   } (Foreground: clSkyBlue;    Background: clWindow)
  );

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
      ccfaAsk,
      ccfaAutoRename,
      ccfaIgnore
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
  TBaseExternalToolList = class(TList)
  public
    function Load(Config: TConfigStorage; const Path: string): TModalResult; virtual; abstract;
    function Save(Config: TConfigStorage; const Path: string): TModalResult; virtual; abstract;
    function Run(ExtTool: TIDEExternalToolOptions;
                 Macros: TTransferMacroList;
                 ShowAbort: boolean;
                 CompilerOptions: TLazCompilerOptions = nil): TModalResult; virtual; abstract;
  end;
  TExternalToolListClass = class of TBaseExternalToolList;
var
  ExternalToolListClass: TExternalToolListClass; // set by ExtToolDialog

type
  { TEnvironmentOptions - class for storing environment options }

  TEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FFilename: string;
    FFileAge: longint;
    FFileHasChangedOnDisk: boolean;
    FIDESpeedButtonsVisible: boolean;
    FIDETitleStartsWithProject: boolean;
    FIDEProjectDirectoryInIdeTitle: boolean;
    FShowButtonGlyphs: TApplicationShowGlyphs;
    FShowMenuGlyphs: TApplicationShowGlyphs;
    FXMLCfg: TRttiXMLConfig;
    FConfigStore: TXMLOptionsStorage;
    FDbgConfigStore: TXMLOptionsStorage; // for debugger

    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    FLastSavedProjectFile: string;
    
    // window layout
    FIDEDialogLayoutList: TIDEDialogLayoutList;
    FSingleTaskBarButton: boolean;
    FHideIDEOnRun: boolean;
    FHideMessagesIcons: boolean;
    FComponentPaletteVisible: boolean;
    // CompletionWindow
    FCompletionWindowWidth: Integer;
    FCompletionWindowHeight: Integer;

    // designer
    FCreateComponentFocusNameProperty: boolean;
    FSwitchToFavoritesOITab: boolean;
    FDesignerPaintLazy: boolean;
    FShowBorderSpacing: boolean;
    FShowGrid: boolean;
    FSnapToGrid: boolean;
    FGridSizeX: integer;
    FGridSizeY: integer;
    FGridColor: TColor;
    FShowGuideLines: boolean;
    FSnapToGuideLines: boolean;
    FGuideLineColorLeftTop: TColor;
    FGuideLineColorRightBottom: TColor;
    FShowComponentCaptions: boolean;
    FShowEditorHints: boolean;
    FAutoCreateFormsOnOpen: boolean;
    FRightClickSelects: boolean;
    FGrabberColor: TColor;
    FMarkerColor: TColor;
    FRubberbandSelectionColor: TColor;
    FRubberbandCreationColor: TColor;
    FRubberbandSelectsGrandChilds: boolean;
    FCheckPackagesOnFormCreate: boolean;

    // object inspector
    FObjectInspectorOptions: TOIOptions;
    
    // hints
    FCheckDiskChangesWithLoading: boolean;
    FShowHintsForComponentPalette: boolean;
    FShowHintsForMainSpeedButtons: boolean;
    
    // messages
    fMsgViewDblClickJumps: boolean;
    fMsgViewFocus: boolean;

    // compiler + debugger + lazarus files
    FLazarusDirectory: string;
    FLazarusDirsHistory: TStringList;
    FCompilerFilename: string;
    FCompilerFilenameParsed: string;
    FCompilerFilenameParsedStamp: integer;
    FCompilerFileHistory: TStringList;
    FFPCSourceDirectory: string;
    FFPCSrcDirParsed: string;
    FFPCSrcDirParsedStamp: integer;
    FFPCSourceDirHistory: TStringList;
    FMakeFileName: string;
    FMakeFileHistory: TStringList;
    FCompilerMessagesFilename: string;
    FCompilerMessagesFileHistory: TStringList;

   // TODO: store per debuggerclass options
    // Maybe these should go to a new TDebuggerOptions class
    FDebuggerConfig: TDebuggerConfigStore;
    FDebuggerSearchPath: string;
    FDebuggerFilename: string;         // per debugger class
    FDebuggerFileHistory: TStringList; // per debugger class
    FDebuggerProperties: TStringList; // per debugger class
    FDebuggerShowStopMessage: Boolean;
    FShowCompileDialog: Boolean;       // show dialog during compile
    FAutoCloseCompileDialog: Boolean;  // auto close dialog after succesed compile
    FTestBuildDirectory: string;
    FTestBuildDirHistory: TStringList;
    FDebuggerEventLogClearOnRun: Boolean;
    FDebuggerEventLogCheckLineLimit: Boolean;
    FDebuggerEventLogLineLimit: Integer;
    FDebuggerEventLogShowBreakpoint: Boolean;
    FDebuggerEventLogShowDebugger: Boolean;
    FDebuggerEventLogShowModule: Boolean;
    FDebuggerEventLogShowOutput: Boolean;
    FDebuggerEventLogShowProcess: Boolean;
    FDebuggerEventLogShowThread: Boolean;
    FDebuggerEventLogShowWindows: Boolean;
    FDebuggerEventLogUseColors: Boolean;
    FDebuggerEventLogColors: array[TDBGEventType] of TDebuggerEventLogColor;

    // recent files and directories
    FRecentOpenFiles: TStringList;
    FMaxRecentOpenFiles: integer;
    FRecentProjectFiles: TStringList;
    FMaxRecentProjectFiles: integer;
    FRecentPackageFiles: TStringList;
    FMaxRecentPackageFiles: integer;
    FOpenLastProjectAtStart: boolean;

    // backup
    FBackupInfoProjectFiles: TBackupInfo;
    FBackupInfoOtherFiles: TBackupInfo;
    
    // external tools
    fExternalTools: TBaseExternalToolList;
    
    // naming conventions
    fPascalFileExtension: TPascalExtType;
    fCharcaseFileAction : TCharCaseFileAction;
    fAmbiguousFileAction: TAmbiguousFileAction;
    FUnitRenameReferencesAction: TUnitRenameReferencesAction;
    FAskForFilenameOnNewFile: boolean;
    FLowercaseDefaultFilename: boolean;

    // lazdoc
    FLazDocPaths: string;

    // language ID (see LazarusTranslations in translations.pas)
    fLanguageID: string;

    // 'new items'
    FNewFormTemplate: string;
    FNewUnitTemplate: string;

    function GetDebuggerEventLogColors(AIndex: TDBGEventType): TDebuggerEventLogColor;
    procedure SetCompilerFilename(const AValue: string);
    procedure SetDebuggerEventLogColors(AIndex: TDBGEventType;
      const AValue: TDebuggerEventLogColor);
    procedure SetDebuggerSearchPath(const AValue: string);
    procedure SetMakeFilename(const AValue: string);
    procedure SetDebuggerFilename(const AValue: string);
    procedure SetFPCSourceDirectory(const AValue: string);
    procedure SetLazarusDirectory(const AValue: string);

    procedure InitLayoutList;
    procedure SetFileName(const NewFilename: string);
    function FileHasChangedOnDisk: boolean;
    function GetXMLCfg(CleanConfig: boolean): TXMLConfig;
    procedure FileUpdated;
    procedure SetTestBuildDirectory(const AValue: string);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(OnlyDesktop:boolean);
    procedure Save(OnlyDesktop:boolean);
    property Filename: string read FFilename write SetFilename;
    procedure CreateConfig;
    procedure GetDefaultFPCSourceDirectory;
    function GetTestBuildDirectory: string;
    function GetFPCSourceDirectory: string;
    function GetCompilerFilename: string;

    // macro functions
    procedure InitMacros(AMacroList: TTransferMacroList);
    function MacroFuncCompPath(const s:string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncFPCSrcDir(const s:string; const Data: PtrInt;
                                var Abort: boolean): string;
    function MacroFuncLazarusDir(const s:string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncExeExt(const s:string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncLanguageID(const s:string; const Data: PtrInt;
                                 var Abort: boolean): string;
    function MacroFuncLanguageName(const s:string; const Data: PtrInt;
                                   var Abort: boolean): string;
    function MacroFuncTestDir(const s:string; const Data: PtrInt;
                              var Abort: boolean): string;
    function MacroFuncConfDir(const s:string; const Data: PtrInt;
                              var Abort: boolean): string;

    // debugger
    procedure SaveDebuggerPropertiesList;
    procedure SaveDebuggerProperties(DebuggerClass: String; Properties: TDebuggerProperties);
    procedure LoadDebuggerProperties(DebuggerClass: String; Properties: TDebuggerProperties);
    property  DebuggerConfig: TDebuggerConfigStore read FDebuggerConfig;

    // auto save
    property AutoSaveEditorFiles: boolean read FAutoSaveEditorFiles
                                          write FAutoSaveEditorFiles;
    property AutoSaveProject: boolean read FAutoSaveProject
                                      write FAutoSaveProject;
    property AutoSaveIntervalInSecs: integer read FAutoSaveIntervalInSecs
                                             write FAutoSaveIntervalInSecs;
       
    // window layouts
    property IDEDialogLayoutList: TIDEDialogLayoutList
                           read FIDEDialogLayoutList write FIDEDialogLayoutList;
    property SingleTaskBarButton: boolean read FSingleTaskBarButton
                                               write FSingleTaskBarButton;
    property HideIDEOnRun: boolean read FHideIDEOnRun write FHideIDEOnRun;
    property HideMessagesIcons: boolean read fHideMessagesIcons write fHideMessagesIcons;
    property IDETitleStartsWithProject: boolean read FIDETitleStartsWithProject
                                               write FIDETitleStartsWithProject;
    property IDEProjectDirectoryInIdeTitle: boolean read FIDEProjectDirectoryInIdeTitle
                                                    write FIDEProjectDirectoryInIdeTitle;
    property ComponentPaletteVisible: boolean read FComponentPaletteVisible
                                              write FComponentPaletteVisible;
    property IDESpeedButtonsVisible: boolean read FIDESpeedButtonsVisible
                                             write FIDESpeedButtonsVisible;

    property CompletionWindowWidth: Integer read FCompletionWindowWidth
                                            write FCompletionWindowWidth;
    property CompletionWindowHeight: Integer read FCompletionWindowHeight
                                             write FCompletionWindowHeight;

    // EnvironmentOptionsDialog editor
    property ShowBorderSpacing: boolean read FShowBorderSpacing write FShowBorderSpacing;
    property ShowGrid: boolean read FShowGrid write FShowGrid;
    property SnapToGrid: boolean read FSnapToGrid write FSnapToGrid;
    property GridColor: TColor read FGridColor write FGridColor;
    property GridSizeX: integer read FGridSizeX write FGridSizeX;
    property GridSizeY: integer read FGridSizeY write FGridSizeY;
    property ShowGuideLines: boolean read FShowGuideLines write FShowGuideLines;
    property SnapToGuideLines: boolean
                                 read FSnapToGuideLines write FSnapToGuideLines;
    property GuideLineColorLeftTop: TColor read FGuideLineColorLeftTop
                                           write FGuideLineColorLeftTop;
    property GuideLineColorRightBottom: TColor read FGuideLineColorRightBottom
                                               write FGuideLineColorRightBottom;
    property ShowComponentCaptions: boolean
       read FShowComponentCaptions write FShowComponentCaptions;
    property ShowEditorHints: boolean read FShowEditorHints
                                      write FShowEditorHints;
    property AutoCreateFormsOnOpen: boolean read FAutoCreateFormsOnOpen
                                            write FAutoCreateFormsOnOpen;
    property CheckPackagesOnFormCreate: boolean read FCheckPackagesOnFormCreate
                                                write FCheckPackagesOnFormCreate;
    property RightClickSelects: boolean read FRightClickSelects
                                        write FRightClickSelects;
    property GrabberColor: TColor read FGrabberColor write FGrabberColor;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property RubberbandSelectionColor: TColor read FRubberbandSelectionColor
                                              write FRubberbandSelectionColor;
    property RubberbandCreationColor: TColor read FRubberbandCreationColor
                                             write FRubberbandCreationColor;
    property RubberbandSelectsGrandChilds: boolean
                                            read FRubberbandSelectsGrandChilds
                                            write FRubberbandSelectsGrandChilds;
    property DesignerPaintLazy: boolean read FDesignerPaintLazy
                                        write FDesignerPaintLazy;
    property CreateComponentFocusNameProperty: boolean read FCreateComponentFocusNameProperty
                                        write FCreateComponentFocusNameProperty;
    property SwitchToFavoritesOITab: boolean read FSwitchToFavoritesOITab
                                             write FSwitchToFavoritesOITab;

    // object inspector
    property ObjectInspectorOptions: TOIOptions read FObjectInspectorOptions
                                                write FObjectInspectorOptions;

    // hints
    property CheckDiskChangesWithLoading: boolean read FCheckDiskChangesWithLoading
                                             write FCheckDiskChangesWithLoading;
    property ShowHintsForComponentPalette: boolean
                                            read FShowHintsForComponentPalette
                                            write FShowHintsForComponentPalette;
    property ShowHintsForMainSpeedButtons: boolean
                                            read FShowHintsForMainSpeedButtons
                                            write FShowHintsForMainSpeedButtons;
    
    // files
    property LazarusDirectory: string read FLazarusDirectory
                                      write SetLazarusDirectory;
    property LazarusDirHistory: TStringList read FLazarusDirsHistory
                                            write FLazarusDirsHistory;
    property CompilerFilename: string read FCompilerFilename
                                      write SetCompilerFilename;
    property CompilerFileHistory: TStringList read FCompilerFileHistory
                                              write FCompilerFileHistory;
    property FPCSourceDirectory: string read FFPCSourceDirectory
                                        write SetFPCSourceDirectory;
    property FPCSourceDirHistory: TStringList read FFPCSourceDirHistory
                                              write FFPCSourceDirHistory;
    property MakeFilename: string read FMakeFilename
                                      write SetMakeFilename;
    property MakeFileHistory: TStringList read FMakeFileHistory
                                              write FMakeFileHistory;
    property DebuggerFilename: string read FDebuggerFilename
                                      write SetDebuggerFilename;
    property DebuggerFileHistory: TStringList read FDebuggerFileHistory
                                              write FDebuggerFileHistory;
    property DebuggerSearchPath: string read FDebuggerSearchPath
                                      write SetDebuggerSearchPath;
    property DebuggerShowStopMessage: boolean read FDebuggerShowStopMessage
                                              write FDebuggerShowStopMessage;
    property ShowCompileDialog: boolean read  FShowCompileDialog
                                        write FShowCompileDialog;
    property AutoCloseCompileDialog: boolean read  FAutoCloseCompileDialog
                                             write FAutoCloseCompileDialog;
    property TestBuildDirectory: string read FTestBuildDirectory
                                        write SetTestBuildDirectory;
    property TestBuildDirHistory: TStringList read FTestBuildDirHistory
                                              write FTestBuildDirHistory;
    property DebuggerEventLogClearOnRun: Boolean read FDebuggerEventLogClearOnRun write FDebuggerEventLogClearOnRun;
    property DebuggerEventLogCheckLineLimit: Boolean read FDebuggerEventLogCheckLineLimit write FDebuggerEventLogCheckLineLimit;
    property DebuggerEventLogLineLimit: Integer read FDebuggerEventLogLineLimit write FDebuggerEventLogLineLimit;
    property DebuggerEventLogShowBreakpoint: Boolean read FDebuggerEventLogShowBreakpoint write FDebuggerEventLogShowBreakpoint;
    property DebuggerEventLogShowProcess: Boolean read FDebuggerEventLogShowProcess write FDebuggerEventLogShowProcess;
    property DebuggerEventLogShowThread: Boolean read FDebuggerEventLogShowThread write FDebuggerEventLogShowThread;
    property DebuggerEventLogShowModule: Boolean read FDebuggerEventLogShowModule write FDebuggerEventLogShowModule;
    property DebuggerEventLogShowOutput: Boolean read FDebuggerEventLogShowOutput write FDebuggerEventLogShowOutput;
    property DebuggerEventLogShowWindows: Boolean read FDebuggerEventLogShowWindows write FDebuggerEventLogShowWindows;
    property DebuggerEventLogShowDebugger: Boolean read FDebuggerEventLogShowDebugger write FDebuggerEventLogShowDebugger;
    property DebuggerEventLogUseColors: Boolean read FDebuggerEventLogUseColors write FDebuggerEventLogUseColors;
    property DebuggerEventLogColors[AIndex: TDBGEventType]: TDebuggerEventLogColor read GetDebuggerEventLogColors write SetDebuggerEventLogColors;

    property CompilerMessagesFilename: string read FCompilerMessagesFilename
                                              write FCompilerMessagesFilename;
    property CompilerMessagesFileHistory: TStringList read FCompilerMessagesFileHistory
                                                     write FCompilerMessagesFileHistory;

    // recent files and directories
    property RecentOpenFiles: TStringList read FRecentOpenFiles
                                          write FRecentOpenFiles;
    property MaxRecentOpenFiles: integer read FMaxRecentOpenFiles
                                         write FMaxRecentOpenFiles;
    procedure AddToRecentOpenFiles(const AFilename: string);
    procedure RemoveFromRecentOpenFiles(const AFilename: string);
    property RecentProjectFiles: TStringList read FRecentProjectFiles
                                             write FRecentProjectFiles;
    property MaxRecentProjectFiles: integer read FMaxRecentProjectFiles
                                            write FMaxRecentProjectFiles;
    procedure AddToRecentProjectFiles(const AFilename: string);
    procedure RemoveFromRecentProjectFiles(const AFilename: string);
    property RecentPackageFiles: TStringList read FRecentPackageFiles
                                          write FRecentPackageFiles;
    property MaxRecentPackageFiles: integer read FMaxRecentPackageFiles
                                         write FMaxRecentPackageFiles;
    property LastSavedProjectFile: string read FLastSavedProjectFile
                                          write FLastSavedProjectFile;
    property OpenLastProjectAtStart: boolean read FOpenLastProjectAtStart
                                             write FOpenLastProjectAtStart;

    // backup
    property BackupInfoProjectFiles: TBackupInfo read FBackupInfoProjectFiles
                                                 write FBackupInfoProjectFiles;
    property BackupInfoOtherFiles: TBackupInfo read FBackupInfoOtherFiles
                                               write FBackupInfoOtherFiles;
       
    // external tools
    property ExternalTools: TBaseExternalToolList read fExternalTools
                                                  write fExternalTools;

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

    // lazdoc
    property LazDocPaths: string read FLazDocPaths write FLazDocPaths;

    // language
    property LanguageID: string read fLanguageID write fLanguageID;
    
    // messages view
    property MsgViewDblClickJumps: boolean read fMsgViewDblClickJumps
                                           write fMsgViewDblClickJumps;
    property MsgViewFocus: boolean read fMsgViewFocus write fMsgViewFocus;

    // glyphs
    property ShowButtonGlyphs: TApplicationShowGlyphs read FShowButtonGlyphs write FShowButtonGlyphs;
    property ShowMenuGlyphs: TApplicationShowGlyphs read FShowMenuGlyphs write FShowMenuGlyphs;

    // default template for each 'new item' category: Name=Path, Value=TemplateName
    property NewUnitTemplate: string read FNewUnitTemplate write FNewUnitTemplate;
    property NewFormTemplate: string read FNewFormTemplate write FNewFormTemplate;
  end;

var
  EnvironmentOptions: TEnvironmentOptions = nil;

function PascalExtToType(const Ext: string): TPascalExtType;
function AmbiguousFileActionNameToType(const Action: string): TAmbiguousFileAction;
function CharCaseFileActionNameToType(const Action: string): TCharCaseFileAction;
function UnitRenameReferencesActionNameToType(const Action: string): TUnitRenameReferencesAction;

function CheckFileChanged(const OldFilename, NewFilename: string): boolean;
function CheckExecutable(const OldFilename, NewFilename: string;
  const ErrorCaption, ErrorMsg: string; SearchInPath: boolean = false): boolean;
function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; out StopChecking: boolean): boolean;

const
  DefaultLazDocPath = '$(LazarusDir)/docs/xml/lcl';
  DefaultMsgViewFocus = {$IFDEF Windows}true{$ELSE}false{$ENDIF};
  MaxComboBoxCount: integer = 20;
  EnvOptsConfFileName = 'environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';
  
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

function CheckFileChanged(const OldFilename,
  NewFilename: string): boolean;
begin
  Result:=(NewFilename<>OldFilename) and (NewFilename<>'');
end;

function CheckExecutable(const OldFilename,
  NewFilename: string; const ErrorCaption, ErrorMsg: string;
  SearchInPath: boolean): boolean;
var
  Filename: String;
begin
  Result:=true;
  if not CheckFileChanged(OldFilename,NewFilename) then exit;
  Filename:=NewFilename;
  if (not FilenameIsAbsolute(NewFilename)) and SearchInPath then begin
    Filename:=FindDefaultExecutablePath(NewFilename);
    if Filename='' then
      Filename:=NewFilename;
  end;

  if (not FileIsExecutable(Filename)) then begin
    if MessageDlg(ErrorCaption,Format(ErrorMsg,[Filename]),
      mtWarning,[mbIgnore,mbCancel],0)=mrCancel
    then begin
      Result:=false;
    end;
  end;
end;

function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
begin
  if not DirPathExists(Dir) then begin
    Result:=MessageDlg(ErrorCaption,Format(ErrorMsg,[Dir]),mtWarning,
                       [mbIgnore,mbCancel],0);
  end else
    Result:=mrOk;
end;

function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; out StopChecking: boolean): boolean;
var
  SubResult: TModalResult;
begin
  StopChecking:=true;
  if not CheckFileChanged(OldDir,NewDir) then begin
    Result:=true;
    exit;
  end;
  SubResult:=CheckDirPathExists(NewDir,lisEnvOptDlgDirectoryNotFound,
                                NotFoundErrMsg);
  if SubResult=mrIgnore then begin
    Result:=true;
    exit;
  end;
  if SubResult=mrCancel then begin
    Result:=false;
    exit;
  end;
  StopChecking:=false;
  Result:=true;
end;

{ TEnvironmentOptions }

constructor TEnvironmentOptions.Create;
begin
  inherited Create;

  FFilename:='';

  // language
  LanguageID:='';

  // auto save
  FAutoSaveEditorFiles:=true;
  FAutoSaveProject:=true;
  FAutoSaveIntervalInSecs:=300; // 5 minutes
  FLastSavedProjectFile:='';

  // windows
  InitLayoutList;
  FIDEDialogLayoutList:=TIDEDialogLayoutList.Create;
  if IDEWindowIntf.IDEDialogLayoutList=nil then
    IDEWindowIntf.IDEDialogLayoutList:=FIDEDialogLayoutList;
  FSingleTaskBarButton:=false;
  FHideIDEOnRun:=false;
  FHideMessagesIcons:=false;
  FIDETitleStartsWithProject:=false;
  FIDEProjectDirectoryInIdeTitle:=false;
  FComponentPaletteVisible:=true;
  FIDESpeedButtonsVisible:=true;

  // EnvironmentOptionsDialog editor
  FShowGrid:=true;
  FShowBorderSpacing:=false;
  FGridColor:=clBlack;
  FSnapToGrid:=true;
  FGridSizeX:=8;
  FGridSizeY:=8;
  FShowGuideLines:=true;
  FSnapToGuideLines:=true;
  FGuideLineColorLeftTop:=clBlue;
  FGuideLineColorRightBottom:=clGreen;
  FShowComponentCaptions:=false;
  FShowEditorHints:=true;
  FAutoCreateFormsOnOpen:=true;
  FCheckPackagesOnFormCreate:=true;
  FRightClickSelects:=true;
  FGrabberColor:=clBlack;
  FMarkerColor:=clDkGray;
  FRubberbandSelectionColor:=clNavy;
  FRubberbandCreationColor:=clMaroon;
  FRubberbandSelectsGrandChilds:=true;
  FDesignerPaintLazy:=true;
  FCreateComponentFocusNameProperty:=false;
  FSwitchToFavoritesOITab:=false;

  FCompletionWindowWidth := 320;
  FCompletionWindowHeight := 6;


  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;
  
  // hints
  FCheckDiskChangesWithLoading:=false;
  FShowHintsForComponentPalette:=true;
  FShowHintsForMainSpeedButtons:=true;
  
  // messages view
  fMsgViewDblClickJumps:=true;
  fMsgViewFocus:=DefaultMsgViewFocus;

  // glyphs
  FShowButtonGlyphs := sbgSystem;
  FShowMenuGlyphs := sbgSystem;

  // files
  LazarusDirectory:=IDEProcs.ProgramDirectory(true);
  FLazarusDirsHistory:=TStringList.Create;
  CompilerFilename:='';
  FCompilerFilenameParsedStamp:=InvalidParseStamp;
  FCompilerFileHistory:=TStringList.Create;
  FPCSourceDirectory:='';
  FFPCSrcDirParsedStamp:=InvalidParseStamp;
  FFPCSourceDirHistory:=TStringList.Create;
  MakeFilename:='';
  FMakeFileHistory:=TStringList.Create;
  DebuggerFilename:='';
  FDebuggerFileHistory:=TStringList.Create;
  FDebuggerProperties := TStringList.Create;
  FDebuggerSearchPath:='';
  FDebuggerEventLogColors:=DebuggerDefaultColors;

  TestBuildDirectory:=GetDefaultTestBuildDirectory;
  FTestBuildDirHistory:=TStringList.Create;
  CompilerMessagesFilename:='';
  FCompilerMessagesFileHistory:=TStringList.Create;

  // recent files and directories
  FRecentOpenFiles:=TStringList.Create;
  FMaxRecentOpenFiles:=10;
  FRecentProjectFiles:=TStringList.Create;
  FMaxRecentProjectFiles:=5;
  FRecentPackageFiles:=TStringList.Create;
  FMaxRecentPackageFiles:=10;
  FOpenLastProjectAtStart:=true;

  // backup
  with FBackupInfoProjectFiles do begin
    BackupType:=bakSameName;
    AdditionalExtension:='bak';  // for bakUserDefinedAddExt
    MaxCounter:=3;               // for bakCounter
    SubDirectory:='';
  end;
  with FBackupInfoOtherFiles do begin
    BackupType:=bakUserDefinedAddExt;
    AdditionalExtension:='bak';  // for bakUserDefinedAddExt
    MaxCounter:=3;               // for bakCounter
    SubDirectory:='';
  end;
  
  // external tools
  fExternalTools:=ExternalToolListClass.Create;
  
  // naming
  fPascalFileExtension:=petPAS;
  fCharcaseFileAction:=ccfaAutoRename;
  FUnitRenameReferencesAction:=urraAsk;
  FAskForFilenameOnNewFile:=false;
  FLowercaseDefaultFilename:=true;

  //debug
  (* TODO: maybe revert relations. Create this in Debugger, and call environmentoptions for the configstore only? *)
  FDebuggerConfig := TDebuggerConfigStore.Create;

  // lazdoc
  FLazDocPaths:=SetDirSeparators(DefaultLazDocPath);
end;

destructor TEnvironmentOptions.Destroy;
var
  i: Integer;
begin
  FreeAndNil(fExternalTools);
  FreeAndNil(FRecentOpenFiles);
  FreeAndNil(FRecentProjectFiles);
  FreeAndNil(FRecentPackageFiles);
  FreeAndNil(FObjectInspectorOptions);
  FreeAndNil(FLazarusDirsHistory);
  FreeAndNil(FCompilerFileHistory);
  FreeAndNil(FFPCSourceDirHistory);
  FreeAndNil(FMakeFileHistory);
  FreeAndNil(FDebuggerFileHistory);
  for i := 0 to FDebuggerProperties.Count - 1 do
    FDebuggerProperties.Objects[i].Free;
  FreeAndNil(FDebuggerProperties);
  FreeAndNil(FTestBuildDirHistory);
  FreeAndNil(FCompilerMessagesFileHistory);
  if IDEWindowIntf.IDEDialogLayoutList=FIDEDialogLayoutList then
    IDEWindowIntf.IDEDialogLayoutList:=nil;
  FreeAndNil(FIDEDialogLayoutList);
  FreeAndNil(FDebuggerConfig);
  FreeAndNil(FConfigStore);
  FreeAndNil(FDbgConfigStore);
  FreeAndNil(FXMLCfg);
  inherited Destroy;
end;

class function TEnvironmentOptions.GetGroupCaption: string;
begin
  Result := dlgGroupEnvironment;
end;

class function TEnvironmentOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := EnvironmentOptions;
end;

procedure TEnvironmentOptions.DoAfterWrite(Restore: boolean);
begin
  if not Restore then
    Save(False);
  inherited DoAfterWrite(Restore);
end;

procedure TEnvironmentOptions.CreateConfig;
var
  ConfFileName: string;
begin
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+EnvOptsConfFileName);
  CopySecondaryConfigFile(EnvOptsConfFileName);
  if (not FileExistsUTF8(ConfFileName)) then begin
    //DebugLn('Note: environment config file not found - using defaults');
  end;
  Filename:=ConfFilename;
end;

procedure TEnvironmentOptions.GetDefaultFPCSourceDirectory;
begin

end;

procedure TEnvironmentOptions.SetFileName(const NewFilename: string);
begin
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
  FFileHasChangedOnDisk:=true;
end;

procedure TEnvironmentOptions.Load(OnlyDesktop:boolean);
var
  XMLConfig: TXMLConfig;
  FileVersion: integer;

  procedure AddRecentProjectInitial(aProjPath, aProjFile: string);
  // Add a project to the list of recent projects if the project has write access.
  // The check can be removed when the IDE allows compiling read-only projects.
  var
    WholeFilePath: String;
  begin
    aProjPath:=SwitchPathDelims(aProjPath, True);
    WholeFilePath:=ExtractFilePath(Application.ExeName) + aProjPath + aProjFile;
    if FileIsWritable(aProjPath) and FileIsWritable(WholeFilePath) then
      AddToRecentList(WholeFilePath, FRecentProjectFiles, FMaxRecentProjectFiles);
  end;

  procedure LoadBackupInfo(var BackupInfo: TBackupInfo; const Path:string);
  var i:integer;
  begin
    with BackupInfo do begin
      i:=XMLConfig.GetValue(Path+'Type',5);
      case i of
       0:BackupType:=bakNone;
       1:BackupType:=bakSymbolInFront;
       2:BackupType:=bakSymbolBehind;
       3:BackupType:=bakCounter;
       4:BackupType:=bakSameName;
      else
        BackupType:=bakUserDefinedAddExt;
      end;
      AdditionalExtension:=XMLConfig.GetValue(Path+'AdditionalExtension','bak');
      MaxCounter:=XMLConfig.GetValue(Path+'MaxCounter',9);
      if FileVersion<101 then
        SubDirectory:=''
      else
        SubDirectory:=XMLConfig.GetValue(Path+'SubDirectory','backup');
    end;
  end;

  procedure LoadPascalFileExt(const Path: string);
  begin
    fPascalFileExtension:=PascalExtToType(XMLConfig.GetValue(
      Path+'Naming/PascalFileExtension',PascalExtension[petPAS]));
    if fPascalFileExtension=petNone then
      fPascalFileExtension:=petPAS;
  end;
  
  procedure LoadLanguage;
  begin
    fLanguageID:=XMLConfig.GetValue('EnvironmentOptions/Language/ID','');
  end;

var
  Path: String;
  CurPath: String;
  i, j: Integer;
  Rec: PIDEOptionsGroupRec;
  Cfg: TXMLOptionsStorage;
  EventType: TDBGEventType;
  NodeName: String;
  s: String;
begin
  Cfg:=nil;
  try
    XMLConfig:=GetXMLCfg(false);
    Cfg:=TXMLOptionsStorage.Create(XMLConfig);
    try
      Path:='EnvironmentOptions/';
      FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);

      // language
      LoadLanguage;

      // auto save
      FAutoSaveEditorFiles:=XMLConfig.GetValue(
         Path+'AutoSave/EditorFiles',true);
      FAutoSaveProject:=XMLConfig.GetValue(
         Path+'AutoSave/Project',true);
      FAutoSaveIntervalInSecs:=XMLConfig.GetValue(
         Path+'AutoSave/IntervalInSecs',600);
      FLastSavedProjectFile:=XMLConfig.GetValue(
         Path+'AutoSave/LastSavedProjectFile','');
      FOpenLastProjectAtStart:=XMLConfig.GetValue(
         Path+'AutoSave/OpenLastProjectAtStart',true);
      FShowCompileDialog:=XMLConfig.GetValue(
         Path+'ShowCompileDialog/Value',false);
      FAutoCloseCompileDialog:=XMLConfig.GetValue(
         Path+'AutoCloseCompileDialog/Value',false);

      IDEWindowCreators.SimpleLayoutStorage.LoadFromConfig(Cfg,Path+'Desktop/');
      FIDEDialogLayoutList.LoadFromConfig(FConfigStore,
        Path+'Desktop/Dialogs/');
      FSingleTaskBarButton := XMLConfig.GetValue(
        Path+'Desktop/SingleTaskBarButton/Value', False);
      FHideIDEOnRun:=XMLConfig.GetValue(
        Path+'Desktop/HideIDEOnRun/Value',false);
      FHideMessagesIcons:=XMLConfig.GetValue(
        Path+'Desktop/HideMessagesIcons/Value',false);
      FIDETitleStartsWithProject:=XMLConfig.GetValue(
        Path+'Desktop/IDETitleStartsWithProject/Value',false);
      IDEProjectDirectoryInIdeTitle:=XMLConfig.GetValue(
        Path+'Desktop/IDEProjectDirectoryInIdeTitle/Value',false);
      FComponentPaletteVisible:=XMLConfig.GetValue(
        Path+'Desktop/ComponentPaletteVisible/Value',true);
      FIDESpeedButtonsVisible:=XMLConfig.GetValue(
        Path+'Desktop/IDESpeedButtonsVisible/Value',true);
      FCompletionWindowWidth:=XMLConfig.GetValue(
        Path+'Desktop/CompletionWindowWidth/Value', 320);
      FCompletionWindowHeight:=XMLConfig.GetValue(
        Path+'Desktop/CompletionWindowHeight/Value', 6);

      // EnvironmentOptionsDialog editor
      FShowGrid:=XMLConfig.GetValue(
         Path+'FormEditor/ShowGrid',true);
      FShowBorderSpacing:=XMLConfig.GetValue(
         Path+'FormEditor/ShowBorderSpacing',false);
      FGridColor:=XMLConfig.GetValue(
         Path+'FormEditor/GridColor',FGridColor);
      FSnapToGrid:=XMLConfig.GetValue(
         Path+'FormEditor/SnapToGrid',true);
      FGridSizeX:=XMLConfig.GetValue(
         Path+'FormEditor/GridSizeX',8);
      FGridSizeY:=XMLConfig.GetValue(
         Path+'FormEditor/GridSizeY',8);
      FShowGuideLines:=XMLConfig.GetValue(
         Path+'FormEditor/ShowGuideLines',true);
      FSnapToGuideLines:=XMLConfig.GetValue(
         Path+'FormEditor/SnapToGuideLines',true);
      FGuideLineColorLeftTop:=XMLConfig.GetValue(
         Path+'FormEditor/GuideLineColorLeftTop',
         FGuideLineColorLeftTop);
      FGuideLineColorRightBottom:=XMLConfig.GetValue(
         Path+'FormEditor/GuideLineColorRightBottom',
         FGuideLineColorRightBottom);
      FShowComponentCaptions:=XMLConfig.GetValue(
         Path+'FormEditor/ShowComponentCaptions',true);
      FShowEditorHints:=XMLConfig.GetValue(
         Path+'FormEditor/ShowEditorHints',true);
      FAutoCreateFormsOnOpen:=XMLConfig.GetValue(
         Path+'FormEditor/AutoCreateFormsOnOpen',true);
      FCheckPackagesOnFormCreate:=XMLConfig.GetValue(
         Path+'FormEditor/CheckPackagesOnFormCreate',true);
      FRightClickSelects:=XMLConfig.GetValue(
         Path+'FormEditor/RightClickSelects',true);
      FGrabberColor:=XMLConfig.GetValue(
         Path+'FormEditor/GrabberColor/Value',FGrabberColor);
      FMarkerColor:=XMLConfig.GetValue(
         Path+'FormEditor/MarkerColor/Value',FMarkerColor);
      FRubberbandSelectionColor:=XMLConfig.GetValue(
         Path+'FormEditor/Rubberband/SelectionColor/Value',
         FRubberbandSelectionColor);
      FRubberbandCreationColor:=XMLConfig.GetValue(
         Path+'FormEditor/Rubberband/CreationColor/Value',
         FRubberbandCreationColor);
      FRubberbandSelectsGrandChilds:=XMLConfig.GetValue(
         Path+'FormEditor/Rubberband/SelectsGrandChilds/Value',
         false);
      FDesignerPaintLazy:=XMLConfig.GetValue(
         Path+'FormEditor/DesignerPaint/Lazy/Value',true);
      FCreateComponentFocusNameProperty:=XMLConfig.GetValue(
         Path+'FormEditor/CreateComponentFocusNameProperty/Value',false);
      FSwitchToFavoritesOITab:=XMLConfig.GetValue(
         Path+'FormEditor/SwitchToFavoritesOITab/Value',false);

      if not OnlyDesktop then begin
        // files
        s:=TrimFilename(
           XMLConfig.GetValue(Path+'LazarusDirectory/Value',FLazarusDirectory));
        if not FilenameIsAbsolute(s) then
          s:=TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+s);
        LazarusDirectory:=s;
        LoadRecentList(XMLConfig,FLazarusDirsHistory,
           Path+'LazarusDirectory/History/');
        if FLazarusDirsHistory.Count=0 then
          FLazarusDirsHistory.Add(ProgramDirectory(true));
        CompilerFilename:=TrimFilename(XMLConfig.GetValue(
                              Path+'CompilerFilename/Value',FCompilerFilename));
        LoadRecentList(XMLConfig,FCompilerFileHistory,
           Path+'CompilerFilename/History/');
        if FCompilerFileHistory.Count=0 then
          GetDefaultCompilerFilenames(FCompilerFileHistory);
        FPCSourceDirectory:=XMLConfig.GetValue(
           Path+'FPCSourceDirectory/Value',FFPCSourceDirectory);
        LoadRecentList(XMLConfig,FFPCSourceDirHistory,
           Path+'FPCSourceDirectory/History/');
        if FFPCSourceDirHistory.Count=0 then begin

        end;
        MakeFilename:=TrimFilename(XMLConfig.GetValue(
           Path+'MakeFilename/Value',FMakeFilename));
        LoadRecentList(XMLConfig,FMakeFileHistory,
           Path+'MakeFilename/History/');
        if FMakeFileHistory.Count=0 then
          GetDefaultMakeFilenames(FMakeFileHistory);

        TestBuildDirectory:=XMLConfig.GetValue(
           Path+'TestBuildDirectory/Value',FTestBuildDirectory);
        LoadRecentList(XMLConfig,FTestBuildDirHistory,
           Path+'TestBuildDirectory/History/');
        if FTestBuildDirHistory.Count=0 then
          GetDefaultTestBuildDirs(FTestBuildDirHistory);
        CompilerMessagesFilename:=XMLConfig.GetValue(
           Path+'CompilerMessagesFilename/Value',FCompilerMessagesFilename);
        LoadRecentList(XMLConfig, FCompilerMessagesFileHistory,
           Path+'CompilerMessagesFilename/History/');

        // backup
        LoadBackupInfo(FBackupInfoProjectFiles
          ,Path+'BackupProjectFiles/');
        LoadBackupInfo(FBackupInfoOtherFiles
          ,Path+'BackupOtherFiles/');

        // Debugger
        FDebuggerConfig.Load;
        DebuggerFilename:=XMLConfig.GetValue(
           Path+'DebuggerFilename/Value','');
        LoadRecentList(XMLConfig,FDebuggerFileHistory,
           Path+'DebuggerFilename/History/');
        DebuggerSearchPath:=XMLConfig.GetValue(
           Path+'DebuggerSearchPath/Value','');
        // Debugger General Options
        DebuggerShowStopMessage:=XMLConfig.GetValue(
           Path+'DebuggerOptions/ShowStopMessage/Value', True);
        FDebuggerEventLogClearOnRun := XMLConfig.GetValue(
          Path+'Debugger/EventLogClearOnRun', True);
        FDebuggerEventLogCheckLineLimit := XMLConfig.GetValue(
          Path+'Debugger/EventLogCheckLineLimit', False);
        FDebuggerEventLogLineLimit := XMLConfig.GetValue(
          Path+'Debugger/EventLogLineLimit', 1000);
        FDebuggerEventLogShowBreakpoint := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowBreakpoint', False);
        FDebuggerEventLogShowProcess := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowProcess', True);
        FDebuggerEventLogShowThread := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowThread', True);
        FDebuggerEventLogShowModule := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowModule', False);
        FDebuggerEventLogShowOutput := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowOutput', True);
        FDebuggerEventLogShowWindows := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowWindows', False);
        FDebuggerEventLogShowDebugger := XMLConfig.GetValue(
          Path+'Debugger/EventLogShowDebugger', True);
        FDebuggerEventLogUseColors := XMLConfig.GetValue(
          Path+'Debugger/EventLogUseColors', True);
        for EventType := Low(TDBGEventType) to High(TDBGEventType) do
        begin
          FDebuggerEventLogColors[EventType].Background :=
            XMLConfig.GetValue(Path+'Debugger/EventLogColors/' +
            GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Background',
            DebuggerDefaultColors[EventType].Background);
          FDebuggerEventLogColors[EventType].Foreground :=
            XMLConfig.GetValue(Path+'Debugger/EventLogColors/' +
            GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Foreground',
            DebuggerDefaultColors[EventType].Foreground);
        end;
      end;

      // hints
      FCheckDiskChangesWithLoading:=XMLConfig.GetValue(
        Path+'CheckDiskChangesWithLoading/Value',false);
      FShowHintsForComponentPalette:=XMLConfig.GetValue(
        Path+'ShowHintsForComponentPalette/Value',true);
      FShowHintsForMainSpeedButtons:=XMLConfig.GetValue(
        Path+'ShowHintsForMainSpeedButtons/Value',true);

      // messages view
      fMsgViewDblClickJumps:=XMLConfig.GetValue(
        Path+'MsgViewDblClickJumps/Value',false);
      fMsgViewFocus:=XMLConfig.GetValue(
        Path+'MsgViewFocus/Value',DefaultMsgViewFocus);

      // glyphs
      FShowButtonGlyphs := TApplicationShowGlyphs(XMLConfig.GetValue(Path+'ShowButtonGlyphs/Value',
        Ord(sbgSystem)));
      FShowMenuGlyphs := TApplicationShowGlyphs(XMLConfig.GetValue(Path+'ShowMenuGlyphs/Value',
        Ord(sbgSystem)));

      // recent files and directories
      FMaxRecentOpenFiles:=XMLConfig.GetValue(
        Path+'Recent/OpenFiles/Max',FMaxRecentOpenFiles);
      LoadRecentList(XMLConfig,FRecentOpenFiles,
        Path+'Recent/OpenFiles/');
      FMaxRecentProjectFiles:=XMLConfig.GetValue(
        Path+'Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
      LoadRecentList(XMLConfig,FRecentProjectFiles,
        Path+'Recent/ProjectFiles/');
      FMaxRecentPackageFiles:=XMLConfig.GetValue(
        Path+'Recent/PackageFiles/Max',FMaxRecentOpenFiles);
      LoadRecentList(XMLConfig,FRecentPackageFiles,
        Path+'Recent/PackageFiles/');

      // Add example projects to an empty project list if examples have write access
      if FRecentProjectFiles.count=0 then begin
        AddRecentProjectInitial('examples/jpeg/',          'jpegexample.lpi');
        AddRecentProjectInitial('examples/sprites/',       'spriteexample.lpi');
        AddRecentProjectInitial('examples/openglcontrol/', 'openglcontrol_demo.lpi');
        AddRecentProjectInitial('examples/barchart/',      'chartdemo.lpi');
        AddRecentProjectInitial('examples/',               'hello.lpi');
      end;

      // external tools
      fExternalTools.Load(FConfigStore,Path+'ExternalTools/');

      // naming
      LoadPascalFileExt(Path+'');
      if FileVersion>=103 then begin
        fCharcaseFileAction:=CharCaseFileActionNameToType(XMLConfig.GetValue(
          Path+'CharcaseFileAction/Value',''));
      end else begin
        if XMLConfig.GetValue(Path+'PascalFileAskLowerCase/Value',true) then
          fCharcaseFileAction:=ccfaAsk
        else if XMLConfig.GetValue(Path+'PascalFileAutoLowerCase/Value',false)
        then
          fCharcaseFileAction:=ccfaAutoRename
        else
          fCharcaseFileAction:=ccfaIgnore;
      end;
      if FileVersion>=104 then
        CurPath:=Path+'AmbiguousFileAction/Value'
      else
        CurPath:=Path+'AmbigiousFileAction/Value';
      fAmbiguousFileAction:=AmbiguousFileActionNameToType(XMLConfig.GetValue(
        CurPath,AmbiguousFileActionNames[fAmbiguousFileAction]));
      FUnitRenameReferencesAction:=UnitRenameReferencesActionNameToType(XMLConfig.GetValue(
        Path+'UnitRenameReferencesAction/Value',UnitRenameReferencesActionNames[urraAsk]));
      FAskForFilenameOnNewFile:=XMLConfig.GetValue(
        Path+'AskForFilenameOnNewFile/Value',false);
      FLowercaseDefaultFilename:=XMLConfig.GetValue(
        Path+'LowercaseDefaultFilename/Value',true);

      //lazdoc
      FLazDocPaths := XMLConfig.GetValue(Path+'LazDoc/Paths', DefaultLazDocPath);
      if FileVersion<=105 then
        FLazDocPaths:=LineBreaksToDelimiter(FLazDocPaths,';');

      // 'new items'
      FNewUnitTemplate:=XMLConfig.GetValue(Path+'New/UnitTemplate/Value',FileDescNamePascalUnit);
      FNewFormTemplate:=XMLConfig.GetValue(Path+'New/FormTemplate/Value',FileDescNameLCLForm);

      // object inspector
      FObjectInspectorOptions.Load;
      FObjectInspectorOptions.SaveBounds:=false;

      // IDEEditorGroups
      for i := 0 to IDEEditorGroups.Count - 1 do begin
        Rec := IDEEditorGroups[i];
        NodeName := Rec^.GroupClass.ClassName;
        Rec^.Collapsed := XMLConfig.GetValue(Path+'OptionDialog/Tree/' + NodeName + '/Value',
                                             Rec^.DefaultCollapsed);
        if Rec^.Items <> nil then begin
          for j := 0 to Rec^.Items.Count - 1 do begin
            Rec^.Items[j]^.Collapsed := XMLConfig.GetValue(Path+'OptionDialog/Tree/' + NodeName
                  + '/' + Rec^.Items[j]^.EditorClass.ClassName + '/Value',
                  Rec^.Items[j]^.DefaultCollapsed);
          end;
        end;
      end;

    finally
      Cfg.Free;
    end;
    FileUpdated;
  except
    // ToDo
    on E: Exception do
      DebugLn('[TEnvironmentOptions.Load]  error reading "',FFilename,'": '+E.Message);
  end;
end;

procedure TEnvironmentOptions.Save(OnlyDesktop: boolean);
var
  XMLConfig: TXMLConfig;

  procedure SaveBackupInfo(var BackupInfo: TBackupInfo; Path:string);
  var i:integer;
  begin
    with BackupInfo do begin
      case BackupType of
       bakNone: i:=0;
       bakSymbolInFront: i:=1;
       bakSymbolBehind: i:=2;
       bakCounter: i:=3;
       bakSameName: i:=4;
      else
        i:=5; // bakUserDefinedAddExt;
      end;
      XMLConfig.SetDeleteValue(Path+'Type',i,5);
      XMLConfig.SetDeleteValue(Path+'AdditionalExtension',AdditionalExtension,'.bak');
      XMLConfig.SetDeleteValue(Path+'MaxCounter',MaxCounter,10);
      XMLConfig.SetDeleteValue(Path+'SubDirectory',SubDirectory,'backup');
    end;
  end;

var
  Path: String;
  i, j: Integer;
  NodeName: String;
  Rec: PIDEOptionsGroupRec;
  Cfg: TXMLOptionsStorage;
  EventType: TDBGEventType;
  CurLazDir: String;
  BaseDir: String;
begin
  Cfg:=nil;
  try
    XMLConfig:=GetXMLCfg(true);
    Cfg:=TXMLOptionsStorage.Create(XMLConfig);
    try
      Path:='EnvironmentOptions/';

      XMLConfig.SetValue(Path+'Version/Value',EnvOptsVersion);

      // language
      XMLConfig.SetDeleteValue(Path+'Language/ID',LanguageID,'');

      // auto save
      XMLConfig.SetDeleteValue(Path+'AutoSave/EditorFiles'
         ,FAutoSaveEditorFiles,true);
      XMLConfig.SetDeleteValue(Path+'AutoSave/Project',
         FAutoSaveProject,true);
      XMLConfig.SetDeleteValue(Path+'AutoSave/IntervalInSecs'
         ,FAutoSaveIntervalInSecs,600);
      XMLConfig.SetDeleteValue(Path+'AutoSave/LastSavedProjectFile'
         ,FLastSavedProjectFile,'');
      XMLConfig.SetDeleteValue(Path+'AutoSave/OpenLastProjectAtStart',
         FOpenLastProjectAtStart,true);

      // windows
      IDEWindowCreators.SimpleLayoutStorage.SaveToConfig(Cfg,Path+'Desktop/');
      FIDEDialogLayoutList.SaveToConfig(FConfigStore,Path+'Desktop/Dialogs/');
      XMLConfig.SetDeleteValue(Path+'Desktop/SingleTaskBarButton/Value',
                               FSingleTaskBarButton, False);
      XMLConfig.SetDeleteValue(Path+'Desktop/HideIDEOnRun/Value',FHideIDEOnRun,
                               false);
      XMLConfig.SetDeleteValue(Path+'Desktop/HideMessagesIcons/Value',FHideMessagesIcons,
                               false);
      XMLConfig.SetDeleteValue(Path+'Desktop/IDETitleStartsWithProject/Value',
                               FIDETitleStartsWithProject,false);
      XMLConfig.SetDeleteValue(Path+'Desktop/IDEProjectDirectoryInIdeTitle/Value',
                               FIDEProjectDirectoryInIdeTitle,false);
      XMLConfig.SetDeleteValue(Path+'Desktop/ComponentPaletteVisible/Value',
                               FComponentPaletteVisible,true);
      XMLConfig.SetDeleteValue(Path+'Desktop/IDESpeedButtonsVisible/Value',
                               FIDESpeedButtonsVisible,true);
      XMLConfig.SetDeleteValue(Path+'Desktop/CompletionWindowWidth/Value',
                               FCompletionWindowWidth, 320);
      XMLConfig.SetDeleteValue(Path+'Desktop/CompletionWindowHeight/Value',
                               FCompletionWindowHeight, 6);

      // EnvironmentOptionsDialog editor
      XMLConfig.SetDeleteValue(Path+'FormEditor/ShowBorderSpacing',
                               FShowBorderSpacing,false);
      XMLConfig.SetDeleteValue(Path+'FormEditor/ShowGrid',FShowGrid,true);
      XMLConfig.SetDeleteValue(Path+'FormEditor/GridColor',FGridColor,clBlack);
      XMLConfig.SetDeleteValue(Path+'FormEditor/SnapToGrid',FSnapToGrid,true);
      XMLConfig.SetDeleteValue(Path+'FormEditor/GridSizeX',FGridSizeX,8);
      XMLConfig.SetDeleteValue(Path+'FormEditor/GridSizeY',FGridSizeY,8);
      XMLConfig.SetDeleteValue(Path+'FormEditor/ShowGuideLines',FShowGuideLines,
                               true);
      XMLConfig.SetDeleteValue(Path+'FormEditor/SnapToGuideLines',
                               FSnapToGuideLines,true);
      XMLConfig.SetDeleteValue(Path+'FormEditor/GuideLineColorLeftTop',
         FGuideLineColorLeftTop,clGreen);
      XMLConfig.SetDeleteValue(Path+'FormEditor/GuideLineColorRightBottom',
         FGuideLineColorRightBottom,clBlue);
      XMLConfig.SetDeleteValue(Path+'FormEditor/ShowComponentCaptions',
         FShowComponentCaptions,true);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/ShowEditorHints',FShowEditorHints,true);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/AutoCreateFormsOnOpen',FAutoCreateFormsOnOpen,true);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/CheckPackagesOnFormCreate',FCheckPackagesOnFormCreate,true);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/RightClickSelects',FRightClickSelects,true);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/GrabberColor/Value',FGrabberColor,clBlack);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/MarkerColor/Value',FMarkerColor,clDkGray);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/Rubberband/SelectionColor/Value',
         FRubberbandSelectionColor,clBlack);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/Rubberband/CreationColor/Value',
         FRubberbandCreationColor,clRed);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/Rubberband/SelectsGrandChilds/Value',
         FRubberbandSelectsGrandChilds,false);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/DesignerPaint/Lazy/Value',FDesignerPaintLazy,true);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/CreateComponentFocusNameProperty/Value',FCreateComponentFocusNameProperty,false);
      XMLConfig.SetDeleteValue(
         Path+'FormEditor/SwitchToFavoritesOITab/Value',FSwitchToFavoritesOITab,false);

      XMLConfig.SetDeleteValue(
         Path+'ShowCompileDialog/Value',FShowCompileDialog,False);
      XMLConfig.SetDeleteValue(
         Path+'AutoCloseCompileDialog/Value',FAutoCloseCompileDialog,False);

      if not OnlyDesktop then begin
        // files
        CurLazDir:=ChompPathDelim(FLazarusDirectory);
        BaseDir:=ExtractFilePath(ChompPathDelim(GetPrimaryConfigPath));
        if (CompareFilenames(BaseDir,CurLazDir)=0)
        or FileIsInPath(CurLazDir,BaseDir) then begin
          // the pcp directory is in the lazarus directory
          // or the lazarus directory is a sibling or a sub dir of a sibling of the pcp
          // examples:
          //   pcp=C:\Lazarus\config, lazdir=C:\Lazarus => store '..'
          //   pcp=/home/user/.lazarus, lazdir=/home/user/freepascal/lazarus => store ../freepascal/lazarus
          CurLazDir:=CreateRelativePath(CurLazDir,GetPrimaryConfigPath);
        end;
        XMLConfig.SetDeleteValue(Path+'LazarusDirectory/Value',CurLazDir,'');
        SaveRecentList(XMLConfig,FLazarusDirsHistory,
           Path+'LazarusDirectory/History/');
        XMLConfig.SetDeleteValue(
           Path+'CompilerFilename/Value',FCompilerFilename,'');
        SaveRecentList(XMLConfig,FCompilerFileHistory,
           Path+'CompilerFilename/History/');
        XMLConfig.SetValue(
           Path+'FPCSourceDirectory/Value',FFPCSourceDirectory);
        SaveRecentList(XMLConfig,FFPCSourceDirHistory,
           Path+'FPCSourceDirectory/History/');
        XMLConfig.SetDeleteValue(
           Path+'MakeFilename/Value',FMakeFilename,'');
        SaveRecentList(XMLConfig,FMakeFileHistory,
           Path+'MakeFilename/History/');
        XMLConfig.SetValue(
           Path+'TestBuildDirectory/Value',FTestBuildDirectory);
        SaveRecentList(XMLConfig,FTestBuildDirHistory,
           Path+'TestBuildDirectory/History/');
        XMLConfig.SetValue(
           Path+'CompilerMessagesFilename/Value',FCompilerMessagesFilename);
        SaveRecentList(XMLConfig,FCompilerMessagesFileHistory,
           Path+'CompilerMessagesFilename/History/');

        // backup
        SaveBackupInfo(FBackupInfoProjectFiles
          ,Path+'BackupProjectFiles/');
        SaveBackupInfo(FBackupInfoOtherFiles
          ,Path+'BackupOtherFiles/');

        // debugger
        FDebuggerConfig.Save;
        SaveDebuggerPropertiesList;
        XMLConfig.SetDeleteValue(Path+'DebuggerFilename/Value',
            FDebuggerFilename,'');
        XMLConfig.SetDeleteValue(Path+'DebuggerOptions/ShowStopMessage/Value',
            FDebuggerShowStopMessage, True);
        SaveRecentList(XMLConfig,FDebuggerFileHistory,
           Path+'DebuggerFilename/History/');
        XMLConfig.SetDeleteValue(Path+'DebuggerSearchPath/Value',
            FDebuggerSearchPath,'');
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogClearOnRun',
            FDebuggerEventLogClearOnRun, True);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogCheckLineLimit',
            FDebuggerEventLogCheckLineLimit, False);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogLineLimit',
            FDebuggerEventLogLineLimit, 1000);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowBreakpoint',
            FDebuggerEventLogShowBreakpoint, False);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowProcess',
            FDebuggerEventLogShowProcess, True);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowThread',
            FDebuggerEventLogShowThread, True);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowModule',
            FDebuggerEventLogShowModule, False);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowOutput',
            FDebuggerEventLogShowOutput, True);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowWindows',
            FDebuggerEventLogShowWindows, False);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogShowDebugger',
            FDebuggerEventLogShowDebugger, True);
        XMLConfig.SetDeleteValue(Path+'Debugger/EventLogUseColors',
            FDebuggerEventLogUseColors, True);
        for EventType := Low(TDBGEventType) to High(TDBGEventType) do
        begin
          XMLConfig.SetDeleteValue(Path+'Debugger/EventLogColors/' +
            GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Background',
              FDebuggerEventLogColors[EventType].Background,
              DebuggerDefaultColors[EventType].Background);
          XMLConfig.SetDeleteValue(Path+'Debugger/EventLogColors/' +
            GetEnumName(TypeInfo(EventType), Ord(EventType)) + '/Foreground',
              FDebuggerEventLogColors[EventType].Foreground,
              DebuggerDefaultColors[EventType].Foreground);
        end;
      end;

      // hints
      XMLConfig.SetDeleteValue(Path+'CheckDiskChangesWithLoading/Value',
        FCheckDiskChangesWithLoading,false);
      XMLConfig.SetDeleteValue(Path+'ShowHintsForComponentPalette/Value',
        FShowHintsForComponentPalette,true);
      XMLConfig.SetDeleteValue(Path+'ShowHintsForMainSpeedButtons/Value',
        FShowHintsForMainSpeedButtons,true);

      // messages view
      XMLConfig.SetDeleteValue(Path+'MsgViewDblClickJumps/Value',
        fMsgViewDblClickJumps,false);
      XMLConfig.SetDeleteValue(Path+'MsgViewFocus/Value',
        fMsgViewFocus,DefaultMsgViewFocus);

      // glyphs
      XMLConfig.SetDeleteValue(Path+'ShowButtonGlyphs/Value',
        Ord(FShowButtonGlyphs), Ord(sbgSystem));
      XMLConfig.SetDeleteValue(Path+'ShowMenuGlyphs/Value',
        Ord(FShowMenuGlyphs), Ord(sbgSystem));

      // recent files and directories
      XMLConfig.SetValue(
        Path+'Recent/OpenFiles/Max',FMaxRecentOpenFiles);
      SaveRecentList(XMLConfig,FRecentOpenFiles,
        Path+'Recent/OpenFiles/');
      XMLConfig.SetValue(
        Path+'Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
      SaveRecentList(XMLConfig,FRecentProjectFiles,
        Path+'Recent/ProjectFiles/');
      XMLConfig.SetValue(
        Path+'Recent/PackageFiles/Max',FMaxRecentPackageFiles);
      SaveRecentList(XMLConfig,FRecentPackageFiles,
        Path+'Recent/PackageFiles/');

      // external tools
      fExternalTools.Save(FConfigStore,Path+'ExternalTools/');

      // naming
      XMLConfig.SetDeleteValue(Path+'Naming/PascalFileExtension',
                               PascalExtension[fPascalFileExtension],'.pas');
      XMLConfig.SetDeleteValue(Path+'CharcaseFileAction/Value',
                               CharCaseFileActionNames[fCharcaseFileAction],
                               CharCaseFileActionNames[ccfaAutoRename]);
      XMLConfig.SetDeleteValue(Path+'AutoDeleteAmbiguousSources/Value',
        AmbiguousFileActionNames[fAmbiguousFileAction],
        AmbiguousFileActionNames[afaAsk]);
      XMLConfig.SetDeleteValue(Path+'AskForFilenameOnNewFile/Value',
                     FAskForFilenameOnNewFile,false);
      XMLConfig.SetDeleteValue(Path+'LowercaseDefaultFilename/Value',
                               FLowercaseDefaultFilename,true);

      // lazdoc
      XMLConfig.SetDeleteValue(Path+'LazDoc/Paths',FLazDocPaths,DefaultLazDocPath);

      // 'new items'
      XMLConfig.SetDeleteValue(Path+'New/UnitTemplate/Value',FNewUnitTemplate,FileDescNamePascalUnit);
      XMLConfig.SetDeleteValue(Path+'New/FormTemplate/Value',FNewFormTemplate,FileDescNameLCLForm);

      // object inspector
      FObjectInspectorOptions.SaveBounds:=false;
      FObjectInspectorOptions.Save;

      // IDEEditorGroups
      for i := 0 to IDEEditorGroups.Count - 1 do begin
        Rec := IDEEditorGroups[i];
        NodeName := Rec^.GroupClass.ClassName;
        XMLConfig.SetDeleteValue(Path+'OptionDialog/Tree/' + NodeName + '/Value',
                                 Rec^.Collapsed,
                                 Rec^.DefaultCollapsed);
        if Rec^.Items <> nil then begin
          for j := 0 to Rec^.Items.Count - 1 do begin
            XMLConfig.SetDeleteValue(Path+'OptionDialog/Tree/' + NodeName
                                     + '/' + Rec^.Items[j]^.EditorClass.ClassName + '/Value',
                                     Rec^.Items[j]^.Collapsed,
                                     Rec^.Items[j]^.DefaultCollapsed);
          end;
        end;
      end;
    finally
      Cfg.Free;
    end;
    XMLConfig.Flush;
    FileUpdated;
  except
    on E: Exception do begin
      // ToDo
      DebugLn('[TEnvironmentOptions.Save]  error writing "',Filename,'": ',E.Message);
    end;
  end;
end;

procedure TEnvironmentOptions.AddToRecentOpenFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentOpenFiles,FMaxRecentOpenFiles);
end;

procedure TEnvironmentOptions.RemoveFromRecentOpenFiles(const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentOpenFiles);
end;

procedure TEnvironmentOptions.AddToRecentProjectFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentProjectFiles,FMaxRecentProjectFiles);
  {$ifdef Windows}
  SHAddToRecentDocs(SHARD_PATHW, PWideChar(UTF8ToUTF16(AFileName)));
  {$endif}
end;

procedure TEnvironmentOptions.RemoveFromRecentProjectFiles(
  const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentProjectFiles);
end;

procedure TEnvironmentOptions.InitLayoutList;
var
  l: TNonModalIDEWindow;
begin
  for l:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if l<>nmiwNone then
      IDEWindowCreators.SimpleLayoutStorage.CreateWindowLayout(NonModalIDEWindowNames[l]);
  IDEWindowCreators.SimpleLayoutStorage.CreateWindowLayout(DefaultObjectInspectorName);
end;

function TEnvironmentOptions.GetTestBuildDirectory: string;
begin
  Result:=AppendPathDelim(TestBuildDirectory);
end;

function TEnvironmentOptions.GetFPCSourceDirectory: string;
begin
  if (FFPCSrcDirParsedStamp=InvalidParseStamp)
  or (FFPCSrcDirParsedStamp<>CompilerParseStamp)
  then begin
    FFPCSrcDirParsed:=FFPCSourceDirectory;
    GlobalMacroList.SubstituteStr(FFPCSrcDirParsed);
    FFPCSrcDirParsedStamp:=CompilerParseStamp;
  end;
  Result:=FFPCSrcDirParsed;
end;

function TEnvironmentOptions.GetCompilerFilename: string;
begin
  if (FCompilerFilenameParsedStamp=InvalidParseStamp)
  or (FCompilerFilenameParsedStamp<>CompilerParseStamp)
  then begin
    FCompilerFilenameParsed:=FCompilerFilename;
    GlobalMacroList.SubstituteStr(FCompilerFilenameParsed);
    FCompilerFilenameParsedStamp:=CompilerParseStamp;
  end;
  Result:=FCompilerFilenameParsed;
end;

procedure TEnvironmentOptions.InitMacros(AMacroList: TTransferMacroList);
begin
  AMacroList.Add(TTransferMacro.Create('CompPath','',
                 lisCompilerFilename,@MacroFuncCompPath,[]));
  AMacroList.Add(TTransferMacro.Create('FPCSrcDir','',
                 lisFreePascalSourceDirectory,@MacroFuncFPCSrcDir,[]));
  AMacroList.Add(TTransferMacro.Create('LazarusDir','',
                 lisLazarusDirectory,@MacroFuncLazarusDir,[]));
  AMacroList.Add(TTransferMacro.Create('ExeExt','',
                 lisFileExtensionOfPrograms, @MacroFuncExeExt, []));
  AMacroList.Add(TTransferMacro.Create('LanguageID','',
                 lisLazarusLanguageID,@MacroFuncLanguageID,[]));
  AMacroList.Add(TTransferMacro.Create('LanguageName','',
                 lisLazarusLanguageName,@MacroFuncLanguageName,[]));
  AMacroList.Add(TTransferMacro.Create('TestDir','',
                 lisTestDirectory,@MacroFuncTestDir,[]));
  AMacroList.Add(TTransferMacro.Create('ConfDir','',
                 lisConfigDirectory,@MacroFuncConfDir,[]));
  AMacroList.Add(TTransferMacro.Create('Home',GetUserDir,
                 lisUserSHomeDirectory, nil, []));
end;

function TEnvironmentOptions.MacroFuncCompPath(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetCompilerFilename;
end;

function TEnvironmentOptions.MacroFuncFPCSrcDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetFPCSourceDirectory;
end;

function TEnvironmentOptions.MacroFuncLazarusDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=LazarusDirectory;
end;

function TEnvironmentOptions.MacroFuncExeExt(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetExecutableExt;
end;

function TEnvironmentOptions.MacroFuncLanguageID(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=LanguageID;
end;

function TEnvironmentOptions.MacroFuncLanguageName(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetLazarusLanguageLocalizedName(LanguageID);
end;

function TEnvironmentOptions.MacroFuncTestDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetTestBuildDirectory;
end;

function TEnvironmentOptions.MacroFuncConfDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetPrimaryConfigPath;
end;

procedure TEnvironmentOptions.SaveDebuggerPropertiesList;
var
  DProp, DDef: TDebuggerProperties;
  i: Integer;
begin
  for i := 0 to FDebuggerProperties.Count - 1 do begin
    DProp := TDebuggerProperties(FDebuggerProperties.Objects[i]);
    DDef := TDebuggerPropertiesClass(DProp.ClassType).Create;
    FXMLCfg.WriteObject(
      'EnvironmentOptions/Debugger/Class' + FDebuggerProperties[i] + '/Properties/',
      DProp, DDef);
    DDef.Free;
  end;
end;

procedure TEnvironmentOptions.SaveDebuggerProperties(DebuggerClass: String;
  Properties: TDebuggerProperties);
var
  i: Integer;
  Prop: TDebuggerProperties;
begin
  i := FDebuggerProperties.IndexOf(DebuggerClass);
  if i < 0 then begin
    Prop := TDebuggerPropertiesClass(Properties.ClassType).Create;
    Prop.Assign(Properties);
    FDebuggerProperties.AddObject(DebuggerClass, Prop);
  end
  else
    TDebuggerProperties(FDebuggerProperties.Objects[i]).Assign(Properties);
end;

procedure TEnvironmentOptions.LoadDebuggerProperties(DebuggerClass: String;
  Properties: TDebuggerProperties);
var
  i: Integer;
  DDef: TDebuggerProperties;
begin
  i := FDebuggerProperties.IndexOf(DebuggerClass);
  if i < 0 then begin
    DDef := TDebuggerPropertiesClass(Properties.ClassType).Create;
    FXMLCfg.ReadObject('EnvironmentOptions/Debugger/Class' + DebuggerClass + '/Properties/',
      Properties, DDef);
    DDef.Free;
  end
  else
    Properties.Assign(TDebuggerProperties(FDebuggerProperties.Objects[i]));
end;

function TEnvironmentOptions.FileHasChangedOnDisk: boolean;
begin
  Result:=FFileHasChangedOnDisk
      or ((FFilename<>'') and (FFileAge<>0) and (FileAgeCached(FFilename)<>FFileAge));
  FFileHasChangedOnDisk:=Result;
end;

function TEnvironmentOptions.GetXMLCfg(CleanConfig: boolean): TXMLConfig;
begin
  if FileHasChangedOnDisk or (FXMLCfg=nil) then begin
    FreeAndNil(FConfigStore);
    FreeAndNil(FDbgConfigStore);
    FreeAndNil(FXMLCfg);
    InvalidateFileStateCache;
    if CleanConfig then
      FXMLCfg:=TRttiXMLConfig.CreateClean(Filename)
    else
      FXMLCfg:=TRttiXMLConfig.Create(Filename);
    FConfigStore:=TXMLOptionsStorage.Create(FXMLCfg);
    ObjectInspectorOptions.ConfigStore:=FConfigStore;
    FDbgConfigStore:=TXMLOptionsStorage.Create(FXMLCfg, 'EnvironmentOptions/Debugger/');
    FDebuggerConfig.ConfigStore := FDbgConfigStore;
  end;
  Result:=FXMLCfg;
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
begin
  if FTestBuildDirectory=AValue then exit;
  FTestBuildDirectory:=AppendPathDelim(TrimFilename(AValue));
end;

procedure TEnvironmentOptions.SetLazarusDirectory(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=AppendPathDelim(TrimFilename(AValue));
  if FLazarusDirectory=NewValue then exit;
  FLazarusDirectory:=NewValue;
end;

procedure TEnvironmentOptions.SetFPCSourceDirectory(const AValue: string);
begin
  if FFPCSourceDirectory=AValue then exit;
  FFPCSourceDirectory:=AppendPathDelim(TrimFilename(AValue));
  FFPCSrcDirParsedStamp:=InvalidParseStamp;
end;

procedure TEnvironmentOptions.SetCompilerFilename(const AValue: string);
begin
  if FCompilerFilename=AValue then exit;
  FCompilerFilename:=TrimFilename(AValue);
  FCompilerFilenameParsedStamp:=InvalidParseStamp;
end;

function TEnvironmentOptions.GetDebuggerEventLogColors(AIndex: TDBGEventType): TDebuggerEventLogColor;
begin
  Result := FDebuggerEventLogColors[AIndex];
end;

procedure TEnvironmentOptions.SetDebuggerEventLogColors(AIndex: TDBGEventType; const AValue: TDebuggerEventLogColor);
begin
  FDebuggerEventLogColors[AIndex] := AValue;
end;

procedure TEnvironmentOptions.SetDebuggerSearchPath(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimSearchPath(AValue,'');
  if FDebuggerSearchPath=NewValue then exit;
  FDebuggerSearchPath:=NewValue;
end;

procedure TEnvironmentOptions.SetMakeFilename(const AValue: string);
begin
  if FMakeFilename=AValue then exit;
  FMakeFilename:=TrimFilename(AValue);
end;

procedure TEnvironmentOptions.SetDebuggerFilename(const AValue: string);
var
  SpacePos: Integer;
begin
  if FDebuggerFilename=AValue then exit;
  FDebuggerFilename:=AValue;
  // trim the filename and keep the options after the space (if any)
  // TODO: split/find filename with spaces
  SpacePos:=1;
  while (SpacePos<=length(FDebuggerFilename))
  and (FDebuggerFilename[SpacePos]<>' ') do
    inc(SpacePos);
  FDebuggerFilename:=Trim(copy(FDebuggerFilename,1,SpacePos-1))+
    copy(FDebuggerFilename,SpacePos,length(FDebuggerFilename)-SpacePos+1);
end;

initialization
  RegisterIDEOptionsGroup(GroupEnvironment, TEnvironmentOptions);
end.

