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
  Classes, SysUtils, TypInfo, Graphics, Controls, Forms, LCLProc, FileProcs,
  Dialogs, LazConfigStorage, Laz2_XMLCfg, LazUTF8,
  // IDEIntf
  ProjectIntf, ObjectInspector, IDEWindowIntf, IDEOptionsIntf,
  CompOptsIntf, ComponentReg, IDEExternToolIntf, IDEDialogs, MacroDefIntf,
  // IDE
  IDEProcs, LazarusIDEStrConsts, IDETranslations, LazConf,
  IDEOptionDefs, TransferMacros, ModeMatrixOpts, Debugger;

const
  EnvOptsVersion: integer = 108;
  // 107: added Lazarus version
  // 108 added LastCalledByLazarusFullPath

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

{$IFDEF EnableNewExtTools}
type
  TBaseExternalToolMenuItems = class
  public
    function Load(Config: TConfigStorage; const Path: string): TModalResult; virtual; abstract;
    function Save(Config: TConfigStorage; const Path: string): TModalResult; virtual; abstract;
  end;
  TExternalToolMenuItemsClass = class of TBaseExternalToolMenuItems;
var
  ExternalToolMenuItemsClass: TExternalToolMenuItemsClass; // set by ExtToolDialog
{$ELSE}
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
{$ENDIF}

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
    eopDebuggerSearchPath
    );
  TEnvOptParseTypes = set of TEnvOptParseType;

type

  { TEnvironmentOptions - class for storing environment options }

  TEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    // config file
    FFilename: string;
    FFileAge: longint;
    FFileHasChangedOnDisk: boolean;
    FOldLazarusVersion: string;
    FXMLCfg: TRttiXMLConfig;
    FConfigStore: TXMLOptionsStorage;
    FDbgConfigStore: TXMLOptionsStorage; // for debugger

    // title
    FIDETitleStartsWithProject: boolean;
    FIDEProjectDirectoryInIdeTitle: boolean;

    // main buttons
    FIDESpeedButtonsVisible: boolean;
    FShowButtonGlyphs: TApplicationShowGlyphs;
    FShowMenuGlyphs: TApplicationShowGlyphs;

    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    FLastSavedProjectFile: string;
    FAskSaveSessionOnly: boolean;

    // window layout
    FIDEDialogLayoutList: TIDEDialogLayoutList;
    FSingleTaskBarButton: boolean;
    FHideIDEOnRun: boolean;
    FHideMessagesIcons: boolean;
    FComponentPaletteVisible: boolean;
    // CompletionWindow
    FCompletionWindowWidth: Integer;
    FCompletionWindowHeight: Integer;

    // component palette
    FComponentPaletteOptions: TCompPaletteOptions;

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

    // project inspector
    FProjInspSortAlphabetically: boolean;
    FProjInspShowDirHierarchy: boolean;

    // package editor
    FPackageEditorSortAlphabetically: boolean;
    FPackageEditorShowDirHierarchy: boolean;

    // hints
    FCheckDiskChangesWithLoading: boolean;
    FShowHintsForComponentPalette: boolean;
    FShowHintsForMainSpeedButtons: boolean;
    
    // messages
    fMsgViewDblClickJumps: boolean;
    fMsgViewFocus: boolean;

    // compiler + debugger + lazarus files
    FParseValues: array[TEnvOptParseType] of TParseString;
    FLazarusDirHistory: TStringList;
    FCompilerFileHistory: TStringList;
    FFPCSourceDirHistory: TStringList;
    FMakeFileHistory: TStringList;
    FCompilerMessagesFileHistory: TStringList;
    FBuildMatrixOptions: TBuildMatrixOptions;
    FUseBuildModes: Boolean;
    FIsGlobalMode: TStrToBoolEvent;

    // Primary-config verification
    FLastCalledByLazarusFullPath: String;

   // TODO: store per debuggerclass options
    // Maybe these should go to a new TDebuggerOptions class
    FDebuggerResetAfterRun: boolean;
    FDebuggerConfig: TDebuggerConfigStore;
    FDebuggerFileHistory: TStringList; // per debugger class
    FDebuggerProperties: TStringList; // per debugger class
    FDebuggerShowStopMessage: Boolean;
    FShowCompileDialog: Boolean;       // show dialog during compile
    FAutoCloseCompileDialog: Boolean;  // auto close dialog after succesed compile
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
    {$IFDEF EnableNewExtTools}
    fExternalToolMenuItems: TBaseExternalToolMenuItems;
    {$ELSE}
    fExternalTools: TBaseExternalToolList;
    {$ENDIF}
    
    // naming conventions
    fPascalFileExtension: TPascalExtType;
    fCharcaseFileAction : TCharCaseFileAction;
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

    // Prevent repopulating Recent project files menu with example projects if it was already cleared up.
    FAlreadyPopulatedRecentFiles : Boolean;

    function GetCompilerFilename: string;
    function GetCompilerMessagesFilename: string;
    function GetDebuggerEventLogColors(AIndex: TDBGEventType): TDebuggerEventLogColor;
    function GetDebuggerFilename: string;
    function GetDebuggerSearchPath: string;
    function GetFPCSourceDirectory: string;
    function GetFPDocPaths: string;
    function GetLazarusDirectory: string;
    function GetMakeFilename: string;
    function GetTestBuildDirectory: string;
    procedure SetCompilerFilename(const AValue: string);
    procedure SetCompilerMessagesFilename(AValue: string);
    procedure SetDebuggerEventLogColors(AIndex: TDBGEventType;
      const AValue: TDebuggerEventLogColor);
    procedure SetDebuggerSearchPath(const AValue: string);
    procedure SetFPDocPaths(const AValue: string);
    procedure SetMakeFilename(const AValue: string);
    procedure SetDebuggerFilename(AValue: string);
    procedure SetFPCSourceDirectory(const AValue: string);
    procedure SetLazarusDirectory(const AValue: string);
    procedure SetParseValue(o: TEnvOptParseType; const NewValue: string);

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
    procedure Load(OnlyDesktop: boolean);
    procedure Save(OnlyDesktop: boolean);
    property IsGlobalMode: TStrToBoolEvent read FIsGlobalMode write FIsGlobalMode;
    property Filename: string read FFilename write SetFilename;
    function GetDefaultConfigFilename: string;
    procedure CreateConfig;
    property OldLazarusVersion: string read FOldLazarusVersion;

    function GetParsedLazarusDirectory: string;
    function GetParsedTestBuildDirectory: string;
    function GetParsedCompilerFilename: string;
    function GetParsedFPCSourceDirectory(FPCVer: string = ''): string;
    function GetParsedMakeFilename: string;
    function GetParsedCompilerMessagesFilename: string;
    function GetParsedFPDocPaths: string;
    function GetParsedDebuggerFilename: string;
    function GetParsedDebuggerSearchPath: string;
    function GetParsedValue(o: TEnvOptParseType): string;

    // macros
    procedure InitMacros(AMacroList: TTransferMacroList);
    function MacroFuncCompPath(const {%H-}s:string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncFPCSrcDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                                var {%H-}Abort: boolean): string;
    function MacroFuncLazarusDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncExeExt(const {%H-}s:string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncLanguageID(const {%H-}s:string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    function MacroFuncLanguageName(const {%H-}s:string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    function MacroFuncTestDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    function MacroFuncConfDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;

    // auto save
    property AskSaveSessionOnly: boolean read FAskSaveSessionOnly write FAskSaveSessionOnly; // ask even if only project session needs saving
    property AutoSaveEditorFiles: boolean read FAutoSaveEditorFiles write FAutoSaveEditorFiles;
    property AutoSaveProject: boolean read FAutoSaveProject write FAutoSaveProject;
    property AutoSaveIntervalInSecs: integer read FAutoSaveIntervalInSecs write FAutoSaveIntervalInSecs;
       
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

    // component palette
    property ComponentPaletteOptions: TCompPaletteOptions read FComponentPaletteOptions
                                                         write FComponentPaletteOptions;

    // form editor
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
    property ShowComponentCaptions: boolean  read FShowComponentCaptions
                                            write FShowComponentCaptions;
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
    property RubberbandSelectsGrandChilds: boolean read FRubberbandSelectsGrandChilds
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
    property LazarusDirectory: string read GetLazarusDirectory
                                      write SetLazarusDirectory;
    property LazarusDirHistory: TStringList read FLazarusDirHistory
                                            write FLazarusDirHistory;
    property CompilerFilename: string read GetCompilerFilename
                                      write SetCompilerFilename;
    property CompilerFileHistory: TStringList read FCompilerFileHistory
                                              write FCompilerFileHistory;
    property FPCSourceDirectory: string read GetFPCSourceDirectory
                                        write SetFPCSourceDirectory;
    property FPCSourceDirHistory: TStringList read FFPCSourceDirHistory
                                              write FFPCSourceDirHistory;
    property MakeFilename: string read GetMakeFilename
                                      write SetMakeFilename;
    property MakeFileHistory: TStringList read FMakeFileHistory
                                              write FMakeFileHistory;
    property DebuggerFilename: string read GetDebuggerFilename
                                      write SetDebuggerFilename;
    property DebuggerFileHistory: TStringList read FDebuggerFileHistory
                                              write FDebuggerFileHistory;
    property DebuggerSearchPath: string read GetDebuggerSearchPath
                                      write SetDebuggerSearchPath;
    property DebuggerShowStopMessage: boolean read FDebuggerShowStopMessage
                                              write FDebuggerShowStopMessage;
    property DebuggerResetAfterRun: boolean read FDebuggerResetAfterRun
                                              write FDebuggerResetAfterRun;
    property ShowCompileDialog: boolean read  FShowCompileDialog
                                        write FShowCompileDialog;
    property AutoCloseCompileDialog: boolean read  FAutoCloseCompileDialog
                                             write FAutoCloseCompileDialog;
    property TestBuildDirectory: string read GetTestBuildDirectory
                                        write SetTestBuildDirectory;
    property TestBuildDirHistory: TStringList read FTestBuildDirHistory
                                              write FTestBuildDirHistory;
    property CompilerMessagesFilename: string read GetCompilerMessagesFilename
                                              write SetCompilerMessagesFilename;
    property CompilerMessagesFileHistory: TStringList read FCompilerMessagesFileHistory
                                                     write FCompilerMessagesFileHistory;
    // Primary-config verification
    property LastCalledByLazarusFullPath: String read FLastCalledByLazarusFullPath write FLastCalledByLazarusFullPath;

    // global build options
    property BuildMatrixOptions: TBuildMatrixOptions read FBuildMatrixOptions;
    property UseBuildModes: Boolean read FUseBuildModes write FUseBuildModes;

    // Debugger
    procedure SaveDebuggerPropertiesList;
    procedure SaveDebuggerProperties(DebuggerClass: String; Properties: TDebuggerProperties);
    procedure LoadDebuggerProperties(DebuggerClass: String; Properties: TDebuggerProperties);
    property  DebuggerConfig: TDebuggerConfigStore read FDebuggerConfig;

    // Debugger event log
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
                     write FLastSavedProjectFile; { if empty then create new project,
                                                    if '-' then do not load/create any project }
    property OpenLastProjectAtStart: boolean read FOpenLastProjectAtStart
                                             write FOpenLastProjectAtStart;
    property FileDialogFilter: string read FFileDialogFilter write FFileDialogFilter;

    // backup
    property BackupInfoProjectFiles: TBackupInfo read FBackupInfoProjectFiles
                                                 write FBackupInfoProjectFiles;
    property BackupInfoOtherFiles: TBackupInfo read FBackupInfoOtherFiles
                                               write FBackupInfoOtherFiles;
       
    // external tools
    {$IFDEF EnableNewExtTools}
    property ExternalToolMenuItems: TBaseExternalToolMenuItems read fExternalToolMenuItems;
    {$ELSE}
    property ExternalTools: TBaseExternalToolList read fExternalTools
                                                  write fExternalTools;
    {$ENDIF}

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
  OverrideFPCVer: string = '';
  EnvironmentOptions: TEnvironmentOptions = nil;

function PascalExtToType(const Ext: string): TPascalExtType;
function AmbiguousFileActionNameToType(const Action: string): TAmbiguousFileAction;
function CharCaseFileActionNameToType(const Action: string): TCharCaseFileAction;
function UnitRenameReferencesActionNameToType(const Action: string): TUnitRenameReferencesAction;

function CheckExecutable(const OldFilename, NewFilename: string;
  const ErrorCaption, ErrorMsg: string; SearchInPath: boolean = false): boolean;
function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; out StopChecking: boolean): boolean;

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
    'DebugPath' // eopDebuggerSearchPath
  );

function dbgs(o: TEnvOptParseType): string; overload;

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

function CheckExecutable(const OldFilename,
  NewFilename: string; const ErrorCaption, ErrorMsg: string;
  SearchInPath: boolean): boolean;
var
  Filename: String;
begin
  Result:=true;
  if OldFilename=NewFilename then exit;
  Filename:=NewFilename;
  if (not FilenameIsAbsolute(NewFilename)) and SearchInPath then begin
    Filename:=FindDefaultExecutablePath(NewFilename);
    if Filename='' then
      Filename:=NewFilename;
  end;

  if (not FileIsExecutable(Filename)) then begin
    if IDEMessageDialog(ErrorCaption,Format(ErrorMsg,[Filename]),
      mtWarning,[mbIgnore,mbCancel])=mrCancel
    then begin
      Result:=false;
    end;
  end;
end;

function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
begin
  if not DirPathExists(Dir) then begin
    Result:=IDEMessageDialog(ErrorCaption,Format(ErrorMsg,[Dir]),mtWarning,
                       [mbIgnore,mbCancel]);
  end else
    Result:=mrOk;
end;

function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; out StopChecking: boolean): boolean;
var
  SubResult: TModalResult;
begin
  StopChecking:=true;
  if OldDir=NewDir then begin
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

function dbgs(o: TEnvOptParseType): string;
begin
  Result:=EnvOptParseTypeNames[o];
end;

{ TEnvironmentOptions }

constructor TEnvironmentOptions.Create;
var
  o: TEnvOptParseType;
begin
  inherited Create;
  for o:=low(FParseValues) to high(FParseValues) do
    FParseValues[o].ParseStamp:=CTInvalidChangeStamp;

  FFilename:='';

  // language
  LanguageID:='';

  // auto save
  FAskSaveSessionOnly:=false;
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

  // component palette
  FComponentPaletteOptions:=TCompPaletteOptions.Create;

  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;

  // project inspector
  FProjInspSortAlphabetically:=false;
  FProjInspShowDirHierarchy:=false;

  // package editor
  FPackageEditorSortAlphabetically:=false;
  FPackageEditorShowDirHierarchy:=false;

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
  LazarusDirectory:='';
  FLazarusDirHistory:=TStringList.Create;
  CompilerFilename:='';
  FCompilerFileHistory:=TStringList.Create;
  FPCSourceDirectory:='';
  FFPCSourceDirHistory:=TStringList.Create;
  MakeFilename:=DefaultMakefilename;
  FMakeFileHistory:=TStringList.Create;
  DebuggerFilename:='';
  FDebuggerFileHistory:=TStringList.Create;
  FDebuggerProperties := TStringList.Create;
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
  {$IFDEF EnableNewExtTools}
  fExternalToolMenuItems:=ExternalToolMenuItemsClass.Create;
  {$ELSE}
  fExternalTools:=ExternalToolListClass.Create;
  {$ENDIF}
  
  // naming
  fPascalFileExtension:=petPAS;
  fCharcaseFileAction:=ccfaAutoRename;
  FUnitRenameReferencesAction:=urraAsk;
  FAskForFilenameOnNewFile:=false;
  FLowercaseDefaultFilename:=true;

  //debug
  (* TODO: maybe revert relations. Create this in Debugger, and call environmentoptions for the configstore only? *)
  FDebuggerConfig := TDebuggerConfigStore.Create;

  // global build options
  FBuildMatrixOptions:=TBuildMatrixOptions.Create;
end;

destructor TEnvironmentOptions.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FBuildMatrixOptions);
  {$IFDEF EnableNewExtTools}
  FreeAndNil(fExternalToolMenuItems);
  {$ELSE}
  FreeAndNil(fExternalTools);
  {$ENDIF}
  FreeAndNil(FRecentOpenFiles);
  FreeAndNil(FRecentProjectFiles);
  FreeAndNil(FRecentPackageFiles);
  FreeAndNil(FObjectInspectorOptions);
  FreeAndNil(FComponentPaletteOptions);
  FreeAndNil(FLazarusDirHistory);
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
      AddToRecentList(WholeFilePath,FRecentProjectFiles,FMaxRecentProjectFiles,rltFile);
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
begin
  Cfg:=nil;
  try
    XMLConfig:=GetXMLCfg(false);
    Cfg:=TXMLOptionsStorage.Create(XMLConfig);
    try
      Path:='EnvironmentOptions/';
      FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);
      FOldLazarusVersion:=XMLConfig.GetValue(Path+'Version/Lazarus','');
      if FOldLazarusVersion='' then begin
        // 1.1     r36507  106
        // 0.9.31  r28811  106
        // 0.9.29  r21344  106
        // 0.9.27  r16725  106
        // 0.9.25  r12751  106
        // 0.9.23  r10809  106
      end;

      // language
      LoadLanguage;

      // auto save
      FAskSaveSessionOnly:=XMLConfig.GetValue(
         Path+'AutoSave/AskSaveSessionOnly',false);
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
      FIDEDialogLayoutList.LoadFromConfig(FConfigStore, Path+'Desktop/Dialogs/');
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

      // form editor
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
        LazarusDirectory:=XMLConfig.GetValue(Path+'LazarusDirectory/Value',LazarusDirectory);
        LoadRecentList(XMLConfig,FLazarusDirHistory,
           Path+'LazarusDirectory/History/',rltFile);
        if FLazarusDirHistory.Count=0 then
          FLazarusDirHistory.Add(ProgramDirectory(true));
        CompilerFilename:=TrimFilename(XMLConfig.GetValue(
                              Path+'CompilerFilename/Value',CompilerFilename));
        LoadRecentList(XMLConfig,FCompilerFileHistory,
           Path+'CompilerFilename/History/',rltFile);
        if FCompilerFileHistory.Count=0 then
          GetDefaultCompilerFilenames(FCompilerFileHistory);
        FPCSourceDirectory:=XMLConfig.GetValue(
           Path+'FPCSourceDirectory/Value',FPCSourceDirectory);
        LoadRecentList(XMLConfig,FFPCSourceDirHistory,
           Path+'FPCSourceDirectory/History/',rltFile);
        MakeFilename:=TrimFilename(XMLConfig.GetValue(
           Path+'MakeFilename/Value',MakeFilename));
        LoadRecentList(XMLConfig,FMakeFileHistory,
           Path+'MakeFilename/History/',rltFile);
        if FMakeFileHistory.Count=0 then
          GetDefaultMakeFilenames(FMakeFileHistory);

        TestBuildDirectory:=XMLConfig.GetValue(
           Path+'TestBuildDirectory/Value',TestBuildDirectory);
        LoadRecentList(XMLConfig,FTestBuildDirHistory,
           Path+'TestBuildDirectory/History/',rltFile);
        if FTestBuildDirHistory.Count=0 then
          GetDefaultTestBuildDirs(FTestBuildDirHistory);
        CompilerMessagesFilename:=XMLConfig.GetValue(
           Path+'CompilerMessagesFilename/Value',CompilerMessagesFilename);
        LoadRecentList(XMLConfig, FCompilerMessagesFileHistory,
           Path+'CompilerMessagesFilename/History/',rltFile);

        // Primary-config verification
        FLastCalledByLazarusFullPath:=XMLConfig.GetValue(
           Path+'LastCalledByLazarusFullPath/Value','');

        // global buid options
        Cfg.AppendBasePath('BuildMatrix');
        FBuildMatrixOptions.LoadFromConfig(Cfg);
        Cfg.UndoAppendBasePath;
        FUseBuildModes:=XMLConfig.GetValue(Path+'Build/UseBuildModes',false);

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
           Path+'DebuggerFilename/History/',rltFile);
        DebuggerSearchPath:=XMLConfig.GetValue(
           Path+'DebuggerSearchPath/Value','');
        // Debugger General Options
        DebuggerShowStopMessage:=XMLConfig.GetValue(
           Path+'DebuggerOptions/ShowStopMessage/Value', True);
        DebuggerResetAfterRun :=XMLConfig.GetValue(
           Path+'DebuggerOptions/DebuggerResetAfterRun/Value', False);
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

      // project inspector
      FProjInspSortAlphabetically:=XMLConfig.GetValue(
        Path+'ProjInspSortAlphabetically/Value',false);
      FProjInspShowDirHierarchy:=XMLConfig.GetValue(
        Path+'ProjInspShowDirHierarchy/Value',false);

      // package editor
      FPackageEditorSortAlphabetically:=XMLConfig.GetValue(
        Path+'PackageEditorSortAlphabetically/Value',false);
      FPackageEditorShowDirHierarchy:=XMLConfig.GetValue(
        Path+'PackageEditorShowDirHierarchy/Value',false);

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
        Path+'Recent/OpenFiles/',rltFile);
      FMaxRecentProjectFiles:=XMLConfig.GetValue(
        Path+'Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
      LoadRecentList(XMLConfig,FRecentProjectFiles,
        Path+'Recent/ProjectFiles/',rltFile);
      FMaxRecentPackageFiles:=XMLConfig.GetValue(
        Path+'Recent/PackageFiles/Max',FMaxRecentOpenFiles);
      LoadRecentList(XMLConfig,FRecentPackageFiles,
        Path+'Recent/PackageFiles/',rltFile);

      FAlreadyPopulatedRecentFiles := XMLConfig.GetValue(Path+'Recent/AlreadyPopulated', false);

      // Add example projects to an empty project list if examples have write access
      if (FRecentProjectFiles.count=0) and (not FAlreadyPopulatedRecentFiles) then begin
        AddRecentProjectInitial('examples/jpeg/',          'jpegexample.lpi');
        AddRecentProjectInitial('examples/sprites/',       'spriteexample.lpi');
        AddRecentProjectInitial('examples/openglcontrol/', 'openglcontrol_demo.lpi');
        AddRecentProjectInitial('examples/barchart/',      'chartdemo.lpi');
        AddRecentProjectInitial('examples/',               'hello.lpi');
        FAlreadyPopulatedRecentFiles := True;
      end;

      // external tools
      {$IFDEF EnableNewExtTools}
      fExternalToolMenuItems.Load(FConfigStore,Path+'ExternalTools/');
      {$ELSE}
      fExternalTools.Load(FConfigStore,Path+'ExternalTools/');
      {$ENDIF}

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

      // component palette
      FComponentPaletteOptions.Load;

      // fpdoc
      FPDocPaths := XMLConfig.GetValue(Path+'LazDoc/Paths','');
      if FileVersion<=105 then
        FPDocPaths:=LineBreaksToDelimiter(FPDocPaths,';');

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
      XMLConfig.SetValue(Path+'Version/Lazarus',LazarusVersionStr);

      // language
      XMLConfig.SetDeleteValue(Path+'Language/ID',LanguageID,'');

      // auto save
      XMLConfig.SetDeleteValue(Path+'AutoSave/AskSaveSessionOnly'
         ,FAskSaveSessionOnly,false);
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

      // form editor
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
        CurLazDir:=ChompPathDelim(LazarusDirectory);
        if not GlobalMacroList.StrHasMacros(CurLazDir) then begin
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
          XMLConfig.SetValue(Path+'LazarusDirectory/Value',CurLazDir); // always store, no SetDeleteValue
        end;
        SaveRecentList(XMLConfig,FLazarusDirHistory,
           Path+'LazarusDirectory/History/');
        XMLConfig.SetDeleteValue(
           Path+'CompilerFilename/Value',CompilerFilename,'');
        SaveRecentList(XMLConfig,FCompilerFileHistory,
           Path+'CompilerFilename/History/');
        XMLConfig.SetDeleteValue(Path+'FPCSourceDirectory/Value',FPCSourceDirectory,'');
        SaveRecentList(XMLConfig,FFPCSourceDirHistory,
           Path+'FPCSourceDirectory/History/');
        XMLConfig.SetDeleteValue(Path+'MakeFilename/Value',MakeFilename,DefaultMakefilename);
        SaveRecentList(XMLConfig,FMakeFileHistory,
           Path+'MakeFilename/History/');
        XMLConfig.SetDeleteValue(Path+'TestBuildDirectory/Value',TestBuildDirectory,'');
        SaveRecentList(XMLConfig,FTestBuildDirHistory,
           Path+'TestBuildDirectory/History/');
        XMLConfig.SetDeleteValue(
           Path+'CompilerMessagesFilename/Value',CompilerMessagesFilename,'');
        SaveRecentList(XMLConfig,FCompilerMessagesFileHistory,
           Path+'CompilerMessagesFilename/History/');

        // Primary-conyfig vurification
        XMLConfig.SetDeleteValue(
           Path+'LastCalledByLazarusFullPath/Value',FLastCalledByLazarusFullPath,'');

        // global buid options
        Cfg.AppendBasePath('BuildMatrix');
        FBuildMatrixOptions.SaveToConfig(Cfg,IsGlobalMode);
        Cfg.UndoAppendBasePath;
        XMLConfig.SetDeleteValue(Path+'Build/UseBuildModes',FUseBuildModes,false);

        // backup
        SaveBackupInfo(FBackupInfoProjectFiles
          ,Path+'BackupProjectFiles/');
        SaveBackupInfo(FBackupInfoOtherFiles
          ,Path+'BackupOtherFiles/');

        // debugger
        FDebuggerConfig.Save;
        SaveDebuggerPropertiesList;
        XMLConfig.SetDeleteValue(Path+'DebuggerFilename/Value',
            DebuggerFilename,'');
        XMLConfig.SetDeleteValue(Path+'DebuggerOptions/ShowStopMessage/Value',
            FDebuggerShowStopMessage, True);
        XMLConfig.SetDeleteValue(Path+'DebuggerOptions/DebuggerResetAfterRun/Value',
            FDebuggerResetAfterRun, False);
        SaveRecentList(XMLConfig,FDebuggerFileHistory,
           Path+'DebuggerFilename/History/');
        XMLConfig.SetDeleteValue(Path+'DebuggerSearchPath/Value',
            DebuggerSearchPath,'');
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

      // project inspector
      XMLConfig.SetDeleteValue(Path+'ProjInspSortAlphabetically/Value',
        FProjInspSortAlphabetically,false);
      XMLConfig.SetDeleteValue(Path+'ProjInspShowDirHierarchy/Value',
        FProjInspShowDirHierarchy,false);

      // package editor
      XMLConfig.SetDeleteValue(Path+'PackageEditorSortAlphabetically/Value',
        FPackageEditorSortAlphabetically,false);
      XMLConfig.SetDeleteValue(Path+'PackageEditorShowDirHierarchy/Value',
        FPackageEditorShowDirHierarchy,false);

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

      XMLConfig.SetValue(Path+'Recent/AlreadyPopulated', FAlreadyPopulatedRecentFiles);

      // external tools
      {$IFDEF EnableNewExtTools}
      fExternalToolMenuItems.Save(FConfigStore,Path+'ExternalTools/');
      {$ELSE}
      fExternalTools.Save(FConfigStore,Path+'ExternalTools/');
      {$ENDIF}

      // naming
      XMLConfig.SetDeleteValue(Path+'Naming/PascalFileExtension',
                               PascalExtension[fPascalFileExtension],'.pas');
      XMLConfig.SetDeleteValue(Path+'CharcaseFileAction/Value',
                               CharCaseFileActionNames[fCharcaseFileAction],
                               CharCaseFileActionNames[ccfaAutoRename]);
      XMLConfig.SetDeleteValue(Path+'AmbiguousFileAction/Value',
        AmbiguousFileActionNames[fAmbiguousFileAction],
        AmbiguousFileActionNames[afaAsk]);
      XMLConfig.SetDeleteValue(Path+'AskForFilenameOnNewFile/Value',
                     FAskForFilenameOnNewFile,false);
      XMLConfig.SetDeleteValue(Path+'LowercaseDefaultFilename/Value',
                               FLowercaseDefaultFilename,true);

      // component palette
      FComponentPaletteOptions.Save;

      // fpdoc
      XMLConfig.SetDeleteValue(Path+'LazDoc/Paths',FPDocPaths,'');

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

function TEnvironmentOptions.GetDefaultConfigFilename: string;
begin
  Result:=TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+EnvOptsConfFileName);
end;

procedure TEnvironmentOptions.AddToRecentOpenFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentOpenFiles,FMaxRecentOpenFiles,rltFile);
end;

procedure TEnvironmentOptions.RemoveFromRecentOpenFiles(const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentOpenFiles,rltFile);
end;

procedure TEnvironmentOptions.AddToRecentProjectFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentProjectFiles,FMaxRecentProjectFiles,rltFile);
  {$ifdef Windows}
  SHAddToRecentDocs(SHARD_PATHW, PWideChar(UTF8ToUTF16(AFileName)));
  {$endif}
end;

procedure TEnvironmentOptions.RemoveFromRecentProjectFiles(
  const AFilename: string);
begin
  RemoveFromRecentList(AFilename,FRecentProjectFiles,rltFile);
end;

procedure InitLayoutHelper(const FormID: string);
begin
  with IDEWindowCreators.SimpleLayoutStorage do
    if not Assigned(ItemByFormID(FormID)) then
      CreateWindowLayout(FormID);
end;

procedure TEnvironmentOptions.InitLayoutList;
var
  l: TNonModalIDEWindow;
begin
  for l:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if l<>nmiwNone then
      InitLayoutHelper(NonModalIDEWindowNames[l]);
  InitLayoutHelper(DefaultObjectInspectorName);
end;

function TEnvironmentOptions.GetParsedTestBuildDirectory: string;
begin
  Result:=GetParsedValue(eopTestBuildDirectory);
end;

function TEnvironmentOptions.GetParsedFPCSourceDirectory(FPCVer: string
  ): string;
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
        debugln(['TEnvironmentOptions.GetParsedFPCSourceDirectory FPCVer=',FPCVer,' FPCSrcDir=',Result]);
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

function TEnvironmentOptions.GetParsedDebuggerFilename: string;
begin
  Result:=GetParsedValue(eopDebuggerFilename);
end;

function TEnvironmentOptions.GetParsedDebuggerSearchPath: string;
begin
  Result:=GetParsedValue(eopDebuggerSearchPath);
end;

function TEnvironmentOptions.GetParsedValue(o: TEnvOptParseType): string;
var
  SpacePos: SizeInt;
  CurParams: String;
begin
  with FParseValues[o] do begin
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
          ParsedValue:=SetDirSeparators('$(FPCSrcDir)/compiler/msg/errore.msg');

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
              ParsedValue:=GetPrimaryConfigPath;
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
          ParsedValue:=TrimAndExpandFilename(ParsedValue,GetParsedLazarusDirectory);
        eopFPDocPaths,eopDebuggerSearchPath:
          // search path
          ParsedValue:=TrimSearchPath(ParsedValue,GetParsedLazarusDirectory,true);
        eopCompilerFilename,eopMakeFilename,eopDebuggerFilename:
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
  Result:=GetParsedCompilerFilename;
end;

function TEnvironmentOptions.MacroFuncFPCSrcDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetParsedFPCSourceDirectory;
end;

function TEnvironmentOptions.MacroFuncLazarusDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetParsedLazarusDirectory;
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
  Result:=GetParsedTestBuildDirectory;
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
    ComponentPaletteOptions.ConfigStore:=FConfigStore;
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

function TEnvironmentOptions.GetDebuggerEventLogColors(AIndex: TDBGEventType): TDebuggerEventLogColor;
begin
  Result := FDebuggerEventLogColors[AIndex];
end;

function TEnvironmentOptions.GetDebuggerFilename: string;
begin
  Result:=FParseValues[eopDebuggerFilename].UnparsedValue;
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

function TEnvironmentOptions.GetMakeFilename: string;
begin
  Result:=FParseValues[eopMakeFilename].UnparsedValue;
end;

function TEnvironmentOptions.GetTestBuildDirectory: string;
begin
  Result:=FParseValues[eopTestBuildDirectory].UnparsedValue;
end;

procedure TEnvironmentOptions.SetDebuggerEventLogColors(AIndex: TDBGEventType; const AValue: TDebuggerEventLogColor);
begin
  FDebuggerEventLogColors[AIndex] := AValue;
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

procedure TEnvironmentOptions.SetDebuggerFilename(AValue: string);
begin
  SetParseValue(eopDebuggerFilename,UTF8Trim(AValue));
end;

initialization
  RegisterIDEOptionsGroup(GroupEnvironment, TEnvironmentOptions);
end.

