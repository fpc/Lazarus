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
    This unit defines a dialog for the lazarus environment options and a class to
    store the options in a xml file.

  ToDo: split this into two units - the dialog and the options.
}
unit EnvironmentOpts;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, FPCAdds, LCLProc, Forms, Controls, Buttons, GraphType,
  Graphics, ExtCtrls, StdCtrls, Spin, FileUtil, LResources, Dialogs,
  Laz_XMLCfg,
  ObjectInspector, IDEWindowIntf,
  LazarusIDEStrConsts, TransferMacros, LazConf, ExtToolDialog, IDEProcs,
  IDEOptionDefs, InputHistory, EditorOptions, IDETranslations;

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
  TDebuggerType = (dtNone, dtGnuDebugger, dtSSHGNUDebugger);

const
  DebuggerName: array[TDebuggerType] of string = (
    '(None)','GNU debugger (gdb)', 'GNU debugger through SSH (gdb)'
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
  
  TCharCaseFileAction = (
      ccfaAsk,
      ccfaAutoRename,
      ccfaIgnore
    );
  TCharCaseFileActions = set of TCharCaseFileAction;

const
  AmbiguousFileActionNames: array[TAmbiguousFileAction] of string = (
      'Ask',
      'AutoDelete',
      'AutoRename',
      'WarnOnCompile',
      'Ignore'
    );

  CharCaseFileActionNames: array[TCharCaseFileAction] of string = (
      'Ask',
      'AutoRename',
      'Ignore'
    );

  { Environment Options }

type
  { class for storing environment options }

  { TEnvironmentOptions }

  TEnvironmentOptions = class
  private
    FFilename: string;
    FFileAge: longint;
    FFileHasChangedOnDisk: boolean;
    FIDESpeedButtonsVisible: boolean;
    FXMLCfg: TXMLConfig;
    FConfigStore: TXMLOptionsStorage;

    FOnApplyWindowLayout: TOnApplyIDEWindowLayout;

    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    FLastSavedProjectFile: string;
    
    // window layout
    FIDEWindowLayoutList: TIDEWindowLayoutList;
    FIDEDialogLayoutList: TIDEDialogLayoutList;
    FMinimizeAllOnMinimizeMain: boolean;
    FHideIDEOnRun: boolean;
    FComponentPaletteVisible: boolean;
    FDebuggerSearchPath: string;

    // EnvironmentOptionsDialog editor
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

    // object inspector
    FObjectInspectorOptions: TOIOptions;
    
    // hints
    FCheckDiskChangesWithLoading: boolean;
    FShowHintsForComponentPalette: boolean;
    FShowHintsForMainSpeedButtons: boolean;
    
    // messages
    fMsgViewDblClickJumps: boolean;

    // compiler + debugger + lazarus files
    FLazarusDirectory: string;
    FLazarusDirsHistory: TStringList;
    FCompilerFilename: string;
    FCompilerFileHistory: TStringList;
    FFPCSourceDirectory: string;
    FFPCSourceDirHistory: TStringList;
    FMakeFileName: string;
    FMakeFileHistory: TStringList;
   // TODO: store per debuggerclass options
    // Maybe these should go to a new TDebuggerOptions class
    FDebuggerClass: string;
    FDebuggerFilename: string;         // per debugger class
    FDebuggerFileHistory: TStringList; // per debugger class
    FDebuggerShowStopMessage: Boolean;
    FTestBuildDirectory: string;
    FTestBuildDirHistory: TStringList;

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
    fExternalTools: TExternalToolList;
    
    // naming conventions
    fPascalFileExtension: TPascalExtType;
    fCharcaseFileAction : TCharCaseFileAction;
    fAmbiguousFileAction: TAmbiguousFileAction;
    
    FLazDocPaths: string;

    // language ID (see LazarusTranslations in translations.pas)
    fLanguageID: string;
    
    procedure SetCompilerFilename(const AValue: string);
    procedure SetDebuggerSearchPath(const AValue: string);
    procedure SetMakeFilename(const AValue: string);
    procedure SetDebuggerFilename(const AValue: string);
    procedure SetFPCSourceDirectory(const AValue: string);
    procedure SetLazarusDirectory(const AValue: string);
    procedure SetOnApplyWindowLayout(const AValue: TOnApplyIDEWindowLayout);

    procedure InitLayoutList;
    procedure InternOnApplyWindowLayout(ALayout: TIDEWindowLayout);
    procedure SetFileName(const NewFilename: string);
    function FileHasChangedOnDisk: boolean;
    function GetXMLCfg(CleanConfig: boolean): TXMLConfig;
    procedure FileUpdated;
    procedure SetTestBuildDirectory(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(OnlyDesktop:boolean);
    procedure Save(OnlyDesktop:boolean);
    property Filename: string read FFilename write SetFilename;
    procedure SetLazarusDefaultFilename;
    procedure GetDefaultFPCSourceDirectory;
    procedure CreateWindowLayout(const TheFormID: string);
    function IsDebuggerClassDefined: boolean;
    function GetTestBuildDirectory: string;

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

    // event
    property OnApplyWindowLayout: TOnApplyIDEWindowLayout
                         read FOnApplyWindowLayout write SetOnApplyWindowLayout;

    // auto save
    property AutoSaveEditorFiles: boolean read FAutoSaveEditorFiles
                                          write FAutoSaveEditorFiles;
    property AutoSaveProject: boolean read FAutoSaveProject
                                      write FAutoSaveProject;
    property AutoSaveIntervalInSecs: integer read FAutoSaveIntervalInSecs
                                             write FAutoSaveIntervalInSecs;
       
    // window layouts
    property IDEWindowLayoutList: TIDEWindowLayoutList
                           read FIDEWindowLayoutList write FIDEWindowLayoutList;
    property IDEDialogLayoutList: TIDEDialogLayoutList
                           read FIDEDialogLayoutList write FIDEDialogLayoutList;
    property MinimizeAllOnMinimizeMain: boolean read FMinimizeAllOnMinimizeMain
                                               write FMinimizeAllOnMinimizeMain;
    property HideIDEOnRun: boolean read FHideIDEOnRun write FHideIDEOnRun;
    property ComponentPaletteVisible: boolean read FComponentPaletteVisible
                                              write FComponentPaletteVisible;
    property IDESpeedButtonsVisible: boolean read FIDESpeedButtonsVisible
                                             write FIDESpeedButtonsVisible;

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
    property DebuggerClass: String read FDebuggerClass write FDebuggerClass;
    property DebuggerFilename: string read FDebuggerFilename
                                      write SetDebuggerFilename;
    property DebuggerFileHistory: TStringList read FDebuggerFileHistory
                                              write FDebuggerFileHistory;
    property DebuggerSearchPath: string read FDebuggerSearchPath
                                      write SetDebuggerSearchPath;
    property DebuggerShowStopMessage: boolean read FDebuggerShowStopMessage
                                              write FDebuggerShowStopMessage;
    property TestBuildDirectory: string read FTestBuildDirectory
                                        write SetTestBuildDirectory;
    property TestBuildDirHistory: TStringList read FTestBuildDirHistory
                                              write FTestBuildDirHistory;

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
    property ExternalTools: TExternalToolList read fExternalTools
                                              write fExternalTools;

    // naming conventions
    property PascalFileExtension: TPascalExtType read fPascalFileExtension
                                                 write fPascalFileExtension;
    property AmbiguousFileAction: TAmbiguousFileAction read fAmbiguousFileAction
                                                     write fAmbiguousFileAction;
    property CharcaseFileAction: TCharCaseFileAction read fCharcaseFileAction
                                                     write fCharcaseFileAction;

    // lazdoc
    property LazDocPaths: string read FLazDocPaths write FLazDocPaths;

    // language
    property LanguageID: string read fLanguageID write fLanguageID;
    
    // messages view
    property MsgViewDblClickJumps: boolean read fMsgViewDblClickJumps
                                           write fMsgViewDblClickJumps;
  end;

  //----------------------------------------------------------------------------

  TOnLoadEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;
  TOnSaveEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;

  { TEnvironmentOptionsDialog: EnvironmentOptionsDialog for environment options }
  
  TEnvOptsDialogPage = (eodpLanguage, eodpAutoSave, eodpDesktop, eodpMainHints,
    eodpWindowPositions, eodpFormEditor, eodpObjectInspector, eodpFiles,
    eodpBackup, eodpNaming);
  
  { TEnvironmentOptionsDialog }

  TEnvironmentOptionsDialog = class(TForm)
    NoteBook: TNoteBook;
    FilesPage: TPage;
    DesktopPage: TPage;
    WindowsPage: TPage;
    FormEditorPage: TPage;
    ObjectInspectorPage: TPage;
    BackupPage: TPage;
    NamingPage: TPage;
    LazDocPage: TPage;
    SelectDirectoryDialog: TSelectDirectoryDialog;

    //lazdoc settings
    LazDocBrowseButton: TButton;
    LazDocPathEdit: TEdit;
    LazDocDeletePathButton: TButton;
    LazDocAddPathButton: TButton;
    LazDocPathsGroupBox: TGroupBox;
    LazDocListBox: TListBox;

    // language
    LanguageGroupBox: TGroupBox;
    LanguageComboBox: TComboBox;
    AutoSaveGroupBox: TGroupBox;
    AutoSaveEditorFilesCheckBox: TCheckBox;
    AutoSaveProjectCheckBox: TCheckBox;
    AutoSaveIntervalInSecsLabel: TLabel;
    AutoSaveIntervalInSecsComboBox: TComboBox;
    
    // desktop files
    DesktopFilesGroupBox: TGroupBox;
    SaveDesktopSettingsToFileButton: TButton;
    LoadDesktopSettingsFromFileButton: TButton;

    // hints
    CheckDiskChangesWithLoadingCheckBox: TCheckBox;
    ShowHintsForComponentPaletteCheckBox: TCheckBox;
    ShowHintsForMainSpeedButtonsCheckBox: TCheckBox;
    
    // messages view
    MsgViewDblClickJumpsCheckBox: TCheckBox;

    // window layout
    WindowPositionsGroupBox: TGroupBox;
    WindowPositionsListBox: TListBox;
    WindowPositionsBox: TIDEWindowSetupLayoutComponent;
    MinimizeAllOnMinimizeMainCheckBox: TCheckBox;
    HideIDEOnRunCheckBox: TCheckBox;

    // EnvironmentOptionsDialog editor
    GridGroupBox: TGroupBox;
    ShowGridCheckBox: TCheckBox;
    ShowBorderSpaceCheckBox: TCheckBox;
    GridColorLabel: TLabel;
    GridColorButton: TColorButton;
    SnapToGridCheckBox: TCheckBox;
    GridSizeXLabel: TLabel;
    GridSizeXComboBox: TComboBox;
    GridSizeYLabel: TLabel;
    GridSizeYComboBox: TComboBox;
    GuideLinesGroupBox: TGroupBox;
    ShowGuideLinesCheckBox: TCheckBox;
    SnapToGuideLinesCheckBox: TCheckBox;
    GuideLineColorLeftTopLabel: TLabel;
    GuideLineColorLeftTopButton: TColorButton;
    GuideLineColorRightBottomLabel: TLabel;
    GuideLineColorRightBottomButton: TColorButton;
    FormEditMiscGroupBox: TGroupBox;
    ShowComponentCaptionsCheckBox: TCheckBox;
    ShowEditorHintsCheckBox: TCheckBox;
    AutoCreateFormsOnOpenCheckBox: TCheckBox;
    RightClickSelectsCheckBox: TCheckBox;
    GrabberColorLabel: TLabel;
    GrabberColorButton: TColorButton;
    MarkerColorLabel: TLabel;
    MarkerColorButton: TColorButton;
    RubberbandGroupBox: TGroupBox;
    RubberbandSelectColorLabel: TLabel;
    RubberbandSelectColorButton: TColorButton;
    RubberbandCreateColorLabel: TLabel;
    RubberbandCreateColorButton: TColorButton;
    RubberbandSelectsGrandChildsCheckBox: TCheckBox;
    DesignerPaintLazyCheckBox: TCheckBox;

    // object inspector
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIBackgroundColorLabel: TLabel;
    OIBackgroundColorButton: TColorButton;

    OISubPropsColorLabel: TLabel;
    OISubPropsColorButton: TColorButton;
    OIReferencesColorLabel: TLabel;
    OIReferencesColorButton: TColorButton;
    OIValueColorLabel: TLabel;
    OIValueColorButton: TColorButton;
    OIDefaultValueColorLabel: TLabel;
    OIDefaultValueColorButton: TColorButton;
    OIPropNameColorLabel: TLabel;
    OIPropNameColorButton: TColorButton;

    OIMiscGroupBox: TGroupBox;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIDefaultItemHeightLabel: TLabel;
    OIShowHintCheckBox: TCheckBox;

    // Files
    MaxRecentOpenFilesLabel: TLabel;
    MaxRecentOpenFilesComboBox: TComboBox;
    MaxRecentProjectFilesLabel: TLabel;
    MaxRecentProjectFilesComboBox: TComboBox;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    LazarusDirGroupBox: TGroupBox;
    LazarusDirComboBox: TComboBox;
    LazarusDirButton: TButton;
    CompilerPathGroupBox: TGroupBox;
    CompilerPathComboBox: TComboBox;
    CompilerPathButton: TButton;
    FPCSourceDirGroupBox: TGroupBox;
    FPCSourceDirComboBox: TComboBox;
    FPCSourceDirButton: TButton;
    MakePathGroupBox: TGroupBox;
    MakePathComboBox: TComboBox;
    MakePathButton: TButton;
    TestBuildDirGroupBox: TGroupBox;
    TestBuildDirComboBox: TComboBox;
    TestBuildDirButton: TButton;

    // backup
    BackupHelpLabel: TLabel;
    BackupProjectGroupBox: TGroupBox;
    BakProjTypeRadioGroup: TRadioGroup;
    BakProjAddExtLabel: TLabel;
    BakProjAddExtComboBox: TComboBox;
    BakProjMaxCounterLabel: TLabel;
    BakProjMaxCounterComboBox: TComboBox;
    BakProjSubDirLabel: TLabel;
    BakProjSubDirComboBox: TComboBox;
    BackupOtherGroupBox: TGroupBox;
    BakOtherTypeRadioGroup: TRadioGroup;
    BakOtherAddExtLabel: TLabel;
    BakOtherAddExtComboBox: TComboBox;
    BakOtherMaxCounterLabel: TLabel;
    BakOtherMaxCounterComboBox: TComboBox;
    BakOtherSubDirLabel: TLabel;
    BakOtherSubDirComboBox: TComboBox;
    
    // naming conventions
    PascalFileExtRadiogroup: TRadioGroup;
    CharCaseFileActionRadioGroup: TRadioGroup;
    AmbiguousFileActionRadioGroup: TRadioGroup;

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
    procedure BakTypeRadioGroupClick(Sender: TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure FormEditorPageResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocBrowseButtonClick(Sender: TObject);
    procedure LazDocDeletePathButtonClick(Sender: TObject);
    procedure NotebookChangeBounds(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveDesktopSettingsToFileButtonClick(Sender: TObject);
    procedure LoadDesktopSettingsFromFileButtonClick(Sender: TObject);
    procedure WindowPositionsListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FLayouts: TIDEWindowLayoutList;
    FOldLazarusDir: string;
    FOldCompilerFilename: string;
    FOldFPCSourceDir: string;
    FOldMakeFilename: string;
    FOldTestDir: string;

    procedure SetCategoryPage(const AValue: TEnvOptsDialogPage);
    procedure SetupFilesPage(Page: integer);
    procedure SetupDesktopPage(Page: integer);
    procedure SetupWindowsPage(Page: integer);
    procedure SetupFormEditorPage(Page: integer);
    procedure SetupObjectInspectorPage(Page: integer);
    procedure SetupBackupPage(Page: integer);
    procedure SetupNamingPage(Page: integer);
    procedure SetupLazDocPage(Page: integer);
    procedure SetWindowPositionsItem(Index: integer);
    function CheckValues: boolean;
    function CheckLazarusDir: boolean;
    function IsFPCSourceDir: boolean;
    function CheckTestDir: boolean;
  published
    property OnSaveEnvironmentSettings: TOnSaveEnvironmentSettings
      read FOnSaveEnvironmentSettings write FOnSaveEnvironmentSettings;
    property OnLoadEnvironmentSettings: TOnLoadEnvironmentSettings
      read FOnLoadEnvironmentSettings write FOnLoadEnvironmentSettings;
    property CategoryPage: TEnvOptsDialogPage write SetCategoryPage;
  public
    procedure ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
    procedure WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;


var
  EnvironmentOptions: TEnvironmentOptions;

function DebuggerNameToType(const s: string): TDebuggerType;
function PascalExtToType(const Ext: string): TPascalExtType;
function AmbiguousFileActionNameToType(const Action: string): TAmbiguousFileAction;
function CharCaseFileActionNameToType(const Action: string): TCharCaseFileAction;

function CheckFileChanged(const OldFilename, NewFilename: string): boolean;
function CheckExecutable(const OldFilename, NewFilename: string;
  const ErrorCaption, ErrorMsg: string): boolean;
function CheckDirPathExists(const Dir,
  ErrorCaption, ErrorMsg: string): TModalResult;
function SimpleDirectoryCheck(const OldDir, NewDir,
  NotFoundErrMsg: string; var StopChecking: boolean): boolean;

procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString);
procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString;
                          MaxCount: integer);

const
  DefaultLazDocPath = '$(LazarusDir)/docs/xml/lcl';
  
implementation


const MaxComboBoxCount: integer = 20;


function DebuggerNameToType(const s: string): TDebuggerType;
begin
  for Result:=Low(TDebuggerType) to High(TDebuggerType) do
    if CompareText(DebuggerName[Result],s)=0 then exit;
  Result:=dtNone;
end;

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
  for Result:=Low(TAmbiguousFileAction) to High(TAmbiguousFileAction) do begin
    if CompareText(AmbiguousFileActionNames[Result],Action)=0 then
      exit;
  end;
  Result:=afaAsk;
end;

function CharCaseFileActionNameToType(
  const Action: string): TCharCaseFileAction;
begin
  for Result:=Low(TCharCaseFileAction) to High(TCharCaseFileAction) do begin
    if CompareText(CharCaseFileActionNames[Result],Action)=0 then
      exit;
  end;
  Result:=ccfaAutoRename;
end;


function CheckFileChanged(const OldFilename,
  NewFilename: string): boolean;
begin
  Result:=(NewFilename<>OldFilename) and (NewFilename<>'');
end;

function CheckExecutable(const OldFilename,
  NewFilename: string; const ErrorCaption, ErrorMsg: string): boolean;
begin
  Result:=true;
  if not CheckFileChanged(OldFilename,NewFilename) then exit;
  if (not FileIsExecutable(NewFilename)) then begin
    if MessageDlg(ErrorCaption,Format(ErrorMsg,[NewFilename]),
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
  NotFoundErrMsg: string; var StopChecking: boolean): boolean;
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

procedure SetComboBoxText(AComboBox:TComboBox; const AText: String);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
  AComboBox.Text:=AText;
end;

procedure SetComboBoxText(AComboBox:TComboBox; const AText: String;
  MaxCount: integer);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Insert(0,AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
    if MaxCount<2 then MaxCount:=2;
    while AComboBox.Items.Count>MaxCount do
      AComboBox.Items.Delete(AComboBox.Items.Count-1);
  end;
  AComboBox.Text:=AText;
end;

{ TEnvironmentOptions }

const
  EnvOptsConfFileName='environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';

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
  FMinimizeAllOnMinimizeMain:=false;
  FHideIDEOnRun:=false;
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
  FRightClickSelects:=true;
  FGrabberColor:=clBlack;
  FMarkerColor:=clDkGray;
  FRubberbandSelectionColor:=clNavy;
  FRubberbandCreationColor:=clMaroon;
  FRubberbandSelectsGrandChilds:=true;
  FDesignerPaintLazy:=true;

  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;
  
  // hints
  FCheckDiskChangesWithLoading:=false;
  FShowHintsForComponentPalette:=true;
  FShowHintsForMainSpeedButtons:=true;
  
  // messages view
  fMsgViewDblClickJumps:=true;

  // files
  LazarusDirectory:=IDEProcs.ProgramDirectory;
  FLazarusDirsHistory:=TStringList.Create;
  CompilerFilename:='';
  FCompilerFileHistory:=TStringList.Create;
  FPCSourceDirectory:='';
  FFPCSourceDirHistory:=TStringList.Create;
  MakeFilename:='';
  FMakeFileHistory:=TStringList.Create;
  DebuggerFilename:='';
  FDebuggerFileHistory:=TStringList.Create;
  FDebuggerSearchPath:='';
  TestBuildDirectory:=GetDefaultTestBuildDirectory;
  FTestBuildDirHistory:=TStringList.Create;

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
  fExternalTools:=TExternalToolList.Create;
  
  // naming
  fPascalFileExtension:=petPAS;
  fCharcaseFileAction:=ccfaAutoRename;

  FLazDocPaths:=SetDirSeparators(DefaultLazDocPath);
end;

destructor TEnvironmentOptions.Destroy;
begin
  fExternalTools.Free;
  FRecentOpenFiles.Free;
  FRecentProjectFiles.Free;
  FRecentPackageFiles.Free;
  FObjectInspectorOptions.Free;
  FLazarusDirsHistory.Free;
  FCompilerFileHistory.Free;
  FFPCSourceDirHistory.Free;
  FMakeFileHistory.Free;
  FDebuggerFileHistory.Free;
  FTestBuildDirHistory.Free;
  if IDEWindowIntf.IDEDialogLayoutList=FIDEDialogLayoutList then
    IDEWindowIntf.IDEDialogLayoutList:=nil;
  FIDEDialogLayoutList.Free;
  fIDEWindowLayoutList.Free;
  FConfigStore.Free;
  FXMLCfg.Free;
  inherited Destroy;
end;

procedure TEnvironmentOptions.SetLazarusDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+EnvOptsConfFileName);
  CopySecondaryConfigFile(EnvOptsConfFileName);
  if (not FileExists(ConfFileName)) then begin
    DebugLn('Note: environment config file not found - using defaults');
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
var XMLConfig: TXMLConfig;
  FileVersion: integer;
  CurDebuggerClass: String;
  OldDebuggerType: TDebuggerType;
  Path: String;
  CurPath: String;

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

  procedure LoadDebuggerType(var ADebuggerType: TDebuggerType;
    const Path: string);
  begin
    ADebuggerType:=DebuggerNameToType(
                                   XMLConfig.GetValue(Path+'Debugger/Type',''));
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

begin
  try
    XMLConfig:=GetXMLCfg(false);
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

    // windows
    FIDEWindowLayoutList.LoadFromXMLConfig(XMLConfig,
      Path+'Desktop/');
    FIDEDialogLayoutList.LoadFromConfig(FConfigStore,
      Path+'Desktop/Dialogs/');
    FMinimizeAllOnMinimizeMain:=XMLConfig.GetValue(
      Path+'Desktop/MinimizeAllOnMinimizeMain/Value',true);
    FHideIDEOnRun:=XMLConfig.GetValue(
      Path+'Desktop/HideIDEOnRun/Value',false);
    FComponentPaletteVisible:=XMLConfig.GetValue(
      Path+'Desktop/ComponentPaletteVisible/Value',true);
    FIDESpeedButtonsVisible:=XMLConfig.GetValue(
      Path+'Desktop/IDESpeedButtonsVisible/Value',true);

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

    if not OnlyDesktop then begin
      // files
      LazarusDirectory:=XMLConfig.GetValue(
         Path+'LazarusDirectory/Value',FLazarusDirectory);
      LoadRecentList(XMLConfig,FLazarusDirsHistory,
         Path+'LazarusDirectory/History/');
      if FLazarusDirsHistory.Count=0 then begin
        FLazarusDirsHistory.Add(ProgramDirectory);
      end;
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

      // backup
      LoadBackupInfo(FBackupInfoProjectFiles
        ,Path+'BackupProjectFiles/');
      LoadBackupInfo(FBackupInfoOtherFiles
        ,Path+'BackupOtherFiles/');

      // Debugger
      // first try to load the old type
      // it will be overwritten by Class if found
      CurDebuggerClass := XMLConfig.GetValue(
         Path+'Debugger/Class','');
      if CurDebuggerClass='' then begin
        // try old format
        OldDebuggerType := DebuggerNameToType(XMLConfig.GetValue(
          Path+'Debugger/Type',''));
        if OldDebuggerType=dtGnuDebugger then
          CurDebuggerClass:='TGDBMIDEBUGGER';
      end;
      DebuggerClass:=CurDebuggerClass;
      DebuggerFilename:=XMLConfig.GetValue(
         Path+'DebuggerFilename/Value','');
      LoadRecentList(XMLConfig,FDebuggerFileHistory,
         Path+'DebuggerFilename/History/');
      DebuggerSearchPath:=XMLConfig.GetValue(
         Path+'DebuggerSearchPath/Value','');
      // Debugger General Options
      DebuggerShowStopMessage:=XMLConfig.GetValue(
         Path+'DebuggerOptions/ShowStopMessage/Value', True);
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

    // external tools
    fExternalTools.Load(FConfigStore,Path+'ExternalTools/');
    
    // naming
    LoadPascalFileExt(Path+'');

    //lazdoc
    FLazDocPaths := XMLConfig.GetValue(Path+'LazDoc/Paths', DefaultLazDocPath);
    if FileVersion<=105 then
      FLazDocPaths:=LineBreaksToDelimiter(FLazDocPaths,';');

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
        
    // object inspector
    FObjectInspectorOptions.Load;
    FObjectInspectorOptions.SaveBounds:=false;
    
    FileUpdated;
  except
    // ToDo
    on E: Exception do
      DebugLn('[TEnvironmentOptions.Load]  error reading "',FFilename,'": '+E.Message);
  end;
end;

procedure TEnvironmentOptions.Save(OnlyDesktop: boolean);
var XMLConfig: TXMLConfig;
  Path: String;

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

  procedure SaveDebuggerType(ADebuggerType: TDebuggerType; Path:string);
  begin
    XMLConfig.SetDeleteValue(Path+'Debugger/Type',DebuggerName[ADebuggerType],
                             DebuggerName[dtNone]);
  end;
  
begin
  try
    XMLConfig:=GetXMLCfg(true);
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
    FIDEWindowLayoutList.SaveToXMLConfig(XMLConfig,Path+'Desktop/');
    FIDEDialogLayoutList.SaveToConfig(FConfigStore,Path+'Desktop/Dialogs/');
    XMLConfig.SetDeleteValue(Path+'Desktop/MinimizeAllOnMinimizeMain/Value',
                             FMinimizeAllOnMinimizeMain,true);
    XMLConfig.SetDeleteValue(Path+'Desktop/HideIDEOnRun/Value',FHideIDEOnRun,
                             false);
    XMLConfig.SetDeleteValue(Path+'Desktop/ComponentPaletteVisible/Value',
                             FComponentPaletteVisible,true);
    XMLConfig.SetDeleteValue(Path+'Desktop/IDESpeedButtonsVisible/Value',
                             FIDESpeedButtonsVisible,true);

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

    if not OnlyDesktop then begin
      // files
      XMLConfig.SetDeleteValue(
         Path+'LazarusDirectory/Value',FLazarusDirectory,'');
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

      // backup
      SaveBackupInfo(FBackupInfoProjectFiles
        ,Path+'BackupProjectFiles/');
      SaveBackupInfo(FBackupInfoOtherFiles
        ,Path+'BackupOtherFiles/');
        
      // debugger
      XMLConfig.SetDeleteValue(Path+'Debugger/Class',
          FDebuggerClass,'');
      XMLConfig.SetDeleteValue(Path+'DebuggerFilename/Value',
          FDebuggerFilename,'');
      XMLConfig.SetDeleteValue(Path+'DebuggerOptions/ShowStopMessage/Value',
          FDebuggerShowStopMessage, True);
      SaveRecentList(XMLConfig,FDebuggerFileHistory,
         Path+'DebuggerFilename/History/');
      XMLConfig.SetDeleteValue(Path+'DebuggerSearchPath/Value',
          FDebuggerSearchPath,'');
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

    //lazdoc
    XMLConfig.SetDeleteValue(Path+'LazDoc/Paths',FLazDocPaths,DefaultLazDocPath);

    // object inspector
    FObjectInspectorOptions.SaveBounds:=false;
    FObjectInspectorOptions.Save;
    
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

procedure TEnvironmentOptions.RemoveFromRecentOpenFiles(const AFilename: string
  );
begin
  RemoveFromRecentList(AFilename,FRecentOpenFiles);
end;

procedure TEnvironmentOptions.AddToRecentProjectFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentProjectFiles,FMaxRecentProjectFiles);
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
  fIDEWindowLayoutList:=TIDEWindowLayoutList.Create;

  for l:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if l<>nmiwNone then
      CreateWindowLayout(NonModalIDEWindowNames[l]);
  CreateWindowLayout(DefaultObjectInspectorName);
end;

procedure TEnvironmentOptions.InternOnApplyWindowLayout(
  ALayout: TIDEWindowLayout);
begin
  if Assigned(OnApplyWindowLayout) then OnApplyWindowLayout(ALayout);
end;

procedure TEnvironmentOptions.CreateWindowLayout(const TheFormID: string);
var
  NewLayout: TIDEWindowLayout;
begin
  if TheFormID='' then
    RaiseException('TEnvironmentOptions.CreateWindowLayout TheFormID empty');
  if IDEWindowLayoutList.ItemByFormID(TheFormID)<>nil then
    RaiseException('TEnvironmentOptions.CreateWindowLayout TheFormID exists');
  NewLayout:=TIDEWindowLayout.Create;
  with NewLayout do begin
    FormID:=TheFormID;
    WindowPlacementsAllowed:=[iwpRestoreWindowGeometry,iwpDefault,
       iwpCustomPosition,iwpUseWindowManagerSetting
       {$IFDEF IDEDocking},iwpDocked{$ENDIF}];
    OnApply:=@Self.InternOnApplyWindowLayout;
    DefaultWindowPlacement:=iwpRestoreWindowGeometry;
  end;
  IDEWindowLayoutList.Add(NewLayout);
end;

function TEnvironmentOptions.IsDebuggerClassDefined: boolean;
begin
  Result := (FDebuggerClass <> '')
         and (CompareText(FDebuggerClass, DebuggerName[dtNone]) <> 0);
end;

function TEnvironmentOptions.GetTestBuildDirectory: string;
begin
  Result:=AppendPathDelim(TestBuildDirectory);
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
                 lisLazarusDirectory,@MacroFuncExeExt,[]));
  AMacroList.Add(TTransferMacro.Create('LanguageID','',
                 lisLazarusLanguageID,@MacroFuncLanguageID,[]));
  AMacroList.Add(TTransferMacro.Create('LanguageName','',
                 lisLazarusLanguageName,@MacroFuncLanguageName,[]));
  AMacroList.Add(TTransferMacro.Create('TestDir','',
                 lisTestDirectory,@MacroFuncTestDir,[]));
  AMacroList.Add(TTransferMacro.Create('ConfDir','',
                 lisProjectSrcPath,@MacroFuncConfDir,[]));
end;

function TEnvironmentOptions.MacroFuncCompPath(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=CompilerFilename;
end;

function TEnvironmentOptions.MacroFuncFPCSrcDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=FPCSourceDirectory;
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

function TEnvironmentOptions.FileHasChangedOnDisk: boolean;
begin
  Result:=FFileHasChangedOnDisk
      or ((FFilename<>'') and (FFileAge<>0) and (FileAge(FFilename)<>FFileAge));
  FFileHasChangedOnDisk:=Result;
end;

function TEnvironmentOptions.GetXMLCfg(CleanConfig: boolean): TXMLConfig;
begin
  if FileHasChangedOnDisk or (FXMLCfg=nil) then begin
    FConfigStore.Free;
    FXMLCfg.Free;
    InvalidateFileStateCache;
    if CleanConfig then
      FXMLCfg:=TXMLConfig.CreateClean(Filename)
    else
      FXMLCfg:=TXMLConfig.Create(Filename);
    FConfigStore:=TXMLOptionsStorage.Create(FXMLCfg);
    ObjectInspectorOptions.ConfigStore:=FConfigStore;
  end;
  Result:=FXMLCfg;
end;

procedure TEnvironmentOptions.FileUpdated;
begin
  FFileHasChangedOnDisk:=false;
  if FFilename<>'' then
    FFileAge:=FileAge(FFilename)
  else
    FFileAge:=0;
end;

procedure TEnvironmentOptions.SetTestBuildDirectory(const AValue: string);
begin
  if FTestBuildDirectory=AValue then exit;
  FTestBuildDirectory:=AppendPathDelim(TrimFilename(AValue));
end;

procedure TEnvironmentOptions.SetOnApplyWindowLayout(
  const AValue: TOnApplyIDEWindowLayout);
begin
  FOnApplyWindowLayout:=AValue;
end;

procedure TEnvironmentOptions.SetLazarusDirectory(const AValue: string);
begin
  if FLazarusDirectory=AValue then exit;
  FLazarusDirectory:=AppendPathDelim(TrimFilename(AValue));
end;

procedure TEnvironmentOptions.SetFPCSourceDirectory(const AValue: string);
begin
  if FFPCSourceDirectory=AValue then exit;
  FFPCSourceDirectory:=AppendPathDelim(TrimFilename(AValue));
end;

procedure TEnvironmentOptions.SetCompilerFilename(const AValue: string);
begin
  if FCompilerFilename=AValue then exit;
  FCompilerFilename:=TrimFilename(AValue);
end;

procedure TEnvironmentOptions.SetDebuggerSearchPath(const AValue: string);
begin
  if FDebuggerSearchPath=AValue then exit;
  FDebuggerSearchPath:=TrimSearchPath(AValue,'');
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

//==============================================================================

{ TEnvironmentOptionsDialog }

constructor TEnvironmentOptionsDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,Width,Height);
  Caption:=lisMenuGeneralOptions;

  NoteBook.PageIndex:=0;

  SetupFilesPage(0);
  SetupDesktopPage(1);
  SetupWindowsPage(2);
  SetupFormEditorPage(3);
  SetupObjectInspectorPage(4);
  SetupBackupPage(5);
  SetupNamingPage(6);
  SetupLazDocPage(7);

  CancelButton.Caption:=dlgCancel;
  CancelControl:=CancelButton;
end;

destructor TEnvironmentOptionsDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TEnvironmentOptionsDialog.SetupDesktopPage(Page: integer);
var
  i: Integer;
  LangID: String;
begin
  NoteBook.Page[Page].Caption := dlgDesktop;
  
  // language
  LanguageGroupBox.Caption:=dlgEnvLanguage;

  with LanguageComboBox.Items do begin
    BeginUpdate;
    for i:=0 to LazarusTranslations.Count-1 do begin
      LangID:=LazarusTranslations[i].ID;
      if LangID='' then
        //No [] if automatic
        Add(GetLazarusLanguageLocalizedName(LangID))
      else
        Add(GetLazarusLanguageLocalizedName(LangID)+' ['+LangID+']');
    end;
    EndUpdate;
  end;

  // auto save
  AutoSaveGroupBox.Caption:=dlgAutoSave;
  AutoSaveEditorFilesCheckBox.Caption:=dlgEdFiles;
  AutoSaveProjectCheckBox.Caption:=dlgEnvProject;
  AutoSaveIntervalInSecsLabel.Caption:=dlgIntvInSec;

  // desktop files
  DesktopFilesGroupBox.Caption:=dlgDesktopFiles;
  SaveDesktopSettingsToFileButton.Caption:=dlgSaveDFile;
  LoadDesktopSettingsFromFileButton.Caption:=dlgLoadDFile;

  // hints
  CheckDiskChangesWithLoadingCheckBox.Caption:=lisCheckChangesOnDiskWithLoading;
  ShowHintsForComponentPaletteCheckBox.Caption:=dlgPalHints;
  ShowHintsForMainSpeedButtonsCheckBox.Caption:=dlgSpBHints;

  // messages view
  MsgViewDblClickJumpsCheckBox.Caption:=lisEnvDoubleClickOnMessagesJumpsOtherwiseSingleClick;
end;

procedure TEnvironmentOptionsDialog.SetupWindowsPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgWindows;

  // windows
  MinimizeAllOnMinimizeMainCheckBox.Caption:=dlgMinimizeAllOnMinimizeMain;
  HideIDEOnRunCheckBox.Caption:=dlgHideIDEOnRun;

  // Window Positions
  WindowPositionsGroupBox.Caption:=dlgWinPos;
  with WindowPositionsListBox.Items do begin
    BeginUpdate;
    Add(dlgMainMenu);
    Add(dlgSrcEdit);
    Add(dlgMsgs);
    Add(dlgObjInsp);
    Add(lisMenuProjectInspector);
    Add(lisCodeExplorer);
    Add(lisMenuPackageGraph);
    Add(dlgUnitDepCaption);
    Add(lisMenuLazDoc);
    EndUpdate;
  end;
  WindowPositionsBox:=TIDEWindowSetupLayoutComponent.Create(Self);
  with WindowPositionsBox do begin
    Name:='WindowPositionsBox';
    Parent:=WindowPositionsGroupBox;
    BorderSpacing.Around:=6;
    Align:=alBottom;
    AnchorToNeighbour(akTop,6,WindowPositionsListBox);
  end;
end;

procedure TEnvironmentOptionsDialog.SetupBackupPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgEnvBckup;

  BackupHelpLabel.Caption:=dlgEnvBackupHelpNote;

  BackupProjectGroupBox.Caption:=dlgProjFiles;

  with BakProjTypeRadioGroup do begin
    Caption:=dlgEnvType;
    with Items do begin
      BeginUpdate;
      Add(lisNoBackupFiles);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
  end;

  BakProjAddExtLabel.Caption:=dlgEdCustomExt;

  with BakProjAddExtComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add('bak');
    Add('old');
    EndUpdate;
  end;

  BakProjMaxCounterLabel.Caption:=dlgMaxCntr;

  with BakProjMaxCounterComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add('1');
    Add('2');
    Add('3');
    Add('5');
    Add('9');
    Add(BakMaxCounterInfiniteTxt);
    EndUpdate;
  end;

  BakProjSubDirLabel.Caption:=dlgEdBSubDir;

  BakProjSubDirComboBox.Text:='';
  with BakProjSubDirComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add(dlgBakNoSubDirectory);
    Add('backup');
    EndUpdate;
  end;

  BackupOtherGroupBox.Caption:=dlgEnvOtherFiles;

  with BakOtherTypeRadioGroup do begin
    Caption:=dlgEnvType;
    with Items do begin
      BeginUpdate;
      Add(lisNoBackupFiles);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
  end;

  BakOtherAddExtLabel.Caption:=dlgEdCustomExt;

  with BakOtherAddExtComboBox.Items do begin
    BeginUpdate;
    Add('bak');
    Add('old');
    EndUpdate;
  end;

  BakOtherMaxCounterLabel.Caption:=dlgMaxCntr;

  with BakOtherMaxCounterComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add('1');
    Add('2');
    Add('3');
    Add('5');
    Add('9');
    Add(BakMaxCounterInfiniteTxt);
    EndUpdate;
  end;

  BakOtherSubDirLabel.Caption:=dlgEdBSubDir;

  with BakOtherSubDirComboBox.Items do begin
    BeginUpdate;
    Clear;
    Add(dlgBakNoSubDirectory);
    Add('backup');
    EndUpdate;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupFilesPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgEnvFiles;

  MaxRecentOpenFilesLabel.Caption:=dlgMaxRecentFiles;

  MaxRecentProjectFilesLabel.Caption:=dlgMaxRecentProjs;

  OpenLastProjectAtStartCheckBox.Caption:=dlgQOpenLastPrj;

  LazarusDirGroupBox.Caption:=dlgLazarusDir;

  with LazarusDirComboBox.Items do begin
    BeginUpdate;
    Add(ProgramDirectory);
    EndUpdate;
  end;

  CompilerPathGroupBox.Caption:=Format(dlgFpcPath,[GetDefaultCompilerFilename]);

  with CompilerPathComboBox do begin
    Items.BeginUpdate;
    GetDefaultCompilerFilenames(Items);
    Items.EndUpdate;
  end;

  FPCSourceDirGroupBox.Caption:=dlgFpcSrcPath;

  MakePathGroupBox.Caption:=dlgMakePath;

  with MakePathComboBox.Items do begin
    BeginUpdate;
    Add('/usr/bin/make');
    EndUpdate;
  end;

  TestBuildDirGroupBox.Caption:=dlgTestPrjDir;

  with TestBuildDirComboBox.Items do begin
    BeginUpdate;
    Add('/tmp');
    Add('/var/tmp');
    Add('c:/tmp');
    Add('c:/windows/temp');
    EndUpdate;
  end;
end;

procedure TEnvironmentOptionsDialog.SetCategoryPage(
  const AValue: TEnvOptsDialogPage);
var
  p: Integer;
begin
  case AValue of
    eodpFiles: p:=0;
    eodpLanguage, eodpAutoSave, eodpDesktop, eodpMainHints,
    eodpWindowPositions: p:=2;
    eodpFormEditor: p:=3;
    eodpObjectInspector: p:=4;
    eodpBackup: p:=5;
    eodpNaming: p:=6;
  end;
  Notebook.PageIndex:=p;
end;

procedure TEnvironmentOptionsDialog.SetupFormEditorPage(Page: integer);

  procedure SetupGridGroupBox;
  begin
    ShowBorderSpaceCheckBox.Caption:=dlgQShowBorderSpacing;
    ShowGridCheckBox.Caption:=dlgQShowGrid;
    GridColorLabel.Caption:=dlgGridColor;
    SnapToGridCheckBox.Caption:=dlgQSnapToGrid;
    GridSizeXComboBox.Hint:=dlgGridXHint;
    GridSizeXLabel.Caption:=dlgGridX;
    GridSizeYComboBox.Hint:=dlgGridYHint;
    GridSizeYLabel.Caption:=dlgGridY;
  end;

  procedure SetupGuideLinesGroupBox;
  begin
    ShowGuideLinesCheckBox.Caption:=dlgGuideLines;
    SnapToGuideLinesCheckBox.Caption:=dlgSnapGuideLines;
    GuideLineColorLeftTopLabel.Caption:=dlgLeftTopClr;
    GuideLineColorRightBottomLabel.Caption:=dlgRightBottomClr;
  end;
  
  procedure SetupMiscGroupBox;
  begin
    ShowComponentCaptionsCheckBox.Caption:=dlgShowCaps;
    ShowEditorHintsCheckBox.Caption:=dlgShowEdrHints;
    AutoCreateFormsOnOpenCheckBox.Caption:=dlgAutoForm;
    RightClickSelectsCheckBox.Caption:=dlgRightClickSelects;
    GrabberColorLabel.Caption:=dlgGrabberColor;
    MarkerColorLabel.Caption:=dlgMarkerColor;

    with DesignerPaintLazyCheckBox do begin
      Caption:=lisFEPaintDesignerItemsOnIdle;
      Hint:=lisFEPaintDesignerItemsOnIdleReduceOverheadForSlowCompu;
    end;
  end;
  
  procedure SetupRubberbandBox;
  begin
    RubberbandSelectColorLabel.Caption:=dlgRuberbandSelectionColor;
    RubberbandCreateColorLabel.Caption:=dlgRuberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Caption:=dlgRubberbandSelectsGrandChilds;
  end;

begin
  // EnvironmentOptionsDialog editor page
  NoteBook.Page[Page].Caption := dlgFrmEditor;

  GridGroupBox.Caption:=dlgEnvGrid;

  SetupGridGroupBox;
  
  GuideLinesGroupBox.Caption:=dlgEnvLGuideLines;

  SetupGuideLinesGroupBox;

  RubberbandGroupBox.Caption:=dlgRubberBandGroup;

  SetupRubberbandBox;

  FormEditMiscGroupBox.Caption:=dlgEnvMisc;

  SetupMiscGroupBox;
end;

procedure TEnvironmentOptionsDialog.SetupNamingPage(Page: integer);
var
  pe: TPascalExtType;
begin
  NoteBook.Page[Page].Caption := dlgNaming;

  with PascalFileExtRadiogroup do begin
    Caption:=dlgPasExt;
    with Items do begin
      BeginUpdate;
      for pe:=Low(TPascalExtType) to High(TPascalExtType) do
        if pe<>petNone then
          Add(PascalExtension[pe]);
      EndUpdate;
    end;
    PascalFileExtRadiogroup.Columns:=PascalFileExtRadiogroup.Items.Count;
  end;

  with CharcaseFileActionRadioGroup do begin
    Caption:=dlgCharCaseFileAct;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvAsk);
      Add(dlgAutoRen);
      Add(dlgnoAutomaticRenaming);
      EndUpdate;
    end;
  end;

  with AmbiguousFileActionRadioGroup do begin
    Caption:=dlgAmbigFileAct;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvAsk);
      Add(dlgAutoDel);
      Add(dlgAutoRen);
      Add(dlgAmbigWarn);
      Add(dlgIgnoreVerb);
      EndUpdate;
    end;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupLazDocPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := lisLazDoc;
  
  LazDocPathsGroupBox.Caption := lisLazDocPathsGroupBox;
  LazDocAddPathButton.Caption := lisLazDocAddPathButton;
  LazDocDeletePathButton.Caption := lisLazDocDeletePathButton;

  LazDocPathEdit.Clear;
end;

procedure TEnvironmentOptionsDialog.BakTypeRadioGroupClick(Sender: TObject);
var i: integer;
begin
  i:=TRadioGroup(Sender).ItemIndex;
  if Sender=BakProjTypeRadioGroup then begin
    BakProjAddExtComboBox.Enabled:=(i=4);
    BakProjAddExtLabel.Enabled:=BakProjAddExtComboBox.Enabled;
    BakProjMaxCounterComboBox.Enabled:=(i=3);
    BakProjMaxCounterLabel.EnableD:=BakProjMaxCounterComboBox.Enabled;
  end else begin
    BakOtherAddExtComboBox.Enabled:=(i=4);
    BakOtherAddExtLabel.Enabled:=BakOtherAddExtComboBox.Enabled;
    BakOtherMaxCounterComboBox.Enabled:=(i=3);
    BakOtherMaxCounterLabel.EnableD:=BakOtherMaxCounterComboBox.Enabled;
  end;
end;

procedure TEnvironmentOptionsDialog.FilesButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=CompilerPathButton then
      OpenDialog.Title:=
                      Format(lisChooseCompilerPath,[GetDefaultCompilerFilename])
    else if Sender=MakePathButton then
      OpenDialog.Title:=lisChooseMakePath
    else
      exit;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);

      if Sender=CompilerPathButton then begin
        // check compiler filename
        SetComboBoxText(CompilerPathComboBox,AFilename);
        CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
          lisEnvOptDlgInvalidCompilerFilename,
          lisEnvOptDlgInvalidCompilerFilenameMsg);
      end else if Sender=MakePathButton then begin
        //check make filename
        SetComboBoxText(MakePathComboBox,AFilename);
        CheckExecutable(FOldMakeFilename,MakePathComboBox.Text,
          lisEnvOptDlgInvalidMakeFilename,
          lisEnvOptDlgInvalidMakeFilenameMsg);
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.DirectoriesButtonClick(Sender: TObject);
var
  OpenDialog: TSelectDirectoryDialog;
  ADirectoryName: string;
begin
  OpenDialog:=TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    if Sender=LazarusDirButton then
      OpenDialog.Title:=lisChooseLazarusSourceDirectory
    else if Sender=FPCSourceDirButton then
      OpenDialog.Title:=lisChooseFPCSourceDir
    else if Sender=TestBuildDirButton then
      OpenDialog.Title:=lisChooseTestBuildDir
    else
      exit;

    if OpenDialog.Execute then begin
      ADirectoryName:=CleanAndExpandDirectory(OpenDialog.Filename);

      if Sender=LazarusDirButton then begin
        // check lazarus directory
        SetComboBoxText(LazarusDirComboBox,ADirectoryName);
        CheckLazarusDir;
      end else if Sender=FPCSourceDirButton then begin
        // check fpc source directory
        SetComboBoxText(FPCSourceDirComboBox,ADirectoryName);
        IsFPCSourceDir;
      end else if Sender=TestBuildDirButton then begin
        // check test directory
        SetComboBoxText(TestBuildDirComboBox,ADirectoryName);
        CheckTestDir;
      end;

    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.FormEditorPageResize(Sender: TObject);
var
  w: Integer;
begin
  w:=((FormEditorPage.ClientWidth-3*5)*6) div 10;
  GridGroupBox.Width:=w;
  FormEditMiscGroupBox.Width:=GridGroupBox.Width;
end;

procedure TEnvironmentOptionsDialog.FormResize(Sender: TObject);
begin
end;

procedure TEnvironmentOptionsDialog.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
end;

procedure TEnvironmentOptionsDialog.LazDocBrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    LazDocPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TEnvironmentOptionsDialog.LazDocDeletePathButtonClick(Sender: TObject
  );
begin
  LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
end;

procedure TEnvironmentOptionsDialog.NotebookChangeBounds(Sender: TObject);
begin

end;

procedure TEnvironmentOptionsDialog.OkButtonClick(Sender: TObject);
begin
  if not CheckValues then exit;
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult:=mrOk;
end;

procedure TEnvironmentOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult:=mrCancel;
end;

procedure TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  SaveDialog: TSaveDialog;
begin
  debugln('TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick A');
  SaveDialog:=TSaveDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      SaveDialog.Filter:=lisLazarusDesktopSettings+' (*.lds)|*.lds'
           +'|'+lisXMLFiles+' (*.xml)|*.xml'
           +'|'+dlgAllFiles+' (*.*)|' + GetAllFilesMask;
      if SaveDialog.Execute then begin
        AnEnvironmentOptions:=TEnvironmentOptions.Create;
        try
          WriteSettings(AnEnvironmentOptions);
          AnEnvironmentOptions.Filename:=SaveDialog.Filename;
          if Assigned(OnSaveEnvironmentSettings) then
            OnSaveEnvironmentSettings(Self,AnEnvironmentOptions);
          AnEnvironmentOptions.Save(true);
        finally
          AnEnvironmentOptions.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(SaveDialog);
    except
      on E: Exception do begin
        DebugLn('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick] ',E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.LoadDesktopSettingsFromFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  OpenDialog: TOpenDialog;
begin
  debugln('TEnvironmentOptionsDialog.LoadDesktopSettingsFromFileButtonClick A');
  OpenDialog:=TOpenDialog.Create(nil);
  try
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Filter:=lisLazarusDesktopSettings+' (*.lds)|*.lds'
           +'|'+lisXMLFiles+' (*.xml)|*.xml'
           +'|'+dlgAllFiles+' (*.*)|' + GetAllFilesMask;
      if OpenDialog.Execute then begin
        AnEnvironmentOptions:=TEnvironmentOptions.Create;
        try
          AnEnvironmentOptions.Filename:=OpenDialog.Filename;
          AnEnvironmentOptions.Load(true);
          if Assigned(OnLoadEnvironmentSettings) then
            OnLoadEnvironmentSettings(Self,AnEnvironmentOptions);
          ReadSettings(AnEnvironmentOptions);
        finally
          AnEnvironmentOptions.Free;
        end;
      end;
      InputHistories.StoreFileDialogSettings(OpenDialog);
    except
      // ToDo
      DebugLn('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick]');
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.ReadSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
var i: integer;
begin
  with AnEnvironmentOptions do begin
    // language
    LanguageComboBox.ItemIndex:=LazarusTranslations.IndexOf(LanguageID);
    //debugln('TEnvironmentOptionsDialog.ReadSettings LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' LanguageID="',LanguageID,'"');

    // auto save
    AutoSaveEditorFilesCheckBox.Checked:=AutoSaveEditorFiles;
    AutoSaveProjectCheckBox.Checked:=AutoSaveProject;
    SetComboBoxText(AutoSaveIntervalInSecsComboBox
       ,IntToStr(AutoSaveIntervalInSecs));

    // desktop
    FLayouts:=IDEWindowLayoutList;
    SetWindowPositionsItem(0);

    // object inspector
    OIBackgroundColorButton.ButtonColor:=
       ObjectInspectorOptions.GridBackgroundColor;
    OISubPropsColorButton.ButtonColor:=
       ObjectInspectorOptions.SubPropertiesColor;
    OIReferencesColorButton.ButtonColor:=
       ObjectInspectorOptions.ReferencesColor;
    OIValueColorButton.ButtonColor:=
       ObjectInspectorOptions.ValueColor;
    OIDefaultValueColorButton.ButtonColor:=
       ObjectInspectorOptions.DefaultValueColor;
    OIPropNameColorButton.ButtonColor:=
       ObjectInspectorOptions.PropertyNameColor;

    OIDefaultItemHeightSpinEdit.Value:=ObjectInspectorOptions.DefaultItemHeight;
    OIShowHintCheckBox.Checked := ObjectInspectorOptions.ShowHints;
       
    // window minimizing and hiding
    MinimizeAllOnMinimizeMainCheckBox.Checked:=MinimizeAllOnMinimizeMain;
    HideIDEOnRunCheckBox.Checked:=HideIDEOnRun;

    // hints
    CheckDiskChangesWithLoadingCheckBox.Checked:=
      CheckDiskChangesWithLoading;
    ShowHintsForComponentPaletteCheckBox.Checked:=
      ShowHintsForComponentPalette;
    ShowHintsForMainSpeedButtonsCheckBox.Checked:=
      ShowHintsForMainSpeedButtons;
      
    // messages view
    MsgViewDblClickJumpsCheckBox.Checked:=MsgViewDblClickJumps;

    // EnvironmentOptionsDialog editor
    ShowBorderSpaceCheckBox.Checked:=ShowBorderSpacing;
    ShowGridCheckBox.Checked:=ShowGrid;
    GridColorButton.ButtonColor:=GridColor;
    SnapToGridCheckBox.Checked:=SnapToGrid;
    SetComboBoxText(GridSizeXComboBox,IntToStr(GridSizeX));
    SetComboBoxText(GridSizeYComboBox,IntToStr(GridSizeY));
    ShowGuideLinesCheckBox.Checked:=ShowGuideLines;
    SnapToGuideLinesCheckBox.Checked:=SnapToGuideLines;
    GuideLineColorLeftTopButton.ButtonColor:=GuideLineColorLeftTop;
    GuideLineColorRightBottomButton.ButtonColor:=GuideLineColorRightBottom;
    ShowComponentCaptionsCheckBox.Checked:=ShowComponentCaptions;
    ShowEditorHintsCheckBox.Checked:=ShowEditorHints;
    AutoCreateFormsOnOpenCheckBox.Checked:=AutoCreateFormsOnOpen;
    RightClickSelectsCheckBox.Checked:=RightClickSelects;
    GrabberColorButton.ButtonColor:=GrabberColor;
    MarkerColorButton.ButtonColor:=MarkerColor;
    RubberbandSelectColorButton.ButtonColor:=RubberbandSelectionColor;
    RubberbandCreateColorButton.ButtonColor:=RubberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Checked:=RubberbandSelectsGrandChilds;
    DesignerPaintLazyCheckBox.Checked:=DesignerPaintLazy;

    // files
    LazarusDirComboBox.Items.Assign(LazarusDirHistory);
    FOldLazarusDir:=LazarusDirectory;
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory,MaxComboBoxCount);
    CompilerPathComboBox.Items.Assign(CompilerFileHistory);
    FOldCompilerFilename:=CompilerFilename;
    SetComboBoxText(CompilerPathComboBox,CompilerFilename,MaxComboBoxCount);
    FPCSourceDirComboBox.Items.Assign(FPCSourceDirHistory);
    FOldFPCSourceDir:=FPCSourceDirectory;
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory,MaxComboBoxCount);
    MakePathComboBox.Items.Assign(MakeFileHistory);
    FOldMakeFilename:=MakeFilename;
    SetComboBoxText(MakePathComboBox,MakeFilename,MaxComboBoxCount);
    TestBuildDirComboBox.Items.Assign(TestBuildDirHistory);
    FOldTestDir:=TestBuildDirectory;
    SetComboBoxText(TestBuildDirComboBox,TestBuildDirectory,MaxComboBoxCount);

    // recent files and directories
    SetComboBoxText(MaxRecentOpenFilesComboBox,IntToStr(MaxRecentOpenFiles));
    SetComboBoxText(MaxRecentProjectFilesComboBox,IntToStr(MaxRecentProjectFiles));
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;

    // backup
    with BackupInfoProjectFiles do begin
      case BackupType of
       bakNone:          BakProjTypeRadioGroup.ItemIndex:=0;
       bakSymbolInFront: BakProjTypeRadioGroup.ItemIndex:=1;
       bakSymbolBehind:  BakProjTypeRadioGroup.ItemIndex:=2;
       bakCounter:       BakProjTypeRadioGroup.ItemIndex:=3;
       bakUserDefinedAddExt: BakProjTypeRadioGroup.ItemIndex:=4;
       bakSameName:      BakProjTypeRadioGroup.ItemIndex:=5;
      end;
      SetComboBoxText(BakProjAddExtComboBox,AdditionalExtension);
      if MaxCounter<=0 then
        SetComboBoxText(BakProjMaxCounterComboBox,BakMaxCounterInfiniteTxt)
      else
        SetComboBoxText(BakProjMaxCounterComboBox,IntToStr(MaxCounter));
      if SubDirectory<>'' then
        SetComboBoxText(BakProjSubDirComboBox,SubDirectory)
      else
        SetComboBoxText(BakProjSubDirComboBox,dlgBakNoSubDirectory);
    end;
    BakTypeRadioGroupClick(BakProjTypeRadioGroup);
    with BackupInfoOtherFiles do begin
      case BackupType of
       bakNone:          BakOtherTypeRadioGroup.ItemIndex:=0;
       bakSymbolInFront: BakOtherTypeRadioGroup.ItemIndex:=1;
       bakSymbolBehind:  BakOtherTypeRadioGroup.ItemIndex:=2;
       bakCounter:       BakOtherTypeRadioGroup.ItemIndex:=3;
       bakUserDefinedAddExt: BakOtherTypeRadioGroup.ItemIndex:=4;
       bakSameName:      BakOtherTypeRadioGroup.ItemIndex:=5;
      end;
      SetComboBoxText(BakOtherAddExtComboBox,AdditionalExtension);
      if MaxCounter<=0 then
        SetComboBoxText(BakOtherMaxCounterComboBox,BakMaxCounterInfiniteTxt)
      else
        SetComboBoxText(BakOtherMaxCounterComboBox,IntToStr(MaxCounter));
      if SubDirectory<>'' then
        SetComboBoxText(BakOtherSubDirComboBox,SubDirectory)
      else
        SetComboBoxText(BakOtherSubDirComboBox,dlgBakNoSubDirectory);
    end;
    BakTypeRadioGroupClick(BakOtherTypeRadioGroup);
    
    // naming
    for i:=0 to PascalFileExtRadiogroup.Items.Count-1 do
      if PascalFileExtRadiogroup.Items[i]=PascalExtension[PascalFileExtension]
      then PascalFileExtRadiogroup.ItemIndex:=i;

    CharCaseFileActionRadioGroup.ItemIndex  := ord(CharCaseFileAction);
    AmbiguousFileActionRadioGroup.ItemIndex := ord(AmbiguousFileAction);
    
    //lazdoc
    SplitString(LazDocPaths,';',LazDocListBox.Items);
  end;
end;

procedure TEnvironmentOptionsDialog.WriteSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
begin
  with AnEnvironmentOptions do begin
    // language
    if (LanguageComboBox.ItemIndex>=0)
    and (LanguageComboBox.ItemIndex<LazarusTranslations.Count) then
      LanguageID:=LazarusTranslations[LanguageComboBox.ItemIndex].ID;
    //debugln('TEnvironmentOptionsDialog.WriteSettings A LanguageID="',LanguageID,'" LanguageComboBox.ItemIndex=',dbgs(LanguageComboBox.ItemIndex),' ',dbgs(LanguageComboBox.HandleAllocated));

    // auto save
    AutoSaveEditorFiles:=AutoSaveEditorFilesCheckBox.Checked;
    AutoSaveProject:=AutoSaveProjectCheckBox.Checked;
    AutoSaveIntervalInSecs:=StrToIntDef(
      AutoSaveIntervalInSecsComboBox.Text,AutoSaveIntervalInSecs);

    // desktop
    WindowPositionsBox.Save;

    // object inspector
    ObjectInspectorOptions.GridBackgroundColor:=
       OIBackgroundColorButton.ButtonColor;
    ObjectInspectorOptions.SubPropertiesColor:=
       OISubPropsColorButton.ButtonColor;
    ObjectInspectorOptions.ReferencesColor:=
       OIReferencesColorButton.ButtonColor;
    ObjectInspectorOptions.ValueColor:=
       OIValueColorButton.ButtonColor;
    ObjectInspectorOptions.DefaultValueColor:=
       OIDefaultValueColorButton.ButtonColor;
    ObjectInspectorOptions.PropertyNameColor:=
       OIPropNameColorButton.ButtonColor;

    ObjectInspectorOptions.DefaultItemHeight:=
       RoundToInt(OIDefaultItemHeightSpinEdit.Value);
    ObjectInspectorOptions.ShowHints := OIShowHintCheckBox.Checked;

    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;

    // hints
    CheckDiskChangesWithLoading:=CheckDiskChangesWithLoadingCheckBox.Checked;
    ShowHintsForComponentPalette:=ShowHintsForComponentPaletteCheckBox.Checked;
    ShowHintsForMainSpeedButtons:=ShowHintsForMainSpeedButtonsCheckBox.Checked;
    
    // messages view
    MsgViewDblClickJumps:=MsgViewDblClickJumpsCheckBox.Checked;

    // EnvironmentOptionsDialog editor
    ShowBorderSpacing:=ShowBorderSpaceCheckBox.Checked;
    ShowGrid:=ShowGridCheckBox.Checked;
    GridColor:=GridColorButton.ButtonColor;
    SnapToGrid:=SnapToGridCheckBox.Checked;
    GridSizeX:=StrToIntDef(GridSizeXComboBox.Text,GridSizeX);
    GridSizeY:=StrToIntDef(GridSizeYComboBox.Text,GridSizeY);
    ShowGuideLines:=ShowGuideLinesCheckBox.Checked;
    SnapToGuideLines:=SnapToGuideLinesCheckBox.Checked;
    GuideLineColorLeftTop:=GuideLineColorLeftTopButton.ButtonColor;
    GuideLineColorRightBottom:=GuideLineColorRightBottomButton.ButtonColor;
    ShowComponentCaptions:=ShowComponentCaptionsCheckBox.Checked;
    ShowEditorHints:=ShowEditorHintsCheckBox.Checked;
    AutoCreateFormsOnOpen:=AutoCreateFormsOnOpenCheckBox.Checked;
    RightClickSelects:=RightClickSelectsCheckBox.Checked;
    GrabberColor:=GrabberColorButton.ButtonColor;
    MarkerColor:=MarkerColorButton.ButtonColor;
    RubberbandSelectionColor:=RubberbandSelectColorButton.ButtonColor;
    RubberbandCreationColor:=RubberbandCreateColorButton.ButtonColor;
    RubberbandSelectsGrandChilds:=RubberbandSelectsGrandChildsCheckBox.Checked;
    DesignerPaintLazy:=DesignerPaintLazyCheckBox.Checked;

    // files
    LazarusDirectory:=LazarusDirComboBox.Text;
    LazarusDirHistory.Assign(LazarusDirComboBox.Items);
    CompilerFilename:=CompilerPathComboBox.Text;
    CompilerFileHistory.Assign(CompilerPathComboBox.Items);
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;
    FPCSourceDirHistory.Assign(FPCSourceDirComboBox.Items);
    MakeFilename:=MakePathComboBox.Text;
    MakeFileHistory.Assign(MakePathComboBox.Items);
    TestBuildDirHistory.Assign(TestBuildDirComboBox.Items);
    TestBuildDirectory:=TestBuildDirComboBox.Text;

    // recent files and directories
    MaxRecentOpenFiles:=StrToIntDef(
        MaxRecentOpenFilesComboBox.Text,MaxRecentOpenFiles);
    MaxRecentProjectFiles:=StrToIntDef(
        MaxRecentProjectFilesComboBox.Text,MaxRecentProjectFiles);
    OpenLastProjectAtStart:=OpenLastProjectAtStartCheckBox.Checked;

    // backup
    with BackupInfoProjectFiles do begin
      case BakProjTypeRadioGroup.ItemIndex of
       0: BackupType:=bakNone;
       1: BackupType:=bakSymbolInFront;
       2: BackupType:=bakSymbolBehind;
       3: BackupType:=bakCounter;
       4: BackupType:=bakUserDefinedAddExt;
       5: BackupType:=bakSameName;
      end;
      AdditionalExtension:=BakProjAddExtComboBox.Text;
      if BakProjMaxCounterComboBox.Text=BakMaxCounterInfiniteTxt then
        MaxCounter:=0
      else
        MaxCounter:=StrToIntDef(BakProjMaxCounterComboBox.Text,1);
      SubDirectory:=BakProjSubDirComboBox.Text;
      if SubDirectory=dlgBakNoSubDirectory then
        SubDirectory:='';
    end;
    with BackupInfoOtherFiles do begin
      case BakOtherTypeRadioGroup.ItemIndex of
       0: BackupType:=bakNone;
       1: BackupType:=bakSymbolInFront;
       2: BackupType:=bakSymbolBehind;
       3: BackupType:=bakCounter;
       4: BackupType:=bakUserDefinedAddExt;
       5: BackupType:=bakSameName;
      end;
      AdditionalExtension:=BakOtherAddExtComboBox.Text;
      if BakOtherMaxCounterComboBox.Text=BakMaxCounterInfiniteTxt then
        MaxCounter:=0
      else
        MaxCounter:=StrToIntDef(BakOtherMaxCounterComboBox.Text,1);
      if BakOtherSubDirComboBox.Text=dlgBakNoSubDirectory then
        SubDirectory:=''
      else
        SubDirectory:=BakOtherSubDirComboBox.Text;
    end;
    
    // naming
    if PascalFileExtRadiogroup.ItemIndex>=0 then
      PascalFileExtension:=PascalExtToType(
        PascalFileExtRadiogroup.Items[PascalFileExtRadiogroup.ItemIndex])
    else
      PascalFileExtension:=petPAS;

    //lazdoc
    LazDocPaths:=StringListToText(LazDocListBox.Items,';',true);

    CharcaseFileAction  := TCharCaseFileAction(CharcaseFileActionRadioGroup.ItemIndex);
    AmbiguousFileAction := TAmbiguousFileAction(AmbiguousFileActionRadioGroup.ItemIndex);
  end;
end;

procedure TEnvironmentOptionsDialog.SetupObjectInspectorPage(Page: integer);
begin
  NoteBook.Page[Page].Caption := dlgObjInsp;

  // object inspector
  ObjectInspectorColorsGroupBox.Caption:=dlgEnvColors;
  OIBackgroundColorLabel.Caption:=dlgBackColor;
  OISubPropsColorLabel.Caption:=dlgSubPropkColor;
  OIReferencesColorLabel.Caption:=dlgReferenceColor;
  OIValueColorLabel.Caption:=dlgValueColor;
  OIDefaultValueColorLabel.Caption:=dlgDefValueColor;
  OIPropNameColorLabel.Caption:=dlgPropNameColor;
  OIMiscGroupBox.Caption:=dlgOIMiscellaneous;
  OIDefaultItemHeightLabel.Caption:=dlgOIItemHeight;
  OIShowHintCheckBox.Caption := lisShowHintsInObjectInspector;
end;

procedure TEnvironmentOptionsDialog.WindowPositionsListBoxMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TEnvironmentOptionsDialog.SetWindowPositionsItem(Index: integer);
begin
  if WindowPositionsBox.Layout<>nil then
    WindowPositionsBox.Save;
  WindowPositionsListBox.ItemIndex:=Index;
  case Index of
  0: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwMainIDEName);
  1: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwSourceNoteBookName);
  2: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwMessagesViewName);
  3: WindowPositionsBox.Layout:=FLayouts.ItemByFormID(DefaultObjectInspectorName);
  4: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwProjectInspector);
  5: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwCodeExplorerName);
  6: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwPkgGraphExplorer);
  7: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwUnitDependenciesName);
  8: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwLazDocName);
  end;
  if Index>=0 then
    WindowPositionsBox.Caption:=WindowPositionsListBox.Items[Index];
end;

function TEnvironmentOptionsDialog.CheckLazarusDir: boolean;
var
  NewLazarusDir: string;
  StopChecking: boolean;
begin
  NewLazarusDir:=LazarusDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldLazarusDir,NewLazarusDir,
                              lisEnvOptDlgLazarusDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;

  // lazarus directory specific tests
  NewLazarusDir:=AppendPathDelim(NewLazarusDir);
  if not CheckLazarusDirectory(NewLazarusDir)
  then begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidLazarusDir,[NewLazarusDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
  Result:=true;
end;

function TEnvironmentOptionsDialog.IsFPCSourceDir: boolean;
var
  NewFPCSrcDir: string;
  StopChecking: boolean;
begin
  NewFPCSrcDir:=FPCSourceDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldFPCSourceDir,NewFPCSrcDir,
                               lisEnvOptDlgFPCSrcDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;

  // FPC source directory specific tests
  if not CheckFPCSourceDir(NewFPCSrcDir) then begin
    Result:=(MessageDlg(Format(lisEnvOptDlgInvalidFPCSrcDir,[NewFPCSrcDir]),
               mtWarning,[mbIgnore,mbCancel],0)=mrIgnore);
    exit;
  end;
  Result:=true;
end;

function TEnvironmentOptionsDialog.CheckTestDir: boolean;
var
  NewTestDir: string;
  StopChecking: boolean;
begin
  NewTestDir:=TestBuildDirComboBox.Text;
  Result:=SimpleDirectoryCheck(FOldTestDir,NewTestDir,
                               lisEnvOptDlgTestDirNotFoundMsg,StopChecking);
  if (not Result) or StopChecking then exit;
end;

function TEnvironmentOptionsDialog.CheckValues: boolean;
begin
  Result:=false;
  // check lazarus directory
  if not CheckLazarusDir then exit;
  // check compiler filename
  if not CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
    lisEnvOptDlgInvalidCompilerFilename,lisEnvOptDlgInvalidCompilerFilenameMsg)
  then exit;
  // check fpc source directory
  if not IsFPCSourceDir then exit;
  // check make filename
  if not CheckExecutable(FOldMakeFilename,MakePathComboBox.Text,
    lisEnvOptDlgInvalidMakeFilename,lisEnvOptDlgInvalidMakeFilenameMsg)
  then exit;
  // check test directory
  if not CheckTestDir then exit;
  
  Result:=true;
end;

initialization
  {$I environmentopts.lrs}

end.

