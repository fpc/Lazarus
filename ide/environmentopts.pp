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
    This unit defines a form for the lazarus environment options and a class to
    store the options in a xml file.

}
unit EnvironmentOpts;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, FPCAdds, Forms, Controls, Buttons, GraphType, Graphics,
  Laz_XMLCfg, ObjectInspector, ExtCtrls, StdCtrls, Spin, EditorOptions,
  LResources, LazConf, Dialogs, ExtToolDialog, IDEProcs, IDEOptionDefs,
  InputHistory, LazarusIDEStrConsts, FileCtrl;

const
  EnvOptsVersion: integer = 102;

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
  TPascalExtType = (petNone, petPAS, petPP);

const
  PascalExtension: array[TPascalExtType] of string = ('', '.pas', '.pp');


  { IDE Language (Human, not computer) }

type
  TLazarusLanguage = (
    llAutomatic,
    llCatalan,
    llEnglish,
    llFrench,
    llGerman,
    llItalian,
    llPolish,
    llRussian,
    llRussianCP1251,
    llSpanish
    );
  
const
  // language names for the config files
  // for the translations see function GetLazarusLanguageNames below
  LazarusLanguageNames: array[TLazarusLanguage] of string = (
    'Automatic (default is english)',
    'Catalan',
    'English',
    'French',
    'German',
    'Italian',
    'Polish',
    'Russian',
    'Russian(CP1251)',
    'Spanish'
  );

  LazarusLanguageIDs: array[TLazarusLanguage] of string = (
    '', 'ca', 'en', 'fr', 'de', 'it', 'pl', 'ru', 'ruwin', 'es'
  );
  
  
  { Ambigious files }

type
  TAmbigiousFileAction = (
      afaAsk,
      afaAutoDelete,
      afaAutoRename,
      afaWarnOnCompile,
      afaIgnore
    );
  TAmbigiousFileActions = set of TAmbigiousFileAction;

const
  AmbigiousFileActionNames: array[TAmbigiousFileAction] of string = (
      'Ask',
      'AutoDelete',
      'AutoRename',
      'WarnOnCompile',
      'Ignore'
    );


  { Environment Options }

type
  { class for storing environment options }
  TEnvironmentOptions = class
  private
    FFilename: string;
    FFileAge: longint;
    FXMLCfg: TXMLConfig;
    FFileHasChangedOnDisk: boolean;
    
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

    // form editor
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
    FGrabberColor: TColor;
    FMarkerColor: TColor;
    FRubberbandSelectionColor: TColor;
    FRubberbandCreationColor: TColor;
    FRubberbandSelectsGrandChilds: boolean;

    // object inspector
    FObjectInspectorOptions: TOIOptions;
    
    // hints
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
    // TODO: store per debuggerclass options
    // Maybe these should go to a new TDebuggerOptions class
    FDebuggerClass: string;
    FDebuggerFilename: string;         // per debugger class
    FDebuggerFileHistory: TStringList; // per debugger class
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
    fPascalFileAutoLowerCase: boolean;
    fPascalFileAskLowerCase: boolean;
    fAmbigiousFileAction: TAmbigiousFileAction;
    
    // language
    fLanguage: TLazarusLanguage;
    
    procedure SetCompilerFilename(const AValue: string);
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
    function DebuggerClassIsDefined: boolean;
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

    // form editor
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
    property GrabberColor: TColor read FGrabberColor write FGrabberColor;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property RubberbandSelectionColor: TColor read FRubberbandSelectionColor
                                              write FRubberbandSelectionColor;
    property RubberbandCreationColor: TColor read FRubberbandCreationColor
                                             write FRubberbandCreationColor;
    property RubberbandSelectsGrandChilds: boolean
                                            read FRubberbandSelectsGrandChilds
                                            write FRubberbandSelectsGrandChilds;

    // object inspector
    property ObjectInspectorOptions: TOIOptions read FObjectInspectorOptions
                                                write FObjectInspectorOptions;

    // hints
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
    property DebuggerClass: String read FDebuggerClass write FDebuggerClass;
    property DebuggerFilename: string read FDebuggerFilename
                                      write SetDebuggerFilename;
    property DebuggerFileHistory: TStringList read FDebuggerFileHistory
                                              write FDebuggerFileHistory;
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
    property PascalFileAutoLowerCase: boolean read fPascalFileAutoLowerCase
                                              write fPascalFileAutoLowerCase;
    property PascalFileAskLowerCase: boolean read fPascalFileAskLowerCase
                                             write fPascalFileAskLowerCase;
    property AmbigiousFileAction: TAmbigiousFileAction read fAmbigiousFileAction
                                                     write fAmbigiousFileAction;
       
    // language
    property Language: TLazarusLanguage read fLanguage write fLanguage;
    
    // messages view
    property MsgViewDblClickJumps: boolean read fMsgViewDblClickJumps
                                           write fMsgViewDblClickJumps;
  end;

  //----------------------------------------------------------------------------

  TOnLoadEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;
  TOnSaveEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;

  { TEnvironmentOptionsDialog: form for environment options }
  
  TEnvOptsDialogPage = (eodpLanguage, eodpAutoSave, eodpDesktop, eodpMainHints,
    eodpWindowPositions, eodpFormEditor, eodpObjectInspector, eodpFiles,
    eodpBackup, eodpNaming);
  
  TEnvironmentOptionsDialog = class(TForm)
    NoteBook: TNoteBook;

    // language
    LanguageGroupBox: TGroupBox;
    LanguageComboBox: TComboBox;
    
    // auto save
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

    // form editor
    GridGroupBox: TGroupBox;
    ShowGridCheckBox: TCheckBox;
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

    // object inspector
    ObjectInspectorColorsGroupBox: TGroupBox;
    OIBackgroundColorLabel: TLabel;
    OIBackgroundColorButton: TColorButton;
    OIMiscGroupBox: TGroupBox;
    OIDefaultItemHeightSpinEdit: TSpinEdit;
    OIDefaultItemHeightLabel: TLabel;

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
    PascalFileAutoLowercaseCheckBox: TCheckBox;
    PascalFileAskLowercaseCheckBox: TCheckBox;
    AmbigiousFileActionRadioGroup: TRadioGroup;

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
    procedure BackupOtherGroupBoxResize(Sender: TObject);
    procedure BackupProjectGroupBoxResize(Sender: TObject);
    procedure BakTypeRadioGroupClick(Sender: TObject);
    procedure CompilerPathGroupBoxResize(Sender: TObject);
    procedure FPCSourceDirGroupBoxResize(Sender: TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure FormEditMiscGroupBoxResize(Sender: TObject);
    procedure GridGroupBoxResize(Sender: TObject);
    procedure GuideLinesGroupBoxResize(Sender: TObject);
    procedure LazarusDirGroupBoxResize(Sender: TObject);
    procedure OIMiscGroupBoxResize(Sender: TObject);
    procedure ObjectInspectorColorsGroupBoxResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OnBackupPageResize(Sender: TObject);
    procedure OnDesktopPageResize(Sender: TObject);
    procedure OnFilesPageResize(Sender: TObject);
    procedure OnFormEditorPageResize(Sender: TObject);
    procedure OnNamingPageResize(Sender: TObject);
    procedure OnObjectInspectorPageResize(Sender: TObject);
    procedure OnWindowsPageResize(Sender: TObject);
    procedure RubberbandGroupBoxResize(Sender: TObject);
    procedure SaveDesktopSettingsToFileButtonClick(Sender: TObject);
    procedure LoadDesktopSettingsFromFileButtonClick(Sender: TObject);
    procedure TestBuildDirGroupBoxResize(Sender: TObject);
    procedure WindowPositionsListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure EnvironmentOptionsDialogResize(Sender: TObject);
    procedure WindowPositionsGroupBoxResize(Sender: TObject);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FLayouts: TIDEWindowLayoutList;
    FOldLazarusDir: string;
    FOldCompilerFilename: string;
    FOldFPCSourceDir: string;
    FOldTestDir: string;
    procedure SetCategoryPage(const AValue: TEnvOptsDialogPage);
    procedure SetupFilesPage(Page: integer);
    procedure SetupDesktopPage(Page: integer);
    procedure SetupWindowsPage(Page: integer);
    procedure SetupFormEditorPage(Page: integer);
    procedure SetupObjectInspectorPage(Page: integer);
    procedure SetupBackupPage(Page: integer);
    procedure SetupNamingPage(Page: integer);
    procedure ResizeFilesPage;
    procedure ResizeDesktopPage;
    procedure ResizeWindowsPage;
    procedure ResizeFormEditorPage;
    procedure ResizeObjectInspectorPage;
    procedure ResizeBackupPage;
    procedure ResizeNamingPage;
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
function AmbigiousFileActionNameToType(const Action: string): TAmbigiousFileAction;
function GetLazarusLanguageNames(aLangId : TLazarusLanguage) : String;

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

implementation


const MaxComboBoxCount: integer = 20;


function Max(i, j: integer): integer;
begin
  if i<=j then
    Result:=j
  else
    Result:=i;
end;

function DebuggerNameToType(const s: string): TDebuggerType;
begin
  for Result:=Low(TDebuggerType) to High(TDebuggerType) do
    if AnsiCompareText(DebuggerName[Result],s)=0 then exit;
  Result:=dtNone;
end;

function PascalExtToType(const Ext: string): TPascalExtType;
begin
  if Ext<>'' then
    for Result:=Low(TPascalExtType) to High(TPascalExtType) do
      if CompareFilenames(Ext,PascalExtension[Result])=0 then exit;
  Result:=petNone;
end;

function AmbigiousFileActionNameToType(
  const Action: string): TAmbigiousFileAction;
begin
  for Result:=Low(TAmbigiousFileAction) to High(TAmbigiousFileAction) do begin
    if AnsiCompareText(AmbigiousFileActionNames[Result],Action)=0 then
      exit;
  end;
  Result:=afaAsk;
end;

function GetLazarusLanguageNames(aLangId: TLazarusLanguage) : String;
begin
  Result := '?';
  Case aLangId of
    llAutomatic: Result:=rsLanguageAutomatic;
    llEnglish  : Result:=rsLanguageEnglish;
    llGerman   : Result:=rsLanguageDeutsch;
    llSpanish  : Result:=rsLanguageSpanish;
    llFrench   : Result:=rsLanguageFrench;
    llRussian  : Result:=rsLanguageRussian;
    llRussianCP1251: Result:=rsLanguageRussianWin;
    llPolish   : Result:=rsLanguagePolish;
    llItalian  : Result:=rsLanguageItalian;
    llCatalan  : Result:=rsLanguageCatalan;
  end;
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

procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
end;

procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString;
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
end;

{ TEnvironmentOptions }

const
  EnvOptsConfFileName='environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';
  BakNoSubDirTxt = '(none)';

constructor TEnvironmentOptions.Create;
begin
  inherited Create;

  FFilename:='';

  // language
  Language:=llAutomatic;

  // auto save
  FAutoSaveEditorFiles:=true;
  FAutoSaveProject:=true;
  FAutoSaveIntervalInSecs:=300; // 5 minutes
  FLastSavedProjectFile:='';

  // windows
  InitLayoutList;
  FIDEDialogLayoutList:=TIDEDialogLayoutList.Create;
  if IDEOptionDefs.IDEDialogLayoutList=nil then
    IDEOptionDefs.IDEDialogLayoutList:=FIDEDialogLayoutList;
  FMinimizeAllOnMinimizeMain:=false;
  FHideIDEOnRun:=false;

  // form editor
  FShowGrid:=true;
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
  FGrabberColor:=clBlack;
  FMarkerColor:=clDkGray;
  FRubberbandSelectionColor:=clNavy;
  FRubberbandCreationColor:=clMaroon;
  FRubberbandSelectsGrandChilds:=true;

  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;
  
  // hints
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
  DebuggerFilename:='';
  FDebuggerFileHistory:=TStringList.Create;
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
  fPascalFileAutoLowerCase:=false;
  fPascalFileAskLowerCase:=true;
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
  FDebuggerFileHistory.Free;
  FTestBuildDirHistory.Free;
  if IDEOptionDefs.IDEDialogLayoutList=FIDEDialogLayoutList then
    IDEOptionDefs.IDEDialogLayoutList:=nil;
  FIDEDialogLayoutList.Free;
  fIDEWindowLayoutList.Free;
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
    writeln('Note: environment config file not found - using defaults');
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
  var l: TLazarusLanguage;
    s: string;
  begin
    s:=XMLConfig.GetValue(
       'EnvironmentOptions/Language/ID',LazarusLanguageIDs[fLanguage]);
    for l:=Low(TLazarusLanguage) to High(TLazarusLanguage) do begin
      if LazarusLanguageIDs[l]=s then
        fLanguage:=l;
    end;
  end;

begin
  try
    XMLConfig:=GetXMLCfg(false);
    FileVersion:=XMLConfig.GetValue('EnvironmentOptions/Version/Value',0);
    
    // language
    LoadLanguage;

    // auto save
    FAutoSaveEditorFiles:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/EditorFiles',true);
    FAutoSaveProject:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/Project',true);
    FAutoSaveIntervalInSecs:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/IntervalInSecs',600);
    FLastSavedProjectFile:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/LastSavedProjectFile','');
    FOpenLastProjectAtStart:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/OpenLastProjectAtStart',true);

    // windows
    FIDEWindowLayoutList.LoadFromXMLConfig(XMLConfig,
      'EnvironmentOptions/Desktop/');
    FIDEDialogLayoutList.LoadFromXMLConfig(XMLConfig,
      'EnvironmentOptions/Desktop/Dialogs');
    FMinimizeAllOnMinimizeMain:=XMLConfig.GetValue(
      'EnvironmentOptions/Desktop/MinimizeAllOnMinimizeMain/Value',true);
    FHideIDEOnRun:=XMLConfig.GetValue(
      'EnvironmentOptions/Desktop/HideIDEOnRun/Value',false);

    // form editor
    FShowGrid:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowGrid',true);
    FGridColor:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridColor',FGridColor);
    FSnapToGrid:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/SnapToGrid',true);
    FGridSizeX:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridSizeX',8);
    FGridSizeY:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridSizeY',8);
    FShowGuideLines:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowGuideLines',true);
    FSnapToGuideLines:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/SnapToGuideLines',true);
    FGuideLineColorLeftTop:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GuideLineColorLeftTop',
       FGuideLineColorLeftTop);
    FGuideLineColorRightBottom:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GuideLineColorRightBottom',
       FGuideLineColorRightBottom);
    FShowComponentCaptions:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowComponentCaptions',true);
    FShowEditorHints:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowEditorHints',true);
    FAutoCreateFormsOnOpen:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/AutoCreateFormsOnOpen',true);
    FGrabberColor:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GrabberColor/Value',FGrabberColor);
    FMarkerColor:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/MarkerColor/Value',FMarkerColor);
    FRubberbandSelectionColor:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/Rubberband/SelectionColor/Value',
       FRubberbandSelectionColor);
    FRubberbandCreationColor:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/Rubberband/CreationColor/Value',
       FRubberbandCreationColor);
    FRubberbandSelectsGrandChilds:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/Rubberband/SelectsGrandChilds/Value',
       false);

    if not OnlyDesktop then begin
      // files
      LazarusDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/LazarusDirectory/Value',FLazarusDirectory);
      LoadRecentList(XMLConfig,FLazarusDirsHistory,
         'EnvironmentOptions/LazarusDirectory/History/');
      if FLazarusDirsHistory.Count=0 then begin
        FLazarusDirsHistory.Add(ProgramDirectory);
      end;
      CompilerFilename:=TrimFilename(XMLConfig.GetValue(
         'EnvironmentOptions/CompilerFilename/Value',FCompilerFilename));
      LoadRecentList(XMLConfig,FCompilerFileHistory,
         'EnvironmentOptions/CompilerFilename/History/');
      if FCompilerFileHistory.Count=0 then
        GetDefaultCompilerFilenames(FCompilerFileHistory);
      FPCSourceDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/FPCSourceDirectory/Value',FFPCSourceDirectory);
      LoadRecentList(XMLConfig,FFPCSourceDirHistory,
         'EnvironmentOptions/FPCSourceDirectory/History/');
      if FFPCSourceDirHistory.Count=0 then begin
      
      end;
      
      TestBuildDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/TestBuildDirectory/Value',FTestBuildDirectory);
      LoadRecentList(XMLConfig,FTestBuildDirHistory,
         'EnvironmentOptions/TestBuildDirectory/History/');
      if FTestBuildDirHistory.Count=0 then
        GetDefaultTestBuildDirs(FTestBuildDirHistory);

      // backup
      LoadBackupInfo(FBackupInfoProjectFiles
        ,'EnvironmentOptions/BackupProjectFiles/');
      LoadBackupInfo(FBackupInfoOtherFiles
        ,'EnvironmentOptions/BackupOtherFiles/');

      // Debugger
      // first try to load the old type
      // it will be overwritten by Class if found
      CurDebuggerClass := XMLConfig.GetValue(
         'EnvironmentOptions/Debugger/Class','');
      if CurDebuggerClass='' then begin
        // try old format
        OldDebuggerType := DebuggerNameToType(XMLConfig.GetValue(
          'EnvironmentOptions/Debugger/Type',''));
        if OldDebuggerType=dtGnuDebugger then
          CurDebuggerClass:='TGDBMIDEBUGGER';
      end;
      DebuggerClass:=CurDebuggerClass;
      DebuggerFilename:=XMLConfig.GetValue(
         'EnvironmentOptions/DebuggerFilename/Value',FDebuggerFilename);
      LoadRecentList(XMLConfig,FDebuggerFileHistory,
         'EnvironmentOptions/DebuggerFilename/History/');
    end;

    // hints
    FShowHintsForComponentPalette:=XMLConfig.GetValue(
      'EnvironmentOptions/ShowHintsForComponentPalette/Value',true);
    FShowHintsForMainSpeedButtons:=XMLConfig.GetValue(
      'EnvironmentOptions/ShowHintsForMainSpeedButtons/Value',true);
      
    // messages view
    fMsgViewDblClickJumps:=XMLConfig.GetValue(
      'EnvironmentOptions/MsgViewDblClickJumps/Value',false);

    // recent files and directories
    FMaxRecentOpenFiles:=XMLConfig.GetValue(
      'EnvironmentOptions/Recent/OpenFiles/Max',FMaxRecentOpenFiles);
    LoadRecentList(XMLConfig,FRecentOpenFiles,
      'EnvironmentOptions/Recent/OpenFiles/');
    FMaxRecentProjectFiles:=XMLConfig.GetValue(
      'EnvironmentOptions/Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
    LoadRecentList(XMLConfig,FRecentProjectFiles,
      'EnvironmentOptions/Recent/ProjectFiles/');
    FMaxRecentPackageFiles:=XMLConfig.GetValue(
      'EnvironmentOptions/Recent/PackageFiles/Max',FMaxRecentOpenFiles);
    LoadRecentList(XMLConfig,FRecentPackageFiles,
      'EnvironmentOptions/Recent/PackageFiles/');

    // external tools
    fExternalTools.Load(XMLConfig,'EnvironmentOptions/ExternalTools/');
    
    // naming
    LoadPascalFileExt('EnvironmentOptions/');
    fPascalFileAutoLowerCase:=XMLConfig.GetValue(
      'EnvironmentOptions/PascalFileAutoLowerCase/Value',false);
    fPascalFileAskLowerCase:=XMLConfig.GetValue(
      'EnvironmentOptions/PascalFileAskLowerCase/Value',true);
    fAmbigiousFileAction:=AmbigiousFileActionNameToType(XMLConfig.GetValue(
      'EnvironmentOptions/AmbigiousFileAction/Value',
        AmbigiousFileActionNames[fAmbigiousFileAction]));
        
    // object inspector
    FObjectInspectorOptions.Load;
    FObjectInspectorOptions.SaveBounds:=false;
    
    FileUpdated;
  except
    // ToDo
    on E: Exception do
      writeln('[TEnvironmentOptions.Load]  error reading "',FFilename,'": '+E.Message);
  end;
end;

procedure TEnvironmentOptions.Save(OnlyDesktop: boolean);
var XMLConfig: TXMLConfig;

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
    XMLConfig.SetValue('EnvironmentOptions/Version/Value',EnvOptsVersion);

    // language
    XMLConfig.SetDeleteValue('EnvironmentOptions/Language/ID'
       ,LazarusLanguageIDs[fLanguage],LazarusLanguageIDs[llAutomatic]);

    // auto save
    XMLConfig.SetDeleteValue('EnvironmentOptions/AutoSave/EditorFiles'
       ,FAutoSaveEditorFiles,true);
    XMLConfig.SetDeleteValue('EnvironmentOptions/AutoSave/Project',
       FAutoSaveProject,true);
    XMLConfig.SetDeleteValue('EnvironmentOptions/AutoSave/IntervalInSecs'
       ,FAutoSaveIntervalInSecs,600);
    XMLConfig.SetDeleteValue('EnvironmentOptions/AutoSave/LastSavedProjectFile'
       ,FLastSavedProjectFile,'');
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/AutoSave/OpenLastProjectAtStart',
       FOpenLastProjectAtStart,true);

    // windows
    FIDEWindowLayoutList.SaveToXMLConfig(XMLConfig,
      'EnvironmentOptions/Desktop/');
    FIDEDialogLayoutList.SaveToXMLConfig(XMLConfig,
      'EnvironmentOptions/Desktop/Dialogs');
    XMLConfig.SetDeleteValue(
      'EnvironmentOptions/Desktop/MinimizeAllOnMinimizeMain/Value',
      FMinimizeAllOnMinimizeMain,true);
    XMLConfig.SetDeleteValue(
      'EnvironmentOptions/Desktop/HideIDEOnRun/Value',FHideIDEOnRun,false);

    // form editor
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/ShowGrid',FShowGrid,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/GridColor',FGridColor,clBlack);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/SnapToGrid',FSnapToGrid,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/GridSizeX',FGridSizeX,8);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/GridSizeY',FGridSizeY,8);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/ShowGuideLines',FShowGuideLines,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/SnapToGuideLines',FSnapToGuideLines,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/GuideLineColorLeftTop',
       FGuideLineColorLeftTop,clGreen);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/GuideLineColorRightBottom',
       FGuideLineColorRightBottom,clBlue);
    XMLConfig.SetDeleteValue('EnvironmentOptions/FormEditor/ShowComponentCaptions',
       FShowComponentCaptions,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/ShowEditorHints',FShowEditorHints,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/AutoCreateFormsOnOpen',
       FAutoCreateFormsOnOpen,true);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/GrabberColor/Value',FGrabberColor,clBlack);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/MarkerColor/Value',FMarkerColor,clDkGray);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/Rubberband/SelectionColor/Value',
       FRubberbandSelectionColor,clBlack);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/Rubberband/CreationColor/Value',
       FRubberbandCreationColor,clRed);
    XMLConfig.SetDeleteValue(
       'EnvironmentOptions/FormEditor/Rubberband/SelectsGrandChilds/Value',
       FRubberbandSelectsGrandChilds,false);

    if not OnlyDesktop then begin
      // files
      XMLConfig.SetDeleteValue(
         'EnvironmentOptions/LazarusDirectory/Value',FLazarusDirectory,'');
      SaveRecentList(XMLConfig,FLazarusDirsHistory,
         'EnvironmentOptions/LazarusDirectory/History/');
      XMLConfig.SetDeleteValue(
         'EnvironmentOptions/CompilerFilename/Value',FCompilerFilename,'');
      SaveRecentList(XMLConfig,FCompilerFileHistory,
         'EnvironmentOptions/CompilerFilename/History/');
      XMLConfig.SetValue(
         'EnvironmentOptions/FPCSourceDirectory/Value',FFPCSourceDirectory);
      SaveRecentList(XMLConfig,FFPCSourceDirHistory,
         'EnvironmentOptions/FPCSourceDirectory/History/');
      XMLConfig.SetValue(
         'EnvironmentOptions/TestBuildDirectory/Value',FTestBuildDirectory);
      SaveRecentList(XMLConfig,FTestBuildDirHistory,
         'EnvironmentOptions/TestBuildDirectory/History/');

      // backup
      SaveBackupInfo(FBackupInfoProjectFiles
        ,'EnvironmentOptions/BackupProjectFiles/');
      SaveBackupInfo(FBackupInfoOtherFiles
        ,'EnvironmentOptions/BackupOtherFiles/');
        
      // debugger
      XMLConfig.SetDeleteValue('EnvironmentOptions/Debugger/Class',
          FDebuggerClass,'');
      XMLConfig.SetDeleteValue('EnvironmentOptions/DebuggerFilename/Value',
          FDebuggerFilename,'');
      SaveRecentList(XMLConfig,FDebuggerFileHistory,
         'EnvironmentOptions/DebuggerFilename/History/');
    end;

    // hints
    XMLConfig.SetDeleteValue('EnvironmentOptions/ShowHintsForComponentPalette/Value',
      FShowHintsForComponentPalette,true);
    XMLConfig.SetDeleteValue('EnvironmentOptions/ShowHintsForMainSpeedButtons/Value',
      FShowHintsForMainSpeedButtons,true);

    // messages view
    XMLConfig.SetDeleteValue('EnvironmentOptions/MsgViewDblClickJumps/Value',
      fMsgViewDblClickJumps,false);

    // recent files and directories
    XMLConfig.SetValue(
      'EnvironmentOptions/Recent/OpenFiles/Max',FMaxRecentOpenFiles);
    SaveRecentList(XMLConfig,FRecentOpenFiles,
      'EnvironmentOptions/Recent/OpenFiles/');
    XMLConfig.SetValue(
      'EnvironmentOptions/Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
    SaveRecentList(XMLConfig,FRecentProjectFiles,
      'EnvironmentOptions/Recent/ProjectFiles/');
    XMLConfig.SetValue(
      'EnvironmentOptions/Recent/PackageFiles/Max',FMaxRecentPackageFiles);
    SaveRecentList(XMLConfig,FRecentPackageFiles,
      'EnvironmentOptions/Recent/PackageFiles/');

    // external tools
    fExternalTools.Save(XMLConfig,'EnvironmentOptions/ExternalTools/');

    // naming
    XMLConfig.SetDeleteValue('EnvironmentOptions/Naming/PascalFileExtension',
      PascalExtension[fPascalFileExtension],'.pas');
    XMLConfig.SetDeleteValue('EnvironmentOptions/PascalFileAutoLowerCase/Value',
      fPascalFileAutoLowerCase,false);
    XMLConfig.SetDeleteValue('EnvironmentOptions/PascalFileAskLowerCase/Value',
      fPascalFileAskLowerCase,true);
    XMLConfig.SetDeleteValue('EnvironmentOptions/AutoDeleteAmbigiousSources/Value',
      AmbigiousFileActionNames[fAmbigiousFileAction],
      AmbigiousFileActionNames[afaAsk]);

    // object inspector
    FObjectInspectorOptions.SaveBounds:=false;
    FObjectInspectorOptions.Save;
    
    XMLConfig.Flush;
    FileUpdated;
  except
    on E: Exception do begin
      // ToDo
      writeln('[TEnvironmentOptions.Save]  error writing "',Filename,'": ',E.Message);
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
       iwpCustomPosition,iwpUseWindowManagerSetting];
    OnApply:=@Self.InternOnApplyWindowLayout;
    DefaultWindowPlacement:=iwpRestoreWindowGeometry;
  end;
  IDEWindowLayoutList.Add(NewLayout);
end;

function TEnvironmentOptions.DebuggerClassIsDefined: boolean;
begin
  Result:=(FDebuggerClass='')
          or (AnsiCompareText(FDebuggerClass,DebuggerName[dtNone])=0);
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
    FXMLCfg.Free;
    if CleanConfig then
      FXMLCfg:=TXMLConfig.CreateClean(Filename)
    else
      FXMLCfg:=TXMLConfig.Create(Filename);
    ObjectInspectorOptions.Filename:=Filename;
    ObjectInspectorOptions.CustomXMLCfg:=FXMLCfg;
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

procedure TEnvironmentOptions.SetDebuggerFilename(const AValue: string);
var
  SpacePos: Integer;
begin
  if FDebuggerFilename=AValue then exit;
  FDebuggerFilename:=AValue;
  // trim the filename and keep the options after the space (if any)
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
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,510,450);
  Caption:=lisMenuGeneralOptions;
  OnResize:=@EnvironmentOptionsDialogResize;
  
  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    Parent:=Self;
    SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
    if PageCount>0 then
      Pages[0]:=dlgEnvFiles
    else
      Pages.Add(dlgEnvFiles);
    Pages.Add(dlgDesktop);
    Pages.Add(dlgWindows);
    Pages.Add(dlgFrmEditor);
    Pages.Add(dlgObjInsp);
    Pages.Add(dlgEnvBckup);
    Pages.Add(dlgNaming);
    PageIndex:=0;
  end;

  SetupFilesPage(0);
  SetupDesktopPage(1);
  SetupWindowsPage(2);
  SetupFormEditorPage(3);
  SetupObjectInspectorPage(4);
  SetupBackupPage(5);
  SetupNamingPage(6);
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Width:=70;
    Height:=23;
    Left:=Self.ClientWidth-Width-15;
    Top:=Self.ClientHeight-Height-15;
    Caption:=dlgCancel;
    OnClick:=@CancelButtonClick;
  end;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Width:=CancelButton.Width;
    Height:=CancelButton.Height;
    Left:=CancelButton.Left-15-Width;
    Top:=CancelButton.Top;
    Caption:='Ok';//"Ok" may be the same in any language. If not, change
    OnClick:=@OkButtonClick;
  end;
  
  OnResize(nil);
end;

destructor TEnvironmentOptionsDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TEnvironmentOptionsDialog.SetupDesktopPage(Page: integer);
var MaxX:integer;
  l: TLazarusLanguage;
begin
  NoteBook.Page[Page].OnResize:=@OnDesktopPageResize;

  MaxX:=ClientWidth-5;

  // language
  LanguageGroupBox:=TGroupBox.Create(Self);
  with LanguageGroupBox do begin
    Name:='LanguageGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=8;
    Top:=2;
    Width:=(MaxX div 2) - 15;
    Height:=50;
    Caption:=dlgEnvLanguage;
    Visible:=true;
  end;
  
  LanguageComboBox:=TComboBox.Create(Self);
  with LanguageComboBox do begin
    Name:='LanguageComboBox';
    Parent:=LanguageGroupBox;
    Left:=5;
    Top:=3;
    Width:=LanguageGroupBox.ClientWidth-2*Left;
    with Items do
    begin
      BeginUpdate;
      for l:=Low(TLazarusLanguage) to High(TLazarusLanguage) do
      begin
        If l<>Low(TLazarusLanguage) then
          Add(GetLazarusLanguageNames(l)+' ['+LazarusLanguageIDs[l]+']')
        else
          Add(GetLazarusLanguageNames(l)); //No [] if automatic
      end;
      EndUpdate;
    end;
  end;

  // auto save
  AutoSaveGroupBox:=TGroupBox.Create(Self);
  with AutoSaveGroupBox do begin
    Name:='AutoSaveGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=LanguageGroupBox.Left;
    Top:=LanguageGroupBox.Top+LanguageGroupBox.Height+5;
    Width:=LanguageGroupBox.Width;
    Height:=108;
    Caption:=dlgAutoSave;
    Visible:=true;
  end;
  
  AutoSaveEditorFilesCheckBox:=TCheckBox.Create(Self);
  with AutoSaveEditorFilesCheckBox do begin
    Name:='AutoSaveEditorFilesCheckBox';
    Parent:=AutoSaveGroupBox;
    Left:=2;
    Top:=2;
    Width:=AutoSaveGroupBox.ClientWidth-2;
    Height:=20;
    Caption:=dlgEdFiles;
    Enabled:=false;
    Visible:=true;
  end;
  
  AutoSaveProjectCheckBox:=TCheckBox.Create(Self);
  with AutoSaveProjectCheckBox do begin
    Name:='AutoSaveProjectCheckBox';
    Parent:=AutoSaveGroupBox;
    Left:=2;
    Top:=27;
    Width:=AutoSaveGroupBox.ClientWidth-2;
    Height:=20;
    Caption:=dlgEnvProject;
    Enabled:=false;
    Visible:=true;
  end;

  AutoSaveIntervalInSecsLabel:=TLabel.Create(Self);
  with AutoSaveIntervalInSecsLabel do begin
    Name:='AutoSaveIntervalInSecsLabel';
    Parent:=AutoSaveGroupBox;
    Left:=4;
    Top:=54;
    Width:=90;
    Height:=23;
    Caption:=dlgIntvInSec;
    Enabled:=false;
    Visible:=true;
  end;

  AutoSaveIntervalInSecsComboBox:=TComboBox.Create(Self);
  with AutoSaveIntervalInSecsComboBox do begin
    Name:='AutoSaveIntervalInSecsComboBox';
    Parent:=AutoSaveGroupBox;
    Left:=AutoSaveIntervalInSecsLabel.Left+AutoSaveIntervalInSecsLabel.Width+5;
    Top:=AutoSaveIntervalInSecsLabel.Top+2;
    Width:=AutoSaveGroupBox.ClientWidth-Left-10;
    with Items do begin
      BeginUpdate;
      Add('1200');
      Add('600');
      Add('300');
      Add('120');
      EndUpdate;
    end;
    Enabled:=false;
  end;

  // desktop files
  DesktopFilesGroupBox:=TGroupBox.Create(Self);
  with DesktopFilesGroupBox do begin
    Name:='DesktopFilesGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=AutoSaveGroupBox.Left;
    Top:=AutoSaveGroupBox.Top+AutoSaveGroupBox.Height+5;
    Width:=AutoSaveGroupBox.Width;
    Height:=90;
    Caption:=dlgDesktopFiles;
    Visible:=true;
  end;

  SaveDesktopSettingsToFileButton:=TButton.Create(Self);
  with SaveDesktopSettingsToFileButton do begin
    Name:='SaveDesktopSettingsToFileButton';
    Parent:=DesktopFilesGroupBox;
    Left:=5;
    Top:=5;
    Width:=DesktopFilesGroupBox.ClientWidth-15;
    Height:=25;
    Caption:=dlgSaveDFile;
    OnClick:=@SaveDesktopSettingsToFileButtonClick;
    Visible:=true;
  end;

  LoadDesktopSettingsFromFileButton:=TButton.Create(Self);
  with LoadDesktopSettingsFromFileButton do begin
    Name:='LoadDesktopSettingsFromFileButton';
    Parent:=DesktopFilesGroupBox;
    Caption:=dlgLoadDFile;
    OnClick:=@LoadDesktopSettingsFromFileButtonClick;
  end;
  
  // hints
  ShowHintsForComponentPaletteCheckBox:=TCheckBox.Create(Self);
  with ShowHintsForComponentPaletteCheckBox do begin
    Name:='ShowHintsForComponentPaletteCheckBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgPalHints;
  end;
  
  ShowHintsForMainSpeedButtonsCheckBox:=TCheckBox.Create(Self);
  with ShowHintsForMainSpeedButtonsCheckBox do begin
    Name:='ShowHintsForMainSpeedButtonsCheckBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgSpBHints;
  end;
  
  // messages view
  MsgViewDblClickJumpsCheckBox:=TCheckBox.Create(Self);
  with MsgViewDblClickJumpsCheckBox do begin
    Name:='MsgViewDblClickJumpsCheckBox';
    Parent:=NoteBook.Page[Page];
    Caption:=lisEnvDoubleClickOnMessagesJumpsOtherwiseSingleClick;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupWindowsPage(Page: integer);
begin
  NoteBook.Page[Page].OnResize:=@OnWindowsPageResize;

  // windows
  MinimizeAllOnMinimizeMainCheckBox:=TCheckBox.Create(Self);
  with MinimizeAllOnMinimizeMainCheckBox do begin
    Name:='MinimizeAllOnMinimizeMainCheckBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgMinimizeAllOnMinimizeMain;
    Enabled:=false;
  end;

  HideIDEOnRunCheckBox:=TCheckBox.Create(Self);
  with HideIDEOnRunCheckBox do begin
    Name:='HideIDEOnRunCheckBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgHideIDEOnRun;
  end;

  // Window Positions
  WindowPositionsGroupBox:=TGroupBox.Create(Self);
  with WindowPositionsGroupBox do begin
    Name:='WindowPositionsGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgWinPos;
    OnResize:=@WindowPositionsGroupBoxResize;
  end;

  WindowPositionsListBox:=TListBox.Create(Self);
  with WindowPositionsListBox do begin
    Name:='WindowPositionsListBox';
    Parent:=WindowPositionsGroupBox;
    with Items do begin
      BeginUpdate;
      Add(dlgMainMenu);
      Add(dlgSrcEdit);
      Add(dlgMsgs);
      Add(dlgObjInsp);
      EndUpdate;
    end;
    OnMouseUp:=@WindowPositionsListBoxMouseUp;
  end;

  WindowPositionsBox:=TIDEWindowSetupLayoutComponent.Create(Self);
  with WindowPositionsBox do begin
    Name:='WindowPositionsBox';
    Parent:=WindowPositionsGroupBox;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupBackupPage(Page: integer);
var MaxX:integer;
begin
  NoteBook.Page[Page].OnResize:=@OnBackupPageResize;

  MaxX:=ClientWidth-5;

  BackupHelpLabel:=TLabel.Create(Self);
  with BackupHelpLabel do begin
    Name:='BackupHelpLabel';
    Parent:=NoteBook.Page[Page];
    Left:=5;
    Top:=2;
    Width:=MaxX-Left*2;
    Height:=23;
    Caption:=dlgEnvBackupHelpNote;
  end;

  BackupProjectGroupBox:=TGroupBox.Create(Self);
  with BackupProjectGroupBox do begin
    Name:='BackupProjectGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Caption:=dlgProjFiles;
    OnResize:=@BackupProjectGroupBoxResize;
  end;

  BakProjTypeRadioGroup:=TRadioGroup.Create(Self);
  with BakProjTypeRadioGroup do begin
    Name:='BakProjTypeRadioGroup';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=4;
    Width:=BackupProjectGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
    Caption:=dlgEnvType;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvNone);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
    OnClick:=@BakTypeRadioGroupClick;
    Visible:=true;
  end;

  BakProjAddExtLabel:=TLabel.Create(Self);
  with BakProjAddExtLabel do begin
    Name:='BakProjAddExtLabel';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=BakProjTypeRadioGroup.Top+BakProjTypeRadioGroup.Height+5;
    Width:=BakProjTypeRadioGroup.Width-62;
    Height:=23;
    Caption:=dlgEdCustomExt;
    Visible:=true;
  end;

  BakProjAddExtComboBox:=TComboBox.Create(Self);
  with BakProjAddExtComboBox do begin
    Name:='BakProjAddExtComboBox';
    Parent:=BackupProjectGroupBox;
    Left:=BakProjAddExtLabel.Left+BakProjAddExtLabel.Width+2;
    Top:=BakProjAddExtLabel.Top;
    Width:=60;
    with Items do begin
      BeginUpdate;
      Add('bak');
      Add('old');
      EndUpdate;
    end;
  end;

  BakProjMaxCounterLabel:=TLabel.Create(Self);
  with BakProjMaxCounterLabel do begin
    Name:='BakProjMaxCounterLabel';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=BakProjAddExtLabel.Top+BakProjAddExtLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:=dlgMaxCntr;
    Visible:=true;
  end;

  BakProjMaxCounterComboBox:=TComboBox.Create(Self);
  with BakProjMaxCounterComboBox do begin
    Name:='BakProjMaxCounterComboBox';
    Parent:=BackupProjectGroupBox;
    Left:=BakProjMaxCounterLabel.Left+BakProjMaxCounterLabel.Width+2;
    Top:=BakProjMaxCounterLabel.Top;
    Width:=100;
    with Items do begin
      BeginUpdate;
      Add('1');
      Add('2');
      Add('3');
      Add('5');
      Add('9');
      Add(BakMaxCounterInfiniteTxt);
      EndUpdate;
    end;
  end;

  BakProjSubDirLabel:=TLabel.Create(Self);
  with BakProjSubDirLabel do begin
    Name:='BakProjSubDirLabel';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=BakProjMaxCounterLabel.Top+BakProjMaxCounterLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:=dlgEdBSubDir;
    Visible:=true;
  end;

  BakProjSubDirComboBox:=TComboBox.Create(Self);
  with BakProjSubDirComboBox do begin
    Name:='BakProjSubDirComboBox';
    Parent:=BackupProjectGroupBox;
    Left:=BakProjSubDirLabel.Left+BakProjSubDirLabel.Width+2;
    Top:=BakProjSubDirLabel.Top;
    Width:=100;
    with Items do begin
      BeginUpdate;
      Add(BakNoSubDirTxt);
      Add('backup');
      EndUpdate;
    end;
  end;

  BackupOtherGroupBox:=TGroupBox.Create(Self);
  with BackupOtherGroupBox do begin
    Name:='BackupOtherGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=BackupProjectGroupBox.Left+BackupProjectGroupBox.Width+10;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Caption:=dlgEnvOtherFiles;
    OnResize:=@BackupOtherGroupBoxResize;
  end;

  BakOtherTypeRadioGroup:=TRadioGroup.Create(Self);
  with BakOtherTypeRadioGroup do begin
    Name:='BakOtherTypeRadioGroup';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=4;
    Width:=BackupOtherGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
    Caption:=dlgEnvType;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvNone);
      Add(dlgSmbFront);
      Add(dlgSmbBehind);
      Add(dlgSmbCounter);
      Add(dlgCustomExt);
      Add(dlgBckUpSubDir);
      EndUpdate;
    end;
    OnClick:=@BakTypeRadioGroupClick;
    Visible:=true;
  end;

  BakOtherAddExtLabel:=TLabel.Create(Self);
  with BakOtherAddExtLabel do begin
    Name:='BakOtherAddExtLabel';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=BakOtherTypeRadioGroup.Top+BakOtherTypeRadioGroup.Height+5;
    Width:=BakOtherTypeRadioGroup.Width-62;
    Height:=23;
    Caption:=dlgEdCustomExt;
    Visible:=true;
  end;

  BakOtherAddExtComboBox:=TComboBox.Create(Self);
  with BakOtherAddExtComboBox do begin
    Name:='BakOtherAddExtComboBox';
    Parent:=BackupOtherGroupBox;
    Left:=BakOtherAddExtLabel.Left+BakOtherAddExtLabel.Width+2;
    Top:=BakOtherAddExtLabel.Top;
    Width:=60;
    with Items do begin
      BeginUpdate;
      Add('bak');
      Add('old');
      EndUpdate;
    end;
  end;

  BakOtherMaxCounterLabel:=TLabel.Create(Self);
  with BakOtherMaxCounterLabel do begin
    Name:='BakOtherMaxCounterLabel';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=BakOtherAddExtLabel.Top+BakOtherAddExtLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:=dlgMaxCntr;
    Visible:=true;
  end;

  BakOtherMaxCounterComboBox:=TComboBox.Create(Self);
  with BakOtherMaxCounterComboBox do begin
    Name:='BakOtherMaxCounterComboBox';
    Parent:=BackupOtherGroupBox;
    Left:=BakOtherMaxCounterLabel.Left+BakOtherMaxCounterLabel.Width+2;
    Top:=BakOtherMaxCounterLabel.Top;
    Width:=100;
    with Items do begin
      BeginUpdate;
      Add('1');
      Add('2');
      Add('3');
      Add('5');
      Add('9');
      Add(BakMaxCounterInfiniteTxt);
      EndUpdate;
    end;
  end;

  BakOtherSubDirLabel:=TLabel.Create(Self);
  with BakOtherSubDirLabel do begin
    Name:='BakOtherSubDirLabel';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=BakOtherMaxCounterLabel.Top+BakOtherMaxCounterLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:=dlgEdBSubDir;
  end;

  BakOtherSubDirComboBox:=TComboBox.Create(Self);
  with BakOtherSubDirComboBox do begin
    Name:='BakOtherSubDirComboBox';
    Parent:=BackupOtherGroupBox;
    Left:=BakOtherSubDirLabel.Left+BakOtherSubDirLabel.Width+2;
    Top:=BakOtherSubDirLabel.Top;
    Width:=100;
    with Items do begin
      BeginUpdate;
      Add(dlgBakDirectory);
      Add('backup');
      EndUpdate;
    end;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupFilesPage(Page: integer);
var MaxX:integer;
begin
  NoteBook.Page[Page].OnResize:=@OnFilesPageResize;

  MaxX:=ClientWidth-5;

  MaxRecentOpenFilesLabel:=TLabel.Create(Self);
  with MaxRecentOpenFilesLabel do begin
    Name:='MaxRecentOpenFilesLabel';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=4;
    Width:=170;
    Height:=23;
    Caption:=dlgMaxRecentFiles;
  end;

  MaxRecentOpenFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentOpenFilesComboBox do begin
    Name:='MaxRecentOpenFilesComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=MaxRecentOpenFilesLabel.Left+MaxRecentOpenFilesLabel.Width+2;
    Top:=MaxRecentOpenFilesLabel.Top;
    Width:=60;
    with Items do begin
      BeginUpdate;
      Add('5');
      Add('10');
      Add('15');
      Add('20');
      Add('25');
      Add('30');
      EndUpdate;
    end;
  end;

  MaxRecentProjectFilesLabel:=TLabel.Create(Self);
  with MaxRecentProjectFilesLabel do begin
    Name:='MaxRecentProjectFilesLabel';
    Parent:=NoteBook.Page[Page];
    Left:=MaxRecentOpenFilesLabel.Left;
    Top:=MaxRecentOpenFilesLabel.Top+MaxRecentOpenFilesLabel.Height+3;
    Width:=MaxRecentOpenFilesLabel.Width;
    Height:=MaxRecentOpenFilesLabel.Height;
    Caption:=dlgMaxRecentProjs;
  end;

  MaxRecentProjectFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentProjectFilesComboBox do begin
    Name:='MaxRecentProjectFilesComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=MaxRecentProjectFilesLabel.Left+MaxRecentProjectFilesLabel.Width+2;
    Top:=MaxRecentProjectFilesLabel.Top;
    Width:=60;
    with Items do begin
      BeginUpdate;
      Add('5');
      Add('10');
      Add('15');
      Add('20');
      Add('25');
      Add('30');
      EndUpdate;
    end;
  end;
  
  OpenLastProjectAtStartCheckBox:=TCheckBox.Create(Self);
  with OpenLastProjectAtStartCheckBox do begin
    Name:='OpenLastProjectAtStartCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=MaxRecentProjectFilesLabel.Top+MaxRecentProjectFilesLabel.Height+5;
    Width:=MaxX-10;
    Height:=23;
    Caption:=dlgQOpenLastPrj;
  end;

  LazarusDirGroupBox:=TGroupBox.Create(Self);
  with LazarusDirGroupBox do begin
    Name:='LazarusDirGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgLazarusDir;
    OnResize:=@LazarusDirGroupBoxResize;
  end;

  LazarusDirComboBox:=TComboBox.Create(Self);
  with LazarusDirComboBox do begin
    Name:='LazarusDirComboBox';
    Parent:=LazarusDirGroupBox;
    with Items do begin
      BeginUpdate;
      Add(ProgramDirectory);
      EndUpdate;
    end;
  end;
  
  LazarusDirButton:=TButton.Create(Self);
  with LazarusDirButton do begin
    Name:='LazarusDirButton';
    Parent:=LazarusDirGroupBox;
    Caption:='...';
    OnClick:=@DirectoriesButtonClick;
  end;

  CompilerPathGroupBox:=TGroupBox.Create(Self);
  with CompilerPathGroupBox do begin
    Name:='CompilerPathGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgFpcPath;
    OnResize:=@CompilerPathGroupBoxResize;
  end;

  CompilerPathComboBox:=TComboBox.Create(Self);
  with CompilerPathComboBox do begin
    Name:='CompilerPathComboBox';
    Parent:=CompilerPathGroupBox;
    with Items do begin
      BeginUpdate;
      Add('/usr/bin/ppc386');
      Add('/opt/fpc/ppc386');
      EndUpdate;
    end;
  end;

  CompilerPathButton:=TButton.Create(Self);
  with CompilerPathButton do begin
    Name:='CompilerPathButton';
    Parent:=CompilerPathGroupBox;
    Caption:='...';
    OnClick:=@FilesButtonClick;
  end;

  FPCSourceDirGroupBox:=TGroupBox.Create(Self);
  with FPCSourceDirGroupBox do begin
    Name:='FPCSourceDirGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgFpcSrcPath;
    OnResize:=@FPCSourceDirGroupBoxResize;
  end;

  FPCSourceDirComboBox:=TComboBox.Create(Self);
  with FPCSourceDirComboBox do begin
    Name:='FPCSourceDirComboBox';
    Parent:=FPCSourceDirGroupBox;
    with Items do begin
      BeginUpdate;
      Add('');
      EndUpdate;
    end;
  end;
  
  FPCSourceDirButton:=TButton.Create(Self);
  with FPCSourceDirButton do begin
    Name:='FPCSourceDirButton';
    Parent:=FPCSourceDirGroupBox;
    Caption:='...';
    OnClick:=@DirectoriesButtonClick;
  end;

  TestBuildDirGroupBox:=TGroupBox.Create(Self);
  with TestBuildDirGroupBox do begin
    Name:='TestBuildDirGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgTestPrjDir;
    OnResize:=@TestBuildDirGroupBoxResize;
  end;

  TestBuildDirComboBox:=TComboBox.Create(Self);
  with TestBuildDirComboBox do begin
    Name:='TestBuildDirComboBox';
    Parent:=TestBuildDirGroupBox;
    with Items do begin
      BeginUpdate;
      Add('/tmp');
      Add('/var/tmp');
      Add('c:/tmp');
      Add('c:/windows/temp');
      EndUpdate;
    end;
  end;

  TestBuildDirButton:=TButton.Create(Self);
  with TestBuildDirButton do begin
    Name:='TestBuildDirButton';
    Parent:=TestBuildDirGroupBox;
    Caption:='...';
    OnClick:=@DirectoriesButtonClick;
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
    ShowGridCheckBox:=TCheckBox.Create(Self);
    with ShowGridCheckBox do begin
      Name:='ShowGridCheckBox';
      Parent:=GridGroupBox;
      Left:=6;
      Top:=2;
      Width:=200;
      Caption:=dlgQShowGrid;
    end;

    GridColorButton:=TColorButton.Create(Self);
    with GridColorButton do begin
      Name:='GridColorButton';
      Parent:=GridGroupBox;
      Left:=ShowGridCheckBox.Left;
      Top:=ShowGridCheckBox.Top+ShowGridCheckBox.Height+5;
      Width:=50;
      Height:=25;
    end;

    GridColorLabel:=TLabel.Create(Self);
    with GridColorLabel do begin
      Name:='GridColorLabel';
      Parent:=GridGroupBox;
      Left:=GridColorButton.Left+GridColorButton.Width+5;
      Top:=GridColorButton.Top+2;
      Width:=100;
      Caption:=dlgGridColor;
    end;

    SnapToGridCheckBox:=TCheckBox.Create(Self);
    with SnapToGridCheckBox do begin
      Name:='SnapToGridCheckBox';
      Parent:=GridGroupBox;
      Top:=GridColorLabel.Top+GridColorLabel.Height+10;
      Left:=ShowGridCheckBox.Left;
      Width:=ShowGridCheckBox.Width;
      Height:=ShowGridCheckBox.Height;
      Caption:=dlgQSnapToGrid;
    end;

    GridSizeXComboBox:=TComboBox.Create(Self);
    with GridSizeXComboBox do begin
      Name:='GridSizeXComboBox';
      Parent:=GridGroupBox;
      Left:=ShowGridCheckBox.Left;
      Top:=SnapToGridCheckBox.Top+SnapToGridCheckBox.Height+5;
      Width:=60;
      with Items do begin
        BeginUpdate;
        Add('2');
        Add('5');
        Add('8');
        Add('10');
        Add('12');
        Add('15');
        Add('20');
        Add('25');
        Add('30');
        EndUpdate;
      end;
      Hint:=dlgGridXHint;
      ShowHint:=true;
    end;

    GridSizeXLabel:=TLabel.Create(Self);
    with GridSizeXLabel do begin
      Name:='GridSizeXLabel';
      Parent:=GridGroupBox;
      Left:=GridSizeXComboBox.Left+GridSizeXComboBox.Width+5;
      Top:=GridSizeXComboBox.Top-2;
      Width:=150;
      Caption:=dlgGridX;
    end;

    GridSizeYComboBox:=TComboBox.Create(Self);
    with GridSizeYComboBox do begin
      Name:='GridSizeYComboBox';
      Parent:=GridGroupBox;
      Left:=GridSizeXComboBox.Left;
      Top:=GridSizeXComboBox.Top+GridSizeXComboBox.Height+4;
      Width:=GridSizeXComboBox.Width;
      with Items do begin
        BeginUpdate;
        Assign(GridSizeXComboBox.Items);
        EndUpdate;
      end;
      Hint:=dlgGridYHint;
      ShowHint:=true;
    end;

    GridSizeYLabel:=TLabel.Create(Self);
    with GridSizeYLabel do begin
      Name:='GridSizeYLabel';
      Parent:=GridGroupBox;
      Left:=GridSizeXLabel.Left;
      Top:=GridSizeYComboBox.Top-1;
      Width:=GridSizeXLabel.Width;
      Caption:=dlgGridY;
    end;
  end;

  procedure SetupGuideLinesGroupBox;
  begin
    ShowGuideLinesCheckBox:=TCheckBox.Create(Self);
    with ShowGuideLinesCheckBox do begin
      Name:='ShowGuideLinesCheckBox';
      Parent:=GuideLinesGroupBox;
      Left:=5;
      Top:=5;
      Width:=Parent.ClientWidth-2*Left;
      Caption:=dlgGuideLines;
      Visible:=true;
    end;
    
    SnapToGuideLinesCheckBox:=TCheckBox.Create(Self);
    with SnapToGuideLinesCheckBox do begin
      Name:='SnapToGuideLinesCheckBox';
      Parent:=GuideLinesGroupBox;
      Left:=ShowGuideLinesCheckBox.Left;
      Top:=ShowGuideLinesCheckBox.Top+ShowGuideLinesCheckBox.Height+5;
      Width:=ShowGuideLinesCheckBox.Width;
      Caption:=dlgSnapGuideLines;
      Visible:=true;
    end;
    
    GuideLineColorLeftTopButton:=TColorButton.Create(Self);
    with GuideLineColorLeftTopButton do begin
      Name:='GuideLineColorLeftTopButton';
      Parent:=GuideLinesGroupBox;
      Left:=SnapToGuideLinesCheckBox.Left;
      Top:=SnapToGuideLinesCheckBox.Top+SnapToGuideLinesCheckBox.Height+5;
      Width:=50;
      Height:=25;
      Visible:=true;
    end;
    
    GuideLineColorLeftTopLabel:=TLabel.Create(Self);
    with GuideLineColorLeftTopLabel do begin
      Name:='GuideLineColorLeftTopLabel';
      Parent:=GuideLinesGroupBox;
      Left:=GuideLineColorLeftTopButton.Left+GuideLineColorLeftTopButton.Width+5;
      Top:=GuideLineColorLeftTopButton.Top+2;
      Width:=150;
      Caption:=dlgLeftTopClr;
      Visible:=true;
    end;

    GuideLineColorRightBottomButton:=TColorButton.Create(Self);
    with GuideLineColorRightBottomButton do begin
      Name:='GuideLineColorRightBottomButton';
      Parent:=GuideLinesGroupBox;
      Left:=GuideLineColorLeftTopButton.Left;
      Top:=GuideLineColorLeftTopButton.Top
          +GuideLineColorLeftTopButton.Height+5;
      Width:=50;
      Height:=25;
      Visible:=true;
    end;

    GuideLineColorRightBottomLabel:=TLabel.Create(Self);
    with GuideLineColorRightBottomLabel do begin
      Name:='GuideLineColorRightBottomLabel';
      Parent:=GuideLinesGroupBox;
      Left:=GuideLineColorLeftTopLabel.Left;
      Top:=GuideLineColorRightBottomButton.Top+2;
      Width:=GuideLineColorLeftTopLabel.Width;
      Caption:=dlgRightBottomClr;
    end;
  end;
  
  procedure SetupMiscGroupBox;
  begin
    ShowComponentCaptionsCheckBox:=TCheckBox.Create(Self);
    with ShowComponentCaptionsCheckBox do begin
      Name:='ShowComponentCaptionsCheckBox';
      Parent:=FormEditMiscGroupBox;
      Top:=5;
      Left:=5;
      Width:=240;
      Caption:=dlgShowCaps;
    end;

    ShowEditorHintsCheckBox:=TCheckBox.Create(Self);
    with ShowEditorHintsCheckBox do begin
      Name:='ShowEditorHintsCheckBox';
      Parent:=FormEditMiscGroupBox;
      Top:=ShowComponentCaptionsCheckBox.Top
           +ShowComponentCaptionsCheckBox.Height+5;
      Left:=ShowComponentCaptionsCheckBox.Left;
      Width:=ShowComponentCaptionsCheckBox.Width;
      Caption:=dlgShowEdrHints;
    end;

    AutoCreateFormsOnOpenCheckBox:=TCheckBox.Create(Self);
    with AutoCreateFormsOnOpenCheckBox do begin
      Name:='AutoCreateFormsOnOpenCheckBox';
      Parent:=FormEditMiscGroupBox;
      Top:=ShowEditorHintsCheckBox.Top+ShowEditorHintsCheckBox.Height+5;
      Left:=ShowEditorHintsCheckBox.Left;
      Width:=ShowEditorHintsCheckBox.Width+100;
      Caption:=dlgAutoForm;
    end;

    GrabberColorButton:=TColorButton.Create(Self);
    with GrabberColorButton do begin
      Name:='GrabberColorButton';
      Parent:=FormEditMiscGroupBox;
      Left:=250;
      Top:=0;
      Width:=50;
      Height:=25;
    end;

    GrabberColorLabel:=TLabel.Create(Self);
    with GrabberColorLabel do begin
      Name:='GrabberColorLabel';
      Parent:=FormEditMiscGroupBox;
      Left:=GrabberColorButton.Left+GrabberColorButton.Width+5;
      Top:=GrabberColorButton.Top+5;
      Width:=110;
      Caption:=dlgGrabberColor;
    end;

    MarkerColorButton:=TColorButton.Create(Self);
    with MarkerColorButton do begin
      Name:='MarkerColorButton';
      Parent:=FormEditMiscGroupBox;
      Left:=GrabberColorButton.Left;
      Top:=GrabberColorButton.Top+GrabberColorButton.Height+5;
      Width:=50;
      Height:=25;
    end;

    MarkerColorLabel:=TLabel.Create(Self);
    with MarkerColorLabel do begin
      Name:='MarkerColorLabel';
      Parent:=FormEditMiscGroupBox;
      Left:=MarkerColorButton.Left+MarkerColorButton.Width+5;
      Top:=MarkerColorButton.Top+5;
      Width:=110;
      Caption:=dlgMarkerColor;
    end;
  end;
  
  procedure SetupRubberbandBox;
  begin
    RubberbandSelectColorButton:=TColorButton.Create(Self);
    with RubberbandSelectColorButton do begin
      Name:='RubberbandSelectColorButton';
      Parent:=RubberbandGroupBox;
      Left:=2;
      Top:=2;
      Width:=50;
      Height:=25;
    end;
    
    RubberbandSelectColorLabel:=TLabel.Create(Self);
    with RubberbandSelectColorLabel do begin
      Name:='RubberbandSelectColorLabel';
      Parent:=RubberbandGroupBox;
      Left:=RubberbandSelectColorButton.Left+RubberbandSelectColorButton.Width+2;
      Top:=RubberbandSelectColorButton.Top+2;
      Width:=100;
      Caption:=dlgRuberbandSelectionColor;
    end;

    RubberbandCreateColorButton:=TColorButton.Create(Self);
    with RubberbandCreateColorButton do begin
      Name:='RubberbandCreateColorButton';
      Parent:=RubberbandGroupBox;
      Left:=2;
      Top:=RubberbandSelectColorButton.Top+RubberbandSelectColorButton.Height+3;
      Width:=50;
      Height:=25;
    end;

    RubberbandCreateColorLabel:=TLabel.Create(Self);
    with RubberbandCreateColorLabel do begin
      Name:='RubberbandCreateColorLabel';
      Parent:=RubberbandGroupBox;
      Left:=RubberbandCreateColorButton.Left+RubberbandCreateColorButton.Width+2;
      Top:=RubberbandCreateColorButton.Top+2;
      Width:=100;
      Caption:=dlgRuberbandCreationColor;
    end;

    RubberbandSelectsGrandChildsCheckBox:=TCheckBox.Create(Self);
    with RubberbandSelectsGrandChildsCheckBox do begin
      Name:='RubberbandSelectsGrandChildsCheckBox';
      Parent:=RubberbandGroupBox;
      Left:=5;
      Top:=RubberbandCreateColorButton.Top+RubberbandCreateColorButton.Height+3;
      Width:=150;
      Caption:=dlgRubberbandSelectsGrandChilds;
    end;
  end;

begin
  // form editor page
  NoteBook.Page[Page].OnResize:=@OnFormEditorPageResize;

  GridGroupBox:=TGroupBox.Create(Self);
  with GridGroupBox do begin
    Name:='GridGroupBox';
    Parent:=Notebook.Page[Page];
    Left:=5;
    Top:=5;
    Width:=((Parent.ClientWidth-3*Left) div 2);
    Height:=170;
    Caption:=dlgEnvGrid;
    OnResize:=@GridGroupBoxResize;
  end;
  
  SetupGridGroupBox;
  
  GuideLinesGroupBox:=TGroupBox.Create(Self);
  with GuideLinesGroupBox do begin
    Name:='GuideLinesGroupBox';
    Parent:=Notebook.Page[Page];
    Left:=GridGroupBox.Left+GridGroupBox.Width+5;
    Top:=GridGroupBox.Top;
    Width:=GridGroupBox.Width;
    Height:=GridGroupBox.Height;
    Caption:=dlgEnvLGuideLines;
    OnResize:=@GuideLinesGroupBoxResize;
  end;
  
  SetupGuideLinesGroupBox;

  FormEditMiscGroupBox:=TGroupBox.Create(Self);
  with FormEditMiscGroupBox do begin
    Name:='FormEditMiscGroupBox';
    Parent:=Notebook.Page[Page];
    Left:=5;
    Top:=GridGroupBox.Top+GridGroupBox.Height+5;
    Width:=Parent.ClientWidth-2*Left;
    Height:=100;
    Caption:=dlgEnvMisc;
    OnResize:=@FormEditMiscGroupBoxResize;
  end;
  
  SetupMiscGroupBox;
  
  RubberbandGroupBox:=TGroupBox.Create(Self);
  with RubberbandGroupBox do begin
    Name:='RubberbandGroupBox';
    Parent:=Notebook.Page[Page];
    Left:=5;
    Top:=FormEditMiscGroupBox.Top+FormEditMiscGroupBox.Height+5;
    Width:=GridGroupBox.Width;
    Height:=120;
    Caption:=dlgRubberBandGroup;
    OnResize:=@RubberbandGroupBoxResize;
  end;

  SetupRubberbandBox;
end;

procedure TEnvironmentOptionsDialog.SetupNamingPage(Page: integer);
var
  pe: TPascalExtType;
begin
  NoteBook.Page[Page].OnResize:=@OnNamingPageResize;

  PascalFileExtRadiogroup:=TRadioGroup.Create(Self);
  with PascalFileExtRadiogroup do begin
    Name:='PascalFileExtRadiogroup';
    Parent:=NoteBook.Page[Page];
    Left:=5;
    Top:=4;
    Width:=200;
    Height:=80;
    Caption:=dlgPasExt;
    with Items do begin
      BeginUpdate;
      for pe:=Low(TPascalExtType) to High(TPascalExtType) do
        if pe<>petNone then
          Add(PascalExtension[pe]);
      EndUpdate;
    end;
    Visible:=true;
  end;

  PascalFileAutoLowercaseCheckBox:=TCheckBox.Create(Self);
  with PascalFileAutoLowercaseCheckBox do begin
    Name:='PascalFileAutoLowercaseCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=PascalFileExtRadiogroup.Left;
    Top:=PascalFileExtRadiogroup.Top+PascalFileExtRadiogroup.Height+10;
    Width:=300;
    Caption:=Format(dlgPasAutoLower,['"','"']);
    Visible:=true;
  end;
  
  PascalFileAskLowercaseCheckBox:=TCheckBox.Create(Self);
  with PascalFileAskLowercaseCheckBox do begin
    Name:='PascalFileAskLowercaseCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=PascalFileAutoLowercaseCheckBox.Left;
    Top:=PascalFileAutoLowercaseCheckBox.Top+PascalFileAutoLowercaseCheckBox.Height+10;
    Width:=300;
    Caption:=Format(dlgPasAskLower,['"','"']);
    Visible:=true;
  end;

  AmbigiousFileActionRadioGroup:=TRadioGroup.Create(Self);
  with AmbigiousFileActionRadioGroup do begin
    Name:='AmbigiousFileActionRadioGroup';
    Parent:=NoteBook.Page[Page];
    Left:=PascalFileAskLowercaseCheckBox.Left;
    Top:=PascalFileAskLowercaseCheckBox.Top+PascalFileAskLowercaseCheckBox.Height+15;
    Width:=200;
    Height:=130;
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
    Visible:=true;
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeDesktopPage;
var MaxX:integer;
  x: Integer;
  y: Integer;
  w: Integer;
begin
  MaxX:=ClientWidth-5;

  // language
  with LanguageGroupBox do begin
    SetBounds(8,2,Max(20,(MaxX div 2)) - 15,50);
  end;

  with LanguageComboBox do begin
    SetBounds(5,3,Max(10,LanguageGroupBox.ClientWidth-2*Left),Height);
  end;

  // auto save
  with AutoSaveGroupBox do begin
    Left:=LanguageGroupBox.Left;
    Top:=LanguageGroupBox.Top+LanguageGroupBox.Height+5;
    Width:=LanguageGroupBox.Width;
    Height:=108;
  end;

  with AutoSaveEditorFilesCheckBox do begin
    Left:=2;
    Top:=2;
    Width:=AutoSaveGroupBox.ClientWidth-2;
    Height:=20;
  end;

  with AutoSaveProjectCheckBox do begin
    Left:=2;
    Top:=27;
    Width:=AutoSaveGroupBox.ClientWidth-2;
    Height:=20;
  end;

  with AutoSaveIntervalInSecsLabel do begin
    Left:=4;
    Top:=58;
    Width:=90;
    Height:=23;
  end;

  with AutoSaveIntervalInSecsComboBox do begin
    Left:=AutoSaveIntervalInSecsLabel.Left+AutoSaveIntervalInSecsLabel.Width+5;
    Top:=AutoSaveIntervalInSecsLabel.Top+2;
    Width:=AutoSaveGroupBox.ClientWidth-Left-10;
    Height:=23;
  end;

  // desktop files
  with DesktopFilesGroupBox do begin
    Left:=AutoSaveGroupBox.Left;
    Top:=AutoSaveGroupBox.Top+AutoSaveGroupBox.Height+5;
    Width:=AutoSaveGroupBox.Width;
    Height:=90;
  end;

  with SaveDesktopSettingsToFileButton do begin
    Left:=5;
    Top:=5;
    Width:=Max(10,DesktopFilesGroupBox.ClientWidth-15);
    Height:=25;
  end;

  with LoadDesktopSettingsFromFileButton do begin
    Left:=5;
    Top:=38;
    Width:=SaveDesktopSettingsToFileButton.Width;
    Height:=25;
  end;

  // hints
  x:=DesktopFilesGroupBox.Left;
  y:=DesktopFilesGroupBox.Top+DesktopFilesGroupBox.Height+20;
  with ShowHintsForComponentPaletteCheckBox do begin
    w:=Max(10,Parent.ClientWidth-x);
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  with ShowHintsForMainSpeedButtonsCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  // messages view
  with MsgViewDblClickJumpsCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeWindowsPage;
var
  x: Integer;
  w: Integer;
  y: Integer;
begin
  x:=10;
  w:=MinimizeAllOnMinimizeMainCheckBox.Parent.ClientWidth-2*x;
  y:=10;

  // window minimizing and hiding
  with MinimizeAllOnMinimizeMainCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+5);
  end;

  with HideIDEOnRunCheckBox do begin
    SetBounds(x,y,w,Height);
    inc(y,Height+10);
  end;

  // Window Positions
  with WindowPositionsGroupBox do
    SetBounds(x,y,Max(10,(w div 2)-5),330);
end;

procedure TEnvironmentOptionsDialog.ResizeFormEditorPage;
begin
  // form editor page
  with GridGroupBox do begin
    SetBounds(5,5,((Parent.ClientWidth-3*Left) div 2),170);
  end;
  with GuideLinesGroupBox do begin
    SetBounds(GridGroupBox.Left+GridGroupBox.Width+5,GridGroupBox.Top,
              GridGroupBox.Width,GridGroupBox.Height);
  end;
  with FormEditMiscGroupBox do begin
    SetBounds(5,GridGroupBox.Top+GridGroupBox.Height+5,
              Max(Parent.ClientWidth-2*Left,10),100);
  end;
  with RubberbandGroupBox do begin
    SetBounds(5,FormEditMiscGroupBox.Top+FormEditMiscGroupBox.Height+5,
              Max(Parent.ClientWidth-2*Left,10),100);
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeObjectInspectorPage;
begin
  // object inspector
  with ObjectInspectorColorsGroupBox do begin
    Left:=6;
    Top:=2;
    Width:=200;
    Height:=55;
  end;

  with OIMiscGroupBox do begin
    Left:=ObjectInspectorColorsGroupBox.Left;
    Top:=ObjectInspectorColorsGroupBox.Top+ObjectInspectorColorsGroupBox.Height+5;
    Width:=200;
    Height:=55;
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeFilesPage;
var
  MaxX:integer;
  y: Integer;
  SpaceH: Integer;
  x: Integer;
  w: Integer;
  h: Integer;
begin
  MaxX:=MaxRecentOpenFilesLabel.Parent.ClientWidth;

  with MaxRecentOpenFilesLabel do begin
    Left:=4;
    Top:=4;
    Width:=170;
  end;

  with MaxRecentOpenFilesComboBox do begin
    Left:=MaxRecentOpenFilesLabel.Left+MaxRecentOpenFilesLabel.Width+2;
    Top:=MaxRecentOpenFilesLabel.Top;
    Width:=60;
  end;

  with MaxRecentProjectFilesLabel do begin
    Left:=MaxRecentOpenFilesLabel.Left;
    Top:=MaxRecentOpenFilesLabel.Top+MaxRecentOpenFilesLabel.Height+3;
    Width:=MaxRecentOpenFilesLabel.Width;
  end;

  with MaxRecentProjectFilesComboBox do begin
    Left:=MaxRecentProjectFilesLabel.Left+MaxRecentProjectFilesLabel.Width+2;
    Top:=MaxRecentProjectFilesLabel.Top;
    Width:=60;
  end;

  with OpenLastProjectAtStartCheckBox do begin
    Left:=4;
    Top:=MaxRecentProjectFilesLabel.Top+MaxRecentProjectFilesLabel.Height+5;
    Width:=MaxX-2*Left;
  end;

  y:=OpenLastProjectAtStartCheckBox.Top
        +OpenLastProjectAtStartCheckBox.Height+8;
  SpaceH:=10;
  x:=4;
  w:=LazarusDirGroupBox.Parent.ClientWidth-2*x;
  h:=50;

  with LazarusDirGroupBox do
    SetBounds(x,y,w,h);
  inc(y,h+SpaceH);

  with CompilerPathGroupBox do
    SetBounds(x,y,w,h);
  inc(y,h+SpaceH);

  with FPCSourceDirGroupBox do
    SetBounds(x,y,w,h);
  inc(y,h+SpaceH);

  with TestBuildDirGroupBox do
    SetBounds(x,y,w,h);
  inc(y,h+SpaceH);
end;

procedure TEnvironmentOptionsDialog.ResizeBackupPage;
var MaxX:integer;
begin
  MaxX:=BackupHelpLabel.Parent.ClientWidth;

  with BackupHelpLabel do begin
    Left:=5;
    Top:=2;
    Width:=MaxX-Left*2;
    Height:=23;
  end;

  with BackupProjectGroupBox do begin
    Left:=4;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
  end;

  with BackupOtherGroupBox do begin
    Left:=BackupProjectGroupBox.Left+BackupProjectGroupBox.Width+10;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=Max(10,(MaxX div 2) - 11);
    Height:=260;
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeNamingPage;
begin
  with PascalFileExtRadiogroup do begin
    Left:=5;
    Top:=4;
    Width:=200;
    Height:=80;
  end;

  with PascalFileAutoLowercaseCheckBox do begin
    Left:=PascalFileExtRadiogroup.Left;
    Top:=PascalFileExtRadiogroup.Top+PascalFileExtRadiogroup.Height+10;
    Width:=300;
  end;

  with PascalFileAskLowercaseCheckBox do begin
    Left:=PascalFileAutoLowercaseCheckBox.Left;
    Top:=PascalFileAutoLowercaseCheckBox.Top+PascalFileAutoLowercaseCheckBox.Height+10;
    Width:=300;
  end;

  with AmbigiousFileActionRadioGroup do begin
    Left:=PascalFileAskLowercaseCheckBox.Left;
    Top:=PascalFileAskLowercaseCheckBox.Top+PascalFileAskLowercaseCheckBox.Height+15;
    Width:=200;
    Height:=130;
  end;
end;

procedure TEnvironmentOptionsDialog.EnvironmentOptionsDialogResize(
  Sender: TObject);
begin
  with NoteBook do begin
    SetBounds(0,0,Max(100,Self.ClientWidth),Max(100,Self.ClientHeight-50));
  end;

  with CancelButton do begin
    Width:=70;
    Height:=23;
    Left:=Max(0,Self.ClientWidth-Width-15);
    Top:=Max(0,Self.ClientHeight-Height-15);
  end;

  with OkButton do begin
    Width:=CancelButton.Width;
    Height:=CancelButton.Height;
    Left:=Max(0,CancelButton.Left-15-Width);
    Top:=CancelButton.Top;
  end;
end;

procedure TEnvironmentOptionsDialog.WindowPositionsGroupBoxResize(
  Sender: TObject);
begin
  with WindowPositionsListBox do begin
    SetBounds(2,2,Max(Parent.ClientWidth-2*2,100),Max(100,Parent.Height div 4));
  end;

  with WindowPositionsBox do begin
    Left:=2;
    Top:=WindowPositionsListBox.Top+WindowPositionsListBox.Height+5;
    Width:=WindowPositionsListBox.Width;
    Height:=Parent.ClientHeight-Top-2;
  end;
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

procedure TEnvironmentOptionsDialog.CompilerPathGroupBoxResize(Sender: TObject);
var
  x: Integer;
  w: Integer;
begin
  w:=CompilerPathGroupBox.ClientWidth;
  x:=w-25;
  with CompilerPathComboBox do
    SetBounds(2,0,x-1-2,Height);
  with CompilerPathButton do
    SetBounds(x+1,0,w-2-x-1,CompilerPathComboBox.Height);
end;

procedure TEnvironmentOptionsDialog.FPCSourceDirGroupBoxResize(Sender: TObject);
var
  x: Integer;
  w: Integer;
begin
  w:=FPCSourceDirGroupBox.ClientWidth;
  x:=w-25;
  with FPCSourceDirComboBox do
    SetBounds(2,0,x-1-2,Height);
  with FPCSourceDirButton do
    SetBounds(x+1,0,w-2-x-1,FPCSourceDirComboBox.Height);
end;

procedure TEnvironmentOptionsDialog.FilesButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    // set title
    OpenDialog.Title:=lisChooseCompilerPath;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);

      // check compiler filename
      SetComboBoxText(CompilerPathComboBox,AFilename);
      CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
        lisEnvOptDlgInvalidCompilerFilename,
        lisEnvOptDlgInvalidCompilerFilenameMsg);
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
  OpenDialog:=TSelectDirectoryDialog.Create(Application);
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
      ADirectoryName:=CleanAndExpandFilename(OpenDialog.Filename);

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

procedure TEnvironmentOptionsDialog.BackupProjectGroupBoxResize(Sender: TObject
  );
begin
  with BakProjTypeRadioGroup do begin
    Left:=5;
    Top:=4;
    Width:=BackupProjectGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
  end;

  with BakProjAddExtLabel do begin
    Left:=5;
    Top:=BakProjTypeRadioGroup.Top+BakProjTypeRadioGroup.Height+5;
    Width:=Max(10,BakProjTypeRadioGroup.Width-62);
    Height:=23;
  end;

  with BakProjAddExtComboBox do begin
    Left:=BakProjAddExtLabel.Left+BakProjAddExtLabel.Width+2;
    Top:=BakProjAddExtLabel.Top;
    Width:=60;
    Height:=25;
  end;

  with BakProjMaxCounterLabel do begin
    Left:=5;
    Top:=BakProjAddExtLabel.Top+BakProjAddExtLabel.Height+5;
    Width:=Max(10,BakProjTypeRadioGroup.Width-102);
    Height:=23;
  end;

  with BakProjMaxCounterComboBox do begin
    Left:=BakProjMaxCounterLabel.Left+BakProjMaxCounterLabel.Width+2;
    Top:=BakProjMaxCounterLabel.Top;
    Width:=100;
    Height:=25;
  end;

  with BakProjSubDirLabel do begin
    Left:=5;
    Top:=BakProjMaxCounterLabel.Top+BakProjMaxCounterLabel.Height+5;
    Width:=BakProjTypeRadioGroup.Width-102;
    Height:=23;
  end;

  with BakProjSubDirComboBox do begin
    Left:=BakProjSubDirLabel.Left+BakProjSubDirLabel.Width+2;
    Top:=BakProjSubDirLabel.Top;
    Width:=100;
    Height:=25;
  end;
end;

procedure TEnvironmentOptionsDialog.BackupOtherGroupBoxResize(Sender: TObject);
begin
  with BakOtherTypeRadioGroup do begin
    Left:=5;
    Top:=4;
    Width:=BackupOtherGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
  end;

  with BakOtherAddExtLabel do begin
    Left:=5;
    Top:=BakOtherTypeRadioGroup.Top+BakOtherTypeRadioGroup.Height+5;
    Width:=Max(10,BakOtherTypeRadioGroup.Width-62);
    Height:=23;
  end;

  with BakOtherAddExtComboBox do begin
    Left:=BakOtherAddExtLabel.Left+BakOtherAddExtLabel.Width+2;
    Top:=BakOtherAddExtLabel.Top;
    Width:=60;
    Height:=25;
  end;

  with BakOtherMaxCounterLabel do begin
    Left:=5;
    Top:=BakOtherAddExtLabel.Top+BakOtherAddExtLabel.Height+5;
    Width:=Max(10,BakOtherTypeRadioGroup.Width-102);
    Height:=23;
  end;

  with BakOtherMaxCounterComboBox do begin
    Left:=BakOtherMaxCounterLabel.Left+BakOtherMaxCounterLabel.Width+2;
    Top:=BakOtherMaxCounterLabel.Top;
    Width:=100;
    Height:=25;
  end;

  with BakOtherSubDirLabel do begin
    Left:=5;
    Top:=BakOtherMaxCounterLabel.Top+BakOtherMaxCounterLabel.Height+5;
    Width:=Max(10,BakOtherTypeRadioGroup.Width-102);
    Height:=23;
  end;

  with BakOtherSubDirComboBox do begin
    Left:=BakOtherSubDirLabel.Left+BakOtherSubDirLabel.Width+2;
    Top:=BakOtherSubDirLabel.Top;
    Width:=100;
    Height:=25;
  end;
end;

procedure TEnvironmentOptionsDialog.FormEditMiscGroupBoxResize(Sender: TObject);
var
  w: Integer;
begin
  w:=(FormEditMiscGroupBox.ClientWidth div 2)-10;
  with ShowComponentCaptionsCheckBox do begin
    SetBounds(5,5,w,Height);
  end;

  with ShowEditorHintsCheckBox do begin
    SetBounds(ShowComponentCaptionsCheckBox.Left,
              ShowComponentCaptionsCheckBox.Top
               +ShowComponentCaptionsCheckBox.Height+5,
              w,Height);
  end;

  with AutoCreateFormsOnOpenCheckBox do begin
    SetBounds(ShowEditorHintsCheckBox.Left,
              ShowEditorHintsCheckBox.Top+ShowEditorHintsCheckBox.Height+5,
              w,Height);
  end;
end;

procedure TEnvironmentOptionsDialog.GridGroupBoxResize(Sender: TObject);
begin
  with ShowGridCheckBox do begin
    SetBounds(6,2,200,Height);
  end;

  with GridColorButton do begin
    SetBounds(ShowGridCheckBox.Left,
              ShowGridCheckBox.Top+ShowGridCheckBox.Height+5,
              50,25);
  end;

  with GridColorLabel do begin
    SetBounds(GridColorButton.Left+GridColorButton.Width+5,
           GridColorButton.Top+2,80,Height);
  end;

  with SnapToGridCheckBox do begin
    SetBounds(ShowGridCheckBox.Left,
              GridColorLabel.Top+GridColorLabel.Height+10,
              ShowGridCheckBox.Width,
              ShowGridCheckBox.Height);
  end;

  with GridSizeXComboBox do begin
    SetBounds(ShowGridCheckBox.Left,
              SnapToGridCheckBox.Top+SnapToGridCheckBox.Height+5,60,Height);
  end;

  with GridSizeXLabel do begin
    SetBounds(GridSizeXComboBox.Left+GridSizeXComboBox.Width+5,
              GridSizeXComboBox.Top+2,150,Height);
  end;

  with GridSizeYComboBox do begin
    SetBounds(GridSizeXComboBox.Left,
              GridSizeXComboBox.Top+GridSizeXComboBox.Height+4,
              GridSizeXComboBox.Width,Height);
  end;

  with GridSizeYLabel do begin
    SetBounds(GridSizeXLabel.Left,GridSizeYComboBox.Top+2,
              GridSizeXLabel.Width,Height);
  end;
end;

procedure TEnvironmentOptionsDialog.GuideLinesGroupBoxResize(Sender: TObject);
begin
  with ShowGuideLinesCheckBox do begin
    SetBounds(5,5,Parent.ClientWidth-2*Left,Height);
  end;

  with SnapToGuideLinesCheckBox do begin
    SetBounds(ShowGuideLinesCheckBox.Left,
              ShowGuideLinesCheckBox.Top+ShowGuideLinesCheckBox.Height+5,
              ShowGuideLinesCheckBox.Width,Height);
  end;

  with GuideLineColorLeftTopButton do begin
    SetBounds(SnapToGuideLinesCheckBox.Left,
              SnapToGuideLinesCheckBox.Top+SnapToGuideLinesCheckBox.Height+5,
              50,25);
  end;

  with GuideLineColorLeftTopLabel do begin
    SetBounds(GuideLineColorLeftTopButton.Left+GuideLineColorLeftTopButton.Width+5,
              GuideLineColorLeftTopButton.Top+2,150,Height);
  end;

  with GuideLineColorRightBottomButton do begin
    SetBounds(GuideLineColorLeftTopButton.Left,
              GuideLineColorLeftTopButton.Top
                +GuideLineColorLeftTopButton.Height+5,50,25);
  end;

  with GuideLineColorRightBottomLabel do begin
    SetBounds(GuideLineColorLeftTopLabel.Left,
              GuideLineColorRightBottomButton.Top+2,
              GuideLineColorLeftTopLabel.Width,Height);
  end;
end;

procedure TEnvironmentOptionsDialog.LazarusDirGroupBoxResize(Sender: TObject);
var
  x: Integer;
  w: Integer;
begin
  w:=LazarusDirGroupBox.ClientWidth;
  x:=w-25;
  with LazarusDirComboBox do
    SetBounds(2,0,x-1-2,Height);
  with LazarusDirButton do
    SetBounds(x+1,0,w-2-x-1,LazarusDirComboBox.Height);
end;

procedure TEnvironmentOptionsDialog.OIMiscGroupBoxResize(Sender: TObject);
begin
  with OIDefaultItemHeightSpinEdit do begin
    Left:=6;
    Top:=4;
    Width:=50;
  end;

  with OIDefaultItemHeightLabel do begin
    Left:=OIDefaultItemHeightSpinEdit.Left+OIDefaultItemHeightSpinEdit.Width+5;
    Top:=OIDefaultItemHeightSpinEdit.Top+2;
    Width:=OIMiscGroupBox.ClientWidth-Left-5;
  end;
end;

procedure TEnvironmentOptionsDialog.ObjectInspectorColorsGroupBoxResize(
  Sender: TObject);
begin
  with OIBackgroundColorButton do begin
    Left:=6;
    Top:=6;
    Width:=50;
    Height:=25;
  end;

  with OIBackgroundColorLabel do begin
    Left:=OIBackgroundColorButton.Left+OIBackgroundColorButton.Width+5;
    Top:=OIBackgroundColorButton.Top;
    Width:=Max(ObjectInspectorColorsGroupBox.ClientWidth-Left-5,10);
    Height:=23;
  end;
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

procedure TEnvironmentOptionsDialog.OnBackupPageResize(Sender: TObject);
begin
  ResizeBackupPage;
end;

procedure TEnvironmentOptionsDialog.OnDesktopPageResize(Sender: TObject);
begin
  ResizeDesktopPage;
end;

procedure TEnvironmentOptionsDialog.OnFilesPageResize(Sender: TObject);
begin
  ResizeFilesPage;
end;

procedure TEnvironmentOptionsDialog.OnFormEditorPageResize(Sender: TObject);
begin
  ResizeFormEditorPage;
end;

procedure TEnvironmentOptionsDialog.OnNamingPageResize(Sender: TObject);
begin
  ResizeNamingPage;
end;

procedure TEnvironmentOptionsDialog.OnObjectInspectorPageResize(Sender: TObject
  );
begin
  ResizeObjectInspectorPage;
end;

procedure TEnvironmentOptionsDialog.OnWindowsPageResize(Sender: TObject);
begin
  ResizeWindowsPage;
end;

procedure TEnvironmentOptionsDialog.RubberbandGroupBoxResize(Sender: TObject);
begin

end;

procedure TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick(
  Sender: TObject);
var AnEnvironmentOptions: TEnvironmentOptions;
  SaveDialog: TSaveDialog;
begin
  SaveDialog:=TSaveDialog.Create(Application);
  try
    try
      InputHistories.ApplyFileDialogSettings(SaveDialog);
      SaveDialog.Filter:='Lazarus Desktop Settings (*.lds)|*.lds'
           +'|XML files (*.xml)|*.xml'
           +'|All files (*.*)|*.*';
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
        writeln('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick] ',E.Message);
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
  OpenDialog:=TOpenDialog.Create(Application);
  try
    try
      InputHistories.ApplyFileDialogSettings(OpenDialog);
      OpenDialog.Filter:='Lazarus Desktop Settings (*.lds)|*.lds'
           +'|XML files (*.xml)|*.xml'
           +'|All files (*.*)|*.*';
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
      writeln('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick]');
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TEnvironmentOptionsDialog.TestBuildDirGroupBoxResize(Sender: TObject);
var
  x: Integer;
  w: Integer;
begin
  w:=TestBuildDirGroupBox.ClientWidth;
  x:=w-25;
  with TestBuildDirComboBox do
    SetBounds(2,0,x-1-2,Height);
  with TestBuildDirButton do
    SetBounds(x+1,0,w-2-x-1,TestBuildDirComboBox.Height);
end;

procedure TEnvironmentOptionsDialog.ReadSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
var i: integer;
begin
  with AnEnvironmentOptions do begin
    // language
    LanguageComboBox.ItemIndex:=ord(Language);

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
    OIDefaultItemHeightSpinEdit.Value:=
       ObjectInspectorOptions.DefaultItemHeight;
       
    // window minimizing and hiding
    MinimizeAllOnMinimizeMainCheckBox.Checked:=MinimizeAllOnMinimizeMain;
    HideIDEOnRunCheckBox.Checked:=HideIDEOnRun;

    // hints
    ShowHintsForComponentPaletteCheckBox.Checked:=
      ShowHintsForComponentPalette;
    ShowHintsForMainSpeedButtonsCheckBox.Checked:=
      ShowHintsForMainSpeedButtons;
      
    // messages view
    MsgViewDblClickJumpsCheckBox.Checked:=MsgViewDblClickJumps;

    // form editor
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
    GrabberColorButton.ButtonColor:=GrabberColor;
    MarkerColorButton.ButtonColor:=MarkerColor;
    RubberbandSelectColorButton.ButtonColor:=RubberbandSelectionColor;
    RubberbandCreateColorButton.ButtonColor:=RubberbandCreationColor;
    RubberbandSelectsGrandChildsCheckBox.Checked:=RubberbandSelectsGrandChilds;

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
        SetComboBoxText(BakProjSubDirComboBox,BakNoSubDirTxt);      
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
        SetComboBoxText(BakOtherSubDirComboBox,BakNoSubDirTxt);      
    end;
    BakTypeRadioGroupClick(BakOtherTypeRadioGroup);
    
    // naming
    for i:=0 to PascalFileExtRadiogroup.Items.Count-1 do
      if PascalFileExtRadiogroup.Items[i]=PascalExtension[PascalFileExtension]
      then PascalFileExtRadiogroup.ItemIndex:=i;
    PascalFileAutoLowercaseCheckBox.Checked:=PascalFileAutoLowerCase;
    PascalFileAskLowercaseCheckBox.Checked:=PascalFileAskLowerCase;
    AmbigiousFileActionRadioGroup.ItemIndex:=ord(AmbigiousFileAction);
  end;
end;

procedure TEnvironmentOptionsDialog.WriteSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
var
  l: TLazarusLanguage;
begin
  with AnEnvironmentOptions do begin
    // language
    for l:=low(TLazarusLanguage) to High(TLazarusLanguage) do
      if ord(l)=LanguageComboBox.ItemIndex then
        Language:=l;

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
    ObjectInspectorOptions.DefaultItemHeight:=
      RoundToInt(OIDefaultItemHeightSpinEdit.Value);

    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;

    // hints
    ShowHintsForComponentPalette:=ShowHintsForComponentPaletteCheckBox.Checked;
    ShowHintsForMainSpeedButtons:=ShowHintsForMainSpeedButtonsCheckBox.Checked;
    
    // messages view
    MsgViewDblClickJumps:=MsgViewDblClickJumpsCheckBox.Checked;

    // form editor
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
    GrabberColor:=GrabberColorButton.ButtonColor;
    MarkerColor:=MarkerColorButton.ButtonColor;
    RubberbandSelectionColor:=RubberbandSelectColorButton.ButtonColor;
    RubberbandCreationColor:=RubberbandCreateColorButton.ButtonColor;
    RubberbandSelectsGrandChilds:=RubberbandSelectsGrandChildsCheckBox.Checked;

    // files
    LazarusDirectory:=LazarusDirComboBox.Text;
    LazarusDirHistory.Assign(LazarusDirComboBox.Items);
    CompilerFilename:=CompilerPathComboBox.Text;
    CompilerFileHistory.Assign(CompilerPathComboBox.Items);
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;
    FPCSourceDirHistory.Assign(FPCSourceDirComboBox.Items);
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
      if BakProjSubDirComboBox.Text=BakNoSubDirTxt then
        SubDirectory:=''
      else
        SubDirectory:=BakProjSubDirComboBox.Text;
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
      if BakOtherSubDirComboBox.Text=BakNoSubDirTxt then
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
    PascalFileAutoLowerCase:=PascalFileAutoLowercaseCheckBox.Checked;
    PascalFileAskLowerCase:=PascalFileAskLowercaseCheckBox.Checked;
    AmbigiousFileAction:=
      TAmbigiousFileAction(AmbigiousFileActionRadioGroup.ItemIndex);
  end;
end;

procedure TEnvironmentOptionsDialog.SetupObjectInspectorPage(Page: integer);
var MaxX: integer;
begin
  NoteBook.Page[Page].OnResize:=@OnObjectInspectorPageResize;

  MaxX:=ClientWidth-5;
  
  // object inspector
  ObjectInspectorColorsGroupBox:=TGroupBox.Create(Self);
  with ObjectInspectorColorsGroupBox do begin
    Name:='ObjectInspectorColorsGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=6;
    Top:=2;
    Width:=(MaxX div 2) - 15;
    Height:=55;
    Caption:=dlgEnvColors;
    OnResize:=@ObjectInspectorColorsGroupBoxResize;
  end;

  OIBackgroundColorButton:=TColorButton.Create(Self);
  with OIBackgroundColorButton do begin
    Name:='OIBackgroundColorButton';
    Parent:=ObjectInspectorColorsGroupBox;
    Left:=6;
    Top:=6;
    Width:=50;
    Height:=25;
  end;

  OIBackgroundColorLabel:=TLabel.Create(Self);
  with OIBackgroundColorLabel do begin
    Name:='OIBackgroundColorLabel';
    Parent:=ObjectInspectorColorsGroupBox;
    Left:=OIBackgroundColorButton.Left+OIBackgroundColorButton.Width+5;
    Top:=OIBackgroundColorButton.Top+2;
    Width:=ObjectInspectorColorsGroupBox.ClientWidth-Left-5;
    Height:=23;
    Caption:=dlgBackColor;
  end;
  
  OIMiscGroupBox:=TGroupBox.Create(Self);
  with OIMiscGroupBox do begin
    Name:='OIMiscGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=ObjectInspectorColorsGroupBox.Left;
    Top:=ObjectInspectorColorsGroupBox.Top+ObjectInspectorColorsGroupBox.Height+5;
    Width:=200;
    Height:=55;
    Caption:=dlgOIMiscellaneous;
    OnResize:=@OIMiscGroupBoxResize;
  end;
  
  OIDefaultItemHeightSpinEdit:=TSpinEdit.Create(Self);
  with OIDefaultItemHeightSpinEdit do begin
    Name:='OIDefaultItemHeightSpinEdit';
    Parent:=OIMiscGroupBox;
    Left:=6;
    Top:=4;
    Width:=50;
    Height:=25;
    Decimal_Places:=0;
    MinValue:=0;
    MaxValue:=100;
  end;

  OIDefaultItemHeightLabel:=TLabel.Create(Self);
  with OIDefaultItemHeightLabel do begin
    Name:='OIDefaultItemHeightLabel';
    Parent:=OIMiscGroupBox;
    Left:=OIDefaultItemHeightSpinEdit.Left+OIDefaultItemHeightSpinEdit.Width+5;
    Top:=OIDefaultItemHeightSpinEdit.Top+2;
    Width:=OIMiscGroupBox.ClientWidth-Left-5;
    Caption:=dlgOIItemHeight;
  end;
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
  // check compiler filename
  if not CheckExecutable(FOldCompilerFilename,CompilerPathComboBox.Text,
    lisEnvOptDlgInvalidCompilerFilename,lisEnvOptDlgInvalidCompilerFilenameMsg)
  then exit;
  // check lazarus directory
  if not CheckLazarusDir then exit;
  // check fpc source directory
  if not IsFPCSourceDir then exit;
  // check test directory
  if not CheckTestDir then exit;
  
  Result:=true;
end;

end.

