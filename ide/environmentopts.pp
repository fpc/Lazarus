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
  Classes, SysUtils, FPCAdds, LCLProc, Forms, Controls, Buttons, GraphType,
  Graphics, ExtCtrls, StdCtrls, Spin, FileUtil, LResources, Dialogs,
  Laz_XMLCfg, ObjectInspector,
  LazarusIDEStrConsts, LazConf, ExtToolDialog, IDEProcs, IDEOptionDefs,
  InputHistory, EditorOptions, Translations;

const
  EnvOptsVersion: integer = 104;

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
  TEnvironmentOptions = class
  private
    FDebuggerSearchPath: string;
    FDesignerPaintLazy: boolean;
    FFilename: string;
    FFileAge: longint;
    FFileHasChangedOnDisk: boolean;
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
    FRightClickSelects: boolean;
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
    
    procedure BackupOtherGroupBoxResize(Sender: TObject);
    procedure BackupProjectGroupBoxResize(Sender: TObject);
    procedure BakTypeRadioGroupClick(Sender: TObject);
    procedure CompilerPathGroupBoxResize(Sender: TObject);
    procedure FPCSourceDirGroupBoxResize(Sender: TObject);
    procedure MakePathGroupBoxResize(Sender: TObject);
    procedure FilesButtonClick(Sender: TObject);
    procedure DirectoriesButtonClick(Sender: TObject);
    procedure FormEditMiscGroupBoxResize(Sender: TObject);
    procedure GridGroupBoxResize(Sender: TObject);
    procedure GuideLinesGroupBoxResize(Sender: TObject);
    procedure LazarusDirGroupBoxResize(Sender: TObject);
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
    FOldMakeFilename: string;
    FOldTestDir: string;
    function CreateColorItem(ATop:Integer; AParent:TWinControl;ACaption:String):TColorButton;

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
  LanguageID:='';

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
  if IDEOptionDefs.IDEDialogLayoutList=FIDEDialogLayoutList then
    IDEOptionDefs.IDEDialogLayoutList:=nil;
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
    FIDEDialogLayoutList.LoadFromXMLConfig(XMLConfig,
      Path+'Desktop/Dialogs');
    FMinimizeAllOnMinimizeMain:=XMLConfig.GetValue(
      Path+'Desktop/MinimizeAllOnMinimizeMain/Value',true);
    FHideIDEOnRun:=XMLConfig.GetValue(
      Path+'Desktop/HideIDEOnRun/Value',false);

    // form editor
    FShowGrid:=XMLConfig.GetValue(
       Path+'FormEditor/ShowGrid',true);
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
    fExternalTools.Load(XMLConfig,Path+'ExternalTools/');
    
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
    FIDEDialogLayoutList.SaveToXMLConfig(XMLConfig,Path+'Desktop/Dialogs');
    XMLConfig.SetDeleteValue(Path+'Desktop/MinimizeAllOnMinimizeMain/Value',
      FMinimizeAllOnMinimizeMain,true);
    XMLConfig.SetDeleteValue(Path+'Desktop/HideIDEOnRun/Value',FHideIDEOnRun,
      false);

    // form editor
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
    fExternalTools.Save(XMLConfig,Path+'ExternalTools/');

    // naming
    XMLConfig.SetDeleteValue( Path+'Naming/PascalFileExtension', PascalExtension[fPascalFileExtension],'.pas');

    XMLConfig.SetDeleteValue( Path+'CharcaseFileAction/Value', ord(fCharcaseFileAction), ord(ccfaAutoRename));

    XMLConfig.SetDeleteValue(Path+'AutoDeleteAmbiguousSources/Value',
      AmbiguousFileActionNames[fAmbiguousFileAction],
      AmbiguousFileActionNames[afaAsk]);

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
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,510,480);
  Caption:=lisMenuGeneralOptions;
  OnResize:=@EnvironmentOptionsDialogResize;
  
  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
    Parent:=Self;
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
    Width:=70;
    Height:=23;
    Left:=Self.ClientWidth-Width-15;
    Top:=Self.ClientHeight-Height-15;
    Parent:=Self;
    Caption:=dlgCancel;
    OnClick:=@CancelButtonClick;
  end;
  CancelControl:=CancelButton;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Width:=CancelButton.Width;
    Height:=CancelButton.Height;
    Left:=CancelButton.Left-15-Width;
    Top:=CancelButton.Top;
    Parent:=Self;
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
var
  MaxX: integer;
  i: Integer;
  LangID: String;
begin
  NoteBook.Page[Page].OnResize:=@OnDesktopPageResize;

  MaxX:=ClientWidth-5;

  // language
  LanguageGroupBox:=TGroupBox.Create(Self);
  with LanguageGroupBox do begin
    Name:='LanguageGroupBox';
    Left:=8;
    Top:=2;
    Width:=(MaxX div 2) - 15;
    Height:=50;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgEnvLanguage;
    Visible:=true;
  end;
  
  LanguageComboBox:=TComboBox.Create(Self);
  with LanguageComboBox do begin
    Name:='LanguageComboBox';
    Left:=5;
    Top:=3;
    Width:=LanguageGroupBox.ClientWidth-2*Left;
    Parent:=LanguageGroupBox;
    with Items do
    begin
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
  end;

  // auto save
  AutoSaveGroupBox:=TGroupBox.Create(Self);
  with AutoSaveGroupBox do begin
    Name:='AutoSaveGroupBox';
    Left:=LanguageGroupBox.Left;
    Top:=LanguageGroupBox.Top+LanguageGroupBox.Height+5;
    Width:=LanguageGroupBox.Width;
    Height:=108;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgAutoSave;
    Visible:=true;
  end;
  
  AutoSaveEditorFilesCheckBox:=TCheckBox.Create(Self);
  with AutoSaveEditorFilesCheckBox do begin
    Name:='AutoSaveEditorFilesCheckBox';
    Left:=2;
    Top:=2;
    Width:=AutoSaveGroupBox.ClientWidth-2;
    Height:=20;
    Parent:=AutoSaveGroupBox;
    Caption:=dlgEdFiles;
    Enabled:=false;
    Visible:=true;
  end;
  
  AutoSaveProjectCheckBox:=TCheckBox.Create(Self);
  with AutoSaveProjectCheckBox do begin
    Name:='AutoSaveProjectCheckBox';
    Left:=2;
    Top:=27;
    Width:=AutoSaveGroupBox.ClientWidth-2;
    Height:=20;
    Parent:=AutoSaveGroupBox;
    Caption:=dlgEnvProject;
    Enabled:=false;
    Visible:=true;
  end;

  AutoSaveIntervalInSecsLabel:=TLabel.Create(Self);
  with AutoSaveIntervalInSecsLabel do begin
    Name:='AutoSaveIntervalInSecsLabel';
    Left:=4;
    Top:=54;
    Width:=90;
    Height:=23;
    Parent:=AutoSaveGroupBox;
    Caption:=dlgIntvInSec;
    Enabled:=false;
    Visible:=true;
  end;

  AutoSaveIntervalInSecsComboBox:=TComboBox.Create(Self);
  with AutoSaveIntervalInSecsComboBox do begin
    Name:='AutoSaveIntervalInSecsComboBox';
    Left:=AutoSaveIntervalInSecsLabel.Left+AutoSaveIntervalInSecsLabel.Width+5;
    Top:=AutoSaveIntervalInSecsLabel.Top+2;
    Width:=AutoSaveGroupBox.ClientWidth-Left-10;
    Parent:=AutoSaveGroupBox;
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
    Left:=AutoSaveGroupBox.Left;
    Top:=AutoSaveGroupBox.Top+AutoSaveGroupBox.Height+5;
    Width:=AutoSaveGroupBox.Width;
    Height:=90;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgDesktopFiles;
    Visible:=true;
  end;

  SaveDesktopSettingsToFileButton:=TButton.Create(Self);
  with SaveDesktopSettingsToFileButton do begin
    Name:='SaveDesktopSettingsToFileButton';
    Left:=5;
    Top:=5;
    Width:=DesktopFilesGroupBox.ClientWidth-15;
    Height:=25;
    Parent:=DesktopFilesGroupBox;
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
      Add(lisMenuProjectInspector);
      Add(lisCodeExplorer);
      Add(lisMenuPackageGraph);
      Add(dlgUnitDepCaption);
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
    Left:=5;
    Top:=2;
    Width:=MaxX-Left*2;
    Height:=23;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgEnvBackupHelpNote;
  end;

  BackupProjectGroupBox:=TGroupBox.Create(Self);
  with BackupProjectGroupBox do begin
    Name:='BackupProjectGroupBox';
    Left:=4;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgProjFiles;
    OnResize:=@BackupProjectGroupBoxResize;
  end;

  BakProjTypeRadioGroup:=TRadioGroup.Create(Self);
  with BakProjTypeRadioGroup do begin
    Name:='BakProjTypeRadioGroup';
    Left:=5;
    Top:=4;
    Width:=BackupProjectGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
    Parent:=BackupProjectGroupBox;
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
    Left:=5;
    Top:=BakProjTypeRadioGroup.Top+BakProjTypeRadioGroup.Height+5;
    Width:=BakProjTypeRadioGroup.Width-62;
    Height:=23;
    Parent:=BackupProjectGroupBox;
    Caption:=dlgEdCustomExt;
    Visible:=true;
  end;

  BakProjAddExtComboBox:=TComboBox.Create(Self);
  with BakProjAddExtComboBox do begin
    Name:='BakProjAddExtComboBox';
    Left:=BakProjAddExtLabel.Left+BakProjAddExtLabel.Width+2;
    Top:=BakProjAddExtLabel.Top;
    Width:=60;
    Parent:=BackupProjectGroupBox;
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
    Left:=5;
    Top:=BakProjAddExtLabel.Top+BakProjAddExtLabel.Height+5;
    Width:=110;
    Height:=23;
    Parent:=BackupProjectGroupBox;
    Caption:=dlgMaxCntr;
    Visible:=true;
  end;

  BakProjMaxCounterComboBox:=TComboBox.Create(Self);
  with BakProjMaxCounterComboBox do begin
    Name:='BakProjMaxCounterComboBox';
    Left:=BakProjMaxCounterLabel.Left+BakProjMaxCounterLabel.Width+2;
    Top:=BakProjMaxCounterLabel.Top;
    Width:=100;
    Parent:=BackupProjectGroupBox;
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
    Left:=5;
    Top:=BakProjMaxCounterLabel.Top+BakProjMaxCounterLabel.Height+5;
    Width:=110;
    Height:=23;
    Parent:=BackupProjectGroupBox;
    Caption:=dlgEdBSubDir;
    Visible:=true;
  end;

  BakProjSubDirComboBox:=TComboBox.Create(Self);
  with BakProjSubDirComboBox do begin
    Name:='BakProjSubDirComboBox';
    Left:=BakProjSubDirLabel.Left+BakProjSubDirLabel.Width+2;
    Top:=BakProjSubDirLabel.Top;
    Width:=100;
    Parent:=BackupProjectGroupBox;
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
    Left:=BackupProjectGroupBox.Left+BackupProjectGroupBox.Width+10;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgEnvOtherFiles;
    OnResize:=@BackupOtherGroupBoxResize;
  end;

  BakOtherTypeRadioGroup:=TRadioGroup.Create(Self);
  with BakOtherTypeRadioGroup do begin
    Name:='BakOtherTypeRadioGroup';
    Left:=5;
    Top:=4;
    Width:=BackupOtherGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
    Parent:=BackupOtherGroupBox;
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
    Left:=5;
    Top:=BakOtherTypeRadioGroup.Top+BakOtherTypeRadioGroup.Height+5;
    Width:=BakOtherTypeRadioGroup.Width-62;
    Height:=23;
    Parent:=BackupOtherGroupBox;
    Caption:=dlgEdCustomExt;
    Visible:=true;
  end;

  BakOtherAddExtComboBox:=TComboBox.Create(Self);
  with BakOtherAddExtComboBox do begin
    Name:='BakOtherAddExtComboBox';
    Left:=BakOtherAddExtLabel.Left+BakOtherAddExtLabel.Width+2;
    Top:=BakOtherAddExtLabel.Top;
    Width:=60;
    Parent:=BackupOtherGroupBox;
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
    Left:=5;
    Top:=BakOtherAddExtLabel.Top+BakOtherAddExtLabel.Height+5;
    Width:=110;
    Height:=23;
    Parent:=BackupOtherGroupBox;
    Caption:=dlgMaxCntr;
    Visible:=true;
  end;

  BakOtherMaxCounterComboBox:=TComboBox.Create(Self);
  with BakOtherMaxCounterComboBox do begin
    Name:='BakOtherMaxCounterComboBox';
    Left:=BakOtherMaxCounterLabel.Left+BakOtherMaxCounterLabel.Width+2;
    Top:=BakOtherMaxCounterLabel.Top;
    Width:=100;
    Parent:=BackupOtherGroupBox;
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
    Left:=5;
    Top:=BakOtherMaxCounterLabel.Top+BakOtherMaxCounterLabel.Height+5;
    Width:=110;
    Height:=23;
    Parent:=BackupOtherGroupBox;
    Caption:=dlgEdBSubDir;
  end;

  BakOtherSubDirComboBox:=TComboBox.Create(Self);
  with BakOtherSubDirComboBox do begin
    Name:='BakOtherSubDirComboBox';
    Left:=BakOtherSubDirLabel.Left+BakOtherSubDirLabel.Width+2;
    Top:=BakOtherSubDirLabel.Top;
    Width:=100;
    Parent:=BackupOtherGroupBox;
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
    Left:=4;
    Top:=4;
    Width:=170;
    Height:=23;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgMaxRecentFiles;
  end;

  MaxRecentOpenFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentOpenFilesComboBox do begin
    Name:='MaxRecentOpenFilesComboBox';
    Left:=MaxRecentOpenFilesLabel.Left+MaxRecentOpenFilesLabel.Width+2;
    Top:=MaxRecentOpenFilesLabel.Top;
    Width:=60;
    Parent:=NoteBook.Page[Page];
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
    Left:=MaxRecentOpenFilesLabel.Left;
    Top:=MaxRecentOpenFilesLabel.Top+MaxRecentOpenFilesLabel.Height+3;
    Width:=MaxRecentOpenFilesLabel.Width;
    Height:=MaxRecentOpenFilesLabel.Height;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgMaxRecentProjs;
  end;

  MaxRecentProjectFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentProjectFilesComboBox do begin
    Name:='MaxRecentProjectFilesComboBox';
    Left:=MaxRecentProjectFilesLabel.Left+MaxRecentProjectFilesLabel.Width+2;
    Top:=MaxRecentProjectFilesLabel.Top;
    Width:=60;
    Parent:=NoteBook.Page[Page];
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
    Left:=4;
    Top:=MaxRecentProjectFilesLabel.Top+MaxRecentProjectFilesLabel.Height+5;
    Width:=MaxX-10;
    Height:=23;
    Parent:=NoteBook.Page[Page];
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
    Caption:=Format(dlgFpcPath,[GetDefaultCompilerFilename]);
    OnResize:=@CompilerPathGroupBoxResize;
  end;

  CompilerPathComboBox:=TComboBox.Create(Self);
  with CompilerPathComboBox do begin
    Name:='CompilerPathComboBox';
    Parent:=CompilerPathGroupBox;
    Items.BeginUpdate;
    GetDefaultCompilerFilenames(Items);
    Items.EndUpdate;
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

  MakePathGroupBox:=TGroupBox.Create(Self);
  with MakePathGroupBox do begin
    Name:='MakePathGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:=dlgMakePath;
    OnResize:=@MakePathGroupBoxResize;
  end;

  MakePathComboBox:=TComboBox.Create(Self);
  with MakePathComboBox do begin
    Name:='MakePathComboBox';
    Parent:=MakePathGroupBox;
    with Items do begin
      BeginUpdate;
      Add('/usr/bin/make');
      EndUpdate;
    end;
  end;

  MakePathButton:=TButton.Create(Self);
  with MakePathButton do begin
    Name:='MakePathButton';
    Parent:=MakePathGroupBox;
    Caption:='...';
    OnClick:=@FilesButtonClick;
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
      Left:=6;
      Top:=2;
      Width:=200;
      Caption:=dlgQShowGrid;
      Parent:=GridGroupBox;
    end;

    GridColorButton:=TColorButton.Create(Self);
    with GridColorButton do begin
      Name:='GridColorButton';
      Left:=ShowGridCheckBox.Left;
      Top:=ShowGridCheckBox.Top+ShowGridCheckBox.Height+5;
      Width:=50;
      Height:=25;
      Parent:=GridGroupBox;
    end;

    GridColorLabel:=TLabel.Create(Self);
    with GridColorLabel do begin
      Name:='GridColorLabel';
      Left:=GridColorButton.Left+GridColorButton.Width+5;
      Top:=GridColorButton.Top+2;
      Width:=100;
      Caption:=dlgGridColor;
      Parent:=GridGroupBox;
    end;

    SnapToGridCheckBox:=TCheckBox.Create(Self);
    with SnapToGridCheckBox do begin
      Name:='SnapToGridCheckBox';
      Top:=GridColorLabel.Top+GridColorLabel.Height+10;
      Left:=ShowGridCheckBox.Left;
      Width:=ShowGridCheckBox.Width;
      Height:=ShowGridCheckBox.Height;
      Parent:=GridGroupBox;
      Caption:=dlgQSnapToGrid;
    end;

    GridSizeXComboBox:=TComboBox.Create(Self);
    with GridSizeXComboBox do begin
      Name:='GridSizeXComboBox';
      Left:=ShowGridCheckBox.Left;
      Top:=SnapToGridCheckBox.Top+SnapToGridCheckBox.Height+5;
      Width:=60;
      Parent:=GridGroupBox;
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
      Left:=GridSizeXComboBox.Left+GridSizeXComboBox.Width+5;
      Top:=GridSizeXComboBox.Top-2;
      Width:=150;
      Caption:=dlgGridX;
      Parent:=GridGroupBox;
    end;

    GridSizeYComboBox:=TComboBox.Create(Self);
    with GridSizeYComboBox do begin
      Name:='GridSizeYComboBox';
      Left:=GridSizeXComboBox.Left;
      Top:=GridSizeXComboBox.Top+GridSizeXComboBox.Height+4;
      Width:=GridSizeXComboBox.Width;
      Parent:=GridGroupBox;
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
      Left:=GridSizeXLabel.Left;
      Top:=GridSizeYComboBox.Top-1;
      Width:=GridSizeXLabel.Width;
      Parent:=GridGroupBox;
      Caption:=dlgGridY;
    end;
  end;

  procedure SetupGuideLinesGroupBox;
  begin
    ShowGuideLinesCheckBox:=TCheckBox.Create(Self);
    with ShowGuideLinesCheckBox do begin
      Name:='ShowGuideLinesCheckBox';
      Left:=5;
      Top:=5;
      Width:=GuideLinesGroupBox.ClientWidth-2*Left;
      Parent:=GuideLinesGroupBox;
      Caption:=dlgGuideLines;
      Visible:=true;
    end;
    
    SnapToGuideLinesCheckBox:=TCheckBox.Create(Self);
    with SnapToGuideLinesCheckBox do begin
      Name:='SnapToGuideLinesCheckBox';
      Left:=ShowGuideLinesCheckBox.Left;
      Top:=ShowGuideLinesCheckBox.Top+ShowGuideLinesCheckBox.Height+5;
      Width:=ShowGuideLinesCheckBox.Width;
      Parent:=GuideLinesGroupBox;
      Caption:=dlgSnapGuideLines;
      Visible:=true;
    end;
    
    GuideLineColorLeftTopButton:=TColorButton.Create(Self);
    with GuideLineColorLeftTopButton do begin
      Name:='GuideLineColorLeftTopButton';
      Left:=SnapToGuideLinesCheckBox.Left;
      Top:=SnapToGuideLinesCheckBox.Top+SnapToGuideLinesCheckBox.Height+5;
      Width:=50;
      Height:=25;
      Parent:=GuideLinesGroupBox;
      Visible:=true;
    end;
    
    GuideLineColorLeftTopLabel:=TLabel.Create(Self);
    with GuideLineColorLeftTopLabel do begin
      Name:='GuideLineColorLeftTopLabel';
      Left:=GuideLineColorLeftTopButton.Left+GuideLineColorLeftTopButton.Width+5;
      Top:=GuideLineColorLeftTopButton.Top+2;
      Width:=150;
      Parent:=GuideLinesGroupBox;
      Caption:=dlgLeftTopClr;
      Visible:=true;
    end;

    GuideLineColorRightBottomButton:=TColorButton.Create(Self);
    with GuideLineColorRightBottomButton do begin
      Name:='GuideLineColorRightBottomButton';
      Left:=GuideLineColorLeftTopButton.Left;
      Top:=GuideLineColorLeftTopButton.Top
          +GuideLineColorLeftTopButton.Height+5;
      Width:=50;
      Height:=25;
      Parent:=GuideLinesGroupBox;
      Visible:=true;
    end;

    GuideLineColorRightBottomLabel:=TLabel.Create(Self);
    with GuideLineColorRightBottomLabel do begin
      Name:='GuideLineColorRightBottomLabel';
      Left:=GuideLineColorLeftTopLabel.Left;
      Top:=GuideLineColorRightBottomButton.Top+2;
      Width:=GuideLineColorLeftTopLabel.Width;
      Parent:=GuideLinesGroupBox;
      Caption:=dlgRightBottomClr;
    end;
  end;
  
  procedure SetupMiscGroupBox;
  var
    x: Integer;
    y: Integer;
    w: Integer;
  begin
    x:=5;
    y:=5;
    w:=300;
  
    ShowComponentCaptionsCheckBox:=TCheckBox.Create(Self);
    with ShowComponentCaptionsCheckBox do begin
      Name:='ShowComponentCaptionsCheckBox';
      SetBounds(x,y,w,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=dlgShowCaps;
      inc(y,Height+5);
    end;

    ShowEditorHintsCheckBox:=TCheckBox.Create(Self);
    with ShowEditorHintsCheckBox do begin
      Name:='ShowEditorHintsCheckBox';
      SetBounds(x,y,w,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=dlgShowEdrHints;
      inc(y,Height+5);
    end;

    AutoCreateFormsOnOpenCheckBox:=TCheckBox.Create(Self);
    with AutoCreateFormsOnOpenCheckBox do begin
      Name:='AutoCreateFormsOnOpenCheckBox';
      SetBounds(x,y,w,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=dlgAutoForm;
      inc(y,Height+5);
    end;

    RightClickSelectsCheckBox:=TCheckBox.Create(Self);
    with RightClickSelectsCheckBox do begin
      Name:='RightClickSelectsCheckBox';
      SetBounds(x,y,w,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=dlgRightClickSelects;
      inc(y,Height+5);
    end;

    GrabberColorButton:=TColorButton.Create(Self);
    with GrabberColorButton do begin
      Name:='GrabberColorButton';
      SetBounds(x,y,50,25);
      Parent:=FormEditMiscGroupBox;
      inc(y,Height+5);
    end;

    GrabberColorLabel:=TLabel.Create(Self);
    with GrabberColorLabel do begin
      Name:='GrabberColorLabel';
      SetBounds(GrabberColorButton.Left+GrabberColorButton.Width+5,
                GrabberColorButton.Top+5,110,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=dlgGrabberColor;
    end;

    MarkerColorButton:=TColorButton.Create(Self);
    with MarkerColorButton do begin
      Name:='MarkerColorButton';
      SetBounds(x,y,50,25);
      Parent:=FormEditMiscGroupBox;
      inc(y,Height+5);
    end;

    MarkerColorLabel:=TLabel.Create(Self);
    with MarkerColorLabel do begin
      Name:='MarkerColorLabel';
      SetBounds(MarkerColorButton.Left+MarkerColorButton.Width+5,
                MarkerColorButton.Top+5,110,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=dlgMarkerColor;
    end;

    DesignerPaintLazyCheckBox:=TCheckBox.Create(Self);
    with DesignerPaintLazyCheckBox do begin
      Name:='DesignerPaintLazyCheckBox';
      SetBounds(x,y,w,Height);
      Parent:=FormEditMiscGroupBox;
      Caption:=lisFEPaintDesignerItemsOnIdle;
      Hint:=lisFEPaintDesignerItemsOnIdleReduceOverheadForSlowCompu;
      ShowHint:=true;
    end;
  end;
  
  procedure SetupRubberbandBox;
  begin
    RubberbandSelectColorButton:=TColorButton.Create(Self);
    with RubberbandSelectColorButton do begin
      Name:='RubberbandSelectColorButton';
      Left:=2;
      Top:=2;
      Width:=50;
      Height:=25;
      Parent:=RubberbandGroupBox;
    end;
    
    RubberbandSelectColorLabel:=TLabel.Create(Self);
    with RubberbandSelectColorLabel do begin
      Name:='RubberbandSelectColorLabel';
      Left:=RubberbandSelectColorButton.Left+RubberbandSelectColorButton.Width+2;
      Top:=RubberbandSelectColorButton.Top+2;
      Width:=100;
      Parent:=RubberbandGroupBox;
      Caption:=dlgRuberbandSelectionColor;
    end;

    RubberbandCreateColorButton:=TColorButton.Create(Self);
    with RubberbandCreateColorButton do begin
      Name:='RubberbandCreateColorButton';
      Left:=2;
      Top:=RubberbandSelectColorButton.Top+RubberbandSelectColorButton.Height+3;
      Width:=50;
      Height:=25;
      Parent:=RubberbandGroupBox;
    end;

    RubberbandCreateColorLabel:=TLabel.Create(Self);
    with RubberbandCreateColorLabel do begin
      Name:='RubberbandCreateColorLabel';
      Left:=RubberbandCreateColorButton.Left+RubberbandCreateColorButton.Width+2;
      Top:=RubberbandCreateColorButton.Top+2;
      Width:=100;
      Parent:=RubberbandGroupBox;
      Caption:=dlgRuberbandCreationColor;
    end;

    RubberbandSelectsGrandChildsCheckBox:=TCheckBox.Create(Self);
    with RubberbandSelectsGrandChildsCheckBox do begin
      Name:='RubberbandSelectsGrandChildsCheckBox';
      Left:=5;
      Top:=RubberbandCreateColorButton.Top+RubberbandCreateColorButton.Height+3;
      Width:=150;
      Parent:=RubberbandGroupBox;
      Caption:=dlgRubberbandSelectsGrandChilds;
    end;
  end;

var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  // form editor page
  NoteBook.Page[Page].OnResize:=@OnFormEditorPageResize;

  x:=5;
  y:=5;
  w:=(Notebook.Page[Page].ClientWidth-3*Left) div 2;
  h:=170;
  GridGroupBox:=TGroupBox.Create(Self);
  with GridGroupBox do begin
    Name:='GridGroupBox';
    SetBounds(x,y,w,h);
    Parent:=Notebook.Page[Page];
    Caption:=dlgEnvGrid;
    OnResize:=@GridGroupBoxResize;
  end;
  
  SetupGridGroupBox;
  
  GuideLinesGroupBox:=TGroupBox.Create(Self);
  inc(y,h+5);
  h:=Notebook.Page[Page].ClientHeight-5-y;
  with GuideLinesGroupBox do begin
    Name:='GuideLinesGroupBox';
    SetBounds(x,y,w,h);
    Left:=GridGroupBox.Left+GridGroupBox.Width+5;
    Top:=GridGroupBox.Top;
    Width:=GridGroupBox.Width;
    Height:=GridGroupBox.Height;
    Parent:=Notebook.Page[Page];
    Caption:=dlgEnvLGuideLines;
    OnResize:=@GuideLinesGroupBoxResize;
  end;
  
  SetupGuideLinesGroupBox;

  RubberbandGroupBox:=TGroupBox.Create(Self);
  inc(x,w);
  y:=5;
  h:=120;
  with RubberbandGroupBox do begin
    Name:='RubberbandGroupBox';
    SetBounds(x,y,w,h);
    Parent:=Notebook.Page[Page];
    Caption:=dlgRubberBandGroup;
    OnResize:=@RubberbandGroupBoxResize;
  end;

  SetupRubberbandBox;

  FormEditMiscGroupBox:=TGroupBox.Create(Self);
  inc(y,h+5);
  h:=Notebook.Page[Page].ClientHeight-5-y;
  with FormEditMiscGroupBox do begin
    Name:='FormEditMiscGroupBox';
    SetBounds(x,y,w,h);
    Parent:=Notebook.Page[Page];
    Caption:=dlgEnvMisc;
    OnResize:=@FormEditMiscGroupBoxResize;
  end;

  SetupMiscGroupBox;
end;

procedure TEnvironmentOptionsDialog.SetupNamingPage(Page: integer);
var
  pe: TPascalExtType;
  Space: Integer;
begin
  NoteBook.Page[Page].OnResize:=@OnNamingPageResize;
  Space:=5;

  PascalFileExtRadiogroup:=TRadioGroup.Create(Self);
  with PascalFileExtRadiogroup do begin
    Name:='PascalFileExtRadiogroup';
    SetBounds(Space,Space,300,80);
    Caption:=dlgPasExt;
    with Items do begin
      BeginUpdate;
      for pe:=Low(TPascalExtType) to High(TPascalExtType) do
        if pe<>petNone then
          Add(PascalExtension[pe]);
      EndUpdate;
    end;
    Parent:=NoteBook.Page[Page];
    AnchorParallel(akRight,Space,Parent);
  end;

  CharcaseFileActionRadioGroup:=TRadioGroup.Create(Self);
  with CharcaseFileActionRadioGroup do begin
    Name:='CharcaseFileActionRadioGroup';
    Caption:=dlgCharCaseFileAct;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvAsk);
      Add(dlgAutoRen);
      Add(dlgnoAutomaticRenaming);
      EndUpdate;
    end;
    Height:=95;
    AnchorParallel(akLeft,0,PascalFileExtRadiogroup);
    AnchorParallel(akRight,0,PascalFileExtRadiogroup);
    AnchorToNeighbour(akTop,Space,PascalFileExtRadiogroup);
    Parent:=NoteBook.Page[Page];
  end;

  AmbiguousFileActionRadioGroup:=TRadioGroup.Create(Self);
  with AmbiguousFileActionRadioGroup do begin
    Name := 'AmbiguousFileActionRadioGroup';
    Height:=150;
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
    Parent:=NoteBook.Page[Page];
    AnchorParallel(akLeft,0,CharcaseFileActionRadioGroup);
    AnchorParallel(akRight,0,CharcaseFileActionRadioGroup);
    AnchorToNeighbour(akTop,Space,CharcaseFileActionRadioGroup);
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
var
  y: Integer;
  x: Integer;
  w: Integer;
  h: Integer;
  CurParent: TWinControl;
begin
  // form editor page
  CurParent:=GridGroupBox.Parent;
  x:=5;
  y:=5;
  w:=(CurParent.ClientWidth-3*x) div 2;
  h:=170;
  with GridGroupBox do SetBounds(x,y,w,h);
  inc(y,h+5);
  h:=CurParent.ClientHeight-y-5;
  with GuideLinesGroupBox do SetBounds(x,y,w,h);
  inc(x,w+5);
  y:=5;
  h:=120;
  with RubberbandGroupBox do SetBounds(x,y,w,h);
  inc(y,h+5);
  h:=CurParent.ClientHeight-y-5;
  with FormEditMiscGroupBox do SetBounds(x,y,w,h);
end;

procedure TEnvironmentOptionsDialog.ResizeObjectInspectorPage;
var
  HalfWidth:integer;
begin
  HalfWidth:=ClientWidth div 2;
  // object inspector
  with ObjectInspectorColorsGroupBox do begin
    Left:=5;
    Top:=2;
    Width:= HalfWidth-15;
    Height:=200;
  end;

  with OIMiscGroupBox do begin
    Left:=HalfWidth+5;
    Top:=ObjectInspectorColorsGroupBox.Top;
    Width:=HalfWidth-15;
    Height:=77;
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

  with MakePathGroupBox do
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

procedure TEnvironmentOptionsDialog.MakePathGroupBoxResize(Sender: TObject);
var
  x: Integer;
  w: Integer;
begin
  w:=MakePathGroupBox.ClientWidth;
  x:=w-25;
  with MakePathComboBox do
    SetBounds(2,0,x-1-2,Height);
  with MakePathButton do
    SetBounds(x+1,0,w-2-x-1,MakePathComboBox.Height);end;

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
  w:=FormEditMiscGroupBox.ClientWidth-10;
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

  with RightClickSelectsCheckBox do begin
    SetBounds(AutoCreateFormsOnOpenCheckBox.Left,
              AutoCreateFormsOnOpenCheckBox.Top+AutoCreateFormsOnOpenCheckBox.Height+5,
              w,Height);
  end;
end;

procedure TEnvironmentOptionsDialog.GridGroupBoxResize(Sender: TObject);
var
  x: Integer;
begin
  with ShowGridCheckBox do begin
    SetBounds(6,2,Parent.ClientWidth-7,Height);
  end;

  with GridColorButton do begin
    SetBounds(ShowGridCheckBox.Left,
              ShowGridCheckBox.Top+ShowGridCheckBox.Height+5,
              50,25);
  end;

  with GridColorLabel do begin
    x:=GridColorButton.Left+GridColorButton.Width+5;
    SetBounds(x,GridColorButton.Top+2,Parent.ClientWidth-x-2,Height);
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
    X:=GridSizeXComboBox.Left+GridSizeXComboBox.Width+5;
    SetBounds(X,GridSizeXComboBox.Top+2,Parent.ClientWidth-X-2,Height);
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
  SaveDialog:=TSaveDialog.Create(nil);
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
  OpenDialog:=TOpenDialog.Create(nil);
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
      DebugLn('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick]');
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

    OIDefaultItemHeightSpinEdit.Value:=
       ObjectInspectorOptions.DefaultItemHeight;
    OIShowHintCheckBox.Checked :=
       ObjectInspectorOptions.ShowHints;
       
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

    CharCaseFileActionRadioGroup.ItemIndex  := ord(CharCaseFileAction);
    AmbiguousFileActionRadioGroup.ItemIndex := ord(AmbiguousFileAction);
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
    ObjectInspectorOptions.ShowHints :=
      OIShowHintCheckBox.Checked;

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

    CharcaseFileAction  := TCharCaseFileAction(CharcaseFileActionRadioGroup.ItemIndex);
    AmbiguousFileAction := TAmbiguousFileAction(AmbiguousFileActionRadioGroup.ItemIndex);
  end;
end;

function TEnvironmentOptionsDialog.CreateColorItem(
  ATop:Integer; AParent:TWinControl;ACaption:String):TColorButton;
var
  ColorButton:TColorButton;
  ColorLabel:TLabel;
begin
  ColorButton:=TColorButton.Create(Self);
  with ColorButton do begin
    Name:='ColorButton'+IntToStr(ATop);
    Left:=6;
    Top:=ATop;
    Width:=50;
    Height:=25;
    Parent:=AParent;
  end;

  ColorLabel:=TLabel.Create(Self);
  with ColorLabel do begin
    Name:='ColorLabel'+IntToStr(ATop);
    Left:=ColorButton.Left+ColorButton.Width+5;
    Top:=ColorButton.Top+2;
    Width:=AParent.ClientWidth-Left-5;
    Height:=23;
    Parent:=AParent;
    Caption:=ACaption;//dlgBackColor;
  end;
  Result:=ColorButton;
end;

procedure TEnvironmentOptionsDialog.SetupObjectInspectorPage(Page: integer);
var HalfWidth: integer;
begin
  NoteBook.Page[Page].OnResize:=@OnObjectInspectorPageResize;

  HalfWidth:=ClientWidth div 2;

  // object inspector
  ObjectInspectorColorsGroupBox:=TGroupBox.Create(Self);
  with ObjectInspectorColorsGroupBox do begin
    Name:='ObjectInspectorColorsGroupBox';
    Left:=5;
    Top:=2;
    Width:= HalfWidth-15;
    Height:=200;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgEnvColors;
  end;

  OIBackgroundColorButton:=CreateColorItem(5, ObjectInspectorColorsGroupBox, dlgBackColor);
  OISubPropsColorButton:=CreateColorItem(32, ObjectInspectorColorsGroupBox, dlgSubPropkColor);
  OIReferencesColorButton:=CreateColorItem(59, ObjectInspectorColorsGroupBox, dlgReferenceColor);
  OIValueColorButton:=CreateColorItem(86, ObjectInspectorColorsGroupBox, dlgValueColor);
  OIDefaultValueColorButton:=CreateColorItem(113, ObjectInspectorColorsGroupBox, dlgDefValueColor);
  OIPropNameColorButton:=CreateColorItem(140, ObjectInspectorColorsGroupBox, dlgPropNameColor);

  OIMiscGroupBox:=TGroupBox.Create(Self);
  with OIMiscGroupBox do begin
    Name:='OIMiscGroupBox';
    Left:=HalfWidth+5;
    Top:=ObjectInspectorColorsGroupBox.Top;
    Width:=HalfWidth-15;
    Height:=77;
    Parent:=NoteBook.Page[Page];
    Caption:=dlgOIMiscellaneous;
  end;
  
  OIDefaultItemHeightSpinEdit:=TSpinEdit.Create(Self);
  with OIDefaultItemHeightSpinEdit do begin
    Name:='OIDefaultItemHeightSpinEdit';
    Left:=6;
    Top:=4;
    Width:=50;
    Height:=25;
    Parent:=OIMiscGroupBox;
    Decimal_Places:=0;
    MinValue:=0;
    MaxValue:=100;
  end;
  
  OIShowHintCheckBox :=TCheckBox.Create(Self);
  with OIShowHintCheckBox do begin
    Name := 'OIShowHintCheckBox';
    Parent := OIMiscGroupBox;
    Left := 6;
    Top := 33;
    Height := 25;
    Caption := 'Show hints';
  end;

  OIDefaultItemHeightLabel:=TLabel.Create(Self);
  with OIDefaultItemHeightLabel do begin
    Name:='OIDefaultItemHeightLabel';
    Left:=OIDefaultItemHeightSpinEdit.Left+OIDefaultItemHeightSpinEdit.Width+5;
    Top:=OIDefaultItemHeightSpinEdit.Top+2;
    Width:=OIMiscGroupBox.ClientWidth-Left-5;
    Parent:=OIMiscGroupBox;
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
  4: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwProjectInspector);
  5: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwCodeExplorerName);
  6: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwPkgGraphExplorer);
  7: WindowPositionsBox.Layout:=FLayouts.ItemByEnum(nmiwUnitDependenciesName);
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

end.

