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

  ToDo:
  
}
unit EnvironmentOpts;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Forms, Controls, Buttons, GraphType, Graphics, Laz_XMLCfg,
  ObjectInspector, ExtCtrls, StdCtrls, EditorOptions, LResources, LazConf,
  Dialogs, ExtToolDialog, IDEProcs, IDEOptionDefs, InputHistory;

const
  EnvOptsVersion: integer = 101;

type
  //----------------------------------------------------------------------------
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
  
  TDebuggerType = (dtNone, dtGnuDebugger);

  TPascalExtType = (petNone, petPAS, petPP);
  
  TLazarusLanguage = (llAutomatic, llEnglish, llGerman);

const
  DebuggerName : array[TDebuggerType] of string = (
    '(None)','GNU debugger (gdb)'
  );
  
  LazarusLanguageNames: array[TLazarusLanguage] of string = (
    'Automatic (default is english)', 'English', 'Deutsch'
  );
  
  LazarusLanguageIDs: array[TLazarusLanguage] of string = (
    '', 'en', 'de'
  );
  
  PascalExtension: array[TPascalExtType] of string = ('', '.pas', '.pp');
  
type
  { class for storing environment options }
  TEnvironmentOptions = class
  private
    FFilename: string;
    FOnApplyWindowLayout: TOnApplyIDEWindowLayout;

    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    FLastSavedProjectFile: string;
    
    // window layout
    FIDEWindowLayoutList: TIDEWindowLayoutList;

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
    FAutoCreateForms: boolean;

    // object inspector
    FObjectInspectorOptions: TOIOptions;
    
    // hints
    FShowHintsForComponentPalette: boolean;
    FShowHintsForMainSpeedButtons: boolean;
    
    // compiler + debugger + lazarus files
    FLazarusDirectory: string;
    FLazarusDirsHistory: TStringList;
    FCompilerFilename: string;
    FCompilerFileHistory: TStringList;
    FFPCSourceDirectory: string;
    FFPCSourceDirHistory: TStringList;
    FDebuggerFilename: string;
    FDebuggerFileHistory: TStringList;
    FDebuggerType: TDebuggerType;
    FTestBuildDirectory: string;
    FTestBuildDirHistory: TStringList;

    // recent files and directories
    FRecentOpenFiles: TStringList;
    FMaxRecentOpenFiles: integer;
    FRecentProjectFiles: TStringList;
    FMaxRecentProjectFiles: integer;
    FOpenLastProjectAtStart: boolean;

    // backup
    FBackupInfoProjectFiles: TBackupInfo;
    FBackupInfoOtherFiles: TBackupInfo;
    
    // external tools
    fExternalTools: TExternalToolList;
    
    // naming conventions
    fPascalFileExtension: TPascalExtType;
    fPascalFileLowerCase: boolean;
    fAutoDeleteAmbigiousSources: boolean;
    
    // language
    fLanguage: TLazarusLanguage;
    
    procedure SetOnApplyWindowLayout(const AValue: TOnApplyIDEWindowLayout);

    procedure SetFileName(const NewFilename: string);
    procedure InitLayoutList;
    procedure InternOnApplyWindowLayout(ALayout: TIDEWindowLayout);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(OnlyDesktop:boolean);
    procedure Save(OnlyDesktop:boolean);
    property Filename: string read FFilename write SetFilename;
    procedure SetLazarusDefaultFilename;
    property OnApplyWindowLayout: TOnApplyIDEWindowLayout
       read FOnApplyWindowLayout write SetOnApplyWindowLayout;
    
    // auto save
    property AutoSaveEditorFiles: boolean
       read FAutoSaveEditorFiles write FAutoSaveEditorFiles;
    property AutoSaveProject: boolean
       read FAutoSaveProject write FAutoSaveProject;
    property AutoSaveIntervalInSecs: integer
       read FAutoSaveIntervalInSecs write FAutoSaveIntervalInSecs;

    // windows
    property IDEWindowLayoutList: TIDEWindowLayoutList
        read FIDEWindowLayoutList write FIDEWindowLayoutList;

    // form editor
    property ShowGrid: boolean read FShowGrid write FShowGrid;
    property SnapToGrid: boolean read FSnapToGrid write FSnapToGrid;
    property GridColor: TColor read FGridColor write FGridColor;
    property GridSizeX: integer read FGridSizeX write FGridSizeX;
    property GridSizeY: integer read FGridSizeY write FGridSizeY;
    property ShowGuideLines: boolean read FShowGuideLines write FShowGuideLines;
    property SnapToGuideLines: boolean read FSnapToGuideLines write FSnapToGuideLines;
    property GuideLineColorLeftTop: TColor
       read FGuideLineColorLeftTop write FGuideLineColorLeftTop;
    property GuideLineColorRightBottom: TColor
       read FGuideLineColorRightBottom write FGuideLineColorRightBottom;
    property ShowComponentCaptions: boolean
       read FShowComponentCaptions write FShowComponentCaptions;
    property ShowEditorHints: boolean read FShowEditorHints write FShowEditorHints;
    property AutoCreateForms: boolean read FAutoCreateForms write FAutoCreateForms;

    // object inspector
    property ObjectInspectorOptions: TOIOptions
       read FObjectInspectorOptions write FObjectInspectorOptions;

    // hints
    property ShowHintsForComponentPalette: boolean
       read FShowHintsForComponentPalette write FShowHintsForComponentPalette;
    property ShowHintsForMainSpeedButtons: boolean
       read FShowHintsForMainSpeedButtons write FShowHintsForMainSpeedButtons;
    
    // files
    property LazarusDirectory: string
       read FLazarusDirectory write FLazarusDirectory;
    property LazarusDirHistory: TStringList
       read FLazarusDirsHistory write FLazarusDirsHistory;
    property CompilerFilename: string
       read FCompilerFilename write FCompilerFilename;
    property CompilerFileHistory: TStringList
       read FCompilerFileHistory write FCompilerFileHistory;
    property FPCSourceDirectory: string
       read FFPCSourceDirectory write FFPCSourceDirectory;
    property FPCSourceDirHistory: TStringList
       read FFPCSourceDirHistory write FFPCSourceDirHistory;
    property DebuggerFilename: string
       read FDebuggerFilename write FDebuggerFilename;
    property DebuggerFileHistory: TStringList
       read FDebuggerFileHistory write FDebuggerFileHistory;
    property DebuggerType: TDebuggerType
       read FDebuggerType write FDebuggerType;
    property TestBuildDirectory: string
       read FTestBuildDirectory write FTestBuildDirectory;
    property TestBuildDirHistory: TStringList
       read FTestBuildDirHistory write FTestBuildDirHistory;

    // recent files and directories
    property RecentOpenFiles: TStringList
       read FRecentOpenFiles write FRecentOpenFiles;
    property MaxRecentOpenFiles: integer
       read FMaxRecentOpenFiles write FMaxRecentOpenFiles;
    procedure AddToRecentOpenFiles(const AFilename: string);
    procedure RemoveFromRecentOpenFiles(const AFilename: string);
    property RecentProjectFiles: TStringList
       read FRecentProjectFiles write FRecentProjectFiles;
    property MaxRecentProjectFiles: integer
       read FMaxRecentProjectFiles write FMaxRecentProjectFiles;
    procedure AddToRecentProjectFiles(const AFilename: string);
    procedure RemoveFromRecentProjectFiles(const AFilename: string);
    property LastSavedProjectFile: string
       read FLastSavedProjectFile write FLastSavedProjectFile;
    property OpenLastProjectAtStart: boolean
       read FOpenLastProjectAtStart write FOpenLastProjectAtStart;

    // backup
    property BackupInfoProjectFiles: TBackupInfo 
       read FBackupInfoProjectFiles write FBackupInfoProjectFiles;
    property BackupInfoOtherFiles: TBackupInfo
       read FBackupInfoOtherFiles write FBackupInfoOtherFiles;
       
    // external tools
    property ExternalTools: TExternalToolList
       read fExternalTools write fExternalTools;

    // naming conventions
    property PascalFileExtension: TPascalExtType 
       read fPascalFileExtension write fPascalFileExtension;
    property PascalFileLowerCase: boolean
       read fPascalFileLowerCase write fPascalFileLowerCase;
    property AutoDeleteAmbigiousSources: boolean
       read fAutoDeleteAmbigiousSources write fAutoDeleteAmbigiousSources;
       
    // language
    property Language: TLazarusLanguage read fLanguage write fLanguage;
  end;

  //----------------------------------------------------------------------------

  TOnLoadEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;
  TOnSaveEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;

  { form for environment options }
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
    
    // window positions
    WindowPositionsGroupBox: TGroupBox;
    WindowPositionsListBox: TListBox;
    WindowPositionsBox: TIDEWindowSetupLayoutComponent;

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
    AutoCreateFormsCheckBox: TCheckBox;

    // object inspector
    ObjectInspectorGroupBox: TGroupBox;
    OIBackgroundColorLabel: TLabel;
    OIBackgroundColorButton: TColorButton;

    // Files
    MaxRecentOpenFilesLabel: TLabel;
    MaxRecentOpenFilesComboBox: TComboBox;
    MaxRecentProjectFilesLabel: TLabel;
    MaxRecentProjectFilesComboBox: TComboBox;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    LazarusDirLabel: TLabel;
    LazarusDirComboBox: TComboBox;
    CompilerPathLabel: TLabel;
    CompilerPathComboBox: TComboBox;
    FPCSourceDirLabel: TLabel;
    FPCSourceDirComboBox: TComboBox;
    DebuggerPathLabel: TLabel;
    DebuggerPathComboBox: TComboBox;
    DebuggerTypeComboBox: TComboBox;
    TestBuildDirLabel: TLabel;
    TestBuildDirComboBox: TComboBox;

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
    PascalFileLowercaseCheckBox: TCheckBox;
    AutoDeleteAmbigiousSourcesCheckBox: TCheckBox;

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
    procedure BakTypeRadioGroupClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveDesktopSettingsToFileButtonClick(Sender: TObject);
    procedure LoadDesktopSettingsFromFileButtonClick(Sender: TObject);
    procedure WindowPositionsListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure EnvironmentOptionsDialogResize(Sender: TObject);
    procedure WindowPositionsGroupBoxResize(Sender: TObject);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FLayouts: TIDEWindowLayoutList;
    procedure SetupDesktopPage(Page: integer);
    procedure SetupFormEditorPage(Page: integer);
    procedure SetupObjectInspectorPage(Page: integer);
    procedure SetupFilesPage(Page: integer);
    procedure SetupBackupPage(Page: integer);
    procedure SetupNamingPage(Page: integer);
    procedure ResizeDesktopPage;
    procedure ResizeFormEditorPage;
    procedure ResizeObjectInspectorPage;
    procedure ResizeFilesPage;
    procedure ResizeBackupPage;
    procedure ResizeNamingPage;
    procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString);
    procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString;
                              MaxCount: integer);
    procedure SetWindowPositionsItem(Index: integer);
  published
    property OnSaveEnvironmentSettings: TOnSaveEnvironmentSettings
      read FOnSaveEnvironmentSettings write FOnSaveEnvironmentSettings;
    property OnLoadEnvironmentSettings: TOnLoadEnvironmentSettings
      read FOnLoadEnvironmentSettings write FOnLoadEnvironmentSettings;
  public
    procedure ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
    procedure WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
    constructor Create(AOwner:TComponent);  override;
    destructor Destroy; override;
  end;


var
  EnvironmentOptions: TEnvironmentOptions;

function DebuggerNameToType(const s: string): TDebuggerType;
function PascalExtToType(const Ext: string): TPascalExtType;
function FilenameIsPascalUnit(const Filename: string): boolean;
function FilenameIsPascalSource(const Filename: string): boolean;
function FilenameIsFormText(const Filename: string): boolean;


implementation


const MaxComboBoxCount: integer = 20;

function FilenameIsPascalSource(const Filename: string): boolean;
var Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=(Ext='.pp') or (Ext='.pas') or (Ext='.lpr')
          or (Ext='.dpr') or (Ext='.dpk');
end;

function FilenameIsPascalUnit(const Filename: string): boolean;
var Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=(Ext='.pp') or (Ext='.pas');
end;

function FilenameIsFormText(const Filename: string): boolean;
var Ext: string;
begin
  Ext:=lowercase(ExtractFileExt(Filename));
  Result:=(Ext='.lfm') or (Ext='.dfm') or (Ext='.xfm');
end;

function DebuggerNameToType(const s: string): TDebuggerType;
begin
  for Result:=Low(TDebuggerType) to High(TDebuggerType) do
    if UpperCase(DebuggerName[Result])=uppercase(s) then exit;
  Result:=dtNone;
end;

function PascalExtToType(const Ext: string): TPascalExtType;
begin
  if Ext<>'' then
    for Result:=Low(TPascalExtType) to High(TPascalExtType) do
      if CompareFilenames(Ext,PascalExtension[Result])=0 then exit;
  Result:=petNone;
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
  FShowEditorHints:=false;
  FAutoCreateForms:=true;

  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;
  
  // hints
  FShowHintsForComponentPalette:=false;
  FShowHintsForMainSpeedButtons:=false;

  // files
  FLazarusDirectory:=ExpandFilename(ExtractFilePath(ParamStr(0)));
  FLazarusDirsHistory:=TStringList.Create;
  FCompilerFilename:='';
  FCompilerFileHistory:=TStringList.Create;
  FFPCSourceDirectory:='';
  FFPCSourceDirHistory:=TStringList.Create;
  FDebuggerFilename:='';
  FDebuggerFileHistory:=TStringList.Create;
  FDebuggerType:=dtNone;
  FTestBuildDirectory:={$ifdef win32}'c:/temp'{$else}'/tmp'{$endif};
  FTestBuildDirHistory:=TStringList.Create;

  // recent files and directories
  FRecentOpenFiles:=TStringList.Create;
  FMaxRecentOpenFiles:=10;
  FRecentProjectFiles:=TStringList.Create;
  FMaxRecentProjectFiles:=5;
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
  
  fPascalFileExtension:=petPAS;
  fPascalFileLowerCase:=true;
end;

destructor TEnvironmentOptions.Destroy;
begin
  fExternalTools.Free;
  FRecentOpenFiles.Free;
  FRecentProjectFiles.Free;
  FObjectInspectorOptions.Free;
  FLazarusDirsHistory.Free;
  FCompilerFileHistory.Free;
  FFPCSourceDirHistory.Free;
  FDebuggerFileHistory.Free;
  FTestBuildDirHistory.Free;
  fIDEWindowLayoutList.Free;
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
  FFilename:=ConfFilename;
end;

procedure TEnvironmentOptions.SetFileName(const NewFilename: string);
begin
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
end;

procedure TEnvironmentOptions.Load(OnlyDesktop:boolean);
var XMLConfig: TXMLConfig;
  FileVersion: integer;

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
  var i:integer;
  begin
    i:=XMLConfig.GetValue(Path+'DebuggerType/Value',5);
    case i of
    1:ADebuggerType:=dtGnuDebugger;
    else
      ADebuggerType:=dtNone;
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
    XMLConfig:=TXMLConfig.Create(FFileName);
    FileVersion:=XMLConfig.GetValue('EnvironmentOptions/Version/Value',0);
    
    // language
    LoadLanguage;

    // auto save
    FAutoSaveEditorFiles:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/EditorFiles',FAutoSaveEditorFiles);
    FAutoSaveProject:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/Project',FAutoSaveProject);
    FAutoSaveIntervalInSecs:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/IntervalInSecs',FAutoSaveIntervalInSecs);
    FLastSavedProjectFile:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/LastSavedProjectFile',FLastSavedProjectFile);
    FOpenLastProjectAtStart:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/OpenLastProjectAtStart',
       FOpenLastProjectAtStart);

    // windows
    FIDEWindowLayoutList.LoadFromXMLConfig(XMLConfig,
      'EnvironmentOptions/Desktop/');

    // form editor
    FShowGrid:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowGrid',FShowGrid);
    FGridColor:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridColor',FGridColor);
    FSnapToGrid:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/SnapToGrid',FSnapToGrid);
    FGridSizeX:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridSizeX',FGridSizeX);
    FGridSizeY:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridSizeY',FGridSizeY);
    FShowGuideLines:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowGuideLines',FShowGuideLines);
    FSnapToGuideLines:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/SnapToGuideLines',FSnapToGuideLines);
    FGuideLineColorLeftTop:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GuideLineColorLeftTop',
       FGuideLineColorLeftTop);
    FGuideLineColorRightBottom:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GuideLineColorRightBottom',
       FGuideLineColorRightBottom);
    FShowComponentCaptions:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowComponentCaptions',FShowComponentCaptions);
    FShowEditorHints:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowEditorHints',FShowEditorHints);
    FAutoCreateForms:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/AutoCreateForms',FAutoCreateForms);

    if not OnlyDesktop then begin
      // files
      FLazarusDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/LazarusDirectory/Value',FLazarusDirectory);
      LoadRecentList(XMLConfig,FLazarusDirsHistory,
         'EnvironmentOptions/LazarusDirectory/History/');
      if FLazarusDirsHistory.Count=0 then begin
        FLazarusDirsHistory.Add(ExtractFilePath(ExpandFilename(ParamStr(0))));
      end;
      FCompilerFilename:=XMLConfig.GetValue(
         'EnvironmentOptions/CompilerFilename/Value',FCompilerFilename);
      LoadRecentList(XMLConfig,FCompilerFileHistory,
         'EnvironmentOptions/CompilerFilename/History/');
      if FCompilerFileHistory.Count=0 then begin
        {$IFDEF win32}
        FCompilerFileHistory.Add('c:/pp/bin/ppc386');
        {$ELSE}
        FCompilerFileHistory.Add('/usr/bin/ppc386');
        FCompilerFileHistory.Add('/opt/fpc/ppc386');
        {$ENDIF}
      end;
      FFPCSourceDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/FPCSourceDirectory/Value',FFPCSourceDirectory);
      LoadRecentList(XMLConfig,FFPCSourceDirHistory,
         'EnvironmentOptions/FPCSourceDirectory/History/');
      if FFPCSourceDirHistory.Count=0 then begin
      
      end;
      FDebuggerFilename:=XMLConfig.GetValue(
         'EnvironmentOptions/DebuggerFilename/Value',FDebuggerFilename);
      LoadRecentList(XMLConfig,FDebuggerFileHistory,
         'EnvironmentOptions/DebuggerFilename/History/');
      if FDebuggerFileHistory.Count=0 then begin
        FDebuggerFileHistory.Add(DebuggerName[dtNone]);
        FDebuggerFileHistory.Add('/usr/bin/gdb');
        FDebuggerFileHistory.Add('/opt/fpc/gdb');
      end;
      LoadDebuggerType(FDebuggerType,'EnvironmentOptions/');
      FTestBuildDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/TestBuildDirectory/Value',FTestBuildDirectory);
      LoadRecentList(XMLConfig,FTestBuildDirHistory,
         'EnvironmentOptions/TestBuildDirectory/History/');
      if FTestBuildDirHistory.Count=0 then begin
        {$IFDEF win32}
        FTestBuildDirHistory.Add('c:/tmp');
        FTestBuildDirHistory.Add('c:/windows/temp');
        {$ELSE}
        FTestBuildDirHistory.Add('/tmp');
        FTestBuildDirHistory.Add('/var/tmp');
        {$ENDIF}
      end;

      // backup
      LoadBackupInfo(FBackupInfoProjectFiles
        ,'EnvironmentOptions/BackupProjectFiles/');
      LoadBackupInfo(FBackupInfoOtherFiles
        ,'EnvironmentOptions/BackupOtherFiles/');
    end;

    // hints
    FShowHintsForComponentPalette:=XMLConfig.GetValue(
      'EnvironmentOptions/ShowHintsForComponentPalette/Value',
      FShowHintsForComponentPalette);
    FShowHintsForMainSpeedButtons:=XMLConfig.GetValue(
      'EnvironmentOptions/ShowHintsForMainSpeedButtons/Value',
      FShowHintsForMainSpeedButtons);

    // recent files and directories
    FMaxRecentOpenFiles:=XMLConfig.GetValue(
      'EnvironmentOptions/Recent/OpenFiles/Max',FMaxRecentOpenFiles);
    LoadRecentList(XMLConfig,FRecentOpenFiles,
      'EnvironmentOptions/Recent/OpenFiles/');
    FMaxRecentProjectFiles:=XMLConfig.GetValue(
      'EnvironmentOptions/Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
    LoadRecentList(XMLConfig,FRecentProjectFiles,
      'EnvironmentOptions/Recent/ProjectFiles/');

    // external tools
    fExternalTools.Load(XMLConfig,'EnvironmentOptions/ExternalTools/');
    
    // naming
    LoadPascalFileExt('EnvironmentOptions/');
    fPascalFileLowerCase:=XMLConfig.GetValue(
      'EnvironmentOptions/PascalFileLowerCase/Value',true);
    fAutoDeleteAmbigiousSources:=XMLConfig.GetValue(
      'EnvironmentOptions/AutoDeleteAmbigiousSources/Value',true);

    XMLConfig.Free;

    // object inspector
    FObjectInspectorOptions.Filename:=FFilename;
    FObjectInspectorOptions.Load;
    FObjectInspectorOptions.SaveBounds:=false;
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
      XMLConfig.SetValue(Path+'Type',i);
      XMLConfig.SetValue(Path+'AdditionalExtension',AdditionalExtension);
      XMLConfig.SetValue(Path+'MaxCounter',MaxCounter);
      XMLConfig.SetValue(Path+'SubDirectory',SubDirectory);
    end;
  end;

  procedure SaveDebuggerType(ADebuggerType: TDebuggerType; Path:string);
  var i:integer;
  begin
    case ADebuggerType of
     dtNone: i:=0;
     dtGnuDebugger: i:=1;
    end;
    XMLConfig.SetValue(Path+'DebuggerType/Value',i);
  end;
  

begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    XMLConfig.SetValue('EnvironmentOptions/Version/Value',EnvOptsVersion);

    // language
    XMLConfig.SetValue('EnvironmentOptions/Language/ID'
       ,LazarusLanguageIDs[fLanguage]);

    // auto save
    XMLConfig.SetValue('EnvironmentOptions/AutoSave/EditorFiles'
       ,FAutoSaveEditorFiles);
    XMLConfig.SetValue('EnvironmentOptions/AutoSave/Project',FAutoSaveProject);
    XMLConfig.SetValue('EnvironmentOptions/AutoSave/IntervalInSecs'
       ,FAutoSaveIntervalInSecs);
    XMLConfig.SetValue('EnvironmentOptions/AutoSave/LastSavedProjectFile'
       ,FLastSavedProjectFile);
    XMLConfig.SetValue(
       'EnvironmentOptions/AutoSave/OpenLastProjectAtStart',
       FOpenLastProjectAtStart);

    // windows
    FIDEWindowLayoutList.SaveToXMLConfig(XMLConfig,
      'EnvironmentOptions/Desktop/');

    // form editor
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/ShowGrid',FShowGrid);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/GridColor',FGridColor);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/SnapToGrid',FSnapToGrid);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/GridSizeX',FGridSizeX);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/GridSizeY',FGridSizeY);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/ShowGuideLines',FShowGuideLines);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/SnapToGuideLines',FSnapToGuideLines);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/GuideLineColorLeftTop',
       FGuideLineColorLeftTop);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/GuideLineColorRightBottom',
       FGuideLineColorRightBottom);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/ShowComponentCaptions',
       FShowComponentCaptions);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/ShowEditorHints',FShowEditorHints);
    XMLConfig.SetValue(
       'EnvironmentOptions/FormEditor/AutoCreateForms',FAutoCreateForms);

    if not OnlyDesktop then begin
      // files
      XMLConfig.SetValue(
         'EnvironmentOptions/LazarusDirectory/Value',FLazarusDirectory);
      SaveRecentList(XMLConfig,FLazarusDirsHistory,
         'EnvironmentOptions/LazarusDirectory/History/');
      XMLConfig.SetValue(
         'EnvironmentOptions/CompilerFilename/Value',FCompilerFilename);
      SaveRecentList(XMLConfig,FCompilerFileHistory,
         'EnvironmentOptions/CompilerFilename/History/');
      XMLConfig.SetValue(
         'EnvironmentOptions/FPCSourceDirectory/Value',FFPCSourceDirectory);
      SaveRecentList(XMLConfig,FFPCSourceDirHistory,
         'EnvironmentOptions/FPCSourceDirectory/History/');
      XMLConfig.SetValue(
         'EnvironmentOptions/DebuggerFilename/Value',FDebuggerFilename);
      SaveRecentList(XMLConfig,FDebuggerFileHistory,
         'EnvironmentOptions/DebuggerFilename/History/');
      SaveDebuggerType(DebuggerType,'EnvironmentOptions/');
      XMLConfig.SetValue(
         'EnvironmentOptions/TestBuildDirectory/Value',FTestBuildDirectory);
      SaveRecentList(XMLConfig,FTestBuildDirHistory,
         'EnvironmentOptions/TestBuildDirectory/History/');

      // backup
      SaveBackupInfo(FBackupInfoProjectFiles
        ,'EnvironmentOptions/BackupProjectFiles/');
      SaveBackupInfo(FBackupInfoOtherFiles
        ,'EnvironmentOptions/BackupOtherFiles/');
    end;

    // hints
    XMLConfig.SetValue('EnvironmentOptions/ShowHintsForComponentPalette/Value',
      FShowHintsForComponentPalette);
    XMLConfig.SetValue('EnvironmentOptions/ShowHintsForMainSpeedButtons/Value',
      FShowHintsForMainSpeedButtons);

    // recent files and directories
    XMLConfig.SetValue(
      'EnvironmentOptions/Recent/OpenFiles/Max',FMaxRecentOpenFiles);
    SaveRecentList(XMLConfig,FRecentOpenFiles,
      'EnvironmentOptions/Recent/OpenFiles/');
    XMLConfig.SetValue(
      'EnvironmentOptions/Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
    SaveRecentList(XMLConfig,FRecentProjectFiles,
      'EnvironmentOptions/Recent/ProjectFiles/');

    // external tools
    fExternalTools.Save(XMLConfig,'EnvironmentOptions/ExternalTools/');

    // naming
    XMLConfig.SetValue('EnvironmentOptions/Naming/PascalFileExtension',
      PascalExtension[fPascalFileExtension]);
    XMLConfig.SetValue('EnvironmentOptions/PascalFileLowerCase/Value',
      fPascalFileLowerCase);
    XMLConfig.SetValue('EnvironmentOptions/AutoDeleteAmbigiousSources/Value',
      fAutoDeleteAmbigiousSources);

    XMLConfig.Flush;
    XMLConfig.Free;

    // object inspector
    FObjectInspectorOptions.Filename:=FFilename;
    FObjectInspectorOptions.SaveBounds:=false;
    FObjectInspectorOptions.Save;
    
  except
    // ToDo
    writeln('[TEnvironmentOptions.Save]  error writing "',FFilename,'"');
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
  NewLayout: TIDEWindowLayout;
  i: integer;
begin
  fIDEWindowLayoutList:=TIDEWindowLayoutList.Create;

  // Main IDE bar
  NewLayout:=TIDEWindowLayout.Create;
  with NewLayout do begin
    FormID:=DefaultMainIDEName;
    WindowPlacementsAllowed:=[iwpRestoreWindowGeometry,iwpDefault,
       iwpCustomPosition,iwpUseWindowManagerSetting];
  end;
  IDEWindowLayoutList.Add(NewLayout);

  // object inspector
  NewLayout:=TIDEWindowLayout.Create;
  with NewLayout do begin
    FormID:=DefaultObjectInspectorName;
    WindowPlacementsAllowed:=[iwpRestoreWindowGeometry,iwpDefault,
       iwpCustomPosition,iwpUseWindowManagerSetting];
  end;
  IDEWindowLayoutList.Add(NewLayout);

  // source editor
  NewLayout:=TIDEWindowLayout.Create;
  with NewLayout do begin
    FormID:=DefaultSourceNoteBookName;
    WindowPlacementsAllowed:=[iwpRestoreWindowGeometry,iwpDefault,
       iwpCustomPosition,iwpUseWindowManagerSetting];
  end;
  IDEWindowLayoutList.Add(NewLayout);

  // messages view
  NewLayout:=TIDEWindowLayout.Create;
  with NewLayout do begin
    FormID:=DefaultMessagesViewName;
    WindowPlacementsAllowed:=[iwpRestoreWindowGeometry,iwpDefault,
       iwpCustomPosition,iwpUseWindowManagerSetting];
  end;
  IDEWindowLayoutList.Add(NewLayout);
  
  for i:=0 to fIDEWindowLayoutList.Count-1 do begin
    IDEWindowLayoutList[i].OnApply:=@InternOnApplyWindowLayout;
    IDEWindowLayoutList[i].DefaultWindowPlacement:=iwpRestoreWindowGeometry;
  end;
end;

procedure TEnvironmentOptions.InternOnApplyWindowLayout(
  ALayout: TIDEWindowLayout);
begin
  if Assigned(OnApplyWindowLayout) then OnApplyWindowLayout(ALayout);
end;

procedure TEnvironmentOptions.SetOnApplyWindowLayout(
  const AValue: TOnApplyIDEWindowLayout);
begin
  FOnApplyWindowLayout:=AValue;
end;

//==============================================================================

{ TEnvironmentOptionsDialog }

constructor TEnvironmentOptionsDialog.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=485;
    Height:=435;
    Position:=poScreenCenter;
    Caption:='Environment Options';
    OnResize:=@EnvironmentOptionsDialogResize;
    
    NoteBook:=TNoteBook.Create(Self);
    with NoteBook do begin
      Name:='NoteBook';
      Parent:=Self;
      SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
      if PageCount>0 then
        Pages[0]:='Desktop'
      else
        Pages.Add('Desktop');
      Pages.Add('Form Editor');
      Pages.Add('Object Inspector');
      Pages.Add('Files');
      Pages.Add('Backup');
      Pages.Add('Naming');
    end;

    SetupDesktopPage(0);
    SetupFormEditorPage(1);
    SetupObjectInspectorPage(2);
    SetupFilesPage(3);
    SetupBackupPage(4);
    SetupNamingPage(5);
    
    NoteBook.Show;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Width:=70;
      Height:=23;
      Left:=Self.ClientWidth-Width-15;
      Top:=Self.ClientHeight-Height-15;
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Width:=CancelButton.Width;
      Height:=CancelButton.Height;
      Left:=CancelButton.Left-15-Width;
      Top:=CancelButton.Top;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;

  end;
  EnvironmentOptionsDialogResize(nil);
end;

destructor TEnvironmentOptionsDialog.Destroy;
begin
  inherited Destroy;
end;

procedure TEnvironmentOptionsDialog.SetupDesktopPage(Page: integer);
var MaxX:integer;
  l: TLazarusLanguage;
begin
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
    Caption:='Language';
    Visible:=true;
  end;
  
  LanguageComboBox:=TComboBox.Create(Self);
  with LanguageComboBox do begin
    Name:='LanguageComboBox';
    Parent:=LanguageGroupBox;
    Left:=5;
    Top:=3;
    Width:=LanguageGroupBox.ClientWidth-2*Left;
    with Items do begin
      BeginUpdate;
      for l:=Low(TLazarusLanguage) to High(TLazarusLanguage) do begin
        Add(LazarusLanguageNames[l]+' ['+LazarusLanguageIDs[l]+']');
      end;
      EndUpdate;
    end;
    Visible:=true;
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
    Caption:='Auto save';
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
    Caption:='Editor files';
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
    Caption:='Project';
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
    Caption:='Interval in secs';
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
    Height:=23;
    with Items do begin
      BeginUpdate;
      Add('1200');
      Add('600');
      Add('300');
      Add('120');
      EndUpdate;
    end;
    Enabled:=false;
    Visible:=true;
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
    Caption:='Desktop files';
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
    Caption:='Save desktop settings to file';
    OnClick:=@SaveDesktopSettingsToFileButtonClick;
    Visible:=true;
  end;

  LoadDesktopSettingsFromFileButton:=TButton.Create(Self);
  with LoadDesktopSettingsFromFileButton do begin
    Name:='LoadDesktopSettingsFromFileButton';
    Parent:=DesktopFilesGroupBox;
    Left:=5;
    Top:=38;
    Width:=SaveDesktopSettingsToFileButton.Width;
    Height:=25;
    Caption:='Load desktop settings from file';
    OnClick:=@LoadDesktopSettingsFromFileButtonClick;
    Visible:=true;
  end;

  // hints
  ShowHintsForComponentPaletteCheckBox:=TCheckBox.Create(Self);
  with ShowHintsForComponentPaletteCheckBox do begin
    Name:='ShowHintsForComponentPaletteCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=DesktopFilesGroupBox.Left;
    Top:=DesktopFilesGroupBox.Top+DesktopFilesGroupBox.Height+100;
    Width:=Parent.ClientWidth-Left;
    Height:=20;
    Caption:='Hints for component palette';
    Visible:=true;
  end;
  
  ShowHintsForMainSpeedButtonsCheckBox:=TCheckBox.Create(Self);
  with ShowHintsForMainSpeedButtonsCheckBox do begin
    Name:='ShowHintsForMainSpeedButtonsCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=ShowHintsForComponentPaletteCheckBox.Left;
    Top:=ShowHintsForComponentPaletteCheckBox.Top
         +ShowHintsForComponentPaletteCheckBox.Height+5;
    Width:=Parent.ClientWidth-Left;
    Height:=20;
    Caption:='Hints for main speed buttons (open, save, ...)';
    Visible:=true;
  end;

  // Window Positions
  WindowPositionsGroupBox:=TGroupBox.Create(Self);
  with WindowPositionsGroupBox do begin
    Name:='WindowPositionsGroupBox';
    Parent:=NoteBook.Page[Page];
    Caption:='Window Positions';
    SetBounds(MaxX div 2,LanguageGroupBox.Top,(MaxX div 2)-5,290);
    OnResize:=@WindowPositionsGroupBoxResize;
    Visible:=true;
  end;
  
  WindowPositionsListBox:=TListBox.Create(Self);
  with WindowPositionsListBox do begin
    Name:='WindowPositionsListBox';
    Parent:=WindowPositionsGroupBox;
    SetBounds(5,5,Parent.ClientWidth-15,60);
    with Items do begin
      BeginUpdate;
      Add('Main Menu');
      Add('Source Editor');
      Add('Messages');
      Add('Object Inspector');
      EndUpdate;
    end;
    OnMouseUp:=@WindowPositionsListBoxMouseUp;
    Visible:=true;
  end;

  WindowPositionsBox:=TIDEWindowSetupLayoutComponent.Create(Self);
  with WindowPositionsBox do begin
    Name:='WindowPositionsBox';
    Parent:=WindowPositionsGroupBox;
    Left:=5;
    Top:=WindowPositionsListBox.Top+WindowPositionsListBox.Height+5;
    Width:=WindowPositionsListBox.Width;
    Height:=Parent.ClientHeight-Top-20;
    Visible:=true;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupBackupPage(Page: integer);
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  BackupHelpLabel:=TLabel.Create(Self);
  with BackupHelpLabel do begin
    Name:='BackupHelpLabel';
    Parent:=NoteBook.Page[Page];
    Left:=5;
    Top:=2;
    Width:=MaxX-Left*2;
    Height:=23;
    Caption:='Notes: ';
    Visible:=true;
  end;

  BackupProjectGroupBox:=TGroupBox.Create(Self);
  with BackupProjectGroupBox do begin
    Name:='BackupProjectGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Caption:='Project files';
    Visible:=true;
  end;

  BakProjTypeRadioGroup:=TRadioGroup.Create(Self);
  with BakProjTypeRadioGroup do begin
    Name:='BakProjTypeRadioGroup';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=4;
    Width:=BackupProjectGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
    Caption:='Type';
    with Items do begin
      BeginUpdate;
      Add('None');
      Add('Symbol in front (.~pp)');
      Add('Symbol behind (.pp~)');
      Add('Counter (.pp;1)');
      Add('User defined extension (.pp.xxx)');
      Add('Same name (in subdirectory)');
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
    Caption:='User defined extension';
    Visible:=true;
  end;

  BakProjAddExtComboBox:=TComboBox.Create(Self);
  with BakProjAddExtComboBox do begin
    Name:='BakProjAddExtComboBox';
    Parent:=BackupProjectGroupBox;
    Left:=BakProjAddExtLabel.Left+BakProjAddExtLabel.Width+2;
    Top:=BakProjAddExtLabel.Top;
    Width:=60;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('bak');
      Add('old');
      EndUpdate;
    end;
    Visible:=true;
  end;

  BakProjMaxCounterLabel:=TLabel.Create(Self);
  with BakProjMaxCounterLabel do begin
    Name:='BakProjMaxCounterLabel';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=BakProjAddExtLabel.Top+BakProjAddExtLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:='Maximum counter';
    Visible:=true;
  end;

  BakProjMaxCounterComboBox:=TComboBox.Create(Self);
  with BakProjMaxCounterComboBox do begin
    Name:='BakProjMaxCounterComboBox';
    Parent:=BackupProjectGroupBox;
    Left:=BakProjMaxCounterLabel.Left+BakProjMaxCounterLabel.Width+2;
    Top:=BakProjMaxCounterLabel.Top;
    Width:=100;
    Height:=25;
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
    Visible:=true;
  end;

  BakProjSubDirLabel:=TLabel.Create(Self);
  with BakProjSubDirLabel do begin
    Name:='BakProjSubDirLabel';
    Parent:=BackupProjectGroupBox;
    Left:=5;
    Top:=BakProjMaxCounterLabel.Top+BakProjMaxCounterLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:='Sub directory';
    Visible:=true;
  end;

  BakProjSubDirComboBox:=TComboBox.Create(Self);
  with BakProjSubDirComboBox do begin
    Name:='BakProjSubDirComboBox';
    Parent:=BackupProjectGroupBox;
    Left:=BakProjSubDirLabel.Left+BakProjSubDirLabel.Width+2;
    Top:=BakProjSubDirLabel.Top;
    Width:=100;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add(BakNoSubDirTxt);
      Add('backup');
      EndUpdate;
    end;
    Visible:=true;
  end;

  BackupOtherGroupBox:=TGroupBox.Create(Self);
  with BackupOtherGroupBox do begin
    Name:='BackupOtherGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=BackupProjectGroupBox.Left+BackupProjectGroupBox.Width+10;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Caption:='Other files';
    Visible:=true;
  end;

  BakOtherTypeRadioGroup:=TRadioGroup.Create(Self);
  with BakOtherTypeRadioGroup do begin
    Name:='BakOtherTypeRadioGroup';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=4;
    Width:=BackupOtherGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
    Caption:='Type';
    with Items do begin
      BeginUpdate;
      Add('None');
      Add('Symbol in front (.~pp)');
      Add('Symbol behind (.pp~)');
      Add('Counter (.pp;1)');
      Add('User defined extension (.pp.xxx)');
      Add('Same name (in subdirectory)');
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
    Caption:='User defined extension';
    Visible:=true;
  end;

  BakOtherAddExtComboBox:=TComboBox.Create(Self);
  with BakOtherAddExtComboBox do begin
    Name:='BakOtherAddExtComboBox';
    Parent:=BackupOtherGroupBox;
    Left:=BakOtherAddExtLabel.Left+BakOtherAddExtLabel.Width+2;
    Top:=BakOtherAddExtLabel.Top;
    Width:=60;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('bak');
      Add('old');
      EndUpdate;
    end;
    Visible:=true;
  end;

  BakOtherMaxCounterLabel:=TLabel.Create(Self);
  with BakOtherMaxCounterLabel do begin
    Name:='BakOtherMaxCounterLabel';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=BakOtherAddExtLabel.Top+BakOtherAddExtLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:='Maximum counter';
    Visible:=true;
  end;

  BakOtherMaxCounterComboBox:=TComboBox.Create(Self);
  with BakOtherMaxCounterComboBox do begin
    Name:='BakOtherMaxCounterComboBox';
    Parent:=BackupOtherGroupBox;
    Left:=BakOtherMaxCounterLabel.Left+BakOtherMaxCounterLabel.Width+2;
    Top:=BakOtherMaxCounterLabel.Top;
    Width:=100;
    Height:=25;
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
    Visible:=true;
  end;

  BakOtherSubDirLabel:=TLabel.Create(Self);
  with BakOtherSubDirLabel do begin
    Name:='BakOtherSubDirLabel';
    Parent:=BackupOtherGroupBox;
    Left:=5;
    Top:=BakOtherMaxCounterLabel.Top+BakOtherMaxCounterLabel.Height+5;
    Width:=110;
    Height:=23;
    Caption:='Sub directory';
    Visible:=true;
  end;

  BakOtherSubDirComboBox:=TComboBox.Create(Self);
  with BakOtherSubDirComboBox do begin
    Name:='BakOtherSubDirComboBox';
    Parent:=BackupOtherGroupBox;
    Left:=BakOtherSubDirLabel.Left+BakOtherSubDirLabel.Width+2;
    Top:=BakOtherSubDirLabel.Top;
    Width:=100;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('(no subdirectoy)');
      Add('backup');
      EndUpdate;
    end;
    Visible:=true;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupFilesPage(Page: integer);
var MaxX:integer;
  ADebuggerType: TDebuggerType;
begin
  MaxX:=ClientWidth-5;

  MaxRecentOpenFilesLabel:=TLabel.Create(Self);
  with MaxRecentOpenFilesLabel do begin
    Name:='MaxRecentOpenFilesLabel';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=4;
    Width:=170;
    Height:=23;
    Caption:='Max recent files';
    Visible:=true;
  end;

  MaxRecentOpenFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentOpenFilesComboBox do begin
    Name:='MaxRecentOpenFilesComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=MaxRecentOpenFilesLabel.Left+MaxRecentOpenFilesLabel.Width+2;
    Top:=MaxRecentOpenFilesLabel.Top;
    Width:=60;
    Height:=25;
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
    Visible:=true;
  end;

  MaxRecentProjectFilesLabel:=TLabel.Create(Self);
  with MaxRecentProjectFilesLabel do begin
    Name:='MaxRecentProjectFilesLabel';
    Parent:=NoteBook.Page[Page];
    Left:=MaxRecentOpenFilesLabel.Left;
    Top:=MaxRecentOpenFilesLabel.Top+MaxRecentOpenFilesLabel.Height+3;
    Width:=MaxRecentOpenFilesLabel.Width;
    Height:=MaxRecentOpenFilesLabel.Height;
    Caption:='Max recent project files';
    Visible:=true;
  end;

  MaxRecentProjectFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentProjectFilesComboBox do begin
    Name:='MaxRecentProjectFilesComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=MaxRecentProjectFilesLabel.Left+MaxRecentProjectFilesLabel.Width+2;
    Top:=MaxRecentProjectFilesLabel.Top;
    Width:=60;
    Height:=25;
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
    Visible:=true;
  end;
  
  OpenLastProjectAtStartCheckBox:=TCheckBox.Create(Self);
  with OpenLastProjectAtStartCheckBox do begin
    Name:='OpenLastProjectAtStartCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=MaxRecentProjectFilesLabel.Top+MaxRecentProjectFilesLabel.Height+5;
    Width:=MaxX-10;
    Height:=23;
    Caption:='Open last project at start';
    Visible:=true;
  end;

  LazarusDirLabel:=TLabel.Create(Self);
  with LazarusDirLabel do begin
    Name:='LazarusDirLabel';
    Parent:=NoteBook.Page[Page];
    Left:=4;
    Top:=OpenLastProjectAtStartCheckBox.Top
        +OpenLastProjectAtStartCheckBox.Height+5;
    Width:=MaxX-10;
    Height:=23;
    Caption:='Lazarus directory (default for all projects)';
    Visible:=true;
  end;

  LazarusDirComboBox:=TComboBox.Create(Self);
  with LazarusDirComboBox do begin
    Name:='LazarusDirComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=LazarusDirLabel.Top+LazarusDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add(ExtractFilePath(ParamStr(0)));
      EndUpdate;
    end;
    Visible:=true;
  end;

  CompilerPathLabel:=TLabel.Create(Self);
  with CompilerPathLabel do begin
    Name:='CompilerPathLabel';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=LazarusDirComboBox.Top+LazarusDirComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    Caption:='Compiler path (ppc386)';
    Visible:=true;
  end;

  CompilerPathComboBox:=TComboBox.Create(Self);
  with CompilerPathComboBox do begin
    Name:='CompilerPathComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=CompilerPathLabel.Top+CompilerPathLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('/usr/bin/ppc386');
      Add('/opt/fpc/ppc386');
      EndUpdate;
    end;
    Visible:=true;
  end;

  FPCSourceDirLabel:=TLabel.Create(Self);
  with FPCSourceDirLabel do begin
    Name:='FPCSourceDirLabel';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=CompilerPathComboBox.Top+CompilerPathComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=23;
    Caption:='FPC source directory';
    Visible:=true;
  end;

  FPCSourceDirComboBox:=TComboBox.Create(Self);
  with FPCSourceDirComboBox do begin
    Name:='FPCSourceDirComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=FPCSourceDirLabel.Top+FPCSourceDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('');
      EndUpdate;
    end;
    Visible:=true;
  end;
  
  DebuggerPathLabel:=TLabel.Create(Self);
  with DebuggerPathLabel do begin
    Name:='DebuggerPathLabel';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=FPCSourceDirComboBox.Top+FPCSourceDirComboBox.Height;
    Width:=FPCSourceDirLabel.Width;
    Height:=25;
    Caption:='Debugger type and path';
    Visible:=true;
  end;

  DebuggerTypeComboBox:=TComboBox.Create(Self);
  with DebuggerTypeComboBox do begin
    Name:='DebuggerTypeComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=FPCSourceDirLabel.Left;
    Top:=DebuggerPathLabel.Top+DebuggerPathLabel.Height+2;
    Width:=LazarusDirLabel.Width div 2;
    Height:=25;
    with Items do begin
      BeginUpdate;
      for ADebuggerType:=Low(TDebuggerType) to High(TDebuggerType) do
        Add(DebuggerName[ADebuggerType]);
      EndUpdate;
    end;
    Visible:=true;
  end;

  DebuggerPathComboBox:=TComboBox.Create(Self);
  with DebuggerPathComboBox do begin
    Name:='DebuggerPathComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=DebuggerTypeComboBox.Left+DebuggerTypeComboBox.Width+10;
    Top:=DebuggerTypeComboBox.Top;
    Width:=LazarusDirLabel.Width-DebuggerTypeComboBox.Width-10;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add(DebuggerName[dtNone]);
      Add('/opt/fpc/gdb');
      EndUpdate;
    end;
    Visible:=true;
  end;

  TestBuildDirLabel:=TLabel.Create(Self);
  with TestBuildDirLabel do begin
    Name:='TestBuildDirLabel';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=DebuggerTypeComboBox.Top+DebuggerTypeComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=23;
    Caption:='Directory for building test projects';
    Visible:=true;
  end;

  TestBuildDirComboBox:=TComboBox.Create(Self);
  with TestBuildDirComboBox do begin
    Name:='TestBuildDirComboBox';
    Parent:=NoteBook.Page[Page];
    Left:=LazarusDirLabel.Left;
    Top:=TestBuildDirLabel.Top+TestBuildDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('/tmp');
      Add('/var/tmp');
      Add('c:/tmp');
      Add('c:/windows/temp');
      EndUpdate;
    end;
    Visible:=true;
  end;
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
      Caption:='Show grid';
      Visible:=true;
    end;

    GridColorButton:=TColorButton.Create(Self);
    with GridColorButton do begin
      Name:='GridColorButton';
      Parent:=GridGroupBox;
      Left:=ShowGridCheckBox.Left;
      Top:=ShowGridCheckBox.Top+ShowGridCheckBox.Height+5;
      Width:=50;
      Height:=25;
      Visible:=true;
    end;

    GridColorLabel:=TLabel.Create(Self);
    with GridColorLabel do begin
      Name:='GridColorLabel';
      Parent:=GridGroupBox;
      Left:=GridColorButton.Left+GridColorButton.Width+5;
      Top:=GridColorButton.Top+2;
      Width:=80;
      Caption:='Grid color';
      Visible:=true;
    end;

    SnapToGridCheckBox:=TCheckBox.Create(Self);
    with SnapToGridCheckBox do begin
      Name:='SnapToGridCheckBox';
      Parent:=GridGroupBox;
      Top:=GridColorLabel.Top+GridColorLabel.Height+10;
      Left:=ShowGridCheckBox.Left;
      Width:=ShowGridCheckBox.Width;
      Height:=ShowGridCheckBox.Height;
      Caption:='Snap to grid';
      Visible:=true;
    end;

    GridSizeXLabel:=TLabel.Create(Self);
    with GridSizeXLabel do begin
      Name:='GridSizeXLabel';
      Parent:=GridGroupBox;
      Left:=ShowGridCheckBox.Left;
      Top:=SnapToGridCheckBox.Top+SnapToGridCheckBox.Height+5;
      Width:=80;
      Caption:='Grid size X';
      Visible:=true;
    end;

    GridSizeXComboBox:=TComboBox.Create(Self);
    with GridSizeXComboBox do begin
      Name:='GridSizeXComboBox';
      Parent:=GridGroupBox;
      Left:=GridSizeXLabel.Left+GridSizeXLabel.Width+5;
      Top:=GridSizeXLabel.Top-2;
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
      Visible:=true;
    end;

    GridSizeYLabel:=TLabel.Create(Self);
    with GridSizeYLabel do begin
      Name:='GridSizeYLabel';
      Parent:=GridGroupBox;
      Left:=GridSizeXLabel.Left;
      Top:=GridSizeXLabel.Top+GridSizeXLabel.Height+5;
      Width:=GridSizeXLabel.Width;
      Caption:='Grid size Y';
      Visible:=true;
    end;

    GridSizeYComboBox:=TComboBox.Create(Self);
    with GridSizeYComboBox do begin
      Name:='GridSizeYComboBox';
      Parent:=GridGroupBox;
      Left:=GridSizeYLabel.Left+GridSizeYLabel.Width+5;
      Top:=GridSizeYLabel.Top-2;
      Width:=GridSizeXComboBox.Width;
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
      Visible:=true;
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
      Caption:='Show Guide Lines';
      Visible:=true;
    end;
    
    SnapToGuideLinesCheckBox:=TCheckBox.Create(Self);
    with SnapToGuideLinesCheckBox do begin
      Name:='SnapToGuideLinesCheckBox';
      Parent:=GuideLinesGroupBox;
      Left:=ShowGuideLinesCheckBox.Left;
      Top:=ShowGuideLinesCheckBox.Top+ShowGuideLinesCheckBox.Height+5;
      Width:=ShowGuideLinesCheckBox.Width;
      Caption:='Snap to Guide Lines';
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
      Caption:='color for left, top';
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
      Caption:='color for right, bottom';
      Visible:=true;
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
      Width:=Parent.ClientWidth-2*Left;
      Caption:='Show component captions';
      Visible:=true;
    end;

    ShowEditorHintsCheckBox:=TCheckBox.Create(Self);
    with ShowEditorHintsCheckBox do begin
      Name:='ShowEditorHintsCheckBox';
      Parent:=FormEditMiscGroupBox;
      Top:=ShowComponentCaptionsCheckBox.Top
           +ShowComponentCaptionsCheckBox.Height+5;
      Left:=ShowComponentCaptionsCheckBox.Left;
      Width:=ShowComponentCaptionsCheckBox.Width;
      Caption:='Show editor hints';
      Visible:=true;
    end;

    AutoCreateFormsCheckBox:=TCheckBox.Create(Self);
    with AutoCreateFormsCheckBox do begin
      Name:='AutoCreateFormsCheckBox';
      Parent:=FormEditMiscGroupBox;
      Top:=ShowEditorHintsCheckBox.Top+ShowEditorHintsCheckBox.Height+5;
      Left:=ShowEditorHintsCheckBox.Left;
      Width:=ShowEditorHintsCheckBox.Width;
      Caption:='Auto create forms';
      Visible:=true;
    end;
  end;

begin
  // form editor page
  GridGroupBox:=TGroupBox.Create(Self);
  with GridGroupBox do begin
    Name:='GridGroupBox';
    Parent:=Notebook.Page[Page];
    Left:=5;
    Top:=5;
    Width:=((Parent.ClientWidth-3*Left) div 2);
    Height:=170;
    Caption:='Grid';
    Visible:=true;
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
    Caption:='Guide lines';
    Visible:=true;
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
    Caption:='Miscellaneous';
    Visible:=true;
  end;
  
  SetupMiscGroupBox;
end;

procedure TEnvironmentOptionsDialog.SetupNamingPage(Page: integer);
var
  pe: TPascalExtType;
begin
  PascalFileExtRadiogroup:=TRadioGroup.Create(Self);
  with PascalFileExtRadiogroup do begin
    Name:='PascalFileExtRadiogroup';
    Parent:=NoteBook.Page[Page];
    Left:=5;
    Top:=4;
    Width:=200;
    Height:=80;
    Caption:='Default pascal extension';
    with Items do begin
      BeginUpdate;
      for pe:=Low(TPascalExtType) to High(TPascalExtType) do
        if pe<>petNone then
          Add(PascalExtension[pe]);
      EndUpdate;
    end;
    Visible:=true;
  end;

  PascalFileLowercaseCheckBox:=TCheckBox.Create(Self);
  with PascalFileLowercaseCheckBox do begin
    Name:='PascalFileLowercaseCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=PascalFileExtRadiogroup.Left;
    Top:=PascalFileExtRadiogroup.Top+PascalFileExtRadiogroup.Height+10;
    Width:=300;
    Caption:='Save pascal files lowercase';
    Visible:=true;
  end;
  
  AutoDeleteAmbigiousSourcesCheckBox:=TCheckBox.Create(Self);
  with AutoDeleteAmbigiousSourcesCheckBox do begin
    Name:='AutoDeleteAmbigiousSourcesCheckBox';
    Parent:=NoteBook.Page[Page];
    Left:=PascalFileLowercaseCheckBox.Left;
    Top:=PascalFileLowercaseCheckBox.Top+PascalFileLowercaseCheckBox.Height+10;
    Width:=300;
    Caption:='Auto delete ambigious sources';
    Visible:=true;
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeDesktopPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  // language
  with LanguageGroupBox do begin
    SetBounds(8,2,(MaxX div 2) - 15,50);
  end;

  with LanguageComboBox do begin
    SetBounds(5,3,LanguageGroupBox.ClientWidth-2*Left,Height);
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
    Top:=54;
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
    Width:=DesktopFilesGroupBox.ClientWidth-15;
    Height:=25;
  end;

  with LoadDesktopSettingsFromFileButton do begin
    Left:=5;
    Top:=38;
    Width:=SaveDesktopSettingsToFileButton.Width;
    Height:=25;
  end;

  // hints
  with ShowHintsForComponentPaletteCheckBox do begin
    Left:=DesktopFilesGroupBox.Left;
    Top:=DesktopFilesGroupBox.Top+DesktopFilesGroupBox.Height+100;
    Width:=Parent.ClientWidth-Left;
    Height:=20;
  end;

  with ShowHintsForMainSpeedButtonsCheckBox do begin
    Left:=ShowHintsForComponentPaletteCheckBox.Left;
    Top:=ShowHintsForComponentPaletteCheckBox.Top
         +ShowHintsForComponentPaletteCheckBox.Height+5;
    Width:=Parent.ClientWidth-Left;
    Height:=20;
  end;

  // Window Positions
  with WindowPositionsGroupBox do begin
    SetBounds(MaxX div 2,LanguageGroupBox.Top,(MaxX div 2)-5,290);
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeFormEditorPage;

  procedure SetupGridGroupBox;
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

    with GridSizeXLabel do begin
      SetBounds(ShowGridCheckBox.Left,
                SnapToGridCheckBox.Top+SnapToGridCheckBox.Height+5,80,Height);
    end;

    with GridSizeXComboBox do begin
      SetBounds(GridSizeXLabel.Left+GridSizeXLabel.Width+5,
                GridSizeXLabel.Top-2,60,Height);
    end;

    with GridSizeYLabel do begin
      SetBounds(GridSizeXLabel.Left,
                GridSizeXLabel.Top+GridSizeXLabel.Height+5,
                GridSizeXLabel.Width,Height);
    end;

    with GridSizeYComboBox do begin
      SetBounds(GridSizeYLabel.Left+GridSizeYLabel.Width+5,
                GridSizeYLabel.Top-2,
                GridSizeXComboBox.Width,Height);
    end;
  end;

  procedure SetupGuideLinesGroupBox;
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

  procedure SetupMiscGroupBox;
  begin
    with ShowComponentCaptionsCheckBox do begin
      SetBounds(5,5,Parent.ClientWidth-2*Left,Height);
    end;

    with ShowEditorHintsCheckBox do begin
      SetBounds(ShowComponentCaptionsCheckBox.Left,
                ShowComponentCaptionsCheckBox.Top
                 +ShowComponentCaptionsCheckBox.Height+5,
                ShowComponentCaptionsCheckBox.Width,Height);
    end;

    with AutoCreateFormsCheckBox do begin
      SetBounds(ShowEditorHintsCheckBox.Left,
                ShowEditorHintsCheckBox.Top+ShowEditorHintsCheckBox.Height+5,
                ShowEditorHintsCheckBox.Width,Height);
    end;
  end;

begin
  // form editor page
  with GridGroupBox do begin
    SetBounds(5,5,((Parent.ClientWidth-3*Left) div 2),170);
  end;
  SetupGridGroupBox;
  with GuideLinesGroupBox do begin
    SetBounds(GridGroupBox.Left+GridGroupBox.Width+5,GridGroupBox.Top,
              GridGroupBox.Width,GridGroupBox.Height);
  end;
  SetupGuideLinesGroupBox;
  with FormEditMiscGroupBox do begin
    SetBounds(5,GridGroupBox.Top+GridGroupBox.Height+5,
              Parent.ClientWidth-2*Left,100);
  end;
  SetupMiscGroupBox;
end;

procedure TEnvironmentOptionsDialog.ResizeObjectInspectorPage;
begin
  // object inspector
  with ObjectInspectorGroupBox do begin
    Left:=6;
    Top:=2;
    Width:=200;
    Height:=55;
  end;

  with OIBackgroundColorButton do begin
    Left:=6;
    Top:=6;
    Width:=50;
    Height:=25;
  end;

  with OIBackgroundColorLabel do begin
    Left:=OIBackgroundColorButton.Left+OIBackgroundColorButton.Width+5;
    Top:=OIBackgroundColorButton.Top;
    Width:=ObjectInspectorGroupBox.ClientWidth-Left-5;
    Height:=23;
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeFilesPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  with MaxRecentOpenFilesLabel do begin
    Left:=4;
    Top:=4;
    Width:=170;
    Height:=23;
  end;

  with MaxRecentOpenFilesComboBox do begin
    Left:=MaxRecentOpenFilesLabel.Left+MaxRecentOpenFilesLabel.Width+2;
    Top:=MaxRecentOpenFilesLabel.Top;
    Width:=60;
    Height:=25;
  end;

  with MaxRecentProjectFilesLabel do begin
    Left:=MaxRecentOpenFilesLabel.Left;
    Top:=MaxRecentOpenFilesLabel.Top+MaxRecentOpenFilesLabel.Height+3;
    Width:=MaxRecentOpenFilesLabel.Width;
    Height:=MaxRecentOpenFilesLabel.Height;
  end;

  with MaxRecentProjectFilesComboBox do begin
    Left:=MaxRecentProjectFilesLabel.Left+MaxRecentProjectFilesLabel.Width+2;
    Top:=MaxRecentProjectFilesLabel.Top;
    Width:=60;
    Height:=25;
  end;

  with OpenLastProjectAtStartCheckBox do begin
    Left:=4;
    Top:=MaxRecentProjectFilesLabel.Top+MaxRecentProjectFilesLabel.Height+5;
    Width:=MaxX-10;
    Height:=23;
  end;

  with LazarusDirLabel do begin
    Left:=4;
    Top:=OpenLastProjectAtStartCheckBox.Top
        +OpenLastProjectAtStartCheckBox.Height+5;
    Width:=MaxX-10;
    Height:=23;
  end;

  with LazarusDirComboBox do begin
    Left:=LazarusDirLabel.Left;
    Top:=LazarusDirLabel.Top+LazarusDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
  end;

  with CompilerPathLabel do begin
    Left:=LazarusDirLabel.Left;
    Top:=LazarusDirComboBox.Top+LazarusDirComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=25;
  end;

  with CompilerPathComboBox do begin
    Left:=LazarusDirLabel.Left;
    Top:=CompilerPathLabel.Top+CompilerPathLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
  end;

  with FPCSourceDirLabel do begin
    Left:=LazarusDirLabel.Left;
    Top:=CompilerPathComboBox.Top+CompilerPathComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=23;
  end;

  with FPCSourceDirComboBox do begin
    Left:=LazarusDirLabel.Left;
    Top:=FPCSourceDirLabel.Top+FPCSourceDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
  end;

  with DebuggerPathLabel do begin
    Left:=LazarusDirLabel.Left;
    Top:=FPCSourceDirComboBox.Top+FPCSourceDirComboBox.Height;
    Width:=FPCSourceDirLabel.Width;
    Height:=25;
  end;

  with DebuggerTypeComboBox do begin
    Left:=FPCSourceDirLabel.Left;
    Top:=DebuggerPathLabel.Top+DebuggerPathLabel.Height+2;
    Width:=LazarusDirLabel.Width div 2;
    Height:=25;
  end;

  with DebuggerPathComboBox do begin
    Left:=DebuggerTypeComboBox.Left+DebuggerTypeComboBox.Width+10;
    Top:=DebuggerTypeComboBox.Top;
    Width:=LazarusDirLabel.Width-DebuggerTypeComboBox.Width-10;
    Height:=25;
  end;

  with TestBuildDirLabel do begin
    Left:=LazarusDirLabel.Left;
    Top:=DebuggerTypeComboBox.Top+DebuggerTypeComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=23;
  end;

  with TestBuildDirComboBox do begin
    Left:=LazarusDirLabel.Left;
    Top:=TestBuildDirLabel.Top+TestBuildDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
  end;
end;

procedure TEnvironmentOptionsDialog.ResizeBackupPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

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

  with BakProjTypeRadioGroup do begin
    Left:=5;
    Top:=4;
    Width:=BackupProjectGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
  end;

  with BakProjAddExtLabel do begin
    Left:=5;
    Top:=BakProjTypeRadioGroup.Top+BakProjTypeRadioGroup.Height+5;
    Width:=BakProjTypeRadioGroup.Width-62;
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
    Width:=BakProjTypeRadioGroup.Width-102;
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

  with BackupOtherGroupBox do begin
    Left:=BackupProjectGroupBox.Left+BackupProjectGroupBox.Width+10;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
  end;

  with BakOtherTypeRadioGroup do begin
    Left:=5;
    Top:=4;
    Width:=BackupOtherGroupBox.ClientWidth-Left-Left-4;
    Height:=140;
  end;

  with BakOtherAddExtLabel do begin
    Left:=5;
    Top:=BakOtherTypeRadioGroup.Top+BakOtherTypeRadioGroup.Height+5;
    Width:=BakOtherTypeRadioGroup.Width-62;
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
    Width:=BakOtherTypeRadioGroup.Width-102;
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
    Width:=BakOtherTypeRadioGroup.Width-102;
    Height:=23;
  end;

  with BakOtherSubDirComboBox do begin
    Left:=BakOtherSubDirLabel.Left+BakOtherSubDirLabel.Width+2;
    Top:=BakOtherSubDirLabel.Top;
    Width:=100;
    Height:=25;
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

  with PascalFileLowercaseCheckBox do begin
    Left:=PascalFileExtRadiogroup.Left;
    Top:=PascalFileExtRadiogroup.Top+PascalFileExtRadiogroup.Height+10;
    Width:=300;
  end;

  with AutoDeleteAmbigiousSourcesCheckBox do begin
    Left:=PascalFileLowercaseCheckBox.Left;
    Top:=PascalFileLowercaseCheckBox.Top+PascalFileLowercaseCheckBox.Height+10;
    Width:=300;
  end;
end;

procedure TEnvironmentOptionsDialog.EnvironmentOptionsDialogResize(
  Sender: TObject);
begin
  with NoteBook do begin
    SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
  end;

  ResizeDesktopPage;
  ResizeFormEditorPage;
  ResizeObjectInspectorPage;
  ResizeFilesPage;
  ResizeBackupPage;
  ResizeNamingPage;

  with CancelButton do begin
    Width:=70;
    Height:=23;
    Left:=Self.ClientWidth-Width-15;
    Top:=Self.ClientHeight-Height-15;
  end;

  with OkButton do begin
    Width:=CancelButton.Width;
    Height:=CancelButton.Height;
    Left:=CancelButton.Left-15-Width;
    Top:=CancelButton.Top;
  end;
end;

procedure TEnvironmentOptionsDialog.WindowPositionsGroupBoxResize(
  Sender: TObject);
begin
  with WindowPositionsListBox do begin
    SetBounds(2,2,Parent.ClientWidth-2*2,Parent.Height div 4);
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

procedure TEnvironmentOptionsDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TEnvironmentOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
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
      // ToDo
      writeln('ERROR: [TEnvironmentOptionsDialog.SaveDesktopSettingsToFileButtonClick]');
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

    // hints
    ShowHintsForComponentPaletteCheckBox.Checked:=
      ShowHintsForComponentPalette;
    ShowHintsForMainSpeedButtonsCheckBox.Checked:=
      ShowHintsForMainSpeedButtons;

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
    AutoCreateFormsCheckBox.Checked:=AutoCreateForms;

    // files
    LazarusDirComboBox.Items.Assign(LazarusDirHistory);
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory,MaxComboBoxCount);
    CompilerPathComboBox.Items.Assign(CompilerFileHistory);
    SetComboBoxText(CompilerPathComboBox,CompilerFilename,MaxComboBoxCount);
    FPCSourceDirComboBox.Items.Assign(FPCSourceDirHistory);
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory,MaxComboBoxCount);
    DebuggerPathComboBox.Items.Assign(DebuggerFileHistory);
    SetComboBoxText(DebuggerPathComboBox,DebuggerFilename,MaxComboBoxCount);
    SetComboBoxText(DebuggerTypeComboBox,DebuggerName[DebuggerType]);
    TestBuildDirComboBox.Items.Assign(TestBuildDirHistory);
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
    PascalFileLowercaseCheckBox.Checked:=PascalFileLowerCase;
    AutoDeleteAmbigiousSourcesCheckBox.Checked:=AutoDeleteAmbigiousSources;
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

    // hints
    ShowHintsForComponentPalette:=ShowHintsForComponentPaletteCheckBox.Checked;
    ShowHintsForMainSpeedButtons:=ShowHintsForMainSpeedButtonsCheckBox.Checked;

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
    AutoCreateForms:=AutoCreateFormsCheckBox.Checked;

    // files
    LazarusDirectory:=LazarusDirComboBox.Text;
    LazarusDirHistory.Assign(LazarusDirComboBox.Items);
    CompilerFilename:=CompilerPathComboBox.Text;
    CompilerFileHistory.Assign(CompilerPathComboBox.Items);
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;
    FPCSourceDirHistory.Assign(FPCSourceDirComboBox.Items);
    DebuggerFilename:=DebuggerPathComboBox.Text;
    DebuggerFileHistory.Assign(DebuggerPathComboBox.Items);
    DebuggerType:=DebuggerNameToType(DebuggerTypeComboBox.Text);
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
    PascalFileLowerCase:=PascalFileLowercaseCheckBox.Checked;
    AutoDeleteAmbigiousSources:=AutoDeleteAmbigiousSourcesCheckBox.Checked;
  end;
end;

procedure TEnvironmentOptionsDialog.SetComboBoxText(
  AComboBox:TComboBox; const AText:AnsiString);
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

procedure TEnvironmentOptionsDialog.SetComboBoxText(
  AComboBox:TComboBox; const AText:AnsiString; MaxCount: integer);
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

procedure TEnvironmentOptionsDialog.SetupObjectInspectorPage(Page: integer);
var MaxX: integer;
begin
  MaxX:=ClientWidth-5;
  
  // object inspector
  ObjectInspectorGroupBox:=TGroupBox.Create(Self);
  with ObjectInspectorGroupBox do begin
    Name:='ObjectInspectorGroupBox';
    Parent:=NoteBook.Page[Page];
    Left:=6;
    Top:=2;
    Width:=(MaxX div 2) - 15;
    Height:=55;
    Caption:='Colors';
    Visible:=true;
  end;

  OIBackgroundColorButton:=TColorButton.Create(Self);
  with OIBackgroundColorButton do begin
    Name:='OIBackgroundColorButton';
    Parent:=ObjectInspectorGroupBox;
    Left:=6;
    Top:=6;
    Width:=50;
    Height:=25;
    Visible:=true;
  end;

  OIBackgroundColorLabel:=TLabel.Create(Self);
  with OIBackgroundColorLabel do begin
    Name:='OIBackgroundColorLabel';
    Parent:=ObjectInspectorGroupBox;
    Left:=OIBackgroundColorButton.Left+OIBackgroundColorButton.Width+5;
    Top:=OIBackgroundColorButton.Top;
    Width:=ObjectInspectorGroupBox.ClientWidth-Left-5;
    Height:=23;
    Caption:='Background color';
    Visible:=true;
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
  0: WindowPositionsBox.Layout:=FLayouts.ItemByFormID(DefaultMainIDEName);
  1: WindowPositionsBox.Layout:=FLayouts.ItemByFormID(DefaultSourceNoteBookName);
  2: WindowPositionsBox.Layout:=FLayouts.ItemByFormID(DefaultMessagesViewName);
  3: WindowPositionsBox.Layout:=FLayouts.ItemByFormID(DefaultObjectInspectorName);
  end;
  if Index>=0 then
    WindowPositionsBox.Caption:=WindowPositionsListBox.Items[Index];
end;

end.

