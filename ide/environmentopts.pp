{
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
  Classes, SysUtils, Forms, Controls, Buttons, XMLCfg, ObjectInspector,
  ExtCtrls, StdCtrls, EditorOptions, LResources, LazConf, Dialogs;

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

  { class for storing environment options }
  TEnvironmentOptions = class
  private
    FFilename: string;
    
    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    FLastSavedProjectFile: string;
    
    // windows
    FSaveWindowPositions: boolean;
    FWindowPositionsValid: boolean; // the following values are valid
    FMainWindowBounds: TRect;
    FSourceEditorBounds: TRect;
    FMessagesViewBoundsValid: boolean;
    FMessagesViewBounds: TRect;
    
    // form editor
    FDisplayGrid: boolean;
    FSnapToGrid: boolean;
    FShowComponentCaptions: boolean;
    FShowEditorHints: boolean;
    FAutoCreateForms: boolean;
    FGridSizeX: integer;
    FGridSizeY: integer;
    
    // object inspector
    FObjectInspectorOptions: TOIOptions;
    
    // compiler + lazarus files
    FLazarusDirectory: string;
    FCompilerFilename: string;
    FFPCSourceDirectory: string;

    // recent files and directories
    FRecentOpenFiles: TStringList;
    FMaxRecentOpenFiles: integer;
    FRecentProjectFiles: TStringList;
    FMaxRecentProjectFiles: integer;
    FLastOpenDialogDir: string;
    FOpenLastProjectAtStart: boolean;

    // backup
    FBackupInfoProjectFiles: TBackupInfo;
    FBackupInfoOtherFiles: TBackupInfo;

    procedure SetFileName(const NewFilename: string);
    procedure AddToRecentList(const AFilename: string; RecentList: TStringList;
        Max: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(OnlyDesktop:boolean);
    procedure Save(OnlyDesktop:boolean);
    property Filename: string read FFilename write SetFilename;
    procedure SetLazarusDefaultFilename;
    
    // auto save
    property AutoSaveEditorFiles: boolean
       read FAutoSaveEditorFiles write FAutoSaveEditorFiles;
    property AutoSaveProject: boolean
       read FAutoSaveProject write FAutoSaveProject;
    property AutoSaveIntervalInSecs: integer
       read FAutoSaveIntervalInSecs write FAutoSaveIntervalInSecs;

    // windows
    property SaveWindowPositions: boolean
       read FSaveWindowPositions write FSaveWindowPositions;
    property WindowPositionsValid: boolean
       read FWindowPositionsValid write FWindowPositionsValid;
    property MainWindowBounds: TRect
       read FMainWindowBounds write FMainWindowBounds;
    property SourceEditorBounds: TRect
       read FSourceEditorBounds write FSourceEditorBounds;
    property MessagesViewBoundsValid: boolean
       read FMessagesViewBoundsValid write FMessagesViewBoundsValid;
    property MessagesViewBounds: TRect
       read FMessagesViewBounds write FMessagesViewBounds;

    // form editor
    property DisplayGrid: boolean read FDisplayGrid write FDisplayGrid;
    property SnapToGrid: boolean read FSnapToGrid write FSnapToGrid;
    property ShowComponentCaptions: boolean
       read FShowComponentCaptions write FShowComponentCaptions;
    property ShowEditorHints: boolean read FShowEditorHints write FShowEditorHints;
    property AutoCreateForms: boolean read FAutoCreateForms write FAutoCreateForms;
    property GridSizeX: integer read FGridSizeX write FGridSizeX;
    property GridSizeY: integer read FGridSizeY write FGridSizeY;

    // object inspector
    property ObjectInspectorOptions: TOIOptions
       read FObjectInspectorOptions write FObjectInspectorOptions;

    // files
    property LazarusDirectory: string
       read FLazarusDirectory write FLazarusDirectory;
    property CompilerFilename: string
       read FCompilerFilename write FCompilerFilename;
    property FPCSourceDirectory: string
       read FFPCSourceDirectory write FFPCSourceDirectory;

    // recent files and directories
    property RecentOpenFiles: TStringList
       read FRecentOpenFiles write FRecentOpenFiles;
    property MaxRecentOpenFiles: integer
       read FMaxRecentOpenFiles write FMaxRecentOpenFiles;
    procedure AddToRecentOpenFiles(const AFilename: string);
    property RecentProjectFiles: TStringList
       read FRecentProjectFiles write FRecentProjectFiles;
    property MaxRecentProjectFiles: integer
       read FMaxRecentProjectFiles write FMaxRecentProjectFiles;
    procedure AddToRecentProjectFiles(const AFilename: string);
    property LastOpenDialogDir: string
       read FLastOpenDialogDir write FLastOpenDialogDir;
    property LastSavedProjectFile: string 
       read FLastSavedProjectFile write FLastSavedProjectFile;
    property OpenLastProjectAtStart: boolean
       read FOpenLastProjectAtStart write FOpenLastProjectAtStart;

    // backup
    property BackupInfoProjectFiles: TBackupInfo 
       read FBackupInfoProjectFiles write FBackupInfoProjectFiles;
    property BackupInfoOtherFiles: TBackupInfo
       read FBackupInfoOtherFiles write FBackupInfoOtherFiles;
  end;

  //----------------------------------------------------------------------------

  TOnLoadEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;
  TOnSaveEnvironmentSettings = procedure (Sender: TObject;
        EnvironmentOptions: TEnvironmentOptions) of object;

  { form for environment options }
  TEnvironmentOptionsDialog = class(TForm)
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    procedure SetupDesktopPage;
    procedure SetupBackupPage;
    procedure SetupFilesPage;
    procedure SetComboBoxText(AComboBox:TComboBox; const AText:AnsiString);

  published
    NoteBook: TNoteBook;
    
    // auto save
    AutoSaveGroupBox: TGroupBox;
    AutoSaveEditorFilesCheckBox: TCheckBox;
    AutoSaveProjectCheckBox: TCheckBox;
    AutoSaveIntervalInSecsLabel: TLabel;
    AutoSaveIntervalInSecsComboBox: TComboBox;

    // windows
    WindowsGroupBox: TGroupBox;
    SaveWindowPositionsCheckBox: TCheckBox;

    // desktop files
    DesktopFilesGroupBox: TGroupBox;
    SaveDesktopSettingsToFileButton: TButton;
    LoadDesktopSettingsFromFileButton: TButton;
    
    // form editor
    FormEditorGroupBox: TGroupBox;
    DisplayGridCheckBox: TCheckBox;
    SnapToGridCheckBox: TCheckBox;
    ShowComponentCaptionsCheckBox: TCheckBox;
    ShowEditorHintsCheckBox: TCheckBox;
    AutoCreateFormsCheckBox: TCheckBox;
    GridSizeXLabel: TLabel;
    GridSizeXComboBox: TComboBox;
    GridSizeYLabel: TLabel;
    GridSizeYComboBox: TComboBox;

    // object inspector
    ObjectInspectorGroupBox: TGroupBox;
    BackgroundColorLabel: TLabel;
    BackgroundColorButton: TColorButton;

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

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
    procedure BakTypeRadioGroupClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveDesktopSettingsToFileButtonClick(Sender: TObject);
    procedure LoadDesktopSettingsFromFileButtonClick(Sender: TObject);
    property OnSaveEnvironmentSettings:TOnSaveEnvironmentSettings
      read FOnSaveEnvironmentSettings write FOnSaveEnvironmentSettings;
    property OnLoadEnvironmentSettings:TOnLoadEnvironmentSettings
      read FOnLoadEnvironmentSettings write FOnLoadEnvironmentSettings;

  public
    procedure ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
    procedure WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
    constructor Create(AOwner:TComponent);  override;
    destructor Destroy; override;
  end;


var
  EnvironmentOptions: TEnvironmentOptions;


implementation


{ TEnvironmentOptions }

const
  EnvOptsConfFileName='environmentoptions.xml';
  BakMaxCounterInfiniteTxt = 'infinite';
  BakNoSubDirTxt = '(none)';

constructor TEnvironmentOptions.Create;
begin
  inherited Create;

  FFilename:='';

  // auto save
  FAutoSaveEditorFiles:=true;
  FAutoSaveProject:=true;
  FAutoSaveIntervalInSecs:=300; // 5 minutes
  FLastSavedProjectFile:='';

  // windows
  FSaveWindowPositions:=true;
  FWindowPositionsValid:=false;
  FMainWindowBounds:=Bounds(0,0,600,100);
  FSourceEditorBounds:=Bounds(230,150,400,200);
  FMessagesViewBoundsValid:=false;
  FMessagesViewBounds:=Bounds(230,350,400,100);

  // form editor
  FDisplayGrid:=true;
  FSnapToGrid:=true;
  FShowComponentCaptions:=false;
  FShowEditorHints:=false;
  FAutoCreateForms:=true;
  FGridSizeX:=8;
  FGridSizeY:=8;

  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;

  // files
  FLazarusDirectory:='';
  FCompilerFilename:='';
  FFPCSourceDirectory:='';

  // recent files and directories
  FRecentOpenFiles:=TStringList.Create;
  FMaxRecentOpenFiles:=10;
  FRecentProjectFiles:=TStringList.Create;
  FMaxRecentProjectFiles:=5;
  FLastOpenDialogDir:='';
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
end;

destructor TEnvironmentOptions.Destroy;
begin
  FRecentOpenFiles.Free;
  FRecentProjectFiles.Free;
  FObjectInspectorOptions.Free;
  inherited Destroy;
end;

procedure TEnvironmentOptions.SetLazarusDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+EnvOptsConfFileName);
  CopySecondaryConfigFile(EnvOptsConfFileName);
  if (not FileExists(ConfFileName)) then begin
    writeln('environment config file not found');
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

  procedure LoadRect(AKey:string; var ARect:TRect);
  begin
    ARect.Left:=XMLConfig.GetValue(AKey+'/Left',ARect.Left);
    ARect.Top:=XMLConfig.GetValue(AKey+'/Top',ARect.Top);
    ARect.Right:=XMLConfig.GetValue(AKey+'/Right',ARect.Right);
    ARect.Bottom:=XMLConfig.GetValue(AKey+'/Bottom',ARect.Bottom);
  end;

  procedure LoadBackupInfo(var BackupInfo: TBackupInfo; Path:string);
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

  procedure LoadRecentList(List: TStringList; Path: string);
  var i,Count: integer;
    s: string;
  begin
    Count:=XMLConfig.GetValue(Path+'Count',0);
    List.Clear;
    for i:=1 to Count do begin
      s:=XMLConfig.GetValue(Path+'Item'+IntToStr(i)+'/Value','');
      if s<>'' then List.Add(s);
    end;
  end;

begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    FileVersion:=XMLConfig.GetValue('EnvironmentOptions/Version/Value',0);

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
    FSaveWindowPositions:=XMLConfig.GetValue(
       'EnvironmentOptions/Desktop/SaveWindowPositions',FSaveWindowPositions);
    FWindowPositionsValid:=XMLConfig.GetValue(
       'EnvironmentOptions/Desktop/WindowPositionsValid',false);
    if FWindowPositionsValid then begin
      LoadRect('EnvironmentOptions/Desktop/MainWindowBounds',FMainWindowBounds);
      LoadRect('EnvironmentOptions/Desktop/SourceEditorBounds'
        ,FSourceEditorBounds);
    end;
    if FileVersion>=100 then begin
      FMessagesViewBoundsValid:=XMLConfig.GetValue(
        'EnvironmentOptions/Desktop/MessagesViewBoundsValid',false);
      if FMessagesViewBoundsValid then
        LoadRect('EnvironmentOptions/Desktop/MessagesViewBounds'
           ,FMessagesViewBounds);
    end;

    // form editor
    FDisplayGrid:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/DisplayGrid',FDisplayGrid);
    FSnapToGrid:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/SnapToGrid',FSnapToGrid);
    FShowComponentCaptions:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowComponentCaptions',FShowComponentCaptions);
    FShowEditorHints:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/ShowEditorHints',FShowEditorHints);
    FAutoCreateForms:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/AutoCreateForms',FAutoCreateForms);
    FGridSizeX:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridSizeX',FGridSizeX);
    FGridSizeY:=XMLConfig.GetValue(
       'EnvironmentOptions/FormEditor/GridSizeY',FGridSizeY);

    if not OnlyDesktop then begin
      // files
      FLazarusDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/LazarusDirectory/Value',FLazarusDirectory);
      FCompilerFilename:=XMLConfig.GetValue(
         'EnvironmentOptions/CompilerFilename/Value',FCompilerFilename);
      FFPCSourceDirectory:=XMLConfig.GetValue(
         'EnvironmentOptions/FPCSourceDirectory/Value',FFPCSourceDirectory);

      // backup
      LoadBackupInfo(FBackupInfoProjectFiles
        ,'EnvironmentOptions/BackupProjectFiles/');
      LoadBackupInfo(FBackupInfoOtherFiles
        ,'EnvironmentOptions/BackupOtherFiles/');
    end;

    // recent files and directories
    FMaxRecentOpenFiles:=XMLConfig.GetValue(
       'EnvironmentOptions/Recent/OpenFiles/Max',FMaxRecentOpenFiles);
    LoadRecentList(FRecentOpenFiles,'EnvironmentOptions/Recent/OpenFiles/');
    FMaxRecentProjectFiles:=XMLConfig.GetValue(
       'EnvironmentOptions/Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
    LoadRecentList(FRecentProjectFiles,'EnvironmentOptions/Recent/ProjectFiles/');
    FLastOpenDialogDir:=XMLConfig.GetValue(
       'EnvironmentOptions/Recent/LastOpenDialogDir/Value',FLastOpenDialogDir);

    XMLConfig.Free;

    // object inspector
    FObjectInspectorOptions.Filename:=FFilename;
    FObjectInspectorOptions.Load;
  except
    // ToDo
    writeln('[TEnvironmentOptions.Load]  error reading "',FFilename,'"');
  end;
end;

procedure TEnvironmentOptions.Save(OnlyDesktop: boolean);
var XMLConfig: TXMLConfig;

  procedure SaveRect(AKey:string; var ARect:TRect);
  begin
    XMLConfig.SetValue(AKey+'/Left',ARect.Left);
    XMLConfig.SetValue(AKey+'/Top',ARect.Top);
    XMLConfig.SetValue(AKey+'/Right',ARect.Right);
    XMLConfig.SetValue(AKey+'/Bottom',ARect.Bottom);
  end;

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

  procedure SaveRecentList(List: TStringList; Path: string);
  var i: integer;
  begin
    XMLConfig.SetValue(Path+'Count',List.Count);
    for i:=0 to List.Count-1 do
      XMLConfig.SetValue(Path+'Item'+IntToStr(i+1)+'/Value',List[i]);
  end;

begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    XMLConfig.SetValue('EnvironmentOptions/Version/Value',EnvOptsVersion);

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
    XMLConfig.SetValue('EnvironmentOptions/Desktop/SaveWindowPositions'
       ,FSaveWindowPositions);
    XMLConfig.SetValue('EnvironmentOptions/Desktop/WindowPositionsValid'
       ,FWindowPositionsValid);
    if FWindowPositionsValid then begin
      SaveRect('EnvironmentOptions/Desktop/MainWindowBounds',FMainWindowBounds);
      SaveRect('EnvironmentOptions/Desktop/SourceEditorBounds'
        ,FSourceEditorBounds);
    end;
    XMLConfig.SetValue('EnvironmentOptions/Desktop/MessagesViewBoundsValid'
       ,FMessagesViewBoundsValid);
    if FMessagesViewBoundsValid then
      SaveRect('EnvironmentOptions/Desktop/MessagesViewBounds'
        ,FMessagesViewBounds);

    // form editor
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/DisplayGrid',FDisplayGrid);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/SnapToGrid',FSnapToGrid);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/ShowComponentCaptions'
       ,FShowComponentCaptions);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/ShowEditorHints'
       ,FShowEditorHints);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/AutoCreateForms'
       ,FAutoCreateForms);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/GridSizeX',FGridSizeX);
    XMLConfig.SetValue('EnvironmentOptions/FormEditor/GridSizeY',FGridSizeY);

    if not OnlyDesktop then begin
      // files
      XMLConfig.SetValue(
         'EnvironmentOptions/LazarusDirectory/Value',FLazarusDirectory);
      XMLConfig.SetValue(
         'EnvironmentOptions/CompilerFilename/Value',FCompilerFilename);
      XMLConfig.SetValue(
         'EnvironmentOptions/FPCSourceDirectory/Value',FFPCSourceDirectory);

      // backup
      SaveBackupInfo(FBackupInfoProjectFiles
        ,'EnvironmentOptions/BackupProjectFiles/');
      SaveBackupInfo(FBackupInfoOtherFiles
        ,'EnvironmentOptions/BackupOtherFiles/');
    end;

    // recent files and directories
    XMLConfig.SetValue(
       'EnvironmentOptions/Recent/OpenFiles/Max',FMaxRecentOpenFiles);
    SaveRecentList(FRecentOpenFiles,'EnvironmentOptions/Recent/OpenFiles/');
    XMLConfig.SetValue(
       'EnvironmentOptions/Recent/ProjectFiles/Max',FMaxRecentProjectFiles);
    SaveRecentList(FRecentProjectFiles,'EnvironmentOptions/Recent/ProjectFiles/');
    XMLConfig.SetValue('EnvironmentOptions/Recent/LastOpenDialogDir/Value'
        ,FLastOpenDialogDir);

    XMLConfig.Flush;
    XMLConfig.Free;

    // object inspector
    FObjectInspectorOptions.Filename:=FFilename;
    FObjectInspectorOptions.SaveBounds:=
      FSaveWindowPositions and FWindowPositionsValid;
    FObjectInspectorOptions.Save;
  except
    // ToDo
    writeln('[TEnvironmentOptions.Load]  error writing "',FFilename,'"');
  end;
end;

procedure TEnvironmentOptions.AddToRecentList(const AFilename: string;
  RecentList: TStringList;  Max: integer);
var i: integer;
begin
  i:=RecentList.Count-1;
  while i>=0 do begin
    if RecentList[i]=AFilename then RecentList.Delete(i)
    else dec(i);
  end;
  RecentList.Insert(0,AFilename);
  if Max>0 then
    while RecentList.Count>Max do
      RecentList.Delete(RecentList.Count-1);
end;

procedure TEnvironmentOptions.AddToRecentOpenFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentOpenFiles,FMaxRecentOpenFiles);
end;

procedure TEnvironmentOptions.AddToRecentProjectFiles(const AFilename: string);
begin
  AddToRecentList(AFilename,FRecentProjectFiles,FMaxRecentProjectFiles);
end;

//==============================================================================

{ TEnvironmentOptionsDialog }

constructor TEnvironmentOptionsDialog.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-480) div 2,(Screen.Height-400) div 2, 485, 405);
    Caption:='Environment Options';
    
    NoteBook:=TNoteBook.Create(Self);
    with NoteBook do begin
      Name:='NoteBook';
      Parent:=Self;
      SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
      Pages[0]:='Desktop';
      Pages.Add('Files');
      Pages.Add('Backup');
    end;

    SetupDesktopPage;
    SetupFilesPage;
    SetupBackupPage;
    
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
      Show;
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
      Show;
    end;

  end;
  
end;

destructor TEnvironmentOptionsDialog.Destroy;
begin

  inherited Destroy;
end;

procedure TEnvironmentOptionsDialog.SetupDesktopPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  // auto save
  AutoSaveGroupBox:=TGroupBox.Create(Self);
  with AutoSaveGroupBox do begin
    Name:='AutoSaveGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=8;
    Top:=2;
    Width:=(MaxX div 2) - 15;
    Height:=108;
    Caption:='Auto save';
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
  end;


  // windows
  WindowsGroupBox:=TGroupBox.Create(Self);
  with WindowsGroupBox do begin
    Name:='WindowsGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=AutoSaveGroupBox.Left;
    Top:=AutoSaveGroupBox.Top+AutoSaveGroupBox.Height+5;
    Width:=AutoSaveGroupBox.Width;
    Height:=50;
    Caption:='Windows';
    Show;
  end;
  
  SaveWindowPositionsCheckBox:=TCheckBox.Create(Self);
  with SaveWindowPositionsCheckBox do begin
    Name:='SaveWindowPositionsCheckBox';
    Parent:=WindowsGroupBox;
    Left:=2;
    Top:=2;
    Width:=WindowsGroupBox.ClientWidth-2*Left;
    Height:=23;
    Caption:='Save window positions';
    Show;
  end;

  // desktop files
  DesktopFilesGroupBox:=TGroupBox.Create(Self);
  with DesktopFilesGroupBox do begin
    Name:='DesktopFilesGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=WindowsGroupBox.Left;
    Top:=WindowsGroupBox.Top+WindowsGroupBox.Height+5;
    Width:=WindowsGroupBox.Width;
    Height:=90;
    Caption:='Desktop files';
    Show;
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
    Show;
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
    Show;
  end;
  

  // form editor
  FormEditorGroupBox:=TGroupBox.Create(Self);
  with FormEditorGroupBox do begin
    Name:='FormEditorGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=AutoSaveGroupBox.Left+AutoSaveGroupBox.Width+10;
    Top:=AutoSaveGroupBox.Top;
    Width:=AutoSaveGroupBox.Width;
    Height:=203;
    Caption:='Form editor';
    Show;
  end;
  
  DisplayGridCheckBox:=TCheckBox.Create(Self);
  with DisplayGridCheckBox do begin
    Name:='DisplayGridCheckBox';
    Parent:=FormEditorGroupBox;
    Left:=2;
    Top:=2;
    Width:=FormEditorGroupBox.ClientWidth-2*Left;
    Height:=23;
    Caption:='Display grid';
    Enabled:=false;
    Show;
  end;
  
  SnapToGridCheckBox:=TCheckBox.Create(Self);
  with SnapToGridCheckBox do begin
    Name:='SnapToGridCheckBox';
    Parent:=FormEditorGroupBox;
    Left:=2;
    Top:=27;
    Width:=FormEditorGroupBox.ClientWidth-2*Left;
    Height:=23;
    Caption:='Snap to grid';
    Enabled:=false;
    Show;
  end;

  ShowComponentCaptionsCheckBox:=TCheckBox.Create(Self);
  with ShowComponentCaptionsCheckBox do begin
    Name:='ShowComponentCaptionsCheckBox';
    Parent:=FormEditorGroupBox;
    Left:=2;
    Top:=52;
    Width:=FormEditorGroupBox.ClientWidth-2*Left;
    Height:=23;
    Caption:='Show component captions';
    Enabled:=false;
    Show;
  end;

  ShowEditorHintsCheckBox:=TCheckBox.Create(Self);
  with ShowEditorHintsCheckBox do begin
    Name:='ShowEditorHintsCheckBox';
    Parent:=FormEditorGroupBox;
    Left:=2;
    Top:=77;
    Width:=FormEditorGroupBox.ClientWidth-2*Left;
    Height:=23;
    Caption:='Show editor hints';
    Enabled:=false;
    Show;
  end;

  AutoCreateFormsCheckBox:=TCheckBox.Create(Self);
  with AutoCreateFormsCheckBox do begin
    Name:='AutoCreateFormsCheckBox';
    Parent:=FormEditorGroupBox;
    Left:=2;
    Top:=102;
    Width:=FormEditorGroupBox.ClientWidth-2*Left;
    Height:=23;
    Caption:='Auto create forms';
    Enabled:=false;
    Show;
  end;

  GridSizeXLabel:=TLabel.Create(Self);
  with GridSizeXLabel do begin
    Name:='GridSizeXLabel';
    Parent:=FormEditorGroupBox;
    Left:=5;
    Top:=129;
    Width:=80;
    Height:=20;
    Caption:='Grid size X';
    Enabled:=false;
    Show;
  end;
  
  GridSizeXComboBox:=TComboBox.Create(Self);
  with GridSizeXComboBox do begin
    Name:='GridSizeXComboBox';
    Parent:=FormEditorGroupBox;
    Left:=GridSizeXLabel.Left+GridSizeXLabel.Width+5;
    Top:=GridSizeXLabel.Top+2;
    Width:=FormEditorGroupBox.ClientWidth-Left-10;
    Height:=23;
    with Items do begin
      BeginUpdate;
      Add('2');
      Add('5');
      Add('8');
      Add('10');
      EndUpdate;
    end;
    Enabled:=false;
    Show;
  end;
  
  GridSizeYLabel:=TLabel.Create(Self);
  with GridSizeYLabel do begin
    Name:='GridSizeYLabel';
    Parent:=FormEditorGroupBox;
    Left:=5;
    Top:=154;
    Width:=GridSizeXLabel.Width;
    Height:=20;
    Caption:='Grid size Y';
    Enabled:=false;
    Show;
  end;

  GridSizeYComboBox:=TComboBox.Create(Self);
  with GridSizeYComboBox do begin
    Name:='GridSizeYComboBox';
    Parent:=FormEditorGroupBox;
    Left:=GridSizeYLabel.Left+GridSizeYLabel.Width+5;
    Top:=GridSizeYLabel.Top+2;
    Width:=FormEditorGroupBox.ClientWidth-Left-10;
    Height:=23;
    with Items do begin
      BeginUpdate;
      Add('2');
      Add('5');
      Add('8');
      Add('10');
      EndUpdate;
    end;
    Enabled:=false;
    Show;
  end;

  // object inspector
  ObjectInspectorGroupBox:=TGroupBox.Create(Self);
  with ObjectInspectorGroupBox do begin
    Name:='ObjectInspectorGroupBox';
    Parent:=NoteBook.Page[0];
    Left:=FormEditorGroupBox.Left;
    Top:=FormEditorGroupBox.Top+FormEditorGroupBox.Height+5;
    Width:=FormEditorGroupBox.Width;
    Height:=50;
    Caption:='Object inspector';
    Show;
  end;

  BackgroundColorButton:=TColorButton.Create(Self);
  with BackgroundColorButton do begin
    Name:='BackgroundColorButton';
    Parent:=ObjectInspectorGroupBox;
    Left:=5;
    Top:=2;
    Width:=50;
    Height:=25;
    Show;
  end;

  BackgroundColorLabel:=TLabel.Create(Self);
  with BackgroundColorLabel do begin
    Name:='BackgroundColorLabel';
    Parent:=ObjectInspectorGroupBox;
    Left:=BackgroundColorButton.Left+BackgroundColorButton.Width+5;
    Top:=BackgroundColorButton.Top;
    Width:=ObjectInspectorGroupBox.ClientWidth-Left-5;
    Height:=23;
    Caption:='Background color';
    Show;
  end;

end;

procedure TEnvironmentOptionsDialog.SetupBackupPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  BackupHelpLabel:=TLabel.Create(Self);
  with BackupHelpLabel do begin
    Name:='BackupHelpLabel';
    Parent:=NoteBook.Page[2];
    Left:=5;
    Top:=2;
    Width:=MaxX-Left*2;
    Height:=23;
    Caption:='Notes: ';
    Show;
  end;

  BackupProjectGroupBox:=TGroupBox.Create(Self);
  with BackupProjectGroupBox do begin
    Name:='BackupProjectGroupBox';
    Parent:=NoteBook.Page[2];
    Left:=4;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Caption:='Project files';
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
  end;

  BackupOtherGroupBox:=TGroupBox.Create(Self);
  with BackupOtherGroupBox do begin
    Name:='BackupOtherGroupBox';
    Parent:=NoteBook.Page[2];
    Left:=BackupProjectGroupBox.Left+BackupProjectGroupBox.Width+10;
    Top:=BackupHelpLabel.Top+BackupHelpLabel.Height+4;
    Width:=(MaxX div 2) - 11;
    Height:=260;
    Caption:='Other files';
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
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
    Show;
  end;
end;

procedure TEnvironmentOptionsDialog.SetupFilesPage;
var MaxX:integer;
begin
  MaxX:=ClientWidth-5;

  MaxRecentOpenFilesLabel:=TLabel.Create(Self);
  with MaxRecentOpenFilesLabel do begin
    Name:='MaxRecentOpenFilesLabel';
    Parent:=NoteBook.Page[1];
    Left:=4;
    Top:=4;
    Width:=150;
    Height:=23;
    Caption:='Max recent files';
    Show;
  end;

  MaxRecentOpenFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentOpenFilesComboBox do begin
    Name:='MaxRecentOpenFilesComboBox';
    Parent:=NoteBook.Page[1];
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
    Show;
  end;

  MaxRecentProjectFilesLabel:=TLabel.Create(Self);
  with MaxRecentProjectFilesLabel do begin
    Name:='MaxRecentProjectFilesLabel';
    Parent:=NoteBook.Page[1];
    Left:=MaxRecentOpenFilesLabel.Left;
    Top:=MaxRecentOpenFilesLabel.Top+MaxRecentOpenFilesLabel.Height+3;
    Width:=MaxRecentOpenFilesLabel.Width;
    Height:=MaxRecentOpenFilesLabel.Height;
    Caption:='Max recent project files';
    Show;
  end;

  MaxRecentProjectFilesComboBox:=TComboBox.Create(Self);
  with MaxRecentProjectFilesComboBox do begin
    Name:='MaxRecentProjectFilesComboBox';
    Parent:=NoteBook.Page[1];
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
    Show;
  end;
  
  OpenLastProjectAtStartCheckBox:=TCheckBox.Create(Self);
  with OpenLastProjectAtStartCheckBox do begin
    Name:='OpenLastProjectAtStartCheckBox';
    Parent:=NoteBook.Page[1];
    Left:=4;
    Top:=MaxRecentProjectFilesLabel.Top+MaxRecentProjectFilesLabel.Height+5;
    Width:=MaxX-10;
    Height:=23;
    Caption:='Open last project at start';
    Show;
  end;

  LazarusDirLabel:=TLabel.Create(Self);
  with LazarusDirLabel do begin
    Name:='LazarusDirLabel';
    Parent:=NoteBook.Page[1];
    Left:=4;
    Top:=OpenLastProjectAtStartCheckBox.Top
        +OpenLastProjectAtStartCheckBox.Height+5;
    Width:=MaxX-10;
    Height:=23;
    Caption:='Lazarus directory (default for all projects)';
    Show;
  end;

  LazarusDirComboBox:=TComboBox.Create(Self);
  with LazarusDirComboBox do begin
    Name:='LazarusDirComboBox';
    Parent:=NoteBook.Page[1];
    Left:=LazarusDirLabel.Left;
    Top:=LazarusDirLabel.Top+LazarusDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add(ExtractFilePath(ParamStr(0)));
      EndUpdate;
    end;
    Show;
  end;

  CompilerPathLabel:=TLabel.Create(Self);
  with CompilerPathLabel do begin
    Name:='CompilerPathLabel';
    Parent:=NoteBook.Page[1];
    Left:=LazarusDirLabel.Left;
    Top:=LazarusDirComboBox.Top+LazarusDirComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    Caption:='Compiler path (ppc386)';
    Show;
  end;

  CompilerPathComboBox:=TComboBox.Create(Self);
  with CompilerPathComboBox do begin
    Name:='CompilerPathComboBox';
    Parent:=NoteBook.Page[1];
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
    Show;
  end;

  FPCSourceDirLabel:=TLabel.Create(Self);
  with FPCSourceDirLabel do begin
    Name:='FPCSourceDirLabel';
    Parent:=NoteBook.Page[1];
    Left:=LazarusDirLabel.Left;
    Top:=CompilerPathComboBox.Top+CompilerPathComboBox.Height;
    Width:=LazarusDirLabel.Width;
    Height:=23;
    Caption:='FPC source directory';
    Show;
  end;

  FPCSourceDirComboBox:=TComboBox.Create(Self);
  with FPCSourceDirComboBox do begin
    Name:='FPCSourceDirComboBox';
    Parent:=NoteBook.Page[1];
    Left:=LazarusDirLabel.Left;
    Top:=FPCSourceDirLabel.Top+FPCSourceDirLabel.Height+2;
    Width:=LazarusDirLabel.Width;
    Height:=25;
    with Items do begin
      BeginUpdate;
      Add('');
      EndUpdate;
    end;
    Show;
  end;
end;

procedure TEnvironmentOptionsDialog.BakTypeRadioGroupClick(Sender: TObject);
var i: integer;
begin
  i:=TRadioGroup(Sender).ItemIndex;
  if Sender=BakProjTypeRadioGroup then begin
writeln('[TEnvironmentOptionsDialog.BakTypeRadioGroupClick] ',i);
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
begin
  with AnEnvironmentOptions do begin
    // auto save
    AutoSaveEditorFilesCheckBox.Checked:=AutoSaveEditorFiles;
    AutoSaveProjectCheckBox.Checked:=AutoSaveProject;
    SetComboBoxText(AutoSaveIntervalInSecsComboBox
       ,IntToStr(AutoSaveIntervalInSecs));

    // desktop
    SaveWindowPositionsCheckBox.Checked:=SaveWindowPositions;

    // object inspector
    BackgroundColorButton.ButtonColor:=
       ObjectInspectorOptions.GridBackgroundColor;

    // form editor
    DisplayGridCheckBox.Checked:=DisplayGrid;
    SnapToGridCheckBox.Checked:=SnapToGrid;
    ShowComponentCaptionsCheckBox.Checked:=ShowComponentCaptions;
    ShowEditorHintsCheckBox.Checked:=ShowEditorHints;
    AutoCreateFormsCheckBox.Checked:=AutoCreateForms;
    SetComboBoxText(GridSizeXComboBox,IntToStr(GridSizeX));
    SetComboBoxText(GridSizeYComboBox,IntToStr(GridSizeY));

    // files
    SetComboBoxText(LazarusDirComboBox,LazarusDirectory);
    SetComboBoxText(CompilerPathComboBox,CompilerFilename);
    SetComboBoxText(FPCSourceDirComboBox,FPCSourceDirectory);

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
  end;
end;

procedure TEnvironmentOptionsDialog.WriteSettings(
  AnEnvironmentOptions: TEnvironmentOptions);
begin
  with AnEnvironmentOptions do begin
    // auto save
    AutoSaveEditorFiles:=AutoSaveEditorFilesCheckBox.Checked;
    AutoSaveProject:=AutoSaveProjectCheckBox.Checked;
    AutoSaveIntervalInSecs:=StrToIntDef(
      AutoSaveIntervalInSecsComboBox.Text,AutoSaveIntervalInSecs);

    // desktop
    SaveWindowPositions:=SaveWindowPositionsCheckBox.Checked;

    // object inspector
    ObjectInspectorOptions.GridBackgroundColor:=
       BackgroundColorButton.ButtonColor;

    // form editor
    DisplayGrid:=DisplayGridCheckBox.Checked;
    SnapToGrid:=SnapToGridCheckBox.Checked;
    ShowComponentCaptions:=ShowComponentCaptionsCheckBox.Checked;
    ShowEditorHints:=ShowEditorHintsCheckBox.Checked;
    AutoCreateForms:=AutoCreateFormsCheckBox.Checked;
    GridSizeX:=StrToIntDef(GridSizeXComboBox.Text,GridSizeX);
    GridSizeY:=StrToIntDef(GridSizeYComboBox.Text,GridSizeY);

    // files
    LazarusDirectory:=LazarusDirComboBox.Text;
    CompilerFilename:=CompilerPathComboBox.Text;
    FPCSourceDirectory:=FPCSourceDirComboBox.Text;

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

end.

