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

type
  //----------------------------------------------------------------------------
  { class for storing environment options }
  TEnvironmentOptions = class
  private
    FFilename: string;
    
    // auto save
    FAutoSaveEditorFiles: boolean;
    FAutoSaveProject: boolean;
    FAutoSaveIntervalInSecs: integer;
    
    // windows
    FSaveWindowPositions: boolean;
    FWindowPositionsValid: boolean; // the following values are valid
    FMainWindowBounds: TRect;
    FSourceEditorBounds: TRect;
    
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
    
    procedure SetFileName(NewFilename: string);
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
    procedure SetComboBoxText(AComboBox:TComboBox; AText:AnsiString);

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

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
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

const EnvOptsConfFileName='environmentoptions.xml';

constructor TEnvironmentOptions.Create;
begin
  inherited Create;

  FFilename:='';

  // auto save
  FAutoSaveEditorFiles:=true;
  FAutoSaveProject:=true;
  FAutoSaveIntervalInSecs:=300; // 5 minutes

  // windows
  FSaveWindowPositions:=true;
  FWindowPositionsValid:=false;

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
end;

destructor TEnvironmentOptions.Destroy;
begin
  FObjectInspectorOptions.Free;
  inherited Destroy;
end;

procedure TEnvironmentOptions.SetLazarusDefaultFilename;
var
  ConfFileName,SecConfFileName:string;
begin
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+EnvOptsConfFileName);
  if (not FileExists(ConfFileName)) then begin
    SecConfFileName:=SetDirSeparators(
      GetSecondaryConfigPath+'/'+EnvOptsConfFileName);
    if (not FileExists(SecConfFileName)) then begin
      // XXX
      writeln('environment config file not found');
    end else begin
      ConfFileName:=SecConfFileName;
    end;
  end;
  FFilename:=ConfFilename;
end;

procedure TEnvironmentOptions.SetFileName(NewFilename: string);
begin
  if FFilename=NewFilename then exit;
  FFilename:=NewFilename;
end;

procedure TEnvironmentOptions.Load(OnlyDesktop:boolean);
var XMLConfig: TXMLConfig;

  procedure LoadRect(AKey:string; var ARect:TRect);
  begin
    ARect.Left:=XMLConfig.GetValue(AKey+'/Left',ARect.Left);
    ARect.Top:=XMLConfig.GetValue(AKey+'/Top',ARect.Top);
    ARect.Right:=XMLConfig.GetValue(AKey+'/Right',ARect.Right);
    ARect.Bottom:=XMLConfig.GetValue(AKey+'/Bottom',ARect.Bottom);
  end;

begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);

    // auto save
    FAutoSaveEditorFiles:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/EditorFiles',FAutoSaveEditorFiles);
    FAutoSaveProject:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/Project',FAutoSaveProject);
    FAutoSaveIntervalInSecs:=XMLConfig.GetValue(
       'EnvironmentOptions/AutoSave/IntervalInSecs',FAutoSaveIntervalInSecs);

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

    end;

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

begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);

    // auto save
    XMLConfig.SetValue('EnvironmentOptions/AutoSave/EditorFiles'
       ,FAutoSaveEditorFiles);
    XMLConfig.SetValue('EnvironmentOptions/AutoSave/Project',FAutoSaveProject);
    XMLConfig.GetValue('EnvironmentOptions/AutoSave/IntervalInSecs'
       ,FAutoSaveIntervalInSecs);

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

    end;

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

//==============================================================================

{ TEnvironmentOptionsDialog }

constructor TEnvironmentOptionsDialog.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-480) div 2,(Screen.Height-350) div 2, 485, 355);
    Caption:='Environment Options';
    
    NoteBook:=TNoteBook.Create(Self);
    with NoteBook do begin
      Name:='NoteBook';
      Parent:=Self;
      SetBounds(0,0,Self.ClientWidth-4,Self.ClientHeight-50);
      Pages[0]:='Desktop';
    end;

    SetupDesktopPage;
    
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
    Show;
  end;

  // object inspector
  ObjectInspectorGroupBox:=TGroupBox.Create(Self);
  with ObjectInspectorGroupBox do begin
    Name:='ObjectInspectorGroupBox';
    Parent:=NoteBook.Page[0];;
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
    Left:=2;
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
    Top:=BackgroundColorButton.Top+2;
    Width:=ObjectInspectorGroupBox.ClientWidth-Left-5;
    Height:=23;
    Caption:='Background color';
    Show;
  end;

end;

procedure TEnvironmentOptionsDialog.OkButtonClick(Sender: TObject);
begin
  // ToDo: save options
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
  end;
end;

procedure TEnvironmentOptionsDialog.SetComboBoxText(
  AComboBox:TComboBox;AText:AnsiString);
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

