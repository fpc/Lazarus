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
    Run Parameters Options (TRunParamsOptions)
    and Dialog for them (TRunParamsOptsDlg)
    
    Run Parameters are project specific options for the debugger like
    command line parameters and working directory.
    
    The options saved in a TRunParamsOptions are stored in the project info file
    (.lpi) together with the rest of the project.
    
    The dialog will be activated by main.pp with the function
    ShowRunParamsOptsDlg (see below) when the user clicks on the
    menu->Run->Run Parameters.
}
unit RunParamsOpts;

{$mode objfpc}
{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils,
  // LCL
  Controls, Forms, Buttons, StdCtrls, ComCtrls, Dialogs, ButtonPanel, ExtCtrls,
  // IdeIntf
  BaseIDEIntf, IDEHelpIntf, ProjectIntf, IDEDialogs, IDEImagesIntf, MacroIntf,
  // LazUtils
  LazFileUtils, LazFileCache, LazUTF8, Laz2_XMLCfg,
  // IDE
  IDEProcs, MiscOptions, SysVarUserOverrideDlg, InputHistory, LazarusIDEStrConsts;

{ The xml format version:
    When the format changes (new values, changed formats) we can distinguish old
    files and are able to convert them.
}
const
  RunParamsOptionsVersion = 2;

type
  {
    the storage object for run parameters
  }

  TRunParamsOptionsModeSave = (rpsLPS, rpsLPI);

  { TRunParamsOptionsMode }

  TRunParamsOptionsMode = class(TAbstractRunParamsOptionsMode)
  private
    fSaveIn: TRunParamsOptionsModeSave;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property SaveIn: TRunParamsOptionsModeSave read fSaveIn write fSaveIn;
  public
    procedure AssignEnvironmentTo(Strings: TStrings); override;

    function LegacyLoad(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function Load(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function LegacySave(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;

    procedure Clear; override;
  end;

  { TRunParamsOptions }

  TRunParamsOptions = class(TAbstractRunParamsOptions)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function CreateMode(const AName: string): TAbstractRunParamsOptionsMode; override;
    procedure SetActiveModeName(const AValue: string); override;
  public
    procedure AssignEnvironmentTo(Strings: TStrings); override;

    function LegacyLoad(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function Load(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean; const ASaveIn: TRunParamsOptionsModeSave): TModalResult;
    function LegacySave(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch; const ASaveIn: TRunParamsOptionsModeSave): TModalResult;
    function GetActiveMode: TRunParamsOptionsMode;
  end;

  { TRunParamsOptsDlg - the form of the run parameters options dialog }

  TRunParamsOptsDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    CmdLineParametersComboBox: TComboBox;
    CmdLineParametersGroupBox: TGroupBox;
    DeleteModeButton: TToolButton;
    DisplayEdit: TEdit;
    DisplayGroupBox: TGroupBox;
    EnvVarsPage: TTabSheet;
    GeneralPage: TTabSheet;
    HostApplicationBrowseBtn: TButton;
    HostApplicationEdit: TEdit;
    HostApplicationGroupBox: TGroupBox;
    IncludeSystemVariablesCheckBox: TCheckBox;
    ModesComboBox: TComboBox;
    ModesLabel: TLabel;
    NewModeButton: TToolButton;
    Notebook: TPageControl;
    Panel1: TPanel;
    PreviewMemo: TMemo;
    PreviewMultilineCheckBox: TCheckBox;
    PreviewPage: TTabSheet;
    SaveInComboBox: TComboBox;
    SaveInLabel: TLabel;
    SystemVariablesGroupBox: TGroupBox;
    SystemVariablesListView: TListView;
    ToolBar1: TToolBar;
    UseDisplayCheckBox: TCheckBox;
    UseLaunchingApplicationCheckBox: TCheckBox;
    UseLaunchingApplicationComboBox: TComboBox;
    UseLaunchingApplicationGroupBox: TGroupBox;
    UserOverridesAddButton: TBitBtn;
    UserOverridesDeleteButton: TBitBtn;
    UserOverridesEditButton: TBitBtn;
    UserOverridesGroupBox: TGroupBox;
    UserOverridesListView: TListView;
    WorkingDirectoryBtn: TButton;
    WorkingDirectoryComboBox: TComboBox;
    WorkingDirectoryGroupBox: TGroupBox;
    procedure DeleteModeButtonClick(Sender: TObject);
    procedure EnvVarsPageResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ModesComboBoxChange(Sender: TObject);
    procedure NotebookChange(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure HostApplicationBrowseBtnClick(Sender: TObject);
    procedure NewModeButtonClick(Sender: TObject);
    procedure PreviewMultilineCheckBoxChange(Sender: TObject);
    procedure UseLaunchingApplicationCheckBoxChange(Sender: TObject);
    procedure UserOverridesListViewSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure WorkingDirectoryBtnClick(Sender: TObject);
    procedure UserOverridesAddButtonClick(Sender: TObject);
    procedure UserOverridesEditButtonClick(Sender: TObject);
    procedure UserOverridesDeleteButtonClick(Sender: TObject);
  private
    fHistoryLists: THistoryLists;
    fOptions: TRunParamsOptions;
    fSaveToOptions: TRunParamsOptions;
    fLastSelectedMode: TRunParamsOptionsMode;
    procedure SetupNotebook;
    procedure SetupLocalPage;
    procedure SetupEnvironmentPage;
    procedure SetupPreviewPage;
    procedure SetOptions(NewOptions: TRunParamsOptions);
    procedure FillListView(ListView: TListView; sl: TStringList);
    procedure FillSystemVariablesListView;
    procedure FillUserOverridesListView(const AMode: TRunParamsOptionsMode);
    procedure ReloadModesComboBox;
    procedure UpdatePreview;
    procedure SaveToOptions;
    procedure SaveToOptionsMode(const AMode: TRunParamsOptionsMode);
    procedure LoadFromOptionsMode(const AMode: TRunParamsOptionsMode);
    procedure SaveUserOverrides(const AMode: TRunParamsOptionsMode);
    procedure SelectMode(const AName: string);
    function SelectedMode: TRunParamsOptionsMode;
    procedure SetComboBoxText(AComboBox: TComboBox; AText: ansistring);
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property Options: TRunParamsOptions Write SetOptions;
    property HistoryLists: THistoryLists read fHistoryLists write fHistoryLists;
  end;

function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions; HistoryLists: THistoryLists): TModalResult;

implementation

{$R *.lfm}

const
  DefaultLauncherTitle = '''Lazarus Run Output''';
  DefaultLauncherApplication = '$(LazarusDir)/tools/runwait.sh $(TargetCmdLine)';

  hlLaunchingApplication = 'LaunchingApplication';
  hlCmdLineParameters = 'CommandLineParameters';
  hlWorkingDirectory = 'WorkingDirectory';

function FindTerminalInPath(const ATerm: String = ''): String;
var
  List: TStrings;
  i: Integer;
  s: String;
  Term: String;
begin
  Result := '';
  List := TStringList.Create;
  {$IFDEF MSWINDOWS}
  List.Delimiter := ';';
  {$ELSE}
  List.Delimiter := ':';
  {$ENDIF}
  Term := ATerm;
  if Term = '' then
    Term := GetEnvironmentVariableUTF8('TERM');
  if Term = '' then
    Term := 'xterm';
  List.DelimitedText := GetEnvironmentVariableUTF8('PATH');
  for i := 0 to List.Count - 1 do
  begin
    S := List.Strings[i] + PathDelim + Term;
    if FileExistsCached(S) and FileIsExecutableCached(S) then
    begin
      // gnome-terminal is not compatible to xterm params.
      if Term = 'gnome-terminal' then
        Result := S + ' -t ' + DefaultLauncherTitle + ' -e ' +
          '''' + DefaultLauncherApplication + ''''
      else
        Result := S + ' -T ' + DefaultLauncherTitle + ' -e ' +
          DefaultLauncherApplication;
      break;
    end;
  end;
  List.Free;
end;

var
  DefaultLaunchingApplicationPathPlusParams: string;

function GetDefaultLaunchingApplicationPathPlusParams: string;
begin
  Result:=DefaultLaunchingApplicationPathPlusParams;
  if Result<>'' then exit;
  Result:=FindTerminalInPath;
  DefaultLaunchingApplicationPathPlusParams:=Result;
end;

function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions;
  HistoryLists: THistoryLists): TModalResult;
var
  RunParamsOptsForm: TRunParamsOptsDlg;
begin
  Result := mrCancel;
  RunParamsOptsForm := TRunParamsOptsDlg.Create(nil);
  try
    RunParamsOptsForm.HistoryLists := HistoryLists;
    RunParamsOptsForm.Options := RunParamsOptions;
    Result := RunParamsOptsForm.ShowModal;
  finally
    RunParamsOptsForm.Free;
  end;
end;

{ TRunParamsOptions }

procedure TRunParamsOptions.AssignEnvironmentTo(Strings: TStrings);
begin
  if GetActiveMode=nil then
    BaseIDEIntf.AssignEnvironmentTo(Strings, nil)
  else
    GetActiveMode.AssignEnvironmentTo(Strings);
end;

procedure TRunParamsOptions.AssignTo(Dest: TPersistent);
var
  ADest: TRunParamsOptions;
begin
  inherited AssignTo(Dest);

  if Dest is TRunParamsOptions then
  begin
    ADest := TRunParamsOptions(Dest);

    ADest.ActiveModeName := ActiveModeName;
  end;
end;

function TRunParamsOptions.CreateMode(const AName: string
  ): TAbstractRunParamsOptionsMode;
begin
  Result := TRunParamsOptionsMode.Create(AName);
end;

procedure TRunParamsOptions.SetActiveModeName(const AValue: string);
var
  NewMode: TAbstractRunParamsOptionsMode;
begin
  if AValue=ActiveModeName then exit;
  NewMode:=Find(AValue);
  if NewMode<>nil then
    inherited SetActiveModeName(NewMode.Name)
  else if AValue<>'' then
    raise EListError.Create('TRunParamsOptions.SetActiveModeName no such mode "'+AValue+'"')
  else
    inherited SetActiveModeName('');
end;

function TRunParamsOptions.GetActiveMode: TRunParamsOptionsMode;
var
  AMode: TAbstractRunParamsOptionsMode;
begin
  AMode := Find(ActiveModeName);
  if AMode=nil then exit(nil);
  Result := AMode as TRunParamsOptionsMode;
end;

function TRunParamsOptions.LegacyLoad(XMLConfig: TXMLConfig;
  const Path: string; AdjustPathDelims: boolean): TModalResult;
var
  NewMode: TRunParamsOptionsMode;
begin
  Clear;

  NewMode := Add('default') as TRunParamsOptionsMode;
  NewMode.SaveIn :=  rpsLPI;
  Result := NewMode.LegacyLoad(XMLConfig, Path+'RunParams/', AdjustPathDelims);
  ActiveModeName := 'default';
end;

function TRunParamsOptions.LegacySave(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch): TModalResult;
var
  AMode: TRunParamsOptionsMode;
begin
  Result := mrOK;

  AMode := GetActiveMode;
  if (AMode<>nil) and (AMode.SaveIn=rpsLPI) then
    AMode.LegacySave(XMLConfig, Path+'RunParams/', UsePathDelim);
end;

function TRunParamsOptions.Load(XMLConfig: TXMLConfig; const Path: string;
  AdjustPathDelims: boolean; const ASaveIn: TRunParamsOptionsModeSave
  ): TModalResult;
var
  Cnt, I: Integer;
  NewMode: TRunParamsOptionsMode;
  ModePath, NewActiveModeName: string;
begin
  //don't clear!  needed for merging lpi and lps

  Cnt := XMLConfig.GetValue(Path + 'Modes/Count', 0);
  Result := mrOK;

  for I := 0 to Cnt-1 do
  begin
    ModePath := Path+'Modes/Mode'+IntToStr(I)+'/';
    NewMode := Add(XMLConfig.GetValue(ModePath+'Name', '')) as TRunParamsOptionsMode;
    NewMode.SaveIn := ASaveIn;
    Result := NewMode.Load(XMLConfig, ModePath, AdjustPathDelims);
    if Result<>mrOK then
      Exit;
  end;

  if ASaveIn=rpsLPS then
  begin
    NewActiveModeName := XMLConfig.GetValue(Path + 'Modes/ActiveMode', '');
    // sanity check -> modes from LPI could be modified independently on LPS and
    // NewActiveModeName doesn't have to exist any more
    if Assigned(Find(NewActiveModeName)) then
      ActiveModeName := NewActiveModeName;
    if (GetActiveMode=nil) and (Count>0) then
      ActiveModeName := Modes[0].Name;
  end;
end;

function TRunParamsOptions.Save(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch; const ASaveIn: TRunParamsOptionsModeSave
  ): TModalResult;
var
  AMode: TRunParamsOptionsMode;
  I, Cnt: Integer;
begin
  Result := mrOK;

  // save a format version to distinguish old formats
  XMLConfig.SetValue(Path + 'FormatVersion/Value',
    RunParamsOptionsVersion);

  Cnt := 0;
  for I := 0 to Count-1 do
  begin
    AMode := Modes[I] as TRunParamsOptionsMode;

    if AMode.SaveIn=ASaveIn then
    begin
      Result := AMode.Save(XMLConfig, Path+'Modes/Mode'+IntToStr(Cnt)+'/', UsePathDelim);
      if Result<>mrOK then
        Exit;
      Inc(Cnt);
    end;
  end;

  XMLConfig.SetValue(Path + 'Modes/Count', Cnt);
  if ASaveIn=rpsLPS then
    XMLConfig.SetValue(Path + 'Modes/ActiveMode', ActiveModeName);
end;

{ TRunParamsOptionsMode }

function TRunParamsOptionsMode.LegacyLoad(XMLConfig: TXMLConfig;
  const Path: string; AdjustPathDelims: boolean): TModalResult;

  function f(const Filename: string): string;
  begin
    Result := SwitchPathDelims(Filename, AdjustPathDelims);
  end;

  procedure LoadUserOverrides(const APath: string);
  var
    i, Cnt: integer;
  begin
    Cnt := XMLConfig.GetValue(APath + 'Count', 0);
    for i := 0 to Cnt - 1 do
    begin
      UserOverrides.Values[XMLConfig.GetValue(
        APath + 'Variable' + IntToStr(i) + '/Name', '')] :=
        XMLConfig.GetValue(APath + 'Variable' + IntToStr(i) + '/Value', '');
    end;
  end;

begin
  // local options
  HostApplicationFilename := f(XMLConfig.GetValue(
    Path + 'local/HostApplicationFilename/Value',
    HostApplicationFilename));
  CmdLineParams := f(XMLConfig.GetValue(
    Path + 'local/CommandLineParams/Value', CmdLineParams));
  UseLaunchingApplication := XMLConfig.GetValue(
    Path + 'local/LaunchingApplication/Use', UseLaunchingApplication);
  LaunchingApplicationPathPlusParams :=
    f(XMLConfig.GetValue(Path + 'local/LaunchingApplication/PathPlusParams',
                         f(GetDefaultLaunchingApplicationPathPlusParams)));
  WorkingDirectory := f(XMLConfig.GetValue(
    Path + 'local/WorkingDirectory/Value', WorkingDirectory));
  UseDisplay := XMLConfig.GetValue(Path + 'local/Display/Use',
    UseDisplay);
  Display    := XMLConfig.GetValue(Path + 'local/Display/Value', Display);

  // environment options
  LoadUserOverrides(Path + 'environment/UserOverrides/');
  IncludeSystemVariables := XMLConfig.GetValue(
    Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables);

  Result := mrOk;
end;

function TRunParamsOptionsMode.LegacySave(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch): TModalResult;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure SaveUserOverrides(const APath: string);
  var
    i: integer;
  begin
    XMLConfig.SetDeleteValue(APath + 'Count', UserOverrides.Count, 0);
    for i := 0 to UserOverrides.Count - 1 do
    begin
      XMLConfig.SetValue(APath + 'Variable' + IntToStr(i) + '/Name',
        UserOverrides.Names[i]); // no default
      XMLConfig.SetDeleteValue(APath + 'Variable' + IntToStr(i) + '/Value',
        UserOverrides.Values[UserOverrides.Names[i]],'');
    end;
  end;

begin
  // local options
  XMLConfig.SetDeleteValue(Path + 'local/HostApplicationFilename/Value',
    f(HostApplicationFilename), '');
  XMLConfig.SetDeleteValue(Path + 'local/CommandLineParams/Value',
    f(CmdLineParams), '');
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/Use',
    UseLaunchingApplication, False);
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/PathPlusParams',
    f(LaunchingApplicationPathPlusParams), f(GetDefaultLaunchingApplicationPathPlusParams));
  XMLConfig.SetDeleteValue(Path + 'local/WorkingDirectory/Value',
    f(WorkingDirectory), '');
  XMLConfig.SetDeleteValue(Path + 'local/Display/Use',
    UseDisplay, False);
  XMLConfig.SetDeleteValue(Path + 'local/Display/Value',
    Display, ':0');

  // environment options
  SaveUserOverrides(Path + 'environment/UserOverrides/');
  XMLConfig.SetDeleteValue(Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables, False);

  Result := mrOk;
end;

function TRunParamsOptionsMode.Load(XMLConfig: TXMLConfig; const Path: string;
  AdjustPathDelims: boolean): TModalResult;

  function f(const Filename: string): string;
  begin
    Result := SwitchPathDelims(Filename, AdjustPathDelims);
  end;

  procedure LoadUserOverrides(const APath: string);
  var
    i, Cnt: integer;
  begin
    Cnt := XMLConfig.GetValue(APath + 'Count', 0);
    for i := 0 to Cnt - 1 do
    begin
      UserOverrides.Values[XMLConfig.GetValue(
        APath + 'Variable' + IntToStr(i) + '/Name', '')] :=
        XMLConfig.GetValue(APath + 'Variable' + IntToStr(i) + '/Value', '');
    end;
  end;

begin
  // local options
  HostApplicationFilename := f(XMLConfig.GetValue(
    Path + 'local/HostApplicationFilename/Value',
    HostApplicationFilename));
  CmdLineParams := f(XMLConfig.GetValue(
    Path + 'local/CommandLineParams/Value', CmdLineParams));
  UseLaunchingApplication := XMLConfig.GetValue(
    Path + 'local/LaunchingApplication/Use', UseLaunchingApplication);
  LaunchingApplicationPathPlusParams :=
    f(XMLConfig.GetValue(Path + 'local/LaunchingApplication/PathPlusParams',
                         f(GetDefaultLaunchingApplicationPathPlusParams)));
  WorkingDirectory := f(XMLConfig.GetValue(
    Path + 'local/WorkingDirectory/Value', WorkingDirectory));
  UseDisplay := XMLConfig.GetValue(Path + 'local/Display/Use',
    UseDisplay);
  Display    := XMLConfig.GetValue(Path + 'local/Display/Value', Display);

  // environment options
  LoadUserOverrides(Path + 'environment/UserOverrides/');
  IncludeSystemVariables := XMLConfig.GetValue(
    Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables);

  Result := mrOk;
end;

function TRunParamsOptionsMode.Save(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch): TModalResult;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure SaveUserOverrides(const APath: string);
  var
    i: integer;
  begin
    XMLConfig.SetDeleteValue(APath + 'Count', UserOverrides.Count, 0);
    for i := 0 to UserOverrides.Count - 1 do
    begin
      XMLConfig.SetValue(APath + 'Variable' + IntToStr(i) + '/Name',
        UserOverrides.Names[i]); // no default
      XMLConfig.SetDeleteValue(APath + 'Variable' + IntToStr(i) + '/Value',
        UserOverrides.Values[UserOverrides.Names[i]],'');
    end;
  end;

begin
  XMLConfig.SetValue(Path + 'Name', Name);

  // local options
  XMLConfig.SetDeleteValue(Path + 'local/HostApplicationFilename/Value',
    f(HostApplicationFilename), '');
  XMLConfig.SetDeleteValue(Path + 'local/CommandLineParams/Value',
    f(CmdLineParams), '');
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/Use',
    UseLaunchingApplication, False);
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/PathPlusParams',
    f(LaunchingApplicationPathPlusParams), f(GetDefaultLaunchingApplicationPathPlusParams));
  XMLConfig.SetDeleteValue(Path + 'local/WorkingDirectory/Value',
    f(WorkingDirectory), '');
  XMLConfig.SetDeleteValue(Path + 'local/Display/Use',
    UseDisplay, False);
  XMLConfig.SetDeleteValue(Path + 'local/Display/Value',
    Display, ':0');

  // environment options
  SaveUserOverrides(Path + 'environment/UserOverrides/');
  XMLConfig.SetDeleteValue(Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables, False);

  Result := mrOk;
end;

procedure TRunParamsOptionsMode.AssignEnvironmentTo(Strings: TStrings);
var
  idx: integer;
begin
  BaseIDEIntf.AssignEnvironmentTo(Strings, UserOverrides);

  if UseDisplay then
  begin
    // assignment is not allowed in a sorted list
    // Strings.Values['DISPLAY']:=Display;
    idx := Strings.IndexOfName('DISPLAY');
    if idx <> -1 then
      Strings.Delete(idx);
    Strings.Add('DISPLAY=' + Display);
  end;
end;

procedure TRunParamsOptionsMode.AssignTo(Dest: TPersistent);
var
  ADest: TRunParamsOptionsMode;
begin
  inherited AssignTo(Dest);

  if Dest is TRunParamsOptionsMode then
  begin
    ADest := Dest as TRunParamsOptionsMode;
    ADest.SaveIn := SaveIn;
  end;
end;

procedure TRunParamsOptionsMode.Clear;
begin
  inherited Clear;

  SaveIn := rpsLPS;
  LaunchingApplicationPathPlusParams := GetDefaultLaunchingApplicationPathPlusParams;
end;


{ TRunParamsOptsDlg }

constructor TRunParamsOptsDlg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  fOptions := TRunParamsOptions.Create;
  Caption := dlgRunParameters;
  ModesLabel.Caption := dlgMode;
  SaveInLabel.Caption := dlgSaveIn;
  NewModeButton.Hint := dlgAddNewMode;
  DeleteModeButton.Hint := dlgDeleteMode;
  NewModeButton.ImageIndex := IDEImages.LoadImage('laz_add');
  DeleteModeButton.ImageIndex := IDEImages.LoadImage('laz_delete');
  ToolBar1.Images := IDEImages.Images_16;
  ButtonPanel.OKButton.Caption:=lisMenuOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
  SaveInComboBox.Items[0] := lisProjectSession+' (.lps)';
  SaveInComboBox.Items[1] := lisProjectMacro+' (.lpi)';
  SetupNotebook;
end;

procedure TRunParamsOptsDlg.DeleteModeButtonClick(Sender: TObject);
begin
  if ModesComboBox.ItemIndex=-1 then
    Exit;

  if fOptions.Count=1 then
  begin
    MessageDlg(lisCannotDeleteLastMode, mtError, [mbOK], 0);
    Exit;
  end;
  if MessageDlg(lisDelete2, mtConfirmation, [mbYes, mbNo], 0)<>mrYes then
    Exit;

  fLastSelectedMode := nil;
  fOptions.Delete(ModesComboBox.ItemIndex);
  ReloadModesComboBox;
  ModesComboBox.ItemIndex := 0;
  ModesComboBoxChange(ModesComboBox);
end;

destructor TRunParamsOptsDlg.Destroy;
begin
  fOptions.Free;

  inherited Destroy;
end;

procedure TRunParamsOptsDlg.SetupNotebook;
begin
  with Notebook do
  begin
    Page[0].Caption := dlgRunOLocal;
    Page[1].Caption := dlgRunOEnvironment;
    PageIndex := 0;
  end;

  SetupLocalPage;
  SetupEnvironmentPage;
  SetupPreviewPage;
  IDEImages.AssignImage(UserOverridesAddButton, 'laz_add');
  IDEImages.AssignImage(UserOverridesEditButton, 'laz_edit');
  IDEImages.AssignImage(UserOverridesDeleteButton, 'laz_delete');
end;

procedure TRunParamsOptsDlg.NewModeButtonClick(Sender: TObject);
var
  NewName: string;
  NewMode: TRunParamsOptionsMode;
begin
  NewName := InputBox(dlgCreateNewRunParametersSettings, lisName, '');
  if NewName='' then
    Exit;
  if fOptions.Find(NewName)<>nil then
  begin
    MessageDlg(lisDuplicateModeName, mtError, [mbOK], 0);
    Exit;
  end;

  fLastSelectedMode := nil;
  NewMode := fOptions.Add(NewName) as TRunParamsOptionsMode;
  SaveToOptionsMode(NewMode);
  ReloadModesComboBox;
  SelectMode(NewName);
  fLastSelectedMode := NewMode;
end;

procedure TRunParamsOptsDlg.PreviewMultilineCheckBoxChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TRunParamsOptsDlg.SetupLocalPage;
begin
  HostApplicationGroupBox.Caption   := dlgHostApplication;
  HostApplicationBrowseBtn.Caption  := '...';
  CmdLineParametersGroupBox.Caption := dlgCommandLineParams;
  UseLaunchingApplicationGroupBox.Caption := lisUseLaunchingApplicationGroupBox;
  UseLaunchingApplicationCheckBox.Caption := dlgUseLaunchingApp;

  WorkingDirectoryGroupBox.Caption := dlgROWorkingDirectory;
  WorkingDirectoryBtn.Caption := '...';
  DisplayGroupBox.Caption := dlgRunODisplay;
  UseDisplayCheckBox.Caption := dlgRunOUsedisplay;
  DisplayEdit.Parent := DisplayGroupBox;
end;

procedure TRunParamsOptsDlg.SetupEnvironmentPage;
begin
  SystemVariablesGroupBox.Caption := dlgRunOSystemVariables;

  with SystemVariablesListView do
  begin
    Columns.BeginUpdate;
    Columns[0].Caption := lisVariable;
    Columns[1].Caption := lisValue;
    Columns.EndUpdate;
  end;

  UserOverridesGroupBox.Caption := dlgRunOUserOverrides;

  with UserOverridesListView do
  begin
    Columns.BeginUpdate;
    Columns[0].Caption := lisVariable;
    Columns[1].Caption := lisValue;
    Columns.EndUpdate;
  end;

  UserOverridesAddButton.Caption    := lisDlgAdd;
  UserOverridesEditButton.Caption   := lisDlgEdit;
  UserOverridesDeleteButton.Caption := lisDelete;
  IncludeSystemVariablesCheckBox.Caption := dlgIncludeSystemVariables;
end;

procedure TRunParamsOptsDlg.SetupPreviewPage;
begin
  PreviewPage.Caption:=dlgWRDPreview;
  PreviewMultilineCheckBox.Caption:=lisShowMultipleLines;
  PreviewMultilineCheckBox.Checked:=MiscellaneousOptions.ShowCompOptMultiLine;
end;

procedure TRunParamsOptsDlg.OkButtonClick(Sender: TObject);
begin
  if SelectedMode<>nil then
    SaveToOptionsMode(SelectedMode);
  SaveToOptions;
  ModalResult := mrOk;
end;

procedure TRunParamsOptsDlg.ReloadModesComboBox;
var
  I: Integer;
  AMode: TAbstractRunParamsOptionsMode;
begin
  ModesComboBox.Clear;
  for I := 0 to fOptions.Count-1 do
  begin
    AMode := fOptions[I];
    ModesComboBox.AddItem(AMode.Name, AMode);
  end;
end;

procedure TRunParamsOptsDlg.UpdatePreview;
var
  sl: TStringList;
  MultiLine: Boolean;

  procedure AddLines(aCaption, Params: string);
  var
    ParamList: TStringList;
  begin
    sl.Add(aCaption);
    if MultiLine then begin
      ParamList:=TStringList.Create;
      try
        SplitCmdLineParams(Params,ParamList);
        sl.AddStrings(ParamList);
      finally
        ParamList.Free;
      end;
    end else begin
      sl.Add(Params);
    end;
  end;

var
  s: string;
begin
  MultiLine:=PreviewMultilineCheckBox.Checked;
  if MultiLine then
    PreviewMemo.ScrollBars:=ssAutoBoth
  else
    PreviewMemo.ScrollBars:=ssAutoVertical;

  sl:=TStringList.Create;
  try
    s:=HostApplicationEdit.Text;
    if s<>'' then begin
      IDEMacros.SubstituteMacros(s);
      sl.Add('Host Application: '+s);
    end;

    s:=WorkingDirectoryComboBox.Text;
    if s<>'' then begin
      IDEMacros.SubstituteMacros(s);
      sl.Add('Working Directory: '+s);
    end;

    if UseLaunchingApplicationCheckBox.Checked then begin
      s:=UseLaunchingApplicationComboBox.Text;
      if s<>'' then begin
        IDEMacros.SubstituteMacros(s);
        AddLines('Launching Application:',s);
      end;
    end;

    s:=CmdLineParametersComboBox.Text;
    if s<>'' then begin
      IDEMacros.SubstituteMacros(s);
      AddLines('Parameters:',s);
    end;

    if UseDisplayCheckBox.Checked then begin
      s:=DisplayEdit.Text;
      if s<>'' then begin
        IDEMacros.SubstituteMacros(s);
        sl.Add('Display: '+s);
      end;
    end;

    PreviewMemo.Lines.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TRunParamsOptsDlg.SaveToOptions;
begin
  fSaveToOptions.Assign(fOptions);
end;

procedure TRunParamsOptsDlg.EnvVarsPageResize(Sender: TObject);
var
  NewHeight: Integer;
begin
  NewHeight:=(Notebook.Page[1].Height - 37) div 2;
  with UserOverridesGroupBox do
    SetBounds(Left,Top+Height-NewHeight,Width,NewHeight);

  SystemVariablesListView.Column[0].Width := SystemVariablesListView.Width div 2;
  SystemVariablesListView.Column[1].Width := SystemVariablesListView.Column[0].Width;

  UserOverridesListView.Column[0].Width := UserOverridesListView.Width div 2;
  UserOverridesListView.Column[1].Width := UserOverridesListView.Column[0].Width;
end;

procedure TRunParamsOptsDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  MiscellaneousOptions.ShowCompOptMultiLine:=PreviewMultilineCheckBox.Checked;
end;

procedure TRunParamsOptsDlg.FormCreate(Sender: TObject);
begin
end;

procedure TRunParamsOptsDlg.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TRunParamsOptsDlg.HostApplicationBrowseBtnClick(Sender: TObject);
var
  OpenDialog: TIDEOpenDialog;
begin
  OpenDialog := IDEOpenDialogClass.Create(Self);
  with OpenDialog do
  begin
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    if HostApplicationEdit.Text <> '' then
      OpenDialog.InitialDir := ExtractFilePath(HostApplicationEdit.Text);
    OpenDialog.Filename := HostApplicationEdit.Text;
    if OpenDialog.Execute then
    begin
      if (FileIsExecutable(OpenDialog.Filename))
      or (IDEMessageDialog(lisRunParamsFileNotExecutable,
        Format(lisRunParamsTheHostApplicationIsNotExecutable,[OpenDialog.Filename]),
        mtWarning, [mbCancel, mbIgnore]) = mrIgnore) then
      begin
        HostApplicationEdit.Text := OpenDialog.Filename;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  end;
end;

procedure TRunParamsOptsDlg.ModesComboBoxChange(Sender: TObject);
begin
  if fLastSelectedMode<>nil then
    SaveToOptionsMode(fLastSelectedMode);
  if SelectedMode<>nil then
    LoadFromOptionsMode(SelectedMode);
end;

procedure TRunParamsOptsDlg.NotebookChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TRunParamsOptsDlg.LoadFromOptionsMode(const AMode: TRunParamsOptionsMode);
var
  List: THistoryList;
  S: String;
begin
  // local
  HostApplicationEdit.Text := AMode.HostApplicationFilename;

  // WorkingDirectoryComboBox
  List:=HistoryLists.GetList(hlWorkingDirectory,true,rltFile);
  List.AppendEntry(AMode.WorkingDirectory);
  WorkingDirectoryComboBox.Items.Assign(List);
  WorkingDirectoryComboBox.Text := AMode.WorkingDirectory;

  SaveInComboBox.ItemIndex := Ord(AMode.SaveIn);
  if SaveInComboBox.ItemIndex = -1 then
    SaveInComboBox.ItemIndex := 0;

  // UseLaunchingApplicationComboBox
  UseLaunchingApplicationCheckBox.Checked := AMode.UseLaunchingApplication;
  List := HistoryLists.GetList(hlLaunchingApplication,true,rltFile);
  List.AppendEntry(AMode.LaunchingApplicationPathPlusParams);
  S := FindTerminalInPath;
  if S <> '' then
    List.AppendEntry(S);
  {$IFNDEF MSWINDOWS}
  S := FindTerminalInPath('gnome-terminal');
  if S <> '' then
    List.AppendEntry(S);
  S := FindTerminalInPath('konsole');
  if S <> '' then
    List.AppendEntry(S);
  {$ENDIF}
  UseLaunchingApplicationComboBox.Items.Assign(List);
  UseLaunchingApplicationComboBox.Text := AMode.LaunchingApplicationPathPlusParams;
  UseLaunchingApplicationComboBox.Enabled := UseLaunchingApplicationCheckBox.Checked;

  // CmdLineParametersComboBox
  List:=HistoryLists.GetList(hlCmdLineParameters,true,rltCaseSensitive);
  List.AppendEntry(AMode.CmdLineParams);
  CmdLineParametersComboBox.Items.Assign(List);
  CmdLineParametersComboBox.Text := AMode.CmdLineParams;

  UseDisplayCheckBox.Checked := AMode.UseDisplay;
  DisplayEdit.Text := AMode.Display;

  // environment
  FillSystemVariablesListView;
  FillUserOverridesListView(AMode);

  IncludeSystemVariablesCheckBox.Checked := AMode.IncludeSystemVariables;

  fOptions.ActiveModeName := AMode.Name;
  fLastSelectedMode := AMode;

  UpdatePreview;
end;

procedure TRunParamsOptsDlg.UseLaunchingApplicationCheckBoxChange(Sender: TObject);
begin
  UseLaunchingApplicationComboBox.Enabled := (Sender as TCheckBox).Checked;
end;

procedure TRunParamsOptsDlg.UserOverridesListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  en: Boolean;
begin
  en := Assigned(UserOverridesListView.Selected);
  UserOverridesDeleteButton.Enabled := en;
  UserOverridesEditButton.Enabled := en;
end;

procedure TRunParamsOptsDlg.WorkingDirectoryBtnClick(Sender: TObject);
var
  NewDirectory: String;
begin
  NewDirectory:=InputHistories.SelectDirectory('Working directory',true,
                                 ExtractFilePath(WorkingDirectoryComboBox.Text),
                                 ExtractFilename(WorkingDirectoryComboBox.Text));
  if NewDirectory<>'' then
    WorkingDirectoryComboBox.Text:=NewDirectory;
end;

procedure TRunParamsOptsDlg.UserOverridesAddButtonClick(Sender: TObject);
var
  Variable, Value: string;
  NewLI, SelLI:    TListItem;
begin
  SelLI := SystemVariablesListView.Selected;
  if SelLI <> nil then
  begin
    Variable := SelLI.Caption;
    Value    := SelLI.SubItems[0];
  end
  else
  begin
    Variable := '';
    Value    := '';
  end;
  if ShowSysVarUserOverrideDialog(Variable, Value) = mrOk then
  begin
    NewLI := UserOverridesListView.Items.Add;
    NewLI.Caption := Variable;
    NewLI.SubItems.Add(Value);
    UserOverridesListView.Selected := NewLI;
  end;
end;

procedure TRunParamsOptsDlg.UserOverridesEditButtonClick(Sender: TObject);
var
  Variable, Value: string;
  SelLI: TListItem;
begin
  SelLI := UserOverridesListView.Selected;
  if SelLI = nil then
    exit;
  Variable := SelLI.Caption;
  Value    := SelLI.SubItems[0];
  if ShowSysVarUserOverrideDialog(Variable, Value) = mrOk then
  begin
    SelLI.Caption     := Variable;
    SelLI.SubItems[0] := Value;
  end;
end;

procedure TRunParamsOptsDlg.UserOverridesDeleteButtonClick(Sender: TObject);
var
  SelLI:    TListItem;
  OldIndex: integer;
begin
  SelLI := UserOverridesListView.Selected;
  if SelLI <> nil then
  begin
    OldIndex := SelLI.Index;
    SelLI.Delete;
    if OldIndex = UserOverridesListView.Items.Count then
      Dec(OldIndex);
    if OldIndex >= 0 then
      UserOverridesListView.Selected := UserOverridesListView.Items[OldIndex];
  end;
end;

procedure TRunParamsOptsDlg.SaveToOptionsMode(const AMode: TRunParamsOptionsMode);

  procedure SaveComboHistory(AComboBox: TComboBox; const History: string;
    ListType: TRecentListType);
  begin
    AComboBox.AddHistoryItem(AComboBox.Text,20,true,false);
    HistoryLists.GetList(History,true,ListType).Assign(AComboBox.Items);
  end;

begin
  if (SaveInComboBox.ItemIndex>=Ord(Low(TRunParamsOptionsModeSave)))
  and(SaveInComboBox.ItemIndex<=Ord(High(TRunParamsOptionsModeSave))) then
    AMode.SaveIn := TRunParamsOptionsModeSave(SaveInComboBox.ItemIndex)
  else
    AMode.SaveIn := Low(TRunParamsOptionsModeSave);

  // local
  AMode.HostApplicationFilename := Trim(HostApplicationEdit.Text);
  AMode.CmdLineParams := Trim(CmdLineParametersComboBox.Text);
  AMode.UseLaunchingApplication := UseLaunchingApplicationCheckBox.Checked;
  AMode.LaunchingApplicationPathPlusParams :=
                                     Trim(UseLaunchingApplicationComboBox.Text);
  AMode.WorkingDirectory := Trim(WorkingDirectoryComboBox.Text);
  AMode.UseDisplay := UseDisplayCheckBox.Checked;
  AMode.Display    := Trim(DisplayEdit.Text);
  
  // history list: WorkingDirectoryComboBox
  SaveComboHistory(WorkingDirectoryComboBox,hlWorkingDirectory,rltFile);

  // history list: UseLaunchingApplicationComboBox
  SaveComboHistory(UseLaunchingApplicationComboBox,hlLaunchingApplication,rltFile);

  // history list: CmdLineParametersComboBox
  SaveComboHistory(CmdLineParametersComboBox,hlCmdLineParameters,rltCaseSensitive);

  // environment
  SaveUserOverrides(AMode);

  AMode.IncludeSystemVariables := IncludeSystemVariablesCheckBox.Checked;
end;

procedure TRunParamsOptsDlg.SaveUserOverrides(const AMode: TRunParamsOptionsMode
  );
var
  i: integer;
begin
  AMode.UserOverrides.Clear;
  for i := 0 to UserOverridesListView.Items.Count - 1 do
  begin
    AMode.UserOverrides.Values[UserOverridesListView.Items[i].Caption] :=
      UserOverridesListView.Items[i].SubItems[0];
  end;
end;

function TRunParamsOptsDlg.SelectedMode: TRunParamsOptionsMode;
begin
  if ModesComboBox.ItemIndex>=0 then
    Result := ModesComboBox.Items.Objects[ModesComboBox.ItemIndex] as TRunParamsOptionsMode
  else
    Result := nil;
end;

procedure TRunParamsOptsDlg.SelectMode(const AName: string);
var
  I: Integer;
begin
  for I := 0 to ModesComboBox.Items.Count-1 do
    if ModesComboBox.Items[I] = AName then
    begin
      ModesComboBox.ItemIndex := I;
      Exit;
    end;
end;

procedure TRunParamsOptsDlg.SetComboBoxText(AComboBox: TComboBox; AText: ansistring);
var
  a: integer;
begin
  a := AComboBox.Items.IndexOf(AText);
  if a >= 0 then
    AComboBox.ItemIndex := a
  else
  begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex := AComboBox.Items.IndexOf(AText);
  end;
end;

procedure TRunParamsOptsDlg.SetOptions(NewOptions: TRunParamsOptions);
begin
  fOptions.Assign(NewOptions);
  fSaveToOptions := NewOptions;

  if fOptions.Count=0 then
    fOptions.Add('default');

  ReloadModesComboBox;
  SelectMode(NewOptions.ActiveModeName);
  if (ModesComboBox.ItemIndex=-1) then
    ModesComboBox.ItemIndex := 0;

  ModesComboBoxChange(ModesComboBox);
end;

procedure TRunParamsOptsDlg.FillListView(ListView: TListView; sl: TStringList);
var
  i: integer;
  Variable, Value: string;
begin
  with ListView.Items do
  begin
    //BeginUpdate;
    for i := 0 to sl.Count - 1 do
    begin
      Variable := sl.Names[i];
      Value    := sl.Values[Variable];
      if Count <= i then
      begin
        // add line to listview
        Add;
        Item[i].SubItems.Add('');
      end;
      Item[i].Caption     := Variable;
      Item[i].SubItems[0] := Value;
    end;
    while Count > sl.Count do
      Delete(Count - 1);
    //EndUpdate;
  end;
end;

procedure TRunParamsOptsDlg.FillSystemVariablesListView;
var
  EnvList: TStringList;
begin
  EnvList := EnvironmentAsStringList;
  FillListView(SystemVariablesListView, EnvList);
  EnvList.Free;
end;

procedure TRunParamsOptsDlg.FillUserOverridesListView(
  const AMode: TRunParamsOptionsMode);
begin
  FillListView(UserOverridesListView, AMode.UserOverrides);
  UserOverridesListView.OnSelectItem(nil, nil, false); //update buttons
end;

end.
