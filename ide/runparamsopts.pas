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
  Classes, SysUtils, LCLProc, Controls, Forms, Buttons, StdCtrls, ComCtrls,
  Dialogs, ExtCtrls, BaseIDEIntf, IDEHelpIntf, ProjectIntf, IDEDialogs,
  IDEProcs, SysVarUserOverrideDlg, InputHistory, LazarusIDEStrConsts, FileUtil,
  Laz2_XMLCfg, ButtonPanel, AdvHistoryList;

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

  { TRunParamsOptions }

  TRunParamsOptions = class(TAbstractRunParamsOptions)
  private
    FCmdLineParamsHistoryList,
    FLaunchingApplicationHistoryList : TAdvHistoryList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function Load(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;
    procedure AssignEnvironmentTo(Strings: TStrings); override;
    property CmdLineParamsHistoryList : TAdvHistoryList read FCmdLineParamsHistoryList;
    property LaunchingApplicationHistoryList : TAdvHistoryList read FLaunchingApplicationHistoryList;
  end;

  {
    TRunParamsOptsDlg is the form of the run parameters options dialog
  }

  { TRunParamsOptsDlg }

  TRunParamsOptsDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    CmdLineParametersComboBox: TComboBox;
    CmdLineParametersMemo: TMemo;
    UseLaunchingApplicationMemo: TMemo;
    UseDisplayCheckBox: TCheckBox;
    DisplayEdit: TEdit;
    DisplayGroupBox: TGroupBox;
    HostApplicationBrowseBtn: TButton;
    UserOverridesAddButton: TBitBtn;
    UserOverridesDeleteButton: TBitBtn;
    UserOverridesEditButton: TBitBtn;
    WorkingDirectoryBtn: TButton;
    WorkingDirectoryComboBox: TComboBox;
    WorkingDirectoryGroupBox: TGroupBox;
    UseLaunchingApplicationCheckBox: TCheckBox;
    IncludeSystemVariablesCheckBox: TCheckBox;
    UseLaunchingApplicationComboBox: TComboBox;
    HostApplicationEdit: TEdit;
    UseLaunchingApplicationGroupBox: TGroupBox;
    CmdLineParametersGroupBox: TGroupBox;
    HostApplicationGroupBox: TGroupBox;
    UserOverridesGroupBox: TGroupBox;
    SystemVariablesGroupBox: TGroupBox;
    SystemVariablesListView: TListView;
    UserOverridesListView: TListView;
    Notebook: TPageControl;
    GeneralPage: TTabSheet;
    EnvVarsPage: TTabSheet;
    procedure CmdLineParametersComboBoxSelect(Sender: TObject);
    procedure CmdLineParametersMemoChange(Sender: TObject);
    procedure EnvVarsPageResize(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure HostApplicationBrowseBtnClick(Sender: TObject);
    procedure UseLaunchingApplicationComboBoxSelect(Sender: TObject);
    procedure UseLaunchingApplicationMemoChange(Sender: TObject);
    procedure WorkingDirectoryBtnClick(Sender: TObject);
    procedure UserOverridesAddButtonClick(Sender: TObject);
    procedure UserOverridesEditButtonClick(Sender: TObject);
    procedure UserOverridesDeleteButtonClick(Sender: TObject);
  private
    fOptions: TRunParamsOptions;
    procedure SetupNotebook;
    procedure SetupLocalPage;
    procedure SetupEnvironmentPage;
    procedure SetOptions(NewOptions: TRunParamsOptions);
    procedure FillListView(ListView: TListView; sl: TStringList);
    procedure FillSystemVariablesListView;
    procedure FillUserOverridesListView;
    procedure SaveToOptions;
    procedure SaveUserOverrides;
    procedure SetComboBoxText(AComboBox: TComboBox; AText: string);
  public
    constructor Create(AnOwner: TComponent); override;
    property Options: TRunParamsOptions Read fOptions Write SetOptions;
  end;


function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions): TModalResult;

implementation

{$R *.lfm}

const
  DefaultLauncherTitle = '''Lazarus Run Output''';
  DefaultLauncherApplication = '$(LazarusDir)/tools/runwait.sh $(TargetCmdLine)';

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
    Term := GetEnvironmentVariable('TERM');
  if Term = '' then
    Term := 'xterm';
  List.DelimitedText := GetEnvironmentVariable('PATH');
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

function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions): TModalResult;
var
  RunParamsOptsForm: TRunParamsOptsDlg;
begin
  Result := mrCancel;
  RunParamsOptsForm := TRunParamsOptsDlg.Create(nil);
  try
    RunParamsOptsForm.Options := RunParamsOptions;
    Result := RunParamsOptsForm.ShowModal;
  finally
    RunParamsOptsForm.Free;
  end;
end;

{ TRunParamsOptions }

constructor TRunParamsOptions.Create;
begin
  inherited Create;

  fUserOverrides := TStringList.Create;

  FCmdLineParamsHistoryList := TAdvHistoryList(TAdvHistoryList.CreateMe(rltCaseSensitive, hlCmdLineParamsHistoryList));
  FLaunchingApplicationHistoryList := TAdvHistoryList(TAdvHistoryList.CreateMe(rltCaseSensitive, hlLaunchingApplicationHistoryList));

  Clear;
end;

destructor TRunParamsOptions.Destroy;
begin
  fUserOverrides.Free;
  FreeAndNil(FCmdLineParamsHistoryList);
  FreeAndNil(FLaunchingApplicationHistoryList);
  inherited Destroy;
end;


procedure TRunParamsOptions.Clear;
begin
  // local options
  fHostApplicationFilename := '';
  fCmdLineParams := '';
  fUseLaunchingApplication := False;
  fLaunchingApplicationPathPlusParams := GetDefaultLaunchingApplicationPathPlusParams;
  // TODO: guess are we under gnome or kde so query for gnome-terminal or konsole.
  fWorkingDirectory := '';
  fUseDisplay := False;
  fDisplay    := ':0';

  // environment options
  fUserOverrides.Clear;
  fIncludeSystemVariables := False;

  FCmdLineParamsHistoryList.Clear;
  FLaunchingApplicationHistoryList.Clear;
end;

function TRunParamsOptions.Load(XMLConfig: TXMLConfig; const Path: string;
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
      fUserOverrides.Values[XMLConfig.GetValue(
        APath + 'Variable' + IntToStr(i) + '/Name', '')] :=
        XMLConfig.GetValue(APath + 'Variable' + IntToStr(i) + '/Value', '');
    end;
  end;

Var
  ARunParamsOptionsVersion : Integer;
begin
  // get format version to distinguish old formats
  ARunParamsOptionsVersion := XMLConfig.GetValue(Path + 'RunParams/local/FormatVersion/Value',
    RunParamsOptionsVersion);

  // local options
  fHostApplicationFilename := f(XMLConfig.GetValue(
    Path + 'RunParams/local/HostApplicationFilename/Value',
    fHostApplicationFilename));

  fUseLaunchingApplication := XMLConfig.GetValue(
    Path + 'RunParams/local/LaunchingApplication/Use', fUseLaunchingApplication);

  Case ARunParamsOptionsVersion Of
    1 : Begin
          fCmdLineParams := f(XMLConfig.GetValue(
          Path + 'RunParams/local/CommandLineParams/Value', fCmdLineParams));

          CmdLineParamsHistoryList.Values['Old value'] := fCmdLineParams;
          CmdLineParamsHistoryList.Selected := 'Old value';

          fLaunchingApplicationPathPlusParams :=
            f(XMLConfig.GetValue(Path + 'RunParams/local/LaunchingApplication/PathPlusParams',
                                 f(GetDefaultLaunchingApplicationPathPlusParams)));

          LaunchingApplicationHistoryList.Values['Old value'] := fLaunchingApplicationPathPlusParams;
          LaunchingApplicationHistoryList.Selected := 'Old value';
        End;
    2 : begin
          FCmdLineParamsHistoryList.AdvLoadFromXMLConfig(XMLConfig, Path + 'RunParams/local/CommandLineParamsList');
          If FCmdLineParamsHistoryList.Selected <> '' Then
            fCmdLineParams := f(FCmdLineParamsHistoryList.Values[FCmdLineParamsHistoryList.Selected]);
          FLaunchingApplicationHistoryList.AdvLoadFromXMLConfig(XMLConfig, Path + 'RunParams/local/LaunchingApplication/PathPlusParams/List');
          if FLaunchingApplicationHistoryList.Selected <> '' Then
            fLaunchingApplicationPathPlusParams := f(FLaunchingApplicationHistoryList.Values[FLaunchingApplicationHistoryList.Selected])
          else
            fLaunchingApplicationPathPlusParams := f(GetDefaultLaunchingApplicationPathPlusParams);
        end;
  end;


  fWorkingDirectory := f(XMLConfig.GetValue(
    Path + 'RunParams/local/WorkingDirectory/Value', fWorkingDirectory));

  fUseDisplay := XMLConfig.GetValue(Path + 'RunParams/local/Display/Use',
    fUseDisplay);
  fDisplay    := XMLConfig.GetValue(Path + 'RunParams/local/Display/Value', fDisplay);

  // environment options
  LoadUserOverrides(Path + 'RunParams/environment/UserOverrides/');
  fIncludeSystemVariables := XMLConfig.GetValue(
    Path + 'RunParams/environment/IncludeSystemVariables/Value',
    fIncludeSystemVariables);

  Result := mrOk;
end;

function TRunParamsOptions.Save(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch): TModalResult;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure SaveUserOverrides(const APath: string);
  var
    i: integer;
  begin
    XMLConfig.SetDeleteValue(APath + 'Count', fUserOverrides.Count, 0);
    for i := 0 to fUserOverrides.Count - 1 do
    begin
      XMLConfig.SetValue(APath + 'Variable' + IntToStr(i) + '/Name',
        fUserOverrides.Names[i]);
      XMLConfig.SetValue(APath + 'Variable' + IntToStr(i) + '/Value',
        fUserOverrides.Values[fUserOverrides.Names[i]]);
    end;
  end;

Var
  APrevValue : String;
begin
  // save a format version to distinguish old formats
  XMLConfig.SetValue(Path + 'RunParams/local/FormatVersion/Value',
    RunParamsOptionsVersion);

  // local options
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/HostApplicationFilename/Value',
    f(fHostApplicationFilename), '');

  APrevValue := fCmdLineParams;
  fCmdLineParams := '';

  XMLConfig.SetDeleteValue(Path + 'RunParams/local/CommandLineParams/Value',
    f(fCmdLineParams), '');

  fCmdLineParams := APrevValue;

  XMLConfig.SetDeleteValue(Path + 'RunParams/local/LaunchingApplication/Use',
    fUseLaunchingApplication, False);

  APrevValue := fLaunchingApplicationPathPlusParams;
  fLaunchingApplicationPathPlusParams := '';

  XMLConfig.SetDeleteValue(Path + 'RunParams/local/LaunchingApplication/PathPlusParams',
    f(fLaunchingApplicationPathPlusParams), '');

  fLaunchingApplicationPathPlusParams := APrevValue;

  XMLConfig.SetDeleteValue(Path + 'RunParams/local/WorkingDirectory/Value',
    f(fWorkingDirectory), '');
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/Display/Use',
    fUseDisplay, False);
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/Display/Value',
    fDisplay, ':0');

  // environment options
  SaveUserOverrides(Path + 'RunParams/environment/UserOverrides/');
  XMLConfig.SetDeleteValue(Path + 'RunParams/environment/IncludeSystemVariables/Value',
    fIncludeSystemVariables, False);

  CmdLineParamsHistoryList.AdvSaveToXMLConfig(XMLConfig, Path + 'RunParams/local/CommandLineParamsList');
  LaunchingApplicationHistoryList.AdvSaveToXMLConfig(XMLConfig, Path + 'RunParams/local/LaunchingApplication/PathPlusParams/List');

  Result := mrOk;
end;

procedure TRunParamsOptions.AssignEnvironmentTo(Strings: TStrings);
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


{ TRunParamsOptsDlg }

constructor TRunParamsOptsDlg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  Caption := dlgRunParameters;
  ButtonPanel.OKButton.Caption:=lisMenuOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
  SetupNotebook;
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
  UserOverridesAddButton.LoadGlyphFromLazarusResource('laz_add');
  UserOverridesEditButton.LoadGlyphFromLazarusResource('laz_edit');
  UserOverridesDeleteButton.LoadGlyphFromLazarusResource('laz_delete');
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

  CmdLineParametersComboBox.Text := '';
  UseLaunchingApplicationComboBox.Text := '';
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

  UserOverridesAddButton.Caption    := dlgEdAdd;
  UserOverridesEditButton.Caption   := dlgEdEdit;
  UserOverridesDeleteButton.Caption := lisDelete;
  IncludeSystemVariablesCheckBox.Caption := dlgIncludeSystemVariables;
end;

procedure TRunParamsOptsDlg.OkButtonClick(Sender: TObject);
begin
  SaveToOptions;
  ModalResult := mrOk;
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

procedure TRunParamsOptsDlg.CmdLineParametersMemoChange(Sender: TObject);
Var
  AValue : String;
begin
  AValue := CmdLineParametersComboBox.Text;
  If AValue = '' Then Begin
    SetComboBoxText(CmdLineParametersComboBox, 'Default');
    AValue := CmdLineParametersComboBox.Text;
  end;
  If AValue <> '' Then Begin
    fOptions.CmdLineParamsHistoryList.Values[AValue] := CmdLineParametersMemo.Lines.Text;
  end;
end;

procedure TRunParamsOptsDlg.CmdLineParametersComboBoxSelect(Sender: TObject);
Var
  AValue : String;
begin
  AValue := CmdLineParametersComboBox.Text;
  If AValue <> '' Then
    CmdLineParametersMemo.Lines.Text := Foptions.CmdLineParamsHistoryList.Values[AValue];
end;

procedure TRunParamsOptsDlg.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TRunParamsOptsDlg.HostApplicationBrowseBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  with OpenDialog do
  begin
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    if HostApplicationEdit.Text <> '' then
      OpenDialog.InitialDir := ExtractFilePath(HostApplicationEdit.Text);
    OpenDialog.Filename := HostApplicationEdit.Text;
    if OpenDialog.Execute then
    begin
      if (FileIsExecutable(OpenDialog.Filename)) or
        (IDEMessageDialog(lisRunParamsFileNotExecutable,
        Format(lisRunParamsTheHostApplicationIsNotExecutable,
        ['"', OpenDialog.Filename, '"']), mtWarning, [mbCancel, mbIgnore]) =
        mrIgnore) then
      begin
        HostApplicationEdit.Text := OpenDialog.Filename;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  end;
end;

procedure TRunParamsOptsDlg.UseLaunchingApplicationComboBoxSelect(
  Sender: TObject);
Var
  AValue : String;
begin
  AValue := UseLaunchingApplicationComboBox.Text;
  If AValue <> '' Then
    UseLaunchingApplicationMemo.Text := Foptions.LaunchingApplicationHistoryList.Values[AValue];
end;

procedure TRunParamsOptsDlg.UseLaunchingApplicationMemoChange(Sender: TObject);
Var
  AValue : String;
begin
  AValue := UseLaunchingApplicationComboBox.Text;
  If AValue = '' Then Begin
    SetComboBoxText(UseLaunchingApplicationComboBox, 'Default');
    AValue := UseLaunchingApplicationComboBox.Text;
  end;
  If AValue <> '' Then
    fOptions.LaunchingApplicationHistoryList.Values[AValue] := UseLaunchingApplicationMemo.Lines.Text;
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

procedure TRunParamsOptsDlg.SaveToOptions;

  procedure SaveComboHistory(AComboBox: TComboBox; const History: string;
    ListType: TRecentListType);
  begin
    AComboBox.AddHistoryItem(AComboBox.Text,20,true,false);
    InputHistories.HistoryLists.GetList(History,true,ListType).Assign(AComboBox.Items);
  end;

  procedure SaveHistoryList(AHistoryList : THistoryList; History : String; ListType : TRecentListType);
  begin
    InputHistories.HistoryLists.GetList(History,true,ListType).Assign(AHistoryList);
  end;

  procedure SaveAdvHistoryList(AComboBox : TComboBox; AHistoryList : TAdvHistoryList; AMemo : TMemo);
  Var
    AValue : String;
  begin
    AValue := AComboBox.Text;
    If (AValue = '') And (AMemo.Lines.Text <> '') Then
      AValue := 'Default';
    If AValue <> '' Then Begin
      AHistoryList.Values[AValue] := AMemo.Lines.Text;
      AHistoryList.Selected := AValue;
    end;
  end;

begin
  // local
  fOptions.HostApplicationFilename := Trim(HostApplicationEdit.Text);

  fOptions.CmdLineParams := Trim(CmdLineParametersMemo.Lines.Text);

  fOptions.UseLaunchingApplication := UseLaunchingApplicationCheckBox.Checked;
  fOptions.LaunchingApplicationPathPlusParams :=
                                     Trim(UseLaunchingApplicationMemo.Lines.Text);

  fOptions.WorkingDirectory := Trim(WorkingDirectoryComboBox.Text);

  fOptions.UseDisplay := UseDisplayCheckBox.Checked;
  fOptions.Display    := Trim(DisplayEdit.Text);
  
  // history list: WorkingDirectoryComboBox
  SaveComboHistory(WorkingDirectoryComboBox,hlWorkingDirectory,rltFile);

  // history list: UseLaunchingApplicationComboBox
  //SaveComboHistory(UseLaunchingApplicationComboBox,hlLaunchingApplication,rltFile);

  SaveAdvHistoryList(CmdLineParametersComboBox, fOptions.CmdLineParamsHistoryList, CmdLineParametersMemo);
  SaveAdvHistoryList(UseLaunchingApplicationComboBox, fOptions.LaunchingApplicationHistoryList, UseLaunchingApplicationMemo);

  SaveHistoryList(fOptions.CmdLineParamsHistoryList, hlCmdLineParamsHistoryList, rltCaseSensitive);
  SaveHistoryList(fOptions.LaunchingApplicationHistoryList, hlLaunchingApplicationHistoryList, rltCaseSensitive);

  // history list: CmdLineParametersComboBox
  //SaveComboHistory(CmdLineParametersComboBox,hlCmdLineParameters,rltCaseSensitive);

  // environment
  SaveUserOverrides;

  fOptions.IncludeSystemVariables := IncludeSystemVariablesCheckBox.Checked;
end;

procedure TRunParamsOptsDlg.SaveUserOverrides;
var
  i: integer;
begin
  Options.UserOverrides.Clear;
  for i := 0 to UserOverridesListView.Items.Count - 1 do
  begin
    Options.UserOverrides.Values[UserOverridesListView.Items[i].Caption] :=
      UserOverridesListView.Items[i].SubItems[0];
  end;
end;

procedure TRunParamsOptsDlg.SetComboBoxText(AComboBox: TComboBox; AText: string);
var
  a: integer;
begin
  a := AComboBox.Items.IndexOf(AText);
  if a >= 0 then
    AComboBox.ItemIndex := a
  else
  begin
    If AText <> '' Then Begin
      AComboBox.Items.Add(AText);
      AComboBox.ItemIndex := AComboBox.Items.IndexOf(AText);
    end;
  end;
end;

procedure TRunParamsOptsDlg.SetOptions(NewOptions: TRunParamsOptions);
var
  List: THistoryList;
  S: String;
  I : Integer;
  AValue : String;
  AAdvHistoryList : TAdvHistoryList;
begin
  fOptions := NewOptions;

  // local
  HostApplicationEdit.Text   := fOptions.HostApplicationFilename;

  // WorkingDirectoryComboBox
  List:=InputHistories.HistoryLists.GetList(hlWorkingDirectory,true,rltFile);
  List.AppendEntry(fOptions.WorkingDirectory);
  WorkingDirectoryComboBox.Items.Assign(List);
  WorkingDirectoryComboBox.Text := fOptions.WorkingDirectory;

  // UseLaunchingApplicationComboBox
  UseLaunchingApplicationCheckBox.Checked := fOptions.UseLaunchingApplication;

  List := InputHistories.HistoryLists.GetList(hlLaunchingApplication,true,rltFile);

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
  For I := 0 To List.Count - 1 Do Begin
    AValue := Format('Old value #%d', [I + 1]);
    fOptions.LaunchingApplicationHistoryList.Values[AValue] := List[I];
  end;

  AAdvHistoryList := TAdvHistoryList(InputHistories.HistoryLists.GetList(hlLaunchingApplicationHistoryList,true,rltCaseSensitive));

  If AAdvHistoryList.Count > 0 Then
    fOptions.LaunchingApplicationHistoryList.AssignKeyValue(AAdvHistoryList);

  fOptions.LaunchingApplicationHistoryList.SetComboBox(UseLaunchingApplicationComboBox);
  SetComboBoxText(UseLaunchingApplicationComboBox, fOptions.LaunchingApplicationHistoryList.Selected);

  UseLaunchingApplicationMemo.Lines.Text:= fOptions.LaunchingApplicationHistoryList.Values[fOptions.FLaunchingApplicationHistoryList.Selected];

  // CmdLineParametersComboBox
  List:=InputHistories.HistoryLists.GetList(hlCmdLineParameters,true,rltCaseSensitive);

  For I := 0 To List.Count - 1 Do Begin
    AValue := Format('Old value #%d', [I + 1]);
    fOptions.CmdLineParamsHistoryList.Values[AValue] := List[I];
  end;

  AAdvHistoryList := TAdvHistoryList(InputHistories.HistoryLists.GetList(hlCmdLineParamsHistoryList,true,rltCaseSensitive));

  If AAdvHistoryList.Count > 0 Then
    fOptions.CmdLineParamsHistoryList.AssignKeyValue(AAdvHistoryList);

  fOptions.CmdLineParamsHistoryList.SetComboBox(CmdLineParametersComboBox);
  AValue := fOptions.CmdLineParamsHistoryList.Selected;
  SetComboBoxText(CmdLineParametersComboBox, AValue);

  CmdLineParametersMemo.Lines.Text := fOptions.CmdLineParamsHistoryList.Values[fOptions.CmdLineParamsHistoryList.Selected];

  UseDisplayCheckBox.Checked := fOptions.UseDisplay;
  DisplayEdit.Text := fOptions.Display;

  // environment
  FillSystemVariablesListView;
  FillUserOverridesListView;

  IncludeSystemVariablesCheckBox.Checked := fOptions.IncludeSystemVariables;
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

procedure TRunParamsOptsDlg.FillUserOverridesListView;
begin
  FillListView(UserOverridesListView, Options.UserOverrides);
end;

end.
