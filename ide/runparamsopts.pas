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
  Classes, SysUtils, Controls, Forms, Buttons, StdCtrls, ComCtrls, Dialogs,
  ExtCtrls, LResources, Laz_XMLCfg,
  BaseIDEIntf,
  IDEProcs, SysVarUserOverrideDlg, InputHistory, LazarusIDEStrConsts, FileUtil;

{ The xml format version:
    When the format changes (new values, changed formats) we can distinguish old
    files and are able to convert them.
}
const
  RunParamsOptionsVersion = '1';

type
  {
    the storage object for run parameters
  }
  TRunParamsOptions = class
  private
    // local options
    fHostApplicationFilename: string;
    fCmdLineParams: string;
    fUseDisplay: boolean;
    fUseLaunchingApplication: boolean;
    fLaunchingApplicationPathPlusParams: string;
    fWorkingDirectory: string;
    fDisplay: string;

    // environment options
    fUserOverrides: TStringList;
    fIncludeSystemVariables: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Load(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string): TModalResult;
    procedure AssignEnvironmentTo(Strings: TStrings);

    // local options
    property HostApplicationFilename: string
      Read fHostApplicationFilename Write fHostApplicationFilename;
    property CmdLineParams: string Read fCmdLineParams Write fCmdLineParams;
    property UseLaunchingApplication: boolean
      Read fUseLaunchingApplication Write fUseLaunchingApplication;
    property LaunchingApplicationPathPlusParams: string
      Read fLaunchingApplicationPathPlusParams Write fLaunchingApplicationPathPlusParams;
    property WorkingDirectory: string Read fWorkingDirectory Write fWorkingDirectory;
    property UseDisplay: boolean Read fUseDisplay Write FUseDisplay;
    property Display: string Read fDisplay Write fDisplay;

    // environment options
    property UserOverrides: TStringList Read fUserOverrides;
    property IncludeSystemVariables: boolean
      Read fIncludeSystemVariables Write fIncludeSystemVariables;
  end;

  {
    TRunParamsOptsDlg is the form of the run parameters options dialog
  }

  { TRunParamsOptsDlg }

  TRunParamsOptsDlg = class(TForm)
    UseDisplayCheckBox: TCheckBox;
    DisplayEdit: TEdit;
    DisplayGroupBox: TGroupBox;
    HostApplicationBrowseBtn: TButton;
    UserOverridesAddButton: TButton;
    UserOverridesEditButton: TButton;
    UserOverridesDeleteButton: TButton;
    WorkingDirectoryBtn: TButton;
    WorkingDirectoryEdit: TEdit;
    WorkingDirectoryGroupBox: TGroupBox;
    UseLaunchingApplicationCheckBox: TCheckBox;
    IncludeSystemVariablesCheckBox: TCheckBox;
    UseLaunchingApplicationComboBox: TComboBox;
    CmdLineParametersEdit: TEdit;
    HostApplicationEdit: TEdit;
    UseLaunchingApplicationGroupBox: TGroupBox;
    CmdLineParametersGroupBox: TGroupBox;
    HostApplicationGroupBox: TGroupBox;
    UserOverridesGroupBox: TGroupBox;
    SystemVariablesGroupBox: TGroupBox;
    SystemVariablesListView: TListView;
    UserOverridesListView: TListView;
    Notebook: TNotebook;
    Page1:    TPage;
    Page2:    TPage;
    OkButton: TButton;
    CancelButton: TButton;
    procedure FormResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure HostApplicationBrowseBtnClick(Sender: TObject);
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
    procedure SetComboBoxText(AComboBox: TComboBox; AText: ansistring);
  public
    constructor Create(AnOwner: TComponent); override;
    property Options: TRunParamsOptions Read fOptions Write SetOptions;
  end;


function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions): TModalResult;


implementation


const
  DefaultLauncherApplication =
    '/usr/X11R6/bin/xterm -T ''Lazarus Run Output''' +
    ' -e $(LazarusDir)/tools/runwait.sh $(TargetCmdLine)';

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
  Clear;
end;

destructor TRunParamsOptions.Destroy;
begin
  fUserOverrides.Free;
  inherited Destroy;
end;

procedure TRunParamsOptions.Clear;
begin
  // local options
  fHostApplicationFilename := '';
  fCmdLineParams := '';
  fUseLaunchingApplication := False;
  fLaunchingApplicationPathPlusParams := DefaultLauncherApplication;
  fWorkingDirectory := '';
  fUseDisplay := False;
  fDisplay    := ':0';

  // environment options
  fUserOverrides.Clear;
  fIncludeSystemVariables := False;
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
    fUserOverrides.Clear;
    Cnt := XMLConfig.GetValue(APath + 'Count', 0);
    for i := 0 to Cnt - 1 do
    begin
      fUserOverrides.Values[XMLConfig.GetValue(
        APath + 'Variable' + IntToStr(i) + '/Name', '')] :=
        XMLConfig.GetValue(APath + 'Variable' + IntToStr(i) + '/Value', '');
    end;
  end;

begin
  // local options
  fHostApplicationFilename := f(XMLConfig.GetValue(
    Path + 'RunParams/local/HostApplicationFilename/Value',
    fHostApplicationFilename));
  fCmdLineParams := f(XMLConfig.GetValue(
    Path + 'RunParams/local/CommandLineParams/Value', fCmdLineParams));
  fUseLaunchingApplication := XMLConfig.GetValue(
    Path + 'RunParams/local/LaunchingApplication/Use', fUseLaunchingApplication);
  fLaunchingApplicationPathPlusParams :=
    f(XMLConfig.GetValue(Path + 'RunParams/local/LaunchingApplication/PathPlusParams',
    fLaunchingApplicationPathPlusParams));
  if (fLaunchingApplicationPathPlusParams = '') then
    fLaunchingApplicationPathPlusParams := DefaultLauncherApplication;
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

function TRunParamsOptions.Save(XMLConfig: TXMLConfig;
  const Path: string): TModalResult;

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

begin
  // save a format version to distinguish old formats
  XMLConfig.SetValue(Path + 'RunParams/local/FormatVersion/Value',
    RunParamsOptionsVersion);

  // local options
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/HostApplicationFilename/Value',
    fHostApplicationFilename, '');
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/CommandLineParams/Value',
    fCmdLineParams, '');
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/LaunchingApplication/Use',
    fUseLaunchingApplication, False);
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/LaunchingApplication/PathPlusParams',
    fLaunchingApplicationPathPlusParams, '');
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/WorkingDirectory/Value',
    fWorkingDirectory, '');
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/Display/Use',
    fUseDisplay, False);
  XMLConfig.SetDeleteValue(Path + 'RunParams/local/Display/Value',
    fDisplay, ':0');

  // environment options
  SaveUserOverrides(Path + 'RunParams/environment/UserOverrides/');
  XMLConfig.SetDeleteValue(Path + 'RunParams/environment/IncludeSystemVariables/Value',
    fIncludeSystemVariables, False);

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
  SetupNotebook;
  OkButton.Caption := lisOkBtn;
  CancelButton.Caption := dlgCancel;
  CancelControl := CancelButton;
  FormResize(nil);
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
end;

procedure TRunParamsOptsDlg.SetupLocalPage;
begin
  HostApplicationGroupBox.Caption   := dlgHostApplication;
  HostApplicationBrowseBtn.Caption  := '...';
  CmdLineParametersGroupBox.Caption := dlgCommandLineParams;
  CmdLineParametersEdit.Clear;
  UseLaunchingApplicationGroupBox.Caption := lisUseLaunchingApplicationGroupBox;
  UseLaunchingApplicationCheckBox.Caption := dlgUseLaunchingApp;

  with UseLaunchingApplicationComboBox.Items do
  begin
    BeginUpdate;
    Add(DefaultLauncherApplication);
      {$IFNdef MSWindows}
    Add('/usr/bin/gnome-terminal -t ''Lazarus Run Output'''
      + ' -e ''$(LazarusDir)/tools/runwait.sh $(TargetCmdLine)''');
      {$ENDIF}
    EndUpdate;
  end;

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
    Columns[0].Caption := dlgRunOVariable;
    Columns[1].Caption := dlgRunOValue;
    Columns.EndUpdate;
  end;

  UserOverridesGroupBox.Caption := dlgRunOUserOverrides;

  with UserOverridesListView do
  begin
    Columns.BeginUpdate;
    Columns[0].Caption := dlgRunOVariable;
    Columns[1].Caption := dlgRunOValue;
    Columns.EndUpdate;
  end;

  UserOverridesAddButton.Caption    := dlgEdAdd;
  UserOverridesEditButton.Caption   := dlgEdEdit;
  UserOverridesDeleteButton.Caption := dlgEdDelete;
  IncludeSystemVariablesCheckBox.Caption := dlgIncludeSystemVariables;
end;

procedure TRunParamsOptsDlg.OkButtonClick(Sender: TObject);
begin
  SaveToOptions;
  ModalResult := mrOk;
end;

procedure TRunParamsOptsDlg.FormResize(Sender: TObject);
begin
  SystemVariablesGroupBox.Height := (Notebook.Page[1].Height - 37) div 2;
  UserOverridesGroupBox.Height   := SystemVariablesGroupBox.Height;

  SystemVariablesListView.Column[0].Width := SystemVariablesListView.Width div 2;
  SystemVariablesListView.Column[1].Width := SystemVariablesListView.Column[0].Width;

  UserOverridesListView.Column[0].Width := UserOverridesListView.Width div 2;
  UserOverridesListView.Column[1].Width := UserOverridesListView.Column[0].Width;
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
        (MessageDlg(lisRunParamsFileNotExecutable,
        Format(lisRunParamsTheHostApplicationIsNotExecutable,
        ['"', OpenDialog.Filename, '"']), mtWarning, [mbCancel, mbIgnore], 0) =
        mrIgnore) then
      begin
        HostApplicationEdit.Text := OpenDialog.Filename;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  end;
end;

procedure TRunParamsOptsDlg.WorkingDirectoryBtnClick(Sender: TObject);
var
  NewDirectory: String;
begin
  NewDirectory:=InputHistories.SelectDirectory('Working directory',true,
                                    ExtractFilePath(WorkingDirectoryEdit.Text),
                                    ExtractFilename(WorkingDirectoryEdit.Text));
  if NewDirectory<>'' then
    WorkingDirectoryEdit.Text:=NewDirectory;
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
begin
  // local
  fOptions.HostApplicationFilename := Trim(HostApplicationEdit.Text);
  fOptions.CmdLineParams := Trim(CmdLineParametersEdit.Text);
  fOptions.UseLaunchingApplication := UseLaunchingApplicationCheckBox.Checked;
  fOptions.LaunchingApplicationPathPlusParams :=
    Trim(UseLaunchingApplicationComboBox.Text);
  fOptions.WorkingDirectory := Trim(WorkingDirectoryEdit.Text);
  fOptions.UseDisplay := UseDisplayCheckBox.Checked;
  fOptions.Display    := Trim(DisplayEdit.Text);

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
  fOptions := NewOptions;

  // local
  HostApplicationEdit.Text   := fOptions.HostApplicationFilename;
  CmdLineParametersEdit.Text := fOptions.CmdLineParams;
  UseLaunchingApplicationCheckBox.Checked := fOptions.UseLaunchingApplication;
  SetComboBoxText(UseLaunchingApplicationComboBox,
    fOptions.LaunchingApplicationPathPlusParams);
  WorkingDirectoryEdit.Text := fOptions.WorkingDirectory;
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

initialization
  {$I runparamsopts.lrs}

end.
