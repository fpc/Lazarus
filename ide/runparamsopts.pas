{
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

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
  ExtCtrls, LResources, XMLCfg, DOS, IDEProcs, SysVarUserOverrideDlg;

{ The xml format version:
    When the format changes (new values, changed formats) we can distinguish old
    files and are able to convert them.
} 
const RunParamsOptionsVersion = '1';

type
  {
    the storage object for run parameters
  }
  TRunParamsOptions = class
  private
    // local options
    fHostApplicationFilename: string;
    fCmdLineParams: string;
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
    function Load(XMLConfig: TXMLConfig; const Path: string): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string): TModalResult;
    
    // local options
    property HostApplicationFilename: string
           read fHostApplicationFilename write fHostApplicationFilename;
    property CmdLineParams: string read fCmdLineParams write fCmdLineParams;
    property UseLaunchingApplication: boolean
           read fUseLaunchingApplication write fUseLaunchingApplication;
    property LaunchingApplicationPathPlusParams: string
           read fLaunchingApplicationPathPlusParams
           write fLaunchingApplicationPathPlusParams;
    property WorkingDirectory: string 
           read fWorkingDirectory write fWorkingDirectory;
    property Display: string read fDisplay write fDisplay;
    
    // environment options
    property UserOverrides: TStringList read fUserOverrides;
    property IncludeSystemVariables: boolean
          read fIncludeSystemVariables write fIncludeSystemVariables;
  end;

  {
    TRunParamsOptsDlg is the form of the run parameters options dialog
  }
  TRunParamsOptsDlg = class(TForm)
    Notebook: TNotebook;
    HostApplicationGroupBox: TGroupBox;
    HostApplicationEdit: TEdit;
    HostApplicationBrowseBtn: TBitBtn;
    CmdLineParametersGroupBox: TGroupBox;
    CmdLineParametersEdit: TEdit;
    UseLaunchingApplicationBevel: TBevel;
    UseLaunchingApplicationCheckBox: TCheckBox;
    UseLaunchingApplicationEdit: TEdit;
    WorkingDirectoryGroupBox: TGroupBox;
    WorkingDirectoryEdit: TEdit;
    WorkingDirectoryBtn: TBitBtn;
    DisplayGroupBox: TGroupBox;
    DisplayEdit: TEdit;
    SystemVariablesGroupBox: TGroupBox;
    SystemVariablesListView: TListView;
    UserOverridesGroupBox: TGroupBox;
    UserOverridesListView: TListView;
    UserOverridesAddButton: TButton;
    UserOverridesEditButton: TButton;
    UserOverridesDeleteButton: TButton;
    IncludeSystemVariablesCheckBox: TCheckBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
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
    procedure FillSystemVariablesListView;
    procedure SaveToOptions;
  public
    constructor Create(AnOwner: TComponent); override;
    property Options: TRunParamsOptions read fOptions write SetOptions;
  end;


function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions):TModalResult;


implementation


const
  DefaultLauncherApplication =
    '/usr/X11R6/bin/xterm -T ''Lazarus Run Output'''
    +' -e bash -i -c ''$(TargetCmdLine)''';

function ShowRunParamsOptsDlg(RunParamsOptions: TRunParamsOptions):TModalResult;
var
  RunParamsOptsForm: TRunParamsOptsDlg;
begin
  Result:=mrCancel;
  RunParamsOptsForm:=TRunParamsOptsDlg.Create(Application);
  try
    RunParamsOptsForm.Options:=RunParamsOptions;
    Result:=RunParamsOptsForm.ShowModal;
  finally
    RunParamsOptsForm.Free;
  end;
end;



{ TRunParamsOptions }

constructor TRunParamsOptions.Create;
begin
  inherited Create;
  fUserOverrides:=TStringList.Create;
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
  fHostApplicationFilename:='';
  fCmdLineParams:='';
  fUseLaunchingApplication:=false;
  fLaunchingApplicationPathPlusParams:=DefaultLauncherApplication;
  fWorkingDirectory:='';
  fDisplay:=':0';
    
  // environment options
  fUserOverrides.Clear;
  fIncludeSystemVariables:=false;
end;

function TRunParamsOptions.Load(XMLConfig: TXMLConfig;
  const Path: string): TModalResult;

  procedure LoadUserOverrides(const APath: string);
  var i, Cnt: integer;
  begin
    fUserOverrides.Clear;
    Cnt:=XMLConfig.GetValue(APath+'Count',0);
    for i:=0 to Cnt-1 do begin
      fUserOverrides.Values[XMLConfig.GetValue(
          APath+'Variable'+IntToStr(i)+'/Name','')]
        :=XMLConfig.GetValue(APath+'Variable'+IntToStr(i)+'/Value','');
    end;
  end;

begin
  // local options
  fHostApplicationFilename:=XMLConfig.GetValue(
    Path+'RunParams/local/HostApplicationFilename/Value',
      fHostApplicationFilename);
  fCmdLineParams:=XMLConfig.GetValue(
    Path+'RunParams/local/CommandLineParams/Value',
      fCmdLineParams);
  fUseLaunchingApplication:=XMLConfig.GetValue(
    Path+'RunParams/local/LaunchingApplication/Use',
      fUseLaunchingApplication);
  fLaunchingApplicationPathPlusParams:=XMLConfig.GetValue(
    Path+'RunParams/local/LaunchingApplication/PathPlusParams',
      fLaunchingApplicationPathPlusParams);
  if (fLaunchingApplicationPathPlusParams='') then
    fLaunchingApplicationPathPlusParams:=DefaultLauncherApplication;
  fWorkingDirectory:=XMLConfig.GetValue(
    Path+'RunParams/local/WorkingDirectory/Value',
      fWorkingDirectory);
  fDisplay:=XMLConfig.GetValue(
    Path+'RunParams/local/Display/Value',
      fDisplay);

  // environment options
  LoadUserOverrides(Path+'RunParams/environment/UserOverrides/');
  fIncludeSystemVariables:=XMLConfig.GetValue(
    Path+'RunParams/environment/IncludeSystemVariables/Value',
      fIncludeSystemVariables);
  
  Result:=mrOk;
end;

function TRunParamsOptions.Save(XMLConfig: TXMLConfig;
  const Path: string): TModalResult;

  procedure SaveUserOverrides(const APath: string);
  var i: integer;
  begin
    XMLConfig.SetValue(APath+'Count',fUserOverrides.Count);
    for i:=0 to fUserOverrides.Count-1 do begin
      XMLConfig.SetValue(APath+'Variable'+IntToStr(i)+'/Name',
        fUserOverrides.Names[i]);
      XMLConfig.SetValue(APath+'Variable'+IntToStr(i)+'/Value',
        fUserOverrides.Values[fUserOverrides.Names[i]]);
    end;
  end;

begin
  // save a format version to distinguish old formats
  XMLConfig.SetValue(Path+'RunParams/local/FormatVersion/Value',
    RunParamsOptionsVersion);

  // local options
  XMLConfig.SetValue(Path+'RunParams/local/HostApplicationFilename/Value',
    fHostApplicationFilename);
  XMLConfig.SetValue(Path+'RunParams/local/CommandLineParams/Value',
    fCmdLineParams);
  XMLConfig.SetValue(Path+'RunParams/local/LaunchingApplication/Use',
    fUseLaunchingApplication);
  XMLConfig.SetValue(Path+'RunParams/local/LaunchingApplication/PathPlusParams',
    fLaunchingApplicationPathPlusParams);
  XMLConfig.SetValue(Path+'RunParams/local/WorkingDirectory/Value',
    fWorkingDirectory);
  XMLConfig.SetValue(Path+'RunParams/local/Display/Value',
    fDisplay);

  // environment options
  SaveUserOverrides(Path+'RunParams/environment/UserOverrides/');
  XMLConfig.SetValue(Path+'RunParams/environment/IncludeSystemVariables/Value',
    fIncludeSystemVariables);
  
  Result:=mrOk;
end;


{ TRunParamsOptsDlg }

constructor TRunParamsOptsDlg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName)=nil then begin

    Caption:='Run parameters';
    SetBounds((Screen.Width-500) div 2,(Screen.Height-450) div 2,500,450);
  
    SetupNotebook;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      SetBounds(270,Self.ClientHeight-40,100,25);
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;
    
    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      SetBounds(390,OkButton.Top,100,25);
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;
  end;
end;

procedure TRunParamsOptsDlg.SetupNotebook;
// create the notebook
begin
  Notebook:=TNotebook.Create(Self);
  with Notebook do begin
    Name:='Notebook';
    Parent:=Self;
    SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
    Pages[0]:='Local';
    Pages.Add('Environment');
    Visible:=true;
  end;

  SetupLocalPage;
  SetupEnvironmentPage;
end;

procedure TRunParamsOptsDlg.SetupLocalPage;
var w: integer;
begin
  w:=Self.ClientWidth-15;
  HostApplicationGroupBox:=TGroupBox.Create(Self);
  with HostApplicationGroupBox do begin
    Name:='HostApplicationGroupBox';
    Parent:=NoteBook.Page[0];
    SetBounds(5,5,w,60);
    Caption:='Host application';
    Visible:=true;
  end;
  
  HostApplicationEdit:=TEdit.Create(Self);
  with HostApplicationEdit do begin
    Name:='HostApplicationEdit';
    Parent:=HostApplicationGroupBox;
    SetBounds(5,5,w-10-35,25);
    Caption:='';
    Visible:=true;
  end;
  
  HostApplicationBrowseBtn:=TBitBtn.Create(Self);
  with HostApplicationBrowseBtn do begin
    Name:='HostApplicationBrowseBtn';
    Parent:=HostApplicationGroupBox;
    SetBounds(HostApplicationEdit.Left+HostApplicationEdit.Width+2,5,25,25);
    Caption:='...';
    HostApplicationBrowseBtn.OnClick:=@HostApplicationBrowseBtnClick;
    Visible:=true;
  end;
  
  CmdLineParametersGroupBox:=TGroupBox.Create(Self);
  with CmdLineParametersGroupBox do begin
    Name:='CmdLineParametersGroupBox';
    Parent:=NoteBook.Page[0];
    SetBounds(5,HostApplicationGroupBox.Top+HostApplicationGroupBox.Height+5,
                 w,60);
    Caption:='Command line parameters (without application name)';
    Visible:=true;
  end;
  
  CmdLineParametersEdit:=TEdit.Create(Self);
  with CmdLineParametersEdit do begin
    Name:='CmdLineParametersEdit';
    Parent:=CmdLineParametersGroupBox;
    SetBounds(5,5,w-15,25);
    Caption:='';
    Visible:=true;
  end;
  
  UseLaunchingApplicationBevel:=TBevel.Create(Self);
  with UseLaunchingApplicationBevel do begin
    Name:='UseLaunchingApplicationBevel';
    Parent:=NoteBook.Page[0];
    SetBounds(
      5,CmdLineParametersGroupBox.Top+CmdLineParametersGroupBox.Height+10,w,60);
    Visible:=true;
  end;
  
  UseLaunchingApplicationCheckBox:=TCheckBox.Create(Self);
  with UseLaunchingApplicationCheckBox do begin
    Name:='UseLaunchingApplicationCheckBox';
    Parent:=NoteBook.Page[0];
    SetBounds(15,
      CmdLineParametersGroupBox.Top+CmdLineParametersGroupBox.Height+10,250,25);
    Caption:='Use launching application';
    Checked:=false;
    Visible:=true;
  end;
  
  UseLaunchingApplicationEdit:=TEdit.Create(Self);
  with UseLaunchingApplicationEdit do begin
    Name:='UseLaunchingApplicationEdit';
    Parent:=NoteBook.Page[0];
    SetBounds(UseLaunchingApplicationCheckBox.Left,
                 UseLaunchingApplicationCheckBox.Top
                 +UseLaunchingApplicationCheckBox.Height+2,w-15,25);
    Caption:='';
    Visible:=true;
  end;
  
  WorkingDirectoryGroupBox:=TGroupBox.Create(Self);
  with WorkingDirectoryGroupBox do begin
    Name:='WorkingDirectoryGroupBox';
    Parent:=NoteBook.Page[0];
    SetBounds(5,UseLaunchingApplicationEdit.Top
                   +UseLaunchingApplicationEdit.Height+15,w,60);
    Caption:='Working directory';
    Visible:=true;
  end;
  
  WorkingDirectoryEdit:=TEdit.Create(Self);
  with WorkingDirectoryEdit do begin
    Name:='WorkingDirectoryEdit';
    Parent:=WorkingDirectoryGroupBox;
    SetBounds(5,5,w-10-35,25);
    Caption:='';
    Visible:=true;
  end;
  
  WorkingDirectoryBtn:=TBitBtn.Create(Self);
  with WorkingDirectoryBtn do begin
    Name:='WorkingDirectoryBtn';
    Parent:=WorkingDirectoryGroupBox;
    SetBounds(WorkingDirectoryEdit.Left+WorkingDirectoryEdit.Width+2,5,25,25);
    Caption:='...';
    WorkingDirectoryBtn.OnClick:=@WorkingDirectoryBtnClick;
    Visible:=true;
  end;
  
  DisplayGroupBox:=TGroupBox.Create(Self);
  with DisplayGroupBox do begin
    Name:='DisplayGroupBox';
    Parent:=NoteBook.Page[0];
    SetBounds(5,WorkingDirectoryGroupBox.Top+WorkingDirectoryGroupBox.Height+10,
                 w,60);
    Caption:='Display (not for win32)';
    Visible:=true;
  end;
  
  DisplayEdit:=TEdit.Create(Self);
  with DisplayEdit do begin
    Name:='DisplayEdit';
    Parent:=DisplayGroupBox;
    SetBounds(5,5,w-15,25);
    Caption:='';
    Visible:=true;
  end;
end;

procedure TRunParamsOptsDlg.SetupEnvironmentPage;
var w: integer;
begin
  w:=Self.ClientWidth-15;
  
  SystemVariablesGroupBox:=TGroupBox.Create(Self);
  with SystemVariablesGroupBox do begin
    Name:='SystemVariablesGroupBox';
    Parent:=NoteBook.Page[1];
    SetBounds(5,5,w,150);
    Caption:='System variables';
    Visible:=true;
  end;
  
  SystemVariablesListView:=TListView.Create(Self);
  with SystemVariablesListView do begin
    Name:='SystemVariablesListView';
    Parent:=SystemVariablesGroupBox;
    Left:=5;
    Top:=5;
    Width:=Parent.ClientWidth-17;
    Height:=Parent.ClientHeight-28;
    Columns.BeginUpdate;
    Columns.Clear;
    Columns.Add;
    Columns.Add;
    Columns[0].Caption:='Variable';
    Columns[0].Width:=130;
    Columns[1].Caption:='Value';
    Columns.EndUpdate;
    ViewStyle := vsReport;
    Sorted := true;
    Visible:=true;
  end;
  
  UserOverridesGroupBox:=TGroupBox.Create(Self);
  with UserOverridesGroupBox do begin
    Name:='UserOverridesGroupBox';
    Parent:=NoteBook.Page[1];
    SetBounds(5,SystemVariablesGroupBox.Top+SystemVariablesGroupBox.Height+10,
                 w,150);
    Caption:='User overrides';
    Visible:=true;
  end;
  
  UserOverridesListView:=TListView.Create(Self);
  with UserOverridesListView do begin
    Name:='UserOverridesListView';
    Parent:=UserOverridesGroupBox;
    Left:=5;
    Top:=5;
    Width:=Parent.ClientWidth-17;
    Height:=Parent.ClientHeight-68;
    Columns.BeginUpdate;
    Columns.Clear;
    Columns.Add;
    Columns.Add;
    Columns[0].Caption:='Variable';
    Columns[0].Width:=130;
    Columns[1].Caption:='Value';
    Columns.EndUpdate;
    ViewStyle := vsReport;
    Sorted := true;
    Visible:=true;
  end;

  UserOverridesAddButton:=TButton.Create(Self);
  with UserOverridesAddButton do begin
    Name:='UserOverridesAddButton';
    Parent:=UserOverridesGroupBox;
    Left:=5;
    Top:=Parent.ClientHeight-Height-28;
    Width:=100;
    Caption:='Add';
    OnClick:=@UserOverridesAddButtonClick;
    Enabled:=false;
    Visible:=true;
  end;

  UserOverridesEditButton:=TButton.Create(Self);
  with UserOverridesEditButton do begin
    Name:='UserOverridesEditButton';
    Parent:=UserOverridesGroupBox;
    Left:=UserOverridesAddButton.Left+UserOverridesAddButton.Width+10;
    Top:=UserOverridesAddButton.Top;
    Width:=100;
    Caption:='Edit';
    OnClick:=@UserOverridesEditButtonClick;
    Enabled:=false;
    Visible:=true;
  end;

  UserOverridesDeleteButton:=TButton.Create(Self);
  with UserOverridesDeleteButton do begin
    Name:='UserOverridesDeleteButton';
    Parent:=UserOverridesGroupBox;
    Left:=UserOverridesEditButton.Left+UserOverridesEditButton.Width+10;
    Top:=UserOverridesEditButton.Top;
    Width:=100;
    Caption:='Delete';
    OnClick:=@UserOverridesDeleteButtonClick;
    Enabled:=false;
    Visible:=true;
  end;

  IncludeSystemVariablesCheckBox:=TCheckBox.Create(Self);
  with IncludeSystemVariablesCheckBox do begin
    Name:='IncludeSystemVariablesCheckBox';
    Parent:=NoteBook.Page[1];
    SetBounds(5,UserOverridesGroupBox.Top+UserOverridesGroupBox.Height+10,w,25);
    Caption:='Include system variables';
    Checked:=false;
    Visible:=true;
  end;
end;

procedure TRunParamsOptsDlg.OkButtonClick(Sender: TObject);
begin
  SaveToOptions;
  ModalResult:=mrOk;
end;

procedure TRunParamsOptsDlg.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TRunParamsOptsDlg.HostApplicationBrowseBtnClick(Sender: TObject);
var OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(Self);
  with OpenDialog do begin
    if HostApplicationEdit.Text<>'' then
      OpenDialog.InitialDir:=ExtractFilePath(HostApplicationEdit.Text);
    OpenDialog.Filename:=HostApplicationEdit.Text;
    if OpenDialog.Execute then begin
      if (FileIsExecutable(OpenDialog.Filename))
      or (MessageDlg('File not executable',
          'The host application "'+OpenDialog.Filename+'" is not executable.',
          mtWarning,[mbCancel,mbIgnore],0)=mrIgnore) then
      begin
        HostApplicationEdit.Text:=OpenDialog.Filename;
      end;
    end;
  end;
end;

procedure TRunParamsOptsDlg.WorkingDirectoryBtnClick(Sender: TObject);
var OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(Self);
  with OpenDialog do begin
    if WorkingDirectoryEdit.Text<>'' then
      OpenDialog.InitialDir:=ExtractFilePath(WorkingDirectoryEdit.Text);
    OpenDialog.Filename:=HostApplicationEdit.Text;
    if OpenDialog.Execute then begin
      if (DirectoryExists(OpenDialog.Filename))
      or (MessageDlg('Directory does not exist',
          'The directory "'+OpenDialog.Filename+'" does not exist.',
          mtWarning,[mbIgnore,mbCancel],0)=mrIgnore) then
      begin
        WorkingDirectoryEdit.Text:=OpenDialog.Filename;
      end;
    end;
  end;
end;

procedure TRunParamsOptsDlg.UserOverridesAddButtonClick(Sender: TObject);
var Variable, Value: string;
begin
  Variable:='';
  Value:='';
  if ShowSysVarUserOverrideDialog(Variable,Value)=mrOk then begin

    
  end;
end;

procedure TRunParamsOptsDlg.UserOverridesEditButtonClick(Sender: TObject);
begin

end;

procedure TRunParamsOptsDlg.UserOverridesDeleteButtonClick(Sender: TObject);
begin

end;

procedure TRunParamsOptsDlg.SaveToOptions;
begin
  // local
  fOptions.HostApplicationFilename:=Trim(HostApplicationEdit.Text);
  fOptions.CmdLineParams:=Trim(CmdLineParametersEdit.Text);
  fOptions.UseLaunchingApplication:=UseLaunchingApplicationCheckBox.Checked;
  fOptions.LaunchingApplicationPathPlusParams:=
                                      Trim(UseLaunchingApplicationEdit.Text);
  fOptions.WorkingDirectory:=Trim(WorkingDirectoryEdit.Text);
  fOptions.Display:=Trim(DisplayEdit.Text);
  
  // environment

  // ToDo: User Overrides

  fOptions.IncludeSystemVariables:=IncludeSystemVariablesCheckBox.Checked;
end;

procedure TRunParamsOptsDlg.SetOptions(NewOptions: TRunParamsOptions);
begin
  fOptions:=NewOptions;
  
  // local
  HostApplicationEdit.Text:=fOptions.HostApplicationFilename;
  CmdLineParametersEdit.Text:=fOptions.CmdLineParams;
  UseLaunchingApplicationCheckBox.Checked:=fOptions.UseLaunchingApplication;
  UseLaunchingApplicationEdit.Text:=fOptions.LaunchingApplicationPathPlusParams;
  WorkingDirectoryEdit.Text:=fOptions.WorkingDirectory;
  DisplayEdit.Text:=fOptions.Display;
  
  // environment
  FillSystemVariablesListView;
  // ToDo: User Overrides
  
  IncludeSystemVariablesCheckBox.Checked:=fOptions.IncludeSystemVariables;
end;

procedure TRunParamsOptsDlg.FillSystemVariablesListView;
var
  i, SysVarCount, e: integer;
  Variable, Value: string;
Begin
  with SystemVariablesListView.Items do begin
    //BeginUpdate;
    SysVarCount:=EnvCount;
    for i:=0 to SysVarCount-1 do begin
      Variable:=EnvStr(i+1);
      e:=1;
      while (e<=length(Variable)) and (Variable[e]<>'=') do inc(e);
      Value:=copy(Variable,e+1,length(Variable)-e);
      Variable:=LeftStr(Variable,e-1);
      if Count<=i then begin
        // add line to listview
        Add;
        Item[i].SubItems.Add('');
      end;
      Item[i].Caption:=Variable;
      Item[i].SubItems[0]:=Value;
    end;
    while Count>EnvCount do
      Delete(Count-1);
    //EndUpdate;
  end;
end;


end.
