{ Options form for the lazarus package manager

  Copyright (C) 2011 Darius Blaszyk

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit fppkg_optionsfrm;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Dialogs, ComCtrls, ButtonPanel, StdCtrls, ExtCtrls, Buttons,
  pkgoptions, pkgglobals;

type
  TPkgColumn = record
    Name: string;
    Visible: boolean;
  end;

  { TPackageManagerOption }

  TPackageManagerOption = class(TObject)
  private
    FPkgColumnCount: integer;
    FPkgColumns: array of TPkgColumn;
    FVerbosity: TLogLevels;

    function GetPkgColumns(index: integer): TPkgColumn;
    procedure SetPkgColumnCount(const AValue: integer);
    procedure SetPkgColumns(index: integer; const AValue: TPkgColumn);

    procedure AddPkgColumn(Name: string; Visible: boolean);
    procedure SetVerbosity(const AValue: TLogLevels);
  public

    constructor Create;
    destructor Destroy; override;

    property PkgColumns[index: integer]: TPkgColumn read GetPkgColumns write SetPkgColumns;
    property PkgColumnCount: integer read FPkgColumnCount write SetPkgColumnCount;
    function PkgColumnByName(AName: string): integer;

    property Verbosity: TLogLevels read FVerbosity write SetVerbosity;
  end;


  { TOptionsForm }

  TOptionsForm = class(TForm)
    Button1: TButton;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    RemoveFromVisibleColumnsButton: TSpeedButton;
    VisibleColumnsLabel: TLabel;
    VisibleColumnsListBox: TListBox;
    AvailableColumnsLabel: TLabel;
    AvailableColumnsListBox: TListBox;
    AddToVisibleColumnsButton: TSpeedButton;
    lblMiddle: TLabel;
    UserInterfaceTabSheet: TTabSheet;
    VerbosityCheckGroup: TCheckGroup;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    CompilerConfigCheckBox2: TCheckBox;
    CompilerOptionsButton: TButton;
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CompilerConfigCheckBox: TCheckBox;
    CompilerConfigCheckBox1: TCheckBox;
    CompilerConfigEdit: TEdit;
    CompilerOptionsButton1: TButton;
    CompilerOptionsEdit: TEdit;
    CompilerOptionsEdit1: TEdit;
    CompilerOptionsGroupBox1: TGroupBox;
    Edit1: TEdit;
    GlobalListView: TListView;
    CompilerListView: TListView;
    FPMakeListView: TListView;
    FPMakePageControl: TPageControl;
    GlobalTabSheet: TTabSheet;
    CompilerTabSheet: TTabSheet;
    FPMakeTabSheet: TTabSheet;
    GroupBox1: TGroupBox;
    CompilerOptionsGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog: TOpenDialog;
    ConfigTabSheet: TTabSheet;
    TabSheet1: TTabSheet;
    procedure AddToVisibleColumnsButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RemoveFromVisibleColumnsButtonClick(Sender: TObject);
  private
    procedure SetupColumnVisibility;
  public

  end; 

var
  OptionsForm: TOptionsForm;
  LazPkgOptions: TPackageManagerOption;

implementation

{$R *.lfm}

uses
  fppkg_const;

{ TPackageManagerOption }

function TPackageManagerOption.GetPkgColumns(index: integer): TPkgColumn;
begin
  Result := FPkgColumns[index];
end;

procedure TPackageManagerOption.SetPkgColumnCount(const AValue: integer);
begin
  if FPkgColumnCount=AValue then exit;
  FPkgColumnCount:=AValue;
  SetLength(FPkgColumns,FPkgColumnCount);
end;

procedure TPackageManagerOption.SetPkgColumns(index: integer;
  const AValue: TPkgColumn);
begin
  FPkgColumns[index] := AValue;
end;

constructor TPackageManagerOption.Create;
begin
  PkgColumnCount := 0;

  AddPkgColumn('Name', True);
  AddPkgColumn('State', True);
  AddPkgColumn('Version', True);
  AddPkgColumn('Info', True);

  AddPkgColumn('Description', False);
  AddPkgColumn('Keywords', False);
  AddPkgColumn('Category', False);
  AddPkgColumn('Support', False);
{  AddPkgColumn('Author', False);
  AddPkgColumn('License', False);
  AddPkgColumn('HomepageURL', False);
  AddPkgColumn('DownloadURL', False);
  AddPkgColumn('FileName', False);
  AddPkgColumn('Email', False);
  AddPkgColumn('OS', False);
  AddPkgColumn('CPU', False);
}
  Verbosity := DefaultLogLevels;
end;

destructor TPackageManagerOption.Destroy;
begin
  inherited Destroy;
end;

function TPackageManagerOption.PkgColumnByName(AName: string): integer;
var
  i: integer;
begin
  for i := 0 to PkgColumnCount - 1 do
    if FPkgColumns[i].Name = AName then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TPackageManagerOption.AddPkgColumn(Name: string;
  Visible: boolean);
begin
  PkgColumnCount := PkgColumnCount + 1;

  FPkgColumns[PkgColumnCount-1].Name := Name;
  FPkgColumns[PkgColumnCount-1].Visible := Visible;
end;

procedure TPackageManagerOption.SetVerbosity(const AValue: TLogLevels);
begin
  if FVerbosity=AValue then exit;
  FVerbosity:=AValue;

  LogLevels := AValue;
end;

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);

  procedure AddListItem(LV: TListView; ACaption, AValue: string);
  var
    li: TListItem;
  begin
    li := LV.Items.Add;
    li.Caption:= ACaption;
    li.SubItems.Add(AValue);
  end;

begin
  Caption := rsFppkgOptions;

  //global
{  AddListItem(GlobalListView, rsRemoteMirrorsURL, GlobalOptions.GlobalSection.RemoteMirrorsURL);
  AddListItem(GlobalListView, rsRemoteRepository, GlobalOptions.GlobalSection.RemoteRepository);
  AddListItem(GlobalListView, rsLocalRepository, GlobalOptions.GlobalSection.LocalRepository);
  AddListItem(GlobalListView, rsBuildDirectory, GlobalOptions.GlobalSection.BuildDir);
  AddListItem(GlobalListView, rsArchivesDirectory, GlobalOptions.GlobalSection.ArchivesDir);
  AddListItem(GlobalListView, rsCompilerConfigDirectory, GlobalOptions.GlobalSection.CompilerConfigDir);
  AddListItem(GlobalListView, rsDefaultCompilerConfig, GlobalOptions.GlobalSection.CompilerConfig);
  AddListItem(GlobalListView, rsFpmakeCompilerConfig, GlobalOptions.GlobalSection.FPMakeCompilerConfig);
  AddListItem(GlobalListView, rsDownloader, GlobalOptions.GlobalSection.Downloader);
  AddListItem(GlobalListView, rsCustomFpmakeOptions, GlobalOptions.GlobalSection.CustomFPMakeOptions);

  //compiler
  AddListItem(CompilerListView, rsCompiler, CompilerOptions.Compiler);
  AddListItem(CompilerListView, rsCompilerTarget, CompilerOptions.CompilerTarget);
  AddListItem(CompilerListView, rsCompilerVersion, CompilerOptions.CompilerVersion);
  AddListItem(CompilerListView, rsGlobalPrefix, CompilerOptions.GlobalPrefix);
  AddListItem(CompilerListView, rsLocalPrefix, CompilerOptions.LocalPrefix);
  AddListItem(CompilerListView, rsGlobalInstallDir, CompilerOptions.GlobalInstallDir);
  AddListItem(CompilerListView, rsLocalInstallDir, CompilerOptions.LocalInstallDir);
  AddListItem(CompilerListView, rsOptions, CompilerOptions.Options.DelimitedText);
 }
  //fpmake
  // Load FPMake compiler config, this is normally the same config as above
{  AddListItem(FPMakeListView, rsCompiler, FPMakeCompilerOptions.Compiler);
  AddListItem(FPMakeListView, rsCompilerTarget, FPMakeCompilerOptions.CompilerTarget);
  AddListItem(FPMakeListView, rsCompilerVersion, FPMakeCompilerOptions.CompilerVersion);
  AddListItem(FPMakeListView, rsGlobalPrefix, FPMakeCompilerOptions.GlobalPrefix);
  AddListItem(FPMakeListView, rsLocalPrefix, FPMakeCompilerOptions.LocalPrefix);
  AddListItem(FPMakeListView, rsGlobalInstallDir, FPMakeCompilerOptions.GlobalInstallDir);
  AddListItem(FPMakeListView, rsLocalInstallDir, FPMakeCompilerOptions.LocalInstallDir);
  AddListItem(FPMakeListView, rsOptions, FPMakeCompilerOptions.Options.DelimitedText);
 }
  FPMakePageControl.ActivePage := ConfigTabSheet;
end;

procedure TOptionsForm.FormShow(Sender: TObject);
begin
  SetupColumnVisibility;

  //setup verbosity
  with VerbosityCheckGroup do
  begin
    Checked[Items.IndexOf('Error')] := llError in LazPkgOptions.Verbosity;
    Checked[Items.IndexOf('Warning')] := llWarning in LazPkgOptions.Verbosity;
    Checked[Items.IndexOf('Info')] := llInfo in LazPkgOptions.Verbosity;
    Checked[Items.IndexOf('Commands')] := llCommands in LazPkgOptions.Verbosity;
    Checked[Items.IndexOf('Debug')] := llDebug in LazPkgOptions.Verbosity;
    Checked[Items.IndexOf('Progress')] := llProgress in LazPkgOptions.Verbosity;
  end;
end;

procedure TOptionsForm.OKButtonClick(Sender: TObject);
begin
  //save the data to settings file

  //save verbosity
  LazPkgOptions.Verbosity := [];
  with VerbosityCheckGroup do
  begin
    if Checked[Items.IndexOf('Error')] then
      LazPkgOptions.Verbosity := LazPkgOptions.Verbosity + [llError];
    if Checked[Items.IndexOf('Warning')] then
      LazPkgOptions.Verbosity := LazPkgOptions.Verbosity + [llWarning];
    if Checked[Items.IndexOf('Info')] then
      LazPkgOptions.Verbosity := LazPkgOptions.Verbosity + [llInfo];
    if Checked[Items.IndexOf('Commands')] then
      LazPkgOptions.Verbosity := LazPkgOptions.Verbosity + [llCommands];
    if Checked[Items.IndexOf('Debug')] then
      LazPkgOptions.Verbosity := LazPkgOptions.Verbosity + [llDebug];
    if Checked[Items.IndexOf('Progress')] then
      LazPkgOptions.Verbosity := LazPkgOptions.Verbosity + [llProgress];
  end;

  Close;
end;

procedure TOptionsForm.RemoveFromVisibleColumnsButtonClick(Sender: TObject);
var
  i: integer;
  pkg: TPkgColumn;
  c: integer;
begin
  i := 0;
  c := -1;
  while i < VisibleColumnsListBox.Items.Count do
  begin
    if VisibleColumnsListBox.Selected[i] then
      c := i;

    Inc(i);
  end;

  if c <> -1 then
  begin
    i := LazPkgOptions.PkgColumnByName(VisibleColumnsListBox.Items[c]);
    pkg := LazPkgOptions.PkgColumns[i];
    pkg.Visible := False;
    LazPkgOptions.PkgColumns[i] := pkg;
    SetupColumnVisibility;
  end;
end;

procedure TOptionsForm.SetupColumnVisibility;
var
  i: integer;
begin
  VisibleColumnsListBox.Clear;
  AvailableColumnsListBox.Clear;

  for i := 0 to LazPkgOptions.PkgColumnCount - 1 do
  begin
    if LazPkgOptions.PkgColumns[i].Visible then
      VisibleColumnsListBox.Items.Add(LazPkgOptions.PkgColumns[i].Name)
    else
      AvailableColumnsListBox.Items.Add(LazPkgOptions.PkgColumns[i].Name)
  end;
end;

procedure TOptionsForm.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    CompilerConfigEdit.Text := OpenDialog.FileName;
end;

procedure TOptionsForm.AddToVisibleColumnsButtonClick(Sender: TObject);
var
  i: integer;
  pkg: TPkgColumn;
  c: integer;
begin
  i := 0;
  c := -1;
  while i < AvailableColumnsListBox.Items.Count do
  begin
    if AvailableColumnsListBox.Selected[i] then
      c := i;

    Inc(i);
  end;

  if c <> -1 then
  begin
    i := LazPkgOptions.PkgColumnByName(AvailableColumnsListBox.Items[c]);
    pkg := LazPkgOptions.PkgColumns[i];
    pkg.Visible := True;
    LazPkgOptions.PkgColumns[i] := pkg;
    SetupColumnVisibility;
  end;
end;

procedure TOptionsForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

initialization
  LazPkgOptions := TPackageManagerOption.Create;

finalization
  FreeAndNil(LazPkgOptions);

end.

