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
    Defines the TBuildLazarusOptions which stores the settings for the
    "Build Lazarus" function of the IDE.
    TConfigureBuildLazarusDlg is used to edit TBuildLazarusOptions.
    
    The BuildLazarus function will build the lazarus parts.
}
unit BuildLazDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Buttons, LResources,
  Laz_XMLCfg, ExtToolDialog, ExtToolEditDlg, TransferMacros, LazConf;

type
  TMakeMode = (mmNone, mmBuild, mmCleanBuild);

  TBuildLazarusOptions = class
  private
    fBuildLCL: TMakeMode;
    fBuildComponents: TMakeMode;
    fBuildSynEdit: TMakeMode;
    fBuildCodeTools: TMakeMode;
    fBuildIDE: TMakeMode;
    fBuildExamples: TMakeMode;
    fCleanAll: boolean;
    fMakeFilename: string;
    fExtraOptions: string;
    fTargetOS: string;
  public
    constructor Create;
    procedure Load(XMLConfig: TXMLConfig; const Path: string);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    property BuildLCL: TMakeMode read fBuildLCL write fBuildLCL;
    property BuildComponents: TMakeMode
      read fBuildComponents write fBuildComponents;
    property BuildSynEdit: TMakeMode read fBuildSynEdit write fBuildSynEdit;
    property BuildCodeTools: TMakeMode read fBuildCodeTools write fBuildCodeTools;
    property BuildIDE: TMakeMode read fBuildIDE write fBuildIDE;
    property BuildExamples: TMakeMode read fBuildExamples write fBuildExamples;
    property CleanAll: boolean read fCleanAll write fCleanAll;
    property MakeFilename: string read fMakeFilename write fMakeFilename;
    property ExtraOptions: string read fExtraOptions write fExtraOptions;
    property TargetOS: string read fTargetOS write fTargetOS;
  end;

  TConfigureBuildLazarusDlg = class(TForm)
    CleanAllCheckBox: TCheckBox;
    BuildAllButton: TButton;
    BuildLCLRadioGroup: TRadioGroup;
    BuildComponentsRadioGroup: TRadioGroup;
    BuildSynEditRadioGroup: TRadioGroup;
    BuildCodeToolsRadioGroup: TRadioGroup;
    BuildIDERadioGroup: TRadioGroup;
    BuildExamplesRadioGroup: TRadioGroup;
    OptionsLabel: TLabel;
    OptionsEdit: TEdit;
    TargetOSLabel: TLabel;
    TargetOSEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure BuildAllButtonClick(Sender: TObject);
    procedure ConfigureBuildLazarusDlgKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    procedure ConfigureBuildLazarusDlgResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    function MakeModeToInt(MakeMode: TMakeMode): integer;
    function IntToMakeMode(i: integer): TMakeMode;
  public
    procedure Load(Options: TBuildLazarusOptions);
    procedure Save(Options: TBuildLazarusOptions);
    constructor Create(AnOwner: TComponent); override;
  end;

function ShowConfigureBuildLazarusDlg(
  Options: TBuildLazarusOptions): TModalResult;

function BuildLazarus(Options: TBuildLazarusOptions;
  ExternalTools: TExternalToolList; Macros: TTransferMacroList): TModalResult;

implementation


uses
  LCLType;

const
  MakeModeNames: array[TMakeMode] of string = (
      'None', 'Build', 'Clean & Build'
    );


function StrToMakeMode(const s: string): TMakeMode;
begin
  for Result:=Succ(mmNone) to High(TMakeMode) do
    if AnsiCompareText(s,MakeModeNames[Result])=0 then exit;
  Result:=mmNone;
end;

function ShowConfigureBuildLazarusDlg(
  Options: TBuildLazarusOptions): TModalResult;
var ConfigBuildLazDlg: TConfigureBuildLazarusDlg;
begin
  Result:=mrCancel;
  ConfigBuildLazDlg:=TConfigureBuildLazarusDlg.Create(Application);
  try
    ConfigBuildLazDlg.Load(Options);
    Result:=ConfigBuildLazDlg.ShowModal;
    if Result=mrOk then
      ConfigBuildLazDlg.Save(Options);
  finally
    ConfigBuildLazDlg.Free;
  end;
  Result:=mrOk;
end;

function BuildLazarus(Options: TBuildLazarusOptions;
  ExternalTools: TExternalToolList; Macros: TTransferMacroList): TModalResult;
var
  Tool: TExternalToolOptions;
  
  procedure SetMakeParams(MakeMode: TMakeMode;
    const ExtraOpts, TargetOS: string);
  begin
    if MakeMode=mmBuild then
      Tool.CmdLineParams:='all'
    else
      Tool.CmdLineParams:='clean all';
    if TargetOS<>'' then
      Tool.CmdLineParams:= 'OS_TARGET='+ TargetOS+' '+Tool.CmdLineParams;
    if ExtraOpts<>'' then
      Tool.CmdLineParams:='OPT='''+ExtraOpts+''' '+Tool.CmdLineParams;
  end;
  
begin
  Result:=mrCancel;
  Tool:=TExternalToolOptions.Create;
  try
    Tool.Filename:=Options.MakeFilename;
    if not FileExists(Tool.Filename) then begin
      Tool.Filename:=FindDefaultMakePath;
      if not FileExists(Tool.Filename) then exit;
    end;
    Tool.ScanOutputForFPCMessages:=true;
    Tool.ScanOutputForMakeMessages:=true;
    if Options.CleanAll then begin
      // clean lazarus source directories
      Tool.Title:='Clean Lazarus Source';
      Tool.WorkingDirectory:='$(LazarusDir)';
      Tool.CmdLineParams:='cleanall';
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildLCL<>mmNone then begin
      // build lcl
      Tool.Title:='Build LCL';
      Tool.WorkingDirectory:='$(LazarusDir)/lcl';
      SetMakeParams(Options.BuildComponents,Options.ExtraOptions,
                    Options.TargetOS);
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildComponents<>mmNone then begin
      // build components
      Tool.Title:='Build Component';
      Tool.WorkingDirectory:='$(LazarusDir)/components';
      SetMakeParams(Options.BuildComponents,Options.ExtraOptions,
                    Options.TargetOS);
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end else begin
      if Options.BuildSynEdit<>mmNone then begin
        // build SynEdit
        Tool.Title:='Build SynEdit';
        Tool.WorkingDirectory:='$(LazarusDir)/components/synedit';
        SetMakeParams(Options.BuildComponents,Options.ExtraOptions,
                      Options.TargetOS);
        Result:=ExternalTools.Run(Tool,Macros);
        if Result<>mrOk then exit;
      end;
      if Options.BuildCodeTools<>mmNone then begin
        // build CodeTools
        Tool.Title:='Build CodeTools';
        Tool.WorkingDirectory:='$(LazarusDir)/components/codetools';
        SetMakeParams(Options.BuildComponents,Options.ExtraOptions,
                      Options.TargetOS);
        Result:=ExternalTools.Run(Tool,Macros);
        if Result<>mrOk then exit;
      end;
    end;
    if Options.BuildIDE<>mmNone then begin
      // build IDE
      Tool.Title:='Build IDE';
      Tool.WorkingDirectory:='$(LazarusDir)';
      if Options.ExtraOptions<>'' then
        Tool.CmdLineParams:='OPT='''+Options.ExtraOptions+''' '
      else
        Tool.CmdLineParams:='';
      if Options.TargetOS<>'' then
        Tool.CmdLineParams:= 'OS_TARGET='+Options.TargetOS+' '
                             +Tool.CmdLineParams;
      if Options.BuildIDE=mmBuild then
        Tool.CmdLineParams:=Tool.CmdLineParams+'ide'
      else
        Tool.CmdLineParams:=Tool.CmdLineParams+'cleanide ide';
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildExamples<>mmNone then begin
      // build Examples
      Tool.Title:='Build Examples';
      Tool.WorkingDirectory:='$(LazarusDir)/examples';
      SetMakeParams(Options.BuildComponents,Options.ExtraOptions,
                    Options.TargetOS);
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    Result:=mrOk;
  finally
    Tool.Free;
  end;
end;

{ TConfigureBuildLazarusDlg }

constructor TConfigureBuildLazarusDlg.Create(AnOwner: TComponent);
var MakeMode: TMakeMode;
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(Classname)=nil then begin
    Width:=350;
    Height:=435;
    Position:=poScreenCenter;
    Caption:='Configure "Build Lazarus"';
    OnResize:=@ConfigureBuildLazarusDlgResize;
    OnKeyDown:=@ConfigureBuildLazarusDlgKeyDown;
    
    CleanAllCheckBox:=TCheckBox.Create(Self);
    with CleanAllCheckBox do begin
      Parent:=Self;
      Name:='CleanAllCheckBox';
      SetBounds(10,10,Self.ClientWidth-20,20);
      Caption:='Clean all';
      Visible:=true;
    end;
    
    BuildAllButton:=TButton.Create(Self);
    with BuildAllButton do begin
      Name:='BuildAllButton';
      Parent:=Self;
      Left:=CleanAllCheckBox.Left;
      Top:=CleanAllCheckBox.Top+CleanAllCheckBox.Height+5;
      Width:=200;
      Caption:='Set to "Build All"';
      OnClick:=@BuildAllButtonClick;
      Visible:=true;
    end;
    
    BuildLCLRadioGroup:=TRadioGroup.Create(Self);
    with BuildLCLRadioGroup do begin
      Parent:=Self;
      Name:='BuildLCLRadioGroup';
      SetBounds(10,BuildAllButton.Top+BuildAllButton.Height+5,
                CleanAllCheckBox.Width,40);
      Caption:='Build LCL';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;

    BuildComponentsRadioGroup:=TRadioGroup.Create(Self);
    with BuildComponentsRadioGroup do begin
      Parent:=Self;
      Name:='BuildComponentsRadioGroup';
      SetBounds(10,BuildLCLRadioGroup.Top+BuildLCLRadioGroup.Height+5,
                BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
      Caption:='Build Components (SynEdit, CodeTools)';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;

    BuildSynEditRadioGroup:=TRadioGroup.Create(Self);
    with BuildSynEditRadioGroup do begin
      Parent:=Self;
      Name:='BuildSynEditRadioGroup';
      SetBounds(10,
                BuildComponentsRadioGroup.Top+BuildComponentsRadioGroup.Height+5,
                BuildComponentsRadioGroup.Width,
                BuildLCLRadioGroup.Height);
      Caption:='Build SynEdit';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;

    BuildCodeToolsRadioGroup:=TRadioGroup.Create(Self);
    with BuildCodeToolsRadioGroup do begin
      Parent:=Self;
      Name:='BuildCodeToolsRadioGroup';
      SetBounds(10,BuildSynEditRadioGroup.Top+BuildSynEditRadioGroup.Height+5,
                BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
      Caption:='Build CodeTools';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;

    BuildIDERadioGroup:=TRadioGroup.Create(Self);
    with BuildIDERadioGroup do begin
      Parent:=Self;
      Name:='BuildIDERadioGroup';
      SetBounds(10,BuildCodeToolsRadioGroup.Top+BuildCodeToolsRadioGroup.Height+5,
                BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
      Caption:='Build IDE';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;

    BuildExamplesRadioGroup:=TRadioGroup.Create(Self);
    with BuildExamplesRadioGroup do begin
      Parent:=Self;
      Name:='BuildExamplesRadioGroup';
      SetBounds(10,BuildIDERadioGroup.Top+BuildIDERadioGroup.Height+5,
                BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
      Caption:='Build Examples';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;
    
    OptionsLabel:=TLabel.Create(Self);
    with OptionsLabel do begin
      Name:='OptionsLabel';
      Parent:=Self;
      SetBounds(10,
                BuildExamplesRadioGroup.Top+BuildExamplesRadioGroup.Height+5,
                80,Height);
      Caption:='Options:';
      Visible:=true;
    end;
    
    OptionsEdit:=TEdit.Create(Self);
    with OptionsEdit do begin
      Name:='OptionsEdit';
      Parent:=Self;
      SetBounds(OptionsLabel.Left+OptionsLabel.Width+5,
                OptionsLabel.Top,
                BuildExamplesRadioGroup.Width-OptionsLabel.Width-5,
                Height);
      Visible:=true;
    end;

    TargetOSLabel:=TLabel.Create(Self);
    with TargetOSLabel do begin
      Name:='TargetOSLabel';
      Parent:=Self;
      SetBounds(10,
                OptionsLabel.Top+OptionsLabel.Height+12,
                80,Height);
      Caption:='Target OS:';
      Visible:=true;
    end;

    TargetOSEdit:=TEdit.Create(Self);
    with TargetOSEdit do begin
      Name:='TargetOSEdit';
      Parent:=Self;
      SetBounds(TargetOSLabel.Left+TargetOSLabel.Width+5,
                TargetOSLabel.Top,
                OptionsEdit.Width,
                Height);
      Visible:=true;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Parent:=Self;
      Name:='OkButton';
      SetBounds(Self.ClientWidth-180,Self.ClientHeight-38,80,25);
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Parent:=Self;
      Name:='CancelButton';
      SetBounds(Self.ClientWidth-90,OkButton.Top,OkButton.Width,OkButton.Height);
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;

  end;
  ConfigureBuildLazarusDlgResize(nil);
end;

procedure TConfigureBuildLazarusDlg.BuildAllButtonClick(Sender: TObject);
begin
  CleanAllCheckBox.Checked:=true;
  BuildLCLRadioGroup.ItemIndex:=1;
  BuildComponentsRadioGroup.ItemIndex:=1;
  BuildSynEditRadioGroup.ItemIndex:=0;
  BuildCodeToolsRadioGroup.ItemIndex:=0;
  BuildIDERadioGroup.ItemIndex:=1;
  BuildExamplesRadioGroup.ItemIndex:=1;
  OptionsEdit.Text:='';
end;

procedure TConfigureBuildLazarusDlg.ConfigureBuildLazarusDlgKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_Escape then
    ModalResult:=mrCancel;
end;

procedure TConfigureBuildLazarusDlg.ConfigureBuildLazarusDlgResize(
  Sender: TObject);
begin
  CleanAllCheckBox.SetBounds(10,10,Self.ClientWidth-24,20);
  BuildAllButton.SetBounds(CleanAllCheckBox.Left,
                           CleanAllCheckBox.Top+CleanAllCheckBox.Height+5,
                           200,BuildAllButton.Height);
  BuildLCLRadioGroup.SetBounds(10,
              BuildAllButton.Top+BuildAllButton.Height+5,
              CleanAllCheckBox.Width,40);
  BuildComponentsRadioGroup.SetBounds(10,
              BuildLCLRadioGroup.Top+BuildLCLRadioGroup.Height+5,
              BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
  BuildSynEditRadioGroup.SetBounds(10,
              BuildComponentsRadioGroup.Top+BuildComponentsRadioGroup.Height+5,
              BuildComponentsRadioGroup.Width,BuildComponentsRadioGroup.Height);
  BuildCodeToolsRadioGroup.SetBounds(10,
              BuildSynEditRadioGroup.Top+BuildSynEditRadioGroup.Height+5,
              BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
  BuildIDERadioGroup.SetBounds(10,
              BuildCodeToolsRadioGroup.Top+BuildCodeToolsRadioGroup.Height+5,
              BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
  BuildExamplesRadioGroup.SetBounds(10,
              BuildIDERadioGroup.Top+BuildIDERadioGroup.Height+5,
              BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
  OptionsLabel.SetBounds(10,
            BuildExamplesRadioGroup.Top+BuildExamplesRadioGroup.Height+5,
            80,OptionsLabel.Height);
  OptionsEdit.SetBounds(OptionsLabel.Left+OptionsLabel.Width+5,
            OptionsLabel.Top,
            BuildExamplesRadioGroup.Width-OptionsLabel.Width-5,
            OptionsEdit.Height);
  TargetOSLabel.SetBounds(10,
                OptionsLabel.Top+OptionsLabel.Height+12,
                80,TargetOsLabel.Height);
  TargetOSEdit.SetBounds(TargetOSLabel.Left+TargetOSLabel.Width+5,
                TargetOSLabel.Top,
                OptionsEdit.Width,
                TargetOSEdit.Height);
  OkButton.SetBounds(Self.ClientWidth-180,Self.ClientHeight-38,80,25);
  CancelButton.SetBounds(Self.ClientWidth-90,OkButton.Top,
              OkButton.Width,OkButton.Height);
end;

procedure TConfigureBuildLazarusDlg.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TConfigureBuildLazarusDlg.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TConfigureBuildLazarusDlg.Load(Options: TBuildLazarusOptions);
begin
  CleanAllCheckBox.Checked:=Options.CleanAll;
  BuildLCLRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildLCL);
  BuildComponentsRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildComponents);
  BuildSynEditRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildSynEdit);
  BuildCodeToolsRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildCodeTools);
  BuildIDERadioGroup.ItemIndex:=MakeModeToInt(Options.BuildIDE);
  BuildExamplesRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildExamples);
  OptionsEdit.Text:=Options.ExtraOptions;
  TargetOSEdit.Text:=Options.TargetOS;
end;

procedure TConfigureBuildLazarusDlg.Save(Options: TBuildLazarusOptions);
begin
  if Options=nil then exit;
  Options.CleanAll:=CleanAllCheckBox.Checked;
  Options.BuildLCL:=IntToMakeMode(BuildLCLRadioGroup.ItemIndex);
  Options.BuildComponents:=IntToMakeMode(BuildComponentsRadioGroup.ItemIndex);
  Options.BuildSynEdit:=IntToMakeMode(BuildSynEditRadioGroup.ItemIndex);
  Options.BuildCodeTools:=IntToMakeMode(BuildCodeToolsRadioGroup.ItemIndex);
  Options.BuildIDE:=IntToMakeMode(BuildIDERadioGroup.ItemIndex);
  Options.BuildExamples:=IntToMakeMode(BuildExamplesRadioGroup.ItemIndex);
  Options.ExtraOptions:=OptionsEdit.Text;
  Options.TargetOS:=TargetOSEdit.Text;
end;

function TConfigureBuildLazarusDlg.MakeModeToInt(MakeMode: TMakeMode): integer;
begin
  case MakeMode of
    mmBuild:      Result:=1;
    mmCleanBuild: Result:=2;
  else            Result:=0;
  end;
end;

function TConfigureBuildLazarusDlg.IntToMakeMode(i: integer): TMakeMode;
begin
  case i of
    1: Result:=mmBuild;
    2: Result:=mmCleanBuild;
  else Result:=mmNone;
  end;
end;

{ TBuildLazarusOptions }

procedure TBuildLazarusOptions.Save(XMLConfig: TXMLConfig; const Path: string);
begin
  XMLConfig.SetValue(Path+'BuildLCL/Value',MakeModeNames[fBuildLCL]);
  XMLConfig.SetValue(Path+'BuildComponents/Value',MakeModeNames[fBuildComponents]);
  XMLConfig.SetValue(Path+'BuildSynEdit/Value',MakeModeNames[fBuildSynEdit]);
  XMLConfig.SetValue(Path+'BuildCodeTools/Value',MakeModeNames[fBuildCodeTools]);
  XMLConfig.SetValue(Path+'BuildIDE/Value',MakeModeNames[fBuildIDE]);
  XMLConfig.SetValue(Path+'BuildExamples/Value',MakeModeNames[fBuildExamples]);
  XMLConfig.SetValue(Path+'CleanAll/Value',fCleanAll);
  XMLConfig.SetValue(Path+'ExtraOptions/Value',fExtraOptions);
  XMLConfig.SetValue(Path+'TargetOS/Value',fTargetOS);
  XMLConfig.SetValue(Path+'MakeFilename/Value',fMakeFilename);
end;

procedure TBuildLazarusOptions.Load(XMLConfig: TXMLConfig; const Path: string);
begin
  fBuildLCL:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildLCL/Value',
                                              MakeModeNames[fBuildLCL]));
  fBuildComponents:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildComponents/Value',
                                              MakeModeNames[fBuildComponents]));
  fBuildSynEdit:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildSynEdit/Value',
                                    MakeModeNames[fBuildSynEdit]));
  fBuildCodeTools:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildCodeTools/Value',
                                      MakeModeNames[fBuildCodeTools]));
  fBuildIDE:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildIDE/Value',
                                              MakeModeNames[fBuildIDE]));
  fBuildExamples:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildExamples/Value',
                                                MakeModeNames[fBuildExamples]));
  fCleanAll:=XMLConfig.GetValue(Path+'CleanAll/Value',fCleanAll);
  fExtraOptions:=XMLConfig.GetValue(Path+'ExtraOptions/Value',fExtraOptions);
  fTargetOS:=XMLConfig.GetValue(Path+'TargetOS/Value','');
  fMakeFilename:=XMLConfig.GetValue(Path+'MakeFilename/Value',fMakeFilename);
end;

constructor TBuildLazarusOptions.Create;
begin
  inherited Create;
  fMakeFilename:='';
end;


end.


