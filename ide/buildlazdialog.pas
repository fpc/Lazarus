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
  XMLCfg, ExtToolDialog, ExtToolEditDlg, TransferMacros;

type
  TMakeMode = (mmNone, mmBuild, mmCleanBuild);

  TBuildLazarusOptions = class
  private
    fBuildLCL: TMakeMode;
    fBuildSynEdit: TMakeMode;
    fBuildCodeTools: TMakeMode;
    fBuildIDE: TMakeMode;
    fBuildExamples: TMakeMode;
    fCleanAll: boolean;
    fMakeFilename: string;
  public
    constructor Create;
    procedure Load(XMLConfig: TXMLConfig; const Path: string);
    procedure Save(XMLConfig: TXMLConfig; const Path: string);
    property BuildLCL: TMakeMode read fBuildLCL write fBuildLCL;
    property BuildSynEdit: TMakeMode read fBuildSynEdit write fBuildSynEdit;
    property BuildCodeTools: TMakeMode read fBuildCodeTools write fBuildCodeTools;
    property BuildIDE: TMakeMode read fBuildIDE write fBuildIDE;
    property BuildExamples: TMakeMode read fBuildExamples write fBuildExamples;
    property CleanAll: boolean read fCleanAll write fCleanAll;
    property MakeFilename: string read fMakeFilename write fMakeFilename;
  end;

  TConfigureBuildLazarusDlg = class(TForm)
    CleanAllCheckBox: TCheckBox;
    BuildLCLRadioGroup: TRadioGroup;
    BuildSynEditRadioGroup: TRadioGroup;
    BuildCodeToolsRadioGroup: TRadioGroup;
    BuildIDERadioGroup: TRadioGroup;
    BuildExamplesRadioGroup: TRadioGroup;
    OkButton: TButton;
    CancelButton: TButton;
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
begin
  Result:=mrCancel;
  Tool:=TExternalToolOptions.Create;
  try
    Tool.Filename:=Options.MakeFilename;
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
      if Options.BuildLCL=mmBuild then
        Tool.CmdLineParams:=''
      else
        Tool.CmdLineParams:='clean all';
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildSynEdit<>mmNone then begin
      // build SynEdit
      Tool.Title:='Build SynEdit';
      Tool.WorkingDirectory:='$(LazarusDir)/components/synedit';
      if Options.BuildSynEdit=mmBuild then
        Tool.CmdLineParams:=''
      else
        Tool.CmdLineParams:='clean all';
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildCodeTools<>mmNone then begin
      // build CodeTools
      Tool.Title:='Build CodeTools';
      Tool.WorkingDirectory:='$(LazarusDir)/components/codetools';
      if Options.BuildCodeTools=mmBuild then
        Tool.CmdLineParams:=''
      else
        Tool.CmdLineParams:='clean all';
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildIDE<>mmNone then begin
      // build IDE
      Tool.Title:='Build IDE';
      Tool.WorkingDirectory:='$(LazarusDir)';
      if Options.BuildIDE=mmBuild then
        Tool.CmdLineParams:='ide'
      else
        // ToDo: the Makefile needs a 'cleanide'
        Tool.CmdLineParams:='clean all';
      Result:=ExternalTools.Run(Tool,Macros);
      if Result<>mrOk then exit;
    end;
    if Options.BuildExamples<>mmNone then begin
      // build Examples
      Tool.Title:='Build Examples';
      Tool.WorkingDirectory:='$(LazarusDir)/examples';
      if Options.BuildExamples=mmBuild then
        Tool.CmdLineParams:=''
      else
        Tool.CmdLineParams:='clean all';
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
    SetBounds((Screen.Width-350) div 2,(Screen.Height-320) div 2,350,320);
    Caption:='Configure "Build Lazarus"';
    
    CleanAllCheckBox:=TCheckBox.Create(Self);
    with CleanAllCheckBox do begin
      Parent:=Self;
      Name:='CleanAllCheckBox';
      SetBounds(10,10,Self.ClientWidth-24,20);
      Caption:='Clean all';
      Visible:=true;
    end;
    
    BuildLCLRadioGroup:=TRadioGroup.Create(Self);
    with BuildLCLRadioGroup do begin
      Parent:=Self;
      Name:='BuildLCLRadioGroup';
      SetBounds(10,CleanAllCheckBox.Top+CleanAllCheckBox.Height+5,
                CleanAllCheckBox.Width,40);
      Caption:='Build LCL';
      for MakeMode:=Low(TMakeMode) to High(TMakeMode) do
        Items.Add(MakeModeNames[MakeMode]);
      Columns:=3;
      Visible:=true;
    end;

    BuildSynEditRadioGroup:=TRadioGroup.Create(Self);
    with BuildSynEditRadioGroup do begin
      Parent:=Self;
      Name:='BuildSynEditRadioGroup';
      SetBounds(10,BuildLCLRadioGroup.Top+BuildLCLRadioGroup.Height+5,
                BuildLCLRadioGroup.Width,BuildLCLRadioGroup.Height);
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
  BuildSynEditRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildSynEdit);
  BuildCodeToolsRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildCodeTools);
  BuildIDERadioGroup.ItemIndex:=MakeModeToInt(Options.BuildIDE);
  BuildExamplesRadioGroup.ItemIndex:=MakeModeToInt(Options.BuildExamples);
end;

procedure TConfigureBuildLazarusDlg.Save(Options: TBuildLazarusOptions);
begin
  if Options=nil then exit;
  Options.CleanAll:=CleanAllCheckBox.Checked;
  Options.BuildLCL:=IntToMakeMode(BuildLCLRadioGroup.ItemIndex);
  Options.BuildSynEdit:=IntToMakeMode(BuildSynEditRadioGroup.ItemIndex);
  Options.BuildCodeTools:=IntToMakeMode(BuildCodeToolsRadioGroup.ItemIndex);
  Options.BuildIDE:=IntToMakeMode(BuildIDERadioGroup.ItemIndex);
  Options.BuildExamples:=IntToMakeMode(BuildExamplesRadioGroup.ItemIndex);
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
  XMLConfig.SetValue(Path+'BuildSynEdit/Value',MakeModeNames[fBuildSynEdit]);
  XMLConfig.SetValue(Path+'BuildCodeTools/Value',MakeModeNames[fBuildCodeTools]);
  XMLConfig.SetValue(Path+'BuildIDE/Value',MakeModeNames[fBuildIDE]);
  XMLConfig.SetValue(Path+'BuildExamples/Value',MakeModeNames[fBuildExamples]);
  XMLConfig.SetValue(Path+'CleanAll/Value',fCleanAll);
  XMLConfig.SetValue(Path+'MakeFilename/Value',fMakeFilename);
end;

procedure TBuildLazarusOptions.Load(XMLConfig: TXMLConfig; const Path: string);
begin
  fBuildLCL:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildLCL/Value',
                                              MakeModeNames[fBuildLCL]));
  fBuildSynEdit:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildSynEdit/Value',
                                    MakeModeNames[fBuildSynEdit]));
  fBuildCodeTools:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildCodeTools/Value',
                                      MakeModeNames[fBuildCodeTools]));
  fBuildIDE:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildIDE/Value',
                                              MakeModeNames[fBuildIDE]));
  fBuildExamples:=StrToMakeMode(XMLConfig.GetValue(Path+'BuildExamples/Value',
                                                MakeModeNames[fBuildExamples]));
  fCleanAll:=XMLConfig.GetValue(Path+'CleanAll/Value',fCleanAll);
  fMakeFilename:=XMLConfig.GetValue(Path+'MakeFilename/Value',fMakeFilename);
end;

constructor TBuildLazarusOptions.Create;
begin
  inherited Create;
  fMakeFilename:='/usr/bin/make';
end;


end.


