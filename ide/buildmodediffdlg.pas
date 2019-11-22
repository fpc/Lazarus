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

 Abstract:
   Modal dialog to show the differences between build modes.
}
unit BuildModeDiffDlg;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  Classes, sysutils,
  // LCL
  StdCtrls, ComCtrls, Forms, ButtonPanel,
  // LazUtils
  LazUTF8, LazLoggerBase, AvgLvlTree,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts, Project, ModeMatrixOpts, CompOptsModes;

type

  { TBuildModeDiffDialog }

  TBuildModeDiffDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    DiffLabel: TLabel;
    DiffTreeView: TTreeView;
    ModeComboBox: TComboBox;
    ModeLabel: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ModeComboBoxChange(Sender: TObject);
  private
    FBaseMode: TProjectBuildMode;
    FBuildModes: TProjectBuildModes;
    procedure FillModeComboBox;
    procedure FillDiffTreeView;
  public
    procedure SetBuildMode(aMode: TProjectBuildMode);
    property BuildModes: TProjectBuildModes read FBuildModes write FBuildModes;
    property BaseMode: TProjectBuildMode read FBaseMode;
  end;

function ShowBuildModeDiffDialog(BuildModes: TProjectBuildModes; aMode: TProjectBuildMode): TModalResult;

implementation

function ShowBuildModeDiffDialog(BuildModes: TProjectBuildModes;
  aMode: TProjectBuildMode): TModalResult;
var
  BuildModeDiffDialog: TBuildModeDiffDialog;
begin
  BuildModeDiffDialog:=TBuildModeDiffDialog.Create(nil);
  try
    BuildModeDiffDialog.BuildModes:=BuildModes;
    BuildModeDiffDialog.SetBuildMode(aMode);
    Result:=BuildModeDiffDialog.ShowModal;
  finally
    BuildModeDiffDialog.Free;
  end;
end;

{ TBuildModeDiffDialog }

procedure TBuildModeDiffDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisBuildModeDiffDifferencesBetweenBuildModes;

  ModeLabel.Caption:=lisBuildModeDiffMode;
  DiffLabel.Caption:=lisBuildModeDiffDifferencesToOtherBuildModes;
  ButtonPanel.CloseButton.Caption:=lisBtnClose;
end;

procedure TBuildModeDiffDialog.ModeComboBoxChange(Sender: TObject);
var
  i: Integer;
begin
  if BuildModes=nil then exit;
  for i:=0 to BuildModes.Count-1 do
    if UTF8CompareText(BuildModes[i].GetCaption,ModeComboBox.Text)=0
    then begin
      fBaseMode:=BuildModes[i];
      FillDiffTreeView;
      break;
    end;
end;

procedure TBuildModeDiffDialog.FillModeComboBox;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    if BuildModes<>nil then
      for i:=0 to BuildModes.Count-1 do
        sl.Add(BuildModes[i].GetCaption);
    ModeComboBox.Items.Assign(sl);
    if BaseMode<>nil then begin
      ModeComboBox.Text:=BaseMode.GetCaption;
      ModeComboBox.ItemIndex:=sl.IndexOf(BaseMode.GetCaption);
    end
    else
      ModeComboBox.Text:='(none)';
  finally
    sl.Free;
  end;
end;

procedure TBuildModeDiffDialog.FormActivate(Sender: TObject);
begin
  ModeComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TBuildModeDiffDialog.FillDiffTreeView;

  procedure DiffsForMatrixCustomOptions(MatrixOptions: TBuildMatrixOptions;
    OldMode, NewMode: string; Diff: TStringList);
  var
    i: Integer;
    Option: TBuildMatrixOption;
    HasOldMode: Boolean;
    HasNewMode: Boolean;
    s: String;
  begin
    for i:=0 to MatrixOptions.Count-1 do begin
      Option:=MatrixOptions[i];
      if Option.Typ<>bmotCustom then continue;
      HasOldMode:=Option.FitsMode(OldMode);
      HasNewMode:=Option.FitsMode(NewMode);
      if HasOldMode=HasNewMode then continue;
      if HasNewMode then
        s:=lisMMAddsCustomOptions
      else
        s:=lisMMDoesNotAddCustomOptions;
      s+=' '+dbgstr(Option.Value);
      //debugln(['AddDiff OldMode="',OldMode,'" NewMode="',NewMode,'" Option="',Option.AsString,'" Diff="',s,'"']);
      Diff.Add(s);
    end;
  end;

  procedure DiffsForMatrixOutputDirectory(MatrixOptions: TBuildMatrixOptions;
    OldMode, NewMode: string; var OldOutputDir, NewOutputDir: string);
  begin
    MatrixOptions.GetOutputDirectory(BuildMatrixProjectName,OldMode,OldOutputDir);
    MatrixOptions.GetOutputDirectory(BuildMatrixProjectName,NewMode,NewOutputDir);
  end;

  procedure GetIDEMacros(MatrixOptions: TBuildMatrixOptions;
    OldMode, NewMode: string; OldMacroValues, NewMacroValues: TStringToStringTree);
  var
    i: Integer;
    Option: TBuildMatrixOption;
  begin
    for i:=0 to MatrixOptions.Count-1 do begin
      Option:=MatrixOptions[i];
      if Option.Typ<>bmotIDEMacro then continue;
      if Option.FitsMode(OldMode) then
        OldMacroValues.Values[Option.MacroName]:=Option.Value;
      if Option.FitsMode(NewMode) then
        NewMacroValues.Values[Option.MacroName]:=Option.Value;
    end;
  end;

var
  i: Integer;
  CurMode: TProjectBuildMode;
  ModeNode: TTreeNode;
  Diff: TStringList;
  DiffTool: TCompilerDiffTool;
  j: Integer;
  OldOutDir: String;
  NewOutDir: String;
  OldMode: String;
  NewMode: String;
  OldMacroValues: TStringToStringTree;
  NewMacroValues: TStringToStringTree;
  OldValue: String;
  S2SItem: PStringToStringItem;
  s: String;
begin
  DiffTreeView.BeginUpdate;
  DiffTreeView.Items.Clear;
  if BuildModes<>nil then
  begin
    for i := 0 to BuildModes.Count - 1 do
    begin
      CurMode:=BuildModes[i];
      if CurMode=BaseMode then continue;

      // add differences from each CurMode to BaseMode
      ModeNode:=DiffTreeView.Items.Add(nil, Format(lisMMFromTo, [CurMode.
        GetCaption, BaseMode.GetCaption]));
      Diff:=TStringList.Create;
      DiffTool:=TCompilerDiffTool.Create(Diff);
      BaseMode.CreateDiff(CurMode,DiffTool);

      NewMode:=BaseMode.Identifier;
      OldMode:=CurMode.Identifier;

      // add diffs for matrix custom options
      DiffsForMatrixCustomOptions(EnvironmentOptions.BuildMatrixOptions,
        OldMode,NewMode,Diff);
      DiffsForMatrixCustomOptions(BuildModes.SharedMatrixOptions,
        OldMode,NewMode,Diff);
      DiffsForMatrixCustomOptions(BuildModes.SessionMatrixOptions,
        OldMode,NewMode,Diff);

      // add diffs for matrix IDE macros
      OldMacroValues:=TStringToStringTree.Create(false);
      NewMacroValues:=TStringToStringTree.Create(false);
      GetIDEMacros(EnvironmentOptions.BuildMatrixOptions,OldMode,NewMode,
        OldMacroValues,NewMacroValues);
      GetIDEMacros(BuildModes.SharedMatrixOptions,OldMode,NewMode,
        OldMacroValues,NewMacroValues);
      GetIDEMacros(BuildModes.SessionMatrixOptions,OldMode,NewMode,
        OldMacroValues,NewMacroValues);
      for S2SItem in NewMacroValues do begin
        OldValue:=OldMacroValues.Values[S2SItem^.Name];
        if OldValue=S2SItem^.Value then continue;
        s:=Format(lisMMIDEMacro2, [S2SItem^.Name, S2SItem^.Value]);
        if OldValue<>'' then
          s+=' '+Format(lisMMWas, [OldValue]);
        Diff.Add(s);
      end;
      for S2SItem in OldMacroValues do begin
        if NewMacroValues.Contains(S2SItem^.Name) then continue;
        s:=Format(lisMMDoesNotHaveIDEMacro, [S2SItem^.Name, S2SItem^.Value]);
        Diff.Add(s);
      end;
      OldMacroValues.Free;
      NewMacroValues.Free;

      // add diffs for matrix output directory overrides
      OldOutDir:='';
      NewOutDir:='';
      DiffsForMatrixOutputDirectory(EnvironmentOptions.BuildMatrixOptions,
        OldMode,NewMode,OldOutDir,NewOutDir);
      DiffsForMatrixOutputDirectory(BuildModes.SharedMatrixOptions,
        OldMode,NewMode,OldOutDir,NewOutDir);
      DiffsForMatrixOutputDirectory(BuildModes.SessionMatrixOptions,
        OldMode,NewMode,OldOutDir,NewOutDir);
      if OldOutDir<>NewOutDir then begin
        if NewOutDir='' then
          s:=lisMMDoesNotOverrideOutDirFU
        else
          s:=Format(lisMMOverrideOutDirFU, [NewOutDir]);
        Diff.Add(s);
      end;

      // create trre nodes
      for j:=0 to Diff.Count-1 do
        DiffTreeView.Items.AddChild(ModeNode,Diff[j]);
      DiffTool.Free;
      Diff.Free;
      ModeNode.Expand(true);
    end;
  end;
  DiffTreeView.EndUpdate;
end;

procedure TBuildModeDiffDialog.SetBuildMode(aMode: TProjectBuildMode);
begin
  if aMode<>nil then
  begin
    if aMode.LazProject<>nil then
      BuildModes:=aMode.LazProject.BuildModes;
    FBaseMode:=aMode;
  end else
  begin
    FBaseMode:=nil;
  end;
  FillModeComboBox;
  FillDiffTreeView;
end;

{$R *.lfm}

end.

