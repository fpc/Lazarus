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

  Abtract:
    Frame for environment options for new projects.
}
unit IdeStartup_Options;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  // LCL
  StdCtrls, Dialogs, Controls, Spin, CheckLst, ExtCtrls,
  // LazUtils
  FileUtil, LazFileUtils, LazLoggerBase,
  // IDE
  EnvironmentOpts,
  // CodeTools
  CodeToolManager, DefineTemplates,
  // BuildIntf
  ProjectIntf,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEDialogs, IDEUtils, DividerBevel,
  // IDE
  LazarusIDEStrConsts, InputHistory, LazConf, DialogProcs, InitialSetupProc, Classes;

type

  { TIdeStartupFrame }

  TIdeStartupFrame = class(TAbstractIDEOptionsEditor)
    lblFileAssociation: TDividerBevel;
    lblProjectToOpen: TDividerBevel;
    MultipleInstancesComboBox: TComboBox;
    LazarusInstancesLabel: TLabel;
    OpenLastProjectAtStartCheckBox: TCheckBox;
    ProjectTypeRG: TRadioGroup;
    procedure OpenLastProjectAtStartCheckBoxChange(Sender: TObject);
  private
    FOldOpenLastProjectAtStart: boolean;
    FOldProjectTemplateAtStart: string;
  public
    //function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TIdeStartupFrame }

procedure TIdeStartupFrame.OpenLastProjectAtStartCheckBoxChange(Sender: TObject);
begin
  ProjectTypeRG.Enabled := not (Sender as TCheckBox).Checked;
end;
{
function TIdeStartupFrame.Check: Boolean;
begin
  Result:=inherited Check;
end;
}
function TIdeStartupFrame.GetTitle: String;
begin
  Result := dlgEnvIdeStartup;
end;

procedure TIdeStartupFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  i: Integer;
begin
  // Using File Association in OS
  lblFileAssociation.Caption := dlgFileAssociationInOS;
  LazarusInstancesLabel.Caption := dlgLazarusInstances;
  with MultipleInstancesComboBox.Items do
  begin
    BeginUpdate;
    Add(dlgMultipleInstances_AlwaysStartNew);
    Add(dlgMultipleInstances_OpenFilesInRunning);
    Add(dlgMultipleInstances_ForceSingleInstance);
    EndUpdate;
  end;
  Assert(MultipleInstancesComboBox.Items.Count = Ord(High(TIDEMultipleInstancesOption))+1);
  // Project to Open or Create
  lblProjectToOpen.Caption := dlgProjectToOpenOrCreate;
  OpenLastProjectAtStartCheckBox.Caption := dlgQOpenLastPrj;
  ProjectTypeRG.Caption := dlgNewProjectType;
  for i:=0 to ProjectDescriptors.Count-1 do
    if ProjectDescriptors[i].VisibleInNewDialog then
      ProjectTypeRG.Items.Add(ProjectDescriptors[i].Name);  // GetLocalizedName
end;

procedure TIdeStartupFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  with AOptions as TEnvironmentOptions do
  begin
    MultipleInstancesComboBox.ItemIndex := Ord(MultipleInstances);

    FOldOpenLastProjectAtStart := OpenLastProjectAtStart;
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;

    FOldProjectTemplateAtStart := NewProjectTemplateAtStart;
    for i := 0 to ProjectTypeRG.Items.Count-1 do
      if ProjectTypeRG.Items[i] = FOldProjectTemplateAtStart then
      begin
        ProjectTypeRG.ItemIndex := i;
        Exit;
      end;
    ProjectTypeRG.ItemIndex := 0;
  end;
end;

procedure TIdeStartupFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    MultipleInstances := TIDEMultipleInstancesOption(MultipleInstancesComboBox.ItemIndex);

    OpenLastProjectAtStart := OpenLastProjectAtStartCheckBox.Checked;
    NewProjectTemplateAtStart := ProjectTypeRG.Items[ProjectTypeRG.ItemIndex];
  end;
end;

procedure TIdeStartupFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
  inherited RestoreSettings(AOptions);
  with AOptions as TEnvironmentOptions do
  begin
    OpenLastProjectAtStart := FOldOpenLastProjectAtStart;
    NewProjectTemplateAtStart := FOldProjectTemplateAtStart;
  end;
end;

class function TIdeStartupFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TIdeStartupFrame, EnvOptionsIdeStartup);

end.

