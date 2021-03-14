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
    Frame for environment options for things happening during startup.
    - Single Lazarus IDE instance / multiple instances.
    - The project that gets opened or created.
}
unit IdeStartup_Options;

{$mode ObjFPC}{$H+}

interface

uses
  // LCL
  StdCtrls, Controls, Dialogs,
  // LazControls
  DividerBevel,
  // LazUtils
  LazFileUtils, LazLoggerBase,
  // CodeTools
  CodeToolManager, DefineTemplates,
  // BuildIntf
  ProjectIntf, IDEOptionsIntf,
  // IdeIntf
  IDEOptEditorIntf,
  // IDE
  EnvironmentOpts, LazarusIDEStrConsts;

type

  { TIdeStartupFrame }

  TIdeStartupFrame = class(TAbstractIDEOptionsEditor)
    ProjectTypeLabel: TLabel;
    ProjectTypeCB: TComboBox;
    lblFileAssociation: TDividerBevel;
    lblProjectToOpen: TDividerBevel;
    LazarusInstancesCB: TComboBox;
    LazarusInstancesLabel: TLabel;
    OpenLastProjectAtStartCheckBox: TCheckBox;
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
  ProjectTypeCB.Enabled := not (Sender as TCheckBox).Checked;
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
  pd: TProjectDescriptor;
begin
  // Using File Association in OS
  lblFileAssociation.Caption := dlgFileAssociationInOS;
  LazarusInstancesLabel.Caption := dlgLazarusInstances;
  with LazarusInstancesCB.Items do
  begin
    BeginUpdate;
    Add(dlgMultipleInstances_AlwaysStartNew);
    Add(dlgMultipleInstances_OpenFilesInRunning);
    Add(dlgMultipleInstances_ForceSingleInstance);
    EndUpdate;
  end;
  Assert(LazarusInstancesCB.Items.Count = Ord(High(TIDEMultipleInstancesOption))+1);
  // Project to Open or Create
  lblProjectToOpen.Caption := dlgProjectToOpenOrCreate;
  OpenLastProjectAtStartCheckBox.Caption := dlgQOpenLastPrj;
  ProjectTypeLabel.Caption := dlgNewProjectType;
  for i:=0 to ProjectDescriptors.Count-1 do
  begin
    pd := ProjectDescriptors[i];
    if pd.VisibleInNewDialog then
      ProjectTypeCB.Items.AddObject(pd.GetLocalizedName, pd);
  end;
end;

procedure TIdeStartupFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
  pd: TProjectDescriptor;
begin
  with AOptions as TEnvironmentOptions do
  begin
    LazarusInstancesCB.ItemIndex := Ord(MultipleInstances);

    FOldOpenLastProjectAtStart := OpenLastProjectAtStart;
    OpenLastProjectAtStartCheckBox.Checked:=OpenLastProjectAtStart;

    FOldProjectTemplateAtStart := NewProjectTemplateAtStart;
    for i := 0 to ProjectTypeCB.Items.Count-1 do
    begin
      pd := TProjectDescriptor(ProjectTypeCB.Items.Objects[i]);
      if pd.Name = FOldProjectTemplateAtStart then
      begin
        ProjectTypeCB.ItemIndex := i;
        Exit;
      end;
    end;
    ProjectTypeCB.ItemIndex := 0;
  end;
end;

procedure TIdeStartupFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  pd: TProjectDescriptor;
begin
  with AOptions as TEnvironmentOptions do
  begin
    MultipleInstances := TIDEMultipleInstancesOption(LazarusInstancesCB.ItemIndex);

    OpenLastProjectAtStart := OpenLastProjectAtStartCheckBox.Checked;
    // Don't use the localized name from ProjectTypeCB.Text.
    pd := TProjectDescriptor(ProjectTypeCB.Items.Objects[ProjectTypeCB.ItemIndex]);
    NewProjectTemplateAtStart := pd.Name;
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

