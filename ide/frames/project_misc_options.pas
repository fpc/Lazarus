unit project_misc_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ProjectIntf, Project, IDEOptionsIntf, LazarusIDEStrConsts;

type

  { TProjectMiscOptionsFrame }

  TProjectMiscOptionsFrame = class(TAbstractIDEOptionsEditor)
    AlwaysBuildCheckBox: TCheckBox;
    Bevel1: TBevel;
    LRSInOutputDirCheckBox: TCheckBox;
    MainUnitHasCreateFormStatementsCheckBox: TCheckBox;
    MainUnitHasTitleStatementCheckBox: TCheckBox;
    MainUnitHasUsesSectionForAllUnitsCheckBox: TCheckBox;
    MainUnitIsPascalSourceCheckBox: TCheckBox;
    ResourceGroupBox: TGroupBox;
    RunnableCheckBox: TCheckBox;
    UseFPCResourcesRadioButton: TRadioButton;
    UseLRSFilesRadioButton: TRadioButton;
  private
    { private declarations }
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectMiscOptionsFrame }

function TProjectMiscOptionsFrame.GetTitle: string;
begin
  Result := dlgPOMisc;
end;

procedure TProjectMiscOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  MainUnitIsPascalSourceCheckBox.Caption := lisMainUnitIsPascalSource;
  MainUnitHasUsesSectionForAllUnitsCheckBox.Caption := lisMainUnitHasUsesSectionContainingAllUnitsOfProject;
  MainUnitHasCreateFormStatementsCheckBox.Caption := lisMainUnitHasApplicationCreateFormStatements;
  MainUnitHasTitleStatementCheckBox.Caption := lisMainUnitHasApplicationTitleStatements;
  RunnableCheckBox.Caption := lisProjectIsRunnable;
  AlwaysBuildCheckBox.Caption := lisProjOptsAlwaysBuildEvenIfNothingChanged;
  LRSInOutputDirCheckBox.Caption := lisPutLrsFilesInOutputDirectory;
  ResourceGroupBox.Caption := lisResourceTypeOfNewFiles;
  UseLRSFilesRadioButton.Caption := lisLrsIncludeFiles;
  UseLRSFilesRadioButton.Hint := lisAutomaticallyConvertLfmFilesToLrsIncludeFiles;
  UseFPCResourcesRadioButton.Caption := lisFPCResources;
  UseFPCResourcesRadioButton.Hint := lisRequiresFPC24OrAboveLikeDelphiResources;
end;

procedure TProjectMiscOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do
  begin
    MainUnitIsPascalSourceCheckBox.Checked := (pfMainUnitIsPascalSource in Flags);
    MainUnitHasUsesSectionForAllUnitsCheckBox.Checked := (pfMainUnitHasUsesSectionForAllUnits in Flags);
    MainUnitHasCreateFormStatementsCheckBox.Checked := (pfMainUnitHasCreateFormStatements in Flags);
    MainUnitHasTitleStatementCheckBox.Checked := (pfMainUnitHasTitleStatement in Flags);
    RunnableCheckBox.Checked := (pfRunnable in Flags);
    AlwaysBuildCheckBox.Checked := (pfAlwaysBuild in Flags);
    LRSInOutputDirCheckBox.Checked := (pfLRSFilesInOutputDirectory in Flags);
    case Resources.ResourceType of
      rtLRS: UseLRSFilesRadioButton.Checked := True;
      rtRes: UseFPCResourcesRadioButton.Checked := True;
    end;
  end;
end;

procedure TProjectMiscOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Project: TProject absolute AOptions;
  NewFlags: TProjectFlags;

  procedure SetProjectFlag(AFlag: TProjectFlag; AValue: Boolean);
  begin
    if AValue then
      Include(NewFlags, AFlag)
    else
      Exclude(NewFlags, AFlag);
  end;

begin
  NewFlags := Project.Flags;
  SetProjectFlag(pfMainUnitIsPascalSource,
                 MainUnitIsPascalSourceCheckBox.Checked);
  SetProjectFlag(pfMainUnitHasUsesSectionForAllUnits,
                 MainUnitHasUsesSectionForAllUnitsCheckBox.Checked);
  SetProjectFlag(pfMainUnitHasCreateFormStatements,
                 MainUnitHasCreateFormStatementsCheckBox.Checked);
  SetProjectFlag(pfMainUnitHasTitleStatement,
                 MainUnitHasTitleStatementCheckBox.Checked);
  SetProjectFlag(pfRunnable, RunnableCheckBox.Checked);
  SetProjectFlag(pfAlwaysBuild, AlwaysBuildCheckBox.Checked);
  SetProjectFlag(pfLRSFilesInOutputDirectory, LRSInOutputDirCheckBox.Checked);
  Project.Flags := NewFlags;
  if UseLRSFilesRadioButton.Checked then
    Project.Resources.ResourceType := rtLRS
  else
    Project.Resources.ResourceType := rtRes;
end;

class function TProjectMiscOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectMiscOptionsFrame, ProjectOptionsMisc);

end.

