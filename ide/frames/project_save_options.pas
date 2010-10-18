unit project_save_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Project, IDEOptionsIntf, ProjectIntf, LCLProc,
  LazarusIDEStrConsts, IDEProcs, BuildModesEditor;

type

  { TProjectSaveOptionsFrame }

  TProjectSaveOptionsFrame = class(TAbstractIDEOptionsEditor)
    SaveClosedUnitInfoCheckBox: TCheckBox;
    SaveOnlyProjectUnitInfoCheckBox: TCheckBox;
    SaveSessionLocationRadioGroup: TRadioGroup;
    procedure SaveSessionLocationRadioGroupClick(Sender: TObject);
  private
    fProject: TProject;
    function GetSessionLocation: TProjectSessionStorage;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    property aProject: TProject read fProject;
  end;

implementation

{$R *.lfm}

function ProjectSessionStorageToLocalizedName(s: TProjectSessionStorage): string;
begin
  case s of
    pssInProjectInfo: Result := lisPOSaveInLpiFil;
    pssInProjectDir:  Result := lisPOSaveInLpsFileInProjectDirectory;
    pssInIDEConfig:   Result := lisPOSaveInIDEConfigDirectory;
    pssNone:          Result := lisPODoNotSaveAnySessionInfo;
  else
    RaiseGDBException('');
  end;
end;

function LocalizedNameToProjectSessionStorage(const s: string): TProjectSessionStorage;
begin
  for Result := Low(TProjectSessionStorage) to High(TProjectSessionStorage) do
    if ProjectSessionStorageToLocalizedName(Result) = s then exit;
  Result := pssInProjectInfo;
end;

{ TProjectSaveOptionsFrame }

procedure TProjectSaveOptionsFrame.SaveSessionLocationRadioGroupClick(
  Sender: TObject);
var
  BuildModesEditor: TBuildModesEditorFrame;
begin
  BuildModesEditor:=TBuildModesEditorFrame(FindOptionControl(TBuildModesEditorFrame));
  if BuildModesEditor<>nil then
  begin
    BuildModesEditor.LoadShowSessionFromProjects:=false;
    BuildModesEditor.ShowSession:=GetSessionLocation in [pssInIDEConfig,pssInProjectDir];
  end;
end;

function TProjectSaveOptionsFrame.GetSessionLocation: TProjectSessionStorage;
begin
  Result := LocalizedNameToProjectSessionStorage(
                     SaveSessionLocationRadioGroup.Items[
                                  SaveSessionLocationRadioGroup.ItemIndex]);
end;

function TProjectSaveOptionsFrame.GetTitle: string;
begin
  Result := dlgPOSaveSession;
end;

procedure TProjectSaveOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  s: TProjectSessionStorage;
begin
  SaveClosedUnitInfoCheckBox.Caption := dlgSaveEditorInfo;
  SaveOnlyProjectUnitInfoCheckBox.Caption := dlgSaveEditorInfoProject;
  SaveSessionLocationRadioGroup.Caption := lisPOSaveSessionInformationIn;
  for s := Low(TProjectSessionStorage) to High(TProjectSessionStorage) do
    SaveSessionLocationRadioGroup.Items.Add(ProjectSessionStorageToLocalizedName(s));
end;

procedure TProjectSaveOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TProject) then exit;
  fProject:=TProject(AOptions);
  with AOptions as TProject do
  begin
    SaveClosedUnitInfoCheckBox.Checked := (pfSaveClosedUnits in Flags);
    SaveOnlyProjectUnitInfoCheckBox.Checked := (pfSaveOnlyProjectUnits in Flags);
    SaveSessionLocationRadioGroup.ItemIndex := ord(SessionStorage);
  end;
end;

procedure TProjectSaveOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  AFlags: TProjectFlags;
begin
  if not (AOptions is TProject) then exit;
  with AOptions as TProject do
  begin
    AFlags := Flags;
    if SaveClosedUnitInfoCheckBox.Checked then
      include(AFlags, pfSaveClosedUnits)
    else
      exclude(AFlags, pfSaveClosedUnits);
    if SaveOnlyProjectUnitInfoCheckBox.Checked then
      include(AFlags, pfSaveOnlyProjectUnits)
    else
      exclude(AFlags, pfSaveOnlyProjectUnits);
    if SaveSessionLocationRadioGroup.ItemIndex >= 0 then
      SessionStorage := Self.GetSessionLocation;
    Flags := AFlags;
  end;
end;

class function TProjectSaveOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectSaveOptionsFrame, ProjectOptionsSave);

end.

