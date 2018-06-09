unit project_save_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LazUtils
  FileUtil,
  // LCL
  LCLProc, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, ProjectIntf,
  // IDE
  Project, IDEProcs, LazarusIDEStrConsts;

type

  { TProjectSaveOptionsFrame }

  TProjectSaveOptionsFrame = class(TAbstractIDEOptionsEditor)
    SaveJumpHistory: TCheckBox;
    SaveFoldState: TCheckBox;
    SaveClosedUnitInfoCheckBox: TCheckBox;
    SaveOnlyProjectUnitInfoCheckBox: TCheckBox;
    SaveSessionLocationRadioGroup: TRadioGroup;
    procedure SaveSessionLocationRadioGroupClick(Sender: TObject);
  private
    fProject: TProject;
  public
    function GetTitle: string; override;
    function GetSessionLocation: TProjectSessionStorage;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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

procedure TProjectSaveOptionsFrame.SaveSessionLocationRadioGroupClick(Sender: TObject);
begin
  ;
end;

function TProjectSaveOptionsFrame.GetSessionLocation: TProjectSessionStorage;
begin
  Result := LocalizedNameToProjectSessionStorage(
    SaveSessionLocationRadioGroup.Items[SaveSessionLocationRadioGroup.ItemIndex]);
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
  SaveClosedUnitInfoCheckBox.Hint := dlgSaveEditorInfoHint;
  SaveOnlyProjectUnitInfoCheckBox.Caption := dlgSaveEditorInfoProject;
  SaveOnlyProjectUnitInfoCheckBox.Hint := dlgSaveEditorInfoProjectHint;
  SaveJumpHistory.Caption := lisSaveSessionJumpHistory;
  SaveJumpHistory.Hint := lisSaveSessionJumpHistoryHint;
  SaveFoldState.Caption := lisSaveSessionFoldState;
  SaveFoldState.Hint := lisSaveSessionFoldStateHint;
  SaveSessionLocationRadioGroup.Caption := lisPOSaveSessionInformationIn;
  SaveSessionLocationRadioGroup.Hint := lisPOSaveSessionInformationInHint;
  for s := Low(TProjectSessionStorage) to High(TProjectSessionStorage) do
    SaveSessionLocationRadioGroup.Items.Add(ProjectSessionStorageToLocalizedName(s));
end;

procedure TProjectSaveOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TProjectIDEOptions) then exit;
  fProject:=(AOptions as TProjectIDEOptions).Project;
  with fProject do
  begin
    SaveClosedUnitInfoCheckBox.Checked := (pfSaveClosedUnits in Flags);
    SaveOnlyProjectUnitInfoCheckBox.Checked := (pfSaveOnlyProjectUnits in Flags);
    SaveSessionLocationRadioGroup.ItemIndex := ord(SessionStorage);
    SaveJumpHistory.Checked := (pfSaveJumpHistory in Flags);
    SaveFoldState.Checked := (pfSaveFoldState in Flags);
  end;
end;

procedure TProjectSaveOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  AFlags: TProjectFlags;
begin
  if not (AOptions is TProjectIDEOptions) then exit;
  with (AOptions as TProjectIDEOptions).Project do
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
    if SaveJumpHistory.Checked then
      include(AFlags, pfSaveJumpHistory)
    else
      exclude(AFlags, pfSaveJumpHistory);
    if SaveFoldState.Checked then
      include(AFlags, pfSaveFoldState)
    else
      exclude(AFlags, pfSaveFoldState);
    if SaveSessionLocationRadioGroup.ItemIndex >= 0 then
      SessionStorage := Self.GetSessionLocation;
    Flags := AFlags;
  end;
end;

class function TProjectSaveOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectSaveOptionsFrame, ProjectOptionsSave);

end.

