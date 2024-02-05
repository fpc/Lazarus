unit Project_ValFormatter_Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  IdeDbgValueFormatterSettingsFrame, IdeDebuggerValueFormatter,
  IdeDebuggerStringConstants, LazarusIDEStrConsts, DebugManager, Project,
  DividerBevel, IDEOptionsIntf, IDEOptEditorIntf, DbgIntfDebuggerBase;

type

  { TIdeProjectValueFormatterOptionsFrame }

  TIdeProjectValueFormatterOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkStoreInSession: TCheckBox;
    chkUseGlobalList: TCheckBox;
    chkUseProjList: TCheckBox;
    DividerBevel1: TDividerBevel;
    IdeDbgVarFormatterFrame1: TIdeDbgVarFormatterFrame;
    Panel1: TPanel;
  private
    FValFormatList: TIdeDbgValueFormatterSelectorList;
  public
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TIdeProjectValueFormatterOptionsFrame }

destructor TIdeProjectValueFormatterOptionsFrame.Destroy;
begin
  inherited Destroy;
  FValFormatList.Free;
end;

function TIdeProjectValueFormatterOptionsFrame.GetTitle: String;
begin
  Result := dlgVarFormatterDebugOptions;
end;

procedure TIdeProjectValueFormatterOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  IdeDbgVarFormatterFrame1.Setup;
  chkStoreInSession.Caption := drsStoreFormatterConfigInSes;
  chkStoreInSession.Hint := drsThisOnlyAffectsTheListOfFormatter;
  chkUseGlobalList.Caption := drsUseTheIDEGlobalListOfFormatter;
  chkUseProjList.Caption := drsUseTheProjectListOfFormatter;
end;

procedure TIdeProjectValueFormatterOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if FValFormatList = nil then
    FValFormatList := TIdeDbgValueFormatterSelectorList.Create;
  FValFormatList.Assign(DebugBossMgr.ProjectLink.ValueFormatterConfig);
  FValFormatList.Changed := False;
  IdeDbgVarFormatterFrame1.ValFormmaterList := FValFormatList;

  chkStoreInSession.Checked := DebugBossMgr.ProjectLink.StoreValueFormatterConfigInSession;
  chkUseGlobalList.Checked  := DebugBossMgr.ProjectLink.UseValueFormatterFromIDE;
  chkUseProjList.Checked    := DebugBossMgr.ProjectLink.UseValueFormatterFromProject;
end;

procedure TIdeProjectValueFormatterOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  HasChg: Boolean;
begin
  IdeDbgVarFormatterFrame1.SaveCurrent;

  HasChg :=
    (DebugBossMgr.ProjectLink.UseValueFormatterFromIDE <> chkUseGlobalList.Checked) or
    (DebugBossMgr.ProjectLink.UseValueFormatterFromProject <> chkUseProjList.Checked) or
    FValFormatList.Changed;

  DebugBossMgr.ProjectLink.StoreValueFormatterConfigInSession := chkStoreInSession.Checked;
  DebugBossMgr.ProjectLink.UseValueFormatterFromIDE := chkUseGlobalList.Checked;
  DebugBossMgr.ProjectLink.UseValueFormatterFromProject := chkUseProjList.Checked;

  if FValFormatList.Changed then begin
    DebugBossMgr.ProjectLink.ValueFormatterConfig.Assign(FValFormatList);
    DebugBossMgr.ProjectLink.ValueFormatterConfig.Changed := True;

  end;
  if (DebugBossManager <> nil) and HasChg then
    DebugBossManager.DoBackendConverterChanged;
end;

class function TIdeProjectValueFormatterOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TIdeProjectValueFormatterOptionsFrame, ProjectOptionsDbgValueFormatter, ProjectOptionsDebug);

end.

