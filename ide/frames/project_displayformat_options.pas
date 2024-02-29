unit Project_DisplayFormat_Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, DebugManager, LazarusIDEStrConsts,
  Project, DisplayFormatDefaultsConfigFrame, IdeDebuggerDisplayFormats, IdeDebuggerStringConstants,
  IdeDebuggerOpts, DividerBevel, IDEOptionsIntf, IDEOptEditorIntf, DbgIntfDebuggerBase;

type

  { TProjectDisplayFormatOptionsFrame }

  TProjectDisplayFormatOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkStoreInSession: TCheckBox;
    chkUseGlobalList: TCheckBox;
    chkUseProjList: TCheckBox;
    DisplayFormatDefaultsConfigFrame1: TDisplayFormatDefaultsConfigFrame;
    DividerBevel1: TDividerBevel;
    Panel1: TPanel;
  private
    FDisplayFormatConfig: TDisplayFormatConfig;
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

{ TProjectDisplayFormatOptionsFrame }

destructor TProjectDisplayFormatOptionsFrame.Destroy;
begin
  inherited Destroy;
  FDisplayFormatConfig.Free;
end;

function TProjectDisplayFormatOptionsFrame.GetTitle: String;
begin
  Result := dlgDisplayFormatDebugOptions;
end;

procedure TProjectDisplayFormatOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  DisplayFormatDefaultsConfigFrame1.Setup;
  chkStoreInSession.Caption := drsStoreDispFormatConfigInSes;
  chkStoreInSession.Hint := drsThisOnlyAffectsDispFormats;
  chkUseGlobalList.Caption := drsUseIDEGlobalDispFormats;
  chkUseProjList.Caption := drsUseProjecfDispFormats;
end;

procedure TProjectDisplayFormatOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if FDisplayFormatConfig = nil then
    FDisplayFormatConfig := TDisplayFormatConfig.Create;
  FDisplayFormatConfig.Assign(DebugBossMgr.ProjectLink.DisplayFormatConfigs);
  DisplayFormatDefaultsConfigFrame1.DisplayFormatConfig := FDisplayFormatConfig;

  chkStoreInSession.Checked := DebugBossMgr.ProjectLink.StoreDisplayFormatConfigsInSession;
  chkUseGlobalList.Checked  := DebugBossMgr.ProjectLink.UseDisplayFormatConfigsFromIDE;
  chkUseProjList.Checked    := DebugBossMgr.ProjectLink.UseDisplayFormatConfigsFromProject;
end;

procedure TProjectDisplayFormatOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  c, HasChg: Boolean;
begin
  DisplayFormatDefaultsConfigFrame1.SaveConfig;
  c := DebugBossMgr.ProjectLink.DisplayFormatConfigs.Changed;
  DebugBossMgr.ProjectLink.DisplayFormatConfigs.Changed := False;
  DebugBossMgr.ProjectLink.DisplayFormatConfigs.Assign(FDisplayFormatConfig); // assign will trigger changed, if anything changed

  HasChg :=
    (DebugBossMgr.ProjectLink.UseDisplayFormatConfigsFromIDE <> chkUseGlobalList.Checked) or
    (DebugBossMgr.ProjectLink.UseDisplayFormatConfigsFromProject <> chkUseProjList.Checked);

  DebugBossMgr.ProjectLink.StoreDisplayFormatConfigsInSession := chkStoreInSession.Checked;
  DebugBossMgr.ProjectLink.UseDisplayFormatConfigsFromIDE := chkUseGlobalList.Checked;
  DebugBossMgr.ProjectLink.UseDisplayFormatConfigsFromProject := chkUseProjList.Checked;

  if HasChg or DebugBossMgr.ProjectLink.DisplayFormatConfigs.Changed then begin
    if (DebugBossManager <> nil) then
      DebugBossManager.DoBackendConverterChanged;
  end
  else
    DebugBossMgr.ProjectLink.DisplayFormatConfigs.Changed := c;
end;

class function TProjectDisplayFormatOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectDisplayFormatOptionsFrame, ProjectOptionsDbgDisplayFormat, ProjectOptionsDebug);

end.

