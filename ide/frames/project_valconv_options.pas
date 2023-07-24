unit Project_ValConv_Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // LCL
  Forms, ExtCtrls, StdCtrls,
  // LazControls
  DividerBevel,
  // BuildIntf
  IDEOptionsIntf,
  // IdeIntf
  IDEOptEditorIntf,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeDebugger
  IdeDebuggerStringConstants, IdeDbgValueConverterSettingsFrame, IdeDebuggerOpts,
  IdeDebuggerBackendValueConv,
  // IDE
  Project, DebugManager, LazarusIDEStrConsts;

type

  { TIdeProjectValConvOptionsFrame }

  TIdeProjectValConvOptionsFrame = class(TAbstractIDEOptionsEditor)
    chkUseProjList: TCheckBox;
    chkUseGlobalList: TCheckBox;
    chkStoreInSession: TCheckBox;
    DbgValConvFrame1: TIdeDbgValConvFrame;
    DividerBevel1: TDividerBevel;
    Panel1: TPanel;
  private
    FValConvList: TIdeDbgValueConvertSelectorList;
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

{ TIdeProjectValConvOptionsFrame }

destructor TIdeProjectValConvOptionsFrame.Destroy;
begin
  inherited Destroy;
  FValConvList.Free;
end;

function TIdeProjectValConvOptionsFrame.GetTitle: String;
begin
  Result := dlgBackConvOptDebugOptions;
end;

procedure TIdeProjectValConvOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  DbgValConvFrame1.Setup;
  chkStoreInSession.Caption := drsStoreConverterConfigInSes;
  chkStoreInSession.Hint := drsThisOnlyAffectsTheListOfC;
  chkUseGlobalList.Caption := drsUseTheIDEGlobalListOfConv;
  chkUseProjList.Caption := drsUseTheProjectListOfConver;
end;

procedure TIdeProjectValConvOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  if FValConvList = nil then
    FValConvList := TIdeDbgValueConvertSelectorList.Create;
  FValConvList.Assign(DebugBossMgr.ProjectLink.BackendConverterConfig);
  FValConvList.Changed := False;
  DbgValConvFrame1.ValConvList := FValConvList;

  chkStoreInSession.Checked := DebugBossMgr.ProjectLink.StoreBackendConverterConfigInSession;
  chkUseGlobalList.Checked  := DebugBossMgr.ProjectLink.UseBackendConverterFromIDE;
  chkUseProjList.Checked    := DebugBossMgr.ProjectLink.UseBackendConverterFromProject;
end;

procedure TIdeProjectValConvOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
var
  HasChg: Boolean;
begin
  DbgValConvFrame1.SaveCurrent;

  HasChg :=
    (DebugBossMgr.ProjectLink.UseBackendConverterFromIDE <> chkUseGlobalList.Checked) or
    (DebugBossMgr.ProjectLink.UseBackendConverterFromProject <> chkUseProjList.Checked) or
    FValConvList.Changed;

  DebugBossMgr.ProjectLink.StoreBackendConverterConfigInSession := chkStoreInSession.Checked;
  DebugBossMgr.ProjectLink.UseBackendConverterFromIDE := chkUseGlobalList.Checked;
  DebugBossMgr.ProjectLink.UseBackendConverterFromProject := chkUseProjList.Checked;

  if FValConvList.Changed then begin
    DebugBossMgr.ProjectLink.BackendConverterConfig.Assign(FValConvList);
    DebugBossMgr.ProjectLink.BackendConverterConfig.Changed := True;

  end;
  if (DebugBossManager <> nil) and HasChg then
    DebugBossManager.DoBackendConverterChanged;
end;

class function TIdeProjectValConvOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TIdeProjectValConvOptionsFrame, ProjectOptionsDbgValueConvert, ProjectOptionsDebug);

end.

