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
  IdeDebuggerBackendValueConv, ProjectDebugLink,
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
  FValConvList.Assign(DbgProjectLink.BackendConverterConfig);
  FValConvList.Changed := False;
  DbgValConvFrame1.ValConvList := FValConvList;

  chkStoreInSession.Checked := DbgProjectLink.StoreBackendConverterConfigInSession;
  chkUseGlobalList.Checked  := DbgProjectLink.UseBackendConverterFromIDE;
  chkUseProjList.Checked    := DbgProjectLink.UseBackendConverterFromProject;
end;

procedure TIdeProjectValConvOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  DbgValConvFrame1.SaveCurrent;

  DbgProjectLink.StoreBackendConverterConfigInSession := chkStoreInSession.Checked;
  DbgProjectLink.UseBackendConverterFromIDE := chkUseGlobalList.Checked;
  DbgProjectLink.UseBackendConverterFromProject := chkUseProjList.Checked;

  if FValConvList.Changed then begin
    DbgProjectLink.BackendConverterConfig.Assign(FValConvList);
    DbgProjectLink.BackendConverterConfig.Changed := True;
  end;
end;

class function TIdeProjectValConvOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TIdeProjectValConvOptionsFrame, ProjectOptionsDbgValueConvert, ProjectOptionsDebug);

end.

