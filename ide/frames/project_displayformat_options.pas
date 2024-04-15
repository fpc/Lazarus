unit Project_DisplayFormat_Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, DebugManager, LazarusIDEStrConsts,
  Project, DisplayFormatDefaultsConfigFrame, IdeDebuggerDisplayFormats, IdeDebuggerStringConstants,
  IdeDebuggerOpts, ProjectDebugLink, DividerBevel, IDEOptionsIntf, IDEOptEditorIntf,
  DbgIntfDebuggerBase;

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
  FDisplayFormatConfig.Assign(DbgProjectLink.DisplayFormatConfigs);
  DisplayFormatDefaultsConfigFrame1.DisplayFormatConfig := FDisplayFormatConfig;

  chkStoreInSession.Checked := DbgProjectLink.StoreDisplayFormatConfigsInSession;
  chkUseGlobalList.Checked  := DbgProjectLink.UseDisplayFormatConfigsFromIDE;
  chkUseProjList.Checked    := DbgProjectLink.UseDisplayFormatConfigsFromProject;
end;

procedure TProjectDisplayFormatOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  HasChg: Boolean;
begin
  DisplayFormatDefaultsConfigFrame1.SaveConfig;
  DbgProjectLink.DisplayFormatConfigs.Changed := False;
  DbgProjectLink.DisplayFormatConfigs.Assign(FDisplayFormatConfig); // assign will trigger changed, if anything changed

  HasChg :=
    DbgProjectLink.DisplayFormatConfigs.Changed or
    (DbgProjectLink.UseDisplayFormatConfigsFromIDE <> chkUseGlobalList.Checked) or
    (DbgProjectLink.UseDisplayFormatConfigsFromProject <> chkUseProjList.Checked);

  DbgProjectLink.StoreDisplayFormatConfigsInSession := chkStoreInSession.Checked;
  DbgProjectLink.UseDisplayFormatConfigsFromIDE := chkUseGlobalList.Checked;
  DbgProjectLink.UseDisplayFormatConfigsFromProject := chkUseProjList.Checked;

  DbgProjectLink.DisplayFormatConfigs.Changed := HasChg;
end;

class function TProjectDisplayFormatOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectDisplayFormatOptionsFrame, ProjectOptionsDbgDisplayFormat, ProjectOptionsDebug);

end.

