unit Project_ValFormatter_Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  IdeDbgValueFormatterSettingsFrame, IdeDebuggerValueFormatter,
  IdeDebuggerStringConstants, ProjectDebugLink, LazarusIDEStrConsts, DebugManager, Project,
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
  FValFormatList.Assign(DbgProjectLink.ValueFormatterConfig);
  FValFormatList.Changed := False;
  IdeDbgVarFormatterFrame1.ValFormatterList := FValFormatList;

  chkStoreInSession.Checked := DbgProjectLink.StoreValueFormatterConfigInSession;
  chkUseGlobalList.Checked  := DbgProjectLink.UseValueFormatterFromIDE;
  chkUseProjList.Checked    := DbgProjectLink.UseValueFormatterFromProject;
end;

procedure TIdeProjectValueFormatterOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  HasChg: Boolean;
begin
  IdeDbgVarFormatterFrame1.SaveCurrent;

  HasChg :=
    (DbgProjectLink.UseValueFormatterFromIDE <> chkUseGlobalList.Checked) or
    (DbgProjectLink.UseValueFormatterFromProject <> chkUseProjList.Checked) or
    FValFormatList.Changed;

  DbgProjectLink.StoreValueFormatterConfigInSession := chkStoreInSession.Checked;
  DbgProjectLink.UseValueFormatterFromIDE := chkUseGlobalList.Checked;
  DbgProjectLink.UseValueFormatterFromProject := chkUseProjList.Checked;

  if FValFormatList.Changed or HasChg then begin
    DbgProjectLink.ValueFormatterConfig.Assign(FValFormatList);
    DbgProjectLink.ValueFormatterConfig.Changed := True;

  end;
end;

class function TIdeProjectValueFormatterOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TIdeProjectValueFormatterOptionsFrame, ProjectOptionsDbgValueFormatter, ProjectOptionsDebug);

end.

