unit project_debug_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LazUtils
  LazTracer,
  // LCL
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, ProjectIntf,
  // IDE
  Project, LazarusIDEStrConsts, EnvironmentOpts;

type

  { TProjectDebugOptionsFrame }

  TProjectDebugOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbProjectDebugger: TComboBox;
    lbProjectDebugger: TLabel;
  private
    fProject: TProject;
    FDebuggerBackend: String;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    //property aProject: TProject read fProject;
  end;

implementation

{$R *.lfm}

{ TProjectDebugOptionsFrame }

function TProjectDebugOptionsFrame.GetTitle: string;
begin
  Result := dlgPODebugger;
end;

procedure TProjectDebugOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  lbProjectDebugger.Caption := lisDebugOptionsFrmDebuggerBackend;
end;

procedure TProjectDebugOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i, sel: Integer;
  dbg: TDebuggerPropertiesConfigList;
begin
  if not (AOptions is TProjectIDEOptions) then exit;
  fProject:=(AOptions as TProjectIDEOptions).Project;
  with fProject do
  begin
    Self.FDebuggerBackend := DebuggerBackend;
  end;

  cbProjectDebugger.Clear;
  sel := -1;
  cbProjectDebugger.AddItem(lisDebugOptionsFrmUseIDEDebugger, TObject(-1));
  if FDebuggerBackend = '' then
    sel := 0;

  dbg := EnvironmentOptions.DebuggerPropertiesConfigList;
  for i := 0 to dbg.Count - 1 do begin
    cbProjectDebugger.AddItem(dbg.Opt[i].DisplayName, TObject(PtrUInt((i))));
    if dbg.Opt[i].UID = FDebuggerBackend then
      sel := i+1;
  end;
  if sel < 0 then
    sel := cbProjectDebugger.Items.AddObject(Format(
      lisDebugOptionsFrmUnknownDebuggerBacke, [FDebuggerBackend]), TObject(PtrUInt(( - 2))));
  cbProjectDebugger.ItemIndex := sel;
end;

procedure TProjectDebugOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  if not (AOptions is TProjectIDEOptions) then exit;

  i := cbProjectDebugger.ItemIndex;
  if i >= 0 then begin
    FDebuggerBackend := ''; // -1
    i := PtrInt(cbProjectDebugger.Items.Objects[i]);
    if i >= 0 then
      FDebuggerBackend := EnvironmentOptions.DebuggerPropertiesConfigList.Opt[i].UID;
  end;

  with (AOptions as TProjectIDEOptions).Project do
  begin
    DebuggerBackend := FDebuggerBackend;
  end;
end;

class function TProjectDebugOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectDebugOptionsFrame, ProjectOptionsDebug);

end.

