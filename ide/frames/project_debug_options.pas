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
  IDEOptionsIntf, IDEOptEditorIntf, ProjectIntf, DividerBevel,
  // IDE
  Project, LazarusIDEStrConsts, EnvironmentOpts, debugger_class_options,
  IdeDebuggerOpts, Classes;

type

  { TProjectDebugOptionsFrame }

  TProjectDebugOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbProjectDebugger: TComboBox;
    chkStoreInSession: TCheckBox;
    DividerBevel1: TDividerBevel;
    lblResolvedDebuggerHint: TLabel;
    lbProjectDebugger: TLabel;
    Panel1: TPanel;
    procedure cbProjectDebuggerChange(Sender: TObject);
  private
    fProject: TProject;
    FDebuggerBackend, FDebuggerBackendUnknown: String;
    FDebuggerBackendIdx: Integer;
    FClassOpts: TDebuggerClassOptionsFrame;
    procedure ClassCountChanged(Sender: TObject);
    procedure UpdateDebuggerBackend;
    procedure FillProjectDebuggerDropDown;
    procedure UpdateResolvedDebuggerHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    //property aProject: TProject read fProject;
  end;

implementation

const
  DBG_IDX_UNKNOWN = -99; // will use "project debugger"
  DBG_IDX_PROJECT =  -2;
  DBG_IDX_IDE     =  -1;
  DBG_TOKEN_IDE = 'IDE';

{$R *.lfm}

{ TProjectDebugOptionsFrame }

procedure TProjectDebugOptionsFrame.cbProjectDebuggerChange(Sender: TObject);
begin
  UpdateResolvedDebuggerHint;
end;

procedure TProjectDebugOptionsFrame.ClassCountChanged(Sender: TObject);
begin
  UpdateResolvedDebuggerHint
end;

procedure TProjectDebugOptionsFrame.UpdateDebuggerBackend;
var
  i: Integer;
begin
  if cbProjectDebugger.Items.Count = 0 then
    exit;

  FDebuggerBackend := '';
  FDebuggerBackendIdx := DBG_IDX_PROJECT;

  i := cbProjectDebugger.ItemIndex;
  if i >= 0 then begin
    FDebuggerBackendIdx := PtrInt(cbProjectDebugger.Items.Objects[i]);
    if FDebuggerBackendIdx = DBG_IDX_IDE then
      FDebuggerBackend := DBG_TOKEN_IDE
    else
    if FDebuggerBackendIdx = DBG_IDX_UNKNOWN then
      FDebuggerBackend := FDebuggerBackendUnknown
    else
    if FDebuggerBackendIdx >= 0 then
      FDebuggerBackend := DebuggerOptions.DebuggerPropertiesConfigList.Opt[FDebuggerBackendIdx].UID;
  end;
end;

procedure TProjectDebugOptionsFrame.FillProjectDebuggerDropDown;
const
  DBG_IDX_OFFSET  = 2; // 2 Hardcoded items (project, ide) // Offset for DropDown.ItemIndex
var
  i, sel: Integer;
  dbg: TDebuggerPropertiesConfigList;
begin
  cbProjectDebugger.Clear;
  sel := -1;
  cbProjectDebugger.AddItem(lisDebugOptionsFrmUseProjectDebugger, TObject(PtrUInt(DBG_IDX_PROJECT)));
  cbProjectDebugger.AddItem(lisDebugOptionsFrmUseIDEDebugger,     TObject(PtrUInt(DBG_IDX_IDE)));
  if FDebuggerBackend = '' then
    sel := 0
  else
  if FDebuggerBackend = DBG_TOKEN_IDE then
    sel := 1;

  dbg := DebuggerOptions.DebuggerPropertiesConfigList;
  for i := 0 to dbg.Count - 1 do begin
    cbProjectDebugger.AddItem(dbg.Opt[i].DisplayName, TObject(PtrUInt(i)));
    if dbg.Opt[i].UID = FDebuggerBackend then
      sel := i + DBG_IDX_OFFSET;
  end;

  if sel < 0 then begin
    sel := cbProjectDebugger.Items.AddObject(Format(
      lisDebugOptionsFrmUnknownDebuggerBacke, [FDebuggerBackend]), TObject(PtrUInt(DBG_IDX_UNKNOWN)));
    FDebuggerBackendUnknown := FDebuggerBackend;
  end;
  cbProjectDebugger.ItemIndex := sel;
end;

procedure TProjectDebugOptionsFrame.UpdateResolvedDebuggerHint;
begin
  UpdateDebuggerBackend;

  lblResolvedDebuggerHint.Visible := False;
  case FDebuggerBackendIdx of
    DBG_IDX_PROJECT: begin
      if FClassOpts.ModifiedDbgPropertiesConfigList.Count = 0 then begin
        lblResolvedDebuggerHint.Caption := drsUsingIDEDefaultDebuggerSe;
        lblResolvedDebuggerHint.Visible := True;
      end;
    end;
    DBG_IDX_IDE: begin
      if FClassOpts.ModifiedDbgPropertiesConfigList.Count > 0 then begin
        lblResolvedDebuggerHint.Caption := drsUsingIDEDefaultDebuggerSe;
        lblResolvedDebuggerHint.Caption := lblResolvedDebuggerHint.Caption + drsIgnoringProjectDebuggerSettings;
        lblResolvedDebuggerHint.Visible := True;
      end;
    end;
    DBG_IDX_UNKNOWN: begin
      lblResolvedDebuggerHint.Caption := drsUsingIDEDefaultDebuggerSe;
      if FClassOpts.ModifiedDbgPropertiesConfigList.Count > 0 then
        lblResolvedDebuggerHint.Caption := lblResolvedDebuggerHint.Caption + drsIgnoringProjectDebuggerSettings;
      lblResolvedDebuggerHint.Visible := True;
    end;
    otherwise begin
      if FClassOpts.ModifiedDbgPropertiesConfigList.Count > 0 then begin
        lblResolvedDebuggerHint.Caption := drsUsingSelectedIDEDebuggerS;
        lblResolvedDebuggerHint.Caption := lblResolvedDebuggerHint.Caption + drsIgnoringProjectDebuggerSettings;
        lblResolvedDebuggerHint.Visible := True;
    end;
    end;
  end;
end;

constructor TProjectDebugOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClassOpts := TDebuggerClassOptionsFrame.Create(AOwner);
  FClassOpts.Parent := Self;
  FClassOpts.Align := alClient;
  FClassOpts.Visible := True;
  FClassOpts.OnModifiedDbgPropertiesCountChanged := @ClassCountChanged;
end;

destructor TProjectDebugOptionsFrame.Destroy;
begin
  FClassOpts.OnModifiedDbgPropertiesCountChanged := nil;
  inherited Destroy;
end;

function TProjectDebugOptionsFrame.GetTitle: string;
begin
  Result := dlgPODebugger;
end;

procedure TProjectDebugOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  lbProjectDebugger.Caption := lisDebugOptionsFrmDebuggerBackend;
  chkStoreInSession.Caption := drsStoreProjectDebuggerConfi;
  chkStoreInSession.Hint := drsTheDebuggerBackendSelecti;
  FClassOpts.Setup(ADialog);
end;

procedure TProjectDebugOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TProjectIDEOptions) then exit;
  fProject:=(AOptions as TProjectIDEOptions).Project;
  with fProject do
  begin
    Self.FDebuggerBackend := DebuggerBackend;
    chkStoreInSession.Checked := StoreDebuggerClassConfInSession;
    FClassOpts.ReadSettings(DebuggerPropertiesConfigList);
  end;

  FillProjectDebuggerDropDown;
end;

procedure TProjectDebugOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TProjectIDEOptions) then exit;

  UpdateDebuggerBackend;

  with (AOptions as TProjectIDEOptions).Project do
  begin
    DebuggerBackend := FDebuggerBackend;
    StoreDebuggerClassConfInSession := chkStoreInSession.Checked;
    FClassOpts.WriteSettings(DebuggerPropertiesConfigList);
    MarkDebuggerClassConfAsModified;
  end;

end;

class function TProjectDebugOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectDebugOptionsFrame, ProjectOptionsDebug);

end.

