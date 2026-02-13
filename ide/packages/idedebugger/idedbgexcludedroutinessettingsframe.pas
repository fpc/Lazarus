unit IdeDbgExcludedRoutinesSettingsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Dialogs,
  IDEOptEditorIntf, IDEOptionsIntf, xregexpr, IdeDebuggerStringConstants,
  LazDebuggerIntfExcludedRoutines, IdeDbgConfigItemCheckListBoxFrame,
  IdeDebuggerExcludedRoutines, IdeDebuggerOpts, ProjectDebugLink, Project;

type

  { TIdeDbgExcludedRoutineConfFrame }

  TIdeDbgExcludedRoutineConfFrame = class(TFrame)
    chkStoreInSession: TCheckBox;
    chkUseGlobalList: TCheckBox;
    chkUseProjList: TCheckBox;
    dropPath: TComboBox;
    dropFunc: TComboBox;
    EdName: TEdit;
    EdPath: TEdit;
    IdeDbgConfigItemCheckListFrame1: TIdeDbgConfigItemCheckListFrame;
    lbWarn1: TLabel;
    lbWarn2: TLabel;
    lblName: TLabel;
    lblPath: TLabel;
    lblFunc: TLabel;
    edFunc: TMemo;
    lbWarn3: TLabel;
    Panel1: TPanel;
    PanelProjectOpts: TPanel;
    Splitter1: TSplitter;
    procedure DropChanged(Sender: TObject);
    procedure EditChanged(Sender: TObject);
    procedure EditDone(Sender: TObject);
    procedure EdNameChange(Sender: TObject);
  private
    FExcludedRoutineConfList: TIdeDebuggerExcludeRoutineConfList;
    FCurrentConf: TIdeDebuggerExcludeRoutineConf;
    FChanging: Boolean;

    procedure DoAddConf;
    procedure DoDeleteConf(AnIndex: integer; AnObject: TObject);
    procedure DoEnabledChange(AnIndex: integer; AnObject: TObject; ANewChecked: boolean);
    procedure DoSelectChange(AnIndex, AnOldIndex: integer; AnObject, AnOldObject: TObject);
    procedure SetExcludedRoutineConfList(AValue: TIdeDebuggerExcludeRoutineConfList);
    procedure SaveCurrent;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Setup;

    property ExcludedRoutineConfList: TIdeDebuggerExcludeRoutineConfList read FExcludedRoutineConfList write SetExcludedRoutineConfList;
  end;

  { TIdeDbgExcludedRoutine_IdeOptionsFrame }

  TIdeDbgExcludedRoutine_IdeOptionsFrame = class(TAbstractIDEOptionsEditor)
  private
    IdeDbgExcludedRoutineConfFrame: TIdeDbgExcludedRoutineConfFrame;
    FExcludedRoutineConfList: TIdeDebuggerExcludeRoutineConfList;
    FChangeStamp: QWord;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AnOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AnOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

  { TIdeDbgExcludedRoutine_ProjectOptsFrame }

  TIdeDbgExcludedRoutine_ProjectOptsFrame = class(TIdeDbgExcludedRoutine_IdeOptionsFrame)
  public
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AnOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AnOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterIDEOptionsEditor(GroupDebugger, TIdeDbgExcludedRoutine_IdeOptionsFrame, DbgOptionsExcludedRoutines);
  RegisterIDEOptionsEditor(GroupProject, TIdeDbgExcludedRoutine_ProjectOptsFrame, ProjectOptionsDbgExcludedRoutines, ProjectOptionsDebug);
end;

{$R *.lfm}

{ TIdeDbgExcludedRoutineConfFrame }

procedure TIdeDbgExcludedRoutineConfFrame.SetExcludedRoutineConfList(
  AValue: TIdeDebuggerExcludeRoutineConfList);
begin
  if FExcludedRoutineConfList = AValue then Exit;
  FExcludedRoutineConfList := AValue;
  FCurrentConf := nil;

  IdeDbgConfigItemCheckListFrame1.Items := FExcludedRoutineConfList;
  IdeDbgConfigItemCheckListFrame1.Refresh;
end;

procedure TIdeDbgExcludedRoutineConfFrame.DoSelectChange(AnIndex, AnOldIndex: integer; AnObject,
  AnOldObject: TObject);
begin
  FChanging := True;
  try
    if FCurrentConf <> nil then
      SaveCurrent;
    FCurrentConf := TIdeDebuggerExcludeRoutineConf(AnObject);

    if FCurrentConf = nil then begin
      Panel1.Enabled := False;
      EdName.Text := '';
      EdPath.Text := '';
      dropPath.ItemIndex := -1;
      dropPath.Text := '';
      edFunc.Text := '';
      dropFunc.ItemIndex := -1;
      dropFunc.Text := '';
      exit;
    end;

    Panel1.Enabled := True;

    EdName.Text := FCurrentConf.Name;
    dropPath.ItemIndex := ord(FCurrentConf.FileMatchKind);
    EdPath.Text := FCurrentConf.FileMatchValue;

    dropFunc.ItemIndex := ord(FCurrentConf.NameMatchKind);
    edFunc.Text := FCurrentConf.NameMatchTextValue;
  finally
    FChanging := False;
    EditDone(nil);
  end;
end;

procedure TIdeDbgExcludedRoutineConfFrame.EdNameChange(Sender: TObject);
begin
  if not FChanging then
    IdeDbgConfigItemCheckListFrame1.CurrentName := EdName.Text;
end;

procedure TIdeDbgExcludedRoutineConfFrame.DropChanged(Sender: TObject);
begin
  if FChanging then exit;

  EditChanged(Sender);
  EditDone(Sender);
end;

procedure TIdeDbgExcludedRoutineConfFrame.EditChanged(Sender: TObject);
begin
  if FChanging then exit;

  lbWarn2.Visible := (EdPath.Text <> '') or
                     (TDbgExeProcMatchNameKind(dropFunc.ItemIndex) in [mpkRegEx]);
end;

procedure TIdeDbgExcludedRoutineConfFrame.EditDone(Sender: TObject);
begin
  if FChanging then exit;

  EditChanged(Sender);

  lbWarn3.Visible := False;

  try
    if (TDbgExeProcMatchFileKind(dropPath.ItemIndex) in [mfkFilePathRegEx]) and
       (EdPath.Text <> '')
    then begin
      ExecRegExpr(EdPath.Text, '');
    end;

    if (TDbgExeProcMatchNameKind(dropFunc.ItemIndex) in [mpkRegEx]) and
       (edFunc.Text <> '')
    then begin
      ExecRegExpr(edFunc.Text, '');
    end;
  except
    on e: exception do begin
      lbWarn3.Visible := True;
      lbWarn3.Caption := e.Message;
    end;
  end;
end;

procedure TIdeDbgExcludedRoutineConfFrame.DoAddConf;
var
  AName: String;
  n: Integer;
  NewConf: TIdeDebuggerExcludeRoutineConf;
begin
  AName := InputBox(DbgOptExclRtNewSteppingExclusion, lisName, '');

  if (AName = '') then
    exit;
//  AName := UniqueName(AName, False);

  SaveCurrent;
  FCurrentConf := nil;

  NewConf := TIdeDebuggerExcludeRoutineConf.Create;
  NewConf.Enabled := True;
  NewConf.Name := AName;
  n := FExcludedRoutineConfList.Add(NewConf);

  IdeDbgConfigItemCheckListFrame1.Refresh;
  IdeDbgConfigItemCheckListFrame1.CurrentIndex := n;
end;

procedure TIdeDbgExcludedRoutineConfFrame.DoDeleteConf(AnIndex: integer; AnObject: TObject);
begin
  FCurrentConf := nil;
  FExcludedRoutineConfList.Delete(AnIndex);
  IdeDbgConfigItemCheckListFrame1.Refresh;
end;

procedure TIdeDbgExcludedRoutineConfFrame.DoEnabledChange(AnIndex: integer; AnObject: TObject;
  ANewChecked: boolean);
begin
  if AnObject <> nil then
    TIdeDebuggerExcludeRoutineConf(AnObject).Enabled := ANewChecked;
end;

procedure TIdeDbgExcludedRoutineConfFrame.SaveCurrent;
begin
  if FCurrentConf = nil then exit;

  FCurrentConf.Name := EdName.Text;
  FCurrentConf.Enabled := IdeDbgConfigItemCheckListFrame1.CurrentChecked;

  FCurrentConf.FileMatchKind  := TDbgExeProcMatchFileKind(dropPath.ItemIndex);
  FCurrentConf.FileMatchValue := EdPath.Text;

  FCurrentConf.NameMatchKind  := TDbgExeProcMatchNameKind(dropFunc.ItemIndex);
  FCurrentConf.NameMatchTextValue := edFunc.Text;
end;

constructor TIdeDbgExcludedRoutineConfFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IdeDbgConfigItemCheckListFrame1.OnSelectionChange := @DoSelectChange;
  IdeDbgConfigItemCheckListFrame1.OnCheckedChange := @DoEnabledChange;
  IdeDbgConfigItemCheckListFrame1.OnAdd    := @DoAddConf;
  IdeDbgConfigItemCheckListFrame1.OnDelete := @DoDeleteConf;
end;

procedure TIdeDbgExcludedRoutineConfFrame.Setup;
begin
  IdeDbgConfigItemCheckListFrame1.Setup;

  lblName.Caption := DbgOptFrmName;
  lblPath.Caption := DbgOptExclRtMatchPathOrFile;
  lblFunc.Caption := DbgOptExclRtMatchFunctionName;

  dropPath.Items.Clear;
  dropPath.Items.Add(DbgOptExclRtFilenameEquals);
  dropPath.Items.Add(DbgOptExclRtPathFileRegex);
  //dropPath.Items.Add('');

  dropFunc.Items.Clear;
  dropFunc.Items.Add(DbgOptExclRtFunctionEquals);
  dropFunc.Items.Add(DbgOptExclRtFunctionRegex);
  //dropFunc.Items.Add('Address (contained)');
  //dropFunc.Items.Add('');

  chkStoreInSession.Caption := DbgOptExclRtStoreStepExclusionsInSess;
  chkStoreInSession.Hint    := DbgOptExclRtThisOnlyAffectsTheListOfE;
  chkUseGlobalList.Caption  := DbgOptExclRtUseTheIDEGlobalListOfExcl;
  chkUseProjList.Caption    := DbgOptExclRtUseTheProjectListOfExclus;

  lbWarn1.Caption := DbgOptExclRtSomeBackendsMayCompareCas;
  lbWarn2.Caption := DbgOptExclRtSomeBackendsMayOnlySuppor;
end;

{ TIdeDbgExcludedRoutine_IdeOptionsFrame }

constructor TIdeDbgExcludedRoutine_IdeOptionsFrame.Create(AnOwner: TComponent);
begin
  SetDesignInstance(True);
  inherited Create(AnOwner);
  SetDesignInstance(False);
  IdeDbgExcludedRoutineConfFrame := TIdeDbgExcludedRoutineConfFrame.Create(AnOwner);
  IdeDbgExcludedRoutineConfFrame.Align := alClient;
  IdeDbgExcludedRoutineConfFrame.Parent := Self;
end;

destructor TIdeDbgExcludedRoutine_IdeOptionsFrame.Destroy;
begin
  inherited Destroy;
  FExcludedRoutineConfList.Free;
end;

function TIdeDbgExcludedRoutine_IdeOptionsFrame.GetTitle: String;
begin
  Result := DbgOptExclRtSteppingExclusions;
end;

procedure TIdeDbgExcludedRoutine_IdeOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  IdeDbgExcludedRoutineConfFrame.Setup;
  IdeDbgExcludedRoutineConfFrame.PanelProjectOpts.Visible := False;
end;

procedure TIdeDbgExcludedRoutine_IdeOptionsFrame.ReadSettings(AnOptions: TAbstractIDEOptions);
begin
  if FExcludedRoutineConfList = nil then
    FExcludedRoutineConfList := TIdeDebuggerExcludeRoutineConfList.Create;
  FExcludedRoutineConfList.Assign(DebuggerOptions.ExcludeRoutineEntryConfig);
  FChangeStamp := FExcludedRoutineConfList.HighestChangeStamp;
  IdeDbgExcludedRoutineConfFrame.ExcludedRoutineConfList := FExcludedRoutineConfList;
end;

procedure TIdeDbgExcludedRoutine_IdeOptionsFrame.WriteSettings(AnOptions: TAbstractIDEOptions);
begin
  IdeDbgExcludedRoutineConfFrame.SaveCurrent;
  if FExcludedRoutineConfList.HighestChangeStamp <> FChangeStamp then begin
    DebuggerOptions.ExcludeRoutineEntryConfig.Assign(FExcludedRoutineConfList);
  end;
end;

class function TIdeDbgExcludedRoutine_IdeOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

{ TIdeDbgExcludedRoutine_ProjectOptsFrame }

procedure TIdeDbgExcludedRoutine_ProjectOptsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  IdeDbgExcludedRoutineConfFrame.Setup;
  IdeDbgExcludedRoutineConfFrame.PanelProjectOpts.Visible := True;
end;

procedure TIdeDbgExcludedRoutine_ProjectOptsFrame.ReadSettings(AnOptions: TAbstractIDEOptions);
begin
  if FExcludedRoutineConfList = nil then
    FExcludedRoutineConfList := TIdeDebuggerExcludeRoutineConfList.Create;
  FExcludedRoutineConfList.Assign(DbgProjectLink.ExcludeRoutineEntryConfig);
  FChangeStamp := FExcludedRoutineConfList.HighestChangeStamp;
  IdeDbgExcludedRoutineConfFrame.ExcludedRoutineConfList := FExcludedRoutineConfList;

  with IdeDbgExcludedRoutineConfFrame do begin
    chkStoreInSession.Checked := DbgProjectLink.StoreExcludeRoutineEntryConfigInSession;
    chkUseGlobalList.Checked  := DbgProjectLink.UseExcludeRoutineEntryConfigFromIDE;
    chkUseProjList.Checked    := DbgProjectLink.UseExcludeRoutineEntryConfigFromProject;
  end;
end;

procedure TIdeDbgExcludedRoutine_ProjectOptsFrame.WriteSettings(AnOptions: TAbstractIDEOptions);
begin
  IdeDbgExcludedRoutineConfFrame.SaveCurrent;
  DbgProjectLink.ExcludeRoutineEntryConfig.BeginUpdate;
  if FExcludedRoutineConfList.HighestChangeStamp <> FChangeStamp then begin
    DbgProjectLink.ExcludeRoutineEntryConfig.Assign(FExcludedRoutineConfList);
  end;

  with IdeDbgExcludedRoutineConfFrame do begin
    if (DbgProjectLink.UseExcludeRoutineEntryConfigFromIDE     <> chkUseGlobalList.Checked) or
       (DbgProjectLink.UseExcludeRoutineEntryConfigFromProject <> chkUseProjList.Checked)
    then
      DbgProjectLink.ExcludeRoutineEntryConfig.CallChangeNotifications;

    DbgProjectLink.StoreExcludeRoutineEntryConfigInSession := chkStoreInSession.Checked;
    DbgProjectLink.UseExcludeRoutineEntryConfigFromIDE     := chkUseGlobalList.Checked;
    DbgProjectLink.UseExcludeRoutineEntryConfigFromProject := chkUseProjList.Checked;
  end;

  DbgProjectLink.ExcludeRoutineEntryConfig.EndUpdate;
end;

class function TIdeDbgExcludedRoutine_ProjectOptsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

end.

