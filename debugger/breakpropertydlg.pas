unit BreakPropertyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ButtonPanel, EditBtn, Spin,
  // IdeIntf
  IDEHelpIntf,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IDE
  BreakPropertyDlgGroups, DebuggerDlg, Debugger,
  BaseDebugManager, LazarusIDEStrConsts, InputHistory, IDEProcs, EnvironmentOpts;

type

  { TBreakPropertyDlg }

  TBreakPropertyDlg = class(TDebuggerDlg)
    ButtonPanel: TButtonPanel;
    chkTakeSnap: TCheckBox;
    chkLogCallStack: TCheckBox;
    chkEnableGroups: TCheckBox;
    chkDisableGroups: TCheckBox;
    chkEvalExpression: TCheckBox;
    chkLogMessage: TCheckBox;
    chkActionBreak: TCheckBox;
    cmbGroup: TComboBox;
    edtCondition: TComboBox;
    edtEvalExpression: TEdit;
    edtLine: TSpinEdit;
    edtLogMessage: TEdit;
    edtEnableGroups: TEditButton;
    edtDisableGroups: TEditButton;
    edtAutocontinueMS: TEdit;
    edtCounter: TEdit;
    edtFilename: TEdit;
    gbActions: TGroupBox;
    Label1: TLabel;
    lblWatchKind: TLabel;
    lblWatchScope: TLabel;
    lblLogCallStackLimit: TLabel;
    lblMS: TLabel;
    lblFileName: TLabel;
    lblLine: TLabel;
    lblCondition: TLabel;
    lblHitCount: TLabel;
    lblGroup: TLabel;
    lblAutoContinue: TLabel;
    edtLogCallStack: TSpinEdit;
    rbWrite: TRadioButton;
    rbRead: TRadioButton;
    rbReadWrite: TRadioButton;
    rbGlobal: TRadioButton;
    rbLocal: TRadioButton;
    rgWatchKind: TPanel;
    rgWatchScope: TPanel;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure BreakPointRemove(const {%H-}ASender: TIDEBreakPoints;
      const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointUpdate(const {%H-}ASender: TIDEBreakPoints;
      const {%H-}ABreakpoint: TIDEBreakPoint);
    procedure chkDisableGroupsChange(Sender: TObject);
    procedure chkEnableGroupsChange(Sender: TObject);
    procedure chkEvalExpressionChange(Sender: TObject);
    procedure chkLogCallStackChange(Sender: TObject);
    procedure chkLogMessageChange(Sender: TObject);
    procedure cmbGroupKeyPress(Sender: TObject; var Key: char);
    procedure edtDisableGroupsButtonClick(Sender: TObject);
    procedure edtEnableGroupsButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBreakpointsNotification : TIDEBreakPointsNotification;
    FBreakpoint: TIDEBreakPoint;
    FUpdatingInfo: Boolean;
  protected
    procedure DoEndUpdate; override;
    procedure UpdateInfo;
  public
    constructor Create(AOwner: TComponent; ABreakPoint: TIDEBreakPoint);overload;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TBreakPropertyDlg }

procedure TBreakPropertyDlg.BreakPointUpdate(
  const ASender: TIDEBreakPoints; const ABreakpoint: TIDEBreakPoint);
begin
  UpdateInfo;
end;

procedure TBreakPropertyDlg.chkDisableGroupsChange(Sender: TObject);
begin
  edtDisableGroups.Enabled := chkDisableGroups.Checked;
end;

procedure TBreakPropertyDlg.chkEnableGroupsChange(Sender: TObject);
begin
  edtEnableGroups.Enabled := chkEnableGroups.Checked;
end;

procedure TBreakPropertyDlg.chkEvalExpressionChange(Sender: TObject);
begin
  edtEvalExpression.Enabled := chkEvalExpression.Checked;
end;

procedure TBreakPropertyDlg.chkLogCallStackChange(Sender: TObject);
begin
  edtLogCallStack.Enabled := chkLogCallStack.Checked;
end;

procedure TBreakPropertyDlg.chkLogMessageChange(Sender: TObject);
begin
  edtLogMessage.Enabled := chkLogMessage.Checked;
end;

procedure TBreakPropertyDlg.cmbGroupKeyPress(Sender: TObject; var Key: char);
begin
  if Key = ';' then Key := #0;
end;

procedure TBreakPropertyDlg.edtDisableGroupsButtonClick(Sender: TObject);
var
  s: TCaption;
begin
  if FBreakpoint = nil then Exit;
  s := edtDisableGroups.Text;
  if ExecuteBreakPointGroupDlg(FBreakpoint, s, DebugBoss.BreakPointGroups, bgaDisable) = mrok
  then edtDisableGroups.Text := s;
end;

procedure TBreakPropertyDlg.edtEnableGroupsButtonClick(Sender: TObject);
var
  s: TCaption;
begin
  if FBreakpoint = nil then Exit;
  s := edtEnableGroups.Text;
  if ExecuteBreakPointGroupDlg(FBreakpoint, s, DebugBoss.BreakPointGroups, bgaEnable) = mrok
  then edtEnableGroups.Text := s;
end;

procedure TBreakPropertyDlg.FormCreate(Sender: TObject);
begin
  edtCondition.DropDownCount := EnvironmentOptions.DropDownCount;
  cmbGroup.DropDownCount := EnvironmentOptions.DropDownCount;
end;

procedure TBreakPropertyDlg.btnHelpClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TBreakPropertyDlg.BreakPointRemove(
  const ASender: TIDEBreakPoints; const ABreakpoint: TIDEBreakPoint);
begin
  if ABreakpoint = FBreakpoint
  then ModalResult := mrCancel;
end;

procedure TBreakPropertyDlg.btnOKClick(Sender: TObject);
  function CheckGroupList(Alist: TStringList): Boolean;
  var
    i: Integer;
    r: TModalResult;
    s: String;
    NewGroup: TIDEBreakPointGroup;
  begin
    Result := True;
    s := '';
  for i := 0 to Alist.Count-1 do begin
      if Alist[i] = '' then continue;
      if DebugBoss.BreakPointGroups.GetGroupByName(Alist[i]) = nil then
        s := s + ', ' + Alist[i];
    end;
    if s = '' then exit;
    delete(s, 1, 2);
    r := MessageDlg(Format(dbgBreakPropertyGroupNotFound, [LineEnding, s]),
                    mtConfirmation, [mbYes, mbIgnore, mbCancel], 0);
    if r = mrCancel then exit(False);
    if r = mrYes then begin
      for i := 0 to Alist.Count-1 do begin
        if Alist[i] = '' then continue;
        if DebugBoss.BreakPointGroups.GetGroupByName(Alist[i]) = nil then begin
          NewGroup := TIDEBreakPointGroup(DebugBoss.BreakPointGroups.Add);
          NewGroup.Name := Alist[i];
        end;
      end;
    end;
  end;
var
  Actions: TIDEBreakPointActions;
  GroupName: String;
  NewGroup: TIDEBreakPointGroup;
  ws: TDBGWatchPointScope;
  wk: TDBGWatchPointKind;
  i: SizeInt;
  EnableGroupList, DisableGroupList: TStringList;
begin
  if FBreakpoint = nil then Exit;

  EnableGroupList := TStringList.Create;
  DisableGroupList := TStringList.Create;

  try
    EnableGroupList.Delimiter := ';';
    DisableGroupList.Delimiter := ';';

    EnableGroupList.DelimitedText := edtEnableGroups.Text+';'+edtDisableGroups.Text;
    if not CheckGroupList(EnableGroupList) then begin
      ModalResult :=  mrNone;
      exit;
    end;

    EnableGroupList.DelimitedText := edtEnableGroups.Text;
    DisableGroupList.DelimitedText := edtDisableGroups.Text;

    FBreakpointsNotification.OnUpdate := nil;
    case FBreakpoint.Kind of
      bpkSource:
        begin
          // filename + line
          FBreakpoint.SetLocation(edtFilename.Text, edtLine.Value);
        end;
      bpkAddress:
        begin
          FBreakpoint.SetAddress(StrToQWordDef(edtFilename.Text, 0));
        end;
      bpkData:
        begin
          if rbGlobal.Checked
          then ws := wpsGlobal
          else ws := wpsLocal;
          wk := wpkWrite;
          if rbRead.Checked
          then wk := wpkRead;
          if rbReadWrite.Checked
          then wk := wpkReadWrite;
          FBreakpoint.SetWatch(edtFilename.Text, ws, wk);
        end;
    end;
    // expression
    FBreakpoint.Expression := edtCondition.Text;
    // hitcount
    FBreakpoint.BreakHitCount := StrToIntDef(edtCounter.Text, FBreakpoint.HitCount);
    //auto continue
    FBreakpoint.AutoContinueTime := StrToIntDef(edtAutocontinueMS.Text, FBreakpoint.AutoContinueTime);
    // group
    GroupName := cmbGroup.Text;
    NewGroup := DebugBoss.BreakPointGroups.GetGroupByName(GroupName);
    if not Assigned(NewGroup) and (GroupName <> '') then
    begin
      NewGroup := TIDEBreakPointGroup(DebugBoss.BreakPointGroups.Add);
      NewGroup.Name := GroupName;
    end;
    FBreakpoint.Group := NewGroup;
    // enable groups
    for i := 0 to DebugBoss.BreakPointGroups.Count-1 do begin
      NewGroup := DebugBoss.BreakPointGroups[i];
      if EnableGroupList.IndexOf(NewGroup.Name) >= 0
      then FBreakpoint.EnableGroupList.Add(NewGroup)
      else FBreakpoint.EnableGroupList.Remove(NewGroup);
    end;
    // disable groups
    for i := 0 to DebugBoss.BreakPointGroups.Count-1 do begin
      NewGroup := DebugBoss.BreakPointGroups[i];
      if DisableGroupList.IndexOf(NewGroup.Name) >= 0
      then FBreakpoint.DisableGroupList.Add(NewGroup)
      else FBreakpoint.DisableGroupList.Remove(NewGroup);
    end;
    // actions
    Actions := [];
    if chkActionBreak.Checked then Include(Actions, bpaStop);
    if chkDisableGroups.Checked then Include(Actions, bpaDisableGroup);
    if chkEnableGroups.Checked then Include(Actions, bpaEnableGroup);
    if chkEvalExpression.Checked then Include(Actions, bpaEValExpression);
    if chkLogMessage.Checked then Include(Actions, bpaLogMessage);
    if chkLogCallStack.Checked then Include(Actions, bpaLogCallStack);
    if chkTakeSnap.Checked then include(Actions, bpaTakeSnapshot);
    FBreakpoint.Actions := Actions;
    FBreakpoint.LogEvalExpression := edtEvalExpression.Text;
    FBreakpoint.LogMessage := edtLogMessage.Text;
    FBreakpoint.LogCallStackLimit := edtLogCallStack.Value;

    if edtCondition.Text <> '' then
      with InputHistories.HistoryLists.GetList('BreakPointExpression',
        True, rltCaseSensitive) do
      begin
        i := IndexOf(edtCondition.Text);
        if i <> -1 then Delete(i);
        Insert(0, edtCondition.Text);
      end;

  finally
    EnableGroupList.Free;
    DisableGroupList.Free;
  end;
end;

procedure TBreakPropertyDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;
  UpdateInfo;
end;

procedure TBreakPropertyDlg.UpdateInfo;
var
  Actions: TIDEBreakPointActions;
  I: Integer;
  s: String;
begin
  FUpdatingInfo := True;
  if FBreakpoint = nil then Exit;
  case FBreakpoint.Kind of
    bpkSource:
      begin
        // filename
        edtFilename.Text := FBreakpoint.Source;
        // line
        if FBreakpoint.Line > 0
        then edtLine.Value := FBreakpoint.Line
        else edtLine.Value := 0;
      end;
    bpkAddress:
      begin
        edtFilename.Text := '$' + IntToHex(FBreakpoint.Address, 8); // todo: 8/16 depends on platform
      end;
    bpkData:
      begin
        edtFilename.Text := FBreakpoint.WatchData;
        rbGlobal.Checked := FBreakpoint.WatchScope = wpsGlobal;
        rbLocal.Checked := FBreakpoint.WatchScope = wpsLocal;
        rbWrite.Checked := FBreakpoint.WatchKind = wpkWrite;
        rbRead.Checked := FBreakpoint.WatchKind = wpkRead;
        rbReadWrite.Checked := FBreakpoint.WatchKind = wpkReadWrite;
      end;
  end;
  // expression
  edtCondition.Text := FBreakpoint.Expression;
  // hitcount
  edtCounter.Text := IntToStr(FBreakpoint.BreakHitCount);
  // auto continue
  edtAutocontinueMS.Text := IntToStr(FBreakpoint.AutoContinueTime);
  // group
  for I := 0 to DebugBoss.BreakPointGroups.Count - 1 do
    cmbGroup.Items.Add(DebugBoss.BreakPointGroups[I].Name);
  if FBreakpoint.Group = nil
  then cmbGroup.Text := ''
  else cmbGroup.Text := FBreakpoint.Group.Name;
  // enable groups
  s := '';
  for i := 0 to FBreakpoint.EnableGroupList.Count - 1 do begin
    if s <> '' then s := s + ';';
    s := s + FBreakpoint.EnableGroupList[i].Name;
  end;
  edtEnableGroups.Text := s;
  // disable groups
  s := '';
  for i := 0 to FBreakpoint.DisableGroupList.Count - 1 do begin
    if s <> '' then s := s + ';';
    s := s + FBreakpoint.DisableGroupList[i].Name;
  end;
  edtDisableGroups.Text := s;

  // actions
  Actions := FBreakpoint.Actions;
  chkActionBreak.Checked := bpaStop in Actions;
  chkDisableGroups.Checked := bpaDisableGroup in Actions;
  chkEnableGroups.Checked := bpaEnableGroup in Actions;
  chkEvalExpression.Checked := bpaEValExpression in Actions;
  chkLogMessage.Checked := bpaLogMessage in Actions;
  edtLogMessage.Text := FBreakpoint.LogMessage;
  edtEvalExpression.Text := FBreakpoint.LogEvalExpression;
  chkLogCallStack.Checked := bpaLogCallStack in Actions;
  edtLogCallStack.Value := FBreakpoint.LogCallStackLimit;
  chkTakeSnap.Checked := bpaTakeSnapshot in Actions;
  FUpdatingInfo := False;
end;

constructor TBreakPropertyDlg.Create(AOwner: TComponent; ABreakPoint: TIDEBreakPoint);
begin
  inherited Create(AOwner);

  Caption := lisBreakPointProperties;
  case ABreakPoint.Kind of
    bpkSource:
      begin
        lblFileName.Caption := lisPEFilename;
        lblLine.Caption := lisLine;
      end;
    bpkAddress:
      begin
        lblFileName.Caption := lisAddress;
        lblLine.Visible := False;
        edtLine.Visible := False;
        edtFilename.ReadOnly := False;
        edtFilename.Color := clDefault;
      end;
    bpkData:
      begin
        lblFileName.Caption := lisWatchData;
        lblLine.Visible := False;
        edtLine.Visible := False;
        edtFilename.ReadOnly := False;
        edtFilename.Color := clDefault;
        lblWatchKind.Visible := True;
        lblWatchScope.Visible := True;
        rgWatchKind.Visible := True;
        rgWatchScope.Visible := True;
        lblWatchScope.Caption := lisWatchScope;
        lblWatchKind.Caption := lisWatchKind;
        rbGlobal.Caption := lisWatchScopeGlobal;
        rbLocal.Caption := lisWatchScopeLocal;
        rbWrite.Caption := lisWatchKindWrite;
        rbRead.Caption := lisWatchKindRead;
        rbReadWrite.Caption := lisWatchKindReadWrite;
      end;
  end;
  lblCondition.Caption := lisCondition + ':';
  lblHitCount.Caption := lisHitCount + ':';
  lblAutoContinue.Caption := lisAutoContinueAfter;
  lblMS.Caption := lisMS;
  lblGroup.Caption := lisGroup + ':';
  gbActions.Caption := lisActions;
  chkActionBreak.Caption := lisBreak;
  chkEnableGroups.Caption := lisEnableGroups;
  chkDisableGroups.Caption := lisDisableGroups;
  chkEvalExpression.Caption := lisEvalExpression;
  chkLogMessage.Caption := lisLogMessage;
  chkLogCallStack.Caption := lisLogCallStack;
  lblLogCallStackLimit.Caption := lisLogCallStackLimit;
  chkTakeSnap.Caption := lisTakeSnapshot;
  edtCondition.Items.Assign(InputHistories.HistoryLists.GetList(
    'BreakPointExpression', True,rltCaseSensitive));

  FBreakpoint := ABreakPoint;
  FBreakpointsNotification := TIDEBreakPointsNotification.Create;
  FBreakpointsNotification.AddReference;
  FBreakpointsNotification.OnUpdate := @BreakPointUpdate;
  FBreakpointsNotification.OnRemove := @BreakPointRemove;
  UpdateInfo;

  ButtonPanel.OKButton.Caption:=lisMenuOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=lisCancel;
end;

destructor TBreakPropertyDlg.Destroy;
begin
  FBreakpointsNotification.OnUpdate := nil;
  FBreakpointsNotification.OnRemove := nil;
  FBreakpointsNotification.ReleaseReference;
  FBreakpointsNotification := nil;
  inherited Destroy;
end;

end.

