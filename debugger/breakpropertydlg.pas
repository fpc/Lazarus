unit BreakPropertyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLProc,
  ExtCtrls, StdCtrls, Buttons, ButtonPanel, EditBtn, Spin,
  IDEHelpIntf,
  DebuggerDlg, Debugger, BaseDebugManager, LazarusIDEStrConsts, InputHistory;

type

  { TBreakPropertyDlg }

  TBreakPropertyDlg = class(TDebuggerDlg)
    ButtonPanel: TButtonPanel;
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
    lblLogCallStackLimit: TLabel;
    lblMS: TLabel;
    lblFileName: TLabel;
    lblLine: TLabel;
    lblCondition: TLabel;
    lblHitCount: TLabel;
    lblGroup: TLabel;
    lblAutoContinue: TLabel;
    edtLogCallStack: TSpinEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure BreakPointRemove(const ASender: TIDEBreakPoints;
      const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointUpdate(const ASender: TIDEBreakPoints;
      const ABreakpoint: TIDEBreakPoint);
    procedure chkLogCallStackChange(Sender: TObject);
    procedure chkLogMessageChange(Sender: TObject);
  private
    FBreakpointsNotification : TIDEBreakPointsNotification;
    FBreakpoint: TIDEBreakPoint;
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

procedure TBreakPropertyDlg.chkLogCallStackChange(Sender: TObject);
begin
  edtLogCallStack.Enabled := chkLogCallStack.Checked;
end;

procedure TBreakPropertyDlg.chkLogMessageChange(Sender: TObject);
begin
  edtLogMessage.Enabled := chkLogMessage.Checked;
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
var
  Actions: TIDEBreakPointActions;
  GroupName: String;
  NewGroup: TIDEBreakPointGroup;
begin
  if FBreakpoint = nil then Exit;

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
  // actions
  Actions := [];
  if chkActionBreak.Checked then Include(Actions, bpaStop);
  if chkDisableGroups.Checked then Include(Actions, bpaDisableGroup);
  if chkEnableGroups.Checked then Include(Actions, bpaEnableGroup);
//  if chkEvalExpression.Checked then Include(Actions, bpaEValExpression);
  if chkLogMessage.Checked then Include(Actions, bpaLogMessage);
  if chkLogCallStack.Checked then Include(Actions, bpaLogCallStack);
  FBreakpoint.Actions := Actions;
  FBreakpoint.LogMessage := edtLogMessage.Text;
  FBreakpoint.LogCallStackLimit := edtLogCallStack.Value;

  InputHistories.HistoryLists.GetList('BreakPointExpression', True).Add(edtCondition.Text);
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
begin
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

  // actions
  Actions := FBreakpoint.Actions;
  chkActionBreak.Checked := bpaStop in Actions;
  chkDisableGroups.Checked := bpaDisableGroup in Actions;
  chkEnableGroups.Checked := bpaEnableGroup in Actions;
//  chkEvalExpression.Checked := bpaEValExpression in Actions;
  chkLogMessage.Checked := bpaLogMessage in Actions;
  edtLogMessage.Text := FBreakpoint.LogMessage;
  chkLogCallStack.Checked := bpaLogCallStack in Actions;
  edtLogCallStack.Value := FBreakpoint.LogCallStackLimit;
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
  end;
  lblCondition.Caption := lisCondition + ':';
  lblHitCount.Caption := lisHitCount + ':';
  lblAutoContinue.Caption := lisAutoContinueAfter;
  lblMS.Caption := lisMS;
  lblGroup.Caption := lisGroup + ':';
  gbActions.Caption := lisActions;
  chkActionBreak.Caption := lisBreak;
  chkEnableGroups.Caption := lisEnableGroup;
  chkDisableGroups.Caption := lisDisableGroup;
  chkEvalExpression.Caption := lisEvalExpression;
  chkLogMessage.Caption := lisLogMessage;
  chkLogCallStack.Caption := lisLogCallStack;
  lblLogCallStackLimit.Caption := lisLogCallStackLimit;
  edtCondition.Items.Assign(InputHistories.HistoryLists.GetList('BreakPointExpression', True));

  FBreakpoint := ABreakPoint;
  FBreakpointsNotification := TIDEBreakPointsNotification.Create;
  FBreakpointsNotification.AddReference;
  FBreakpointsNotification.OnUpdate := @BreakPointUpdate;
  FBreakpointsNotification.OnRemove := @BreakPointRemove;
  UpdateInfo;

  ButtonPanel.OKButton.Caption:=lisOk;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CancelButton.Caption:=dlgCancel;
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

