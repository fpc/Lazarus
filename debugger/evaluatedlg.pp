{ $Id$ }
{               ----------------------------------------------
                 evaluatedlg.pp  -  Evaluate and Modify
                ----------------------------------------------

 @created(Mon Nov 22st WET 2004)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@freepascal.org>)

 This unit contains the evaluate and modify dialog.


 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

unit EvaluateDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, ComCtrls, StdCtrls, Menus, Dialogs, ExtCtrls,
  Buttons,
  // IdeIntf
  IDEWindowIntf, IDEImagesIntf,
  // DebuggerIntf
  DbgIntfDebuggerBase, LazClasses, LazDebuggerIntf, LazDebuggerIntfBaseTypes,
  // IDE
  LazarusIDEStrConsts, BaseDebugManager, InputHistory, IDEProcs, Debugger,
  IdeDebuggerWatchResPrinter, IdeDebuggerWatchResult, IdeDebuggerOpts,
  IdeDebuggerFpDbgValueConv, WatchInspectToolbar, DebuggerDlg, DebuggerStrConst,
  IdeDebuggerStringConstants, IdeDebuggerBase, EnvironmentOpts;

type

  { TEvaluateDlg }

  TEvaluateDlg = class(TDebuggerDlg)
    Panel1: TPanel;
    EdModify: TComboBox;
    BtnExecModify: TSpeedButton;
    txtResult: TMemo;
    WatchInspectNav1: TWatchInspectNav;
    procedure BtnExecModifyClick(Sender: TObject);
    procedure EdModifyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
  private
    FWatchPrinter: TWatchResultPrinter;

    procedure DoAddInspect(Sender: TObject);
    procedure DoAddWatch(Sender: TObject);
    function DoBeforeUpdate(ASender: TObject): boolean;
    procedure DoDebuggerState(ADebugger: TDebuggerIntf; AnOldState: TDBGState);
    procedure DoDispFormatChanged(Sender: TObject);
    procedure DoEnvOptChanged(Sender: TObject; Restore: boolean);
    procedure DoHistDirChanged(Sender: TObject; NewDir: TEvalHistDirection);
    procedure DoWatchesInvalidated(Sender: TObject);
    procedure DoWatchUpdated(const ASender: TIdeWatches; const AWatch: TIdeWatch);
    function GetEvalExpression: string;
    procedure SetEvalExpression(const NewExpression: string);
    procedure Modify;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const AExpression: String; AWatch: TWatch = nil);
    property EvalExpression: string read GetEvalExpression write SetEvalExpression;
  end;

implementation

{$R *.lfm}

var
  EvaluateDlgWindowCreator: TIDEWindowCreator;

const
  RESULTSEPARATOR='-----------';
  RESULTEVAL='>>>> ';

{ TEvaluateDlg }

constructor TEvaluateDlg.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);

  ThreadsMonitor := DebugBoss.Threads;
  CallStackMonitor := DebugBoss.CallStack;
  WatchesMonitor := DebugBoss.Watches;
  WatchesNotification.OnUpdate    := @DoWatchUpdated;

  DebugBoss.RegisterStateChangeHandler(@DoDebuggerState);
  DebugBoss.RegisterWatchesInvalidatedHandler(@DoWatchesInvalidated);

  FWatchPrinter := TWatchResultPrinter.Create;

  WatchInspectNav1.Init(WatchesMonitor, ThreadsMonitor, CallStackMonitor, []);
  WatchInspectNav1.HistoryList := InputHistories.HistoryLists.
    GetList(ClassName,True,rltCaseSensitive);

  Caption := lisKMEvaluateModify;
  EdModify.TextHint := drsNewValue;
  EdModify.Hint := drsNewValueToAssignToTheVari;
  Panel1.Enabled := False;

  WatchInspectNav1.btnUseInstance.Down := EnvironmentOptions.DebuggerAutoSetInstanceFromClass;

  WatchInspectNav1.ShowInspectColumns := False;
  WatchInspectNav1.ShowArrayNav := False;
  WatchInspectNav1.ShowEvalHist := True;
  WatchInspectNav1.ShowAddEval:= False;
  WatchInspectNav1.ShowDisplayFormat := True;

  WatchInspectNav1.OnAddWatchClicked := @DoAddWatch;
  WatchInspectNav1.OnAddInspectClicked := @DoAddInspect;

  WatchInspectNav1.OnEvalHistDirectionChanged := @DoHistDirChanged;
  WatchInspectNav1.OnDisplayFormatChanged := @DoDispFormatChanged;

  //Clear;
  //WatchInspectNav1.OnClear := @DoClear;
  WatchInspectNav1.OnBeforeEvaluate := @DoBeforeUpdate;
  WatchInspectNav1.OnWatchUpdated := @DoWatchUpdated;

  EnvironmentOptions.AddHandlerAfterWrite(@DoEnvOptChanged);
  DoEnvOptChanged(nil, False);

end;

destructor TEvaluateDlg.Destroy;
begin
  DebugBoss.UnregisterStateChangeHandler(@DoDebuggerState);
  DebugBoss.UnregisterWatchesInvalidatedHandler(@DoWatchesInvalidated);
  EnvironmentOptions.RemoveHandlerAfterWrite(@DoEnvOptChanged);

  FreeAndNil(FWatchPrinter);
  inherited Destroy;
end;

procedure TEvaluateDlg.Execute(const AExpression: String; AWatch: TWatch);
begin
  if AWatch <> nil then
    WatchInspectNav1.ReadFromWatch(AWatch, AExpression)
  else
    SetEvalExpression(AExpression);
end;

procedure TEvaluateDlg.DoAddWatch(Sender: TObject);
var
  w: TCurrentWatch;
begin
  if DebugBoss = nil then
    exit;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
  try
    w := DebugBoss.Watches.CurrentWatches.Find(WatchInspectNav1.Expression);
    if w = nil then
      w := DebugBoss.Watches.CurrentWatches.Add(WatchInspectNav1.Expression);
    if (w <> nil) then begin
      WatchInspectNav1.InitWatch(w);
      w.Enabled := True;
      DebugBoss.ViewDebugDialog(ddtWatches, False);
    end;
  finally
    DebugBoss.Watches.CurrentWatches.EndUpdate;
  end;
end;

function TEvaluateDlg.DoBeforeUpdate(ASender: TObject): boolean;
begin
  Result := DebugBoss.State = dsPause;
end;

procedure TEvaluateDlg.DoDebuggerState(ADebugger: TDebuggerIntf;
  AnOldState: TDBGState);
begin
  if (not WatchInspectNav1.PowerIsDown) or (not Visible) then exit;
  if (ADebugger.State = dsPause) and (AnOldState <> dsPause) then begin
    WatchInspectNav1.UpdateData(True);
  end;
end;

procedure TEvaluateDlg.DoDispFormatChanged(Sender: TObject);
begin
  if (WatchInspectNav1.CurrentWatchValue = nil) or (WatchInspectNav1.CurrentWatchValue.Watch = nil)
  then begin
    if (not WatchInspectNav1.PowerIsDown) or (not Visible) then exit;
    WatchInspectNav1.UpdateData;
    exit;
  end;

  DoWatchUpdated(WatchInspectNav1.Watches, WatchInspectNav1.CurrentWatchValue.Watch);
end;

procedure TEvaluateDlg.DoEnvOptChanged(Sender: TObject; Restore: boolean);
begin
  WatchInspectNav1.ShowCallFunction := EnvironmentOptions.DebuggerAllowFunctionCalls;
  WatchInspectNav1.EdInspect.DropDownCount := EnvironmentOptions.DropDownCount;
  EdModify.DropDownCount := EnvironmentOptions.DropDownCount;
end;

procedure TEvaluateDlg.DoHistDirChanged(Sender: TObject;
  NewDir: TEvalHistDirection);
begin
  txtResult.Lines.Clear;
end;

procedure TEvaluateDlg.DoWatchesInvalidated(Sender: TObject);
begin
  if (not WatchInspectNav1.PowerIsDown) or (not Visible) then exit;
  WatchInspectNav1.UpdateData(True);
end;

procedure TEvaluateDlg.DoWatchUpdated(const ASender: TIdeWatches;
  const AWatch: TIdeWatch);
var
  expr, ResultText: String;
begin
  if (WatchInspectNav1.CurrentWatchValue = nil) or
     not (WatchInspectNav1.CurrentWatchValue.Validity in [ddsError, ddsInvalid, ddsValid])
  then
    exit;
  if (AWatch <> WatchInspectNav1.CurrentWatchValue.Watch) or
     (ASender <> WatchInspectNav1.Watches)
  then
    exit;


  expr := WatchInspectNav1.Expression;
  ResultText := '';

  if WatchInspectNav1.CurrentWatchValue.Validity = ddsValid then begin
    ResultText := FWatchPrinter.PrintWatchValue(WatchInspectNav1.CurrentWatchValue.ResultData, WatchInspectNav1.DisplayFormat);
    if (WatchInspectNav1.CurrentWatchValue.ResultData <> nil) and
       (WatchInspectNav1.CurrentWatchValue.ResultData.ValueKind = rdkArray) and (WatchInspectNav1.CurrentWatchValue.ResultData.ArrayLength > 0)
    then
      ResultText := Format(drsLen, [WatchInspectNav1.CurrentWatchValue.ResultData.ArrayLength]) + ResultText
    else
    if (WatchInspectNav1.CurrentWatchValue.TypeInfo <> nil) and
       (WatchInspectNav1.CurrentWatchValue.TypeInfo.Attributes * [saArray, saDynArray] <> []) and
       (WatchInspectNav1.CurrentWatchValue.TypeInfo.Len >= 0)
    then
      ResultText := Format(drsLen, [WatchInspectNav1.CurrentWatchValue.TypeInfo.Len]) + ResultText;
  end
  else
    ResultText := WatchInspectNav1.CurrentWatchValue.Value;

  Panel1.Enabled := WatchInspectNav1.CurrentWatchValue.Validity = ddsValid;

  if WatchInspectNav1.EvalHistDirection <> EHDNone then
    begin
    if txtResult.Lines.Text='' then
      txtResult.Lines.Text := RESULTEVAL+ expr+':'+LineEnding+ ResultText + LineEnding
    else
      if WatchInspectNav1.EvalHistDirection = EHDUp then
        txtResult.Lines.Text := RESULTEVAL+ expr+':'+LineEnding+ ResultText + LineEnding
           + RESULTSEPARATOR + LineEnding + txtResult.Lines.Text
      else
        begin
        txtResult.Lines.Text := txtResult.Lines.Text + RESULTSEPARATOR + LineEnding
           + RESULTEVAL+ expr+':'+LineEnding+ ResultText+LineEnding;
        txtResult.SelStart:=length(txtResult.Lines.Text);
        end;
    end
  else
    txtResult.Lines.Text := ResultText;
end;

procedure TEvaluateDlg.DoAddInspect(Sender: TObject);
var
  w: TIdeWatch;
begin
  w := nil;
  if WatchInspectNav1.CurrentWatchValue <> nil then
    w := WatchInspectNav1.CurrentWatchValue.Watch;
  DebugBoss.Inspect(WatchInspectNav1.Expression, w);
end;

procedure TEvaluateDlg.SetEvalExpression(const NewExpression: string);
begin
  if NewExpression<>'' then
    WatchInspectNav1.Execute(NewExpression);
  WatchInspectNav1.FocusEnterExpression;
end;

function TEvaluateDlg.GetEvalExpression: string;
begin
  Result := WatchInspectNav1.Expression;
end;

procedure TEvaluateDlg.Modify;
var
  S, V: String;
begin
  S := Trim(WatchInspectNav1.Expression);
  if S = '' then Exit;
  V := EdModify.Text;
  if not DebugBoss.Modify(S, V) then begin
    MessageDlg(lisCCOErrorCaption, synfTheDebuggerWasNotAbleToModifyTheValue, mtError, [mbOK],
      0);
    Exit;
  end;

  if EdModify.Items.IndexOf(V) = -1
  then EdModify.Items.Insert(0, V);

  WatchInspectNav1.UpdateData(True);
end;

procedure TEvaluateDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TEvaluateDlg.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,400,300);
end;

procedure TEvaluateDlg.EdModifyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift * [ssShift, ssCtrl, ssAlt] = [ssShift]) and
     (EdModify.Text <> '')
  then begin
    Modify;
    Key := 0;
  end;
end;

procedure TEvaluateDlg.BtnExecModifyClick(Sender: TObject);
begin
  if EdModify.Text = '' then begin
    MessageDlg(lisCCOErrorCaption, synfNewValueIsEmpty, mtError, [mbOK], 0);
    exit;
  end;
  Modify;
end;

procedure TEvaluateDlg.FormShow(Sender: TObject);
begin
  WatchInspectNav1.FocusEnterExpression;
end;

procedure TEvaluateDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (not Docked) and
     ( (not WatchInspectNav1.DropDownOpen) or
       (EdModify.DroppedDown and EdModify.Focused) )
  then
    Close
  else
    inherited;
end;

procedure TEvaluateDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbExtra1 then
    WatchInspectNav1.GoPrevBrowseEntry
  else
  if Button = mbExtra2 then
    WatchInspectNav1.GoNextBrowseEntry;
end;

initialization

  EvaluateDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtEvaluate]);
  EvaluateDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  EvaluateDlgWindowCreator.CreateSimpleLayout;

end.


