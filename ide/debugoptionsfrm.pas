unit DebugOptionsFrm;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Menus, Spin, BaseDebugManager, Debugger,
  CheckLst;

type
  TDebuggerOptionsForm = class (TForm )
    clbExceptions: TCHECKLISTBOX;
    chkMessagesInterface: TCHECKBOX;
    chkClearLogOnRun: TCHECKBOX;
    chkLimitLinecount: TCHECKBOX;
    chkMessagesBreakpoint: TCHECKBOX;
    chkMessagesProcess: TCHECKBOX;
    chkMessagesThread: TCHECKBOX;
    chkMessagesModule: TCHECKBOX;
    chkMessagesOutput: TCHECKBOX;
    chkMessagesWindow: TCHECKBOX;
    cmdOpenDebuggerPath: TBUTTON;
    cmdOpenAdditionalPath: TBUTTON;
    cmdCancel: TBUTTON;
    cmdOK: TBUTTON;
    cmdExceptionRemove: TBUTTON;
    cmdExceptionAdd: TBUTTON;
    cmdSignalRemove: TBUTTON;
    cmdSignalAdd: TBUTTON;
    chkBreakOnException: TCHECKBOX;
    cmbDebuggerType: TCOMBOBOX;
    cmbDebuggerPath: TCOMBOBOX;
    N1: TMENUITEM;
    seLimitLinecount: TSPINEDIT;
    txtAdditionalPath: TEDIT;
    gbDebuggerType: TGROUPBOX;
    gbAdditionalSearchPath: TGROUPBOX;
    gbDebuggerSpecific: TGROUPBOX;
    gbGeneral: TGROUPBOX;
    gbMessages: TGROUPBOX;
    bgIgnoreExceptions: TGROUPBOX;
    gbSignals: TGROUPBOX;
    lvSignals: TLISTVIEW;
    mnuResumeUnhandled: TMENUITEM;
    mnuHandledByProgram: TMENUITEM;
    mnuiHandledByDebugger: TMENUITEM;
    mnuResumeHandled: TMENUITEM;
    nbDebugOptions: TNOTEBOOK;
    pgSignals: TPAGE;
    pgExceptions: TPAGE;
    pgEventLog: TPAGE;
    pgGeneral: TPAGE;
    popSignal: TPOPUPMENU;
    procedure DebuggerOptionsFormCREATE(Sender: TObject);
    procedure DebuggerOptionsFormDESTROY(Sender: TObject);
    procedure clbExceptionsCLICK (Sender: TObject );
    procedure cmdExceptionAddCLICK (Sender: TObject );
    procedure cmdExceptionRemoveCLICK (Sender: TObject );
    procedure cmdOKCLICK (Sender: TObject );
  private
    FExceptionDeleteList: TStringList;
    procedure AddExceptionLine(const AException: TIDEException; AName: String);
    procedure AddSignalLine(const ASignal: TIDESignal);
  public
  end;

var
  DebuggerOptionsForm: TDebuggerOptionsForm;

implementation

const
  HANDLEDBY_CAPTION: array [Boolean] of String = ('Program', 'Debugger');
  RESUME_CAPTION: array[Boolean] of String = ('Unhandled', 'Handled');

{ TDebuggerOptionsForm }

procedure TDebuggerOptionsForm.AddExceptionLine(const AException: TIDEException; AName: String);
var
  idx: Integer;
begin
  if (AName = '') and (AException <> nil)
  then AName := AException.Name;
  if AName = '' then Exit;

  idx := clbExceptions.Items.AddObject(AName, AException);
  clbExceptions.Checked[idx] := (AException = nil) or AException.Enabled;
end;

procedure TDebuggerOptionsForm.AddSignalLine(const ASignal: TIDESignal);
var
  Item: TListItem;
begin
  Item := lvSignals.Items.Add;
  Item.Caption := ASignal.Name;
  Item.SubItems.Add(IntToStr(ASignal.ID));
  Item.SubItems.Add(HANDLEDBY_CAPTION[ASignal.HandledByDebugger]);
  Item.SubItems.Add(RESUME_CAPTION[ASignal.ResumeHandled]);
  Item.Data := ASignal;
end;

procedure TDebuggerOptionsForm.clbExceptionsCLICK (Sender: TObject );
begin
  cmdExceptionRemove.Enabled :=  clbExceptions.ItemIndex <> -1;
end;

procedure TDebuggerOptionsForm.cmdExceptionAddCLICK(Sender: TObject);
var
  idx: Integer;
  S: String;
begin
  if not InputQuery('Add Exception', 'Enter the name of the exception', S)
  then Exit;
  
  if clbExceptions.Items.IndexOf(S) = -1
  then begin
    idx := FExceptionDeleteList.IndexOf(S);
    if idx = -1
    then begin
      AddExceptionLine(nil, S);
    end
    else begin
      AddExceptionLine(TIDEException(FExceptionDeleteList.Objects[idx]), S);
      FExceptionDeleteList.Delete(idx);
    end;
  end
  else begin
    MessageDlg('Duplicate Exception name', mtError, [mbOK], 0);
  end;
end;

procedure TDebuggerOptionsForm.cmdExceptionRemoveCLICK(Sender: TObject);
var
  idx: Integer;
  obj: TObject;
begin
  idx := clbExceptions.ItemIndex;
  if idx <> -1
  then begin
    obj := clbExceptions.Items.Objects[idx];
    if obj <> nil
    then FExceptionDeleteList.AddObject(clbExceptions.Items[idx], obj);
    clbExceptions.Items.Delete(idx);
  end;
  cmdExceptionRemove.Enabled :=  clbExceptions.ItemIndex <> -1;
end;

procedure TDebuggerOptionsForm.cmdOKCLICK (Sender: TObject );
var
  n: Integer;
  ie: TIDEException;
begin
  for n := 0 to FExceptionDeleteList.Count - 1 do
    FExceptionDeleteList.Objects[n].Free;
    
  for n := 0 to clbExceptions.Items.Count - 1 do
  begin
    ie := TIDEException(clbExceptions.Items.Objects[n]);
    if ie = nil
    then begin
      ie := DebugBoss.Exceptions.Add(clbExceptions.Items[n]);
      ie.Enabled := clbExceptions.Checked[n];
    end
    else begin
      ie.BeginUpdate;        //ie^
      try
        ie.Name := clbExceptions.Items[n];
        ie.Enabled := clbExceptions.Checked[n];
      finally
        ie.EndUpdate;
      end;
    end;
  end;
end;

procedure TDebuggerOptionsForm.DebuggerOptionsFormCREATE(Sender: TObject);
var
  n: Integer;
begin
  FExceptionDeleteList := TStringList.Create;
  FExceptionDeleteList.Sorted := True;

  for n := 0 to DebugBoss.Exceptions.Count - 1 do
  begin
    AddExceptionLine(DebugBoss.Exceptions[n], '');
  end;
  
  for n := 0 to DebugBoss.Signals.Count - 1 do
  begin
    AddSignalLine(DebugBoss.Signals[n]);
  end;

end;

procedure TDebuggerOptionsForm.DebuggerOptionsFormDESTROY(Sender: TObject);
begin
  FreeAndNil(FExceptionDeleteList);
end;


initialization
  {$I debugoptionsfrm.lrs}

end.

