unit DebugOptionsFrm;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Menus, Spin, BaseDebugManager, Debugger;

type
  TDebuggerOptionsForm = class (TForm )
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
    sbExceptions: TSCROLLBOX;
    procedure DebuggerOptionsFormCREATE (Sender: TObject );
    procedure chkBreakOnExceptionCHANGE (Sender: TObject );
    procedure cmdExceptionAddCLICK (Sender: TObject );
  private
    procedure AddExceptionLine(const AException: TIDEException);
    procedure ExceptionEnableClick(Sender: TObject);
  public
    { public declarations }
  end; 

var
  DebuggerOptionsForm: TDebuggerOptionsForm;

implementation

{ TDebuggerOptionsForm }

procedure TDebuggerOptionsForm.AddExceptionLine(const AException: TIDEException);
var
  cb: TCheckBox;
begin
  cb := TCheckBox.Create(self);
  cb.Top := Maxint div 2;
  cb.Align := alTop;
  cb.Caption := AException.Name;
  cb.Checked := AException.Enabled;
  cb.Parent := sbExceptions;
  cb.Visible := True;
  cb.Tag := Integer(AException);
  cb.OnClick := @ExceptionEnableClick;
end;

procedure TDebuggerOptionsForm.chkBreakOnExceptionCHANGE (Sender: TObject );
begin

end;

procedure TDebuggerOptionsForm.cmdExceptionAddCLICK(Sender: TObject);
var
  S: String;
  IdeException: TIDEException;
begin
  if InputQuery('Add Exception', 'Enter the name of the exception', S)
  then begin
    try
      IdeException := DebugBoss.Exceptions.Add(S);
    except
      on E: EDBGExceptions do
      begin
        ShowMessage(E.Message);
        Exit;
      end;
    end;
    AddExceptionLine(IdeException);
  end;
end;

procedure TDebuggerOptionsForm.DebuggerOptionsFormCREATE(Sender: TObject);
var
  n: Integer;
begin
  for n := 0 to DebugBoss.Exceptions.Count - 1 do
  begin
    AddExceptionLine(DebugBoss.Exceptions[n]);
  end;
end;

procedure TDebuggerOptionsForm.ExceptionEnableClick(Sender: TObject);
var
  IdeException: TIDEException;
begin
  IDEException := TIDEException(TCheckBox(Sender).Tag);
  if IDEException <> nil
  then IDEException.Enabled := TCheckBox(Sender).Checked;
end;

initialization
  {$I debugoptionsfrm.lrs}

end.

