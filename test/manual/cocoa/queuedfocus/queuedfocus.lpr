program QueuedFocus;

{$mode objfpc}{$H+}{$modeswitch objectivec2}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, CocoaAll;

type
  TProbe = class(TForm)
  public
    OldEdit, NextEdit: TEdit;
    NewButton: TButton;
    Timer: TTimer;
    EnterCount: Integer;
    constructor Create(AOwner: TComponent); override;
    procedure RedirectFocus(Sender: TObject);
    procedure Run(Sender: TObject);
  end;

var
  Probe: TProbe;
  ExitStatus: Integer;

procedure CheckFocus(Form: TForm; Control: TWinControl);
var
  R: NSResponder;
  NativeFocused: Boolean;
begin
  R := NSView(Control.Handle).window.firstResponder;
  NativeFocused := R = NSResponder(Control.Handle);
  if not NativeFocused and R.isKindOfClass(NSTextView.classClass) then
    NativeFocused := Pointer(NSTextView(R).delegate) = Pointer(Control.Handle);
  if (Form.ActiveControl <> Control) or not NativeFocused then
    raise Exception.Create('The expected control lost LCL or native focus');
end;

constructor TProbe.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  SetBounds(40, 40, 420, 180);
  OldEdit := TEdit.Create(Self);
  OldEdit.Parent := Self;
  OldEdit.SetBounds(16, 16, 180, 28);
  NextEdit := TEdit.Create(Self);
  NextEdit.Parent := Self;
  NextEdit.SetBounds(16, 56, 180, 28);
  NextEdit.OnEnter := @RedirectFocus;
  NewButton := TButton.Create(Self);
  NewButton.Parent := Self;
  NewButton.Caption := 'New target';
  NewButton.SetBounds(220, 16, 160, 32);
  ActiveControl := OldEdit;
  Timer := TTimer.Create(Self);
  Timer.Interval := 100;
  Timer.OnTimer := @Run;
end;

procedure TProbe.RedirectFocus(Sender: TObject);
begin
  Inc(EnterCount);
  OldEdit.SetFocus;
end;

procedure TProbe.Run(Sender: TObject);
var
  Expected: TWinControl;
begin
  Timer.Enabled := False;
  try
    OldEdit.SetFocus;
    Application.ProcessMessages;
    NextEdit.SetFocus; // OnEnter queues a return to OldEdit.
    if EnterCount = 0 then raise Exception.Create('OnEnter did not run');
    Expected := OldEdit;
    if ParamStr(1) <> 'retain' then
    begin
      NewButton.SetFocus; // Supersede the queued request before it runs.
      Expected := NewButton;
      CheckFocus(Self, Expected);
    end;
    Application.ProcessMessages;
    CheckFocus(Self, Expected);
    WriteLn('PASS');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ', E.Message);
      ExitStatus := 1;
    end;
  end;
  Application.Terminate;
end;

begin
  Application.Initialize;
  Application.CreateForm(TProbe, Probe);
  Application.Run;
  Halt(ExitStatus);
end.
