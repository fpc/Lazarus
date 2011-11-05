unit PseudoTerminalDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  IDEWindowIntf, IDEOptionDefs,
  Forms, Controls, Graphics, Dialogs, StdCtrls, DebuggerDlg, BaseDebugManager;

type

  { TPseudoConsoleDlg }

  TPseudoConsoleDlg = class(TDebuggerDlg)
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    { public declarations }
    procedure AddOutput(const AText: String);
    procedure Clear;
  end;

var
  PseudoConsoleDlg: TPseudoConsoleDlg;

implementation

var
  PseudeoTerminalDlgWindowCreator: TIDEWindowCreator;

{ TPseudoConsoleDlg }

procedure TPseudoConsoleDlg.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if Key <> #13 then exit;
  DebugBoss.DoSendConsoleInput(Edit1.Text+LineEnding);
  Edit1.Text := '';
end;

procedure TPseudoConsoleDlg.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caHide;
end;

procedure TPseudoConsoleDlg.AddOutput(const AText: String);
begin
  Memo1.Text:=Memo1.Text+AText;
  while Memo1.Lines.Count > 5000 do
    Memo1.Lines.Delete(0);
  Memo1.SelStart := length(Memo1.Text);
end;

procedure TPseudoConsoleDlg.Clear;
begin
  Memo1.Text := '';
end;

{$R *.lfm}

initialization

  PseudeoTerminalDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtPseudoTerminal]);
  PseudeoTerminalDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  PseudeoTerminalDlgWindowCreator.CreateSimpleLayout;

end.

