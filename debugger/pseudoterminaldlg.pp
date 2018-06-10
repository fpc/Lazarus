unit PseudoTerminalDlg;

{$mode objfpc}{$H+}

interface

uses
  IDEWindowIntf, Classes, Graphics,
  Forms, StdCtrls, DebuggerDlg, BaseDebugManager, LazarusIDEStrConsts, LCLType;

type

  { TPseudoConsoleDlg }

  TPseudoConsoleDlg = class(TDebuggerDlg)
    Memo1: TMemo;
    procedure Memo1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure AddOutput(const AText: String);
    procedure Clear;
  end;

var
  PseudoConsoleDlg: TPseudoConsoleDlg;

implementation

var
  PseudeoTerminalDlgWindowCreator: TIDEWindowCreator;

{ TPseudoConsoleDlg }

procedure TPseudoConsoleDlg.Memo1UTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  DebugBoss.DoSendConsoleInput(Utf8Key);
  Utf8Key := '';
end;

procedure TPseudoConsoleDlg.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
  CloseAction := caHide;
end;

constructor TPseudoConsoleDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  font.Name := 'monospace';
  Caption:= lisDbgTerminal;
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

