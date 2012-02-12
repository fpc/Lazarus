unit FeedbackDlg;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, ButtonPanel, StdCtrls, ExtCtrls, Debugger, LazarusIDEStrConsts;

type

  { TDbgFeedbackDlg }

  TDbgFeedbackDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    lblMsg: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(const AText, AInfo: String;
                     AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults
                    ): TDBGFeedbackResult;
  end;

var
  DbgFeedbackDlg: TDbgFeedbackDlg;

function ExecuteFeedbackDialog(const AText, AInfo: String;
                               AType: TDBGFeedbackType; AButtons: TDBGFeedbackResults
                              ): TDBGFeedbackResult;

implementation

function ExecuteFeedbackDialog(const AText, AInfo: String; AType: TDBGFeedbackType;
  AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
var
  ADialog: TDbgFeedbackDlg;
begin
  ADialog := TDbgFeedbackDlg.Create(Application);
  try
    Result := ADialog.Execute(AText, AInfo, AType, AButtons);
  finally
    ADialog.Free;
  end;
end;

{ TDbgFeedbackDlg }

procedure TDbgFeedbackDlg.HelpButtonClick(Sender: TObject);
begin
  AutoSize := False;
  Memo1.Visible := not Memo1.Visible;
  if Memo1.Visible then
    ButtonPanel1.HelpButton.Caption := lisDebuggerFeedbackLess
  else
    ButtonPanel1.HelpButton.Caption := lisDebuggerFeedbackMore;
  AutoSize := True;
end;

function TDbgFeedbackDlg.Execute(const AText, AInfo: String; AType: TDBGFeedbackType;
  AButtons: TDBGFeedbackResults): TDBGFeedbackResult;
begin
  case AType of
    ftInformation: begin
        Caption := lisDebuggerFeedbackInformation;
      end;
    ftWarning: begin
        Caption := lisDebuggerFeedbackWarning;
      end;
    ftError: begin
        Caption := lisDebuggerFeedbackError;
      end;
  end;
  lblMsg.Caption := AText;
  memo1.Text := AInfo;
  memo1.Visible := False;
  if AInfo <> '' then begin
    ButtonPanel1.HelpButton.Caption := lisDebuggerFeedbackMore;
    ButtonPanel1.HelpButton.Visible := True;
  end
  else begin
    ButtonPanel1.HelpButton.Visible := False;
  end;

  ButtonPanel1.OKButton.Visible := frOk in AButtons;
  ButtonPanel1.CancelButton.Visible := frStop in AButtons;

  ButtonPanel1.OKButton.Caption := lisOk;
  ButtonPanel1.CancelButton.Caption := lisDebuggerFeedbackStop;

  case ShowModal of
    mrOk: Result := frOk;
    mrCancel: Result := frStop;
  end;
end;

{$R *.lfm}

end.

