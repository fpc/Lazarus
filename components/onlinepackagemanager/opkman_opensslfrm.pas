unit opkman_OpenSSLfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  ExtCtrls,
  // OpkMan
  opkman_const;

type

  { TOpenSSLFrm }

  TOpenSSLFrm = class(TForm)
    Bp: TButtonPanel;
    cbPermanent: TCheckBox;
    lbMessage1: TLabel;
    lbMessage2: TLabel;
    pnBuffer: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  OpenSSLFrm: TOpenSSLFrm;

implementation

{$R *.lfm}

{ TOpenSSLFrm }

procedure TOpenSSLFrm.FormCreate(Sender: TObject);
begin
  Caption := rsOpenSSLFrm_Caption;
  Bp.OKButton.Caption := rsOpenSSLFrm_Bp_OKButton_Caption;
  Bp.OKButton.ModalResult := mrYes;
  Bp.CancelButton.Caption := rsOpenSSLFrm_Bp_CancelButton_Caption;
  BP.CancelButton.ModalResult := mrNo;
  cbPermanent.Caption := rsOpenSSLFrm_chPermanent_Caption;
  lbMessage1.Caption := rsOpenSSLFrm_lbMessage1_Caption;
  lbMessage2.Caption := rsOpenSSLFrm_lbMessage2_Caption;
end;


end.

