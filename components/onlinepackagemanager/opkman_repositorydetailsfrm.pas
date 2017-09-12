unit opkman_repositorydetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, StdCtrls;

type

  { TRepositoryDetailsFrm }

  TRepositoryDetailsFrm = class(TForm)
    bOk: TButton;
    bCancel: TButton;
    edName: TEdit;
    edAddress: TEdit;
    lbName: TLabel;
    lbAddress: TLabel;
    lbDescription: TLabel;
    lbOF2: TLabel;
    mDescription: TMemo;
    pnButtons: TPanel;
    procedure bOkClick(Sender: TObject);
    procedure edAddressChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAddress: String;
    function IsDuplicateRepository(const AAddress: String): Boolean;
  public
    property Address: String read FAddress;
  end;

var
  RepositoryDetailsFrm: TRepositoryDetailsFrm;

implementation
uses opkman_const, opkman_common, opkman_options;
{$R *.lfm}

{ TRepositoryDetailsFrm }

procedure TRepositoryDetailsFrm.FormCreate(Sender: TObject);
begin
  Caption := rsRepositoryDetails_Caption;
  lbName.Caption := rsRepositoryDetails_lbName_Caption;
  edName.Hint := rsRepositoryDetails_edName_Hint;
  lbAddress.Caption := rsRepositoryDetails_lbAddress_Caption;
  edAddress.Hint := rsRepositoryDetails_edAddress_Hint;
  lbDescription.Caption := rsRepositoryDetails_lbDescription_Caption;
  mDescription.Hint := rsRepositoryDetails_mDescription_Hint;
  bOk.Caption := rsRepositoryDetails_bOk_Caption;
  bOk.Hint := rsRepositoryDetails_bOk_Hint;
  bCancel.Caption := rsRepositoryDetails_bCancel_Caption;
  bCancel.Hint := rsRepositoryDetails_bCancel_Hint;
  FAddress := '';
end;

function TRepositoryDetailsFrm.IsDuplicateRepository(const AAddress: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Options.RemoteRepository.Count - 1 do
  begin
    if UpperCase(Options.RemoteRepository.Strings[I]) = UpperCase(AAddress) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TRepositoryDetailsFrm.bOkClick(Sender: TObject);
begin
  if Trim(edName.Text) = '' then
  begin
    MessageDlgEx(rsRepositoryDetails_Info1, mtInformation, [mbOk], Self);
    edName.SetFocus;
    Exit;
  end;
  if (Trim(edAddress.Text) <> '') and (edAddress.Font.Color <> clGray) then
  begin
    FAddress := Trim(edAddress.Text);
    if FAddress[Length(FAddress)] <> '/' then
      FAddress := FAddress + '/';
    if IsDuplicateRepository(FAddress) then
    begin
      if MessageDlgEx(Format(rsRepositoryDetails_Info3, [FAddress]), mtInformation, [mbYes, mbNo], Self) = mrNo then
      begin
        edAddress.SetFocus;
        Exit;
      end;
    end;
  end;
  ModalResult := mrOk;
end;

procedure TRepositoryDetailsFrm.edAddressChange(Sender: TObject);
begin
  edAddress.Font.Color := clDefault;
end;

end.

