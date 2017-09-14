unit opkman_repositorydetailsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LazFileUtils;

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
    SDRep: TSaveDialog;
    procedure bOkClick(Sender: TObject);
    procedure edAddressChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FIsNew: boolean;
    function IsDuplicateRepository(const AAddress: string): boolean;
  public
    property FileName: string read FFileName;
    property IsNew: boolean read FIsNew write FIsNew;
  end;

var
  RepositoryDetailsFrm: TRepositoryDetailsFrm;

implementation

uses opkman_const, opkman_common, opkman_options;

{$R *.lfm}

{ TRepositoryDetailsFrm }

procedure TRepositoryDetailsFrm.FormCreate(Sender: TObject);
begin
  Caption := rsRepositoryDetailsFrm_Caption;
  lbName.Caption := rsRepositoryDetailsFrm_lbName_Caption;
  edName.Hint := rsRepositoryDetailsFrm_edName_Hint;
  lbAddress.Caption := rsRepositoryDetailsFrm_lbAddress_Caption;
  edAddress.Hint := rsRepositoryDetailsFrm_edAddress_Hint;
  lbDescription.Caption := rsRepositoryDetailsFrm_lbDescription_Caption;
  mDescription.Hint := rsRepositoryDetailsFrm_mDescription_Hint;
  bOk.Caption := rsRepositoryDetailsFrm_bOk_Caption;
  bOk.Hint := rsRepositoryDetailsFrm_bOk_Hint;
  bCancel.Caption := rsRepositoryDetailsFrm_bCancel_Caption;
  bCancel.Hint := rsRepositoryDetailsFrm_bCancel_Hint;
end;

function TRepositoryDetailsFrm.IsDuplicateRepository(const AAddress: string): boolean;
var
  I: integer;
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
var
  Address: String;
begin
  if Trim(edName.Text) = '' then
  begin
    MessageDlgEx(rsRepositoryDetailsFrm_Info1, mtInformation, [mbOK], Self);
    edName.SetFocus;
    Exit;
  end;

  if (FIsNew) then
  begin
    if SDRep.Execute then
    begin
      if (not IsDirectoryEmpty(ExtractFilePath(SDRep.FileName))) then
        if MessageDlgEx(Format(rsCreateRepositoryFrm_Info1, [ExtractFilePath(SDRep.FileName)]), mtConfirmation, [mbYes, mbNo], Self) = mrNo then
          Exit;
      if not DirectoryIsWritable(ExtractFilePath(SDRep.FileName)) then
      begin
        MessageDlgEx(Format(rsCreateRepositoryFrm_Info1, [ExtractFilePath(SDRep.FileName)]), mtConfirmation, [mbOK], Self);
        Exit;
      end;
      FFileName := SDRep.FileName;
    end
    else
      Exit;
  end;

  if Trim(edAddress.Text) <> '' then
  begin
    Address := Trim(edAddress.Text);
    if Address[Length(Address)] <> '/' then
      Address := Address + '/';
    if not IsDuplicateRepository(Address) then
      Options.RemoteRepository.Add(Address);
  end;

  ModalResult := mrOk;
end;

procedure TRepositoryDetailsFrm.edAddressChange(Sender: TObject);
begin
  edAddress.Font.Color := clDefault;
end;

end.
