unit DBLogDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ButtonPanel, DB;

type

  { TLoginDialog }

  TLoginDialog = class(TForm)
  protected
    lDatabaseName: TLabel;
    lDatabase: TLabel;
    lUserName: TLabel;
    lPassword: TLabel;
    eUserName: TEdit;
    ePassword: TEdit;
    BtnPanel: TButtonPanel;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

function LoginDialogEx(const ADatabaseName: string; var AUserName, APassword: string;
  UserNameReadOnly: Boolean=False): Boolean;

resourcestring
  rsDBLogDlgCaption = 'Database Login';
  rsDBLogDlgDatabase = 'Database';
  rsDBLogDlgUserName = '&User Name';
  rsDBLogDlgPassword = '&Password';
  rsDBLogDlgLogin = '&Login';

implementation

{ TLoginDialog }

constructor TLoginDialog.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner, 0);

  Caption := rsDBLogDlgCaption;
  Position := poScreenCenter;
  AutoSize := True;
  BorderStyle := bsDialog;
  ChildSizing.LeftRightSpacing := Scale96ToForm(16);
  ChildSizing.TopBottomSpacing := Scale96ToForm(10);

  lDatabase := TLabel.Create(Self);
  lDatabase.Parent := Self;
  lDatabase.Caption := rsDBLogDlgDatabase;

  lDatabaseName := TLabel.Create(Self);
  lDatabaseName.Parent := Self;
  lDatabaseName.AnchorSide[akTop].Control := lDatabase;
  lDatabaseName.AnchorSide[akTop].Side := asrCenter;
  lDatabaseName.AnchorSide[akLeft].Control := lDatabase;
  lDatabaseName.AnchorSide[akLeft].Side := asrLeft;
  lDatabaseName.BorderSpacing.Left := Scale96ToForm(80);

  lUserName := TLabel.Create(Self);
  lUserName.Parent := Self;
  lUserName.Caption := rsDBLogDlgUserName;
  lUserName.AnchorSide[akTop].Control := lDatabase;
  lUserName.AnchorSide[akTop].Side := asrBottom;
  lUserName.BorderSpacing.Top := Scale96ToForm(14);

  eUserName := TEdit.Create(Self);
  eUserName.Parent := Self;
  eUserName.Width := Scale96ToForm(164);
  eUserName.AnchorSide[akTop].Control := lUserName;
  eUserName.AnchorSide[akTop].Side := asrCenter;
  eUserName.AnchorSide[akLeft].Control := lUserName;
  eUserName.AnchorSide[akLeft].Side := asrLeft;
  eUserName.Anchors := [akTop, akLeft];
  eUserName.BorderSpacing.Left := lDatabaseName.BorderSpacing.Left;
  lUserName.FocusControl := eUserName;

  lPassword := TLabel.Create(Self);
  lPassword.Parent := Self;
  lPassword.Caption := rsDBLogDlgPassword;
  lPassword.AnchorSide[akTop].Control := lUserName;
  lPassword.AnchorSide[akTop].Side := asrBottom;
  lPassword.BorderSpacing.Top := Scale96ToForm(12);

  ePassword := TEdit.Create(Self);
  ePassword.Parent := Self;
  ePassword.Width := eUserName.Width;
  ePassword.PasswordChar := '*';
  ePassword.AnchorSide[akTop].Control := lPassword;
  ePassword.AnchorSide[akTop].Side := asrCenter;
  ePassword.AnchorSide[akLeft].Control := lPassword;
  ePassword.AnchorSide[akLeft].Side := asrLeft;
  ePassword.Anchors := [akTop, akLeft];
  ePassword.BorderSpacing.Left := lDatabaseName.BorderSpacing.Left;
  lPassword.FocusControl := ePassword;

  BtnPanel := TButtonPanel.Create(Self);
  BtnPanel.Parent := Self;
  BtnPanel.ShowBevel:= False;
  BtnPanel.ShowButtons := [pbOK, pbCancel];
  BtnPanel.OKButton.Caption := rsDBLogDlgLogin;
  BtnPanel.AnchorSide[akTop].Control := ePassword;
  BtnPanel.AnchorSide[akTop].Side := asrBottom;
  BtnPanel.AnchorSide[akRight].Control := ePassword;
  BtnPanel.AnchorSide[akRight].Side := asrRight;
  BtnPanel.Anchors := [akTop, akRight];
  BtnPanel.BorderSpacing.Top := Scale96ToForm(10);
end;


function LoginDialogEx(const ADatabaseName: string; var AUserName, APassword: string;
  UserNameReadOnly: Boolean=False): Boolean;
var
  F: TLoginDialog;
begin
  F := TLoginDialog.Create(nil);
  try
    F.lDatabaseName.Caption := ADatabaseName;
    F.eUserName.Text := AUserName;
    F.ePassword.Text := APassword;
    if UserNameReadOnly then
    begin
      F.eUserName.ReadOnly := True;
      F.ActiveControl := F.ePassword;
    end;
    Result := F.ShowModal = mrOK;
    if Result then
    begin
      AUserName := F.eUserName.Text;
      APassword := F.ePassword.Text;
    end;
  finally
    F.Free;
  end;
end;

initialization
  if not Assigned(LoginDialogExProc) then
    LoginDialogExProc := @LoginDialogEx;

end.

