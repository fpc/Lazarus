unit dbloginform;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls;

type

  { TLoginForm }

  TLoginForm = class(TForm)
    OSAuthentication: TCheckBox;
    DatabaseType: TComboBox;
    Label6: TLabel;
    LocalMachine: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    User: TEdit;
    Server: TEdit;
    Port: TEdit;
    Database: TEdit;
    UserLabel: TLabel;
    PasswordLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Password: TEdit;
    procedure LocalMachineClick(Sender: TObject);
    procedure OSAuthenticationChange(Sender: TObject);
  private

  public

  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.lfm}

{ TLoginForm }

procedure TLoginForm.LocalMachineClick(Sender: TObject);
begin
  Server.Text:='127.0.0.1';
end;

procedure TLoginForm.OSAuthenticationChange(Sender: TObject);
begin
  if OSAuthentication.Checked then
  begin
    // Switch from username/password to OS authentication.
    User.Enabled:=false;
    UserLabel.Enabled:=false;
    Password.Enabled:=false;
    PasswordLabel.Enabled:=false;
  end
  else
  begin
    // Switch other way round
    User.Enabled:=true;
    UserLabel.Enabled:=true;
    Password.Enabled:=true;
    PasswordLabel.Enabled:=true;
  end;
end;


end.

