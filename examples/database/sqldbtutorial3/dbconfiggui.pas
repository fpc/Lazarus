unit dbconfiggui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, dbconfig;

type
  TConnectionTestFunction = function(ChosenConfig: TDBConnectionConfig): boolean of object;
  { TDBConfigForm }

  TDBConfigForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    TestButton: TButton;
    ConnectorType: TComboBox;
    Host: TEdit;
    Database: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Password: TEdit;
    User: TEdit;
    procedure ConnectorTypeEditingDone(Sender: TObject);
    procedure DatabaseEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HostEditingDone(Sender: TObject);
    procedure PasswordEditingDone(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure UserEditingDone(Sender: TObject);
  private
    FConnectionConfig: TDBConnectionConfig;
    FConnectionTestFunction: TConnectionTestFunction;
    FSetupComplete: boolean;
    { private declarations }
  public
    property Config: TDBConnectionConfig read FConnectionConfig;
    property ConnectionTestCallback: TConnectionTestFunction write FConnectionTestFunction;
    { public declarations }
  end;

var
  DBConfigForm: TDBConfigForm;

implementation

{$R *.lfm}

{ TDBConfigForm }

procedure TDBConfigForm.TestButtonClick(Sender: TObject);
begin
  // Call callback with settings, let it figure out if connection succeeded and
  // get test result back
  if assigned(FConnectionTestFunction) and assigned(FConnectionConfig) then
    if FConnectionTestFunction(FConnectionConfig) then
       showmessage('Connection test succeeded.')
      else
        showmessage('Connection test failed.')
  else
    showmessage('Error: connection test code has not been implemented.');
end;

procedure TDBConfigForm.UserEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBUser:=User.Text;
end;

procedure TDBConfigForm.FormCreate(Sender: TObject);
begin
  FConnectionConfig:=TDBConnectionConfig.Create;
  FSetupComplete:=false;
end;

procedure TDBConfigForm.ConnectorTypeEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBType:=ConnectorType.Text;
end;

procedure TDBConfigForm.DatabaseEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBPath:=Database.Text;
end;

procedure TDBConfigForm.FormDestroy(Sender: TObject);
begin
  FConnectionConfig.Free;
end;

procedure TDBConfigForm.FormShow(Sender: TObject);
begin
  if not FSetupComplete then
  begin
    // Only do this once in form's lifetime
    FSetupComplete:=true;
    ConnectorType.Text:=FConnectionConfig.DBType;
    Host.Text:=FConnectionConfig.DBHost;
    Database.Text:=FConnectionConfig.DBPath;
    User.Text:=FConnectionConfig.DBUser;
    Password.Text:=FConnectionConfig.DBPassword;
  end;
end;

procedure TDBConfigForm.HostEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBHost:=Host.Text;
end;

procedure TDBConfigForm.PasswordEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBPassword:=Password.Text;
end;

end.

