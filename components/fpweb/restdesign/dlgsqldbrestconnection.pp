{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Form to edit a SQLDB data connection, used in data management.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dlgsqldbrestconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, Buttons, sqldb, sqldbrestbridge;

type

  { TSQLDBRestConnectionEditorForm }

  TSQLDBRestConnectionEditorForm = class(TForm)
    BPConnection: TButtonPanel;
    CBType: TComboBox;
    ECharset: TEdit;
    EName: TEdit;
    ERole: TEdit;
    EUserName: TEdit;
    EPassword: TEdit;
    EHostName: TEdit;
    EDatabaseName: TEdit;
    Label2: TLabel;
    LEHostName1: TLabel;
    LEUserName: TLabel;
    LCBType: TLabel;
    LEHostName: TLabel;
    LEDatabaseName: TLabel;
    LEPassword: TLabel;
    LERole: TLabel;
    LMParams: TLabel;
    MParams: TMemo;
    SBTest: TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SBTestClick(Sender: TObject);
  private
    FConnection: TSQLDBRestConnection;
    Function TestConnection : String;
    procedure FormToConnection;
    procedure ConnectionToForm;
    procedure SetConnection(AValue: TSQLDBRestConnection);
  public
    Property Connection : TSQLDBRestConnection Read FConnection Write SetConnection;
  end;

Function EditSQLDBRestConnection(aConnection : TSQLDBRestConnection) : Boolean;

Resourcestring
  SConnectionSuccesful = 'Connection to the database was succesfully made.';
  SErrConnectionNotOK = 'Error connecting to the database';
  SSuccess = 'Succesfully connected.';


implementation

{$R *.lfm}

Function EditSQLDBRestConnection(aConnection : TSQLDBRestConnection) : Boolean;

begin
  With TSQLDBRestConnectionEditorForm.Create(Application) do
    try
      Connection:=aConnection;
      result:= (ShowModal=mrOK);
      if Result then
        aConnection.Assign(Connection);
    finally

    end;
end;

procedure TSQLDBRestConnectionEditorForm.FormCreate(Sender: TObject);
begin
  FConnection:=TSQLDBRestConnection.Create(Nil);
  GetConnectionList(CBType.Items);
end;

procedure TSQLDBRestConnectionEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);

Var
  S : String;

begin
  if ModalResult=mrOK then
    begin
    FormToConnection;
    S:=TestConnection;
    if (S<>'') then
      if MessageDlg(SErrConnectionNotOK,S,mtError,[mbIgnore,mbCancel],0)=mrIgnore then
        S:='';
    end;
  CanClose:=(S='');
end;

procedure TSQLDBRestConnectionEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FConnection);
end;

procedure TSQLDBRestConnectionEditorForm.SBTestClick(Sender: TObject);

Var
  S : String;

begin
  FormToConnection;
  S:=TestConnection;
  if (S<>'') then
    MessageDlg(SErrConnectionNotOK,S,mtError,[mbOK],0)
  else
    MessageDlg(SSuccess,SConnectionSuccesful,mtInformation,[mbOK],0);
end;

function TSQLDBRestConnectionEditorForm.TestConnection: String;

Var
  Conn : TSQLConnector;
  TR : TSQLTransaction;

begin
  Conn:=TSQLConnector.Create(Self);
  try
    TR:=TSQLTransaction.Create(Conn);
    Conn.Transaction:=TR;
    FConnection.ConfigConnection(Conn);
    try
      Conn.Connected:=true;
    except
      On E: Exception do
        Result:=E.Message;
    end;
  finally
    Conn.Free;
  end;
end;


procedure TSQLDBRestConnectionEditorForm.SetConnection(AValue: TSQLDBRestConnection);
begin
  if FConnection=AValue then Exit;
  FConnection.Assign(AValue);
  ConnectionToForm;
end;

procedure TSQLDBRestConnectionEditorForm.FormToConnection;

begin
  With FConnection do
    begin
    Name:=EName.Text;
    ConnectionType:=CBType.Text;
    HostName:=EHostName.Text;
    DatabaseName:=EDatabaseName.Text;
    Params:=MParams.Lines;
    UserName:=EUserName.Text;
    Password:=EPassword.Text;
    Role:=ERole.Text;
    Charset:=ECharset.Text;
    end;
end;

procedure TSQLDBRestConnectionEditorForm.ConnectionToForm;

begin
  With FConnection do
    begin
    EName.Text:=Name;
    CBType.Text:=ConnectionType;
    EHostName.Text:=HostName;
    EDatabaseName.Text:=DatabaseName;
    MParams.Lines:=Params;
    EUserName.Text:=UserName;
    EPassword.Text:=Password;
    ERole.Text:=Role;
    ECharset.Text:=Charset;
    end;
end;

end.

