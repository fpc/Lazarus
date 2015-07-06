unit formunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FBAdmin, Forms, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ConnectButton: TButton;
    FBAdmin1: TFBAdmin;
    Host: TLabeledEdit;
    OutputMemo: TMemo;
    User: TLabeledEdit;
    Password: TLabeledEdit;
    Port: TLabeledEdit;
    procedure ConnectButtonClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  ibconnection { for EIBDatabaseError};
{$R *.lfm}

{ TForm1 }

procedure TForm1.ConnectButtonClick(Sender: TObject);
var
  Users: TStringList;
  // For filling user details:
  GroupName,FirstName,MiddleName,LastName:string;
  UserID, GroupID: longint;
begin
  OutputMemo.Lines.Clear;
  FBAdmin1.Host:=Host.Text;
  try
    FBAdmin1.Port:=StrToInt(Port.Text);
  except
    OutputMemo.Lines.Add('Error setting port to '+Port.Text+'. Using 3050 instead.');
    FBAdmin1.Port:=3050; //a default Firebird port
  end;

  FBAdmin1.User:=User.Text;
  FBAdmin1.Password:=Password.Text;
  // Big chance server supports TCP/IP
  // Change this if you use embedded.
  FBAdmin1.Protocol:=IBSPTCPIP;

  FBAdmin1.UseExceptions:=true;
  try
    // Make sure we close off previous connections.
    try
      FBAdmin1.DisConnect;
    except
      // This will generate an exception if we're not connected.
      // Ignore it.
    end;
    FBAdmin1.Connect;

    // Shamelessly copied from the FPC example.
    // Note that backups are omitted in this example...
    // FBadmin allows lets you run backups/restores on the server.
    OutputMemo.Lines.Add('Server type: '+FBAdmin1.ServerImplementation);
    OutputMemo.Lines.Add('Server version: '+FBAdmin1.ServerVersion);
    // Handy to know for backup purposes...
    OutputMemo.Lines.Add('Server root directory: '+FBAdmin1.ServerRootDir);
    Users:=TStringList.Create;
    try
      if FBAdmin1.GetUsers(Users) then
        OutputMemo.Lines.Add('List of users: '+Users.Text)
      else
        OutputMemo.Lines.Add('Sorry, could not get user list.');
    finally
      Users.Free;
    end;


    // Get details for current user:
    if FBAdmin1.GetUser(FBAdmin1.User,GroupName,FirstName,MiddleName,LastName,UserID, GroupID) then
    begin
      OutputMemo.Lines.Add('Name:      '+FBAdmin1.User);
      OutputMemo.Lines.Add('Full name: '+Trim(Trim(FirstName+Trim(' '+MiddleName)+' ')+LastName));
      OutputMemo.Lines.Add('User ID:   '+IntToStr(UserID));
      OutputMemo.Lines.Add('Group:     '+GroupName);
      OutputMemo.Lines.Add('Group ID:  '+IntToStr(GroupID));
    end
    else
      OutputMemo.Lines.Add('Sorry, could not get user details for '+FBAdmin1.User);

    OutputMemo.Lines.Add('Database log:');
    if FBAdmin1.GetDatabaseLog then
    begin
      OutputMemo.Lines.AddStrings(FBAdmin1.Output);
    end
    else
      OutputMemo.Lines.Add('Could not get database log, sorry.');
    // Component will disconnect automatically.
  except
    on B: EIBDatabaseError do
    begin
      OutputMemo.Lines.Add('Database error: '+B.ClassName+'/'+B.Message+
        '. GDS error code: '+IntToStr(B.GDSErrorCode));
    end;
    on E: Exception do
    begin
      OutputMemo.Lines.Add('Exception: '+E.ClassName+'/'+E.Message);
    end;
  end;
end;

end.

