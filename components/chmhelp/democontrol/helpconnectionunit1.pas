unit HelpConnectionUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LHelpControl,
  Buttons, StdCtrls, LazFileUtils;

const
  IPCFile = 'letstestagain';
type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
    function GetLHelpFilename: string;
  end;

var
  Form1: TForm1; 
  Help: TLHelpConnection;

implementation

{ TForm1 }

function ResponseToString(Ares: TLHelpResponse): String;
begin
  case Ares of
    srNoAnswer: Result := 'NoAnswer';
    srSuccess: Result := 'Success';
    srInvalidFile: Result := 'InvalidFileName';
    srInvalidURL: Result := 'InvalidURL';
    srInvalidContext: Result := 'InvalidContext';
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Res: TLHelpResponse;
begin
  OpenDialog1.InitialDir:=GetCurrentDirUTF8;
  if not OpenDialog1.Execute then exit;
  Screen.Cursor := crHourGlass;
  try
    if Help.ServerRunning = false then
      Help.StartHelpServer(IPCFile, GetLHelpFilename);
    Res := Help.OpenFile(OpenDialog1.FileName);
  finally
    Screen.Cursor := crDefault;
  end;
  Label1.Caption := ResponseToString(Res);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LHelp: String;
begin
  {$IFDEF Unix}
  DeleteFile('/tmp/'+IPCFile);
  {$ENDIF}
  LHelp := GetLHelpFilename;
  if not FileExistsUTF8(LHelp) then
    MessageDlg('Missing lhelp','Can not find the lhelp application "'+LHelp+'"',
       mtError,[mbOk],0);
  Help := TLHelpConnection.Create;
  Help.ProcessWhileWaiting := @Application.ProcessMessages;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Help.Free;
end;

function TForm1.GetLHelpFilename: string;
begin
  Result := '../lhelp/lhelp';
  {$IFDEF Windows}
  Result := '..\lhelp\lhelp.exe';
  {$ENDIF}
  {$IFDEF darwin} //OS X
  Result:=Result+'.app/Contents/MacOS/'+ExtractFilename(Result);
  {$ENDIF}
end;

{$R *.lfm}

end.

