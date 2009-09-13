unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LHelpControl,
  Buttons, StdCtrls;

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
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 
  Help: TLHelpConnection;

implementation

{ TForm1 }

function ResponseToString(Ares: TLHelpResponse): String;
begin
  case Ares of
    srNoAnswer:  Result := 'NoAnswer';
    srSuccess: Result := 'Success';
    srInvalidFile:Result := 'InvalidFileName';
    srInvalidURL:Result := 'InvalidURL';
    srInvalidContext:Result := 'InvalidContext';

  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Res: TLHelpResponse;
begin
  if not OpenDialog1.Execute then exit;
  if Help.ServerRunning = false then
    Help.StartHelpServer('letstestagain', '../lhelp/lhelp');
  Res :=Help.OpenFile(OpenDialog1.FileName);
  Label1.Caption := ResponseToString(Res);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Help := TLHelpConnection.Create;
  Help.ProcessWhileWaiting := @Application.ProcessMessages;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Help.Free;
end;

initialization
  {$I unit1.lrs}

end.

