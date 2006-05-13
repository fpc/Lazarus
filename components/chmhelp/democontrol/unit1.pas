unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LHelpControl,
  Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  Help.StartHelpServer('letstestagain', '../lhelp/lhelp --display=192.168.0.250:0');
  Help.OpenFile(OpenDialog1.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Help := TLHelpConnection.Create;
end;

initialization
  {$I unit1.lrs}

end.

