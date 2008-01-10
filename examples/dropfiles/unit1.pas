unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Label1: TLabel;
    Memo1: TMemo;
    procedure ApplicationProperties1DropFiles(Sender: TObject;
      const FileNames: array of String);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.ApplicationProperties1DropFiles(Sender: TObject;
  const FileNames: array of String);
var
  I: Integer;
begin
  Memo1.Lines.Add(IntToStr(Length(FileNames)) + ' file(s) dropped on Application:');
  for I := 0 to High(FileNames) do
    Memo1.Lines.Add(FileNames[I]);
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  Memo1.Lines.Add(IntToStr(Length(FileNames)) + ' file(s) dropped on ' + Name + ':');
  for I := 0 to High(FileNames) do
    Memo1.Lines.Add(FileNames[I]);
end;

initialization
  {$I unit1.lrs}

end.

