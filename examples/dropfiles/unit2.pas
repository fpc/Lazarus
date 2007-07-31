unit Unit2; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure CheckBox1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2; 

implementation

uses
  Unit1;

{ TForm2 }

procedure TForm2.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  Form1.Memo1.Lines.Add(IntToStr(Length(FileNames)) + ' file(s) dropped on ' + Name + ':');
  for I := 0 to High(FileNames) do
    Form1.Memo1.Lines.Add(FileNames[I]);
end;

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  AllowDropFiles := CheckBox1.Checked;
end;

initialization
  {$I unit2.lrs}

end.

