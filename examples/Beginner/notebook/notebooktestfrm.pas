unit NoteBookTestFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Notebook1: TNotebook;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  NewPageIndex: integer;
  NewPage: TPage;
  PageLabel: TLabel;
begin
  NewPageIndex := Notebook1.Pages.Add(Format('[Page %d]', [Notebook1.Pages.Count]));
  NewPage := Notebook1.Page[NewPageIndex];
  NewPage.Color := RgbToColor(Random(128)+127, Random(128)+127, Random(128)+127);
  PageLabel := TLabel.Create(Self);
  with PageLabel do
  begin
    Left := 20;
    Top := 10 + NewPageIndex * 20;
    Width := 500;
    Height := 20;
    Caption := Format('This is page [%d]',[NewPageIndex]);
    Parent := NewPage;
  end;
  Label1.Caption := IntToStr(Notebook1.PageCount)+ ' pages total';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Notebook1.PageIndex > 0 then
    Notebook1.PageIndex := Notebook1.PageIndex - 1;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Notebook1.PageIndex < Notebook1.PageCount-1 then
    Notebook1.PageIndex := Notebook1.PageIndex + 1;
end;

end.

