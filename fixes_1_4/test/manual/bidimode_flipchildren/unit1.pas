unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CheckLst;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckGroup1: TCheckGroup;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    ListBox1: TListBox;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Application.BiDiMode = bdLeftToRight then
    Application.BiDiMode := bdRightToLeft
  else
    Application.BiDiMode := bdLeftToRight;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FlipChildren(true);
end;

end.

