program listboxtest;

{$mode objfpc}

uses
  buttons, classes, forms, stdctrls, sysutils, Vclglobals;

type
  TMemoTestForm = class(TForm)
  public
    Button1, Button2, Button3, Button4:   TButton;
    Memo1, Memo2:  TMemo;
    MyLabel: TLabel;
    constructor Create(AOwner: TComponent); override;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  end;

var
  MemoTestForm: TMemoTestForm;

{------------------------------------------------------------------------------}
{  TMemoTestorm                                          }
{------------------------------------------------------------------------------}
constructor TMemoTestForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
  Left := 200;
  Top := 200;

  // create childs
  Button1 := TButton.Create(Self);
  Button1.OnClick := @Button1Click;
  Button1.Parent := Self;
  Button1.left := 40;
  Button1.top :=  170;
  Button1.width := 50;
  Button1.height := 25;
  Button1.caption := '->';
  Button1.Show;

  Button2 := TButton.Create(Self);
  Button2.OnClick := @Button2Click;
  Button2.Parent := Self;
  Button2.left := 95;
  Button2.top := 170;
  Button2.width := 50;
  Button2.height := 25;
  Button2.caption := '<-';
  Button2.Show;

  Button3 := TButton.Create(Self);
  Button3.OnClick := @Button3Click;
  Button3.Parent := Self;
  Button3.left := 150;
  Button3.top := 170;
  Button3.width := 50;
  Button3.height := 25;
  Button3.caption := 'Clear 1';
  Button3.Show;

  Button4 := TButton.Create(Self);
  Button4.OnClick := @button4click;
  Button4.Parent := Self;
  Button4.left := 205;
  Button4.top := 170;
  Button4.width := 50;
  Button4.height := 25;
  Button4.caption := 'Clear 2';
  Button4.Show;

  MyLabel := TLabel.Create(Self);
  with MyLabel
  do begin
    Parent := Self;
    Top := 1;
    Left := 10;
    Width := 150;
    Height := 16;
    Caption := 'These are 2 TMemo:';
    Show;
  end;

  Memo1 := TMemo.Create(Self);
  with Memo1
  do begin
    Parent := Self;
    Left := 10;
    Top := 20;
    Width := 135;
    Height := 155;
    Show;
  end;

  Memo2 := TMemo.Create(Self);
  with Memo2
  do begin
    Parent := Self;
    WordWrap := false;
    Left := 145;
    Top := 20;
    Width := 135;
    Height := 155;
    Show;
  end;
end;

procedure TMemoTestForm.Button1Click(Sender: TObject);
begin
  Memo2.Text := Memo1.Text;
end;

procedure TMemoTestForm.Button2Click(Sender: TObject);
begin
  Memo1.Text := Memo2.Text;
end;

procedure TMemoTestForm.Button3Click(Sender: TObject);
begin
  Memo1.Text := '';
end;

procedure TMemoTestForm.Button4Click(Sender: TObject);
begin
  Memo2.Text := '';
end;

begin
   Application.Initialize;
   Application.CreateForm(TMemoTestForm, MemoTestForm);
   Application.Run;
end.
