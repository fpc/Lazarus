unit MemoTestFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  
  { TMemoTestForm }

  TMemoTestForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    MyLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private

  public

  end;

var
  MemoTestForm: TMemoTestForm;

implementation

{$R *.lfm}

{ TMemoTestForm }

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

procedure TMemoTestForm.Button5Click(Sender: TObject);
begin
  Memo1.Append(Edit1.Text);
end;

procedure TMemoTestForm.Button6Click(Sender: TObject);
begin
  Memo2.Append(Edit1.Text);
end;

end.

