unit groupbox_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    grpTst: TGroupBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Assigned(grpTst) then 
    grpTst.Height := grpTst.Height + 10;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Assigned(grpTst) then begin
    grpTst.Width := grpTst.Width + 10;
    grpTst.Show;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Assigned(grpTst) then begin
    grpTst.Show;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Assigned(grpTst) then begin
    grpTst.Hide;
  end;
end;

end.

