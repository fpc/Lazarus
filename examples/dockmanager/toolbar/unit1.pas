unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    swView1: TCheckBox;
    swInitSize: TCheckBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure swView1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  btn: TToolButton;
begin
  btn := TToolButton.Create(ToolBar1);
  btn.Parent := ToolBar1;
  btn.Style := tbsCheck;
  btn.AutoSize := True;
  if swInitSize.Checked then
    btn.Width := 100;
  btn.Caption := 'button ' + IntToStr(btn.Index);
  btn.Grouped := True;
end;

procedure TForm1.swView1Click(Sender: TObject);
begin
  ToolButton1.Visible := swView1.Checked;
end;

initialization
  {$I unit1.lrs}

end.

