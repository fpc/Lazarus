unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, LCLProc, ColorBox;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StatusBar1: TStatusBar;
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
  i, rows: integer;
  w: integer;
  r: TRect;
begin
  btn := TToolButton.Create(ToolBar1);
  btn.Name:='TestButton'+IntToStr(ToolBar1.ComponentCount);
  btn.Parent := ToolBar1;
  btn.Style := tbsCheck;
  btn.AutoSize := True;
  if swInitSize.Checked then
    btn.Width := 100;
  btn.Caption := 'button ' + IntToStr(btn.Index);
  btn.Grouped := True;
{
  w := ToolBar1.BorderWidth;
  rows := 1;
  for i := 0 to ToolBar1.ButtonCount - 1 do begin
    btn := ToolBar1.Buttons[i];
    inc(w, btn.Width);
    if w > ToolBar1.Width then begin
      inc(rows);
      w := btn.Width + ToolBar1.BorderWidth;
    end;
  end;
  //rows := (w div ToolBar1.Width) + 1;
  ToolBar1.Height := btn.Height * rows;
}

  btn := ToolBar1.Buttons[ToolBar1.ButtonCount-1];
  r := btn.BoundsRect;
  StatusBar1.SimpleText := 'Bottom: ' + IntToStr(r.Bottom);
  //ToolBar1.Height := r.Bottom;

end;

procedure TForm1.swView1Click(Sender: TObject);
begin
  ToolButton1.Visible := swView1.Checked;
end;

initialization
  {$I unit1.lrs}

end.

