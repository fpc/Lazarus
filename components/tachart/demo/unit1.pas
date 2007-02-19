unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  TAGraph, TASeries, Buttons, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Chart1: TChart;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Legend: TCheckBox;
    Panel1: TPanel;
    procedure Panel1Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure LegendChange(Sender: TObject);

  private
    s: TBarSeries;
    d: TSerie;
    p: TPieSeries;
    a: TAreaSeries;
    x,y,x1,y1, x3,y3: double;

    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }


procedure TForm1.Panel1Click(Sender: TObject);
begin

end;

procedure TForm1.Button9Click(Sender: TObject);
begin
      s := TBarSeries.Create(Chart1);
      Chart1.AddSerie(s);
      s.title := 'barras';
      s.SeriesColor := clRed;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
      p := TPieSeries.Create(Chart1);
      Chart1.AddSerie(p);
      p.title := 'pie';
      p.SeriesColor := clRed;
      p.MarksStyle := taseries.smsLabelPercent;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
     d:= TSerie.Create(Chart1);
     d.ShowLines := true;
     d.ShowPoints := true;
     d.PointStyle := taseries.psRectangle;
     d.title := 'line';
     d.SeriesColor := clRed;
     Chart1.AddSerie(d);

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     a := TAreaSeries.Create(Chart1);
     Chart1.AddSerie(a);
     a.SeriesColor := clred;
     a.Stairs := true;
     a.InvertedStairs := false;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
    X3 := X3 +1;
    if (random(2) >= 0.7) then Y3 := Y3 + random(5)
    else if (random(2) >= 0.7) then
         Y3 := 0
    else Y3 := Y3 - random(5);

    a.AddXY(x3, y3, '', clTAColor);
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
     s.AddXY(x,y, '', clred);

     X := X +1;
     if (random(2) >= 0.7) then Y := Y + random(5)
     else if (random(2) >= 0.7) then
          Y := 0
     else Y := Y - random(5);

end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  d.AddXY(x1,y1, '', clgreen);

  X1 := X1 +1.5;
  if (random(2) >= 0.5) then Y1 := Y1 + random(10)
  else Y1 := Y1 - random(5);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
p.AddPie(3.4234235235, 'sde21312', clTAColor);
p.AddPie(0.2323, 'adassssssdddddd', clTAColor);
p.AddPie(30, 'filipe romao', clTAColor);
p.AddPie(40, '234eds sa', clTAColor);
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
          Chart1.BottomAxis.Visible := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
       Chart1.LeftAxis.Visible := CheckBox2.Checked;
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
         Chart1.Title.Visible := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
     Chart1.Foot.Visible := CheckBox4.Checked;
end;

procedure TForm1.LegendChange(Sender: TObject);
begin
chart1.Legend.Visible := Legend.Checked;
end;


initialization
  {$I unit1.lrs}

end.

