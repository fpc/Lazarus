unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddSeries: TButton;
    cbRotated: TCheckBox;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    cbLineType: TComboBox;
    cb3D: TCheckBox;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure btnAddSeriesClick(Sender: TObject);
    procedure cb3DChange(Sender: TObject);
    procedure cbLineTypeChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnAddSeriesClick(Sender: TObject);
var
  s: TLineSeries;
  i, j: Integer;
begin
  for i := 1 to 10 do begin
    s := TLineSeries.Create(Chart1);
    s.SeriesColor := clRed;
    for j := 1 to 1000 do
      s.AddXY(j, Random * 5 + Chart1.SeriesCount * 10);
    Chart1.AddSeries(s);
  end;
end;

procedure TForm1.cb3DChange(Sender: TObject);
begin
  with Chart1LineSeries1 do
    Depth := 15 - Depth;
end;

procedure TForm1.cbLineTypeChange(Sender: TObject);
begin
  Chart1LineSeries1.LineType := TLineType(cbLineType.ItemIndex);
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
begin
  with Chart1LineSeries1 do begin
    AxisIndexY := Ord(cbRotated.Checked);
    AxisIndexX := 1 - AxisIndexY;
  end;
end;

end.

