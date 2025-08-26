unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, TAGraph, TASeries, TACustomSource, TASources, TAStyles, TAChartUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    BarSeries: TBarSeries;
    cb3D: TCheckBox;
    ChartStyles1: TChartStyles;
    cbRotated: TCheckBox;
    cbShowLabels: TCheckBox;
    cbGridCentered: TCheckBox;
    cmbShape: TComboBox;
    lblShape: TLabel;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    procedure cb3DChange(Sender: TObject);
    procedure cbGridCenteredChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
    procedure cbShowLabelsChange(Sender: TObject);
    procedure cmbShapeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cb3DChange(Sender: TObject);
begin
  if cb3D.Checked then
    BarSeries.Depth := 20
  else
    BarSeries.Depth := 0;
  cbGridCenteredChange(nil);
  cbGridCentered.Enabled := not cb3D.Checked;
end;

procedure TForm1.cbGridCenteredChange(Sender: TObject);
begin
  Chart1.BottomAxis.GridCentered := cbGridCentered.Checked and not cb3D.Checked;
  Chart1.BottomAxis.Grid.Visible := Chart1.BottomAxis.GridCentered;
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
begin
  if cbRotated.Checked then begin
    BarSeries.AxisIndexX := 0;
    BarSeries.AxisIndexY := 1;
  end else begin
    BarSeries.AxisIndexX := 1;
    BarSeries.AxisIndexY := 0;
  end;
end;

procedure TForm1.cbShowLabelsChange(Sender: TObject);
begin
  BarSeries.Marks.Visible := cbShowLabels.Checked;
end;

procedure TForm1.cmbShapeChange(Sender: TObject);
begin
  BarSeries.BarShape := TBarShape(cmbShape.ItemIndex);
  cb3DChange(nil);
end;

function RandomString(ALength: Integer): String;
var
  i: Integer;
begin
  SetLength(Result, ALength);
  Result[1] := Char(ord('A') + Random(26));
  for i := 2 to ALength do
    Result[i] := Char(ord('a') + Random(26));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: Integer;
  s: String;
begin
  RandSeed := 1;
  BarSeries.Marks.Style := smsLabel;
  BarSeries.Marks.YIndex := -1;
  BarSeries.ListSource.YCount := 3;
  BarSeries.ListSource.LabelSeparator:= ';';
  for i := 0 to 5 do
  begin
    s := RandomString(1 + Random(4));
    for j := 2 to BarSeries.ListSource.YCount do
      s := s + ';' + RandomString(1 + Random(4));
    BarSeries.AddXY(i, 20+Random*100, [20+Random*100, 20+Random*100], s);
  end;

  // Set up bottom axis such that only ticks are displayed at the bars.
  Chart1.BottomAxis.Intervals.Options := Chart1.BottomAxis.Intervals.Options + [aipInteger];
end;

end.

