unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, TAGraph, TASeries, TASources, TAStyles;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    BarSeries: TBarSeries;
    cb3D: TCheckBox;
    ChartStyles1: TChartStyles;
    cbRotated: TCheckBox;
    cmbShape: TComboBox;
    lblLevels: TLabel;
    lblShape: TLabel;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    seLevels: TSpinEdit;
    procedure cb3DChange(Sender: TObject);
    procedure cbRotatedChange(Sender: TObject);
    procedure cmbShapeChange(Sender: TObject);
    procedure seLevelsChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.cb3DChange(Sender: TObject);
begin
  if cb3D.Checked then
    BarSeries.Depth := 20
  else
    BarSeries.Depth := 0;
  if BarSeries.IsRotated then begin
    Chart1.Margins.Top := 4 + IfThen(BarSeries.BarShape in [bsRectangular, bsPyramid], Barseries.Depth, 0);
    Chart1.Margins.Right := 4 + BarSeries.Depth;
  end else begin
    Chart1.Margins.Top := 4 + Barseries.Depth;
    Chart1.Margins.Right := 4 + IfThen(BarSeries.BarShape in [bsRectangular, bsPyramid], Barseries.Depth, 0);
  end;
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

procedure TForm1.cmbShapeChange(Sender: TObject);
begin
  BarSeries.BarShape := TBarShape(cmbShape.ItemIndex);
  cb3DChange(nil);
end;

procedure TForm1.seLevelsChange(Sender: TObject);
begin
  RandomChartSource1.YCount := seLevels.Value;
end;

end.

