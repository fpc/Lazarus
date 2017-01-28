unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TATransformations, TASeries, TASources,
  Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    ChartAxisTransformations2: TChartAxisTransformations;
    ChartAxisTransformations2AutoScaleAxisTransform1: TAutoScaleAxisTransform;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TAChartAxis;

{ TForm1 }

procedure TForm1.CheckBox1Change(Sender: TObject);
var
  leftAxis1, leftAxis2: TChartAxis;
begin
  leftAxis1 := Chart1.AxisList[0];
  leftAxis2 := Chart1.AxisList[2];

  leftAxis1.AtDataOnly := Checkbox1.Checked;
  leftAxis1.Marks.AtDataOnly := Checkbox1.Checked;
  leftAxis1.Title.PositionOnMarks := Checkbox1.Checked;

  leftAxis2.AtDataOnly := Checkbox1.Checked;
  leftAxis2.Marks.AtDataOnly := Checkbox1.Checked;
  leftAxis2.Title.PositionOnMarks := Checkbox1.Checked;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
var
  leftAxis1, leftAxis2: TChartAxis;
begin
  leftAxis1 := Chart1.AxisList[0];
  leftAxis2 := Chart1.AxisList[2];

  if Checkbox2.Checked then begin
    leftAxis1.Group := 1;
    leftAxis2.Group := 1;
  end else begin
    leftAxis1.Group := 0;
    leftAxis2.Group := 0;
  end;
end;

end.

