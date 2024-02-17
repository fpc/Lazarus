unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  TAGraph, TATools, TAChartAxis, TASeries, TASources, TATransformations, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Chart: TChart;
    cbShowGrid_LeftAxis2: TCheckBox;
    cbShowGrid_LeftAxis1: TCheckBox;
    cbShowGrid_RightAxis: TCheckBox;
    cbShowGrid_BottomAxis: TCheckBox;
    RedSeries: TLineSeries;
    BlueSeries: TLineSeries;
    RedAxisTransformations: TChartAxisTransformations;
    RedAxisTransformationsAutoScaleAxisTransform: TAutoScaleAxisTransform;
    BlueAxisTransformations: TChartAxisTransformations;
    BlueAxisTransformationsAutoScaleAxisTransform: TAutoScaleAxisTransform;
    ChartToolset: TChartToolset;
    ChartToolsetAxisClickTool1: TAxisClickTool;
    ChartToolsetUserDefinedTool1: TUserDefinedTool;
    Label1: TLabel;
    Label2: TLabel;
    lblClickedAxis: TLabel;
    lblClickedAxisPart: TLabel;
    BottomPanel: TPanel;
    RedChartSource: TRandomChartSource;
    BlueChartSource: TRandomChartSource;
    procedure ChartToolsetAxisClickTool1Click(ASender: TChartTool;
      Axis: TChartAxis; AHitInfo: TChartAxisHitTests);
    procedure ChartToolsetUserDefinedTool1AfterMouseDown(ATool: TChartTool;
      APoint: TPoint);
    procedure cbShowGrid_LeftAxis2Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ChartToolsetAxisClickTool1Click(ASender: TChartTool;
  Axis: TChartAxis; AHitInfo: TChartAxisHitTests);
var
  s: String;
begin
  s := '';
  if ahtGrid in AHitInfo then s := s + ', Axis grid';
  if ahtTitle in AHitInfo then s := s + ', Axis title';
  if ahtLine in AHitInfo then s := s + ', Axis line';
  if ahtLabels in AHitInfo then s := s + ', Axis labels';
  if ahtAxisStart in AHitInfo then s := s + ', begin of axis';
  if ahtAxisCenter in AHitInfo then s := s + ', center of axis';
  if ahtAxisEnd in AHitInfo then s := s + ', end of axis';
  if s <> '' then
  begin
    Delete(s, 1, 2);
    lblClickedAxis.Caption := Axis.Title.Caption;
    lblClickedAxisPart.Caption := s;
  end;
end;

procedure TForm1.ChartToolsetUserDefinedTool1AfterMouseDown(ATool: TChartTool;
  APoint: TPoint);
begin
  lblClickedAxis.Caption := '(none)';
  lblClickedAxisPart.Caption := '(none)';
  ATool.Handled;
end;

procedure TForm1.cbShowGrid_LeftAxis2Change(Sender: TObject);
begin
  if Sender = cbShowGrid_LeftAxis2 then
    Chart.LeftAxis.Grid.Visible := TCheckbox(Sender).Checked
  else if Sender = cbShowGrid_LeftAxis1 then
    Chart.AxisList[3].Grid.Visible := TCheckbox(Sender).Checked
  else if Sender = cbShowGrid_RightAxis then
    Chart.AxisList[2].Grid.Visible := TCheckbox(Sender).Checked
  else if Sender = cbShowGrid_BottomAxis then
    Chart.BottomAxis.Grid.Visible := TCheckbox(Sender).Checked;
end;

end.

