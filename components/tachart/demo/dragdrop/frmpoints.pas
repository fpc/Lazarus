unit frmPoints;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, Math,
  Forms, Graphics, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAChartUtils, TASeries, TATools, Types, TACustomSeries;

type
  TPointsFrame = class(TFrame)
    cbSorted: TCheckBox;
    Chart: TChart;
    ChartLineSeries1: TLineSeries;
    Toolset: TChartToolset;
    DataPointClickTool: TDataPointClickTool;
    DataPointDragTool: TDataPointDragTool;
    DataPointHintTool: TDataPointHintTool;
    Panel1: TPanel;
    procedure cbSortedChange(Sender: TObject);
    procedure ChartLineSeries1GetMark(out AFormattedMark: String; AIndex:
      Integer);
    procedure DataPointClickToolPointClick(ATool: TChartTool; APoint: TPoint);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool; const APoint:
      TPoint; var AHint: String);
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TPointsFrame.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 1 to 10 do
    ChartLineSeries1.AddXY(i, Random(20) - 10);
end;

procedure TPointsFrame.cbSortedChange(Sender: TObject);
begin
  ChartLineSeries1.ListSource.Sorted := cbSorted.Checked;
end;

procedure TPointsFrame.ChartLineSeries1GetMark(out AFormattedMark: String;
  AIndex: Integer);
begin
  // Show item label for last dragged point.
  if AIndex = DataPointDragTool.PointIndex then
    with ChartLineSeries1 do
      AFormattedMark := Source{%H-}.FormatItem(Marks.Format, AIndex, 0)
  else
    AFormattedMark := '';
end;

procedure TPointsFrame.DataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
var
  pi: Integer;
begin
  Unused(ATool, APoint);
  pi := DataPointClickTool.PointIndex;
  with ChartLineSeries1 do
    SetColor(pi, IfThen(GetColor(pi) = clYellow, clTAColor, clYellow));
end;

procedure TPointsFrame.DataPointHintToolHint(ATool: TDataPointHintTool; const
  APoint: TPoint; var AHint: String);
begin
  Unused(APoint);
  AHint := 'Custom hint for point ' + IntToStr(ATool.PointIndex);
end;

end.

