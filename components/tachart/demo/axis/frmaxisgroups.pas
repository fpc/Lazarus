unit frmAxisGroups;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  TAGraph, TAChartAxis, TACustomSource, TASeries, TATransformations;

type
  TAxisGroupsFrame = class(TFrame)
    ChartAxisGroup: TChart;
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TAxisGroupsFrame.Create(AOwner: TComponent);
const
  COLORS: array [1..5] of Integer =
    ($0000A0, $002080, $004060, $006040, $008020);
var
  i, j: Integer;
  ls: TLineSeries;
  tr: TChartAxisTransformations;
begin
  inherited;

  for i := 1 to 5 do begin
    ls := TLineSeries.Create(Self);
    ChartAxisGroup.AddSeries(ls);
    ls.SeriesColor := COLORS[i];
    ls.LinePen.Width := 2;
    for j := 1 to 20 do
      ls.AddXY(j, Random * 8);
    tr := TChartAxisTransformations.Create(Self);
    with TAutoScaleAxisTransform.Create(Self) do begin
      Transformations := tr;
      MinValue := i + 0.2;
      MaxValue := i + 0.8;
    end;
    with TChartAxis.Create(ChartAxisGroup.AxisList) do begin
      Transformations := tr;
      Marks.AtDataOnly := true;
      Marks.Range.UseMin := true;
      Marks.Range.UseMax := true;
      Marks.Range.Min := 1;
      Marks.Range.Max := 9;
      Marks.LabelFont.Orientation := 900;
      Marks.LabelFont.Color := COLORS[i];
      with Marks.DefaultSource.Params do begin
        MinLength := 5;
        MaxLength := 20;
        Options := Options + [aipUseCount];
      end;
      TickColor := COLORS[i];
      Group := 1;
    end;
    ls.AxisIndexY := ChartAxisGroup.AxisList.Count - 1;
  end;
end;

end.

