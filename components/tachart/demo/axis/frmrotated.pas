unit frmRotated;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  SysUtils, Classes, Controls, StdCtrls, ExtCtrls, Forms,
  TAGraph, TAChartUtils, TASeries, TASources, TAStyles;

type
  TRotatedSeriesFrame = class(TFrame)
    cbRotated: TCheckBox;
    ChartRotated: TChart;
    ChartRotatedBarSeries: TBarSeries;
    csRotated: TChartStyles;
    lcsRotatedSeries: TListChartSource;
    Panel2: TPanel;
    procedure cbRotatedChange(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TRotatedSeriesFrame.cbRotatedChange(Sender: TObject);
begin
  if cbRotated.Checked then
  begin
    ChartRotatedBarSeries.AxisIndexX := 0;
    ChartRotatedBarSeries.AxisIndexY := 1;
    ChartRotated.BottomAxis.Marks.Source := nil;
    ChartRotated.BottomAxis.Marks.Style := smsValue;
    ChartRotated.LeftAxis.Marks.Source := lcsRotatedSeries;
    ChartRotated.LeftAxis.Marks.Style := smsLabel;
  end else
  begin
    ChartRotatedBarSeries.AxisIndexX := 1;
    ChartRotatedBarSeries.AxisIndexY := 0;
    ChartRotated.LeftAxis.Marks.Source := nil;
    ChartRotated.LeftAxis.Marks.Style := smsValue;
    ChartRotated.BottomAxis.Marks.Source := lcsRotatedSeries;
    ChartRotated.BottomAxis.Marks.Style := smsLabel;
  end;

  // This exchanges the x/y value of the source for the marks being displayed
  // along the axis. This is necessary because the axis does not "know" that
  // the series uses rotated x/y values.
  ChartRotated.LeftAxis.Marks.SourceExchangeXY := cbRotated.Checked;
end;

end.

