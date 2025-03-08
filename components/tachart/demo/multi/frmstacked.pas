unit frmStacked;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, StdCtrls, ExtCtrls,
  TAGraph, TAChartUtils, TASources, TAStyles, TASeries;

type
  TStackedFrame = class(TFrame)
    cbPercentage: TCheckBox;
    ccsStacked: TCalculatedChartSource;
    cgShowStackLevels: TCheckGroup;
    ChartStyles: TChartStyles;
    Chart: TChart;
    AreaSeries: TAreaSeries;
    BarSeries: TBarSeries;
    LineSeries: TLineSeries;
    pnStackedControls: TPanel;
    rcsStacked: TRandomChartSource;
    rgStackedSeries: TRadioGroup;
    procedure cbPercentageChange(Sender: TObject);
    procedure cgShowStackLevelsItemClick(Sender: TObject; Index: integer);
    procedure rgStackedSeriesClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TStackedFrame.cbPercentageChange(Sender: TObject);
begin
  ccsStacked.Percentage := cbPercentage.Checked;
end;

procedure TStackedFrame.cgShowStackLevelsItemClick(Sender: TObject; Index:
  integer);
var
  s: String;
  i: Integer;
begin
  Unused(Index);
  s := '';
  for i := 0 to cgShowStackLevels.Items.Count - 1 do begin
    if cgShowStackLevels.Checked[i] then
      s += Format('%d,', [i]);
    ChartStyles.Styles[i].RepeatCount := Ord(cgShowStackLevels.Checked[i]);
  end;
  ccsStacked.ReorderYList := s[1..Length(s) - 1];
end;

procedure TStackedFrame.rgStackedSeriesClick(Sender: TObject);
var
  i: Integer;
begin
  i := rgStackedSeries.ItemIndex;
  AreaSeries.Active := i = 0;
  BarSeries.Active := i = 1;
  LineSeries.Active := i = 2;
end;

end.

