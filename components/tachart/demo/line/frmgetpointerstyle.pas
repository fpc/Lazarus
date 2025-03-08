unit frmGetPointerStyle;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls,
  TAGraph, TASources, TATypes, TACustomSeries, TASeries;

type
  TGetPointerStyleFrame = class(TFrame)
    Chart: TChart;
    LineSeries: TLineSeries;
    RandomChartSource: TRandomChartSource;
    procedure LineSeriesGetPointerStyle({%H-}ASender: TChartSeries;
      AValueIndex: Integer; var AStyle: TSeriesPointerStyle);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TGetPointerStyleFrame.LineSeriesGetPointerStyle(ASender:
  TChartSeries; AValueIndex: Integer; var AStyle: TSeriesPointerStyle);
begin
  AStyle := TSeriesPointerStyle(AValueIndex mod (ord(High(TSeriesPointerStyle))+1));
end;

end.

