unit frmSubMarks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  TAGraph, TAChartUtils, TASeries, TASources, TACustomSource;

type
  TSubMarksFrame = class(TFrame)
    ChartSubmarks: TChart;
    ChartSubmarksLineSeries1: TLineSeries;
    udcsGraph: TUserDefinedChartSource;
    procedure udcsGraphGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TSubMarksFrame.udcsGraphGetChartDataItem(ASource:
  TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  AItem.X := (AIndex - 50) * 0.1;
  AItem.Y := Sin(AItem.X * 3) + AItem.X / 10;
end;

end.

