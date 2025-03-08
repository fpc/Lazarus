unit frmOHLC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls,
  TAGraph, TAChartUtils, TAMultiSeries;

type
  TOHLCFrame = class(TFrame)
    Chart: TChart;
    OpenHighLowCloseSeries: TOpenHighLowCloseSeries;
  private

  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation

{$R *.lfm}

constructor TOHLCFrame.Create(AOwner: TComponent);
var
  ylist: array[1..4] of Double;
  i, j: Integer;
  y, y0: Double;
begin
  inherited;

  OpenHighLowCloseSeries.ListSource.YCount := 4;
  y := 50;
  for i := 1 to 50 do begin
    y += Random(80) / 10 - 4;
    ylist[1] := y;
    for j := 1 to 3 do begin
      ylist[j] += Random(20) / 10 + 1;
      ylist[j + 1] := ylist[j];
    end;
    if Random(3) = 1 then
      Exchange(ylist[1], ylist[2]);
    OpenHighLowCloseSeries.AddXY(i, y, ylist);
  end;
end;

end.

