program noguidemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, tachartlazaruspkg
  { you can add units after this },
  Interfaces, TAGraph, TASeries, Graphics;

var
  chart: TChart;
  bs: TBarSeries;
begin
  chart := TChart.Create(nil);
  bs := TBarSeries.Create(nil);
  chart.AddSeries(bs);
  bs.AddXY(1, 10);
  bs.AddXY(2, 7);
  bs.AddXY(3, 8);
  chart.SaveToBitmapFile('test.bmp');
  bs.Free;
  chart.Free;
end.

