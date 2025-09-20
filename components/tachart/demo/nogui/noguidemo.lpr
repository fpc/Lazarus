program noguidemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
  Interfaces, Classes, { you can add units after this }
  FPCanvas, FPImage, FPImgCanv,
  TAGraph, TASeries, TADrawerFPCanvas, TADrawerCanvas, TADrawUtils;

const
  FONT_NAME = 'LiberationSans-Regular.ttf';

var
  chart: TChart;
  bs: TBarSeries;
  img: TFPMemoryImage;
  c: TFPImageCanvas;
  d: IChartDrawer;
begin
  InitFonts;
  chart := TChart.Create(nil);
  chart.Width := 600;
  chart.Height := 400;
  chart.Title.Text.Text := 'TAChart nogui demo';
  chart.Title.Font.Size := 16;
  chart.Title.Font.Name := FONT_NAME;
  chart.Title.Visible := true;
  chart.LeftAxis.Marks.LabelFont.Name := FONT_NAME;
  chart.LeftAxis.Marks.LabelFont.Size := 10;
  chart.LeftAxis.Marks.LabelFont.Orientation := 450;
  chart.LeftAxis.Marks.Frame.Visible := true;
  chart.LeftAxis.Marks.Frame.Style := psSolid;
  chart.LeftAxis.Marks.Frame.FPColor := colBlack;
  chart.LeftAxis.Grid.FPColor := colSilver;
  chart.BottomAxis.Marks.Visible := false;
  chart.BottomAxis.Grid.FPColor := colSilver;
  chart.Color := $FFA0A0;
  chart.BackColor := $FFFFFF;
  bs := TBarSeries.Create(nil);
  chart.AddSeries(bs);
  bs.AddXY(1, 10);
  bs.AddXY(2, 7);
  bs.AddXY(3, 8);
  img := TFPMemoryImage.Create(chart.Width, chart.Height);
  c := TFPImageCanvas.Create(img);
  d := TFPCanvasDrawer.Create(c);
  d.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
  chart.Draw(d, Rect(0, 0, chart.Width, chart.Height));
  img.SaveToFile('test.png');
  c.Free;
  img.Free;
  bs.Free;
  chart.Free;
end.

