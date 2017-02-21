unit uplot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, types,
  TAGraph, TATypes, TACustomSeries;

type
  TPlotType = (ptSymbolsOnly, ptSegments, ptCubicSpline, ptBSpline,
    ptLinearFit, ptSquareFit, ptArea, ptColumns, ptBars);

procedure PrepareChart(AChart: TChart;
  xTitle, yTitle, AChartTitle, AChartFooter: String);

function Plot(AChart: TChart; x,y: TDoubleDynArray; ALegendText: String;
  AColor: TColor; ASymbol: TSeriesPointerStyle; ALineStyle: TPenStyle;
  APlotType: TPlotType; AShowSymbols: Boolean = false): TChartSeries;


implementation

uses
  Math, LCLIntf,
  TAFitUtils, TASeries, TAFuncSeries, TATools;

procedure PrepareChart(AChart: TChart;
  xTitle, yTitle, AChartTitle, AChartFooter: String);
begin
  AChart.BottomAxis.Title.Visible := xTitle <> '';
  AChart.BottomAxis.Title.Caption := xTitle;
  AChart.BottomAxis.Title.LabelFont.Style := [fsBold];
  AChart.BottomAxis.Title.LabelFont.Size := 10;
  AChart.BottomAxis.Grid.Color := $E0E0E0;
  AChart.BottomAxis.Grid.Style := psSolid;
  AChart.BottomAxis.AxisPen.Visible := true;

  AChart.LeftAxis.Title.Visible := yTitle <> '';
  AChart.LeftAxis.Title.Caption := yTitle;
  AChart.LeftAxis.Title.LabelFont.Style := [fsBold];
  AChart.LeftAxis.Title.LabelFont.Size := 10;
  AChart.LeftAxis.Grid.Color := $E0E0E0;
  AChart.LeftAxis.Grid.Style := psSolid;
  AChart.LeftAxis.AxisPen.Visible := true;

  AChart.Title.Visible := AChartTitle <> '';
  AChart.Title.Text.Text := AChartTitle;
  AChart.Title.Font.Style := [fsBold];
  AChart.Title.Font.Size := 14;

  AChart.Foot.Visible := AChartFooter <> '';
  AChart.Foot.Text.Text := AChartFooter;
  AChart.Foot.Font.Size := 8;
  AChart.Foot.Alignment := taLeftJustify;

  AChart.Legend.SymbolWidth := 40;
  AChart.Legend.Frame.Color := clSilver;

  AChart.Backcolor := clWhite;
  AChart.Frame.Color := clSilver;

  AChart.Visible := True;
end;

function Plot(AChart: TChart; x,y: TDoubleDynArray; ALegendText: String;
  AColor: TColor; ASymbol: TSeriesPointerStyle; ALineStyle: TPenStyle;
  APlotType: TPlotType; AShowSymbols: Boolean = false): TChartSeries;
var
  i: Integer;
begin
  case APlotType of
    ptSymbolsOnly, ptSegments:
      begin
        Result := TLineSeries.Create(AChart.Owner);
        with TLineSeries(Result) do
        begin
          ShowPoints := (APlotType = ptSymbolsOnly) or AShowSymbols;
          ShowLines := (APlotType <> ptSymbolsOnly);
          LinePen.Style := ALineStyle;
          SeriesColor := AColor;
          Pointer.Brush.Color := AColor;
          Pointer.Pen.Color := AColor;
          Pointer.Style := ASymbol;
        end;
      end;
    ptCubicSpline:
      begin
        Result := TCubicSplineSeries.create(AChart.Owner);
        with TCubicSplineSeries(Result) do
        begin
          Pen.Color := AColor;
          Pen.Style := ALineStyle;
          Pointer.Brush.Color := AColor;
          Pointer.Pen.Color := AColor;
          Pointer.Style := ASymbol;
          Pointer.Visible := AShowSymbols;
        end;
      end;
    ptBSpline:
      begin
        Result := TBSplineSeries.Create(AChart.Owner);
        with TBSplineSeries(Result) do
        begin
          Pen.Color := AColor;
          Pen.Style := ALineStyle;
          Pointer.Brush.Color := AColor;
          Pointer.Pen.Color := AColor;
          Pointer.Style := ASymbol;
          Pointer.Visible := AShowSymbols;
        end;
      end;
    ptLinearFit, ptSquareFit:
      begin
        Result := TFitSeries.Create(AChart.Owner);
        with TFitSeries(Result) do
        begin
          Pen.Color := AColor;
          Pen.Style := ALineStyle;
          if APlotType = ptLinearfit then
          begin
            FitEquation := feLinear;
            Title := 'linear fit to ' + ALegendText;
          end else
          begin
            FitEquation := fePolynomial;
            ParamCount := 3;  // parabolic fit needs 3 parameters
            Title := 'parabolic fit to ' + ALegendText;
          end;
          Pointer.Brush.Color := AColor;
          Pointer.Pen.Color := AColor;
          Pointer.Style := ASymbol;
          Pointer.Visible := AShowSymbols;
        end;
      end;
    ptArea:
      begin
        Result := TAreaSeries.Create(AChart.Owner);
        with TAreaSeries(Result) do
        begin
          SeriesColor := AColor;
          AreaContourPen.Color := AColor;
          AreaLinesPen.Color := AColor;
        end;
      end;
    ptColumns, ptBars:
      begin
        Result := TBarSeries.Create(AChart.Owner);
        with TBarSeries(Result) do
        begin
          BarBrush.Color := AColor;
          BarPen.Color := AColor;
          BarWidthStyle := bwPercentMin;
          if APlotType = ptBars then begin
            AxisIndexX := AChart.LeftAxis.Index;
            AxisIndexY := AChart.BottomAxis.Index;
          end else begin
            AxisIndexX := AChart.BottomAxis.Index;
            AxisIndexY := AChart.LeftAxis.Index;
          end;
        end;
      end;
  end;

  Result.Title := ALegendText;

  if Result is TBasicPointSeries then
    with TBasicPointSeries(Result) do
      for i:=0 to Min(High(x), High(y)) do
        AddXY(x[i], y[i]);

  AChart.AddSeries(Result);
  AChart.Legend.Visible := AChart.SeriesCount > 1;

  // Paint new series in front of the others
  Result.ZPosition := AChart.SeriesCount;
end;

end.
