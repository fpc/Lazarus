unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Types,
  TAGraph, TASeries, TATools, TAStyles, TASources,  TAChartUtils, TAMultiSeries,
  TARadialSeries, TAFuncSeries, TACustomSeries, TADrawUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart: TChart;
    BarSeries: TBarSeries;
    AreaSeries: TAreaSeries;
    CbCandleStick: TCheckBox;
    BoxWhiskerSeries: TBoxAndWhiskerSeries;
    BubbleSeries: TBubbleSeries;
    BSplineSeries: TBSplineSeries;
    PieSeriesDatapointDragTool: TDataPointDragTool;
    PieSeries: TPieSeries;
    FitSeries: TFitSeries;
    FieldSeries: TFieldSeries;
    FuncSeries: TFuncSeries;
    ConstantLineSeries: TConstantLine;
    CubicSplineSeries: TCubicSplineSeries;
    ClickTool: TDataPointClickTool;
    FieldSeriesDatapointDragTool: TDataPointDragTool;
    PolarSeries: TPolarSeries;
    OHLCSeries: TOpenHighLowCloseSeries;
    ChartStyles: TChartStyles;
    CbDragXY: TCheckBox;
    DatapointInfo: TLabel;
    LblNOTE: TLabel;
    LblUSAGE: TLabel;
    ListChartSource: TListChartSource;
    CrosshairTool: TDataPointCrosshairTool;
    HintTool: TDataPointHintTool;
    BubbleSeriesDatapointDragTool: TDataPointDragTool;
    Panel1: TPanel;
    Toolset: TChartToolset;
    LineSeries: TLineSeries;
    DataPointDragTool: TDataPointDragTool;
    TabControl: TTabControl;
    procedure CbCandleStickChange(Sender: TObject);
    procedure CbDragXYChange(Sender: TObject);
    procedure ClickToolPointClick(ATool: TChartTool; APoint: TPoint);
    procedure CrosshairToolAfterKeyUp(ATool: TChartTool; APoint: TPoint);
    procedure CrosshairToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure CrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure FormCreate(Sender: TObject);
    procedure DataPointDragToolDragXY(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure DataPointDragToolDragY(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure FuncSeriesCalculate(const AX: Double; out AY: Double);
    procedure HintToolHint(ATool: TDataPointHintTool; const APoint: TPoint;
      var AHint: String);
    procedure TabControlChange(Sender: TObject);

  private
    function GetDatapointInfo(ATool: TDatapointTool): String;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TALegend;

{ TMainForm }

procedure TMainForm.CbCandleStickChange(Sender: TObject);
begin
  if CbCandleStick.Checked then
    OHLCSeries.Mode := mCandleStick
  else
    OHLCSeries.Mode := mOHLC;
end;

procedure TMainForm.CbDragXYChange(Sender: TObject);
begin
  if CbDragXY.Checked then
  begin
    DataPointDragTool.OnDrag := @DatapointDragToolDragXY;
    BubbleSeriesDataPointDragTool.OnDrag := nil;
  end else
  begin
    DataPointDragTool.OnDrag := @DatapointDragToolDragY;
    BubbleSeriesdataPointDragTool.OnDrag := @DatapointDragToolDragY;
  end;
end;

procedure TMainForm.ClickToolPointClick(ATool: TChartTool; APoint: TPoint);
begin
  DatapointInfo.Caption := '';
  ShowMessage(GetDatapointInfo(TDatapointTool(ATool)));
end;

procedure TMainForm.CrosshairToolAfterKeyUp(ATool: TChartTool; APoint: TPoint);
begin
  TDatapointCrosshairTool(ATool).Hide;
end;

procedure TMainForm.CrosshairToolAfterMouseUp(ATool: TChartTool; APoint: TPoint
  );
begin
  TDatapointCrosshairTool(ATool).Hide;
end;

procedure TMainForm.CrosshairToolDraw(ASender: TDataPointDrawTool);
begin
  DatapointInfo.Caption := GetDatapointInfo(ASender);
end;

// Avoids changing of the x value while dragging a data point
procedure TMainForm.DataPointDragToolDragY(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
begin
  AGraphPoint.X := ASender.Origin.X;
end;

// Modifies the x value if one of the levels of a multi-y-value series is dragged.
// e.g, stacked series, ohlc, box-whisker.
procedure TMainForm.DataPointDragToolDragXY(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
var
  ser: TChartSeries;
begin
  if (ASender.YIndex > 0) and (ASender.Series is TChartSeries) then begin
    ser := TChartSeries(ASender.Series);
    ser.XValue[ASender.PointIndex] := AGraphPoint.X;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  N = 10;
var
  i: Integer;
  x, x2, y1, y2, y3,yO, yH, yL, yC: Double;
begin
  ListChartSource.YCount := 2;
  ListChartSource.Sorted := true;
  for i:=0 to N-1 do begin
    x := i + (Random - 0.5) * 0.4;
    y1 := x + Random*2;
    y2 := y1 + Random*2;
    ListChartSource.AddXYList(x, [y1, y2]);
  end;
  ListChartSource.Sorted := false;

  OHLCSeries.ListSource.YCount := 4;
  OHLCSeries.ListSource.Sorted := true;
  for i:=0 to N-1 do begin
    x := i + Random;
    yL := x + Random;
    yH := yL + Random + 5;
    yO := yL + Random * 5;
    yC := yL + Random * 5;
    OHLCSeries.AddXOHLC(x, yO, yH, yL, yC);
  end;
  OHLCSeries.ListSource.Sorted := false;

  BoxWhiskerSeries.ListSource.YCount := 5;
  BoxWhiskerSeries.ListSource.Sorted := true;
  for i:=0 to N-1 do begin
    x := i + (Random-0.5)*0.4;
    yL := x * Random;
    y1 := yL + Random;
    y2 := y1 + Random;
    y3 := y2 + Random;
    yH := y3 + Random;
    BoxWhiskerSeries.AddXY(x, yL, y1, y2, y3, yH);
  end;
  BoxWhiskerSeries.ListSource.Sorted := true;

  BubbleSeries.ListSource.YCount := 3;
  for i:=0 to N-1 do begin
    x := (Random - 0.5) * 2.0;
    y1 := (Random - 0.5) * 2.0;
    y2 := Random*0.3;
    BubbleSeries.AddXY(x, y1, y2);
  end;

  FitSeries.Clear;
  for i:= 0 to N - 1 do begin
    x := Random * 10;
    y1 := x + random * 1;
    FitSeries.AddXY(x, y1);
  end;

  FieldSeries.ListSource.XCount := 2;
  FieldSeries.ListSource.YCount := 2;
  for i:=0 to N-1 do begin
    x := Random * 10;
    x2 := x + Random;
    y1 := Random * 10;
    y2 := y1 + Random;
    FieldSeries.AddVector(x, y1, x2, y2);
  end;

  PieSeries.ListSource.Clear;
  for i:=0 to 4 do begin
    y1 := Random*10;
    PieSeries.AddXY(0, y1, 'Item ' + IntToStr(i+1));
  end;
  PieSeries.Legend.Multiplicity := lmPoint;
  PieSeries.Marks.Style := smsLabel;
  PieSeries.Exploded := true;

  LineSeries.Index := 0;
  BarSeries.Index := 1;
  AreaSeries.Index := 2;
  PolarSeries.Index := 3;
  CubicSplineSeries.Index := 4;
  BSplineSeries.Index := 5;
  FitSeries.Index := 6;
  OHLCSeries.Index := 7;
  BoxWhiskerSeries.Index := 8;
  BubbleSeries.Index := 9;
  FieldSeries.Index := 10;
  FuncSeries.Index := 11;
  ConstantLineSeries.Index := 12;
  PieSeries.Index := 13;

  TabControlChange(nil);
  DatapointInfo.Caption := '';
end;

procedure TMainForm.FuncSeriesCalculate(const AX: Double; out AY: Double);
begin
  AY := sin(AX);
end;

function TMainForm.GetDataPointInfo(ATool: TDataPointTool): String;
var
  ser: TChartSeries;
begin
  Result := '';
  if ATool.Series is TConstantLine then
    Result := Format('"%s": Position %.2f', [
      TConstantLine(ATool.Series).Title, TConstantLine(ATool.Series).Position
    ])
  else
  if ATool.Series is TFuncSeries then
    Result := Format('"%s": x = %.2f, y = %,2f', [
      TFuncSeries(ATool.Series).Title, ATool.NearestGraphPoint.X, ATool.NearestGraphPoint.Y
    ])
  else
  if ATool.Series is TChartSeries then begin
    ser := TChartSeries(ATool.Series);
    if ser <> nil then begin
      if (ATool.YIndex = -1) and
        ((ser is TFitSeries) or (ser is TCubicSplineSeries) or (ser is TBSplineSeries))
      then
        Result := Format('"%s": x = %.2f, y = %.2f', [
          ser.Title,
          ATool.NearestGraphPoint.X, ATool.NearestGraphPoint.Y
        ])
      else
      if (ATool.PointIndex > -1) and (ser.Source.YCount = 1) then
        Result := Format('"%s": Point index %d, x = %.2f, y = %.2f', [
          ser.Title,
          ATool.PointIndex,
          ser.XValue[ATool.PointIndex],
          ser.YValue[ATool.PointIndex]
        ])
      else
      if (ATool.PointIndex > -1) and (ser.Source.YCount > 1) and (ATool.YIndex > -1) then
        Result := Format('"%s": Point index %d, x = %.2f, y = %.2f (y index %d)', [
          ser.Title,
          ATool.PointIndex,
          ser.XValue[ATool.PointIndex],
          ser.YValues[ATool.PointIndex, ATool.YIndex],
          ATool.YIndex
        ]);
    end;
  end;
end;

procedure TMainForm.HintToolHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
begin
  AHint := GetDatapointInfo(ATool);
  DatapointInfo.Caption := '';
end;

procedure TMainForm.TabControlChange(Sender: TObject);
var
  ser: TBasicChartSeries;
  s: String;
begin
  // Show only the series of the corresponding tab
  for ser in Chart.Series do
    ser.Active := ser.Index = TabControl.TabIndex;

  // Avoid tools react on hidden series
  s := IntToStr(TabControl.TabIndex);
  CrosshairTool.AffectedSeries := s;
  HintTool.AffectedSeries := s;

  BubbleSeriesDatapointDragTool.Enabled := TabControl.TabIndex = BubbleSeries.Index;
  FieldSeriesDatapointDragTool.Enabled := TabControl.TabIndex = FieldSeries.Index;
  PieSeriesDatapointDragTool.Enabled := TabControl.TabIndex = PieSeries.Index;
  DataPointDragTool.Enabled := not
    (BubbleSeriesDatapointDragTool.Enabled or FieldSeriesDatapointDragtool.Enabled);

  if BubbleSeriesDatapointDragTool.Enabled then
    BubbleSeriesDatapointDragTool.AffectedSeries := s
  else
  if FieldSeriesDatapointDragTool.Enabled then
    FieldSeriesDatapointDragTool.AffectedSeries := s
  else
  if PieSeriesDatapointDragTool.Enabled then
    PieSeriesDatapointDragtool.AffectedSeries := s
  else
    DatapointDragtool.AffectedSeries := s;

  // Treatment of special requirements of particular series
  CbCandleStick.Visible := OHLCSeries.Active;
  LblNOTE.Visible := LineSeries.Active or BarSeries.Active or AreaSeries.Active;
  Chart.LeftAxis.Range.UseMax := ConstantLineSeries.Active;
  Chart.LeftAxis.Range.UseMin := ConstantLineSeries.Active;

  if PieSeries.Active then
    LblUsage.Caption := StringReplace(LblUsage.Caption, 'datapoint', 'pie', [])
  else
    LblUsage.Caption := StringReplace(LblUsage.Caption, 'pie', 'datapoint', []);

  s := '';
  if LineSeries.Active or BarSeries.Active or AreaSeries.Active then begin
    s := 'These are stacked series, i.e. they share the same x ' +
      'value defined by the red series --> When you drag a datapoint of any ' +
      'series the x value of the other series will follow.';
    if BarSeries.Active then
      s := s + LineEnding + 'Grab the bars at their upper ends.';
  end else
  if FieldSeries.Active then
    s := 'Rotate an arrow by grabbing it at its ends.' + LineEnding +
      'Shift it by grabbing it in the middle.' + LineEnding +
      'The checkbox "Drag both x and y" is not operating here.'
  else
  if BubbleSeries.Active then
    s := 'Grab a bubble at its perimeter to change the radius.' + LineEnding +
      'Grab it in the center to change its location.' + LineEnding +
      'Overlapping bubbles may be detected erroneously.';
  if s <> '' then
    LblNOTE.Caption := 'NOTE:' + LineEnding + s;
  LblNOTE.Visible := (s <> '');

  CbDragXY.Enabled := not PieSeries.Active;
end;

end.

