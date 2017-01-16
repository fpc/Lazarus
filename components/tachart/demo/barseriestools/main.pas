unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, TATools, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Types,
  TAChartUtils, TACustomSeries, TAStyles, TADataTools;

type

  { TMainForm }

  TMainForm = class(TForm)
    CalculatedChartSource: TCalculatedChartSource;
    Chart: TChart;
    BarSeriesREDYELLOW: TBarSeries;
    BarSeriesBLUE: TBarSeries;
    ChartStyles: TChartStyles;
    ChartToolset: TChartToolset;
    DataPointDragTool: TDataPointDragTool;
    DataPointClickTool: TDataPointClickTool;
    DataPointHintTool: TDataPointHintTool;
    CbHorizontal: TCheckBox;
    Cb100Percent: TCheckBox;
    LblToolTargets: TLabel;
    LblInfo: TLabel;
    ListChartSourceLABELS: TListChartSource;
    ListChartSourceREDYELLOW: TListChartSource;
    ListChartSourceBLUE: TListChartSource;
    Log: TMemo;
    ToolInfoPanel: TPanel;
    TargetPanel: TPanel;
    LeftPanel: TPanel;
    RbToolTargetDataPoint: TRadioButton;
    RbToolTargetBar: TRadioButton;
    LogSplitter: TSplitter;
    procedure DataPointClickToolPointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure DataPointDragToolDrag(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure DataPointDragToolDragStart(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure CbHorizontalChange(Sender: TObject);
    procedure Cb100PercentChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolTargetChanged(Sender: TObject);
  private
    FDragStart: TDoublePoint;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType;

{ TMainForm }

procedure TMainForm.CbHorizontalChange(Sender: TObject);
begin
  if CbHorizontal.Checked then
  begin
    TChartSeries(Chart.Series[0]).AxisIndexX := 0;
    TChartSeries(Chart.Series[0]).AxisIndexY := 1;
    TChartSeries(Chart.Series[1]).AxisIndexX := 0;
    TChartSeries(Chart.Series[1]).AxisIndexY := 1;
    Chart.LeftAxis.Marks.Source := ListChartSourceLABELS;
    Chart.LeftAxis.Marks.Style := smsLabel;
    Chart.BottomAxis.Marks.Source := nil;
    Chart.BottomAxis.Marks.Style := smsValue;
    Chart.Margins.Left := 0;
    Chart.Margins.Bottom := 4;
  end else
  begin
    TChartSeries(Chart.Series[0]).AxisIndexX := 1;
    TChartSeries(Chart.Series[0]).AxisIndexY := 0;
    TChartSeries(Chart.Series[1]).AxisIndexX := 1;
    TChartSeries(Chart.Series[1]).AxisIndexY := 0;
    Chart.BottomAxis.Marks.Source := ListChartSourceLABELS;
    Chart.BottomAxis.Marks.Style := smsLabel;
    Chart.LeftAxis.Marks.Source := nil;
    Chart.LeftAxis.Marks.Style := smsValue;
    Chart.Margins.Bottom := 0;
    Chart.Margins.Left := 4;
  end;
end;

procedure TMainForm.Cb100PercentChange(Sender: TObject);
begin
  if Cb100Percent.Checked then
    BarSeriesREDYELLOW.Source := CalculatedChartSource
  else
    BarSeriesREDYELLOW.Source := ListChartSourceREDYELLOW;
//  CalculatedChartSource.Percentage := Cb100Percent.Checked;
end;

procedure TMainForm.DataPointClickToolPointClick(ATool: TChartTool;
  APoint: TPoint);
var
  ser: TChartSeries;
  idx: Integer;
  yidx: Integer;
  ttl: String;
begin
  with TDatapointClickTool(ATool) do begin
    ser := Series as TChartSeries;
    idx := PointIndex;
    yidx := YIndex;
  end;

  if ser <> nil then begin
    ttl := ser.Title;
    if yidx > 0 then
      ttl := TBarSeries(ser).Styles.Styles[yidx].Text;
    Log.Lines.Add(Format('"%s" clicked:', [ttl]));
    Log.Lines.Add(Format('    point #%d, x = %.1f (%s), y = %.1f',
      [ idx, ser.GetXValue(idx), ListChartSourceLABELS.Item[idx]^.Text,
        ser.Source[idx]^.GetY(yidx) ]
    ));
    Log.SelStart := Length(Log.Lines.Text);
  end;
end;

procedure TMainForm.DataPointDragToolDrag(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
var
  ser: TChartSeries;
begin
  ser := TChartSeries(ASender.Series);
  if ser.IsRotated then
    AGraphPoint.Y := ASender.Origin.Y
  else
    AGraphPoint.X := ASender.Origin.X;    // Keep x value fixed
end;

procedure TMainForm.DataPointDragToolDragStart(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
begin
  FDragStart := AGraphPoint;
end;

procedure TMainForm.DataPointHintToolHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
var
  ser: TChartSeries;
  idx: Integer;
  yidx: Integer;
  ttl: String;
begin
  with TDatapointHintTool(ATool) do begin
    ser := Series as TChartSeries;
    idx := PointIndex;
    yidx := YIndex;
  end;

  if ser <> nil then begin
    ttl := ser.Title;
    if yidx > 0 then
      ttl := TBarSeries(ser).Styles.Styles[yidx].Text;
    AHint := Format(
      'Mouse over "%s": point #%d, x value  %.1f (%s), y value %.1f',
      [ ttl, idx, ser.GetXValue(idx), ListChartSourceLABELS.Item[idx]^.Text,
        ser.Source[idx]^.GetY(yidx) ]
    );
  end else
    AHint := '';
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  N = 6;
var
  i: Integer;
begin
  for i:=0 to 5 do begin
    ListChartSourceRedYellow.AddXYList(i, [random*50, random*60]);
    ListChartSourceBlue.Add(i, Random*80);
    ListChartSourceLABELS.Add(i, i, char(ord('A') + i));
  end;
  CbHorizontalChange(nil);
end;

procedure TMainForm.ToolTargetChanged(Sender: TObject);
begin
  if Sender = RbToolTargetDatapoint then begin
    DataPointClickTool.Targets := DataPointClickTool.Targets - [nptCustom];
    DataPointHintTool.Targets := DatapointHintTool.Targets - [nptCustom];
  end else
  if Sender = RbToolTargetBar then begin
    DataPointClickTool.Targets := DataPointClickTool.Targets + [nptCustom];
    DataPointHintTool.Targets := DatapointHintTool.Targets + [nptCustom];
  end;
end;

end.

