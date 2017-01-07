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
    CbStacked: TCheckBox;
    LblToolTargets: TLabel;
    LblInfo: TLabel;
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
    procedure CbStackedChange(Sender: TObject);
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
  end else
  begin
    TChartSeries(Chart.Series[0]).AxisIndexX := 1;
    TChartSeries(Chart.Series[0]).AxisIndexY := 0;
    TChartSeries(Chart.Series[1]).AxisIndexX := 1;
    TChartSeries(Chart.Series[1]).AxisIndexY := 0;
  end;
end;

procedure TMainForm.CbStackedChange(Sender: TObject);
begin
  CalculatedChartSource.Percentage := CbStacked.Checked;
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
      [ idx, ser.GetXValue(idx), ser.Source[idx]^.Text, ser.Source[idx]^.GetY(yidx) ]
    ));
    Log.SelStart := Length(Log.Lines.Text);
  end;
end;

procedure TMainForm.DataPointDragToolDrag(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
begin
  AGraphPoint.X := ASender.Origin.X;    // Keep x value fixed

  // Optionally remove next line to see the difference...
  AGraphPoint.Y := ASender.Origin.Y + (AGraphPoint.Y - FDragStart.Y);
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
      [ ttl, idx, ser.GetXValue(idx), ser.Source[idx]^.Text, ser.Source[idx]^.GetY(yidx) ]
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
    ListChartSourceRedYellow.AddXYList(i, [random*50, random*60], char(ord('A') + i));
    ListChartSourceBlue.Add(i, Random*80, char(ord('A') + i));
  end;
end;

procedure TMainForm.ToolTargetChanged(Sender: TObject);
begin
  if Sender = RbToolTargetDatapoint then begin
    BarSeriesREDYELLOW.ToolTarget := bttDatapoint;
    BarSeriesBLUE.ToolTarget := bttDataPoint;
  end else
  if Sender = RbToolTargetBar then begin
    BarSeriesREDYELLOW.ToolTarget := bttBar;
    BarSeriesBLUE.ToolTarget := bttBar;
  end;
end;

end.

