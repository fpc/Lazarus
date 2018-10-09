unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAMultiSeries, TATools,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ColorBox, types;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbCandleStickSameColor: TCheckBox;
    cbSeriesType: TComboBox;
    ChartToolset1: TChartToolset;
    cbUpColor: TColorBox;
    cbDownColor: TColorBox;
    DataPointHintTool_Details: TDataPointHintTool;
    DataPointHintTool_All: TDataPointHintTool;
    FinancialChart: TChart;
    Label1: TLabel;
    LblDown: TLabel;
    LblUp: TLabel;
    ohlcSeries: TOpenHighLowCloseSeries;
    BottomPanel: TPanel;
    TopPanel: TPanel;
    procedure cbCandleStickSameColorChange(Sender: TObject);
    procedure cbSeriesTypeChange(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure DataPointHintTool_AllHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure DataPointHintToolHintLocation(ATool: TDataPointHintTool;
      AHintSize: TSize; var APoint: TPoint);
    procedure DataPointHintTool_DetailsHint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  DateUtils;

const
  DATA_FILE = 'data.txt';

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  dataList: TStringList;
  lines: TStringList;
  fs: TFormatSettings;
  i: Integer;
  xDate: TDate;
  yOpen, yClose, yHigh, yLow: Double;
begin
  fs := DefaultFormatSettings;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'd/mmm/yyyy';
  fs.DecimalSeparator := '.';
  dataList := TStringList.Create;
  try
    dataList.LoadFromFile(DATA_FILE);
    lines := TStringList.Create;
    try
      for i:=1 to dataList.Count-1 do begin   // skip header line
        lines.CommaText := dataList[i];
        xDate := ScanDateTime('yyyy-mm-dd', lines[0]);
        yOpen := StrToFloat(lines[1], fs);
        yHigh := StrToFloat(lines[2], fs);
        yLow := StrToFloat(lines[3], fs);
        yClose := StrToFloat(lines[4], fs);
        // We don't use the date for x because we want to skip the weekends in the chart
        // Therefore, we use the index and add the date as a label. Diplay of
        // the data labels is activated by BottomAxis.Marks.Style = smsLabel.
        ohlcSeries.AddXOHLC(i, yOpen, yHigh, yLow, yClose, DateToStr(xDate));
      end;
    finally
      lines.Free;
    end;
  finally
    dataList.Free;
  end;

  FinancialChart.BottomAxis.Marks.Source := ohlcSeries.ListSource;
end;

{ The combobox switches between OHLC and candle stick display modes. }
procedure TMainForm.cbSeriesTypeChange(Sender: TObject);
begin
  ohlcSeries.Mode := TOHLCMode(CbSeriesType.ItemIndex);
  cbCandleStickSameColor.Visible := cbSeriesType.ItemIndex = 1;
end;

procedure TMainForm.cbColorChange(Sender: TObject);
begin
  ohlcSeries.LinePen.Color := cbUpColor.Selected;
  ohlcSeries.DownLinePen.Color := cbDownColor.Selected;
  ohlcSeries.CandlestickUpBrush.Color := cbUpColor.Selected;
  ohlcSeries.CandleStickDownBrush.Color := cbDownColor.Selected;
end;

procedure TMainForm.cbCandleStickSameColorChange(Sender: TObject);
begin
  if cbCandleStickSameColor.Checked then
    ohlcSeries.CandlestickLinePen.Color := clDefault
  else
    ohlcSeries.CandleStickLinePen.Color := clBlack;
end;

{ This event handler returns the text to be displayed as a mouse-over hint.
  We construct the text from the date, and the open, high, low, close values. }
procedure TMainForm.DataPointHintTool_AllHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
var
  ser: TOpenHighLowCloseSeries;
begin
  ser := ATool.Series as TOpenHighLowCloseSeries;
  AHint := AnsiToUTF8(Format('Date: %s'#13'  Open: %.2m'#13'  High: %.2m'#13'  Low: %.2m'#13'  Close: %.2m', [
    FormatDateTime('dddddd', StrToDate(ser.ListSource[ATool.PointIndex]^.Text)),
    ser.ListSource[ATool.PointIndex]^.GetY(ser.YIndexOpen),
    ser.ListSource[ATool.PointIndex]^.GetY(ser.YIndexHigh),
    ser.ListSource[ATool.PointIndex]^.GetY(ser.YIndexHigh),
    ser.ListSource[ATool.PointIndex]^.GetY(ser.YIndexClose)
  ]));
end;

{ Similar to the method above. Displays only the date and the value clicked }
procedure TMainForm.DataPointHintTool_DetailsHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
var
  ser: TOpenHighLowCloseSeries;
  x, y: Integer;
  idx: Integer;
  yidx: Integer;
  ohlcName: String;
begin
  ser := ATool.Series as TOpenHighLowCloseSeries;
  idx := ATool.PointIndex;
  yidx := ATool.YIndex;
  if yidx = ohlcSeries.YIndexClose then
    ohlcName := 'Close'
  else if yidx = ohlcSeries.YIndexHigh then
    ohlcName := 'High'
  else if yidx = ohlcSeries.YIndexLow then
    ohlcName := 'Low'
  else if yidx = ohlcSeries.YIndexOpen then
    ohlcName := 'Open';
  AHint := AnsiToUTF8(Format('Date: %s'#13'  %s: %.2m', [
    FormatDateTime('dddddd', StrToDate(ser.ListSource[idx]^.Text)),
    ohlcName,
    ser.ListSource[idx]^.GetY(yidx)
  ]));
end;

{ This event handler returns the location of the hint on the screen. To enhance
  the assosciation with the data point and to avoid covering of the data point
  we move center the hint window and move it up to the top of a OHLC/candle
  data point.

  Note:
  The tool's UseApplicationHint must be set to FALSE for this even to fire. }
procedure TMainForm.DataPointHintToolHintLocation(ATool: TDataPointHintTool;
  AHintSize: TSize; var APoint: TPoint);
var
  ser: TOpenHighLowCloseSeries;
  x, y: Integer;
begin
  { Calculate screen coordinates of the "high" point }
  ser := ATool.Series as TOpenHighLowCloseSeries;
  x := FinancialChart.XGraphToImage(ser.ListSource[ATool.PointIndex]^.X);
  y := FinancialChart.YGraphToImage(ser.ListSource[ATool.PointIndex]^.YList[2]);
    // "High" value, i.e. max of data point

  // Center hint horizontally relative to data point
  APoint.x := x - AHintSize.CX div 2;

  // Move hint 4 pixels above the "High" data point
  APoint.y := y - AHintSize.CY - 4;

  // Hint coordinates are relative to screen
  APoint := FinancialChart.ClientToScreen(APoint);
end;

end.

