unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TAMultiSeries, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbSeriesType: TComboBox;
    FinancialChart: TChart;
    ohlcSeries: TOpenHighLowCloseSeries;
    TopPanel: TPanel;
    procedure cbSeriesTypeChange(Sender: TObject);
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
  fs.ShortMonthNames[1] := 'Jan';
  fs.ShortMonthNames[2] := 'Feb';
  fs.ShortMonthNames[3] := 'Mar';
  fs.ShortMonthNames[4] := 'Apr';
  fs.ShortMonthNames[5] := 'May';
  fs.ShortMonthNames[6] := 'Jun';
  fs.ShortMonthNames[7] := 'Jul';
  fs.ShortMonthNames[8] := 'Aug';
  fs.ShortMonthNames[9] := 'Sep';
  fs.ShortMonthNames[10] := 'Oct';
  fs.ShortMonthNames[11] := 'Nov';
  fs.ShortMonthNames[12] := 'Dec';

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
        ohlcSeries.AddXOHLC(i, yOpen, yHigh, yLow, yClose, DateToStr(xDate, fs));
      end;
    finally
      lines.Free;
    end;
  finally
    dataList.Free;
  end;

  FinancialChart.BottomAxis.Marks.Source := ohlcSeries.ListSource;
end;

procedure TMainForm.cbSeriesTypeChange(Sender: TObject);
begin
  ohlcSeries.Mode := TOHLCMode(CbSeriesType.ItemIndex);
end;

end.

