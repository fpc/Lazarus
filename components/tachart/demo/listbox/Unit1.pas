unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TASeries, TAGraph, CheckLst, Spin,
  ExtCtrls, StdCtrls, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAChartListbox, TACustomSeries, TALegend;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnAddSeries: TButton;
    BtnDeleteSeries: TButton;
    BtnToggleCOS: TButton;
    BtnToggleChart: TButton;
    BtnToggleSIN: TButton;
    BtnAddPoint: TButton;
    Chart: TChart;
    CbShowCheckboxes: TCheckBox;
    CbShowSeriesIcon: TCheckBox;
    CbCheckStyle: TCheckBox;
    CbKeepSeriesOut: TCheckBox;
    ChartListbox: TChartListbox;
    ColorDialog: TColorDialog;
    Label1: TLabel;
    Label2: TLabel;
    Memo: TMemo;
    SinSeries: TLineSeries;
    CosSeries: TLineSeries;
    ListboxPanel: TPanel;
    Panel1: TPanel;
    RandomChartSource: TRandomChartSource;
    EdColumns: TSpinEdit;
    Splitter: TSplitter;
    procedure BtnAddSeriesClick(Sender: TObject);
    procedure BtnDeleteSeriesClick(Sender: TObject);
    procedure BtnToggleCOSClick(Sender: TObject);
    procedure BtnToggleChartClick(Sender: TObject);
    procedure BtnToggleSINClick(Sender: TObject);
    procedure BtnAddPointClick(Sender: TObject);
    procedure CbShowCheckboxesChange(Sender: TObject);
    procedure CbShowSeriesIconChange(Sender: TObject);
    procedure CbCheckStyleChange(Sender: TObject);
    procedure CbKeepSeriesOutChange(Sender: TObject);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems;
      var ASkip: Boolean);
    procedure ChartListboxPopulate(Sender: TObject);
    procedure EdColumnsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChartListboxCheckboxClick(Sender: TObject; Index: Integer);
    procedure ChartListboxClick(Sender: TObject);
    procedure ChartListboxItemClick(Sender: TObject; Index: Integer);
    procedure ChartListboxSeriesIconDblClick(Sender: TObject; Index: Integer);
  private
    procedure CreateData;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TATypes;

{ TForm1 }

procedure TForm1.CreateData;
const
  n = 100;
var
  i: Integer;
  mn, mx, x: Double;
begin
  mx := 10.0;
  mn := 0.0;
  for i := 0 to n - 1 do begin
    x := mn + (mx - mn) / (n - 1) * i;
    SinSeries.AddXY(x, sin(x));
    CosSeries.AddXY(x, cos(x));
  end;
end;

procedure TForm1.ChartListboxCheckboxClick(Sender: TObject; Index: Integer);
begin
  Memo.Lines.Add(Format('Checkbox of item #%d (series "%s") clicked.',
    [Index, ChartListbox.Series[Index].Title]));
end;

procedure TForm1.ChartListboxSeriesIconDblClick(Sender: TObject; Index: Integer);
begin
  Memo.Lines.Add(Format('Icon of item #%d (series "%s") clicked.',
    [Index, ChartListbox.Series[Index].Title]));

  if ChartListbox.Series[Index] is TLineSeries then
    with ColorDialog do begin
      Color := TLineSeries(ChartListbox.Series[Index]).SeriesColor;
      if Execute then
        TLineSeries(ChartListbox.Series[Index]).SeriesColor := Color;
    end;
end;

procedure TForm1.ChartListboxItemClick(Sender: TObject; Index: Integer);
begin
  Memo.Lines.Add(Format('Title of item #%d (series "%s") clicked.',
    [Index, ChartListbox.Series[Index].Title]));
end;

procedure TForm1.ChartListboxPopulate(Sender: TObject);
begin
  Memo.Lines.Add('Populate');
end;

procedure TForm1.ChartListboxClick(Sender: TObject);
begin
  with ChartListbox do
    if ItemIndex <> -1 then
      Memo.Lines.Add(Format('Item #%d (series "%s") clicked.',
        [ItemIndex, Series[ItemIndex].Title]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CreateData;
end;

procedure TForm1.BtnToggleSINClick(Sender: TObject);
begin
  SinSeries.Active := not SinSeries.Active;
end;

procedure TForm1.BtnAddPointClick(Sender: TObject);
begin
  SinSeries.Add(Random(5), '', clRed);
end;

procedure TForm1.CbShowCheckboxesChange(Sender: TObject);
begin
  with ChartListbox do
    if CbShowCheckboxes.Checked then
      Options := Options + [cloShowCheckboxes]
    else
      Options := Options - [cloShowCheckboxes];
end;

procedure TForm1.CbShowSeriesIconChange(Sender: TObject);
begin
  with ChartListbox do
    if CbShowSeriesIcon.Checked then
      Options := Options + [cloShowIcons]
    else
      Options := Options - [cloShowIcons];
end;

procedure TForm1.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
begin
  ASkip := CbKeepSeriesOut.Checked and
    ((ASeries = SinSeries) or (ASeries = CosSeries));
end;

procedure TForm1.CbCheckStyleChange(Sender:TObject);
begin
  if CbCheckStyle.Checked then
    Chartlistbox.CheckStyle := cbsRadioButton
  else
    ChartListbox.CheckStyle := cbsCheckbox;
end;

procedure TForm1.CbKeepSeriesOutChange(Sender: TObject);
begin
  ChartListbox.SeriesChanged(Self);
end;

procedure TForm1.EdColumnsChange(Sender: TObject);
begin
  ChartListbox.Columns := EdColumns.Value;
end;

procedure TForm1.BtnAddSeriesClick(Sender: TObject);
var
  ser : TLineSeries;
  cs : TRandomChartSource;
begin
  cs := TRandomChartSource.Create(Chart);
  cs.RandSeed := Random(65000);
  cs.PointsNumber := Random(10) + 3;
  cs.XMax := 10;
  cs.XMin := 0;
  cs.YMax := 1;
  cs.YMin := -1;
  cs.YCount := 1;
  ser := TLineSeries.Create(Chart);
  ser.Source := cs;
  ser.SeriesColor := rgbToColor(Random(255), Random(256), Random(256));
  ser.Title := Format('Series %d', [Chart.SeriesCount + 1]);
  ser.ShowPoints := Odd(Chart.SeriesCount);
  ser.Pointer.Brush.Color := ser.SeriesColor;
  ser.Pointer.Style :=
    TSeriesPointerStyle(Random(Ord(High(TSeriesPointerStyle))));
  Chart.AddSeries(ser);
end;

procedure TForm1.BtnDeleteSeriesClick(Sender: TObject);
begin
  if ChartListbox.ItemIndex = -1 then
    ShowMessage('Select the series to be deleted from the listbox first.')
  else if (ChartListbox.ItemIndex < 2) and not CbKeepSeriesOut.Checked then
    ShowMessage(
      'This demo is designed to have at least the sine and cosine ' +
      'series in the chart. Deleting is not allowed.')
  else
    ChartListbox.Series[ChartListbox.ItemIndex].Free;
end;

procedure TForm1.BtnToggleCOSClick(Sender: TObject);
begin
  CosSeries.Active := not CosSeries.Active;
end;

procedure TForm1.BtnToggleChartClick(Sender: TObject);
begin
  if ChartListbox.Chart = nil then
    ChartListbox.Chart := Chart
  else
    ChartListbox.Chart := nil;
end;


end.

