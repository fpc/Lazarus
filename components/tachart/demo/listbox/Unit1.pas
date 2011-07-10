unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TASources, TASeries, TAGraph, CheckLst, Spin, ComCtrls,
  ExtCtrls, StdCtrls, FileUtil, Forms, Controls, Graphics, Dialogs,
  TAChartListbox;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1 : TBevel;
    BtnAddSeries:TButton;
    BtnDeleteSeries:TButton;
    BtnToggleCOS:TButton;
    BtnToggleChart:TButton;
    BtnToggleSIN:TButton;
    Chart:TChart;
    CbShowCheckboxes:TCheckBox;
    CbShowSeriesIcon:TCheckBox;
    CbCheckStyle:TCheckBox;
    CbMultiSelect : TCheckBox;
    CbKeepSeriesOut:TCheckBox;
    CheckListBox1:TCheckListBox;
    ColorDialog:TColorDialog;
    Label1 : TLabel;
    Label2:TLabel;
    ListBox1:TListBox;
    Memo : TMemo;
    SinSeries:TLineSeries;
    CosSeries:TLineSeries;
    ListboxPanel:TPanel;
    Panel1:TPanel;
    RandomChartSource:TRandomChartSource;
    EdColumns:TSpinEdit;
    Splitter:TSplitter;
    procedure BtnAddSeriesClick(Sender:TObject);
    procedure BtnDeleteSeriesClick(Sender:TObject);
    procedure BtnToggleCOSClick(Sender:TObject);
    procedure BtnToggleChartClick(Sender:TObject);
    procedure BtnToggleSINClick(Sender:TObject);
    procedure CbMultiSelectChange(Sender : TObject);
    procedure CbShowCheckboxesChange(Sender:TObject);
    procedure CbShowSeriesIconChange(Sender:TObject);
    procedure CbCheckStyleChange(Sender:TObject);
    procedure CbKeepSeriesOutChange(Sender:TObject);
    procedure EdColumnsChange(Sender:TObject);
    procedure FormCreate(Sender:TObject);
    procedure ChartListboxCheckboxClick(Sender:TObject; Index:integer);
    procedure ChartListboxClick(Sender:TObject);
    procedure ChartListboxItemClick(Sender:TObject; Index:integer);
    procedure ChartListboxSeriesIconClick(Sender:TObject; Index:integer);
    procedure ChartListboxPopulate(Sender:TObject);
  private
    { private declarations }
    ChartListbox : TChartListbox;
    procedure CreateData;
  public
    { public declarations }
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
  i : integer;
  mn, mx : double;
  x : double;
begin
  mx := 10.0;
  mn := 0.0;
  for i:=0 to n-1 do begin
    x := mn + (mx - mn) / (n - 1) * i;
    SinSeries.AddXY(x, sin(x));
    CosSeries.AddXY(x, cos(x));
  end;
  SinSeries.Title := 'sin(x)';
  SinSeries.SeriesColor := clRed;
  CosSeries.Title := 'cos(x)';
  CosSeries.SeriesColor := clBlue;
end;

procedure TForm1.ChartListboxCheckboxClick(Sender:TObject; Index:integer);
begin
  Memo.Lines.Add(Format('Checkbox of item #%d (series "%s") clicked.',
    [Index, ChartListbox.Series[Index].Title]));
end;

procedure TForm1.ChartListboxSeriesIconClick(Sender:TObject; Index:integer);
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

procedure TForm1.ChartListboxItemClick(Sender:TObject; Index:integer);
begin
  Memo.Lines.Add(Format('Title of item #%d (series "%s") clicked.',
    [Index, ChartListbox.Series[Index].Title]));
end;

procedure TForm1.ChartListboxClick(Sender:TObject);
begin
  Memo.Lines.Add(Format('Item #%d (series "%s") clicked.',
    [ChartListbox.ItemIndex, ChartListbox.Series[ChartListbox.ItemIndex].Title]));
end;

procedure TForm1.ChartListboxPopulate(Sender:TObject);
begin
  ChartListbox.RemoveSeries(SinSeries);
  ChartListbox.RemoveSeries(CosSeries);
end;

procedure TForm1.FormCreate(Sender:TObject);
begin
  ChartListbox := TChartListbox.Create(self);
  ChartListbox.Parent := ListboxPanel;
  ChartListbox.Align := alClient;
  ChartListbox.Chart := Chart;
  ChartListbox.OnSeriesIconClick := @ChartListboxSeriesIconClick;
  ChartListbox.OnCheckboxClick := @ChartListboxCheckboxClick;
  ChartListbox.OnItemClick := @ChartListboxItemClick;
  ChartListbox.OnClick := @ChartListboxClick;

  Memo.Lines.Clear;

  CreateData;
end;

procedure TForm1.BtnToggleSINClick(Sender:TObject);
begin
  SinSeries.Active := not SinSeries.Active;
end;

procedure TForm1.CbMultiSelectChange(Sender : TObject);
begin
  ChartListbox.MultiSelect := CbMultiSelect.Checked;
end;

procedure TForm1.CbShowCheckboxesChange(Sender:TObject);
begin
  ChartListbox.ShowCheckboxes := CbShowCheckboxes.Checked;
end;

procedure TForm1.CbShowSeriesIconChange(Sender:TObject);
begin
  ChartListbox.ShowSeriesIcons := CbShowSeriesIcon.Checked;
end;

procedure TForm1.CbCheckStyleChange(Sender:TObject);
begin
  if CbCheckStyle.Checked then
    Chartlistbox.CheckStyle := cbsRadioButton
  else
    ChartListbox.CheckStyle := cbsCheckbox;
end;

procedure TForm1.CbKeepSeriesOutChange(Sender:TObject);
begin
  if CbKeepSeriesOut.Checked then begin
    ChartListbox.OnPopulate := @ChartListboxPopulate;
    ChartListbox.RemoveSeries(SinSeries);
    ChartListbox.RemoveSeries(CosSeries);
  end else
    ChartListbox.OnPopulate := nil;
end;

procedure TForm1.EdColumnsChange(Sender:TObject);
begin
  ChartListbox.Columns := EdColumns.Value;
  CheckListbox1.Columns := EdColumns.Value;
  Listbox1.Columns := EdColumns.Value;
end;

procedure TForm1.BtnAddSeriesClick(Sender:TObject);
var
  ser : TLineSeries;
  cs : TRandomChartSource;
begin
  cs := TRandomChartSource.Create(Chart);
  cs.RandSeed := random(65000);
  cs.PointsNumber := random(10) + 3;
  cs.XMax := 10;
  cs.XMin := 0;
  cs.YMax := 1;
  cs.YMin := -1;
  cs.YCount := 1;
  ser := TLineSeries.Create(Chart);
  ser.Source := cs;
  ser.SeriesColor := rgbToColor(random(255), random(256), random(256));
  ser.Title := Format('Series %d', [Chart.SeriesCount+1]);
  ser.ShowPoints := odd(Chart.SeriesCount);
  ser.Pointer.Brush.Color := ser.SeriesColor;
  ser.Pointer.Style := TSeriesPointerStyle(random(ord(High(TSeriesPointerStyle))));
  Chart.AddSeries(ser);
end;

procedure TForm1.BtnDeleteSeriesClick(Sender:TObject);
var
  ser : TBasicChartSeries;
begin
  if (ChartListbox.ItemIndex = -1) then begin
    MessageDlg('Select the series to be deleted from the listbox first.',
      mtInformation, [mbOK], 0);
    exit;
  end;
  if (ChartListbox.ItemIndex < 2) and (not CbKeepSeriesOut.Checked) then
    MessageDlg('This demo is designed to have at least the sine and cosine '+
      'series in the chart. Deleting is not allowed.', mtInformation, [mbOK], 0)
  else begin
    ser := ChartListbox.Series[ChartListbox.ItemIndex];
    Chart.DeleteSeries(ser);
    FreeAndNil(ser);
  end;
end;

procedure TForm1.BtnToggleCOSClick(Sender:TObject);
begin
  CosSeries.Active := not CosSeries.Active;
end;

procedure TForm1.BtnToggleChartClick(Sender:TObject);
begin
  if ChartListbox.Chart = nil then
    ChartListbox.Chart := Chart
  else
    ChartListbox.Chart := nil;
end;


end.

