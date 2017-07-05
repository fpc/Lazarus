unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, TAChartAxisUtils, TAFuncSeries;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnCopyToClipboard: TButton;
    BtnSaveWMF: TButton;
    BtnSaveSVG: TButton;
    Chart: TChart;
    CgHTML: TCheckGroup;
    FitSeries: TFitSeries;
    ListChartSource: TListChartSource;
    DataSeries: TLineSeries;
    BottomPanel: TPanel;
    ListChartSource_Fit: TListChartSource;
    procedure BtnCopyToClipboardClick(Sender: TObject);
    procedure BtnSaveWMFClick(Sender: TObject);
    procedure BtnSaveSVGClick(Sender: TObject);
    procedure CgHTMLItemClick(Sender: TObject; Index: integer);
    procedure ChartAxisList1MarkToText(var AText: String; AMark: Double);
    procedure FitSeriesFitComplete(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    procedure CreateData;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils, {$IFDEF WINDOWS}TADrawerWMF,{$ENDIF} TADrawerSVG;

{ TMainForm }

procedure TMainForm.BtnCopyToClipboardClick(Sender: TObject);
begin
  Chart.CopyToClipboardBitmap;
end;

procedure TMainForm.BtnSaveWMFClick(Sender: TObject);
begin
  {$IFDEF WINDOWS}
  with Chart do
    Draw(TWindowsMetafileDrawer.Create('test.wmf'), Rect(0, 0, Width, Height));
  ShowMessage('Chart saved to file "test.wmf"');
  {$ENDIF}
end;

procedure TMainForm.BtnSaveSVGClick(Sender: TObject);
begin
  Chart.SaveToSVGFile('test.svg');
  ShowMessage('Chart saved to file "test.svg"');
end;

procedure TMainForm.CgHTMLItemClick(Sender: TObject; Index: integer);
var
  tf: TChartTextFormat;
begin
  if CgHTML.Checked[Index] then tf := tfNormal else tf := tfHTML;
  case Index of
    0: Chart.Title.TextFormat := tf;
    1: Chart.Foot.TextFormat := tf;
    2: Chart.Legend.TextFormat := tf;
    3: DataSeries.Marks.TextFormat := tf;
    4: Chart.BottomAxis.Marks.TextFormat := tf;
    5: Chart.BottomAxis.Title.TextFormat := tf;
    6: Chart.LeftAxis.Title.TextFormat := tf;
  end;
end;

procedure TMainForm.ChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  AText := AText + '&deg;';
end;

procedure TMainForm.CreateData;
const
  N = 20;
  MIN = 0;
  MAX = 90;
  OUTLIER_INDEX = 12;
var
  i: Integer;
  x, y: Double;
  s: String;
begin
  for i:=0 to N-1 do begin
    x := MIN + (MAX - MIN) * i / (N-1) + 5*(random - 0.5);
    if i = OUTLIER_INDEX then begin
      y := 631;
      s := 'Defective device!' + LineEnding + '(&alpha; = ' + FormatFloat('0.00', x) + '&deg;)';
    end else
    begin
      y := x*x / 10 + (random - 0.5) * 100;
      s := '';
    end;
    ListChartSource.Add(x, y, s);
    if i <> OUTLIER_INDEX then
      ListChartSource_Fit.Add(x, y);
  end;
  DataSeries.Source := ListChartSource;
  FitSeries.Source := ListChartSource_Fit;
end;

procedure TMainForm.FitSeriesFitComplete(Sender: TObject);
var
  p: Array of Double;
  i: Integer;
  s: String;
begin
  SetLength(p, FitSeries.ParamCount);
  for i:=0 to FitSeries.ParamCount-1 do
    p[i] := FitSeries.Param[i];

  s := FitSeries.EquationText.
    x('&alpha;').
    y('A').
    NumFormat('%.2f').
    DecimalSeparator('.').
    Params(p).
    Get;
  FitSeries.Title := '<font color="blue">Fitted:</font> ' + s;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateData;

  CgHTML.Checked[0] := Chart.Title.TextFormat = tfNormal;
  CgHTML.Checked[1] := Chart.Foot.TextFormat = tfNormal;
  CgHTML.Checked[2] := Chart.Legend.TextFormat = tfNormal;
  CgHTML.Checked[3] := DataSeries.Marks.TextFormat = tfNormal;
  CgHTML.Checked[4] := Chart.BottomAxis.Marks.TextFormat = tfNormal;
  CgHTML.Checked[5] := Chart.BottomAxis.Title.TextFormat = tfNormal;
  CgHTML.Checked[6] := Chart.LeftAxis.Title.TextFormat = tfNormal;

  {$IFDEF WINDOWS}
  Chart.Foot.Text[1] := '<font name="Times New Roman" color="gray">' + Chart.Foot.Text[1] + '</font>';
  {$ELSE}
  BtnSaveWMF.Hide;
  {$ENDIF}
end;

end.

