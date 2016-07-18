unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, Spin, TAGraph, TAMultiSeries, TASeries, TASources,
  TAStyles;

type

  { TForm1 }

  TForm1 = class(TForm)
    ccsStacked: TCalculatedChartSource;
    cbPercentage: TCheckBox;
    cgShowStackLevels: TCheckGroup;
    chField: TChart;
    chFieldFieldSeries1: TFieldSeries;
    chOHLC: TChart;
    ChartStyles1: TChartStyles;
    chOHLCOpenHighLowCloseSeries1: TOpenHighLowCloseSeries;
    chStackedAreaSeries1: TAreaSeries;
    chStackedLineSeries1: TLineSeries;
    chWhiskers: TChart;
    chStacked: TChart;
    chBubble: TChart;
    Chart1BubbleSeries1: TBubbleSeries;
    chStackedBarSeries1: TBarSeries;
    chWhiskersBoxAndWhiskerSeries1: TBoxAndWhiskerSeries;
    edMaxVectorLength: TFloatSpinEdit;
    Label1: TLabel;
    lcsBubble: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pnStackedControls: TPanel;
    rbRadial: TRadioButton;
    rbTangential: TRadioButton;
    rgStackedSeries: TRadioGroup;
    rcsStacked: TRandomChartSource;
    tsField: TTabSheet;
    tsOHLC: TTabSheet;
    tsWhiskers: TTabSheet;
    tsStacked: TTabSheet;
    tsBubble: TTabSheet;
    procedure cbPercentageChange(Sender: TObject);
    procedure cgShowStackLevelsItemClick(Sender: TObject; Index: integer);
    procedure edMaxVectorLengthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FieldTypeChange(Sender: TObject);
    procedure rgStackedSeriesClick(Sender: TObject);
  private
    procedure CreateFieldSeriesData;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TAChartUtils, TAGeometry;

{ TForm1 }

procedure TForm1.cbPercentageChange(Sender: TObject);
begin
  ccsStacked.Percentage := cbPercentage.Checked;
end;

procedure TForm1.cgShowStackLevelsItemClick(Sender: TObject; Index: integer);
var
  s: String;
  i: Integer;
begin
  Unused(Index);
  s := '';
  for i := 0 to cgShowStackLevels.Items.Count - 1 do begin
    if cgShowStackLevels.Checked[i] then
      s += Format('%d,', [i]);
    ChartStyles1.Styles[i].RepeatCount := Ord(cgShowStackLevels.Checked[i]);
  end;
  ccsStacked.ReorderYList := s[1..Length(s) - 1];
end;

procedure TForm1.CreateFieldSeriesData;
const
  NX = 21;
  NY = 21;
  MIN = -5.0;
  MAX = +5.0;
var
  i, j: Integer;
  x, y, r: Double;
  v: TDoublePoint;
begin
  v := DoublePoint(2.0, 2.0);
  r := sqrt(sqr(v.x) + sqr(v.y));
  chFieldFieldSeries1.Clear;
  for j := 0 to NY - 1 do begin
    y := MIN + (MAX - MIN) / (NY - 1) * j;
    for i := 0 to NX - 1 do begin
      x := MIN + (MAX - MIN) / (NX - 1) * i;
      r := sqr(x) + sqr(y);
      if r > 0.1 then begin
        if rbRadial.Checked then
          v := DoublePoint(x/r, y/r)    // radial vector
        else
        if rbTangential.Checked then
          v := DoublePoint(y/r, -x/r);  // tangential vector
        chFieldFieldSeries1.AddVector(x, y, v.x, v.y);
      end;
    end;
  end;
  // Since the data points, in this example, have a distance of 0.5 units we
  // can avoid overlapping of vectors if they are scaled to a length of 0.5
  // units as well.
  chFieldFieldSeries1.NormalizeVectors(0.5);
end;

procedure TForm1.edMaxVectorLengthChange(Sender: TObject);
begin
  chFieldFieldSeries1.NormalizeVectors(EdMaxVectorLength.Value);
  chField.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ylist: array [1..4] of Double;
  i, j: Integer;
  y, y0: Double;
begin
  chWhiskersBoxAndWhiskerSeries1.ListSource.YCount := 5;
  for i := 1 to 6 do begin
    y := Random(80) + 10;
    y0 := y;
    for j := 1 to 4 do begin
      y += Random(20) + 5;
      ylist[j] := y;
    end;
    chWhiskersBoxAndWhiskerSeries1.AddXY(i, y0, ylist);
  end;

  chOHLCOpenHighLowCloseSeries1.ListSource.YCount := 4;
  y := 50;
  for i := 1 to 50 do begin
    y += Random(80) / 10 - 4;
    ylist[1] := y;
    for j := 1 to 3 do begin
      ylist[j] += Random(20) / 10 + 1;
      ylist[j + 1] := ylist[j];
    end;
    if Random(3) = 1 then
      Exchange(ylist[1], ylist[2]);
    chOHLCOpenHighLowCloseSeries1.AddXY(i, y, ylist);
  end;

  CreateFieldSeriesData;
end;

procedure TForm1.FieldTypeChange(Sender: TObject);
begin
  CreateFieldSeriesData;
end;

procedure TForm1.rgStackedSeriesClick(Sender: TObject);
var
  i: Integer;
begin
  i := rgStackedSeries.ItemIndex;
  chStackedAreaSeries1.Active := i = 0;
  chStackedBarSeries1.Active := i = 1;
  chStackedLineSeries1.Active := i = 2;
end;

end.

