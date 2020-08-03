unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, ExtCtrls, Forms, Spin, StdCtrls, TACustomSource, TAFuncSeries,
  TAGraph, TAIntervalSources, TASeries, TASources, TAStyles, TATools,
  TATransformations, TAChartAxis, Classes;

type

  { TForm1 }

  TForm1 = class(TForm)
    catT: TChartAxisTransformations;
    catTAutoAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    catTFahrToCel: TLinearAxisTransform;
    cbStaticY: TCheckBox;
    cbUnitsY: TComboBox;
    ChartRotated: TChart;
    ChartPosition: TChart;
    ChartIntervals: TChart;
    ChartAxisGroup: TChart;
    ChartCustomMarks: TChart;
    ChartCustomMarksBarSeries1: TBarSeries;
    ChartDateTime: TChart;
    ChartDateTimeLineSeries1: TLineSeries;
    ChartPositionFuncSeries1: TFuncSeries;
    ChartRotatedBarSeries: TBarSeries;
    ChartStyles1: TChartStyles;
    ChartSubmarks: TChart;
    ChartSubmarksLineSeries1: TLineSeries;
    ChartToolset1ZoomIn: TZoomClickTool;
    ChartToolset1ZoomOut: TZoomClickTool;
    ChartToolsetDateTime: TChartToolset;
    cbStaticX: TCheckBox;
    cbUnitsX: TComboBox;
    CbSuppressPrevUnit: TCheckBox;
    CbAlternateFormat: TCheckBox;
    cbRotated: TCheckBox;
    csStripes: TChartStyles;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    gbPositionX: TGroupBox;
    gbPositionY: TGroupBox;
    Label1: TLabel;
    lblPositionX: TLabel;
    lblUnitsX: TLabel;
    lblPositionY: TLabel;
    lblUnitsY: TLabel;
    lcsMarks: TListChartSource;
    lcsRotatedSeries: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlPosition: TPanel;
    rbPositionBottom: TRadioButton;
    rbPositionLeft: TRadioButton;
    rbPositionTop: TRadioButton;
    rbPositionRight: TRadioButton;
    rcsDates: TRandomChartSource;
    seXPosition: TSpinEdit;
    seYPosition: TSpinEdit;
    TabSheet1: TTabSheet;
    tsPosition: TTabSheet;
    tsIntervals: TTabSheet;
    tsAxisGroup: TTabSheet;
    tsCustomMarks: TTabSheet;
    tsDateTime: TTabSheet;
    tsSubmarks: TTabSheet;
    udcsGraph: TUserDefinedChartSource;
    udcsMain: TUserDefinedChartSource;
    udcsSub: TUserDefinedChartSource;
    procedure cbRotatedChange(Sender: TObject);
    procedure cbStaticXChange(Sender: TObject);
    procedure cbStaticYChange(Sender: TObject);
    procedure CbSuppressPrevUnitChange(Sender: TObject);
    procedure cbUnitsXChange(Sender: TObject);
    procedure cbUnitsYChange(Sender: TObject);
    procedure ChartCustomMarksAxisList1MarkToText(var AText: String; AMark: Double);
    procedure ChartPositionFuncSeries1Calculate(const AX: Double; out
      AY: Double);
    procedure CbAlternateFormatChange(Sender: TObject);
    procedure DateTimeIntervalChartSource1DateTimeStepChange(Sender: TObject;
      ASteps: TDateTimeStep);
    procedure FormCreate(Sender: TObject);
    procedure rbPositionBottomChange(Sender: TObject);
    procedure rbPositionLeftChange(Sender: TObject);
    procedure seXPositionChange(Sender: TObject);
    procedure seYPositionChange(Sender: TObject);
    procedure udcsGraphGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure udcsMainGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure udcsSubGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  end;

var
  Form1: TForm1;

implementation

uses
  SysUtils, TypInfo, TAChartAxisUtils, TAChartUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbStaticXChange(Sender: TObject);
begin
  ChartPosition.AxisList[3].Visible := cbStaticX.Checked;
end;

procedure TForm1.cbRotatedChange(Sender: TObject);
begin
  if cbRotated.Checked then
  begin
    ChartRotatedBarSeries.AxisIndexX := 0;
    ChartRotatedBarSeries.AxisIndexY := 1;
    ChartRotated.BottomAxis.Marks.Source := nil;
    ChartRotated.BottomAxis.Marks.Style := smsValue;
    ChartRotated.LeftAxis.Marks.Source := lcsRotatedSeries;
    ChartRotated.LeftAxis.Marks.Style := smsLabel;
  end else
  begin
    ChartRotatedBarSeries.AxisIndexX := 1;
    ChartRotatedBarSeries.AxisIndexY := 0;
    ChartRotated.LeftAxis.Marks.Source := nil;
    ChartRotated.LeftAxis.Marks.Style := smsValue;
    ChartRotated.BottomAxis.Marks.Source := lcsRotatedSeries;
    ChartRotated.BottomAxis.Marks.Style := smsLabel;
  end;

  // This exchanges the x/y value of the source for the marks being displayed
  // along the axis. This is necessary because the axis does not "know" that
  // the series uses rotated x/y values.
  ChartRotated.LeftAxis.Marks.SourceExchangeXY := cbRotated.Checked;
end;

procedure TForm1.cbStaticYChange(Sender: TObject);
begin
  ChartPosition.AxisList[2].Visible := cbStaticY.Checked;
end;

procedure TForm1.CbSuppressPrevUnitChange(Sender: TObject);
begin
  DateTimeIntervalChartSource1.SuppressPrevUnit := CbSuppressPrevUnit.Checked;
end;

procedure TForm1.cbUnitsXChange(Sender: TObject);
begin
  ChartPosition.AxisList[1].PositionUnits := TChartUnits(cbUnitsX.ItemIndex);
end;

procedure TForm1.cbUnitsYChange(Sender: TObject);
begin
  ChartPosition.AxisList[0].PositionUnits := TChartUnits(cbUnitsY.ItemIndex);
end;

procedure TForm1.ChartCustomMarksAxisList1MarkToText(var AText: String; AMark: Double);
begin
  if AMark = 3 then
    AText := '*' + AText + '*';
end;

procedure TForm1.ChartPositionFuncSeries1Calculate(
  const AX: Double; out AY: Double);
begin
  AY := Sin(AX / 30) * 10 + Cos(AX / 10) * 20;
end;

procedure TForm1.DateTimeIntervalChartSource1DateTimeStepChange(
  Sender: TObject; ASteps: TDateTimeStep);
begin
  Label1.Caption := GetEnumName(TypeInfo(TDateTimeStep), ord(ASteps));
end;

procedure TForm1.CbAlternateFormatChange(Sender: TObject);
begin
  with DateTimeIntervalChartSource1.DateTimeStepFormat do
    if CbAlternateFormat.Checked then begin
      WeekFormat := 'dd"."mmm"."';
      DayFormat := 'dd"."mm"."';
      HourFormat := 'dd". "hh:nn';
      SecondFormat := 'nn"."ss';
      MillisecondFormat := 'ss"."zzz';
    end else begin
      WeekFormat := DEFAULT_WEEK_FORMAT;
      DayFormat := DEFAULT_DAY_FORMAT;
      HourFormat := DEFAULT_HOUR_FORMAT;
      SecondFormat := DEFAULT_SECOND_FORMAT;
      MillisecondFormat := DEFAULT_MILLISECOND_FORMAT;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  COLORS: array [1..5] of Integer =
    ($0000A0, $002080, $004060, $006040, $008020);
var
  i, j: Integer;
  ls: TLineSeries;
  tr: TChartAxisTransformations;
begin
  for i := 1 to 5 do begin
    ls := TLineSeries.Create(Self);
    ChartAxisGroup.AddSeries(ls);
    ls.SeriesColor := COLORS[i];
    ls.LinePen.Width := 2;
    for j := 1 to 20 do
      ls.AddXY(j, Random * 8);
    tr := TChartAxisTransformations.Create(Self);
    with TAutoScaleAxisTransform.Create(Self) do begin
      Transformations := tr;
      MinValue := i + 0.2;
      MaxValue := i + 0.8;
    end;
    with TChartAxis.Create(ChartAxisGroup.AxisList) do begin
      Transformations := tr;
      Marks.AtDataOnly := true;
      Marks.Range.UseMin := true;
      Marks.Range.UseMax := true;
      Marks.Range.Min := 1;
      Marks.Range.Max := 9;
      Marks.LabelFont.Orientation := 900;
      Marks.LabelFont.Color := COLORS[i];
      with Marks.DefaultSource.Params do begin
        MinLength := 5;
        MaxLength := 20;
        Options := Options + [aipUseCount];
      end;
      TickColor := COLORS[i];
      Group := 1;
    end;
    ls.AxisIndexY := ChartAxisGroup.AxisList.Count - 1;
  end;
  with rcsDates do begin
    XMin := Now - 5 * 365;
    XMax := Now + 5 * 365;
    PointsNumber := 10 * 365 * 24;
  end;
end;

procedure TForm1.rbPositionBottomChange(Sender: TObject);
begin
  with ChartPosition.AxisList[1] do
    if rbPositionBottom.Checked then
      Alignment := calBottom
    else
      Alignment := calTop;
end;

procedure TForm1.rbPositionLeftChange(Sender: TObject);
begin
  with ChartPosition.AxisList[0] do
    if rbPositionLeft.Checked then
      Alignment := calLeft
    else
      Alignment := calRight;
end;

procedure TForm1.seXPositionChange(Sender: TObject);
begin
  ChartPosition.AxisList[1].Position := seXPosition.Value;
end;

procedure TForm1.seYPositionChange(Sender: TObject);
begin
  ChartPosition.AxisList[0].Position := seYPosition.Value;
end;

procedure TForm1.udcsGraphGetChartDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  AItem.X := (AIndex - 50) * 0.1;
  AItem.Y := Sin(AItem.X * 3) + AItem.X / 10;
end;

procedure TForm1.udcsMainGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  AItem.X := AIndex - 5;
  AItem.Y := AItem.X;
end;

procedure TForm1.udcsSubGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  Unused(ASource);
  AItem.X := (AIndex - 25) * 0.2;
  AItem.Y := AItem.X;
end;

end.

