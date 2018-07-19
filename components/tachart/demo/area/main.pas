unit main;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ComCtrls, ExtCtrls,
  TAGraph, TASeries, TASources, TAStyles, TACustomSeries, TACustomSource;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnNewData: TButton;
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart2: TChart;
    Chart2AreaSeries1: TAreaSeries;
    Chart2AreaSeries2: TAreaSeries;
    Chart2LineSeries1: TLineSeries;
    Chart2LineSeries2: TLineSeries;
    ChartStyles1: TChartStyles;
    CbStacked: TCheckBox;
    CbRotated: TCheckBox;
    Cb3D: TCheckBox;
    CbUseZeroLevel: TCheckBox;
    CbBanded: TCheckBox;
    CbShowDropLines: TCheckBox;
    CbShowDataLabels: TCheckBox;
    CbLabelsAt: TComboBox;
    CbShowLegend: TCheckBox;
    EdYCount: TSpinEdit;
    EdZeroLevel: TFloatSpinEdit;
    Label1: TLabel;
    LblCount: TLabel;
    LblYCount: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    EdCount: TSpinEdit;
    PgGeneral: TTabSheet;
    PgErrorRange: TTabSheet;
    ChartSourceArea1: TUserDefinedChartSource;
    ChartSourceLine1: TUserDefinedChartSource;
    ChartSourceArea2: TUserDefinedChartSource;
    ChartSourceLine2: TUserDefinedChartSource;
    procedure BtnNewDataClick(Sender: TObject);
    procedure CbBandedChange(Sender: TObject);
    procedure CbLabelsAtChange(Sender: TObject);
    procedure CbShowDataLabelsChange(Sender: TObject);
    procedure CbShowLegendChange(Sender: TObject);
    procedure CbStackedChange(Sender: TObject);
    procedure CbRotatedChange(Sender: TObject);
    procedure Cb3DChange(Sender: TObject);
    procedure CbUseZeroLevelChange(Sender: TObject);
    procedure ChartSourceArea1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure ChartSourceArea2GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure ChartSourceLine1GetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure ChartSourceLine2GetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
    procedure CbShowDropLinesChange(Sender: TObject);
    procedure EdYCountChange(Sender: TObject);
    procedure EdZeroLevelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdCountChange(Sender: TObject);
  private
    procedure PopulateSeries(N, NY: Integer);
    procedure UpdateStyles(Count: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, TAChartUtils;

type
  TData = record
    x, y, dy: Double;
  end;
  TDataArray = array of TData;

var
  Data1, Data2: TDataArray;

{ TForm1 }

procedure TForm1.BtnNewDataClick(Sender: TObject);
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TForm1.CbBandedChange(Sender: TObject);
begin
  {$IF LCL_FullVersion >= 1090000}
  Chart1AreaSeries1.Banded := CbBanded.Checked;
  if Chart1AreaSeries1.Banded then
    UpdateStyles(Chart1AreaSeries1.ListSource.YCount-1)
  else
    UpdateStyles(Chart1AreaSeries1.ListSource.YCount);
  {$ELSE}
  ShowMessage('This functionality requires at least Lazarus version 1.9');
  {$IFEND}
end;

procedure TForm1.CbLabelsAtChange(Sender: TObject);
begin
  if CbLabelsAt.ItemIndex = 0 then
    Chart1AreaSeries1.Marks.YIndex := - 1
  else
    Chart1AreaSeries1.Marks.YIndex := CbLabelsAt.ItemIndex - 1;
end;

procedure TForm1.CbShowDataLabelsChange(Sender: TObject);
begin
  if CbShowDataLabels.Checked then begin
    Chart1AreaSeries1.Marks.Style := smsLabel;
    Chart1AreaSeries1.Marks.Format := '%.2f';
  end else
    Chart1AreaSeries1.Marks.Style := smsNone;
end;

procedure TForm1.CbShowLegendChange(Sender: TObject);
begin
  Chart1.Legend.Visible := CbShowLegend.Checked;
end;

procedure TForm1.CbStackedChange(Sender: TObject);
begin
  Chart1AreaSeries1.Stacked := CbStacked.Checked;
end;

procedure TForm1.CbRotatedChange(Sender: TObject);
var
  tmp: Integer;
begin
  tmp := Chart1AreaSeries1.AxisIndexX;
  Chart1AreaSeries1.AxisIndexX := Chart1AreaSeries1.AxisIndexY;
  Chart1AreaSeries1.AxisIndexY := tmp;
end;

procedure TForm1.Cb3DChange(Sender: TObject);
const
  DEPTH = 20;
begin
  Chart1AreaSeries1.Depth := IfThen(Cb3D.Checked, DEPTH, 0);
  Chart1.Margins.Right := 4 + Chart1AreaSeries1.Depth;
  Chart1.Margins.Top := 4 + Chart1AreaSeries1.Depth;
end;

procedure TForm1.CbUseZeroLevelChange(Sender: TObject);
begin
  Chart1AreaSeries1.UseZeroLevel := CbUseZeroLevel.Checked;
end;

procedure TForm1.ChartSourceArea1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data1[AIndex].x;
  AItem.Y := Data1[AIndex].y - Data1[AIndex].dy;
  AItem.YList[0] := Data1[AIndex].y + Data1[AIndex].dy;
end;

procedure TForm1.ChartSourceArea2GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data2[AIndex].x;
  AItem.Y := Data2[AIndex].y;
  AItem.YList[0] := -Data2[AIndex].dy;
  AItem.YList[1] := 2*Data2[AIndex].dy;
end;

procedure TForm1.ChartSourceLine1GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data1[AIndex].X;
  AItem.Y := Data1[AIndex].Y;
end;

procedure TForm1.ChartSourceLine2GetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := Data2[AIndex].X;
  AItem.Y := Data2[AIndex].Y;
end;

procedure TForm1.CbShowDropLinesChange(Sender: TObject);
begin
  if CbShowDropLines.Checked then
    Chart1AreaSeries1.AreaLinesPen.Style := psSolid
  else
    Chart1AreaSeries1.AreaLinesPen.Style := psClear;
end;

procedure TForm1.EdYCountChange(Sender: TObject);
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TForm1.EdZeroLevelChange(Sender: TObject);
begin
  Chart1AreaSeries1.ZeroLevel := EdZeroLevel.Value;
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  N1 = 20;
  N2 = 25;
  XMIN = -10;
  XMAX = +10;
var
  i: Integer;
  x, y, dy: Double;
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);

  SetLength(Data1, N1);
  for i:=0 to N1-1 do begin
    Data1[i].x := XMIN + (XMAX - XMIN) * i / (N1-1);
    Data1[i].y := sin(Data1[i].x * 0.5);
    Data1[i].dy := randg(0.2, 0.05);
  end;
  ChartSourceLine1.YCount := 1;
  ChartSourceLine1.PointsNumber := N1;
  ChartSourceArea1.YCount := 2;
  ChartSourceArea1.PointsNumber := N1;

  SetLength(Data2, N2);
  for i:=0 to N2-1 do begin
    Data2[i].x := XMIN + (XMAX - XMIN) * i / (N2-1);
    Data2[i].y := cos(Data2[i].x); // + randg(0, 0.2);
    Data2[i].dy := randg(0.2, 0.05);
  end;
  ChartSourceLine2.YCount := 1;
  ChartSourceLine2.PointsNumber := N2;
  ChartSourceArea2.YCount := 3;
  ChartSourceArea2.PointsNumber := N2;

  {$IF LCL_FullVersion >= 1090000}
  Chart2AreaSeries1.Banded := true;
  Chart2AreaSeries2.Banded := true;
  {$ELSE}
  Label1.Show;
  {$ENDIF}
end;

procedure TForm1.EdCountChange(Sender: TObject);
begin
  PopulateSeries(EdCount.Value, EdYCount.Value);
end;

procedure TForm1.PopulateSeries(N, NY: Integer);
var
  i: Integer;
  idx: Integer;
begin
  Chart1AreaSeries1.Clear;
  Chart1AreaSeries1.ListSource.YCount := NY;
  for i:=1 to N do
    Chart1AreaSeries1.AddXY(i, Random, [0.1 + Random, 0.2 + Random, 0.3 + Random, 0.2 + Random]);

  UpdateStyles(Chart1AreaSeries1.ListSource.YCount);

  idx := CbLabelsAt.ItemIndex;
  CbLabelsAt.Clear;
  CbLabelsAt.Items.Add('all');
  for i:= 0 to NY - 1 do
    CbLabelsAt.Items.Add('y index ' + IntToStr(i));
  if (idx > -1) and (idx < CblabelsAt.Items.Count) then
    CbLabelsAt.ItemIndex := idx
  else
    CbLabelsAt.ItemIndex := 0;
end;

procedure TForm1.UpdateStyles(Count: Integer);
const
  COLORS: array[0..4] of TColor = (clRed, clBlue, clYellow, clGreen, clFuchsia);
var
  i: Integer;
begin
  ChartStyles1.Styles.Clear;
  for i := 1 to Count do
    {$IF LCL_FullVersion >= 1090000}
    with ChartStyles1.Add do begin
    {$ELSE}
    with TChartStyle(ChartStyles1.Styles.Add) do begin
    {$IFEND}
      Brush.Color := COLORS[i-1];
      Pen.Width := 3;
      Text := 'Curve ' + IntToStr(i);
    end;
end;


{
  if CbStacked.Checked then begin
    Chart1AreaSeries1.AxisIndexX := 0;
    Chart1AreaSeries1.AxisIndexY := 1;
  end else begin
    Chart1AreaSeries1.AxisIndexX := 1;
    Chart1AreaSeries1.AxisIndexY := 0;
  end;
end;
 }
end.

