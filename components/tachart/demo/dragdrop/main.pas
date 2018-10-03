unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources, TATools, Types, TAChartUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbSorted: TCheckBox;
    chPoints: TChart;
    chBarsBarSeries1: TBarSeries;
    chPointsLineSeries1: TLineSeries;
    chBars: TChart;
    ctBars: TChartToolset;
    ctBarsDataPointDragTool1: TDataPointDragTool;
    ctPoints: TChartToolset;
    ctPointsDataPointClickTool1: TDataPointClickTool;
    ctPointsDataPointDragTool1: TDataPointDragTool;
    ctPointsDataPointHintTool1: TDataPointHintTool;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RandomChartSource1: TRandomChartSource;
    tsPoints: TTabSheet;
    tsBars: TTabSheet;
    procedure cbSortedChange(Sender: TObject);
    procedure chPointsLineSeries1GetMark(out AFormattedMark: String;
      AIndex: Integer);
    procedure ctBarsDataPointDragTool1Drag(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure ctBarsDataPointDragTool1DragStart(ASender: TDataPointDragTool;
      var AGraphPoint: TDoublePoint);
    procedure ctPointsDataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure ctPointsDataPointHintTool1Hint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.cbSortedChange(Sender: TObject);
begin
  chPointsLineSeries1.ListSource.Sorted := cbSorted.Checked;
end;

procedure TForm1.chPointsLineSeries1GetMark(
  out AFormattedMark: String; AIndex: Integer);
begin
  // Show item label for last dragged point.
  if AIndex = ctPointsDataPointDragTool1.PointIndex then
    with chPointsLineSeries1 do
      AFormattedMark := Source.FormatItem(Marks.Format, AIndex, 0)
  else
    AFormattedMark := '';
end;

procedure TForm1.ctBarsDataPointDragTool1Drag(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
begin
  // Only allow vertical dragging.
  AGraphPoint.X := ASender.Origin.X;
end;

procedure TForm1.ctBarsDataPointDragTool1DragStart(ASender: TDataPointDragTool;
  var AGraphPoint: TDoublePoint);
const
  Y_TOLERANCE = 3;
var
  f: TGraphToImageFunc;
begin
  f := @chBars.YGraphToImage;
  if Abs(f(ASender.Origin.Y) - f(AGraphPoint.Y)) > Y_TOLERANCE then
    ASender.Handled;
end;

procedure TForm1.ctPointsDataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);
var
  pi: Integer;
begin
  Unused(ATool, APoint);
  pi := ctPointsDataPointClickTool1.PointIndex;
  with chPointsLineSeries1 do
    SetColor(pi, IfThen(GetColor(pi) = clRed, clTAColor, clRed));
end;

procedure TForm1.ctPointsDataPointHintTool1Hint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
begin
  Unused(APoint);
  AHint := 'Custom hint for point ' + IntToStr(ATool.PointIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RandSeed := 675402;
  for i := 1 to 10 do
    chPointsLineSeries1.AddXY(i, Random(20) - 10);
  chBarsBarSeries1.ListSource.CopyFrom(RandomChartSource1);
end;

end.

