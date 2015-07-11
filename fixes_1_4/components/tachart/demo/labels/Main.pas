unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms, Types,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources, TAChartAxis,
  TAChartUtils, TATextElements;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    cbHideOverlapping: TCheckBox;
    ChartMulti: TChart;
    ChartMultiLineSeries1: TLineSeries;
    cbShape: TComboBox;
    gbAngles: TGroupBox;
    lblAxisAngle: TLabel;
    lblSeriesAngle: TLabel;
    lblTitleAngle: TLabel;
    lblCalloutAngle: TLabel;
    pcMain: TPageControl;
    pnlControls: TPanel;
    RandomChartSource1: TRandomChartSource;
    seAxisAngle: TSpinEdit;
    seSeriesAngle: TSpinEdit;
    seTitleAngle: TSpinEdit;
    Multiline: TTabSheet;
    seCalloutAngle: TSpinEdit;
    tsBar: TTabSheet;
    procedure cbHideOverlappingChange(Sender: TObject);
    procedure cbShapeChange(Sender: TObject);
    procedure Chart1BarSeries1MarksGetShape(ASender: TChartTextElement;
      const ABoundingBox: TRect; var APolygon: TPointArray);
    procedure ChartMultiAxisList1MarkToText(var AText: String; AMark: Double);
    procedure seAxisAngleChange(Sender: TObject);
    procedure seCalloutAngleChange(Sender: TObject);
    procedure seSeriesAngleChange(Sender: TObject);
    procedure seTitleAngleChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math, TAGeometry;

{ TForm1 }

procedure TForm1.cbHideOverlappingChange(Sender: TObject);
var
  op: TChartMarksOverlapPolicy;
begin
  if cbHideOverlapping.Checked then
    op := opHideNeighbour
  else
    op := opIgnore;
  Chart1.LeftAxis.Marks.OverlapPolicy := op;
  Chart1.BottomAxis.Marks.OverlapPolicy := op;
  ChartMulti.LeftAxis.Marks.OverlapPolicy := op;
  ChartMulti.BottomAxis.Marks.OverlapPolicy := op;
  Chart1BarSeries1.Marks.OverlapPolicy := op;
end;

procedure TForm1.cbShapeChange(Sender: TObject);
var
  s: TChartLabelShape;
begin
  s := TChartLabelShape(cbShape.ItemIndex);
  Chart1BarSeries1.Marks.Shape := s;
  ChartMulti.LeftAxis.Marks.Shape := s;
  ChartMulti.BottomAxis.Marks.Shape := s;
  with Chart1BarSeries1.Marks do
    if s = clsUserDefined then
      OnGetShape := @Chart1BarSeries1MarksGetShape
    else
      OnGetShape := nil;
end;

procedure TForm1.Chart1BarSeries1MarksGetShape(
  ASender: TChartTextElement; const ABoundingBox: TRect;
  var APolygon: TPointArray);
const
  N = 24;
var
  i: Integer;
  a, b, k: Double;
  cp: TPoint;
  s, c: Extended;
begin
  Unused(ASender);
  SetLength(APolygon, N);
  with ABoundingBox do begin
    a := (Right - Left) / 2;
    b := (Bottom - Top) / 2;
  end;
  cp := CenterPoint(ABoundingBox);
  for i := 0 to High(APolygon) do begin
    SinCos(i * 2 * Pi / N, s, c);
    k := (i mod 2) * 0.5 + 1;
    APolygon[i] := cp + Point(Round(a * c * k), Round(b * s * k));
  end;
end;

procedure TForm1.ChartMultiAxisList1MarkToText(
  var AText: String; AMark: Double);
begin
  AText += Format(#13#10'%.9g', [AMark * 2]);
end;

procedure TForm1.seAxisAngleChange(Sender: TObject);
begin
  Chart1.LeftAxis.Marks.LabelFont.Orientation := 900 + seAxisAngle.Value * 10;
  Chart1.BottomAxis.Marks.LabelFont.Orientation := seAxisAngle.Value * 10;
  ChartMulti.LeftAxis.Marks.LabelFont.Orientation := 900 + seAxisAngle.Value * 10;
  ChartMulti.BottomAxis.Marks.LabelFont.Orientation := seAxisAngle.Value * 10;
end;

procedure TForm1.seCalloutAngleChange(Sender: TObject);
begin
  with Chart1BarSeries1.Marks do begin
    CalloutAngle := seCalloutAngle.Value * 10;
    LinkPen.Visible := seCalloutAngle.Value = 0;
    Arrow.Visible := seCalloutAngle.Value = 0;
  end;
end;

procedure TForm1.seSeriesAngleChange(Sender: TObject);
begin
  Chart1BarSeries1.Marks.LabelFont.Orientation := seSeriesAngle.Value * 10;
end;

procedure TForm1.seTitleAngleChange(Sender: TObject);
begin
  Chart1.LeftAxis.Title.LabelFont.Orientation := 900 + seTitleAngle.Value * 10;
  Chart1.BottomAxis.Title.LabelFont.Orientation := seTitleAngle.Value * 10;
  ChartMulti.Title.Font.Orientation := seTitleAngle.Value * 10;
end;

end.

