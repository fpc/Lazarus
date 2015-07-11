unit main;
 
{$mode objfpc}{$H+}
 
interface
 
uses
  Classes, ExtCtrls, StdCtrls, SysUtils, TAGraph, TAFuncSeries,
  TASources, Forms, Controls, Graphics, Dialogs, TATypes, TATools, Types;
 
type
 
  { TForm1 }
 
  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1ColorMapSeries1: TColorMapSeries;
    ChartToolset1: TChartToolset;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1UserDefinedTool1: TUserDefinedTool;
    ChartToolset1UserDefinedTool2: TUserDefinedTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ColorSource: TListChartSource;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LblMagnification: TLabel;
    LblHistoryCount: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Chart1ColorMapSeries1Calculate(const AX, AY: Double;
      out AZ: Double);
    procedure Chart1ExtentChanged(ASender: TChart);
    procedure ChartToolset1PanDragTool1AfterMouseDown(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1UserDefinedTool1AfterMouseUp(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1UserDefinedTool2AfterMouseUp(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset1ZoomDragTool1AfterMouseUp(ATool: TChartTool;
      APoint: TPoint);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ZoomHistory: TChartExtentHistory;
    procedure PopulateColorSource;
  end;
 
var
  Form1: TForm1;
 
implementation
 
{$R *.lfm}
 
uses
  StrUtils,
  TAChartUtils, TAGeometry;
 
const
  MANDELBROT_NUM_ITERATIONS = 100;
  MANDELBROT_ESCAPE_RADIUS = 2.0;
  MANDELBROT_LIMIT = sqr(MANDELBROT_ESCAPE_RADIUS);
 
function InMandelBrotSet(
  AC: TDoublePoint; out AIterations: Integer; out AZ: TDoublePoint): Boolean;
var
  j: Integer;
begin
  AIterations := 0;
  AZ := DoublePoint(0.0, 0.0);
  for j := 0 to MANDELBROT_NUM_ITERATIONS - 1 do begin
    AZ := DoublePoint(
      Sqr(AZ.X) - Sqr(AZ.Y) + AC.X,
      2 * AZ.X * AZ.Y + AC.Y);
    if Sqr(AZ.X) + Sqr(AZ.Y) > MANDELBROT_LIMIT then
      // point did escape --> AC is not in Mandelbrot set
      exit(false);
    AIterations += 1;
  end;
  Result := true;
end;
 
{ TForm1 }
 
procedure TForm1.FormCreate(Sender:TObject);
begin
  PopulateColorSource;
  ZoomHistory := TChartExtentHistory.Create;
  ZoomHistory.Capacity := 100;
end;
 
procedure TForm1.FormDestroy(Sender: TObject);
begin
  ZoomHistory.Free;
end;
 
procedure TForm1.Chart1ColorMapSeries1Calculate(
  const AX, AY: Double; out AZ: Double);
var
  iterations: Integer;
  z: TDoublePoint;
begin
  if InMandelBrotSet(DoublePoint(AX, AY), iterations, z) then
    AZ := -1
    // or - as a solution to the "homework" exercise: 
    // AZ := sqrt(sqr(z.x) + sqr(z.y)) / MANDELBROT_ESCAPE_RADIUS
  else
    AZ := iterations / MANDELBROT_NUM_ITERATIONS;
end;
 
procedure TForm1.Chart1ExtentChanged(ASender: TChart);
var
  cex, fex: TDoubleRect;
  factor: Double;
begin
  cex := ASender.CurrentExtent;
  fex := ASender.GetFullExtent;
  if cex.b.x = cex.a.x then exit;

  factor := (fex.b.x - fex.a.x) / (cex.b.x - cex.a.x);
  LblMagnification.Caption :=
    'Magnification: ' + Format(IfThen(factor > 1e6, '%.0e', '%0.g'), [factor]);
  LblHistoryCount.Caption := Format('History count: %d', [ZoomHistory.Count]);
end;

procedure TForm1.ChartToolset1PanDragTool1AfterMouseDown(
  ATool: TChartTool; APoint: TPoint);
begin
  Unused(ATool, APoint);
  ZoomHistory.Add(Chart1.PrevLogicalExtent);
end;
 
procedure TForm1.ChartToolset1UserDefinedTool1AfterMouseUp(
  ATool: TChartTool; APoint: TPoint);
begin
  Unused(ATool, APoint);
  if ZoomHistory.Count > 0 then
    Chart1.LogicalExtent := ZoomHistory.Pop;
end;
 
procedure TForm1.ChartToolset1UserDefinedTool2AfterMouseUp(
  ATool: TChartTool; APoint: TPoint);
begin
  Unused(ATool, APoint);
  Chart1.ZoomFull;
end;
 
procedure TForm1.ChartToolset1ZoomDragTool1AfterMouseUp(
  ATool: TChartTool; APoint: TPoint);
begin
  Unused(ATool, APoint);
  ZoomHistory.Add(Chart1.PrevLogicalExtent);
end;
 
procedure TForm1.PopulateColorSource;
const
  DUMMY = 0.0;
begin
  with ColorSource do begin
    Clear;
    Add(-1.0, DUMMY, '', clBlack);
    Add( 0.0, DUMMY, '', clBlue);
    Add( 0.3, DUMMY, '', clRed);
    Add( 1.0, DUMMY, '', clYellow);
  end;
end;

end.
