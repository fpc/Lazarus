unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  TAGraph, TASeries, TAChartUtils, TATools, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    Chart: TChart;
    ReferenceLine: TConstantLine;
    Series: TUserDrawnSeries;
    ChartToolset: TChartToolset;
    DataPointDragTool: TDataPointDragTool;
    cbShowDataPoints: TCheckBox;
    Panel: TPanel;
    procedure SeriesDraw(ACanvas: TCanvas; const ARect: TRect);
    procedure SeriesGetBounds(var ABounds: TDoubleRect);
    procedure cbShowDataPointsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Pen: TPen;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math, TAGeometry;

const
  COLOR_ABOVE = clRed;
  COLOR_BELOW = clGreen;
  RADIUS = 4;

type
  TDataRec = record
    x, y: Double;
  end;

var
  Data: array of TDataRec;
  Extent: TDoubleRect;

function Intersect(p1, p2: TPoint; y: Integer): Integer;
begin
  if p1.x = p2.x then
    Result := p1.x
  else
    Result := round((p2.x - p1.x) / (p2.y - p1.y) * (y - p1.y) + p1.x);
end;


{ TMainForm }

procedure TMainForm.SeriesDraw(ACanvas: TCanvas;
  const ARect: TRect);

  procedure DrawCircle(p: TPoint; AColor: TColor; R: Integer);
  begin
    Chart.Drawer.SetBrushParams(bsSolid, AColor);
    Chart.Drawer.Ellipse(P.x - R, P.y - R, P.x + R, P.y + R);
  end;

  function GetColor(y, yref: Integer): TColor;
  begin
    if y > yref then Result := COLOR_BELOW else Result := COLOR_ABOVE;
  end;

var
  i: Integer;
  gp: TDoublePoint;
  p1, p2, p3: TPoint;
  yref: Integer;
  x: Integer;
  showDataPoints: Boolean;
begin
  showDataPoints := cbShowDataPoints.Checked;
  Chart.Drawer.Pen := Pen;
  yref := Chart.YGraphToImage(ReferenceLine.Position);

  i := 0;
  gp := DoublePoint(Data[i].X, Data[i].Y);
  p1 := Chart.GraphToImage(gp);
  if showDataPoints then begin
    Chart.Drawer.SetPenParams(psSolid, GetColor(p1.y, yref));
    DrawCircle(p2, GetColor(p2.y, yref), RADIUS);
  end;

  for i:=0 to High(Data)-1 do begin
    gp := DoublePoint(Data[i].X, Data[i].Y);
    p2 := Chart.GraphToImage(gp);
    if (p2.y - yref) * (p1.y - yref) < 0 then begin
      p3 := Point(Intersect(p1, p2, yref), yref);
      Chart.Drawer.SetPenParams(psSolid, GetColor(p1.y, yref));
      Chart.Drawer.Line(p1, p3);
      Chart.Drawer.SetPenParams(psSolid, GetColor(p2.y, yref));
      Chart.Drawer.Line(p3, p2);
    end else begin
      Chart.Drawer.SetPenParams(psSolid, GetColor(p2.y, yref));
      Chart.Drawer.Line(p1, p2);
    end;
    if showDataPoints then
      DrawCircle(p2, GetColor(p2.y, yref), RADIUS);
    p1 := p2;
  end;
end;

procedure TMainForm.SeriesGetBounds(var ABounds: TDoubleRect);
begin
  ABounds := Extent;
end;

procedure TMainForm.cbShowDataPointsChange(Sender: TObject);
begin
  Chart.Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  N = 20;
  XMIN = -10;
  XMAX = +10;
var
  i: Integer;
  x, y: Double;
begin
  Chart.DoubleBuffered := true;

  Pen := TPen.Create;
  Pen.Width := 3;

  SetLength(Data, N);
  for i:=0 to N-1 do begin
    Data[i].x := XMIN + (XMAX - XMIN) / (N - 1) * i;
//    Data[i].y := sin(Data[i].x);
    Data[i].y := (Random * 2 - 1) * 100;
  end;

  Extent.a.x := Data[0].x;
  Extent.a.y := Data[0].y;
  Extent.b.x := Data[0].x;
  Extent.b.y := Data[0].y;
  for i:= 1 to High(Data) do begin
    Extent.a.x := Min(Extent.a.x, Data[i].x);
    Extent.b.x := Max(Extent.b.x, Data[i].x);
    Extent.a.y := Min(Extent.a.y, Data[i].y);
    Extent.b.y := Max(Extent.b.y, Data[i].y);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Pen.Free;
end;


end.

