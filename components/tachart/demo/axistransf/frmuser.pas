unit frmUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  TAGraph, TASeries, TASources, TATools, TATransformations;

type
  TUserFrame = class(TFrame)
    catUser: TChartAxisTransformations;
    catUserUserDefinedAxisTransform1: TUserDefinedAxisTransform;
    ChartToolset1: TChartToolset;
    DataPointDragTool: TDataPointDragTool;
    Chart: TChart;
    VerticalLine: TConstantLine;
    LineSeries: TLineSeries;
    rcsUser: TRandomChartSource;
    procedure catUserUserDefinedAxisTransform1AxisToGraph(AX: Double; out AT:
      Double);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TUserFrame.catUserUserDefinedAxisTransform1AxisToGraph(AX: Double;
  out AT: Double);
const
  R1 = 8.0;
  C = 2.5;
  R2 = R1 / C;
var
  zx: Double;
begin
  zx := VerticalLine.Position;
  if Abs(AX - zx) > R1 then AT := AX
  else if AX < zx - R2 then AT := zx - R1
  else if AX > zx + R2 then AT := zx + R1
  else AT := (AX - zx + R2) * C + zx - R1;
end;

end.

