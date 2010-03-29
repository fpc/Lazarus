{

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Alexander Klenin

}

unit TATools;

interface

{$H+}

uses
  Types,
  TAGraph;

{ TChartZoomDragTool }

type
  TChartZoomDragTool = class(TChartTool)
  private
    FSelectionRect: TRect;
  public
    procedure MouseDown(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp(APoint: TPoint); override;
  end;

implementation

uses
  Classes, TAChartUtils;

procedure InitBuitlinTools(AToolset: TChartToolset);
begin
  TChartZoomDragTool.Create((AToolset as TChartToolset).Tools).Shift := [ssLeft];
end;

{ TChartZoomDragTool }

procedure TChartZoomDragTool.MouseDown(APoint: TPoint);
begin
  Activate;
  with APoint do
    FSelectionRect := Rect(X, Y, X, Y);
end;

procedure TChartZoomDragTool.MouseMove(APoint: TPoint);
begin
  if not IsActive then exit;
  PrepareXorPen(Chart.Canvas);
  Chart.Canvas.Rectangle(FSelectionRect);
  FSelectionRect.BottomRight := APoint;
  Chart.Canvas.Rectangle(FSelectionRect);
end;

procedure TChartZoomDragTool.MouseUp(APoint: TPoint);
begin
  Unused(APoint);
  Deactivate;
  with Chart do begin
    PrepareXorPen(Canvas);
    Canvas.Rectangle(FSelectionRect);
    ZoomToRect(FSelectionRect);
  end;
end;

initialization

  OnInitBuiltinTools := @InitBuitlinTools;

end.

