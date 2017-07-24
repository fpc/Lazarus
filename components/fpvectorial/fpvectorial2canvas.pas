{
Implements support for drawing to the LCL TCanvas

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
}
unit fpvectorial2canvas;

{$ifdef fpc}
  {$mode objfpc}{$h+}
{$endif}

{$define USE_CANVAS_CLIP_REGION}
{.$define DEBUG_CANVAS_CLIP_REGION}
{$define USE_LCL_CANVAS}

{.$define FPVECTORIAL_DEBUG_DIMENSIONS}
{.$define FPVECTORIAL_TOCANVAS_DEBUG}
{.$define FPVECTORIAL_DEBUG_BLOCKS}
{.$define FPVECTORIAL_AUTOFIT_DEBUG}
{.$define FPVECTORIAL_SUPPORT_LAZARUS_1_6}
// visual debugs
{.$define FPVECTORIAL_TOCANVAS_ELLIPSE_VISUALDEBUG}
{.$define FPVECTORIAL_RENDERINFO_VISUALDEBUG}

interface

uses
  Classes, SysUtils, Math, TypInfo, contnrs, types,
  // FCL-Image
  fpcanvas, fpimage, fpwritebmp,
  // lazutils
  laz2_dom,
  // LCL
  lazutf8, lazregions,
  Graphics, LCLIntf, LCLType, intfgraphics, graphtype, interfacebase,
  // fpvectorial
  fpvutils, fpvectorial;

type

  { TFPVCanvasRenderer }

  TFPVCanvasRenderer = class(TvRenderer)
  public
    procedure BeginRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean); override;
    procedure EndRender(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean); override;
    // TPath
    procedure TPath_Render(var ARenderInfo: TvRenderInfo; ADoDraw: Boolean; APath: TPath); override;
  end;

implementation

{ TFPVCanvasRenderer }

procedure TFPVCanvasRenderer.BeginRender(var ARenderInfo: TvRenderInfo;
  ADoDraw: Boolean);
begin

end;

procedure TFPVCanvasRenderer.EndRender(var ARenderInfo: TvRenderInfo;
  ADoDraw: Boolean);
begin

end;

procedure TFPVCanvasRenderer.TPath_Render(var ARenderInfo: TvRenderInfo;
  ADoDraw: Boolean; APath: TPath);
var
  ADest: TFPCustomCanvas absolute ARenderInfo.Canvas;
  ADestX: Integer absolute ARenderInfo.DestX;
  ADestY: Integer absolute ARenderInfo.DestY;
  AMulX: Double absolute ARenderInfo.MulX;
  AMulY: Double absolute ARenderInfo.MulY;
  //
  i: Integer;
  j, n: Integer;
  x1, y1, x2, y2: Integer;
  pts: TPointsArray;
  ACanvas: TCanvas absolute ARenderInfo.Canvas;
  coordX, coordY: Integer;
  curSegment: TPathSegment;
  cur2DSegment: T2DSegment absolute curSegment;
  lRect: TRect;
  gv1, gv2: T2DPoint;
begin
  with APath do begin
  ConvertPathToPolygons(APath, ADestX, ADestY, AMulX, AMulY, FPolyPoints, FPolyStarts);
  x1 := MaxInt;
  y1 := maxInt;
  x2 := -MaxInt;
  y2 := -MaxInt;
  for i := 0 to High(FPolyPoints) do
  begin
    {$ifdef FPVECTORIAL_AUTOFIT_DEBUG}
    if AutoFitDebug <> nil then AutoFitDebug.Add(Format('==[%d=%d]', [FPolyPoints[i].X, FPolyPoints[i].Y]));
    {$endif}
    x1 := min(x1, FPolyPoints[i].X);
    y1 := min(y1, FPolyPoints[i].Y);
    x2 := max(x2, FPolyPoints[i].X);
    y2 := max(y2, FPolyPoints[i].Y);
  end;
  CalcEntityCanvasMinMaxXY_With2Points(ARenderInfo, x1, y1, x2, y2);
  // Boundary rect of shape filled with a gradient
  lRect := Rect(x1, y1, x2, y2);

  if ADoDraw then
  begin
    // (1) draw background only
    ADest.Pen.Style := psClear;
    if (Length(FPolyPoints) > 2) then
      case Brush.Kind of
        bkSimpleBrush:
          if Brush.Style <> bsClear then
          begin
            if (Brush.Style = bsSolid) and (Length(FPolyStarts) > 1) then
              // Non-contiguous polygon (polygon with "holes") --> use special procedure
              // Disadvantage: it can only do solid fills!
              APath.DrawPolygon(ARenderInfo, FPolyPoints, FPolyStarts, lRect)
            else
              {$IFDEF USE_LCL_CANVAS}
              for i := 0 to High(FPolyStarts) do
              begin
                j := FPolyStarts[i];
                if i = High(FPolyStarts) then
                  n := Length(FPolyPoints) - j
                else
                  n := FPolyStarts[i+1] - FPolyStarts[i]; // + 1;
                ACanvas.Polygon(@FPolyPoints[j], n, WindingRule = vcmNonZeroWindingRule);
              end;
              {$ELSE}
              ADest.Polygon(FPolyPoints);
              {$ENDIF}
          end;

        bkHorizontalGradient,
        bkVerticalGradient,
        bkOtherLinearGradient:
          begin
            // calculate gradient vector
            CalcGradientVector(gv1, gv2, lRect, ADestX, ADestY, AMulX, AMulY);
            // Draw the gradient
            DrawPolygonBrushLinearGradient(ARenderInfo, FPolyPoints, FPolyStarts, lRect, gv1, gv2);
          end;

        bkRadialGradient:
          DrawPolygonBrushRadialGradient(ARenderInfo, FPolyPoints, lRect);
      end;  // case Brush.Kind of...

    // (2) draw border, take care of the segments with modified pen
    ADest.Brush.Style := bsClear;               // We will paint no background
    ApplyPenToCanvas(ARenderInfo, Pen);  // Restore pen

    PrepareForSequentialReading;
    for j := 0 to Len - 1 do
    begin
      curSegment := TPathSegment(Next);
      case curSegment.SegmentType of
        stMoveTo:
          begin
            inc(i);
            coordX := CoordToCanvasX(cur2DSegment.X, ADestX, AMulX);
            coordY := CoordToCanvasY(cur2DSegment.Y, ADestY, AMulY);
            ADest.MoveTo(coordX, coordY);
          end;
        st2DLineWithPen, st2DLine, st3DLine:
          begin
            coordX := CoordToCanvasX(cur2DSegment.X, ADestX, AMulX);
            coordY := CoordToCanvasY(cur2DSegment.Y, ADestY, AMulY);
            if curSegment.SegmentType = st2DLineWithPen then
            begin
              ADest.Pen.FPColor := AdjustColorToBackground(T2DSegmentWithPen(Cur2DSegment).Pen.Color, ARenderInfo);
              ADest.Pen.Width := T2DSegmentWithPen(cur2DSegment).Pen.Width;
              ADest.Pen.Style := T2DSegmentWithPen(cur2DSegment).Pen.Style;
              ADest.LineTo(coordX, coordY);
              ApplyPenToCanvas(ARenderInfo, Pen);
            end else
              ADest.LineTo(coordX, coordY);
          end;
        st2DBezier, st3DBezier, st2DEllipticalArc:
          begin
            coordX := CoordToCanvasX(T2DSegment(curSegment.Previous).X, ADestX, AMulX);
            coordY := CoordToCanvasY(T2DSegment(curSegment.Previous).Y, ADestY, AMulY);
            SetLength(pts, 1);
            pts[0] := Point(coordX, coordY);
            curSegment.AddToPoints(ADestX, ADestY, AMulX, AMulY, pts);
            if Length(pts) > 0 then
            begin
              ADest.PolyLine(pts);
              ADest.MoveTo(pts[High(pts)].X, pts[High(pts)].Y);
            end;
          end;
      end;
    end;
  end;
  end;
end;

initialization

RegisterDefaultRenderer(TFPVCanvasRenderer);

end.

