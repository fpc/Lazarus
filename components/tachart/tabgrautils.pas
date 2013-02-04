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
unit TABGRAUtils;

interface

{$H+}

uses
  BGRABitmap, BGRABitmapTypes, BGRAGradients, Graphics, Types,
  TASeries;

function CreateChocolateBar(
  ALightPos: TPoint; ARect: TRect; ABackColor: TBGRAPixel;
  ARoundedCorners: Boolean; AOptions: TRectangleMapOptions): TBGRABitmap;
procedure DrawPhong3DBar(ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect);

implementation

uses
  TAChartUtils, TAGeometry;

function CreateChocolateBar(
  ALightPos: TPoint; ARect: TRect; ABackColor: TBGRAPixel;
  ARoundedCorners: Boolean; AOptions: TRectangleMapOptions): TBGRABitmap;
var
  phong: TPhongShading;
  t: TPoint;
begin
  t := MaxPoint(ARect.BottomRight - ARect.TopLeft, Point(0, 0));
  Result := TBGRABitmap.Create(t.X, t.Y, ABackColor);
  if (t.X = 0) and (t.Y = 0) then exit;
  phong := TPhongShading.Create;
  try
    phong.AmbientFactor := 0.5;
    phong.LightPosition := ALightPos - ARect.TopLeft;
    phong.DrawRectangle(
      Result, BoundsSize(0, 0, t), t.X div 8, t.X div 8,
      GammaCompression(SetIntensity(GammaExpansion(BGRA(86, 41, 38)), 30000)),
      ARoundedCorners, AOptions);
  finally
    phong.Free;
  end;
end;

procedure DrawPhong3DBar(ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect);

  function CreatePhong3DBar(
    ALightPos: TPoint; ARect: TRect; ADepth: Integer;
    AColor: TBGRAPixel): TBGRABitmap;
  var
    phong: TPhongShading;
    i: Integer;
    map: TBGRABitmap;
    h: TBGRAPixel;
    t: TPoint;
  begin
    t := MaxPoint(ARect.BottomRight - ARect.TopLeft, Point(0, 0));
    map := TBGRABitmap.Create(t.X + ADepth,t.Y + ADepth);
    try
      map.FillRect(0, ADepth, t.X, t.Y + ADepth, BGRAWhite, dmSet);
      for i := 1 to ADepth do begin
        h := MapHeightToBGRA((ADepth - i) / ADepth, 255);
        map.SetHorizLine(i, ADepth - i, t.X - 1 + i - 1, h);
        map.SetVertLine(t.X - 1 + i, ADepth - i, t.Y + ADepth - 1 - i, h);
      end;
      Result := TBGRABitmap.Create(t.X + ADepth, t.Y + ADepth);
      if (Result.width = 0) or (Result.Height = 0) then exit;
      phong := TPhongShading.Create;
      try
        phong.AmbientFactor := 0.5;
        phong.LightPosition := ALightPos - ARect.TopLeft;
        phong.Draw(Result, map, ADepth, 0, 0, AColor);
      finally
        phong.Free;
      end;
    finally
      map.Free;
    end;
  end;

  function DrawContour(ABar: TBGRABitmap): TPoint;
  var
    size: TPoint;
    temp: TBGRABitmap;
    margin, depth: integer;
  begin
    Result := point(0, 0);
    if ASeries.BarPen.Style = psClear then exit;
    size := ARect.BottomRight - ARect.TopLeft;
    if ASeries.BarPen.Width > 1 then begin
      margin := (ASeries.BarPen.Width + 1) div 2;
      Result := Point(margin, margin);
      temp := TBGRABitmap.Create(ABar.Width + 2 * margin, ABar.Height + 2 * margin);
      temp.PutImage(Result.X, Result.Y, ABar, dmSet);
      BGRAReplace(ABar, temp);
    end;
    depth := ASeries.Depth;
    with ABar.CanvasBGRA do begin
      Pen.Assign(ASeries.BarPen);
      Brush.Style := bsClear;
      Polygon([
         Point(Result.x + 0, Result.y + depth),
         Point(Result.x + depth, Result.y + 0),
         Point(Result.x + size.x - 1 + depth, Result.y + 0),
         Point(Result.x + size.x - 1 + depth, Result.y + size.y - 1),
         Point(Result.x + size.x - 1, Result.y + size.y - 1 + depth),
         Point(Result.x + 0, Result.y + size.y - 1 + depth)
      ]);
    end;
  end;

var
  bar: TBGRABitmap;
begin
  bar := CreatePhong3DBar(
    Point(ASeries.ParentChart.ClientWidth div 2, 0),
    ARect, ASeries.Depth, ColorToBGRA(ASeries.BarBrush.Color));
  try
    with (ARect.TopLeft - DrawContour(bar)) do
      bar.Draw(ACanvas, X, Y - ASeries.Depth, false);
  finally
    bar.Free;
  end;
end;

end.
