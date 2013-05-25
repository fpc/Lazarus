{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  AColor: TBGRAPixel; ALightPos: TPoint; ARect: TRect; ABorder: Integer;
  ARoundedCorners: Boolean; AOptions: TRectangleMapOptions): TBGRABitmap;
procedure DrawChocolateBar(
  ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect; APointIndex: Integer;
  ARounded: boolean);
function CreatePhong3DBar(
  AColor: TBGRAPixel; ALightPos: TPoint; var ARect: TRect;
  ADepth: Integer): TBGRABitmap;
procedure DrawPhong3DBar(
  ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect; APointIndex: Integer);

implementation

uses
  TAChartUtils, TADrawUtils, TAGeometry;

function BarColor(ASeries: TBarSeries; APointIndex: Integer): TBGRAPixel;
begin
  with ASeries do
    Result := ColorToBGRA(ColorToRGB(
      ColorDef(Source[APointIndex]^.Color, BarBrush.Color)), 255 - Transparency);
end;

function CreateChocolateBar(
  AColor: TBGRAPixel; ALightPos: TPoint; ARect: TRect; ABorder: Integer;
  ARoundedCorners: Boolean; AOptions: TRectangleMapOptions): TBGRABitmap;
var
  phong: TPhongShading;
  t: TPoint;
begin
  t := MaxPoint(ARect.BottomRight - ARect.TopLeft, Point(0, 0));
  Result := TBGRABitmap.Create(t.X, t.Y);
  if (t.X = 0) and (t.Y = 0) then exit;
  if ABorder < 0 then ABorder := 0;
  phong := TPhongShading.Create;
  try
    phong.AmbientFactor := 0.5;
    phong.LightPosition := ALightPos - ARect.TopLeft;
    phong.DrawRectangle(
      Result, BoundsSize(0, 0, t), ABorder, ABorder,
      AColor, ARoundedCorners, AOptions);
  finally
    phong.Free;
  end;
end;

procedure DrawChocolateBar(
  ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect;
  APointIndex: Integer; ARounded: boolean);
var
  bar: TBGRABitmap;
  border: Integer;
begin
  border := (ARect.Right - ARect.Left) div 8;
  ARect.Top += -border div 2 + 1;
  ARect.Bottom += border div 2 + 1;
  bar := CreateChocolateBar(
    BarColor(ASeries, APointIndex),
    Point(ASeries.ParentChart.ClientWidth div 2, 0),
    ARect, border, ARounded, []);
  try
    with ARect.TopLeft do
      bar.Draw(ACanvas, X, Y, false);
  finally
    bar.Free;
  end;
end;

function CreatePhong3DBar(
  AColor: TBGRAPixel; ALightPos: TPoint; var ARect: TRect;
  ADepth: Integer): TBGRABitmap;
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
    ARect.Top -= ADepth;
    ARect.Right += ADepth;
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

procedure DrawPhong3DBar(
  ASeries: TBarSeries; ACanvas: TCanvas; ARect: TRect; APointIndex: Integer);

  procedure DrawContour(var ABar: TBGRABitmap; var ADrawnRect: TRect);
  var
    size: TPoint;
    temp: TBGRABitmap;
    marginValue, depth: integer;
    margin: TPoint;
  begin
    margin := point(0, 0);
    if ASeries.BarPen.Style = psClear then exit;
    size := ARect.BottomRight - ARect.TopLeft;
    if ASeries.BarPen.Width > 1 then begin
      marginValue := (ASeries.BarPen.Width + 1) div 2;
      margin := Point(marginValue, marginValue);
      temp := TBGRABitmap.Create(
        ABar.Width + 2 * margin.X, ABar.Height + 2 * margin.Y);
      temp.PutImage(margin.X, margin.Y, ABar, dmSet);
      BGRAReplace(ABar, temp);
      ADrawnRect.TopLeft -= margin;
      ADrawnRect.BottomRight += margin;
    end;
    depth := ASeries.Depth;
    with ABar.CanvasBGRA do begin
      Pen.Assign(ASeries.BarPen);
      Brush.Style := bsClear;
      Polygon([
         Point(margin.x + 0, margin.y + depth),
         Point(margin.x + depth, margin.y + 0),
         Point(margin.x + size.x - 1 + depth, margin.y + 0),
         Point(margin.x + size.x - 1 + depth, margin.y + size.y - 1),
         Point(margin.x + size.x - 1, margin.y + size.y - 1 + depth),
         Point(margin.x + 0, margin.y + size.y - 1 + depth)
      ]);
    end;
  end;

var
  bar: TBGRABitmap;
begin
  bar := CreatePhong3DBar(
    BarColor(ASeries, APointIndex),
    Point(ASeries.ParentChart.ClientWidth div 2, 0), ARect, ASeries.Depth);
  try
    DrawContour(bar, ARect);
    with ARect.TopLeft do
      bar.Draw(ACanvas, X, Y, false);
  finally
    bar.Free;
  end;
end;

end.
