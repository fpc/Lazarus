unit vtprimitives;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcanvas, fpimage, fpvectorial;

function CreateCircle(APage: TvVectorialPage; CtrX, CtrY, R: Double): TvCircle;
function CreateEllipse(APage: TvVectorialPage; X1, Y1, X2, Y2: Double): TvEllipse;
function CreateRectangle(APage: TvVectorialPage; X1, Y1, X2, Y2: Double): TvRectangle;
function CreateRoundedRect(APage: TvVectorialPage; X1, Y1, X2, Y2, RX, RY: Double): TvRectangle;
function CreatePolygon(APage: TvVectorialPage; const APoints: array of T3DPoint): TvPolygon;
function CreateArc(APage: TvVectorialPage; X1,Y1, X2,Y2, CX,CY, RX, RY, Angle: Double;
  Clockwise: Boolean): TPath;
function CreateBezier(APage: TvVectorialPage; X1,Y1, X2,Y2, X3,Y3, X4,Y4: Double): TPath;

function CreateSimpleBrush(AStyle: TFPBrushStyle; AColor: TFPColor): TvBrush; overload;
function CreateSimpleBrush(AStyle: TFPBrushStyle): TvBrush; overload;
function CreateLinearGradientBrush(AStartPt, AEndPt: T2DPoint; AFlags: TvGradientFlags;
  AStartColor, AEndColor: TFPColor): TvBrush;
function CreateRadialGradientBrush(CX, CY, R, FX, FY: Double;
  AStartColor, AEndColor: TFPColor): TvBrush;
function CreatePen(AStyle: TFPPenStyle; AWidth: Integer; AColor: TFPColor): TvPen;

function CreateStdCircle(APage: TvVectorialPage): TvCircle;
function CreateStdEllipse(APage: TvVectorialPage): TvEllipse;
function CreateStdRect(APage: TvVectorialPage): TvRectangle;
function CreateStdRoundedRect(APage: TvVectorialPage): TvRectangle;
function CreateStdPolygon(APage: TvVectorialPage): TvPolygon;
function CreateStdSelfIntersectingPolygon(APage: TvVectorialPage): TvPolygon;
function CreatePathWithHole(APage: TvVectorialPage): TPath;

function StdSolidBrush: TvBrush;
function StdHorizGradientBrush: TvBrush;
function StdVertGradientBrush: TvBrush;
function StdLinearGradientBrush: TvBrush;
function StdRadialGradientBrush: TvBrush;
function StdPen: TvPen;

const
  PAGE_SIZE = 100;


implementation

uses
  Math, fpvutils;

{ Shapes }

{ circle with specified center and radius.
  Valid for any coordinate system }
function CreateCircle(APage: TvVectorialPage; CtrX, CtrY, R: Double): TvCircle;
begin
  Result := TvCircle.Create(APage);
  Result.X := CtrX;
  Result.Y := CtrY;
  Result.Radius := R;
  Result.Brush := CreateSimpleBrush(bsClear);
  Result.Pen := CreatePen(psSolid, 1, colBlack);
end;

{ Ellipse with specified center and halfaxes.
  Coordinate system uses an upward y axis for input data, but is flipped if needed }
function CreateEllipse(APage: TvVectorialPage; X1, Y1, X2, Y2: Double): TvEllipse;
begin
  Result := TvEllipse.Create(APage);
  Result.X := (X1 + X2) / 2;      // Center
  Result.Y := (Y1 + Y2) / 2;
  if APage.UseTopLeftCoordinates then
    Result.Y := PAGE_SIZE - Result.Y;
  Result.HorzHalfAxis := abs(X2 - X1) / 2;
  Result.VertHalfAxis := abs(Y2 - Y1) / 2;
  Result.Brush := CreateSimpleBrush(bsClear);
  Result.Pen := CreatePen(psSolid, 1, colBlack);
end;

{ Rectangle with specified top/left corner and width and height.
  Coordinate system uses an upward y axis for input data, but is flipped if needed. }
function CreateRectangle(APage: TvVectorialPage; X1, Y1, X2, Y2: Double): TvRectangle;
begin
  Result := TvRectangle.Create(APage);
  Result.X := Min(X1, X2);
  if APage.UseTopLeftCoordinates then
    Result.Y := Min(PAGE_SIZE-Y1, PAGE_SIZE-Y2) else
    Result.Y := Max(Y1, Y2);
  Result.CX := abs(X2 - X1);    // width
  Result.CY := abs(Y2 - Y1);    // height
  Result.Brush := CreateSimpleBrush(bsClear);
  Result.Pen := CreatePen(psSolid, 1, colBlack);
end;

{ Rectangle with rounded corner
  Coordinate system uses an upward y axis for input data, but is flipped if needed. }
function CreateRoundedRect(APage: TvVectorialPage;
  X1, Y1, X2, Y2, RX, RY: Double): TvRectangle;
begin
  Result := TvRectangle.Create(APage);
  Result.X := Min(X1, X2);
  if APage.UseTopLeftCoordinates then
    Result.Y := Min(PAGE_SIZE-Y1, PAGE_SIZE-Y2) else
    Result.Y := Max(Y1, Y2);
  Result.CX := abs(X2 - X1);
  Result.CY := abs(Y2 - Y1);
  Result.RX := RX;
  Result.RY := RY;
  Result.Brush := CreateSimpleBrush(bsClear);
  Result.Pen := CreatePen(psSolid, 1, colBlack);
end;

{ Polygon with vertices specified in the array.
  Valid for any coordinate system. }
function CreatePolygon(APage: TvVectorialPage;
  const APoints: Array of T3DPoint): TvPolygon;
var
  i: Integer;
begin
  Result := TvPolygon.Create(APage);
  SetLength(Result.Points, Length(APoints));
  for i:=0 to High(APoints) do
    Result.Points[i] := APoints[i];
  Result.X := Result.Points[0].X;
  Result.Y := Result.Points[0].Y;
  Result.Brush := CreateSimpleBrush(bsClear);
  Result.Pen := CreatePen(psSolid, 1, colBlack);
end;

function CreateArc(APage: TvVectorialPage; X1,Y1, X2,Y2, CX,CY, RX, RY, Angle: Double;
  Clockwise: Boolean): TPath;
var
  txt: TvText;
begin
  if APage.UseTopLeftCoordinates then begin
    Y1 := PAGE_SIZE - Y1;
    Y2 := PAGE_SIZE - Y2;
    CY := PAGE_SIZE - CY;
    Angle := -Angle;
  end;
  // Don't invert "Clockwise" here. It does not matter where the y axis points to.

  APage.StartPath(X1, Y1);
  APage.AddEllipticalArcWithCenterToPath(RX, RY, Angle, X2, Y2, CX, CY, Clockwise);
  Result := APage.EndPath;
  Result.Pen := StdPen;

  txt := TvText.Create(APage);
  txt.Value.Add('1');
  txt.X := X1;
  txt.Y := Y1;
  txt.Font.Color := colRed;
  APage.AddEntity(txt);

  txt := TvText.Create(APage);
  txt.Value.Add('2');
  txt.X := X2;
  txt.Y := Y2;
  txt.Font.Color := colRed;
  APage.AddEntity(txt);
end;

function CreateBezier(APage: TvVectorialPage;
  X1,Y1, X2,Y2, X3,Y3, X4,Y4: Double): TPath;
var
  txt: TvText;
begin
  if APage.UseTopLeftCoordinates then begin
    Y1 := PAGE_SIZE - Y1;
    Y2 := PAGE_SIZE - Y2;
    Y3 := PAGE_SIZE - Y3;
    Y4 := PAGE_SIZE - Y4;
  end;
  APage.StartPath(X1, Y1);
  APage.AddBezierToPath(X2,Y2, X3,Y3, X4,Y4);
  Result := APage.EndPath;
  Result.Pen := StdPen;

  APage.StartPath(X1, Y1);
  APage.AddLineToPath(X2, Y2);
  APage.Endpath.Pen.Color := colRed;

  APage.StartPath(X4,Y4);
  APage.AddLineToPath(X3, Y3);
  APage.EndPath.Pen.Color := colRed;

  txt := TvText.Create(APage);
  txt.Value.Add('1');
  txt.X := X1;
  txt.Y := Y1;
  txt.Font.Color := colRed;
  APage.AddEntity(txt);

  txt := TvText.Create(APage);
  txt.Value.Add('2');
  txt.X := X2;
  txt.Y := Y2;
  txt.Font.Color := colRed;
  APage.AddEntity(txt);

  txt := TvText.Create(APage);
  txt.Value.Add('3');
  txt.X := X3;
  txt.Y := Y3;
  txt.Font.Color := colRed;
  APage.AddEntity(txt);

  txt := TvText.Create(APage);
  txt.Value.Add('4');
  txt.X := X4;
  txt.Y := Y4;
  txt.Font.Color := colRed;
  APage.AddEntity(txt);
end;


{ Brushes }

function CreateSimpleBrush(AStyle: TFPBrushStyle): TvBrush;
begin
  Result := CreateSimpleBrush(AStyle, colBlack);
end;

function CreateSimpleBrush(AStyle: TFPBrushStyle; AColor: TFPColor): TvBrush;
begin
  Result.Kind := bkSimpleBrush;
  Result.Color := TFPColor(AColor);
  Result.Style := AStyle;
end;

function CreateLinearGradientBrush(AStartPt, AEndPt: T2DPoint;
  AFlags: TvGradientFlags; AStartColor, AEndColor: TFPColor): TvBrush;
var
  p1, p2: T2dPoint;
  x1str, x2str, y1str, y2str: String;
begin
  if AStartPt.Y = AEndPt.Y then
    Result.Kind := bkHorizontalGradient
  else if AStartPt.X = AEndPt.X then
    Result.Kind := bkVerticalGradient
  else
    Result.Kind := bkOtherLinearGradient;
  Result.Gradient_start := AStartPt;
  Result.Gradient_end := AEndPt;
  Result.Gradient_flags := AFlags;
  SetLength(Result.Gradient_colors, 2);
  Result.Gradient_colors[0].Color := AStartColor;
  Result.Gradient_colors[0].Position := 0;
  Result.Gradient_colors[1].Color := AEndColor;
  Result.Gradient_colors[1].Position := 1;
end;

function CreateRadialGradientBrush(CX, CY, R, FX, FY: Double;
  AStartColor, AEndColor: TFPColor): TvBrush;
begin
  Result.Kind := bkRadialGradient;
  Result.Gradient_cx := CX;
  Result.Gradient_cy := CY;
  Result.Gradient_r := R;
  Result.Gradient_fx := FX;
  Result.Gradient_fy := FY;
  SetLength(Result.Gradient_colors, 2);
  Result.Gradient_colors[0].Color := AStartColor;
  Result.Gradient_colors[0].Position := 0;
  Result.Gradient_colors[1].Color := AEndColor;
  Result.Gradient_colors[1].Position := 1;
end;


{ Pen }

function CreatePen(AStyle: TFPPenStyle; AWidth: Integer; AColor: TFPColor): TvPen;
begin
  Result.Style := AStyle;
  Result.Width := AWidth;
  Result.Color := AColor;
end;


{ Standardized objects }

{ A circle shifted up }
function CreateStdCircle(APage: TvVectorialPage): TvCircle;
const
  CENTER_X = 50;
  CENTER_Y = 55;  // y points up for this number
  RADIUS = 40;
begin
  if APage.UseTopLeftCoordinates then
    Result := CreateCircle(APage, CENTER_X, PAGE_SIZE - CENTER_Y, RADIUS) else
    Result := CreateCircle(APage, CENTER_X, CENTER_Y, RADIUS);
  Result.Pen := StdPen;
end;

{ An ellipse shifted up }
function CreateStdEllipse(APage: TvVectorialPage): TvEllipse;
begin
  Result := CreateEllipse(APage, 10, 30, 90, 80);
    // CreateEllipse will invert the axis if needed
  Result.Pen := StdPen;
end;

{ A rectangle shifted up }
function CreateStdRect(APage: TvVectorialPage): TvRectangle;
const
  LEFT = 10;
  RIGHT = 90;
  TOP = 95;     // for bottom-up y axis
  BOTTOM = 15;  // dto.
begin
  Result := CreateRectangle(APage, LEFT, TOP, RIGHT, BOTTOM);
    // CreateRect will invert the y axis if needed
  Result.Pen := StdPen;
end;

{ A rounded rectangle shifted up }
function CreateStdRoundedRect(APage: TvVectorialPage): TvRectangle;
const
  LEFT = 10;
  RIGHT = 90;
  TOP = 95;     // for bottom-up y axis
  BOTTOM = 15;  // dto.
  RX = 10;
  RY = 10;
begin
  Result := CreateRoundedRect(APage,LEFT, TOP, RIGHT, BOTTOM, RX, RY);
    // CreateRect will invert the y axis if needed
  Result.Pen := StdPen;
end;

{ A triangle as polygon, base line at bottom }
function CreateStdPolygon(APage: TvVectorialPage):TvPolygon;
var
  pts: array[0..3] of T3DPoint;
  i: Integer;
begin
  pts[0] := Make3DPoint(10, 10);
  pts[1] := Make3dPoint(90, 10);
  pts[2] := Make3DPoint(50, 90);
  pts[3] := pts[0];
  if APage.UseTopLeftCoordinates then
    for i:=0 to High(pts) do
      pts[i].Y := PAGE_SIZE - pts[i].Y;
  Result := CreatePolygon(APage, pts);
  Result.Pen := StdPen;
end;

{ A star-like self-intersecting polygon, tip at bottom }
function CreateStdSelfIntersectingPolygon(APage: TvVectorialPage): TvPolygon;
var
  pts: array[0..5] of T3DPoint;
  i: Integer;
begin
  pts[0] := Make3DPoint(50, 5);
  pts[1] := Make3DPoint(20, 90);
  pts[2] := Make3DPoint(95, 30);
  pts[3] := Make3DPoint(5,  30);
  pts[4] := Make3DPoint(80, 90);
  pts[5] := Make3DPoint(50, 5);
  if APage.UseTopLeftCoordinates then
    for i:=0 to High(pts) do
      pts[i].Y := PAGE_SIZE - pts[i].Y;
  Result := CreatePolygon(APage, pts);
  Result.Pen := StdPen;
end;

function CreatePathWithHole(APage: TvVectorialPage): TPath;
const
  OUTER_POINTS: array[0..4] of T2DPoint = (
    (X:10; Y:5), (X:90; Y:5), (X:90; Y:90), (X:10; Y:90), (X:10; Y:5)
  );
  INNER_POINTS: array[0..4] of T2DPoint = (
    (X:50; Y:45), (X:40; Y:55), (X:50; Y:65), (X:60; Y:55), (X:50; Y:45)
  );
var
  i: Integer;
begin
  if APage.UseTopLeftCoordinates then begin
    APage.StartPath(OUTER_POINTS[0].X, PAGE_SIZE - OUTER_POINTS[0].Y);
    for i:=1 to High(OUTER_POINTS) do
      APage.AddLineToPath(OUTER_POINTS[i].X, PAGE_SIZE - OUTER_POINTS[i].Y);
    APage.AddMoveToPath(INNER_POINTS[0].X, PAGE_SIZE - INNER_POINTS[0].Y);
    for i:=1 to High(INNER_POINTS) do
      APage.AddLineToPath(INNER_POINTS[i].X, PAGE_SIZE - INNER_POINTS[i].Y);
  end else begin
    APage.StartPath(OUTER_POINTS[0].X, OUTER_POINTS[0].Y);
    for i:=1 to High(OUTER_POINTS) do
      APage.AddLineToPath(OUTER_POINTS[i].X, OUTER_POINTS[i].Y);
    APage.AddMoveToPath(INNER_POINTS[0].X, INNER_POINTS[0].Y);
    for i:=1 to High(INNER_POINTS) do
      APage.AddLineToPath(INNER_POINTS[i].X, INNER_POINTS[i].Y);
  end;
  Result := APage.EndPath;
  Result.Pen := StdPen;
end;
  (*
{ Quarter circle in quadrant I }
function CreateStdCircArcQ1(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean): TPath;
const
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX + RX;
  Y1 = CY;
  X2 = CX;
  Y2 = CY + RY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle reaching from quadrant I into quadrant II}
function CreateStdCircArcQ12(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX + RX/SQRT2;
  Y1 = CY + RY/SQRT2;
  X2 = CX - RX/SQRT2;
  Y2 = CY + RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle in quadrant II }
function CreateStdCircArcQ2(APage: TvVectorialPage; Clockwise: Boolean): TPath;
const
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX;
  Y1 = CY + RY;
  X2 = CX - RX;
  Y2 = CY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle reaching from quadrant II into quadrant III}
function CreateStdCircArcQ23(APage: TvVectorialPage; Clockwise: Boolean): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX - RX/SQRT2;
  Y1 = CY + RY/SQRT2;
  X2 = CX - RX/SQRT2;
  Y2 = CY - RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle in quadrant III }
function CreateStdCircArcQ3(APage: TvVectorialPage; Clockwise: Boolean): TPath;
const
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX - RX;
  Y1 = CY;
  X2 = CX;
  Y2 = CY - RY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle reaching from quadrant III into quadrant IV}
function CreateStdCircArcQ34(APage: TvVectorialPage; Clockwise: Boolean): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX - RX/SQRT2;
  Y1 = CY - RY/SQRT2;
  X2 = CX + RX/SQRT2;
  Y2 = CY - RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle in quadrant IV }
function CreateStdCircArcQ4(APage: TvVectorialPage; Clockwise: Boolean): TPath;
const
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX;
  Y1 = CY - RY;
  X2 = CX + RX;
  Y2 = CY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

{ Quarter circle reaching from quadrant IV into quadrant I}
function CreateStdCircArcQ41(APage: TvVectorialPage; Clockwise: Boolean): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 30.0;
  X1 = CX + RX/SQRT2;
  Y1 = CY - RY/SQRT2;
  X2 = CX + RX/SQRT2;
  Y2 = CY + RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, 0, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, 0, Clockwise);
end;

function CreateStdEllArcQ1(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX + RX;
  Y1 = CY;
  X2 = CX;
  Y2 = CY + RY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ12(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX + RX/SQRT2;
  Y1 = CY + RY/SQRT2;
  X2 = CX - RX/SQRT2;
  Y2 = CY + RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ2(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX;
  Y1 = CY + RY;
  X2 = CX - RX;
  Y2 = CY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ23(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX - RX/SQRT2;
  Y1 = CY + RY/SQRT2;
  X2 = CX - RX/SQRT2;
  Y2 = CY - RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ3(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX - RX;
  Y1 = CY;
  X2 = CX;
  Y2 = CY - RY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ34(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX - RX/SQRT2;
  Y1 = CY - RY/SQRT2;
  X2 = CX + RX/SQRT2;
  Y2 = CY - RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ4(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX;
  Y1 = CY - RY;
  X2 = CX + RX;
  Y2 = CY;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;

function CreateStdEllArcQ41(APage: TvVectorialPage;
  Clockwise, Reverse: Boolean; Angle: Double): TPath;
const
  SQRT2 = 1.4142135623731;
  CX = 50.0;
  CY = 55.0;
  RX = 30.0;
  RY = 20.0;
  X1 = CX + RX/SQRT2;
  Y1 = CY - RY/SQRT2;
  X2 = CX + RX/SQRT2;
  Y2 = CY + RY/SQRT2;
begin
  if Reverse then
    Result := CreateArc(APage, X2, Y2, X1, Y1, CX, CY, RX, RY, Angle, Clockwise)
  else
    Result := CreateArc(APage, X1, Y1, X2, Y2, CX, CY, RX, RY, Angle, Clockwise);
end;
    *)

{ ---- }

function StdSolidBrush: TvBrush;
begin
  Result := CreateSimpleBrush(bsSolid, colRed);
end;

function StdHorizGradientBrush: TvBrush;
begin
  Result := CreateLinearGradientBrush(Point2D(0, 0), Point2D(1, 0),
    [gfRelStartX, gfRelEndX, gfRelStartY, gfRelEndY],
    colBlue, colWhite);
end;

{ A vertical gradient, yellow at top, red at bottom }
function StdVertGradientBrush: TvBrush;
var
  P1, P2: T2DPoint;
begin
  {if APage.UseTopLeftCoordinates then begin
    P1 := Point2D(0, 1);
    P2 := Point2D(0, 0);
  end else
  }
  begin
    P1 := Point2D(0, 0);
    P2 := Point2D(0, 1);
  end;
  Result := CreateLinearGradientBrush(P1, P2,
    [gfRelStartX, gfRelEndX, gfRelStartY, gfRelEndY],
    colYellow, colRed);
end;

{ A diagonal gradient running from bottom/left (yellow) to top/right (red) }
function StdLinearGradientBrush: TvBrush;
var
  P1, P2: T2DPoint;
begin
  {
  if APage.UseTopLeftCoordinates then begin
    P1 := Point2D(0, 1);
    P2 := Point2D(1, 0);
  end else
  }
  begin
    P1 := Point2D(0, 0);
    P2 := Point2D(1, 1);
  end;
  Result := CreateLinearGradientBrush(Point2D(0, 0), Point2D(1, 1),
    [gfRelStartX, gfRelEndX, gfRelStartY, gfRelEndY],
    colYellow, colRed);
end;

function StdRadialGradientBrush: TvBrush;
begin
  Result := CreateRadialGradientBrush(0.5, 0.5, 0.5, 0.5, 0.5,
    colRed, colYellow);
end;

function StdPen: TvPen;
begin
  Result := CreatePen(psSolid, 4, colBlack);
end;

end.

