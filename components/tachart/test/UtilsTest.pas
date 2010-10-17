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

unit UtilsTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, TAChartUtils;

type

  { TIntervalListTest }

  TIntervalListTest = class(TTestCase)
  private
    FIList: TIntervalList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Basic;
    procedure Intersect;
    procedure Merge;
  end;


  { TGeometryTest }

  TGeometryTest = class(TTestCase)
  private
    procedure AssertEquals(const Expected, Actual: TDoublePoint); overload;
    procedure AssertEquals(const Expected, Actual: TPoint); overload;
  published
    procedure TestLineIntersectsLine;
    procedure TestLineIntersectsRect;
    procedure TestPointOnLine;
    procedure TestPointOperations;
    procedure TestPointInPolygon;
    procedure TestPolygonIntersectsPolygon;
  end;

  { TColorTest }

  TColorTest = class(TTestCase)
  private
    procedure AssertEqualsHex(Expected, Actual: Integer); overload;
  published
    procedure TestInterpolate;
  end;

implementation

{ TIntervalListTest }

procedure TIntervalListTest.Basic;
begin
  AssertEquals(0, FIList.IntervalCount);
  FIList.AddRange(1.0, 2.0);
  AssertEquals(1, FIList.IntervalCount);
  FIList.AddPoint(3.0);
  AssertEquals(2, FIList.IntervalCount);
  AssertEquals(3.0, FIList.Interval[1].FEnd);
  FIList.Clear;
  AssertEquals(0, FIList.IntervalCount);
end;

procedure TIntervalListTest.Intersect;
var
  l, r: Double;
  hint: Integer = 0;
begin
  FIList.Clear;
  FIList.AddRange(1.0, 2.0);
  l := 5.0;
  r := 6.0;
  AssertFalse(FIList.Intersect(l, r, hint));
  l := 1.5;
  r := 6.0;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(2.0, r);
  FIList.Epsilon := 0.1;
  l := 0.5;
  r := 2.5;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(0.9, l);
  AssertEquals(2.1, r);
end;

procedure TIntervalListTest.Merge;
begin
  FIList.Clear;
  FIList.AddRange(1.0, 2.0);
  FIList.AddRange(3.0, 4.0);
  AssertEquals(2, FIList.IntervalCount);
  FIList.AddRange(1.5, 2.5);
  AssertEquals(2, FIList.IntervalCount);
  AssertEquals(2.5, FIList.Interval[0].FEnd);
  FIList.AddRange(3.5, 3.6);
  AssertEquals(2, FIList.IntervalCount);
  FIList.AddRange(2.5, 3.0);
  AssertEquals(1, FIList.IntervalCount);
  FIList.AddPoint(4.0);
  AssertEquals(1, FIList.IntervalCount);
  FIList.AddPoint(4.1);
  AssertEquals(2, FIList.IntervalCount);
end;

procedure TIntervalListTest.SetUp;
begin
  inherited SetUp;
  FIList := TIntervalList.Create;
end;

procedure TIntervalListTest.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FIList);
end;

{ TGeometryTest }

procedure TGeometryTest.AssertEquals(const Expected, Actual: TDoublePoint);
begin
  AssertEquals(Expected.X, Actual.X);
  AssertEquals(Expected.Y, Actual.Y);
end;

procedure TGeometryTest.AssertEquals(const Expected, Actual: TPoint);
begin
  AssertEquals(Expected.X, Actual.X);
  AssertEquals(Expected.Y, Actual.Y);
end;

procedure TGeometryTest.TestLineIntersectsLine;
var
  p1, p2: TPoint;
begin
  p1 := Point(0, 0);
  p2 := Point(1, 1);
  AssertTrue(IsLineIntersectsLine(Point(1, 0), Point(0, 1), p1, p2));
  AssertTrue(IsLineIntersectsLine(Point(1, 0), Point(0, 0), p1, p2));
  AssertTrue(IsLineIntersectsLine(Point(1, 1), Point(2, 2), p1, p2));
  AssertFalse(IsLineIntersectsLine(Point(2, 2), Point(3, 3), p1, p2));
  AssertTrue(IsLineIntersectsLine(Point(2, 0), Point(0, 2), p1, p2));
  AssertFalse(IsLineIntersectsLine(Point(3, 0), Point(0, 3), p1, p2));
  p2 := Point(1, 0);
  AssertTrue(IsLineIntersectsLine(Point(0, 0), Point(2, 0), p1, p2));
  AssertFalse(IsLineIntersectsLine(Point(0, 1), Point(1, 1), p1, p2));
end;

procedure TGeometryTest.TestLineIntersectsRect;
var
  r: TDoubleRect = (a: (X: 0; Y: 0); b: (X: 20; Y: 10));

  procedure Check(AP1, AP2, AR1, AR2: TDoublePoint);
  begin
    AssertTrue(LineIntersectsRect(AP1, AP2, r));
    AssertEquals(AR1, AP1);
    AssertEquals(AR2, AP2);
  end;

var
  p1, p2: TDoublePoint;
begin
  p1 := DoublePoint(-1, -1);
  p2 := DoublePoint(0, 20);
  AssertFalse(LineIntersectsRect(p1, p2, r));

  p1 := DoublePoint(100, 20);
  AssertFalse(LineIntersectsRect(p1, p2, r));

  p1 := DoublePoint(-1, -1);
  p2 := DoublePoint(1, 1);
  Check(p1, p2, DoublePoint(0, 0), p2);

  p1 := DoublePoint(0, 0);
  Check(p1, p2, p1, p2);

  p1 := DoublePoint(20, 20);
  p2 := DoublePoint(20, -10);
  Check(p1, p2, DoublePoint(20, 10), DoublePoint(20, 0));

  p1 := DoublePoint(10, 20);
  p2 := DoublePoint(15, -10);
  Check(p1, p2, DoublePoint(11.6667, 10), DoublePoint(13.3333, 0));
end;

procedure TGeometryTest.TestPointInPolygon;
var
  p: TPoint;
  r: array [1..4] of TPoint =
    ((X: 0; Y: 0), (X: 10; Y: 0), (X: 10; Y: 5), (X: 0; Y: 5));
begin
  p := Point(1, 1);
  AssertFalse(IsPointInPolygon(p, []));

  AssertTrue(IsPointInPolygon(p, [Point(0, 0), Point(0, 2), Point(3, 0)]));
  AssertTrue(IsPointInPolygon(p, [Point(0, 0), Point(0, 2), Point(3, 1)]));
  AssertTrue(IsPointInPolygon(p, [Point(0, 0), Point(0, 2), Point(1, 1)]));
  AssertFalse(IsPointInPolygon(p, [Point(2, 0), Point(2, 2), Point(3, 1)]));
  AssertFalse(IsPointInPolygon(p, [Point(2, 0), Point(1, 2), Point(0, 10)]));

  AssertTrue(IsPointInPolygon(Point(5, 5), r));
  AssertTrue(IsPointInPolygon(Point(10, 5), r));
  AssertFalse(IsPointInPolygon(Point(11, 5), r));
  AssertFalse(IsPointInPolygon(Point(0, -1), r));
end;

procedure TGeometryTest.TestPointOnLine;
begin
  AssertTrue(IsPointOnLine(Point(0, 0), Point(-1, -1), Point(1, 1)));
  AssertFalse(IsPointOnLine(Point(1, 0), Point(-1, -1), Point(1, 1)));

  AssertTrue(IsPointOnLine(Point(0, 0), Point(0, -1), Point(0, 1)));
  AssertFalse(IsPointOnLine(Point(-1, 0), Point(0, -1), Point(0, 1)));

  AssertTrue(IsPointOnLine(Point(0, 0), Point(-1, 0), Point(1, 0)));
  AssertFalse(IsPointOnLine(Point(0, 1), Point(-1, 0), Point(1, 0)));
end;

procedure TGeometryTest.TestPointOperations;
begin
  AssertEquals(Point(1, 0), RotatePoint(Point(1, 0), 0.0));
  AssertEquals(Point(0, 1), RotatePoint(Point(1, 0), Pi / 2));
  AssertEquals(Point(14, 0), RotatePoint(Point(10, 10), -Pi / 4));
end;

procedure TGeometryTest.TestPolygonIntersectsPolygon;

  function OffsetPolygon(AP: array of TPoint; AOffset: TPoint): TPointArray;
  var
    i: Integer;
  begin
    SetLength(Result, Length(AP));
    for i := 0 to High(AP) do
      Result[i] := AP[i] + AOffset;
  end;

var
  p1: array [1..4] of TPoint =
    ((X: 0; Y: 0), (X: 10; Y: 0), (X: 10; Y: 5), (X: 0; Y: 5));
begin
  AssertTrue(IsPolygonIntersectsPolygon(p1, OffsetPolygon(p1, Point(0, 0))));
  AssertTrue(IsPolygonIntersectsPolygon(p1, OffsetPolygon(p1, Point(1, 1))));
  AssertTrue(IsPolygonIntersectsPolygon(p1, OffsetPolygon(p1, Point(5, 0))));
  AssertTrue(IsPolygonIntersectsPolygon(p1, OffsetPolygon(p1, Point(10, 0))));
  AssertFalse(IsPolygonIntersectsPolygon(p1, OffsetPolygon(p1, Point(11, 0))));
  AssertFalse(IsPolygonIntersectsPolygon(p1, OffsetPolygon(p1, Point(0, -6))));
end;

{ TColorTest }

procedure TColorTest.AssertEqualsHex(Expected, Actual: Integer);
begin
  AssertTrue(
    ComparisonMsg(IntToHex(Expected, 8), IntToHex(Actual, 8)),
    Expected = Actual);
end;

procedure TColorTest.TestInterpolate;
begin
  AssertEqualsHex($01020304, InterpolateRGB($01020304, $00787980, 0.0));
  AssertEqualsHex($00787980, InterpolateRGB($01020304, $00787980, 1.0));
  AssertEqualsHex($003D3E42, InterpolateRGB($01020304, $00787980, 0.5));
  AssertEqualsHex($01010102, InterpolateRGB($01010100, $02020214, 0.1));
end;

initialization

  RegisterTests([TIntervalListTest, TGeometryTest, TColorTest]);

end.

