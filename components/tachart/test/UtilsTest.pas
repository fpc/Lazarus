{
 *****************************************************************************
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,
 *  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit UtilsTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, TAChartUtils;

type

  TIntervalListTest = class(TTestCase)
  strict private
    FIList: TIntervalList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Basic;
    procedure Intersect;
    procedure Merge;
  end;

  TMathTest = class(TTestCase)
  published
    procedure CumulNurmDistrTest;
    procedure TestIsEquivalent;
  end;

  TGeometryTest = class(TTestCase)
  strict private
    procedure AssertEquals(const Expected, Actual: TDoublePoint); overload;
    procedure AssertEquals(const Expected, Actual: TPoint); overload;
    procedure AssertEquals(const Expected, Actual: TRect); overload;
  published
    procedure TestExpandRect;
    procedure TestLineIntersectsLine;
    procedure TestLineIntersectsRect;
    procedure TestPointInPolygon;
    procedure TestPointOnLine;
    procedure TestPointOperations;
    procedure TestPolygonIntersectsPolygon;
  end;

  TColorTest = class(TTestCase)
  strict private
    procedure AssertEqualsHex(Expected, Actual: Integer); overload;
  published
    procedure TestInterpolate;
  end;

  TRTTITest = class(TTestCase)
  published
    procedure TestSetPropDefaults;
  end;

  TPublishedIntegerSetTest = class(TTestCase)
  strict private
    FISet: TPublishedIntegerSet;
  protected
    procedure SetUp; override;
  published
    procedure TestAsBooleans;
    procedure TestAsString;
    procedure TestIsSet;
  end;

  THistoryTest = class(TTestCase)
  published
    procedure TestHistory;
  end;

  TFitTest = class(TTestCase)
  published
    procedure TestFitEquationText;
  end;

implementation

uses
  Math, TAGeometry, TAMath, TAFitUtils, AssertHelpers;

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
  FIList.AddRange(101.0, 102.0);
  l := 100.5;
  r := 102.5;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(100.9, l);
  AssertEquals(102.1, r);
  FIList.Epsilon := -0.1;
  FIList.AddRange(201.0, 202.0);
  l := 200.5;
  r := 202.5;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(201.1, l);
  AssertEquals(201.9, r);
  FIList.Epsilon := 0.1;
  FIList.AddRange(301.0, 302.0, [ioOpenStart]);
  l := 300.5;
  r := 302.5;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(301.0, l);
  AssertEquals(302.1, r);
  FIList.AddRange(401.0, 402.0, [ioOpenEnd]);
  l := 400.5;
  r := 402.5;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(400.9, l);
  AssertEquals(402.0, r);
  FIList.AddRange(501.0, 502.0, [ioOpenStart, ioOpenEnd]);
  l := 500.5;
  r := 502.5;
  AssertTrue(FIList.Intersect(l, r, hint));
  AssertEquals(501.0, l);
  AssertEquals(502.0, r);
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

{ TMathTest }

procedure TMathTest.CumulNurmDistrTest;
const
  INV_PTS: array [1..3] of Double = (-1.5, 0.33, 2.0);
var
  p: Double;
begin
  AssertEquals(0, CumulNormDistr(0));
  AssertEquals(0.84134, CumulNormDistr(1.0));
  for p in INV_PTS do
    AssertEquals(p, InvCumulNormDistr(CumulNormDistr(p)));
end;

procedure TMathTest.TestIsEquivalent;
begin
  AssertTrue(IsEquivalent(1.2345, 1.2345));
  AssertTrue(IsEquivalent(SafeNaN, SafeNaN));
  AssertTrue(IsEquivalent(1e100, 1e100+1));
  AssertFalse(IsEquivalent(1e10, 1e10+1));
  AssertFalse(IsEquivalent(5, SafeNaN));
  AssertFalse(IsEquivalent(SafeNaN, 5));
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

procedure TGeometryTest.AssertEquals(const Expected, Actual: TRect);
begin
  AssertEquals(Expected.TopLeft, Actual.TopLeft);
  AssertEquals(Expected.BottomRight, Actual.BottomRight);
end;

procedure TGeometryTest.TestExpandRect;
var
  r: TRect;
begin
  r := ZeroRect;
  ExpandRect(r, Point(1, 2));
  AssertEquals(Rect(0, 0, 1, 2), r);
  ExpandRect(r, Point(-5, -6));
  AssertEquals(Rect(-5, -6, 1, 2), r);

  r := Rect(100, 100, 0, 0);
  ExpandRect(r, Point(3, 1));
  AssertEquals(Rect(3, 1, 3, 1), r);
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

  p1 := DoublePoint(10, 5);
  p2 := DoublePoint(SafeInfinity, 5);
  Check(p1, p2, p1, DoublePoint(20, 5));
  p2 := DoublePoint(10, NegInfinity);
  Check(p1, p2, p1, DoublePoint(10, 0));
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


type
  TE = (eA, eB, eC);
  TESet = set of TE;

  T1 = class(TPersistent)
  private
    FP1: Integer;
    FP2: Boolean;
    FP3: TESet;
    procedure SetP2(AValue: Boolean);
  public
    constructor Create;
  published
    property P1: Integer read FP1 write FP1 default 5;
    property P2: Boolean read FP2 write SetP2 default true;
    property P3: TESet read FP3 write FP3 default [eC];
  end;

  T2 = class(T1)
  published
    property P1 default 6;
    property P3 default [eA];
  end;

{ T1 }

constructor T1.Create;
begin
  SetPropDefaults(Self, ['P1', 'P2', 'P3']);
end;

procedure T1.SetP2(AValue: Boolean);
begin
  FP2 := AValue;
end;

{ TRTTITest }

procedure TRTTITest.TestSetPropDefaults;
var
  v1: T1;
  v2: T2;
begin
  v1 := T1.Create;
  AssertEquals(5, v1.P1);
  AssertTrue(v1.P2);
  AssertTrue(v1.P3 = [eC]);
  v1.Free;
  v2 := T2.Create;
  AssertEquals(6, v2.P1);
  AssertTrue(v2.P2);
  AssertTrue(v2.P3 = [eA]);
  v2.Free;
end;

{ TPublishedIntegerSetTest }

procedure TPublishedIntegerSetTest.SetUp;
begin
  inherited SetUp;
  FISet.Init;
end;

procedure TPublishedIntegerSetTest.TestAsBooleans;

  procedure AssertBooleans(const AExpected: array of Boolean; ACount: Integer);
  begin
    AssertEquals(AExpected, FISet.AsBooleans(ACount));
  end;

begin
  AssertBooleans([], 0);
  FISet.AllSet := false;
  FISet.IsSet[2] := true;
  AssertBooleans([false, false, true], 3);
  FISet.AllSet := true;
  AssertBooleans([true, true, true, true], 4);
end;

procedure TPublishedIntegerSetTest.TestAsString;
begin
  AssertTrue(FISet.AllSet);
  AssertEquals(PUB_INT_SET_ALL, FISet.AsString);
  FISet.AllSet := false;
  AssertFalse(FISet.AllSet);
  AssertEquals(PUB_INT_SET_EMPTY, FISet.AsString);
  FISet.AsString := '3 ,1,,  2';
  AssertEquals('3,1,2', FISet.AsString);
  FISet.AsString := PUB_INT_SET_ALL;
  AssertTrue(FISet.AllSet);
  FISet.AsString := '+';
  AssertEquals(PUB_INT_SET_EMPTY, FISet.AsString);
  FISet.AsString := '3 ;1;; 2';
  AssertEquals('3,1,2', FISet.AsString);
  FISet.AsString := '3|1||2';
  AssertEquals('3,1,2', FISet.AsString);
end;

procedure TPublishedIntegerSetTest.TestIsSet;
begin
  AssertTrue(FISet.AllSet);
  AssertTrue(FISet.IsSet[100000]);
  FISet.AllSet := false;
  AssertFalse(FISet.IsSet[100000]);
  FISet.IsSet[99] := true;
  AssertEquals('99', FISet.AsString);
  FISet.AsString := '3,5';
  AssertTrue(FISet.IsSet[3]);
  AssertFalse(FISet.IsSet[99]);
  FISet.IsSet[3] := false;
  FISet.IsSet[5] := false;
  AssertEquals(PUB_INT_SET_EMPTY, FISet.AsString);
end;

// Workaround: FPC 2.6 fails if this type is made local to TestHistory.
type
  TCharHistory = specialize THistory<Char>;

{ THistoryTest }

procedure THistoryTest.TestHistory;
var
  h: TCharHistory;

  procedure Check(AMessage, AExpected: String);
  var
    actual: String = '';
    i: Integer;
  begin
    for i := 0 to h.Count - 1 do
      actual += h.Item[i];
    AssertEquals(AMessage, AExpected, actual);
  end;

begin
  h := TCharHistory.Create;
  try
    AssertEquals('Initial capacity', 0, h.Capacity);
    Check('Initial state', '');
    h.Add('a');
    Check('Zero capacity', '');
    h.Capacity := 3;
    h.Add('a');
    h.Add('b');
    Check('Normal', 'ab');
    h.Add('c');
    h.Add('d');
    Check('Overflow', 'bcd');
    h.Capacity := 2;
    Check('Reduce capacity 1', 'cd');
    h.Add('e');
    Check('Reduce capacity 2', 'de');
    AssertEquals('Item[-1]', 'e', h[-1]);
    AssertEquals('Pop', 'e', h.Pop);
    Check('After pop', 'd');
  finally
    FreeAndNil(h);
  end;
end;


{ TFitTest }

procedure TFitTest.TestFitEquationText;
var
  t: IFitEquationText;
begin
  t := TFitEquationText.Create.Equation(fePolynomial).NumFormat('%.1f')
    .DecimalSeparator('.').X('x').Y('y').TextFormat(tfNormal);
  AssertEquals('y = 1.2 + 10.5*x + 2.5*x^2', t.Params([1.2, 10.5, +2.5]));
  AssertEquals('y = 1.2 + 10.5*x - 2.5*x^2', t.Params([1.2, 10.5, -2.5]));
  AssertEquals('y = -1.2 + 10.5*x + 2.5*x^2', t.Params([-1.2, 10.5, +2.5]));
  AssertEquals('y = -1.2 + x + 2.5*x^2', t.Params([-1.2, 1.0, +2.5]));
  AssertEquals('y = -1.2 - x + 2.5*x^2', t.Params([-1.2, -1.0, +2.5]));
  AssertEquals('y = -1.2 + 10.5*x + x^2', t.Params([-1.2, 10.5, +1.0]));
  AssertEquals('y = -1.2 + 10.5*x - x^2', t.Params([-1.2, 10.5, -1.0]));
  AssertEquals('y = 10.5*x - x^2', t.Params([0.0, 10.5, -1.0]));
  AssertEquals('y = -10.5*x - x^2', t.Params([0.0, -10.5, -1.0]));
  AssertEquals('y = x - x^2', t.Params([0.0, 1.0, -1.0]));
  AssertEquals('y = -x - x^2', t.Params([0.0, -1.0, -1.0]));

  t.TextFormat(tfHtml);
  AssertEquals('y = 1.2 + 10.5&middot;x + 2.5&middot;x<sup>2</sup>', t.Params([1.2, 10.5, +2.5]));
  AssertEquals('y = 1.2 + 10.5&middot;x - 2.5&middot;x<sup>2</sup>', t.Params([1.2, 10.5, -2.5]));
  AssertEquals('y = -1.2 + 10.5&middot;x + 2.5&middot;x<sup>2</sup>', t.Params([-1.2, 10.5, +2.5]));
  AssertEquals('y = 10.5&middot;x - x<sup>2</sup>', t.Params([0.0, 10.5, -1.0]));

  t.Equation(feExp).TextFormat(tfNormal);
  AssertEquals('y = 1.2*exp(-3.4*x)', t.Params([1.2, -3.4]));
  AssertEquals('y = -1.2*exp(3.4*x)', t.Params([-1.2, 3.4]));
  AssertEquals('y = 1.2*exp(x)', t.Params([1.2, 1.0]));
  AssertEquals('y = 1.2*exp(-x)', t.Params([1.2, -1.0]));
  AssertEquals('y = exp(x)', t.Params([1.0, 1.0]));
  AssertEquals('y = -exp(x)', t.Params([-1.0, 1.0]));
  t.TextFormat(tfHtml);
  AssertEquals('y = 1.2&middot;e<sup>-3.4&middot;x</sup>', t.Params([1.2, -3.4]));
  AssertEquals('y = -1.2&middot;e<sup>3.4&middot;x</sup>', t.Params([-1.2, 3.4]));
  AssertEquals('y = e<sup>x</sup>', t.Params([1.0, 1.0]));

  t.Equation(fePower).Textformat(tfNormal);
  AssertEquals('y = 1.2*x^3.4', t.Params([1.2, 3.4]));
  AssertEquals('y = 1.2*x^-3.4', t.Params([1.2, -3.4]));
  AssertEquals('y = -1.2*x^3.4', t.Params([-1.2, 3.4]));
  AssertEquals('y = -1.2*x^-3.4', t.Params([-1.2, -3.4]));
  AssertEquals('y = x^3.4', t.Params([1.0, 3.4]));
  AssertEquals('y = -x^3.4', t.Params([-1.0, 3.4]));
  t.TextFormat(tfHtml);
  AssertEquals('y = 1.2&middot;x<sup>3.4</sup>', t.Params([1.2, 3.4]));
  AssertEquals('y = 1.2&middot;x<sup>-3.4</sup>', t.Params([1.2, -3.4]));
  AssertEquals('y = -1.2&middot;x<sup>3.4</sup>', t.Params([-1.2, 3.4]));
  AssertEquals('y = -1.2&middot;x<sup>-3.4</sup>', t.Params([-1.2, -3.4]));
  AssertEquals('y = -x<sup>3.4</sup>', t.Params([-1.0, 3.4]));

  t.Equation(feCustom).BasisFuncs(['sin(x)', 'sin(2x)', '']).TextFormat(tfNormal);
  AssertEquals('y = 1.2*sin(x) + 3.4*sin(2x) + 10.0', t.Params([1.2, 3.4, 10]));
  AssertEquals('y = -1.2*sin(x) + 3.4*sin(2x) + 10.0', t.Params([-1.2, 3.4, 10]));
  AssertEquals('y = 1.2*sin(x) + 3.4*sin(2x) - 10.0', t.Params([1.2, 3.4, -10]));
  AssertEquals('y = 1.2*sin(x) + 3.4*sin(2x)', t.Params([1.2, 3.4, 0.0]));
  t.Equation(feCustom).BasisFuncs(['', 'sin(x)', 'sin(2x)']).TextFormat(tfNormal);
  AssertEquals('y = 10.0 + 1.2*sin(x) + 3.4*sin(2x)', t.Params([10, 1.2, 3.4]));
  AssertEquals('y = -10.0 + 1.2*sin(x) + 3.4*sin(2x)', t.Params([-10, 1.2, 3.4]));
  AssertEquals('y = 1.2*sin(x) + 3.4*sin(2x)', t.Params([0.0, 1.2, 3.4]));
  AssertEquals('y = -1.2*sin(x) + 3.4*sin(2x)', t.Params([0.0, -1.2, 3.4]));
  AssertEquals('y = 10.0 + sin(x) + 3.4*sin(2x)', t.Params([10, 1.0, 3.4]));
  AssertEquals('y = 10.0 - sin(x) + 3.4*sin(2x)', t.Params([10, -1.0, 3.4]));
end;


initialization

  RegisterTests([
    TIntervalListTest, TMathTest, TGeometryTest, TColorTest, TRTTITest,
    TPublishedIntegerSetTest, THistoryTest, TFitTest]);

end.

