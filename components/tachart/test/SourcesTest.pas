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

unit SourcesTest;

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry,
  TAChartUtils, TACustomSource, TAIntervalSources, TASources;

type

  { TListSourceTest }

  TListSourceTest = class(TTestCase)
  private
    FSource: TListChartSource;

    procedure AssertItemEquals(
      const AItem: TChartDataItem; AX, AY: Double; AText: String = '';
      AColor: TChartColor = clTAColor);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Basic;
    procedure DataPoint;
    procedure DataPointSeparator;
    procedure Enum;
    procedure Extent;
    procedure Multi;
  end;

  { TRandomSourceTest }

  TRandomSourceTest = class(TTestCase)
  published
    procedure Extent;
  end;

  { TCalculatedSourceTest }

  TCalculatedSourceTest = class(TTestCase)
  private
    FOrigin: TListChartSource;
    FSource: TCalculatedChartSource;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Accumulate;
    procedure Derivative;
    procedure Percentage;
    procedure Reorder;
  end;

  TIntervalSourceTest = class(TTestCase)
  private
    procedure AssertValueEquals(
      const AExpected: array of Double; const AActual: TChartValueTextArray);
  published
    procedure IntervalSource;
    procedure ListSource;
  end;

implementation

uses
  Math, TAMath;

type
  TDummyTransform = object
  public
    function IdentityDouble(AX: Double): Double;
    function IdentityInteger(AX: Integer): Integer;
    function PrepareValuesInRangeParams: TValuesInRangeParams;
    function Round(AX: Double): Integer;
  end;

var
  VDummyTransform: TDummyTransform;

{ TDummyTransform }

function TDummyTransform.IdentityDouble(AX: Double): Double;
begin
  Result := AX;
end;

function TDummyTransform.IdentityInteger(AX: Integer): Integer;
begin
  Result := AX;
end;

function TDummyTransform.PrepareValuesInRangeParams: TValuesInRangeParams;
begin
  with Result do begin
    FAxisToGraph := @IdentityDouble;
    FGraphToAxis := @IdentityDouble;
    FFormat := '';
    FGraphToImage := @Round;
    FMin := 30;
    FMax := 69;
    FMinStep := 0;
    FScale := @IdentityInteger;
    FUseY := false;
    FIntervals := TChartAxisIntervalParams.Create(nil);
  end;
end;

function TDummyTransform.Round(AX: Double): Integer;
begin
  Result := System.Round(AX);
end;

{ TCalculatedSourceTest }

procedure TCalculatedSourceTest.Accumulate;
var
  i, j: Integer;
  rng: TMWCRandomGenerator;
begin
  FSource.AccumulationMethod := camSum;
  FSource.AccumulationRange := 2;
  AssertEquals(3, FSource.YCount);
  AssertEquals(1, FSource[0]^.X);
  AssertEquals(102, FSource[0]^.Y);
  AssertEquals(2, FSource[1]^.X);
  AssertEquals(102 + 202, FSource[1]^.Y);
  AssertEquals(202 + 302, FSource[2]^.Y);
  FSource.AccumulationDirection := cadForward;
  AssertEquals(202 + 302, FSource[1]^.Y);
  AssertEquals(302 + 402, FSource[2]^.Y);
  FSource.AccumulationDirection := cadBackward;
  FSource.AccumulationMethod := camAverage;
  AssertEquals((2002 + 2102) / 2, FSource[20]^.Y);
  AssertEquals(1, FSource[0]^.X);
  AssertEquals(102, FSource[0]^.Y);
  AssertEquals((102 + 202) / 2, FSource[1]^.Y);
  AssertEquals(102, FSource[0]^.Y);
  FSource.AccumulationDirection := cadCenter;
  AssertEquals((1102 + 1202 + 1302) / 3, FSource[11]^.Y);
  FSource.AccumulationDirection := cadBackward;

  FSource.AccumulationRange := 5;
  rng := TMWCRandomGenerator.Create;
  try
    rng.Seed := 89237634;
    for i := 1 to 100 do begin
      j := rng.GetInRange(5, FSource.Count - 1);
      AssertEquals(IntToStr(j), (j - 1) * 100 + 2, FSource[j]^.Y);
    end;
    FSource.AccumulationRange := 0;
    FSource.AccumulationMethod := camSum;
    rng.Seed := 23784538;
    for i := 1 to 20 do begin
      j := rng.GetInRange(0, FSource.Count - 1);
      AssertEquals(
        IntToStr(j), (j + 1) * (j + 2) * 50 + (j + 1) * 3, FSource[j]^.YList[0]);
    end;
  finally
    rng.Free;
  end;
end;

procedure TCalculatedSourceTest.Derivative;
begin
  FSource.AccumulationMethod := camDerivative;
  FSource.AccumulationRange := 2;
  FOrigin.SetYValue(1, 202);
  AssertTrue(IsNan(FSource[0]^.Y));
  AssertEquals(100, FSource[1]^.Y);
  FSource.AccumulationDirection := cadCenter;
  AssertEquals(100, FSource[0]^.Y);
end;

procedure TCalculatedSourceTest.Percentage;
begin
  FSource.Percentage := true;
  AssertEquals(3, FSource.YCount);
  AssertEquals(102 / (102 + 103 + 104) * 100, FSource[0]^.Y);
  AssertEquals(103 / (102 + 103 + 104) * 100, FSource[0]^.YList[0]);
end;

procedure TCalculatedSourceTest.Reorder;
var
  i, j: Integer;
begin
  AssertEquals(3, FSource.YCount);
  FSource.ReorderYList := '2';
  AssertEquals(2, FSource.YCount);
  AssertEquals(104, FSource[0]^.YList[0]);
  AssertEquals(204, FSource[1]^.YList[0]);
  FSource.ReorderYList := '0,0,0';
  AssertEquals(4, FSource.YCount);
  AssertEquals(103, FSource[0]^.YList[0]);
  AssertEquals(103, FSource[0]^.YList[1]);
  AssertEquals(103, FSource[0]^.YList[2]);
  FSource.ReorderYList := '';
  for i := 0 to FSource.Count - 1 do begin
    AssertEquals(FOrigin[i]^.Y, FSource[i]^.Y);
    for j := 0 to FSource.YCount - 2 do
      AssertEquals(FOrigin[i]^.YList[j], FSource[i]^.YList[j]);
  end;
end;

procedure TCalculatedSourceTest.SetUp;
var
  i: Integer;
begin
  inherited SetUp;
  FOrigin := TListChartSource.Create(nil);
  FSource := TCalculatedChartSource.Create(nil);
  FSource.Origin := FOrigin;
  FOrigin.YCount := 3;
  for i := 1 to 100 do
    FOrigin.SetYList(FOrigin.Add(i, i * 100 + 2), [i * 100 + 3, i * 100 + 4]);
end;

procedure TCalculatedSourceTest.TearDown;
begin
  FreeAndNil(FSource);
  FreeAndNil(FOrigin);
  inherited TearDown;
end;

{ TListSourceTest }

procedure TListSourceTest.AssertItemEquals(
  const AItem: TChartDataItem; AX, AY: Double; AText: String;
  AColor: TChartColor);
begin
  AssertEquals('X', AX, AItem.X);
  AssertEquals('Y', AY, AItem.Y);
  AssertEquals('Text', AText, AItem.Text);
  AssertEquals('Color', AColor, AItem.Color);
end;

procedure TListSourceTest.Basic;
var
  i: Integer;
  srcDest: TListChartSource;
begin
  FSource.Clear;
  AssertEquals(0, FSource.Count);
  AssertEquals(0, FSource.Add(1, 2, 'text', $FFFFFF));
  AssertEquals(1, FSource.Count);
  FSource.Delete(0);
  AssertEquals(0, FSource.Count);
  for i := 1 to 10 do
    FSource.Add(i, i * 2, IntToStr(i));
  srcDest := TListChartSource.Create(nil);
  try
    srcDest.CopyFrom(FSource);
    AssertEquals(FSource.Count, srcDest.Count);
    for i := 0 to FSource.Count - 1 do
      with FSource[i]^ do
        AssertItemEquals(srcDest[i]^, X, Y, Text, Color);
  finally
    srcDest.Free;
  end;
end;

procedure TListSourceTest.DataPoint;
begin
  FSource.Clear;
  FSource.DataPoints.Add('3|4|?|text1');
  FSource.DataPoints.Add('5|6|$FF0000|');
  AssertEquals(2, FSource.Count);
  AssertItemEquals(FSource[0]^, 3, 4, 'text1');
  AssertItemEquals(FSource[1]^, 5, 6, '', $FF0000);
  FSource[0]^.Color := 0;
  AssertEquals('3|4|$000000|text1', FSource.DataPoints[0]);
  FSource.DataPoints.Add('7|8|0|two words');
  AssertEquals('two words', FSource[2]^.Text);
end;

procedure TListSourceTest.DataPointSeparator;
var
  oldSeparator: Char;
begin
  FSource.Clear;
  oldSeparator := DefaultFormatSettings.DecimalSeparator;
  try
    DefaultFormatSettings.DecimalSeparator := ':';
    FSource.DataPoints.Add('3:5');
    AssertEquals(3.5, FSource[0]^.X);
    FSource.DataPoints[0] := '4.5';
    AssertEquals(4.5, FSource[0]^.X);
  finally
    DefaultFormatSettings.DecimalSeparator := oldSeparator;
  end;
end;

procedure TListSourceTest.Enum;
var
  it: PChartDataItem;
  s: Double = 0;
begin
  FSource.Clear;
  for it in FSource do
    s += 1;
  AssertEquals(0, s);
  FSource.Add(10, 1);
  FSource.Add(20, 7);
  for it in FSource do
    s += it^.X + it^.Y;
  AssertEquals(38, s);
end;

procedure TListSourceTest.Extent;

  procedure AssertExtent(AX1, AY1, AX2, AY2: Double);
  begin
    with FSource.Extent do begin
      AssertEquals('X1', AX1, a.X);
      AssertEquals('Y1', AY1, a.Y);
      AssertEquals('X2', AX2, b.X);
      AssertEquals('Y2', AY2, b.Y);
    end;
  end;

begin
  FSource.Clear;
  Assert(IsInfinite(FSource.Extent.a.X) and IsInfinite(FSource.Extent.a.Y));
  Assert(IsInfinite(FSource.Extent.b.X) and IsInfinite(FSource.Extent.b.Y));

  FSource.Add(1, 2);
  AssertExtent(1, 2, 1, 2);

  FSource.Add(3, 4);
  AssertExtent(1, 2, 3, 4);

  FSource.SetXValue(0, -1);
  AssertExtent(-1, 2, 3, 4);

  FSource.SetXValue(1, -2);
  AssertExtent(-2, 2, -1, 4);

  FSource.SetXValue(1, SafeNaN);
  AssertExtent(-1, 2, -1, 4);
  FSource.SetXValue(1, -2);

  FSource.SetYValue(0, 5);
  AssertExtent(-2, 4, -1, 5);

  FSource.SetYValue(0, 4.5);
  AssertExtent(-2, 4, -1, 4.5);

  FSource.SetYValue(1, SafeNaN);
  AssertExtent(-2, 4.5, -1, 4.5);

  FSource.Delete(1);
  AssertExtent(-1, 4.5, -1, 4.5);

  FSource.Clear;
  FSource.Add(1, 1);
  FSource.Add(2, 2);
  FSource.Add(3, 3);
  FSource.Add(4, 4);
  FSource.Delete(0);
  FSource.Delete(1);
  AssertExtent(2, 2, 4, 4);
end;

procedure TListSourceTest.Multi;
begin
  FSource.Clear;
  AssertEquals(1, FSource.YCount);
  FSource.Add(1, 2);
  FSource.YCount := 2;
  AssertEquals(1, Length(FSource[0]^.YList));
  AssertEquals(0, FSource[0]^.YList[0]);
  FSource.SetYList(0, [3, 4]);
  AssertEquals(3, FSource[0]^.YList[0]);
  FSource.DataPoints.Add('1|2|3|4|?|t');
  AssertEquals(3, FSource.YCount);
  AssertEquals(4, FSource[1]^.YList[1]);
end;

procedure TListSourceTest.SetUp;
begin
  inherited SetUp;
  FSource := TListChartSource.Create(nil);
end;

procedure TListSourceTest.TearDown;
begin
  FreeAndNil(FSource);
  inherited TearDown;
end;

{ TRandomSourceTest }

procedure TRandomSourceTest.Extent;
var
  s: TRandomChartSource;
  ext: TDoubleRect;
begin
  s := TRandomChartSource.Create(nil);
  try
    s.XMin := 10;
    s.XMax := 20;
    s.YMin := 5;
    s.YMax := 6;
    s.PointsNumber := 1000;
    ext := s.Extent;
    AssertEquals(10, ext.a.X);
    AssertEquals(20, ext.b.X);
    Assert(ext.a.Y > 5);
    Assert(ext.b.Y < 6);
    Assert(ext.a.Y < ext.b.Y);
  finally
    s.Free;
  end;
end;

{ TIntervalSourceTest }

procedure TIntervalSourceTest.AssertValueEquals(
  const AExpected: array of Double; const AActual: TChartValueTextArray);
var
  i: Integer;
begin
  AssertEquals(Length(AExpected), Length(AActual));
  for i := 0 to High(AExpected) do
    AssertEquals(AExpected[i], AActual[i].FValue);
end;

procedure TIntervalSourceTest.IntervalSource;
var
  p: TValuesInRangeParams;
  src: TIntervalChartSource;
  r: TChartValueTextArray = nil;
begin
  p := VDummyTransform.PrepareValuesInRangeParams;
  src := TIntervalChartSource.Create(nil);
  try
    src.Params.MaxLength := 15;
    src.ValuesInRange(p, r);
    AssertValueEquals([20, 30, 40, 50, 60, 70], r);
    src.Params.Options := [aipUseCount];
    src.Params.Count := 7;
    src.Params.Tolerance := 1;
    src.ValuesInRange(p, r);
    AssertValueEquals([24, 30, 36, 41, 47, 52, 58, 63, 69, 75], r);
  finally
    p.FIntervals.Free;
    src.Free;
  end;
end;

procedure TIntervalSourceTest.ListSource;
var
  i: Integer;
  p: TValuesInRangeParams;
  r: TChartValueTextArray = nil;
  src: TListChartSource;

  procedure Check(const AExpected: array of Double);
  begin
    r := nil;
    src.ValuesInRange(p, r);
    AssertValueEquals(AExpected, r);
  end;

begin
  p := VDummyTransform.PrepareValuesInRangeParams;
  src := TListChartSource.Create(nil);
  for i := 1 to 10 do
    src.Add(10 * i, i);
  try
    Check([20, 30, 40, 50, 60, 70]);
    p.FIntervals.MinLength := 20;
    Check([20, 30, 50, 70]);
    p.FMin := 81;
    p.FMax := 82;
    Check([80, 90]);
    p.FMin := 9;
    p.FMax := 11;
    Check([10, 20]);
    src.Add(8, 11);
    Check([8, 10, 20]);
    p.FMin := 1;
    p.FMax := 20;
    p.FIntervals.Options := p.FIntervals.Options - [aipUseMinLength];
    Check([8, 10, 20, 30]);
    p.FIntervals.Options := p.FIntervals.Options + [aipUseMinLength];
    p.FMax := 50;
    Check([8, 30, 50, 60]);
  finally
    p.FIntervals.Free;
    src.Free;
  end;
end;

initialization

  RegisterTests([
    TListSourceTest, TRandomSourceTest, TCalculatedSourceTest,
    TIntervalSourceTest]);

end.

