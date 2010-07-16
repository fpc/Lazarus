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

initialization

  RegisterTests([TIntervalListTest]);

end.

