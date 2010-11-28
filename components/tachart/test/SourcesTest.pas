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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCUnit, TestRegistry, TASources;

type

  { TListSourceTest }

  TListSourceTest = class(TTestCase)
  private
    FSource: TListChartSource;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Basic;
    procedure DataPoint;
    procedure Extent;
  end;

  { TRandomSourceTest }

  TRandomSourceTest = class(TTestCase)
  published
    procedure Extent;
  end;

implementation

uses
  Math, TAChartUtils;

{ TListSourceTest }

procedure TListSourceTest.Basic;
begin
  FSource.Clear;
  AssertEquals(0, FSource.Count);
  AssertEquals(0, FSource.Add(1, 2, 'text', $FFFFFF));
  AssertEquals(1, FSource.Count);
  FSource.Delete(0);
  AssertEquals(0, FSource.Count);
end;

procedure TListSourceTest.DataPoint;
begin
  FSource.Clear;
  FSource.DataPoints.Add('3|4|?|text1');
  FSource.DataPoints.Add('5|6|$FF0000|');
  AssertEquals(2, FSource.Count);
  AssertEquals(3, FSource[0]^.X);
  AssertEquals(4, FSource[0]^.Y);
  AssertEquals('text1', FSource[0]^.Text);
  AssertEquals(clTAColor, FSource[0]^.Color);
  AssertEquals(5, FSource[1]^.X);
  AssertEquals(6, FSource[1]^.Y);
  AssertEquals('', FSource[1]^.Text);
  AssertEquals($FF0000, FSource[1]^.Color);
  FSource[0]^.Color := 0;
  AssertEquals('3|4|$000000|text1', FSource.DataPoints[0]);
  FSource.DataPoints.Add('7|8|0|two words');
  AssertEquals('two words', FSource[2]^.Text);
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

  FSource.Add(1, 2, '', 0);
  AssertExtent(1, 2, 1, 2);

  FSource.Add(3, 4, '', 0);
  AssertExtent(1, 2, 3, 4);

  FSource.SetXValue(0, -1);
  AssertExtent(-1, 2, 3, 4);

  FSource.SetXValue(1, -2);
  AssertExtent(-2, 2, -1, 4);

  FSource.SetYValue(0, 5);
  AssertExtent(-2, 4, -1, 5);

  FSource.SetYValue(0, 4.5);
  AssertExtent(-2, 4, -1, 4.5);
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

initialization

  RegisterTests([TListSourceTest, TRandomSourceTest]);

end.

