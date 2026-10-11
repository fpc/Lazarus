{ Regression tests for LazCanvas clipping state and region value semantics.
  See COPYING.modifiedLGPL.txt for the license and linking exception. }

unit TestLazCanvas;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Types, fpcunit, TestGlobals, FPImage, FPCanvas,
  GraphType, IntfGraphics, LazCanvas, LazRegions;

type
  TLazCanvasTest = class(TTestCase)
  private
    FImage: TLazIntfImage;
    FCanvas: TLazCanvas;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure RectClipState;
    procedure ComplexClipState;
  end;

implementation

procedure TLazCanvasTest.SetUp;
var
  Desc: TRawImageDescription;
begin
  Desc.Init_BPP32_A8R8G8B8_BIO_TTB(8, 8);
  FImage := TLazIntfImage.Create(0, 0);
  FImage.DataDescription := Desc;
  FImage.FillPixels(colBlack);
  FCanvas := TLazCanvas.Create(FImage);
end;

procedure TLazCanvasTest.TearDown;
begin
  FCanvas.Free;
  FImage.Free;
end;

procedure TLazCanvasTest.RectClipState;
var
  State: Integer;
begin
  FCanvas.ClipRect := Rect(4, 4, 7, 7);
  FCanvas.Clipping := True;
  State := FCanvas.SaveState;
  FCanvas.ClipRect := Rect(0, 0, 1, 1);
  FCanvas.RestoreState(State);
  AssertNotNull('restored region', FCanvas.ClipRegion);
  FCanvas.Colors[5, 5] := colRed;
  FCanvas.Colors[1, 1] := colRed;
  AssertEquals('restored inside pixel', $FFFF, FImage.Colors[5, 5].Red);
  AssertEquals('restored outside pixel', 0, FImage.Colors[1, 1].Red);
end;

procedure TLazCanvasTest.ComplexClipState;
var
  Region: TLazRegion;
  Points: GraphType.TPointArray;
  State, I: Integer;
begin
  Region := TLazRegion.Create;
  Region.AddRectangle(Rect(1, 1, 2, 2));
  Region.AddEllipse(4, 1, 7, 4);
  SetLength(Points, 4);
  Points[0] := Point(1, 5); Points[1] := Point(3, 5);
  Points[2] := Point(3, 7); Points[3] := Point(1, 7);
  Region.AddPolygon(Points, rfmOddEven);
  FCanvas.ClipRegion := Region;
  FCanvas.Clipping := True;
  State := FCanvas.SaveState;
  for I := 0 to High(Points) do
    TLazRegionPolygon(Region.Parts[2]).Points[I] := Point(0, 0);
  FCanvas.ClipRect := Rect(0, 0, 1, 1);
  FCanvas.RestoreState(State);
  AssertTrue('restored complex region', FCanvas.ClipRegion is TLazRegion);
  Region := TLazRegion(FCanvas.ClipRegion);
  Region.Assign(Region);
  AssertTrue('rectangle snapshot', Region.IsPointInRegion(1, 1));
  AssertTrue('ellipse snapshot', Region.IsPointInRegion(5, 2));
  AssertTrue('polygon snapshot', Region.IsPointInRegion(2, 6));
  AssertFalse('outside snapshot', Region.IsPointInRegion(7, 7));
end;

initialization
  AddToLCLTestSuite(TLazCanvasTest);

end.
