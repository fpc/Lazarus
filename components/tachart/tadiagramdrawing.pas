{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TADiagramDrawing;

{$H+}

interface

uses
  FPCanvas,
  TADrawUtils, TADiagram;

type
  TDiaContextDrawer = class(TDiaContext)
  private
    FDrawer: IChartDrawer;
  public
    property Drawer: IChartDrawer read FDrawer write FDrawer;
  end;

implementation

uses
  Math, Types,
  TAGeometry;

function ToImage(const AP: TDiaPoint): TPoint; inline;
begin
  Result := Point(Round(AP.X[duPixels]), Round(AP.Y[duPixels]));
end;

procedure DrawDiaBox(ASelf: TDiaBox);
var
  id: IChartDrawer;
begin
  id := (ASelf.Owner.Context as TDiaContextDrawer).Drawer;
  id.PrepareSimplePen($000000);
  id.SetBrushColor($FFFFFF);
  with ASelf do
    id.Polygon([
      ToImage(FTopLeft), ToImage(FTopRight),
      ToImage(FBottomRight), ToImage(FBottomLeft)
    ], 0, 4);
  id.TextOut.Pos(ToImage(ASelf.FTopLeft) + Point(4, 4)).Text(ASelf.Caption).Done;
end;

procedure DrawDiaLink(ASelf: TDiaLink);
var
  id: IChartDrawer;
var
  da: Double;
  diag: Integer;
  pt1, pt2, ptBase: TPoint;
  Width: Integer = 10;
  Length: Integer = 20;
  startPos, endPos: TPoint;
  AAngle: float;
begin
  if (ASelf.Start = nil) or (ASelf.Finish = nil) then exit;
  id := (ASelf.Owner.Context as TDiaContextDrawer).Drawer;
  id.PrepareSimplePen($000000);
  if ASelf.Dashes then
    id.SetPenParams(psDash, $000000);
  id.SetBrushColor($FFFFFF);
  startPos := ToImage(ASelf.Start.ActualPos);
  endPos := ToImage(ASelf.Finish.ActualPos);
  id.Line(startPos, endPos);
  if not ASelf.Arrow then exit;
  id.SetPenParams(psSolid, $000000);
  da := ArcTan2(Width, Length);

  endPos := ToImage(ASelf.Finish.ActualPos);
  AAngle := ArcTan2(endPos.Y - startPos.Y, endPos.X - startPos.X);
  diag := -Round(Sqrt(Sqr(Length) + Sqr(Width)));
  pt1 := endPos + RotatePointX(diag, AAngle - da);
  pt2 := endPos + RotatePointX(diag, AAngle + da);
  id.Polygon([pt1, endPos, pt2], 0, 3);
end;

initialization
  TDiaBox.FInternalDraw := @DrawDiaBox;
  TDiaLink.FInternalDraw := @DrawDiaLink;

end.

