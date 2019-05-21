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
  Classes, FPCanvas,
  TADrawUtils, TADiagram;

type
  TDiaContextDrawer = class(TDiaContext)
  private
    FDrawer: IChartDrawer;
  public
    property Drawer: IChartDrawer read FDrawer write FDrawer;
  end;

  IDiaDrawerDecorator = interface
    ['{A2AB054F-D725-401D-A249-18BF03CFF6FA}']
    procedure Apply(ADrawer: IChartDrawer);
  end;

  TDiaBrushDecorator = class(TDiaDecorator, IDiaDrawerDecorator)
  private
    FBrush: TFPCustomBrush;
  public
    constructor Create(AOwner: TDiaDecoratorList);
    destructor Destroy; override;
    procedure Apply(ADrawer: IChartDrawer);
    property Brush: TFPCustomBrush read FBrush;
  end;

  TDiaFontDecorator = class(TDiaDecorator, IDiaDrawerDecorator)
  private
    FFont: TFPCustomFont;
  public
    constructor Create(AOwner: TDiaDecoratorList);
    destructor Destroy; override;
    procedure Apply(ADrawer: IChartDrawer);
    property Font: TFPCustomFont read FFont;
  end;

  TDiaPenDecorator = class(TDiaDecorator, IDiaDrawerDecorator)
  private
    FPen: TFPCustomPen;
  public
    constructor Create(AOwner: TDiaDecoratorList);
    destructor Destroy; override;
    procedure Apply(ADrawer: IChartDrawer);
    property Pen: TFPCustomPen read FPen;
  end;

implementation

uses
  Math, Types, SysUtils,
  TAGeometry;

function ToImage(const AP: TDiaPoint): TPoint; inline;
begin
  Result := RoundPoint(AP.AsUnits(duPixels));
end;

procedure DrawDiaBox(ASelf: TDiaBox);
var
  id: IChartDrawer;
  d: IDiaDecorator;
begin
  id := (ASelf.Owner.Context as TDiaContextDrawer).Drawer;
  id.PrepareSimplePen($000000);
  id.SetBrushColor($FFFFFF);
  for d in ASelf.Decorators do
    if d is IDiaDrawerDecorator then
      IDiaDrawerDecorator(d).Apply(id);
  with ASelf do
    id.Polygon([
      ToImage(FTopLeft), ToImage(FTopRight),
      ToImage(FBottomRight), ToImage(FBottomLeft)
    ], 0, 4);
  id.TextOut.Pos(ToImage(ASelf.FTopLeft) + Point(4, 4)).Text(ASelf.Caption).Done;
end;

procedure DrawEndPoint(
  ADrawer: IChartDrawer; AEndPoint: TDiaEndPoint;
  const APos: TPoint; AAngle: Double);
var
  da: Double;
  diag: Integer;
  pt1, pt2: TPoint;
  d: IDiaDecorator;
begin
  ADrawer.SetPenParams(psSolid, $000000);
  ADrawer.SetBrushColor($FFFFFF);
  for d in AEndPoint.Decorators do
    if d is IDiaDrawerDecorator then
      IDiaDrawerDecorator(d).Apply(ADrawer);
  da := ArcTan2(AEndPoint.Width.Value, AEndPoint.Length.Value);

  diag := -Round(Sqrt(Sqr(AEndPoint.Length.Value) + Sqr(AEndPoint.Width.Value)));
  pt1 := APos + RotatePointX(diag, AAngle - da);
  pt2 := APos + RotatePointX(diag, AAngle + da);
  case AEndPoint.Shape of
    depsClosedArrow: ADrawer.Polygon([pt1, APos, pt2], 0, 3);
    depsOpenArrow: ADrawer.Polyline([pt1, APos, pt2], 0, 3);
  end;
end;

procedure DrawDiaLink(ASelf: TDiaLink);
var
  id: IChartDrawer;
  startPos, endPos, p: TPoint;
  d: IDiaDecorator;
begin
  if (ASelf.Start.Connector = nil) or (ASelf.Finish.Connector = nil) then exit;
  id := (ASelf.Owner.Context as TDiaContextDrawer).Drawer;
  id.PrepareSimplePen($000000);
  for d in ASelf.Decorators do
    if d is IDiaDrawerDecorator then
      IDiaDrawerDecorator(d).Apply(id);
  startPos := ToImage(ASelf.Start.Connector.ActualPos);
  endPos := ToImage(ASelf.Finish.Connector.ActualPos);
  case ASelf.Routing of
    dlrStraight: begin
      id.Line(startPos, endPos);
      p := startPos;
    end;
    dlrXThenY: begin
      p := Point(endPos.X, startPos.Y);
      id.Polyline([startPos, p, endPos], 0, 3);
    end;
    dlrYThenX: begin
      p := Point(startPos.X, endPos.Y);
      id.Polyline([startPos, p, endPos], 0, 3);
    end;
  end;
  if ASelf.Start.Shape <> depsNone then
    with p - endPos do
      DrawEndPoint(id, ASelf.Start, startPos, ArcTan2(Y, X));
  if ASelf.Finish.Shape <> depsNone then
    with endPos - p do
      DrawEndPoint(id, ASelf.Finish, endPos, ArcTan2(Y, X));
end;

{ TDiaBrushDecorator }

procedure TDiaBrushDecorator.Apply(ADrawer: IChartDrawer);
begin
  ADrawer.Brush := Brush;
end;

constructor TDiaBrushDecorator.Create(AOwner: TDiaDecoratorList);
begin
  inherited Create(AOwner);
  FBrush := TFPCustomBrush.Create;
end;

destructor TDiaBrushDecorator.Destroy;
begin
  FreeAndNil(FBrush);
  inherited;
end;

{ TDiaFontDecorator }

procedure TDiaFontDecorator.Apply(ADrawer: IChartDrawer);
begin
  ADrawer.Font := Font;
end;

constructor TDiaFontDecorator.Create(AOwner: TDiaDecoratorList);
begin
  inherited Create(AOwner);
  FFont := TFPCustomFont.Create;
end;

destructor TDiaFontDecorator.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

{ TDiaPenDecorator }

procedure TDiaPenDecorator.Apply(ADrawer: IChartDrawer);
begin
  ADrawer.Pen := Pen;
end;

constructor TDiaPenDecorator.Create(AOwner: TDiaDecoratorList);
begin
  inherited Create(AOwner);
  FPen := TFPCustomPen.Create;
  FPen.Mode := pmCopy;
end;

destructor TDiaPenDecorator.Destroy;
begin
  FreeAndNil(FPen);
  inherited;
end;

initialization
  TDiaBox.FInternalDraw := @DrawDiaBox;
  TDiaLink.FInternalDraw := @DrawDiaLink;

end.

