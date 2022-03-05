unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ExtCtrls, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    timerRedraw: TTimer;
    trackSpeed: TTrackBar;
    procedure FormPaint(Sender: TObject);
    procedure timerRedrawTimer(Sender: TObject);
    procedure trackSpeedChange(Sender: TObject);
  private

  public
    CurStep: Double;
    procedure RotatePolygon(var APoints: array of TPoint; AAngle: Double);
    function RotatePoint(APoint, ACenter: TPoint; AAngle: Double): TPoint;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.timerRedrawTimer(Sender: TObject);
begin
  CurStep := CurStep + 0.1;
  if CurStep > 360 then CurStep := 0;
  Form1.Invalidate;
end;

procedure TForm1.trackSpeedChange(Sender: TObject);
begin
  timerRedraw.Interval := 1000 div trackSpeed.Position;
end;

procedure TForm1.RotatePolygon(var APoints: array of TPoint; AAngle: Double);
var
  lCenter: TPoint;
  i: Integer;
begin
  lCenter := Point(0, 0);
  for i := 0 to Length(APoints)-1 do
  begin
    lCenter.X := lCenter.X + APoints[i].X;
    lCenter.Y := lCenter.Y + APoints[i].Y;
  end;
  lCenter.X := lCenter.X div Length(APoints);
  lCenter.Y := lCenter.Y div Length(APoints);

  for i := 0 to Length(APoints)-1 do
    APoints[i] := RotatePoint(APoints[i], lCenter, AAngle);
end;

function TForm1.RotatePoint(APoint, ACenter: TPoint; AAngle: Double): TPoint;
var
  dx, dy: Double;
  sinAngle, cosAngle: Double;
begin
  SinCos(AAngle, sinAngle, cosAngle);
  dx :=  ACenter.Y * sinAngle - ACenter.X * cosAngle + ACenter.X + 10;
  dy := -ACenter.X * sinAngle - ACenter.Y * cosAngle + ACenter.Y + Height div 4;
  Result.X := Round(APoint.X * cosAngle - APoint.Y * sinAngle + dx);
  Result.Y := Round(APoint.X * sinAngle + APoint.Y * cosAngle + dy);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  lPoints: array[0..2] of TPoint;
begin
  lPoints[0].X := Self.Width  div 4;
  lPoints[0].Y := Self.Height div 4;
  lPoints[1].X := Self.Width  div 2;
  lPoints[1].Y := 0;
  lPoints[2].X := Self.Width  div 2;
  lPoints[2].Y := Self.Height div 2;
  RotatePolygon(lPoints, CurStep);
  Canvas.Polygon(lPoints);
end;

end.

