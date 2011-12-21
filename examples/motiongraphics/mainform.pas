unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls;

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
    { private declarations }
  public
    { public declarations }
    CurStep: Double;
    procedure RotatePolygon(var APoints: array of TPoint; AAngle: Double);
    function RotatePoint(APoint, ACenter: TPoint; AAngle: Double): TPoint;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

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
begin
  dx := (ACenter.Y * Sin(AAngle)) - (ACenter.X * Cos(AAngle)) + ACenter.X;
  dy := -(ACenter.X * Sin(AAngle)) - (ACenter.Y * Cos(AAngle)) + ACenter.Y;
  Result.X := Round((APoint.X * Cos(AAngle)) - (APoint.Y * Sin(AAngle)) + dx);
  Result.Y := Round((APoint.X * Sin(AAngle)) + (APoint.Y * Cos(AAngle)) + dy);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  lPoints: array[0..2] of TPoint;
begin
  lPoints[0].X := 100;
  lPoints[0].Y := 100;
  lPoints[1].X := 200;
  lPoints[1].Y := 0;
  lPoints[2].X := 200;
  lPoints[2].Y := 200;
  RotatePolygon(lPoints, CurStep);
  Canvas.Polygon(lPoints);
end;

end.

