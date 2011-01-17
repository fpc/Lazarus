unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, TAGraph,
  TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    RandomChartSource1: TRandomChartSource;
    procedure Chart1BarSeries1BeforeDrawBar(ASender: TBarSeries;
      ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
      var ADoDefaultDrawing: Boolean);
    procedure Chart1BeforeDrawBackground(ASender: TChart; ACanvas: TCanvas;
      const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure Chart1BeforeDrawBackWall(ASender: TChart; ACanvas: TCanvas;
      const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Chart1BarSeries1BeforeDrawBar(ASender: TBarSeries;
  ACanvas: TCanvas; const ARect: TRect; APointIndex, AStackIndex: Integer;
  var ADoDefaultDrawing: Boolean);
begin
  if APointIndex mod 2 = 0 then
    ACanvas.Brush.Style := bsDiagCross;
  ADoDefaultDrawing := APointIndex <> 4;
end;

procedure TForm1.Chart1BeforeDrawBackground(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect; var ADoDefaultDrawing: Boolean);
begin
  ACanvas.GradientFill(ARect, $FFFFFF, $FF8080, gdVertical);
  ADoDefaultDrawing := false;
end;

procedure TForm1.Chart1BeforeDrawBackWall(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect; var ADoDefaultDrawing: Boolean);
begin
  ACanvas.GradientFill(ARect, $FFFFFF, $80FF80, gdVertical);
  ADoDefaultDrawing := false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Workaround for issue #18418
  Chart1.OnBeforeDrawBackground := @Chart1BeforeDrawBackground;
  Chart1.OnBeforeDrawBackWall := @Chart1BeforeDrawBackWall;
  Chart1BarSeries1.OnBeforeDrawBar := @Chart1BarSeries1BeforeDrawBar;
end;

end.

