unit main;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls,
  Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    ListChartSource1: TListChartSource;
    procedure Chart1BeforeDrawBackground(ASender: TChart; ACanvas: TCanvas;
      const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure Chart1BeforeDrawBackWall(ASender: TChart; ACanvas: TCanvas;
      const ARect: TRect; var ADoDefaultDrawing: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FBackImage: TPicture;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

//{$R splash_logo.res}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBackImage := TPicture.Create;
  FBackImage.LoadFromFile('..\..\..\..\images\splash_logo.png');
  // above path assumes standard Lazarus installation

  // or, use resources:
  //FBackImage.LoadFromResourceName(HInstance, 'splash_logo');
  // Don't forget to active the "{$R" directive above.
end;

procedure TForm1.Chart1BeforeDrawBackWall(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect; var ADoDefaultDrawing: Boolean);
begin
  ACanvas.StretchDraw(ARect, FBackImage.Graphic);
  ADoDefaultDrawing := false;
end;

procedure TForm1.Chart1BeforeDrawBackground(ASender: TChart; ACanvas: TCanvas;
  const ARect: TRect; var ADoDefaultDrawing: Boolean);
begin
  ACanvas.GradientFill(ARect, clSkyBlue, clWhite, gdVertical);
  ADoDefaultDrawing := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBackImage.Free;
end;

end.

