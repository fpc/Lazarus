unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms, Controls, Graphics,
  Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    SinSeries: TLineSeries;
    CosSeries: TLineSeries;
    SinCosSeries: TLineSeries;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
const
  N = 100;
  MIN = -10;
  MAX = 10;
var
  i: Integer;
  x: Double;
begin
  for i := 0 to N - 1 do begin
    x := MIN + (MAX - MIN) * i / (N - 1);
    SinSeries.AddXY(x, Sin(x));
    CosSeries.AddXY(x, Cos(x));
    SinCosSeries.AddXY(x, Sin(x) * Cos(x));
  end;
end;

end.

