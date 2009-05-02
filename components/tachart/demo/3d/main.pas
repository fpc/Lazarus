unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  TAGraph, TASeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1BarSeries2: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  RandSeed := 876234;
  for i := 1 to 7 do begin
    Chart1BarSeries1.AddXY(i, Random(8) + 4);
    Chart1BarSeries2.AddXY(i, Random(8) + 4);
    Chart1LineSeries1.AddXY(i, Random(8) + 4);
  end;
end;

initialization
  {$I main.lrs}

end.

