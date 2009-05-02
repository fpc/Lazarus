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
    Chart1FuncSeries1: TFuncSeries;
    Chart1YAxis: TLine;
    Chart1XAxis: TLine;
    procedure Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Chart1FuncSeries1Calculate(const AX: Double; out AY: Double);
begin
  AY := Sin(AX);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Chart1FuncSeries1.OnCalculate := @Chart1FuncSeries1Calculate;
end;

initialization
  {$I main.lrs}

end.

