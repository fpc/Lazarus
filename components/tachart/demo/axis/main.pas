unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    RandomChartSource1: TRandomChartSource;
    procedure TChartAxisList1MarkToText(var AText: String; AMark: Double);
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.TChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  if AMark < 15 then
    AText := '*' + AText + '*';
end;

initialization
  {$I main.lrs}

end.

