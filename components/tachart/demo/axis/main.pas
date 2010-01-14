unit Main;

{$mode objfpc}{$H+}

interface

uses
  Forms, TAGraph, TASeries, TASources;

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

{$R *.lfm}

{ TForm1 }

procedure TForm1.TChartAxisList1MarkToText(var AText: String; AMark: Double);
begin
  if AMark < 15 then
    AText := '*' + AText + '*';
end;

end.

