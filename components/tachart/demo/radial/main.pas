unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, SysUtils, FileUtil, Forms, Controls,
  Graphics, Dialogs, TAGraph, TASeries, TASources, TATools;

type

  { TForm1 }

  TForm1 = class(TForm)
    ChartPie: TChart;
    ChartPiePieSeries1: TPieSeries;
    ChartToolset1: TChartToolset;
    ListChartSource1: TListChartSource;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsPie: TTabSheet;
    procedure ChartPieMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ChartPieMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := ChartPiePieSeries1.FindContainingSlice(Point(X, Y));
  if i < 0 then exit;
  ListChartSource1.SetXValue(i, 0.2 - ListChartSource1[i]^.X);
  ChartPie.Invalidate;
end;

end.

