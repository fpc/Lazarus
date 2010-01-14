unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, db, DBGrids, memds, Forms, TADbSource, TAGraph, TASeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Datasource1: TDatasource;
    DbChartSource1: TDbChartSource;
    DBGrid1: TDBGrid;
    MemDataset1: TMemDataset;
    ToolBar1: TToolBar;
    tbCopy: TToolButton;
    procedure tbCopyClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.tbCopyClick(Sender: TObject);
begin
  Chart1BarSeries1.ListSource.CopyForm(DbChartSource1);
end;

end.

