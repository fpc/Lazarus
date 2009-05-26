unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, db, DBGrids, memds, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, TADbSource, TAGraph, TASeries;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Datasource1: TDatasource;
    DbChartSource1: TDbChartSource;
    DBGrid1: TDBGrid;
    MemDataset1: TMemDataset;
  end;

var
  Form1: TForm1; 

implementation

initialization
  {$I main.lrs}

end.

