unit Main;

{$mode objfpc}{$H+}

interface

uses
  ComCtrls, db, DBGrids, memds, Forms, TADbSource, TAGraph, TASeries, Classes;

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
    procedure FormCreate(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

{ Add dummy data to start with }
procedure TForm1.FormCreate(Sender: TObject);
const
  N = 3;
var
  i: Integer;
  Fx, Fy: TField;
begin
  Fx := MemDataset1.FieldByName('X');
  Fy := MemDataset1.FieldByName('Y');
  for i:= 1 to N do begin
    MemDataset1.Append;
    Fx.AsInteger := i;
    Fy.AsFloat := Random;
    MemDataset1.Post;
  end;
end;

procedure TForm1.tbCopyClick(Sender: TObject);
begin
  Chart1BarSeries1.ListSource.CopyFrom(DbChartSource1);
end;

end.

