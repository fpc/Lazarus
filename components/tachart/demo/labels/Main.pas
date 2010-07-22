unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, Spin, StdCtrls, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, TAGraph, TASeries, TASources;

type

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1BarSeries1: TBarSeries;
    cbHideOverlapping: TCheckBox;
    lblAxisAngle: TLabel;
    lblSeriesAngle: TLabel;
    pcMain: TPageControl;
    pnlControls: TPanel;
    RandomChartSource1: TRandomChartSource;
    seAxisAngle: TSpinEdit;
    seSeriesAngle: TSpinEdit;
    tsBar: TTabSheet;
    procedure cbHideOverlappingChange(Sender: TObject);
    procedure seAxisAngleChange(Sender: TObject);
    procedure seSeriesAngleChange(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  TATypes;

{ TForm1 }

procedure TForm1.cbHideOverlappingChange(Sender: TObject);
var
  op: TChartMarksOverlapPolicy;
begin
  if cbHideOverlapping.Checked then
    op := opHideNeighbour
  else
    op := opIgnore;
  Chart1.LeftAxis.Marks.OverlapPolicy := op;
  Chart1.BottomAxis.Marks.OverlapPolicy := op;
  Chart1BarSeries1.Marks.OverlapPolicy := op;
end;

procedure TForm1.seAxisAngleChange(Sender: TObject);
begin
  Chart1.LeftAxis.Marks.LabelFont.Orientation := seAxisAngle.Value * 10;
  Chart1.BottomAxis.Marks.LabelFont.Orientation := seAxisAngle.Value * 10;
end;

procedure TForm1.seSeriesAngleChange(Sender: TObject);
begin
  Chart1BarSeries1.Marks.LabelFont.Orientation := seSeriesAngle.Value * 10;
end;

end.

