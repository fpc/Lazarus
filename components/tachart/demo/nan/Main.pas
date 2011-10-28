unit Main;

{$mode objfpc}{$H+}

interface

uses
  CheckLst, Classes, ExtCtrls, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, TAGraph, TASeries, TASources;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1AreaSeries1: TAreaSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1LineSeries1: TLineSeries;
    Chart1PieSeries1: TPieSeries;
    clbNans: TCheckListBox;
    lcs1: TListChartSource;
    lcs2: TListChartSource;
    Panel2: TPanel;
    rgXY: TRadioGroup;
    rgSeriesType: TRadioGroup;
    procedure clbNansItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure rgSeriesTypeClick(Sender: TObject);
    procedure rgXYClick(Sender: TObject);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  Math, TAChartUtils;

{ TForm1 }

procedure TForm1.clbNansItemClick(Sender: TObject; Index: integer);
begin
  if rgXY.ItemIndex = 0 then
    lcs1.SetXValue(Index, IfThen(clbNans.Checked[Index], Nan, lcs2[Index]^.X))
  else
    lcs1.SetYValue(Index, IfThen(clbNans.Checked[Index], Nan, lcs2[Index]^.Y));
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Randomize;
  for i := 1 to 20 do begin
    lcs1.Add(i, Random(100) / 50);
    clbNans.Items.Add(IntToStr(i));
  end;
  lcs2.CopyFrom(lcs1);
end;

procedure TForm1.rgSeriesTypeClick(Sender: TObject);
var
  s: TBasicChartSeries;
begin
  for s in Chart1.Series do
    s.Active := s.Index = rgSeriesType.ItemIndex;
end;

procedure TForm1.rgXYClick(Sender: TObject);
var
  i: Integer;
begin
  lcs1.CopyFrom(lcs2);
  for i := 0 to lcs1.Count - 1 do
    clbNansItemClick(nil, i);
end;

end.

