unit frmCustomMarks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls,
  TAGraph, TASources, TASeries;

type
  TCustomMarksFrame = class(TFrame)
    ChartCustomMarks: TChart;
    ChartCustomMarksBarSeries1: TBarSeries;
    lcsMarks: TListChartSource;
    procedure ChartCustomMarksAxisList1MarkToText(var AText: String;
      AMark: Double);
  private

  public

  end;

implementation

{$R *.lfm}

procedure TCustomMarksFrame.ChartCustomMarksAxisList1MarkToText(var AText:
  String; AMark: Double);
begin
  if AMark = 3 then
    AText := '*' + AText + '*';
end;

end.

