unit frmBoxWhisker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls,
  TAGraph, TAMultiSeries;

type
  TBoxWhiskerFrame = class(TFrame)
    Chart: TChart;
    BoxAndWhiskerSeries: TBoxAndWhiskerSeries;
  private

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

constructor TBoxWhiskerFrame.Create(AOwner: TComponent);
var
  ylist: array [1..4] of Double;
  i, j: Integer;
  y, y0: Double;
begin
  inherited;

  BoxAndWhiskerSeries.ListSource.YCount := 5;
  for i := 1 to 6 do begin
    y := Random(80) + 10;
    y0 := y;
    for j := 1 to 4 do begin
      y += Random(20) + 5;
      ylist[j] := y;
    end;
    BoxAndWhiskerSeries.AddXY(i, y0, ylist);
  end;
end;

end.

