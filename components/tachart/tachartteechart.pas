{

 TeeChart compatibility.

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TAChartTeeChart;

{$H+}

interface

uses
  Classes, SysUtils,
  TAGraph, TAChartAxis, TAChartAxisUtils, TAChartUtils, TASeries;

type
  TChartTeeChart = class helper for TChart
  strict private
    // Workaround for issue #21809.
    function GetAxisByAlign1(AIndex: TChartAxisAlignment): TChartAxis; inline;
    function GetMargin(AIndex: Integer): Integer; inline;
    procedure SetMargin(AIndex: Integer; AValue: TChartDistance); inline;
  public
    property RightAxis: TChartAxis index calRight read GetAxisByAlign1;
    property TopAxis: TChartAxis index calTop read GetAxisByAlign1;
  public
    property MarginBottom: TChartDistance index 4 read GetMargin write SetMargin;
    property MarginLeft: TChartDistance index 1 read GetMargin write SetMargin;
    property MarginRight: TChartDistance index 3 read GetMargin write SetMargin;
    property MarginTop: TChartDistance index 2 read GetMargin write SetMargin;
  end;

  TPointerSeries = class(TLineSeries)
  published
    property LineType default ltNone;
    property ShowPoints default true;
  end;

implementation

{ TChartTeeChart }

function TChartTeeChart.GetAxisByAlign1(AIndex: TChartAxisAlignment): TChartAxis;
begin
  // Using "inherited" here results in a crash, probably due to FPC bug.
  Result := GetAxisByAlign(AIndex);
end;

function TChartTeeChart.GetMargin(AIndex: Integer): Integer;
begin
  Result := Margins.GetValue(AIndex);
end;

procedure TChartTeeChart.SetMargin(AIndex: Integer; AValue: TChartDistance);
begin
  Margins.SetValue(AIndex, AValue);
end;

procedure Dummy;
begin
  // Workaround for issue #21808.
end;

initialization

  Dummy;

end.

