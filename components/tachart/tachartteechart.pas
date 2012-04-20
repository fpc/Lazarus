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
  TAGraph, TAChartAxis, TAChartAxisUtils;

type
  TChartTeeChart = class helper for TChart
  private
    // Workaround for issue #21809.
    function GetAxisByAlign(AIndex: TChartAxisAlignment): TChartAxis;
  public
    property RightAxis: TChartAxis index calRight read GetAxisByAlign;
    property TopAxis: TChartAxis index calTop read GetAxisByAlign;
  end;

implementation

{ TChartTeeChart }

function TChartTeeChart.GetAxisByAlign(AIndex: TChartAxisAlignment): TChartAxis;
begin
  Result := inherited GetAxisByAlign(AIndex);
end;

procedure Dummy;
begin
  // Workaround for issue #21808.
end;

initialization

  Dummy;

end.

