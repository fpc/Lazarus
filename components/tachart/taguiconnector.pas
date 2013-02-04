{

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
unit TAGUIConnector;

{$H+}

interface

uses
  Classes, Graphics,
  TADrawUtils, TADrawerCanvas;

type
  TChartGUIConnectorData = record
    FBounds: TRect;
    FCanvas: TCanvas;
    FDrawer: IChartDrawer;
    FDrawerBounds: TRect;
  end;

  TChartGUIConnector = class(TComponent)
  public
    procedure CreateDrawer(var AData: TChartGUIConnectorData); virtual; abstract;
    procedure SetBounds(var AData: TChartGUIConnectorData); virtual; abstract;
    procedure Display(var AData: TChartGUIConnectorData); virtual; abstract;
  end;

  TChartGUIConnectorCanvas = class(TChartGUIConnector)
  public
    procedure CreateDrawer(var AData: TChartGUIConnectorData); override;
    procedure SetBounds(var AData: TChartGUIConnectorData); override;
    procedure Display(var AData: TChartGUIConnectorData); override;
  end;

implementation

uses
  TAChartUtils;

{ TChartGUIConnectorCanvas }

procedure TChartGUIConnectorCanvas.CreateDrawer(
  var AData: TChartGUIConnectorData);
begin
  AData.FDrawer := TCanvasDrawer.Create(AData.FCanvas);
end;

procedure TChartGUIConnectorCanvas.Display(var AData: TChartGUIConnectorData);
begin
  Unused(AData);
end;

procedure TChartGUIConnectorCanvas.SetBounds(var AData: TChartGUIConnectorData);
begin
  AData.FDrawerBounds := AData.FBounds;
end;

end.
