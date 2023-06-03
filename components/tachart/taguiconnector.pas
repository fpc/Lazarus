{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Alexander Klenin

}
unit TAGUIConnector;

{$MODE ObjFPC}{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TADrawUtils, TADrawerCanvas;

type
  TChartGUIConnectorData = record
    FBounds: TRect;
    FCanvas: TCanvas;
    FDrawer: IChartDrawer;
    FDrawerBounds: TRect;
  end;

  TChartGUIConnector = class(TComponent)
  strict private
    FBroadcaster: TBroadcaster;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateDrawer(var AData: TChartGUIConnectorData); virtual; abstract;
    procedure SetBounds(var AData: TChartGUIConnectorData); virtual; abstract;
    procedure Display(var AData: TChartGUIConnectorData); virtual; abstract;

    property Broadcaster: TBroadcaster read FBroadcaster;
  end;

  TChartGUIConnectorCanvas = class(TChartGUIConnector)
  public
    procedure CreateDrawer(var AData: TChartGUIConnectorData); override;
    procedure SetBounds(var AData: TChartGUIConnectorData); override;
    procedure Display(var AData: TChartGUIConnectorData); override;
  end;

implementation

uses
  SysUtils;

{ TChartGUIConnector }

constructor TChartGUIConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBroadcaster := TBroadcaster.Create;
end;

destructor TChartGUIConnector.Destroy;
begin
  FreeAndNil(FBroadcaster);
  inherited;
end;

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
