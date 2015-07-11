{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TAGUIConnectorBGRA;

{$H+}

interface

uses
  Classes, Graphics,
  TAGUIConnector;

type
  TChartGUIConnectorBGRA = class(TChartGUIConnector)
  public
    procedure CreateDrawer(var AData: TChartGUIConnectorData); override;
    procedure SetBounds(var AData: TChartGUIConnectorData); override;
    procedure Display(var AData: TChartGUIConnectorData); override;
  end;

procedure Register;

implementation

uses
  BGRABitmap, SysUtils, TAChartUtils, TADrawerBGRA, TADrawerCanvas, TAGeometry;

type
  TBGRABitmapOwnerDrawer = class(TBGRABitmapDrawer)
  public
    destructor Destroy; override;
    procedure SetSize(ASize: TPoint);
    procedure PaintOnCanvas(ACanvas: TCanvas; const ARect: TRect);
  end;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartGUIConnectorBGRA]);
end;

{ TChartGUIConnectorBGRA }

procedure TChartGUIConnectorBGRA.CreateDrawer(
  var AData: TChartGUIConnectorData);
begin
  AData.FDrawer := TBGRABitmapOwnerDrawer.Create(TBGRABitmap.Create(0, 0));
  AData.FDrawer.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
end;

procedure TChartGUIConnectorBGRA.Display(var AData: TChartGUIConnectorData);
begin
  (AData.FDrawer as TBGRABitmapOwnerDrawer).PaintOnCanvas(
    AData.FCanvas, AData.FBounds);
end;

procedure TChartGUIConnectorBGRA.SetBounds(var AData: TChartGUIConnectorData);
begin
  AData.FDrawerBounds.TopLeft := Point(0, 0);
  AData.FDrawerBounds.BottomRight :=
    AData.FBounds.BottomRight - AData.FBounds.TopLeft;
  (AData.FDrawer as TBGRABitmapOwnerDrawer).SetSize(
    AData.FDrawerBounds.BottomRight);
end;

{ TBGRABitmapOwnerDrawer }

destructor TBGRABitmapOwnerDrawer.Destroy;
begin
  inherited;
  FreeAndNil(FBitmap);
end;

procedure TBGRABitmapOwnerDrawer.PaintOnCanvas(
  ACanvas: TCanvas; const ARect: TRect);
begin
  FBitmap.Draw(ACanvas, ARect);
end;

procedure TBGRABitmapOwnerDrawer.SetSize(ASize: TPoint);
begin
  FBitmap.SetSize(ASize.X, ASize.Y);
end;

end.
