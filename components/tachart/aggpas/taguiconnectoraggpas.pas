{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}
unit TAGUIConnectorAggPas;

{$H+}

interface

uses
  Agg_FPImage,
  Classes,
  TAGUIConnector;

type
  TChartGUIConnectorAggPas = class(TChartGUIConnector)
  private
    FFontDir: String;
    FPixelFormat: TAggFPImgPixelFormat;
    procedure SetPixelFormat(AValue: TAggFPImgPixelFormat);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateDrawer(var AData: TChartGUIConnectorData); override;
    procedure SetBounds(var AData: TChartGUIConnectorData); override;
    procedure Display(var AData: TChartGUIConnectorData); override;
  published
    property FontDir: String
      read FFontDir write FFontDir;
    property PixelFormat: TAggFPImgPixelFormat
      read FPixelFormat write SetPixelFormat default afpimRGBA32;
  end;

procedure Register;

implementation

{$R taaggpas.res}

uses
  Agg_LCL, Graphics, GraphType, SysUtils, fpImage, IntfGraphics,
  TAChartUtils, TADrawerAggPas, TADrawerCanvas, TAGeometry;

type
  TAggPasOwnerDrawer = class(TAggPasDrawer)
  strict protected
    FBitmap: TBitmap;
  public
    constructor Create(ACanvas: TAggLCLCanvas);
    destructor Destroy; override;
    procedure SetSize(ASize: TPoint);
    procedure PaintOnCanvas(ACanvas: TCanvas; const ARect: TRect);
    property Canvas: TAggLCLCanvas read FCanvas;
  end;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartGUIConnectorAggPas]);
end;

{ TChartGUIConnectorAggPas }

constructor TChartGUIConnectorAggPas.Create(AOwner: TComponent); 
begin
  inherited;
  FPixelFormat := afpimRGBA32;
end;

procedure TChartGUIConnectorAggPas.CreateDrawer(
  var AData: TChartGUIConnectorData);
begin
  AData.FDrawer := TAggPasOwnerDrawer.Create(TAggLCLCanvas.Create);
  AData.FDrawer.DoGetFontOrientation := @CanvasGetFontOrientationFunc;
  (AData.FDrawer as TAggPasOwnerDrawer).FontDir := FFontDir;
end;

procedure TChartGUIConnectorAggPas.Display(var AData: TChartGUIConnectorData);
begin
  (AData.FDrawer as TAggPasOwnerDrawer).PaintOnCanvas(AData.FCanvas, AData.FBounds);
end;

procedure TChartGUIConnectorAggPas.SetBounds(var AData: TChartGUIConnectorData);
begin
  AData.FDrawerBounds.TopLeft := Point(0, 0);
  AData.FDrawerBounds.BottomRight :=
    AData.FBounds.BottomRight - AData.FBounds.TopLeft;
  with AData.FDrawer as TAggPasOwnerDrawer do begin
    SetSize(AData.FDrawerBounds.BottomRight);
    Canvas.Image.PixelFormat := PixelFormat;
  end;
end;

procedure TChartGUIConnectorAggPas.SetPixelFormat(AValue: TAggFPImgPixelFormat);
begin
  if FPixelFormat = AValue then exit;
  FPixelFormat := AValue;
  Broadcaster.Broadcast(Self);
end;

{ TAggPasOwnerDrawer }

constructor TAggPasOwnerDrawer.Create(ACanvas: TAggLCLCanvas);
begin
  inherited Create(ACanvas);
  FBitmap := TBitmap.Create;
end;

destructor TAggPasOwnerDrawer.Destroy;
begin
  inherited;
  FreeAndNil(FBitmap);
  FreeAndNil(FCanvas);
end;

procedure TAggPasOwnerDrawer.PaintOnCanvas(
  ACanvas: TCanvas; const ARect: TRect);
{$IF DEFINED(LCLWin32) or DEFINED(LCLCocoa)}
begin
  FBitmap.LoadFromIntfImage(FCanvas.Image.IntfImg);
  ACanvas.Draw(ARect.Left, ARect.Top, FBitmap);
end;
{$ELSE}
{ The default pixel settings of AggPas are correct for Windows. 
  On Linux, however, the red and blue components are interchanged. 
  The following work-around creates an auxiliary image in which R and B are 
  swapped to be compatible with AggPas. }
var
  img: TLazIntfImage;
  raw: TRawImage;
begin
  FCanvas.Image.IntfImg.GetRawImage(raw, false);
  img := TLazIntfImage.Create(raw, false);
  try
    SwapRedBlue(img);
    FBitmap.LoadFromIntfImage(img);
  finally
    img.Free;
  end;
  ACanvas.Draw(ARect.Left, ARect.Top, FBitmap);
end;
{$ENDIF}

procedure TAggPasOwnerDrawer.SetSize(ASize: TPoint);
begin
  FBitmap.SetSize(ASize.X, ASize.Y);
  FCanvas.Width := ASize.X;
  FCanvas.Height := ASize.Y;
end;

end.
