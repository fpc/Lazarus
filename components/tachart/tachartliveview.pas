unit TAChartLiveView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TAChartUtils;

type
  TChartLiveViewExtentY = (lveAuto, lveFull, lveLogical);

  TChartLiveView = class(TComponent)
  private
    FActive: Boolean;
    FChart: TChart;
    FExtentY: TChartLiveViewExtentY;
    FListener: TListener;
    FViewportSize: Double;
    procedure FullExtentChanged(Sender: TObject);
    procedure SetActive(const AValue: Boolean);
    procedure SetChart(const AValue: TChart);
    procedure SetExtentY(const AValue: TChartLiveViewExtentY);
    procedure SetViewportSize(const AValue: Double);
    procedure UpdateViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default false;
    property Chart: TChart read FChart write SetChart default nil;
    property ExtentY: TChartLiveViewExtentY read FExtentY write SetExtentY default lveAuto;
    property ViewportSize: double read FViewportSize write SetViewportSize;
  end;

procedure Register;


implementation

uses
  Math, TACustomSeries;

constructor TChartLiveView.Create(AOwner: TComponent);
begin
  inherited;
  FListener := TListener.Create(@FChart, @FullExtentChanged);
end;

destructor TChartLiveView.Destroy;
begin
  FreeAndNil(FListener);
  inherited;
end;

procedure TChartLiveView.FullExtentChanged(Sender: TObject);
begin
  if (not FActive) or (FChart = nil) then
    exit;
  UpdateViewport;
end;

procedure TChartLiveView.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then exit;

  FActive := AValue;
  FullExtentChanged(nil);
end;

procedure TChartLiveView.SetChart(const AValue: TChart);
begin
  if FChart = AValue then exit;

  if FListener.IsListening then
    FChart.FullExtentBroadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.FullExtentBroadcaster.Subscribe(FListener);
  FullExtentChanged(Self);
end;

procedure TChartLiveview.SetExtentY(const AValue: TChartLiveViewExtentY);
begin
  if FExtentY = AValue then exit;
  FExtentY := AValue;
  FullExtentChanged(nil);
end;

procedure TChartLiveView.SetViewportSize(const AValue: Double);
begin
  if FViewportSize = AValue then exit;

  FViewportSize := AValue;
  FullExtentChanged(nil);
end;

procedure TChartLiveView.UpdateViewport;
var
  fext, lext: TDoubleRect;
  w: double;
  i: Integer;
  ymin, ymax: Double;
begin
  if not FChart.ScalingValid then
    exit;

  fext := FChart.GetFullExtent();
  lext := FChart.LogicalExtent;
  w := lext.b.x - lext.a.x;
  if FViewportSize = 0 then
    w := lext.b.x - lext.a.x
  else
    w := FViewportSize;
  lext.b.x := fext.b.x;
  lext.a.x := lext.b.X - w;
  if lext.a.x < fext.a.x then begin
    lext.a.x := fext.a.x;
    lext.b.x := lext.a.x + w;
  end;
  case FExtentY of
    lveAuto:
      begin
        ymin := Infinity;
        ymax := -Infinity;
        for i := 0 to FChart.SeriesCount-1 do
          if FChart.Series[i] is TChartSeries then
            TChartSeries(FChart.Series[i]).FindYRange(lext.a.x, lext.b.x, ymin, ymax);
        if (ymin <> Infinity) and (ymax <> -Infinity) then
        begin
          lext.a.y := ymin;
          lext.b.y := ymax;
        end;
      end;
    lveFull:
      begin
        lext.a.y := fext.a.y;
        lext.b.y := fext.b.y;
      end;
    lveLogical:
      ;
  end;
  FChart.LogicalExtent := lext;
end;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartLiveView]);
end;

end.

