unit TAChartExtentLink;

{$H+}

interface

uses
  Classes, TAChartUtils, TAGraph;

type
  TLinkedChart = class(TCollectionItem)
  strict private
    FChart: TChart;
    FListener: TListener;
    procedure OnExtentChanged(ASender: TObject);
    procedure SetChart(AValue: TChart);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Chart: TChart read FChart write SetChart;
  end;

  TLinkedCharts = class(TCollection)
  strict private
    FOwner: TComponent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
  end;

  TChartExtendLinkMode = (elmXY, elmOnlyX, elmOnlyY);

  TChartExtentLink = class(TComponent)
  strict private
    FEnabled: Boolean;
    FLinkedCharts: TLinkedCharts;
    FMode: TChartExtendLinkMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SyncWith(AChart: TChart);
  published
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property LinkedCharts: TLinkedCharts read FLinkedCharts write FLinkedCharts;
    property Mode: TChartExtendLinkMode read FMode write FMode default elmXY;
  end;

procedure Register;

implementation

uses
  SysUtils, TAGeometry;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartExtentLink]);
end;

{ TLinkedCharts }

constructor TLinkedCharts.Create(AOwner: TComponent);
begin
  inherited Create(TLinkedChart);
  FOwner := AOwner;
end;

function TLinkedCharts.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TLinkedChart }

constructor TLinkedChart.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FListener := TListener.Create(@FListener, @OnExtentChanged);
end;

destructor TLinkedChart.Destroy;
begin
  FreeAndNil(FListener);
  inherited;
end;

procedure TLinkedChart.OnExtentChanged(ASender: TObject);
begin
  Unused(ASender);
  (Collection.Owner as TChartExtentLink).SyncWith(Chart);
end;

procedure TLinkedChart.SetChart(AValue: TChart);
begin
  if FChart = AValue then exit;
  if Chart <> nil then
    Chart.ExtentBroadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if Chart <> nil then
    Chart.ExtentBroadcaster.Subscribe(FListener);
end;

{ TChartExtentLink }

constructor TChartExtentLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := true;
  FLinkedCharts := TLinkedCharts.Create(Self);
end;

destructor TChartExtentLink.Destroy;
begin
  FreeAndNil(FLinkedCharts);
  inherited;
end;

procedure TChartExtentLink.SyncWith(AChart: TChart);

  function CombineXY(const AX, AY: TDoubleRect): TDoubleRect;
  begin
    Result.a := DoublePoint(AX.a.X, AY.a.Y);
    Result.b := DoublePoint(AX.b.X, AY.b.Y);
  end;

var
  c: TCollectionItem;
begin
  if not FEnabled or (AChart = nil) then exit;
  for c in LinkedCharts do
    with TLinkedChart(c).Chart do begin
      // Do not sync if the chart was never drawn yet.
      if LogicalExtent = EmptyExtent then continue;
      // An event loop will be broken since setting LogicalExtent to
      // the same value does not initiale the extent broadcast.
      case Mode of
        elmXY:
          LogicalExtent := AChart.LogicalExtent;
        elmOnlyX:
          LogicalExtent := CombineXY(AChart.LogicalExtent, LogicalExtent);
        elmOnlyY:
          LogicalExtent := CombineXY(LogicalExtent, AChart.LogicalExtent);
      end;
    end;
end;

end.

