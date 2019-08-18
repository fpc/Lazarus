unit TAChartExtentLink;

{$H+}

interface

uses
  Classes, TAChartUtils, TAChartAxisUtils, TAGraph;

type
  TLinkedChart = class(TCollectionItem)
  strict private
    FChart: TChart;
    FListener: TListener;
    procedure OnExtentChanged(ASender: TObject);
    procedure SetChart(AValue: TChart);
  protected
    function GetDisplayName: String; override;
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
    function Add: TLinkedChart; // Should be inline, but FPC 2.6 miscompiles it.
  end;

  TChartExtendLinkMode = (elmXY, elmOnlyX, elmOnlyY);
  TChartSides = set of TChartAxisAlignment;

  TChartExtentLink = class(TComponent)
  strict private
    FEnabled: Boolean;
    FLinkedCharts: TLinkedCharts;
    FMode: TChartExtendLinkMode;
    FAlignSides: TChartSides;
    procedure SetAlignSides(AValue: TChartSides);
  protected
    procedure DoAlignSides;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddChart(AChart: TChart);
    procedure SyncWith(AChart: TChart);
  published
    property AlignSides: TChartSides read FAlignSides write SetAlignSides default [];
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property LinkedCharts: TLinkedCharts read FLinkedCharts write FLinkedCharts;
    property Mode: TChartExtendLinkMode read FMode write FMode default elmXY;
  end;

procedure Register;

implementation

uses
  SysUtils, Math, TAGeometry, TAChartAxis;

procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartExtentLink]);
end;

{ TLinkedCharts }

function TLinkedCharts.Add: TLinkedChart;
begin
  Result := TLinkedChart(inherited Add);
end;

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
  FListener := TListener.Create(@FChart, @OnExtentChanged);
end;

destructor TLinkedChart.Destroy;
begin
  FreeAndNil(FListener);
  inherited;
end;

function TLinkedChart.GetDisplayName: String;
begin
  Result := inherited GetDisplayName;
  if Chart <> nil then
    Result += ' -> ' + Chart.Name;
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

procedure TChartExtentLink.AddChart(AChart: TChart);
var
  i: TCollectionItem;
begin
  if AChart = nil then
    exit;

  for i in LinkedCharts do
    if TLinkedChart(i).Chart = AChart then
      exit;

  LinkedCharts.Add.Chart := AChart;
  SyncWith(AChart);
end;

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

// Note: ignores several axes on the same chart side
procedure TChartExtentLink.DoAlignSides;
var
  c: TCollectionItem;
  ch: TChart;
  labelSize: array[TChartAxisAlignment] of Integer = (0, 0, 0, 0);
  sideUsed: array[TChartAxisAlignment] of boolean = (false, false, false, false);
  al: TChartAxisAlignment;
  axis: TChartAxis;
begin
  for c in LinkedCharts do begin
    ch := TLinkedChart(c).Chart;
    FillChar(sideUsed, SizeOf(sideUsed), 0);
    for al in TChartAxisAlignment do
      if (al in FAlignsides) and not sideUsed[al] then begin
        sideUsed[al] := true;
        axis := ch.AxisList.GetAxisByAlign(al);
        if axis <> nil then
          labelsize[al] := Max(labelsize[al], axis.MeasureLabelSize(ch.Drawer));
      end;
  end;

  for c in LinkedCharts do begin
    ch := TLinkedChart(c).Chart;
    FillChar(sideUsed, SizeOf(sideUsed), 0);
    for al in TChartAxisAlignment do begin
      axis := ch.AxisList.GetAxisByAlign(al);
      if (axis <> nil) then begin
        if (al in FAlignSides) and not sideUsed[al] then
          sideUsed[al] := true;
        if sideUsed[al] then
          axis.labelSize := labelSize[al]
        else
          axis.LabelSize := 0;
      end;
    end;
  end;
end;

procedure TChartExtentLink.SetAlignSides(AValue: TChartSides);
begin
  if AValue = FAlignSides then exit;
  FAlignSides := AValue;
  DoAlignSides;
end;

procedure TChartExtentLink.SyncWith(AChart: TChart);

  function CombineXY(const AX, AY: TDoubleRect): TDoubleRect;
  begin
    Result.a := DoublePoint(AX.a.X, AY.a.Y);
    Result.b := DoublePoint(AX.b.X, AY.b.Y);
  end;

var
  c: TCollectionItem;
  ch: TChart;
begin
  if (AChart = nil) then
    exit;

  if FEnabled then
    for c in LinkedCharts do begin
      ch := TLinkedChart(c).Chart;
      // Do not sync if the chart was never drawn yet.
      if (ch = nil) or (ch.LogicalExtent = EmptyExtent) then continue;
      // ZoomFull is lazy by default, so full extent may be not recalculated yet.
      if not ch.IsZoomed and (ch <> AChart) then
        ch.LogicalExtent := ch.GetFullExtent;
      // An event loop will be broken since setting LogicalExtent to
      // the same value does not initiale the extent broadcast.
      case Mode of
        elmXY:
          ch.LogicalExtent := AChart.LogicalExtent;
        elmOnlyX:
          ch.LogicalExtent := CombineXY(AChart.LogicalExtent, ch.LogicalExtent);
        elmOnlyY:
          ch.LogicalExtent := CombineXY(ch.LogicalExtent, AChart.LogicalExtent);
      end;
    end;

  // Re-align the chart sides if they are aligned and labels have become longer
  // or shorter upon synchronization.
  DoAlignSides;
end;

end.

