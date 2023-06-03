unit TAChartExtentLink;

{$MODE ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, TAChartUtils, TAChartAxisUtils, TAGraph;

type
  TLinkedChart = class(TCollectionItem)
  strict private
    FChart: TChart;
    FListener: TListener;
    FClipRectListener: TListener;
    procedure OnClipRectChanged(ASender: TObject);
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
    FAlignMissingAxes: Boolean;
    procedure SetAlignSides(AValue: TChartSides);
  protected
    procedure DoAlignSides;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddChart(AChart: TChart);
    procedure RemoveChart(AChart: TChart);
    procedure SyncSides(AChart: TChart); virtual;
    procedure SyncWith(AChart: TChart);
  published
    property AlignMissingAxes: Boolean read FAlignMissingAxes write FAlignMissingAxes default true;
    property AlignSides: TChartSides read FAlignSides write SetAlignSides default [];
    property Enabled: Boolean read FEnabled write FEnabled default true;
    property LinkedCharts: TLinkedCharts read FLinkedCharts write FLinkedCharts;
    property Mode: TChartExtendLinkMode read FMode write FMode default elmXY;
  end;

procedure Register;

implementation

uses
  SysUtils, Math,
  TAGeometry, TAChartAxis;

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
  FClipRectListener := TListener.Create(@FChart, @OnClipRectChanged);
end;

destructor TLinkedChart.Destroy;
begin
  FreeAndNil(FListener);
  FreeAndNil(FClipRectListener);
  inherited;
end;

function TLinkedChart.GetDisplayName: String;
begin
  Result := inherited GetDisplayName;
  if Chart <> nil then
    Result += ' -> ' + Chart.Name;
end;

procedure TLinkedChart.OnClipRectChanged(ASender: TObject);
begin
  Unused(ASender);
  (Collection.Owner as TChartExtentLink).SyncSides(Chart);
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
  begin
    Chart.ExtentBroadcaster.Unsubscribe(FListener);
    Chart.ClipRectBroadcaster.Unsubscribe(FClipRectListener);
  end;

  FChart := AValue;

  if Chart <> nil then
  begin
    Chart.ExtentBroadcaster.Subscribe(FListener);
    Chart.ClipRectBroadcaster.Subscribe(FClipRectListener);
  end;
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

procedure TChartExtentLink.RemoveChart(AChart: TChart);
var
  i: TCollectionItem;
begin
  if AChart = nil then
    exit;

  for i in LinkedCharts do
    if TLinkedChart(i).Chart = AChart then
    begin
      LinkedCharts.Delete(i.Index);
      exit;
    end;
end;

constructor TChartExtentLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := true;
  FLinkedCharts := TLinkedCharts.Create(Self);
  FAlignMissingAxes := true;
end;

destructor TChartExtentLink.Destroy;
begin
  FreeAndNil(FLinkedCharts);
  inherited;
end;

// Note: ignores multiple axes on the same chart side
procedure TChartExtentLink.DoAlignSides;
var
  c: TCollectionItem;
  ch: TChart;
  maxLabelSize: array[TChartAxisAlignment] of Integer = (0, 0, 0, 0);
  maxTitleSize: array[TChartAxisAlignment] of Integer = (0, 0, 0, 0);
  titleSize: Integer;
  al: TChartAxisAlignment;
  axis: TChartAxis;
begin
  if FAlignMissingAxes then begin
    for c in LinkedCharts do begin
      ch := TLinkedChart(c).Chart;
      for al in TChartAxisAlignment do
        if (al in FAlignSides) then
        begin
          axis := ch.AxisList.GetAxisByAlign(al);
          if axis = nil then
          begin
            axis := ch.AxisList.Add;
            axis.Alignment := al;
            axis.Marks.Visible := false;
            axis.Title.Caption := '  ';
            axis.Title.Visible := true;
          end;
        end;
    end;
  end;

  for c in LinkedCharts do begin
    ch := TLinkedChart(c).Chart;
    for al in TChartAxisAlignment do
      if (al in FAlignsides) then begin
        axis := ch.AxisList.GetAxisByAlign(al);
        if axis <> nil then
        begin
          maxTitleSize[al] := Max(maxTitleSize[al], axis.MeasureTitleSize(ch.Drawer));
          maxLabelSize[al] := Max(maxLabelSize[al], axis.MeasureLabelSize(ch.Drawer));
        end;
      end;
  end;

  for c in LinkedCharts do begin
    ch := TLinkedChart(c).Chart;
    for al in TChartAxisAlignment do begin
      if (al in FAlignSides) then
      begin
        axis := ch.AxisList.GetAxisByAlign(al);
        if axis <> nil then
        begin
          titleSize := axis.MeasureTitleSize(ch.Drawer);
          axis.LabelSize := maxTitleSize[al] + maxLabelSize[al] - titleSize;
        end;
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

procedure TChartExtentLink.SyncSides(AChart: TChart);
begin
  Unused(AChart);
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

