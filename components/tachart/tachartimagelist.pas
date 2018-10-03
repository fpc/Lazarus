{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  An ImageList with TCustomChartSeries icons

  Authors: Werner Pamler, Alexander Klenin

  Notes:
  - The image list can be used like any other image list.
  - Assigning the Chart property to a TChart adds the series icons of all
    series to the image list. Series created at run-time will be added automatically
    to the end of the list.
  - Make sure to populate toolbar icons etc. before assigning the chart since the
    series images are added to the end of the list; otherwise image indices of
    these icons will change.
}

unit TAChartImageList;

{$H+}

interface

uses
  Classes, Graphics, Controls,
  TAChartUtils, TACustomSeries, TAGraph;

type
  TChartImageList = class(TImageList)
  private
    FChart: TChart;
    FFirstSeriesIndex: Integer;
    FListener: TListener;
    FOnPopulate: TNotifyEvent;
    FSeriesCount: Integer;
    procedure SetChart(AValue: TChart);
  protected
    procedure ClearAllSeries;
    procedure Populate;
  public
    procedure ReadData(AStream: TStream); override;
    procedure WriteData(AStream: TStream); override;
    procedure ReadAdvData(AStream: TStream); override;
    procedure WriteAdvData(AStream: TStream); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSeries(AImgIndex: Integer): TCustomChartSeries;
    function ImageIndexOfSeries(ASeries: TCustomChartSeries): Integer;
    procedure SeriesChanged(ASender: TObject);
    property FirstSeriesIndex: Integer read FFirstSeriesIndex;
    property SeriesCount: Integer read FSeriesCount;
  published
    property Chart: TChart read FChart write SetChart;
    property OnPopulate: TNotifyEvent read FOnPopulate write FOnPopulate;
  end;

procedure Register;

implementation

uses
  Math, SysUtils,
  TADrawUtils, TADrawerCanvas, TAEnumerators, TALegend;


procedure Register;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChartImageList]);
end;


{ TChartImageList }

procedure TChartImageList.ClearAllSeries;
var
  i: Integer;
begin
  if FFirstSeriesIndex < 0 then exit;
  for i := FFirstSeriesIndex + FSeriesCount - 1 downto FFirstSeriesIndex do
    Delete(i);
  FFirstSeriesIndex := -1;
  FSeriesCount := 0;
end;

constructor TChartImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListener := TListener.Create(@FChart, @SeriesChanged);
  FFirstSeriesIndex := -1;
  FSeriesCount := 0;
end;

destructor TChartImageList.Destroy;
begin
  FreeAndNil(FListener);
  inherited Destroy;
end;

function TChartImageList.GetSeries(AImgIndex: Integer): TCustomChartSeries;
begin
  Result := nil;
  AImgIndex -= FFirstSeriesIndex;
  if
    (FFirstSeriesIndex > -1) and (FChart <> nil) and
    InRange(AImgIndex, 0, FSeriesCount - 1)
  then
    Result := FChart.Series[AImgIndex] as TCustomChartSeries;
end;

function TChartImageList.ImageIndexOfSeries(ASeries: TCustomChartSeries): Integer;
begin
  Result := -1;
  if ASeries = nil then exit;
  for Result := 0 to Count - 1 do
    if GetSeries(Result) = ASeries then exit;
end;

procedure TChartImageList.Populate;
var
  legendItems: TChartLegendItems = nil;
  bmp: TBitmap;
  r: TRect;
  s: TCustomChartSeries;
  id: IChartDrawer;
  li: TLegendItem;
begin
  ClearAllSeries;
  if FChart = nil then exit;

  FFirstSeriesIndex := Count;
  FSeriesCount := 0;

  legendItems := TChartLegendItems.Create;
  bmp := TBitmap.Create;
  try
    bmp.Width := Width;
    bmp.Height := Height;
    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.Pen.Style := psSolid;
    bmp.Canvas.Pen.Width := 1;
    bmp.Transparent := true;
    bmp.TransparentMode := tmAuto;
    r := Rect(0, 0, Width, Height);
    id := TCanvasDrawer.Create(bmp.Canvas);
    id.Pen := FChart.Legend.SymbolFrame;
    for s in CustomSeries(FChart) do
      s.GetSingleLegendItem(legendItems);
    for li in legendItems do begin
      bmp.Canvas.Brush.Color := BkColor;
      bmp.Canvas.FillRect(r);
      li.Draw(id, R);
      AddMasked(bmp, bmp.TransparentColor);
      FSeriesCount += 1;
    end;
    if Assigned(FOnPopulate) then FOnPopulate(Self);
  finally
    FreeAndNil(legendItems);
    FreeAndNil(bmp);
  end;
end;

procedure TChartImageList.ReadAdvData(AStream: TStream);
begin
  Unused(AStream);
end;

procedure TChartImageList.ReadData(AStream: TStream);
var
  ch: TChart;
begin
  ch := Chart;
  Chart := nil;
  Clear;
  inherited ReadData(AStream);
  Chart := ch;
end;

// Notification procedure of the listener. Responds to chart broadcasts
// by populating the imagelist with the chart's series icons.
procedure TChartImageList.SeriesChanged(ASender:TObject);
begin
  Unused(ASender);
  Populate;
end;

procedure TChartImageList.SetChart(AValue:TChart);
begin
  if FChart = AValue then exit;

  if FListener.IsListening then
    FChart.Broadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.Broadcaster.Subscribe(FListener);

  SeriesChanged(Self);
end;

procedure TChartImageList.WriteAdvData(AStream: TStream);
begin
  Unused(AStream);
end;

procedure TChartImageList.WriteData(AStream: TStream);
var
  ch: TChart;
begin
  ch := Chart;
  Chart := nil;
  inherited WriteData(AStream);
  Chart := ch;{

  // Don't write the series images to stream.
  // They will be recreated automatically when the chart is assigned on loading.
  Unused(AStream);
  }
end;

end.

