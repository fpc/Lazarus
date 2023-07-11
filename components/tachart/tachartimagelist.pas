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

{$MODE ObjFPC}{$H+}

interface

uses
  LCLIntf, Classes, Graphics, Controls,
  TAChartUtils, TACustomSeries, TAGraph;

type
  TChartImageList = class(TImageList)
  private
    FChart: TChart;
    FChartPending: Boolean;
    FFirstSeriesIndex: Integer;
    FListener: TListener;
    FOnPopulate: TNotifyEvent;
    FSeriesCount: Integer;
    procedure SetChart(AValue: TChart);
  protected
    procedure ClearAllSeries;
    procedure Loaded; override;
    procedure Populate;
  public
    procedure DefineProperties(Filer: TFiler); override;
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
  Math, SysUtils, ImgList,
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

{ We don't want to write the series images to stream.
  They will be recreated automatically when the chart is assigned on loading. }
procedure TChartImageList.DefineProperties(Filer: TFiler);
var
  ch: TChart;
begin
  ch := FChart;
  SetChart(nil);  // This removes the series images
  inherited;
  SetChart(ch);
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

procedure TChartImageList.Loaded;
var
  ch: TChart;
begin
  inherited;
  if FChartPending then
  begin
    ch := FChart;
    FChart := nil;
    SetChart(ch);
    FChartPending := false;
  end;
end;

procedure TChartImageList.Populate;
var
  legendItems: TChartLegendItems = nil;
  res: TCustomImageListResolution;
  bmp: array of TCustomBitmap = nil;
  r: TRect;
  s: TCustomChartSeries;
  id: IChartDrawer;
  li: TLegendItem;
  i, n, idx: Integer;
begin
  ClearAllSeries;
  if FChart = nil then exit;

  FFirstSeriesIndex := Count;
  FSeriesCount := 0;

  legendItems := TChartLegendItems.Create;
  try
    for s in CustomSeries(FChart) do
      s.GetSingleLegendItem(legendItems);
    if ResolutionCount = 0 then
      n := 1
    else
      n := ResolutionCount;
    SetLength(bmp, n);
    for i := 0 to n-1 do
      bmp[i] := TBitmap.Create;
    try
      for li in legendItems do
      begin
        for i := 0 to n-1 do
        begin
          if ResolutionCount = 0 then
            r := Rect(0, 0, Width, Height)
          else
          begin
            res := ResolutionByIndex[i];
            r := Rect(0, 0, res.Width, res.Height);
          end;
          id := TCanvasDrawer.Create(bmp[i].Canvas);
          id.Pen := FChart.Legend.SymbolFrame;
          bmp[i].SetSize(r.Width, r.Height);
          bmp[i].Canvas.Brush.Style := bsSolid;
          bmp[i].Canvas.Brush.Color := BkColor;
          bmp[i].Canvas.Pen.Style := psSolid;
          bmp[i].Canvas.Pen.Width := 1;
          bmp[i].Transparent := true;
          bmp[i].TransparentMode := tmAuto;
          bmp[i].Canvas.FillRect(r);
          InflateRect(r, -1, -1);
          li.Draw(id, r);
        end;
        idx := AddMasked(TBitmap(bmp[0]), bmp[0].TransparentColor);
        for i := 1 to n-1 do
          ReplaceMasked(idx, bmp[i], bmp[i].TransparentColor, false);
        inc(FSeriesCount);
      end;
      if Assigned(FOnPopulate) then FOnPopulate(self);
    finally
      for i := 0 to high(bmp) do
        bmp[i].Free;
    end;
  finally
    legendItems.Free;
  end;
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
  if csLoading in ComponentState then
  begin
    // During lfm reading wait with assigning the chart until the static images
    // have been loaded.
    FChart := AValue;
    FChartPending := true;
    exit;
  end;

  if FListener.IsListening then
    FChart.Broadcaster.Unsubscribe(FListener);
  FChart := AValue;
  if FChart <> nil then
    FChart.Broadcaster.Subscribe(FListener);

  SeriesChanged(Self);
end;

end.

