{
 /***************************************************************************
                               TAGraph.pas
                               -----------
                    Component Library Standard Graph


 ***************************************************************************/

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

Authors: Luís Rodrigues, Philippe Martinole, Alexander Klenin

}
unit TAGraph;

{$H+}

interface

uses
  LCLIntF, LCLType, LResources,
  SysUtils, Classes, Controls, Graphics, Dialogs,
  TAChartUtils, TATypes, TALegend, TAChartAxis;

type
  TChart = class;

  TReticuleMode = (rmNone, rmVertical, rmHorizontal, rmCross);

  TDrawReticuleEvent = procedure(
    ASender: TChart; ASeriesIndex, AIndex: Integer;
    const AData: TDoublePoint) of object;

  { TBasicChartSeries }

  TBasicChartSeries = class(TComponent)
  protected
    FActive: Boolean;
    FChart: TChart;
    FDepth: TChartDistance;
    FShowInLegend: Boolean;
    FTitle: String;
    FZPosition: TChartDistance;

    procedure AfterAdd; virtual;
    procedure AfterDraw; virtual;
    procedure BeforeDraw; virtual;
    procedure GetBounds(out ABounds: TDoubleRect); virtual; abstract;
    procedure GetGraphBounds(out ABounds: TDoubleRect); virtual; abstract;
    procedure GetLegendItems(AItems: TChartLegendItems); virtual; abstract;
    procedure SetActive(AValue: Boolean); virtual; abstract;
    procedure SetDepth(AValue: TChartDistance); virtual; abstract;
    procedure SetShowInLegend(AValue: Boolean); virtual; abstract;
    procedure SetZPosition(AValue: TChartDistance); virtual; abstract;
    procedure UpdateMargins(ACanvas: TCanvas; var AMargins: TRect); virtual;

  protected
    function AxisToGraphX(AX: Double): Double; virtual;
    function AxisToGraphY(AY: Double): Double; virtual;
    function GraphToAxisX(AX: Double): Double; virtual;
    function GraphToAxisY(AY: Double): Double; virtual;

  public
    destructor Destroy; override;

  public
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    function GetNearestPoint(
      ADistFunc: TPointDistFunc; const APoint: TPoint;
      out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
      virtual;
    function IsEmpty: Boolean; virtual; abstract;

    property Active: Boolean read FActive write SetActive default true;
    property Depth: TChartDistance read FDepth write SetDepth default 0;
    property ParentChart: TChart read FChart;
    property ShowInLegend: Boolean
      read FShowInLegend write SetShowInLegend default true;
    property Title: String read FTitle write FTitle;
    property ZPosition: TChartDistance read FZPosition write SetZPosition default 0;
  end;

  TSeriesClass = class of TBasicChartSeries;

  TChartToolEvent = procedure (AChart: TChart; AX, AY: Integer) of object;

  { TBasicСhartTool }

  TBasicChartTool = class(TComponent)
  strict protected
    FChart: TChart;

    procedure Activate; virtual;
    procedure Deactivate; virtual;
    function Index: Integer; virtual; abstract;
  end;

  TChartToolEventId = (evidMouseDown, evidMouseMove, evidMouseUp);

  { TBasicChartToolset }

  TBasicChartToolset = class(TComponent)
  protected
    function Dispatch(
      AChart: TChart; AEventId: TChartToolEventId;
      AShift: TShiftState; APoint: TPoint): Boolean; virtual; abstract;
  end;

  { TChartSeriesList }

  TChartSeriesList = class(TPersistent)
  private
    FList: TFPList;
    function GetItem(AIndex: Integer): TBasicChartSeries;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Clear;
    function Count: Integer;
  public
    property List: TFPList read FList;
    property Items[AIndex: Integer]: TBasicChartSeries read GetItem; default;
  end;

  { TChart }

  TChart = class(TCustomChart, ICoordTransformer)
  private // Property fields
    FAllowZoom: Boolean;
    FAxisList: TChartAxisList;
    FAxisVisible: Boolean;
    FBackColor: TColor;
    FDepth: TChartDistance;
    FExpandPercentage: Integer;
    FExtent: TChartExtent;
    FFoot: TChartTitle;
    FFrame: TChartPen;
    FGraphBrush: TBrush;
    FLegend: TChartLegend;
    FLogicalExtent: TDoubleRect;
    FMargins: TChartMargins;
    FOnDrawReticule: TDrawReticuleEvent;
    FSeries: TChartSeriesList;
    FTitle: TChartTitle;
    FToolset: TBasicChartToolset;

  private
    FActiveToolIndex: Integer;
    FBuiltinToolset: TBasicChartToolset;
    FClipRect: TRect;
    FCurrentExtent: TDoubleRect;
    FIsZoomed: Boolean;
    FOffset: TDoublePoint;   // Coordinates transformation
    FReticuleMode: TReticuleMode;
    FReticulePos: TPoint;
    FScale: TDoublePoint;    // Coordinates transformation

    procedure CalculateTransformationCoeffs(const AMargin: TRect);
    procedure DrawReticule(ACanvas: TCanvas);
    function GetAxis(AIndex: integer): TChartAxis; inline;
    function GetChartHeight: Integer;
    function GetChartWidth: Integer;
    function GetMargins(ACanvas: TCanvas): TRect;
    function GetSeriesCount: Integer;
    function GetToolset: TBasicChartToolset;
    procedure HideReticule;

    procedure SetAxis(AIndex: Integer; AValue: TChartAxis);
    procedure SetAxisList(AValue: TChartAxisList);
    procedure SetAxisVisible(Value: Boolean);
    procedure SetBackColor(const AValue: TColor);
    procedure SetDepth(AValue: TChartDistance);
    procedure SetExpandPercentage(AValue: Integer);
    procedure SetExtent(const AValue: TChartExtent);
    procedure SetFoot(Value: TChartTitle);
    procedure SetFrame(Value: TChartPen);
    procedure SetGraphBrush(Value: TBrush);
    procedure SetLegend(Value: TChartLegend);
    procedure SetLogicalExtent(const AValue: TDoubleRect);
    procedure SetMargins(AValue: TChartMargins);
    procedure SetReticuleMode(const AValue: TReticuleMode);
    procedure SetReticulePos(const AValue: TPoint);
    procedure SetTitle(Value: TChartTitle);
    procedure SetToolset(const AValue: TBasicChartToolset);
    procedure UpdateExtent;
  protected
    procedure Clean(ACanvas: TCanvas; ARect: TRect);
    procedure DisplaySeries(ACanvas: TCanvas);
    procedure DrawAxis(ACanvas: TCanvas);
    procedure DrawTitleFoot(ACanvas: TCanvas);
    procedure MouseDown(
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF LCLGtk2}
    procedure DoOnResize; override;
    {$ENDIF}
    procedure PrepareLegend(
      ACanvas: TCanvas; out ALegendItems: TChartLegendItems; out ARect: TRect);
    procedure StyleChanged(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;
    procedure Paint; override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;

  public // Helpers for series drawing
    procedure DrawLineHoriz(ACanvas: TCanvas; AY: Integer);
    procedure DrawLineVert(ACanvas: TCanvas; AX: Integer);
    procedure DrawOnCanvas(Rect: TRect; ACanvas: TCanvas); deprecated;
    function IsPointInViewPort(const AP: TDoublePoint): Boolean;

  public
    procedure AddSeries(ASeries: TBasicChartSeries);
    procedure ClearSeries;
    procedure CopyToClipboardBitmap;
    procedure DeleteSeries(ASeries: TBasicChartSeries);
    procedure PaintOnCanvas(ACanvas: TCanvas; ARect: TRect);
    procedure SaveToBitmapFile(const AFileName: String); inline;
    procedure SaveToFile(AClass: TRasterImageClass; const AFileName: String);
    function SaveToImage(AClass: TRasterImageClass): TRasterImage;
    procedure ZoomFull;
  public // Coordinate conversion
    function GraphToImage(const AGraphPoint: TDoublePoint): TPoint;
    function ImageToGraph(const APoint: TPoint): TDoublePoint;
    function XGraphToImage(AX: Double): Integer; inline;
    function XImageToGraph(AX: Integer): Double; inline;
    function YGraphToImage(AY: Double): Integer; inline;
    function YImageToGraph(AY: Integer): Double; inline;

  public
    property ActiveToolIndex: Integer read FActiveToolIndex;
    property ChartHeight: Integer read GetChartHeight;
    property ChartWidth: Integer read GetChartWidth;
    property ClipRect: TRect read FClipRect;
    property CurrentExtent: TDoubleRect read FCurrentExtent;
    property LogicalExtent: TDoubleRect read FLogicalExtent write SetLogicalExtent;
    property ReticulePos: TPoint read FReticulePos write SetReticulePos;
    property SeriesCount: Integer read GetSeriesCount;
    property XGraphMax: Double read FCurrentExtent.b.X;
    property XGraphMin: Double read FCurrentExtent.a.X;
    property YGraphMax: Double read FCurrentExtent.b.Y;
    property YGraphMin: Double read FCurrentExtent.a.Y;

  published
    property AllowZoom: Boolean read FAllowZoom write FAllowZoom default true;
    property AxisList: TChartAxisList read FAxisList write SetAxisList;
    property AxisVisible: Boolean read FAxisVisible write SetAxisVisible default true;
    property BackColor: TColor read FBackColor write SetBackColor default clBtnFace;
    property BottomAxis: TChartAxis index 1 read GetAxis write SetAxis stored false;
    property Depth: TChartDistance read FDepth write SetDepth default 0;
    property ExpandPercentage: Integer
      read FExpandPercentage write SetExpandPercentage default 0;
    property Extent: TChartExtent read FExtent write SetExtent;
    property Foot: TChartTitle read FFoot write SetFoot;
    property Frame: TChartPen read FFrame write SetFrame;
    property GraphBrush: TBrush read FGraphBrush write SetGraphBrush;
    property LeftAxis: TChartAxis index 2 read GetAxis write SetAxis stored false;
    property Legend: TChartLegend read FLegend write SetLegend;
    property Margins: TChartMargins read FMargins write SetMargins;
    property ReticuleMode: TReticuleMode
      read FReticuleMode write SetReticuleMode default rmNone;
    property Series: TChartSeriesList read FSeries;
    property Title: TChartTitle read FTitle write SetTitle;
    property Toolset: TBasicChartToolset read FToolset write SetToolset;

  published
    property OnDrawReticule: TDrawReticuleEvent
      read FOnDrawReticule write FOnDrawReticule;

  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color default clBtnFace;
    property DoubleBuffered;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

  published
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;
procedure RegisterSeriesClass(ASeriesClass: TSeriesClass; const ACaption: string);

var
  SeriesClassRegistry: TStringList;
  OnInitBuiltinTools: function(AChart: TChart): TBasicChartToolset;

implementation

uses
  Clipbrd, GraphMath, LCLProc, Math, Types;

function CompareZPosition(AItem1, AItem2: Pointer): Integer;
begin
  Result :=
    TBasicChartSeries(AItem1).ZPosition - TBasicChartSeries(AItem2).ZPosition;
end;

procedure Register;
var
  i: Integer;
  sc: TSeriesClass;
begin
  RegisterComponents(CHART_COMPONENT_IDE_PAGE, [TChart]);
  for i := 0 to SeriesClassRegistry.Count - 1 do begin
    sc := TSeriesClass(SeriesClassRegistry.Objects[i]);
    RegisterClass(sc);
    RegisterNoIcon([sc]);
  end;
end;

procedure RegisterSeriesClass(ASeriesClass: TSeriesClass; const ACaption: string);
begin
  if SeriesClassRegistry.IndexOfObject(TObject(ASeriesClass)) < 0 then
    SeriesClassRegistry.AddObject(ACaption, TObject(ASeriesClass));
end;

{ TChart }

constructor TChart.Create(AOwner: TComponent);
const
  DEFAULT_CHART_WIDTH = 300;
  DEFAULT_CHART_HEIGHT = 200;
  DEFAULT_CHART_TITLE = 'TAChart';
  FONT_VERTICAL = 900;
begin
  inherited Create(AOwner);

  FAllowZoom := true;
  FAxisVisible := true;

  Width := DEFAULT_CHART_WIDTH;
  Height := DEFAULT_CHART_HEIGHT;

  FReticulePos := Point(-1, -1);
  FReticuleMode := rmNone;

  FSeries := TChartSeriesList.Create;

  Color := clBtnFace;
  FBackColor := clBtnFace;

  FCurrentExtent := EmptyDoubleRect;

  FIsZoomed := false;

  FGraphBrush := TBrush.Create;
  FGraphBrush.OnChange := @StyleChanged;

  FLegend := TChartLegend.Create(Self);
  FTitle := TChartTitle.Create(Self);
  FTitle.Alignment := taCenter;
  FTitle.Text.Add(DEFAULT_CHART_TITLE);
  FFoot := TChartTitle.Create(Self);

  FAxisList := TChartAxisList.Create(Self);
  with TChartAxis.Create(FAxisList) do begin
    Alignment := calLeft;
    Title.Font.Orientation := FONT_VERTICAL;
  end;
  with TChartAxis.Create(FAxisList) do
    Alignment := calBottom;

  FFrame :=  TChartPen.Create;
  FFrame.OnChange := @StyleChanged;

  FExtent := TChartExtent.Create(Self);
  FMargins := TChartMargins.Create(Self);

  FBuiltinToolset := OnInitBuiltinTools(Self);
  FActiveToolIndex := -1;
end;

destructor TChart.Destroy;
begin
  FSeries.Free;
  FGraphBrush.Free;

  FLegend.Free;
  FTitle.Free;
  FFoot.Free;
  FAxisList.Free;
  FFrame.Free;
  FExtent.Free;
  FMargins.Free;
  FBuiltinToolset.Free;

  inherited Destroy;
end;

{$IFDEF LCLGtk2}
procedure TChart.DoOnResize;
begin
  inherited;
  // FIXME: GTK does not invalidate the control on resizing, do it manually
  Invalidate;
end;
{$ENDIF}

procedure TChart.EraseBackground(DC: HDC);
begin
  // do not erase, since we will paint over it anyway
  Unused(DC);
end;

function TChart.GetAxis(AIndex: integer): TChartAxis;
begin
  Result := FAxisList.GetAxis(AIndex);
end;

procedure TChart.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TChart.Paint;
begin
  PaintOnCanvas(Canvas, GetClientRect);
end;

procedure TChart.PaintOnCanvas(ACanvas: TCanvas; ARect: TRect);
var
  i: Integer;
  legendItems: TChartLegendItems = nil;
  legendRect: TRect;
begin
  Clean(ACanvas, ARect);

  FClipRect := ARect;
  InflateRect(FClipRect, -2, -2);

  for i := 0 to SeriesCount - 1 do
    Series[i].BeforeDraw;

  if not FIsZoomed then
    UpdateExtent;
  FLogicalExtent := FCurrentExtent;
  DrawTitleFoot(ACanvas);
  PrepareLegend(ACanvas, legendItems, legendRect);
  try
    DrawAxis(ACanvas);
    DisplaySeries(ACanvas);
    Legend.Draw(ACanvas, legendItems, legendRect);
  finally
    legendItems.Free;
  end;
  DrawReticule(ACanvas);

  for i := 0 to SeriesCount - 1 do
    Series[i].AfterDraw;
end;

procedure TChart.PrepareLegend(
  ACanvas: TCanvas; out ALegendItems: TChartLegendItems; out ARect: TRect);
var
  i: Integer;
begin
  if not Legend.Visible then exit;
  ALegendItems := TChartLegendItems.Create;
  try
    for i := 0 to SeriesCount - 1 do
      with Series[i] do
        if Active and ShowInLegend then
          GetLegendItems(ALegendItems);
    ARect := Legend.Prepare(ACanvas, ALegendItems, FClipRect);
  except
    FreeAndNil(ALegendItems);
    raise;
  end;
end;

procedure TChart.HideReticule;
begin
  // Hide reticule - - it will be drawn again in the next MouseMove.
  FReticulePos := Point( - 1, - 1);
end;

procedure TChart.CalculateTransformationCoeffs(const AMargin: TRect);
type
  TConvFunc = function (AX: Integer): Double of object;

  procedure CalcOneCoord(
    AAxis: TChartAxis; AConv: TConvFunc; var AGraphMin, AGraphMax: Double;
    AImageLo, AImageHi, AMarginLo, AMarginHi, ASign: Integer;
    out AScale, AOffset: Double);
  var
    lo, hi: Integer;
  begin
    lo := AImageLo + AMarginLo;
    hi := AImageHi + AMarginHi;

    if (AGraphMax = AGraphMin) or (Sign(hi - lo) <> ASign) then begin
      AScale := 1;
      AOffset := 0;
      exit;
    end;

    if (AAxis <> nil) and AAxis.Inverted then
      Exchange(lo, hi);

    AScale := (hi - lo) / (AGraphMax - AGraphMin);
    AOffset := hi - AScale * AGraphMax;
    AGraphMin := AConv(AImageLo);
    AGraphMax := AConv(AImageHi);;
    if (AAxis <> nil) and AAxis.Inverted then
      Exchange(AGraphMin, AGraphMax);
  end;

begin
  CalcOneCoord(
    BottomAxis, @XImageToGraph, FCurrentExtent.a.X, FCurrentExtent.b.X,
    FClipRect.Left, FClipRect.Right, AMargin.Left, -AMargin.Right, 1,
    FScale.X, FOffset.X);
  CalcOneCoord(
    LeftAxis, @YImageToGraph, FCurrentExtent.a.Y, FCurrentExtent.b.Y,
    FClipRect.Bottom, FClipRect.Top, -AMargin.Bottom, AMargin.Top, -1,
    FScale.Y, FOffset.Y);
end;

procedure TChart.Clean(ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := Color;
  ACanvas.Brush.Color := Color;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Rectangle(ARect);
end;

procedure TChart.ClearSeries;
begin
  FSeries.Clear;
  Invalidate;
end;

procedure TChart.DrawTitleFoot(ACanvas: TCanvas);

  function AlignedTextPos(AAlign: TAlignment; const AText: String): TSize;
  begin
    Result := ACanvas.TextExtent(AText);
    case AAlign of
      taLeftJustify:
        Result.cx := FClipRect.Left;
      taCenter:
        Result.cx := (FClipRect.Left + FClipRect.Right - Result.cx) div 2;
      taRightJustify:
        Result.cx := FClipRect.Right - Result.cx;
    end;
  end;

var
  sz: TSize;
  i: Integer;
  pbf: TPenBrushFontRecall;
begin
  pbf := TPenBrushFontRecall.Create(ACanvas, [pbfBrush, pbfFont]);
  try
    with FTitle do
      if Visible and (Text.Count > 0) then begin
        ACanvas.Brush.Assign(Brush);
        ACanvas.Font.Assign(Font);
        for i := 0 to Text.Count - 1 do begin
          sz := AlignedTextPos(Alignment, Text[i]);
          ACanvas.TextOut(sz.cx, FClipRect.Top, Text[i]);
          FClipRect.Top += sz.cy;
        end;
        FClipRect.Top += 4;
      end;
    with FFoot do
      if Visible and (Text.Count > 0) then begin
        ACanvas.Brush.Assign(Brush);
        ACanvas.Font.Assign(Font);
        for i := Text.Count - 1 downto 0 do begin
          sz := AlignedTextPos(Alignment, Text[i]);
          FClipRect.Bottom -= sz.cy;
          ACanvas.TextOut(sz.cx, FClipRect.Bottom, Text[i]);
        end;
        FClipRect.Bottom -= 4;
      end;
  finally
    pbf.Free;
  end;
end;

procedure TChart.DrawAxis(ACanvas: TCanvas);
var
  axisMargin: TChartAxisMargins = (0, 0, 0, 0);
  i: Integer;
  r: TRect;
  a: TChartAxisAlignment;
begin
  if not FAxisVisible then begin
    FClipRect.Left += Depth;
    FClipRect.Bottom -= Depth;
    exit;
  end;

  for i := 0 to AxisList.Count - 1 do
    AxisList[i].Measure(ACanvas, FCurrentExtent, axisMargin);
  axisMargin[calLeft] := Max(axisMargin[calLeft], Depth);
  axisMargin[calBottom] := Max(axisMargin[calBottom], Depth);
  for a := Low(a) to High(a) do
    SideByAlignment(FClipRect, a, -axisMargin[a]);

  CalculateTransformationCoeffs(GetMargins(ACanvas));

  // Background
  with ACanvas do begin
    if FFrame.Visible then
      Pen.Assign(FFrame)
    else
      Pen.Style := psClear;
    Brush.Color := BackColor;
    with FClipRect do
      Rectangle(Left, Top, Right + 1, Bottom + 1);
  end;

  r := FClipRect;
  for i := 0 to AxisList.Count - 1 do begin
    AxisList[i].Draw(ACanvas, FCurrentExtent, Self, r);
    AxisList[i].DrawTitle(ACanvas, CenterPoint(FClipRect), r);
  end;
  // Z axis
  if Depth > 0 then
    with FClipRect do
      ACanvas.Line(Left, Bottom, Left - Depth, Bottom + Depth);
end;

procedure TChart.DrawLineHoriz(ACanvas: TCanvas; AY: Integer);
begin
  if (FClipRect.Top < AY) and (AY < FClipRect.Bottom) then
    ACanvas.Line(FClipRect.Left, AY, FClipRect.Right, AY);
end;

procedure TChart.DrawLineVert(ACanvas: TCanvas; AX: Integer);
begin
  if (FClipRect.Left < AX) and (AX < FClipRect.Right) then
    ACanvas.Line(AX, FClipRect.Top, AX, FClipRect.Bottom);
end;

procedure TChart.SetReticuleMode(const AValue: TReticuleMode);
begin
  if FReticuleMode = AValue then exit;
  FReticuleMode := AValue;
  Invalidate;
end;

procedure TChart.SetReticulePos(const AValue: TPoint);
begin
  if FReticulePos = AValue then exit;
  DrawReticule(Canvas);
  FReticulePos := AValue;
  DrawReticule(Canvas);
end;

procedure TChart.SetTitle(Value: TChartTitle);
begin
  FTitle.Assign(Value);
  Invalidate;
end;

procedure TChart.SetToolset(const AValue: TBasicChartToolset);
begin
  if FToolset = AValue then exit;
  FToolset := AValue;
  FActiveToolIndex := -1;
end;

procedure TChart.SetFoot(Value: TChartTitle);
begin
  FFoot.Assign(Value);
  Invalidate;
end;


function TChart.GetMargins(ACanvas: TCanvas): TRect;
var
  i: Integer;
begin
  Result := FMargins.Data;
  for i := 0 to SeriesCount - 1 do
    if Series[i].Active then
      Series[i].UpdateMargins(ACanvas, Result);
end;

procedure TChart.SetGraphBrush(Value: TBrush);
begin
  FGraphBrush.Assign(Value);
end;

procedure TChart.AddSeries(ASeries: TBasicChartSeries);
begin
  DrawReticule(Canvas);
  Series.FList.Add(ASeries);
  ASeries.FChart := Self;
  ASeries.AfterAdd;
end;

procedure TChart.DeleteSeries(ASeries: TBasicChartSeries);
var
  i: Integer;
begin
  i := FSeries.FList.IndexOf(ASeries);
  if i < 0 then exit;
  FSeries.FList.Delete(i);
  ASeries.FChart := nil;
  Invalidate;
end;

function TChart.XGraphToImage(AX: Double): Integer;
begin
  Result := RoundChecked(FScale.X * AX + FOffset.X);
end;

function TChart.YGraphToImage(AY: Double): Integer;
begin
  Result := RoundChecked(FScale.Y * AY + FOffset.Y);
end;

function TChart.GraphToImage(const AGraphPoint: TDoublePoint): TPoint;
begin
  Result := Point(XGraphToImage(AGraphPoint.X), YGraphToImage(AGraphPoint.Y));
end;

function TChart.XImageToGraph(AX: Integer): Double;
begin
  Result := (AX - FOffset.X) / FScale.X;
end;

function TChart.YImageToGraph(AY: Integer): Double;
begin
  Result := (AY - FOffset.Y) / FScale.Y;
end;

function TChart.ImageToGraph(const APoint: TPoint): TDoublePoint;
begin
  Result.X := XImageToGraph(APoint.X);
  Result.Y := YImageToGraph(APoint.Y);
end;

function TChart.IsPointInViewPort(const AP: TDoublePoint): Boolean;
begin
  Result :=
    InRange(AP.X, XGraphMin, XGraphMax) and InRange(AP.Y, YGraphMin, YGraphMax);
end;

procedure TChart.SaveToBitmapFile(const AFileName: String);
begin
  SaveToFile(TBitmap, AFileName);
end;

procedure TChart.SaveToFile(AClass: TRasterImageClass; const AFileName: String);
begin
  with SaveToImage(AClass) do
    try
      SaveToFile(AFileName);
    finally
      Free;
    end;
end;

function TChart.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := AClass.Create;
  try
    Result.Width := Width;
    Result.Height := Height;
    PaintOnCanvas(Result.Canvas, Rect(0, 0, Width, Height));
  except
    Result.Free;
    raise;
  end;
end;

procedure TChart.SetAxis(AIndex: Integer; AValue: TChartAxis);
begin
  FAxisList.SetAxis(AIndex, AValue);
  Invalidate;
end;

procedure TChart.SetAxisList(AValue: TChartAxisList);
begin
  FAxisList.Assign(AValue);
  Invalidate;
end;

procedure TChart.CopyToClipboardBitmap;
begin
  with SaveToImage(TBitmap) do
    try
      SaveToClipboardFormat(RegisterClipboardFormat(MimeType));
    finally
      Free;
    end;
end;

procedure TChart.DrawOnCanvas(Rect: TRect; ACanvas: TCanvas);
begin
  PaintOnCanvas(ACanvas, Rect);
end;

procedure TChart.DisplaySeries(ACanvas: TCanvas);

  procedure OffsetDrawArea(AZPos, ADepth: Integer);
  begin
    FOffset.X -= AZPos;
    FOffset.Y += AZPos;
    OffsetRect(FClipRect, -AZPos, AZPos);
    FClipRect.Right += ADepth;
    FClipRect.Top -= ADepth;
  end;

var
  i, d: Integer;
  seriesInZOrder: TFPList;
begin
  if SeriesCount = 0 then exit;

  seriesInZOrder := TFPList.Create;
  try
    seriesInZOrder.Assign(FSeries.FList);
    seriesInZOrder.Sort(@CompareZPosition);

    d := Depth;
    for i := 0 to SeriesCount - 1 do
      with TBasicChartSeries(seriesInZOrder[i]) do begin
        if not Active then continue;
        OffsetDrawArea(Min(ZPosition, d), Min(Depth, d));
        ACanvas.ClipRect := FClipRect;
        ACanvas.Clipping := true;
        Draw(ACanvas);
        OffsetDrawArea(-Min(ZPosition, d), -Min(Depth, d));
        ACanvas.Clipping := false;
      end;
  finally
    seriesInZOrder.Free;
  end;
end;

procedure TChart.DrawReticule(ACanvas: TCanvas);
begin
  PrepareXorPen(ACanvas);
  if ReticuleMode in [rmVertical, rmCross] then
    DrawLineVert(ACanvas, FReticulePos.X);
  if ReticuleMode in [rmHorizontal, rmCross] then
    DrawLineHoriz(ACanvas, FReticulePos.Y);
end;

procedure TChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if
    PtInRect(FClipRect, Point(X, Y)) and
    GetToolset.Dispatch(Self, evidMouseDown, Shift, Point(X, Y))
  then
    exit;
  inherited;
end;

procedure TChart.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if GetToolset.Dispatch(Self, evidMouseMove, Shift, Point(X, Y)) then exit;
  inherited;
end;

procedure TChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GetToolset.Dispatch(Self, evidMouseUp, Shift, Point(X, Y)) then exit;
  inherited;
end;

procedure TChart.SetLegend(Value: TChartLegend);
begin
  FLegend.Assign(Value);
  Invalidate;
end;

procedure TChart.SetLogicalExtent(const AValue: TDoubleRect);
begin
  HideReticule;
  FLogicalExtent := AValue;
  FIsZoomed := true;
  FCurrentExtent := FLogicalExtent;
  Invalidate;
end;

procedure TChart.SetMargins(AValue: TChartMargins);
begin
  FMargins.Assign(AValue);
  Invalidate;
end;

procedure TChart.SetChildOrder(Child: TComponent; Order: Integer);
var
  i: Integer;
begin
  i := Series.FList.IndexOf(Child);
  if i >= 0 then
    Series.FList.Move(i, Order);
end;

procedure TChart.SetDepth(AValue: TChartDistance);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  Invalidate;
end;

procedure TChart.SetExpandPercentage(AValue: Integer);
begin
  if FExpandPercentage = AValue then exit;
  FExpandPercentage := AValue;
  Invalidate;
end;

procedure TChart.SetExtent(const AValue: TChartExtent);
begin
  FExtent.Assign(AValue);
  Invalidate;
end;

procedure TChart.SetFrame(Value: TChartPen);
begin
  FFrame.Assign(Value);
  Invalidate;
end;

procedure TChart.SetAxisVisible(Value: Boolean);
begin
  FAxisVisible := Value;
  Invalidate;
end; 

procedure TChart.SetBackColor(const AValue: TColor);
begin
  FBackColor:= AValue;
  Invalidate;
end;

function TChart.GetChartHeight: Integer;
begin
  Result := FClipRect.Right - FClipRect.Left;
end;

function TChart.GetChartWidth: Integer;
begin
  Result := FClipRect.Bottom - FClipRect.Top;
end;

procedure TChart.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
var
  i: Integer;
begin
  for i := 0 to SeriesCount - 1 do
   if Series[i].Owner = ARoot then
     AProc(Series[i]);
end;

function TChart.GetSeriesCount: Integer;
begin
  Result := FSeries.FList.Count;
end;

function TChart.GetToolset: TBasicChartToolset;
begin
  Result := FToolset;
  if Result = nil then
    Result := FBuiltinToolset;
end;

procedure TChart.UpdateExtent;

  procedure SetBounds(
    var ALo, AHi: Double; AMin, AMax: Double; AUseMin, AUseMax: Boolean);
  const
    DEFAULT_WIDTH = 2.0;
  begin
    if AUseMin then ALo := AMin;
    if AUseMax then AHi := AMax;
    case CASE_OF_TWO[ALo = Infinity, AHi = NegInfinity] of
      cotNone: begin // Both high and low boundary defined
        if ALo = AHi then begin
          ALo -= DEFAULT_WIDTH / 2;
          AHi += DEFAULT_WIDTH / 2;
        end
        else begin
          if ALo > AHi then Exchange(ALo, AHi);
          // Expand view slightly to avoid data points on the chart edge.
          ExpandRange(ALo, AHi, ExpandPercentage * PERCENT);
        end;
      end;
      cotFirst: ALo := AHi - DEFAULT_WIDTH;
      cotSecond: AHi := ALo + DEFAULT_WIDTH;
      cotBoth: begin // No boundaries defined, take some arbitrary values
        ALo := -DEFAULT_WIDTH / 2;
        AHi := DEFAULT_WIDTH / 2;
      end;
    end;
  end;

var
  i: Integer;
  seriesBounds: TDoubleRect;
  s: TBasicChartSeries;
begin
  Extent.CheckBoundsOrder;

  FCurrentExtent := EmptyExtent;
  for i := 0 to SeriesCount - 1 do begin
    s := Series[i];
    if not s.Active then continue;
    seriesBounds := EmptyExtent;
    s.GetGraphBounds(seriesBounds);
    with FCurrentExtent do begin
      a.X := Min(a.X, seriesBounds.a.X);
      b.X := Max(b.X, seriesBounds.b.X);
      a.Y := Min(a.Y, seriesBounds.a.Y);
      b.Y := Max(b.Y, seriesBounds.b.Y);
    end;
  end;
  with FCurrentExtent, Extent do begin
    SetBounds(a.X, b.X, XMin, XMax, UseXMin, UseXMax);
    SetBounds(a.Y, b.Y, YMin, YMax, UseYMin, UseYMax);
  end;
end;

procedure TChart.ZoomFull;
begin
  if not FIsZoomed then exit;
  HideReticule;
  FIsZoomed := false;
  Invalidate;
end;

{ TBasicChartSeries }

procedure TBasicChartSeries.AfterAdd;
begin
  // nothing
end;

procedure TBasicChartSeries.AfterDraw;
begin
  // empty
end;

function TBasicChartSeries.AxisToGraphX(AX: Double): Double;
begin
  Result := AX;
end;

function TBasicChartSeries.AxisToGraphY(AY: Double): Double;
begin
  Result := AY;
end;

procedure TBasicChartSeries.BeforeDraw;
begin
  // empty
end;

destructor TBasicChartSeries.Destroy;
begin
  if FChart <> nil then
    FChart.DeleteSeries(Self);
  inherited Destroy;
end;

function TBasicChartSeries.GetNearestPoint(
  ADistFunc: TPointDistFunc; const APoint: TPoint;
  out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
begin
  Unused(ADistFunc, APoint);
  AIndex := 0;
  AImg := Point(0, 0);
  AValue := ZeroDoublePoint;
  Result := false;
end;

function TBasicChartSeries.GraphToAxisX(AX: Double): Double;
begin
  Result := AX;
end;

function TBasicChartSeries.GraphToAxisY(AY: Double): Double;
begin
  Result := AY;
end;

procedure TBasicChartSeries.UpdateMargins(
  ACanvas: TCanvas; var AMargins: TRect);
begin
  // nothing
  Unused(ACanvas, AMargins);
end;

{ TChartSeriesList }

procedure TChartSeriesList.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Items[i].FChart := nil;
    Items[i].Free;
  end;
  FList.Clear;
end;

function TChartSeriesList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TChartSeriesList.Create;
begin
  FList := TFPList.Create;
end;

destructor TChartSeriesList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TChartSeriesList.GetItem(AIndex: Integer): TBasicChartSeries;
begin
  Result := TBasicChartSeries(FList.Items[AIndex]);
end;

{ TBasicChartTool }

procedure TBasicChartTool.Activate;
begin
  FChart.FActiveToolIndex := Index;
  FChart.MouseCapture := true;
end;

procedure TBasicChartTool.Deactivate;
begin
  FChart.MouseCapture := false;
  FChart.FActiveToolIndex := -1;
end;

procedure SkipObsoleteChartProperties;
const
  MIRRORX_NOTE = 'Obsolete, use BottomAxis.Invert instead';
  AXIS_COLOR_NOTE = 'Obsolete, use Axis.TickColor instead';
  ANGLE_NOTE = 'Obsolete, use Font.Orientation instead';
  NOTE = 'Obsolete, use Extent instead';
  NAMES: array [1..4] of String = (
    'XGraph', 'YGraph', 'AutoUpdateX', 'AutoUpdateY');
var
  i: Integer;
begin
  RegisterPropertyToSkip(TChart, 'MirrorX', MIRRORX_NOTE, '');
  RegisterPropertyToSkip(TChart, 'AxisColor', AXIS_COLOR_NOTE, '');
  RegisterPropertyToSkip(TChartAxisTitle, 'Angle', ANGLE_NOTE, '');
  for i := 1 to High(NAMES) do begin
    RegisterPropertyToSkip(TChart, NAMES[i] + 'Min', NOTE, '');
    RegisterPropertyToSkip(TChart, NAMES[i] + 'Max', NOTE, '');
  end;
end;

initialization
  {$I tagraph.lrs}
  SkipObsoleteChartProperties;
  SeriesClassRegistry := TStringList.Create;

finalization
  SeriesClassRegistry.Free;

end.
