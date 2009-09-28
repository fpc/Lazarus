{
 /***************************************************************************
                               TASeries.pas
                               ------------
                Component Library Standard Graph Series


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

unit TASeries;

{$H+}

interface

uses
  Classes, Graphics, SysUtils,
  TAGraph, TAChartUtils, TATypes, TASources;

type
  EBarError = class(EChartError);

  { TChartSeries }

  TChartSeries = class(TBasicChartSeries)
  private
    FBuiltinSource: TCustomChartSource;
    FListener: TListener;
    FMarks: TChartMarks;
    FSource: TCustomChartSource;

    function GetSource: TCustomChartSource;
    function GetXMaxVal: Integer;
    function IsSourceStored: boolean;
    procedure SetMarks(const AValue: TChartMarks);
    procedure SetSource(AValue: TCustomChartSource);
  protected
    procedure AfterAdd; override;
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    function ColorOrDefault(AColor: TColor; ADefault: TColor = clTAColor): TColor;
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    procedure GetCoords(AIndex: Integer; out AG: TDoublePoint; out AI: TPoint);
    function GetLegendCount: Integer; override;
    function GetLegendWidth(ACanvas: TCanvas): Integer; override;
    procedure SetActive(AValue: Boolean); override;
    procedure SetDepth(AValue: TChartDistance); override;
    procedure SetShowInLegend(AValue: Boolean); override;
    procedure SetZPosition(AValue: TChartDistance); override;
    procedure StyleChanged(Sender: TObject);
    procedure UpdateBounds(var ABounds: TDoubleRect); override;
    procedure UpdateParentChart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function Add(AValue: Double; XLabel: String; Color: TColor): Integer; inline;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor): Integer; virtual; overload;
    function AddXY(X, Y: Double): Integer; overload; inline;
    procedure Clear; inline;
    function Count: Integer; inline;
    procedure Delete(AIndex: Integer); virtual;
    function Extent: TDoubleRect; virtual;
    function FormattedMark(AIndex: integer): String;
    function IsEmpty: Boolean; override;
    function ListSource: TListChartSource;
    property Source: TCustomChartSource
      read GetSource write SetSource stored IsSourceStored;
  published
    property Active default true;
    property Marks: TChartMarks read FMarks write SetMarks;
    property ShowInLegend;
    property Title;
    property ZPosition;
  end;

  { TBasicPointSeries }

  TBasicPointSeries = class(TChartSeries)
  private
    FPrevLabelRect: TRect;
  protected
    procedure DrawLabel(
      ACanvas: TCanvas; AIndex: Integer; const ADataPoint: TPoint;
      ADown: Boolean);
    procedure DrawLabels(ACanvas: TCanvas; ADrawDown: Boolean);
    procedure UpdateMargins(ACanvas: TCanvas; var AMargins: TRect); override;
  end;

  { TBarSeries }

  TBarSeries = class(TBasicPointSeries)
  private
    FBarBrush: TBrush;
    FBarPen: TPen;
    FBarWidthPercent: Integer;

    function CalcBarWidth(AX: Double; AIndex: Integer): Double;
    procedure SetBarBrush(Value: TBrush);
    procedure SetBarPen(Value: TPen);
    procedure SetBarWidthPercent(Value: Integer);
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Draw(ACanvas: TCanvas); override;
    function Extent: TDoubleRect; override;
  published
    property BarBrush: TBrush read FBarBrush write SetBarBrush;
    property BarPen: TPen read FBarPen write SetBarPen;
    property BarWidthPercent: Integer
      read FBarWidthPercent write SetBarWidthPercent default 70;
    property Depth;
    property SeriesColor;
    property Source;
  end;

  { TPieSeries }

  TPieSeries = class(TChartSeries)
  protected
    procedure AfterAdd; override;
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetLegendCount: Integer; override;
    function GetLegendWidth(ACanvas: TCanvas): Integer; override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    function AddPie(Value: Double; Text: String; Color: TColor): Longint;
    procedure Draw(ACanvas: TCanvas); override;
  published
    property Source;
  end;

  { TAreaSeries }

  TAreaSeries = class(TBasicPointSeries)
  private
    FAreaBrush: TBrush;
    FAreaLinesPen: TChartPen;
    FInvertedStairs: Boolean;
    FStairs: Boolean;

    procedure SetAreaBrush(Value: TBrush);
    procedure SetInvertedStairs(Value: Boolean);
    procedure SetStairs(Value: Boolean);
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
  published
    property AreaBrush: TBrush read FAreaBrush write SetAreaBrush;
    property AreaLinesPen: TChartPen read FAreaLinesPen write FAreaLinesPen;
    property Depth;
    property InvertedStairs: Boolean
      read FInvertedStairs write SetInvertedStairs default false;
    property SeriesColor;
    property Source;
    property Stairs: Boolean read FStairs write SetStairs default false;
  end;

  { TBasicLineSeries }

  TBasicLineSeries  = class(TBasicPointSeries)
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
  end;

  TSeriesPointerDrawEvent = procedure (
    ASender: TChartSeries; ACanvas: TCanvas; AIndex: Integer;
    ACenter: TPoint) of object;

  { TLineSeries }

  TLineSeries = class(TBasicLineSeries)
  private
    FLinePen: TPen;
    FOnDrawPointer: TSeriesPointerDrawEvent;
    FPointer: TSeriesPointer;
    FShowLines: Boolean;
    FShowPoints: Boolean;

    procedure SetLinePen(AValue: TPen);
    procedure SetPointer(Value: TSeriesPointer);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowPoints(Value: Boolean);
  protected
    procedure AfterAdd; override;
    function GetNearestPoint(
      ADistFunc: TPointDistFunc; const APoint: TPoint;
      out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
      override;
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function  GetColor(AIndex: Integer): TColor;
    procedure GetMax(out X, Y: Double);
    procedure GetMin(out X, Y: Double);
    function  GetXImgValue(AIndex: Integer): Integer;
    function  GetXMax: Double;
    function  GetXMin: Double;
    function  GetXValue(AIndex: Integer): Double;
    function  GetYImgValue(AIndex: Integer): Integer;
    function  GetYMax: Double;
    function  GetYMin: Double;
    function  GetYValue(AIndex: Integer): Double;
    procedure SetColor(AIndex: Integer; AColor: TColor);
    procedure SetXValue(AIndex: Integer; AValue: Double); inline;
    procedure SetYValue(AIndex: Integer; AValue: Double); inline;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Depth;
    property LinePen: TPen read FLinePen write SetLinePen;
    property OnDrawPointer: TSeriesPointerDrawEvent
      read FOnDrawPointer write FOnDrawPointer;
    property Pointer: TSeriesPointer read FPointer write SetPointer;
    property SeriesColor;
    property ShowLines: Boolean read FShowLines write SetShowLines default true;
    property ShowPoints: Boolean read FShowPoints write SetShowPoints default false;
    property Source;
  end;

  // 'TSerie' alias is for compatibility with older versions of TAChart.
  // Do not use it.
  TSerie = TLineSeries;

  TLineStyle = (lsVertical, lsHorizontal);

  { TLine }

  TLine = class(TBasicLineSeries)
  private
    FLineStyle: TLineStyle;
    FPen: TPen;
    FPosGraph: Double;                      // Graph coordinates of line
    FUseBounds: Boolean;

    procedure SetLineStyle(AValue: TLineStyle);
    procedure SetPen(AValue: TPen);
    procedure SetPos(AValue: Double);
    procedure SetUseBounds(AValue: Boolean);
  protected
    function GetSeriesColor: TColor; override;
    procedure SetSeriesColor(const AValue: TColor); override;
    procedure UpdateBounds(var ABounds: TDoubleRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;

  published
    property LineStyle: TLineStyle
      read FLineStyle write SetLineStyle default lsHorizontal;
    property Pen: TPen read FPen write SetPen;
    property Position: Double read FPosGraph write SetPos;
    property SeriesColor;
    property UseBounds: Boolean read FUseBounds write SetUseBounds default true;
  end;

  TFuncCalculateEvent = procedure (const AX: Double; out AY: Double) of object;

  TFuncSeriesStep = 1..MaxInt;

  { TFuncSeries }

  TFuncSeries = class(TBasicChartSeries)
  private
    FExtent: TChartExtent;
    FOnCalculate: TFuncCalculateEvent;
    FPen: TChartPen;
    FStep: TFuncSeriesStep;

    procedure SetExtent(const AValue: TChartExtent);
    procedure SetOnCalculate(const AValue: TFuncCalculateEvent);
    procedure SetPen(const AValue: TChartPen);
    procedure SetStep(AValue: TFuncSeriesStep);
  protected
    procedure DrawLegend(ACanvas: TCanvas; const ARect: TRect); override;
    function GetLegendCount: Integer; override;
    function GetLegendWidth(ACanvas: TCanvas): Integer; override;
    function GetSeriesColor: TColor; override;
    procedure SetActive(AValue: Boolean); override;
    procedure SetDepth(AValue: TChartDistance); override;
    procedure SetSeriesColor(const AValue: TColor); override;
    procedure SetShowInLegend(AValue: Boolean); override;
    procedure SetZPosition(AValue: TChartDistance); override;
    procedure StyleChanged(Sender: TObject);
    procedure UpdateBounds(var ABounds: TDoubleRect); override;
    procedure UpdateParentChart;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas); override;
    function IsEmpty: Boolean; override;

  published
    property Active default true;
    property Extent: TChartExtent read FExtent write SetExtent;
    property OnCalculate: TFuncCalculateEvent read FOnCalculate write SetOnCalculate;
    property Pen: TChartPen read FPen write SetPen;
    property ShowInLegend;
    property Step: TFuncSeriesStep read FStep write SetStep default 2;
    property Title;
    property ZPosition;
  end;

implementation

uses
  GraphMath, Math, Types;

type

  { TChartSeriesListener }

  TChartSeriesListener = class(TListener)
  private
    FSeries: TChartSeries;
  public
    constructor Create(ASeries: TChartSeries);
    procedure Forget; override;
    procedure Notify; override;
  end;

{ TChartSeriesListener }

constructor TChartSeriesListener.Create(ASeries: TChartSeries);
begin
  FSeries := ASeries;
end;

procedure TChartSeriesListener.Forget;
begin
  inherited Forget;
  FSeries.FSource := nil;
end;

procedure TChartSeriesListener.Notify;
begin
  FSeries.UpdateParentChart;
end;

{ TChartSeries }

function TChartSeries.Add(AValue: Double; XLabel: String; Color: TColor): Integer;
begin
  Result := AddXY(GetXMaxVal + 1, AValue, XLabel, Color);
end;

function TChartSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor): Integer;
begin
  Result := ListSource.Add(X, Y, XLabel, Color);
end;

function TChartSeries.AddXY(X, Y: Double): Integer;
begin
  Result := AddXY(X, Y, '', clTAColor);
end;

procedure TChartSeries.AfterAdd;
begin
  FMarks.SetOwner(FChart);
end;

procedure TChartSeries.AfterDraw;
begin
  Source.AfterDraw;
end;

procedure TChartSeries.BeforeDraw;
begin
  Source.BeforeDraw;
end;

procedure TChartSeries.Clear;
begin
  ListSource.Clear;
end;

function TChartSeries.ColorOrDefault(AColor: TColor; ADefault: TColor): TColor;
begin
  Result := AColor;
  if Result <> clTAColor then exit;
  Result := ADefault;
  if Result <> clTAColor then exit;
  Result := SeriesColor;
end;

function TChartSeries.Count: Integer;
begin
  Result := Source.Count;
end;

constructor TChartSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActive := true;
  FListener := TChartSeriesListener.Create(Self);
  FBuiltinSource := TListChartSource.Create(Self);
  FBuiltinSource.Name := 'Builtin';
  FBuiltinSource.Subscribe(FListener);
  FMarks := TChartMarks.Create(FChart);
  FShowInLegend := true;
end;

procedure TChartSeries.Delete(AIndex: Integer);
begin
  ListSource.Delete(AIndex);
end;

destructor TChartSeries.Destroy;
begin
  if FListener.IsListening then
    Source.Unsubscribe(FListener);
  FBuiltinSource.Free;
  FMarks.Free;

  inherited Destroy;
end;

procedure TChartSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.TextOut(ARect.Right + 3, ARect.Top, Title);
end;

function TChartSeries.Extent: TDoubleRect;
begin
  Result := Source.Extent;
end;

function TChartSeries.FormattedMark(AIndex: integer): String;
var
  total, percent: Double;
begin
  total := Source.ValuesTotal;
  with Source[AIndex]^ do begin
    if total = 0 then
      percent := 0
    else
      percent := Y / total * 100;
    Result := Format(FMarks.Format, [y, percent, Text, total, X]);
  end;
end;

procedure TChartSeries.GetCoords(
  AIndex: Integer; out AG: TDoublePoint; out AI: TPoint);
begin
  AG := DoublePoint(Source[AIndex]^);
  AI := ParentChart.GraphToImage(AG);
end;

function TChartSeries.GetLegendCount: Integer;
begin
  Result := 1;
end;

function TChartSeries.GetLegendWidth(ACanvas: TCanvas): Integer;
begin
  Result := ACanvas.TextWidth(Title);
end;

function TChartSeries.GetSource: TCustomChartSource;
begin
  if Assigned(FSource) then
    Result := FSource
  else
    Result := FBuiltinSource;
end;

function TChartSeries.GetXMaxVal: Integer;
begin
  if Count > 0 then
    Result := Round(Source[Count - 1]^.X)
  else
    Result := 0;
end;

function TChartSeries.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TChartSeries.IsSourceStored: boolean;
begin
  Result := FSource <> nil;
end;

function TChartSeries.ListSource: TListChartSource;
begin
  if not (Source is TListChartSource) then
    raise EEditableSourceRequired.Create('Editable chart source required');
  Result := Source as TListChartSource;
end;

procedure TChartSeries.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetDepth(AValue: TChartDistance);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetMarks(const AValue: TChartMarks);
begin
  if FMarks = AValue then exit;
  FMarks.Assign(AValue);
end;

procedure TChartSeries.SetShowInLegend(AValue: Boolean);
begin
  if FShowInLegend = AValue then exit;
  FShowInLegend := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.SetSource(AValue: TCustomChartSource);
begin
  if FSource = AValue then exit;
  if FListener.IsListening then
    Source.Unsubscribe(FListener);
  FSource := AValue;
  Source.Subscribe(FListener);
  UpdateParentChart;
end;

procedure TChartSeries.SetZPosition(AValue: TChartDistance);
begin
  if FZPosition = AValue then exit;
  FZPosition := AValue;
  UpdateParentChart;
end;

procedure TChartSeries.StyleChanged(Sender: TObject);
begin
  UpdateParentChart;
end;

procedure TChartSeries.UpdateBounds(var ABounds: TDoubleRect);
begin
  if not Active or (Count = 0) then exit;
  with Extent do begin
    if a.X < ABounds.a.X then ABounds.a.X := a.X;
    if a.Y < ABounds.a.Y then ABounds.a.Y := a.Y;
    if b.X > ABounds.b.X then ABounds.b.X := b.X;
    if b.Y > ABounds.b.Y then ABounds.b.Y := b.Y;
  end;
end;

procedure TChartSeries.UpdateParentChart;
begin
  if ParentChart <> nil then ParentChart.Invalidate;
end;

{ TLineSeries }

constructor TLineSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLinePen := TPen.Create;
  FLinePen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(FChart);
  FShowLines := true;
end;

destructor TLineSeries.Destroy;
begin
  FPointer.Free;
  inherited Destroy;
end;

procedure TLineSeries.SetPointer(Value: TSeriesPointer);
begin
  FPointer.Assign(Value);
  UpdateParentChart;
end;

procedure TLineSeries.SetSeriesColor(const AValue: TColor);
begin
  FLinePen.Color := AValue;
end;

procedure TLineSeries.Draw(ACanvas: TCanvas);
var
  i: Integer;
  ai, bi: TPoint;
  a, b: TDoublePoint;
begin
  if FShowLines then
    for i := 0 to Count - 2 do begin
      a := DoublePoint(Source[i]^);
      b := DoublePoint(Source[i + 1]^);
      if not LineIntersectsRect(a, b, ParentChart.CurrentExtent) then continue;
      ai := ParentChart.GraphToImage(a);
      bi := ParentChart.GraphToImage(b);
      ACanvas.Pen.Assign(LinePen);
      if Depth = 0 then
        ACanvas.Line(ai, bi)
      else begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := ACanvas.Pen.Color;
        ACanvas.Pen.Color := clBlack;
        DrawLineDepth(ACanvas, ai, bi, Depth);
      end;
    end;

  DrawLabels(ACanvas, true);

  if FShowPoints then
    for i := 0 to Count - 1 do begin
      a := DoublePoint(Source[i]^);
      if not ParentChart.IsPointInViewPort(a) then continue;
      ai := ParentChart.GraphToImage(a);
      FPointer.Draw(ACanvas, ai, GetColor(i));
      if Assigned(FOnDrawPointer) then
        FOnDrawPointer(Self, ACanvas, i, ai);
    end;
end;

procedure TLineSeries.AfterAdd;
begin
  inherited AfterAdd;
  FPointer.SetOwner(FChart);
end;

function TLineSeries.GetXValue(AIndex: Integer): Double;
begin
  Result := Source[AIndex]^.X;
end;

function TLineSeries.GetYValue(AIndex: Integer): Double;
begin
  Result := Source[AIndex]^.Y;
end;

procedure TLineSeries.SetXValue(AIndex: Integer; AValue: Double);
begin
  ListSource.SetXValue(AIndex, AValue);
end;

procedure TLineSeries.SetYValue(AIndex: Integer; AValue: Double);
begin
  ListSource.SetYValue(AIndex, AValue);
end;

function TLineSeries.GetXImgValue(AIndex: Integer): Integer;
begin
  Result := ParentChart.XGraphToImage(Source[AIndex]^.X);
end;

function TLineSeries.GetYImgValue(AIndex: Integer): Integer;
begin
  Result := ParentChart.YGraphToImage(Source[AIndex]^.Y);
end;

function TLineSeries.GetXMin: Double;
begin
  Result := Extent.a.X;
end;

function TLineSeries.GetXMax: Double;
begin
  Result := Extent.b.X;
end;

function TLineSeries.GetYMin: Double;
begin
  Result := Extent.a.Y;
end;

function TLineSeries.GetYMax: Double;
begin
  Result := Extent.b.Y;
end;

procedure TLineSeries.GetMax(out X, Y: Double);
begin
  X := Source.XOfMax;
  Y := Extent.b.Y;
end;

procedure TLineSeries.GetMin(out X, Y: Double);
begin
  X := Source.XOfMin;
  Y := Extent.a.Y;
end;

function TLineSeries.GetNearestPoint(
  ADistFunc: TPointDistFunc; const APoint: TPoint;
  out AIndex: Integer; out AImg: TPoint; out AValue: TDoublePoint): Boolean;
var
  dist, minDist, i: Integer;
  pt: TPoint;
begin
  Result := Count > 0;
  minDist := MaxInt;
  for i := 0 to Count - 1 do begin
    pt := Point(GetXImgValue(i), GetYImgValue(i));
    dist := ADistFunc(APoint, pt);
    if dist >= minDist then
      Continue;
    minDist := dist;
    AIndex := i;
    AImg := pt;
    AValue.X := GetXValue(i);
    AValue.Y := GetYValue(i);
  end;
end;

function TLineSeries.GetSeriesColor: TColor;
begin
  Result := FLinePen.Color;
end;

procedure TLineSeries.SetColor(AIndex: Integer; AColor: TColor);
begin
  Source[AIndex]^.Color := AColor;
end;

procedure TLineSeries.SetLinePen(AValue: TPen);
begin
  FLinePen.Assign(AValue);
end;

function TLineSeries.GetColor(AIndex: Integer): TColor;
begin
  Result := ColorOrDefault(Source[AIndex]^.Color);
end;

procedure TLineSeries.SetShowPoints(Value: Boolean);
begin
  FShowPoints := Value;
  UpdateParentChart;
end;

procedure TLineSeries.SetShowLines(Value: Boolean);
begin
  FShowLines := Value;
  UpdateParentChart;
end;

procedure TLineSeries.BeginUpdate;
begin
  ListSource.BeginUpdate;
end;

procedure TLineSeries.EndUpdate;
begin
  ListSource.EndUpdate;
  UpdateParentChart;
end;

{ TLine }

constructor TLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLineStyle := lsHorizontal;
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FUseBounds := true;
end;

destructor TLine.Destroy;
begin
  inherited Destroy;
  FPen.Free;
end;

procedure TLine.SetLineStyle(AValue: TLineStyle);
begin
  if FLineStyle = AValue then exit;
  FLineStyle := AValue;
  UpdateParentChart;
end;

procedure TLine.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
end;

procedure TLine.SetUseBounds(AValue: Boolean);
begin
  if FUseBounds = AValue then exit;
  FUseBounds := AValue;
  UpdateParentChart;
end;

procedure TLine.UpdateBounds(var ABounds: TDoubleRect);
begin
  if not UseBounds then exit;
  case LineStyle of
    lsHorizontal: UpdateMinMax(FPosGraph, ABounds.a.Y, ABounds.b.Y);
    lsVertical: UpdateMinMax(FPosGraph, ABounds.a.X, ABounds.b.X);
  end;
end;

procedure TLine.SetPos(AValue: Double);
begin
  if FPosGraph = AValue then exit;
  FPosGraph := AValue;
  UpdateParentChart;
end;

procedure TLine.SetSeriesColor(const AValue: TColor);
begin
  if FPen.Color = AValue then exit;
  FPen.Color := AValue;
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Assign(FPen);

  with ParentChart do
    case LineStyle of
      lsHorizontal:
        DrawLineHoriz(ACanvas, YGraphToImage(FPosGraph));
      lsVertical:
        DrawLineVert(ACanvas, XGraphToImage(FPosGraph));
    end;
end;

function TLine.GetSeriesColor: TColor;
begin
  Result := FPen.Color;
end;

{ TBasicPointSeries }

procedure TBasicPointSeries.DrawLabel(
  ACanvas: TCanvas; AIndex: Integer; const ADataPoint: TPoint; ADown: Boolean);
var
  labelRect: TRect;
  dummy: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);
  labelText: String;
  labelSize: TSize;
begin
  labelText := FormattedMark(AIndex);
  if labelText = '' then exit;

  labelSize := ACanvas.TextExtent(labelText);
  labelRect.Left := ADataPoint.X - labelSize.cx div 2;
  if ADown then
    labelRect.Top := ADataPoint.Y + Marks.Distance
  else
    labelRect.Top := ADataPoint.Y - Marks.Distance - labelSize.cy;
  labelRect.BottomRight := labelRect.TopLeft + labelSize;
  InflateRect(labelRect, MARKS_MARGIN_X, MARKS_MARGIN_Y);
  if
    not IsRectEmpty(FPrevLabelRect) and
    IntersectRect(dummy, labelRect, FPrevLabelRect)
  then
    exit;
  FPrevLabelRect := labelRect;

  // Link between the label and the bar.
  ACanvas.Pen.Assign(Marks.LinkPen);
  with ADataPoint do
    if ADown then
      ACanvas.Line(X, Y, X, labelRect.Top)
    else
      ACanvas.Line(X, Y - 1, X, labelRect.Bottom - 1);

  Marks.DrawLabel(ACanvas, labelRect, labelText);
end;

procedure TBasicPointSeries.DrawLabels(ACanvas: TCanvas; ADrawDown: Boolean);
var
  g: TDoublePoint;
  pt: TPoint;
  i: Integer;
begin
  if not Marks.IsMarkLabelsVisible then exit;
  for i := 0 to Count - 1 do begin
    GetCoords(i, g, pt);
    with ParentChart do
      if IsPointInViewPort(g) then
        DrawLabel(ACanvas, i, pt, ADrawDown and (g.Y < 0));
  end;
end;

procedure TBasicPointSeries.UpdateMargins(ACanvas: TCanvas; var AMargins: TRect);
var
  h: Integer;
begin
  if not Marks.IsMarkLabelsVisible then exit;
  h := ACanvas.TextHeight('0') + Marks.Distance + 2 * MARKS_MARGIN_Y + 4;
  AMargins.Top := Max(AMargins.Top, h);
  AMargins.Bottom := Max(AMargins.Bottom, h);
  FPrevLabelRect := Rect(0, 0, 0, 0);
end;

{ TBarSeries }

function TBarSeries.CalcBarWidth(AX: Double; AIndex: Integer): Double;
begin
  case CASE_OF_TWO[AIndex > 0, AIndex < Count - 1] of
    cotNone: Result := 1.0;
    cotFirst: Result := Abs(AX - Source[AIndex - 1]^.X);
    cotSecond: Result := Abs(AX - Source[AIndex + 1]^.X);
    cotBoth: Result := Min(
      Abs(AX - Source[AIndex - 1]^.X),
      Abs(AX - Source[AIndex + 1]^.X));
  end;
  Result *= FBarWidthPercent * PERCENT / 2;
end;

constructor TBarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarWidthPercent := 70; //70%

  FBarBrush := TBrush.Create;
  FBarBrush.OnChange := @StyleChanged;

  FBarPen := TPen.Create;
  FBarPen.OnChange := @StyleChanged;
  FBarPen.Mode := pmCopy;
  FBarPen.Style := psSolid;
  FBarPen.Width := 1;
  FBarPen.Color := clBlack;
  FBarBrush.Color := clRed;
end;

destructor TBarSeries.Destroy;
begin
  FBarPen.Free;
  FBarBrush.Free;
  inherited Destroy;
end;

procedure TBarSeries.SetBarBrush(Value: TBrush);
begin
  FBarBrush.Assign(Value);
end;

procedure TBarSeries.SetBarPen(Value:TPen);
begin
  FBarPen.Assign(Value);
end;

procedure TBarSeries.SetBarWidthPercent(Value: Integer);
begin
  if (Value < 1) or (Value > 100) then
    raise EBarError.Create('Wrong BarWidth Percent');
  FBarWidthPercent := Value;
end;

procedure TBarSeries.SetSeriesColor(const AValue: TColor);
begin
  FBarBrush.Color := AValue;
end;

procedure TBarSeries.Draw(ACanvas: TCanvas);

  procedure DrawBar(const AR: TRect);
  var
    sz: TSize;
  begin
    sz := Size(AR);
    if (sz.cx > 2) and (sz.cy > 2) then
      ACanvas.Pen.Assign(BarPen)
    else begin
      // Bars are too small to distinguish border from interior.
      ACanvas.Pen.Color := ACanvas.Brush.Color;
      ACanvas.Pen.Style := psSolid;
    end;

    ACanvas.Rectangle(AR);

    if Depth = 0 then exit;
    DrawLineDepth(ACanvas, AR.Left, AR.Top, AR.Right - 1, AR.Top, Depth);
    DrawLineDepth(
      ACanvas, AR.Right - 1, AR.Top, AR.Right - 1, AR.Bottom - 1, Depth);
  end;

var
  i: Integer;
  ext2, graphBar: TDoubleRect;
  imageBar: TRect;
  w: Double;
  p: TDoublePoint;
begin
  if IsEmpty then exit;

  ext2 := ParentChart.CurrentExtent;
  ExpandRange(ext2.a.X, ext2.b.X, 1.0);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  ACanvas.Brush.Assign(BarBrush);
  for i := 0 to Count - 1 do begin
    p := DoublePoint(Source[i]^);
    w := CalcBarWidth(p.X, i);
    graphBar := DoubleRect(p.X - w, 0, p.X + w, p.Y);
    if not RectIntersectsRect(graphBar, ext2) then continue;

    with imageBar do begin
      TopLeft := ParentChart.GraphToImage(graphBar.a);
      BottomRight := ParentChart.GraphToImage(graphBar.b);
      NormalizeRect(imageBar);

      // Draw a line instead of an empty rectangle.
      if Bottom = Top then Dec(Top);
      if Left = Right then Inc(Right);
    end;
    ACanvas.Brush.Color := ColorOrDefault(Source[i]^.Color);
    DrawBar(imageBar);
  end;

  if not Marks.IsMarkLabelsVisible then exit;
  for i := 0 to Count - 1 do begin
    p := DoublePoint(Source[i]^);
    if ParentChart.IsPointInViewPort(p) then
      DrawLabel(ACanvas, i, ParentChart.GraphToImage(p), p.Y < 0);
  end;
end;

procedure TBarSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  inherited DrawLegend(ACanvas, ARect);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Assign(BarBrush);
  ACanvas.Rectangle(ARect);
end;

function TBarSeries.Extent: TDoubleRect;
begin
  Result := inherited Extent;
  if IsEmpty then exit;
  UpdateMinMax(0, Result.a.Y, Result.b.Y);
  // Show first and last bars fully.
  with Source[0]^ do
    Result.a.X := Min(Result.a.X, X - CalcBarWidth(X, 0));
  with Source[Count - 1]^ do
    Result.b.X := Max(Result.b.X, X + CalcBarWidth(X, Count - 1));
end;

function TBarSeries.GetSeriesColor: TColor;
begin
  Result := FBarBrush.Color;
end;

{ TPieSeries }

function TPieSeries.AddPie(Value: Double; Text: String; Color: TColor): Longint;
begin
  Result := AddXY(GetXMaxVal + 1, Value, Text, Color);
end;

procedure TPieSeries.AfterAdd;
begin
  // disable axis when we have TPie series
  ParentChart.LeftAxis.Visible := false;
  ParentChart.BottomAxis.Visible := false;
end;

procedure TPieSeries.Draw(ACanvas: TCanvas);
var
  i, radius: Integer;
  prevAngle, angleStep: Double;
  graphCoord: PChartDataItem;
  labelWidths, labelHeights: TIntegerDynArray;
  labelTexts: TStringDynArray;
  a, b, center: TPoint;
  r: TRect;
const
  MARGIN = 8;
begin
  if IsEmpty then exit;

  SetLength(labelWidths, Count);
  SetLength(labelHeights, Count);
  SetLength(labelTexts, Count);
  for i := 0 to Count - 1 do begin
    labelTexts[i] := FormattedMark(i);
    with ACanvas.TextExtent(labelTexts[i]) do begin
      labelWidths[i] := cx;
      labelHeights[i] := cy;
    end;
  end;

  with ParentChart do begin
    center := CenterPoint(ClipRect);
    // Reserve space for labels.
    radius := Min(
      ClipRect.Right - center.x - MaxIntValue(labelWidths),
      ClipRect.Bottom - center.y - MaxIntValue(labelHeights));
  end;
  if Marks.IsMarkLabelsVisible then
    radius -= Marks.Distance;
  radius := Max(radius - MARGIN, 0);

  prevAngle := 0;
  for i := 0 to Count - 1 do begin
    // if y < 0 then y := -y;
    // if y = 0 then y := 0.1; // just to simulate tchart when y=0

    graphCoord := Source[i];
    angleStep := graphCoord^.Y / Source.ValuesTotal * 360 * 16;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color :=
      ColorOrDefault(graphCoord^.Color, Colors[i mod MaxColor + 1]);

    ACanvas.RadialPie(
      center.x - radius, center.y - radius,
      center.x + radius, center.y + radius, round(prevAngle), round(angleStep));

    prevAngle += angleStep;

    if not Marks.IsMarkLabelsVisible then continue;

    a := LineEndPoint(center, prevAngle - angleStep / 2, radius);
    b := LineEndPoint(center, prevAngle - angleStep / 2, radius + Marks.Distance);

    // line from mark to pie
    ACanvas.Pen.Assign(Marks.LinkPen);
    ACanvas.Line(a, b);

    if b.x < center.x then
      b.x -= labelWidths[i];
    if b.y < center.y then
      b.y -= labelHeights[i];

    r := Rect(b.x, b.y, b.x + labelWidths[i], b.y + labelHeights[i]);
    InflateRect(r, MARKS_MARGIN_X, MARKS_MARGIN_Y);
    Marks.DrawLabel(ACanvas, r, labelTexts[i]);
  end;
end;

procedure TPieSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
var
  i: Integer;
  pc, bc: TColor;
  r: TRect;
begin
  r := ARect;
  pc := ACanvas.Pen.Color;
  bc := ACanvas.Brush.Color;
  for i := 0 to Count - 1 do begin
    ACanvas.Pen.Color := pc;
    ACanvas.Brush.Color := bc;
    with Source[i]^ do begin
      ACanvas.TextOut(r.Right + 3, r.Top, Format('%1.2g %s', [Y, Text]));
      ACanvas.Pen.Color := clBlack;
      ACanvas.Brush.Color := Color;
    end;
    ACanvas.Rectangle(r);
    OffsetRect(r, 0, r.Bottom - r.Top + LEGEND_SPACING);
  end;
end;

function TPieSeries.GetLegendCount: Integer;
begin
  Result := Count;
end;

function TPieSeries.GetLegendWidth(ACanvas: TCanvas): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    with Source[i]^ do
      Result := Max(ACanvas.TextWidth(Format('%1.2g %s', [Y, Text])), Result);
end;

function TPieSeries.GetSeriesColor: TColor;
begin
  Result := clBlack; // SeriesColor is meaningless for PieSeries
end;

procedure TPieSeries.SetSeriesColor(const AValue: TColor);
begin
  // SeriesColor is meaningless for PieSeries
  Unused(AValue);
end;

{ TAreaSeries }

constructor TAreaSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAreaLinesPen := TChartPen.Create;
  FAreaBrush := TBrush.Create;
end;

destructor TAreaSeries.Destroy;
begin
  FAreaLinesPen.Free;
  FAreaBrush.Free;
  inherited Destroy;
end;

procedure TAreaSeries.SetAreaBrush(Value: TBrush);
begin
  FAreaBrush.Assign(Value);
  UpdateParentChart;
end;

procedure TAreaSeries.SetStairs(Value: Boolean);
begin
  FStairs := Value;
  UpdateParentChart;
end;

procedure TAreaSeries.SetInvertedStairs(Value: Boolean);
begin
  FInvertedStairs := Value;
  UpdateParentChart;
end;

procedure TAreaSeries.SetSeriesColor(const AValue: TColor);
begin
  FAreaBrush.Color := AValue;
end;

procedure TAreaSeries.Draw(ACanvas: TCanvas);
var
  pts: array [0..4] of TPoint;
  numPts: Integer;

  procedure PushPoint(const A: TPoint);
  begin
    pts[numPts] := A;
    Inc(numPts);
  end;

var
  i, ax, bx, ymin: Integer;
  a, b: TDoublePoint;
  ext, ext2: TDoubleRect;
  imageRect: TRect;
begin
  if Count = 0 then exit;

  ACanvas.Brush.Assign(AreaBrush);
  ACanvas.Pen.Assign(AreaLinesPen);

  ext := ParentChart.CurrentExtent;
  ext2 := ext;
  ExpandRange(ext2.a.X, ext2.b.X, 0.1);
  ExpandRange(ext2.a.Y, ext2.b.Y, 1.0);

  ymin := ParentChart.ClipRect.Bottom - 1;

  for i := 0 to Count - 2 do begin
    a := DoublePoint(Source[i]^);
    b := DoublePoint(Source[i + 1]^);
    if a.X > b.X then begin
      Exchange(a.X, b.X);
      Exchange(a.Y, b.Y);
    end;
    if (a.X > ext.b.X) or (b.X < ext.a.X) then continue;
    if Stairs then begin
      if InvertedStairs then
        a.Y := b.Y
      else
        b.Y := a.Y;
    end;
    ax := ParentChart.XGraphToImage(Max(a.X, ext2.a.X));
    bx := ParentChart.XGraphToImage(Min(b.X, ext2.b.X));

    if LineIntersectsRect(a, b, ext2) then begin
      numPts := 0;
      PushPoint(Point(ax, ymin));
      if a.Y = ext2.b.Y then
        PushPoint(Point(ax, ParentChart.YGraphToImage(a.Y)));
      PushPoint(ParentChart.GraphToImage(a));
      PushPoint(ParentChart.GraphToImage(b));
      if b.Y = ext2.b.Y then
        PushPoint(Point(bx, ParentChart.YGraphToImage(b.Y)));
      PushPoint(Point(bx, ymin));
      ACanvas.Polygon(pts, false, 0, numPts);
    end
    else begin
      if a.Y > ext.b.Y then begin
        imageRect := Rect(ax, ParentChart.ClipRect.Top - 1, bx, ymin);
        NormalizeRect(imageRect);
        ACanvas.Rectangle(imageRect);
      end;
    end;
  end;
  DrawLabels(ACanvas, false);
end;

procedure TAreaSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
begin
  inherited DrawLegend(ACanvas, ARect);
  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := SeriesColor;
  ACanvas.Rectangle(ARect);
end;

function TAreaSeries.GetSeriesColor: TColor;
begin
  Result := FAreaBrush.Color;
end;

{ TBasicLineSeries }

procedure TBasicLineSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
var
  y: Integer;
begin
  inherited DrawLegend(ACanvas, ARect);
  ACanvas.Pen.Color := SeriesColor;
  y := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.Line(ARect.Left, y, ARect.Right, y);
end;

{ TFuncSeries }

constructor TFuncSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := true;
  FExtent := TChartExtent.Create(FChart);
  FShowInLegend := true;
  FPen := TChartPen.Create;
  FPen.OnChange := @StyleChanged;
  FStep := 2;
end;

destructor TFuncSeries.Destroy;
begin
  FExtent.Free;
  FPen.Free;
  inherited Destroy;
end;

procedure TFuncSeries.Draw(ACanvas: TCanvas);

  function CalcY(AX: Integer): Integer;
  var
    yg: Double;
  begin
    OnCalculate(FChart.XImageToGraph(AX), yg);
    if Extent.UseYMin and (yg < Extent.YMin) then
      yg := Extent.YMin;
    if Extent.UseYMax and (yg > Extent.YMax) then
      yg := Extent.YMax;
    Result := FChart.YGraphToImage(yg);
  end;

var
  x, xmax: Integer;
begin
  if not Assigned(OnCalculate) then exit;

  x := FChart.ClipRect.Left;
  if Extent.UseXMin then
    x := Max(FChart.XGraphToImage(Extent.XMin), x);
  xmax := FChart.ClipRect.Right;
  if Extent.UseXMax then
    xmax := Min(FChart.XGraphToImage(Extent.XMax), xmax);

  ACanvas.Pen.Assign(Pen);

  ACanvas.MoveTo(x, CalcY(x));
  while x < xmax do begin
    Inc(x, FStep);
    ACanvas.LineTo(x, CalcY(x));
  end;
end;

procedure TFuncSeries.DrawLegend(ACanvas: TCanvas; const ARect: TRect);
var
  y: Integer;
begin
  ACanvas.TextOut(ARect.Right + 3, ARect.Top, Title);
  ACanvas.Pen.Assign(Pen);
  y := (ARect.Top + ARect.Bottom) div 2;
  ACanvas.Line(ARect.Left, y, ARect.Right, y);
end;

function TFuncSeries.GetLegendCount: Integer;
begin
  Result := 1;
end;

function TFuncSeries.GetLegendWidth(ACanvas: TCanvas): Integer;
begin
  Result := ACanvas.TextWidth(Title);
end;

function TFuncSeries.GetSeriesColor: TColor;
begin
  Result := FPen.Color;
end;

function TFuncSeries.IsEmpty: Boolean;
begin
  Result := not Assigned(OnCalculate);
end;

procedure TFuncSeries.SetActive(AValue: Boolean);
begin
  if FActive = AValue then exit;
  FActive := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetDepth(AValue: TChartDistance);
begin
  if FDepth = AValue then exit;
  FDepth := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetExtent(const AValue: TChartExtent);
begin
  if FExtent = AValue then exit;
  FExtent.Assign(AValue);
  UpdateParentChart;
end;

procedure TFuncSeries.SetOnCalculate(const AValue: TFuncCalculateEvent);
begin
  if FOnCalculate = AValue then exit;
  FOnCalculate := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetPen(const AValue: TChartPen);
begin
  if FPen = AValue then exit;
  FPen.Assign(AValue);
  UpdateParentChart;
end;

procedure TFuncSeries.SetSeriesColor(const AValue: TColor);
begin
  if FPen.Color = AValue then exit;
  FPen.Color := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetShowInLegend(AValue: Boolean);
begin
  if FShowInLegend = AValue then exit;
  FShowInLegend := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetStep(AValue: TFuncSeriesStep);
begin
  if FStep = AValue then exit;
  FStep := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.SetZPosition(AValue: TChartDistance);
begin
  if FZPosition = AValue then exit;
  FZPosition := AValue;
  UpdateParentChart;
end;

procedure TFuncSeries.StyleChanged(Sender: TObject);
begin
  UpdateParentChart;
end;

procedure TFuncSeries.UpdateBounds(var ABounds: TDoubleRect);
begin
  with Extent do begin
    if UseXMin and (XMin < ABounds.a.X) then ABounds.a.X := XMin;
    if UseYMin and (YMin < ABounds.a.Y) then ABounds.a.Y := YMin;
    if UseXMax and (XMax > ABounds.b.X) then ABounds.b.X := XMax;
    if UseYMax and (YMax > ABounds.b.Y) then ABounds.b.Y := YMax;
  end;
end;

procedure TFuncSeries.UpdateParentChart;
begin
  if ParentChart <> nil then
    ParentChart.Invalidate;
end;

initialization
  RegisterSeriesClass(TLineSeries, 'Line series');
  RegisterSeriesClass(TAreaSeries, 'Area series');
  RegisterSeriesClass(TBarSeries, 'Bar series');
  RegisterSeriesClass(TPieSeries, 'Pie series');
  RegisterSeriesClass(TFuncSeries, 'Function series');
  RegisterSeriesClass(TLine, 'Line');

end.
