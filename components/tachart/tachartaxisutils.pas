{

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

 Authors: Alexander Klenin

}
unit TAChartAxisUtils;

{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TACustomSource, TADrawUtils, TAIntervalSources, TAStyles,
  TATypes;

const
  DEF_TITLE_DISTANCE = 4;

type
  TChartAxisBrush = class(TBrush)
  published
    property Style default bsClear;
  end;

  TChartAxisFramePen = class(TChartPen)
  published
    property Style default psClear;
  end;

  {$IFNDEF fpdoc}  // Workaround for issue #18549.
  TCustomChartAxisTitle =
    specialize TGenericChartMarks<TChartAxisBrush, TChartPen, TChartAxisFramePen>;
  {$ENDIF}

  { TChartAxisTitle }

  TChartAxisTitle = class(TCustomChartAxisTitle)
  strict private
    FCaption: String;

    function GetFont: TFont;
    procedure SetCaption(AValue: String);
    procedure SetFont(AValue: TFont);
  public
    constructor Create(AOwner: TCustomChart);

  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write SetCaption;
    property Distance default DEF_TITLE_DISTANCE;
    // Use LabelFont instead.
    property Font: TFont read GetFont write SetFont stored false; deprecated;
    property Frame;
    property LabelBrush;
    property Visible default false;
  end;

  ICoordTransformer = interface
  ['{6EDA0F9F-ED59-4CA6-BA68-E247EB88AE3D}']
    function XGraphToImage(AX: Double): Integer;
    function YGraphToImage(AY: Double): Integer;
  end;

  TChartAxisAlignment = (calLeft, calTop, calRight, calBottom);
  TChartAxisMargins = array [TChartAxisAlignment] of Integer;
  TChartAxisMarkToTextEvent =
    procedure (var AText: String; AMark: Double) of object;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  TBasicChartAxisMarks =
    specialize TGenericChartMarks<TChartAxisBrush, TChartPen, TChartAxisFramePen>;
  {$ENDIF}

  TCustomChartAxisMarks = class(TBasicChartAxisMarks)
  strict private
    FDefaultListener: TListener;
    FDefaultSource: TIntervalChartSource;
    FStripes: TChartStyles;
    procedure SetStripes(AValue: TChartStyles);
  strict protected
    function IsFormatStored: Boolean;
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
    function Measure(
      ADrawer: IChartDrawer; AIsVertical: Boolean; ATickLength: Integer;
      AValues: TChartValueTextArray): Integer;
    property DefaultSource: TIntervalChartSource read FDefaultSource;
    property Stripes: TChartStyles read FStripes write SetStripes;
  end;

  TChartMinorAxisMarks = class(TCustomChartAxisMarks)
  public
    constructor Create(AOwner: TCustomChart);
  published
    property Distance default 1;
    property Format;
    property Frame;
    property LabelBrush;
    property OverlapPolicy;
    property Style default smsNone;
  end;

  { TChartAxisMarks }

  TChartAxisMarks = class(TCustomChartAxisMarks)
  strict private
    FAtDataOnly: Boolean;
    FListener: TListener;
    FRange: TChartRange;
    FSource: TCustomChartSource;

    procedure SetAtDataOnly(AValue: Boolean);
    procedure SetRange(AValue: TChartRange);
    procedure SetSource(AValue: TCustomChartSource);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    function SourceDef: TCustomChartSource;
  published
    property AtDataOnly: Boolean
      read FAtDataOnly write SetAtDataOnly default false;
    property Distance default 1;
    property Format stored IsFormatStored;
    property Frame;
    property LabelBrush;
    property OverlapPolicy;
    property Range: TChartRange read FRange write SetRange;
    property Source: TCustomChartSource read FSource write SetSource;
    property Stripes;
    property Style default smsValue;
    property YIndex;
  end;

  TChartAxisGridPen = class(TChartPen)
  published
    property Style default psDot;
  end;

  TChartBasicAxis = class(TCollectionItem)
  strict private
    FArrow: TChartArrow;
    FGrid: TChartAxisGridPen;
    FTickColor: TColor;
    FTickLength: Integer;
    FVisible: Boolean;
    function GetIntervals: TChartAxisIntervalParams;
    procedure SetArrow(AValue: TChartArrow);
    procedure SetGrid(AValue: TChartAxisGridPen);
    procedure SetIntervals(AValue: TChartAxisIntervalParams);
    procedure SetTickColor(AValue: TColor);
    procedure SetTickLength(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  strict protected
    FMarks: TCustomChartAxisMarks;
    function GetAlignment: TChartAxisAlignment; virtual; abstract;
    procedure SetAlignment(AValue: TChartAxisAlignment); virtual; abstract;
    procedure SetMarks(AValue: TCustomChartAxisMarks);
    procedure StyleChanged(ASender: TObject); virtual; abstract;
  public
    constructor Create(ACollection: TCollection; AChart: TCustomChart); overload;
    destructor Destroy; override;
  public
    procedure Assign(ASource: TPersistent); override;
    function TryApplyStripes(
      ADrawer: IChartDrawer; var AIndex: Cardinal): Boolean;

    property Alignment: TChartAxisAlignment
      read GetAlignment write SetAlignment;
    property Arrow: TChartArrow read FArrow write SetArrow;
    property Marks: TCustomChartAxisMarks read FMarks write SetMarks;
  published
    property Grid: TChartAxisGridPen read FGrid write SetGrid;
    property Intervals: TChartAxisIntervalParams
      read GetIntervals write SetIntervals;
    property TickColor: TColor read FTickColor write SetTickColor default clBlack;
    property TickLength: Integer read FTickLength write SetTickLength;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  { TAxisDrawHelper }

  TAxisDrawHelper = class
  strict private
    FPrevLabelPoly: TPointArray;
  strict protected
    procedure BarZ(AX1, AY1, AX2, AY2: Integer); inline;
    procedure DrawLabel(ALabelCenter: TPoint; const AText: String); inline;
    procedure DrawLabelAndTick(
      ACoord, AFixedCoord: Integer; const AText: String); virtual; abstract;
    procedure GridLine(ACoord: Integer); virtual; abstract;
    procedure InternalAxisLine(
      APen: TChartPen; const AStart, AEnd: TPoint; AAngle: Double);
    function IsInClipRange(ACoord: Integer): Boolean;
    procedure LineZ(AP1, AP2: TPoint); inline;
    function TryApplyStripes: Boolean; inline;
  public
    FAxis: TChartBasicAxis;
    FAxisTransf: TTransformFunc;
    FClipRect: ^TRect;
    FDrawer: IChartDrawer;
    FPrevCoord: Integer;
    FScaledTickLength: Integer;
    FStripeIndex: Cardinal;
    FTransf: ICoordTransformer;
    FValueMax: Double;
    FValueMin: Double;
    FZOffset: TPoint;

    procedure BeginDrawing; virtual;
    function Clone: TAxisDrawHelper;
    constructor Create; virtual;
    procedure DrawAxisLine(
      APen: TChartPen; AFixedCoord: Integer); virtual; abstract;
    procedure DrawMark(
      AFixedCoord: Integer; AMark: Double; const AText: String);
    procedure EndDrawing; virtual; abstract;
    procedure GetClipRange(out AMin, AMax: Integer); virtual; abstract;
    function GraphToImage(AGraph: Double): Integer; virtual; abstract;
  end;

  TAxisDrawHelperClass = class of TAxisDrawHelper;

  { TAxisDrawHelperX }

  TAxisDrawHelperX = class(TAxisDrawHelper)
  strict protected
    procedure DrawLabelAndTick(
      ACoord, AFixedCoord: Integer; const AText: String); override;
    procedure GridLine(ACoord: Integer); override;
  public
    procedure BeginDrawing; override;
    procedure DrawAxisLine(APen: TChartPen; AFixedCoord: Integer); override;
    procedure EndDrawing; override;
    procedure GetClipRange(out AMin, AMax: Integer); override;
    function GraphToImage(AGraph: Double): Integer; override;
  end;

  { TAxisDrawHelperY }

  TAxisDrawHelperY = class(TAxisDrawHelper)
  strict protected
    procedure DrawLabelAndTick(
      ACoord, AFixedCoord: Integer; const AText: String); override;
    procedure GridLine(ACoord: Integer); override;
  public
    procedure BeginDrawing; override;
    procedure DrawAxisLine(APen: TChartPen; AFixedCoord: Integer); override;
    procedure EndDrawing; override;
    procedure GetClipRange(out AMin, AMax: Integer); override;
    function GraphToImage(AGraph: Double): Integer; override;
  end;

implementation

uses
  Math, SysUtils,
  TAGeometry;

{ TChartMinorAxisMarks }

constructor TChartMinorAxisMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FStyle := smsNone;
  FFormat := SERIES_MARK_FORMATS[FStyle];
end;

{ TAxisDrawHelper }

procedure TAxisDrawHelper.BarZ(AX1, AY1, AX2, AY2: Integer);
begin
  with FZOffset do
    FDrawer.FillRect(AX1 + X, AY1 + Y, AX2 + X, AY2 + Y);
end;

procedure TAxisDrawHelper.BeginDrawing;
begin
  FScaledTickLength := FDrawer.Scale(FAxis.TickLength);
end;

function TAxisDrawHelper.Clone: TAxisDrawHelper;
begin
  Result := TAxisDrawHelperClass(ClassType).Create;
  Result.FAxis := FAxis;
  Result.FAxisTransf := FAxisTransf;
  Result.FClipRect := FClipRect;
  Result.FDrawer := FDrawer;
  Result.FTransf := FTransf;
  Result.FValueMax := FValueMax;
  Result.FValueMin := FValueMin;
  Result.FZOffset := FZOffset;
end;

constructor TAxisDrawHelper.Create;
begin
  inherited; // Empty -- just to enforce a virtual constructor.
end;

procedure TAxisDrawHelper.DrawLabel(ALabelCenter: TPoint; const AText: String);
begin
  ALabelCenter += FZOffset;
  FAxis.Marks.DrawLabel(
    FDrawer, ALabelCenter, ALabelCenter, AText, FPrevLabelPoly);
end;

procedure TAxisDrawHelper.DrawMark(
  AFixedCoord: Integer; AMark: Double; const AText: String);
var
  coord: Integer;
begin
  coord := GraphToImage(AMark);
  if
    not IsInClipRange(coord) or not InRange(AMark, FValueMin, FValueMax)
  then exit;

  if FAxis.Grid.Visible then begin
    FDrawer.Pen := FAxis.Grid;
    FDrawer.SetBrushParams(bsClear, clTAColor);
    GridLine(coord);
    FPrevCoord := coord;
  end;

  if FAxis.Marks.Visible then begin
    FDrawer.PrepareSimplePen(FAxis.TickColor);
    DrawLabelAndTick(coord, AFixedCoord, AText);
  end;
end;

procedure TAxisDrawHelper.InternalAxisLine(
  APen: TChartPen; const AStart, AEnd: TPoint; AAngle: Double);
begin
  if not APen.Visible and not FAxis.Arrow.Visible then exit;
  FDrawer.Pen := APen;
  if APen.Visible then
    LineZ(AStart, AEnd);
  if FAxis.Arrow.Visible then
    FAxis.Arrow.Draw(FDrawer, AEnd + FZOffset, AAngle);
end;

function TAxisDrawHelper.IsInClipRange(ACoord: Integer): Boolean;
var
  rmin, rmax: Integer;
begin
  GetClipRange(rmin, rmax);
  Result := InRange(ACoord, rmin + 1, rmax - 1);
end;

procedure TAxisDrawHelper.LineZ(AP1, AP2: TPoint);
begin
  FDrawer.Line(AP1 + FZOffset, AP2 + FZOffset);
end;

function TAxisDrawHelper.TryApplyStripes: Boolean;
begin
  Result := FAxis.TryApplyStripes(FDrawer, FStripeIndex);
end;

{ TAxisDrawHelperX }

procedure TAxisDrawHelperX.BeginDrawing;
begin
  inherited;
  FPrevCoord := FClipRect^.Left;
end;

procedure TAxisDrawHelperX.DrawAxisLine(APen: TChartPen; AFixedCoord: Integer);
var
  p: TPoint;
begin
  p := Point(FClipRect^.Right, AFixedCoord);
  if FAxis.Arrow.Visible then
    p.X += FDrawer.Scale(FAxis.Arrow.Length);
  InternalAxisLine(APen, Point(FClipRect^.Left, AFixedCoord), p, 0);
end;

procedure TAxisDrawHelperX.DrawLabelAndTick(
  ACoord, AFixedCoord: Integer; const AText: String);
var
  d: Integer;
begin
  d := FScaledTickLength + FAxis.Marks.CenterOffset(FDrawer, AText).cy;
  if FAxis.Alignment = calTop then
    d := -d;
  LineZ(
    Point(ACoord, AFixedCoord - FScaledTickLength),
    Point(ACoord, AFixedCoord + FScaledTickLength));
  DrawLabel(Point(ACoord, AFixedCoord + d), AText);
end;

procedure TAxisDrawHelperX.EndDrawing;
begin
  if FAxis.Grid.Visible and TryApplyStripes then
    BarZ(FPrevCoord + 1, FClipRect^.Top + 1, FClipRect^.Right, FClipRect^.Bottom);
end;

procedure TAxisDrawHelperX.GetClipRange(out AMin, AMax: Integer);
begin
  AMin := FClipRect^.Left;
  AMax := FClipRect^.Right;
end;

function TAxisDrawHelperX.GraphToImage(AGraph: Double): Integer;
begin
  Result := FTransf.XGraphToImage(AGraph);
end;

procedure TAxisDrawHelperX.GridLine(ACoord: Integer);
begin
  if TryApplyStripes then
    BarZ(FPrevCoord + 1, FClipRect^.Top + 1, ACoord, FClipRect^.Bottom);
  LineZ(Point(ACoord, FClipRect^.Top), Point(ACoord, FClipRect^.Bottom));
end;

{ TAxisDrawHelperY }

procedure TAxisDrawHelperY.BeginDrawing;
begin
  inherited;
  FPrevCoord := FClipRect^.Bottom;
end;

procedure TAxisDrawHelperY.DrawAxisLine(APen: TChartPen; AFixedCoord: Integer);
var
  p: TPoint;
begin
  p := Point(AFixedCoord, FClipRect^.Top);
  if FAxis.Arrow.Visible then
    p.Y -= FDrawer.Scale(FAxis.Arrow.Length);
  InternalAxisLine(APen, Point(AFixedCoord, FClipRect^.Bottom), p, -Pi / 2);
end;

procedure TAxisDrawHelperY.DrawLabelAndTick(
  ACoord, AFixedCoord: Integer; const AText: String);
var
  d: Integer;
begin
  d := FScaledTickLength + FAxis.Marks.CenterOffset(FDrawer, AText).cx;
  if FAxis.Alignment = calLeft then
    d := -d;
  LineZ(
    Point(AFixedCoord - FScaledTickLength, ACoord),
    Point(AFixedCoord + FScaledTickLength, ACoord));
  DrawLabel(Point(AFixedCoord + d, ACoord), AText);
end;

procedure TAxisDrawHelperY.EndDrawing;
begin
  if FAxis.Grid.Visible and TryApplyStripes then
    BarZ(FClipRect^.Left + 1, FClipRect^.Top + 1, FClipRect^.Right, FPrevCoord);
end;

procedure TAxisDrawHelperY.GetClipRange(out AMin, AMax: Integer);
begin
  AMin := FClipRect^.Top;
  AMax := FClipRect^.Bottom;
end;

function TAxisDrawHelperY.GraphToImage(AGraph: Double): Integer;
begin
  Result := FTransf.YGraphToImage(AGraph);
end;

procedure TAxisDrawHelperY.GridLine(ACoord: Integer);
begin
  if TryApplyStripes then
    BarZ(FClipRect^.Left + 1, FPrevCoord, FClipRect^.Right, ACoord);
  LineZ(Point(FClipRect^.Left, ACoord), Point(FClipRect^.Right, ACoord));
end;

{ TChartAxisTitle }

procedure TChartAxisTitle.Assign(Source: TPersistent);
begin
  if Source is TChartAxisTitle then
    with TChartAxisTitle(Source) do begin
      Self.FLabelBrush.Assign(FLabelBrush);
      Self.FLabelFont.Assign(FLabelFont);
      Self.FLinkPen.Assign(FLinkPen);
      Self.FCaption := FCaption;
    end;
  inherited Assign(Source);
end;

constructor TChartAxisTitle.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := DEF_TITLE_DISTANCE;
  FLabelBrush.Style := bsClear;
  FVisible := false;
end;

function TChartAxisTitle.GetFont: TFont;
begin
  Result := LabelFont;
end;

procedure TChartAxisTitle.SetCaption(AValue: String);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;
  StyleChanged(Self);
end;

procedure TChartAxisTitle.SetFont(AValue: TFont);
begin
  LabelFont := AValue;
end;

{ TCustomChartAxisMarks }

constructor TCustomChartAxisMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDefaultListener := TListener.Create(nil, @StyleChanged);
  FDefaultSource := TIntervalChartSource.Create(AOwner);
  FDefaultSource.Broadcaster.Subscribe(FDefaultListener);
  FDistance := 1;
  FLabelBrush.Style := bsClear;
end;

destructor TCustomChartAxisMarks.Destroy;
begin
  FreeAndNil(FDefaultListener);
  FreeAndNil(FDefaultSource);
  inherited;
end;

function TCustomChartAxisMarks.IsFormatStored: Boolean;
begin
  Result := FStyle <> smsValue;
end;

function TCustomChartAxisMarks.Measure(ADrawer: IChartDrawer;
  AIsVertical: Boolean; ATickLength: Integer;
  AValues: TChartValueTextArray): Integer;
var
  t: TChartValueText;
begin
  Result := 0;
  if not Visible then exit;
  for t in AValues do
    // Workaround for issue #19780, fix after upgrade to FPC 2.6.
    with MeasureLabel(ADrawer, t.FText) do
      Result := Max(IfThen(AIsVertical, cy, cx), Result);
  if Result = 0 then exit;
  if DistanceToCenter then
    Result := Result div 2;
  Result += ADrawer.Scale(ATickLength) + ADrawer.Scale(Distance);
end;

procedure TCustomChartAxisMarks.SetStripes(AValue: TChartStyles);
begin
  if FStripes = AValue then exit;
  FStripes := AValue;
  StyleChanged(Self);
end;

{ TChartAxisMarks }

constructor TChartAxisMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FListener := TListener.Create(@FSource, @StyleChanged);
  FRange := TChartRange.Create(AOwner);
  FStyle := smsValue;
  FFormat := SERIES_MARK_FORMATS[FStyle];
end;

destructor TChartAxisMarks.Destroy;
begin
  FreeAndNil(FRange);
  FreeAndNil(FListener);
  inherited;
end;

procedure TChartAxisMarks.SetAtDataOnly(AValue: Boolean);
begin
  if FAtDataOnly = AValue then exit;
  FAtDataOnly := AValue;
  StyleChanged(Self);
end;

procedure TChartAxisMarks.SetRange(AValue: TChartRange);
begin
  if FRange = AValue then exit;
  FRange.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartAxisMarks.SetSource(AValue: TCustomChartSource);
begin
  if FSource = AValue then exit;
  if FListener.IsListening then
    FSource.Broadcaster.Unsubscribe(FListener);
  FSource := AValue;
  if FSource <> nil then
    FSource.Broadcaster.Subscribe(FListener);
  StyleChanged(Self);
end;

function TChartAxisMarks.SourceDef: TCustomChartSource;
begin
  Result := FSource;
  if Result = nil then
    Result := DefaultSource;
end;

{ TChartBasicAxis }

procedure TChartBasicAxis.Assign(ASource: TPersistent);
begin
  if ASource is TChartBasicAxis then
    with TChartBasicAxis(ASource) do begin
      Self.FGrid.Assign(Grid);
      Self.FMarks.Assign(Marks);
      Self.FTickColor := TickColor;
      Self.FTickLength := TickLength;
      Self.FVisible := Visible;
    end
  else
    inherited Assign(ASource);
end;

constructor TChartBasicAxis.Create(
  ACollection: TCollection; AChart: TCustomChart);
begin
  inherited Create(ACollection);
  FArrow := TChartArrow.Create(AChart);
  FGrid := TChartAxisGridPen.Create;
  FGrid.OnChange := @StyleChanged;
  // FMarks must be created in descendants.
  FTickColor := clBlack;
  FVisible := true;
end;

destructor TChartBasicAxis.Destroy;
begin
  FreeAndNil(FArrow);
  FreeAndNil(FGrid);
  FreeAndNil(FMarks);
  inherited;
end;

function TChartBasicAxis.GetIntervals: TChartAxisIntervalParams;
begin
  Result := Marks.DefaultSource.Params;
end;

procedure TChartBasicAxis.SetArrow(AValue: TChartArrow);
begin
  FArrow.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartBasicAxis.SetGrid(AValue: TChartAxisGridPen);
begin
  FGrid.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartBasicAxis.SetIntervals(AValue: TChartAxisIntervalParams);
begin
  Marks.DefaultSource.Params := AValue;
end;

procedure TChartBasicAxis.SetMarks(AValue: TCustomChartAxisMarks);
begin
  FMarks.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartBasicAxis.SetTickColor(AValue: TColor);
begin
  if FTickColor = AValue then exit;
  FTickColor := AValue;
  StyleChanged(Self);
end;

procedure TChartBasicAxis.SetTickLength(AValue: Integer);
begin
  if FTickLength = AValue then exit;
  FTickLength := AValue;
  StyleChanged(Self);
end;

procedure TChartBasicAxis.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  StyleChanged(Self);
end;

function TChartBasicAxis.TryApplyStripes(
  ADrawer: IChartDrawer; var AIndex: Cardinal): Boolean;
begin
  Result := Marks.Stripes <> nil;
  if not Result then exit;
  Marks.Stripes.Apply(ADrawer, AIndex);
  AIndex += 1;
end;

end.
