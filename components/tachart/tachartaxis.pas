{

 Axises for TAChart series.

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
unit TAChartAxis;

{$H+}

interface

uses
  Classes, Graphics, SysUtils, Types,
  TAChartUtils, TACustomSource, TADrawUtils, TAStyles, TATransformations,
  TATypes;

const
  DEF_TICK_LENGTH = 4;
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
  private
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

  TChartAxisPen = class(TChartPen)
  published
    property Style default psDot;
  end;

  {$IFNDEF fpdoc} // Workaround for issue #18549.
  TCustomChartAxisMarks =
    specialize TGenericChartMarks<TChartAxisBrush, TChartPen, TChartAxisFramePen>;
  {$ENDIF}

  { TChartAxisMarks }

  TChartAxisMarks = class(TCustomChartAxisMarks)
  private
    FAtDataOnly: Boolean;
    FDefaultSource: TCustomChartSource;
    FListener: TListener;
    FSource: TCustomChartSource;
    FStripes: TChartStyles;

    function IsFormatStored: Boolean;
    procedure SetAtDataOnly(AValue: Boolean);
    procedure SetSource(AValue: TCustomChartSource);
    procedure SetStripes(AValue: TChartStyles);
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
    property Source: TCustomChartSource read FSource write SetSource;
    property Stripes: TChartStyles read FStripes write SetStripes;
    property Style default smsValue;
  end;

  TChartAxisGroup = record
    FCount: Integer;
    FSize: Integer;
    FTitleSize: Integer;
  end;

  { TChartBasicAxis }

  TChartBasicAxis = class(TCollectionItem)
  strict private
    FGrid: TChartAxisPen;
    FMarks: TChartAxisMarks;
    FTickColor: TColor;
    FTickLength: Integer;
    FVisible: Boolean;
    procedure SetGrid(AValue: TChartAxisPen);
    procedure SetMarks(AValue: TChartAxisMarks);
    procedure SetTickColor(AValue: TColor);
    procedure SetTickLength(AValue: Integer);
    procedure SetVisible(AValue: Boolean);
  strict protected
    function GetAlignment: TChartAxisAlignment; virtual; abstract;
    procedure SetAlignment(AValue: TChartAxisAlignment); virtual; abstract;
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
    property Marks: TChartAxisMarks read FMarks write SetMarks;
  published
    property Grid: TChartAxisPen read FGrid write SetGrid;
    property TickColor: TColor read FTickColor write SetTickColor default clBlack;
    property TickLength: Integer read FTickLength write SetTickLength;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  { TChartMinorAxis }

  TChartMinorAxis = class(TChartBasicAxis)
  protected
    function GetDisplayName: String; override;
    procedure StyleChanged(ASender: TObject); override;
  strict protected
    function GetAlignment: TChartAxisAlignment; override;
    procedure SetAlignment(AValue: TChartAxisAlignment); override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property TickLength default DEF_TICK_LENGTH div 2;
  end;

  TChartAxis = class;

  { TChartMinorAxisList }

  TChartMinorAxisList = class(TCollection)
  strict private
    FAxis: TChartAxis;
    function GetAxes(AIndex: Integer): TChartMinorAxis;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TChartAxis);
  public
    function Add: TChartMinorAxis;
    function GetChart: TCustomChart; inline;
    property Axes[AIndex: Integer]: TChartMinorAxis read GetAxes; default;
  end;

  { TChartAxis }

  TChartAxis = class(TChartBasicAxis)
  strict private
    FListener: TListener;
    FMarkTexts: TStringDynArray;
    FMarkValues: TDoubleDynArray;

    procedure GetMarkValues(AMin, AMax: Double);
    procedure VisitSource(ASource: TCustomChartSource; var AData);
  private
    FAxisRect: TRect;
    FGroupIndex: Integer;
    FTitleRect: TRect;
  strict private
    FAlignment: TChartAxisAlignment;
    FGroup: Integer;
    FInverted: Boolean;
    FMinors: TChartMinorAxisList;
    FOnMarkToText: TChartAxisMarkToTextEvent;
    FTitle: TChartAxisTitle;
    FTransformations: TChartAxisTransformations;
    FZPosition: TChartDistance;

    function GetTransform: TChartAxisTransformations;
    procedure SetGroup(AValue: Integer);
    procedure SetInverted(AValue: Boolean);
    procedure SetMinors(AValue: TChartMinorAxisList);
    procedure SetOnMarkToText(AValue: TChartAxisMarkToTextEvent);
    procedure SetTitle(AValue: TChartAxisTitle);
    procedure SetTransformations(AValue: TChartAxisTransformations);
    procedure SetZPosition(const AValue: TChartDistance);

  protected
    function GetDisplayName: String; override;
    procedure StyleChanged(ASender: TObject); override;
  strict protected
    function GetAlignment: TChartAxisAlignment; override;
    procedure SetAlignment(AValue: TChartAxisAlignment); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(
      ADrawer: IChartDrawer; const AClipRect: TRect;
      const ATransf: ICoordTransformer; const AZOffset: TPoint);
    procedure DrawTitle(
      ADrawer: IChartDrawer; const ACenter, AZOffset: TPoint; ASize: Integer);
    function GetChart: TCustomChart; inline;
    function IsVertical: Boolean; inline;
    procedure Measure(
      ADrawer: IChartDrawer; const AExtent: TDoubleRect; AFirstPass: Boolean;
      var AMeasureData: TChartAxisGroup);
  published
    property Alignment default calLeft;
    property Group: Integer read FGroup write SetGroup default 0;
    // Inverts the axis scale from increasing to decreasing.
    property Inverted: boolean read FInverted write SetInverted default false;
    property Marks;
    property Minors: TChartMinorAxisList read FMinors write SetMinors;
    property TickLength default DEF_TICK_LENGTH;
    property Title: TChartAxisTitle read FTitle write SetTitle;
    property Transformations: TChartAxisTransformations
      read FTransformations write SetTransformations;
    property ZPosition: TChartDistance read FZPosition write SetZPosition default 0;
  published
    property OnMarkToText: TChartAxisMarkToTextEvent
      read FOnMarkToText write SetOnMarkToText;
  end;

  TChartOnSourceVisitor =
    procedure (ASource: TCustomChartSource; var AData) of object;
  TChartOnVisitSources = procedure (
    AVisitor: TChartOnSourceVisitor; AAxis: TChartAxis; var AData) of object;

  { TChartAxisList }

  TChartAxisList = class(TCollection)
  private
    FChart: TCustomChart;
    FOnVisitSources: TChartOnVisitSources;
    function GetAxes(AIndex: Integer): TChartAxis;
  private
    FCenterPoint: TPoint;
    FGroupOrder: TFPList;
    FGroups: array of TChartAxisGroup;
    FZOrder: TFPList;
    procedure InitAndSort(AList: TFPList; ACompare: TListSortCompare);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
  public
    function Add: TChartAxis; inline;
    procedure Draw(
      ADrawer: IChartDrawer; const AClipRect: TRect;
      const ATransf: ICoordTransformer; ACurrentZ, AMaxZ: Integer;
      var AIndex: Integer);
    function GetAxis(AIndex: Integer): TChartAxis;
    procedure Measure(
      ADrawer: IChartDrawer; const AExtent: TDoubleRect;
      AFirstPass: Boolean; var AMargins: TChartAxisMargins);
    procedure Prepare(ARect: TRect);
    procedure PrepareGroups;
    procedure SetAxis(AIndex: Integer; AValue: TChartAxis);

    property Axes[AIndex: Integer]: TChartAxis read GetAxes; default;
    property BottomAxis: TChartAxis index 1 read GetAxis write SetAxis;
    property LeftAxis: TChartAxis index 2 read GetAxis write SetAxis;
    property OnVisitSources: TChartOnVisitSources
      read FOnVisitSources write FOnVisitSources;
  end;

  TAxisConvFunc = function (AX: Integer): Double of object;

  { TAxisCoeffHelper }

  TAxisCoeffHelper = object
    FAxis: TChartAxis;
    FImageLo, FImageHi, FMarginLo, FMarginHi: Integer;
    FLo, FHi: Integer;
    FMin, FMax: PDouble;
    function CalcOffset(AScale: Double): Double;
    function CalcScale(ASign: Integer): Double;
    constructor Init(
      AAxis: TChartAxis; AImageLo, AImageHi, AMarginLo, AMarginHi: Integer;
      AMin, AMax: PDouble);
    procedure UpdateMinMax(AConv: TAxisConvFunc);
  end;

  procedure SideByAlignment(
    var ARect: TRect; AAlignment: TChartAxisAlignment; ADelta: Integer);
  function TransformByAxis(
    AAxisList: TChartAxisList; AIndex: Integer): TChartAxisTransformations;

implementation

uses
  LResources, Math, PropEdits, TAGeometry, TAIntervalSources;

type
  TAxisDataExtent = record
    FMin, FMax: Double;
  end;

  { TAxisDrawHelper }

  TAxisDrawHelper = class
  strict protected
    procedure BarZ(AX1, AY1, AX2, AY2: Integer); inline;
    procedure DrawLabel(ALabelCenter: TPoint; const AText: String); inline;
    procedure DrawLabelAndTick(
      ACoord, AFixedCoord: Integer; const AText: String); virtual; abstract;
    function GraphToImage(AGraph: Double): Integer; virtual; abstract;
    procedure GridLine(ACoord: Integer); virtual; abstract;
    procedure LineZ(AP1, AP2: TPoint); inline;
    function TryApplyStripes: Boolean; inline;
  public
    FAxis: TChartBasicAxis;
    FClipRect: TRect;
    FDrawer: IChartDrawer;
    FPrevCoord: Integer;
    FPrevLabelPoly: TPointArray;
    FScaledTickLength: Integer;
    FStripeIndex: Cardinal;
    FTransf: ICoordTransformer;
    FZOffset: TPoint;

    procedure BeginDrawing; virtual;
    procedure DrawMark(
      AFixedCoord: Integer; AMark: Double; const AText: String);
    procedure EndDrawing; virtual; abstract;
  end;

  { TAxisDrawHelperX }

  TAxisDrawHelperX = class(TAxisDrawHelper)
  protected
    procedure DrawLabelAndTick(
      ACoord, AFixedCoord: Integer; const AText: String); override;
    function GraphToImage(AGraph: Double): Integer; override;
    procedure GridLine(ACoord: Integer); override;
  public
    procedure BeginDrawing; override;
    procedure EndDrawing; override;
  end;

  { TAxisDrawHelperY }

  TAxisDrawHelperY = class(TAxisDrawHelper)
  protected
    procedure DrawLabelAndTick(
      ACoord, AFixedCoord: Integer; const AText: String); override;
    function GraphToImage(AGraph: Double): Integer; override;
    procedure GridLine(ACoord: Integer); override;
  public
    procedure BeginDrawing; override;
    procedure EndDrawing; override;
  end;

var
  VIdentityTransform: TChartAxisTransformations;

function AxisGroupCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TChartAxis(Item1).Group - TChartAxis(Item2).Group;
end;

function AxisZCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TChartAxis(Item1).ZPosition - TChartAxis(Item2).ZPosition;
end;

procedure SideByAlignment(
  var ARect: TRect; AAlignment: TChartAxisAlignment; ADelta: Integer);
var
  a: TChartAxisMargins absolute ARect;
begin
  if AAlignment in [calLeft, calTop] then
    ADelta := -ADelta;
  a[AAlignment] += ADelta;
end;

function TransformByAxis(
  AAxisList: TChartAxisList; AIndex: Integer): TChartAxisTransformations;
begin
  Result := nil;
  if InRange(AIndex, 0, AAxisList.Count - 1) then
    Result := AAxisList[AIndex].Transformations;
  if Result = nil then
    Result := VIdentityTransform;
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
  inherited BeginDrawing;
  FPrevCoord := FClipRect.Left;
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
    BarZ(FPrevCoord + 1, FClipRect.Top + 1, FClipRect.Right, FClipRect.Bottom);
end;

function TAxisDrawHelperX.GraphToImage(AGraph: Double): Integer;
begin
  Result := FTransf.XGraphToImage(AGraph);
end;

procedure TAxisDrawHelperX.GridLine(ACoord: Integer);
begin
  if TryApplyStripes then
    BarZ(FPrevCoord + 1, FClipRect.Top + 1, ACoord, FClipRect.Bottom);
  LineZ(Point(ACoord, FClipRect.Top), Point(ACoord, FClipRect.Bottom));
end;

{ TAxisDrawHelperY }

procedure TAxisDrawHelperY.BeginDrawing;
begin
  inherited BeginDrawing;
  FPrevCoord := FClipRect.Bottom;
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
    BarZ(FClipRect.Left + 1, FClipRect.Top + 1, FClipRect.Right, FPrevCoord)
end;

function TAxisDrawHelperY.GraphToImage(AGraph: Double): Integer;
begin
  Result := FTransf.YGraphToImage(AGraph);
end;

procedure TAxisDrawHelperY.GridLine(ACoord: Integer);
begin
  if TryApplyStripes then
    BarZ(FClipRect.Left + 1, FPrevCoord, FClipRect.Right, ACoord);
  LineZ(Point(FClipRect.Left, ACoord), Point(FClipRect.Right, ACoord));
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
  FFrame.Style := psClear;
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

{ TChartAxisMarks }

constructor TChartAxisMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDefaultSource := TIntervalChartSource.Create(AOwner);
  FDistance := 1;
  FFrame.Style := psClear;
  FLabelBrush.Style := bsClear;
  FListener := TListener.Create(@FSource, @StyleChanged);
  FStyle := smsValue;
  FFormat := SERIES_MARK_FORMATS[FStyle];
end;

destructor TChartAxisMarks.Destroy;
begin
  FreeAndNil(FListener);
  FreeAndNil(FDefaultSource);
  inherited;
end;

function TChartAxisMarks.IsFormatStored: Boolean;
begin
  Result := FStyle <> smsValue;
end;

procedure TChartAxisMarks.SetAtDataOnly(AValue: Boolean);
begin
  if FAtDataOnly = AValue then exit;
  FAtDataOnly := AValue;
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

procedure TChartAxisMarks.SetStripes(AValue: TChartStyles);
begin
  if FStripes = AValue then exit;
  FStripes := AValue;
  StyleChanged(Self);
end;

function TChartAxisMarks.SourceDef: TCustomChartSource;
begin
  Result := FSource;
  if Result = nil then
    Result := FDefaultSource;
end;

{ TChartBasicAxis }

procedure TChartBasicAxis.Assign(ASource: TPersistent);
begin
  if ASource is TChartBasicAxis then
    with TChartAxis(ASource) do begin
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
  FGrid := TChartAxisPen.Create;
  FGrid.OnChange := @StyleChanged;
  FGrid.Style := psDot;
  FMarks := TChartAxisMarks.Create(AChart);
  FTickColor := clBlack;
  FVisible := true;
end;

destructor TChartBasicAxis.Destroy;
begin
  FreeAndNil(FGrid);
  FreeAndNil(FMarks);
  inherited;
end;

procedure TChartBasicAxis.SetGrid(AValue: TChartAxisPen);
begin
  FGrid.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartBasicAxis.SetMarks(AValue: TChartAxisMarks);
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

{ TChartMinorAxis }

constructor TChartMinorAxis.Create(ACollection: TCollection);
begin
  inherited Create(ACollection, (ACollection as TChartMinorAxisList).GetChart);
  TickLength := DEF_TICK_LENGTH div 2;
end;

function TChartMinorAxis.GetAlignment: TChartAxisAlignment;
begin
  Result := (Collection.Owner as TChartAxis).GetAlignment;
end;

function TChartMinorAxis.GetDisplayName: String;
begin
  Result := 'M';
end;

procedure TChartMinorAxis.SetAlignment(AValue: TChartAxisAlignment);
begin
  Unused(AValue);
  raise EChartError.Create('TChartMinorAxis.SetAlignment');
end;

procedure TChartMinorAxis.StyleChanged(ASender: TObject);
begin
  (Collection.Owner as TChartAxis).StyleChanged(ASender);
end;

{ TChartMinorAxisList }

function TChartMinorAxisList.Add: TChartMinorAxis;
begin
  Result := TChartMinorAxis(inherited Add);
end;

constructor TChartMinorAxisList.Create(AOwner: TChartAxis);
begin
  inherited Create(TChartMinorAxis);
  FAxis := AOwner;
end;

function TChartMinorAxisList.GetAxes(AIndex: Integer): TChartMinorAxis;
begin
  Result := TChartMinorAxis(Items[AIndex]);
end;

function TChartMinorAxisList.GetChart: TCustomChart;
begin
  Result := FAxis.GetChart;
end;

function TChartMinorAxisList.GetOwner: TPersistent;
begin
  Result := FAxis;
end;

procedure TChartMinorAxisList.Update(AItem: TCollectionItem);
begin
  FAxis.StyleChanged(AItem);
end;

{ TChartAxis }

procedure TChartAxis.Assign(ASource: TPersistent);
begin
  if ASource is TChartAxis then
    with TChartAxis(ASource) do begin
      Self.FGroup := Group;
      Self.FInverted := Inverted;
      Self.FTitle.Assign(Title);
      Self.FTransformations := Transformations;
      Self.FZPosition := ZPosition;

      Self.FOnMarkToText := OnMarkToText;
    end;
  inherited Assign(ASource);
end;

constructor TChartAxis.Create(ACollection: TCollection);
begin
  inherited Create(ACollection, ACollection.Owner as TCustomChart);
  FListener := TListener.Create(@FTransformations, @StyleChanged);
  FMinors := TChartMinorAxisList.Create(Self);
  TickLength := DEF_TICK_LENGTH;
  FTitle := TChartAxisTitle.Create(ACollection.Owner as TCustomChart);
end;

destructor TChartAxis.Destroy;
begin
  FreeAndNil(FTitle);
  FreeAndNil(FMinors);
  FreeAndNil(FListener);
  inherited;
end;

procedure TChartAxis.Draw(
  ADrawer: IChartDrawer; const AClipRect: TRect;
  const ATransf: ICoordTransformer; const AZOffset: TPoint);
var
  i, fixedCoord: Integer;
  axisTransf: TTransformFunc;
  dh: TAxisDrawHelper;
begin
  if not Visible then exit;
  if Marks.Visible then
    ADrawer.Font := Marks.LabelFont;
  fixedCoord := TChartAxisMargins(FAxisRect)[Alignment];

  if IsVertical then
    dh := TAxisDrawHelperY.Create
  else
    dh := TAxisDrawHelperX.Create;
  try
    dh.FAxis := Self;
    dh.FClipRect := AClipRect;
    dh.FDrawer := ADrawer;
    dh.FTransf := ATransf;
    dh.FZOffset := AZOffset;
    dh.BeginDrawing;
    axisTransf := @GetTransform.AxisToGraph;
    for i := 0 to High(FMarkValues) do
      dh.DrawMark(fixedCoord, axisTransf(FMarkValues[i]), FMarkTexts[i]);
    dh.EndDrawing;
  finally
    dh.Free;
  end;
end;

procedure TChartAxis.DrawTitle(
  ADrawer: IChartDrawer; const ACenter, AZOffset: TPoint; ASize: Integer);
var
  p: TPoint;
  dummy: TPointArray = nil;
  d: Integer;
begin
  if not Visible or (ASize = 0) then exit;
  p := ACenter;
  d := (ASize + Title.Distance) div 2;
  case Alignment of
    calLeft: p.X := FTitleRect.Left - d;
    calTop: p.Y := FTitleRect.Top - d;
    calRight: p.X := FTitleRect.Right + d;
    calBottom: p.Y := FTitleRect.Bottom + d;
  end;
  p += AZOffset;
  Title.DrawLabel(ADrawer, p, p, Title.Caption, dummy);
end;

function TChartAxis.GetAlignment: TChartAxisAlignment;
begin
  Result := FAlignment;
end;

function TChartAxis.GetChart: TCustomChart;
begin
  Result := Collection.Owner as TCustomChart;
end;

function TChartAxis.GetDisplayName: string;
const
  SIDE_NAME: array [TChartAxisAlignment] of String =
    ('Left', 'Top', 'Right', 'Bottom');
  VISIBLE_NAME: array [Boolean] of String = (' Hidden', '');
  INVERTED_NAME: array [Boolean] of String = ('', ' Inverted');
  CAPTION_FMT = ' (%s)';
begin
  Result :=
    SIDE_NAME[Alignment] + VISIBLE_NAME[Visible] + INVERTED_NAME[Inverted];
  if Title.Caption <> '' then
    Result += Format(CAPTION_FMT, [Title.Caption]);
end;

procedure TChartAxis.GetMarkValues(AMin, AMax: Double);
var
  i: Integer;
  d: TAxisDataExtent;
  vis: TChartOnVisitSources;
begin
  AMin := GetTransform.GraphToAxis(AMin);
  AMax := GetTransform.GraphToAxis(AMax);
  EnsureOrder(AMin, AMax);
  SetLength(FMarkValues, 0);
  SetLength(FMarkTexts, 0);
  vis := TChartAxisList(Collection).OnVisitSources;
  if Marks.AtDataOnly and Assigned(vis) then begin
    d.FMin := AMin;
    d.FMax := AMax;
    vis(@VisitSource, Self, d);
  end
  else
    Marks.SourceDef.ValuesInRange(
      AMin, AMax, Marks.Format, IsVertical, FMarkValues, FMarkTexts);
  if Inverted then
    for i := 0 to High(FMarkValues) div 2 do begin
      Exchange(FMarkValues[i], FMarkValues[High(FMarkValues) - i]);
      Exchange(FMarkTexts[i], FMarkTexts[High(FMarkValues) - i]);
    end;

  if Assigned(FOnMarkToText) then
    for i := 0 to High(FMarkTexts) do
      FOnMarkToText(FMarkTexts[i], FMarkValues[i]);
end;

function TChartAxis.GetTransform: TChartAxisTransformations;
begin
  Result := Transformations;
  if Result = nil then
    Result := VIdentityTransform;
end;

function TChartAxis.IsVertical: Boolean; inline;
begin
  Result := Alignment in [calLeft, calRight];
end;

procedure TChartAxis.Measure(
  ADrawer: IChartDrawer; const AExtent: TDoubleRect;
  AFirstPass: Boolean; var AMeasureData: TChartAxisGroup);

  function CalcMarksSize(AMin, AMax: Double): TSize;
  const
    SOME_DIGIT = '0';
  var
    i, d: Integer;
    t: String;
  begin
    Result := Size(0, 0);
    if AMin = AMax then exit;
    GetMarkValues(AMin, AMax);
    if not Marks.Visible then exit;
    for i := 0 to High(FMarkTexts) do begin
      // CalculateTransformationCoeffs changes axis interval, so it is possibile
      // that a new mark longer then existing ones is introduced.
      // That will change marks width and reduce view area,
      // requiring another call to CalculateTransformationCoeffs...
      // So punt for now and just reserve space for extra digit unconditionally.
      t := FMarkTexts[i];
      if AFirstPass then
        t += SOME_DIGIT;
      d := IfThen(Marks.DistanceToCenter, 2, 1);
      with Marks.MeasureLabel(ADrawer, t) do begin
        Result.cx := Max(cx div d, Result.cx);
        Result.cy := Max(cy div d, Result.cy);
      end;
    end;
  end;

  function CalcTitleSize: Integer;
  var
    sz: TSize;
  begin
    if not Title.Visible or (Title.Caption = '') then
      exit(0);
    sz := Title.MeasureLabel(ADrawer, Title.Caption);

    Result := IfThen(IsVertical, sz.cx, sz.cy) + Title.Distance;
  end;

var
  sz: Integer;
begin
  if not Visible then exit;
  if IsVertical then
    sz := CalcMarksSize(AExtent.a.Y, AExtent.b.Y).cx
  else
    sz := CalcMarksSize(AExtent.a.X, AExtent.b.X).cy;
  if sz > 0 then
    sz += ADrawer.Scale(TickLength) + ADrawer.Scale(Marks.Distance);
  with AMeasureData do begin
    FSize := Max(sz, FSize);
    FTitleSize := Max(CalcTitleSize, FTitleSize);
  end;
end;

procedure TChartAxis.SetAlignment(AValue: TChartAxisAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetGroup(AValue: Integer);
begin
  if FGroup = AValue then exit;
  FGroup := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetInverted(AValue: Boolean);
begin
  if FInverted = AValue then exit;
  FInverted := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetMinors(AValue: TChartMinorAxisList);
begin
  if FMinors = AValue then exit;
  FMinors.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartAxis.SetOnMarkToText(AValue: TChartAxisMarkToTextEvent);
begin
  if TMethod(FOnMarkToText) = TMethod(AValue) then exit;
  FOnMarkToText := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetTitle(AValue: TChartAxisTitle);
begin
  FTitle.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartAxis.SetTransformations(AValue: TChartAxisTransformations);
begin
  if FTransformations = AValue then exit;
  if FListener.IsListening then
    Transformations.Broadcaster.Unsubscribe(FListener);
  FTransformations := AValue;
  if FTransformations <> nil then
    Transformations.Broadcaster.Subscribe(FListener);
  StyleChanged(Self);
end;

procedure TChartAxis.SetZPosition(const AValue: TChartDistance);
begin
  if FZPosition = AValue then exit;
  FZPosition := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.StyleChanged(ASender: TObject);
begin
  with GetChart do begin
    // Transformation change could have invalidated the current extent,
    // so revert to full extent for now.
    if ASender is TAxisTransform then
      ZoomFull;
    Invalidate;
  end;
end;

procedure TChartAxis.VisitSource(ASource: TCustomChartSource; var AData);
var
  lmin, lmax: Double;
  ext: TDoubleRect;
begin
  ext := ASource.Extent;
  with TAxisDataExtent(AData) do begin
    if IsVertical then begin
      lmin := Max(ext.a.Y, FMin);
      lmax := Min(ext.b.Y, FMax);
    end
    else begin
      lmin := Max(ext.a.X, FMin);
      lmax := Min(ext.b.X, FMax);
    end;
    Marks.SourceDef.ValuesInRange(
      lmin, lmax, Marks.Format, IsVertical, FMarkValues, FMarkTexts);
  end;
end;

const
  AXIS_INDEX: array [1..2] of TChartAxisAlignment = (calBottom, calLeft);

{ TChartAxisList }

function TChartAxisList.Add: TChartAxis; inline;
begin
  Result := TChartAxis(inherited Add);
end;

constructor TChartAxisList.Create(AOwner: TCustomChart);
begin
  inherited Create(TChartAxis);
  FChart := AOwner;
  FGroupOrder := TFPList.Create;
  FZOrder := TFPList.Create;
end;

destructor TChartAxisList.Destroy;
begin
  FreeAndNil(FGroupOrder);
  FreeAndNil(FZOrder);
  inherited Destroy;
end;

procedure TChartAxisList.Draw(
  ADrawer: IChartDrawer; const AClipRect: TRect;
  const ATransf: ICoordTransformer; ACurrentZ, AMaxZ: Integer;
  var AIndex: Integer);
var
  zoffset: TPoint;
begin
  while AIndex < FZOrder.Count do
    with TChartAxis(FZOrder[AIndex]) do begin
      if ACurrentZ < ZPosition then break;
      zoffset.Y := Min(ZPosition, AMaxZ);
      zoffset.X := - zoffset.Y;
      Draw(ADrawer, AClipRect, ATransf, zoffset);
      DrawTitle(ADrawer, FCenterPoint, zoffset, FGroups[FGroupIndex].FTitleSize);
      AIndex += 1;
    end;
end;

function TChartAxisList.GetAxes(AIndex: Integer): TChartAxis;
begin
  Result := TChartAxis(Items[AIndex]);
end;

function TChartAxisList.GetAxis(AIndex: Integer): TChartAxis;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Axes[i].Alignment = AXIS_INDEX[AIndex] then
      exit(Axes[i]);
  Result := nil;
end;

function TChartAxisList.GetOwner: TPersistent;
begin
  Result := FChart;
end;

procedure TChartAxisList.InitAndSort(
  AList: TFPList; ACompare: TListSortCompare);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to Count - 1 do
    AList.Add(Pointer(Axes[i]));
  AList.Sort(ACompare);
end;

procedure TChartAxisList.Measure(
  ADrawer: IChartDrawer; const AExtent: TDoubleRect;
  AFirstPass: Boolean; var AMargins: TChartAxisMargins);
var
  i, j, ai: Integer;
  axis: TChartAxis;
  g: ^TChartAxisGroup;
begin
  ai := 0;
  for i := 0 to High(FGroups) do begin
    g := @FGroups[i];
    g^.FSize := 0;
    g^.FTitleSize := 0;
    for j := 0 to g^.FCount - 1 do begin
      axis := TChartAxis(FGroupOrder[ai]);
      axis.Measure(ADrawer, AExtent, AFirstPass, g^);
      ai += 1;
    end;
    if AFirstPass then
      AMargins[axis.Alignment] += g^.FSize + g^.FTitleSize;
  end;
end;

procedure TChartAxisList.Prepare(ARect: TRect);
var
  i, j, ai: Integer;
  axis: TChartAxis;
  g: ^TChartAxisGroup;
begin
  FCenterPoint := CenterPoint(ARect);
  ai := 0;
  for i := 0 to High(FGroups) do begin
    g := @FGroups[i];
    for j := 0 to g^.FCount - 1 do begin
      axis := TChartAxis(FGroupOrder[ai + j]);
      axis.FAxisRect := ARect;
    end;
    SideByAlignment(ARect, axis.Alignment, g^.FSize);
    for j := 0 to g^.FCount - 1 do begin
      axis := TChartAxis(FGroupOrder[ai]);
      axis.FTitleRect := ARect;
      ai += 1;
    end;
    SideByAlignment(ARect, axis.Alignment, g^.FTitleSize);
  end;
  InitAndSort(FZOrder, @AxisZCompare);
end;

procedure TChartAxisList.PrepareGroups;
var
  i, prevGroup, groupCount: Integer;
begin
  InitAndSort(FGroupOrder, @AxisGroupCompare);
  SetLength(FGroups, Count);
  groupCount := 0;
  prevGroup := 0;
  for i := 0 to FGroupOrder.Count - 1 do
    with TChartAxis(FGroupOrder[i]) do begin
      if (Group = 0) or (Group <> prevGroup) then begin
        FGroups[groupCount].FCount := 1;
        groupCount += 1;
        prevGroup := Group;
      end
      else
        FGroups[groupCount - 1].FCount += 1;
      FGroupIndex := groupCount - 1;
    end;
  SetLength(FGroups, groupCount);
end;

procedure TChartAxisList.SetAxis(AIndex: Integer; AValue: TChartAxis);
var
  a: TChartAxis;
begin
  a := GetAxis(AIndex);
  if a = nil then
    a := Add;
  a.Assign(AValue);
  a.Alignment := AXIS_INDEX[AIndex];
end;

procedure TChartAxisList.Update(AItem: TCollectionItem);
begin
  Unused(AItem);
  FChart.Invalidate;
end;

{ TAxisCoeffHelper }

constructor TAxisCoeffHelper.Init(
  AAxis: TChartAxis; AImageLo, AImageHi, AMarginLo, AMarginHi: Integer;
  AMin, AMax: PDouble);
begin
  FAxis := AAxis;
  FImageLo := AImageLo;
  FImageHi := AImageHi;
  FMarginLo := AMarginLo;
  FMarginHi := AMarginHi;
  FMin := AMin;
  FMax := AMax;
  FLo := FImageLo + FMarginLo;
  FHi := FImageHi + FMarginHi;
end;

function TAxisCoeffHelper.CalcScale(ASign: Integer): Double;
begin
  if (FMax^ = FMin^) or (Sign(FHi - FLo) <> ASign) then exit(1.0);
  if (FAxis <> nil) and FAxis.Inverted then
    Exchange(FLo, FHi);
  Result := (FHi - FLo) / (FMax^ - FMin^);
end;

function TAxisCoeffHelper.CalcOffset(AScale: Double): Double;
begin
  Result := (FLo + FHi) / 2 - AScale * (FMin^ + FMax^) / 2;
end;

procedure TAxisCoeffHelper.UpdateMinMax(AConv: TAxisConvFunc);
begin
  FMin^ := AConv(FImageLo);
  FMax^ := AConv(FImageHi);
  if (FAxis <> nil) and FAxis.Inverted then
    Exchange(FMin^, FMax^);
end;

procedure SkipObsoleteAxisProperties;
const
  TRANSFORM_NOTE = 'Obsolete, use Transformations instead';
begin
  RegisterPropertyToSkip(TChartAxis, 'Offset', TRANSFORM_NOTE, '');
  RegisterPropertyToSkip(TChartAxis, 'Scale', TRANSFORM_NOTE, '');
  RegisterPropertyToSkip(TChartAxis, 'Transformation', TRANSFORM_NOTE, '');
  RegisterPropertyEditor(
    TypeInfo(TFont), TChartAxisTitle, 'Font', THiddenPropertyEditor);
end;

initialization
  VIdentityTransform := TChartAxisTransformations.Create(nil);
  SkipObsoleteAxisProperties;

finalization
  FreeAndNil(VIdentityTransform);

end.

