{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TARadialSeries;

{$H+}

interface

uses
  Classes, Graphics, SysUtils, Types,
  TAChartUtils, TACustomSeries, TADrawUtils, TALegend, TAStyles;

type

  { TLegendItemPie }

  TLegendItemPie = class(TLegendItem)
  private
    FColors: array [0..2] of TChartColor;
    procedure SetColors(AIndex: Integer; AValue: TChartColor);
  public
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
    property Colors[AIndex: Integer]: TChartColor write SetColors;
  end;

  { TLegendItemPieSlice }

  TLegendItemPieSlice = class(TLegendItem)
  public
    procedure Draw(ADrawer: IChartDrawer; const ARect: TRect); override;
  end;

  TLabelParams = record
    FAttachment: TPoint;
    FCenter: TPoint;
    FText: String;
  end;

  TPieSlice = object
    FBase: TPoint;
    FFlags: Integer;  // 1: 1st part, 2: 2nd part of diviced slice
    FLabel: TLabelParams;
    FOrigIndex: Integer;
    FPrevAngle, FNextAngle: Double; // in CCW direction
                                    // FNextAngle may become < FPrevAngle when crossing 360°
    FVisible: Boolean;
    function Angle: Double; inline;
    function CenterAngle: Double; inline;
    function FixedNextAngle: Double; inline;
  end;

  TPieMarkPositions = (pmpAround, pmpInside, pmpLeftRight);

  { TCustomPieSeries }

  TCustomPieSeries = class;
  TSliceArray = array of TPieSlice;
  TPieOrientation = (poNormal, poHorizontal, poVertical);
  TSlicePart = (spTop, spOuterArcSide, spInnerArcSide, spStartSide, spEndSide);
  TCustomDrawPieEvent = procedure(ASeries: TCustomPieSeries; ADrawer: IChartDrawer;
    ASlice: TPieSlice; APart: TSlicePart; const APoints: TPointArray) of object;

  TCustomPieSeries = class(TChartSeries)
  private
    FAspectRatio: Double;
    FCenter: TPoint;
    FMarkDistancePercent: Boolean;
    FMarkPositionCentered: Boolean;
    FMarkPositions: TPieMarkPositions;
    FOrientation: TPieOrientation;
    FRadius: Integer;
    FInnerRadiusPercent: Integer;
    FSlices: array of TPieSlice;
    FStartAngle: Integer;
    FEdgePen: TPen;
    FExploded: Boolean;
    FFixedRadius: TChartDistance;
    FRotateLabels: Boolean;
    FOnCustomDrawPie: TCustomDrawPieEvent;
    function FixAspectRatio(P: TPoint): TPoint;
    function GetViewAngle: Integer;
    procedure Measure(ADrawer: IChartDrawer);
    procedure SetEdgePen(AValue: TPen);
    procedure SetExploded(AValue: Boolean);
    procedure SetFixedRadius(AValue: TChartDistance);
    procedure SetInnerRadiusPercent(AValue: Integer);
    procedure SetMarkDistancePercent(AValue: Boolean);
    procedure SetMarkPositionCentered(AValue: Boolean);
    procedure SetMarkPositions(AValue: TPieMarkPositions);
    procedure SetOnCustomDrawPie(AValue: TCustomDrawPieEvent);
    procedure SetOrientation(AValue: TPieOrientation);
    procedure SetRotateLabels(AValue: Boolean);
    procedure SetStartAngle(AValue: Integer);
    procedure SetViewAngle(AValue: Integer);
    function SliceExploded(ASlice: TPieSlice): Boolean; inline;
    function TryRadius(ADrawer: IChartDrawer): TRect;
  protected
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure SortSlices(out ASlices: TSliceArray);
    property InnerRadiusPercent: Integer
      read FInnerRadiusPercent write SetInnerRadiusPercent default 0;
    property MarkPositionCentered: Boolean
      read FMarkPositionCentered write SetMarkPositionCentered default false;
    property Orientation: TPieOrientation
      read FOrientation write SetOrientation default poNormal;
    property Radius: Integer read FRadius;
    property StartAngle: Integer
      read FStartAngle write SetStartAngle default 0;
    property ViewAngle: Integer
      read GetViewAngle write SetViewAngle default 60;
    property OnCustomDrawPie: TCustomDrawPieEvent
      read FOnCustomDrawPie write SetOnCustomDrawPie;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function AddPie(AValue: Double; AText: String; AColor: TColor): Integer;
    procedure Assign(ASource: TPersistent); override;
    function CalcBorderPoint(ASlice: TPieSlice; ARadius, AAngle: Double): TPoint; inline;
    function CalcInnerRadius: Integer; inline;
    procedure Draw(ADrawer: IChartDrawer); override;
    function FindContainingSlice(const APoint: TPoint): Integer;
    function GetNearestPoint(const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
      const ANewPos: TDoublePoint); override;
    function SliceColor(AIndex: Integer): TColor;

    property EdgePen: TPen read FEdgePen write SetEdgePen;
    property Exploded: Boolean read FExploded write SetExploded default false;
    property FixedRadius: TChartDistance
      read FFixedRadius write SetFixedRadius default 0;
    property MarkDistancePercent: Boolean
      read FMarkDistancePercent write SetMarkDistancePercent default false;
    property MarkPositions: TPieMarkPositions
      read FMarkPositions write SetMarkPositions default pmpAround;
    property RotateLabels: Boolean
      read FRotateLabels write SetRotateLabels default false;

  end;

  TSinCos = record
    FSin, FCos: Double;
  end;

  { TPolarSeries }

  TPolarSeries = class(TBasicPointSeries)
  strict private
    FBrush: TBrush;
    FCloseCircle: Boolean;
    FFilled: Boolean;
    FLinePen: TPen;
    FOriginX: Double;
    FOriginY: Double;
    FShowPoints: Boolean;
    function IsOriginXStored: Boolean;
    function IsOriginYStored: Boolean;
    procedure SetBrush(AValue: TBrush);
    procedure SetCloseCircle(AValue: Boolean);
    procedure SetFilled(AValue: Boolean);
    procedure SetLinePen(AValue: TPen);
    procedure SetOriginX(AValue: Double);
    procedure SetOriginY(AValue: Double);
    procedure SetShowPoints(AValue: Boolean);
  strict private
    FAngleCache: array of TSinCos;
    function GraphPoint(AIndex, AYIndex: Integer): TDoublePoint;
    procedure PrepareAngleCache;
    procedure PrepareGraphPoints(AYIndex: Integer);
  protected
    function GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint; override;
    procedure GetLegendItems(AItems: TChartLegendItems); override;
    procedure SourceChanged(ASender: TObject); override;
    procedure UpdateLabelDirectionReferenceLevel(AIndex, AYIndex: Integer;
      var ALevel: Double); override;
  public
    procedure Assign(ASource: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Draw(ADrawer: IChartDrawer); override;
    function Extent: TDoubleRect; override;
    function GetNearestPoint(
      const AParams: TNearestPointParams;
      out AResults: TNearestPointResults): Boolean; override;
    procedure MovePoint(var AIndex: Integer; const ANewPos: TDoublePoint); override;
    procedure MovePointEx(var AIndex: Integer; AXIndex, AYIndex: Integer;
        const ANewPos: TDoublePoint); override;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property CloseCircle: Boolean read FCloseCircle write SetCloseCircle default false;
    property Filled: Boolean read FFilled write SetFilled default false;
    property LinePen: TPen read FLinePen write SetLinePen;
    property MarkPositions;
    property Marks;
    property OriginX: Double read FOriginX write SetOriginX stored IsOriginXStored;
    property OriginY: Double read FOriginY write SetOriginY stored IsOriginYStored;
    property Pointer;
    property ShowPoints: Boolean read FShowPoints write SetShowPoints;
    property Source;
    property Styles;
    property OnCustomDrawPointer;
    property OnGetPointerStyle;
  end;

implementation

uses
  Math,
  TAChartStrConsts, TATypes, TACustomSource, TAGeometry, TAGraph;

const
  TWO_PI = 2 * pi;
  PI_1_2 = pi / 2;
  PI_3_2 = (3 / 2) * pi;
  PI_1_4 = pi / 4;
  PI_3_4 = (3 / 4) * pi;
  PI_5_4 = (5 / 4) * pi;
  PI_7_4 = (7 / 4) * pi;

{ TPieSlice }

function TPieSlice.Angle: Double;
begin
  if FNextAngle < FPrevAngle then
    Result := TWO_PI + FNextAngle - FPrevAngle
  else
    Result := FNextAngle - FPrevAngle;
end;

function TPieSlice.CenterAngle: Double;
begin
  if FNextAngle <= FPrevAngle then
    Result := NormalizeAngle((TWO_PI + FNextAngle + FPrevAngle) * 0.5)
  else
    Result := NormalizeAngle((FNextAngle + FPrevAngle) * 0.5);
end;

{ FixedNextAngle is guaranteed to be greater than FPrevAngle }
function TPieSlice.FixedNextAngle: Double;
begin
  if FPrevAngle > FNextAngle then
    Result := FNextAngle + TWO_PI
  else
    Result := FNextAngle;
end;


{ TLegendItemPieS }

procedure TLegendItemPie.Draw(ADrawer: IChartDrawer; const ARect: TRect);
const
  INDEX_TO_ANGLE = 360 * 16 / Length(FColors);
var
  i: Integer;
begin
  inherited Draw(ADrawer, ARect);
  for i := 0 to High(FColors) do begin
    ADrawer.SetBrushColor(FColors[i]);
    with MakeSquare(ARect) do
      ADrawer.RadialPie(
        Left, Top, Right, Bottom,
        Round(i * INDEX_TO_ANGLE), Round(INDEX_TO_ANGLE));
  end;
end;

procedure TLegendItemPie.SetColors(AIndex: Integer; AValue: TChartColor);
begin
  FColors[AIndex] := AValue;
end;


{ TLegendItemPieSlice }

procedure TLegendItemPieSlice.Draw(ADrawer: IChartDrawer; const ARect: TRect);
const
  ANGLE = 30 * 16;
begin
  inherited Draw(ADrawer, ARect);
  ADrawer.SetBrushParams(bsSolid, ColorDef(Color, clRed));
  ADrawer.RadialPie(
    2 * ARect.Left - ARect.Right, ARect.Top, ARect.Right, ARect.Bottom,
    -ANGLE, 2 * ANGLE);
end;

{ TCustomPieSeries }

function TCustomPieSeries.AddPie(
  AValue: Double; AText: String; AColor: TColor): Integer;
begin
  Result := AddXY(GetXMaxVal + 1, AValue, AText, AColor);
end;

procedure TCustomPieSeries.Assign(ASource: TPersistent);
begin
  if ASource is TCustomPieSeries then
    with TCustomPieSeries(ASource) do begin
      Self.FAspectRatio := FAspectRatio;
      Self.FExploded := FExploded;
      Self.FFixedRadius := FFixedRadius;
      Self.FInnerRadiusPercent := FInnerRadiusPercent;
      Self.FMarkDistancePercent := FMarkDistancePercent;
      Self.FMarkPositionCentered := FMarkPositionCentered;
      Self.FOrientation := FOrientation;
      Self.FRotateLabels := FRotateLabels;
      Self.FStartAngle := FStartAngle;
    end;
  inherited Assign(ASource);
end;

function TCustomPieSeries.CalcBorderPoint(ASlice: TPieSlice;
  ARadius, AAngle: Double): TPoint;
begin
  result := ASlice.FBase + FixAspectRatio(RotatePointX(ARadius, -AAngle));
end;

function TCustomPieSeries.CalcInnerRadius: Integer;
begin
  Result := Round(0.01 * FRadius * FInnerRadiusPercent);
end;

constructor TCustomPieSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewAngle := 60;
  FEdgePen := TPen.Create;
  FEdgePen.OnChange := @StyleChanged;

  // Tell DataPointTool that data points are not at their regular x/y coordinates
  FSpecialPointPos := True;
end;

destructor TCustomPieSeries.Destroy;
begin
  FreeAndNil(FEdgePen);
  inherited;
end;

procedure TCustomPieSeries.Draw(ADrawer: IChartDrawer);
const
  STEP = 4;
var
  scaled_depth: Integer;
  innerRadius: Integer;
  drawslices: array of TPieSlice;

  function PrevSlice(ASlice: TPieSlice): TPieSlice;
  var
    slice: TPieSlice;
  begin
    for slice in drawSlices do
      if slice.FNextAngle = ASlice.FPrevAngle then begin
        Result := slice;
        exit;
      end;
  end;

  function NextSlice(ASlice: TPieSlice): TPieSlice;
  var
    slice: TPieSlice;
  begin
    for slice in drawSlices do
      if slice.FPrevAngle = ASlice.FNextAngle then begin
        Result := slice;
        exit;
      end;
  end;

  function StartEdgeVisible(ASlice: TPieSlice): Boolean;
  var
    prev: TPieSlice;
  begin
    case FOrientation of
      poNormal:
        Result := InRange(ASlice.FPrevAngle, PI_1_4, PI_5_4);
      poHorizontal:
        Result := not InRange(ASlice.FPrevAngle, PI_1_2, PI_3_2);
      poVertical:
        Result := InRange(ASlice.FPrevAngle, 0, pi);
    end;
    if Result then begin
      prev := PrevSlice(ASlice);
      Result := SliceExploded(ASlice) or SliceExploded(prev) or not prev.FVisible;
    end;
  end;

  function EndEdgeVisible(ASlice: TPieSlice): Boolean;
  var
    next: TPieSlice;
  begin
    case FOrientation of
      poNormal:
        Result := not InRange(ASlice.FNextAngle, PI_1_4, PI_5_4);
      poHorizontal:
        Result := InRange(ASlice.FNextAngle, PI_1_2, PI_3_2);
      poVertical:
        Result := InRange(ASlice.FNextAngle, pi, TWO_PI);
    end;
    if Result then begin
      next := NextSlice(ASlice);
      Result := SliceExploded(ASlice) or SliceExploded(next) or not next.FVisible;
    end;
  end;

  procedure CalcArcPoints(ASlice: TPieSlice; Angle1, Angle2, ARadius: Double;
    var APoints: TPointArray);
  var
    a: Double;
    i, j, dj, n: Integer;
    isInnerArc: Boolean;
  begin
    isInnerArc := Length(APoints) > 0;
    if (innerRadius = 0) and isInnerArc then begin
      SetLength(APoints, Length(APoints) + 1);
      APoints[High(APoints)] := ASlice.FBase;
      exit;
    end;

    n := Max(Round(TWO_PI * (Angle2 - Angle1) * ARadius / STEP), 2);
    SetLength(APoints, Length(APoints) + n);
    if isInnerArc then begin
      j := Length(APoints) - 1;
      dj := -1;
    end else begin
      j := 0;
      dj := 1;
    end;
    for i := 0 to n - 1 do begin
      a := WeightedAverage(Angle1, Angle2, i / (n - 1));
      APoints[j] := CalcBorderpoint(ASlice, ARadius, a);
      inc(j, dj);
    end;
  end;

  procedure CalcSlicePoints(ASlice: TPieSlice; out APoints: TPointArray);
  var
    next_angle: Double;
  begin
    SetLength(APoints, 0);
    next_angle := ASlice.FixedNextAngle;
    CalcArcPoints(ASlice, ASlice.FPrevAngle, next_angle, FRadius, APoints);
    CalcArcPoints(ASlice, ASlice.FPrevAngle, next_angle, innerRadius, APoints);
  end;

  function Displacement: TPoint;
  begin
    case FOrientation of
      poNormal: Result := Point(scaled_depth, -scaled_depth);
      poHorizontal: Result := Point(0, scaled_depth);
      poVertical: Result := Point(scaled_depth, 0);
    end;
  end;

  { Draws the arc of a 3D slice }
  procedure DrawArc3D(ASlice: TPieSlice; AInside: Boolean);
  var
    i, numSteps: Integer;
    a: Double;
    p: Array of TPoint;
    angle1, angle2: Double;
    clr: TColor;
    r: Integer;
    isVisible: Boolean;
    ofs: TPoint;
  begin
    if AInside and (FInnerRadiusPercent = 0) then
      exit;

    if ASlice.Angle >= pi then
      isVisible := true        // this case should not happen with drawslices
    else
    if AInside then
      case FOrientation of
        poNormal:
          isVisible := InRange(ASlice.FPrevAngle, PI_3_4, PI_7_4) or
                       InRange(ASlice.FNextAngle, PI_3_4, PI_7_4);
        poHorizontal:
          isVisible := InRange(ASlice.FPrevAngle, 0, pi) or
                       InRange(ASlice.FNextAngle, 0, pi);
        poVertical:
          isVisible := InRange(ASlice.FPrevAngle, PI_1_2, PI_3_2) or
                       InRange(ASlice.FNextAngle, PI_1_2, PI_3_2);
      end
    else
      case FOrientation of
        poNormal:
          isVisible := (ASlice.FPrevAngle >= PI_7_4) or (ASlice.FPrevAngle <= PI_3_4) or
                       (ASlice.FNextAngle >= PI_7_4) or (ASlice.FNextAngle <= PI_3_4);
        poHorizontal:
          isVisible := InRange(ASlice.FPrevAngle, pi, TWO_PI) or
                       InRange(ASlice.FNextAngle, pi, TWO_PI);
        poVertical:
          isVisible := (ASlice.FPrevAngle >= PI_3_2) or (ASlice.FPrevAngle <= PI_1_2) or
                       (ASlice.FNextAngle >= PI_3_2) or (ASlice.FNextAngle <= PI_1_2);
      end;
    if not isVisible then
      exit;

    if AInside then begin
      r := innerRadius;
      case FOrientation of
        poNormal:
          begin
            angle1 := IfThen(InRange(ASlice.FPrevAngle, PI_3_4, PI_7_4), ASlice.FPrevAngle, PI_3_4);
            angle2 := IfThen(InRange(ASlice.FNextAngle, PI_3_4, PI_7_4), ASlice.FNextAngle, PI_7_4);
          end;
        poHorizontal:
          begin
            angle1 := IfThen(InRange(ASlice.FPrevAngle, 0, pi), ASlice.FPrevAngle, 0);
            angle2 := IfThen(InRange(ASlice.FNextAngle, 0, pi), ASlice.FNextAngle, pi);
          end;
        poVertical:
          begin
            angle1 := IfThen(InRange(ASlice.FPrevAngle, PI_1_2, PI_3_2), ASlice.FPrevAngle, PI_1_2);
            angle2 := IfThen(InRange(ASlice.FNextAngle, PI_1_2, PI_3_2), ASlice.FNextAngle, PI_3_2);
          end;
      end;
    end else begin
      r := FRadius;
      case FOrientation of
        poNormal:
          begin
            angle1 := IfThen(InRange(ASlice.FPrevAngle, PI_3_4, PI_7_4), PI_7_4, ASlice.FPrevAngle);
            angle2 := IfThen(InRange(ASlice.FNextAngle, PI_3_4, PI_7_4), PI_3_4, ASlice.FNextAngle);
          end;
        poHorizontal:
          begin
            angle1 := IfThen(InRange(ASlice.FPrevAngle, 0, pi), pi, ASlice.FPrevAngle);
            angle2 := Ifthen(InRange(ASlice.FNextAngle, 0, pi), TWO_PI, ASlice.FNextAngle);
          end;
        poVertical:
          begin
            angle1 := IfThen(InRange(ASlice.FPrevAngle, PI_1_2, PI_3_2), PI_3_2, ASlice.FPrevAngle);
            angle2 := IfThen(InRange(ASlice.FNextAngle, PI_1_2, PI_3_2), PI_1_2, ASlice.FNextAngle);
          end;
      end;
    end;
    if angle2 < angle1 then angle2 += TWO_PI;

    SetLength(p, 0);
    CalcArcPoints(ASlice, angle1, angle2, r, p);
    numSteps := Length(p);
    ofs := Displacement;

    SetLength(p, numSteps * 2 + 1);
    for i:=0 to numSteps - 1 do
      p[High(p) - i - 1] := p[i] + ofs;
    p[High(p)] := p[0];

    // Fill the polygon first...
    clr := GetDepthColor(SliceColor(ASlice.FOrigIndex));
    ADrawer.SetBrushParams(bsSolid, clr);
    ADrawer.SetPenParams(psSolid, clr);
    if Assigned(FOnCustomDrawPie) then
      case AInside of
        false: FOnCustomDrawPie(Self, ADrawer, ASlice, spOuterArcSide, p);
        true: FOnCustomDrawPie(self, ADrawer, ASlice, spInnerArcSide, p);
      end
    else
      ADrawer.Polygon(p, 0, Length(p));
    // then draw the border separately to suppress the lines of divided slices.
    ADrawer.SetPen(EdgePen);
    case ASlice.FFlags of
      0: ADrawer.PolyLine(p, numSteps-1, numSteps+2);  // not divided
      1: ADrawer.Polyline(p, numSteps, numSteps);      // 1st part of divided slice
      2: ADrawer.PolyLine(p, numSteps-1, numsteps+1);  // 2nd part of divided slice
    end;
  end;

  { Draws the radial edge of a 3D slice }
  procedure DrawEdge3D(ASlice: TPieSlice; Angle: Double; APart: TSlicePart);
  var
    P1, P2, ofs: TPoint;
    p: TPointArray;
  begin
    ADrawer.SetBrushParams(
      bsSolid, GetDepthColor(SliceColor(ASlice.FOrigIndex)));
    P1 := ASlice.FBase + FixAspectRatio(RotatePointX(innerRadius, -Angle));
    P2 := ASlice.FBase + FixAspectRatio(RotatePointX(FRadius, -Angle));
    ofs := Displacement;
    if Assigned(FOnCustomDrawPie) then begin
      SetLength(p, 4);
      p[0] := P1;
      p[1] := P1 + ofs;
      p[2] := P2 + ofs;
      P[3] := P2;
      FOnCustomDrawPie(Self, ADrawer, ASlice, APart, p)
    end else
      ADrawer.Polygon([P1, P1 + ofs, P2 + ofs, P2], 0, 4);
  end;

  { Draws the 3D parts of the slice: the radial edges and the arc }
  procedure DrawSlice3D(ASlice: TPieSlice);
  begin
    if EndEdgeVisible(ASlice) then
      DrawEdge3D(ASlice, ASlice.FNextAngle, spEndSide);
    if ASlice.FVisible then begin
      DrawArc3D(ASlice, false);
      DrawArc3D(ASlice, true);
    end;
    if StartEdgeVisible(ASlice) then
      DrawEdge3D(ASlice, ASlice.FPrevAngle, spStartSide);
  end;

  { Draws the top part of the slice which is visible also in 2D mode. }
  procedure DrawSlice(ASlice: TPieSlice);
  var
    p: TPointArray;
  begin
    CalcSlicePoints(ASlice, p);
    if Assigned(FOnCustomDrawPie) then
      FOnCustomDrawPie(Self, ADrawer, ASlice, spTop, p)
    else
      ADrawer.Polygon(p, 0, Length(p));
  end;

var
  prevLabelPoly: TPointArray = nil;
  ps: TPieSlice;
  r: TPoint;
begin
  if IsEmpty then exit;

  Marks.SetAdditionalAngle(0);
  Measure(ADrawer);
  innerRadius := CalcInnerRadius;
  r := FixAspectRatio(Point(FRadius, FRadius));

  if Depth > 0 then begin
    scaled_depth := ADrawer.Scale(Depth);
    SortSlices(drawSlices);
    ADrawer.SetPen(EdgePen);
    for ps in drawSlices do
      DrawSlice3D(ps);
  end;

  ADrawer.SetPen(EdgePen);
  for ps in FSlices do begin
    if not ps.FVisible then continue;
    ADrawer.SetBrushParams(bsSolid, SliceColor(ps.FOrigIndex));
    //DrawSlice(ps);
    if FInnerRadiusPercent = 0 then
      ADrawer.RadialPie(
        ps.FBase.X - r.x, ps.FBase.Y - r.y,
        ps.FBase.X + r.x, ps.FBase.Y + r.y,
        RadToDeg16(ps.FPrevAngle), RadToDeg16(ps.Angle))
    else
      DrawSlice(ps);
  end;

  if not Marks.IsMarkLabelsVisible then exit;
  for ps in FSlices do begin
    if not ps.FVisible then continue;
    with ps.FLabel do
      if FText <> '' then begin
        if RotateLabels then
          Marks.SetAdditionalAngle(ps.CenterAngle);
        Marks.DrawLabel(ADrawer, FAttachment, FCenter, FText, prevLabelPoly);
      end;
  end;
end;

function TCustomPieSeries.FindContainingSlice(const APoint: TPoint): Integer;
var
  c: TPoint;
  pointAngle: Double;
  ps: TPieSlice;
  innerRadius: Integer;
begin
  innerRadius := CalcInnerRadius;
  for ps in FSlices do begin
    if not ps.FVisible then continue;
    c := APoint - ps.FBase;
    if (FDepth > 0) then
      case FOrientation of
        poNormal: ;
        poHorizontal: c.Y := round(c.Y / FAspectRatio);
        poVertical: c.X := round(c.X / FAspectRatio);
      end;
    if not InRange(sqr(c.X) + sqr(c.Y), sqr(innerRadius), sqr(FRadius)) then
      continue;
    pointAngle := NormalizeAngle(ArcTan2(-c.Y, c.X));
    if ps.FNextAngle <= ps.FPrevAngle then begin
      if InRange(pointAngle, ps.FPrevAngle - TWO_PI, ps.FNextAngle) or
         InRange(pointAngle, ps.FPrevAngle, ps.FNextAngle + TWO_PI)
      then
        exit(ps.FOrigIndex);
    end else begin
      if InRange(pointAngle, ps.FPrevAngle, ps.FNextAngle) then
        exit(ps.FOrigIndex);
    end;
  end;
  Result := -1;
end;

function TCustomPieSeries.FixAspectRatio(P: TPoint): TPoint;
begin
  Result := P;
  if FDepth > 0 then
    case FOrientation of
      poNormal: ;
      poVertical: Result.X := round(P.X * FAspectRatio);
      poHorizontal: Result.Y := round(P.Y * FAspectRatio);
    end;
end;

procedure TCustomPieSeries.GetLegendItems(AItems: TChartLegendItems);
var
  i: Integer;
  p: TLegendItemPie;
  ps: TLegendItemPieSlice;
begin
  case Legend.Multiplicity of
    lmSingle: begin
      p := TLegendItemPie.Create(LegendTextSingle);
      for i := 0 to 2 do
        p.Colors[i] := SliceColor(i);
      AItems.Add(p);
    end;
    lmPoint:
      for i := 0 to Count - 1 do begin
        ps := TLegendItemPieSlice.Create(LegendTextPoint(i));
        ps.Color := SliceColor(i);
        AItems.Add(ps);
      end;
  end;
end;

function TCustomPieSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  idx: Integer;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;

  idx := FindContainingSlice(AParams.FPoint);
  if idx > -1 then begin
    AResults.FDist := 0;
    AResults.FIndex := idx;
    AResults.FImg := AParams.FPoint;
    Result := true;
  end;
end;

function TCustomPieSeries.GetViewAngle: Integer;
begin
  Result := round(arccos(FAspectRatio) / pi * 180);
end;

procedure TCustomPieSeries.Measure(ADrawer: IChartDrawer);
const
  MIN_RADIUS = 5;
var
  a, b: Integer;
begin
  FCenter := CenterPoint(ParentChart.ClipRect);
  if FixedRadius = 0 then begin
    // Use binary search to find maximum radius fitting into the parent chart.
    a := MIN_RADIUS;
    with Size(ParentChart.ClipRect) do
      b := Max(cx div 2, cy div 2);
    repeat
      FRadius := (a + b) div 2;
      if IsRectInRect(TryRadius(ADrawer), ParentChart.ClipRect) then
        a := FRadius
      else
        b := FRadius - 1;
    until a >= b - 1;
  end
  else begin
    FRadius := FixedRadius;
    TryRadius(ADrawer);
  end;
end;

procedure TCustomPieSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
var
  idx: Integer;
  p: TPoint;
  r1, r2, dist: Double;
begin
  Unused(AIndex, AXIndex, AYIndex);

  if FExploded then begin
    idx := FindContainingSlice(FDragOrigin);
    if idx > -1 then begin
      p := ParentChart.GraphToImage(ANewPos);
      r1 := sqrt(PointDist(FDragOrigin, FCenter));
      r2 := sqrt(PointDist(p, FCenter));
      dist := Source.Item[idx]^.X + (r2 - r1) / FRadius;
      if dist < 0 then dist := 0;  // Don't let value go negative
      ListSource.BeginUpdate;
      try
        ListSource.SetXValue(idx, dist);
      finally
        ListSource.EndUpdate;
      end;
      FDragOrigin := p;
    end;
  end;
end;

procedure TCustomPieSeries.SetEdgePen(AValue: TPen);
begin
  if FEdgePen = AValue then exit;
  FEdgePen.Assign(AValue);
end;

procedure TCustomPieSeries.SetExploded(AValue: Boolean);
begin
  if FExploded = AValue then exit;
  FExploded := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetFixedRadius(AValue: TChartDistance);
begin
  if FFixedRadius = AValue then exit;
  FFixedRadius := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetInnerRadiusPercent(AValue: Integer);
begin
  AValue := EnsureRange(AValue, 0, 100);
  if FInnerRadiusPercent = AValue then exit;
  FInnerRadiusPercent := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetMarkPositionCentered(AValue: Boolean);
begin
  if FMarkPositionCentered = AValue then exit;
  FMarkPositionCentered := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetMarkPositions(AValue: TPieMarkPositions);
begin
  if FMarkPositions = AValue then exit;
  FMarkPositions := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetMarkDistancePercent(AValue: Boolean);
begin
  if FMarkDistancePercent = AValue then exit;
  FMarkDistancePercent := AVAlue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetOnCustomDrawPie(AValue: TCustomDrawPieEvent);
begin
  if TMethod(FOnCustomDrawPie) = TMethod(AValue) then exit;
  FOnCustomDrawPie := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetOrientation(AValue: TPieOrientation);
begin
  if FOrientation = AValue then exit;
  FOrientation := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetRotateLabels(AValue: Boolean);
begin
  if FRotateLabels = AValue then exit;
  FRotateLabels := AValue;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetStartAngle(AValue: Integer);
begin
  if FStartAngle = AValue then exit;
  FStartAngle := AValue mod 360;
  UpdateParentChart;
end;

procedure TCustomPieSeries.SetViewAngle(AValue: Integer);
begin
  if GetViewAngle = AValue then exit;
  FAspectRatio := cos(pi * EnsureRange(AValue, 0, 89) / 180);
  UpdateParentChart;
end;

function TCustomPieSeries.SliceColor(AIndex: Integer): TColor;
const
  SLICE_COLORS: array [0..14] of TColor = (
    clRed, clGreen, clYellow, clBlue, clWhite, clGray, clFuchsia,
    clTeal, clNavy, clMaroon, clLime, clOlive, clPurple, clSilver, clAqua);
begin
  if AIndex < Count then
    Result := Source[AIndex]^.Color
  else
    Result := clTAColor;
  Result := ColorDef(Result, SLICE_COLORS[AIndex mod Length(SLICE_COLORS)]);
end;

function TCustomPieSeries.SliceExploded(ASlice: TPieSlice): Boolean;
begin
  Result := ASlice.FBase <> FCenter;
end;

type
  TAngleFunc = function (ASlice: TPieSlice): Double;

function GetAngleForSortingNormal(ASlice: TPieSlice): Double;
var
  next_angle: Double;
begin
  next_angle := ASlice.FixedNextAngle;
  if ((ASlice.FPrevAngle >= PI_5_4) or (ASlice.FPrevAngle <= PI_1_4)) and
     InRange(ASlice.FNextAngle, PI_1_4, PI_5_4)
  then
    // Slice crossing the 45° point --> must be last slice to draw
    Result := 0
  else
  if InRange(ASlice.FPrevAngle, PI_1_4, PI_5_4) and InRange(next_angle, PI_5_4, TWO_PI + PI_1_4)
  then
    // Slice crossing the 225° point --> must be first slice to draw
    Result := pi
  else
    Result := IfThen(InRange(ASlice.FPrevAngle, PI_1_4, PI_5_4), ASlice.FPrevAngle, next_angle) - PI_1_4;
end;

function GetAngleForSortingHoriz(ASlice: TPieSlice): Double;
begin
  if (InRange(ASlice.FPrevAngle, 0, PI_1_2) or InRange(ASlice.FPrevAngle, PI_3_2, TWO_pi)) and
     InRange(ASlice.FNextAngle, PI_1_2, PI_3_2)
  then
    // Most backward slice --> must be first to draw
    Result := pi
  else
  if InRange(ASlice.FPrevAngle, PI_1_2, PI_3_2) and
     (InRange(ASlice.FNextAngle, PI_3_2, TWO_PI) or InRange(ASlice.FNextAngle, 0, PI_1_2))
  then
    // Most foremost slice --> must be the last to draw
    Result := 0
  else
    Result := IfThen(InRange(ASlice.FPrevAngle, PI_3_2, TWO_PI) or InRange(ASlice.FPrevAngle, 0, PI_1_2),
      ASlice.FPrevAngle, ASlice.FNextAngle) - PI_3_2
end;

function GetAngleForSortingVert(ASlice: TPieSlice): Double;
var
  next_angle: Double;
begin
  next_angle := ASlice.FixedNextAngle;
  if (ASlice.FPrevAngle <= pi) and (ASlice.FNextAngle > pi) then
    // Slice crossing the 180° point, most backward slice --> first to draw
    Result := pi
  else
  if (ASlice.FPrevAngle >= pi) and (ASlice.FNextAngle < pi) then
    // Slice crossing the 0° point, most foreward slice --> last to draw
    Result := 0
  else
    Result :=IfThen(ASlice.FPrevAngle > pi, ASlice.FPrevAngle, next_angle);
end;

procedure TCustomPieSeries.SortSlices(out ASlices: TSliceArray);

  function CompareSlices(ASlice1, ASlice2: TPieSlice; AngleFunc: TAngleFunc): Integer;
  var
    angle1, angle2: Double;
  begin
    angle1 := AngleFunc(ASlice1);
    angle2 := AngleFunc(ASlice2);
    Result := CompareValue(cos(angle1), cos(angle2));
  end;

  procedure QuickSort(const L, R: Integer; AngleFunc: TAngleFunc);
  var
    i, j, m: Integer;
    ps: TPieSlice;
  begin
    i := L;
    j := R;
    m := (L + R) div 2;
    while (i <= j) do begin
      while CompareSlices(ASlices[i], ASlices[m], AngleFunc) < 0 do inc(i);
      while CompareSlices(ASlices[j], ASlices[m], AngleFunc) > 0 do dec(j);
      if i <= j then begin
        ps := ASlices[i];
        ASlices[i] := ASlices[j];
        ASlices[j] := ps;
        inc(i);
        dec(j);
      end;
      if L < j then QuickSort(L, j, AngleFunc);
      if i < R then QuickSort(i, R, AngleFunc);
    end;
  end;

var
  i, j: Integer;
  compareFunc: TAngleFunc;
begin
  SetLength(ASlices, Length(FSlices) + 1);
  j := 0;
  for i:=0 to High(FSlices) do begin
    if FSlices[i].Angle >= pi then begin
      ASlices[j] := FSlices[i];
      ASlices[j].FNextAngle := FSlices[i].CenterAngle;
      ASlices[j].FFlags := 1;    // 1st piece of divided slice
      ASlices[j+1] := FSlices[i];
      ASlices[j+1].FPrevAngle := ASlices[j].FNextAngle;
      ASlices[j+1].FFlags := 2;   // 2nd piece of divided slice
      inc(j, 2);
    end else begin
      ASlices[j] := FSlices[i];
      inc(j);
    end;
  end;
  case FOrientation of
    poNormal: compareFunc := @GetAngleForSortingNormal;
    poHorizontal: compareFunc := @GetAngleForSortingHoriz;
    poVertical: compareFunc := @GetAngleForSortingVert;
  end;
  SetLength(ASlices, j);
  QuickSort(0, High(ASlices), compareFunc);
end;

function TCustomPieSeries.TryRadius(ADrawer: IChartDrawer): TRect;

  function EndPoint(AAngle, ARadius: Double): TPoint;
  begin
    Result := FixAspectRatio(RotatePointX(ARadius, -AAngle));
  end;

  function LabelExtraDist(APoly: TPointArray; AAngle: Double): Double;
  const
    ALMOST_INF = 1e100;
  var
    sa, ca: Extended;
    denom, t, tmin: Double;
    a, b, d: TPoint;
    i: Integer;
  begin
    // x = t * ca; y = t * sa
    // (t * ca - a.x) * dy = (t * sa - a.y) * dx
    // t * (ca * dy - sa * dx) = a.x * dy - a.y * dx
    SinCos(-Pi - AAngle, sa, ca);
    b := APoly[High(APoly)];
    tmin := ALMOST_INF;
    for i := 0 to High(APoly) do begin
      a := APoly[i];
      d := b - a;
      denom := ca * d.Y - sa * d.X;
      if denom <> 0 then begin
        t := (a.X * d.Y - a.Y * d.X) / denom;
        if t > 0 then
          tmin := Min(tmin, t);
      end;
      b := a;
    end;
    if tmin = ALMOST_INF then // Should never happen.
      Result := 0
    else
      Result := Norm([tmin * ca, tmin * sa]);
  end;

  procedure PrepareLabel(
    var ALabel: TLabelParams; AIndex: Integer; AAngle: Double);
  var
    i: Integer;
    p: TPointArray;

    function Ofs(AAngle: Double): TPoint;
    var
      d: Double;
    begin
      d := IfThen(FMarkDistancePercent, Marks.Distance * FRadius / 100, Marks.Distance);
      if not Marks.DistanceToCenter then
        d += LabelExtraDist(p, AAngle);
      Result := EndPoint(AAngle, d);
    end;

  begin
    with ALabel do begin
      FCenter := FAttachment;
      if not Marks.IsMarkLabelsVisible then exit;
      FText := FormattedMark(AIndex);
      if FText = '' then exit;
      if RotateLabels then
        Marks.SetAdditionalAngle(AAngle);
      p := Marks.GetLabelPolygon(ADrawer, ADrawer.TextExtent(FText));
      case MarkPositions of
        pmpAround:
          FCenter += Ofs(AAngle);
        pmpInside:
          FCenter -= Ofs(AAngle);
        pmpLeftRight:
          FCenter += Ofs(IfThen(InRange(AAngle, PI_1_2, PI_3_2), pi, 0));
      end;
      for i := 0 to High(p) do
        ExpandRect(Result, p[i] + FCenter);
    end;
  end;

const
  MARGIN = 4;
var
  i, j: Integer;
  di: PChartDataItem;
  prevAngle: Double;
  a, total: Double;
  scaled_depth: Integer;
  start_angle: Double;
  next_angle: Double;
begin
  Result.TopLeft := FCenter;
  Result.BottomRight := FCenter;
  scaled_depth := ADrawer.Scale(Depth);
  start_angle := NormalizeAngle((FStartAngle mod 360) / 180 * pi);
  SetLength(FSlices, Count);
  j := 0;
  // This is a workaround for db source invalidating the cache due to
  // unnecessary "dataset changed" events.
  total := Source.ValuesTotal;
  if total = 0 then
    exit;
  prevAngle := start_angle;
  for i := 0 to Count - 1 do begin
    di := Source[i];
    if IsNan(di^.Y) then continue;
    with FSlices[j] do begin
      FOrigIndex := i;
      FPrevAngle := prevAngle;
      next_angle := FPrevAngle + CycleToRad(di^.Y / total);
      FNextAngle := NormalizeAngle(next_angle);
      FVisible := not IsNan(di^.X);
      if FVisible then begin
        FBase := FCenter;
        a := CenterAngle;
        if Exploded and (di^.X > 0) then
          FBase += EndPoint(a, FRadius * di^.X);
        ExpandRect(Result, FBase, FRadius, -FPrevAngle, -next_angle);
        if Depth > 0 then
          ExpandRect(
            Result, FBase + Point(scaled_depth, -scaled_depth),
            FRadius, -FPrevAngle, -next_angle);
        if FMarkPositionCentered  then
          FLabel.FAttachment := EndPoint(a, (CalcInnerRadius + FRadius) div 2) + FBase
        else
          FLabel.FAttachment := EndPoint(a, FRadius) + FBase;
        PrepareLabel(FLabel, i, a);
      end;
      prevAngle := FNextAngle;
    end;
    inc(j);
  end;
  SetLength(FSlices, j);
  InflateRect(Result, MARGIN, MARGIN);
end;


{ TPolarSeries }

procedure TPolarSeries.Assign(ASource: TPersistent);
begin
  if ASource is TPolarSeries then
    with TPolarSeries(ASource) do begin
      Self.Brush := FBrush;
      Self.LinePen := FLinePen;
      Self.FOriginX := FOriginX;
      Self.FOriginY := FOriginY;
      Self.FShowPoints := FShowPoints;
    end;
  inherited Assign(ASource);
end;

constructor TPolarSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
  FLinePen := TPen.Create;
  FLinePen.OnChange := @StyleChanged;
  FPointer := TSeriesPointer.Create(FChart);
  FFilled := true;      // needed for SetFilled to execute its code
  FShowPoints := true;  // needed for SetShowPoints to execute its code
  SetFilled(false);
  SetShowPoints(false);
end;

destructor TPolarSeries.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FLinePen);
  inherited;
end;

procedure TPolarSeries.Draw(ADrawer: IChartDrawer);
var
  originPt: TPoint;
  fill: Boolean;
  pts: Array of TPoint;

  procedure DrawYLevel(AYIndex: Integer);
  var
    i: Integer;
    gp: TDoublepoint;
    cnt: Integer;
    firstPoint, lastPoint: TPoint;
    firstPointSet: Boolean;

    procedure DrawPart;
    begin
      if cnt = 0 then
        exit;

      ADrawer.Brush := FBrush;
      if Styles <> nil then
        Styles.Apply(ADrawer, AYIndex);

      if fill then begin
        pts[cnt] := originPt;
        ADrawer.SetPenParams(psClear, clBlack);
        ADrawer.Polygon(pts, 0, cnt + 1);
      end;
      ADrawer.Pen := LinePen;
      if Styles <> nil then
        Styles.Apply(ADrawer, AYIndex);
      ADrawer.PolyLine(pts, 0, cnt);
    end;

  begin
    firstPointSet := false;
    cnt := 0;
    for i := 0 to Count - 1 do begin
      gp := GraphPoint(i, AYIndex);
      if IsNaN(gp) then begin
        DrawPart;
        cnt := 0;
      end else begin
        if IsRotated then Exchange(gp.X, gp.Y);
        lastPoint := FChart.GraphToImage(gp);
        pts[cnt] := lastPoint;
        cnt += 1;
        if not firstPointSet then begin
          firstPoint := lastPoint;
          firstPointSet := true;
        end;
      end;
    end;
    DrawPart;
    if firstPointSet and CloseCircle then begin
      SetLength(pts, 3);
      pts[0] := lastPoint;
      pts[1] := firstPoint;
      cnt := 2;
      DrawPart;
    end;
  end;

var
  j: Integer;
begin
  originPt := ParentChart.GraphToImage(DoublePoint(OriginX, OriginY));
  fill := FFilled and (FBrush.Style <> bsClear);
  SetLength(pts, Count + 1);  // +1 for origin
  for j := 0 to Source.YCount-1 do
    DrawYLevel(j);
  for j := 0 to Source.YCount-1 do begin
    PrepareGraphPoints(j);
    DrawLabels(ADrawer, j);
    DrawPointers(ADrawer, j, true);
  end;
end;

function TPolarSeries.Extent: TDoubleRect;
var
  i, j: Integer;
begin
  //FindExtentInterval(DoubleRect(0, 0, 0, 0), false);
  PrepareAngleCache;
  Result := EmptyExtent;
  for i := 0 to Count - 1 do
    for j := 0 to Source.YCount-1 do
      ExpandRect(Result, GraphPoint(i, j));
end;

function TPolarSeries.GetLabelDataPoint(AIndex, AYIndex: Integer): TDoublePoint;
begin
  Result := GraphPoint(AIndex, AYIndex);
  if IsRotated then Exchange(Result.X, Result.Y);
end;

procedure TPolarSeries.GetLegendItems(AItems: TChartLegendItems);
var
  p: TSeriesPointer;
  li: TLegendItemLinePointer;
  s: TChartStyle;
  i: Integer;
  lBrush: TBrush;
begin
  if ShowPoints then
    p := Pointer
  else
    p := nil;

  case Legend.Multiplicity of
    lmSingle:
      AItems.Add(TLegendItemLinePointer.Create(LinePen, p, LegendTextSingle));
    lmPoint:
      for i := 0 to Count - 1 do begin
        li := TLegendItemLinePointer.Create(LinePen, p, LegendTextPoint(i));
        li.Color := GetColor(i);
        AItems.Add(li);
      end;
    lmStyle:
      if Styles <> nil then begin
        if Assigned(p) then lBrush := p.Brush else lBrush := nil;
        for s in Styles.Styles do
          AItems.Add(TLegendItemLinePointer.CreateWithBrush(
            IfThen((LinePen <> nil) and s.UsePen, s.Pen, LinePen) as TPen,
            IfThen(s.UseBrush, s.Brush, lBrush) as TBrush,
            p,
            LegendTextStyle(s)
          ));
        end;
  end;
end;

function TPolarSeries.GetNearestPoint(const AParams: TNearestPointParams;
  out AResults: TNearestPointResults): Boolean;
var
  dist: Integer;
  gp: TDoublePoint;
  i: Integer;
begin
  Result := false;
  AResults.FDist := Sqr(AParams.FRadius) + 1;  // the dist func does not calc sqrt
  AResults.FIndex := -1;
  AResults.FXIndex := 0;
  AResults.FYIndex := 0;
  if Length(FAngleCache) = 0 then
    exit;

  dist := AResults.FDist;
  for i := 0 to Count - 1 do begin
    gp := GraphPoint(i, 0);
    if IsNan(gp) then
      continue;
    // Find nearest point of datapoint at (x, y)
    if (nptPoint in AParams.FTargets) and (nptPoint in ToolTargets) then
    begin
      dist := Min(dist, ToolTargetDistance(AParams, gp, i, 0, 0));
    end;
    if dist >= AResults.FDist then
      continue;

    AResults.FDist := dist;
    AResults.FIndex := i;
    AResults.FValue := DoublePoint(gp.y*cos(gp.x), gp.y*sin(gp.x));
    AResults.FImg := ParentChart.GraphToImage(gp);
    if dist = 0 then break;
  end;
  Result := AResults.FIndex >= 0;
end;

function TPolarSeries.GraphPoint(AIndex, AYIndex: Integer): TDoublePoint;
var
  r: Double;
begin
  r := Source.Item[AIndex]^.GetY(AYIndex);
  with FAngleCache[AIndex] do
    Result := DoublePoint(r * FCos + OriginX, r * FSin + OriginY);
end;

function TPolarSeries.IsOriginXStored: Boolean;
begin
  Result := not SameValue(OriginX, 0.0);
end;

function TPolarSeries.IsOriginYStored: Boolean;
begin
  Result := not SameValue(OriginY, 0.0);
end;

{ ANewPos is in cartesioan coordinates. Convert to polar coordinates and store
 in ListSource }
procedure TPolarSeries.MovePoint(var AIndex: Integer;
  const ANewPos: TDoublePoint);
var
  p: TDoublePoint;
  r, phi: Double;
begin
  if not InRange(AIndex, 0, Count - 1) then exit;
  p := ANewPos - DoublePoint(OriginX, OriginY);
  r := Sqrt(sqr(p.x) + sqr(p.y));
  phi := arctan2(p.y, p.x);
  with ListSource do begin
    AIndex := SetXValue(AIndex, phi);
    SetYValue(AIndex, r);
  end;
end;

procedure TPolarSeries.MovePointEx(var AIndex: Integer;
  AXIndex, AYIndex: Integer; const ANewPos: TDoublePoint);
begin
  Unused(AXIndex, AYIndex);
  MovePoint(AIndex, ANewPos);
end;

procedure TPolarSeries.PrepareAngleCache;
var
  i: Integer;
  s, c: Extended;
begin
  if Length(FAngleCache) = Count then exit;
  SetLength(FAngleCache, Count);
  for i := 0 to Count - 1 do begin
    SinCos(Source[i]^.X, s, c);
    FAngleCache[i].FSin := s;
    FAngleCache[i].FCos := c;
  end;
end;

procedure TPolarSeries.PrepareGraphPoints(AYIndex: Integer);
var
  i: Integer;
begin
  SetLength(FGraphPoints, Count);
  for i := 0 to Count - 1 do begin
    FGraphPoints[i] := GraphPoint(i, AYIndex);
    if IsRotated then Exchange(FGraphPoints[i].X, FGraphPoints[i].Y);
  end;
end;

procedure TPolarSeries.SetBrush(AValue: TBrush);
begin
  if FBrush = AValue then exit;
  FBrush.Assign(AValue);
  UpdateParentChart;
end;

procedure TPolarSeries.SetCloseCircle(AValue: Boolean);
begin
  if FCloseCircle = AValue then exit;
  FCloseCircle := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetFilled(AValue: Boolean);
begin
  if FFilled = AValue then exit;
  FFilled := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetLinePen(AValue: TPen);
begin
  if FLinePen = AValue then exit;
  FLinePen.Assign(AValue);
  UpdateParentChart;
end;

procedure TPolarSeries.SetOriginX(AValue: Double);
begin
  if SameValue(FOriginX, AValue) then exit;
  FOriginX := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetOriginY(AValue: Double);
begin
  if SameValue(FOriginY, AValue) then exit;
  FOriginY := AValue;
  UpdateParentChart;
end;

procedure TPolarSeries.SetShowPoints(AValue: Boolean);
begin
  if ShowPoints = AValue then exit;
  FShowPoints := AValue;
  Pointer.Visible := FShowPoints;;
  UpdateParentChart;
end;

procedure TPolarSeries.SourceChanged(ASender: TObject);
begin
  FAngleCache := nil;
  inherited;
end;

procedure TPolarSeries.UpdateLabelDirectionReferenceLevel(AIndex, AYIndex: Integer;
  var ALevel: Double);
begin
  Unused(AYIndex);
  // Level is constant, we only need to calculate it once.
  if AIndex = 0 then
    ALevel := AxisToGraphY(OriginY);
end;


initialization

  RegisterSeriesClass(TPolarSeries, @rsPolarSeries);

end.

