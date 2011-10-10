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
  Classes, SysUtils, Types,
  TAChartAxisUtils, TAChartUtils, TACustomSource, TADrawUtils,
  TATransformations, TATypes;

const
  DEF_TICK_LENGTH = 4;

type

  { TChartMinorAxis }

  TChartMinorAxis = class(TChartBasicAxis)
  strict private
    function GetMarks: TChartMinorAxisMarks; inline;
    procedure SetMarks(AValue: TChartMinorAxisMarks);
  protected
    function GetDisplayName: String; override;
  strict protected
    function GetAlignment: TChartAxisAlignment; override;
    procedure SetAlignment(AValue: TChartAxisAlignment); override;
    procedure StyleChanged(ASender: TObject); override;
  public
    constructor Create(ACollection: TCollection); override;
    function GetMarkValues(AMin, AMax: Double): TChartValueTextArray;
  published
    property Marks: TChartMinorAxisMarks read GetMarks write SetMarks;
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
    property ParentAxis: TChartAxis read FAxis;
  end;

  TChartAxisGroup = record
    FCount: Integer;
    FFirstMark: Integer;
    FLastMark: Integer;
    FSize: Integer;
    FTitleSize: Integer;
  end;

  TChartAxisPen = class(TChartPen)
  published
    property Visible default false;
  end;

  { TChartAxis }

  TChartAxis = class(TChartBasicAxis)
  strict private
    FListener: TListener;
    FMarkValues: TChartValueTextArray;

    procedure GetMarkValues;
    procedure VisitSource(ASource: TCustomChartSource; var AData);
  private
    FAxisRect: TRect;
    FGroupIndex: Integer;
    FTitleRect: TRect;
    function MakeValuesInRangeParams(AMin, AMax: Double): TValuesInRangeParams;
  strict private
    FAlignment: TChartAxisAlignment;
    FAxisPen: TChartAxisPen;
    FGroup: Integer;
    FHelper: TAxisDrawHelper;
    FInverted: Boolean;
    FMinors: TChartMinorAxisList;
    FOnMarkToText: TChartAxisMarkToTextEvent;
    FRange: TChartRange;
    FTitle: TChartAxisTitle;
    FTransformations: TChartAxisTransformations;
    FZPosition: TChartDistance;

    function GetMarks: TChartAxisMarks; inline;
    procedure SetAxisPen(AValue: TChartAxisPen);
    procedure SetGroup(AValue: Integer);
    procedure SetInverted(AValue: Boolean);
    procedure SetMarks(AValue: TChartAxisMarks);
    procedure SetMinors(AValue: TChartMinorAxisList);
    procedure SetOnMarkToText(AValue: TChartAxisMarkToTextEvent);
    procedure SetRange(AValue: TChartRange);
    procedure SetTitle(AValue: TChartAxisTitle);
    procedure SetTransformations(AValue: TChartAxisTransformations);
    procedure SetZPosition(AValue: TChartDistance);

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
    procedure Draw;
    procedure DrawTitle(const ACenter: TPoint; ASize: Integer);
    function GetChart: TCustomChart; inline;
    function GetTransform: TChartAxisTransformations;
    function IsVertical: Boolean; inline;
    procedure Measure(
      const AExtent: TDoubleRect; var AMeasureData: TChartAxisGroup);
    procedure PrepareHelper(
      ADrawer: IChartDrawer; const ATransf: ICoordTransformer;
      AClipRect: PRect; AMaxZPosition: Integer);
    procedure UpdateBounds(var AMin, AMax: Double);
  published
    property Alignment default calLeft;
    property Arrow;
    property AxisPen: TChartAxisPen read FAxisPen write SetAxisPen;
    property Group: Integer read FGroup write SetGroup default 0;
    // Inverts the axis scale from increasing to decreasing.
    property Inverted: boolean read FInverted write SetInverted default false;
    property Marks: TChartAxisMarks read GetMarks write SetMarks;
    property Minors: TChartMinorAxisList read FMinors write SetMinors;
    property Range: TChartRange read FRange write SetRange;
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

  TChartAxisEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TChartAxis;
    property Current: TChartAxis read GetCurrent;
  end;

  { TChartAxisList }

  TChartAxisList = class(TCollection)
  strict private
    FChart: TCustomChart;
    FOnVisitSources: TChartOnVisitSources;
    function GetAxes(AIndex: Integer): TChartAxis;
  strict private
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
    procedure Draw(ACurrentZ: Integer; var AIndex: Integer);
    function GetAxis(AIndex: Integer): TChartAxis;
    function GetEnumerator: TChartAxisEnumerator;
    function Measure(const AExtent: TDoubleRect): TChartAxisMargins;
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
    FImageLo, FImageHi: Integer;
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
  procedure UpdateBoundsByAxisRange(
    AAxisList: TChartAxisList; AIndex: Integer; var AMin, AMax: Double);

implementation

uses
  LResources, Math, PropEdits, TAGeometry, TAMath;

var
  VIdentityTransform: TChartAxisTransformations;

function AxisZCompare(AItem1, AItem2: Pointer): Integer; forward;

function AxisGroupCompare(AItem1, AItem2: Pointer): Integer;
begin
  Result := TChartAxis(AItem1).Group - TChartAxis(AItem2).Group;
  if Result = 0 then
    Result := AxisZCompare(AItem1, AItem2);
end;

function AxisZCompare(AItem1, AItem2: Pointer): Integer;
var
  a1: TChartAxis absolute AItem1;
  a2: TChartAxis absolute AItem2;
begin
  Result := a1.ZPosition - a2.ZPosition;
  if Result = 0 then
    Result := a2.Index - a1.Index;
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

procedure UpdateBoundsByAxisRange(
  AAxisList: TChartAxisList; AIndex: Integer; var AMin, AMax: Double);
begin
  if not InRange(AIndex, 0, AAxisList.Count - 1) then exit;
  AAxisList[AIndex].UpdateBounds(AMin, AMax);
end;

{ TChartAxisEnumerator }

function TChartAxisEnumerator.GetCurrent: TChartAxis;
begin
  Result := TChartAxis(inherited GetCurrent);
end;

{ TChartMinorAxis }

constructor TChartMinorAxis.Create(ACollection: TCollection);
begin
  inherited Create(ACollection, (ACollection as TChartMinorAxisList).GetChart);
  FMarks := TChartMinorAxisMarks.Create(
    (ACollection as TChartMinorAxisList).GetChart);
  with Intervals do begin
    Options := [aipUseCount, aipUseMinLength];
    MinLength := 5;
  end;
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

function TChartMinorAxis.GetMarks: TChartMinorAxisMarks;
begin
  Result := TChartMinorAxisMarks(inherited Marks);
end;

function TChartMinorAxis.GetMarkValues(AMin, AMax: Double): TChartValueTextArray;
var
  vp: TValuesInRangeParams;
begin
  if not Visible then exit(nil);
  with Collection as TChartMinorAxisList do
    vp := ParentAxis.MakeValuesInRangeParams(AMin, AMax);
  vp.FFormat := Marks.Format;
  Marks.DefaultSource.ValuesInRange(vp, Result);
end;

procedure TChartMinorAxis.SetAlignment(AValue: TChartAxisAlignment);
begin
  Unused(AValue);
  raise EChartError.Create('TChartMinorAxis.SetAlignment');
end;

procedure TChartMinorAxis.SetMarks(AValue: TChartMinorAxisMarks);
begin
  inherited Marks := AValue;
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
      Self.FAxisPen.Assign(AxisPen);
      Self.FGroup := Group;
      Self.FInverted := Inverted;
      Self.FRange.Assign(Range);
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
  FAxisPen := TChartAxisPen.Create;
  FAxisPen.OnChange := @StyleChanged;
  FListener := TListener.Create(@FTransformations, @StyleChanged);
  FMarks := TChartAxisMarks.Create(ACollection.Owner as TCustomChart);
  FMinors := TChartMinorAxisList.Create(Self);
  FRange := TChartRange.Create(ACollection.Owner as TCustomChart);
  TickLength := DEF_TICK_LENGTH;
  FTitle := TChartAxisTitle.Create(ACollection.Owner as TCustomChart);
end;

destructor TChartAxis.Destroy;
begin
  FreeAndNil(FTitle);
  FreeAndNil(FRange);
  FreeAndNil(FMinors);
  FreeAndNil(FListener);
  FreeAndNil(FHelper);
  FreeAndNil(FAxisPen);
  inherited;
end;

procedure TChartAxis.Draw;

  procedure DrawMinors(AFixedCoord: Integer; AMin, AMax: Double);
  const
    EPS = 1e-6;
  var
    j: Integer;
    minorMarks: TChartValueTextArray;
    m: TChartValueText;
  begin
    if IsNan(AMin) or (AMin = AMax) then exit;
    for j := 0 to Minors.Count - 1 do begin
      minorMarks := Minors[j].GetMarkValues(AMin, AMax);
      if minorMarks = nil then continue;
      with FHelper.Clone do begin
        FAxis := Minors[j];
        // Only draw minor marks strictly inside the major mark interval.
        FValueMin := FAxisTransf(AMin);
        FValueMax := FAxisTransf(AMax);
        ExpandRange(FValueMin, FValueMax, -EPS);
        try
          BeginDrawing;
          for m in minorMarks do
            DrawMark(AFixedCoord, FHelper.FAxisTransf(m.FValue), m.FText);
          EndDrawing;
        finally
          Free;
        end;
      end;
    end;
  end;

var
  fixedCoord: Integer;
  pv, v: Double;
  t: TChartValueText;
begin
  if not Visible then exit;
  if Marks.Visible then
    FHelper.FDrawer.Font := Marks.LabelFont;
  fixedCoord := TChartAxisMargins(FAxisRect)[Alignment];
  pv := SafeNaN;
  FHelper.BeginDrawing;
  FHelper.DrawAxisLine(AxisPen, fixedCoord);
  for t in FMarkValues do begin
    v := FHelper.FAxisTransf(t.FValue);
    FHelper.DrawMark(fixedCoord, v, t.FText);
    DrawMinors(fixedCoord, pv, t.FValue);
    pv := t.FValue;
  end;
  FHelper.EndDrawing;
end;

procedure TChartAxis.DrawTitle(const ACenter: TPoint; ASize: Integer);
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
  p += FHelper.FZOffset;
  Title.DrawLabel(FHelper.FDrawer, p, p, Title.Caption, dummy);
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
begin
  Result :=
    SIDE_NAME[Alignment] + VISIBLE_NAME[Visible] + INVERTED_NAME[Inverted] +
    FormatIfNotEmpty(' (%s)', Title.Caption);
end;

function TChartAxis.GetMarks: TChartAxisMarks;
begin
  Result := TChartAxisMarks(inherited Marks);
end;

procedure TChartAxis.GetMarkValues;
var
  i: Integer;
  d: TValuesInRangeParams;
  vis: TChartOnVisitSources;
  t: TChartValueText;
begin
  with FHelper do begin
    FValueMin := GetTransform.GraphToAxis(FValueMin);
    FValueMax := GetTransform.GraphToAxis(FValueMax);
    EnsureOrder(FValueMin, FValueMax);
    Marks.Range.Intersect(FValueMin, FValueMax);
    d := MakeValuesInRangeParams(FValueMin, FValueMax);
    FValueMin := GetTransform.AxisToGraph(FValueMin);
    FValueMax := GetTransform.AxisToGraph(FValueMax);
  end;
  SetLength(FMarkValues, 0);
  vis := TChartAxisList(Collection).OnVisitSources;
  if Marks.AtDataOnly and Assigned(vis) then
    vis(@VisitSource, Self, d)
  else
    Marks.SourceDef.ValuesInRange(d, FMarkValues);
  if Inverted then
    for i := 0 to High(FMarkValues) div 2 do begin
      t := FMarkValues[i];
      FMarkValues[i] := FMarkValues[High(FMarkValues) - i];
      FMarkValues[High(FMarkValues) - i] := t;
    end;

  if Assigned(FOnMarkToText) then
    for i := 0 to High(FMarkValues) do
      FOnMarkToText(FMarkValues[i].FText, FMarkValues[i].FValue);
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

function TChartAxis.MakeValuesInRangeParams(
  AMin, AMax: Double): TValuesInRangeParams;
begin
  Result.FMin := AMin;
  Result.FMax := AMax;
  Result.FFormat := Marks.Format;
  Result.FUseY := IsVertical;
  Result.FAxisToGraph := @GetTransform.AxisToGraph;
  Result.FGraphToAxis := @GetTransform.GraphToAxis;
  Result.FGraphToImage := @FHelper.GraphToImage;
  Result.FScale := @FHelper.FDrawer.Scale;
  Result.FIntervals := Intervals;
  Result.FMinStep := 0;
end;

procedure TChartAxis.Measure(
  const AExtent: TDoubleRect; var AMeasureData: TChartAxisGroup);
var
  v: Boolean;
  d: IChartDrawer;

  function TitleSize: Integer;
  begin
    if not Title.Visible or (Title.Caption = '') then exit(0);
    // Workaround for issue #19780, fix after upgrade to FPC 2.6.
    with Title.MeasureLabel(d, Title.Caption) do
      Result := IfThen(v, cx, cy);
    if Title.DistanceToCenter then
      Result := Result div 2;
    Result += d.Scale(Title.Distance);
  end;

  procedure UpdateFirstLast(ACoord, AIndex, ARMin, ARMax: Integer);
  var
    sz, fm, lm: Integer;
  begin
    // Workaround for issue #19780, fix after upgrade to FPC 2.6.
    with Marks.MeasureLabel(d, FMarkValues[AIndex].FText) do
      sz := IfThen(v, cy, cx) div 2;
    fm := sz - ACoord + ARMin;
    lm := sz - ARMax + ACoord;
    if v then
      Exchange(fm, lm);
    with AMeasureData do begin
      FFirstMark := Max(fm, FFirstMark);
      FLastMark := Max(lm, FLastMark);
    end;
  end;

var
  sz, rmin, rmax, c, i, j, minc, maxc, mini, maxi: Integer;
  minorValues: TChartValueTextArray;
  pv: Double = NaN;
  cv: Double;
begin
  if not Visible then exit;
  v := IsVertical;
  d := FHelper.FDrawer;
  FHelper.FValueMin := TDoublePointBoolArr(AExtent.a)[v];
  FHelper.FValueMax := TDoublePointBoolArr(AExtent.b)[v];
  GetMarkValues;
  sz := Marks.Measure(d, not v, TickLength, FMarkValues);
  FHelper.GetClipRange(rmin, rmax);
  minc := MaxInt;
  maxc := -MaxInt;
  for i := 0 to High(FMarkValues) do begin
    cv := FMarkValues[i].FValue;
    if not IsNan(pv) then begin
      for j := 0 to Minors.Count - 1 do
        with Minors[j] do begin
          minorValues := GetMarkValues(pv, cv);
          sz := Max(Marks.Measure(d, not v, TickLength, minorValues), sz);
        end;
    end;
    pv := cv;
    // Optimization: only measure edge labels to calculate longitudinal margins.
    c := FHelper.GraphToImage(FHelper.FAxisTransf(cv));
    if not InRange(c, rmin, rmax) then continue;
    if c < minc then begin
      minc := c;
      mini := i;
    end;
    if c > maxc then begin
      maxc := c;
      maxi := i;
    end;
  end;
  with AMeasureData do begin
    FSize := Max(sz, FSize);
    FTitleSize := Max(TitleSize, FTitleSize);
  end;
  if minc < MaxInt then begin
    UpdateFirstLast(minc, mini, rmin, rmax);
    UpdateFirstLast(maxc, maxi, rmin, rmax);
  end;

  if Arrow.Visible then
    with AMeasureData do begin
      FSize := Max(d.Scale(Arrow.Width), FSize);
      FLastMark := Max(d.Scale(Arrow.Length), FLastMark);
    end;
end;

procedure TChartAxis.PrepareHelper(
  ADrawer: IChartDrawer; const ATransf: ICoordTransformer;
  AClipRect: PRect; AMaxZPosition: Integer);
begin
  FreeAndNil(FHelper);
  if IsVertical then
    FHelper := TAxisDrawHelperY.Create
  else
    FHelper := TAxisDrawHelperX.Create;
  FHelper.FAxis := Self;
  FHelper.FAxisTransf := @GetTransform.AxisToGraph;
  FHelper.FClipRect := AClipRect;
  FHelper.FDrawer := ADrawer;
  FHelper.FTransf := ATransf;
  FHelper.FZOffset.Y := Min(ZPosition, AMaxZPosition);
  FHelper.FZOffset.X := -FHelper.FZOffset.Y;
end;

procedure TChartAxis.SetAlignment(AValue: TChartAxisAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetAxisPen(AValue: TChartAxisPen);
begin
  FAxisPen.Assign(AValue);
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

procedure TChartAxis.SetMarks(AValue: TChartAxisMarks);
begin
  inherited Marks := AValue;
end;

procedure TChartAxis.SetMinors(AValue: TChartMinorAxisList);
begin
  FMinors.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartAxis.SetOnMarkToText(AValue: TChartAxisMarkToTextEvent);
begin
  if TMethod(FOnMarkToText) = TMethod(AValue) then exit;
  FOnMarkToText := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetRange(AValue: TChartRange);
begin
  if FRange = AValue then exit;
  FRange.Assign(AValue);
  StyleChanged(Range);
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
  StyleChanged(AValue);
end;

procedure TChartAxis.SetZPosition(AValue: TChartDistance);
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
    if (ASender is TAxisTransform) or (ASender is TChartAxisTransformations) then
      ZoomFull;
    Invalidate;
  end;
end;

procedure TChartAxis.UpdateBounds(var AMin, AMax: Double);
begin
  with Range do begin
    if UseMin then
      AMin := Min;
    if UseMax then
      AMax := Max;
  end;
end;

procedure TChartAxis.VisitSource(ASource: TCustomChartSource; var AData);
var
  ext: TDoubleRect;
  p: TValuesInRangeParams;
begin
  ext := ASource.Extent;
  p := TValuesInRangeParams(AData);
  p.FMin := Max(TDoublePointBoolArr(ext.a)[IsVertical], p.FMin);
  p.FMax := Min(TDoublePointBoolArr(ext.b)[IsVertical], p.FMax);
  Marks.SourceDef.ValuesInRange(p, FMarkValues);
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

procedure TChartAxisList.Draw(ACurrentZ: Integer; var AIndex: Integer);
begin
  while AIndex < FZOrder.Count do
    with TChartAxis(FZOrder[AIndex]) do begin
      if ACurrentZ < ZPosition then break;
      try
        Draw;
        DrawTitle(FCenterPoint, FGroups[FGroupIndex].FTitleSize);
      except
        Visible := false;
        raise;
      end;
      AIndex += 1;
    end;
end;

function TChartAxisList.GetAxes(AIndex: Integer): TChartAxis;
begin
  Result := TChartAxis(Items[AIndex]);
end;

function TChartAxisList.GetAxis(AIndex: Integer): TChartAxis;
begin
  for Result in Self do
    if Result.Alignment = AXIS_INDEX[AIndex] then exit;
  Result := nil;
end;

function TChartAxisList.GetEnumerator: TChartAxisEnumerator;
begin
  Result := TChartAxisEnumerator.Create(Self);
end;

function TChartAxisList.GetOwner: TPersistent;
begin
  Result := FChart;
end;

procedure TChartAxisList.InitAndSort(
  AList: TFPList; ACompare: TListSortCompare);
var
  a: TChartAxis;
begin
  AList.Clear;
  for a in Self do
    AList.Add(Pointer(a));
  AList.Sort(ACompare);
end;

function TChartAxisList.Measure(const AExtent: TDoubleRect): TChartAxisMargins;
var
  g: ^TChartAxisGroup;

  procedure UpdateMarginsForMarks(AFirst, ALast: TChartAxisAlignment);
  begin
    Result[AFirst] := Max(Result[AFirst], g^.FFirstMark);
    Result[ALast] := Max(Result[ALast], g^.FLastMark);
  end;

var
  i, j, ai: Integer;
  axis: TChartAxis;
begin
  FillChar(Result, SizeOf(Result), 0);
  ai := 0;
  for i := 0 to High(FGroups) do begin
    g := @FGroups[i];
    g^.FFirstMark := 0;
    g^.FLastMark := 0;
    g^.FSize := 0;
    g^.FTitleSize := 0;
    for j := 0 to g^.FCount - 1 do begin
      axis := TChartAxis(FGroupOrder[ai]);
      try
        axis.Measure(AExtent, g^);
      except
        axis.Visible := false;
        raise;
      end;
      ai += 1;
    end;
    // Axises of the same group should have the same Alignment and ZPosition.
    Result[axis.Alignment] += g^.FSize + g^.FTitleSize;
  end;
  ai := 0;
  for i := 0 to High(FGroups) do begin
    g := @FGroups[i];
    if TChartAxis(FGroupOrder[ai]).IsVertical then
      UpdateMarginsForMarks(calBottom, calTop)
    else
      UpdateMarginsForMarks(calLeft, calRight);
    ai += g^.FCount;
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
  FMin := AMin;
  FMax := AMax;
  FLo := FImageLo + AMarginLo;
  FHi := FImageHi + AMarginHi;
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
end;

initialization
  VIdentityTransform := TChartAxisTransformations.Create(nil);
  SkipObsoleteAxisProperties;

finalization
  FreeAndNil(VIdentityTransform);

end.

