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
  DEF_INTERVALS_COUNT = 5;

type

  { TChartMinorAxis }

  TChartMinorAxis = class(TChartBasicAxis)
  strict private
    FIntervalsCount: Cardinal;
    procedure SetIntervalsCount(AValue: Cardinal);
  protected
    function GetDisplayName: String; override;
  strict protected
    function GetAlignment: TChartAxisAlignment; override;
    procedure SetAlignment(AValue: TChartAxisAlignment); override;
    procedure StyleChanged(ASender: TObject); override;
  public
    constructor Create(ACollection: TCollection); override;
  published
    property IntervalsCount: Cardinal
      read FIntervalsCount write SetIntervalsCount default DEF_INTERVALS_COUNT;
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

  TChartAxisGroup = record
    FCount: Integer;
    FFirstMark: Integer;
    FLastMark: Integer;
    FSize: Integer;
    FTitleSize: Integer;
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
    FHelper: TAxisDrawHelper;
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
    function IsVertical: Boolean; inline;
    procedure Measure(
      const AExtent: TDoubleRect; var AMeasureData: TChartAxisGroup);
    procedure PrepareHelper(
      ADrawer: IChartDrawer; const ATransf: ICoordTransformer;
      AClipRect: PRect; AMaxZPosition: Integer);
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

implementation

uses
  LResources, Math, PropEdits, TAGeometry;

type
  TAxisDataExtent = record
    FMin, FMax: Double;
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

{ TChartAxisEnumerator }

function TChartAxisEnumerator.GetCurrent: TChartAxis;
begin
  Result := TChartAxis(inherited GetCurrent);
end;

{ TChartMinorAxis }

constructor TChartMinorAxis.Create(ACollection: TCollection);
begin
  inherited Create(ACollection, (ACollection as TChartMinorAxisList).GetChart);
  FIntervalsCount := DEF_INTERVALS_COUNT;
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

procedure TChartMinorAxis.SetIntervalsCount(AValue: Cardinal);
begin
  if FIntervalsCount = AValue then exit;
  FIntervalsCount := AValue;
  StyleChanged(Self);
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
  FreeAndNil(FHelper);
  inherited;
end;

procedure TChartAxis.Draw;
var
  i, j, c, ic, fixedCoord: Integer;
  axisTransf: TTransformFunc;
  dhMinor: TAxisDrawHelper;
  pv, v: Double;
begin
  if not Visible then exit;
  if Marks.Visible then
    FHelper.FDrawer.Font := Marks.LabelFont;
  fixedCoord := TChartAxisMargins(FAxisRect)[Alignment];
  v := 0;
  FHelper.BeginDrawing;
  axisTransf := @GetTransform.AxisToGraph;
  for i := 0 to High(FMarkValues) do begin
    pv := v;
    v := axisTransf(FMarkValues[i]);
    FHelper.DrawMark(fixedCoord, v, FMarkTexts[i]);
    if (i = 0) or (v = pv) then continue;
    for j := 0 to Minors.Count - 1 do begin
      ic := Minors[j].IntervalsCount;
      if not Minors[j].Visible or (ic < 2) then continue;
      dhMinor := FHelper.Clone;
      dhMinor.FAxis := Minors[j];
      try
        dhMinor.BeginDrawing;
        for c := 1 to ic - 1 do
          dhMinor.DrawMark(fixedCoord, WeightedAverage(pv, v, c / ic), '');
        dhMinor.EndDrawing;
      finally
        dhMinor.Free;
      end;
    end;
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
  const AExtent: TDoubleRect; var AMeasureData: TChartAxisGroup);

  function MaxMarksSize(AMin, AMax: Double): TPoint;
  var
    t: String;
  begin
    Result := Point(0, 0);
    if AMin = AMax then exit;
    GetMarkValues(AMin, AMax);
    if not Marks.Visible then exit;
    for t in FMarkTexts do
      Result := MaxPoint(Marks.MeasureLabel(FHelper.FDrawer, t), Result);
  end;

  function TitleSize: Integer;
  var
    sz: TSize;
  begin
    if not Title.Visible or (Title.Caption = '') then
      exit(0);
    sz := Title.MeasureLabel(FHelper.FDrawer, Title.Caption);
    Result := IfThen(IsVertical, sz.cx, sz.cy) + Title.Distance;
  end;

  function FirstLastSize(AIndex: Integer): Integer;
  begin
    with Marks.MeasureLabel(FHelper.FDrawer, FMarkTexts[AIndex]) do
      Result := IfThen(IsVertical, cy, cx) div 2;
  end;

var
  sz, rmin, rmax, c, i: Integer;
begin
  if not Visible then exit;
  if IsVertical then
    sz := MaxMarksSize(AExtent.a.Y, AExtent.b.Y).X
  else
    sz := MaxMarksSize(AExtent.a.X, AExtent.b.X).Y;
  if Marks.DistanceToCenter then
    sz := sz div 2;
  if sz > 0 then
    sz += FHelper.FDrawer.Scale(TickLength) +
      FHelper.FDrawer.Scale(Marks.Distance);
  FHelper.GetClipRange(rmin, rmax);
  with AMeasureData do begin
    FSize := Max(sz, FSize);
    FTitleSize := Max(TitleSize, FTitleSize);
    for i := 0 to High(FMarkTexts) do begin
      c := FHelper.GraphToImage(FMarkValues[i]);
      if not InRange(c, rmin, rmax) then continue;
      FFirstMark := Max(FirstLastSize(i) - c + rmin, FFirstMark);
      break;
    end;
    for i := High(FMarkTexts) downto 0 do begin
      c := FHelper.GraphToImage(FMarkValues[i]);
      if not InRange(c, rmin, rmax) then continue;
      FLastMark := Max(FirstLastSize(i) - rmax + c, FLastMark);
      break;
    end;
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
  FHelper.FClipRect := AClipRect;
  FHelper.FDrawer := ADrawer;
  FHelper.FTransf := ATransf;
  FHelper.FZOffset.X := Min(ZPosition, AMaxZPosition);
  FHelper.FZOffset.Y := -FHelper.FZOffset.X;
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

procedure TChartAxisList.Draw(ACurrentZ: Integer; var AIndex: Integer);
begin
  while AIndex < FZOrder.Count do
    with TChartAxis(FZOrder[AIndex]) do begin
      if ACurrentZ < ZPosition then break;
      Draw;
      DrawTitle(FCenterPoint, FGroups[FGroupIndex].FTitleSize);
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
      axis.Measure(AExtent, g^);
      ai += 1;
    end;
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

