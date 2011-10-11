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
  TAChartUtils, TACustomSource, TATransformations, TATypes;

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

    function IsFormatStored: Boolean;
    procedure SetAtDataOnly(AValue: Boolean);
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
    property Source: TCustomChartSource read FSource write SetSource;
    property Style default smsValue;
  end;


  TChartAxisGroup = record
    FCount: Integer;
    FSize: Integer;
    FTitleSize: Integer;
  end;

  { TChartAxis }

  TChartAxis = class(TCollectionItem)
  private
    FListener: TListener;
    FMarkTexts: TStringDynArray;
    FMarkValues: TDoubleDynArray;

    procedure GetMarkValues(AMin, AMax: Double);
    procedure VisitSource(ASource: TCustomChartSource; var AData);
  private
    FAxisRect: TRect;
    FGroupIndex: Integer;
    FTitleRect: TRect;
  private
    FAlignment: TChartAxisAlignment;
    FGrid: TChartAxisPen;
    FGroup: Integer;
    FInverted: Boolean;
    FMarks: TChartAxisMarks;
    FOnMarkToText: TChartAxisMarkToTextEvent;
    FTickColor: TColor;
    FTickLength: Integer;
    FTitle: TChartAxisTitle;
    FTransformations: TChartAxisTransformations;
    FVisible: Boolean;
    FZPosition: TChartDistance;

    function GetTransform: TChartAxisTransformations;
    procedure SetAlignment(AValue: TChartAxisAlignment);
    procedure SetGrid(AValue: TChartAxisPen);
    procedure SetGroup(AValue: Integer);
    procedure SetInverted(AValue: Boolean);
    procedure SetMarks(const AValue: TChartAxisMarks);
    procedure SetOnMarkToText(const AValue: TChartAxisMarkToTextEvent);
    procedure SetTickColor(AValue: TColor);
    procedure SetTickLength(AValue: Integer);
    procedure SetTitle(AValue: TChartAxisTitle);
    procedure SetTransformations(AValue: TChartAxisTransformations);
    procedure SetVisible(const AValue: Boolean);
    procedure SetZPosition(const AValue: TChartDistance);

    procedure StyleChanged(ASender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(
      ACanvas: TCanvas; const AExtent: TDoubleRect;
      const ATransf: ICoordTransformer; const AZOffset: TPoint);
    procedure DrawTitle(
      ACanvas: TCanvas; const ACenter, AZOffset: TPoint; ASize: Integer);
    function IsVertical: Boolean; inline;
    procedure Measure(
      ACanvas: TCanvas; const AExtent: TDoubleRect; AFirstPass: Boolean;
      var AMeasureData: TChartAxisGroup);
  published
    property Alignment: TChartAxisAlignment
      read FAlignment write SetAlignment default calLeft;
    property Grid: TChartAxisPen read FGrid write SetGrid;
    property Group: Integer read FGroup write SetGroup default 0;
    // Inverts the axis scale from increasing to decreasing.
    property Inverted: boolean read FInverted write SetInverted default false;
    property Marks: TChartAxisMarks read FMarks write SetMarks;
    property TickColor: TColor read FTickColor write SetTickColor default clBlack;
    property TickLength: Integer
      read FTickLength write SetTickLength default DEF_TICK_LENGTH;
    property Title: TChartAxisTitle read FTitle write SetTitle;
    property Transformations: TChartAxisTransformations
      read FTransformations write SetTransformations;
    property Visible: Boolean read FVisible write SetVisible default true;
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
      ACanvas: TCanvas; const AExtent: TDoubleRect;
      const ATransf: ICoordTransformer; ACurrentZ, AMaxZ: Integer;
      var AIndex: Integer);
    function GetAxis(AIndex: Integer): TChartAxis;
    procedure Measure(
      ACanvas: TCanvas; const AExtent: TDoubleRect;
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
  LResources, Math, PropEdits, TADrawUtils, TASources;

type
  TAxisDataExtent = record
    FMin, FMax: Double;
  end;

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

{ TChartAxisTitle }

procedure TChartAxisTitle.Assign(Source: TPersistent);
begin
  if Source is TChartAxisTitle then
    with TChartAxisTitle(Source) do
      FCaption := Caption;
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

function TChartAxisMarks.SourceDef: TCustomChartSource;
begin
  Result := FSource;
  if Result = nil then
    Result := FDefaultSource;
end;

{ TChartAxis }

procedure TChartAxis.Assign(ASource: TPersistent);
begin
  if ASource is TChartAxis then
    with TChartAxis(ASource) do begin
      Self.FGrid.Assign(Grid);
      Self.FGroup := Group;
      Self.FInverted := Inverted;
      Self.FMarks.Assign(Marks);
      Self.FTickColor := TickColor;
      Self.FTickLength := TickLength;
      Self.FTitle.Assign(Title);
      Self.FTransformations := Transformations;
      Self.FZPosition := ZPosition;
      Self.FVisible := Visible;

      Self.FOnMarkToText := OnMarkToText;
    end
  else
    inherited Assign(ASource);
end;

constructor TChartAxis.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FListener := TListener.Create(@FTransformations, @StyleChanged);
  FGrid := TChartAxisPen.Create;
  FGrid.OnChange := @StyleChanged;
  FGrid.Style := psDot;
  FMarks := TChartAxisMarks.Create(ACollection.Owner as TCustomChart);
  FTickColor := clBlack;
  FTickLength := DEF_TICK_LENGTH;
  FTitle := TChartAxisTitle.Create(ACollection.Owner as TCustomChart);
  FVisible := true;
end;

destructor TChartAxis.Destroy;
begin
  FreeAndNil(FTitle);
  FreeAndNil(FMarks);
  FreeAndNil(FListener);
  FreeAndNil(FGrid);
  inherited;
end;

procedure TChartAxis.Draw(
  ACanvas: TCanvas; const AExtent: TDoubleRect;
  const ATransf: ICoordTransformer; const AZOffset: TPoint);

var
  prevLabelPoly: TPointArray = nil;


  procedure LineZ(AP1, AP2: TPoint);
  begin
    ACanvas.Line(AP1 + AZOffset, AP2 + AZOffset);
  end;

  procedure DrawLabelAndTick(
    ALabelCenter: TPoint; const ATickRect: TRect; const AText: String);
  begin
    PrepareSimplePen(ACanvas, TickColor);
    LineZ(ATickRect.TopLeft, ATickRect.BottomRight);
    ALabelCenter += AZOffset;
    Marks.DrawLabel(ACanvas, ALabelCenter, ALabelCenter, AText, prevLabelPoly);
  end;

  procedure DrawXMark(AY: Integer; AMark: Double; const AText: String);
  var
    x, d: Integer;
  begin
    x := ATransf.XGraphToImage(AMark);

    if Grid.Visible then begin
      ACanvas.Pen.Assign(Grid);
      ACanvas.Brush.Style := bsClear;
      LineZ(
        Point(x, ATransf.YGraphToImage(AExtent.a.Y)),
        Point(x, ATransf.YGraphToImage(AExtent.b.Y)));
    end;

    if Marks.Visible then begin
      d := TickLength + Marks.CenterOffset(ACanvas, AText).cy;
      if Alignment = calTop then
        d := -d;
      DrawLabelAndTick(
        Point(x, AY + d), Rect(x, AY - TickLength, x, AY + TickLength), AText);
    end;
  end;

  procedure DrawYMark(AX: Integer; AMark: Double; const AText: String);
  var
    y, d: Integer;
  begin
    y := ATransf.YGraphToImage(AMark);

    if Grid.Visible then begin
      ACanvas.Pen.Assign(Grid);
      ACanvas.Brush.Style := bsClear;
      LineZ(
        Point(ATransf.XGraphToImage(AExtent.a.X), y),
        Point(ATransf.XGraphToImage(AExtent.b.X), y));
    end;

    if Marks.Visible then begin
      d := TickLength + Marks.CenterOffset(ACanvas, AText).cx;
      if Alignment = calLeft then
        d := -d;
      DrawLabelAndTick(
        Point(AX + d, y), Rect(AX - TickLength, y, AX + TickLength, y), AText);
    end;
  end;

var
  i, coord: Integer;
  v: Double;
begin
  if not Visible then exit;
  ACanvas.Font := Marks.LabelFont;
  coord := TChartAxisMargins(FAxisRect)[Alignment];
  for i := 0 to High(FMarkValues) do begin
    v := GetTransform.AxisToGraph(FMarkValues[i]);
    if IsVertical then
      DrawYMark(coord, v, FMarkTexts[i])
    else
      DrawXMark(coord, v, FMarkTexts[i]);
  end;
end;

procedure TChartAxis.DrawTitle(
  ACanvas: TCanvas; const ACenter, AZOffset: TPoint; ASize: Integer);
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
  Title.DrawLabel(ACanvas, p, p, Title.Caption, dummy);
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
  ACanvas: TCanvas; const AExtent: TDoubleRect;
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
      with Marks.MeasureLabel(ACanvas, t) do begin
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
    sz := Title.MeasureLabel(ACanvas, Title.Caption);

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
    sz += TickLength + Marks.Distance;
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

procedure TChartAxis.SetGrid(AValue: TChartAxisPen);
begin
  FGrid.Assign(AValue);
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

procedure TChartAxis.SetMarks(const AValue: TChartAxisMarks);
begin
  if FMarks = AValue then exit;
  FMarks := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetOnMarkToText(const AValue: TChartAxisMarkToTextEvent);
begin
  if TMethod(FOnMarkToText) = TMethod(AValue) then exit;
  FOnMarkToText := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetTickColor(AValue: TColor);
begin
  if FTickColor = AValue then exit;
  FTickColor := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetTickLength(AValue: Integer);
begin
  if FTickLength = AValue then exit;
  FTickLength := AValue;
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
  StyleChanged(AValue);
end;

procedure TChartAxis.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
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
  with Collection.Owner as TCustomChart do begin
    // Transformation change could have invalidated the current extent,
    // so revert to full extent for now.
    if (ASender is TAxisTransform) or (ASender is TChartAxisTransformations) then
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
  ACanvas: TCanvas; const AExtent: TDoubleRect;
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
      Draw(ACanvas, AExtent, ATransf, zoffset);
      DrawTitle(ACanvas, FCenterPoint, zoffset, FGroups[FGroupIndex].FTitleSize);
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
  ACanvas: TCanvas; const AExtent: TDoubleRect;
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
      axis.Measure(ACanvas, AExtent, AFirstPass, g^);
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
  a.FAlignment := AXIS_INDEX[AIndex];
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

