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
  TAChartUtils, TASources, TATransformations, TATypes;

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

  { TChartAxisTitle }

  TChartAxisTitle = class(
    specialize TGenericChartMarks<TChartAxisBrush, TChartPen, TChartAxisFramePen>)
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
    property LabelBrush;
     // Use LabelFont instead.
    property Font: TFont read GetFont write SetFont; deprecated;
    property Frame;
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

  { TChartAxisMarks }

  TChartAxisMarks = class(
    specialize TGenericChartMarks<TChartAxisBrush, TChartPen, TChartAxisFramePen>)
  private
    FAtDataOnly: Boolean;
    FDefaultSource: TIntervalChartSource;
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

  { TChartAxis }

  TChartAxis = class(TCollectionItem)
  private
    FListener: TListener;
    FMarkTexts: TStringDynArray;
    FMarkValues: TDoubleDynArray;
    FSize: Integer;
    FTitleSize: Integer;

    procedure VisitSource(ASource: TCustomChartSource; var AData);
    procedure GetMarkValues(AMin, AMax: Double);
  private
    FAlignment: TChartAxisAlignment;
    FGrid: TChartAxisPen;
    FInverted: Boolean;
    FMarks: TChartAxisMarks;
    FOnMarkToText: TChartAxisMarkToTextEvent;
    FTickColor: TColor;
    FTickLength: Integer;
    FTitle: TChartAxisTitle;
    FTransformations: TChartAxisTransformations;
    FVisible: Boolean;

    function GetTransform: TChartAxisTransformations;
    procedure SetAlignment(AValue: TChartAxisAlignment);
    procedure SetGrid(AValue: TChartAxisPen);
    procedure SetInverted(AValue: Boolean);
    procedure SetMarks(const AValue: TChartAxisMarks);
    procedure SetOnMarkToText(const AValue: TChartAxisMarkToTextEvent);
    procedure SetTickColor(AValue: TColor);
    procedure SetTickLength(AValue: Integer);
    procedure SetTitle(AValue: TChartAxisTitle);
    procedure SetTransformations(AValue: TChartAxisTransformations);
    procedure SetVisible(const AValue: Boolean);

    procedure StyleChanged(ASender: TObject);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Draw(
      ACanvas: TCanvas; const AExtent: TDoubleRect;
      const ATransf: ICoordTransformer; var ARect: TRect);
    procedure DrawTitle(
      ACanvas: TCanvas; const ACenter: TPoint; var ARect: TRect);
    function IsVertical: Boolean; inline;
    procedure Measure(
      ACanvas: TCanvas; const AExtent: TDoubleRect; AFirstPass: Boolean;
      var AMargins: TChartAxisMargins);

  published
    property Alignment: TChartAxisAlignment read FAlignment write SetAlignment;
    property Grid: TChartAxisPen read FGrid write SetGrid;
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
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TCustomChart);
  public
    function Add: TChartAxis; inline;
    function GetAxis(AIndex: Integer): TChartAxis;
    procedure SetAxis(AIndex: Integer; AValue: TChartAxis);

    property Axes[AIndex: Integer]: TChartAxis read GetAxes; default;
    property BottomAxis: TChartAxis index 1 read GetAxis write SetAxis;
    property LeftAxis: TChartAxis index 2 read GetAxis write SetAxis;
    property OnVisitSources: TChartOnVisitSources
      read FOnVisitSources write FOnVisitSources;
  end;

  function SideByAlignment(
    var ARect: TRect; AAlignment: TChartAxisAlignment; ADelta: Integer): Integer;
  function TransformByAxis(
    AAxisList: TChartAxisList; AIndex: Integer): TChartAxisTransformations;

implementation

uses
  LResources, Math, PropEdits, TADrawUtils;

type
  TAxisDataExtent = record
    FMin, FMax: Double;
  end;

var
  VIdentityTransform: TChartAxisTransformations;

function SideByAlignment(
  var ARect: TRect; AAlignment: TChartAxisAlignment; ADelta: Integer): Integer;
var
  a: TChartAxisMargins absolute ARect;
begin
  Result := a[AAlignment];
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

procedure TChartAxis.Assign(Source: TPersistent);
begin
  if Source is TChartAxis then
    with TChartAxis(Source) do begin
      FGrid.Assign(Grid);
      FInverted := Inverted;
      FTitle.Assign(Title);
    end
  else
    inherited Assign(Source);
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
  const ATransf: ICoordTransformer; var ARect: TRect);

var
  prevLabelPoly: TPointArray = nil;

  procedure DrawLabelAndTick(
    const ALabelCenter: TPoint; const ATickRect: TRect; const AText: String);
  begin
    PrepareSimplePen(ACanvas, TickColor);
    ACanvas.Line(ATickRect);
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
      ACanvas.Line(
        x, ATransf.YGraphToImage(AExtent.a.Y),
        x, ATransf.YGraphToImage(AExtent.b.Y));
    end;

    d :=
      TickLength + Marks.Distance + Marks.MeasureLabel(ACanvas, AText).cy div 2;
    if Alignment = calTop then
      d := -d;
    DrawLabelAndTick(
      Point(x, AY + d), Rect(x, AY - TickLength, x, AY + TickLength), AText);
  end;

  procedure DrawYMark(AX: Integer; AMark: Double; const AText: String);
  var
    y, d: Integer;
  begin
    y := ATransf.YGraphToImage(AMark);

    if Grid.Visible then begin
      ACanvas.Pen.Assign(Grid);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Line(
        ATransf.XGraphToImage(AExtent.a.X), y,
        ATransf.XGraphToImage(AExtent.b.X), y);
    end;

    d :=
      TickLength + Marks.Distance + Marks.MeasureLabel(ACanvas, AText).cx div 2;
    if Alignment = calLeft then
      d := -d;
    DrawLabelAndTick(
      Point(AX + d, y), Rect(AX - TickLength, y, AX + TickLength, y), AText);
  end;

var
  i, coord: Integer;
  v: Double;
begin
  if not Visible then exit;
  ACanvas.Font := Marks.LabelFont;
  coord := SideByAlignment(ARect, Alignment, FSize);
  for i := 0 to High(FMarkValues) do begin
    v := GetTransform.AxisToGraph(FMarkValues[i]);
    if IsVertical then
      DrawYMark(coord, v, FMarkTexts[i])
    else
      DrawXMark(coord, v, FMarkTexts[i]);
  end;
end;

procedure TChartAxis.DrawTitle(
  ACanvas: TCanvas; const ACenter: TPoint; var ARect: TRect);
var
  p: TPoint;
  dummy: TPointArray = nil;
  d: Integer;
begin
  if not Visible or (FTitleSize = 0) then exit;
  p := ACenter;
  d := (FTitleSize + Title.Distance) div 2;
  case Alignment of
    calLeft: p.X := ARect.Left - d;
    calTop: p.Y := ARect.Top - d;
    calRight: p.X := ARect.Right + d;
    calBottom: p.Y := ARect.Bottom + d;
  end;
  Title.DrawLabel(ACanvas, p, p, Title.Caption, dummy);
  SideByAlignment(ARect, Alignment, FTitleSize);
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
  ACanvas: TCanvas; const AExtent: TDoubleRect; AFirstPass: Boolean;
  var AMargins: TChartAxisMargins);

  function CalcMarksSize(AMin, AMax: Double): TSize;
  const
    SOME_DIGIT = '0';
  var
    i: Integer;
    t: String;
  begin
    Result := Size(0, 0);
    if AMin = AMax then exit;
    GetMarkValues(AMin, AMax);
    for i := 0 to High(FMarkTexts) do begin
      // CalculateTransformationCoeffs changes axis interval, so it is possibile
      // that a new mark longer then existing ones is introduced.
      // That will change marks width and reduce view area,
      // requiring another call to CalculateTransformationCoeffs...
      // So punt for now and just reserve space for extra digit unconditionally.
      t := FMarkTexts[i];
      if AFirstPass then
        t += SOME_DIGIT;
      with Marks.MeasureLabel(ACanvas, t) do begin
        Result.cx := Max(cx, Result.cx);
        Result.cy := Max(cy, Result.cy);
      end;
    end;
  end;

  procedure CalcTitleSize;
  var
    sz: TSize;
  begin
    if not Title.Visible or (Title.Caption = '') then exit;
    sz := Title.MeasureLabel(ACanvas, Title.Caption);
    FTitleSize := IfThen(IsVertical, sz.cx, sz.cy) + Title.Distance;
  end;

begin
  FSize := 0;
  FTitleSize := 0;
  if not Visible then exit;
  if IsVertical then
    FSize := CalcMarksSize(AExtent.a.Y, AExtent.b.Y).cx
  else
    FSize := CalcMarksSize(AExtent.a.X, AExtent.b.X).cy;
  if FSize > 0 then
    FSize += TickLength + Marks.Distance;
  CalcTitleSize;
  AMargins[Alignment] += FSize + FTitleSize;
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

procedure TChartAxis.SetInverted(AValue: Boolean);
begin
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
  if FOnMarkToText = AValue then exit;
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
  StyleChanged(Self);
end;

procedure TChartAxis.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.StyleChanged(ASender: TObject);
begin
  with Collection.Owner as TCustomChart do begin
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

