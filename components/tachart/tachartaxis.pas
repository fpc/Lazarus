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

type

  { TChartAxisTitle }

  TChartAxisTitle = class(TChartElement)
  private
    FCaption: String;
    FDistance: TChartDistance;
    FFont: TFont;

    procedure SetCaption(AValue: String);
    procedure SetDistance(AValue: TChartDistance);
    procedure SetFont(AValue: TFont);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: String read FCaption write SetCaption;
    property Distance: TChartDistance
      read FDistance write SetDistance default DEF_TITLE_DISTANCE;
    property Font: TFont read FFont write SetFont;
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

  TChartAxisBrush = class(TBrush)
  published
    property Style default bsClear;
  end;

  TChartAxisFramePen = class(TChartPen)
  published
    property Style default psClear;
  end;

  { TChartAxisMarks }

  TChartAxisMarks = class(
    specialize TGenericChartMarks<TChartAxisBrush, TChartPen, TChartAxisFramePen>)
  private
    FListener: TListener;
    FSource: TCustomChartSource;
    function IsFormatStored: Boolean;
    procedure SetSource(AValue: TCustomChartSource);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
  published
    property Distance default 1;
    property Format stored IsFormatStored;
    property Frame;
    property LabelBrush;
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
      ACanvas: TCanvas; const AExtent: TDoubleRect;
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

  { TChartAxisList }

  TChartAxisList = class(TCollection)
  private
    FChart: TCustomChart;
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
  end;

  function SideByAlignment(
    var ARect: TRect; AAlignment: TChartAxisAlignment; ADelta: Integer): Integer;
  function TransformByAxis(
    AAxisList: TChartAxisList; AIndex: Integer): TChartAxisTransformations;

implementation

uses
  LResources, Math;

const
  FONT_SLOPE_VERTICAL = 45 * 10;

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
    with TChartAxisTitle(Source) do begin
      FCaption := Caption;
      FFont.Assign(Font);
    end;
  inherited Assign(Source);
end;

constructor TChartAxisTitle.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := DEF_TITLE_DISTANCE;
  InitHelper(FFont, TFont);
end;

destructor TChartAxisTitle.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TChartAxisTitle.SetCaption(AValue: String);
begin
  FCaption := AValue;
  StyleChanged(Self);
end;

procedure TChartAxisTitle.SetDistance(AValue: TChartDistance);
begin
  if FDistance = AValue then exit;
  FDistance := AValue;
  StyleChanged(Self);
end;

procedure TChartAxisTitle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

{ TChartAxisMarks }

constructor TChartAxisMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := 1;
  FFrame.Style := psClear;
  FLabelBrush.Style := bsClear;
  FListener := TListener.Create(@FSource, @StyleChanged);
  FStyle := smsValue;
  FFormat := SERIES_MARK_FORMATS[FStyle];
end;

destructor TChartAxisMarks.Destroy;
begin
  FListener.Free;
  inherited;
end;

function TChartAxisMarks.IsFormatStored: Boolean;
begin
  Result := FStyle <> smsValue;
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
  FTitle.Free;
  FMarks.Free;
  FListener.Free;
  FGrid.Free;
  inherited;
end;

procedure TChartAxis.Draw(
  ACanvas: TCanvas; const AExtent: TDoubleRect;
  const ATransf: ICoordTransformer; var ARect: TRect);

  procedure DrawXMark(AY: Integer; AMark: Double; const AText: String);
  var
    x: Integer;
    sz: TSize;
  begin
    x := ATransf.XGraphToImage(AMark);

    if Grid.Visible then begin
      ACanvas.Pen.Assign(Grid);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Line(
        x, ATransf.YGraphToImage(AExtent.a.Y),
        x, ATransf.YGraphToImage(AExtent.b.Y));
    end;

    ACanvas.Pen.Color := TickColor;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Mode := pmCopy;
    ACanvas.Line(x, AY - TickLength, x, AY + TickLength);

    sz := Marks.MeasureLabel(ACanvas, AText);
    if Alignment = calTop then
      AY += -TickLength - Marks.Distance - sz.cy
    else
      AY += TickLength + Marks.Distance;
    Marks.DrawLabel(ACanvas, BoundsSize(x - sz.cx div 2, AY, sz), AText);
  end;

  procedure DrawYMark(AX: Integer; AMark: Double; const AText: String);
  var
    y: Integer;
    sz: TSize;
  begin
    y := ATransf.YGraphToImage(AMark);

    if Grid.Visible then begin
      ACanvas.Pen.Assign(Grid);
      ACanvas.Brush.Style := bsClear;
      ACanvas.Line(
        ATransf.XGraphToImage(AExtent.a.X), y,
        ATransf.XGraphToImage(AExtent.b.X), y);
    end;

    ACanvas.Pen.Color := TickColor;
    ACanvas.Pen.Style := psSolid;
    ACanvas.Pen.Mode := pmCopy;
    ACanvas.Line(AX - TickLength, y, AX + TickLength, y);

    sz := Marks.MeasureLabel(ACanvas, AText);
    if Alignment = calLeft then
      AX += -TickLength - Marks.Distance - sz.cx
    else
      AX += TickLength + Marks.Distance;
    Marks.DrawLabel(ACanvas, BoundsSize(AX, y - sz.cy div 2, sz), AText);
  end;

  procedure DoDraw(AMin, AMax: Double);
  var
    i, coord: Integer;
    v: Double;
  begin
    if AMin = AMax then exit;
    coord := SideByAlignment(ARect, Alignment, FSize);
    for i := 0 to High(FMarkValues) do begin
      v := GetTransform.AxisToGraph(FMarkValues[i]);
      if IsVertical then
        DrawYMark(coord, v, FMarkTexts[i])
      else
        DrawXMark(coord, v, FMarkTexts[i]);

    end;
  end;

begin
  if not Visible then exit;
  ACanvas.Font := Marks.LabelFont;
  if IsVertical then
    DoDraw(AExtent.a.Y, AExtent.b.Y)
  else
    DoDraw(AExtent.a.X, AExtent.b.X);
end;

procedure TChartAxis.DrawTitle(
  ACanvas: TCanvas; const ACenter: TPoint; var ARect: TRect);
var
  p: TPoint;
  sz: TSize;
  pbf: TPenBrushFontRecall;
begin
  if not Visible or (FTitleSize = 0) then exit;
  // FIXME: Angle assumed to be either ~0 or ~90 degrees
  pbf := TPenBrushFontRecall.Create(ACanvas, [pbfFont]);
  try
    ACanvas.Font := Title.Font;
    sz := ACanvas.TextExtent(Title.Caption);
    if Title.Font.Orientation >= FONT_SLOPE_VERTICAL then begin
      Exchange(sz.cx, sz.cy);
      sz.cy := -sz.cy;
    end;
    p.X := ACenter.X - sz.cx div 2;
    p.Y := ACenter.Y - sz.cy div 2;
    case Alignment of
      calLeft: p.X := ARect.Left - FTitleSize;
      calTop: p.Y := ARect.Top - FTitleSize;
      calRight: p.X := ARect.Right + Title.Distance;
      calBottom: p.Y := ARect.Bottom + Title.Distance;
    end;
    ACanvas.TextOut(p.X, p.Y, Title.Caption);
  finally
    pbf.Free;
  end;
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
  i, count: Integer;
  v: Double;
begin
  AMin := GetTransform.GraphToAxis(AMin);
  AMax := GetTransform.GraphToAxis(AMax);
  if AMin > AMax then
    Exchange(AMin, AMax);
  if Marks.Source = nil then begin
    FMarkValues := GetIntervals(AMin, AMax, Inverted);
    SetLength(FMarkTexts, Length(FMarkValues));
    for i := 0 to High(FMarkValues) do
      FMarkTexts[i] := Format(Marks.Format, [FMarkValues[i]]);
  end
  else begin
    count := 0;
    SetLength(FMarkValues, Marks.Source.Count);
    SetLength(FMarkTexts, Marks.Source.Count);
    for i := 0 to Marks.Source.Count - 1 do begin
      with Marks.Source[i]^ do
        v := IfThen(IsVertical, Y, X);
      if not InRange(v, AMin, AMax) then continue;
      FMarkValues[count] := v;
      FMarkTexts[count] := Marks.Source.FormatItem(Marks.Format, i);
      count += 1;
    end;
    SetLength(FMarkValues, count);
    SetLength(FMarkTexts, count);
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
  ACanvas: TCanvas; const AExtent: TDoubleRect; var AMargins: TChartAxisMargins);

const
  SOME_DIGIT = '0';

  procedure CalcVertSize;
  var
    i, maxWidth: Integer;
  begin
    if AExtent.a.Y = AExtent.b.Y then exit;
    GetMarkValues(AExtent.a.Y, AExtent.b.Y);
    maxWidth := 0;
    for i := 0 to High(FMarkTexts) do
      with Marks.MeasureLabel(ACanvas, FMarkTexts[i]) do
        maxWidth := Max(cx, maxWidth);
    // CalculateTransformationCoeffs changes axis interval, so it is possibile
    // that a new mark longer then existing ones is introduced.
    // That will change marks width and reduce view area,
    // requiring another call to CalculateTransformationCoeffs...
    // So punt for now and just reserve space for extra digit unconditionally.
    FSize := maxWidth + ACanvas.TextWidth(SOME_DIGIT);
  end;

  procedure CalcHorSize;
  begin
    if AExtent.a.X = AExtent.b.X then exit;
    GetMarkValues(AExtent.a.X, AExtent.b.X);
    FSize := Marks.MeasureLabel(ACanvas, SOME_DIGIT).cy;
  end;

  procedure CalcTitleSize;
  var
    d: Integer;
    sz: TSize;
    pbf: TPenBrushFontRecall;
  begin
    if not Title.Visible or (Title.Caption = '') then exit;
    pbf := TPenBrushFontRecall.Create(ACanvas, [pbfFont]);
    try
      ACanvas.Font := Title.Font;
      sz := ACanvas.TextExtent(Title.Caption);
    finally
      pbf.Free;
    end;
    if (Title.Font.Orientation < FONT_SLOPE_VERTICAL) = IsVertical then
      d := sz.cx
    else
      d := sz.cy;
    FTitleSize := d + Title.Distance;
  end;

begin
  FSize := 0;
  FTitleSize := 0;
  if not Visible then exit;
  if IsVertical then
    CalcVertSize
  else
    CalcHorSize;
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
  Unused(ASender);
  (Collection.Owner as TCustomChart).Invalidate;
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
end;

initialization
  VIdentityTransform := TChartAxisTransformations.Create(nil);
  SkipObsoleteAxisProperties;

finalization
  VIdentityTransform.Free;

end.

