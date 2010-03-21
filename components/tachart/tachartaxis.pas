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
  TAChartUtils, TASources, TATypes;

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

  { TChartAxisTransformation }

  TChartAxisTransformation = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    procedure SetOnChanged(const AValue: TNotifyEvent);
  protected
    procedure Changed;
  public
    function AxisToGraph(AX: Double): Double; virtual;
    function GraphToAxis(AX: Double): Double; virtual;

    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
  end;

  { TChartAxisLogLinearTransformation }

  TChartAxisLogLinearTransformation = class(TChartAxisTransformation)
  private
    FLogarithmic: Boolean;
    FOffset: Double;
    FScale: Double;
    procedure SetLogarithmic(AValue: Boolean);
    procedure SetOffset(AValue: Double);
    procedure SetScale(AValue: Double);
  public
    constructor Create;
  public
    procedure Assign(Source: TPersistent); override;

    function AxisToGraph(AX: Double): Double; override;
    function GraphToAxis(AX: Double): Double; override;
  published
    property Logarithmic: Boolean read FLogarithmic write SetLogarithmic default false;
    property Offset: Double read FOffset write SetOffset;
    property Scale: Double read FScale write SetScale;
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
    FSource: TCustomChartSource;
    function IsFormatStored: Boolean;
    procedure SetSource(const AValue: TCustomChartSource);
  public
    constructor Create(AOwner: TCustomChart);
  published
    property Format stored IsFormatStored;
    property Frame;
    property LabelBrush;
    property LinkPen;
    property Source: TCustomChartSource read FSource write SetSource;
    property Style default smsValue;
  end;

  { TChartAxis }

  TChartAxis = class(TCollectionItem)
  private
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
    FTransformation: TChartAxisLogLinearTransformation;
    FVisible: Boolean;

    procedure SetAlignment(AValue: TChartAxisAlignment);
    procedure SetGrid(AValue: TChartAxisPen);
    procedure SetInverted(AValue: Boolean);
    procedure SetMarks(const AValue: TChartAxisMarks);
    procedure SetOnMarkToText(const AValue: TChartAxisMarkToTextEvent);
    procedure SetTickColor(AValue: TColor);
    procedure SetTickLength(AValue: Integer);
    procedure SetTitle(AValue: TChartAxisTitle);
    procedure SetTransformation(AValue: TChartAxisLogLinearTransformation);
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
    property Transformation: TChartAxisLogLinearTransformation
      read FTransformation write SetTransformation;
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

implementation

uses
  LResources, Math;

const
  FONT_SLOPE_VERTICAL = 45 * 10;

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
  FFrame.Style := psClear;
  FLabelBrush.Style := bsClear;
  FStyle := smsValue;
  FFormat := SERIES_MARK_FORMATS[FStyle];
end;

function TChartAxisMarks.IsFormatStored: Boolean;
begin
  Result := FStyle <> smsValue;
end;

procedure TChartAxisMarks.SetSource(const AValue: TCustomChartSource);
begin
  if FSource = AValue then exit;
  FSource := AValue;
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
    end;
  //inherited Assign(Source);
end;

constructor TChartAxis.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FGrid := TChartAxisPen.Create;
  FGrid.OnChange := @StyleChanged;
  FGrid.Style := psDot;
  FMarks := TChartAxisMarks.Create(ACollection.Owner as TCustomChart);
  FTickColor := clBlack;
  FTickLength := DEF_TICK_LENGTH;
  FTitle := TChartAxisTitle.Create(ACollection.Owner as TCustomChart);
  FTransformation := TChartAxisLogLinearTransformation.Create;
  FTransformation.OnChanged := @StyleChanged;
  FVisible := true;
end;

destructor TChartAxis.Destroy;
begin
  FTransformation.Free;
  FTitle.Free;
  FMarks.Free;
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
      AY += -TickLength - 1 - sz.cy
    else
      AY += TickLength + 1;
    Marks.DrawLabel(ACanvas, Bounds(x - sz.cx div 2, AY, sz.cx, sz.cy), AText);
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
      AX += -TickLength - 1 - sz.cx
    else
      AX += TickLength + 1;
    Marks.DrawLabel(ACanvas, Bounds(AX, y - sz.cy div 2, sz.cx, sz.cy), AText);
  end;

  procedure DoDraw(AMin, AMax: Double);
  var
    i, coord: Integer;
    v: Double;
  begin
    if AMin = AMax then exit;
    coord := SideByAlignment(ARect, Alignment, FSize);
    for i := 0 to High(FMarkValues) do begin
      v := Transformation.AxisToGraph(FMarkValues[i]);
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
  AMin := Transformation.GraphToAxis(AMin);
  AMax := Transformation.GraphToAxis(AMax);
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
    FSize := maxWidth + ACanvas.TextWidth(SOME_DIGIT) + TickLength;
  end;

  procedure CalcHorSize;
  begin
    if AExtent.a.X = AExtent.b.X then exit;
    GetMarkValues(AExtent.a.X, AExtent.b.X);
    FSize := Marks.MeasureLabel(ACanvas, SOME_DIGIT).cy + TickLength;
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

procedure TChartAxis.SetTransformation(
  AValue: TChartAxisLogLinearTransformation);
begin
  if FTransformation = AValue then exit;
  FTransformation.Assign(AValue);
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

{ TChartAxisTransformation }

function TChartAxisTransformation.AxisToGraph(AX: Double): Double;
begin
  Result := AX;
end;

procedure TChartAxisTransformation.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TChartAxisTransformation.GraphToAxis(AX: Double): Double;
begin
  Result := AX;
end;

procedure TChartAxisTransformation.SetOnChanged(const AValue: TNotifyEvent);
begin
  if TMethod(FOnChanged) = TMethod(AValue) then exit;
  FOnChanged := AValue;
end;

{ TChartAxisLogLinearTransformation }

procedure TChartAxisLogLinearTransformation.Assign(Source: TPersistent);
begin
  if Source is TChartAxisLogLinearTransformation then
    with Source as TChartAxisLogLinearTransformation do begin
      Self.Logarithmic := Logarithmic;
      Self.Offset := Offset;
      Self.Scale := Scale;
    end;
end;

function TChartAxisLogLinearTransformation.AxisToGraph(AX: Double): Double;
begin
  if not Logarithmic then
    Result := AX
  else if AX > 0 then
    Result := Ln(AX)
  else
    Result := NegInfinity;
  Result := Result * Scale + Offset;
end;

constructor TChartAxisLogLinearTransformation.Create;
begin
  inherited Create;
  FScale := 1.0;
end;

function TChartAxisLogLinearTransformation.GraphToAxis(AX: Double): Double;
begin
  Result := (AX - Offset) / Scale;
  if Logarithmic then
    Result := Exp(AX);
end;

procedure TChartAxisLogLinearTransformation.SetLogarithmic(AValue: Boolean);
begin
  if FLogarithmic = AValue then exit;
  FLogarithmic := AValue;
  Changed;
end;

procedure TChartAxisLogLinearTransformation.SetOffset(AValue: Double);
begin
  if FOffset = AValue then exit;
  FOffset := AValue;
  Changed;
end;

procedure TChartAxisLogLinearTransformation.SetScale(AValue: Double);
begin
  if FScale = AValue then exit;
  FScale := AValue;
  if FScale = 0 then FScale := 1.0;
  Changed;
end;

procedure SkipObsoleteAxisProperties;
const
  TRANSFORM_NOTE = 'Obsolete, use Transformation instead';
begin
  RegisterPropertyToSkip(TChartAxis, 'Offset', TRANSFORM_NOTE, '');
  RegisterPropertyToSkip(TChartAxis, 'Scale', TRANSFORM_NOTE, '');
end;

initialization
  SkipObsoleteAxisProperties;

end.

