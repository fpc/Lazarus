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
  TATypes, TAChartUtils;

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

  TChartAxisTransformation = class (TPersistent)
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

  TChartAxisLogLinearTransformation = class (TChartAxisTransformation)
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

  { TChartAxis }

  TChartAxis = class(TCollectionItem)
  private
    FSize: Integer;
    FTitleSize: Integer;
    function GetMarks(AMin, AMax: Double): TDoubleDynArray;
  private
    FAlignment: TChartAxisAlignment;
    FGrid: TChartAxisPen;
    FInverted: Boolean;
    FOnMarkToText: TChartAxisMarkToTextEvent;
    FTickColor: TColor;
    FTickLength: Integer;
    FTitle: TChartAxisTitle;
    FTransformation: TChartAxisLogLinearTransformation;
    FVisible: Boolean;

    procedure SetAlignment(AValue: TChartAxisAlignment);
    procedure SetGrid(AValue: TChartAxisPen);
    procedure SetInverted(AValue: Boolean);
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
    function MarkToText(AMark: Double): String;
    function MarkToTextDefault(AMark: Double): String;
    procedure Measure(
      ACanvas: TCanvas; const AExtent: TDoubleRect;
      var AMargins: TChartAxisMargins);

  published
    property Alignment: TChartAxisAlignment read FAlignment write SetAlignment;
    property Grid: TChartAxisPen read FGrid write SetGrid;
    // Inverts the axis scale from increasing to decreasing.
    property Inverted: boolean read FInverted write SetInverted default false;
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
  FGrid.Free;
  inherited;
end;

procedure TChartAxis.Draw(
  ACanvas: TCanvas; const AExtent: TDoubleRect;
  const ATransf: ICoordTransformer; var ARect: TRect);

  procedure DrawXMark(AY: Integer; AMark: Double);
  var
    x, dy: Integer;
    sz: TSize;
    markText: String;
  begin
    x := ATransf.XGraphToImage(Transformation.AxisToGraph(AMark));

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

    //ACanvas.Brush.Assign(FGraphBrush);
    //ACanvas.Brush.Color := Color;
    markText := MarkToText(AMark);
    sz := ACanvas.TextExtent(markText);
    if Alignment = calTop then
      dy := -TickLength - 1 - sz.cy
    else
      dy := TickLength + 1;
    ACanvas.TextOut(x - sz.cx div 2, AY + dy, markText);
  end;

  procedure DrawYMark(AX: Integer; AMark: Double);
  var
    dx, y: Integer;
    markText: String;
    sz: TSize;
  begin
    y := ATransf.YGraphToImage(Transformation.AxisToGraph(AMark));

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

    //ACanvas.Brush.Assign(FGraphBrush);
    //ACanvas.Brush.Color := Color;
    markText := MarkToText(AMark);
    sz := ACanvas.TextExtent(markText);
    if Alignment = calLeft then
      dx := -TickLength - 1 - sz.cx
    else
      dx := TickLength + 1;
    ACanvas.TextOut(AX + dx, y - sz.cy div 2, markText)
  end;

  procedure DoDraw(AMin, AMax: Double);
  var
    i, coord: Integer;
    marks: TDoubleDynArray;
  begin
    if AMin = AMax then exit;
    marks := GetMarks(AMin, AMax);
    coord := SideByAlignment(ARect, Alignment, FSize);
    for i := 0 to High(marks) do
      if IsVertical then
        DrawYMark(coord, marks[i])
      else
        DrawXMark(coord, marks[i]);
  end;

begin
  if not Visible then exit;
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

function TChartAxis.GetMarks(AMin, AMax: Double): TDoubleDynArray;
begin
  AMin := Transformation.GraphToAxis(AMin);
  AMax := Transformation.GraphToAxis(AMax);
  if AMin > AMax then
    Exchange(AMin, AMax);
  Result := GetIntervals(AMin, AMax, Inverted);
end;

function TChartAxis.IsVertical: Boolean; inline;
begin
  Result := Alignment in [calLeft, calRight];
end;

function TChartAxis.MarkToText(AMark: Double): String;
begin
  Result := MarkToTextDefault(AMark);
  if Assigned(FOnMarkToText) then
    FOnMarkToText(Result, AMark);
end;

function TChartAxis.MarkToTextDefault(AMark: Double): String;
const
  EPSILON = 1e-16;
begin
  if Abs(AMark) <= EPSILON then AMark := 0;
  Result := Trim(FloatToStr(AMark));
end;

procedure TChartAxis.Measure(
  ACanvas: TCanvas; const AExtent: TDoubleRect;
  var AMargins: TChartAxisMargins);

var
  digitSize: TSize;

  procedure CalcVertSize;
  var
    i, maxWidth: Integer;
    marks: TDoubleDynArray;
  begin
    if AExtent.a.Y = AExtent.b.Y then exit;
    maxWidth := 0;
    marks := GetMarks(AExtent.a.Y, AExtent.b.Y);
    for i := 0 to High(marks) do
      maxWidth := Max(ACanvas.TextWidth(MarkToText(marks[i])), maxWidth);
    // CalculateTransformationCoeffs changes axis interval, so it is possibile
    // that a new mark longer then existing ones is introduced.
    // That will change marks width and reduce view area,
    // requiring another call to CalculateTransformationCoeffs...
    // So punt for now and just reserve space for extra digit unconditionally.
    FSize := maxWidth + digitSize.cx + TickLength;
  end;

  procedure CalcHorSize;
  begin
    if AExtent.a.X = AExtent.b.X then exit;
    FSize := digitSize.cy + TickLength;
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

const
  SOME_DIGIT = '0';
begin
  FSize := 0;
  FTitleSize := 0;
  if not Visible then exit;
  digitSize := ACanvas.TextExtent(SOME_DIGIT);
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

