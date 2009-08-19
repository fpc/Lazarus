{
 /***************************************************************************
                               TATypes.pas
                               -----------
              Component Library Standard Graph Element Types


 ***************************************************************************/

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

Authors: Luнs Rodrigues, Philippe Martinole, Alexander Klenin

}
unit TATypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, FPCanvas, Types,
  TAChartUtils;

const
  MARKS_MARGIN_X = 4;
  MARKS_MARGIN_Y = 2;
  DEF_MARGIN = 4;
  DEF_MARKS_DISTANCE = 20;
  DEF_POINTER_SIZE = 4;
  DEF_TICK_LENGTH = 4;
  DEF_TITLE_DISTANCE = 4;

type
  TCustomChart = class(TCustomControl);

  { TChartPen }

  TChartPen = class(TPen)
  private
    FVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TLegendAlignment = (laLeft, laRight, laTop, laBottom);

  TFPCanvasHelperClass = class of TFPCanvasHelper;

  { TChartElement }

  TChartElement = class(TPersistent)
  private
    FVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    FOwner: TCustomChart;
    procedure InitHelper(
      var AResult: TFPCanvasHelper; AClass: TFPCanvasHelperClass);
    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
    procedure Assign(Source: TPersistent); override;

    procedure SetOwner(AOwner: TCustomChart);

    property Visible: Boolean read FVisible write SetVisible;
  end;

  TChartLegend = class(TChartElement)
  private
    FAlignment: TLegendAlignment;
    FFont: TFont;
    FFrame: TChartPen;

    procedure SetAlignment(AValue: TLegendAlignment);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TLegendAlignment
      read FAlignment write SetAlignment default laRight;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Visible default false;
  end;

  TChartTitle = class(TChartElement)
  private
    FAlignment: TAlignment;
    FBrush: TBrush;
    FFont: TFont;
    FFrame: TChartPen;
    FText: TStrings;

    procedure SetAlignment(AValue: TAlignment);
    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetText(AValue: TStrings);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment
      read FAlignment write SetAlignment default taCenter;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Text: TStrings read FText write SetText;
    property Visible default false;
  end;

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

  TChartAxisPen = class(TChartPen)
  published
    property Style default psDot;
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
    FOffset: Double;
    FScale: Double;
    FTickColor: TColor;
    FTickLength: Integer;
    FTitle: TChartAxisTitle;
    FVisible: Boolean;

    procedure SetAlignment(AValue: TChartAxisAlignment);
    procedure SetGrid(AValue: TChartAxisPen);
    procedure SetInverted(AValue: Boolean);
    procedure SetOffset(AValue: Double);
    procedure SetScale(AValue: Double);
    procedure SetTickColor(AValue: TColor);
    procedure SetTickLength(AValue: Integer);
    procedure SetTitle(AValue: TChartAxisTitle);
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
    property Offset: Double read FOffset write SetOffset;
    property Scale: Double read FScale write SetScale;
    property TickColor: TColor read FTickColor write SetTickColor default clBlack;
    property TickLength: Integer
      read FTickLength write SetTickLength default DEF_TICK_LENGTH;
    property Title: TChartAxisTitle read FTitle write SetTitle;
    property Visible: Boolean read FVisible write SetVisible default true;
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

  TChartLinkPen = class(TChartPen)
  published
    property Color default clWhite;
  end;

  TChartLabelBrush = class(TBrush)
  published
    property Color default clYellow;
  end;

  { TChartMarks }

  TChartMarks = class(TChartElement)
  private
    FDistance: Integer;
    FFormat: String;
    FFrame: TChartPen;
    FLabelBrush: TChartLabelBrush;
    FLabelFont: TFont;
    FLinkPen: TChartLinkPen;
    FStyle: TSeriesMarksStyle;

    procedure SetDistance(const AValue: Integer);
    procedure SetFormat(const AValue: String);
    procedure SetFrame(const AValue: TChartPen);
    procedure SetLabelBrush(const AValue: TChartLabelBrush);
    procedure SetLabelFont(const AValue: TFont);
    procedure SetLinkPen(const AValue: TChartLinkPen);
    procedure SetStyle(const AValue: TSeriesMarksStyle);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure DrawLabel(
      ACanvas: TCanvas; const ALabelRect: TRect; const AText: String);
    function IsMarkLabelsVisible: Boolean;
  published
    // Distance between series point and label.
    property Distance: Integer
      read FDistance write SetDistance default DEF_MARKS_DISTANCE;
    property Format: String read FFormat write SetFormat;
    property Frame: TChartPen read FFrame write SetFrame;
    property LabelBrush: TChartLabelBrush read FLabelBrush write SetLabelBrush;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property LinkPen: TChartLinkPen read FLinkPen write SetLinkPen;
    property Style: TSeriesMarksStyle
      read FStyle write SetStyle default smsNone;
    property Visible default true;
  end;

  { TSeriesPointer }

  TSeriesPointer = class(TChartElement)
  private
    FBrush: TBrush;
    FHorizSize: Integer;
    FPen: TChartPen;
    FStyle: TSeriesPointerStyle;
    FVertSize: Integer;

    procedure SetBrush(AValue: TBrush);
    procedure SetHorizSize(AValue: Integer);
    procedure SetPen(AValue: TChartPen);
    procedure SetStyle(AValue: TSeriesPointerStyle);
    procedure SetVertSize(AValue: Integer);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Draw(ACanvas: TCanvas; ACenter: TPoint; AColor: TColor);
  published
    property Brush: TBrush read FBrush write SetBrush;
    property HorizSize: Integer read FHorizSize write SetHorizSize default DEF_POINTER_SIZE;
    property Pen: TChartPen read FPen write SetPen;
    property Style: TSeriesPointerStyle read FStyle write SetStyle default psRectangle;
    property VertSize: Integer read FVertSize write SetVertSize default DEF_POINTER_SIZE;
    property Visible default true;
  end;

  EExtentError = class(EChartError);

  { TChartExtent }

  TChartExtent = class (TChartElement)
  private
    FExtent: TDoubleRect;
    FUseBounds: array [1..4] of Boolean;

    function GetBounds(AIndex: Integer): Double;
    function GetUseBounds(AIndex: integer): Boolean;
    function IsBoundsStored(AIndex: Integer): boolean;
    procedure SetBounds(AIndex: Integer; const AValue: Double);
    procedure SetUseBounds(AIndex: Integer; AValue: Boolean);
  public
    procedure CheckBoundsOrder;
    property Extent: TDoubleRect read FExtent;
  published
    property XMin: Double index 1 read GetBounds write SetBounds stored IsBoundsStored;
    property YMin: Double index 2 read GetBounds write SetBounds stored IsBoundsStored;
    property XMax: Double index 3 read GetBounds write SetBounds stored IsBoundsStored;
    property YMax: Double index 4 read GetBounds write SetBounds stored IsBoundsStored;
    property UseXMin: Boolean index 1 read GetUseBounds write SetUseBounds default false;
    property UseYMin: Boolean index 2 read GetUseBounds write SetUseBounds default false;
    property UseXMax: Boolean index 3 read GetUseBounds write SetUseBounds default false;
    property UseYMax: Boolean index 4 read GetUseBounds write SetUseBounds default false;
  end;

  { TChartMargins }

  TChartMargins = class (TChartElement)
  private
    FData: record
      case Integer of
        0: (FRect: TRect;);
        1: (FCoords: array [1..4] of Integer;);
      end;
    function GetValue(AIndex: Integer): integer;
    procedure SetValue(AIndex: integer; AValue: TChartDistance);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(Source: TPersistent); override;
    property Data: TRect read FData.FRect;
  published
    property Left: TChartDistance index 1 read GetValue write SetValue default DEF_MARGIN;
    property Top: TChartDistance index 2 read GetValue write SetValue default DEF_MARGIN;
    property Right: TChartDistance index 3 read GetValue write SetValue default DEF_MARGIN;
    property Bottom: TChartDistance index 4 read GetValue write SetValue default DEF_MARGIN;
  end;

function SideByAlignment(
  var ARect: TRect; AAlignment: TChartAxisAlignment; ADelta: Integer): Integer;

implementation

uses
  Math;

const
  FONT_SLOPE_VERTICAL = 45 * 10;

function MarkToText(AMark: Double): String;
begin
  if Abs(AMark) <= 1e-16 then AMark := 0;
  Result := Trim(FloatToStr(AMark));
end;

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

{ TChartPen }

procedure TChartPen.Assign(Source: TPersistent);
begin
  if Source is TChartPen then
    with TChartPen(Source) do
      FVisible := Visible;
  inherited Assign( Source );
end;

constructor TChartPen.Create;
begin
  inherited Create;
  FVisible := true;
end;

procedure TChartPen.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  if Assigned(OnChange) then OnChange(Self);
end;

{ TChartElement }

procedure TChartElement.Assign(Source: TPersistent);
begin
  //inherited Assign(Source);
  if Source is TChartElement then
    Self.FVisible := TChartElement(Source).FVisible;
end;

constructor TChartElement.Create(AOwner: TCustomChart);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TChartElement.InitHelper(
  var AResult: TFPCanvasHelper; AClass: TFPCanvasHelperClass);
begin
  AResult := AClass.Create;
  AResult.OnChange := @StyleChanged;
end;

procedure TChartElement.SetOwner(AOwner: TCustomChart);
begin
  FOwner := AOwner;
end;

procedure TChartElement.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then exit;
  FVisible := AValue;
  StyleChanged(Self);
end;

procedure TChartElement.StyleChanged(Sender: TObject);
begin
  if FOwner <> nil then
    FOwner.Invalidate;
end;

{ TChartLegend }

procedure TChartLegend.Assign(Source: TPersistent);
begin
  if Source is TChartLegend then
    with TChartLegend(Source) do begin
      Self.FAlignment := FAlignment;
      Self.FVisible := FVisible;
    end;

  inherited Assign(Source);
end;

constructor TChartLegend.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FAlignment := laRight;
  FVisible := false;

  InitHelper(TFPCanvasHelper(FFont), TFont);
  InitHelper(TFPCanvasHelper(FFrame), TChartPen);
end;

destructor TChartLegend.Destroy;
begin
  FFont.Free;
  FFrame.Free;

  inherited;
end;

procedure TChartLegend.SetAlignment(AValue: TLegendAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartLegend.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartLegend.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

{ TChartTitle }

procedure TChartTitle.Assign(Source: TPersistent);
begin
  if Source is TChartTitle then
    with TChartTitle(Source) do begin
      Self.FAlignment := Alignment;
      Self.FBrush.Assign(Brush);
      Self.FFont.Assign(Font);
      Self.FFrame.Assign(Frame);
      Self.FText.Assign(Text);
   end;

  inherited Assign(Source);
end;

constructor TChartTitle.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);

  FAlignment := taCenter;
  InitHelper(TFPCanvasHelper(FBrush), TBrush);
  FBrush.Color := FOwner.Color;
  InitHelper(TFPCanvasHelper(FFont), TFont);
  FFont.Color := clBlue;
  InitHelper(TFPCanvasHelper(FFrame), TChartPen);
  FText := TStringList.Create;
end;

destructor TChartTitle.Destroy;
begin
  FBrush.Free;
  FFont.Free;
  FFrame.Free;
  FText.Free;

  inherited;
end;

procedure TChartTitle.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartTitle.SetBrush(AValue: TBrush);
begin
  FBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartTitle.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartTitle.SetFrame(AValue: TChartPen);
begin
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartTitle.SetText(AValue: TStrings);
begin
  FText.Assign(AValue);
  StyleChanged(Self);
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
  InitHelper(TFPCanvasHelper(FFont), TFont);
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
  FScale := 1.0;
  FTickColor := clBlack;
  FTickLength := DEF_TICK_LENGTH;
  FTitle := TChartAxisTitle.Create(ACollection.Owner as TCustomChart);
  FVisible := true;
end;

destructor TChartAxis.Destroy;
begin
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
    x := ATransf.XGraphToImage((AMark - Offset) / Scale);

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
    y := ATransf.YGraphToImage((AMark - Offset) / Scale);

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
begin
  Result :=
    SIDE_NAME[Alignment] + VISIBLE_NAME[Visible] + INVERTED_NAME[Inverted];
  if Title.Caption <> '' then
    Result += ' (' + Title.Caption + ')';
end;

function TChartAxis.GetMarks(AMin, AMax: Double): TDoubleDynArray;
begin
  AMin := AMin * Scale + Offset;
  AMax := AMax * Scale + Offset;
  if AMin > AMax then
    Exchange(AMin, AMax);
  Result := GetIntervals(AMin, AMax, Inverted);
end;

function TChartAxis.IsVertical: Boolean; inline;
begin
  Result := Alignment in [calLeft, calRight];
end;

procedure TChartAxis.Measure(
  ACanvas: TCanvas; const AExtent: TDoubleRect;
  var AMargins: TChartAxisMargins);

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
    FSize := maxWidth + ACanvas.TextWidth('0') + TickLength;
  end;

  procedure CalcHorSize;
  begin
    if AExtent.a.X = AExtent.b.X then exit;
    FSize := ACanvas.TextHeight('0') + TickLength;
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

procedure TChartAxis.SetOffset(AValue: Double);
begin
  if FOffset = AValue then exit;
  FOffset := AValue;
  StyleChanged(Self);
end;

procedure TChartAxis.SetScale(AValue: Double);
begin
  if FScale = AValue then exit;
  FScale := AValue;
  if FScale = 0.0 then
    FScale := 1.0;
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

{ TChartMarks }

procedure TChartMarks.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TChartMarks then
    with TChartMarks(Source) do begin
      Self.FDistance := FDistance;
      Self.FFrame.Assign(FFrame);
      Self.FFormat := FFormat;
      Self.FLabelBrush.Assign(FLabelBrush);
      Self.FLabelFont.Assign(FLabelFont);
      Self.FLinkPen.Assign(FLinkPen);
      Self.FStyle := FStyle;
    end;
end;

constructor TChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := DEF_MARKS_DISTANCE;
  InitHelper(TFPCanvasHelper(FFrame), TChartPen);
  InitHelper(TFPCanvasHelper(FLabelBrush), TChartLabelBrush);
  FLabelBrush.Color := clYellow;
  InitHelper(TFPCanvasHelper(FLabelFont), TFont);
  InitHelper(TFPCanvasHelper(FLinkPen), TChartLinkPen);
  FLinkPen.Color := clWhite;
  FStyle := smsNone;
  FVisible := true;
end;

destructor TChartMarks.Destroy;
begin
  FFrame.Free;
  FLabelBrush.Free;
  FLabelFont.Free;
  FLinkPen.Free;
  inherited Destroy;
end;

procedure TChartMarks.DrawLabel(
  ACanvas: TCanvas; const ALabelRect: TRect; const AText: String);
begin
  ACanvas.Brush.Assign(LabelBrush);
  ACanvas.Pen.Assign(Frame);
  ACanvas.Rectangle(ALabelRect);
  ACanvas.Font.Assign(LabelFont);
  ACanvas.TextOut(
    ALabelRect.Left + MARKS_MARGIN_X, ALabelRect.Top + MARKS_MARGIN_Y, AText);
end;

function TChartMarks.IsMarkLabelsVisible: Boolean;
begin
  Result := Visible and (Style <> smsNone) and (Format <> '');
end;

procedure TChartMarks.SetDistance(const AValue: Integer);
begin
  if FDistance = AValue then exit;
  FDistance := AValue;
  StyleChanged(Self);
end;

procedure TChartMarks.SetFormat(const AValue: String);
begin
  if FFormat = AValue then exit;
  FFormat := AValue;
  FStyle := High(FStyle);
  while (FStyle > smsCustom) and (SERIES_MARK_FORMATS[FStyle] <> AValue) do
    Dec(FStyle);
  StyleChanged(Self);
end;

procedure TChartMarks.SetFrame(const AValue: TChartPen);
begin
  if FFrame = AValue then exit;
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartMarks.SetLabelBrush(const AValue: TChartLabelBrush);
begin
  if FLabelBrush = AValue then exit;
  FLabelBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TChartMarks.SetLabelFont(const AValue: TFont);
begin
  if FLabelFont = AValue then exit;
  FLabelFont := AValue;
  StyleChanged(Self);
end;

procedure TChartMarks.SetLinkPen(const AValue: TChartLinkPen);
begin
  if FLinkPen = AValue then exit;
  FLinkPen := AValue;
  StyleChanged(Self);
end;

procedure TChartMarks.SetStyle(const AValue: TSeriesMarksStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  if FStyle <> smsCustom then
    FFormat := SERIES_MARK_FORMATS[FStyle];
  StyleChanged(Self);
end;

{ TSeriesPointer }

procedure TSeriesPointer.Assign(Source: TPersistent);
begin
  if Source is TSeriesPointer then
    with TSeriesPointer(Source) do begin
      Self.FBrush.Assign(Brush);
      Self.FPen.Assign(Pen);
      Self.FStyle := Style;
    end;
  inherited Assign(Source);
end;

constructor TSeriesPointer.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);

  InitHelper(TFPCanvasHelper(FBrush), TBrush);
  InitHelper(TFPCanvasHelper(FPen), TChartPen);

  FHorizSize := DEF_POINTER_SIZE;
  FStyle := psRectangle;
  FVertSize  := DEF_POINTER_SIZE;
  FVisible := true;
end;

destructor TSeriesPointer.Destroy;
begin
  FBrush.Free;
  FPen.Free;
  inherited Destroy;
end;

procedure TSeriesPointer.Draw(ACanvas: TCanvas; ACenter: TPoint; AColor: TColor);

  function PointByIndex(AIndex: Char): TPoint;
  // 7--8--9
  // 4  5  6
  // 1--2--3
  const
    V: array ['1'..'9'] of -1..1 = (1, 1, 1, 0, 0, 0, -1, -1, -1);
    H: array ['1'..'9'] of -1..1 = (-1, 0, 1, -1, 0, 1, -1, 0, 1);
  begin
    Result := ACenter;
    Result.X += H[AIndex] * HorizSize;
    Result.Y += V[AIndex] * VertSize;
  end;

  procedure DrawByString(const AStr: String);
  var
    pts: array of TPoint;
    i: Integer;
    j: Integer = 0;
  begin
    SetLength(pts, Length(AStr));
    for i := 1 to Length(AStr) do begin
      if AStr[i] = ' ' then begin
        if Brush.Style = bsClear then begin
          ACanvas.Polyline(pts, 0, j);
          // Polyline does not draw the end point.
          ACanvas.Pixels[pts[j - 1].X, pts[j - 1].Y] := Pen.Color;
        end
        else
          ACanvas.Polygon(pts, true, 0, j);
        j := 0;
      end
      else begin
        pts[j] := PointByIndex(AStr[i]);
        Inc(j);
      end;
    end;
  end;

const
  DRAW_STRINGS: array [TSeriesPointerStyle] of String = (
    //psNone, psRectangle, psCircle, psCross, psDiagCross, psStar,
    //psLowBracket, psHighBracket, psLeftBracket, psRightBracket, psDiamond);
    '', '17931', '', '28 46', '19 73', '28 46 19 73',
    '41236', '47896', '87412', '89632', '84268');
begin
  ACanvas.Brush.Assign(FBrush);
  ACanvas.Pen.Assign(FPen);

  if FStyle = psCircle then
    ACanvas.Ellipse(
      ACenter.X - HorizSize, ACenter.Y - VertSize,
      ACenter.X + HorizSize, ACenter.Y + VertSize)
  else
    DrawByString(DRAW_STRINGS[FStyle] + ' ');
end;

procedure TSeriesPointer.SetBrush(AValue: TBrush);
begin
  FBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TSeriesPointer.SetHorizSize(AValue: Integer);
begin
  if FHorizSize = AValue then exit;
  FHorizSize := AValue;
  StyleChanged(Self);
end;

procedure TSeriesPointer.SetPen(AValue: TChartPen);
begin
  FPen.Assign(AValue);
  StyleChanged(Self);
end;

procedure TSeriesPointer.SetStyle(AValue: TSeriesPointerStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  StyleChanged(Self);
end;

procedure TSeriesPointer.SetVertSize(AValue: Integer);
begin
  if FVertSize = AValue then exit;
  FVertSize := AValue;
  StyleChanged(Self);
end;

{ TChartExtent }

function TChartExtent.GetUseBounds(AIndex: Integer): Boolean;
begin
  Result := FUseBounds[AIndex];
end;

function TChartExtent.IsBoundsStored(AIndex: Integer): boolean;
begin
  Result := FExtent.coords[AIndex] <> 0;
end;

procedure TChartExtent.CheckBoundsOrder;
begin
  if UseXMin and UseXMax and (XMin >= XMax) then begin
    UseXMin := false;
    UseXMax := false;
    raise EExtentError.Create('ChartExtent: XMin >= XMax');
  end;
  if UseYMin and UseYMax and (YMin >= YMax) then begin
    UseYMin := false;
    UseYMax := false;
    raise EExtentError.Create('ChartExtent: YMin >= YMax');
  end;
end;

function TChartExtent.GetBounds(AIndex: Integer): Double;
begin
  Result := FExtent.coords[AIndex];
end;

procedure TChartExtent.SetUseBounds(AIndex: Integer; AValue: Boolean);
begin
  FUseBounds[AIndex] := AValue;
  StyleChanged(Self);
end;

procedure TChartExtent.SetBounds(AIndex: Integer; const AValue: Double);
begin
  FExtent.coords[AIndex] := AValue;
  StyleChanged(Self);
end;

{ TChartMargins }

procedure TChartMargins.Assign(Source: TPersistent);
begin
  if Source is TChartMargins then
    TChartMargins(Source).FData.FRect := Data;
  inherited Assign(Source);
end;

constructor TChartMargins.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FData.FRect := Rect(DEF_MARGIN, DEF_MARGIN, DEF_MARGIN, DEF_MARGIN);
end;

function TChartMargins.GetValue(AIndex: Integer): integer;
begin
  Result := FData.FCoords[AIndex];
end;

procedure TChartMargins.SetValue(AIndex: integer; AValue: TChartDistance);
begin
  if FData.FCoords[AIndex] = AValue then exit;
  FData.FCoords[AIndex] := AValue;
  StyleChanged(Self);
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

end.

