{

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
unit TAChartAxisUtils;

{$H+}

interface

uses
  Classes, Graphics,
  TAChartUtils, TACustomSource, TADrawUtils, TAStyles, TATypes;

const
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

implementation

uses
  SysUtils,
  TAGeometry, TAIntervalSources;

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
    with TChartBasicAxis(ASource) do begin
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

end.
