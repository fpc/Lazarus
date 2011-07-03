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

type
  TCustomChart = class(TCustomControl)
  public
    procedure StyleChanged(Sender: TObject); virtual; abstract;
    procedure ZoomFull; virtual; abstract;
  end;

  { TChartPen }

  TChartPen = class(TPen)
  private
    FVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TFPCanvasHelperClass = class of TFPCanvasHelper;

  { TChartElement }

  TChartElement = class(TPersistent)
  protected
    FVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    FOwner: TCustomChart;
    function GetOwner: TPersistent; override;
    procedure InitHelper(var AResult; AClass: TFPCanvasHelperClass);
    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(Source: TPersistent); override;

    procedure SetOwner(AOwner: TCustomChart);

    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TChartTitle }

  TChartTitle = class(TChartElement)
  private
    FAlignment: TAlignment;
    FBrush: TBrush;
    FFont: TFont;
    FFrame: TChartPen;
    FMargin: TChartDistance;
    FText: TStrings;

    procedure SetAlignment(AValue: TAlignment);
    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartPen);
    procedure SetMargin(AValue: TChartDistance);
    procedure SetText(AValue: TStrings);
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment
      read FAlignment write SetAlignment default taCenter;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Margin: TChartDistance
      read FMargin write SetMargin default DEF_MARGIN;
    property Text: TStrings read FText write SetText;
    property Visible default false;
  end;

  TChartMarksOverlapPolicy = (opIgnore, opHideNeighbour);

  TChartMarkAttachment = (maDefault, maEdge, maCenter);

  { TGenericChartMarks }

  {$IFNDEF fpdoc}  // Workaround for issue #18549.
  generic TGenericChartMarks<_TLabelBrush, _TLinkPen, _TFramePen> =
    class(TChartElement)
  {$ELSE}
  TGenericChartMarks = class(TChartElement)
  {$ENDIF}
  private
    function GetDistanceToCenter: Boolean;
    function LabelAngle: Double; inline;
    procedure PutLabelFontTo(ACanvas: TCanvas);
    procedure SetAttachment(AValue: TChartMarkAttachment);
    procedure SetDistanceToCenter(AValue: Boolean);
  protected
    FAdditionalAngle: Double;
    FAttachment: TChartMarkAttachment;
    FClipped: Boolean;
    FDistance: TChartDistance;
    FFormat: String;
    FFrame: _TFramePen;
    FLabelBrush: _TLabelBrush;
    FLabelFont: TFont;
    FLinkPen: _TLinkPen;
    FOverlapPolicy: TChartMarksOverlapPolicy;
    FStyle: TSeriesMarksStyle;

    procedure SetClipped(AValue: Boolean);
    procedure SetDistance(AValue: TChartDistance);
    procedure SetFormat(const AValue: String);
    procedure SetFrame(const AValue: _TFramePen);
    procedure SetLabelBrush(const AValue: _TLabelBrush);
    procedure SetLabelFont(const AValue: TFont);
    procedure SetLinkPen(const AValue: _TLinkPen);
    procedure SetOverlapPolicy(AValue: TChartMarksOverlapPolicy);
    procedure SetStyle(const AValue: TSeriesMarksStyle);
  protected
    function IsMarginRequired: Boolean;
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure Assign(Source: TPersistent); override;
    function CenterOffset(ACanvas: TCanvas; const AText: String): TSize;
    procedure DrawLabel(
      ACanvas: TCanvas; const ADataPoint, ALabelCenter: TPoint;
      const AText: String; var APrevLabelPoly: TPointArray);
    function GetLabelPolygon(ASize: TPoint): TPointArray;
    function IsMarkLabelsVisible: Boolean;
    function MeasureLabel(ACanvas: TCanvas; const AText: String): TSize;
    procedure SetAdditionalAngle(AAngle: Double);
  public
    property DistanceToCenter: Boolean
      read GetDistanceToCenter write SetDistanceToCenter
      stored false default false;
    property Format: String read FFormat write SetFormat;
    property Frame: _TFramePen read FFrame write SetFrame;
    property LabelBrush: _TLabelBrush read FLabelBrush write SetLabelBrush;
    property LinkPen: _TLinkPen read FLinkPen write SetLinkPen;
    property OverlapPolicy: TChartMarksOverlapPolicy
      read FOverlapPolicy write SetOverlapPolicy default opIgnore;
    property Style: TSeriesMarksStyle read FStyle write SetStyle;
  published
    property Attachment: TChartMarkAttachment
      read FAttachment write SetAttachment default maDefault;
    // If false, labels may overlap axises and legend.
    property Clipped: Boolean read FClipped write SetClipped default true;
    // Distance between labelled object and label.
    property Distance: TChartDistance read FDistance write SetDistance;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property Visible default true;
  end;

  TChartLinkPen = class(TChartPen)
  published
    property Color default clWhite;
  end;

  TChartLabelBrush = class(TBrush)
  published
    property Color default clYellow;
  end;

  {$IFNDEF fpdoc}  // Workaround for issue #18549.
  TCustomChartMarks =
    specialize TGenericChartMarks<TChartLabelBrush, TChartLinkPen, TChartPen>;
  {$ENDIF}

  { TChartMarks }

  TChartMarks = class(TCustomChartMarks)
  public
    constructor Create(AOwner: TCustomChart);
  published
    property Distance default DEF_MARKS_DISTANCE;
    property Format;
    property Frame;
    property LabelBrush;
    property LinkPen;
    property OverlapPolicy;
    property Style default smsNone;
  end;

  TSeriesPointerStyle = (
    psNone, psRectangle, psCircle, psCross, psDiagCross, psStar,
    psLowBracket, psHighBracket, psLeftBracket, psRightBracket, psDiamond,
    psTriangle, psLeftTriangle, psRightTriangle);

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
  public
    procedure Assign(Source: TPersistent); override;

    procedure Draw(ACanvas: TCanvas; ACenter: TPoint; AColor: TColor);
    procedure DrawSize(
      ACanvas: TCanvas; ACenter, ASize: TPoint; AColor: TColor);
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

implementation

uses
  TACustomSource;

{ TChartPen }

procedure TChartPen.Assign(Source: TPersistent);
begin
  if Source is TChartPen then
    with TChartPen(Source) do
      FVisible := Visible;
  inherited Assign(Source);
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
  if Source is TChartElement then
    with TChartElement(Source) do begin
      Self.FVisible := FVisible;
      Self.FOwner := FOwner;
    end;
  inherited Assign(Source);
end;

constructor TChartElement.Create(AOwner: TCustomChart);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TChartElement.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TChartElement.InitHelper(var AResult; AClass: TFPCanvasHelperClass);
begin
  TFPCanvasHelper(AResult) := AClass.Create;
  TFPCanvasHelper(AResult).OnChange := @StyleChanged;
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
    FOwner.StyleChanged(Sender);
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
  InitHelper(FBrush, TBrush);
  FBrush.Color := FOwner.Color;
  InitHelper(FFont, TFont);
  FFont.Color := clBlue;
  InitHelper(FFrame, TChartPen);
  FMargin := DEF_MARGIN;
  FText := TStringList.Create;
  TStringList(FText).OnChange := @StyleChanged;
end;

destructor TChartTitle.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FFont);
  FreeAndNil(FFrame);
  FreeAndNil(FText);

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

procedure TChartTitle.SetMargin(AValue: TChartDistance);
begin
  if FMargin = AValue then exit;
  FMargin := AValue;
  StyleChanged(Self);
end;

procedure TChartTitle.SetText(AValue: TStrings);
begin
  FText.Assign(AValue);
  StyleChanged(Self);
end;

{ TGenericChartMarks }

procedure TGenericChartMarks.Assign(Source: TPersistent);
begin
  if Source is Self.ClassType then
    with TGenericChartMarks(Source) do begin
      Self.FClipped := FClipped;
      Self.FDistance := FDistance;
      Self.FFormat := FFormat;
      Self.FFrame.Assign(FFrame);
      Self.FLabelBrush.Assign(FLabelBrush);
      Self.FLabelFont.Assign(FLabelFont);
      Self.FLinkPen.Assign(FLinkPen);
      Self.FOverlapPolicy := FOverlapPolicy;
      Self.FStyle := FStyle;
    end;
  inherited Assign(Source);
end;

function TGenericChartMarks.CenterOffset(
  ACanvas: TCanvas; const AText: String): TSize;
begin
  Result := Point(Distance, Distance);
  if not DistanceToCenter then
    Result += MeasureLabel(ACanvas, AText) div 2;
end;

constructor TGenericChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FClipped := true;
  InitHelper(FFrame, _TFramePen);
  InitHelper(FLabelBrush, _TLabelBrush);
  InitHelper(FLabelFont, TFont);
  InitHelper(FLinkPen, _TLinkPen);
  FOverlapPolicy := opIgnore;
  FStyle := smsNone;
  FVisible := true;
end;

destructor TGenericChartMarks.Destroy;
begin
  FreeAndNil(FFrame);
  FreeAndNil(FLabelBrush);
  FreeAndNil(FLabelFont);
  FreeAndNil(FLinkPen);
  inherited;
end;

procedure TGenericChartMarks.DrawLabel(
  ACanvas: TCanvas; const ADataPoint, ALabelCenter: TPoint;
  const AText: String; var APrevLabelPoly: TPointArray);
var
  wasClipping: Boolean = false;
  labelPoly: TPointArray;
  ptText: TPoint;
  i: Integer;
begin
  PutLabelFontTo(ACanvas);
  ptText := ACanvas.TextExtent(AText);
  labelPoly := GetLabelPolygon(ptText);
  for i := 0 to High(labelPoly) do
    labelPoly[i] += ALabelCenter;

  if
    (OverlapPolicy = opHideNeighbour) and
    IsPolygonIntersectsPolygon(APrevLabelPoly, labelPoly)
  then
    exit;
  APrevLabelPoly := labelPoly;

  if not Clipped and ACanvas.Clipping then begin
    ACanvas.Clipping := false;
    wasClipping := true;
  end;

  if LinkPen.Visible then begin
    ACanvas.Pen.Assign(LinkPen);
    ACanvas.Line(ADataPoint, ALabelCenter);
  end;
  ACanvas.Brush.Assign(LabelBrush);
  if IsMarginRequired then begin
    ACanvas.Pen.Assign(Frame);
    ACanvas.Polygon(labelPoly);
  end;

  ptText := RotatePoint(-ptText div 2, LabelAngle) + ALabelCenter;
  ACanvas.TextOut(ptText.X, ptText.Y, AText);
  if wasClipping then
    ACanvas.Clipping := true;
end;

function TGenericChartMarks.GetDistanceToCenter: Boolean;
begin
  Result := Attachment = maCenter;
end;

function TGenericChartMarks.GetLabelPolygon(ASize: TPoint): TPointArray;
var
  i: Integer;
  a: Double;
begin
  if IsMarginRequired then
    ASize += Point(MARKS_MARGIN_X, MARKS_MARGIN_Y) * 2;
  SetLength(Result, 4);
  Result[0] := -ASize div 2;
  Result[2] := Result[0] + ASize;
  Result[1] := Point(Result[2].X, Result[0].Y);
  Result[3] := Point(Result[0].X, Result[2].Y);
  a := LabelAngle;
  for i := 0 to High(Result) do
    Result[i] := RotatePoint(Result[i], a);
end;

function TGenericChartMarks.IsMarginRequired: Boolean;
begin
  Result :=
    (LabelBrush.Style <> bsClear) or
    (Frame.Style <> psClear) and Frame.Visible;
end;

function TGenericChartMarks.IsMarkLabelsVisible: Boolean;
begin
  Result := Visible and (Style <> smsNone) and (Format <> '');
end;

function TGenericChartMarks.LabelAngle: Double;
begin
  // Negate to take into account top-down Y axis.
  Result := -OrientToRad(LabelFont.Orientation) - FAdditionalAngle;
end;

function TGenericChartMarks.MeasureLabel(
  ACanvas: TCanvas; const AText: String): TSize;
var
  sz: TPoint;
begin
  PutLabelFontTo(ACanvas);
  sz := ACanvas.TextExtent(AText);
  if IsMarginRequired then
    sz += Point(MARKS_MARGIN_X, MARKS_MARGIN_Y) * 2;
  Result := MeasureRotatedRect(sz, LabelAngle);
end;

procedure TGenericChartMarks.PutLabelFontTo(ACanvas: TCanvas);
begin
  with ACanvas.Font do begin
    Assign(LabelFont);
    if FAdditionalAngle <> 0 then
      Orientation := Orientation + RadToOrient(FAdditionalAngle);
  end;
end;

procedure TGenericChartMarks.SetAdditionalAngle(AAngle: Double);
begin
  FAdditionalAngle := AAngle;
end;

procedure TGenericChartMarks.SetAttachment(AValue: TChartMarkAttachment);
begin
  if FAttachment = AValue then exit;
  FAttachment := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetClipped(AValue: Boolean);
begin
  if FClipped = AValue then exit;
  FClipped := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetDistance(AValue: TChartDistance);
begin
  if FDistance = AValue then exit;
  FDistance := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetDistanceToCenter(AValue: Boolean);
begin
  if AValue then
    Attachment := maCenter
  else
    Attachment := maDefault;
end;

procedure TGenericChartMarks.SetFormat(const AValue: String);
begin
  if FFormat = AValue then exit;
  TCustomChartSource.CheckFormat(AValue);
  FFormat := AValue;
  FStyle := High(FStyle);
  while (FStyle > smsCustom) and (SERIES_MARK_FORMATS[FStyle] <> AValue) do
    Dec(FStyle);
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetFrame(const AValue: _TFramePen);
begin
  if FFrame = AValue then exit;
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetLabelBrush(const AValue: _TLabelBrush);
begin
  if FLabelBrush = AValue then exit;
  FLabelBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetLabelFont(const AValue: TFont);
begin
  if FLabelFont = AValue then exit;
  FLabelFont := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetLinkPen(const AValue: _TLinkPen);
begin
  if FLinkPen = AValue then exit;
  FLinkPen := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetOverlapPolicy(AValue: TChartMarksOverlapPolicy);
begin
  if FOverlapPolicy = AValue then exit;
  FOverlapPolicy := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetStyle(const AValue: TSeriesMarksStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  if FStyle <> smsCustom then
    FFormat := SERIES_MARK_FORMATS[FStyle];
  StyleChanged(Self);
end;

{ TChartMarks }

constructor TChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := DEF_MARKS_DISTANCE;
  FLabelBrush.Color := clYellow;
  FLinkPen.Color := clWhite;
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

  InitHelper(FBrush, TBrush);
  InitHelper(FPen, TChartPen);

  FHorizSize := DEF_POINTER_SIZE;
  FStyle := psRectangle;
  FVertSize  := DEF_POINTER_SIZE;
  FVisible := true;
end;

destructor TSeriesPointer.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FPen);
  inherited;
end;

procedure TSeriesPointer.Draw(ACanvas: TCanvas; ACenter: TPoint; AColor: TColor);
begin
  DrawSize(ACanvas, ACenter, Point(HorizSize, VertSize), AColor);
end;

procedure TSeriesPointer.DrawSize(
  ACanvas: TCanvas; ACenter, ASize: TPoint; AColor: TColor);

  function PointByIndex(AIndex: Char): TPoint;
  // 7--8--9
  // 4  5  6
  // 1--2--3
  const
    V: array ['1'..'9'] of -1..1 = (1, 1, 1, 0, 0, 0, -1, -1, -1);
    H: array ['1'..'9'] of -1..1 = (-1, 0, 1, -1, 0, 1, -1, 0, 1);
  begin
    Result := ACenter;
    Result.X += H[AIndex] * ASize.X;
    Result.Y += V[AIndex] * ASize.Y;
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
    // psNone, psRectangle, psCircle, psCross, psDiagCross, psStar,
    // psLowBracket, psHighBracket, psLeftBracket, psRightBracket, psDiamond,
    // psTriangle, psLeftTriangle, psRightTriangle
    '', '17931', '', '28 46', '19 73', '28 46 19 73',
    '41236', '47896', '87412', '89632', '84268',
    '183', '842', '862');
begin
  ACanvas.Brush.Assign(FBrush);
  if AColor <> clTAColor then
    ACanvas.Brush.Color := AColor;
  ACanvas.Pen.Assign(FPen);

  if FStyle = psCircle then
    ACanvas.Ellipse(
      ACenter.X - ASize.X, ACenter.Y - ASize.Y,
      ACenter.X + ASize.X, ACenter.Y + ASize.Y)
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

end.

