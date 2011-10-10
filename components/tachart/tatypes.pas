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

{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, FPCanvas, Types,
  TAChartUtils, TADrawUtils;

const
  MARKS_MARGIN_X = 4;
  MARKS_MARGIN_Y = 2;
  DEF_MARGIN = 4;
  DEF_MARKS_DISTANCE = 20;
  DEF_POINTER_SIZE = 4;
  MARKS_YINDEX_ALL = -1;
  DEF_ARROW_LENGTH = 10;
  DEF_ARROW_WIDTH = 5;

type
  TCustomChart = class(TCustomControl)
  public
    procedure StyleChanged(Sender: TObject); virtual; abstract;
    procedure ZoomFull; virtual; abstract;
  end;

  { TChartPen }

  TChartPen = class(TPen)
  strict private
    FVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create; override;
  public
    procedure Assign(ASource: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TFPCanvasHelperClass = class of TFPCanvasHelper;

  { TChartElement }

  TChartElement = class(TPersistent)
  strict protected
    FOwner: TCustomChart;
    FVisible: Boolean;
    procedure InitHelper(var AResult; AClass: TFPCanvasHelperClass);
    procedure SetVisible(AValue: Boolean);
    procedure StyleChanged(Sender: TObject); virtual;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(ASource: TPersistent); override;

    procedure SetOwner(AOwner: TCustomChart);

    property Visible: Boolean read FVisible write SetVisible;
  end;

  TChartMarksOverlapPolicy = (opIgnore, opHideNeighbour);

  TChartTextElement = class(TChartElement)
  strict private
    FClipped: Boolean;
    FOverlapPolicy: TChartMarksOverlapPolicy;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetClipped(AValue: Boolean);
    procedure SetOverlapPolicy(AValue: TChartMarksOverlapPolicy);
  strict protected
    FAlignment: TAlignment;
    procedure AddMargins(ADrawer: IChartDrawer; var ASize: TPoint);
    procedure ApplyLabelFont(ADrawer: IChartDrawer); virtual;
    function IsMarginRequired: Boolean;
  strict protected
    function GetFrame: TChartPen; virtual; abstract;
    function GetLabelAngle: Double; virtual;
    function GetLabelBrush: TBrush; virtual; abstract;
    function GetLabelFont: TFont; virtual; abstract;
    function GetLinkPen: TChartPen; virtual;
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(ASource: TPersistent); override;
    procedure DrawLabel(
      ADrawer: IChartDrawer; const ADataPoint, ALabelCenter: TPoint;
      const AText: String; var APrevLabelPoly: TPointArray);
    function GetLabelPolygon(ADrawer: IChartDrawer; ASize: TPoint): TPointArray;
    function MeasureLabel(ADrawer: IChartDrawer; const AText: String): TSize;
  public
    // If false, labels may overlap axises and legend.
    property Clipped: Boolean read FClipped write SetClipped default true;
    property OverlapPolicy: TChartMarksOverlapPolicy
      read FOverlapPolicy write SetOverlapPolicy default opIgnore;
  published
    property Alignment: TAlignment
      read FAlignment write SetAlignment;
  end;

  TChartTitleFramePen = class(TChartPen)
  published
    property Visible default false;
  end;

  { TChartTitle }

  TChartTitle = class(TChartTextElement)
  strict private
    FBrush: TBrush;
    FFont: TFont;
    FFrame: TChartTitleFramePen;
    FMargin: TChartDistance;
    FText: TStrings;

    procedure SetBrush(AValue: TBrush);
    procedure SetFont(AValue: TFont);
    procedure SetFrame(AValue: TChartTitleFramePen);
    procedure SetMargin(AValue: TChartDistance);
    procedure SetText(AValue: TStrings);
  strict protected
    function GetFrame: TChartPen; override;
    function GetLabelBrush: TBrush; override;
    function GetLabelFont: TFont; override;
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(
      ADrawer: IChartDrawer; ADir, ALeft, ARight: Integer; var AY: Integer);
  published
    property Alignment default taCenter;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartTitleFramePen read FFrame write SetFrame;
    property Margin: TChartDistance
      read FMargin write SetMargin default DEF_MARGIN;
    property Text: TStrings read FText write SetText;
    property Visible default false;
  end;

  TChartMarkAttachment = (maDefault, maEdge, maCenter);

  { TGenericChartMarks }

  {$IFNDEF fpdoc}  // Workaround for issue #18549.
  generic TGenericChartMarks<_TLabelBrush, _TLinkPen, _TFramePen> =
    class(TChartTextElement)
  {$ELSE}
  TGenericChartMarks = class(TChartTextElement)
  {$ENDIF}
  strict private
    FAdditionalAngle: Double;
    FAttachment: TChartMarkAttachment;
    FFrame: _TFramePen;
    FYIndex: Integer;
    function GetDistanceToCenter: Boolean;
    procedure SetAttachment(AValue: TChartMarkAttachment);
    procedure SetDistance(AValue: TChartDistance);
    procedure SetDistanceToCenter(AValue: Boolean);
    procedure SetFormat(AValue: String);
    procedure SetFrame(AValue: _TFramePen);
    procedure SetLabelBrush(AValue: _TLabelBrush);
    procedure SetLabelFont(AValue: TFont);
    procedure SetLinkPen(AValue: _TLinkPen);
    procedure SetStyle(AValue: TSeriesMarksStyle);
    procedure SetYIndex(AValue: Integer);
  strict protected
    FDistance: TChartDistance;
    FFormat: String;
    FLabelBrush: _TLabelBrush;
    FLabelFont: TFont;
    FLinkPen: _TLinkPen;
    FStyle: TSeriesMarksStyle;
  strict protected
    procedure ApplyLabelFont(ADrawer: IChartDrawer); override;
    function GetFrame: TChartPen; override;
    function GetLabelAngle: Double; override;
    function GetLabelBrush: TBrush; override;
    function GetLabelFont: TFont; override;
    function GetLinkPen: TChartPen; override;
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;
  public
    procedure Assign(ASource: TPersistent); override;
    function CenterOffset(ADrawer: IChartDrawer; const AText: String): TSize;
    function IsMarkLabelsVisible: Boolean;
    procedure SetAdditionalAngle(AAngle: Double);
  public
    property DistanceToCenter: Boolean
      read GetDistanceToCenter write SetDistanceToCenter
      stored false default false;
    property Format: String read FFormat write SetFormat;
    property Frame: _TFramePen read FFrame write SetFrame;
    property LabelBrush: _TLabelBrush read FLabelBrush write SetLabelBrush;
    property LinkPen: _TLinkPen read FLinkPen write SetLinkPen;
    property Style: TSeriesMarksStyle read FStyle write SetStyle;
    property YIndex: Integer read FYIndex write SetYIndex default 0;
  published
    property Alignment default taLeftJustify;
    property Attachment: TChartMarkAttachment
      read FAttachment write SetAttachment default maDefault;
    // Distance between labelled object and label.
    property Clipped;
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
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TCustomChart);
  published
    property Distance default DEF_MARKS_DISTANCE;
    property Format;
    property Frame;
    property LabelBrush;
    property LinkPen;
    property OverlapPolicy;
    property Style default smsNone;
    property YIndex;
  end;

  TSeriesPointerStyle = (
    psNone, psRectangle, psCircle, psCross, psDiagCross, psStar,
    psLowBracket, psHighBracket, psLeftBracket, psRightBracket, psDiamond,
    psTriangle, psLeftTriangle, psRightTriangle);

  { TSeriesPointer }

  TSeriesPointer = class(TChartElement)
  strict private
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

    procedure Draw(ADrawer: IChartDrawer; ACenter: TPoint; AColor: TColor);
    procedure DrawSize(
      ADrawer: IChartDrawer; ACenter, ASize: TPoint; AColor: TColor);
  published
    property Brush: TBrush read FBrush write SetBrush;
    property HorizSize: Integer read FHorizSize write SetHorizSize default DEF_POINTER_SIZE;
    property Pen: TChartPen read FPen write SetPen;
    property Style: TSeriesPointerStyle read FStyle write SetStyle default psRectangle;
    property VertSize: Integer read FVertSize write SetVertSize default DEF_POINTER_SIZE;
    property Visible default true;
  end;

  EExtentError = class(EChartError);

  TChartRange = class(TChartElement)
  strict private
    FBounds: array [1..2] of Double;
    FUseBounds: array [1..2] of Boolean;

    function GetBounds(AIndex: Integer): Double;
    function GetUseBounds(AIndex: integer): Boolean;
    function IsBoundsStored(AIndex: Integer): Boolean;
    procedure SetBounds(AIndex: Integer; const AValue: Double);
    procedure SetUseBounds(AIndex: Integer; AValue: Boolean);
  public
    procedure Assign(ASource: TPersistent); override;
    procedure CheckBoundsOrder;
    procedure Intersect(var AMin, AMax: Double);
  published
    property Max: Double index 2 read GetBounds write SetBounds stored IsBoundsStored;
    property Min: Double index 1 read GetBounds write SetBounds stored IsBoundsStored;
    property UseMax: Boolean index 2 read GetUseBounds write SetUseBounds default false;
    property UseMin: Boolean index 1 read GetUseBounds write SetUseBounds default false;
  end;

  { TChartExtent }

  TChartExtent = class(TChartElement)
  strict private
    FExtent: TDoubleRect;
    FUseBounds: array [1..4] of Boolean;

    function GetBounds(AIndex: Integer): Double;
    function GetUseBounds(AIndex: integer): Boolean;
    function IsBoundsStored(AIndex: Integer): Boolean;
    procedure SetBounds(AIndex: Integer; const AValue: Double);
    procedure SetUseBounds(AIndex: Integer; AValue: Boolean);
  public
    procedure Assign(ASource: TPersistent); override;
    procedure CheckBoundsOrder;
  published
    property UseXMax: Boolean index 3 read GetUseBounds write SetUseBounds default false;
    property UseXMin: Boolean index 1 read GetUseBounds write SetUseBounds default false;
    property UseYMax: Boolean index 4 read GetUseBounds write SetUseBounds default false;
    property UseYMin: Boolean index 2 read GetUseBounds write SetUseBounds default false;
    property XMax: Double index 3 read GetBounds write SetBounds stored IsBoundsStored;
    property XMin: Double index 1 read GetBounds write SetBounds stored IsBoundsStored;
    property YMax: Double index 4 read GetBounds write SetBounds stored IsBoundsStored;
    property YMin: Double index 2 read GetBounds write SetBounds stored IsBoundsStored;
  end;

  TRectArray = array [1..4] of Integer;

  { TChartMargins }

  TChartMargins = class(TChartElement)
  strict private
    FData: record
      case Integer of
        0: (FRect: TRect;);
        1: (FCoords: TRectArray;);
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

  TChartArrow = class(TChartElement)
  strict private
    FLength: TChartDistance;
    FWidth: TChartDistance;
    procedure SetLength(AValue: TChartDistance);
    procedure SetWidth(AValue: TChartDistance);
  public
    constructor Create(AOwner: TCustomChart);
  public
    procedure Assign(ASource: TPersistent); override;
    procedure Draw(ADrawer: IChartDrawer; const AEndPos: TPoint; AAngle: Double);
  published
    property Length: TChartDistance
      read FLength write SetLength default DEF_ARROW_LENGTH;
    property Visible default false;
    property Width: TChartDistance
      read FWidth write SetWidth default DEF_ARROW_WIDTH;
  end;

implementation

uses
  GraphMath, Math,
  TACustomSource, TAGeometry;

{ TChartPen }

procedure TChartPen.Assign(ASource: TPersistent);
begin
  if ASource is TChartPen then
    FVisible := TChartPen(ASource).Visible;
  inherited Assign(ASource);
end;

constructor TChartPen.Create;
begin
  inherited Create;
  SetPropDefaults(Self, ['Color', 'Style', 'Visible']);
end;

procedure TChartPen.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
  if Assigned(OnChange) then OnChange(Self);
end;

{ TChartElement }

procedure TChartElement.Assign(ASource: TPersistent);
begin
  if ASource is TChartElement then
    with TChartElement(ASource) do begin
      Self.FVisible := FVisible;
      Self.FOwner := FOwner;
    end;
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

procedure TChartElement.SetVisible(AValue: Boolean);
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

{ TChartTextElement }

procedure TChartTextElement.AddMargins(
  ADrawer: IChartDrawer; var ASize: TPoint);
begin
  if not IsMarginRequired then exit;
  with ADrawer do
    ASize += Point(Scale(MARKS_MARGIN_X), Scale(MARKS_MARGIN_Y)) * 2;
end;

procedure TChartTextElement.ApplyLabelFont(ADrawer: IChartDrawer);
begin
  ADrawer.Font := GetLabelFont;
end;

procedure TChartTextElement.Assign(ASource: TPersistent);
begin
  if ASource is TChartTextElement then
    with TChartTextElement(ASource) do begin
      Self.FAlignment := Alignment;
      Self.FClipped := FClipped;
      Self.FOverlapPolicy := FOverlapPolicy;
    end;
  inherited Assign(ASource);
end;

constructor TChartTextElement.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FClipped := true;
  FOverlapPolicy := opIgnore;
end;

procedure TChartTextElement.DrawLabel(
  ADrawer: IChartDrawer; const ADataPoint, ALabelCenter: TPoint;
  const AText: String; var APrevLabelPoly: TPointArray);
var
  labelPoly: TPointArray;
  ptText: TPoint;
  i, w: Integer;
begin
  ApplyLabelFont(ADrawer);
  ptText := ADrawer.TextExtent(AText);
  w := ptText.X;
  labelPoly := GetLabelPolygon(ADrawer, ptText);
  for i := 0 to High(labelPoly) do
    labelPoly[i] += ALabelCenter;

  if
    (OverlapPolicy = opHideNeighbour) and
    IsPolygonIntersectsPolygon(APrevLabelPoly, labelPoly)
  then
    exit;
  APrevLabelPoly := labelPoly;

  if not Clipped then
    ADrawer.ClippingStop;

  if (ADataPoint <> ALabelCenter) and GetLinkPen.Visible then begin
    ADrawer.Pen := GetLinkPen;
    ADrawer.Line(ADataPoint, ALabelCenter);
  end;
  ADrawer.Brush := GetLabelBrush;
  if IsMarginRequired then begin
    if GetFrame.Visible then
      ADrawer.Pen := GetFrame
    else
      ADrawer.SetPenParams(psClear, clTAColor);
    ADrawer.Polygon(labelPoly, 0, Length(labelPoly));
  end;

  ptText := RotatePoint(-ptText div 2, GetLabelAngle) + ALabelCenter;
  ADrawer.TextOut.Pos(ptText).Alignment(Alignment).Width(w).Text(AText).Done;
  if not Clipped then
    ADrawer.ClippingStart;
end;

function TChartTextElement.GetLabelAngle: Double;
begin
  // Negate to take into account top-down Y axis.
  Result := -OrientToRad(GetLabelFont.Orientation);
end;

function TChartTextElement.GetLabelPolygon(
  ADrawer: IChartDrawer; ASize: TPoint): TPointArray;
begin
  AddMargins(ADrawer, ASize);
  Result := RotateRect(ASize, GetLabelAngle);
end;

function TChartTextElement.GetLinkPen: TChartPen;
begin
  Result := nil;
end;

function TChartTextElement.IsMarginRequired: Boolean;
begin
  with GetFrame do
    Result := (GetLabelBrush.Style <> bsClear) or (Style <> psClear) and Visible;
end;

function TChartTextElement.MeasureLabel(
  ADrawer: IChartDrawer; const AText: String): TSize;
var
  sz: TPoint;
begin
  ApplyLabelFont(ADrawer);
  sz := ADrawer.TextExtent(AText);
  AddMargins(ADrawer, sz);
  Result := MeasureRotatedRect(sz, GetLabelAngle);
end;

procedure TChartTextElement.SetAlignment(AValue: TAlignment);
begin
  if FAlignment = AValue then exit;
  FAlignment := AValue;
  StyleChanged(Self);
end;

procedure TChartTextElement.SetClipped(AValue: Boolean);
begin
  if FClipped = AValue then exit;
  FClipped := AValue;
  StyleChanged(Self);
end;

procedure TChartTextElement.SetOverlapPolicy(AValue: TChartMarksOverlapPolicy);
begin
  if FOverlapPolicy = AValue then exit;
  FOverlapPolicy := AValue;
  StyleChanged(Self);
end;

{ TChartTitle }

procedure TChartTitle.Assign(ASource: TPersistent);
begin
  if ASource is TChartTitle then
    with TChartTitle(ASource) do begin
      Self.FBrush.Assign(Brush);
      Self.FFont.Assign(Font);
      Self.FFrame.Assign(Frame);
      Self.FText.Assign(Text);
   end;

  inherited Assign(ASource);
end;

constructor TChartTitle.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);

  FAlignment := taCenter;
  InitHelper(FBrush, TBrush);
  FBrush.Color := FOwner.Color;
  InitHelper(FFont, TFont);
  FFont.Color := clBlue;
  InitHelper(FFrame, TChartTitleFramePen);
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

procedure TChartTitle.Draw(
  ADrawer: IChartDrawer; ADir, ALeft, ARight: Integer; var AY: Integer);
var
  p, ptSize: TPoint;
  dummy: TPointArray = nil;
begin
  if not Visible or (Text.Count = 0) then exit;
  ptSize := MeasureLabel(ADrawer, Text.Text);
  case Alignment of
    taLeftJustify: p.X := ALeft + ptSize.X div 2;
    taRightJustify: p.X := ARight - ptSize.X div 2;
    taCenter: p.X := (ALeft + ARight) div 2;
  end;
  p.Y := AY + ADir * ptSize.Y div 2;
  DrawLabel(ADrawer, p, p, Text.Text, dummy);
  AY += ADir * (ptSize.Y + Margin);
end;

function TChartTitle.GetFrame: TChartPen;
begin
  Result := Frame;
end;

function TChartTitle.GetLabelBrush: TBrush;
begin
  Result := Brush;
end;

function TChartTitle.GetLabelFont: TFont;
begin
  Result := Font;
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

procedure TChartTitle.SetFrame(AValue: TChartTitleFramePen);
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

procedure TGenericChartMarks.ApplyLabelFont(ADrawer: IChartDrawer);
begin
  inherited ApplyLabelFont(ADrawer);
  if FAdditionalAngle <> 0 then
    ADrawer.AddToFontOrientation(RadToOrient(FAdditionalAngle));
end;

procedure TGenericChartMarks.Assign(ASource: TPersistent);
begin
  if ASource is Self.ClassType then
    with TGenericChartMarks(ASource) do begin
      Self.FDistance := FDistance;
      Self.FFormat := FFormat;
      Self.FFrame.Assign(FFrame);
      // FPC miscompiles virtual calls to generic type arguments,
      // so as a workaround these assignments are moved to the specializations.
      // Self.FLabelBrush.Assign(FLabelBrush);
      // Self.FLabelFont.Assign(FLabelFont);
      // Self.FLinkPen.Assign(FLinkPen);
      Self.FStyle := FStyle;
    end;
  inherited Assign(ASource);
end;

function TGenericChartMarks.CenterOffset(
  ADrawer: IChartDrawer; const AText: String): TSize;
var
  d: Integer;
begin
  d := ADrawer.Scale(Distance);
  Result := Point(d, d);
  if not DistanceToCenter then
    Result += MeasureLabel(ADrawer, AText) div 2;
end;

constructor TGenericChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  InitHelper(FFrame, _TFramePen);
  InitHelper(FLabelBrush, _TLabelBrush);
  InitHelper(FLabelFont, TFont);
  InitHelper(FLinkPen, _TLinkPen);
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

function TGenericChartMarks.GetDistanceToCenter: Boolean;
begin
  Result := Attachment = maCenter;
end;

function TGenericChartMarks.GetFrame: TChartPen;
begin
  Result := Frame;
end;

function TGenericChartMarks.GetLabelAngle: Double;
begin
  Result := inherited GetLabelAngle - FAdditionalAngle;
end;

function TGenericChartMarks.GetLabelBrush: TBrush;
begin
  Result := LabelBrush;
end;

function TGenericChartMarks.GetLabelFont: TFont;
begin
  Result := LabelFont;
end;

function TGenericChartMarks.GetLinkPen: TChartPen;
begin
  Result := LinkPen;
end;

function TGenericChartMarks.IsMarkLabelsVisible: Boolean;
begin
  Result := Visible and (Style <> smsNone) and (Format <> '');
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

procedure TGenericChartMarks.SetFormat(AValue: String);
begin
  if FFormat = AValue then exit;
  TCustomChartSource.CheckFormat(AValue);
  FFormat := AValue;
  FStyle := High(FStyle);
  while (FStyle > smsCustom) and (SERIES_MARK_FORMATS[FStyle] <> AValue) do
    Dec(FStyle);
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetFrame(AValue: _TFramePen);
begin
  if FFrame = AValue then exit;
  FFrame.Assign(AValue);
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetLabelBrush(AValue: _TLabelBrush);
begin
  if FLabelBrush = AValue then exit;
  FLabelBrush.Assign(AValue);
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetLabelFont(AValue: TFont);
begin
  if FLabelFont = AValue then exit;
  FLabelFont := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetLinkPen(AValue: _TLinkPen);
begin
  if FLinkPen = AValue then exit;
  FLinkPen := AValue;
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetStyle(AValue: TSeriesMarksStyle);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
  if FStyle <> smsCustom then
    FFormat := SERIES_MARK_FORMATS[FStyle];
  StyleChanged(Self);
end;

procedure TGenericChartMarks.SetYIndex(AValue: Integer);
begin
  if FYIndex = AValue then exit;
  FYIndex := AValue;
  StyleChanged(Self);
end;

{ TChartMarks }

procedure TChartMarks.Assign(Source: TPersistent);
begin
  if Source is TChartMarks then
    with TChartMarks(Source) do begin
      Self.FLabelBrush.Assign(FLabelBrush);
      Self.FLabelFont.Assign(FLabelFont);
      Self.FLinkPen.Assign(FLinkPen);
    end;
  inherited Assign(Source);
end;

constructor TChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FDistance := DEF_MARKS_DISTANCE;
  FLabelBrush.Color := clYellow;
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

procedure TSeriesPointer.Draw(
  ADrawer: IChartDrawer; ACenter: TPoint; AColor: TColor);
begin
  DrawSize(ADrawer, ACenter, Point(HorizSize, VertSize), AColor);
end;

procedure TSeriesPointer.DrawSize(
  ADrawer: IChartDrawer; ACenter, ASize: TPoint; AColor: TColor);

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
        if Brush.Style = bsClear then
          ADrawer.Polyline(pts, 0, j)
        else
          ADrawer.Polygon(pts, 0, j); // Winding?
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
  ADrawer.Brush := Brush;
  if AColor <> clTAColor then
    ADrawer.BrushColor := AColor;
  ADrawer.Pen := Pen;

  if Style = psCircle then
    ADrawer.Ellipse(
      ACenter.X - ASize.X, ACenter.Y - ASize.Y,
      ACenter.X + ASize.X + 1, ACenter.Y + ASize.Y + 1)
  else
    DrawByString(DRAW_STRINGS[Style] + ' ');
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

{ TChartRange }

procedure TChartRange.Assign(ASource: TPersistent);
begin
  if ASource is TChartRange then
    with TChartRange(ASource) do begin
      Self.FBounds := FBounds;
      Self.FUseBounds := FUseBounds;
    end;
  inherited Assign(ASource);
end;

procedure TChartRange.CheckBoundsOrder;
begin
  if UseMin and UseMax and (Min >= Max) then begin
    UseMin := false;
    UseMax := false;
    raise EExtentError.Create('ChartRange: Min >= Max');
  end;
end;

function TChartRange.GetBounds(AIndex: Integer): Double;
begin
  Result := FBounds[AIndex];
end;

function TChartRange.GetUseBounds(AIndex: integer): Boolean;
begin
  Result := FUseBounds[AIndex];
end;

procedure TChartRange.Intersect(var AMin, AMax: Double);
begin
  if UseMin and (Min > AMin) then
    AMin := Min;
  if UseMax and (Max < AMax)then
    AMax := Max;
end;

function TChartRange.IsBoundsStored(AIndex: Integer): Boolean;
begin
  Result := FBounds[AIndex] <> 0;
end;

procedure TChartRange.SetBounds(AIndex: Integer; const AValue: Double);
begin
  FBounds[AIndex] := AValue;
  StyleChanged(Self);
end;

procedure TChartRange.SetUseBounds(AIndex: Integer; AValue: Boolean);
begin
  FUseBounds[AIndex] := AValue;
  StyleChanged(Self);
end;

{ TChartExtent }

procedure TChartExtent.Assign(ASource: TPersistent);
begin
  if ASource is TChartExtent then
    with TChartExtent(ASource) do begin
      Self.FExtent := FExtent;
      Self.FUseBounds := FUseBounds;
    end;
  inherited Assign(ASource);
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

function TChartExtent.GetUseBounds(AIndex: Integer): Boolean;
begin
  Result := FUseBounds[AIndex];
end;

function TChartExtent.IsBoundsStored(AIndex: Integer): Boolean;
begin
  Result := FExtent.coords[AIndex] <> 0;
end;

procedure TChartExtent.SetBounds(AIndex: Integer; const AValue: Double);
begin
  FExtent.coords[AIndex] := AValue;
  StyleChanged(Self);
end;

procedure TChartExtent.SetUseBounds(AIndex: Integer; AValue: Boolean);
begin
  FUseBounds[AIndex] := AValue;
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

{ TChartArrow }

procedure TChartArrow.Assign(ASource: TPersistent);
begin
  if ASource is TChartArrow then
    with TChartArrow(ASource) do begin
      Self.FLength := FLength;
      Self.FWidth := FWidth;
    end;
  inherited Assign(ASource);
end;

constructor TChartArrow.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FLength := DEF_ARROW_LENGTH;
  FVisible := false;
  FWidth := DEF_ARROW_WIDTH;
end;

procedure TChartArrow.Draw(
  ADrawer: IChartDrawer; const AEndPos: TPoint; AAngle: Double);
var
  da: Double;
  pt: TPoint;
begin
  if not Visible then exit;
  da := ArcTan2(ADrawer.Scale(Width), ADrawer.Scale(Length));
  pt := Point(-ADrawer.Scale(Round(Sqrt(Sqr(Length) + Sqr(Width)))), 0);
  ADrawer.Polyline([
    AEndPos + RotatePoint(pt, AAngle - da), AEndPos,
    AEndPos + RotatePoint(pt, AAngle + da)], 0, 3);
end;

procedure TChartArrow.SetLength(AValue: TChartDistance);
begin
  if FLength = AValue then exit;
  FLength := AValue;
  StyleChanged(Self);
end;

procedure TChartArrow.SetWidth(AValue: TChartDistance);
begin
  if FWidth = AValue then exit;
  FWidth := AValue;
  StyleChanged(Self);
end;

end.

