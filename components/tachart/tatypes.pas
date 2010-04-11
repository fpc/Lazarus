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

  TFPCanvasHelperClass = class of TFPCanvasHelper;

  { TChartElement }

  TChartElement = class(TPersistent)
  protected
    FVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
  protected
    FOwner: TCustomChart;
    procedure InitHelper(var AResult; AClass: TFPCanvasHelperClass);
    procedure StyleChanged(Sender: TObject);
  public
    constructor Create(AOwner: TCustomChart);
    procedure Assign(Source: TPersistent); override;

    procedure SetOwner(AOwner: TCustomChart);

    property Visible: Boolean read FVisible write SetVisible;
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

  { TGenericChartMarks }

  generic TGenericChartMarks<_TLabelBrush, _TLinkPen, _TFramePen> =
    class(TChartElement)
  protected
    FClipped: Boolean;
    FDistance: TChartDistance;
    FFormat: String;
    FFrame: _TFramePen;
    FLabelBrush: _TLabelBrush;
    FLabelFont: TFont;
    FLinkPen: _TLinkPen;
    FStyle: TSeriesMarksStyle;

    procedure SetClipped(const AValue: Boolean);
    procedure SetDistance(AValue: TChartDistance);
    procedure SetFormat(const AValue: String);
    procedure SetFrame(const AValue: _TFramePen);
    procedure SetLabelBrush(const AValue: _TLabelBrush);
    procedure SetLabelFont(const AValue: TFont);
    procedure SetLinkPen(const AValue: _TLinkPen);
    procedure SetStyle(const AValue: TSeriesMarksStyle);
  protected
    function IsMarginRequired: Boolean;
  public
    constructor Create(AOwner: TCustomChart);
    destructor Destroy; override;

  public
    procedure Assign(Source: TPersistent); override;
    procedure DrawLabel(
      ACanvas: TCanvas; const ALabelRect: TRect; const AText: String);
    function IsMarkLabelsVisible: Boolean;
    function MeasureLabel(ACanvas: TCanvas; const AText: String): TSize;

  public
    property Frame: _TFramePen read FFrame write SetFrame;
    property LabelBrush: _TLabelBrush read FLabelBrush write SetLabelBrush;
    property LinkPen: _TLinkPen read FLinkPen write SetLinkPen;
  published
    // If false, labels may overlap axises and legend.
    property Clipped: Boolean read FClipped write SetClipped default true;
    // Distance between series point and label.
    property Distance: TChartDistance read FDistance write SetDistance;
    property Format: String read FFormat write SetFormat;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property Style: TSeriesMarksStyle read FStyle write SetStyle;
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

  { TChartMarks }

  TChartMarks = class(
    specialize TGenericChartMarks<TChartLabelBrush, TChartLinkPen, TChartPen>)
  public
    constructor Create(AOwner: TCustomChart);
  published
    property Distance default DEF_MARKS_DISTANCE;
    property Frame;
    property LabelBrush;
    property LinkPen;
    property Style default smsNone;
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

implementation

uses
  TASources;

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
  //inherited Assign(Source);
  if Source is TChartElement then
    Self.FVisible := TChartElement(Source).FVisible;
end;

constructor TChartElement.Create(AOwner: TCustomChart);
begin
  inherited Create;
  FOwner := AOwner;
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
    FOwner.Invalidate;
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

{ TGenericChartMarks }

procedure TGenericChartMarks.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is Self.ClassType then
    with TGenericChartMarks(Source) do begin
      Self.FDistance := FDistance;
      Self.FFrame.Assign(FFrame);
      Self.FFormat := FFormat;
      Self.FLabelBrush.Assign(FLabelBrush);
      Self.FLabelFont.Assign(FLabelFont);
      Self.FLinkPen.Assign(FLinkPen);
      Self.FStyle := FStyle;
    end;
end;

constructor TGenericChartMarks.Create(AOwner: TCustomChart);
begin
  inherited Create(AOwner);
  FClipped := true;
  InitHelper(FFrame, _TFramePen);
  InitHelper(FLabelBrush, _TLabelBrush);
  InitHelper(FLabelFont, TFont);
  InitHelper(FLinkPen, _TLinkPen);
  FStyle := smsNone;
  FVisible := true;
end;

destructor TGenericChartMarks.Destroy;
begin
  FFrame.Free;
  FLabelBrush.Free;
  FLabelFont.Free;
  FLinkPen.Free;
  inherited Destroy;
end;

procedure TGenericChartMarks.DrawLabel(
  ACanvas: TCanvas; const ALabelRect: TRect; const AText: String);
var
  wasClipping: Boolean = false;
  pt: TPoint;
begin
  if not Clipped and ACanvas.Clipping then begin
    ACanvas.Clipping := false;
    wasClipping := true;
  end;
  pt := ALabelRect.TopLeft;
  ACanvas.Font.Assign(LabelFont);
  ACanvas.Brush.Assign(LabelBrush);
  if IsMarginRequired then begin
    ACanvas.Pen.Assign(Frame);
    ACanvas.Rectangle(ALabelRect);
    pt += Point(MARKS_MARGIN_X, MARKS_MARGIN_Y);
  end;
  ACanvas.TextOut(pt.X, pt.Y, AText);
  if wasClipping then
    ACanvas.Clipping := true;
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

function TGenericChartMarks.MeasureLabel(
  ACanvas: TCanvas; const AText: String): TSize;
begin
  ACanvas.Font.Assign(LabelFont);
  Result := ACanvas.TextExtent(AText);
  if IsMarginRequired then begin
    Result.cx += 2 * MARKS_MARGIN_X;
    Result.cy += 2 * MARKS_MARGIN_Y;
  end;
end;

procedure TGenericChartMarks.SetClipped(const AValue: Boolean);
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
  Unused(AColor); // TODO
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

end.

