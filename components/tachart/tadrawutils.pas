{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TADrawUtils;

{$H+}

interface

uses
  Classes, FPCanvas, FPImage, Types, TAChartUtils;

type
  // Same types as in Graphics unit, but without dependency.
  TChartAntialiasingMode = (amDontCare, amOn, amOff);

type
  ISimpleTextOut = interface
    function HtmlTextExtent(const AText: String): TPoint;
    procedure HtmlTextOut(AX, AY: Integer; const AText: String);
    procedure SimpleTextOut(AX, AY: Integer; const AText: String);
    function SimpleTextExtent(const AText: String): TPoint;
    function GetFontAngle: Double;
  end;

  { TChartTextOut }

  TChartTextOut = class
  strict private
    FAlignment: TAlignment;
    FPos: TPoint;
    FSimpleTextOut: ISimpleTextOut;
    FText1: String;
    FText2: TStrings;
    FTextFormat: TChartTextFormat;
    FWidth: Integer;

    procedure DoTextOutList;
    procedure DoTextOutString;
  public
    constructor Create(ASimpleTextOut: ISimpleTextOut);
  public
    function Alignment(AAlignment: TAlignment): TChartTextOut;
    procedure Done;
    function Pos(AX, AY: Integer): TChartTextOut;
    function Pos(const APos: TPoint): TChartTextOut;
    function Text(const AText: String): TChartTextOut;
    function Text(AText: TStrings): TChartTextOut;
    function TextFormat(AFormat: TChartTextFormat): TChartTextOut;
    function Width(AWidth: Integer): TChartTextOut;
  end;

  TChartColorToFPColorFunc = function (AColor: TChartColor): TFPColor;
  TGetFontOrientationFunc = function (AFont: TFPCustomFont): Integer;

  TChartTransparency = 0..255;

  TScaleItem = (scaleFont, scalePen);
  TScaleItems = set of TScaleItem;

  IChartDrawer = interface
   ['{6D8E5591-6788-4D2D-9FE6-596D5157C3C3}']
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStart;
    procedure ClippingStop;
    procedure DrawingBegin(const ABoundingBox: TRect);
    procedure DrawingEnd;
    procedure DrawLineDepth(AX1, AY1, AX2, AY2, ADepth: Integer);
    procedure DrawLineDepth(const AP1, AP2: TPoint; ADepth: Integer);
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetFontAngle: Double;       // in radians
    function GetFontColor: TFPColor;
    function GetFontName: String;
    function GetFontSize: Integer;
    function GetFontStyle: TChartFontStyles;
    procedure SetDoChartColorToFPColorFunc(AValue: TChartColorToFPColorFunc);
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer);
    procedure LineTo(const AP: TPoint);
    procedure MoveTo(AX, AY: Integer);
    procedure MoveTo(const AP: TPoint);
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure PutImage(AX, AY: Integer; AImage: TFPCustomImage);
    procedure PutPixel(AX, AY: Integer; AColor: TChartColor);
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure ResetFont;
    function Scale(ADistance: Integer): Integer;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetFont(AValue: TFPCustomFont);
    procedure SetGetFontOrientationFunc(AValue: TGetFontOrientationFunc);
    procedure SetMonochromeColor(AColor: TChartColor);
    procedure SetPen(APen: TFPCustomPen);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
    function GetRightToLeft: Boolean;
    procedure SetRightToLeft(AValue: Boolean);
    procedure SetTransparency(ATransparency: TChartTransparency);
    procedure SetXor(AXor: Boolean);
    function TextExtent(const AText: String;
      ATextFormat: TChartTextFormat = tfNormal): TPoint;
    function TextExtent(AText: TStrings;
      ATextFormat: TChartTextFormat = tfNormal): TPoint;
    function TextOut: TChartTextOut;

    property Brush: TFPCustomBrush write SetBrush;
    property BrushColor: TChartColor read GetBrushColor write SetBrushColor;
    property Font: TFPCustomFont write SetFont;
    property Pen: TFPCustomPen write SetPen;
    property DoChartColorToFPColor: TChartColorToFPColorFunc
      write SetDoChartColorToFPColorFunc;
    property DoGetFontOrientation: TGetFontOrientationFunc
      write SetGetFontOrientationFunc;
  end;

  { TBasicDrawer }

  TBasicDrawer = class(TInterfacedObject, ISimpleTextOut)
  strict protected
    FChartColorToFPColorFunc: TChartColorToFPColorFunc;
    FGetFontOrientationFunc: TGetFontOrientationFunc;
    FMonochromeColor: TChartColor;
    FRightToLeft: Boolean;
    FTransparency: TChartTransparency;
    FXor: Boolean;
    FScaleItems: TScaleItems;
    function ColorOrMono(AColor: TChartColor): TChartColor; inline;
    function FPColorOrMono(const AColor: TFPColor): TFPColor; inline;
//    function GetFontAngle: Double; virtual; abstract;
    function SimpleTextExtent(const AText: String): TPoint; virtual; abstract;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); virtual; abstract;
    function HtmlTextExtent(const AText: String): TPoint;
    procedure HtmlTextOut(AX, AY: Integer; const AText: String);
  public
    constructor Create;
    procedure DrawingBegin(const ABoundingBox: TRect); virtual;
    procedure DrawingEnd; virtual;
    procedure DrawLineDepth(AX1, AY1, AX2, AY2, ADepth: Integer);
    procedure DrawLineDepth(const AP1, AP2: TPoint; ADepth: Integer);
    function GetFontAngle: Double; virtual; abstract;
    function GetFontColor: TFPColor; virtual; abstract;
    function GetFontName: String; virtual; abstract;
    function GetFontSize: Integer; virtual; abstract;
    function GetFontStyle: TChartFontStyles; virtual; abstract;
    function GetRightToLeft: Boolean;
    procedure LineTo(AX, AY: Integer); virtual; abstract;
    procedure LineTo(const AP: TPoint);
    procedure MoveTo(AX, AY: Integer); virtual; abstract;
    procedure MoveTo(const AP: TPoint);
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); virtual; abstract;
    procedure PutImage(AX, AY: Integer; AImage: TFPCustomImage); virtual;
    procedure PutPixel(AX, AY: Integer; AColor: TChartColor); virtual;
    function Scale(ADistance: Integer): Integer; virtual;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    procedure SetDoChartColorToFPColorFunc(AValue: TChartColorToFPColorFunc);
    procedure SetGetFontOrientationFunc(AValue: TGetFontOrientationFunc);
    procedure SetMonochromeColor(AColor: TChartColor);
    procedure SetRightToLeft(AValue: Boolean);
    procedure SetTransparency(ATransparency: TChartTransparency);
    procedure SetXor(AXor: Boolean);
    function TextExtent(const AText: String; ATextFormat: TChartTextFormat = tfNormal): TPoint;
    function TextExtent(AText: TStrings; ATextFormat: TChartTextFormat = tfNormal): TPoint;
    function TextOut: TChartTextOut;
  end;

  function ChartColorToFPColor(AChartColor: TChartColor): TFPColor;
  function FPColorToChartColor(AFPColor: TFPColor): TChartColor;
  function ColorDef(AColor, ADefaultColor: TChartColor): TChartColor; inline;

implementation

uses
  Math, TAGeometry, TAHtml;

const
  LINE_INTERVAL = 2;

function ChartColorToFPColor(AChartColor: TChartColor): TFPColor;
begin
  with Result do begin
    red := AChartColor and $FF;
    red += red shl 8;
    green := (AChartColor and $FF00);
    green += green shr 8;
    blue := (AChartColor and $FF0000) shr 8;
    blue += blue shr 8;
    alpha := alphaOpaque;
  end;
end;

function DummyGetFontOrientationFunc(AFont: TFPCustomFont): Integer;
begin
  Unused(AFont);
  Result := 0;
end;

function FPColorToChartColor(AFPColor: TFPColor): TChartColor;
begin
  Result :=
    ((AFPColor.red shr 8) and $FF) or
    (AFPColor.green and $FF00) or
    ((AFPColor.blue shl 8) and $FF0000);
end;

function ColorDef(AColor, ADefaultColor: TChartColor): TChartColor;
begin
  Result := IfThen(AColor = clTAColor, ADefaultColor, AColor);
end;

{ TChartTextOut }

function TChartTextOut.Alignment(AAlignment: TAlignment): TChartTextOut;
begin
  FAlignment := AAlignment;
  Result := Self;
end;

constructor TChartTextOut.Create(ASimpleTextOut: ISimpleTextOut);
begin
  FSimpleTextOut := ASimpleTextOut;
  FAlignment := taLeftJustify;
end;

procedure TChartTextOut.Done;
begin
  if FText2 = nil then
    DoTextOutString
  else
    DoTextOutList;
  Free;
end;

procedure TChartTextOut.DoTextOutList;
var
  i: Integer;
  a: Double;
  lineExtent, p: TPoint;
begin
  a := -FSimpleTextOut.GetFontAngle;
  for i := 0 to FText2.Count - 1 do begin
    case FTextFormat of
      tfNormal: lineExtent := FSimpleTextOut.SimpleTextExtent(FText2[i]);
      tfHtml  : lineExtent := FSimpleTextOut.HtmlTextExtent(FText2[i]);
    end;
    p := FPos;
    case FAlignment of
      taCenter: p += RotatePointX((FWidth - lineExtent.X) div 2, a);
      taRightJustify: p += RotatePointX(FWidth - lineExtent.X, a);
    end;
    case FTextFormat of
      tfNormal: FSimpleTextOut.SimpleTextOut(p.X, p.Y, FText2[i]);
      tfHtml  : FSimpleTextOut.HtmlTextOut(p.X, p.Y, FText2[i]);
    end;
    FPos += RotatePoint(Point(0, lineExtent.Y + LINE_INTERVAL), a);
  end;
end;

procedure TChartTextOut.DoTextOutString;
begin
  if System.Pos(LineEnding, FText1) = 0 then begin
    case FTextFormat of
      tfNormal: FSimpleTextOut.SimpleTextOut(FPos.X, FPos.Y, FText1);
      tfHtml  : FSimpleTextOut.HtmlTextOut(FPos.X, FPos.Y, FText1);
    end;
    exit;
  end;
  FText2 := TStringList.Create;
  try
    FText2.Text := FText1;
    DoTextOutList;
  finally
    FText2.Free;
  end;
end;

function TChartTextOut.Pos(AX, AY: Integer): TChartTextOut;
begin
  FPos := Point(AX, AY);
  Result := Self;
end;

function TChartTextOut.Pos(const APos: TPoint): TChartTextOut;
begin
  FPos := APos;
  Result := Self;
end;

function TChartTextOut.Text(const AText: String): TChartTextOut;
begin
  FText1 := AText;
  Result := Self;
end;

function TChartTextOut.Text(AText: TStrings): TChartTextOut;
begin
  FText2 := AText;
  Result := Self;
end;

function TChartTextOut.TextFormat(AFormat: TChartTextFormat): TChartTextOut;
begin
  FTextFormat := AFormat;
  Result := Self;
end;

function TChartTextOut.Width(AWidth: Integer): TChartTextOut;
begin
  FWidth := AWidth;
  Result := Self;
end;

{ TBasicDrawer }

function TBasicDrawer.ColorOrMono(AColor: TChartColor): TChartColor;
begin
  Result := ColorDef(FMonochromeColor, AColor);
end;

constructor TBasicDrawer.Create;
begin
  FChartColorToFPColorFunc := @ChartColorToFPColor;
  FGetFontOrientationFunc := @DummyGetFontOrientationFunc;
  FMonochromeColor := clTAColor;
end;

procedure TBasicDrawer.DrawingBegin(const ABoundingBox: TRect);
begin
  Unused(ABoundingBox);
end;

procedure TBasicDrawer.DrawingEnd;
begin
  // Empty
end;

procedure TBasicDrawer.DrawLineDepth(AX1, AY1, AX2, AY2, ADepth: Integer);
begin
  DrawLineDepth(Point(AX1, AY1), Point(AX2, AY2), ADepth);
end;

procedure TBasicDrawer.DrawLineDepth(const AP1, AP2: TPoint; ADepth: Integer);
var
  d: TPoint;
begin
  d := Point(ADepth, -ADepth);
  Polygon([AP1, AP1 + d, AP2 + d, AP2], 0, 4);
end;

function TBasicDrawer.FPColorOrMono(const AColor: TFPColor): TFPColor;
begin
  if FMonochromeColor = clTAColor then
    Result := AColor
  else
    Result := FChartColorToFPColorFunc(FMonochromeColor);
end;

function TBasicDrawer.GetRightToLeft: Boolean;
begin
  Result := FRightToLeft;
end;

function TBasicDrawer.HtmlTextExtent(const AText: String): TPoint;
var
  IDrawer: IChartDrawer;
begin
  IDrawer := Self as IChartDrawer;
//  GetInterface('IChartDrawer', IDrawer);
  with THtmlAnalyzer.Create(IDrawer) do
    try
      Result := TextExtent(AText);
    finally
      Free;
    end;
end;

procedure TBasicDrawer.HtmlTextOut(AX, AY: Integer; const AText: String);
var
  IDrawer: IChartDrawer;
begin
  IDrawer := Self as IChartDrawer;
//  GetInterface('IChartDrawer', IDrawer);
  with THtmlAnalyzer.Create(IDrawer) do
    try
      TextOut(AX, AY, AText);
    finally
      Free;
    end;
end;

procedure TBasicDrawer.LineTo(const AP: TPoint);
begin
  LineTo(AP.X, AP.Y)
end;

procedure TBasicDrawer.MoveTo(const AP: TPoint);
begin
  MoveTo(AP.X, AP.Y)
end;

procedure TBasicDrawer.PutImage(AX, AY: Integer; AImage: TFPCustomImage);
begin
  Unused(AX, AY);
  Unused(AImage);
end;

procedure TBasicDrawer.PutPixel(AX, AY: Integer; AColor: TChartColor);
begin
  Unused(AX, AY);
  Unused(AColor);
end;

function TBasicDrawer.Scale(ADistance: Integer): Integer;
begin
  Result := ADistance;
end;

procedure TBasicDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
begin
  Unused(AValue);
end;

procedure TBasicDrawer.SetDoChartColorToFPColorFunc(
  AValue: TChartColorToFPColorFunc);
begin
  FChartColorToFPColorFunc := AValue;
end;

procedure TBasicDrawer.SetGetFontOrientationFunc(
  AValue: TGetFontOrientationFunc);
begin
  FGetFontOrientationFunc := AValue;
end;

procedure TBasicDrawer.SetMonochromeColor(AColor: TChartColor);
begin
  FMonochromeColor := AColor;
end;

procedure TBasicDrawer.SetRightToLeft(AValue: Boolean);
begin
  FRightToLeft := AValue;
end;

procedure TBasicDrawer.SetTransparency(ATransparency: TChartTransparency);
begin
  FTransparency := ATransparency;
end;

procedure TBasicDrawer.SetXor(AXor: Boolean);
begin
  FXor := AXor;
end;

function TBasicDrawer.TextExtent(const AText: String;
  ATextFormat: TChartTextFormat = tfNormal): TPoint;
var
  sl: TStrings;
begin
  if Pos(LineEnding, AText) = 0 then
    case ATextFormat of
      tfNormal: exit(SimpleTextExtent(AText));
      tfHTML  : exit(HtmlTextExtent(AText));
    end;

  sl := TStringList.Create;
  try
    sl.Text := AText;
    Result := TextExtent(sl, ATextFormat);
  finally
    sl.Free;
  end;
end;

function TBasicDrawer.TextExtent(AText: TStrings;
  ATextFormat: TChartTextFormat = tfNormal): TPoint;
var
  i: Integer;
begin
  Result := Size(0, -LINE_INTERVAL);
  case ATextFormat of
    tfNormal:
      for i := 0 to AText.Count - 1 do
        with SimpleTextExtent(AText[i]) do begin
          Result.X := Max(Result.X, X);
          Result.Y += Y + LINE_INTERVAL;
        end;
    tfHtml:
      for i := 0 to AText.Count - 1 do
        with HtmlTextExtent(AText[i]) do begin
          Result.X := Max(Result.X, X);
          Result.Y += Y + LINE_INTERVAL;
        end;
  end;
end;

function TBasicDrawer.TextOut: TChartTextOut;
begin
  Result := TChartTextOut.Create(Self);
end;

end.

