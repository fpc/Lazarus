{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TADrawerCanvas;

{$H+}

interface

uses
  Classes, FPCanvas, FPImage, Graphics, SysUtils, TAChartUtils, TADrawUtils;

type
  IChartTCanvasDrawer = interface
  ['{6D8E5591-6788-4D2D-9FE6-596D5157C3C2}']
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  { TCanvasDrawer }

  TCanvasDrawer = class(
    TBasicDrawer, IChartDrawer, IChartTCanvasDrawer)
  strict private
    procedure SetBrush(ABrush: TFPCustomBrush);
    procedure SetFont(AFont: TFPCustomFont);
    procedure SetPen(APen: TFPCustomPen);
  strict protected
    FCanvas: TCanvas;
    FBuffer: TBitmap;
//    function GetFontAngle: Double; override;
    function SimpleTextExtent(const AText: String): TPoint; override;
    procedure SimpleTextOut(AX, AY: Integer; const AText: String); override;
  public
    procedure AddToFontOrientation(ADelta: Integer);
    procedure ClippingStart;
    procedure ClippingStart(const AClipRect: TRect);
    procedure ClippingStop;
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;
    procedure Ellipse(AX1, AY1, AX2, AY2: Integer);
    procedure FillRect(AX1, AY1, AX2, AY2: Integer);
    function GetBrushColor: TChartColor;
    function GetCanvas: TCanvas; virtual;
    function GetFontAngle: Double; override;
    function GetFontColor: TFPColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TChartFontStyles; override;
    procedure Line(AX1, AY1, AX2, AY2: Integer);
    procedure Line(const AP1, AP2: TPoint);
    procedure LineTo(AX, AY: Integer); override;
    procedure MoveTo(AX, AY: Integer); override;
    procedure Polygon(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer); override;
    procedure Polyline(
      const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
    procedure PrepareSimplePen(AColor: TChartColor);
    procedure PutImage(AX, AY: Integer; AImage: TFPCustomImage); override;
    procedure PutPixel(AX, AY: Integer; AColor: TChartColor); override;
    procedure RadialPie(
      AX1, AY1, AX2, AY2: Integer;
      AStartAngle16Deg, AAngleLength16Deg: Integer);
    procedure Rectangle(const ARect: TRect);
    procedure Rectangle(AX1, AY1, AX2, AY2: Integer);
    procedure ResetFont;
    procedure SetAntialiasingMode(AValue: TChartAntialiasingMode);
    procedure SetBrushColor(AColor: TChartColor);
    procedure SetBrushParams(AStyle: TFPBrushStyle; AColor: TChartColor);
    procedure SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
    procedure SetTransparency(ATransparency: TChartTransparency);
  end;

  TScaledCanvasDrawer = class(TCanvasDrawer)
  protected
    FCoeff: Double;
  public
    constructor Create(ACanvas: TCanvas; ACoeff: Double; AScaleItems: TScaleItems);
    function Scale(ADistance: Integer): Integer; override;
  end;

  function CanvasGetFontOrientationFunc(AFont: TFPCustomFont): Integer;
  function ChartColorSysToFPColor(AChartColor: TChartColor): TFPColor;


implementation

uses
  GraphType, Math, LCLIntf, LCLType, IntfGraphics,
  TAGeometry;

function CanvasGetFontOrientationFunc(AFont: TFPCustomFont): Integer;
begin
  if AFont is TFont then
    Result := (AFont as TFont).Orientation
  else
    Result := AFont.Orientation; //0;
end;

function ChartColorSysToFPColor(AChartColor: TChartColor): TFPColor;
begin
  Result := ChartColorToFPColor(ColorToRGB(AChartColor));
end;

{ TCanvasDrawer }

procedure TCanvasDrawer.AddToFontOrientation(ADelta: Integer);
begin
  with GetCanvas.Font do
    Orientation := Orientation + ADelta;
end;

procedure TCanvasDrawer.ClippingStart(const AClipRect: TRect);
begin
  FCanvas.ClipRect := AClipRect;
  FBuffer.Canvas.ClipRect := AClipRect;
  ClippingStart;
end;

procedure TCanvasDrawer.ClippingStart;
begin
  FCanvas.Clipping := true;
  FBuffer.Canvas.Clipping := true;
end;

procedure TCanvasDrawer.ClippingStop;
begin
  FCanvas.Clipping := false;
  FBuffer.Canvas.Clipping := false;
end;

constructor TCanvasDrawer.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
end;

destructor TCanvasDrawer.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TCanvasDrawer.Ellipse(AX1, AY1, AX2, AY2: Integer);
begin
  GetCanvas.Ellipse(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.FillRect(AX1, AY1, AX2, AY2: Integer);
begin
  GetCanvas.FillRect(AX1, AY1, AX2, AY2);
end;

function TCanvasDrawer.GetBrushColor: TChartColor;
begin
  Result := GetCanvas.Brush.Color;
end;

function TCanvasDrawer.GetCanvas: TCanvas;
begin
  // When transparency is off, draw directly on canvas for better speed.
  if FTransparency > 0 then
    Result := FBuffer.Canvas
  else
    Result := FCanvas;
end;

function TCanvasDrawer.GetFontAngle: Double;
begin
  Result := OrientToRad(GetCanvas.Font.Orientation);
end;

function TCanvasDrawer.GetFontColor: TFPColor;
begin
  Result := TColorToFPColor(GetCanvas.Font.Color);
end;

function TCanvasDrawer.GetFontName: String;
begin
  Result := GetCanvas.Font.Name;
end;

function TCanvasDrawer.GetFontSize: Integer;
var
  h: Integer;
begin
  Result := GetCanvas.Font.Size;
  if Result = 0 then begin
    h := GetFontData(GetCanvas.Font.Reference.Handle).Height;
    Result := round(abs(h) * 72 / ScreenInfo.PixelsPerInchY);
  end;
end;

function TCanvasDrawer.GetFontStyle: TChartFontStyles;
begin
  Result := TChartFontStyles(GetCanvas.Font.Style);
end;

procedure TCanvasDrawer.Line(AX1, AY1, AX2, AY2: Integer);
begin
  GetCanvas.Line(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.Line(const AP1, AP2: TPoint);
begin
  GetCanvas.Line(AP1, AP2);
end;

procedure TCanvasDrawer.LineTo(AX, AY: Integer);
begin
  GetCanvas.LineTo(AX, AY);
end;

procedure TCanvasDrawer.MoveTo(AX, AY: Integer);
begin
  GetCanvas.MoveTo(AX, AY);
end;

procedure TCanvasDrawer.Polygon(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  GetCanvas.Polygon(APoints, false, AStartIndex, ANumPts);
end;

procedure TCanvasDrawer.Polyline(
  const APoints: array of TPoint; AStartIndex, ANumPts: Integer);
begin
  if ANumPts <= 0 then exit;
  GetCanvas.Polyline(APoints, AStartIndex, ANumPts);
  // TCanvas.Polyline does not draw the end point.
  with APoints[AStartIndex + ANumPts - 1] do
    GetCanvas.Pixels[X, Y] := GetCanvas.Pen.Color;
end;

procedure TCanvasDrawer.PrepareSimplePen(AColor: TChartColor);
begin
  with GetCanvas.Pen do begin
    if FXor then
      Color := clWhite
    else
      Color := ColorOrMono(AColor);
    Style := psSolid;
    if FXor then
      Mode := pmXor
    else
      Mode := pmCopy;
    if (scalePen in FScaleItems) then
      Width := Scale(1) else
      Width := 1;
  end;
end;

procedure TCanvasDrawer.PutImage(AX, AY: Integer; AImage: TFPCustomImage);
var
  x, y: Integer;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    if AImage is TLazIntfImage then
      bmp.LoadFromIntfImage(TLazIntfImage(AImage))
    else begin
      bmp.SetSize(AImage.Width, AImage.Height);
      bmp.Transparent := true;
      bmp.TransparentMode := tmFixed;
      bmp.TransparentColor := bmp.Canvas.Pixels[0, 0];
      for y := 0 to AImage.Height - 1 do
        for x := 0 to AImage.Width - 1 do
          if AImage[x, y].alpha > 0 then
            bmp.Canvas.Colors[x, y] := AImage[x, y];
    end;
    GetCanvas.Draw(AX, AY, bmp);
  finally
    bmp.Free;
  end;
end;

procedure TCanvasDrawer.PutPixel(AX, AY: Integer; AColor: TChartColor);
begin
  GetCanvas.Pixels[AX, AY] := AColor;
end;

procedure TCanvasDrawer.RadialPie(
  AX1, AY1, AX2, AY2: Integer;
  AStartAngle16Deg, AAngleLength16Deg: Integer);
begin
  GetCanvas.RadialPie(
    AX1, AY1, AX2, AY2, AStartAngle16Deg, AAngleLength16Deg);
end;

procedure TCanvasDrawer.Rectangle(AX1, AY1, AX2, AY2: Integer);
begin
  GetCanvas.Rectangle(AX1, AY1, AX2, AY2);
end;

procedure TCanvasDrawer.Rectangle(const ARect: TRect);
begin
  GetCanvas.Rectangle(ARect);
end;

procedure TCanvasDrawer.ResetFont;
begin
  GetCanvas.Font.Orientation := 0;
end;

procedure TCanvasDrawer.SetAntialiasingMode(AValue: TChartAntialiasingMode);
begin
  GetCanvas.AntialiasingMode := TAntialiasingMode(AValue);
end;

procedure TCanvasDrawer.SetBrush(ABrush: TFPCustomBrush);
begin
  with GetCanvas.Brush do begin
    if ABrush is TBrush then
      Assign(ABrush)
    else begin
      FPColor := ABrush.FPColor;
      Pattern := ABrush.Pattern;
      Style := ABrush.Style;
    end;
    if FXor then
      Style := bsClear
    else if FMonochromeColor <> clTAColor then
      Color := FMonochromeColor;
  end;
end;

procedure TCanvasDrawer.SetBrushColor(AColor: TChartColor);
begin
  GetCanvas.Brush.Color := ColorOrMono(AColor);
end;

procedure TCanvasDrawer.SetBrushParams(
  AStyle: TFPBrushStyle; AColor: TChartColor);
begin
  GetCanvas.Brush.Color := ColorOrMono(AColor);
  GetCanvas.Brush.Style := AStyle;
end;

procedure TCanvasDrawer.SetFont(AFont: TFPCustomFont);
var
  st: TFontStyles = [];
begin
  with GetCanvas.Font do begin
    if AFont is TFont then
      Assign(AFont)
    else begin
      BeginUpdate;
      FPColor := AFont.FPColor;
      Name := AFont.Name;
      Size := AFont.Size;
      Orientation := AFont.Orientation;
      if AFont.Italic then
        Include(st, fsItalic);
      if AFont.Bold then
        Include(st, fsBold);
      if AFont.Underline then
        Include(st, fsUnderline);
      {$IF (FPC_FULLVERSION<=20600) or (FPC_FULLVERSION=20602)}
      if AFont.StrikeTrough then
      {$ELSE}
      if AFont.StrikeThrough then
      {$ENDIF}
        Include(st, fsStrikeOut);
      Style := st;
      EndUpdate;
    end;
    if FMonochromeColor <> clTAColor then
      Color := FMonochromeColor;
    if scaleFont in FScaleItems then
      Size := Scale(GetFontSize)
    else
      Size := GetFontSize;
  end;
end;

procedure TCanvasDrawer.SetPen(APen: TFPCustomPen);
begin
  with GetCanvas do begin
    if FXor then begin
      Brush.Style := bsClear;
      if APen = nil then
        Pen.Style := psSolid
      else
        Pen.Style := APen.Style;
      Pen.Mode := pmXor;
      Pen.Color := clWhite;
      if APen = nil then
        Pen.Width := 1
      else
        Pen.Width := APen.Width;
    end
    else begin
      if APen is TPen then
        Pen.Assign(APen)
      else  begin
        Pen.Color := FPColorToChartColor(APen.FPColor);
        Pen.Style := APen.Style;
        Pen.Width := APen.Width;
        Pen.Mode := APen.Mode;
        Pen.Pattern := APen.Pattern;
      end;
      if FMonochromeColor <> clTAColor then
        Pen.Color := FMonochromeColor;
    end;
    if scalePen in FScaleItems then
      Pen.Width := Scale(Pen.Width);
  end;
end;

procedure TCanvasDrawer.SetPenParams(AStyle: TFPPenStyle; AColor: TChartColor);
begin
  GetCanvas.Pen.Style := AStyle;
  if not FXor then
    GetCanvas.Pen.Color := ColorOrMono(AColor);
end;

procedure TCanvasDrawer.SetTransparency(ATransparency: TChartTransparency);

  function FillAlpha(AAlpha: Byte): Byte;
  var
    img: TRawImage;
    p, pEnd: PCardinal;
    x: Cardinal = 0;
    r: Cardinal = 0;
  begin
    FBuffer.BeginUpdate;
    img := FBuffer.RawImage;
    p := PCardinal(img.Data);
    TRGBAQuad(x).Alpha := AAlpha;
    pEnd := PCardinal(img.Data + img.DataSize);
    // This loop is time-critical, so: avoid conditionals inside,
    // use dword-sized instead of byte-sized access.
    while p < pEnd do begin
      // On the first pass, set all alpha values to AAlpha.
      // Drawing will reset alpha of changed pixels to zero.
      // On the second pass, flip unchanged pixels back to zero alpha,
      // and changed ones to the desired alpha level.
      p^ := p^ xor x;
      r := r or p^;
      Inc(p);
    end;
    FBuffer.EndUpdate;
    Result := TRGBAQuad(r).Alpha;
  end;

begin
  if FTransparency = ATransparency then exit;
  // For each transparency change, create a buffer bitmap, draw on that,
  // then alpha-blend the bitmap to the canvas.
  // This is slow, but currently seems the only way.
  if FTransparency > 0 then begin
    // StretchMaskBlt performs alpha blending only if the image contains
    // at least one non-zero alpha value, so fully transparent image
    // becomes black box. Workround: do not call StretchMaskBlt in this case.
    if FillAlpha(255 - FTransparency) > 0 then
      StretchMaskBlt(
        FCanvas.Handle, 0, 0, FCanvas.Width, FCanvas.Height,
        FBuffer.Canvas.Handle, 0, 0, FCanvas.Width, FCanvas.Height,
        0, 0, 0, SRCCOPY);
  end;
  inherited;
  if FTransparency > 0 then begin
    FBuffer.SetSize(0, 0);
    FBuffer.SetSize(FCanvas.Width, FCanvas.Height);
    FillAlpha(255 - FTransparency);
  end;
end;

function TCanvasDrawer.SimpleTextExtent(const AText: String): TPoint;
begin
  Result := GetCanvas.TextExtent(AText);
end;

procedure TCanvasDrawer.SimpleTextOut(AX, AY: Integer; const AText: String);

  procedure DrawSimpleText(ACanvas: TCanvas; x, y: Integer; const txt: String);
  // add right-to-left mode. Cannot use TextOut since it does not respect TextStyle
  var
    r: TRect;
    ts: TTextStyle;
  begin
    ts := ACanvas.TextStyle;
    ts.RightToLeft := FRightToLeft;
    ts.WordBreak := false;   // added to disable erroneous workbreaks in Linux printing
    ts.Clipping := false;
    r := Bounds(x, y, 1, 1);
    ACanvas.TextRect(r, x, y, txt, ts);
  end;

  procedure DrawXorText;
  var
    bmp: TBitmap;
    p, ext, bmpSize: TPoint;
    a: Double;
  begin
    ext := GetCanvas.TextExtent(AText);
    a := OrientToRad(GetCanvas.Font.Orientation);
    bmpSize := MeasureRotatedRect(ext, a);
    p := bmpSize div 2 - RotatePoint(ext div 2, -a);

    bmp := TBitmap.Create;
    try
      bmp.SetSize(bmpSize.X, bmpSize.Y);
      bmp.Canvas.Brush.Style := bsClear;
      bmp.Canvas.Font := GetCanvas.Font;
      bmp.Canvas.Font.Color := clWhite;
      DrawSimpleText(bmp.Canvas, p.X, p.Y, AText);
      bmp.Canvas.Pen.Color := clWhite;
      BitBlt(
        GetCanvas.Handle, AX - p.X, AY - p.Y, bmpSize.X, bmpSize.Y,
        bmp.Canvas.Handle, 0, 0, SRCINVERT);
    finally
      bmp.Free;
    end;
  end;

begin
  if FXor then
    DrawXorText
  else
    DrawSimpleText(GetCanvas, AX, AY, AText);
end;


{ TScaledCanvasDrawer }

constructor TScaledCanvasDrawer.Create(ACanvas: TCanvas; ACoeff: Double;
  AScaleItems: TScaleItems);
begin
  inherited Create(ACanvas);
  FCoeff := ACoeff;
  FScaleItems := AScaleItems;
end;

function TScaledCanvasDrawer.Scale(ADistance: Integer): Integer;
begin
  Result := Round(FCoeff * ADistance);
end;

initialization
  // Suppress incorrect "TAGeometry is unused" hint
  Unused(DoublePoint(0, 0));

end.

