{
  Utility functions utilized by the custom drawn controls
}
unit customdrawnutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, LMessages, LCLProc, Controls, Graphics,
  Types, IntfGraphics, FPImage, Math;

function GetAColor(Color: TColor; Rate: byte): TColor;
function GetUColor(Color: TColor; Rate: byte): TColor;
procedure GradientFill(Clr1, Clr2: TColor; Canvas: TCanvas);
procedure DrawAndroidButton(Canvas: TCanvas; Color: TColor);

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..32767] of TRGBTriple;

implementation

function ResetCDColor(r, g, b: byte): TColor;
begin
  if r <= 0 then
    r := 1;
  if g <= 0 then
    g := 1;
  if b <= 0 then
    b := 1;
  Result := RGB(r, g, b);
end;

function GetAColor(Color: TColor; Rate: byte): TColor;
var
  r, g, b: byte;
begin
  r := GetRValue(ColorToRGB(Color));
  g := GetGValue(ColorToRGB(Color));
  b := GetBValue(ColorToRGB(Color));
  r := r * Rate div 100;
  g := g * Rate div 100;
  b := b * Rate div 100;
  Result := ResetCDColor(r, g, b);
end;

function GetUColor(Color: TColor; Rate: byte): TColor;
var
  r, g, b: byte;
begin
  r := GetRValue(ColorToRGB(Color));
  g := GetGValue(ColorToRGB(Color));
  b := GetBValue(ColorToRGB(Color));
  r := r * 100 div Rate;
  g := g * 100 div Rate;
  b := b * 100 div Rate;
  Result := ResetCDColor(r, g, b);
end;

procedure GradientFill(Clr1, Clr2: TColor; Canvas: TCanvas);
var
  RGBFrom: array[0..2] of byte;
  RGBDiff: array[0..2] of integer;
  ColorBand: TRect;
  I: integer;
  R, G, B: byte;
begin
  RGBFrom[0] := GetRValue(ColorToRGB(Clr1));
  RGBFrom[1] := GetGValue(ColorToRGB(Clr1));
  RGBFrom[2] := GetBValue(ColorToRGB(Clr1));
  RGBDiff[0] := GetRValue(ColorToRGB(Clr2)) - RGBFrom[0];
  RGBDiff[1] := GetGValue(ColorToRGB(Clr2)) - RGBFrom[1];
  RGBDiff[2] := GetBValue(ColorToRGB(Clr2)) - RGBFrom[2];
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode := pmCopy;
  ColorBand.Left := 0;
  ColorBand.Right := Canvas.Width;
  for I := 0 to $ff do
  begin
    ColorBand.Top := MulDiv(I, Canvas.Height, $100);
    ColorBand.Bottom := MulDiv(I + 1, Canvas.Height, $100);
    R := RGBFrom[0] + MulDiv(I, RGBDiff[0], $ff);
    G := RGBFrom[1] + MulDiv(I, RGBDiff[1], $ff);
    B := RGBFrom[2] + MulDiv(I, RGBDiff[2], $ff);
    Canvas.Brush.Color := RGB(R, G, B);
    Canvas.FillRect(ColorBand);
  end;
end;

function GetNomalColor(a: byte): byte;
begin
  Result := a;
  if a < 1 then
    a := 1;
  if a > 255 then
    a := 255;
end;

procedure DrawAndroidButton(Canvas: TCanvas; Color: TColor);
const
  vedge = 12;
  rr = 3;
var
  i, xx, yy: integer;
  c2: TColor;
  r, g, b, r1, g1, b1: byte;
begin
  //Canvas.Brush.Color := clWhite;
  //Canvas.FillRect(0, 0, Canvas.Width, Canvas.Height);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  r1 := GetRValue(Color);
  g1 := GetGValue(Color);
  b1 := GetBValue(Color);
  for yy := 0 to Canvas.Height do
  begin
 {   if yy < vedge then
    begin
      r := GetNomalColor(r1 - (vedge - yy) * rr);
      g := GetNomalColor(g1 - (vedge - yy) * rr);
      b := GetNomalColor(b1 - (vedge - yy) * rr);
      c2 := RGB(r, g, b);
      Canvas.Pen.Color := c2;
    end
    else       }
    if yy > Canvas.Height - vedge then
    begin
      r := GetNomalColor(r1 - (yy - Canvas.Height + vedge) * rr);
      g := GetNomalColor(g1 - (yy - Canvas.Height + vedge) * rr);
      b := GetNomalColor(b1 - (yy - Canvas.Height + vedge) * rr);
      c2 := RGB(r, g, b);
      Canvas.Pen.Color := c2;
    end
    else
      Canvas.Pen.Color := Color;

    if yy < 4 then
      Canvas.Line(4 - yy, yy, Canvas.Width - 4 + yy, yy)
    else
    if yy > Canvas.Height - 5 then
      Canvas.Line(4 - Canvas.Height + yy, yy, Canvas.Width - yy - 4 + Canvas.Height, yy)
    else
      Canvas.Line(0, yy, Canvas.Width, yy);
  end;
end;

//DrawGradient functions

procedure GradHorizontal(Canvas: TCanvas; Rect: TRect; FromColor, ToColor: TColor);
var
  X: integer;
  dr, dg, db: extended;
  C1, C2: TColor;
  r1, r2, g1, g2, b1, b2: byte;
  R, G, B: byte;
  cnt: integer;
begin
  C1 := FromColor;
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  C2 := ToColor;
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  dr := (R2 - R1) / Rect.Right - Rect.Left;
  dg := (G2 - G1) / Rect.Right - Rect.Left;
  db := (B2 - B1) / Rect.Right - Rect.Left;

  cnt := 0;
  for X := Rect.Left to Rect.Right - 1 do
  begin
    R := R1 + Ceil(dr * cnt);
    G := G1 + Ceil(dg * cnt);
    B := B1 + Ceil(db * cnt);

    Canvas.Pen.Color := RGB(R, G, B);
    Canvas.MoveTo(X, Rect.Top);
    Canvas.LineTo(X, Rect.Bottom);
    Inc(cnt);
  end;
end;

procedure GradVertical(Canvas: TCanvas; Rect: TRect; FromColor, ToColor: TColor);
var
  Y: integer;
  dr, dg, db: extended;
  C1, C2: TColor;
  r1, r2, g1, g2, b1, b2: byte;
  R, G, B: byte;
  cnt: integer;
begin
  C1 := FromColor;
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);

  C2 := ToColor;
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);

  dr := (R2 - R1) / Rect.Bottom - Rect.Top;
  dg := (G2 - G1) / Rect.Bottom - Rect.Top;
  db := (B2 - B1) / Rect.Bottom - Rect.Top;

  cnt := 0;
  for Y := Rect.Top to Rect.Bottom - 1 do
  begin
    R := R1 + Ceil(dr * cnt);
    G := G1 + Ceil(dg * cnt);
    B := B1 + Ceil(db * cnt);

    Canvas.Pen.Color := RGB(R, G, B);
    Canvas.MoveTo(Rect.Left, Y);
    Canvas.LineTo(Rect.Right, Y);
    Inc(cnt);
  end;
end;

procedure FastAntiAliasPicture(orig_bmp, dest_bmp: TBitmap);
var
  x, y, cx, cy: integer;
  totr, totg, totb: integer;
  Row1, Row2, Row3, DestRow: PRGBTripleArray;
  i: integer;
  IntfImg1, IntfImg2: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  FadeStep: integer;
  px, py: integer;
  CurColor: TFPColor;
  TmpBmp: TBitmap;
begin
  if (orig_bmp = nil) or (dest_bmp = nil) then
    Exit;
  TmpBmp:=TBitmap.Create;
  TmpBmp.PixelFormat := pf24bit;
  IntfImg1 := TLazIntfImage.Create(0, 0);
  IntfImg1.LoadFromBitmap(orig_bmp.Handle, orig_bmp.MaskHandle);
  IntfImg2 := TLazIntfImage.Create(0, 0);
  IntfImg2.LoadFromBitmap(dest_bmp.Handle, dest_bmp.MaskHandle);
  // For each row
  for y := 0 to dest_bmp.Height - 1 do
  begin
    // We compute samples of 3 x 3 pixels
    cy := y * 3;
    // Get pointers to actual, previous and next rows in supersampled bitmap
    Row1 := IntfImg1.GetDataLineStart(cy);
    Row2 := IntfImg1.GetDataLineStart(cy + 1);
    Row3 := IntfImg1.GetDataLineStart(cy + 2);
    // Get a pointer to destination row in output bitmap
    DestRow := IntfImg2.GetDataLineStart(y);
    // For each column...
    for x := 0 to dest_bmp.Width - 1 do
    begin
      // We compute samples of 3 x 3 pixels
      cx := 3 * x;
      // Initialize result color
      totr := 0;
      totg := 0;
      totb := 0;
      // For each pixel in sample
      for i := 0 to 2 do
      begin
        // New red value
        totr := totr + Row1^[cx + i].rgbtRed +
          Row2^[cx + i].rgbtRed + Row3^[cx + i].rgbtRed;
        // New green value
        totg := totg + Row1^[cx + i].rgbtGreen +
          Row2^[cx + i].rgbtGreen + Row3^[cx + i].rgbtGreen;
        // New blue value
        totb := totb + Row1^[cx + i].rgbtBlue +
          Row2^[cx + i].rgbtBlue + Row3^[cx + i].rgbtBlue;
      end;
      // Set output pixel colors
      DestRow^[x].rgbtRed := totr div 9;
      DestRow^[x].rgbtGreen := totg div 9;
      DestRow^[x].rgbtBlue := totb div 9;
    end;
  end;
  IntfImg2.CreateBitmaps(ImgHandle, ImgMaskHandle, False);
  TmpBmp.Handle := ImgHandle;
  TmpBmp.MaskHandle := ImgMaskHandle;
  dest_bmp.Empty;
  dest_bmp.Assign(TmpBmp);
  IntfImg1.Free;
  IntfImg2.Free;
  TmpBmp.Free;
end;

end.

