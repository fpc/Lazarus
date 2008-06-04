{ $Id$ }
{
 /***************************************************************************
                                graphtype.pp
                                ------------
                          Graphic utility functions.

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit GraphUtil;

{$mode objfpc}{$H+}

interface

uses
  Types, Graphics, Math, LCLType;

function ColorToGray(const AColor: TColor): Byte;
procedure ColorToHLS(const AColor: TColor; out H, L, S: Byte);
procedure RGBtoHLS(const R, G, B: Byte; out H, L, S: Byte);
function HLStoColor(const H, L, S: Byte): TColor;
procedure HLStoRGB(const H, L, S: Byte; out R, G, B: Byte);

// specific things:

{
  Draw gradient from top to bottom with parabolic color grow
}
procedure DrawVerticalGradient(Canvas: TCanvas; ARect: TRect; TopColor, BottomColor: TColor);

{
 Draw nice looking window with Title
}
procedure DrawGradientWindow(Canvas: TCanvas; WindowRect: TRect; TitleHeight: Integer; BaseColor: TColor);


{
 Draw arrows
}
type TScrollDirection=(sdLeft,sdRight,sdUp,sdDown);
     TArrowType = (atSolid, atArrows);
const NiceArrowAngle=45*pi/180;

procedure DrawArrow(Canvas:TCanvas;Direction:TScrollDirection; Location: TPoint; Size: Longint; ArrowType: TArrowType=atSolid);
procedure DrawArrow(Canvas:TCanvas;p1,p2: TPoint; ArrowType: TArrowType=atSolid);
procedure DrawArrow(Canvas:TCanvas;p1,p2: TPoint; ArrowLen: longint; ArrowAngleRad: float=NiceArrowAngle; ArrowType: TArrowType=atSolid);


// delphi compatibility
procedure ColorRGBToHLS(clrRGB: COLORREF; var Hue, Luminance, Saturation: Word);
function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;
function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;
function GetHighLightColor(const Color: TColor; Luminance: Integer = 19): TColor;
function GetShadowColor(const Color: TColor; Luminance: Integer = -50): TColor;

implementation

//TODO: Check code on endianess

procedure ExtractRGB(RGB: Cardinal; var R, G, B: Byte); inline;
begin
  R := RGB and $FF;
  G := (RGB shr 8) and $FF;
  B := (RGB shr 16) and $FF;
end;

function ColorToGray(const AColor: TColor): Byte;
var
  RGB: LongInt;
begin
  if AColor = clNone
  then RGB := 0
  else RGB := ColorToRGB(AColor);
  Result := Trunc(0.222 * (RGB and $FF) + 0.707 * ((RGB shr 8) and $FF) + 0.071 * (RGB shr 16 and $FF));
end;

procedure ColorToHLS(const AColor: TColor; out H, L, S: Byte);
var
  R, G, B: Byte;
  RGB: Cardinal;
begin
  RGB := ColorToRGB(AColor);
  ExtractRGB(RGB, R, G, B);

  RGBtoHLS(R, G, B, H, L, S);
end;

function HLStoColor(const H, L, S: Byte): TColor;
var
  R, G, B: Byte;
begin
  HLStoRGB(H, L, S, R, G, B);
  Result := R or (G shl 8) or (B shl 16);
end;

const
  HUE_000 = 0;
  HUE_060 = 43;
  HUE_120 = 85;
  HUE_180 = 128;
  HUE_240 = 170;
  HUE_300 = 213;

procedure RGBtoHLS(const R, G, B: Byte; out H, L, S: Byte);
var
  cMax, cMin: Byte;             // max and min RGB values
  Rdelta, Gdelta, Bdelta: Byte; // intermediate value: % of spread from max
  diff: Byte;
begin
  // calculate lightness
  cMax := MaxIntValue([R, G, B]);
  cMin := MinIntValue([R, G, B]);
  L := (cMax + cMin + 1) div 2;
  diff := cMax - cMin;

  if diff = 0
  then begin
    // r=g=b --> achromatic case
    S := 0;
    H := 0;
  end
  else begin
    // chromatic case
    // saturation
    if L <= 128
    then S := (diff * 255) div (cMax + cMin)
    else S := (diff * 255) div (510 - cMax - cMin);

    // hue
    Rdelta := (cMax - R);
    Gdelta := (cMax - G);
    Bdelta := (cMax - B);

    if R = cMax
    then H := HUE_000 + ((Bdelta - Gdelta) * HUE_060) div diff
    else if G = cMax
    then H := HUE_120 + ((Rdelta - Bdelta) * HUE_060) div diff
    else H := HUE_240 + ((Gdelta - Rdelta) * HUE_060) div diff;
  end;
end;


procedure HLStoRGB(const H, L, S: Byte; out R, G, B: Byte);

  // utility routine for HLStoRGB
  function HueToRGB(const n1, n2: Byte; Hue: Integer): Byte;
  begin
    if Hue > 255
    then Dec(Hue, 255)
    else if Hue < 0
    then Inc(Hue, 255);
     
    // return r,g, or b value from this tridrant
    case Hue of
      HUE_000..HUE_060 - 1: Result := n1 + (n2 - n1) * Hue div HUE_060;
      HUE_060..HUE_180 - 1: Result := n2;
      HUE_180..HUE_240 - 1: Result := n1 + (n2 - n1) * (HUE_240 - Hue) div HUE_060;
    else
      Result := n1;
    end;
  end;

var
  n1, n2: Byte;
begin
  if S = 0
  then begin
    // achromatic case
    R := L;
    G := L;
    B := L;
  end
  else begin
    // chromatic case
    // set up magic numbers
    if L < 128
    then begin
      n2 := L + (L * S) div 255;
      n1 := 2 * L - n2;
    end
    else begin
      n2 := S + L - (L * S) div 255;
      n1 := 2 * L - n2 - 1;
    end;


    // get RGB
    R := HueToRGB(n1, n2, H + HUE_120);
    G := HueToRGB(n1, n2, H);
    B := HueToRGB(n1, n2, H - HUE_120);
  end;
end;




procedure DrawArrow(Canvas: TCanvas; Direction: TScrollDirection;
  Location: TPoint; Size: Longint; ArrowType: TArrowType);
const ScrollDirectionX:array[TScrollDirection]of longint=(-1,+1,0,0);
      ScrollDirectionY:array[TScrollDirection]of longint=(0,0,-1,+1);
begin
  DrawArrow(Canvas,Location,
            point(ScrollDirectionX[Direction]*size+Location.x,ScrollDirectionY[Direction]*size+Location.y),
            max(5,size div 10),
            NiceArrowAngle,ArrowType);
end;

procedure DrawArrow(Canvas: TCanvas; p1, p2: TPoint; ArrowType: TArrowType);
begin
  DrawArrow(Canvas,p1,p2,round(sqrt(sqr(p1.x-p2.x)+sqr(p1.y-p2.y))/10),NiceArrowAngle,ArrowType);
end;

procedure DrawArrow(Canvas: TCanvas; p1, p2: TPoint; ArrowLen: longint;
  ArrowAngleRad: float; ArrowType: TArrowType);
var {NormalizedLineX, NormalizedLineY, LineLen,} LineAngle: float;
    ArrowPoint1, ArrowPoint2: TPoint;
begin
  LineAngle:=arctan2(p2.y-p1.y,p2.x-p1.x);
  ArrowPoint1.x:=round(ArrowLen*cos(pi+LineAngle-ArrowAngleRad))+p2.x;
  ArrowPoint1.y:=round(ArrowLen*sin(pi+LineAngle-ArrowAngleRad))+p2.y;
  ArrowPoint2.x:=round(ArrowLen*cos(pi+LineAngle+ArrowAngleRad))+p2.x;
  ArrowPoint2.y:=round(ArrowLen*sin(pi+LineAngle+ArrowAngleRad))+p2.y;

  Canvas.Line(p1,p2);

  case ArrowType of
    atSolid: begin
      canvas.Polygon([ArrowPoint1,p2,ArrowPoint2]);
    end;
    atArrows: begin
      Canvas.LineTo(ArrowPoint1.x,ArrowPoint1.y);
      Canvas.Line(p2.x,p2.y,ArrowPoint2.x,ArrowPoint2.y);
    end;
  end;
end;

procedure ColorRGBToHLS(clrRGB: COLORREF; var Hue, Luminance, Saturation: Word);
var
  H, L, S: Byte;
begin
  ColorToHLS(clrRGB, H, L, S);
  Hue := H;
  Luminance := L;
  Saturation := S;
end;

function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;
begin
  Result := HLStoColor(Hue, Luminance, Saturation);
end;

function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;
var
  H, L, S: Byte;
begin
  // what is fScale?
  ColorToHLS(clrRGB, H, L, S);
  Result := HLStoColor(H, L + n, S);
end;

function GetHighLightColor(const Color: TColor; Luminance: Integer): TColor;
begin
  Result := ColorAdjustLuma(Color, Luminance, False);
end;

function GetShadowColor(const Color: TColor; Luminance: Integer): TColor;
begin
  Result := ColorAdjustLuma(Color, Luminance, False);
end;

procedure DrawVerticalGradient(Canvas: TCanvas; ARect: TRect; TopColor, BottomColor: TColor);
var
  y, h: Integer;
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  dr, dg, db: integer;

 function GetColor(pos, total: integer): TColor;

   function GetComponent(c1, dc: integer): integer;
   begin
     Result := Round(dc / sqr(total) * sqr(pos) + c1);
   end;

 begin
   Result :=
     GetComponent(r1, dr) or
     (GetComponent(g1, dg) shl 8) or
     (GetComponent(b1, db) shl 16);
 end;

 procedure CalcDeltas;
 begin
   ExtractRGB(TopColor, r1, g1, b1);
   ExtractRGB(BottomColor, r2, g2, b2);
   dr := r2 - r1;
   dg := g2 - g1;
   db := b2 - b1;
 end;

begin
  TopColor := ColorToRGB(TopColor);
  BottomColor := ColorToRGB(BottomColor);
  CalcDeltas;

  h := ARect.Bottom - ARect.Top;
  for y := ARect.Top to ARect.Bottom do
  begin
    Canvas.Pen.Color := GetColor(y - ARect.Top, h);
    Canvas.Line(ARect.Left, y, ARect.Right, y);
  end;
end;

procedure DrawGradientWindow(Canvas: TCanvas; WindowRect: TRect; TitleHeight: Integer; BaseColor: TColor);
begin
  Canvas.Brush.Color := BaseColor;
  Canvas.FrameRect(WindowRect);
  InflateRect(WindowRect, -1, -1);
  WindowRect.Bottom := WindowRect.Top + TitleHeight;
  DrawVerticalGradient(Canvas, WindowRect, GetHighLightColor(BaseColor), GetShadowColor(BaseColor));
end;

end.
