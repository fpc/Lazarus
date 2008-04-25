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
  Graphics, Math, LCLType;

function ColorToGray(const AColor: TColor): Byte;
procedure ColorToHLS(const AColor: TColor; var H, L, S: Byte);
procedure RGBtoHLS(const R, G, B: Byte; var H, L, S: Byte);
function HLStoColor(const H, L, S: Byte): TColor;
procedure HLStoRGB(const H, L, S: Byte; var R, G, B: Byte);

// delphi compatibility
procedure ColorRGBToHLS(clrRGB: COLORREF; var Hue, Luminance, Saturation: Word);
function ColorHLSToRGB(Hue, Luminance, Saturation: Word): TColorRef;
function ColorAdjustLuma(clrRGB: TColor; n: Integer; fScale: BOOL): TColor;
function GetHighLightColor(const Color: TColor; Luminance: Integer = 19): TColor;
function GetShadowColor(const Color: TColor; Luminance: Integer = -50): TColor;

implementation

//TODO: Check code on endianess

function ColorToGray(const AColor: TColor): Byte;
var
  RGB: LongInt;
begin
  if AColor = clNone
  then RGB := 0
  else RGB := ColorToRGB(AColor);
  Result := Trunc(0.222 * (RGB and $FF) + 0.707 * ((RGB shr 8) and $FF) + 0.071 * (RGB shr 16 and $FF));
end;

procedure ColorToHLS(const AColor: TColor; var H, L, S: Byte);
var
  R, G, B: Byte;
  RGB: Cardinal;
begin
  RGB := ColorToRGB(AColor);
  R := RGB and $FF;
  G := (RGB shr 8) and $FF;
  B := (RGB shr 16) and $FF;

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

procedure RGBtoHLS(const R, G, B: Byte; var H, L, S: Byte);
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


procedure HLStoRGB(const H, L, S: Byte; var R, G, B: Byte);

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


end.
