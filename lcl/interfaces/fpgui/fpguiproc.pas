{
 *****************************************************************************
 *                                FpGuiProc.pp                               *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fpguiproc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gfxbase, Graphics;

function TColorToTfpgColor(AColor: TColor): TfpgColor;

implementation

{
  Converts from TColor to TfpgColor

  TfpgColor   = type longword;    // Always in RRGGBB (Alpha, Red, Green, Blue) format!!
}
function TColorToTfpgColor(AColor: TColor): TfpgColor;
var
  RGBColor: TColor;
  RGBTriple: TRGBTriple;
begin
  RGBColor := ColorToRGB(AColor);

  RGBTriple.Alpha := 0;
  RGBTriple.Red := Graphics.Red(RGBColor);
  RGBTriple.Green := Graphics.Green(RGBColor);
  RGBTriple.Blue := Graphics.Blue(RGBColor);

  Result := RGBTripleTofpgColor(RGBTriple);
end;

end.

