{   Unit indcyGraphics from cyGraphics

    Description:
    Unit with graphic functions

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * No contributors for now ...
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
    
unit indcyGraphics;

{$mode objfpc}{$H+}

// {$I cyCompilerDefines.inc}

interface

// We need to put jpeg to the uses for avoid run-time not handled jpeg image ...
uses
  LCLIntf, LCLType, Types, Classes, Forms, Graphics, Math, Buttons, Controls,
  ExtCtrls, SysUtils, indcyTypes;

// Objects painting functions :
procedure cyFrame3D(Canvas: TCanvas; var Rect: TRect; TopLeftColor, BottomRightColor: TColor; Width: Integer;
                    const DrawLeft: Boolean = true; const DrawTop: Boolean = true; const DrawRight: Boolean = true; const DrawBottom: Boolean = true;
                    const RoundRect: boolean = false);

// TPicture and TGraphic functions:
function PictureIsTransparentAtPos(aPicture: TPicture; aPoint: TPoint): boolean;
function IconIsTransparentAtPos(aIcon: TIcon; aPoint: TPoint): boolean;
function ValidGraphic(aGraphic: TGraphic): Boolean;

// Other functions:
function PointInEllipse(const aPt: TPoint; const aRect: TRect): boolean;

implementation

{ Procedures and functions}

procedure cyFrame3D(Canvas: TCanvas; var Rect: TRect; TopLeftColor, BottomRightColor: TColor; Width: Integer;
                    const DrawLeft: Boolean = true; const DrawTop: Boolean = true; const DrawRight: Boolean = true; const DrawBottom: Boolean = true;
                    const RoundRect: boolean = false);
var incValue: Integer;

  procedure DrawLines;
  begin
    with Canvas, Rect do
    begin
      // Draw Left and Top line :
      Pen.Color := TopLeftColor;

      if DrawLeft
      then begin
        MoveTo(Left, Top + incValue);
        LineTo(Left, Bottom);
      end;

      if DrawTop
      then begin
        MoveTo(Left + incValue, Top);
        LineTo(Right, Top);
      end;

      // Draw right and bottom line :
      Pen.Color := BottomRightColor;

      if DrawRight
      then begin
        MoveTo(Right, Top + incValue);
        LineTo(Right, Bottom);
      end;

      if DrawBottom
      then begin
        MoveTo(Right - incValue, Bottom);
        LineTo(Left-1 + incValue, Bottom);
      end;
    end;
  end;

begin
  if RoundRect
  then incValue := 1
  else incValue := 0;

  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);

  while Width > 0 do
  begin
    Dec(Width);
    DrawLines;
    incValue := 0;
    InflateRect(Rect, -1, -1);
  end;

  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

function PointInEllipse(const aPt: TPoint; const aRect: TRect): boolean;
var
  CenterEllipseCoord: TPoint;
  EllipseWidth, EllipseHeight: Integer;
begin
  CenterEllipseCoord := Point((aRect.Right + aRect.Left) div 2, (aRect.Bottom + aRect.Top) div 2);
  EllipseWidth := (aRect.Right - aRect.Left) div 2;
  EllipseHeight := (aRect.Bottom - aRect.Top) div 2;

  RESULT := Sqr((aPt.x - CenterEllipseCoord.x)/EllipseWidth) + Sqr((aPt.y - CenterEllipseCoord.y)/EllipseHeight)
           <= 1;
  //  = 0 On the center of ellipse
  //  < 1 Inside the ellipse
  //  = on the border of ellipse
  //  > 1 Outside the ellipse
end;

function PictureIsTransparentAtPos(aPicture: TPicture; aPoint: TPoint): boolean;
begin
  RESULT := false;       // TJPEGImage and others formats not handled ...
  if aPicture.Graphic = nil then Exit;
  if aPicture.Graphic.Empty then Exit;

  if aPicture.Graphic is TBitmap
  then begin
    RESULT := aPicture.Bitmap.Canvas.Pixels[aPoint.X, aPoint.Y]
                = aPicture.Bitmap.Canvas.Pixels[0, aPicture.Bitmap.Height-1];
  end
  else
    if aPicture.Graphic is TIcon
    then
      RESULT := IconIsTransparentAtPos(aPicture.Icon, aPoint)
end;

// 9999 New function for CodeTyphon
function IconIsTransparentAtPos(aIcon: TIcon; aPoint: TPoint): boolean;
var aPic: TPicture;
begin
  RESULT := false;
  aPic := TPicture.Create;

  try
    aPic.Bitmap.Width := aIcon.Width;
    aPic.Bitmap.Height := aIcon.Height;
    aPic.Bitmap.PixelFormat := pf1bit;  // Black = not transparent
    aPic.Bitmap.Canvas.Brush.Color := clWhite;
    aPic.Bitmap.Canvas.FillRect(Rect(0, 0, aIcon.Width, aIcon.Height));

    aPic.Assign(aIcon);

    aPic.Bitmap.PixelFormat := pf1bit;  // Black = not transparent

    RESULT := aPic.Bitmap.Canvas.Pixels[aPoint.X, aPoint.Y] <> clBlack;

  finally
    aPic.Free;
  end;
end;

function ValidGraphic(aGraphic: TGraphic): Boolean;
begin
  RESULT := false;
  if aGraphic <> Nil
  then
    if not aGraphic.Empty
    then RESULT := true;
end;

end.

