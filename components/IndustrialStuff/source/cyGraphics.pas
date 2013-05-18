{   Unit cyGraphics

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
    
{**********************************************************************
 Package pl_Cindy.pkg
 for CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit cyGraphics;

{$mode objfpc}{$H+}

// {$I cyCompilerDefines.inc}

interface

// We need to put jpeg to the uses for avoid run-time not handled jpeg image ...
uses
  LCLIntf, LCLType, Types, Classes, Forms, Graphics, Math, Buttons, Controls,
  ExtCtrls, SysUtils, cyTypes;

// Background functions :
procedure cyGradientFill(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; adgradOrientation: TdgradOrientation; Balance: Word; balanceMode: TDgradBalanceMode; Maxdegrade: Byte; SpeedPercent: Integer);
procedure cyGradientFillVertical(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: byte);
procedure cyGradientFillHorizontal(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: byte);
procedure cyGradientFillShape(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; toRect: TRect; OrientationShape: TDgradOrientationShape);

// Objects painting functions :
procedure cyFrame3D(Canvas: TCanvas; var Rect: TRect; TopLeftColor, BottomRightColor: TColor; Width: Integer;
                    const DrawLeft: Boolean = true; const DrawTop: Boolean = true; const DrawRight: Boolean = true; const DrawBottom: Boolean = true;
                    const RoundRect: boolean = false);
procedure cyDrawButtonFace(Canvas: TCanvas; var Rect: TRect; GradientColor1, GradientColor2: TColor; aState: TButtonState; Focused, Hot: Boolean);

// TPicture and TGraphic functions:
function PictureIsTransparentAtPos(aPicture: TPicture; aPoint: TPoint): boolean;
function IconIsTransparentAtPos(aIcon: TIcon; aPoint: TPoint): boolean;
// Draw a canvas Rect into another (Windows GDI library - not transparent) :
procedure DrawCanvas(Destination: TCanvas; DestRect: TRect; Source: TCanvas; SourceRect: TRect); overload;
function ValidGraphic(aGraphic: TGraphic): Boolean;
function ColorSetPercentBrightness(Color: TColor; PercentLight: Integer): TColor;
function MediumColor(Color1, Color2: TColor): TColor;

// Other functions:
function CombineRectKeepingCenterPosition(RectPos, AddRect: TRect): TRect;
procedure InflateRectPercent(var aRect: TRect; withPercent: Double);
function GetIntermediateRect(Rect1, Rect2: TRect; Percent: Double): TRect;
function PointInEllispe(const aPt: TPoint; const aRect: TRect): boolean;

implementation

{ Procedures and functions}

procedure cyGradientFill(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor;
  adgradOrientation: TdgradOrientation; Balance: Word; balanceMode: TDgradBalanceMode;
  Maxdegrade: Byte; SpeedPercent: Integer);
var
  IntermediateRect, Eye : TRect;
  fromColorRGB, toColorRGB : Integer;
  MaxDegradeBalance1, MaxDegradeBalance2: byte;
  Arr_StartRGB, Arr_Med1RGB, Arr_Med2RGB, Arr_EndRGB : Array[0..2] of Byte;
  ProgressionRGB: Array[0..2] of SmallInt;
begin
  if (Balance = 50) and (BalanceMode = bmNormal) and (SpeedPercent = 100)
  then begin
    case adgradOrientation of
      dgdVertical, dgdAngle:
        cyGradientFillVertical(aCanvas, aRect, fromColor, toColor, MaxDegrade);

      dgdHorizontal:
        cyGradientFillHorizontal(aCanvas, aRect, fromColor, toColor, MaxDegrade);

//     dgdAngle: ;  To do !!!

      dgdRadial:
      begin
        eye := aRect;
        InflateRectPercent(eye, -1);  // Inflate Rect -100%
        aCanvas.Brush.color := fromColor;
        aCanvas.FillRect(aRect);
        // Retrieve Rect from witch, we will draw the radial :
        aRect := CombineRectKeepingCenterPosition(Eye, aRect);
        cyGradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegrade, Eye, osRadial);
      end;

      dgdRectangle:
      begin
        eye := aRect;
        InflateRectPercent(eye, -1);  // Inflate Rect -100%
        aCanvas.Brush.color := fromColor;
        aCanvas.FillRect(aRect);
        cyGradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegrade, eye, osRectangle);
      end;
    end;
  end
  else begin       // Execute 2 Fills because of balance :
    MaxDegradeBalance1 := MulDiv(MaxDegrade, Balance, 100);
    MaxDegradeBalance2 := MaxDegrade - MaxDegradeBalance1;

    if BalanceMode <> bmMirror
    then begin
      // Get Start and end color:
      fromColorRGB := ColorToRGB(fromColor);
      toColorRGB   := ColorToRGB(toColor);

      Arr_StartRGB[0] := GetRValue(fromColorRGB);
      Arr_StartRGB[1] := GetGValue(fromColorRGB);
      Arr_StartRGB[2] := GetBValue(fromColorRGB);

      Arr_EndRGB[0] := GetRValue(toColorRGB);
      Arr_EndRGB[1] := GetGValue(toColorRGB);
      Arr_EndRGB[2] := GetBValue(toColorRGB);

      // Calc the 2 intermediate colors :
      ProgressionRGB[0] := MulDiv(Arr_EndRGB[0] - Arr_StartRGB[0], SpeedPercent - 50, 100);
      ProgressionRGB[1] := MulDiv(Arr_EndRGB[1] - Arr_StartRGB[1], SpeedPercent - 50, 100);
      ProgressionRGB[2] := MulDiv(Arr_EndRGB[2] - Arr_StartRGB[2], SpeedPercent - 50, 100);

      Arr_Med1RGB[0] := Arr_StartRGB[0] + ProgressionRGB[0];
      Arr_Med1RGB[1] := Arr_StartRGB[1] + ProgressionRGB[1];
      Arr_Med1RGB[2] := Arr_StartRGB[2] + ProgressionRGB[2];

      Arr_Med2RGB[0] := Arr_EndRGB[0] - ProgressionRGB[0];
      Arr_Med2RGB[1] := Arr_EndRGB[1] - ProgressionRGB[1];
      Arr_Med2RGB[2] := Arr_EndRGB[2] - ProgressionRGB[2];
    end;
    
    case adgradOrientation of
      dgdVertical, dgdAngle:
      begin
        IntermediateRect := classes.Rect(aRect.Left, aRect.Top, aRect.Right, aRect.Top + MulDiv(Balance, aRect.Bottom - aRect.Top, 100));

        case BalanceMode of
          bmNormal:
          begin
            cyGradientFillVertical(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
            IntermediateRect.Top := IntermediateRect.Bottom;
            IntermediateRect.Bottom := aRect.Bottom;
            cyGradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2);
          end;

          bmMirror:
          begin
            cyGradientFillVertical(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
            IntermediateRect.Top := IntermediateRect.Bottom;
            IntermediateRect.Bottom := aRect.Bottom;
            cyGradientFillVertical(aCanvas, IntermediateRect, toColor, FromColor, MaxDegradeBalance2);
          end;

          bmReverse:
          begin
            cyGradientFillVertical(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
            IntermediateRect.Top := IntermediateRect.Bottom;
            IntermediateRect.Bottom := aRect.Bottom;
            cyGradientFillVertical(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2);
          end;

          bmReverseFromColor:
          begin
            cyGradientFillVertical(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
            IntermediateRect.Top := IntermediateRect.Bottom;
            IntermediateRect.Bottom := aRect.Bottom;
            cyGradientFillVertical(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
          end;

          bmInvertReverse:
          begin
            cyGradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1);
            IntermediateRect.Top := IntermediateRect.Bottom;
            IntermediateRect.Bottom := aRect.Bottom;
            cyGradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
          end;

          bmInvertReverseFromColor:
          begin
            cyGradientFillVertical(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
            IntermediateRect.Top := IntermediateRect.Bottom;
            IntermediateRect.Bottom := aRect.Bottom;
            cyGradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
          end;
        end;
      end;

      dgdHorizontal:
      begin
        IntermediateRect := classes.Rect(aRect.Left, aRect.Top, aRect.Left + MulDiv(Balance, aRect.Right - aRect.Left, 100), aRect.Bottom);

        case BalanceMode of
          bmNormal:
          begin
            cyGradientFillHorizontal(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
            IntermediateRect.Left := IntermediateRect.Right;
            IntermediateRect.Right := aRect.Right;
            cyGradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2);
          end;

          bmMirror:
          begin
            cyGradientFillHorizontal(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
            IntermediateRect.Left := IntermediateRect.Right;
            IntermediateRect.Right := aRect.Right;
            cyGradientFillHorizontal(aCanvas, IntermediateRect, toColor, FromColor, MaxDegradeBalance2);
          end;

          bmReverse:
          begin
            cyGradientFillHorizontal(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
            IntermediateRect.Left := IntermediateRect.Right;
            IntermediateRect.Right := aRect.Right;
            cyGradientFillHorizontal(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2);
          end;

          bmReverseFromColor:
          begin
            cyGradientFillHorizontal(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
            IntermediateRect.Left := IntermediateRect.Right;
            IntermediateRect.Right := aRect.Right;
            cyGradientFillHorizontal(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
          end;

          bmInvertReverse:
          begin
            cyGradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1);
            IntermediateRect.Left := IntermediateRect.Right;
            IntermediateRect.Right := aRect.Right;
            cyGradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
          end;

          bmInvertReverseFromColor:
          begin
            cyGradientFillHorizontal(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
            IntermediateRect.Left := IntermediateRect.Right;
            IntermediateRect.Right := aRect.Right;
            cyGradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
          end;
        end;
      end;

{      dgdAngle:   // To do !!!
      begin

      end;  }

      dgdRadial:
      begin
        eye := aRect;
        InflateRectPercent(eye, -1);  // Inflate Rect -100%

        aCanvas.Brush.color := fromColor;
        aCanvas.FillRect(aRect);

        // Retrieve Rect from witch, we will draw the radial :
        aRect := CombineRectKeepingCenterPosition(Eye, aRect);

        IntermediateRect := GetIntermediateRect(aRect, Eye, Balance/100);
        InflateRectPercent(IntermediateRect,(-1) * Balance div 100);

        case BalanceMode of
          bmNormal:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect, osRadial);
            cyGradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2, Eye, osRadial);
          end;

          bmMirror:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRadial);
            cyGradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRadial);
          end;

          bmReverse:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect, osRadial);
            cyGradientFillShape(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2, Eye, osRadial);
          end;

          bmReverseFromColor:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect, osRadial);
            cyGradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRadial);
          end;

          bmInvertReverse:
          begin
            cyGradientFillShape(aCanvas, aRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1, IntermediateRect, osRadial);
            cyGradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye, osRadial);
          end;

          bmInvertReverseFromColor:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRadial);
            cyGradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye, osRadial);
          end;
        end;
      end;

      dgdRectangle:
      begin
        eye := aRect;
        InflateRectPercent(eye, -1);  // Inflate Rect -100%
        aCanvas.Brush.color := fromColor;
        aCanvas.FillRect(aRect);
        // Retrieve Rect from witch, we will end and begin the rectangle :
        IntermediateRect := GetIntermediateRect(aRect, Eye, Balance/100);
        InflateRectPercent(IntermediateRect, (-1) * Balance div 100);

        case BalanceMode of
          bmNormal:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect, osRectangle);
            cyGradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2, Eye, osRectangle);
          end;

          bmMirror:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRectangle);
            cyGradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRectangle);
          end;

          bmReverse:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect, osRectangle);
            cyGradientFillShape(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2, Eye, osRectangle);
          end;

          bmReverseFromColor:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect, osRectangle);
            cyGradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRectangle);
          end;

          bmInvertReverse:
          begin
            cyGradientFillShape(aCanvas, aRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1, IntermediateRect, osRectangle);
            cyGradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye, osRectangle);
          end;

          bmInvertReverseFromColor:
          begin
            cyGradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRectangle);
            cyGradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye, osRectangle);
          end;
        end;
      end;
    end;  // End case adgradOrientation of ...
  end;
end;

procedure cyGradientFillVertical(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte);
var
  aLimit : TRect;
  fromColorRGB, toColorRGB, i, nbDgrad: integer;
  Arr_StartRGB : Array[0..2] of Byte;
  Arr_DifRGB   : Array[0..2] of integer;
  Arr_CurRGB   : Array[0..2] of Byte;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB   := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_DifRGB[0] := GetRValue(toColorRGB) - Arr_StartRGB[0] ;
  Arr_DifRGB[1] := GetGValue(toColorRGB) - Arr_StartRGB[1] ;
  Arr_DifRGB[2] := GetBValue(toColorRGB) - Arr_StartRGB[2] ;

  if aRect.Bottom - aRect.Top < MaxDegrad
  then nbDgrad := aRect.Bottom - aRect.Top
  else nbDgrad := MaxDegrad;

  with aCanvas do
  begin
    aLimit.Left   := aRect.Left;
    aLimit.Right  := aRect.Right;

    for i:= 1 to nbDgrad do
    begin
      if i = 1
      then aLimit.Top := aRect.Top
      else aLimit.Top := aLimit.Bottom;

      if i = nbDgrad
      then aLimit.Bottom := aRect.Bottom
      else aLimit.Bottom := aRect.Top + MulDiv(i+1, aRect.Bottom - aRect.Top, nbDgrad);

      Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i, Arr_DifRGB[0], nbDgrad);
      Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i, Arr_DifRGB[1], nbDgrad);
      Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i, Arr_DifRGB[2], nbDgrad);

      Brush.color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
      FillRect(aLimit);
    end;
  end;
end;

procedure cyGradientFillHorizontal(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte);
var
  aLimit : TRect;
  fromColorRGB, toColorRGB, i, nbDgrad: integer;
  Arr_StartRGB : Array[0..2] of Byte;
  Arr_DifRGB   : Array[0..2] of integer;
  Arr_CurRGB   : Array[0..2] of Byte;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB   := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_DifRGB[0] := GetRValue(toColorRGB) - Arr_StartRGB[0] ;
  Arr_DifRGB[1] := GetGValue(toColorRGB) - Arr_StartRGB[1] ;
  Arr_DifRGB[2] := GetBValue(toColorRGB) - Arr_StartRGB[2] ;

  if aRect.Right - aRect.Left < MaxDegrad
  then nbDgrad := aRect.Right - aRect.Left
  else nbDgrad := MaxDegrad;

  with aCanvas do
  begin
    aLimit.Top    := aRect.Top;
    aLimit.Bottom := aRect.Bottom;

    for i:= 1 to nbDgrad do
    begin
      if i = 1
      then aLimit.Left := aRect.Left
      else aLimit.Left := aLimit.Right;

      if i = nbDgrad
      then aLimit.Right := aRect.Right
      else aLimit.right  := aRect.Left + MulDiv(i+1, aRect.Right - aRect.Left, nbDgrad);

      Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i, Arr_DifRGB[0], nbDgrad);
      Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i, Arr_DifRGB[1], nbDgrad);
      Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i, Arr_DifRGB[2], nbDgrad);

      Brush.color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
      FillRect(aLimit);
    end;
  end;
end;

procedure cyGradientFillShape(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; toRect: TRect; OrientationShape: TDgradOrientationShape);
var
  aLimit : TRect;
  InflateX, InflateY : Integer;
  fromColorRGB, toColorRGB, DifX, DifY, i, nbDgrad: integer;
  Arr_StartRGB, Arr_EndRGB, Arr_CurRGB : Array[0..2] of Byte;
  Arr_DifRGB   : Array[0..2] of integer;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB   := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_EndRGB[0] := GetRValue(toColorRGB);
  Arr_EndRGB[1] := GetGValue(toColorRGB);
  Arr_EndRGB[2] := GetBValue(toColorRGB);

  Arr_DifRGB[0] := Arr_EndRGB[0] - Arr_StartRGB[0];
  Arr_DifRGB[1] := Arr_EndRGB[1] - Arr_StartRGB[1];
  Arr_DifRGB[2] := Arr_EndRGB[2] - Arr_StartRGB[2];

  DifX := abs((aRect.Right - aRect.Left) - (toRect.Right - toRect.Left));
  DifX := DifX div 2;
  DifY := abs((aRect.Bottom - aRect.Top) - (toRect.Bottom - toRect.Top));
  DifY := DifY div 2;

  if DifX > DifY
  then begin
    if DifX < MaxDegrad
    then nbDgrad := DifX + 1    // Because of DifX := DifX div 2 for small Rects ...
    else nbDgrad := MaxDegrad;
  end
  else begin
    if DifY < MaxDegrad
    then nbDgrad := DifY + 1
    else nbDgrad := MaxDegrad;
  end;

  with aCanvas do
  begin
    for i := 1 to nbDgrad do
    begin
      if i = 1
      then begin
        aLimit := aRect;
        Arr_CurRGB[0] := Arr_StartRGB[0];
        Arr_CurRGB[1] := Arr_StartRGB[1];
        Arr_CurRGB[2] := Arr_StartRGB[2];
      end
      else
        if i = nbDgrad
        then begin
          aLimit := toRect;
          Arr_CurRGB[0] := Arr_EndRGB[0];
          Arr_CurRGB[1] := Arr_EndRGB[1];
          Arr_CurRGB[2] := Arr_EndRGB[2];

          if (aLimit.Right-aLimit.Left = 1) or (aLimit.Bottom-aLimit.Top = 1)
          then OrientationShape := osRectangle;   // Draw 1 pixel or 1 line ...
        end
        else begin
          aLimit := aRect;
          InflateX := (-1) * MulDiv(i-1, DifX, nbDgrad);
          InflateY := (-1) * MulDiv(i-1, DifY, nbDgrad);
          InflateRect(aLimit, InflateX, InflateY);

          Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i-1, Arr_DifRGB[0], nbDgrad);
          Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i-1, Arr_DifRGB[1], nbDgrad);
          Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i-1, Arr_DifRGB[2], nbDgrad);
        end;

      case OrientationShape of
        osRadial:
        begin
          Brush.color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
          Pen.Color := Brush.color;
          Ellipse(aLimit);
        end;

        osRectangle:
        begin
          Brush.color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
          FillRect(aLimit);
        end;
      end;
    end;
  end;
end;

function CombineRectKeepingCenterPosition(RectPos, AddRect: TRect): TRect;
var
  MaxDif, DifLeft, DifTop, DifRight, DifBottom: Integer;
begin
  // Calculate position diference between the 2 rects :
  DifLeft   := abs(RectPos.Left - AddRect.Left);
  DifRight  := abs(RectPos.Right - AddRect.Right);
  DifTop    := abs(RectPos.Top - AddRect.Top);
  DifBottom := abs(RectPos.Bottom - AddRect.Bottom);

  if DifLeft > DifRight
  then MaxDif := DifLeft
  else MaxDif := DifRight;

  if DifTop > MaxDif
  then MaxDif := DifTop;

  if DifBottom > MaxDif
  then MaxDif := DifBottom;

  RESULT := classes.Rect(RectPos.Left - MaxDif, RectPos.Top - MaxDif,
                           RectPos.Right + MaxDif, RectPos.Bottom + MaxDif);
end;

procedure InflateRectPercent(var aRect: TRect; withPercent: Double);
var
  dx, dy: Integer;
begin
  dx := Round((aRect.Right - aRect.Left) * withPercent) div 2;
  dy := Round((aRect.Bottom - aRect.Top) * withPercent) div 2;
  InflateRect(aRect, dx, dy);
end;

function GetIntermediateRect(Rect1, Rect2: TRect; Percent: Double): TRect;
begin
  RESULT := classes.Rect(Rect1.Left + Round((Rect2.Left - Rect1.Left) * Percent),
                          Rect1.Top + Round((Rect2.Top - Rect1.Top) * Percent),
                           Rect1.Right + Round((Rect2.Right - Rect1.Right) * Percent),
                            Rect1.Bottom + Round((Rect2.Bottom - Rect1.Bottom) * Percent));
end;

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

procedure cyDrawButtonFace(Canvas: TCanvas; var Rect: TRect; GradientColor1, GradientColor2: TColor; aState: TButtonState; Focused, Hot: Boolean);

    procedure DrawInnerBorders(var Rect: TRect; TopColor, BottomColor: TColor);
    var
      aHeight, Y16, Y26, Y46, Y56: Integer;
      CenterColor, InterMedColor: TColor;
    begin
      aHeight := Rect.Bottom - Rect.Top;
      Y16     := Rect.Top + aHeight div 6;
      Y26     := Rect.Top + MulDiv(aHeight, 2, 6);
      Y46     := Rect.Top + MulDiv(aHeight, 4, 6);
      Y56     := Rect.Top + MulDiv(aHeight, 5, 6);

      CenterColor := MediumColor(TopColor, BottomColor);

      // LeftCenter to TopLeft to TopRight to RightCenter:
      Canvas.Pen.Color := CenterColor;
      Canvas.MoveTo(Rect.Left, Y46);
      Canvas.LineTo(Rect.Left, Y26);
      InterMedColor := MediumColor(TopColor, CenterColor);
      Canvas.Pen.Color := InterMedColor;
      Canvas.LineTo(Rect.Left, Y16);
      Canvas.Pen.Color := TopColor;
      Canvas.LineTo(Rect.Left, Rect.Top);
      Canvas.LineTo(Rect.Right-1, Rect.Top);
      Canvas.LineTo(Rect.Right-1, Y16);
      Canvas.Pen.Color := InterMedColor;
      Canvas.LineTo(Rect.Right-1, Y26);
      Canvas.Pen.Color := CenterColor;
      Canvas.LineTo(Rect.Right-1, Y46);
      // RightCenter to RightBottom to LeftBottom to LeftCenter:
      InterMedColor := MediumColor(BottomColor, CenterColor);
      Canvas.Pen.Color := InterMedColor;
      Canvas.LineTo(Rect.Right-1, Y56);
      Canvas.Pen.Color := BottomColor;
      Canvas.LineTo(Rect.Right-1, Rect.Bottom-1);
      Canvas.LineTo(Rect.Left, Rect.Bottom-1);
      Canvas.LineTo(Rect.Left, Y56);
      Canvas.Pen.Color := InterMedColor;
      Canvas.LineTo(Rect.Left, Y46);
      InflateRect(Rect, -1, -1);
    end;

var
  fromColor, toColor, FrameColor, BorderLeftTopColor, BorderRightBottomColor: TColor;
  balanceMode: TDgradBalanceMode;
  TopLeftColor, TopRightColor, BottomRightColor, BottomLeftColor: TColor;
begin
  // inherited;
  case aState of
    bsUp, bsHot: // 9999 RockyLuck
      begin
        if Hot then
        begin
          fromColor := ColorSetPercentBrightness(GradientColor1, 20);
          toColor := ColorSetPercentBrightness(GradientColor2, 20);
          balanceMode := bmReverseFromColor;
        end
        else begin
          fromColor := GradientColor1;
          toColor := GradientColor2;
          balanceMode := bmNormal;
        end;

        BorderLeftTopColor := ColorSetPercentBrightness(fromColor, 40);
        BorderRightBottomColor := MediumColor(fromColor, ToColor);
        FrameColor := ColorSetPercentBrightness(toColor, -20);
      end;

    bsDisabled:
      begin
        fromColor := ColorSetPercentBrightness(GradientColor1, 30);
        toColor := ColorSetPercentBrightness(GradientColor2, 30);
        balanceMode := bmNormal;
        BorderLeftTopColor := ColorSetPercentBrightness(GradientColor1, 40);
        BorderRightBottomColor := MediumColor(GradientColor1, GradientColor2);
        FrameColor := ColorSetPercentBrightness(toColor, -20);
      end;

    bsDown:
      begin
        fromColor := ColorSetPercentBrightness(GradientColor1, -20);
        toColor := ColorSetPercentBrightness(GradientColor2, -20);
        balanceMode := bmMirror;
        BorderLeftTopColor := MediumColor(GradientColor1, GradientColor2);
        BorderRightBottomColor := ColorSetPercentBrightness(fromColor, 40);
        FrameColor := ColorSetPercentBrightness(toColor, -20);
      end;
  end;

  // Save bounds pixel color :
  TopLeftColor := Canvas.Pixels[Rect.Left, Rect.Top];
  TopRightColor := Canvas.Pixels[Rect.Right-1, Rect.Top];
  BottomRightColor := Canvas.Pixels[Rect.Right-1, Rect.Bottom-1];
  BottomLeftColor := Canvas.Pixels[Rect.Left, Rect.Bottom-1];

  // Draw button body if not transarent:
  if (fromColor <> clNone) or (toColor <> clNone) then
    cyGradientFill(Canvas, Rect, fromColor, toColor, dgdVertical, 50, balanceMode, 255, 95);

  // Restore bounds pixel color :
  Canvas.Pixels[Rect.Left, Rect.Top]         := TopLeftColor;
  Canvas.Pixels[Rect.Right-1, Rect.Top]      := TopRightColor;
  Canvas.Pixels[Rect.Right-1, Rect.Bottom-1] := BottomRightColor;
  Canvas.Pixels[Rect.Left, Rect.Bottom-1]    := BottomLeftColor;

  // Draw frame :
  cyFrame3D(Canvas, Rect, FrameColor, FrameColor, 1);

  if aState = bsDown
  then cyFrame3D(Canvas, Rect, BorderLeftTopColor, BorderRightBottomColor, 1)
  else DrawInnerBorders(Rect, BorderLeftTopColor, BorderRightBottomColor);

  // Draw focus rect :
  if Focused then
    DrawFocusRect(Canvas.Handle, Rect);
end;

function PointInEllispe(const aPt: TPoint; const aRect: TRect): boolean;
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

function ColorSetPercentBrightness(Color: TColor; PercentLight: Integer): TColor;
var r,g,b, incValue: Integer;
begin
  incValue := MulDiv(255, PercentLight, 100);
  Color:= ColorToRGB(Color);

  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);

  r := r + incValue;
  g := g + incValue;
  b := b + incValue;

  if r < 0 then r := 0; if r > 255 then r := 255;
  if g < 0 then g := 0; if g > 255 then g := 255;
  if b < 0 then b := 0; if b > 255 then b := 255;

  RESULT := RGB(r,g,b);
end;

function MediumColor(Color1, Color2: TColor): TColor;
var
  r,g,b: Integer;
begin
  if Color1 <> Color2
  then begin
    Color1 := ColorToRGB(Color1);
    Color2 := ColorToRGB(Color2);

    r := ( GetRValue(Color1) + GetRValue(Color2) ) div 2;
    g := ( GetGValue(Color1) + GetGValue(Color2) ) div 2;
    b := ( GetBValue(Color1) + GetBValue(Color2) ) div 2;
//    RESULT := TColor( RGB(r, g, b) );
    RESULT := RGB(r, g, b);
  end
  else
    RESULT := Color1;
end;

function ValidGraphic(aGraphic: TGraphic): Boolean;
begin
  RESULT := false;
  if aGraphic <> Nil
  then
    if not aGraphic.Empty
    then RESULT := true;
end;

procedure DrawCanvas(Destination: TCanvas; DestRect: TRect; Source: TCanvas; SourceRect: TRect);
begin
  SetStretchBltMode(Destination.Handle, HALFTONE);//STRETCH_HALFTONE); // STRETCH_HALFTONE for high quality!
  StretchBlt(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left,
             DestRect.Bottom - DestRect.Top, Source.Handle, SourceRect.Left, SourceRect.Top,
             SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top, SRCCOPY);
end;

end.

