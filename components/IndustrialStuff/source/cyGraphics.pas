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

{$MODE Delphi}

// {$I cyCompilerDefines.inc}

interface

// We need to put jpeg to the uses for avoid run-time not handled jpeg image ...
uses
     LCLIntf, LCLType, Classes,  Forms, Graphics, Math, Buttons, Controls, ExtCtrls,
     {$IFDEF DELPHI2009_OR_ABOVE} pngimage, {$ENDIF} SysUtils, cyTypes;

// Background functions :
procedure cyGradientFill(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; adgradOrientation: TdgradOrientation; Balance: Word; balanceMode: TDgradBalanceMode; Maxdegrade: Byte; SpeedPercent: Integer);
procedure cyGradientFillVertical(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: byte);
procedure cyGradientFillHorizontal(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: byte);
procedure cyGradientFillShape(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; toRect: TRect; OrientationShape: TDgradOrientationShape);

// Objects painting functions :
procedure DrawRectangleInside(aCanvas: TCanvas; InsideRect: TRect; FrameWidth: Integer);
procedure cyFrame(aCanvas: TCanvas; var InsideRect: TRect; Color: TColor; const Width: Integer = 1); overload;
procedure cyFrame(Canvas: TCanvas; var InsideRect: TRect; LeftColor, TopColor, RightColor, BottomColor: TColor;
                    const Width: Integer = 1; const RoundRect: boolean = false); overload;
procedure cyFrame3D(Canvas: TCanvas; var Rect: TRect; TopLeftColor, BottomRightColor: TColor; Width: Integer;
                    const DrawLeft: Boolean = true; const DrawTop: Boolean = true; const DrawRight: Boolean = true; const DrawBottom: Boolean = true;
                    const RoundRect: boolean = false);
procedure cyDrawButtonFace(Canvas: TCanvas; var Rect: TRect; GradientColor1, GradientColor2: TColor; aState: TButtonState; Focused, Hot: Boolean);
procedure cyDrawButton(Canvas: TCanvas; Caption: String; ARect: TRect; GradientColor1, GradientColor2: TColor; aState: TButtonState; Focused, Hot: Boolean);
procedure cyDrawCheckBox(Canvas: TCanvas; IsChecked: Boolean; ARect: TRect; const BgColor: TColor = clWindow; const DarkFrameColor: TColor = clGray; const LightFrameColor: TColor = clWindow; const MarkColor: TColor = clBlack);

// Text functions :
procedure cyDrawSingleLineText(Canvas: TCanvas; Text: String; ARect: TRect; Alignment: TAlignment; TextLayout: TTextLayout; const IndentX: Integer = 0; const IndentY: Integer = 0);
function DrawTextFormatFlags(aTextFormat: LongInt; Alignment: TAlignment; Layout: TTextLayout; WordWrap: Boolean): LongInt; overload;
function DrawTextFormatFlags(aTextFormat: LongInt; Alignment: TAlignment; Layout: TTextLayout; WordWrap: Boolean; CaptionRender: TCaptionRender): LongInt; overload;
procedure cyDrawText(CanvasHandle: Cardinal; Text: String; var Rect: TRect; TextFormat: LongInt);
function cyCreateFontIndirect(fromFont: TFont; Angle: Double): TFont; overload;
function cyCreateFontIndirect(fromFont: TFont; CaptionOrientation: TCaptionOrientation): TFont; overload;
procedure cyDrawVerticalText(Canvas: TCanvas; Text: String; var Rect: TRect; TextFormat: Longint;
           CaptionOrientation: TCaptionOrientation; Alignment: TAlignment; Layout: TTextLayout);
function DrawLeftTurnPageEffect(Canvas: TCanvas; PageColor: TColor; PageRect: TRect; PercentDone: Integer; const OnlyCalcFoldLine: Boolean = false): TLineCoord;
function DrawRightTurnPageEffect(Canvas: TCanvas; PageColor: TColor; PageRect: TRect; PercentDone: Integer; const OnlyCalcFoldLine: Boolean = false): TLineCoord;

// TPicture and TGraphic functions:
function PictureIsTransparentAtPos(aPicture: TPicture; aPoint: TPoint): boolean;
function IconIsTransparentAtPos(aIcon: TIcon; aPoint: TPoint): boolean;
//function MetafileIsTransparentAtPos(aMetafile: TMetafile; aPoint: TPoint): boolean;
{$IFDEF DELPHI2009_OR_ABOVE}
function PngImageIsTransparentAtPos(aPngImage: TPngImage; aPoint: TPoint): boolean;
{$ENDIF}
// Draw a canvas Rect into another (Windows GDI library - not transparent) :
procedure DrawCanvas(Destination: TCanvas; DestRect: TRect; Source: TCanvas; SourceRect: TRect); overload;
procedure DrawCanvas(Destination: TCanvas; DestRect: TRect; Src: TCanvas; SrcRect: TRect; TransparentColor: TColor;
           const aStyle: TBgStyle = bgNormal; const aPosition: TBgPosition = bgTopLeft;
           const IndentX: Integer = 0; const IndentY: Integer = 0;
           const IntervalX: Integer = 0; const IntervalY: Integer = 0;
           const RepeatX: Integer = 1; const RepeatY: Integer = 1); overload;
procedure DrawGraphic(Destination: TCanvas; DestRect: TRect; aGraphic: TGraphic; SrcRect: TRect; TransparentColor: TColor;
           const aStyle: TBgStyle = bgNormal; const aPosition: TBgPosition = bgTopLeft;
           const IndentX: Integer = 0; const IndentY: Integer = 0;
           const IntervalX: Integer = 0; const IntervalY: Integer = 0;
           const RepeatX: Integer = 1; const RepeatY: Integer = 1); overload;
procedure DrawGraphic(Destination: TCanvas; DestRect: TRect; aGraphic: TGraphic; Transparent: Boolean;
           const aStyle: TBgStyle = bgNormal; const aPosition: TBgPosition = bgTopLeft;
           const IndentX: Integer = 0; const IndentY: Integer = 0;
           const IntervalX: Integer = 0; const IntervalY: Integer = 0;
           const RepeatX: Integer = 1; const RepeatY: Integer = 1); overload;
procedure DrawMosaicPortion(Destination: TCanvas; Portion: TRect; Pattern: TBitmap);
function ValidGraphic(aGraphic: TGraphic): Boolean;
function ColorSetPercentBrightness(Color: TColor; PercentLight: Integer): TColor;
function ColorModify(Color: TColor; incR, incG, incB: Integer): TColor;
function ColorSetPercentContrast(Color: TColor; IncPercent: Integer): TColor;
function ColorSetPercentPale(Color: TColor; IncPercent: integer): TColor;
function MediumColor(Color1, Color2: TColor): TColor;

// Other functions:
function ClientToScreenRect(aControl: TControl; aControlRect: TRect): TRect;
function ScreenToClientRect(aControl: TControl; aScreenRect: TRect): TRect;
function CombineRectKeepingCenterPosition(RectPos, AddRect: TRect): TRect;
procedure InflateRectPercent(var aRect: TRect; withPercent: Double);
function GetIntermediateRect(Rect1, Rect2: TRect; Percent: Double): TRect;
function GetProportionalRect(fromRect, InsideRect: TRect): TRect;
function PointInRect(const aPt: TPoint; const aRect: TRect):boolean;
function PointInEllispe(const aPt: TPoint; const aRect: TRect): boolean;
// Width in pixels of Text that can have accelerator (&) :
function CanvasAcceleratorTextWidth(aCanvas: TCanvas; aText: String): Integer;

implementation

uses Types;

{ Procedures and functions}
procedure cyGradientFill(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; adgradOrientation: TdgradOrientation; Balance: Word; balanceMode: TDgradBalanceMode; Maxdegrade: Byte; SpeedPercent: Integer);
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

function GetProportionalRect(fromRect, InsideRect: TRect): TRect;
var PercentH, PercentW: Extended;
begin
  Result.Left := InsideRect.Left;
  Result.Top := InsideRect.Top;

  PercentH := (InsideRect.Bottom - InsideRect.Top) / (fromRect.Bottom - fromRect.Top);
  PercentW := (InsideRect.Right - InsideRect.Left) / (fromRect.Right - fromRect.Left);

  if PercentH < PercentW then
  begin
    Result.Bottom := InsideRect.Bottom;
    Result.Right  := InsideRect.Left + Trunc((fromRect.Right - fromRect.Left) * PercentH);
  end
  else begin
    Result.Right := InsideRect.Right;
    Result.Bottom := InsideRect.Top + Trunc((fromRect.Bottom - fromRect.Top) * PercentW);
  end;
end;

function GetIntermediateRect(Rect1, Rect2: TRect; Percent: Double): TRect;
begin
  RESULT := classes.Rect(Rect1.Left + Round((Rect2.Left - Rect1.Left) * Percent),
                          Rect1.Top + Round((Rect2.Top - Rect1.Top) * Percent),
                           Rect1.Right + Round((Rect2.Right - Rect1.Right) * Percent),
                            Rect1.Bottom + Round((Rect2.Bottom - Rect1.Bottom) * Percent));
end;

procedure DrawRectangleInside(aCanvas: TCanvas; InsideRect: TRect; FrameWidth: Integer);
var Sauv: TColor;
begin
  Sauv := aCanvas.Brush.Color;

  // Draw frame:
  aCanvas.Brush.Color := aCanvas.Pen.Color;
  aCanvas.FillRect(InsideRect);

  // Draw inside:
  InflateRect(InsideRect, (-1) * FrameWidth, (-1) * FrameWidth);
  aCanvas.Brush.Color := Sauv;
  aCanvas.FillRect(InsideRect);
end;

procedure cyFrame(aCanvas: TCanvas; var InsideRect: TRect; Color: TColor; const Width: Integer = 1);
var i: Integer;
begin
  aCanvas.Brush.Color := Color;

  for i := 1 to Width do
  begin
    aCanvas.FrameRect(InsideRect);
    InflateRect(InsideRect, -1, -1);
  end;
end;

procedure cyFrame(Canvas: TCanvas; var InsideRect: TRect; LeftColor, TopColor, RightColor, BottomColor: TColor;
                    const Width: Integer = 1; const RoundRect: boolean = false);
var i, IncValue: Integer;
begin
  if RoundRect
  then incValue := 1
  else incValue := 0;

  for i := 1 to Width do
  begin
    with Canvas, InsideRect do
    begin
      if LeftColor <> clNone then
      begin
        Pen.Color := LeftColor;
        MoveTo(Left, Top + incValue);
        LineTo(Left, Bottom);
      end;

      if TopColor <> clNone then
      begin
        Pen.Color := TopColor;
        MoveTo(Left + incValue, Top);
        LineTo(Right, Top);
      end;

      if RightColor <> clNone then
      begin
        Pen.Color := RightColor;
        MoveTo(Right, Top + incValue);
        LineTo(Right, Bottom);
      end;

      if BottomColor <> clNone then
      begin
        Pen.Color := BottomColor;
        MoveTo(Right - incValue, Bottom);
        LineTo(Left-1 + incValue, Bottom);
      end;
    end;

    IncValue := 0;
    InflateRect(InsideRect, -1, -1);
  end;
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

procedure cyDrawButton(Canvas: TCanvas; Caption: String; ARect: TRect; GradientColor1, GradientColor2: TColor; aState: TButtonState; Focused, Hot: Boolean);
var
  TextFormat: Integer;
  savStyle: TBrushStyle;
begin
  cyDrawButtonFace(Canvas, ARect, GradientColor1, GradientColor2, aState, Focused, Hot);

  // Draw caption :
  savStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  TextFormat := DrawTextFormatFlags(0, taCenter, tlCenter, true);
  cyDrawText(Canvas.Handle, Caption, ARect, TextFormat);
  Canvas.Brush.Style := savStyle;
end;

procedure cyDrawCheckBox(Canvas: TCanvas; IsChecked: Boolean; ARect: TRect; const BgColor: TColor = clWindow; const DarkFrameColor: TColor = clGray; const LightFrameColor: TColor = clWindow; const MarkColor: TColor = clBlack);
var
  MedColor, LessDarkFrameColor, LessLightFrameColor: TColor;
  w, l, m, r, y: Integer;  // mark Width, left, middle, right, top
begin
  if BgColor <> clNone then
  begin
    Canvas.Pen.Color   := BgColor;
    Canvas.Brush.Color := BgColor;
    Canvas.Rectangle(ARect);
  end;

  // Checkbox Frame:
  MedColor := MediumColor(DarkFrameColor, LightFrameColor);
  LessDarkFrameColor := MediumColor(DarkFrameColor, MedColor);
  LessLightFrameColor := MediumColor(LightFrameColor, MedColor);

  cyFrame3D(Canvas, aRect, LessDarkFrameColor, LightFrameColor, 1);
  cyFrame3D(Canvas, aRect, DarkFrameColor, LessLightFrameColor, 1);

  if IsChecked then
  begin
    Canvas.Pen.Color := MarkColor;
    l := ARect.Left + (ARect.Right - ARect.Left) div 15 + 1;
    m := ARect.Left + (ARect.Right - ARect.Left) div 3;
    r := ARect.Left + Round((ARect.Right - ARect.Left) * 0.75);
    y := ARect.Top  + (ARect.Bottom - ARect.Top) div 3;

    for w := 0 to ((ARect.Bottom - ARect.Top) div 4) do   // Mark width
    begin
      Canvas.MoveTo(l, y + w);
      Canvas.LineTo(m, y + w + (m-l));
      Canvas.LineTo(r, y + w + (m-l) - (r-m));
    end;
  end;
end;

function PointInRect(const aPt: TPoint; const aRect: TRect):boolean;
begin
  if (aPt.X>=aRect.Left) and (aPt.X<=aRect.Right) and (aPt.Y>= aRect.Top) and (aPt.Y<=aRect.Bottom) then
    result:=True
  else result:=false;
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
    //else
     // if aPicture.Graphic is TMetafile
     // then
     //   RESULT := MetafileIsTransparentAtPos(aPicture.Metafile, aPoint)
      {$IFDEF DELPHI2009_OR_ABOVE}
      else
        if aPicture.Graphic is TPngImage
        then
          RESULT := PngImageIsTransparentAtPos(TPngImage(aPicture.Graphic), aPoint);
      {$ENDIF}
end;

  {   Old Function
function IconIsTransparentAtPos(aIcon: TIcon; aPoint: TPoint): boolean;
var aBmp: TBitmap;
begin
  RESULT := false;
  aBmp := TBitmap.Create;

  try
    aBmp.Width := aIcon.Width;
    aBmp.Height := aIcon.Height;
    aBmp.PixelFormat := pf1bit;  // Black = not transparent
    aBmp.Canvas.Brush.Color := clWhite;
    aBmp.Canvas.FillRect(Rect(0, 0, aBmp.Width, aBmp.Height));
    DrawIconEx(aBmp.Canvas.Handle, 0, 0, aIcon.Handle, aIcon.Width, aIcon.Height, 0, 0, DI_MASK);
    RESULT := aBmp.Canvas.Pixels[aPoint.X, aPoint.Y] <> clBlack;
  finally
    aBmp.Free;
  end;
end;  }

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

  {
function MetafileIsTransparentAtPos(aMetafile: TMetafile; aPoint: TPoint): boolean;
var aBmp: TBitmap;
begin
  RESULT := false;
  aBmp := TBitmap.Create;

  try
    aBmp.Width := aMetafile.Width;
    aBmp.Height := aMetafile.Height;
    aBmp.PixelFormat := pf24bit;
    aBmp.Canvas.Brush.Color := clFuchsia - 1;  // We determine that it will be the background
    aBmp.Canvas.FillRect(Rect(0, 0, aBmp.Width, aBmp.Height));
    aBmp.Canvas.Draw(0, 0, aMetafile);
    RESULT := aBmp.Canvas.Pixels[aPoint.X, aPoint.Y] = clFuchsia-1;
  finally
    aBmp.Free;
  end;
end;
   }
{$IFDEF DELPHI2009_OR_ABOVE}
function PngImageIsTransparentAtPos(aPngImage: TPngImage; aPoint: TPoint): boolean;
begin
  RESULT := aPngImage.Pixels[aPoint.X, aPoint.Y] = aPngImage.TransparentColor;
end;
{$ENDIF}

function ClientToScreenRect(aControl: TControl; aControlRect: TRect): TRect;
var aScreenPt: TPoint;
begin
  aScreenPt := aControl.ClientToScreen(aControlRect.TopLeft);
  RESULT := classes.Rect(aScreenPt.X,
                         aScreenPt.Y,
                         aScreenPt.X + (aControlRect.Right - aControlRect.Left),
                         aScreenPt.Y + (aControlRect.Bottom - aControlRect.Top));
end;

function ScreenToClientRect(aControl: TControl; aScreenRect: TRect): TRect;
var aControlPt: TPoint;
begin
  aControlPt := aControl.ScreenToClient(aScreenRect.TopLeft);
  RESULT := classes.Rect(aControlPt.X,
                         aControlPt.Y,
                         aControlPt.X + (aScreenRect.Right - aScreenRect.Left),
                         aControlPt.Y + (aScreenRect.Bottom - aScreenRect.Top));
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

function ColorModify(Color: TColor; incR, incG, incB: Integer): TColor;
var r,g,b: Integer;
begin
  Color:= ColorToRGB(Color);

  r:= GetRValue(Color);
  g:= GetGValue(Color);
  b:= GetBValue(Color);

  r := r + incR;
  g := g + incG;
  b := b + incB;

  if r < 0 then r := 0; if r > 255 then r := 255;
  if g < 0 then g := 0; if g > 255 then g := 255;
  if b < 0 then b := 0; if b > 255 then b := 255;

  Result := RGB(r,g,b);
end;

function ColorSetPercentContrast(Color: TColor; IncPercent: Integer): TColor;
var r,g,b, Media: Integer;
begin
  if IncPercent > 100 then IncPercent := 100;
  if IncPercent < -100 then IncPercent := -100;

  Color:= ColorToRGB(Color);

  r:= GetRValue(Color);
  g:= GetGValue(Color);
  b:= GetBValue(Color);

  Media := (r+g+b) Div 3;

  r := r + Round(  (r - Media) * (IncPercent / 100)  );
  g := g + Round(  (g - Media) * (IncPercent / 100)  );
  b := b + Round(  (b - Media) * (IncPercent / 100)  );

  if r < 0 then r := 0; if r > 255 then r := 255;
  if g < 0 then g := 0; if g > 255 then g := 255;
  if b < 0 then b := 0; if b > 255 then b := 255;

  RESULT := RGB(r,g,b);
end;

function ColorSetPercentPale(Color: TColor; IncPercent: integer): TColor;
var r, g, b: Integer;
begin
  r:= GetRValue(Color);
  g:= GetGValue(Color);
  b:= GetBValue(Color);

  r := r + Round((255 - r) * IncPercent / 100);
  g := g + Round((255 - g) * IncPercent / 100);
  b := b + Round((255 - b) * IncPercent / 100);

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

// for aText = '&', noAcceText = '' and RESULT = 0
// for aText = '&&', noAcceText = '&' and RESULT = 1
function CanvasAcceleratorTextWidth(aCanvas: TCanvas; aText: String): Integer;
var
  i: Integer;
  noAcceText: String;
  AcceleratorFound, AcceleratorFounded: Boolean;
begin
  noAcceText := '';
  AcceleratorFounded := false;

  for i := 1 to Length(aText) do
  begin
    AcceleratorFound := aText[i] = '&';

    if AcceleratorFound
    then begin
      if AcceleratorFounded                     // There' s one before this one ...
      then begin
        noAcceText := noAcceText + '&';
        AcceleratorFound := false;
      end;
    end
    else
      noAcceText := noAcceText + aText[i];

    AcceleratorFounded := AcceleratorFound;
  end;


  RESULT := aCanvas.TextWidth(noAcceText);
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

{ Not working ...
  if Destination.Brush.Style = bsClear
  then
    TransparentStretchBlt(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
               Source.Handle, SourceRect.Left, SourceRect.Top, SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top,
               Destination.Handle, DestRect.Left, DestRect.Top)
  else  }

    StretchBlt(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
               Source.Handle, SourceRect.Left, SourceRect.Top, SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top,
               SRCCOPY);
end;

procedure DrawCanvas(Destination: TCanvas; DestRect: TRect; Src: TCanvas; SrcRect: TRect; TransparentColor: TColor;
           const aStyle: TBgStyle = bgNormal; const aPosition: TBgPosition = bgTopLeft;
           const IndentX: Integer = 0; const IndentY: Integer = 0;
           const IntervalX: Integer = 0; const IntervalY: Integer = 0;
           const RepeatX: Integer = 1; const RepeatY: Integer = 1);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.Width := SrcRect.Right - SrcRect.Left;
  Bmp.Height := SrcRect.Bottom - SrcRect.Top;
  Bmp.Canvas.Brush.Color := TransparentColor;
  Bmp.Canvas.FillRect(classes.Rect(0, 0, Bmp.Width, Bmp.Height));
  Bmp.Canvas.CopyRect(classes.Rect(0, 0, Bmp.Width, Bmp.Height), Src, SrcRect);
  Bmp.TransparentColor := TransparentColor;
  Bmp.Transparent := True;

  DrawGraphic(Destination, DestRect, Bmp, TransparentColor <> clNone,
              aStyle, aPosition, IndentX, IndentY, IntervalX, IntervalY, RepeatX, RepeatY);

  Bmp.Free;
end;

procedure DrawGraphic(Destination: TCanvas; DestRect: TRect; aGraphic: TGraphic; SrcRect: TRect; TransparentColor: TColor;
           const aStyle: TBgStyle = bgNormal; const aPosition: TBgPosition = bgTopLeft;
           const IndentX: Integer = 0; const IndentY: Integer = 0;
           const IntervalX: Integer = 0; const IntervalY: Integer = 0;
           const RepeatX: Integer = 1; const RepeatY: Integer = 1);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.Width := SrcRect.Right - SrcRect.Left;
  Bmp.Height := SrcRect.Bottom - SrcRect.Top;
  Bmp.Canvas.Brush.Color := TransparentColor;
  Bmp.Canvas.FillRect(classes.Rect(0, 0, Bmp.Width, Bmp.Height));
  Bmp.Canvas.Draw((-1) * SrcRect.Left, (-1) * SrcRect.Top, aGraphic);

  if TransparentColor <> clNone then
  begin
    Bmp.Transparent := True;
    Bmp.TransparentColor := TransparentColor;
  end;

  DrawGraphic(Destination, DestRect, Bmp, TransparentColor <> clNone,
              aStyle, aPosition, IndentX, IndentY, IntervalX, IntervalY, RepeatX, RepeatY);

  Bmp.Free;
end;

procedure DrawGraphic(Destination: TCanvas; DestRect: TRect; aGraphic: TGraphic; Transparent: Boolean;
           const aStyle: TBgStyle = bgNormal; const aPosition: TBgPosition = bgTopLeft;
           const IndentX: Integer = 0; const IndentY: Integer = 0;
           const IntervalX: Integer = 0; const IntervalY: Integer = 0;
           const RepeatX: Integer = 1; const RepeatY: Integer = 1);
var
  x, y, SauvY, rx, ry: integer;
  _RepeatX, _RepeatY: word;
  Style: TBgStyle;

  procedure CalculateRepetitionPosition;

      function HorizontalRectCenter: Integer;
      begin
        RESULT := DestRect.Left + (DestRect.Right - DestRect.Left) div 2;
      end;

      function VerticalRectCenter: Integer;
      begin
        RESULT := DestRect.Top + (DestRect.Bottom - DestRect.Top) div 2;
      end;

      function HorizontalNeeds: integer;
      begin
        RESULT := (aGraphic.Width + IntervalX) * _RepeatX - IntervalX;
      end;

      function VerticalNeeds: integer;
      begin
        RESULT := (aGraphic.Height + IntervalY) * _RepeatY - IntervalY;
      end;

  begin
    case aPosition of
      bgCentered:
      begin
        y := VerticalRectCenter - VerticalNeeds div 2;
        x := HorizontalRectCenter - HorizontalNeeds div 2;
      end;

      bgTopLeft:
      begin
        y := DestRect.Top + IndentY;
        x := DestRect.Left + IndentX;
      end;

      bgTopCenter:
      begin
        y := DestRect.Top + IndentY;
        x := HorizontalRectCenter - HorizontalNeeds div 2;
      end;

      bgTopRight:
      begin
        y := DestRect.Top + IndentY;
        x := DestRect.Right - IndentX - HorizontalNeeds;
      end;

      bgCenterRight:
      begin
        y := VerticalRectCenter - VerticalNeeds div 2;
        x := DestRect.Right - IndentX - HorizontalNeeds;
      end;

      bgBottomRight:
      begin
        y := DestRect.Bottom - IndentY - VerticalNeeds;
        x := DestRect.Right - IndentX - HorizontalNeeds;
      end;

      bgBottomCenter:
      begin
        y := DestRect.Bottom - IndentY - VerticalNeeds;
        x := HorizontalRectCenter - HorizontalNeeds div 2;
      end;

      bgBottomLeft:
      begin
        y := DestRect.Bottom - IndentY - VerticalNeeds;
        x := DestRect.Left + IndentX;
      end;

      bgCenterLeft:
      begin
        y := VerticalRectCenter - VerticalNeeds div 2;
        x := DestRect.Left + IndentX;
      end;
    end;
  end;

  function RetrieveProporcionalRect: TRect;
  var aLeft, aTop, aRight, aBottom, ProporcionalSize: Integer;
  begin
    // Calculate avaible space:
    aLeft := DestRect.Left;
    aTop := DestRect.Top;
    aRight := DestRect.Right;
    aBottom := DestRect.Bottom;

    case aPosition of
      bgTopLeft:
      begin
        inc(aTop, IndentY);
        inc(aLeft, IndentX);
      end;

      bgTopCenter:
        inc(aTop, IndentY);

      bgTopRight:
      begin
        inc(aTop, IndentY);
        dec(aRight, IndentX);
      end;

      bgCenterRight:
        dec(aRight, IndentX);

      bgBottomRight:
      begin
        dec(aBottom, IndentY);
        dec(aRight, IndentX);
      end;

      bgBottomCenter:
        dec(aBottom, IndentY);

      bgBottomLeft:
      begin
        dec(aBottom, IndentY);
        inc(aLeft, IndentX);
      end;

      bgCenterLeft:
        inc(aLeft, IndentX);
    end;

    if (aRight > aLeft) and (aBottom > aTop)     // Avoid division by 0 ...
    then
      if aGraphic.Width / (aRight - aLeft) < aGraphic.Height / (aBottom - aTop)
      then begin               // *** Adjust horizontally *** //
        // Calculate width:
        ProporcionalSize := Trunc( (aBottom - aTop) / aGraphic.Height * aGraphic.Width );

        case aPosition of
          bgTopLeft, bgBottomLeft, bgCenterLeft:
            aRight := aLeft + ProporcionalSize;

          bgTopRight, bgBottomRight, bgCenterRight:
            aLeft := aRight - ProporcionalSize;

          bgCentered, bgBottomCenter, bgTopCenter:
          begin
            aLeft := aLeft + (aRight - aLeft - ProporcionalSize) div 2;
            aRight := aLeft + ProporcionalSize;
          end;
        end;
      end
      else begin               // *** Adjust vertically *** //
        // Calculate height:
        ProporcionalSize := Trunc( (aRight - aLeft) / aGraphic.Width * aGraphic.Height );

        case aPosition of
          bgTopLeft, bgTopCenter, bgTopRight:
            aBottom := aTop + ProporcionalSize;

          bgBottomRight, bgBottomCenter, bgBottomLeft:
            aTop := aBottom - ProporcionalSize;

          bgCentered, bgCenterLeft, bgCenterRight:
          begin
            aTop := aTop + (aBottom - aTop - ProporcionalSize) div 2;
            aBottom := aTop + ProporcionalSize;
          end;
        end;
      end;

    RESULT := classes.Rect(aLeft, aTop, aRight, aBottom);
  end;

begin
  Style := aStyle;
  if Style = bgNone then Exit;
  if not ValidGraphic(aGraphic) then Exit;

  if Style = bgStretchProportional
  then begin
    Style := bgStretch;
    DestRect := RetrieveProporcionalRect;
  end;

  if DestRect.Right <= DestRect.Left then Exit;
  if DestRect.Bottom <= DestRect.Top then Exit;

  if aGraphic.Transparent <> Transparent
  then aGraphic.Transparent := Transparent;

  if Style = bgStretch
  then begin
    // Old Function
    {if aGraphic is TIcon    // Stretch draw doesn't work for icons!
    then DrawIconEx(Destination.Handle, DestRect.Left, DestRect.Top, TIcon(aGraphic).Handle, DestRect.Right-DestRect.Left, DestRect.Bottom-DestRect.Top, 0, 0, DI_Normal)
    else   Destination.StretchDraw(DestRect, aGraphic);  }

    if (aGraphic is TBitmap) and (not Transparent) then // use Drawcanvas() function for better rendering!
      begin
        with TBitmap(aGraphic) do
          DrawCanvas(Destination, DestRect,  Canvas, classes.Rect(0, 0, Width, Height));
      end
      else
        Destination.StretchDraw(DestRect, aGraphic);
        
  end
  else begin
    _RepeatX := RepeatX;
    _RepeatY := RepeatY;

    if Style = bgMosaic
    then begin
      x := aGraphic.Width + IntervalX;
      y := aGraphic.Height + IntervalY;
      _RepeatX := Ceil( (DestRect.Right-DestRect.Left+IntervalX)/ x );
      _RepeatY := Ceil( (DestRect.Bottom-DestRect.Top+IntervalY)/ y );
      x := DestRect.Left;
      y := DestRect.Top;
    end
    else   // Style = bgNormal
      CalculateRepetitionPosition;

    // Draw from left to right and from top to bottom starting on position x, y :
    SauvY := y;
    for rx := 1 to _RepeatX do
    begin
      y := SauvY;
      for ry := 1 to _RepeatY do
      begin
        Destination.Draw(x, y,  aGraphic);
        inc(y, IntervalY + aGraphic.Height);
      end;

      inc(x, IntervalX + aGraphic.Width);
    end;
  end;
end;

procedure DrawMosaicPortion(Destination: TCanvas; Portion: TRect; Pattern: TBitmap);
var X, Y, GraphicPosX, GraphicPosY, _Width, _Height: Integer;
begin
  Y := Portion.Top;

  while y < Portion.Bottom do
  begin
    X := Portion.Left;

    GraphicPosY := Y - ((Y Div Pattern.Height) * Pattern.Height);
    _Height     := Pattern.Height - GraphicPosY;

    if _Height > Portion.Bottom - Y then
      _Height := Portion.Bottom - Y;   // Max _Height ...

    while x < Portion.Right do
    begin
      GraphicPosX := X - ((X Div Pattern.Width) * Pattern.Width);
      _Width    := Pattern.Width - GraphicPosX;

      if _Width > Portion.Right - X then
        _Width := Portion.Right - X; // Max _Width ...

      Destination.CopyRect( Rect(X, Y, X + _Width, Y + _Height),
                        Pattern.Canvas,
                        Rect(GraphicPosX, GraphicPosY, GraphicPosX + _Width, GraphicPosY + _Height) );

      X := X + _Width;
    end;

    Y := Y + _Height;
  end;
end;

procedure cyDrawSingleLineText(Canvas: TCanvas; Text: String; ARect: TRect; Alignment: TAlignment; TextLayout: TTextLayout; const IndentX: Integer = 0; const IndentY: Integer = 0);
var x, y: Integer;
begin
  case Alignment of
    taCenter       : x := ARect.Left + ((ARect.Right - ARect.Left - Canvas.TextWidth(Text)) Div 2);
    taLeftJustify  : x := ARect.Left + IndentX;
    taRightJustify : x := ARect.Right - IndentX - Canvas.TextWidth(Text);
  end;

  case TextLayout of
    tlCenter  : y := ARect.Top + ((ARect.Bottom - ARect.Top - Canvas.TextHeight(Text)) Div 2);
    tlTop     : y := ARect.Top + IndentY;
    tlBottom  : y := ARect.Bottom - IndentY - Canvas.TextHeight(Text);
  end;

  Canvas.TextRect(ARect, x, y, Text);
end;

function DrawTextFormatFlags(aTextFormat: LongInt; Alignment: TAlignment; Layout: TTextLayout; WordWrap: Boolean): LongInt;
begin
  RESULT := aTextFormat or Alignments[Alignment] or TextLayouts[Layout] or WordWraps[WordWrap];
end;

function DrawTextFormatFlags(aTextFormat: LongInt; Alignment: TAlignment; Layout: TTextLayout;
                             WordWrap: Boolean; CaptionRender: TCaptionRender): LongInt;
begin
  RESULT := aTextFormat or Alignments[Alignment] or TextLayouts[Layout] or WordWraps[WordWrap] or CaptionRenders[CaptionRender];
end;

{ Very important:
  DT_VCENTER- align to vertical center, only works with DT_SINGLELINE
  DT_BOTTOM - align to bottom, only works with DT_SINGLELINE }
procedure cyDrawText(CanvasHandle: Cardinal; Text: String; var Rect: TRect; TextFormat: LongInt);

        function NeedRectAdjustment: Boolean;
        begin
          RESULT := false;
          if TextFormat and DT_WORDBREAK <> 0    // WordWrap caption
          then
            if TextFormat and DT_VCENTER <> 0    // Vertical alignment to center
            then
              RESULT := true
            else
              if TextFormat and DT_BOTTOM <> 0   // Vertical alignment to bottom
              then
                RESULT := true;
        end;

var
  CalcFlags: LongInt;
  CalcRect: TRect;
begin
  if NeedRectAdjustment     // We have to adjust aRect if DT_VCENTER / DT_BOTTOM and DT_WORDBREAK
  then begin
    CalcRect := Rect;
    CalcFlags := TextFormat or DT_CALCRECT;

    {$IFDEF DELPHI2009_OR_ABOVE}
      Windows.DrawText(CanvasHandle, Text, -1, CalcRect, CalcFlags);
    {$ELSE}
      DrawText(CanvasHandle, PChar(Text), -1, CalcRect, CalcFlags);
    {$ENDIF}

    // *** From here, variable CalcRect was been modified by Windows.DrawText() function *** //

    // Adjust Rect for draw text from Rect.Top:
    if TextFormat and DT_BOTTOM <> 0   // Vertical alignment to bottom
    then OffsetRect(Rect, 0, Rect.Bottom - Rect.Top - (CalcRect.Bottom - CalcRect.Top))
    else OffsetRect(Rect, 0, (Rect.Bottom - Rect.Top - (CalcRect.Bottom - CalcRect.Top)) div 2);

    if TextFormat and DT_CALCRECT <> 0         // cyDrawText() only called for retrieve Rect
    then begin
      Rect.Bottom := Rect.Top + (CalcRect.Bottom - CalcRect.Top);
      Exit;
    end;
  end;

  {$IFDEF DELPHI2009_OR_ABOVE}
    Windows.DrawText(CanvasHandle, Text, -1, Rect, TextFormat);
  {$ELSE}
    DrawText(CanvasHandle, PChar(Text), -1, Rect, TextFormat);
  {$ENDIF}
end;

function cyCreateFontIndirect(fromFont: TFont; Angle: Double): TFont;
var
  TmpLogFont: TLogFont;
begin
  Result := TFont.Create;
  Result.Assign(fromFont);
  // Retrieve information about TmpFont in order to change properties :
  GetObject(Result.Handle, SizeOf(TmpLogFont), @TmpLogFont);
  TmpLogFont.lfEscapement := Trunc(Angle * 10);
  TmpLogFont.lfOrientation := TmpLogFont.lfEscapement;
  TmpLogFont.lfQuality := ANTIALIASED_QUALITY;             // Better quality !
  Result.Handle := CreateFontIndirect(TmpLogFont);
end;

function cyCreateFontIndirect(fromFont: TFont; CaptionOrientation: TCaptionOrientation): TFont;
var
  TmpLogFont: TLogFont;
begin
  Result := TFont.Create;
  Result.Assign(fromFont);
  // Retrieve information about TmpFont in order to change properties :
  GetObject(Result.Handle, SizeOf(TmpLogFont), @TmpLogFont);
  TmpLogFont.lfEscapement := CaptionOrientations[CaptionOrientation];
  TmpLogFont.lfOrientation := TmpLogFont.lfEscapement;
  TmpLogFont.lfQuality := ANTIALIASED_QUALITY;             // Better quality !
  Result.Handle := CreateFontIndirect(TmpLogFont);
end;

procedure cyDrawVerticalText(Canvas: TCanvas; Text: String; var Rect: TRect; TextFormat: Longint;
           CaptionOrientation: TCaptionOrientation; Alignment: TAlignment; Layout: TTextLayout);
var
  TextWidth, TextHeight, PosX, PosY: Integer;
begin
  TextWidth  := Canvas.TextWidth(Text);
  TextHeight := Canvas.TextHeight(Text);

  case CaptionOrientation of
    coHorizontalReversed:
    begin
      case Alignment of
        taLeftJustify:  PosX := Rect.Left + TextWidth;
        taCenter:       PosX := Rect.Left + (Rect.Right-Rect.Left) div 2 + TextWidth div 2;
        taRightJustify: PosX := Rect.Right;
      end;

      case Layout of
        tlTop:    PosY := Rect.Top + TextHeight;
        tlCenter: PosY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 + TextHeight div 2;
        tlBottom: PosY := Rect.Bottom;
      end;
    end;

    coVertical:
    begin
      case Alignment of
        taLeftJustify:  PosX := Rect.Left;
        taCenter:       PosX := Rect.Left + (Rect.Right-Rect.Left) div 2 - TextHeight div 2;
        taRightJustify: PosX := Rect.Right - TextHeight;
      end;

      case Layout of
        tlTop:    PosY := Rect.Top + TextWidth;
        tlCenter: PosY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 + TextWidth div 2;
        tlBottom: PosY := Rect.Bottom;
      end;
    end;

    coVerticalReversed:
    begin
      case Alignment of
        taLeftJustify:  PosX := Rect.Left + TextHeight;
        taCenter:       PosX := Rect.Left + (Rect.Right-Rect.Left) div 2 + TextHeight div 2;
        taRightJustify: PosX := Rect.Right;
      end;

      case Layout of
        tlTop:    PosY := Rect.Top;
        tlCenter: PosY := Rect.Top + (Rect.Bottom-Rect.Top) div 2 - TextWidth div 2;
        tlBottom: PosY := Rect.Bottom - TextWidth;
      end;
    end;
  end;

//  Rect := classes.Rect(0, 200, 0, 200);
//  DrawTextEx(Canvas.Handle, PChar(Text), -1, Rect, TextFormat, Nil);
//  cyDrawText(Canvas.Handle, Text, Rect, TextFormat);

  Canvas.TextOut(PosX, PosY, Text);
end;

// Draw page turn effect calculating 4 points :
function DrawLeftTurnPageEffect(Canvas: TCanvas; PageColor: TColor; PageRect: TRect; PercentDone: Integer; const OnlyCalcFoldLine: Boolean): TLineCoord;
var
  PtBottomPage, PtLeftPage, ptDraggedCorner, ptTopPageCorner: TPoint;
  x, y, Dif, i: Integer;
begin
  // PtBottomPage :
  x := Round(PageRect.Left + (PageRect.Right - PageRect.Left) * PercentDone / 100);
  PtBottomPage := Point(x, PageRect.Bottom-1);

  // PtLeftPage :
  x := PageRect.Left;
  y := PageRect.Bottom - (PtBottomPage.X - PageRect.Left) * 2;

  if y < PageRect.Top then
  begin
    Dif := PageRect.Top - y;
    y := PageRect.Top;

    x := Round(x + Dif);

    if x > PtBottomPage.X then
      x := PtBottomPage.X;

    if x > PageRect.Right then
      x := PageRect.Right;
  end
  else
    Dif := 0;

  PtLeftPage := Point(x, y);

  Result.BottomCoord := PtBottomPage;
  Result.TopCoord := PtLeftPage;

  if OnlyCalcFoldLine then Exit;

  // ptTopPageCorner :
  if Dif <> 0 then
  begin
    x := PtLeftPage.x + Dif div 4;
    y := PtLeftPage.Y - Dif;
    if y < 0 then
      y := 0;
    ptTopPageCorner := Point(x, y);
  end
  else
    ptTopPageCorner := PtLeftPage;

  // ptDraggedCorner :
  x := Round(PtBottomPage.x + (PageRect.Right-PageRect.Left) * PercentDone/100 * 0.5);
  y := Round(PtLeftPage.Y + (PageRect.Right-PageRect.Left) * PercentDone/100);
  if y > PageRect.Bottom then
    y := PageRect.Bottom;
  ptDraggedCorner := Point(x, y);

  // Draw :
  Canvas.Pen.Width := 1;
  Canvas.Pen.Mode := pmMask;  // pmNotXor
  Canvas.Pen.Color := cyGraphics.ColorModify(PageColor, -40, -40, -40);

  // Draw inside shade effect :
  if PercentDone < 90 then
  begin
    for i := 1 to (PageRect.Right-PageRect.Left) div 10 + 3 do
    begin
      x := PtBottomPage.x+i;
      if x > PageRect.Right then x := PageRect.Right;
      Canvas.MoveTo(x, PtBottomPage.y);
      y := ptLeftPage.y-i;
      if y < PageRect.Top then y := PageRect.Top;
      Canvas.LineTo(ptLeftPage.x, y);
    end;
  end;

  // Draw outside shade effect :
  if PercentDone > 5 then
  begin
    for i := 1 to (PageRect.Right-PageRect.Left) div 60 + 1 do
    begin
      Canvas.MoveTo(PtBottomPage.x-i, PtBottomPage.y);
      Canvas.LineTo(ptLeftPage.x, ptLeftPage.y+i);
    end;
  end;

  // Draw polygon :
  Canvas.Pen.Mode := pmCopy;
  Canvas.Brush.Color := PageColor;
  Canvas.Polygon([PtBottomPage, PtLeftPage, ptTopPageCorner, ptDraggedCorner]);
end;

// Draw page turn effect calculating 4 points :
function DrawRightTurnPageEffect(Canvas: TCanvas; PageColor: TColor; PageRect: TRect; PercentDone: Integer; const OnlyCalcFoldLine: Boolean): TLineCoord;
var
  PtBottomPage, ptRightPage, ptDraggedCorner, ptTopPageCorner: TPoint;
  x, y, Dif, i: Integer;
begin
  // PtBottomPage :
  x := Round(PageRect.Left + (PageRect.Right - PageRect.Left) * (100-PercentDone) / 100);
  PtBottomPage := Point(x, PageRect.Bottom-1);

  // ptRightPage :
  x := PageRect.Right-1;
  y := PageRect.Bottom - (PageRect.Right - PtBottomPage.X) * 2;

  if y < PageRect.Top then
  begin
    Dif := PageRect.Top - y;
    y := PageRect.Top;

    x := Round(x - Dif);

    if x < PtBottomPage.X then
      x := PtBottomPage.X;

    if x < PageRect.Left then
      x := PageRect.Left;
  end
  else
    Dif := 0;

  ptRightPage := Point(x, y);

  Result.BottomCoord := PtBottomPage;
  Result.TopCoord := ptRightPage;

  if OnlyCalcFoldLine then Exit;

  // ptTopPageCorner :
  if Dif <> 0 then
  begin
    x := ptRightPage.x - Dif div 4;
    y := ptRightPage.Y - Dif;
    if y < 0 then
      y := 0;
    ptTopPageCorner := Point(x, y);
  end
  else
    ptTopPageCorner := ptRightPage;

  // ptDraggedCorner :
  x := Round(PtBottomPage.x - (PageRect.Right-PageRect.Left) * PercentDone/100 * 0.5);
  y := Round(ptRightPage.Y + (PageRect.Right-PageRect.Left) * PercentDone/100);
  if y > PageRect.Bottom then
    y := PageRect.Bottom;
  ptDraggedCorner := Point(x, y);

  // Draw :
  Canvas.Pen.Width := 1;
  Canvas.Pen.Mode := pmMask;  // pmNotXor
  Canvas.Pen.Color := cyGraphics.ColorModify(PageColor, -40, -40, -40);

  // Draw inside shade effect :
  if PercentDone < 90 then
  begin
    for i := 1 to (PageRect.Right-PageRect.Left) div 10 + 3 do
    begin
      x := PtBottomPage.x-i;
      if x < PageRect.Left then x := PageRect.Left;
      Canvas.MoveTo(x, PtBottomPage.y);
      y := ptRightPage.y-i;
      if y < PageRect.Top then y := PageRect.Top;
      Canvas.LineTo(ptRightPage.x, y);
    end;
  end;

  // Draw outside shade effect :
  if PercentDone > 5 then
  begin
    for i := 1 to (PageRect.Right-PageRect.Left) div 40 + 1 do
    begin
      Canvas.MoveTo(PtBottomPage.x+i, PtBottomPage.y);
      Canvas.LineTo(ptRightPage.x, ptRightPage.y+i);
    end;
  end;

  // Draw polygon :
  Canvas.Pen.Mode := pmCopy;
  Canvas.Brush.Color := PageColor;
  Canvas.Polygon([PtBottomPage, ptRightPage, ptTopPageCorner, ptDraggedCorner]);
end;

end.

