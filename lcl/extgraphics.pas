{  $Id$  }
{
 /***************************************************************************
                              extgraphics.pas
                              ---------------

 ***************************************************************************/

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
unit ExtGraphics;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics;

type
  TArrowDirection = (atUp, atDown, atLeft, atRight);

procedure PaintDiamond(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintCross(Canvas: TCanvas; XLeft,YUp,XRight,YLow,
          CrossX1,CrossX2,CrossY1,CrossY2:integer);
procedure PaintPlus(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintTriangle(Canvas: TCanvas; const PaintRect: TRect;
  AArrowType: TArrowDirection);
procedure PaintBoldArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowType: TArrowDirection);
procedure PaintChevronArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowType: TArrowDirection);
procedure PaintVArrow(Canvas: TCanvas; const PaintRect : TRect;
  AArrowType: TArrowDirection);

implementation

procedure PaintDiamond(Canvas: TCanvas; const PaintRect: TRect);
var
  P: array[0..3] of TPoint;
begin
  with PaintRect do begin
    P[0].x:=Left; P[0].y:=Top + (Bottom - Top) div 2;
    P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Bottom;
    P[2].x:=Right; P[2].y:= P[0].y;
    P[3].x:=P[1].x; P[3].y:=Top;
    Canvas.Polygon(P);
  end;
end;

procedure PaintCross(Canvas: TCanvas; XLeft,YUp,XRight,YLow,
          CrossX1,CrossX2,CrossY1,CrossY2:integer);
var P:array[0..12] of TPoint;
begin
  P[ 0].x:=XLeft;   P[ 0].y:=CrossY1;
  P[ 1].x:=CrossX1; P[ 1].y:=P[0].y;
  P[ 2].x:=P[ 1].x; P[ 2].y:= YUp;
  P[ 3].x:=CrossX2; P[ 3].y:=P[2].y;
  P[ 4].x:=P[ 3].x; P[ 4].y:=CrossY1;
  P[ 5].x:=XRight;  P[ 5].y:=P[4].y;
  P[ 6].x:=P[ 5].x; P[ 6].y:=CrossY2;
  P[ 7].x:=CrossX2; P[ 7].y:=P[6].y;
  P[ 8].x:=P[ 7].x; P[ 8].y:=YLow;
  P[ 9].x:=CrossX1; P[ 9].y:=P[8].y;
  P[10].x:=P[ 9].x; P[10].y:=CrossY2;
  P[11].x:=XLeft;   P[11].y:=P[10].y;
  P[12].x:=P[11].x; P[12].y:=CrossY1;
  Canvas.Polygon(P);
end;


procedure PaintPlus(Canvas: TCanvas; const PaintRect: TRect);
var      CrossX1,CrossX2,CrossY1,CrossY2:integer;
begin
  with PaintRect do begin
    CrossX1:=Left+(Right-Left)  div 3 ;
    CrossX2:=Left+(Right-Left) * 2 div 3;
    CrossY1:=Top+(Bottom-Top) div 3 ;
    CrossY2:=Top+(Bottom-Top) * 2 div 3 ;
    PaintCross(Canvas,Left,Top,Right,Bottom,CrossX1,CrossX2,CrossY1,CrossY2);
  end;
end; 

Procedure PaintTriangle(Canvas: TCanvas; const PaintRect: TRect;
 AArrowType :TArrowDirection);
var P:array[0..2] of TPoint;
begin
  Case AArrowType of
     AtUp: with PaintRect do begin
       P[0].x:=Left; P[0].y:=Bottom;
       P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Top;
       P[2].x:=Right; P[2].y:= P[0].y;
     end;
     AtDown: with PaintRect do begin
       P[0].x:=Left; P[0].y:=Top;
       P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Bottom;
       P[2].x:=Right; P[2].y:= P[0].y;
     end;
     AtRight: with PaintRect do begin
       P[0].x:=Left; P[0].y:=Top;
       P[1].x:=Right; P[1].y:=Top+(Bottom-Top) div 2;
       P[2].x:=P[0].x; P[2].y:= Bottom;
     end;
     AtLeft: with PaintRect do begin
       P[0].x:=Right; P[0].y:=Top;
       P[1].x:=Left; P[1].y:=Top+(Bottom-Top) div 2;
       P[2].x:=P[0].x; P[2].y:= Bottom;
     end;
     end;
  Canvas.Polygon(P);
end;

Procedure PaintBoldArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowType :TArrowDirection);
var P:array[0..6] of TPoint;
begin
  with PaintRect do begin
    Case AArrowType of
      AtUp:    begin
          P[2].y:= Top;
          P[5].y:= Bottom;
        end;
      AtDown:    begin
          P[2].y:= Bottom;
          P[5].y:= Top;
        end;
      AtRight: begin
          P[0].x:= Left;
          P[3].x:= Right;
        end;
      AtLeft:  begin
          P[0].x:= Right;
          P[3].x:= Left;
        end;
    end;
    Case AArrowType of
      AtUp, AtDown: begin
          P[0].x:=Left + (Right - Left) div 4; P[0].y:=Top + (Bottom - Top) div 2;
          P[1].x:=Left; P[1].y:=P[0].y;
          P[2].x:=Left + (Right - Left) div 2;
          P[3].x:=Right; P[3].y:=P[0].y;
          P[4].x:=Right - (Right - Left) div 4; P[4].y:= P[0].y;
          P[5].x:=P[4].x;
          P[6].x:=P[0].x; P[6].y:=P[5].y;
        end;
      AtRight, AtLeft: begin
          P[0].y:=Top+(Bottom-Top) div 4;
          P[1].x:=Left + (Right - Left) div 2; P[1].y:=P[0].y;
          P[2].x:=P[1].x; P[2].y:= Top;
          P[3].y:=Top + (Bottom - Top) div 2;
          P[4].x:=P[1].x; P[4].y:= Bottom;
          P[5].x:=P[1].x; P[5].y:=Bottom-(Bottom-Top) div 4;
          P[6].x:=P[0].x; P[6].y:=P[5].y;
        end;
    end;
    Canvas.Polygon(P);
  end;
end; 

Procedure PaintChevronArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowType: TArrowDirection);
var P: array[0..6] of TPoint;
begin
  with PaintRect do begin
    P[0].y:=Top;
    Case AArrowType of
      AtUp:    begin
          P[2].y:= Top;
          P[5].y:= Bottom;
        end;
      AtDown:    begin
          P[2].y:= Bottom;
          P[5].y:= Top;
        end;
      AtRight: begin
          P[0].x:= Left;
          P[1].x:= Right-(Right - Left) div 3;
          P[2].x:= Right;
          P[5].x:= Left + (Right - Left) div 3;
        end;
      AtLeft:  begin
          P[0].x:= Left + (Right - Left) div 3;
          P[1].x:= Right;
          P[2].x:= Right-(Right - Left) div 3;
          P[5].x:= Left;
       end;
    end;
    Case AArrowType of
      AtUp, AtDown: begin
          P[0].x:=Left + (Right - Left) div 4; P[0].y:=Top + (Bottom - Top) div 2;
          P[1].x:=Left; P[1].y:=P[0].y;
          P[2].x:=Left + (Right - Left) div 2;
          P[3].x:=Right; P[3].y:=P[0].y;
          P[4].x:=Right - (Right - Left) div 4; P[4].y:= P[0].y;
          P[5].x:=P[4].x;
          P[6].x:=P[0].x; P[6].y:=P[5].y;
        end;
      AtRight, AtLeft: begin
          P[1].y:=P[0].y;
          P[2].y:= Bottom-(Bottom-Top) div 2;
          P[3].x:=P[1].x;
          P[3].y:=Bottom;
          P[4].x:=P[0].x; P[4].y:= P[3].y;
          P[5].y:=P[2].y;
        end;
    end;
    Canvas.Polygon(P);
  end;
end;


Procedure PaintVArrow(Canvas: TCanvas; const PaintRect : TRect;
  AArrowType :TArrowDirection);
var P:array[0..3] of TPoint;
begin
  with PaintRect do begin
    P[3].x:=Left+ (Right - Left) div 2; P[3].y:=Top+(Bottom-Top) div 2;
    Case AArrowType of
      AtUp:  begin
          P[0].x:=Left; P[0].y:=Bottom;
          P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Top;
          P[2].x:=Right; P[2].y:= P[0].y;
        end;
      AtDown:begin
          P[0].x:=Left; P[0].y:=Top;
          P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Bottom;
          P[2].x:=Right; P[2].y:= P[0].y;
        end;
      AtRight: begin
          P[0].x:=Left; P[0].y:=Top;
          P[1].x:=Right; P[1].y:=Top+(Bottom-Top) div 2;
          P[2].x:=P[0].x; P[2].y:= Bottom;
        end;
      AtLeft: begin
          P[0].x:=Right; P[0].y:=Top;
          P[1].x:=Left; P[1].y:=Top+(Bottom-Top) div 2;
          P[2].x:=P[0].x; P[2].y:= Bottom;
        end;
    end;
  end;
  Canvas.Polygon(P);
end;
 
 
end.
