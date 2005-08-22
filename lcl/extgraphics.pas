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

uses Classes, LCLProc, Graphics, math;

type
  TShapeDirection = (atUp, atDown, atLeft, atRight);

procedure PaintDiamond(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintCross(Canvas: TCanvas; XLeft,YUp,XRight,YLow,
          CrossX1,CrossX2,CrossY1,CrossY2:integer);
procedure PaintPlus(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintTriangle(Canvas: TCanvas; const PaintRect: TRect;
  AArrowDirection: TShapeDirection);
procedure PaintBoldArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowDirection: TShapeDirection);
procedure PaintChevronArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowDirection: TShapeDirection);
procedure PaintVArrow(Canvas: TCanvas; const PaintRect : TRect;
  AArrowDirection: TShapeDirection);
procedure PaintHalfEllipse(Canvas: TCanvas; Const PaintRect: TRect;
  AHalfEllipseDirection: TShapeDirection);
procedure PaintFivePointStar(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintFivePointLineStar(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintStarN(Canvas: TCanvas;cx,cy,r,n,a:Integer);


procedure CalculatePentagonPoints(const PentagonRect:TRect;var P1,P2,P3,P4,P5:TPoint);
function LinesPointOfIntersection(const Line1a,Line1b,Line2a,line2b:TPoint):TPoint;

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
 AArrowDirection :TShapeDirection);
var P:array[0..2] of TPoint;
begin
  Case AArrowDirection of
     atUp: with PaintRect do begin
       P[0].x:=Left; P[0].y:=Bottom;
       P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Top;
       P[2].x:=Right; P[2].y:= P[0].y;
     end;
     atDown: with PaintRect do begin
       P[0].x:=Left; P[0].y:=Top;
       P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Bottom;
       P[2].x:=Right; P[2].y:= P[0].y;
     end;
     atRight: with PaintRect do begin
       P[0].x:=Left; P[0].y:=Top;
       P[1].x:=Right; P[1].y:=Top+(Bottom-Top) div 2;
       P[2].x:=P[0].x; P[2].y:= Bottom;
     end;
     atLeft: with PaintRect do begin
       P[0].x:=Right; P[0].y:=Top;
       P[1].x:=Left; P[1].y:=Top+(Bottom-Top) div 2;
       P[2].x:=P[0].x; P[2].y:= Bottom;
     end;
     end;
  Canvas.Polygon(P);
end;

Procedure PaintBoldArrow(Canvas: TCanvas; const PaintRect: TRect;
  AArrowDirection :TShapeDirection);
var P:array[0..6] of TPoint;
begin
  with PaintRect do begin
    Case AArrowDirection of
      atUp:    begin
          P[2].y:= Top;
          P[5].y:= Bottom;
        end;
      atDown:    begin
          P[2].y:= Bottom;
          P[5].y:= Top;
        end;
      atRight: begin
          P[0].x:= Left;
          P[3].x:= Right;
        end;
      atLeft:  begin
          P[0].x:= Right;
          P[3].x:= Left;
        end;
    end;
    Case AArrowDirection of
      atUp, atDown: begin
          P[0].x:=Left + (Right - Left) div 4; P[0].y:=Top + (Bottom - Top) div 2;
          P[1].x:=Left; P[1].y:=P[0].y;
          P[2].x:=Left + (Right - Left) div 2;
          P[3].x:=Right; P[3].y:=P[0].y;
          P[4].x:=Right - (Right - Left) div 4; P[4].y:= P[0].y;
          P[5].x:=P[4].x;
          P[6].x:=P[0].x; P[6].y:=P[5].y;
        end;
      atRight, atLeft: begin
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
  AArrowDirection: TShapeDirection);
var P: array[0..6] of TPoint;
begin
  with PaintRect do begin
    P[0].y:=Top;
    Case AArrowDirection of
      atUp:    begin
          P[0].x:= Left+(Right - Left) div 2;
          P[1].x:= Right;
          P[2].y:= Bottom;
          P[3].x:= P[0].x;
          P[4].x:= Left;
          P[5].y:= Top+(Bottom-Top) div 3;
        end;
      atDown:    begin
          P[0].x:= Left;
          P[1].x:= Left+(Right - Left) div 2;
          P[2].y:= Top;
          P[3].x:= Right;
          P[4].x:= P[1].x;
          P[5].y:= Bottom-(Bottom-Top) div 3;
        end;
      atRight: begin
          P[0].x:= Left;
          P[1].x:= Right-(Right - Left) div 3;
          P[2].x:= Right;
          P[5].x:= Left + (Right - Left) div 3;
        end;
      atLeft:  begin
          P[0].x:= Left + (Right - Left) div 3;
          P[1].x:= Right;
          P[2].x:= Right-(Right - Left) div 3;
          P[5].x:= Left;
       end;
    end;
    Case AArrowDirection of
      atUp, atDown: begin
          P[1].y:= Top+(Bottom-Top) div 3;
          P[2].x:= Right;
          P[3].y:= Bottom-(Bottom-Top) div 3;
          P[4].y:= Bottom;
          P[5].x:= Left;
        end;
      atRight, atLeft: begin
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
  AArrowDirection :TShapeDirection);
var P:array[0..3] of TPoint;
begin
  with PaintRect do begin
    P[3].x:=Left+ (Right - Left) div 2; P[3].y:=Top+(Bottom-Top) div 2;
    Case AArrowDirection of
      atUp:  begin
          P[0].x:=Left; P[0].y:=Bottom;
          P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Top;
          P[2].x:=Right; P[2].y:= P[0].y;
        end;
      atDown:begin
          P[0].x:=Left; P[0].y:=Top;
          P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Bottom;
          P[2].x:=Right; P[2].y:= P[0].y;
        end;
      atRight: begin
          P[0].x:=Left; P[0].y:=Top;
          P[1].x:=Right; P[1].y:=Top+(Bottom-Top) div 2;
          P[2].x:=P[0].x; P[2].y:= Bottom;
        end;
      atLeft: begin
          P[0].x:=Right; P[0].y:=Top;
          P[1].x:=Left; P[1].y:=Top+(Bottom-Top) div 2;
          P[2].x:=P[0].x; P[2].y:= Bottom;
        end;
    end;
  end;
  Canvas.Polygon(P);
end;

procedure PaintHalfEllipse(Canvas: TCanvas;const PaintRect: TRect;
  AHalfEllipseDirection: TShapeDirection);
var Ex1,Ex2,Ey1,Ey2,Sx,Sy,Ex,Ey,i:integer;
begin
  Case AHalfEllipseDirection of
     atUp: with PaintRect do begin
       Ex1:=Left; Ex2:=Right;
       Ex:=Left; Sx:=Right;
       i:=Bottom-Top;
       Ey1:=Top;Ey2:=Bottom+i;
       Sy:=Top+i;Ey:=Top+i;
     end;
     atDown: with PaintRect do begin
       Ex1:=Left; Ex2:=Right;
       Sx:=Left; Ex:=Right;
       i:=Bottom-Top;
       Ey1:=Top-i;Ey2:=Bottom;
       Sy:=Top;Ey:=Top;
     end;
     atRight: with PaintRect do begin
       Ey1:=Top; Ey2:=Bottom;
       Ey:=Top; Sy:=Bottom;
       i:=Right-Left;
       Ex1:=Left-i;Ex2:=Right;
       Sx:=Left;Ex:=Left;
     end;
     atLeft: with PaintRect do begin
       Ey1:=Top; Ey2:=Bottom;
       Sy:=Top; Ey:=Bottom;
       i:=Right-Left;
       Ex1:=Left;Ex2:=Right+i;
       Sx:=Left+i;Ex:=Left+i;
     end;
   end;  ;
   Canvas.Pie(Ex1,Ey1,Ex2,Ey2,Sx,Sy,Ex,Ey);
end;

procedure PaintFivePointStar(Canvas: TCanvas; const PaintRect: TRect);
var  P: array[0..9] of TPoint;
begin
  CalculatePentagonPoints(PaintRect,P[0],P[2],P[4],P[6],P[8]);
  P[1]:=LinesPointOfIntersection(P[0],P[4],P[2],P[8]);
  P[3]:=LinesPointOfIntersection(P[0],P[4],P[2],P[6]);
  P[5]:=LinesPointOfIntersection(P[8],P[4],P[2],P[6]);
  P[7]:=LinesPointOfIntersection(P[8],P[4],P[0],P[6]);
  P[9]:=LinesPointOfIntersection(P[8],P[2],P[0],P[6]);
  Canvas.Polygon(P);
end;

procedure PaintFivePointLineStar(Canvas: TCanvas; const PaintRect: TRect);
var P: array[0..4] of TPoint;
begin
  CalculatePentagonPoints(PaintRect,P[0],P[1],P[2],P[3],P[4]);
  Canvas.Line(P[0].x,P[0].y,P[2].x,P[2].y);
  Canvas.Line(P[0].x,P[0].y,P[3].x,P[3].y);
  Canvas.Line(P[1].x,P[1].y,P[3].x,P[3].y);
  Canvas.Line(P[1].x,P[1].y,P[4].x,P[4].y);
  Canvas.Line(P[2].x,P[2].y,P[4].x,P[4].y);
end;

procedure PaintStarN(Canvas: TCanvas;cx,cy,r,n,a:Integer);
const MaxStarPoint=36;
var
  r1,r0,alpha:double;
  P:array[0..MaxStarPoint*2-1] of TPoint;
  i,cs:Integer;
begin
  r1:=r/2;
  for i:=0 to 2*n
    do begin
       if (i mod 2)=0 then r0:=r else r0:=r1;
       alpha:=a+(0.5+i/n)*Pi;
       cs:=RoundToInt(r0*cos(alpha));
       P[i].x:=cx+cs;
       P[i].y:=cy-Round(r0*sin(alpha));
    end;
  for i:=2*n to MaxStarPoint*2-1
    do begin
      P[i].x:=P[2*n-1].x;
      P[i].y:=P[2*n-1].y;
    end;
  Canvas.Polygon(P);
end;

procedure CalculatePentagonPoints(const PentagonRect:TRect;
  var P1,P2,P3,P4,P5:TPoint);
var cx,cy,dy,dx:Integer; r:real;
begin
  P1.y:=PentagonRect.Top;
  P2.x:=PentagonRect.Left;
  P3.y:=PentagonRect.Bottom;
  P4.y:=PentagonRect.Bottom;
  P5.x:=PentagonRect.Right;
  P1.x:=(PentagonRect.Right+PentagonRect.Left) div 2;
  dy:=RoundToInt((P1.x-P2.x)*tan(Pi/10));
  r := sqrt(dy*dy+(P1.x-P2.x)*(P1.x-P2.x));
  cx:=P1.x;
  cy:=P1.y+round(r);
  P2.y:=cy-dy;
  P5.y:=P2.y;
  dx:=RoundToInt(r*sin(Pi/5));
  P3.x:=cx-dx;
  P4.x:=cx+dx;
end;

function LinesPointOfIntersection(const Line1a,Line1b,Line2a,line2b:TPoint):TPoint;
var k1,k2,b1,b2,x,x1,x2,x3,x4,y,y1,y2,y3,y4:real;
     p:TPoint;
begin
  x1:=Line1a.x;  y1:=Line1a.y;
  x2:=Line1b.x;  y2:=Line1b.y;
  x3:=Line2a.x;  y3:=Line2a.y;
  x4:=Line2b.x;  y4:=Line2b.y;
  k1:=(y2-y1)/(x2-x1);
  k2:=(y4-y3)/(x4-x3);
  b1:=-k1*x1+y1;
  b2:=-k2*x3+y3;
  x:=(b1-b2)/(k2-k1);
  y:=(k2*b1-k1*b2)/(k2-k1);
  p.x:=RoundToInt(x);
  p.y:=RoundToInt(y);
  LinesPointOfIntersection:=p;
end;

 
end.



