{  $Id$  }
{
 /***************************************************************************
                              extgraphics.pas
                              ---------------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ExtGraphics;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, Math,
  // LazUtils
  LazUtilities, GraphMath,
  // LCL
  Graphics;

type
  TShapeDirection = (atUp, atDown, atLeft, atRight);
  TInitShapeProc = procedure(var P: array of TPoint;const R: TRect;
    var NumPts: Integer);


procedure Paint2HeadArrow(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintBarbadosTrident(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintBigI(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintBoldArrow(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintCanadianMaple(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintChevronArrow(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintFivePointStar(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintHexagon(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintNotchedArrow(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintOctogon(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintPentagon(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintPlus(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintQuadrangle(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintRightTriangle(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintTriangle(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0);
procedure PaintTriangular(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0; RightLeftFactor:extended=0.5);
procedure PaintValve(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle: Extended = 0.0);
procedure PaintVArrow(Canvas: TCanvas; const PaintRect : TRect;
  RadAngle :Extended=0.0);



procedure PaintCross(Canvas: TCanvas; XLeft,YUp,XRight,YLow,
          CrossX1,CrossX2,CrossY1,CrossY2:integer);
procedure PaintHalfEllipse(Canvas: TCanvas; Const PaintRect: TRect;
  AHalfEllipseDirection: TShapeDirection);
procedure PaintFivePointLineStar(Canvas: TCanvas; const PaintRect: TRect);
procedure PaintStarN(Canvas: TCanvas;cx,cy,r,n,a:Integer);


procedure InitPolygon(Canvas: TCanvas;PaintRect: TRect;RadAngle: Extended;
  InitShapeProc: TInitShapeProc);

procedure CalculatePentagonPoints
  (const PentagonRect:TRect; var P1,P2,P3,P4,P5:TPoint);
function LinesPointOfIntersection
  (const Line1a,Line1b,Line2a,line2b:TPoint):TPoint;


implementation

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


function LinesPointOfIntersection
  (const Line1a,Line1b,Line2a,line2b:TPoint):TPoint;
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

procedure PolycRotate
  (var Pts:array of TPoint; CountPts:Integer; cntPoint:TPoint; fii:Extended);
var i,dx,dy:Integer;
    x,y:Extended;
begin
  for i:=0 to CountPts-1 do
    begin
      dx:=Pts[i].x-cntPoint.x;
      dy:=Pts[i].y-cntPoint.y;
      x:=dx*cos(fii)+dy*sin(fii);
      y:=dy*cos(fii)-dx*sin(fii);
      Pts[i].x:=cntPoint.x+Round(x);
      Pts[i].y:=cntPoint.y+Round(y);
    end;
end;


procedure PolycMinMax
  (var N:array of TPoint; const P:array of TPoint; CountPts:Integer);
var i,Xmin,Xmax,Ymin,Ymax:Integer;
begin
  Xmin:=P[0].x; Xmax:=P[0].x; Ymin:=P[0].y; Ymax:=P[0].y;
  for i:=0 to CountPts-1 do
    begin
      if P[i].x<Xmin then Xmin:=P[i].x;
      if P[i].x>Xmax then Xmax:=P[i].x;
      if P[i].y<Ymin then Ymin:=P[i].y;
      if P[i].y>Ymax then Ymax:=P[i].y;
    end;
  N[0]:=Point(Xmin,Ymin); N[1]:=Point(Xmin,Ymax);
  N[2]:=Point(Xmax,Ymax); N[3]:=Point(Xmax,Ymin);
end;


procedure PolycNewPaintRect(var PR:TRect; cP:TPoint; wv,hv:Integer);
begin
  PR.Left:=cP.x-wv;
  PR.Right:=cP.x+wv;
  PR.Top:=cP.y-hv;
  PR.Bottom:=cP.y+hv;
end;

procedure PolycFixCenterpoint
  (var N:array Of TPoint; cP:TPoint; var P:array Of TPoint; CountPts:Integer);
var i,nx,ny,dx,dy:Integer;
begin
  nx:=(N[0].x+N[2].x) div 2;
  ny:=(N[0].y+N[2].y) div 2;
  dx:=cP.x-nx;
  dy:=cP.y-ny;
  for i:=0 to 3 do
    begin
      N[i].x:=N[i].x+dx;
      N[i].y:=N[i].y+dy;
    end;
  for i:=0 to CountPts-1 do
    begin
      P[i].x:=P[i].x+dx;
      P[i].y:=P[i].y+dy;
    end;
end;

procedure PolycSetHalfWidthAndHeight
  (const PR:TRect;var hv,wv:Integer;fii:Extended);
var h,w:Integer;
begin
  h:=PR.Bottom-PR.Top;
  w:=PR.Right-PR.Left;
  hv:=Round(h*abs(cos(fii))+w*abs(sin(fii))) div 2;
  wv:=Round(h*abs(sin(fii))+w*abs(cos(fii))) div 2;
end;

procedure PolycScale(var P:array of TPoint; CountPts:Integer;
  const PaintRect:TRect; cntPoint:TPoint; N:array of TPoint);
var k,kx,ky:Extended;
    i:Integer;
begin
  kx:=(PaintRect.Right-PaintRect.Left)/(N[2].x-N[0].x);
  ky:=(PaintRect.Bottom-PaintRect.Top)/(N[2].y-N[0].y);
  k:=min(kx,ky);
  for i:=0 to CountPts-1 do
    begin
      P[i].x:=cntPoint.x+Round(k*(P[i].x-cntPoint.x));
      P[i].y:=cntPoint.y+Round(k*(P[i].y-cntPoint.y));
    end;
end;




procedure PaintPolycon(Canvas: TCanvas;PR : TRect; fii :Extended;
  P:array of TPoint; CountPts:Integer;cntPoint:TPoint);
var     N:array[0..3] of TPoint;

begin
  PolycRotate(P,CountPts,cntPoint,fii);
  PolycMinMax(N,P,CountPts);
  PolycFixCenterpoint(N,cntPoint,P,CountPts);
  PolycScale(P,CountPts,PR,cntPoint,N);
  Case CountPts of
    3: Canvas.Polygon([P[0],P[1],P[2]]);
    4: Canvas.Polygon([P[0],P[1],P[2],P[3]]);
    5: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4]]);
    6: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5]]);
    7: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6]]);
    8: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7]]);
    9: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8]]);
    10: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9]]);
    11: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9],
               P[10]]);
    12: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9],
               P[10],P[11]]);
    13: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9],
               P[10],P[11],P[12]]);

    20: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9],
               P[10],P[11],P[12],P[13],P[14],P[15],P[16],P[17],P[18],P[19]]);
    33: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9],
               P[10],P[11],P[12],P[13],P[14],P[15],P[16],P[17],P[18],P[19],
               P[20],P[21],P[22],P[23],P[24],P[25],P[26],P[27],P[28],P[29],
               P[30],P[31],P[32]]);
    35: Canvas.Polygon([P[0],P[1],P[2],P[3],P[4],P[5],P[6],P[7],P[8],P[9],
               P[10],P[11],P[12],P[13],P[14],P[15],P[16],P[17],P[18],P[19],
               P[20],P[21],P[22],P[23],P[24],P[25],P[26],P[27],P[28],P[29],
               P[30],P[31],P[32],P[33],P[34]]);

  end;
end;


procedure InitPolygon(Canvas: TCanvas;PaintRect: TRect;RadAngle: Extended;
  InitShapeProc: TInitShapeProc);
var
  PR, vPR: TRect;
  P: array[0..35] of TPoint;
  CountPts, hv, wv: Integer;
  cntPoint: TPoint;
begin
  PR := PaintRect;
  cntPoint := CenterPoint(PR);
  PolycSetHalfWidthAndHeight(PR, hv, wv, RadAngle);
  PolycNewPaintRect(vPR, cntPoint, wv, hv);
  InitShapeProc(P, vPR, CountPts);
  PaintPolycon(Canvas, PR, RadAngle, P, CountPts, cntPoint);
end;

procedure Init2HeadArrow
  (var P:array of TPoint;const R:TRect;var NumPts:Integer);
var dx,dy:Integer;
begin
  dx:=(R.Right-R.Left)div 4;
  dy:=(R.Bottom-R.Top)div 4;
  P[0].x:=R.Left;    P[0].y:=R.Top+(R.Bottom-R.Top) div 2;
  P[1].x:=R.Left+dx; P[1].y:=R.Top;
  P[2].x:=P[1].x;  P[2].y:=R.Top+dy;
  P[3].x:=R.Right-dx;P[3].y:=P[2].y;
  P[4].x:=P[3].x;  P[4].y:= R.Top;
  P[5].x:=R.Right;   P[5].y:=P[0].y;
  P[6].x:=P[3].x;  P[6].y:=R.Bottom;
  P[7].x:=P[3].x;  P[7].y:=R.Bottom-dy;
  P[8].x:=P[1].x;  P[8].y:=P[7].y;
  P[9].x:=P[1].x;  P[9].y:=R.Bottom;
  NumPts:=10;
end;

procedure InitBarbadosTrident
  (var P:array of TPoint; const R: TRect; var NumPts:Integer);
var RmLpW,BmTpH:extended;cntPoint:TPoint;
begin
  cntPoint:=CenterPoint(R);
  RmLpW:=(R.Right-R.Left)/140;
  BmTpH:=(R.Bottom-R.Top)/160;
  P[0].x:=cntPoint.x-round(RmLpW*10); P[0].y:=R.Bottom;
  P[34].x:=cntPoint.x+round(RmLpW*10);P[34].y:=P[0].y;
  P[1].x:=P[0].x;   P[1].y:=R.Bottom-round(BmTpH*50);
  P[33].x:=P[34].x; P[33].y:=P[1].y;
  P[2].x:=cntPoint.x-round(RmLpW*35); P[2].y:=P[1].y;
  P[32].x:=cntPoint.x+round(RmLpW*35);P[32].y:=P[2].y;
  P[3].x:=cntPoint.x-round(RmLpW*48); P[3].y:=R.Bottom-round(BmTpH*98);
  P[31].x:=cntPoint.x+round(RmLpW*48);P[31].y:=P[3].y;
  P[4].x:=R.left;     P[4].y:=R.top;
  P[30].x:=R.Right;   P[30].y:=P[4].y;
  P[5].x:=cntPoint.x-round(RmLpW*42); P[5].y:=R.Top+round(BmTpH*4);
  P[29].x:=cntPoint.x+round(RmLpW*42);P[29].y:=P[5].y;
  P[6].x:=cntPoint.x-round(RmLpW*40); P[6].y:=R.Top+round(BmTpH*6);
  P[28].x:=cntPoint.x+round(RmLpW*40);P[28].y:=P[6].y;
  P[7].x:=cntPoint.x-round(RmLpW*39); P[7].y:=R.Top+round(BmTpH*11);
  P[27].x:=cntPoint.x+round(RmLpW*39);P[27].y:=P[7].y;
  P[8].x:=cntPoint.x-round(RmLpW*45); P[8].y:=R.Top+round(BmTpH*16);
  P[26].x:=cntPoint.x+round(RmLpW*45);P[26].y:=P[8].y;
  P[9].x:=cntPoint.x-round(RmLpW*45); P[9].y:=R.Top+round(BmTpH*21);
  P[25].x:=cntPoint.x+round(RmLpW*45);P[25].y:=P[9].y;
  P[10].x:=cntPoint.x-round(RmLpW*32);P[10].y:=R.Top+round(BmTpH*47);
  P[24].x:=cntPoint.x+round(RmLpW*32);P[24].y:=P[10].y;
  P[11].x:=cntPoint.x-round(RmLpW*28);P[11].y:=R.Top+round(BmTpH*70);
  P[23].x:=cntPoint.x+round(RmLpW*28);P[23].y:=P[11].y;
  P[12].x:=cntPoint.x-round(RmLpW*22);P[12].y:=R.Top+round(BmTpH*92);
  P[22].x:=cntPoint.x+round(RmLpW*22);P[22].y:=P[12].y;
  P[13].x:=P[0].x;  P[13].y:=P[12].y;
  P[21].x:=P[34].x; P[21].y:=P[13].y;
  P[14].x:=P[0].x;  P[14].y:=R.Top+round(BmTpH*30);
  P[20].x:=P[34].x; P[20].y:=P[14].y;
  P[15].x:=cntPoint.x-round(RmLpW*22);P[15].y:=R.Top+round(BmTpH*22);
  P[19].x:=cntPoint.x+round(RmLpW*22);P[19].y:=P[15].y;
  P[16].x:=cntPoint.x-round(RmLpW*9); P[16].y:=R.Top+round(BmTpH*12);
  P[18].x:=cntPoint.x+round(RmLpW*9); P[18].y:=P[16].y;
  P[17].x:=cntPoint.x;   P[17].y:=R.Top;

  NumPts:=35;
end;

procedure InitBigI(var P:array of TPoint; const R: TRect; var NumPts:Integer);
var dx,dy:Integer;
begin
  dx:=(R.Right-R.Left) div 4;
  dy:=(R.Bottom-R.Top) div 18;
  P[0].x:=R.Left; P[0].y:=R.Top;
  P[1].x:=R.Right; P[1].y:=R.TOP;
  P[2].x:=R.Right-dx; P[2].y:=R.Top+dy;
  P[3].x:=P[2].x; P[3].y:=R.Bottom-dy;
  P[4].x:=R.Right;  P[4].y:=R.Bottom;
  P[5].x:=R.Left;   P[5].y:=R.Bottom;
  P[6].x:=R.Left+dx;P[6].y:=P[3].y;
  P[7].x:=P[6].x;P[7].y:=P[2].y;
  NumPts:=8;
end;

procedure InitBoldArrow(var P:array of TPoint;const R:TRect;var NumPts:Integer);
var dy:Integer;cntPoint:TPoint;
begin
  cntPoint:=CenterPoint(R);
  dy:=(R.Bottom - R.Top) div 4;
  P[0].x:=R.Left;
  P[0].y:=R.Top+dy;
  P[1].x:=cntPoint.x;
  P[1].y:=P[0].y;
  P[2].x:=cntPoint.x;
  P[2].y:=R.Top;
  P[3].x:=R.Right;
  P[3].y:=cntPoint.y;
  P[4].x:=cntPoint.x;
  P[4].y:= R.Bottom;
  P[5].x:= cntPoint.x;
  P[5].y:=R.Bottom-dy;
  P[6].x:= R.Left;
  P[6].y:=P[5].y;
  NumPts:=7;
end;

procedure InitCanadianMaple
  (var P:array of TPoint;const R:TRect; var NumPts:Integer);
const leafheight=54;      leafwidth=50;
var xcenter,x2:integer;
  RmLpLW, //  (Right - Left)/LeafWidth;
  BmTpLH //(Bottom-Top)/ LeafHeight
  :extended;
begin
  xcenter:=R.Left+(R.Right - R.Left) div 2;
  p[0].y:=R.Top;
  p[0].x:=xcenter;
  RmLpLW:=(R.Right - R.Left)/LeafWidth;
  BmTpLH:=(R.Bottom-R.Top)/ LeafHeight;
  x2:=RoundToInt(RmLpLW*5);
  P[1].x:=xcenter-x2; P[1].y:=RoundToInt(BmTpLH*9+R.Top);
  P[32].x:=xcenter+x2; P[32].y:=P[1].y;
  x2:=RoundToInt(RmLpLW*10);
  P[2].x:=xcenter-x2; P[2].y:=RoundToInt(BmTpLH *7+R.Top);
  P[31].x:=xcenter+x2; P[31].y:=P[2].y;
  x2:=RoundToInt(RmLpLW*7);
  P[3].x:=xcenter-x2; P[3].y:=RoundToInt(BmTpLH*21+R.Top);
  P[30].x:=xcenter+x2; P[30].y:=P[3].y;
  x2:=RoundToInt(RmLpLW*9);
  P[4].x:=xcenter-x2; P[4].y:=P[3].y;
  P[29].x:=xcenter+x2; P[29].y:=P[3].y;
  x2:=RoundtoInt(RmLpLW*15);
  P[5].x:=xcenter-x2; P[5].y:=RoundtoInt(BmTpLH*15+R.Top);
  P[28].x:=xcenter+x2; P[28].y:=P[5].y;
  x2:=RoundtoInt(RmLpLW*17);
  P[6].x:=xcenter-x2; P[6].y:=RoundtoInt(BmTpLH*19+R.Top);
  P[27].x:=xcenter+x2; P[27].y:=P[6].y;
  x2:=RoundtoInt(RmLpLW*24);
  P[7].x:=xcenter-x2; P[7].y:=RoundtoInt(BmTpLH*17+R.Top);
  P[26].x:=xcenter+x2; P[26].y:=P[7].y;
  x2:=RoundtoInt(RmLpLW*22);
  P[8].x:=xcenter-x2; P[8].y:=RoundtoInt(BmTpLH*26+R.Top);
  P[25].x:=xcenter+x2; P[25].y:=P[8].y;
  x2:=RoundtoInt(RmLpLW*25);
  P[9].x:=xcenter-x2; P[9].y:=RoundtoInt(BmTpLH*28+R.Top);
  P[24].x:=xcenter+x2; P[24].y:=P[9].y;
  x2:=RoundtoInt(RmLpLW*14);
  P[10].x:=xcenter-x2; P[10].y:=RoundtoInt(BmTpLH*38+R.Top);
  P[23].x:=xcenter+x2; P[23].y:=P[10].y;
  x2:=RoundtoInt(RmLpLW*15);
  P[11].x:=xcenter-x2; P[11].y:=RoundtoInt(BmTpLH*43+R.Top);
  P[22].x:=xcenter+x2; P[22].y:=P[11].y;
  x2:=RoundtoInt(RmLpLW);
  P[12].x:=xcenter-x2; P[12].y:=RoundtoInt(BmTpLH*41+R.Top);
  P[21].x:=xcenter+x2; P[21].y:=P[12].y;
  x2:=RoundtoInt(RmLpLW/2);
  P[13].x:=xcenter-x2; P[13].y:=RoundtoInt(BmTpLH*42+R.Top);
  P[20].x:=xcenter+x2; P[20].y:=P[13].y;
  P[14].x:=P[13].x; P[14].y:=RoundtoInt(BmTpLH*47+R.Top);
  P[19].x:=P[20].x; P[19].y:=P[14].y;
  x2:=RoundtoInt(RmLpLW);
  P[15].x:=xcenter-x2; P[15].y:=P[14].y;
  P[18].x:=xcenter+x2; P[18].y:=P[14].y;
  P[16].x:=P[15].x; P[16].y:=R.bottom;
  P[17].x:=P[18].x; P[17].y:=R.bottom;
  NumPts:=33;
end;

procedure InitChevronArrow
  (var P:array of TPoint;const R:TRect; var NumPts:Integer);
var dx:Integer;
begin
  dx:=(R.Right - R.Left) div 3;
  P[0].x:=R.Left;
  P[0].y:=R.Top;
  P[1].x:= R.Right-dx;
  P[1].y:=R.Top;
  P[2].x:=R.Right;
  P[2].y:=(R.Top+R.Bottom) div 2;
  P[3].x:=P[1].x;
  P[3].y:=R.Bottom;
  P[4].x:=R.Left;
  P[4].y:= R.Bottom;
  P[5].x:= R.Left+dx;
  P[5].y:=P[2].y;
  NumPts:=6;
end;


procedure InitFivePointStar
  ( var P:array of TPoint;const R: TRect;var NumPts:Integer);
begin
  CalculatePentagonPoints(R,P[0],P[2],P[4],P[6],P[8]);
  P[1]:=LinesPointOfIntersection(P[0],P[4],P[2],P[8]);
  P[3]:=LinesPointOfIntersection(P[0],P[4],P[2],P[6]);
  P[5]:=LinesPointOfIntersection(P[8],P[4],P[2],P[6]);
  P[7]:=LinesPointOfIntersection(P[8],P[4],P[0],P[6]);
  P[9]:=LinesPointOfIntersection(P[8],P[2],P[0],P[6]);
  NumPts:=10;
end;


procedure InitHexagon(var P:array of TPoint;const R: TRect;var NumPts:Integer);
var dx:Integer;
begin
  dx:=round(((R.Right - R.Left) /2*cos(DegToRad(15)))/2);
  P[0].x:=R.Left+dx; P[0].y:=R.Top;
  P[1].x:=R.Left; P[1].y:=(R.Top+R.Bottom) div 2;
  P[2].x:=P[0].x; P[2].y:= R.Bottom;
  P[3].x:=R.Right-dx; P[3].y:=R.Bottom;
  P[4].x:=R.Right; P[4].y:=P[1].y;
  P[5].x:=R.Right-dx; P[5].y:=R.Top;
  NumPts:=6;
end;

procedure InitNotchedArrow
  (var P:array of TPoint;const R:TRect;var NumPts:Integer);
begin
  InitBoldArrow(P,R,NumPts);
  P[7].x:=R.Left+(R.Right-R.Left) div 4;
  P[7].y:=P[3].y; // centerpoint y
  NumPts:=8;
end;

procedure InitOctogon(var P:array of TPoint;const R: TRect;var NumPts:Integer);
var dx,dy:Integer;
begin
  dx:=R.Right - R.Left;
  dx:=round((dx-dx/(sqrt(2)+1))/2);
  dy:=R.Bottom - R.Top;
  dy:=round((dy-dy/(sqrt(2)+1))/2);
  P[0].x:=R.Left+dx; P[0].y:=R.Top;
  P[1].x:=R.Right-dx;P[1].y:=R.Top;
  P[2].x:=R.Right;   P[2].y:= R.Top+dy;
  P[3].x:=R.Right;   P[3].y:=R.Bottom-dy;
  P[4].x:=P[1].x;  P[4].y:=R.Bottom;
  P[5].x:=P[0].x;  P[5].y:=R.Bottom;
  P[6].x:=R.Left;    P[6].y:= P[3].y;
  P[7].x:=R.Left;    P[7].y:= P[2].y;
  NumPts:=8;
end;

procedure InitPentagon(var P:array of TPoint;const R: TRect;var NumPts:Integer);
begin
  CalculatePentagonPoints(R,P[0],P[1],P[2],P[3],P[4]);
  NumPts:=5;
end;

procedure InitPlus(var P:array of TPoint;const R: TRect;var NumPts:Integer);
var CrossX,Crossy:integer;
begin
  CrossX:=(R.Right-R.Left)  div 3 ;
  CrossY:=(R.Bottom-R.Top) div 3 ;
  P[ 0].x:=R.Left;    P[ 0].y:=R.Top+CrossY;
  P[ 1].x:=R.Left+CrossX; P[ 1].y:=P[0].y;
  P[ 2].x:=P[ 1].x; P[ 2].y:= R.Top;
  P[ 3].x:=R.Right-CrossX; P[ 3].y:=P[2].y;
  P[ 4].x:=P[ 3].x; P[ 4].y:=P[ 0].y;
  P[ 5].x:=R.Right;   P[ 5].y:=P[4].y;
  P[ 6].x:=P[ 5].x; P[ 6].y:=R.Bottom-CrossY;
  P[ 7].x:=P[ 3].x; P[ 7].y:=P[6].y;
  P[ 8].x:=P[ 7].x; P[ 8].y:=R.Bottom;
  P[ 9].x:=P[ 1].x; P[ 9].y:=P[8].y;
  P[10].x:=P[ 9].x; P[10].y:=P[ 6].y;
  P[11].x:=R.Left;    P[11].y:=P[10].y;
  P[12].x:=P[11].x; P[12].y:=P[ 0].y;
  NumPts:=13;
end;

procedure InitQuadrangle
  (var P:array of TPoint;const R: TRect;var NumPts:Integer);
begin
  P[0].x:=R.Left; P[0].y:=R.Top;
  P[1].x:=R.Left; P[1].y:=R.Bottom;
  P[2].x:=R.Right; P[2].y:= R.Bottom;
  P[3].x:=R.Right; P[3].y:=R.Top;
  NumPts:=4;
end;

procedure InitRightTriangle
  (var P:array of TPoint; const R: TRect; var NumPts:Integer);
begin
  P[0].x:=R.Left; P[0].y:=R.Top;
  P[1].x:=R.Right; P[1].y:=R.Bottom;
  P[2].x:=P[0].x; P[2].y:= R.Bottom;
  NumPts:=3;
end;

procedure InitTriangle(var P:array of TPoint; const R: TRect;
  var NumPts:Integer);
begin
  P[0].x:=R.Left; P[0].y:=R.Top;
  P[1].x:=R.Right; P[1].y:=R.Top+(R.Bottom-R.Top) div 2;
  P[2].x:=P[0].x; P[2].y:= R.Bottom;
  NumPts:=3;
end;

procedure InitValve(var P: array of TPoint; const R: TRect; var NumPts: Integer);
var
  cntPoint: TPoint;
begin
  cntPoint := CenterPoint(R);
  P[0].x := R.Left;
  P[0].y := R.Top;
  P[1].x := cntPoint.x;
  P[1].y := cntPoint.y;
  P[2].x := R.Right;
  P[2].y := R.Top;
  P[3].x := R.Right;
  P[3].y := R.Bottom;
  P[4].x := cntPoint.x;
  P[4].y := cntPoint.y;
  P[5].x := R.Left;
  P[5].y := R.Bottom;
  NumPts := 6;
end;

procedure InitVArrow(var P:array of TPoint;const R:TRect; var NumPts:Integer);
var  cntPoint:TPoint;
begin
  cntPoint:=CenterPoint(R);
  P[0].x:=R.Left;
  P[0].y:=R.Top;
  P[1].x:=R.Right;
  P[1].y:=cntPoint.y;
  P[2].x:=R.Left;
  P[2].y:=R.Bottom;
  P[3].x:=cntPoint.x;
  P[3].y:=cntPoint.y;
  NumPts:=4;
end;



procedure Paint2HeadArrow(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@Init2HeadArrow);
end;

procedure PaintBarbadosTrident(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitBarbadosTrident);
end;

procedure PaintBigI(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitBigI);
end;

procedure PaintBoldArrow(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitBoldArrow);
end;

procedure PaintCanadianMaple(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitCanadianMaple);
end;

procedure PaintChevronArrow(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitChevronArrow);
end;

procedure PaintFivePointStar(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitFivePointStar);
end;

procedure PaintHexagon(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitHexagon);
end;

procedure PaintNotchedArrow(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitNotchedArrow);
end;

procedure PaintOctogon(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitOctogon);
end;

procedure PaintPentagon(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitPentagon);
end;

procedure PaintPlus(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitPlus);
end;

procedure PaintQuadrangle(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitQuadrangle);
end;

procedure PaintRightTriangle(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitRightTriangle);
end;

procedure PaintTriangle(Canvas: TCanvas; const PaintRect: TRect;RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitTriangle);
end;

procedure PaintValve(Canvas: TCanvas; const PaintRect: TRect; RadAngle: Extended = 0.0);
begin
  InitPolygon(Canvas, PaintRect, RadAngle, @InitValve);
end;

procedure PaintVArrow(Canvas: TCanvas; const PaintRect : TRect; RadAngle :Extended=0.0);
begin
  InitPolygon(Canvas,PaintRect,RadAngle,@InitVArrow);
end;

procedure PaintTriangular(Canvas: TCanvas; const PaintRect: TRect;
  RadAngle :Extended=0.0; RightLeftFactor:Extended=0.5);
var PR,vPR:TRect;
    P:array[0..35] of TPoint;
    CountPts,hv,wv:Integer;
    cntPoint:TPoint;
begin
  PR:=PaintRect;
  cntPoint:=CenterPoint(PR);
  PolycSetHalfWidthAndHeight(PR,hv,wv,RadAngle);
  PolycNewPaintRect(vPR,cntPoint,wv,hv);

  P[0].x:=vPR.Left; P[0].y:=vPR.Bottom;
  P[1].x:=vPR.Left+round((vPR.Right-vPR.left)* RightLeftFactor); P[1].y:=vPR.Top;
  P[2].x:=vPR.Right; P[2].y:= vPR.Bottom;
  CountPts:=3;
  
  PaintPolycon(Canvas,PR,RadAngle,P,CountPts,cntPoint);
end;




procedure PaintHalfEllipse(Canvas: TCanvas;const PaintRect: TRect;
  AHalfEllipseDirection: TShapeDirection);
var Ex1,Ex2,Ey1,Ey2,Sx,Sy,Ex,Ey,i:integer;
begin
  Case AHalfEllipseDirection of
     atUp:
       begin
       Ex1:=PaintRect.Left; Ex2:=PaintRect.Right;
       Ex:=PaintRect.Left; Sx:=PaintRect.Right;
       i:=PaintRect.Bottom-PaintRect.Top;
       Ey1:=PaintRect.Top;Ey2:=PaintRect.Bottom+i;
       Sy:=PaintRect.Top+i;Ey:=PaintRect.Top+i;
       end;
     atDown:
       begin
       Ex1:=PaintRect.Left; Ex2:=PaintRect.Right;
       Sx:=PaintRect.Left; Ex:=PaintRect.Right;
       i:=PaintRect.Bottom-PaintRect.Top;
       Ey1:=PaintRect.Top-i;Ey2:=PaintRect.Bottom;
       Sy:=PaintRect.Top;Ey:=PaintRect.Top;
       end;
     atRight:
       begin
       Ey1:=PaintRect.Top; Ey2:=PaintRect.Bottom;
       Ey:=PaintRect.Top; Sy:=PaintRect.Bottom;
       i:=PaintRect.Right-PaintRect.Left;
       Ex1:=PaintRect.Left-i;Ex2:=PaintRect.Right;
       Sx:=PaintRect.Left;Ex:=PaintRect.Left;
       end;
     atLeft:
       begin
       Ey1:=PaintRect.Top; Ey2:=PaintRect.Bottom;
       Sy:=PaintRect.Top; Ey:=PaintRect.Bottom;
       i:=PaintRect.Right-PaintRect.Left;
       Ex1:=PaintRect.Left;Ex2:=PaintRect.Right+i;
       Sx:=PaintRect.Left+i;Ex:=PaintRect.Left+i;
       end;
   end;
   Canvas.Pie(Ex1,Ey1,Ex2,Ey2,Sx,Sy,Ex,Ey);
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



 
end.


