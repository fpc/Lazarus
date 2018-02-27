{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Auxiliary class to draw a ruler.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdrawruler;

{$mode objfpc}{$H+}

interface

uses
  Types,Classes, SysUtils, Controls, Graphics;

type
  TRulerUnits = (ruPx,ruIn,ruPt,ruM,ruDm,ruCm,ruMm);
  TRulerType = (rtTop,rtLeft,rtBottom,rtRight);

  { TDrawRuler }

  TDrawRuler = class(TPersistent)
  private
    FBoundsRect: TRect;
    FCanvas: TCanvas;
    FColor: TColor;
    FDPI: Integer;
    FFont: TFont;
    FType : TRulerType;
    FUnits : TRulerUnits;
    FTickColor : TColor;
    FMajorTicks : Double;
    FPPU : Double;
    FZeroOffset : Integer;
    FMaxTickLength : Integer;
    procedure DrawHTicks(tkStep: single);
    procedure DrawVTicks(tkStep: single);
    procedure SetBoundsRect(AValue: TRect);
    procedure SetDPI(AValue: Integer);
    procedure SetFont(AValue: TFont);
    { Protected declarations }
    procedure SetRulerType(AType: TRulerType);
    procedure SetRulerUnits(AUnits: TRulerUnits);
  protected
    procedure RecalcParams;
  public
    { Public declarations }
    constructor Create(ACanvas: TCanvas);
    Destructor Destroy; override;
    function  Scale(AValue: Double): Integer;
    function HorizontalRuler: Boolean;
    procedure PaintRuler;
    Property Canvas : TCanvas Read FCanvas;
    Property BoundsRect : TRect Read FBoundsRect Write SetBoundsRect;
    property RulerType: TRulerType read FType write SetRulerType;
    property RulerUnits: TRulerUnits read FUnits write SetRulerUnits;
    property TickColor: TColor read FTickColor write FTickColor;
    Property ZeroOffset: Integer read FZeroOffset write FZeroOffset;
    property Font : TFont Read FFont Write SetFont;
    property Color : TColor Read FColor Write FCOlor;
    Property DPI : Integer Read FDPI Write SetDPI;
  end;

implementation

function PixelsPerUnit(DPI : Integer; AUnit: TRulerUnits): Single;

const
   m2i = 39.3700787; // Inches in a meter

begin
  Result:=DPI;
  case AUnit of
    ruPx : Result:=1;
    ruIn : ;
    ruPt : Result:=Result/12;
    ruM  : Result:=Result*m2i;
    ruDM : Result:=Result*(m2i/10);
    ruCM : Result:=Result*(m2i/100);
    ruMM : Result:=Result*(m2i/1000);
  end;
end;

constructor TDrawRuler.Create(ACanvas: TCanvas);
begin
  FCanvas:=ACanvas;
  FFont:=TFont.Create;
  Color:=clWhite;
  FTickColor:=cldkGray;
  FType:=rtTop;
  FUnits:=ruCm;
  FZeroOffset:=0;
  FDPI:=96;
  RecalcParams;
end;

destructor TDrawRuler.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TDrawRuler.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(AValue);
end;

procedure TDrawRuler.SetBoundsRect(AValue: TRect);
begin
  if EqualRect(FBoundsRect,AValue) then Exit;
  FBoundsRect:=AValue;
  RecalcParams;
end;

procedure TDrawRuler.SetDPI(AValue: Integer);
begin
  if FDPI=AValue then Exit;
  FDPI:=AValue;
end;

procedure TDrawRuler.SetRulerType(AType: TRulerType);
begin
  if FType=AType then Exit;
  FType:=AType;
end;

procedure TDrawRuler.SetRulerUnits(AUnits: TRulerUnits);
begin
  if FUnits=AUnits then Exit;
  FUnits:=AUnits;
  RecalcParams;
end;


Function TDrawRuler.HorizontalRuler : Boolean;

begin
  Result:=RulerType in [rtTop,rtBottom];
end;

procedure TDrawRuler.RecalcParams;
var I:Integer;
begin
  FPPU:=PixelsPerUnit(DPI,FUnits);
  FMajorTicks:=(DPI/FPPU);
  I:=Trunc(FMajorTicks);
  case I of
      0: begin
           FMajorTicks:=Int(FMajorTicks/0.05+0.5)*0.05 ; // to the nearest 5/100
           if FMajorTicks=0 then FMajorTicks:=0.01; //  we are to close to zero
         end;
   1..4: FMajorTicks:=Int(FMajorTicks); // to the nearest int
   5..9: FMajorTicks:=Int(FMajorTicks/5+0.5)*5; // to the nearest 5
   10..MaxInt:  FMajorTicks:=Int(FMajorTicks/10+0.5)*10 // to the nearest 10;
  end;
  if HorizontalRuler then
    begin
    FMaxTickLength:=BoundsRect.Bottom-BoundsRect.Top - Canvas.TextHeight('W')- 2
    end
  else
    begin
    FMaxTickLength:=BoundsRect.Right-BoundsRect.Left - Canvas.TextHeight('W')- 2;
    end;
end;

function TDrawRuler.Scale(AValue: Double): Integer;inline;
begin
   Result:=Round(AValue * FPPU);
end;

procedure TDrawRuler.DrawHTicks(tkStep: single);
var
  w,h,tkCount,tkLen,tkPos,y1,y2: Integer;
  tkUnits : Single;
  s : String;

begin
  tkUnits:=0;
  tkCount:=0;
  if RulerType=rtTop then
    y1:=FBoundsRect.Top
  else
    y1:=FBoundsRect.Bottom;
  tkPos:=FBoundsRect.Left+FZeroOffset;
  While (tkPos>=FBoundsRect.Left) and (tkPos<=FBoundsRect.Right) do
    begin
    case tkCount mod 10 of
      0: tkLen:=FMaxTickLength;
      5: tkLen:=FMaxTickLength div 2;
    else
      tkLen:= FMaxTickLength div 4;
    end;
    if RulerType=rtTop then
      y2:=y1+tkLen
    else
      y2:=y1-tkLen;
    Canvas.Line(tkPos,y1,tkPos,y2);
    if (tkCount mod 10=0) then
       begin
       S:=FloatToStr(Round((tkUnits)*100)/100);
       w:=Canvas.TextWidth(S);
       H:=Canvas.TextHeight(S);
       Canvas.TextRect(BoundsRect,tkPos-W div 2,BoundsRect.Bottom-H-2,S);
       end;
    tkUnits:=tkUnits+tkStep;
    tkPos:=FBoundsRect.Left+FZeroOffset+Scale(tkUnits);
    Inc(tkCount);
    end
end;

procedure TDrawRuler.DrawVTicks(tkStep: single);
var
  tkCount,tkLen,tkPos,x1,x2: Integer;
  tkUnits : Single;
  s : String;
  E : TSize;

begin
  tkUnits:=0;
  tkCount:=0;
  if RulerType=rtLeft then
    x1:=FBoundsRect.Left
  else
    x1:=FBoundsRect.Right;
  tkPos:=FBoundsRect.Top+FZeroOffset;
  While (tkPos>=FBoundsRect.Top) and (tkPos<=FBoundsRect.Bottom) do
    begin
    case tkCount mod 10 of
      0: tkLen:=FMaxTickLength;
      5: tkLen:=FMaxTickLength div 2;
    else
      tkLen:= FMaxTickLength div 4;
    end;
    if RulerType=rtLeft then
      x2:=x1+tkLen
    else
      x2:=x1-tkLen;
    Canvas.Line(x1,tkPos,x2,tkPos);
    if (tkCount mod 10=0) then
       begin
       S:=FloatToStr(Round((tkUnits)*100)/100);
       Canvas.Font.Orientation:=0;
       E:=Canvas.TextExtent(S);
       Canvas.Font.Orientation:=900;
       // Unfortunataly, TextRect does not work with fonts that are oriented. So we set the cliprect
       Canvas.ClipRect:=BoundsRect;
       Canvas.Clipping:=True;
       Canvas.TextOut(BoundsRect.Right-E.cy-2,tkPos+(e.cx div 2),S);
       Canvas.Font.Orientation:=0;
       Canvas.Clipping:=False;
       end;
    tkUnits:=tkUnits+tkStep;
    tkPos:=FBoundsRect.Top+FZeroOffset+Scale(tkUnits);
    Inc(tkCount);
    end
end;

procedure TDrawRuler.PaintRuler;

Var
  T : TFont;

begin
  Canvas.Brush.Color:=Color;
  Canvas.Pen.Color:=FTickColor;
  Canvas.Pen.Width:=1;
  Canvas.Rectangle(BoundsRect);
  T:=TFont.Create;
  try
    T.Assign(Canvas.Font);
    Canvas.Font:=Font;
    if HorizontalRuler then
      begin
      DrawHTicks(FMajorTicks/10);
      DrawHTicks(-FMajorTicks/10);
      end
    else
      begin
      DrawVTicks(FMajorTicks/10);
      DrawVTicks(-FMajorTicks/10);
      end;
  finally
    Canvas.Font.Assign(T);
    FreeAndNil(T);
  end;
end;


end.

