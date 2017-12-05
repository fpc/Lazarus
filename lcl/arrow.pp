{
  *********************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
  *********************************************************************

  Author: H. Page-Clark

  Abstract:
    Show an arrow. Its size, direction, color and shadow can be adjusted.
}

unit Arrow;

{$mode objfpc}{$H+}

interface

uses
  Classes, types, math, Controls, Graphics, IndustrialBase;

type

  TArrowType = (atUp, atDown, atLeft, atRight);
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut, stFilled);
  TTriPts = (ptA, ptB, ptC);
  TTrianglePoints = array[TTriPts] of TPoint;

  { TArrow }

  TArrow = class(TIndustrialBase)
  private
    FArrowColor: TColor;
    FArrowType: TArrowType;
    FArrowAngle: integer;
    FShadowType: TShadowType;
    FShadowColor: TColor;
    FR: TRect;
    FT: TTrianglePoints;
    procedure CalcTrianglePoints;
    procedure SetArrowAngle(AValue: integer);
    procedure SetArrowColor(AValue: TColor);
    procedure SetArrowType(AValue: TArrowType);
    procedure SetShadowColor(AValue: TColor);
    procedure SetShadowType(AValue: TShadowType);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clBlack;
    property ArrowType: TArrowType read FArrowType write SetArrowType default atLeft;
    property ArrowPointerAngle: integer read FArrowAngle write SetArrowAngle default 60;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Enabled;
    property Hint;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPaint;
    property OnResize;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShadowType: TShadowType read FShadowType write SetShadowType default stEtchedIn;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default cl3DShadow;
    property ShowHint;
    property Visible;
  end;

procedure Register;


implementation

const
  cDefaultControlSize = 20;
  cMinArrowSize = 8;
  cMinAngle = 20;
  cMaxAngle = 160;
  cShadowColors: array[TShadowType] of TColor =
    (clWindow, cl3DShadow, cl3DShadow, cl3DHiLight, cl3DHiLight, clBlue{not used});
  cInnerOffset = 2;
  cShadowSize = 2; //must be <= cInnerOffset


procedure Register;
begin
  RegisterComponents('Misc',[TArrow]);
end;

{ TArrow }

procedure TArrow.CalcTrianglePoints;
var
  midY, midX: integer;
  ratioNeed, ratioThis: double;
  size: TSize;
begin
  FR:= ClientRect;
  InflateRect(FR, -cInnerOffset, -cInnerOffset);
  Dec(FR.Bottom); // for "filled" shadow

  midX:= (FR.Left + FR.Right) div 2;
  midY:= (FR.Top + FR.Bottom) div 2;
  size:= Types.Size(FR);

  ratioNeed:= 2*Tan(FArrowAngle*pi/(180*2));
  if FArrowType in [atLeft, atRight] then
    ratioNeed:= 1/ratioNeed;

  ratioThis:= size.cx/size.cy;
  if ratioThis>=ratioNeed then
    size.cx:= Trunc(size.cx*ratioNeed/ratioThis)
  else
    size.cy:= Trunc(size.cy*ratioThis/ratioNeed);

  FR.Top:= midY - size.cy div 2;
  FR.Bottom:= FR.Top + size.cy;
  FR.Left:= midX - size.cx div 2;
  FR.Right:= FR.Left + size.cx;

  // angle=90: 1pixel shift appears (reason: float math)
  // workaround:
  if FArrowAngle=90 then
  begin
    if FArrowType in [atUp, atDown] then
    begin
      FR.Left:= midX-size.cy;
      FR.Right:= midX+size.cy;
    end
    else
    begin
      FR.Top:= midY-size.cx;
      FR.Bottom:= midY+size.cx;
    end;
  end;

  case FArrowType of
    atUp: begin
        FT[ptC] := Point(midX, FR.Top);
        FT[ptA] := Point(FR.Left, FR.Bottom);
        FT[ptB] := FR.BottomRight;
       end;
    atDown: begin
        FT[ptA] := FR.TopLeft;
        FT[ptB] := Point(FR.Right, FR.Top);
        FT[ptC] := Point(midX, FR.Bottom);
       end;
    atLeft: begin
        FT[ptA] := Point(FR.Right, FR.Top);
        FT[ptB] := FR.BottomRight;
        FT[ptC] := Point(FR.Left, midY);
       end;
    atRight: begin
        FT[ptA] := FR.TopLeft;
        FT[ptB] := Point(FR.Right, midY);
        FT[ptC] := Point(FR.Left, FR.Bottom);
       end;
  end;
end;

procedure TArrow.SetArrowColor(AValue: TColor);
begin
  if FArrowColor=AValue then Exit;
  FArrowColor:=AValue;
  GraphicChanged;
end;

procedure TArrow.SetArrowType(AValue: TArrowType);
begin
  if FArrowType=AValue then Exit;
  FArrowType:=AValue;
  GraphicChanged;
end;

procedure TArrow.SetShadowColor(AValue: TColor);
begin
  if FShadowColor=AValue then Exit;
  FShadowColor:= AValue;
  GraphicChanged;
end;

procedure TArrow.SetArrowAngle(AValue: integer);
begin
  if FArrowAngle=AValue then Exit;
  FArrowAngle:=Max(Min(AValue, cMaxAngle), cMinAngle);
  GraphicChanged;
end;


procedure TArrow.SetShadowType(AValue: TShadowType);
begin
  if FShadowType=AValue then Exit;
  FShadowType:=AValue;
  GraphicChanged;
end;

class function TArrow.GetControlClassDefaultSize: TSize;
begin
  Result.cx:=cDefaultControlSize;
  Result.cy:=cDefaultControlSize;
end;

procedure TArrow.Paint;
  procedure Offset(var ptA, ptB: TPoint);
  begin
    case FArrowType of
      atUp: begin Inc(ptA.x); Dec(ptA.y); Inc(ptB.x); Dec(ptB.y); end;
      atDown: begin Inc(ptA.x); Inc(ptA.y); Inc(ptB.x); Inc(ptB.y); end;
      atLeft: begin Dec(ptA.x); Inc(ptA.y); Dec(ptB.x); Inc(ptB.y); end;
      atRight: begin Inc(ptA.x); Inc(ptA.y); Inc(ptB.x); Inc(ptB.y); end;
    end;
  end;

  procedure ShadowLine(p1, p2: TPoint);
  begin
    Canvas.Pen.Color:= cShadowColors[FShadowType];
    Canvas.MoveTo(p1);
    Canvas.LineTo(p2);
    Offset(p1, p2);
    Canvas.Pen.Color:= FShadowColor;
    Canvas.MoveTo(p1);
    Canvas.LineTo(p2);
    if (Height>13) then
      begin
        Offset(p1, p2);
        Canvas.MoveTo(p1);
        Canvas.LineTo(p2);
      end;
  end;

  procedure ShadowTriangle;
  var
    Pts: TTrianglePoints;
  begin
    Pts:= FT;
    Inc(Pts[ptA].x, cShadowSize);
    Inc(Pts[ptA].y, cShadowSize);
    Inc(Pts[ptB].x, cShadowSize);
    Inc(Pts[ptB].y, cShadowSize);
    Inc(Pts[ptC].x, cShadowSize);
    Inc(Pts[ptC].y, cShadowSize);
    Canvas.Pen.Color:= FShadowColor;
    Canvas.Brush.Color:= FShadowColor;
    Canvas.Polygon(Pts);
  end;

begin
  CalcTrianglePoints;

  Canvas.AntialiasingMode := AntiAliasingMode;
  // Paint background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  // Paint shadow area
  if (FShadowType=stFilled) then
    ShadowTriangle;

  // Paint arrow
  Canvas.Pen.Color:= FArrowColor;
  Canvas.Brush.Color:= FArrowColor;
  Canvas.Polygon(FT);

  if not (FShadowType in [stNone, stFilled]) then
    ShadowLine(FT[ptB], FT[ptC]);

  inherited Paint;
end;

constructor TArrow.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Constraints.MinHeight:= cMinArrowSize;
  Constraints.MinWidth:= cMinArrowSize;
  FArrowType:= atLeft; // set defaults to match TArrow component
  FArrowAngle:= 60; // angle of equal side triangle
  FShadowType:= stEtchedIn;
  FShadowColor:= cl3DShadow;
  FArrowColor:= clBlack;
end;

end.

