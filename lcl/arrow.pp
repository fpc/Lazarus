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
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut);
  TTriPts = (ptA, ptB, ptC);
  TTrianglePoints = array[TTriPts] of TPoint;

  { TArrow }

  TArrow = class(TIndustrialBase)
  private
    FArrowColor: TColor;
    FArrowType: TArrowType;
    FArrowAngle: integer;
    FShadowType: TShadowType;
    FR: TRect;
    FT: TTrianglePoints;
    procedure CalcTrianglePoints;
    procedure SetArrowAngle(AValue: integer);
    procedure SetArrowColor(AValue: TColor);
    procedure SetArrowType(AValue: TArrowType);
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
    property OnPaint;
    property OnResize;
    property OnStartDrag;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShadowType: TShadowType read FShadowType write SetShadowType default stEtchedIn;
    property ShowHint;
    property Visible;
  end;

procedure Register;


implementation

const
  Default_Height_Width = 20;
  ArrowMinHeight = 8;
  cMinAngle = 20;
  cMaxAngle = 160;


procedure Register;
begin
  RegisterComponents('Misc',[TArrow]);
end;

{ TArrow }

procedure TArrow.CalcTrianglePoints;
const
  cOffset = 2;
var
  midY, midX: integer;
  ratioNeed, ratioThis: double;
  size: TSize;
begin
  FR:= ClientRect;
  InflateRect(FR, -cOffset, -cOffset);
  midX:= (FR.Left + FR.Right) div 2;
  midY:= (FR.Top + FR.Bottom) div 2;
  size:= Types.Size(FR);

  ratioNeed:= 2*tan(FArrowAngle*pi/180/2);
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
  Result.cx:=Default_Height_Width;
  Result.cy:=Default_Height_Width;
end;

procedure TArrow.Paint;
const
  Colors: array[TShadowType] of TColor
    =(clWindow, cl3DShadow, cl3DShadow, cl3DHiLight, cl3DHiLight);

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
    Canvas.Pen.Color:= Colors[FShadowType];
    Canvas.MoveTo(p1);
    Canvas.LineTo(p2);
    Offset(p1, p2);
    Canvas.Pen.Color:= cl3DShadow;
    Canvas.MoveTo(p1);
    Canvas.LineTo(p2);
    if (Height>13) then
      begin
        Offset(p1, p2);
        Canvas.MoveTo(p1);
        Canvas.LineTo(p2);
      end;
  end;

begin
  Canvas.AntialiasingMode := AntiAliasingMode;
  // Paint background
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  // Paint arrow
  Canvas.Pen.Color:= FArrowColor;
  Canvas.Brush.Color:= FArrowColor;
  CalcTrianglePoints;
  Canvas.Polygon(FT);

  if (FShadowType <> stNone)
    then ShadowLine(FT[ptB], FT[ptC]);

  inherited Paint;
end;

constructor TArrow.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Constraints.MinHeight:= ArrowMinHeight;
  Constraints.MinWidth:= ArrowMinHeight;
  FArrowType:= atLeft; // set defaults to match TArrow component
  FArrowAngle:= 60; // angle of equal side triangle
  FShadowType:= stEtchedIn;
  FArrowColor:= clBlack;
end;

end.

