{
  Copyright (C) 2013 H. Page-Clark

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit Arrow;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, types;

const Default_Height_Width = 10;
      ArrowMinHeight = 8;

type

  TArrowType = (atUp, atDown, atLeft, atRight);
  TShadowType = (stNone, stIn, stOut, stEtchedIn, stEtchedOut);
  TTriPts = (ptA, ptB, ptC);
  TTrianglePoints = array[TTriPts] of TPoint;

  { TArrow }

  TArrow = class(TGraphicControl)
  private
     FArrowColor: TColor;
     FArrowType: TArrowType;
     FR: TRect;
     FShadowType: TShadowType;
     FT: TTrianglePoints;
     procedure CalcTrianglePoints;
     procedure GraphicChanged(Sender: TObject);
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
     property BorderSpacing;
     property Color;
     property Constraints;
     property OnChangeBounds;
     property OnClick;
     property OnContextPopup;
     property OnDblClick;
     //property OnDragDrop;
     //property OnDragOver;
     //property OnEndDrag;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     property OnPaint;
     property OnResize;
     //property OnStartDrag;
     property ParentShowHint;
     property PopupMenu;
     property ShadowType: TShadowType read FShadowType write SetShadowType default stEtchedIn;
     property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Industrial',[TArrow]);
end;

{ TArrow }

procedure TArrow.CalcTrianglePoints;
var midY, midX, half: integer;
    sz: TSize;
    square, tall: boolean;
begin
  FR:= ClientRect;
  InflateRect(FR, -2, -2);
  sz:= Size(FR);
  square:= (sz.cx = sz.cy);
  if not square then
    begin
      tall:= (sz.cy > sz.cx);
      case tall of
        False:InflateRect(FR, -((sz.cx - sz.cy) div 2), 0);
        True: InflateRect(FR, 0, -((sz.cy - sz.cx) div 2));
      end;
      sz:= Size(FR);
    end;
  half:= sz.cx div 2;
  midX:= FR.Left + half;
  midY:= FR.Top + half;
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

procedure TArrow.GraphicChanged(Sender: TObject);
begin
  if Assigned(Parent) and
   (Visible or (csDesigning in ComponentState))
   then Invalidate;
end;

procedure TArrow.SetArrowColor(AValue: TColor);
begin
  if FArrowColor=AValue then Exit;
  FArrowColor:=AValue;
  GraphicChanged(nil);
end;

procedure TArrow.SetArrowType(AValue: TArrowType);
begin
  if FArrowType=AValue then Exit;
  FArrowType:=AValue;
  GraphicChanged(nil);
end;

procedure TArrow.SetShadowType(AValue: TShadowType);
begin
  if FShadowType=AValue then Exit;
  FShadowType:=AValue;
  GraphicChanged(nil);
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
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

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
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);
  ControlStyle := ControlStyle - [csSetCaption];
  FArrowType:= atLeft;      // set defaults to match TArrow component
  FShadowType:= stEtchedIn;
  FArrowColor:= clBlack;
  Canvas.Pen.Color := clBlack;
  if Assigned(Parent)
    then Color:= Parent.Color
  else Color:= clBtnFace;
  Canvas.Brush.Color := Color;
end;

end.
