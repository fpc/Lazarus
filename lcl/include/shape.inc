{%MainUnit ../extctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

constructor TShape.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  ControlStyle := ControlStyle + [csReplicatable];
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
end;

destructor TShape.Destroy;
begin
  FreeThenNil(FPen);
  FreeThenNil(FBrush);
  inherited Destroy;
end;

procedure TShape.Paint;
var
  PaintRect : TRect;
  MinSize : Longint;
  P: array[0..3] of TPoint;
begin
  with Canvas do
  begin
    Pen := FPen;
    Brush := FBrush;

    PaintRect := Rect(0,0,Self.Width - 1, Self.Height - 1);
    InflateRect(PaintRect, -(Pen.Width div 2), -(Pen.Width div 2));
    With PaintRect do begin
      MinSize := Min(Right - Left, Bottom - Top);
      if FShape in [stSquare, stRoundSquare, stCircle, stSquaredDiamond] then
      begin
        Left := Left + ((Right - Left) - MinSize) div 2;
        Top := Top + ((Bottom - Top) - MinSize) div 2;
        Right := Left + MinSize;
        Bottom := Top + MinSize;
      end;
    end;

    case FShape of
      stRectangle, stSquare:
        Rectangle(PaintRect);
      stRoundRect, stRoundSquare:
        RoundRect(PaintRect, MinSize div 4, MinSize div 4);
      stCircle, stEllipse:
        Ellipse(PaintRect);
      stSquaredDiamond, stDiamond:
        with PaintRect do begin
          P[0].x:=Left; P[0].y:=Top + (Bottom - Top) div 2;
          P[1].x:=Left+ (Right - Left) div 2; P[1].y:=Bottom;
          P[2].x:=Right; P[2].y:= P[0].y;
          P[3].x:=P[1].x; P[3].y:=Top;
          Polygon(P);
        end;
    end;
  end;
  // to fire OnPaint event
  inherited Paint;
end;

procedure TShape.StyleChanged(Sender: TObject);
begin
  If (Parent <> nil) and (Visible or (csDesigning in ComponentState))
   and Parent.HandleAllocated
  then
    Invalidate;
end;

procedure TShape.SetBrush(Value: TBrush);
begin
  If Value <> Brush then
    FBrush.Assign(Value);
end;

procedure TShape.SetPen(Value: TPen);
begin
  If Value <> Pen then
    FPen.Assign(Value);
end;

procedure TShape.SetShape(Value: TShapeType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    StyleChanged(Self);
  end;
end;

class function TShape.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=65;
  Result.Y:=65;
end;

