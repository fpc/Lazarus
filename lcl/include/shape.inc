{%MainUnit ../extctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
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
  PaintRect: TRect;
  MinSize: Longint;
  P: array[0..3] of TPoint;
  PenInc, PenDec: Integer;
begin
  with Canvas do
  begin
    Pen := FPen;
    Brush := FBrush;

    PenInc := Pen.Width div 2;
    PenDec := (Pen.Width - 1) div 2;

    PaintRect := Rect(PenInc, PenInc, Self.Width - PenDec, Self.Height - PenDec);
    with PaintRect do
    begin
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
      begin
        with Self do
        begin
          P[0].x := PenInc;
          P[0].y := (Height - 1) div 2;
          P[1].x := (Width - 1) div 2;
          P[1].y := PenInc;
          P[2].x := Width - PenInc - 1;
          P[2].y := P[0].y;
          P[3].x := P[1].x;
          P[3].y := Height - PenInc - 1;
          Polygon(P);
        end;
      end;
      stTriangle:
      begin
        with Self do
        begin
          P[0].x := (Width - 1) div 2;
          P[0].y := PenInc;
          P[1].x := Width - PenInc - 1;
          P[1].y := Height - PenInc - 1;
          P[2].x := PenInc;
          P[2].y := Height - PenInc - 1;
          P[3].x := P[0].x;
          P[3].y := P[0].y;
          Polygon(P);
        end;
      end; 
    end;
  end;
  // to fire OnPaint event
  inherited Paint;
end;

procedure TShape.StyleChanged(Sender: TObject);
begin
  if (Parent <> nil) and (Visible or (csDesigning in ComponentState)) and
     Parent.HandleAllocated then
    Invalidate;
end;

procedure TShape.SetBrush(Value: TBrush);
begin
  if Value <> Brush then
    FBrush.Assign(Value);
end;

procedure TShape.SetPen(Value: TPen);
begin
  if Value <> Pen then
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

class procedure TShape.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterShape;
end;

class function TShape.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 65;
  Result.CY := 65;
end;

