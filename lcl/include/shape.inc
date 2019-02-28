{%MainUnit ../extctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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

// Angle of 5-angled star is function(N=0..9, Down) = pi/5 * N + pi/2 * IfThen(Down, -1, 1);
const
  CosStarBig: array[0..4, Boolean] of Double = (
    (Cos(       + pi/2), Cos(       - pi/2)),
    (Cos(2*pi/5 + pi/2), Cos(2*pi/5 - pi/2)),
    (Cos(4*pi/5 + pi/2), Cos(4*pi/5 - pi/2)),
    (Cos(6*pi/5 + pi/2), Cos(6*pi/5 - pi/2)),
    (Cos(8*pi/5 + pi/2), Cos(8*pi/5 - pi/2))
    );
  SinStarBig: array[0..4, Boolean] of Double = (
    (Sin(       + pi/2), Sin(       - pi/2)),
    (Sin(2*pi/5 + pi/2), Sin(2*pi/5 - pi/2)),
    (Sin(4*pi/5 + pi/2), Sin(4*pi/5 - pi/2)),
    (Sin(6*pi/5 + pi/2), Sin(6*pi/5 - pi/2)),
    (Sin(8*pi/5 + pi/2), Sin(8*pi/5 - pi/2))
    );
  CosStarSmall: array[0..4, Boolean] of Double = (
    (Cos(  pi/5 + pi/2), Cos(  pi/5 - pi/2)),
    (Cos(3*pi/5 + pi/2), Cos(3*pi/5 - pi/2)),
    (Cos(5*pi/5 + pi/2), Cos(5*pi/5 - pi/2)),
    (Cos(7*pi/5 + pi/2), Cos(7*pi/5 - pi/2)),
    (Cos(9*pi/5 + pi/2), Cos(9*pi/5 - pi/2))
    );
  SinStarSmall: array[0..4, Boolean] of Double = (
    (Sin(  pi/5 + pi/2), Sin(  pi/5 - pi/2)),
    (Sin(3*pi/5 + pi/2), Sin(3*pi/5 - pi/2)),
    (Sin(5*pi/5 + pi/2), Sin(5*pi/5 - pi/2)),
    (Sin(7*pi/5 + pi/2), Sin(7*pi/5 - pi/2)),
    (Sin(9*pi/5 + pi/2), Sin(9*pi/5 - pi/2))
    );

procedure TShape.Paint;
const
  cStarError = 2; // Detect N pixels error for 5-star horizontal lines
var
  PaintRect: TRect;
  MinSize: Longint;
  P: array[0..3] of TPoint;
  PStar: array[0..10] of TPoint;
  PenInc, PenDec: Integer;
  RadiusBig, RadiusBig2, RadiusSm, i: Integer;
  PCenter: TPoint;
  Alfa: Double;
begin
  with Canvas do
  begin
    Pen := FPen;
    Brush := FBrush;

    PenInc := Pen.Width div 2;
    PenDec := (Pen.Width - 1) div 2;

    PaintRect := Rect(PenInc, PenInc, Self.Width - PenDec, Self.Height - PenDec);
    if PaintRect.Left = PaintRect.Right then
      PaintRect.Right := PaintRect.Right + 1;
    if PaintRect.Top = PaintRect.Bottom then
      PaintRect.Bottom := PaintRect.Bottom + 1;

    MinSize := Min(PaintRect.Right - PaintRect.Left, PaintRect.Bottom - PaintRect.Top);
    if FShape in [stSquare, stRoundSquare, stCircle, stSquaredDiamond] then
    begin
      PaintRect.Left := PaintRect.Left + ((PaintRect.Right - PaintRect.Left) - MinSize) div 2;
      PaintRect.Top := PaintRect.Top + ((PaintRect.Bottom - PaintRect.Top) - MinSize) div 2;
      PaintRect.Right := PaintRect.Left + MinSize;
      PaintRect.Bottom := PaintRect.Top + MinSize;
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
          P[0].x := PaintRect.Left;
          P[0].y := (PaintRect.Top + PaintRect.Bottom) div 2;
          P[1].x := (PaintRect.Left + PaintRect.Right) div 2;
          P[1].y := PaintRect.Top;
          P[2].x := PaintRect.Right - 1;
          P[2].y := P[0].y;
          P[3].x := P[1].x;
          P[3].y := PaintRect.Bottom - 1;
          Polygon(P);
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
      stTriangleDown:
        begin
          with Self do
          begin
            P[0].x := (Width - 1) div 2;
            P[0].y := Height - PenInc - 1;
            P[1].x := Width - PenInc - 1;
            P[1].y := PenInc;
            P[2].x := PenInc;
            P[2].y := PenInc;
            P[3].x := P[0].x;
            P[3].y := P[0].y;
            Polygon(P);
          end;
        end;
      stTriangleLeft:
        begin
          with Self do
          begin
            P[0].x := PenInc;
            P[0].y := Height div 2;
            P[1].x := Width - PenInc - 1;
            P[1].y := PenInc;
            P[2].x := Width - PenInc - 1;
            P[2].y := Height - PenInc - 1;
            P[3].x := P[0].x;
            P[3].y := P[0].y;
            Polygon(P);
          end;
        end;
      stTriangleRight:
        begin
          with Self do
          begin
            P[0].x := Width - PenInc - 1;
            P[0].y := Height div 2;
            P[1].x := PenInc;
            P[1].y := PenInc;
            P[2].x := PenInc;
            P[2].y := Height - PenInc - 1;
            P[3].x := P[0].x;
            P[3].y := P[0].y;
            Polygon(P);
          end;
        end;
      stStar,
      stStarDown:
        begin
          with Self do
          begin
            //radius if star scaled by height
            RadiusBig := Trunc((Height-Pen.Width) / (1+cos(pi/5)));
            //radius if star scaled by width
            RadiusBig2 := Trunc((Width-Pen.Width) / (2*sin(pi*2/5)));

            if RadiusBig<=RadiusBig2 then
            begin
              if FShape=stStar then
                PCenter.Y := RadiusBig+PenDec
              else
                PCenter.Y := Height-RadiusBig-PenDec;
            end
            else
            begin
              RadiusBig := RadiusBig2;
              PCenter.Y := Height div 2;
            end;
            PCenter.X := Width div 2;
            RadiusSm := RadiusBig * 57 div 150;

            for i := 0 to 4 do
            begin
              PStar[i*2].x := PCenter.X + Round(RadiusBig*CosStarBig[i, FShape=stStarDown]);
              PStar[i*2].y := PCenter.Y - Round(RadiusBig*SinStarBig[i, FShape=stStarDown]);
              PStar[i*2+1].x := PCenter.X + Round(RadiusSm*CosStarSmall[i, FShape=stStarDown]);
              PStar[i*2+1].y := PCenter.Y - Round(RadiusSm*SinStarSmall[i, FShape=stStarDown]);
            end;

            // Fix 1 pixel error of horizontal lines, adjust point on small radius to the point on big one
            for i := 0 to 4 do
              if Abs(PStar[i*2].y - PStar[i*2+1].y) <= cStarError then
                PStar[i*2+1].y := PStar[i*2].y;
            for i := 1 to 4 do
              if Abs(PStar[i*2].y - PStar[i*2-1].y) <= cStarError then
                PStar[i*2-1].y := PStar[i*2].y;

            PStar[10] := PStar[0];
            Polygon(PStar);
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

