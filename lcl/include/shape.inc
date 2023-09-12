{%MainUnit ../extctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

constructor TCustomShape.Create(TheOwner: TComponent);
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

destructor TCustomShape.Destroy;
begin
  FreeAndNil(FBitmapCopy);
  FreeThenNil(FPen);
  FreeThenNil(FBrush);
  inherited Destroy;
end;

function TCustomShape.PtInShape(const P: TPoint): Boolean;
// Return True if point P is inside the actual shape.
begin
  UpdateMask;
  Result := (P.X >= 0) and (P.X < FBitmapCopy.Width)
        and (P.Y >= 0) and (P.Y < FBitmapCopy.Height)
        and (FBitmapCopy.Canvas.Pixels[P.X, P.Y] = clBlack);
end;

// Angle of 5-angled star is function(N=0..9, Down) = pi/5 * N + pi/2 * IfThen(Down, -1, 1);
const
  CosStarBig: array[0..4, Boolean] of Single = (
    (Cos(       + pi/2), Cos(       - pi/2)),
    (Cos(2*pi/5 + pi/2), Cos(2*pi/5 - pi/2)),
    (Cos(4*pi/5 + pi/2), Cos(4*pi/5 - pi/2)),
    (Cos(6*pi/5 + pi/2), Cos(6*pi/5 - pi/2)),
    (Cos(8*pi/5 + pi/2), Cos(8*pi/5 - pi/2))
    );
  SinStarBig: array[0..4, Boolean] of Single = (
    (Sin(       + pi/2), Sin(       - pi/2)),
    (Sin(2*pi/5 + pi/2), Sin(2*pi/5 - pi/2)),
    (Sin(4*pi/5 + pi/2), Sin(4*pi/5 - pi/2)),
    (Sin(6*pi/5 + pi/2), Sin(6*pi/5 - pi/2)),
    (Sin(8*pi/5 + pi/2), Sin(8*pi/5 - pi/2))
    );
  CosStarSmall: array[0..4, Boolean] of Single = (
    (Cos(  pi/5 + pi/2), Cos(  pi/5 - pi/2)),
    (Cos(3*pi/5 + pi/2), Cos(3*pi/5 - pi/2)),
    (Cos(5*pi/5 + pi/2), Cos(5*pi/5 - pi/2)),
    (Cos(7*pi/5 + pi/2), Cos(7*pi/5 - pi/2)),
    (Cos(9*pi/5 + pi/2), Cos(9*pi/5 - pi/2))
    );
  SinStarSmall: array[0..4, Boolean] of Single = (
    (Sin(  pi/5 + pi/2), Sin(  pi/5 - pi/2)),
    (Sin(3*pi/5 + pi/2), Sin(3*pi/5 - pi/2)),
    (Sin(5*pi/5 + pi/2), Sin(5*pi/5 - pi/2)),
    (Sin(7*pi/5 + pi/2), Sin(7*pi/5 - pi/2)),
    (Sin(9*pi/5 + pi/2), Sin(9*pi/5 - pi/2))
    );

procedure TCustomShape.DrawToCanvas(ACanvas: TCanvas);
const
  cStarError = 2; // Detect N pixels error for 5-star horizontal lines
var
  PaintRect: TRect;
  MinSize: Longint;
  P: array of TPoint;
  PenInc, PenDec: Integer;
  PolygonWinding: Boolean;
  RadiusBig, RadiusBig2, RadiusSm, i: Integer;
  PCenter: TPoint;
begin
  PenInc := ACanvas.Pen.Width div 2;
  PenDec := (ACanvas.Pen.Width - 1) div 2;

  PaintRect := Rect(PenInc, PenInc, Width - PenDec, Height - PenDec);
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
      ACanvas.Rectangle(PaintRect);
    stRoundRect, stRoundSquare:
      ACanvas.RoundRect(PaintRect, MinSize div 4, MinSize div 4);
    stCircle, stEllipse:
      ACanvas.Ellipse(PaintRect);
    stSquaredDiamond, stDiamond:
      begin
        SetLength(P, 4);
        P[0].x := PaintRect.Left;
        P[0].y := (PaintRect.Top + PaintRect.Bottom) div 2;
        P[1].x := (PaintRect.Left + PaintRect.Right) div 2;
        P[1].y := PaintRect.Top;
        P[2].x := PaintRect.Right - 1;
        P[2].y := P[0].y;
        P[3].x := P[1].x;
        P[3].y := PaintRect.Bottom - 1;
        ACanvas.Polygon(P);
      end;
    stTriangle:
      begin
        SetLength(P, 3);
        P[0].x := (Width - 1) div 2;
        P[0].y := PenInc;
        P[1].x := Width - PenInc - 1;
        P[1].y := Height - PenInc - 1;
        P[2].x := PenInc;
        P[2].y := Height - PenInc - 1;
        ACanvas.Polygon(P);
      end;
    stTriangleDown:
      begin
        SetLength(P, 3);
        P[0].x := (Width - 1) div 2;
        P[0].y := Height - PenInc - 1;
        P[1].x := Width - PenInc - 1;
        P[1].y := PenInc;
        P[2].x := PenInc;
        P[2].y := PenInc;
        ACanvas.Polygon(P);
      end;
    stTriangleLeft:
      begin
        SetLength(P, 3);
        P[0].x := PenInc;
        P[0].y := Height div 2;
        P[1].x := Width - PenInc - 1;
        P[1].y := PenInc;
        P[2].x := Width - PenInc - 1;
        P[2].y := Height - PenInc - 1;
        ACanvas.Polygon(P);
      end;
    stTriangleRight:
      begin
        SetLength(P, 3);
        P[0].x := Width - PenInc - 1;
        P[0].y := Height div 2;
        P[1].x := PenInc;
        P[1].y := PenInc;
        P[2].x := PenInc;
        P[2].y := Height - PenInc - 1;
        ACanvas.Polygon(P);
      end;
    stStar, stStarDown:
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
        else begin
          RadiusBig := RadiusBig2;
          PCenter.Y := Height div 2;
        end;
        PCenter.X := Width div 2;
        RadiusSm := RadiusBig * 57 div 150;

        SetLength(P, 10);
        for i := 0 to 4 do
        begin
          P[i*2].x := PCenter.X + Round(RadiusBig*CosStarBig[i, FShape=stStarDown]);
          P[i*2].y := PCenter.Y - Round(RadiusBig*SinStarBig[i, FShape=stStarDown]);
          P[i*2+1].x := PCenter.X + Round(RadiusSm*CosStarSmall[i, FShape=stStarDown]);
          P[i*2+1].y := PCenter.Y - Round(RadiusSm*SinStarSmall[i, FShape=stStarDown]);
        end;

        // Fix 1 pixel error of horizontal lines, adjust point on small radius to the point on big one
        for i := 0 to 4 do
          if Abs(P[i*2].y - P[i*2+1].y) <= cStarError then
            P[i*2+1].y := P[i*2].y;
        for i := 1 to 4 do
          if Abs(P[i*2].y - P[i*2-1].y) <= cStarError then
            P[i*2-1].y := P[i*2].y;

        ACanvas.Polygon(P);
      end;
    stPolygon:
      if (csDesigning in ComponentState) then
      begin
        if Assigned(FBitmapCopy) and (ACanvas <> FBitmapCopy.Canvas) then
        begin
          ACanvas.Brush.Color := Parent.Color;
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Pen.Style := psDash;
          ACanvas.Pen.Color := clWindowText;
          ACanvas.Rectangle(PaintRect);
          ACanvas.Pen.Color := clRed;
          ACanvas.Line(0, 0, Width-1, Height-1);
          ACanvas.Line(0, Height-1, Width-1, 0);
        end else
          ACanvas.Rectangle(PaintRect);
      end else
      if Assigned(FOnShapePoints) then
      begin
        SetLength(P, 0);
        PolygonWinding := false;
        FOnShapePoints(Self, P, PolygonWinding);
        if Length(P) > 2 then
          ACanvas.Polygon(P, PolygonWinding);
      end;
  end;
end;

procedure TCustomShape.UpdateMask;
begin
  if FBitmapCopy = nil then
  begin
    FBitmapCopy := TBitmap.Create;
    FBitmapCopy.Monochrome := true;
  end;
  FBitmapCopy.SetSize(Width, Height);
  FBitmapCopy.Canvas.Brush.Style := bsSolid;
  FBitmapCopy.Canvas.Brush.Color := clWhite;
  FBitmapCopy.Canvas.FillRect(0,0,Width,Height);
  FBitmapCopy.Canvas.Brush.Color := clBlack;
  FBitmapCopy.Canvas.Pen := Pen;
  FBitmapCopy.Canvas.Pen.Color := clBlack;
  DrawToCanvas(FBitmapCopy.Canvas);
end;

procedure TCustomShape.Paint;
begin
  Canvas.Pen := FPen;
  Canvas.Brush := FBrush;
  DrawToCanvas(Canvas);

  // to fire OnPaint event
  inherited Paint;
end;

procedure TCustomShape.StyleChanged(Sender: TObject);
begin
  if (Parent <> nil) and (Visible or (csDesigning in ComponentState))
  and Parent.HandleAllocated then
    Invalidate;
end;

procedure TCustomShape.SetBrush(Value: TBrush);
begin
  if Value <> Brush then
    FBrush.Assign(Value);
end;

procedure TCustomShape.SetOnShapePoints(Value: TShapePointsEvent);
begin
  FOnShapePoints := Value;
  Invalidate;
end;

procedure TCustomShape.SetPen(Value: TPen);
begin
  if Value <> Pen then
    FPen.Assign(Value);
end;

procedure TCustomShape.SetShape(Value: TShapeType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    StyleChanged(Self);
  end;
end;

class procedure TCustomShape.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomShape;
end;

class function TCustomShape.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 65;
  Result.CY := 65;
end;

procedure TCustomShape.Click;
begin
  inherited Click;
  if Assigned(OnShapeClick)
  and PtInShape(ScreenToClient(Mouse.CursorPos)) then
    OnShapeClick(Self);
end;

procedure TCustomShape.CMShapeHitTest(var Message: TCMHittest);
var
  p: TPoint;
begin
  p := ParentToClient(Point(Message.XPos, Message.YPos),GetDesignerForm(Self));
  if PtInShape(p) then
    Message.Result := 0
  else
    Message.Result := 1;
end;

