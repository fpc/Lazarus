{
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

constructor TShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0,0,65,65);
  ControlStyle := ControlStyle + [csReplicatable];
  FPen := TPen.Create;
  FPen.OnChange := @StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := @StyleChanged;
end;

destructor TShape.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TShape.Paint;
var
  PaintRect : TRect;
  MinSize : Longint;
begin
  with Canvas do
  begin
    Pen := FPen;
    Brush := FBrush;

    PaintRect := Rect(0,0,Width - 1, Height - 1);
    InflateRect(PaintRect, -(Pen.Width div 2), -(Pen.Width div 2));
    With PaintRect do begin
      MinSize := Min(Right - Left, Bottom - Top);
      if FShape in [stSquare, stRoundSquare, stCircle] then begin
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
    end;
  end;
end;

procedure TShape.StyleChanged(Sender: TObject);
begin
  If (Parent <> nil) and Visible and Parent.HandleAllocated
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

{ =============================================================================

 $Log$
 Revision 1.2  2002/08/18 04:57:01  mattias
 fixed csDashDot

 Revision 1.1  2002/10/31 04:27:59  lazarus
 AJ: added TShape


}
