{%MainUnit ../extctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
constructor TBevel.Create(AOwner:TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption,csOpaque];
  FStyle:=bsLowered;
  FShape:=bsBox;

  SetBounds(0,0,50,50);
end;

destructor TBevel.Destroy;
begin
  Inherited Destroy;
end;

procedure TBevel.Assign(Source: TPersistent);
begin
  if Source is TBevel then
  begin
    FShape := TBevel(Source).Shape;
    FStyle := TBevel(Source).Style;
  end else
    inherited;
end;

procedure TBevel.Invalidate;
begin
  if Canvas.HandleAllocated then Paint;
end;

function TBevel.GetStyle:TBevelStyle;
begin
  Result:=FStyle;
end;

procedure TBevel.SetStyle(aStyle:TBevelStyle);
begin
  If FStyle<>aStyle Then
  begin
    FStyle:=aStyle;
    Invalidate;
  end;
end;

function TBevel.GetShape:TBevelShape;
begin
  Result := FShape;
end;

procedure TBevel.SetShape(aShape:TBevelShape);
begin
  if FShape<>aShape then
  begin
    FShape:=aShape;
    Invalidate;
  end;
end;

procedure TBevel.Paint;
Var
  Colora, Colorb: TColor;
  ALeft, ATop: integer;
  AWidth: LongInt;
  AHeight: LongInt;
Begin
  Case Style Of
  bsRaised:
    Begin
      Colora:=cl3DHilight;
      Colorb:=cl3DShadow;
    End;
  bsLowered:
    Begin
      Colora:=cl3DShadow;
      Colorb:=cl3DHilight;
    End;
  End;
  Canvas.Pen.Width:=1;
  
  ALeft:=0;
  ATop:=0;
  AWidth:=Width;
  AHeight:=Height;
  
  Case Shape Of
  bsBox:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop + AHeight - 1);
      LineTo(ALeft, ATop);
      LineTo(ALeft + AWidth - 1, ATop);
      Pen.Color:=Colorb;
      LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
      LineTo(ALeft , ATop + AHeight - 1);
    End;
  bsFrame:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop + AHeight - 1);
      LineTo(ALeft, ATop);
      LineTo(ALeft + AWidth - 1, ATop);
      MoveTo(ALeft + AWidth - 2, ATop + 1);
      LineTo(ALeft + AWidth - 2, ATop + AHeight - 2);
      LineTo(ALeft + 1, ATop + AHeight - 2);
      Pen.Color:=Colorb;
      MoveTo(ALeft + 1, ATop + AHeight - 2);
      LineTo(ALeft + 1, ATop + 1);
      LineTo(ALeft + AWidth - 2, ATop + 1);
      MoveTo(ALeft + AWidth - 1, ATop);
      LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
      LineTo(ALeft, ATop + AHeight - 1);
    End;
  bsTopLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop);
      LineTo(ALeft + AWidth - 1, ATop);
      Pen.Color:=Colorb;
      MoveTo(ALeft, ATop + 1);
      LineTo(ALeft + AWidth - 1, ATop + 1);
    End;
  bsBottomLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(ALeft, ATop + AHeight - 1);
      LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop + AHeight - 2);
      LineTo(ALeft + AWidth - 1, ATop + AHeight - 2);
    End;
  bsLeftLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop);
      LineTo(ALeft, ATop + AHeight - 1);
      Pen.Color:=Colorb;
      MoveTo(ALeft + 1, ATop);
      LineTo(ALeft + 1, ATop + AHeight - 1);
    End;
  bsRightLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(ALeft + AWidth - 1, ATop);
      LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
      Pen.Color:=Colora;
      MoveTo(ALeft + AWidth - 2, ATop);
      LineTo(ALeft + AWidth - 2, ATop + AHeight);
    End;
  End;
  
  inherited Paint;
End;

// included by extctrls.pp

{
procedure TBevel.Paint;
Var
  Colora, Colorb:TColor;
Begin
  Case Style Of
  bsRaised:
    Begin
      Colora:=clSilver;
      Colorb:=clGray;
    End;
  bsLowered:
    Begin
      Colora:=clGray;
      Colorb:=clSilver;
    End;
  End;
  Case Shape Of
  bsBox:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(0, Height - 1);
      LineTo(0, 0);
      LineTo(Width - 1, 0);
      Pen.Color:=Colorb;
      LineTo(Width - 1, Height - 1);
      LIneTo(0, Height - 1);
    End;
  bsFrame:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(0, 0);
      LineTo(Width - 2, 0);
      LineTo(Width - 2, Height - 2);
      LineTo(0, Height - 2);
      LineTo(0, 0);
      Pen.Color:=Colorb;
      MoveTo(1, 1);
      LineTo(Width - 1, 1);
      LineTo(Width - 1, Height - 1);
      LineTo(1, Height - 1);
      LineTo(1, 1);
    End;
  bsTopLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(0, 0);
      LineTo(Width - 1, 0);
      Pen.Color:=Colorb;
      MoveTo(0, 1);
      LineTo(Width - 1, 1);
    End;
  bsBottomLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(0, Height - 1);
      LineTo(Width - 1, Height - 1);
      Pen.Color:=Colora;
      MoveTo(0, Height - 2);
      LineTo(Width - 1, Height - 2);
    End;
  bsLeftLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(0, 0);
      LineTo(0, Height - 1);
      Pen.Color:=Colorb;
      MoveTo(1, 0);
      LineTo(1, Height - 1);
    End;
  bsRightLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height - 1);
      Pen.Color:=Colora;
      MoveTo(Width - 2, 0);
      LineTo(Width - 2, Height);
    End;
  End;
End;
}

