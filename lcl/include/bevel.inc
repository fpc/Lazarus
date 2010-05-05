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
constructor TBevel.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption,csOpaque];
  FStyle := bsLowered;
  FShape := bsBox;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TBevel.Destroy;
begin
  inherited Destroy;
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

procedure TBevel.SetStyle(AStyle: TBevelStyle);
begin
  if FStyle <> AStyle Then
  begin
    FStyle := AStyle;
    Invalidate;
  end;
end;

procedure TBevel.SetShape(AShape:TBevelShape);
begin
  if FShape <> AShape then
  begin
    FShape := AShape;
    Invalidate;
  end;
end;

class procedure TBevel.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterBevel;
end;

class function TBevel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 50;
  Result.CY := 50;
end;

procedure TBevel.Paint;
var
  Colora, Colorb: TColor;
  ALeft, ATop: integer;
  AWidth: LongInt;
  AHeight: LongInt;
begin
  case Style of
    bsRaised:
      begin
        Colora := cl3DHilight;
        Colorb := cl3DShadow;
      end;
    bsLowered:
      begin
        Colora := cl3DShadow;
        Colorb := cl3DHilight;
      end;
  end;
  Canvas.Pen.Width:=1;
  
  ALeft := 0;
  ATop := 0;
  AWidth := Width;
  AHeight := Height;
  
  case Shape of
    bsBox:
      with Canvas do
      begin
        Pen.Color:=Colora;
        MoveTo(ALeft, ATop + AHeight - 1);
        LineTo(ALeft, ATop);
        LineTo(ALeft + AWidth - 1, ATop);
        Pen.Color:=Colorb;
        LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
        LineTo(ALeft , ATop + AHeight - 1);
      end;
    bsFrame:
      with Canvas do
      begin
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
      end;
    bsTopLine:
      with Canvas do
      begin
        Pen.Color:=Colora;
        MoveTo(ALeft, ATop);
        LineTo(ALeft + AWidth - 1, ATop);
        Pen.Color:=Colorb;
        MoveTo(ALeft, ATop + 1);
        LineTo(ALeft + AWidth - 1, ATop + 1);
      end;
    bsBottomLine:
      with Canvas do
      begin
        Pen.Color:=Colorb;
        MoveTo(ALeft, ATop + AHeight - 1);
        LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
        Pen.Color:=Colora;
        MoveTo(ALeft, ATop + AHeight - 2);
        LineTo(ALeft + AWidth - 1, ATop + AHeight - 2);
      end;
    bsLeftLine:
      with Canvas do
      begin
        Pen.Color:=Colora;
        MoveTo(ALeft, ATop);
        LineTo(ALeft, ATop + AHeight - 1);
        Pen.Color:=Colorb;
        MoveTo(ALeft + 1, ATop);
        LineTo(ALeft + 1, ATop + AHeight - 1);
      end;
    bsRightLine:
      with Canvas do
      begin
        Pen.Color:=Colorb;
        MoveTo(ALeft + AWidth - 1, ATop);
        LineTo(ALeft + AWidth - 1, ATop + AHeight - 1);
        Pen.Color:=Colora;
        MoveTo(ALeft + AWidth - 2, ATop);
        LineTo(ALeft + AWidth - 2, ATop + AHeight);
      end;
    bsSpacer: ;//nothing
  end;
  
  inherited Paint;
end;

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

