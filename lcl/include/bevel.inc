Constructor TBevel.Create(AOwner:TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  FStyle:=bsLowered;
  FShape:=bsBox;

  SetBounds(0,0,50,50);
End;

Destructor TBevel.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TBevel.Invalidate;
Begin
  Paint;
End;

Function TBevel.GetStyle:TBevelStyle;
Begin
  Result:=FStyle;
End;

Procedure TBevel.SetStyle(aStyle:TBevelStyle);
Begin
  If FStyle<>aStyle Then
  Begin
    FStyle:=aStyle;
    Invalidate;
  End;
End;

Function TBevel.GetShape:TBevelShape;
Begin
  Result:=FShape;
End;

Procedure TBevel.SetShape(aShape:TBevelShape);
Begin
  If FShape<>aShape Then
  Begin
    FShape:=aShape;
    Invalidate;
  End;
End;

Procedure TBevel.Paint;
Var
  Colora, Colorb:TColor;
Begin
  Case Style Of
  bsRaised:
    Begin
      Colora:=clWhite;
      Colorb:=clGray;
    End;
  bsLowered:
    Begin
      Colora:=clGray;
      Colorb:=clWhite;
    End;
  End;
  Canvas.Pen.Width:=1;
  Case Shape Of
  bsBox:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(Left, Top + Height - 1);
      LineTo(Left, Top);
      LineTo(Left + Width - 2, Top);
      Pen.Color:=Colorb;
      LineTo(Left + Width - 2, Top + Height - 1);
      LIneTo(Left , Top + Height - 1);
    End;
  bsFrame:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(Left, Top + Height - 1);
      LineTo(Left, Top);
      LineTo(Left + Width - 1, Top);
      MoveTo(Left + Width - 2, Top + 1);
      LineTo(Left + Width - 2, Top + Height - 2);
      LineTo(Left + 1, Top + Height - 2);
      Pen.Color:=Colorb;
      MoveTo(Left + 1, Top + Height - 2);
      LineTo(Left + 1, Top + 1);
      LineTo(Left + Width - 2, Top + 1);
      MoveTo(Left + Width - 1, Top);
      LineTo(Left + Width - 1, Top + Height - 1);
      LineTo(Left, Top + Height - 1);
    End;
  bsTopLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(Left, Top);
      LineTo(Left + Width - 1, Top);
      Pen.Color:=Colorb;
      MoveTo(Left, Top + 1);
      LineTo(Left + Width - 1, Top + 1);
    End;
  bsBottomLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(Left, Top + Height - 1);
      LineTo(Left + Width - 1, Top + Height - 1);
      Pen.Color:=Colora;
      MoveTo(Left, Top + Height - 2);
      LineTo(Left + Width - 1, Top + Height - 2);
    End;
  bsLeftLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(Left, Top);
      LineTo(Left, Top + Height - 1);
      Pen.Color:=Colorb;
      MoveTo(Left + 1, Top);
      LineTo(Left + 1, Top + Height - 1);
    End;
  bsRightLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(Left + Width - 1, Top);
      LineTo(Left + Width - 1, Top + Height - 1);
      Pen.Color:=Colora;
      MoveTo(Left + Width - 2, Top);
      LineTo(Left + Width - 2, Top + Height);
    End;
  End;
End;


{
Procedure TBevel.Paint;
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

{
  $Log$
  Revision 1.3  2001/04/02 14:45:26  lazarus
  MG: bugfixes for TBevel

  Revision 1.2  2001/03/27 14:27:43  lazarus
  Changes from Nagy Zsolt
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.1  2000/01/02 00:22:54  lazarus
  stoppok:
    - introduced TBevel
    - enhanced TCustomRadioGroup

}

