// included by extctrls.pp
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
Constructor TBevel.Create(AOwner:TComponent);
Begin
  Inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption,csOpaque];
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
  if Canvas.HandleAllocated then Paint;
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
  Colora, Colorb: TColor;
  ALeft, ATop: integer;
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
  
  ALeft:=0;
  ATop:=0;
  
  Case Shape Of
  bsBox:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop + Height - 1);
      LineTo(ALeft, ATop);
      LineTo(ALeft + Width - 1, ATop);
      Pen.Color:=Colorb;
      LineTo(ALeft + Width - 1, ATop + Height - 1);
      LineTo(ALeft , ATop + Height - 1);
    End;
  bsFrame:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop + Height - 1);
      LineTo(ALeft, ATop);
      LineTo(ALeft + Width - 1, ATop);
      MoveTo(ALeft + Width - 2, ATop + 1);
      LineTo(ALeft + Width - 2, ATop + Height - 2);
      LineTo(ALeft + 1, ATop + Height - 2);
      Pen.Color:=Colorb;
      MoveTo(ALeft + 1, ATop + Height - 2);
      LineTo(ALeft + 1, ATop + 1);
      LineTo(ALeft + Width - 2, ATop + 1);
      MoveTo(ALeft + Width - 1, ATop);
      LineTo(ALeft + Width - 1, ATop + Height - 1);
      LineTo(ALeft, ATop + Height - 1);
    End;
  bsTopLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop);
      LineTo(ALeft + Width - 1, ATop);
      Pen.Color:=Colorb;
      MoveTo(ALeft, ATop + 1);
      LineTo(ALeft + Width - 1, ATop + 1);
    End;
  bsBottomLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(ALeft, ATop + Height - 1);
      LineTo(ALeft + Width - 1, ATop + Height - 1);
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop + Height - 2);
      LineTo(ALeft + Width - 1, ATop + Height - 2);
    End;
  bsLeftLine:
    With Canvas Do
    Begin
      Pen.Color:=Colora;
      MoveTo(ALeft, ATop);
      LineTo(ALeft, ATop + Height - 1);
      Pen.Color:=Colorb;
      MoveTo(ALeft + 1, ATop);
      LineTo(ALeft + 1, ATop + Height - 1);
    End;
  bsRightLine:
    With Canvas Do
    Begin
      Pen.Color:=Colorb;
      MoveTo(ALeft + Width - 1, ATop);
      LineTo(ALeft + Width - 1, ATop + Height - 1);
      Pen.Color:=Colora;
      MoveTo(ALeft + Width - 2, ATop);
      LineTo(ALeft + Width - 2, ATop + Height);
    End;
  End;
End;

// included by extctrls.pp

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
  Revision 1.7  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.6  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.5  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.4  2002/03/14 23:25:51  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

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

