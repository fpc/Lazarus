// included by comctrls.pp
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

{------------------------------------------------------------------------------}
{   TStatusBar Constructor                                                     }
{------------------------------------------------------------------------------}
constructor TStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csStatusBar;
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  Color := clBtnFace;
  Height := 19;
  Setbounds(0,TWinControl(AOwner).Height-21,TWinControl(AOwner).Width,20);
  Align := alBottom;
  FPanels := TStatusPanels.Create(Self);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FSimplePanel := True;
//  FSizeGrip := True;
end;


{------------------------------------------------------------------------------}
{   TStatusBar SetSimpleText                                                   }
{------------------------------------------------------------------------------}
procedure TStatusBar.SetSimpleText(Value : String);
begin
   if FSimpleText <> value then
   begin
      FSimpleText := Value;
      Invalidate;
   end;
end;

Procedure TStatusBar.SetSimplePanel(Value : Boolean);
Begin
  if FSimplePanel <> Value then
     Begin
      FSimplePanel := Value;
      Invalidate;
     end;
End;

procedure TStatusBar.SetPanels(Value: TStatusPanels);
begin
  FPanels.Assign(Value);
end;



{------------------------------------------------------------------------------}
{   TStatusBar Destructor                                                      }
{------------------------------------------------------------------------------}
destructor TStatusBar.Destroy;
begin
  FPanels.Free;
  FCanvas.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{   TStatusBar DrawBevel                                                       }
{------------------------------------------------------------------------------}
Procedure TStatusBar.DrawBevel(xLeft, PanelNum : Integer );
var
  Colora,Colorb:TColor;
Begin

  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(XLeft,Top,XLeft+Panels[PanelNum].Width,Top+Height));

  if Panels[PanelNum].Bevel = pbRaised then
  begin
   Colora:=clWhite;
   Colorb:=clGray;
  end;
  if Panels[PanelNum].Bevel = pbLowered then
  begin
   Colora:=clGray;
   Colorb:=clWhite;
  end;

  Canvas.Pen.Width:=1;
  if (Panels[PanelNum].Bevel = pbRaised) or (Panels[PanelNum].Bevel = pbLowered) then
   With Canvas Do
   Begin
      Pen.Color:=Colora;
      MoveTo(XLeft,Top+Height-1);
      LineTo(XLeft,Top);
      LineTo(XLeft+Panels[PanelNum].Width-1,Top);
      Pen.Color:=Colorb;
      LineTo(XLeft+Panels[PanelNum].Width-1,Top+Height-1);
      LIneTo(XLeft,Top+Height-1);
   End;

End;


Procedure TStatusBar.DrawDivider(X : Integer);
Begin
  Canvas.Pen.Width:=1;
  Canvas.Pen.Color := clBtnFace;
  Canvas.Line(X,Top,X,Top+Height-1);
  Canvas.Pen.Color := clBtnFace;
  Canvas.Line(X+1,Top,X+1,Top+Height-1);
End;


Procedure TStatusBar.WMPaint(var Msg: TLMPaint);
var
  I : Integer;
  X,Y,X2 : Integer;
Begin
  inherited;
  X2:=(Height-Canvas.TextHeight('L')) div 2;
  if SimplePanel = False then
  Begin
    if Panels.Count = 0 then exit;
    Y := Top; //this shouldn't be needed but it is...
    X := Left;

    For I := 0 to Panels.Count-1 do
      Begin
        if I = Panels.Count-1 then
          // this sets the last panel to the width of the statusbar
          Panels[I].Width := ClientWidth-X;
        DrawBevel(X,I);
        Canvas.TextOut(X+2,Y+X2,Panels[i].Text);
        //draw divider
        if I < Panels.Count-1 then
          DrawDivider(X+Panels[i].Width);
        inc(X);
        X := X + Panels[i].Width+1;
      end;
  end
  else
    Canvas.TextOut(Left+2,Top+X2,SimpleText);
End;
// included by comctrls.pp

