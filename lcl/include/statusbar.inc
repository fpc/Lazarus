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
  SetBounds(0,TWinControl(AOwner).ClientHeight-21,
            TWinControl(AOwner).ClientWidth,20);
  Align := alBottom;
  FPanels := TStatusPanels.Create(Self);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FSimplePanel := True;
end;


{------------------------------------------------------------------------------}
{   TStatusBar SetSimpleText                                                   }
{------------------------------------------------------------------------------}
procedure TStatusBar.SetSimpleText(const Value : String);
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
  FreeThenNil(FPanels);
  FreeThenNil(FCanvas);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  procedure TStatusBar.InvalidatePanel(PanelIndex: integer;
    PanelParts: TPanelParts);
------------------------------------------------------------------------------}
procedure TStatusBar.InvalidatePanel(PanelIndex: integer;
  PanelParts: TPanelParts);
var
  PanelRect, TextRect: TRect;
begin
  if (PanelParts=[]) or (not HandleAllocated) or (csLoading in ComponentState)
  then exit;
  if ppWidth in PanelParts then begin
    Invalidate;
  end else begin
    GetPanelRect(PanelIndex,PanelRect);
    if ppText in PanelParts then begin
      TextRect:=PanelRect;
      inc(TextRect.Left);
      inc(TextRect.Top);
      dec(TextRect.Right);
      dec(TextRect.Bottom);
      InvalidateRect(Handle,@TextRect,false);
    end;
    if ppBorder in PanelParts then begin
      InvalidateFrame(Handle,@PanelRect,false,2);
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TStatusBar.GetPanelRect(PanelIndex: integer; var ARect: TRect);
------------------------------------------------------------------------------}
procedure TStatusBar.GetPanelRect(PanelIndex: integer; var ARect: TRect);
var
  i: Integer;
begin
  ARect.Left:=0;
  ARect.Top:=0;
  ARect.Bottom:=ClientHeight;
  for i:=0 to PanelIndex-1 do
    inc(ARect.Left,Panels[i].Width);
  if PanelIndex = Panels.Count-1 then begin
    ARect.Right:=ClientWidth-ARect.Left;
    if ARect.Right<ARect.Left then
      ARect.Right:=ARect.Left;
  end
  else
    ARect.Right:=ARect.Left+Panels[PanelIndex].Width;
end;

{------------------------------------------------------------------------------
   TStatusBar DrawBevel
------------------------------------------------------------------------------}
Procedure TStatusBar.DrawBevel(xLeft, PanelNum : Integer );
var
  LeftTopColor,RightBottomColor:TColor;
  I, PL, PW : Longint;
Begin
  if PanelNum = Panels.Count-1 then begin
    PL := Left;
    If Panels.Count > 1 then
      For I := 0 to Panels.Count-2 do
        PL := PL + Panels[I].Width;
    PW := ClientWidth - PL;
  end
  else
    PW := Panels[PanelNum].Width;
  if PW<=0 then exit;

  if (Panels[PanelNum].Bevel in [pbRaised,pbLowered]) then begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(XLeft+1, Top+1, XLeft + PW-1, Top + Height - 1));
    
    if Panels[PanelNum].Bevel = pbRaised then
    begin
      LeftTopColor:=clBtnHighlight;
      RightBottomColor:=clBtnShadow;
    end else begin
      LeftTopColor:=clBtnShadow;
      RightBottomColor:=clBtnHighlight;
    end;
    With Canvas Do Begin
      Pen.Width:=1;
      Pen.Color:=LeftTopColor;
      MoveTo(XLeft,Top+Height-1);
      LineTo(XLeft,Top);
      LineTo(XLeft+PW-1,Top);
      Pen.Color:=RightBottomColor;
      LineTo(XLeft+PW-1,Top+Height);
      MoveTo(XLeft+PW-1,Top+Height-1);
      LineTo(XLeft,Top+Height-1);
    End;
  end else begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect(XLeft, Top, XLeft + PW, Top + Height));
  end;
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
  Style : TTextStyle;
  R : TRect;
  PW : Longint;
Begin
  inherited;
  FillChar(Style, SizeOf(Style),0);
  With Style do begin
    Layout := tlCenter;
    Alignment := taLeftJustify;
    WordBreak := False;
    SingleLine := True;
    ShowPrefix := False;
  end;
  Canvas.Color := Color;
  R := Rect(Left, Top, Left + ClientWidth, Top + ClientHeight);
  if SimplePanel = False then
  Begin
    Style.Opaque := True;
    Style.Clipping := True;
    if Panels.Count = 0 then exit;
    For I := 0 to Panels.Count-1 do
      Begin
        if I = Panels.Count-1 then
          PW := ClientWidth-R.Left
        else
          PW := Panels[I].Width;
        R.Right := R.Left + PW;
        DrawBevel(R.Left,I);
        InflateRect(R, -2, -1);
        Style.Alignment := Panels[I].Alignment;
        Canvas.TextRect(R, 0, 0, Panels[i].Text, Style);
        InflateRect(R, 2, 1);
        //draw divider
        if I < Panels.Count-1 then
          DrawDivider(R.Right);
        R.Left := R.Right;
      end;
  end
  else begin
    Style.Clipping := False;
    Style.Opaque := False;
    InflateRect(R, -2, -2);
    Canvas.FillRect(R);
    InflateRect(R, 2, 2);
    Canvas.TextRect(R, 2, 0, SimpleText, Style);
  end;
End;
// included by comctrls.pp

