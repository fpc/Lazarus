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
  Setbounds(0,TWinControl(AOwner).Height-21,TWInControl(AOwner).Width,20);
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
 FPanels.free;
 inherited Destroy;
end;

Procedure TStatusBar.DrawBevel(xLeft, PanelNum : Integer );
Begin
    Canvas.Brush.Color := color;
    Canvas.FillRect(Rect(XLeft,Top,XLeft +Panels[PanelNum].Width,Top+Height));

  if Panels[PanelNum].Bevel = pbRaised then
  Begin
    Canvas.Pen.Color := clWhite;
    Canvas.Line(XLeft,Top,XLeft+Panels[PanelNum].Width-1,Top);
    Canvas.Line(XLeft,Top,XLeft,Top+Height-1);
    Canvas.Line(XLeft,Top+1,XLeft+Panels[PanelNum].Width-1,Top+1);
    Canvas.Line(XLeft+1,Top,XLeft+1,Top+Height-1);
    Canvas.Pen.Color := clBlack;
    Canvas.Line(XLeft,Top+Height-5,XLeft+Panels[PanelNum].Width-1,Top+Height-5);
    Canvas.Line(XLeft+Panels[PanelNum].Width-2,Top,XLeft+Panels[PanelNum].Width-2,Top+Height-1);
    Canvas.Line(XLeft,Top+Height-6,XLeft+Panels[PanelNum].Width-1,Top+Height-6);
    Canvas.Line(XLeft+Panels[PanelNum].Width-3,Top,XLeft+Panels[PanelNum].Width-3,Top+Height-2);
  end
  else
  if Panels[PanelNum].Bevel = pbLowered then
  Begin
    Canvas.Pen.Color := clBlack;
    Canvas.Line(XLeft,Top,XLeft+Panels[PanelNum].Width-1,Top);
    Canvas.Line(XLeft,Top,XLeft,Top+Height-1);
    Canvas.Line(XLeft,Top+1,XLeft+Panels[PanelNum].Width-1,Top+1);
    Canvas.Line(XLeft+1,Top,XLeft+1,Top+Height-1);
    Canvas.Pen.Color := clWhite;
    Canvas.Line(XLeft,Top+Height-5,XLeft+Panels[PanelNum].Width-1,Top+Height-5);
    Canvas.Line(XLeft+Panels[PanelNum].Width-1,Top,XLeft+Panels[PanelNum].Width-1,Top+Height-1);
    Canvas.Line(XLeft,Top+Height-6,XLeft+Panels[PanelNum].Width-1,Top+Height-6);
    Canvas.Line(XLeft+Panels[PanelNum].Width-2,Top,XLeft+Panels[PanelNum].Width-2,Top+Height-4);
  end
  else
  if Panels[PanelNum].Bevel = pbNone then
  Begin
{    Canvas.Pen.Color := clBlack;
    Canvas.Line(XLeft,Top,XLeft+Panels[PanelNum].Width-1,Top);
    Canvas.Line(XLeft,Top,XLeft,Top+Height-1);
    Canvas.Line(XLeft,Top+Height-1,XLeft+Panels[PanelNum].Width-1,Top+Height-1);
    Canvas.Line(XLeft+Panels[PanelNum].Width-1,Top,XLeft+Panels[PanelNum].Width-1,Top+Height-1);}

  end


End;

Procedure TStatusBar.DrawDivider(X : Integer);
Begin
  Canvas.Pen.Color := clWhite;
  Canvas.Line(X,Top,X,Top+Height);
  Canvas.Pen.Color := clBlack;
  Canvas.Line(X+1,Top,X+1,Top+Height);
End;


Procedure TStatusBar.WMPaint(var Msg: TLMPaint);
var
  I : Integer;
  X,Y : Integer;
Begin
//Writeln('[TSTATUSBAR].WMPAINT');
  inherited;
  if SimplePanel = False then
  Begin
    if Panels.Count = 0 then exit;
    Y := Top; //this shouldn't be needed but it is...
    X := Left;

    For I := 0 to Panels.Count-1 do
      Begin
        if I = Panels.Count-1 then
          Panels[I].Width := Width-X;  //this sets the last panel to the width of the statusbar
        DrawBevel(X,I);
        Canvas.TextOut(X+2,Y+2,Panels[i].Text);
        //draw divider
        if I < Panels.Count-1 then
          DrawDivider(X+Panels[i].Width);
        X := X + Panels[i].Width +1;
 //  Writeln('SimplePanel = False');
 //  Writeln('Panels[0].Text = '+Panels[0].Text);
      end;

//   ExtTextOut(Handle,1,1,0,nil,pchar(Panels[0].Text),Length(Panels[0].Text),nil);
  end
  else
  if SimplePanel = True then
    Begin
      Canvas.TextOut(Left+2,Top+2,SimpleText);
    end;

End;
