(******************************************************************************
                                   TSpeedButton
 ******************************************************************************)

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{------------------------------------------------------------------------------
  Method:  TSpeedbutton.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TSpeedbutton.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FCompStyle := csSpeedButton;

  FGlyph := TButtonGlyph.Create;
  FGlyph.OnChange := @GlyphChanged;

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];

  {set default alignment}
  Align := alNone;
  FMouseInControl := False;
  FDragging := False;
  FSpacing := 4;
  FMargin := -1;
end;

{------------------------------------------------------------------------------
  Method: TSpeedbutton.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TSpeedbutton.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TSpeedbutton.Click
  Params: 
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedbutton.Click;
begin
  inherited Click;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetAllowAllUp
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetAllowAllUp(Value : Boolean);
begin
  if FAllowAllUp <> Value 
  then begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;


{------------------------------------------------------------------------------
  Method: TSpeedButton.SetDown
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetDown(Value : Boolean);
begin
  if FDown <> value 
  then begin
    FDown := Value;
    if FDown 
    then begin
      if FState = bsUp then Invalidate;
      fState := bsExclusive;
    end
    else begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetFlat
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetFlat(Value : Boolean);
begin
  FFlat := Value;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetGlyph
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph := Value;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetGroupIndex
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetGroupIndex(Value : Integer);
begin
  if FGroupIndex <> Value 
  then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetNumGlyphs
  Params: Value : Integer = Number of glyphs in the file/resource
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetNumGlyphs(Value : Integer);
Begin
if Value < 0 then Value := 1;
if Value > 4 then Value := 4;

if Value <> TButtonGlyph(fGlyph).NumGlyphs then
   Begin
    TButtonGlyph(fGlyph).NumGlyphs := Value;
    Invalidate;
   end;
end;


{------------------------------------------------------------------------------
  Method: TSpeedButton.UpdateExclusive
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.UpdateExclusive;
var
  msg : TLMessage;
begin

  if (FGroupIndex <> 0) and (Parent <> nil) 
  then begin
    Assert(False,'Trace:UpdateExclusive-FGroupIndex <> 0 and Parent <> nil');
    MSg.MSg := CM_ButtonPressed;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

{------------------------------------------------------------------------------
  Function: TSpeedButton.GetGlyph
  Params: none
  Returns:  The bitmap

 ------------------------------------------------------------------------------}
function TSpeedbutton.GetGlyph : TBitmap;
begin
  Result := FGlyph.Glyph;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.GetNumGlyphs
  Params: none
  Returns:  The number stored in TButtonGlyph(FGlyph).NumGlyphs

 ------------------------------------------------------------------------------}
Function TSpeedButton.GetNumGlyphs : Integer;
Begin
  Result :=  TButtonGlyph(fGlyph).NumGlyphs;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.GlyphChanged
  Params: Sender - The glyph that changed
  Returns:  zippo

 ------------------------------------------------------------------------------}
Procedure TSpeedButton.GlyphChanged(Sender : TObject);
Begin
  //redraw the button;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TSpeedbutton.Paint
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedbutton.Paint;
const
//  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENINNER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  R : TRect;
  Offset: TPoint;
begin
  if not Enabled
  then begin
    FState := bsDisabled;
    FDragging := False;
  end
//  else
//  if not Visible then exit
  else begin
    if FState = bsDisabled 
    then begin
      if FDown and (GroupIndex <> 0) 
      then FState := bsExclusive
      else FState := bsUp;
    end;
  end;

  Canvas.Font := Self.Font;
  PaintRect := Bounds(Left, Top, Width, Height);
//  PaintRect := Rect(0, 0, Width, Height);

  if not FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if FState in [bsDown, bsExclusive]
    then DrawFlags := DrawFlags or DFCS_PUSHED;
    DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
  end
  else begin   //flat
    if (FState in [bsDown, bsExclusive])
    or (FMouseInControl and (FState <> bsDisabled)) 
    or (csDesigning in ComponentState) 
    then
      DrawEdge(Canvas.Handle, PaintRect, 
        DOWNSTYLES[FState in [bsDown, bsExclusive]],
        FILLSTYLES[Transparent] or BF_RECT)
    else 
      if not Transparent 
      then begin
        Canvas.Brush.Color := clbtnface;//color;
        // Fillrect doesnt paint the outer right and bottom border
        Canvas.FillRect(PaintRect);
      end;
    InflateRect(PaintRect, -1, -1);
  end;

  if FState in [bsDown, bsExclusive]
  then begin
    if (FState = bsExclusive)
    and (not FFlat or not FMouseInControl) 
    then begin
      //this needs to be done yet.
      Assert(False,'Trace:TODO: ALLOCPATTERNBITMAP');
//      Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      Canvas.FillRect(PaintRect);
    end;
    Offset.X := 1;
    Offset.Y := 1;
  end
  else begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  //this needs to be done yet.
  Assert(False,'Trace:TODO: DRAWTEXTBIDIMODEFLAGS');
  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
    FSpacing, FState, Transparent, (0));
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.UpdateTracking
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.UpdateTracking;
var
  P : TPoint;
begin
  Assert(False,'Trace:[Update Tracking]');
  
  if FFlat and Enabled 
  then begin
    GetCursorPos(p);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    Assert(False, Format('Trace:[TSpeedButton.UpdateTracking] ParentHandle: 0x%x', [Integer(self.parent.handle)]));
    if FMouseInControl 
    then Perform(CM_MOUSELEAVE,0,0)
    else Perform(CM_MOUSEENTER,0,0);
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.MouseDown
  Params: Button:
          Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Assert(False,'Trace:[TSpeedButton.MouseDown]');
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled 
  then begin
    Assert(False,'Trace:[TSpeedButton.MouseDown] Checking FDown');
    if not FDown 
    then begin
      Assert(False,'Trace:[TSpeedButton.MouseDown] It wasn''t down, so now it will display it down!');
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.MouseMove
  Params: Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  Assert(False,Format('Trace:[TSpeedButton.MouseMove] X:%d Y:%d', [X, Y]));
  inherited MouseMove(Shift, X, Y);

  if FDragging 
  then begin
    Assert(False,'Trace:FDragging is true');
    if not FDown 
    then NewState := bsUp
    else NewState := bsExclusive;
    
    if  (X >= 0) and (X < ClientWidth) 
    and (Y >= 0) and (Y <= ClientHeight) 
    then begin
      if FDown 
      then NewState := bsExclusive 
      else NewState := bsDown;
    end;
    
    if NewState <> FState 
    then begin
      FState := NewState;
      Invalidate;
    end;
  end
  else begin
    Assert(False,'Trace:FDragging is False');
    if not FMouseInControl 
    then begin
      Assert(False,'Trace:FMouseInControl is false');
      UpdateTracking;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.MouseUp
  Params: Button:
          Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  Assert(False,'Trace:TSPEEDBUTTON.MOUSEUP');
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging 
  then begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    
    if FGroupIndex = 0 
    then begin
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else begin
      if DoClick 
      then begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    end;
    if DoClick then Click;
    UpdateTracking;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.CMButtonPressed
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.CMButtonPressed(var Message : TLMessage);
var
  Sender : TSpeedButton;
begin
  if Message.WParam = FGroupIndex 
  then begin
    Sender := TSpeedButton(Message.LParam);
    if Sender <> Self 
    then begin
      if Sender.Down and FDown 
      then begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;


{------------------------------------------------------------------------------
  Method: TSpeedButton.CMEnabledChanged
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.CMEnabledChanged(var Message: TLMEssage);
Begin
  //Should create a new glyph based on the new state

  UpdateTracking;
  Repaint;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.CMMouseEnter
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.CMMouseEnter(var Message :TLMessage);
begin
  Assert(False,'Trace:[TSpeedButton.CMMouseEnter]');

  inherited CMMouseEnter(Message);

  if  FFlat and not FMouseInControl 
  and Enabled and (GetCapture = 0) 
  then begin
    FMouseInControl := True;
    RePaint;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.CMMouseLeave
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.CMMouseLeave(var Message :TLMessage);
begin
   Assert(False,'Trace:[TSpeedButton.CMMouseLeave]');

   inherited CMMouseLeave(Message);

   if  FFlat and FMouseInControl 
   and Enabled and not FDragging 
   then begin
     FMouseInCOntrol := False;
     Invalidate;
   end;
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.6  2001/03/19 14:00:51  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.5  2001/02/06 14:52:47  lazarus
  Changed TSpeedbutton in gtkobject so it erases itself when it's set to visible=false;
  Shane

  Revision 1.4  2001/01/12 18:46:50  lazarus
  Named the speedbuttons in MAINIDE and took out some writelns.
  Shane

  Revision 1.3  2001/01/04 16:12:54  lazarus
  Removed some writelns and changed the property editor for TStrings a bit.
  Shane

  Revision 1.2  2001/01/03 18:44:54  lazarus
  The Speedbutton now has a numglyphs setting.
  I started the TStringPropertyEditor

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.10  2000/06/04 10:00:33  lazarus
  MWE:
    * Fixed bug #6.

  Revision 1.9  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

}
