{******************************************************************************
                                   TSpeedButton
 ******************************************************************************
 
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
  ControlStyle := ControlStyle + [csCaptureMouse]-[csSetCaption]-csMultiClicks;

  {set default alignment}
  Align := alNone;
  FLayout:= blGlyphLeft;
  FAllowAllUp:= false;
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
  FreeAndNil(FGlyph);
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
var
  OldState: TButtonState;
begin
  if FGroupIndex = 0 then Value:= false;
  if FDown <> Value then begin
    if FDown and not FAllowAllUp then Exit;
    FDown := Value;
    OldState := fState;
    if FDown then begin
      fState := bsExclusive;
    end else begin
      FState := bsUp;
    end;
    if OldState<>FState then
      Invalidate;
    if Value then UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetFlat
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetFlat(const Value : boolean);
begin
  if FFlat <> Value then begin
    FFlat := Value;
    Invalidate;
  end;
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
procedure TSpeedButton.SetGroupIndex(const Value : Integer);
begin
  if FGroupIndex <> Value 
  then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetMargin
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetMargin(const Value : Integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetNumGlyphs
  Params: Value : Integer = Number of glyphs in the file/resource
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetNumGlyphs(Value : integer);
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
  Method: TSpeedButton.SetSpacing
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetSpacing(const Value : Integer);
begin
  if FSpacing <> Value then begin
    FSpacing := Value;
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
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENINNER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags, GlyphWidth, GlyphHeight: Integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize: TSize;
  //BrushStyle : TBrushStyle;
  M, S : integer;
  TXTStyle : TTextStyle;
  SIndex : Longint;
  TMP : String;
begin
  if not Enabled then begin
    FState := bsDisabled;
    FDragging := False;
  end else begin
    if FState = bsDisabled 
    then begin
      if FDown and (GroupIndex <> 0) 
      then FState := bsExclusive
      else FState := bsUp;
    end;
  end;
  if FGlyph=nil then exit;

  PaintRect := Bounds(Left, Top, Width, Height);
//  PaintRect := Rect(0, 0, Width, Height);

  {if Transparent and not (csDesigning in ComponentState) then
    BrushStyle:= bsClear
  else
    BrushStyle:= bsSolid;}

  DrawFlags:=DFCS_BUTTONPUSH;
  if Flat and (not (csDesigning in ComponentState)) then inc(DrawFlags,DFCS_FLAT);
  if FState in [bsDown, bsExclusive] then inc(DrawFlags,DFCS_PUSHED);
  if FMouseInControl then inc(DrawFlags,DFCS_CHECKED);
  if not Enabled then inc(DrawFlags,DFCS_INACTIVE);
  
  DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);

  GlyphWidth:= TButtonGlyph(FGlyph).Glyph.Width;
  if TButtonGlyph(FGlyph).NumGlyphs > 1 then
    GlyphWidth:=GlyphWidth div NumGlyphs;
  GlyphHeight:=TButtonGlyph(FGlyph).Glyph.Height;
  
  ClientSize.cx:= PaintRect.Right - PaintRect.Left;
  ClientSize.cy:= PaintRect.Bottom - PaintRect.Top;
  
  if Caption <> '' then begin
    TMP := Caption;
    SIndex := DeleteAmpersands(TMP);
    TextSize:= Canvas.TextExtent(TMP);
    If SIndex > 0 then
      If SIndex <= Length(TMP) then begin
        FShortcut := Ord(TMP[SIndex]);
      end;
  end
  else begin
    TextSize.cx:= 0;
    TextSize.cy:= 0;
  end;

  if (GlyphWidth = 0) or (GlyphHeight = 0)
  or (TextSize.cx = 0) or (TextSize.cy = 0)
  then
    S:= 0
  else
    S:= Spacing;
  
  { Calculate caption and glyph layout }
  
  if Margin = -1 then begin
    if S = -1 then begin
      TotalSize.cx:= TextSize.cx + GlyphWidth;
      TotalSize.cy:= TextSize.cy + GlyphHeight;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M:= (ClientSize.cx - TotalSize.cx) div 3
      else
        M:= (ClientSize.cy - TotalSize.cy) div 3;
      S:= M;
    end else begin
      TotalSize.cx:= GlyphWidth + S + TextSize.cx;
      TotalSize.cy:= GlyphHeight + S + TextSize.cy;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M:= (ClientSize.cx - TotalSize.cx + 1) div 2
      else
        M:= (ClientSize.cy - TotalSize.cy + 1) div 2
    end;  
  end else begin
    if S = -1 then begin
      TotalSize.cx:= ClientSize.cx - (Margin + GlyphWidth);
      TotalSize.cy:= ClientSize.cy - (Margin + GlyphHeight);
      if Layout in [blGlyphLeft, blGlyphRight] then
        S:= (TotalSize.cx - TextSize.cx) div 2
      else
      	S:= (TotalSize.cy - TextSize.cy) div 2;
    end;
    M:= Margin
  end;  

  case Layout of
    blGlyphLeft : begin
      Offset.X:= M;
      Offset.Y:= (ClientSize.cy - GlyphHeight + 1) div 2;
      OffsetCap.X:= Offset.X + GlyphWidth + S;
      OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
    end;
    blGlyphRight : begin
      Offset.X:= ClientSize.cx - M - GlyphWidth;
      Offset.Y:= (ClientSize.cy - GlyphHeight + 1) div 2;
      OffsetCap.X:= Offset.X - S - TextSize.cx;
      OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
    end;
    blGlyphTop : begin
      Offset.X:= (ClientSize.cx - GlyphWidth + 1) div 2;
      Offset.Y:= M;
      OffsetCap.X:= (ClientSize.cx - TextSize.cx + 1) div 2;
      OffsetCap.Y:= Offset.Y + GlyphHeight + S;
    end;
    blGlyphBottom : begin
      Offset.X:= (ClientSize.cx - GlyphWidth + 1) div 2;
      Offset.Y:= ClientSize.cy - M - GlyphHeight;
      OffsetCap.X:= (ClientSize.cx - TextSize.cx + 1) div 2;
      OffsetCap.Y:= Offset.Y - S - TextSize.cy; 
    end;
  end;  

  //this needs to be done yet.
  //Assert(False,'Trace:TODO: DRAWTEXTBIDIMODEFLAGS');

  FGlyph.Draw(Canvas, PaintRect, Offset, FState, Transparent, 0);
  if Caption <> '' then begin
    Canvas.Brush.Color:=clBlue;
    Canvas.FillRect(PaintRect);
    TXTStyle := Canvas.TextStyle;
    TXTStyle.Opaque := False;
    TXTStyle.Clipping := True;
    TXTStyle.ShowPrefix := True;
    TXTStyle.Alignment := taLeftJustify;
    TXTStyle.Layout := tlTop;
    TXTStyle.SystemFont := True;//Match System Default Style
    Canvas.TextRect(PaintRect, OffsetCap.X, OffsetCap.Y, Caption, TXTStyle);
  end;
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
  if FFlat and Enabled
  then begin
    GetCursorPos(p);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
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
procedure TSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  
  if csDesigning in ComponentState then exit;
  
  if (Button = mbLeft) and Enabled
  then begin
    if not FDown
    then begin
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
  inherited MouseMove(Shift, X, Y);
  
  if csDesigning in ComponentState then exit;

  if FDragging 
  then begin
    Assert(False,'Trace:FDragging is true');
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;
    
    if  (X >= 0) and (X < Width)
    and (Y >= 0) and (Y < Height)
    then begin
      if FDown then
        NewState := bsExclusive
      else
        NewState := bsDown;
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
procedure TSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not (csDesigning in ComponentState) and FDragging
  then begin
    FDragging := False;

    if FGroupIndex = 0 
    then begin
      FState := bsUp;
      FMouseInControl := False;
      if not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else begin
      SetDown(not FDown);
      if FDown then Repaint;
    end;
    UpdateTracking;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetLayout
  Params: Value: new layout value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetLayout(const Value : TButtonLayout);
begin
  if Value <> FLayout then begin
    FLayout:= Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TSpeedButton.SetTransparent
  Params: Value: new transparency value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TSpeedButton.SetTransparent(const Value : boolean);
begin
  if Value <> FTransparent then begin
    FTransparent:= Value;
    if Value then ControlStyle:= ControlStyle + [csOpaque]
    else ControlStyle:= ControlStyle - [csOpaque];
    Invalidate;
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
  if csDesigning in ComponentState then exit;
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
  inherited CMMouseEnter(Message);

  if csDesigning in ComponentState then exit;
  
  if  {FFlat and }not FMouseInControl
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
  inherited CMMouseLeave(Message);

  if csDesigning in ComponentState then exit;
  
  if  {FFlat and }FMouseInControl
  and Enabled and not FDragging
  then begin
    FMouseInControl := False;
    Invalidate;
  end;
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.26  2002/09/12 15:35:57  lazarus
  MG: small bugfixes

  Revision 1.25  2002/09/06 16:14:19  lazarus
  MG: fixed removing TSpeedButton

  Revision 1.24  2002/09/03 22:31:25  lazarus
  MG: removed old workaround in TSpeedButton

  Revision 1.23  2002/09/03 08:07:20  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.22  2002/09/01 16:11:21  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.21  2002/08/30 12:32:21  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.20  2002/08/27 06:34:26  lazarus
  MG: fixed codetools proc collection

  Revision 1.19  2002/08/26 17:28:21  lazarus
  MG: fixed speedbutton in designmode

  Revision 1.18  2002/08/24 13:41:29  lazarus
  MG: fixed TSpeedButton.SetDown and Invalidate

  Revision 1.17  2002/08/22 16:43:35  lazarus
  MG: improved theme support from Andrew

  Revision 1.16  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.15  2002/06/01 08:41:28  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.14  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.13  2002/02/25 17:08:50  lazarus
  MG: reduced hints

  Revision 1.12  2002/02/24 20:51:24  lazarus
  Improved TSpeedButton (Glyph, Spacing, Margin, drawing)
  Added PageCount to TNotebook
  Optimized component selection buttons a bit.

  Revision 1.11  2001/11/22 14:33:26  lazarus
  MG: fixed painting background of flat speedbuttons

  Revision 1.10  2001/10/18 13:01:33  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.9  2001/07/03 10:30:32  lazarus
  MG: speedbuttonglyph centered, buttonglyph border fixed

  Revision 1.8  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.7  2001/03/19 14:40:49  lazarus
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
