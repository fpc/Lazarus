{%MainUnit ../buttons.pp}

{******************************************************************************
                                   TCustomSpeedButton
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
  Method:  TCustomSpeedButton.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCustomSpeedButton.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FCompStyle := csSpeedButton;

  FGlyph := TButtonGlyph.Create;
  FGlyph.OnChange := @GlyphChanged;

  SetInitialBounds(0, 0, 23, 22);
  ControlStyle := ControlStyle + [csCaptureMouse]-[csSetCaption];

  FLayout:= blGlyphLeft;
  FAllowAllUp:= false;
  FMouseInControl := False;
  FDragging := False;
  FSpacing := 4;
  FMargin := -1;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCustomSpeedButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.Click
  Params:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.Click;
begin
  inherited Click;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetAllowAllUp
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetAllowAllUp(Value : Boolean);
begin
  if FAllowAllUp <> Value
  then begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetDown
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetDown(Value : Boolean);
var
  OldState: TButtonState;
  OldDown: Boolean;
begin
  if FGroupIndex = 0 then Value:= false;
  if FDown <> Value then begin
    if FDown and not FAllowAllUp then Exit;
    OldDown:=FDown;
    FDown := Value;
    OldState := fState;
    if FDown then begin
      fState := bsExclusive;
    end else begin
      FState := bsUp;
    end;
    if (OldDown<>FDown) or (OldState<>FState) then Invalidate;
    if Value then UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetFlat
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetFlat(const Value : boolean);
begin
  if FFlat <> Value then begin
    FFlat := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetGlyph
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph := Value;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetGroupIndex
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetGroupIndex(const Value : Integer);
begin
  if FGroupIndex <> Value
  then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetMargin
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetMargin(const Value : Integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetNumGlyphs
  Params: Value : Integer = Number of glyphs in the file/resource
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetNumGlyphs(Value : integer);
Begin
  if Value < 0 then Value := 1;
  if Value > 4 then Value := 4;

  if Value <> TButtonGlyph(fGlyph).NumGlyphs then
  Begin
    TButtonGlyph(fGlyph).NumGlyphs :=  TNumGlyphs(Value);
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetSpacing
  Params: Value:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetSpacing(const Value : Integer);
begin
  if FSpacing <> Value then begin
    FSpacing := Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  procedure TCustomSpeedButton.RealSetText(const Value: TCaption);
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.RealSetText(const Value: TCaption);
begin
  if Caption=Value then exit;
  inherited RealSetText(Value);
  Invalidate;
end;

{------------------------------------------------------------------------------
  procedure TCustomSpeedButton.UpdateState(InvalidateOnChange: boolean);
 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.UpdateState(InvalidateOnChange: boolean);
var
  OldState: TButtonState;
begin
  OldState:=FState;
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
  if FState<>OldState then
    if (Action is TCustomAction) then
      TCustomAction(Action).Checked := FState=bsDown;

  if InvalidateOnChange
  and ((FState<>OldState) or (FLastDrawFlags<>GetDrawFlags))
  then
    Invalidate;
end;

{------------------------------------------------------------------------------
  function TCustomSpeedButton.GetDrawFlags: integer;
 ------------------------------------------------------------------------------}
function TCustomSpeedButton.GetDrawFlags: integer;
begin
  // if flat and not mouse in control and not down, don't draw anything
  if FFlat and not FMouseInControl and not (FState in [bsDown, bsExclusive]) then
  begin
    Result := 0;
  end else begin
    Result:=DFCS_BUTTONPUSH;
    if FState in [bsDown, bsExclusive] then 
      inc(Result,DFCS_PUSHED);
    if not Enabled then
      inc(Result,DFCS_INACTIVE);
    if FFlat then
      inc(Result,DFCS_FLAT);
  end;
end;

procedure TCustomSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clMaroon; // whatever
      Canvas.FillRect(Rect(0,0,Width,Height));
      ImageList.Draw(Canvas,0,0,Index,true);
    end;
  end;

begin
  inherited ActionChange(Sender,CheckDefaults);
  if Sender is TCustomAction then begin
    with TCustomAction(Sender) do begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil)
      and (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images,ImageIndex);
    end;
  end;
end;

function TCustomSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TSpeedButtonActionLink;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.UpdateExclusive
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.UpdateExclusive;
var
  msg : TLMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil)
  then begin
    MSg.MSg := CM_ButtonPressed;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

{------------------------------------------------------------------------------
  Function: TCustomSpeedButton.GetGlyph
  Params: none
  Returns:  The bitmap

 ------------------------------------------------------------------------------}
function TCustomSpeedButton.GetGlyph : TBitmap;
begin
  Result := FGlyph.Glyph;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.GetNumGlyphs
  Params: none
  Returns:  The number stored in TButtonGlyph(FGlyph).NumGlyphs

 ------------------------------------------------------------------------------}
Function TCustomSpeedButton.GetNumGlyphs : Integer;
Begin
  Result :=  TButtonGlyph(fGlyph).NumGlyphs;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.GlyphChanged
  Params: Sender - The glyph that changed
  Returns:  zippo

 ------------------------------------------------------------------------------}
Procedure TCustomSpeedButton.GlyphChanged(Sender : TObject);
Begin
  //redraw the button;
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.Paint
  Params: none
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.Paint;
var
  PaintRect: TRect;
  GlyphWidth, GlyphHeight: Integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize: TSize;
  //BrushStyle : TBrushStyle;
  M, S : integer;
  TXTStyle : TTextStyle;
  SIndex : Longint;
  TMP : String;
begin
  UpdateState(false);
  if FGlyph=nil then exit;

  PaintRect:=ClientRect;

  {if Transparent and not (csDesigning in ComponentState) then
    BrushStyle:= bsClear
  else
    BrushStyle:= bsSolid;}

  FLastDrawFlags:=GetDrawFlags;
  //DebugLn('TCustomSpeedButton.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  
  // do not draw anything if flat and mouse not in control (simplified)
  if FLastDrawFlags <> 0 then
    DrawFrameControl(Canvas.GetUpdatedHandle([csBrushValid,csPenValid]),
      PaintRect, DFC_BUTTON, FLastDrawFlags);
  //writeln('TCustomSpeedButton.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name,
  //  ' DFCS_BUTTONPUSH=',FLastDrawFlags and DFCS_BUTTONPUSH,
  //  ' DFCS_PUSHED=',FLastDrawFlags and DFCS_PUSHED,
  //  ' DFCS_INACTIVE=',FLastDrawFlags and DFCS_INACTIVE,
  //  ' DFCS_FLAT=',FLastDrawFlags and DFCS_FLAT,
  //  '');

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

  // Calculate caption and glyph layout

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

  FGlyph.Draw(Canvas, PaintRect, Offset, FState, Transparent, 0);
  if Caption <> '' then begin
    TXTStyle := Canvas.TextStyle;
    TXTStyle.Opaque := False;
    TXTStyle.Clipping := True;
    TXTStyle.ShowPrefix := True;
    TXTStyle.Alignment := taLeftJustify;
    TXTStyle.Layout := tlTop;
    TXTStyle.SystemFont := True;//Match System Default Style
    With PaintRect, OffsetCap do begin
      Left := Left + X;
      Top := Top + Y;
    end;
    If not Enabled then begin
      Canvas.Font.Color := clBtnHighlight;
      OffsetRect(PaintRect, 1, 1);
      Canvas.TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
      Canvas.Font.Color := clBtnShadow;
      OffsetRect(PaintRect, -1, -1);
    end
    else
      Canvas.Font.Color := clBtnText;
    //DebugLn('TCustomSpeedButton.Paint PaintRect=',PaintRect.Left,',',PaintRect.TOp,',',PaintRect.Right,',',PaintRect.Bottom);
    Canvas.TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
  end;

  inherited Paint;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseDown
  Params: Button:
          Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then exit;

  if (Button = mbLeft) and Enabled
  then begin
    if not FDown
    then begin
      FState := bsDown;
      if (Action is TCustomAction) then
        TCustomAction(Action).Checked := False;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseMove
  Params: Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
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
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseUp
  Params: Button:
          Shift:
          X, Y:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  OldState: TButtonState;
begin
  if not (csDesigning in ComponentState) and FDragging
  then begin
    FDragging := False;
    OldState:=FState;

    if FGroupIndex = 0
    then begin
      FState := bsUp;
      if OldState<>FState then Invalidate;
    end
    else begin
      SetDown(not FDown);
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetLayout
  Params: Value: new layout value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetLayout(const Value : TButtonLayout);
begin
  if Value <> FLayout then begin
    FLayout:= Value;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.SetTransparent
  Params: Value: new transparency value
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.SetTransparent(const Value : boolean);
begin
  if Value <> FTransparent then begin
    FTransparent:= Value;
    if Value then
      ControlStyle:= ControlStyle + [csOpaque]
    else
      ControlStyle:= ControlStyle - [csOpaque];
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.CMButtonPressed
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.CMButtonPressed(var Message : TLMessage);
var
  Sender : TCustomSpeedButton;
begin
  if Message.WParam = WParam(FGroupIndex)
  then begin
    Sender := TCustomSpeedButton(Message.LParam);
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
  Method: TCustomSpeedButton.CMEnabledChanged
  Params: Message:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.CMEnabledChanged(var Message: TLMEssage);
Begin
  //Should create a new glyph based on the new state
  Invalidate;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseEnter
  Params:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseEnter;
begin
  inherited MouseEnter;
  if csDesigning in ComponentState then exit;

  if not FMouseInControl
  and Enabled and (GetCapture = 0)
  then begin
    FMouseInControl := True;
    UpdateState(true);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomSpeedButton.MouseLeave
  Params:
  Returns:  nothing

 ------------------------------------------------------------------------------}
procedure TCustomSpeedButton.MouseLeave;
begin
  inherited MouseLeave;
  if csDesigning in ComponentState then exit;

  if FMouseInControl
  and Enabled {and not FDragging}
  then begin
    FMouseInControl := False;
    UpdateState(true);
  end;
end;

{ TSpeedButtonActionLink }

procedure TSpeedButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomSpeedButton;
end;

function TSpeedButtonActionLink.IsCheckedLinked: Boolean;
var
  SpeedButton: TCustomSpeedButton;
begin
  SpeedButton:=TCustomSpeedButton(FClient);
  Result := inherited IsCheckedLinked
            and (SpeedButton.GroupIndex <> 0)
            and SpeedButton.AllowAllUp
            and (SpeedButton.Down = (Action as TCustomAction).Checked);
end;

function TSpeedButtonActionLink.IsGroupIndexLinked: Boolean;
var
  SpeedButton: TCustomSpeedButton;
begin
  SpeedButton:=TCustomSpeedButton(FClient);
  Result := (SpeedButton is TCustomSpeedButton) and
    (SpeedButton.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TSpeedButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TCustomSpeedButton(FClient).GroupIndex := Value;
end;

procedure TSpeedButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TCustomSpeedButton(FClient).Down := Value;
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}


// included by buttons.pp

{ =============================================================================

  $Log$
  Revision 1.61  2004/12/12 23:40:55  mattias
  published TSpeedButton.OnDoubleClick

  Revision 1.60  2004/09/24 13:45:32  mattias
  fixed TCanvas.TextRect Delphi compatible Rect and added TBarChart from Michael VC

  Revision 1.59  2004/07/15 10:43:38  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.58  2004/07/13 17:47:15  mattias
  fixed mouse enter/leave for TCustomSpeedButton

  Revision 1.57  2004/06/28 20:03:33  mattias
  fixed TGtkWidgetSet.DrawFrameControl

  Revision 1.56  2004/06/28 18:57:55  mattias
  fixed GetControlAtPos for non designing

  Revision 1.55  2004/06/20 12:29:26  micha
  make flat speedbuttons nicer

  Revision 1.54  2004/06/19 15:36:29  micha
  fix speedbutton group in design mode

  Revision 1.53  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.52  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.51  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.50  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.49  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.48  2004/02/11 11:34:15  mattias
  started new TToolBar

  Revision 1.47  2004/02/10 00:05:03  mattias
  TCustomSpeedButton now uses MaskBlt

  Revision 1.46  2004/02/05 09:45:33  mattias
  implemented Actions for TCustomSpeedButton, TMenuItem, TCheckBox

  Revision 1.45  2004/02/02 18:01:31  mattias
  added TCustomSpeedButton.Action and TBitBtn.Action

  Revision 1.44  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.43  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.42  2003/09/27 09:49:30  mattias
  fix for speedbutton from Micha

  Revision 1.41  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.40  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.39  2003/06/18 11:21:06  mattias
  fixed taborder=0, implemented TabOrder Editor

  Revision 1.38  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.37  2003/04/02 13:23:23  mattias
  fixed default font

  Revision 1.36  2003/03/11 07:46:44  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.35  2003/02/16 01:40:43  mattias
  fixed uninitialized style

  Revision 1.34  2003/02/06 20:46:51  mattias
  default fpc src dirs and clean ups

  Revision 1.33  2003/02/06 06:33:57  mattias
  fixed message

  Revision 1.32  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.31  2002/11/21 18:49:53  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.30  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.29  2002/09/18 17:07:25  lazarus
  MG: added patch from Andrew

  Revision 1.28  2002/09/13 11:49:47  lazarus
  Cleanups, extended TStatusBar, graphic control cleanups.

  Revision 1.27  2002/09/12 15:53:10  lazarus
  MG: small bugfixes

  Revision 1.26  2002/09/12 15:35:57  lazarus
  MG: small bugfixes

  Revision 1.25  2002/09/06 16:14:19  lazarus
  MG: fixed removing TCustomSpeedButton

  Revision 1.24  2002/09/03 22:31:25  lazarus
  MG: removed old workaround in TCustomSpeedButton

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
  MG: fixed TCustomSpeedButton.SetDown and Invalidate

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
  Improved TCustomSpeedButton (Glyph, Spacing, Margin, drawing)
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
  Changed TCustomSpeedButton in gtkobject so it erases itself when it's set to visible=false;
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
