{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TCustomTrackBar
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  current design flaws:

  - I decided to support some gtk-specific properties in this class. This
    won't break Delphi compatibility but for 100% Delphi compatibility
    a better approach would be to derive another class.
    BTW: When porting to another widget library you can safely ignore

           FScalePosition, FScaleDigits

  Delphi compatibility:

   - the interface is almost like in delphi 4
   - some Delphi properties are not supported by GTK and are currently not
     implemented here, These are:
       frequency, tickstyle and tickmark
   - what about these private procs
      procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
      procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
   - there is a new property which I've implemented because it is a
     nice addon for the GTK interface
      * ScalePos (left, top, right, bottom)

  TODO:

    - implement some more Delphi stuff
    - range checking for min/max could raise an exception
    - use RecreateWnd when the orientation changes!

}
{------------------------------------------------------------------------------
  Method: TCustomTrackBar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the trackbar.
 ------------------------------------------------------------------------------}
constructor TCustomTrackBar.Create (AOwner : TComponent);
begin
  inherited Create (aOwner);
  fCompStyle := csTrackbar;
  ControlStyle := ControlStyle - [csCaptureMouse];
  FLineSize := 1;
  FMax := 10;
  FMin := 0;
  FPosition := 0;
  FPageSize := 2;
  FFrequency := 1;
  FOrientation := trHorizontal;
  FScalePos := trTop;
  FScaleDigits := 0;
  FTickMarks := tmBottomRight;
  FTickStyle := tsAuto;
  FSelStart := 0;
  FSelEnd := 0;
  FShowSelRange := True;
  FReversed := False;
  TabStop := True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.InitializeWnd
  Params: none
  Returns: Nothing

  Set all properties after visual component has been created. Will be called
  from TWinControl.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.InitializeWnd;
begin
  inherited InitializeWnd;
  ApplyChanges;
end;

procedure TCustomTrackBar.Loaded;
begin
  inherited Loaded;
  ApplyChanges;
end;

procedure TCustomTrackBar.ShouldAutoAdjust(var AWidth, AHeight: Boolean);
begin
  if Orientation = trHorizontal then
    begin
      AWidth := True;
      AHeight := not AutoSize;
    end
  else
    begin
      AWidth := not AutoSize;
      AHeight := True;
    end
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetTick
  Params: Value : new tick
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetTick(Value: Integer);
begin
  if HandleAllocated then
    TWSTrackBarClass(WidgetSetClass).SetTick(Self, Value);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetOrientation
  Params: Value : new orientation
  Returns: Nothing

  Change the orientation of the trackbar.
------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetOrientation(Value: TTrackBarOrientation);
var
  OldWidth: LongInt;
  OldHeight: LongInt;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    // switch width and height, but not when loading, because we assume that the
    // lfm contains a consistent combination of Orientation and (width, height)
    if not (csLoading in ComponentState) then
    begin
      OldWidth:=Width;
      OldHeight:=Height;
      Constraints.UpdateInterfaceConstraints;
      SetBounds(Left,Top,OldHeight,OldWidth);
      if HandleAllocated then
        TWSTrackBarClass(WidgetSetClass).SetOrientation(Self, FOrientation);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetParams
  Params:  APosition : new position
           AMin      : new minimum
           AMax      : new maximum
  Returns: Nothing

  Set new parameters for the trackbar.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetParams(APosition, AMin, AMax: Integer);
begin
  if not (csLoading in ComponentState) then
    FixParams(APosition, AMin, AMax);

  if (FPosition = APosition) and (FMin = AMin) and (FMax = AMax) then
    Exit;

  FPosition := APosition;
  FMax := AMax;
  FMin := AMin;
  ApplyChanges;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetPosition
  Params: Value : new position
  Returns: Nothing

  Set actual position of the trackbar.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetPosition(Value: Integer);
begin
  FixParams(Value, FMin, FMax);
  if FPosition <> Value then
  begin
    FPosition := Value;
    if HandleAllocated then
      TWSTrackBarClass(WidgetSetClass).SetPosition(Self, FPosition);
    if not (csLoading in ComponentState) then 
      Changed;
  end;
end;

procedure TCustomTrackBar.SetReversed(const AValue: Boolean);
begin
  if FReversed <> AValue then
  begin
    FReversed := AValue;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetMin
  Params: Value : new minimum
  Returns: Nothing

  Set minimum value of the trackbar.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetMin(Value: Integer);
begin
  if FMin <> Value then
    SetParams(FPosition, Value, FMax);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetMax
  Params: Value : new maximum
  Returns: Nothing

  Set maximum value of the trackbar.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetMax(Value: Integer);
begin
  if FMax <> Value then
    SetParams(FPosition, FMin, Value);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetFrequency
  Params: Value : new frequency
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetFrequency(Value: Integer);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetTickStyle
  Params: Value : new tickstyle
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetTickStyle(Value: TTickStyle);
begin
  if FTickStyle <> Value then
  begin
    FTickStyle := Value;
    ApplyChanges;
    Constraints.UpdateInterfaceConstraints;
    DoAutoSize;
    if HandleAllocated then
      TWSTrackBarClass(WidgetSetClass).SetTickStyle(Self, FTickStyle);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetTickMarks
  Params: Value : new tickmarks
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetTickMarks(Value: TTickMark);
begin
  if FTickMarks <> Value then
  begin
    FTickMarks := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetLineSize
  Params: Value : new linesize
  Returns: Nothing

  Set the increment which is used when one of the arrow-keys is pressed.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetLineSize(Value: Integer);
begin
  if FLineSize <> Value then
  begin
    FLineSize := Value;
    ApplyChanges;
  end
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetPageSize
  Params:  Value : new pagesize
  Returns: Nothing

  Set the increment which is used when one of the arrow-keys is pressed together
  with a modifier or when PgUp/PgDwn are pressed.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetPageSize(Value: Integer);
begin
  if FPageSize <> Value then
  begin
    FPageSize := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.UpdateSelection
  Params:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.UpdateSelection;
begin
end;

class procedure TCustomTrackBar.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomTrackBar;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.ApplyChanges
  Params: none
  Returns: Nothing

  Sends message to update the visual apperance of the object.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.ApplyChanges;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
    TWSTrackBarClass(WidgetSetClass).ApplyChanges(Self);
end;

procedure TCustomTrackBar.Changed;
begin
  if Assigned (FOnChange) then
    FOnChange(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.DoChange
  Params:  Msg  (longint = LM_CHANGE)
  Returns: Nothing

  Update position and call user's callback for Change event.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.DoChange(var msg);
begin
  FPosition := TWSTrackBarClass(WidgetSetClass).GetPosition(Self);
  Assert(True, 'Trace:Trackbar received a message -CHANGE');
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then
    Exit;
  EditingDone;
  Changed;
end;

procedure TCustomTrackBar.FixParams(var APosition, AMin, AMax: Integer);
begin
  if AMin > AMax then AMin := AMax;
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if AMax - AMin > $3FFF then
    // avoid massive slow-down when drawing too many tick marks
    SetTickStyle(tsNone);
end;

class function TCustomTrackBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 100;
  Result.CY := 25;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetScalePos
  Params:  value : position of the scaling text
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetScalePos(Value: TTrackBarScalePos);
begin
  if FScalePos <> Value then
  begin
    FScalePos := Value;
    ApplyChanges;
  end;
end;

procedure TCustomTrackBar.SetSelEnd(const AValue: Integer);
begin
  if FSelEnd <> AValue then
  begin
    FSelEnd := AValue;
    ApplyChanges;
  end;
end;

procedure TCustomTrackBar.SetSelStart(const AValue: Integer);
begin
  if FSelStart <> AValue then
  begin
    FSelStart := AValue;
    ApplyChanges;
  end;
end;

procedure TCustomTrackBar.SetShowSelRange(const AValue: Boolean);
begin
  if FShowSelRange <> AValue then
  begin
    FShowSelRange := AValue;
    ApplyChanges;
  end;
end;

