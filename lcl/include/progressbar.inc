{%MainUnit ../comctrls.pp}
{******************************************************************************
                                  TCustomProgressBar
 ******************************************************************************

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
 
  current design flaws:

  - I decided to support some gtk-specific properties in this class. This
    won't break Delphi compatibility but for 100% Delphi compatibility
    a better approach would be to derive another class.
    BTW: When porting to another widget library you can safely ignore

           FBarShowText 
           FBarTextFormat

  - FBarTextFormat is a fixed string by now, hard-coded in the gtk-interface
  - lot's of properties are missing
  - I spend no thought on the usage of type integer for the range for the bar,
    maybe this can cause trouble some day (and already will when FMin < 0!)
}


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the progressbar.
 ------------------------------------------------------------------------------}
constructor TCustomProgressBar.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csProgressBar;
  FPosition  := 0;
  FStep := 10;
  FMin := 0;
  FMax := 100;
  FSmooth := False;
  FOrientation := pbHorizontal;
  FBarShowText := False;
  FBarTextFormat := '%v from [%l-%u] (=%p%%)';
  FStyle := pbstNormal;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.InitializeWnd
  Params: none
  Returns: Nothing

  Set all properties after visual component has been created. Will be called
  from TWinControl.
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.InitializeWnd;
begin
  inherited InitializeWnd;
  ApplyChanges;
end;

procedure TCustomProgressBar.Loaded;
begin
  inherited Loaded;
  ApplyChanges;
end;

class function TCustomProgressBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 100;
  Result.CY := 20;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.GetMin
  Params:  Nothing
  Returns: actual minimum value of the progressbar

  Retrieve the actual minimum value of the progressbar.
 ------------------------------------------------------------------------------}
function TCustomProgressBar.GetMin: Integer;
begin
  Result := FMin;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.GetMax
  Params:  Nothing
  Returns: actual maximum value of the progressbar

  Retrieve the actual maximum value of the progressbar.
 ------------------------------------------------------------------------------}
function TCustomProgressBar.GetMax: Integer;
begin
  Result := FMax;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.GetPosition
  Params:  Nothing
  Returns: actual position of the progressbar

  Retrieve the position of the progressbar.
 ------------------------------------------------------------------------------}
function TCustomProgressBar.GetPosition: Integer;
begin
  Result := FPosition;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetParams
  Params:  Min & Max for the progressbar
  Returns: Nothing

  Set new minimum and maximum values for the progressbar.
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetParams(AMin, AMax: Integer);
begin
  SetMax(AMax);
  SetMin(AMin);
end;


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetMin
  Params:  Minimum value for the progressbar
  Returns: Nothing

  Set new minimum value for the progressbar.
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetMin(Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    ApplyChanges;
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetMax
  Params:  Maximum value for the progressbar
  Returns: Nothing

  Set new maximum value for the progressbar.
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetMax(Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetPosition
  Params:  New acutal position for the progressbar
  Returns: Nothing

  Set new new acutal position for the progressbar
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetPosition(Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    ApplyChanges;
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetPosition
  Params:  New stepping value for the progressbar
  Returns: Nothing

  Set new stepping value for the progressbar
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetStep(Value: Integer);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetPosition
  Params:  New orientation of the progressbar
  Returns: Nothing

  Set new orientation.
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetOrientation (Value : TProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    Forientation := Value;
    ApplyChanges;
  end;
end;

procedure TCustomProgressBar.SetStyle(const AValue: TProgressBarStyle);
begin
  if FStyle <> AValue then
  begin
    FStyle := AValue;
    if HandleAllocated then
      TWSProgressBarClass(WidgetSetClass).SetStyle(Self, AValue);
  end;
end;

class procedure TCustomProgressBar.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomProgressBar;
end;
 
{------------------------------------------------------------------------------
  Method: TCustomProgressBar.StepIt
  Params:  Nothing
  Returns: Nothing

  Let the progressbar proceed from actual position to actualposition + Step
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.StepIt;
begin
  inc(FPosition, FStep);
  if FPosition > FMax then FPosition := FMax;
  if FPosition < FMin then FPosition := FMin;
  if HandleAllocated then
    TWSProgressBarClass(WidgetSetClass).SetPosition(Self, FPosition);
end;


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.StepIt
  Params:  Delta : the value to add to actual position
  Returns: Nothing

  Let the progressbar proceed from actual position to actualposition + Delta

  Implementation detail:
   StepBy is realized by faking the current position to get a solution
   which is independant from the widget set in use.
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.StepBy(Delta: Integer);
begin
  FPosition := FPosition + Delta - FStep;
  StepIt;
end;

{------------------------------------------------------------------------------
  Method: TCustomProgressBar.ApplyChanges
  Params:  Nothing
  Returns: Nothing

  Apply the current parameters to the object
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.ApplyChanges;
begin
  if not (csLoading in ComponentState) then
  begin
    if FMin > Max then FMin := Max;
    if Position < Min then FPosition := Min;
    if Position > Max then FPosition := Max;
    if HandleAllocated then
      TWSProgressBarClass(WidgetSetClass).ApplyChanges(Self);
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetSmooth
  Params:  Value : Smoothing on or off
  Returns: Nothing

  Set the style of the progressbar
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetSmooth (Value : boolean);
begin
  if FSmooth <> value then
  begin
    FSmooth := value;
    ApplyChanges;
  end;
end;


{------------------------------------------------------------------------------
  Method: TCustomProgressBar.SetBarShowText
  Params:  Value : ShowText on or off
  Returns: Nothing

  Some widget sets can put a label on the progressbar which shows the
  current position

  Implementation detail:
   This functionality is not Delphi-compatible
 ------------------------------------------------------------------------------}
procedure TCustomProgressBar.SetBarShowText (Value : boolean);
begin
  if FBarShowText <> Value then
  begin
    FBarShowText := Value;
    ApplyChanges;
  end;
end;

// included by comctrls.pp
