{%MainUnit ../comctrls.pp}
{******************************************************************************
                                  TProgressBar
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
  Method: TProgressBar.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the progressbar.
 ------------------------------------------------------------------------------}
constructor TProgressBar.Create (AOwner : TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csProgressBar;
  FPosition  := 0;
  FStep      := 10;
  FMin	      := 0;
  FMax	      := 100;
  FSmooth    := false;
  FOrientation      := pbHorizontal;
  FBarShowText      := false;
  FBarTextFormat    := '%v from [%l-%u] (=%p%%)';
  SetInitialBounds(0,0,100,20);
end;   

{------------------------------------------------------------------------------
  Method: TProgressBar.InitializeWnd
  Params: none
  Returns: Nothing

  Set all properties after visual component has been created. Will be called
  from TWinControl.
 ------------------------------------------------------------------------------}
procedure TProgressBar.InitializeWnd;
begin
  inherited InitializeWnd;
  ApplyChanges;
end;

procedure TProgressBar.Loaded;
begin
  inherited Loaded;
  ApplyChanges;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.GetMin
  Params:  Nothing
  Returns: actual minimum value of the progressbar

  Retrieve the actual minimum value of the progressbar.
 ------------------------------------------------------------------------------}
function TProgressBar.GetMin: Integer;
begin
  Result := FMin;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.GetMax
  Params:  Nothing
  Returns: actual maximum value of the progressbar

  Retrieve the actual maximum value of the progressbar.
 ------------------------------------------------------------------------------}
function TProgressBar.GetMax: Integer;
begin
  Result := FMax;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.GetPosition
  Params:  Nothing
  Returns: actual position of the progressbar

  Retrieve the position of the progressbar.
 ------------------------------------------------------------------------------}
function TProgressBar.GetPosition: Integer;
begin
  Result := FPosition;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.SetParams
  Params:  Min & Max for the progressbar
  Returns: Nothing

  Set new minimum and maximum values for the progressbar.
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetParams(AMin, AMax: Integer);
begin
  SetMax(AMax);
  SetMin(AMin);
end;


{------------------------------------------------------------------------------
  Method: TProgressBar.SetMin
  Params:  Minimum value for the progressbar
  Returns: Nothing

  Set new minimum value for the progressbar.
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetMin(Value: Integer);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    ApplyChanges;
  end;
end;


{------------------------------------------------------------------------------
  Method: TProgressBar.SetMax
  Params:  Maximum value for the progressbar
  Returns: Nothing

  Set new maximum value for the progressbar.
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetMax(Value: Integer);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.SetPosition
  Params:  New acutal position for the progressbar
  Returns: Nothing

  Set new new acutal position for the progressbar
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetPosition(Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    ApplyChanges;
  end;
end;


{------------------------------------------------------------------------------
  Method: TProgressBar.SetPosition
  Params:  New stepping value for the progressbar
  Returns: Nothing

  Set new stepping value for the progressbar
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetStep(Value: Integer);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.SetPosition
  Params:  New orientation of the progressbar
  Returns: Nothing

  Set new orientation.
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetOrientation (Value : TProgressBarOrientation);
begin
  if FOrientation <> Value then
  begin
    Forientation := Value;
    ApplyChanges;
  end;
end;
 
{------------------------------------------------------------------------------
  Method: TProgressBar.StepIt
  Params:  Nothing
  Returns: Nothing

  Let the progressbar proceed from actual position to actualposition + Step
 ------------------------------------------------------------------------------}
procedure TProgressBar.StepIt;
begin
  inc (FPosition, FStep);
  if FPosition > FMax then FPosition := FMax;
  if FPosition < FMin then FPosition := FMin;
  TWSProgressBarClass(WidgetSetClass).SetPosition(Self, FPosition);
end;


{------------------------------------------------------------------------------
  Method: TProgressBar.StepIt
  Params:  Delta : the value to add to actual position
  Returns: Nothing

  Let the progressbar proceed from actual position to actualposition + Delta

  Implementation detail:
   StepBy is realized by faking the current position to get a solution
   which is independant from the widget set in use.
 ------------------------------------------------------------------------------}
procedure TProgressBar.StepBy(Delta: Integer);
begin
  if FStep < Delta then FPosition := FPosition + (Delta - FStep);
  if FStep > Delta then FPosition := FPosition - (Delta);
  StepIt;
end;

{------------------------------------------------------------------------------
  Method: TProgressBar.ApplyChanges
  Params:  Nothing
  Returns: Nothing

  Apply the current parameters to the object
 ------------------------------------------------------------------------------}
procedure TProgressBar.ApplyChanges;
begin
  if not (csLoading in ComponentState) then begin
    if FMin>Max then FMin:=Max;
    if Position<Min then FPosition:=Min;
    if Position>Max then FPosition:=Max;
    if HandleAllocated then begin
      TWSProgressBarClass(WidgetSetClass).ApplyChanges(Self);
    end;
  end;
end;


{------------------------------------------------------------------------------
  Method: TProgressBar.SetSmooth
  Params:  Value : Smoothing on or off
  Returns: Nothing

  Set the style of the progressbar
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetSmooth (Value : boolean);
begin
  if FSmooth <> value then
  begin
    FSmooth := value;
    ApplyChanges;
  end;
end;


{------------------------------------------------------------------------------
  Method: TProgressBar.SetBarShowText
  Params:  Value : ShowText on or off
  Returns: Nothing

  Some widget sets can put a label on the progressbar which shows the
  current position

  Implementation detail:
   This functionality is not Delphi-compatible
 ------------------------------------------------------------------------------}
procedure TProgressBar.SetBarShowText (Value : boolean);
begin
  if FBarShowText <> Value then
  begin
    FBarShowText      := Value;
    ApplyChanges;
  end;
end;

// included by comctrls.pp

{
  $Log$
  Revision 1.11  2004/12/12 03:54:09  mattias
  implemented open project after open standard windows

  Revision 1.10  2004/09/23 14:50:47  micha
  convert LM_SETPROPERTIES message to interface method for TProgressBar

  Revision 1.9  2004/09/21 10:05:26  mattias
  fixed disable at designtime and bounding TProgressBar position

  Revision 1.8  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.7  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.6  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.4  2003/06/25 21:02:20  mattias
  reduced TProgressBar setproperties calls

  Revision 1.3  2002/05/10 06:05:55  lazarus
  MG: changed license to LGPL

  Revision 1.2  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.1  2000/07/13 10:28:27  michael
  + Initial import

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.4  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.3  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

}
