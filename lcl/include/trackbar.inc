{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TCustomTrackBar
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

           FScalePosition, FScaleDigits

  Delphi compatibility:

   - the interface is almost like in delphi 4
   - some Delphi properties are not supported by GTK and are currently not
     implemented here, These are:
       frequency, tickstyle and tickmark
   - what about these private procs
      procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
      procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
   - there are 2 new properties which I've implemented because they're
     nice addons for the GTK interface
      * ShowScale
      * ScalePos (left, top, right, bottom)

  TODO:

    - implement some more Delphi stuff
    - range checking for min/max could raise an exception
    - use RecreateWnd when the orientation changes!

  Bugs:

    - When changing orientation after the Trackbar has been constructed
      the GTK version will CRASH
}
{ASSERTIONS ON}
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
  Caption    := 'TrackBar';
  FLineSize  := 1;
  FMax       := 10;
  FMin       := 0;
  FPosition  := 0;
  FPageSize  := 2;
  FOrientation := trHorizontal;
  FShowScale := false;
  FScalePos  := trTop;
  FScaleDigits := 0;
  FTickMarks:=tmBottomRight;
  FTickStyle:=tsAuto;
  TabStop := true;
  SetInitialBounds(0,0,100,20);
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

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetTick
  Params: Value : new tick
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetTick(Value: Integer);
begin
{   := Value; }
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetOrientation
  Params: Value : new orientation
  Returns: Nothing

  Change the orientation of the trackbar.
------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetOrientation(Value: TTrackBarOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if HandleAllocated then
    begin
      // TODO: Remove RecreateWnd
      RecreateWnd(Self);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetParams
  Params:  APosition : new position
           AMin      : new minimum
	   AMax	     : new maximum
  Returns: Nothing

  Set new parameters for the trackbar.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetParams(APosition, AMin, AMax: Integer);
begin
  if (not (csLoading in ComponentState)) then begin
    if AMin>AMax then AMin:=AMax;
    if APosition<AMin then APosition:=AMin;
    if APosition>AMax then APosition:=AMax;
  end;
  if (FPosition=APosition) and (FMin=AMin) and (AMax=AMin) then exit;
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
  if FPosition <> Value then
  begin
    FPosition := Value;
    if HandleAllocated then 
      TWSTrackBarClass(WidgetSetClass).SetPosition(Self, FPosition);
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
  if FMin <> Value then SetParams(FPosition,Value,FMax);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetMax
  Params: Value : new maximum
  Returns: Nothing

  Set maximum value of the trackbar.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetMax(Value: Integer);
begin
  if FMax<>Value then SetParams(FPosition,FMin,Value);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetFrequency
  Params: Value : new frequency
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetFrequency(Value: Integer);
begin
  FFrequency := Value;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetTickStyle
  Params: Value : new tickstyle
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetTickStyle(Value: TTickStyle);
begin
  FTickStyle := Value;
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetTickMarks
  Params: Value : new tickmarks
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetTickMarks(Value: TTickMark);
begin
  FTickMarks := Value;
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

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.ApplyChanges
  Params: none
  Returns: Nothing

  Sends message to update the visual apperance of the object.
 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.ApplyChanges;
begin
  if HandleAllocated and (not (csLoading in ComponentState))
  then TWSTrackBarClass(WidgetSetClass).ApplyChanges(Self);
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
  if [csLoading,csDestroying,csDesigning]*ComponentState<>[] then exit;
  EditingDone;
  if Assigned (FOnChange) then FOnChange(Self);
end;

{------------------------------------------------------------------------------
  Method: TCustomTrackBar.SetShowScale
  Params:  value : true = show scaling text
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TCustomTrackBar.SetShowScale(Value: boolean);
begin
  if FShowScale <> Value then
  begin
    FShowScale := Value;
    ApplyChanges;
  end;
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

{ -------------------- unimplemented stuff below ------------------------------}
{
  $Log$
  Revision 1.17  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.16  2004/09/24 07:52:35  micha
  convert LM_SETPROPERTIES message to interface method for TCustomTrackBar
  remove message LM_SETPROPERTIES, conversion done

  Revision 1.15  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.14  2004/09/18 17:07:57  micha
  convert LM_GETVALUE message to interface method

  Revision 1.13  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.12  2004/07/11 13:03:54  mattias
  extended RolesForForm to manage multiple roles for on control

  Revision 1.11  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.10  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.9  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.7  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.6  2003/06/13 21:13:20  mattias
  fixed TCustomTrackBar initial size

  Revision 1.5  2002/05/10 06:05:56  lazarus
  MG: changed license to LGPL

  Revision 1.4  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.3  2002/01/04 21:07:49  lazarus
  MG: added TTreeView

  Revision 1.2  2000/07/23 19:03:10  lazarus
  changed some comments, stoppok

  Revision 1.1  2000/07/13 10:28:28  michael
  + Initial import

  Revision 1.3  2000/07/09 20:43:29  lazarus
  Fixed crash when changing orientation, stoppok

  Revision 1.2  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.1  2000/04/02 20:49:57  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.8  2000/03/30 18:07:55  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.7  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.6  1999/12/30 10:38:59  lazarus

    Some changes to Checkbox code.
      stoppok

  Revision 1.5  1999/12/29 00:39:35  lazarus

    Changes to make trackbar/progressbar working again.
      stopppok

  Revision 1.4  1999/09/26 13:30:15  lazarus

     Implemented OnEnter & OnExit events for TCustomTrackBar. These properties
     and handler functions have been added to TWincontrol, two new
     callbacks have been added to gtkcallback.
      stoppok

  Revision 1.3  1999/09/23 20:33:32  lazarus
    reintroduced changes to TCustomTrackBar from v1.46 which where lost in 1.48.
    Some addtional changes to TCustomTrackBar also applied.
      stoppok

  Revision 1.2  1999/09/17 20:49:03  lazarus

    Some changes to trackbar component (added lineSize/PageSize properties,
    removed scaledigits property)
      stoppok

  Revision 1.1  1999/09/03 22:01:01  lazarus
  Added TCustomTrackBar
    stoppok

}
