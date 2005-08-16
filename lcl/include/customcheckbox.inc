{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TCustomCheckbox
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

  Delphi compatibility:

   - alignment property is missing
}

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.SetState
  Params:  value: new state of the object
  Returns: Nothing

  Set new state of the checkbox.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    FState := Value;
    //debugln('TCustomCheckBox.SetState ',dbgsname(Self),' ',dbgs(ord(FState)));
    ApplyChanges;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.GetState
  Params:  none
  Returns: current state of the object

  Get current state of the checkbox. To get the real state a call to the
  interface is made here.
 ------------------------------------------------------------------------------}
function TCustomCheckBox.GetState: TCheckBoxState;
begin
  fState:=RetrieveState;
  Result := fState;
end;

procedure TCustomCheckBox.DoChange(var Msg);
var
  NewState: TCheckBoxState;
begin
  //debugln('TCustomCheckBox.DoChange START ',dbgsname(Self),' ',dbgs(ord(FState)));
  NewState:=RetrieveState;
  if FState=NewState then exit;
  FState:=NewState;
  //debugln('TCustomCheckBox.DoChange CHANGED ',dbgsname(Self),' ',dbgs(ord(FState)));
  DoOnChange;
end;

function TCustomCheckBox.RetrieveState: TCheckBoxState;
begin
  Result:=FState;
  // get the actual state of the component
  // don't read from interface during loading
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then begin
    Result := TWSCustomCheckBoxClass(WidgetSetClass).RetrieveState(Self);
    //debugln('TCustomCheckBox.RetrieveState ',dbgsname(Self),' ',dbgs(ord(Result)));
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.Create
  Params:  TheOwner: the owner of the class
  Returns: Nothing

  Constructor for custom checkbox.
 ------------------------------------------------------------------------------}
constructor TCustomCheckBox.Create(TheOwner : TComponent);
begin
  Inherited Create(TheOwner);
  fCompStyle := csCheckbox;
  FState       := cbUnchecked;
  FAllowGrayed := false;
  TabStop := true;
  SetInitialBounds(0,0,90,23);
  AutoSize:=true;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.InitializeWnd
  Params: none
  Returns: Nothing

  Set all properties after visual component has been created. Will be called
  from TWinControl.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.InitializeWnd;
begin
  inherited InitializeWnd;
  ApplyChanges;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.GetChecked
  Params:  none
  Returns: current state of the object

  Get current state of the checkbox and return it as boolean.
 ------------------------------------------------------------------------------}
function TCustomCheckBox.GetChecked : Boolean;
begin
  GetChecked := (GetState = cbChecked);
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.SetChecked
  Params:  Value - new state of checkbox
  Returns: Nothing

  Set the new state of the checkbox as boolean.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.SetChecked(Value : Boolean);
var
  OldState : TCheckBoxState;
begin
  OldState := FState;
  If Value then
    FState := cbChecked
  else
    FState := cbUnChecked;
  //debugln('TCustomCheckBox.DoChange ',dbgsname(Self),' ',dbgs(ord(FState)));
  if FState <> OldState then
  begin
    if Assigned(Action)
    and (Action is TCustomAction) then
      TCustomAction(Action).Checked := FState=cbChecked;
    ApplyChanges;
    DoOnChange;
    if (not UseOnChange) and (not ClicksDisabled) then Click;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.Toggle
  Params: none
  Returns: Nothing

  Toggle the current state of the checkbox.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.Toggle;
begin
  SetChecked(not GetChecked);
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.ApplyChanges
  Params: none
  Returns: Nothing

  Sends message to update the visual apperance of the object.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.ApplyChanges;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then begin
    //debugln('TCustomCheckBox.ApplyChanges ',dbgsname(Self),' ',dbgs(ord(FState)),' ',WidgetSetClass.ClassName);
    TWSCustomCheckBoxClass(WidgetSetClass).SetState(Self, FState);
  end;
end;

procedure TCustomCheckBox.Loaded;
begin
  // Send first the FState to the interface before calling inherited,
  // otherwise the FState will be lost and the default interface State is taken.
  // TODO: MWE, remove this, it should be handled by InitializeWnd
  if HandleAllocated
  then TWSCustomCheckBoxClass(WidgetSetClass).SetState(Self, FState);
  inherited Loaded;
end;

{------------------------------------------------------------------------------
  procedure TCustomCheckBox.RealSetText(const Value: TCaption);
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.RealSetText(const Value: TCaption);
var
  ParseStr : String;
  AccelIndex : Longint;
  OldKey: word;
begin
  if Value=Text then exit;
  Inherited RealSetText(Value);
  If HandleAllocated and (not (csDesigning in ComponentState)) then begin
    ParseStr := Value;
    AccelIndex := DeleteAmpersands(ParseStr);
    If AccelIndex > -1 then begin
      OldKey := FShortCut;
      FShortCut := Char2VK(ParseStr[AccelIndex]);
      TWSCustomCheckBoxClass(WidgetSetClass).SetShortCut(Self, OldKey, FShortCut);
    end;
  end;
  InvalidatePreferredSize;
  AdjustSize;
end;

function TCustomCheckBox.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    if Focused then Toggle;
    Result := true;
  end else
    Result := inherited;
end;

// included by stdctrls.pp

{
  $Log$
  Revision 1.34  2005/04/27 12:37:28  micha
  implement/fix button/label shortcut accelchar handling

  Revision 1.33  2005/04/06 10:43:40  micha
  customcheckbox: do not unnecessarily ask state twice
  first let widget process BM_SETCHECK, so we do not see itemindex=-1 in between

  Revision 1.32  2005/02/21 20:15:28  mattias
  fixed componentpalette adding via double click

  Revision 1.31  2005/01/24 12:23:11  mattias
  fixed TColorButton.Paint

  Revision 1.30  2005/01/13 23:37:33  marc
  * Fixed and clearified loaded

  Revision 1.29  2005/01/11 13:25:23  vincents
  TCustomCheckBox.AllowGray defaults to false.

  Revision 1.28  2005/01/01 19:36:40  mattias
  fixed loading TRadioButton.Checked

  Revision 1.27  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.26  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.25  2004/09/18 17:07:57  micha
  convert LM_GETVALUE message to interface method

  Revision 1.24  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.23  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.22  2004/07/10 18:17:30  mattias
  added Delphi ToDo support, Application.WndProc, small bugfixes  from Colin

  Revision 1.21  2004/05/30 14:02:30  mattias
  implemented OnChange for TRadioButton, TCheckBox, TToggleBox and some more docking stuff

  Revision 1.20  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.19  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.18  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.16  2004/02/05 09:45:33  mattias
  implemented Actions for TSpeedButton, TMenuItem, TCheckBox

  Revision 1.15  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.14  2003/09/16 11:35:14  mattias
  started TDBCheckBox

  Revision 1.13  2003/03/25 16:56:57  mattias
  implemented TButtonControl.UseOnChange

  Revision 1.12  2003/03/25 16:29:53  mattias
  fixed sending TButtonControl.OnClick on every change

  Revision 1.11  2003/03/17 20:50:30  mattias
  fixed TRadioGroup.ItemIndex=-1

  Revision 1.10  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.9  2002/10/09 10:22:54  lazarus
  MG: fixed client origin coordinates

  Revision 1.8  2002/10/03 00:08:50  lazarus
  AJ: TCustomLabel Autosize, TCustomCheckbox '&' shortcuts started

  Revision 1.7  2002/09/10 07:33:37  lazarus
  MG: fixed TCheckBox.DoAutoSize on loading

  Revision 1.6  2002/08/27 14:33:37  lazarus
  MG: fixed designer component deletion

  Revision 1.5  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.4  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.3  2002/04/02 16:44:41  lazarus
  MG: bugfixes, finddeclaration cache dependencies

  Revision 1.2  2002/02/08 16:45:09  lazarus
  MG: added codetools options

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.2  2000/06/22 20:57:07  lazarus
  *** empty log message ***

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.6  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.5  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.4  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.3  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

}
// included by stdctrls.pp

