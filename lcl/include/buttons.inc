{%MainUnit ../buttons.pp}
{******************************************************************************
                                   TCustomButton
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

{------------------------------------------------------------------------------}
{       TCustomButton Constructor                                                    }
{------------------------------------------------------------------------------}

constructor TCustomButton.Create(TheOwner: TComponent);
begin
  Inherited Create(TheOwner);
  // set the component style to csButton
  fCompStyle := csButton;
  ControlStyle:=ControlStyle-[csClickEvents]+[csHasDefaultAction,csHasCancelAction];
  TabStop := true;
  // set default alignment
  Align := alNone;
  // setup default sizes
  SetInitialBounds(0,0,75,25);
end;

{------------------------------------------------------------------------------
  Method:  TCustomButton.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TCustomButton.CreateWnd;
begin
  inherited CreateWnd;
  //this is done in TWinControl
  //SetText(Caption);//To ensure shortcut is set
  DoSendBtnDefault;
end;

{------------------------------------------------------------------------------
  procedure TCustomButton.DoSendBtnDefault;
 ------------------------------------------------------------------------------}
procedure TCustomButton.DoSendBtnDefault;
begin
  if HandleAllocated then
    TWSButtonClass(WidgetSetClass).DefaultButtonChanged(Self);
end;

procedure TCustomButton.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift=[]) then begin
    Key:=VK_UNKNOWN;
    Click;
  end else
    inherited ControlKeyDown(Key, Shift);
end;

{------------------------------------------------------------------------------
  procedure TCustomButton.SetParent(AParent: TWinControl);
 ------------------------------------------------------------------------------}
procedure TCustomButton.SetParent(AParent: TWinControl);
var
  Form: TCustomForm;
begin
  if Parent=AParent then exit;
  inherited SetParent(AParent);
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    if FDefault then
      Form.DefaultControl := Self;
    if FCancel then
      Form.CancelControl := Self;
  end;
  DoSendBtnDefault;
end;

procedure TCustomButton.UpdateRolesForForm;
var
  AForm: TCustomForm;
  NewRoles: TControlRolesForForm;
begin
  AForm:=GetParentForm(Self);
  if AForm=nil then begin
    // not on a form => keep settings
    exit;
  end else begin
    // on a form => use settings of parent form
    NewRoles:=AForm.GetRolesForControl(Self);
    Default := crffDefault in NewRoles;
    Cancel := crffCancel in NewRoles;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomButton.SetCancel
  Params:  NewCancel - new cancel value
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomButton.SetCancel(NewCancel: boolean);
var
  Form: TCustomForm;
begin
  if FCancel = NewCancel then Exit;
  FCancel := NewCancel;
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    if NewCancel then
      Form.CancelControl := Self
    else
      Form.CancelControl := nil;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCustomButton.SetDefault
  Params:  Value
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TCustomButton.SetDefault(Value : Boolean);
var
  Form: TCustomForm;
begin
  if FDefault = Value then Exit;
  FDefault := Value;
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    if Value then
      Form.DefaultControl := Self
    else
      Form.DefaultControl := nil;
  end;
  DoSendBtnDefault;
end;

procedure TCustomButton.SetModalResult(const AValue: TModalResult);
begin
  if AValue=FModalResult then exit;
  FModalResult:=AValue;
end;

procedure TCustomButton.ExecuteDefaultAction;
begin
  if FDefault then
    Click;
end;

procedure TCustomButton.ExecuteCancelAction;
begin
  if FCancel then
    Click;
end;

{------------------------------------------------------------------------------
  Method:  TCustomButton.Click
  Params:  None
  Returns: Nothing

  Handles the event that the button is clicked
 ------------------------------------------------------------------------------}
procedure TCustomButton.Click;
var
  Form : TCustomForm;
Begin
  if ModalResult <> mrNone
  then begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.ModalResult := ModalResult;
  end;
  inherited Click;
end;

{------------------------------------------------------------------------------
  Method:  TCustomButton.CMDefaultClicked
  Params:  None
  Returns: Nothing

  Handles the event when the button Leaves
 ------------------------------------------------------------------------------}
procedure TCustomButton.WMDefaultClicked(var Message: TLMessage);
begin
  Click;
end;

procedure TCustomButton.RealSetText(const Value: TCaption);
var
  ParseStr : String;
  AccelIndex : Longint;
  OldShortCut: TShortCut;
begin
  Inherited RealSetText(Value);
  If (not HandleAllocated) or (csDesigning in ComponentState) then exit;
  ParseStr := Value;
  AccelIndex := DeleteAmpersands(ParseStr);
  If AccelIndex > -1 then begin
    OldShortCut := FShortCut;
    FShortCut := ShortCut(Char2VK(ParseStr[AccelIndex]), [ssCtrl]);
    TWSButtonClass(WidgetSetClass).SetShortCut(Self, OldShortCut, FShortCut);
  end;
  DoAutoSize;
end;

function TCustomButton.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  // no childs
  Result:=false;
end;

{ =============================================================================

  $Log$
  Revision 1.38  2005/01/14 00:27:05  mattias
  fixed several dialogs to react on esacpe

  Revision 1.37  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.36  2004/09/17 10:56:25  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.35  2004/09/11 14:54:01  micha
  convert LM_BTNDEFAULT_CHANGED message to interface method

  Revision 1.34  2004/08/26 19:09:34  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.33  2004/07/15 10:43:38  mattias
  added TCustomButton, TCustomBitBtn, TCustomSpeedButton

  Revision 1.32  2004/07/13 10:34:15  mattias
  fixed lcl package unit file name checklist.pas

  Revision 1.31  2004/07/11 13:03:54  mattias
  extended RolesForForm to manage multiple roles for on control

  Revision 1.30  2004/07/04 20:07:08  micha
  form notifies control of new role

  Revision 1.29  2004/07/01 20:42:11  micha
  implement better ExecuteXXAction design; break dependency on TCustomButton class in TCustomForm

  Revision 1.28  2004/06/30 11:07:20  micha
  implement return key clicks default button; escape key clicks cancel button

  Revision 1.27  2004/06/29 14:38:28  micha
  fix default button notification win32 intf

  Revision 1.26  2004/05/22 14:35:32  mattias
  fixed button return key

  Revision 1.25  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.24  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.23  2004/03/19 00:03:15  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.22  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.21  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.20  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.19  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.17  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.16  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.15  2003/11/15 13:07:09  mattias
  added ambigious unit check for IDE

  Revision 1.14  2003/10/22 18:43:23  mattias
  prepared image sharing

  Revision 1.13  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.12  2002/12/25 14:21:28  mattias
  fixed setting activecontrol to nil when removing component

  Revision 1.11  2002/12/25 11:53:47  mattias
  Button.Default now sets focus

  Revision 1.10  2002/09/06 15:57:34  lazarus
  MG: fixed notebook client area, send messages and minor bugs

  Revision 1.9  2002/08/27 06:40:50  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.8  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.7  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

  Revision 1.6  2001/11/21 19:32:32  lazarus
  TComboBox can now be moved in FormEditor
  Shane

  Revision 1.5  2001/10/16 14:19:13  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.3  2001/06/06 12:30:41  lazarus
  MG: bugfixes

  Revision 1.2  2000/07/16 12:37:52  lazarus
   Added OnMouseEnter, OnMouseLeave property
   (code from christer, added by stoppok)

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.2  2000/06/13 20:50:42  lazarus
  MWE:
    - Started to remove obsolete/dead code/messages

  HJO:
    * Fixed messages in showmodal of 2nd form
    * Fixed modal result for button


}
