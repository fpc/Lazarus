{******************************************************************************
                                   TButton
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
{       TButton Constructor                                                    }
{------------------------------------------------------------------------------}

constructor TButton.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  With FShortcut do begin
    OldKey := 0;
    OldModifier := [ssCtrl];
    NewModifier := [ssCtrl];
    NewKey := 0;
  end;
  {set the component style to csButton}
  fCompStyle := csButton;
  ControlStyle:=ControlStyle-[csClickEvents];
  TabStop := true;
  {set default alignment}
  Align := alNone;
  {setup default sizes}
  SetInitialBounds(1, 1, 75, 25);
end;

function TButton.CreateWindowHandle(const AParams: TCreateParams): THandle;
begin
  if fCompStyle=csButton then
    Result := ButtonCreateHandle(Self, APArams)
  else
    Result := inherited CreateWindowHandle(AParams);
end;

{------------------------------------------------------------------------------
  Method:  TButton.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object.
 ------------------------------------------------------------------------------}
procedure TButton.CreateWnd;
begin
  inherited CreateWnd;
  SetText(Caption);//To ensure shortcut is set
  DoSendBtnDefault;
end;

{------------------------------------------------------------------------------
  procedure TButton.DoSendBtnDefault;
 ------------------------------------------------------------------------------}
procedure TButton.DoSendBtnDefault;
begin
  if HandleAllocated and (not (csDesigning in ComponentState)) then
    CNSendMessage(LM_BTNDEFAULT_CHANGED,Self,nil);
end;

{------------------------------------------------------------------------------
  procedure TButton.SetParent(AParent: TWinControl);
 ------------------------------------------------------------------------------}
procedure TButton.SetParent(AParent: TWinControl);
begin
  if Parent=AParent then exit;
  inherited SetParent(AParent);
  DoSendBtnDefault;
end;

{------------------------------------------------------------------------------
  Method:  TButton.SetDefault
  Params:  Value
  Returns: Nothing
 ------------------------------------------------------------------------------}
procedure TButton.SetDefault(Value : Boolean);
begin
  if FDefault = Value then Exit;
  FDefault := Value;
  DoSendBtnDefault;
End;

{------------------------------------------------------------------------------
  Method:  TButton.Click
  Params:  None
  Returns: Nothing

  Handles the event that the button is clicked
 ------------------------------------------------------------------------------}
procedure TButton.Click;
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
  Method:  TButton.CMDefaultClicked
  Params:  None
  Returns: Nothing

  Handles the event when the button Leaves
 ------------------------------------------------------------------------------}
procedure TButton.WMDefaultClicked(var Message: TLMessage);
begin
  Click;
end;

procedure TButton.SetText(const Value: TCaption);
var
  ParseStr : String;
  AccelIndex : Longint;
begin
  Inherited SetText(Value);
  If (not HandleAllocated) or (csDesigning in ComponentState) then exit;
  ParseStr := Value;
  AccelIndex := DeleteAmpersands(ParseStr);
  If AccelIndex > -1 then begin
    With FShortcut do begin
      Handle := Self.Handle;
      OldKey := NewKey;
      NewKey := Char2VK(ParseStr[AccelIndex]);
    end;
    CNSendMessage(LM_SETSHORTCUT, Self, @FShortcut);
  end;
end;

function TButton.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  // no childs
  Result:=false;
end;

{ =============================================================================

  $Log$
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
