{%MainUnit ../buttons.pp}
{******************************************************************************
                                   TCustomButton
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Color:=clBtnFace;
  ParentColor:=false;
  TabStop := true;
  // set default alignment
  Align := alNone;
  BorderSpacing.InnerBorder:=4;
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
    TWSButtonClass(WidgetSetClass).ActiveDefaultButtonChanged(Self);
end;

procedure TCustomButton.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited ControlKeyDown(Key, Shift);
end;

procedure TCustomButton.ControlKeyUp(var Key: Word; Shift: TShiftState
  );
begin
  inherited ControlKeyUp(Key, Shift);
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
  if FActive or FDefault then
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

function TCustomButton.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    Click;
    Result := true;
  end else
    Result := inherited;
end;

procedure TCustomButton.ActiveDefaultControlChanged(NewControl: TControl);
var
  lPrevActive: boolean;
  lForm: TCustomForm;
begin
  lPrevActive := FActive;
  if NewControl = Self then
  begin
    FActive := true;
    lForm := GetParentForm(Self);
    if lForm <> nil then
      lForm.ActiveDefaultControl := Self;
  end else 
  if NewControl <> nil then
  begin
    FActive := false;
  end else begin
    FActive := FDefault;
  end;
  if lPrevActive <> FActive then
    DoSendBtnDefault;
end;

procedure TCustomButton.CMUIActivate(var Message: TLMessage);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then exit;

  ActiveDefaultControlChanged(lForm.ActiveControl);
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

procedure TCustomButton.WMKillFocus(var Message: TLMKillFocus);
var
  lForm: TCustomForm;
begin
  inherited;

  if FActive then
  begin
    FActive := FDefault;
    if not FActive then
    begin
      lForm := GetParentForm(Self);
      if (lForm <> nil) and (lForm.ActiveDefaultControl = Self) then
        lForm.ActiveDefaultControl := nil;
      DoSendBtnDefault;
    end;
  end;
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
  AdjustSize;
end;

function TCustomButton.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  // no childs
  Result:=false;
end;

function TCustomButton.IsBorderSpacingInnerBorderStored: Boolean;
begin
  Result:=BorderSpacing.InnerBorder<>2;
end;


{ TButton }

procedure TButton.Click;
begin
  inherited Click;
end;

