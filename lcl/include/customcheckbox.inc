{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               TCustomCheckbox
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
    ApplyChanges;
  end;
end;

function TCustomCheckBox.GetAlignment: TLeftRight;
begin
  Result := FAlignment;
end;

procedure TCustomCheckBox.SetAlignment(AValue: TLeftRight);
begin
  if (GetAlignment = AValue) then Exit;
  FAlignment := AValue;
  if HandleAllocated and (not (csLoading in ComponentState)) then
  begin
    //debugln('TCustomCheckBox.SetAlignment ',dbgsname(Self),' ',dbgs((FAlignment)),' ',WidgetSetClass.ClassName);
    TWSCustomCheckBoxClass(WidgetSetClass).SetAlignment(Self, FAlignment);
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
  FState := RetrieveState;
  Result := FState;
end;

procedure TCustomCheckBox.DoChange(var Msg);
begin
  FState := RetrieveState;
  DoClickOnChange;
end;

class procedure TCustomCheckBox.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomCheckBox;
end;

procedure TCustomCheckBox.Click;
begin
  // skip clicks by WM_MOUSEUP
end;

function TCustomCheckBox.RetrieveState: TCheckBoxState;
begin
  Result := FState;
  // get the actual state of the component
  // don't read from interface during loading
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
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
constructor TCustomCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FAlignment := taRightJustify;
  //todo: move to TButtonControl
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
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
  Result := (GetState = cbChecked);
end;

{------------------------------------------------------------------------------
  Method: TCustomCheckBox.SetChecked
  Params:  Value - new state of checkbox
  Returns: Nothing

  Set the new state of the checkbox as boolean.
 ------------------------------------------------------------------------------}
procedure TCustomCheckBox.SetChecked(Value : Boolean);
var
  OldState: TCheckBoxState;
begin
  OldState := FState;
  if Value then
    FState := cbChecked
  else
    FState := cbUnChecked;
  //debugln('TCustomCheckBox.SetChecked ',dbgsname(Self),' ',dbgs(ord(FState)));
  if FState <> OldState then
  begin
    if Action is TCustomAction then
      TCustomAction(Action).Checked := FState = cbChecked;
    ApplyChanges;
    //some widgetsets (gtk*) does not allow to uncheck a radio button
    //only call OnChange if effectivelly changed
    FState := RetrieveState;
    if FState <> OldState then
      DoClickOnChange;
  end;
end;

procedure TCustomCheckBox.RealSetText(const Value: TCaption);
begin
  if Value = Text then
    Exit;
  inherited RealSetText(Value);
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
  if HandleAllocated and (not (csLoading in ComponentState)) then
  begin
    //debugln('TCustomCheckBox.ApplyChanges ',dbgsname(Self),' ',dbgs(ord(FState)),' ',WidgetSetClass.ClassName);
    TWSCustomCheckBoxClass(WidgetSetClass).SetState(Self, FState);
  end;
end;

class function TCustomCheckBox.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 23;
end;

procedure TCustomCheckBox.Loaded;
begin
  // Send first the FState to the interface before calling inherited,
  // otherwise the FState will be lost and the default interface State is taken.
  if HandleAllocated then
    TWSCustomCheckBoxClass(WidgetSetClass).SetState(Self, FState);
  inherited Loaded;
end;

procedure TCustomCheckBox.WSSetText(const AText: String);
var
  ParseStr : String;
  AccelIndex : Longint;
begin
  if (not HandleAllocated) then
    exit;
  if not (csDesigning in ComponentState) then
  begin
    ParseStr := AText;
    AccelIndex := DeleteAmpersands(ParseStr);
    if AccelIndex > -1 then
    begin
      FShortCut := Menus.ShortCut(Char2VK(ParseStr[AccelIndex]), [ssCtrl]);
      TWSCustomCheckBoxClass(WidgetSetClass).SetShortCut(Self, FShortCut, FShortCutKey2);
    end;
  end;
  inherited WSSetText(AText);
end;

procedure TCustomCheckBox.TextChanged;
begin
  InvalidatePreferredSize;
  if (Parent<>nil) and Parent.AutoSize then
    Parent.AdjustSize;
  AdjustSize;
  inherited TextChanged;
end;

procedure TCustomCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_3STATE;
  if (FAlignment = taLeftJustify) then Params.Style := Params.Style or BS_RIGHTBUTTON;
end;

procedure TCustomCheckBox.DoClickOnChange;
begin
  Changed;
  // emulate delphi OnClick behaviour (click will call OnChange)
  if not ClicksDisabled then
    inherited Click
  else
    DoOnChange;
end;

function TCustomCheckBox.DialogChar(var Message: TLMKey): boolean;
begin
  if IsAccel(Message.CharCode, Caption) and CanFocus then
  begin
    SetFocus;
    if Focused then
      Toggle;
    Result := true;
  end else
    Result := inherited;
end;

// included by stdctrls.pp
