{%MainUnit ../actnlist.pas}

{
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

{ TCustomActionList }

constructor TCustomActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActions := TList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FState := asNormal;
end;

destructor TCustomActionList.Destroy;
begin
  FImageChangeLink.Free;
  while FActions.Count > 0 do TContainedAction(FActions.Last).Free;
  FActions.Free;
  inherited Destroy;
end;

procedure TCustomActionList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Action: TAction;
begin
  for I := 0 to FActions.Count - 1 do
  begin
    Action := TAction(FActions.List^[I]);
    if Action.Owner = Root then Proc(Action);
  end;
end;

procedure TCustomActionList.SetChildOrder(Component: TComponent; Order: Integer);
begin
  if FActions.IndexOf(Component) >= 0 then
    (Component as TContainedAction).Index := Order;
end;

function TCustomActionList.GetAction(Index: Integer): TContainedAction;
begin
  Result := TContainedAction(FActions[Index]);
end;

function TCustomActionList.GetActionCount: Integer;
begin
  Result := FActions.Count;
end;

procedure TCustomActionList.SetAction(Index: Integer; Value: TContainedAction);
begin
  TContainedAction(FActions[Index]).Assign(Value);
end;

procedure TCustomActionList.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
end;

procedure TCustomActionList.ImageListChange(Sender: TObject);
begin
  if Sender = Images then Change;
end;

procedure TCustomActionList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil
    else if (AComponent is TContainedAction) then
      RemoveAction(TContainedAction(AComponent));
end;

procedure TCustomActionList.AddAction(Action: TContainedAction);
begin
  FActions.Add(Action);
  Action.FActionList := Self;
  Action.FreeNotification(Self);
end;

procedure TCustomActionList.RemoveAction(Action: TContainedAction);
begin
  if FActions.Remove(Action) >= 0 then
    Action.FActionList := nil;
end;

procedure TCustomActionList.Change;
var
  I: Integer;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  for I := 0 to FActions.Count - 1 do
    TContainedAction(FActions.List^[I]).Change;
  OwnerFormDesignerModified(Self);
end;

function TCustomActionList.IsShortCut(var Message: TLMKey): Boolean;
var
  I: Integer;
  ShortCut: TShortCut;
  ShiftState: TShiftState;
  Action: TCustomAction;
begin
  ShiftState := MsgKeyDataToShiftState(Message.KeyData);
  ShortCut := KeyToShortCut(Message.CharCode, ShiftState);
  if ShortCut <> scNone then
    for I := 0 to FActions.Count - 1 do
    begin
      Action := TCustomAction(FActions.Items[I]);
      if (TObject(Action) is TCustomAction) then
        if (Action.ShortCut = ShortCut) or (Assigned(Action.FSecondaryShortCuts) and
           (Action.SecondaryShortCuts.IndexOfShortCut(ShortCut) <> -1)) then
        begin
          Result := Action.HandleShortCut;
          Exit;
        end;
    end;
  Result := False;
end;

function TCustomActionList.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnExecute) then FOnExecute(Action, Result);
end;

function TCustomActionList.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := False;
  if Assigned(FOnUpdate) then FOnUpdate(Action, Result);
end;

function TCustomActionList.IndexOfName(const ActionName: string): integer;
begin
  Result:=FActions.Count-1;
  while (Result>=0)
  and (AnsiCompareText(TAction(FActions[Result]).Name,ActionName)<>0) do
    dec(Result);
end;

function TCustomActionList.ActionByName(const ActionName: string
  ): TContainedAction;
var
  i: Integer;
begin
  i:=IndexOfName(ActionName);
  if i>=0 then
    Result:=Actions[i]
  else
    Result:=nil;
end;

procedure TCustomActionList.SetState(const Value: TActionListState);
var
  I: Integer;
  Action: TCustomAction;
begin
  if FState <> Value then
  begin
    FState := Value;
    if State = asSuspended then exit;
    for I := 0 to FActions.Count - 1 do
    begin
      Action := TAction(FActions.List^[I]);
      case Value of
        asNormal:
          begin
            if Action is TCustomAction then
              if State = asSuspendedEnabled then
                 with Action as TCustomAction do
                  Enabled := SavedEnabledState;
            Action.Update;
          end;
        asSuspendedEnabled:
          if Action is TCustomAction then
            if Value = asSuspendedEnabled then
              with Action as TCustomAction do
              begin
                SavedEnabledState := Enabled;
                Enabled := True;
              end;
      end;
    end;
  end;
end;

// included by actnlist.pas
