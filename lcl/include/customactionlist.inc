{%MainUnit ../actnlist.pas}

{
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
}

{ TCustomActionList }

constructor TCustomActionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActions := TFPList.Create;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  FState := asNormal;
end;

destructor TCustomActionList.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  while FActions.Count > 0 do TContainedAction(FActions.Last).Free;
  FreeAndNil(FActions);
  inherited Destroy;
end;

procedure TCustomActionList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Action: TContainedAction;
begin
  for I := 0 to FActions.Count - 1 do
  begin
    Action := TContainedAction(FActions[I]);
    if Action.Owner = Root then Proc(Action);
  end;
end;

procedure TCustomActionList.SetChildOrder(Component: TComponent; Order: Integer);
var
  i: LongInt;
begin
  i:=FActions.IndexOf(Component);
  if (i>=0) and (i<>Order) then
    TContainedAction(Component).Index:=Order;
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
  if TObject(FActions[Index])=Value then exit;
  TContainedAction(FActions[Index]).Assign(Value);
end;

procedure TCustomActionList.SetImages(Value: TCustomImageList);
begin
  if FImages=Value then exit;
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FImageChangeLink);
    FImages.RemoveFreeNotification(Self);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
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
    if AComponent = Images then begin
      Images := nil; // call SetImages, because it can be overridden
    end else if (AComponent is TContainedAction) then
      RemoveAction(TContainedAction(AComponent));
end;

procedure TCustomActionList.AddAction(Action: TContainedAction);
begin
  if Action.FActionList = Self then
    raise Exception.Create('TCustomActionList.AddAction already added');
  FActions.Add(Action);
  Action.FActionList := Self;
  Action.FreeNotification(Self);
end;

procedure TCustomActionList.RemoveAction(Action: TContainedAction);
begin
  if Action.FActionList<>Self then exit;
  Action.FActionList:=nil;
  FActions.Remove(Action);
  Action.RemoveFreeNotification(Self);
end;

procedure TCustomActionList.Change;
var
  i: Integer;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  for i := 0 to FActions.Count - 1 do
    TContainedAction(FActions[i]).Change;
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
      Action := TCustomAction(FActions[I]);
      if (Action is TCustomAction)
      and ((Action.ShortCut = ShortCut)
           or ((Assigned(Action.FSecondaryShortCuts) and
               (Action.SecondaryShortCuts.IndexOfShortCut(ShortCut) >= 0))))
      then begin
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

function TCustomActionList.GetEnumerator: TActionListEnumerator;
begin
  Result := TActionListEnumerator.Create(Self);
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
  and (SysUtils.CompareText(TAction(FActions[Result]).Name,ActionName)<>0) do
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
  if FState = Value then exit;
  FState := Value;
  if State = asSuspended then exit;
  for I := 0 to FActions.Count - 1 do
  begin
    Action := TAction(FActions[I]);
    if Action is TCustomAction then
    begin
      case Value of
      asNormal:
        begin
          if State = asSuspendedEnabled then
            Action.Enabled := Action.SavedEnabledState;
          Action.Update;
        end;
      asSuspendedEnabled:
        if Value = asSuspendedEnabled then
        begin
          Action.SavedEnabledState := Action.Enabled;
          Action.Enabled := True;
        end;
      end;
    end;
  end;
end;

// included by actnlist.pas
