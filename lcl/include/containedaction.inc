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


{ TContainedAction }

destructor TContainedAction.Destroy;
begin
  if ActionList <> nil then ActionList.RemoveAction(Self);
  inherited Destroy;
end;

function TContainedAction.GetIndex: Integer;
begin
  if ActionList <> nil then
    Result := ActionList.FActions.IndexOf(Self)
  else
    Result := -1;
end;

function TContainedAction.GetParentComponent: TComponent;
begin
  if ActionList <> nil then
    Result := ActionList
  else
    Result := inherited GetParentComponent;
end;

function TContainedAction.HasParent: Boolean;
begin
  if ActionList <> nil then
    Result := True
  else
    Result := inherited HasParent;
end;

procedure TContainedAction.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TCustomActionList then
    ActionList := TCustomActionList(Reader.Parent);
end;

procedure TContainedAction.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := ActionList.FActions.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex
    then ActionList.FActions.Move(CurIndex, Value);
  end;
end;

procedure TContainedAction.SetCategory(const Value: string);
begin
  if Value <> Category then
  begin
    FCategory := Value;
    if ActionList <> nil then
      ActionList.Change;
  end;
end;

procedure TContainedAction.SetActionList(NewActionList: TCustomActionList);
begin
  if NewActionList <> ActionList then
  begin
    if ActionList <> nil then ActionList.RemoveAction(Self);
    // FActionList is set by AddAction
    if NewActionList <> nil then NewActionList.AddAction(Self);
  end;
end;

procedure TContainedAction.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) and (AParent is TCustomActionList) then
    ActionList := TCustomActionList(AParent);
end;

function TContainedAction.Execute: Boolean;
begin
  Result := ((ActionList <> nil) and ActionList.ExecuteAction(Self))
    or ((ApplicationActionComponent<>nil)
         and ApplicationActionComponent.ExecuteAction(Self))
    or (inherited Execute)
    or (SendApplicationMessage(CM_ACTIONEXECUTE, 0, PtrInt(Self)) = 1);
end;

function TContainedAction.Update: Boolean;
begin
  Result := ((ActionList <> nil) and ActionList.UpdateAction(Self))
    or ((ApplicationActionComponent<>nil)
         and ApplicationActionComponent.UpdateAction(Self))
    or (inherited Update)
    or (SendApplicationMessage(CM_ACTIONUPDATE, 0, PtrInt(Self)) = 1);
end;

// included by actnlist.pas
