{%MainUnit ../controls.pp}

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

{ TControlActionLink }

procedure TControlActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TControl;
end;

function TControlActionLink.DoShowHint(var HintStr: string): Boolean;
begin
  Result := True;
  if Action is TCustomAction then
  begin
    if TCustomAction(Action).DoHint(HintStr)
    and Application.HintShortCuts
    and (TCustomAction(Action).ShortCut <> scNone) then
    begin
      if HintStr <> '' then
        HintStr := Format('%s (%s)', [HintStr,
                               ShortCutToText(TCustomAction(Action).ShortCut)]);
    end;
  end;
end;

function TControlActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked
            and (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TControlActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked
            and (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TControlActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked
            and (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TControlActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked
            and (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TControlActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked
            and (FClient.OnClick = Action.OnExecute);
end;

procedure TControlActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TControlActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TControlActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TControlActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TControlActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;

function TControlActionLink.IsHelpLinked: Boolean;
begin
  Result := (FClient.HelpContext = (Action as TCustomAction).HelpContext)
        and (FClient.HelpKeyword = (Action as TCustomAction).HelpKeyword)
        and (FClient.HelpType = (Action as TCustomAction).HelpType);
end;

procedure TControlActionLink.SetHelpKeyword(const Value: String);
begin
  if IsHelpLinked then FClient.HelpKeyword := Value;
end;

procedure TControlActionLink.SetHelpContext(Value: THelpContext);
begin
  if IsHelpLinked then FClient.HelpContext := Value;
end;

procedure TControlActionLink.SetHelpType(Value: THelpType);
begin
  if IsHelpLinked then FClient.HelpType := Value;
end;

// included by controls.pp
