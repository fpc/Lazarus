{%MainUnit ../menus.pas}

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
{ TMenuActionLink }

procedure TMenuActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TMenuItem;
end;

function TMenuActionLink.IsAutoCheckLinked: Boolean;
begin
  Result := FClient.AutoCheck = (Action as TCustomAction).AutoCheck;
end;

function TMenuActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked
    and (AnsiCompareText(FClient.Caption, (Action as TCustomAction).Caption)=0);
end;

function TMenuActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked
    and (FClient.Checked = (Action as TCustomAction).Checked);
end;

function TMenuActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked
    and (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TMenuActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked
            and (FClient.HelpContext = (Action as TCustomAction).HelpContext);
end;

function TMenuActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked
            and (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TMenuActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := FClient.RadioItem and inherited IsGroupIndexLinked
            and (FClient.GroupIndex = (Action as TCustomAction).GroupIndex);
end;

function TMenuActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked
            and (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TMenuActionLink.IsShortCutLinked: Boolean;
begin
  Result := inherited IsShortCutLinked
            and (FClient.ShortCut = (Action as TCustomAction).ShortCut);
end;

function TMenuActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked
            and (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TMenuActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked
            and (@FClient.OnClick = @Action.OnExecute);
end;

procedure TMenuActionLink.SetAutoCheck(Value: Boolean);
begin
  if IsAutoCheckLinked then FClient.AutoCheck := Value;
end;

procedure TMenuActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TMenuActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Checked := Value;
end;

procedure TMenuActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TMenuActionLink.SetHelpContext(Value: THelpContext);
begin
  if IsHelpContextLinked then FClient.HelpContext := Value;
end;

procedure TMenuActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TMenuActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;

procedure TMenuActionLink.SetShortCut(Value: TShortCut);
begin
  if IsShortCutLinked then FClient.ShortCut := Value;
end;

procedure TMenuActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TMenuActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;


// included by menus.pas
