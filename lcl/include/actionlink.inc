{%MainUnit ../actnlist.pas}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TActionLink }

function TActionLink.IsCaptionLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsCheckedLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsEnabledLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsHelpContextLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsHelpLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsHintLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsImageIndexLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsShortCutLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

function TActionLink.IsVisibleLinked: Boolean;
begin
  Result := Action is TCustomAction;
end;

procedure TActionLink.SetAutoCheck(Value: Boolean);
begin

end;

procedure TActionLink.SetCaption(const Value: string);
begin

end;

procedure TActionLink.SetChecked(Value: Boolean);
begin

end;

procedure TActionLink.SetEnabled(Value: Boolean);
begin

end;

procedure TActionLink.SetGroupIndex(Value: Integer);
begin

end;

procedure TActionLink.SetHelpContext(Value: THelpContext);
begin

end;

procedure TActionLink.SetHelpKeyword(const Value: string);
begin

end;

procedure TActionLink.SetHelpType(Value: THelpType);
begin

end;

procedure TActionLink.SetHint(const Value: string);
begin

end;

procedure TActionLink.SetImageIndex(Value: Integer);
begin

end;

procedure TActionLink.SetShortCut(Value: TShortCut);
begin

end;

procedure TActionLink.SetVisible(Value: Boolean);
begin

end;

function TActionLink.Update: Boolean;
begin
  {$IF FPC_Fullversion<30301} // Workaround for issue #41063 / Partial fix for actionlinks that inherit TActionLink only
  Result := False;
  if Assigned(Action) then
  {$ENDIF}
  Result := inherited Update;
end;

// included by actnlist.pas
