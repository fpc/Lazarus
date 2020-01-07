{%MainUnit ../forms.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TCustomDesignControl }

constructor TCustomDesignControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FScaled := True;
  FDesignTimePPI := 96;
  FPixelsPerInch := FDesignTimePPI;
  if Application.Scaled then
    Font.PixelsPerInch := FDesignTimePPI;
end;

procedure TCustomDesignControl.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin
  if (AMode = lapAutoAdjustForDPI) and (AToPPI=FPixelsPerInch) then // don't do anything if FPixelsPerInch already is AToPPI
    Exit;

  // override AFromPPI with FPixelsPerInch - it is needed e.g. when a frame is placed on form
  // - the frame can have a different PPI than the parent form.
  // See issue #36370
  inherited AutoAdjustLayout(AMode, FPixelsPerInch, AToPPI, AOldFormWidth, ANewFormWidth);

  if AMode = lapAutoAdjustForDPI then
    FPixelsPerInch := AToPPI;
end;

function TCustomDesignControl.DesignTimePPIIsStored: Boolean;
begin
  if Assigned(Owner) then
    Result := False // inplace frames do not store DesignTimePPI - always the parent's DesignTimePPI is used
  else
    Result := FDesignTimePPI<>96;
end;

procedure TCustomDesignControl.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double
  );
var
  NewWidth, NewHeight: Integer;
begin
  if Assigned(Parent) then
  begin
    inherited;
    Exit;
  end;

  // Apply the changes
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    NewWidth := Round(Width*AXProportion);
    NewHeight := Round(Height*AYProportion);

    BorderSpacing.AutoAdjustLayout(AXProportion, AYProportion);
    Constraints.AutoAdjustLayout(AXProportion, AYProportion);

    SetBounds(Left, Top, NewWidth, NewHeight);
  end;
end;

procedure TCustomDesignControl.SetDesignTimePPI(const ADesignTimePPI: Integer);
begin
  if FDesignTimePPI=ADesignTimePPI then
    Exit;

  FDesignTimePPI := ADesignTimePPI;
  if csLoading in ComponentState then
    FPixelsPerInch := FDesignTimePPI; // set FPixelsPerInch in LFM loading
end;

procedure TCustomDesignControl.SetScaled(const AScaled: Boolean);
begin
  if FScaled=AScaled then
    Exit;

  FScaled := AScaled;
  if not FScaled then
    Font.PixelsPerInch := Screen.PixelsPerInch;
end;
