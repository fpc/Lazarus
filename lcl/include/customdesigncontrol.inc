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
  inherited;

  if AMode = lapAutoAdjustForDPI then
    FPixelsPerInch := AToPPI;
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

procedure TCustomDesignControl.Loaded;
  procedure FixChildren(const AParent: TWinControl);
  var
    I: Integer;
  begin
    for I := 0 to AParent.ControlCount-1 do
    begin
      AParent.Controls[I].FixDesignFontsPPI(FDesignTimePPI);
      if AParent.Controls[I] is TWinControl then
        FixChildren(TWinControl(AParent.Controls[I]));
    end;
  end;
begin
  inherited Loaded;

  FPixelsPerInch := FDesignTimePPI;

  if Application.Scaled and Scaled then
  begin
    FixDesignFontsPPI(FDesignTimePPI);
    FixChildren(Self);
  end;
end;

procedure TCustomDesignControl.SetDesignTimePPI(const ADesignTimePPI: Integer);
begin
  if FDesignTimePPI=ADesignTimePPI then
    Exit;

  if (csLoading in ComponentState) // allow setting only when loading
  or not (csDesigning in ComponentState) // or in runtime (the programmer has to know why he is doing that)
  or ((csDesigning in ComponentState) and (ADesignTimePPI=Screen.PixelsPerInch)) // or in designtime when setting the correct value
  then
    FDesignTimePPI := ADesignTimePPI
  else
    raise EInvalidOperation.Create(sCannotSetDesignTimePPI);
end;

procedure TCustomDesignControl.SetScaled(const AScaled: Boolean);
begin
  if FScaled=AScaled then
    Exit;

  FScaled := AScaled;
  if not FScaled then
    Font.PixelsPerInch := Screen.PixelsPerInch;
end;
