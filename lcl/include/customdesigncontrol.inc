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
  Font.PixelsPerInch := FDesignTimePPI;
end;

procedure TCustomDesignControl.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
begin
  inherited;

  FPixelsPerInch := AToPPI;
end;

procedure TCustomDesignControl.Loaded;
var
  I: Integer;
begin
  inherited Loaded;

  FPixelsPerInch := FDesignTimePPI;

  if Application.Scaled and Scaled then
  begin
    FixDesignFontsPPI(FDesignTimePPI);
    for I := 0 to ComponentCount-1 do
      if Components[I] is TControl then
        TControl(Components[I]).FixDesignFontsPPI(FDesignTimePPI);
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
  FScaled := AScaled;
end;
