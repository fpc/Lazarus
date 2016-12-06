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

  FDesignTimePPI := 96;
  FPixelsPerInch := FDesignTimePPI;
end;

procedure TCustomDesignControl.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromDPI, AToDPI, AOldFormWidth, ANewFormWidth: Integer;
  const AScaleFonts: Boolean);
begin
  inherited AutoAdjustLayout(AMode, AFromDPI, AToDPI, AOldFormWidth,
    ANewFormWidth, AScaleFonts);

  FPixelsPerInch := AToDPI;
end;

procedure TCustomDesignControl.SetDesignTimePPI(const ADesignTimePPI: Integer);
begin
  if (csLoading in ComponentState) // allow setting only when loading
  or not (csDesigning in ComponentState) then // or in runtime (the programmer has to know why he is doing that)
    FDesignTimePPI := ADesignTimePPI
  else
    raise EInvalidOperation.Create(sCannotSetDesignTimePPI);
end;
