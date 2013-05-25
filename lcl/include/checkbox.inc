{%MainUnit ../stdctrls.pp}

{******************************************************************************
                               checkbox.inc
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// included by stdctrls.pp

{ TCheckBox }

constructor TCheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csCheckbox;
  TabStop := True;
  AutoSize := True;
  //FState := cbUnchecked;
  //FAllowGrayed := false;
end;
