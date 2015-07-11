{%MainUnit ../extctrls.pp}

{******************************************************************************
                                 TBoundLabel
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


{ TBoundLabel }

constructor TBoundLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name := 'SubLabel';
  SetSubComponent(True);
  if Assigned(TheOwner) then
    Caption := TheOwner.Name;
end;


// included by extctrls.pp
