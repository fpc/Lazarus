{%MainUnit ../extctrls.pp}

{******************************************************************************
                                 TBoundLabel
 ******************************************************************************

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
