// included by stdctrls.pp

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{------------------------------------------------------------------------------}

constructor TToggleBox.Create(AOwner : TComponent);
begin
   if assigned(AOwner) then
   begin
      inherited Create(AOwner);
      fCompStyle := csToggleBox;
   end;
end;

{------------------------------------------------------------------------------}

// included by stdctrls.pp

