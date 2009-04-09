{%MainUnit ../menus.pp}
{******************************************************************************
                                  TMainMenu
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{------------------------------------------------------------------------------
  Method: TMainMenu.ItemChanged
  Params:  none
  Returns: Nothing

  Called whenever
 ------------------------------------------------------------------------------}
procedure TMainMenu.ItemChanged;
begin
  {MenuChanged(nil, nil, False);
  if FWindowHandle <> 0 then
    SendMessage(FWindowHandle, CM_MENUCHANGED, 0, 0);}
end;

{------------------------------------------------------------------------------
  Method: TMainMenu.Create
  Params:  AOwner: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TMainMenu.Create(AOwner : TComponent);
begin
  FCompStyle := csMainMenu;
  inherited Create(AOwner);
end;

// included by menus.pp

