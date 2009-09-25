{%MainUnit ../menus.pp}
{******************************************************************************
                                  TMainMenu
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

procedure TMainMenu.SetWindowHandle(const AValue: HWND);
begin
  FWindowHandle := AValue;
  if HandleAllocated then
  begin
    SetMenu(FWindowHandle, Handle);
    BidiModeChanged;
  end
  else
    SetMenu(FWindowHandle, 0);
end;

{------------------------------------------------------------------------------
  Method: TMainMenu.ItemChanged
  Params:  none
  Returns: Nothing

  Called whenever
 ------------------------------------------------------------------------------}
procedure TMainMenu.ItemChanged;
begin
  MenuChanged(nil, nil, False);
  // perform CM_MENUCHANGED in owner
end;

class procedure TMainMenu.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterMainMenu;
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
  FWindowHandle := 0;
  inherited Create(AOwner);
end;

// included by menus.pp

