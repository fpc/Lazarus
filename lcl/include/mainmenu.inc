{%MainUnit ../menus.pp}
{******************************************************************************
                                  TMainMenu
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
end;

class procedure TMainMenu.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterMainMenu;
  RegisterPropertyToSkip(TMainMenu, 'AutoMerge', 'VCL compatibility property', '');
end;

procedure TMainMenu.MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean);
begin
  // pass CM_MENUCANGED to the form which own the menu
  if WindowHandle <> 0 then
    SendMessage(WindowHandle, CM_MENUCHANGED, 0, 0);
  inherited MenuChanged(Sender, Source, Rebuild);
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

function TMainMenu.GetHeight: Integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
end;

// included by menus.pp

