{ $Id$}
{
 *****************************************************************************
 *                              Win32WSMenus.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit Win32WSMenus;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Menus,
////////////////////////////////////////////////////
  WSMenus, WSLCLClasses,
  Windows, Controls, Classes;

type

  { TWin32WSMenuItem }

  TWin32WSMenuItem = class(TWSMenuItem)
  private
  protected
  public
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
  end;

  { TWin32WSMenu }

  TWin32WSMenu = class(TWSMenu)
  private
  protected
  public
  end;

  { TWin32WSMainMenu }

  TWin32WSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TWin32WSPopupMenu }

  TWin32WSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
  end;


implementation

procedure TWin32WSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
var 
  MenuInfo: MENUITEMINFO;
  Style: integer;
begin
  if AMenuItem.Caption = '-' then 
    Style := MFT_SEPARATOR
  else 
    Style := MFT_STRING;
    
  with MenuInfo do
  begin
    cbsize:=sizeof(MENUITEMINFO);
    {In Win32 Menu items that are created without a initial caption default to disabled,
     the next three lines are to counter that.}
    fMask:=MIIM_STATE;
    GetMenuItemInfo(AMenuItem.Parent.Handle,
                    AMenuItem.Command, false, @MenuInfo);
    if AMenuItem.Enabled then
      fState := fState and DWORD(not (MFS_DISABLED or MFS_GRAYED));

    fMask:=MIIM_TYPE or MIIM_STATE;
    fType:=Style;
    dwTypeData:=PChar(ACaption);
    if dwTypeData <> nil then
      cch := Length(ACaption);
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  // owner could be a popupmenu too
  if (AMenuItem.Owner is TWinControl) and
      TWinControl(AMenuItem.Owner).HandleAllocated and
      ([csLoading,csDestroying] * TWinControl(AMenuItem.Owner).ComponentState = []) then
    DrawMenuBar(TWinControl(AMenuItem.Owner).Handle);
end;
  
initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TWin32WSMenuItem);
//  RegisterWSComponent(TMenu, TWin32WSMenu);
//  RegisterWSComponent(TMainMenu, TWin32WSMainMenu);
//  RegisterWSComponent(TPopupMenu, TWin32WSPopupMenu);
////////////////////////////////////////////////////
end.
