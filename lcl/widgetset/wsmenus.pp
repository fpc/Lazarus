{ $Id$}
{
 *****************************************************************************
 *                                WSMenus.pp                                 * 
 *                                ----------                                 * 
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
unit WSMenus;

{$mode objfpc}{$H+}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Menus,
////////////////////////////////////////////////////
  WSLCLClasses, LCLType;

type
  { TWSMenuItem }

  TWSMenuItem = class(TWSLCLComponent)
    class procedure AttachMenu(const AMenuItem: TMenuItem); virtual;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; virtual;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); virtual;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); virtual;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); virtual;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); virtual;
  end;
  TWSMenuItemClass = class of TWSMenuItem;

  { TWSMenu }

  TWSMenuClass = class of TWSMenu;
  TWSMenu = class(TWSLCLComponent)
    class function  CreateHandle(const AMenu: TMenu): HMENU; virtual;
  end;

  { TWSMainMenu }

  TWSMainMenu = class(TWSMenu)
  end;

  { TWSPopupMenu }

  TWSPopupMenu = class(TWSMenu)
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); virtual;
  end;
  TWSPopupMenuClass = class of TWSPopupMenu;


implementation

{ TWSMenuItem }

procedure TWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
end;

function  TWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := 0;
end;

procedure TWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
end;

procedure TWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
end;

procedure TWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
end;

procedure TWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin

end;
          
{ TWSMenu }

function  TWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := 0;
end;

{ TWSPopupMenu }

procedure TWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
end;

initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TMenuItem, TWSMenuItem);
//  RegisterWSComponent(TMenu, TWSMenu);
//  RegisterWSComponent(TMainMenu, TWSMainMenu);
//  RegisterWSComponent(TPopupMenu, TWSPopupMenu);
////////////////////////////////////////////////////
end.
