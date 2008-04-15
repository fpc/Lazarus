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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Menus, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, LCLType, LCLProc;

type
  { TWSMenuItem }

  TWSMenuItem = class(TWSLCLComponent)
    class procedure AttachMenu(const AMenuItem: TMenuItem); virtual;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; virtual;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); virtual;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); virtual;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); virtual;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); virtual;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; virtual;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; virtual;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; virtual;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; virtual;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); virtual;
  end;
  TWSMenuItemClass = class of TWSMenuItem;

  { TWSMenu }

  TWSMenuClass = class of TWSMenu;
  TWSMenu = class(TWSLCLComponent)
    class function CreateHandle(const AMenu: TMenu): HMENU; virtual;
    
    class procedure SetBiDiMode(const AMenu: TMenu; const ABiDiMode: TBiDiMode); virtual;
  end;

  { TWSMainMenu }

  TWSMainMenu = class(TWSMenu)
  end;

  { TWSPopupMenu }

  TWSPopupMenu = class(TWSMenu)
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); virtual;
  end;
  TWSPopupMenuClass = class of TWSPopupMenu;

function WSCheckMenuItem(const AMenuItem: TMenuItem;
  const AProcName: String): Boolean;

implementation

{ TWSMenuItem }

class procedure TWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
end;

class function  TWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := 0;
end;

class procedure TWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
end;

class procedure TWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
end;

class procedure TWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
end;

class procedure TWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin
end;

class function TWSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  Result := false;
end;

class function TWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
begin
  Result := false;
end;

class function TWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
begin
  Result := false;
end;

class function TWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := false;
end;

class procedure TWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap);
begin
  // emulate old behaviour
  AMenuItem.RecreateHandle;
end;


          
{ TWSMenu }

class function  TWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := 0;
end;

class procedure TWSMenu.SetBiDiMode(const AMenu: TMenu; const ABiDiMode: TBiDiMode);
begin
end;

{ TWSPopupMenu }

class procedure TWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
end;

function WSCheckMenuItem(const AMenuItem: TMenuItem;
  const AProcName: String): Boolean;

  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without handle for %s(%s)', [AProcName, AMenuItem.Name, AMenuItem.ClassName]);
  end;
begin
  Result := AMenuItem.HandleAllocated;
  if Result then Exit;
  Warn;
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
