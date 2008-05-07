{ $Id: FpGuiwsmenus.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                               FpGuiWSMenus.pp                                * 
 *                               ------------                                * 
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
unit FpGuiWSMenus;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Menus, Forms,
  // widgetset
  WSMenus, WSLCLClasses, LCLType, fpguiobjects, fpguiwsprivate,
  // interface
  gui_menu;

type

  { TFpGuiWSMenuItem }

  TFpGuiWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
//    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
//    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
//    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
//    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
  end;

  { TFpGuiWSMenu }

  TFpGuiWSMenu = class(TWSMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
//    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); override;
  end;

  { TFpGuiWSMainMenu }

  TFpGuiWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TFpGuiWSPopupMenu }

  TFpGuiWSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{ TFpGuiWSMenuItem }

class procedure TFpGuiWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin

end;

class function TFpGuiWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := HMENU(TFPGUIPrivateMenuItem.Create(AMenuItem));
end;

class procedure TFpGuiWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  TFPGUIPrivateMenuItem(AMenuItem.Handle).Free;
end;

class procedure TFpGuiWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
begin

end;

class procedure TFpGuiWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin

end;

class function TFpGuiWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
begin

end;

class function TFpGuiWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin

end;

{ TFpGuiWSMenu }

class function TFpGuiWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  MenuBar: TfpgMenuBar;
  Menu: TFPGUIPrivatePopUpMenu;
begin
  { If the menu is a main menu, there is no need to create a handle for it.
    It's already created on the window }
  if (AMenu is TMainMenu) and (AMenu.Owner is TCustomForm) then
  begin
    MenuBar := TFPGUIPrivateWindow(TCustomForm(AMenu.Owner).Handle).MenuBar;

    Result := HMENU(MenuBar);
  end
  else if (AMenu is TPopUpMenu) then
  begin
    Menu := TFPGUIPrivatePopUpMenu.Create(TPopUpMenu(AMenu), AMenu.Items);

    Result := HMENU(Menu);
  end;

  {$ifdef VerboseFPGUIPrivate}
    Write('[TFpGuiWSMenu.CreateHandle] ');

    if (AMenu is TMainMenu) then Write('IsMainMenu ');

    WriteLn(' Handle: ', dbghex(Result), ' Name: ', AMenu.Name);
  {$endif}
end;

{ TFpGuiWSPopupMenu }

class procedure TFpGuiWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
  TFPGUIPrivatePopUpMenu(APopUpMenu.Handle).PopUp(X, Y);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TFpGuiWSMenuItem);
  RegisterWSComponent(TMenu, TFpGuiWSMenu);
//  RegisterWSComponent(TMainMenu, TFpGuiWSMainMenu);
  RegisterWSComponent(TPopupMenu, TFpGuiWSPopupMenu);
////////////////////////////////////////////////////
end.
