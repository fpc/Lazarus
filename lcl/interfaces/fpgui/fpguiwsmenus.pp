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
  Menus,
  WSMenus, WSLCLClasses, LCLType,
  fpguiobjects;

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
//    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
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

{ TFpGuiWSPopupMenu }

class procedure TFpGuiWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
  Y: integer);
begin

end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TFpGuiWSMenuItem);
//  RegisterWSComponent(TMenu, TFpGuiWSMenu);
//  RegisterWSComponent(TMainMenu, TFpGuiWSMainMenu);
  RegisterWSComponent(TPopupMenu, TFpGuiWSPopupMenu);
////////////////////////////////////////////////////
end.
