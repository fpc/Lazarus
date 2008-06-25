{ $Id: Cocoawsmenus.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                               CocoaWSMenus.pp                                *
 *                               ------------                                * 
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
unit CocoaWSMenus;

{$mode objfpc}{$H+}

interface

uses
  // Libs
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  // LCL
  Controls, Forms, Menus, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSMenus, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate;

type

  { TCocoaWSMenuItem }

  TCocoaWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    //class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
  end;

  { TCocoaWSMenu }

  TCocoaWSMenu = class(TWSMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TCocoaWSMainMenu }

  TCocoaWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TCocoaWSPopupMenu }

  TCocoaWSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: Integer); override;
  end;


implementation

{ TCocoaWSMenu }

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenu.CreateHandle
  Params:  AMenu - LCL menu
  Returns: Handle to the menu in Cocoa interface

  Creates new menu in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
//  Result := HMENU(TCocoaMenu.Create(AMenu.Items, True));
end;

{ TCocoaWSMenuItem }

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.AttachMenu
  Params:  AMenuItem - LCL menu item

  Attaches menu item to its parent menu in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.CreateHandle
  Params:  AMenuItem - LCL menu item
  Returns: Handle to the menu item in Cocoa interface

  Creates new menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
//  Result := HMENU(TCocoaMenu.Create(AMenuItem));
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.DestroyHandle
  Params:  AMenuItem - LCL menu item

  Destroys menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetCaption
  Params:  AMenuItem - LCL menu item
           ACaption  - Menu item caption

  Sets the caption of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetShortCut
  Params:  AMenuItem   - LCL menu item
           OldShortCut - Old shortcut
           NewShortCut - New shortcut

  Sets the shortcut of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetVisible
  Params:  AMenuItem - LCL menu item
           Visible   - Menu item visibility

  Sets the visibility of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetCheck
  Params:  AMenuItem - LCL menu item
           Checked   - Menu item checked
  Returns: If the function succeeds

  Sets the check of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetEnable
  Params:  AMenuItem - LCL menu item
           Enabled   - Menu item enabled
  Returns: If the function succeeds

  Sets the enabled of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetRadioItem
  Params:  AMenuItem - LCL menu item
           RadioItem - Menu item has radio
  Returns: If the function succeeds

  Sets the radio behaviour of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
begin

end;

{ TCocoaWSPopupMenu }

{------------------------------------------------------------------------------
  Method:  TCocoaWSPopupMenu.Popup
  Params:  APopupMenu - LCL popup menu
           X, Y       - Screen coordinates to popup

  Popups menu in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
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
///  RegisterWSComponent(TMenuItem, TCocoaWSMenuItem);
///  RegisterWSComponent(TMenu, TCocoaWSMenu);
//  RegisterWSComponent(TMainMenu, TCocoaWSMainMenu);
///  RegisterWSComponent(TPopupMenu, TCocoaWSPopupMenu);
////////////////////////////////////////////////////
end.
