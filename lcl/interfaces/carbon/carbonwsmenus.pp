{ $Id$}
{
 *****************************************************************************
 *                               CarbonWSMenus.pp                                * 
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
unit CarbonWSMenus;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  Menus,
////////////////////////////////////////////////////
  // Libs
  FPCMacOSAll, CarbonUtils, CarbonExtra,
  // LCL
  Controls, Forms, Menus, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSMenus, WSLCLClasses,
  // Interface
  CarbonDef, CarbonProc, CarbonMenus,
  CarbonWSControls;

type

  { TCarbonWSMenuItem }

  TCarbonWSMenuItem = class(TWSMenuItem)
  private
  protected
    class function CheckMenuItem(const AMenuItem: TMenuItem;
      const AMethodName: String; AParamName: String = ''): Boolean;
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

  { TCarbonWSMenu }

  TCarbonWSMenu = class(TWSMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TCarbonWSMainMenu }

  TCarbonWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TCarbonWSPopupMenu }

  TCarbonWSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: Integer); override;
  end;


implementation

{ TCarbonWSMenu }

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenu.CreateHandle
  Params:  AMenu - LCL menu
  Returns: Handle to the menu in Carbon interface

  Creates new menu in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := HMENU(TCarbonMenu.Create(AMenu.Items, True));
end;

{ TCarbonWSMenuItem }

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenu.CheckMenuItem
  Params:  AMenuItem   - LCL menu item
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the menu item is valid
 ------------------------------------------------------------------------------}
class function TCarbonWSMenuItem.CheckMenuItem(const AMenuItem: TMenuItem;
  const AMethodName: String; AParamName: String): Boolean;
begin
  if AMenuItem <> nil then
  begin
    if TObject(AMenuItem.Handle) is TCarbonMenu then
    begin
      {$IFDEF VerboseWSClass}
        DebugLn(Self.ClassName + '.' + DbgText + ' ' + AParamName + ' for ' + AMenuItem.Name);
      {$ENDIF}

      Result := True;
    end
    else
    begin
      Result := False;
      DebugLn(Self.ClassName + '.' + AMethodName + ' ' + AParamName + ' for ' +
        AMenuItem.Name + ' failed: Handle ' +
        DbgS(Integer(AMenuItem.Handle)) + ' is invalid!');
    end;
  end
  else
  begin
    Result := False;
    DebugLn(Self.ClassName + '.' + AMethodName + ' ' + AParamName + ' for ' +
      AMenuItem.Name + ' failed: MenuItem is nil!');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.AttachMenu
  Params:  AMenuItem - LCL menu item
  Returns: Nothinh

  Attaches menu item to its parent menu in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
  if not CheckMenuItem(AMenuItem, 'AttachMenu') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'AttachMenu', 'Parent') then Exit;
  
  TCarbonMenu(AMenuItem.Parent.Handle).Add(TCarbonMenu(AMenuItem.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.CreateHandle
  Params:  AMenuItem - LCL menu item
  Returns: Handle to the menu item in Carbon interface

  Creates new menu item in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := HMENU(TCarbonMenu.Create(AMenuItem));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.DestroyHandle
  Params:  AMenuItem - LCL menu item
  Returns: Nothing

  Destroys menu item in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  if not CheckMenuItem(AMenuItem, 'DestroyHandle') then Exit;
  
  TCarbonMenu(AMenuItem.Handle).Free;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetCaption
  Params:  AMenuItem - LCL menu item
           ACaption  - Menu item caption
  Returns: Nothing

  Sets the caption of menu item in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
begin
  if not CheckMenuItem(AMenuItem, 'SetCaption') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'SetCaption', 'Parent') then Exit;

  TCarbonMenu(AMenuItem.Handle).SetCaption(ACaption);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetShortCut
  Params:  AMenuItem   - LCL menu item
           OldShortCut - Old shortcut
           NewShortCut - New shortcut
  Returns: Nothing

  Sets the shortcut of menu item in Carbon interface
  NOTE: only Command Key (ssCtrl) is supported
 ------------------------------------------------------------------------------}
class procedure TCarbonWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
var
  Shift: TShiftState;
  Key: Word;
begin
  if not CheckMenuItem(AMenuItem, 'SetShortCut') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'SetShortCut', 'Parent') then Exit;
  
  ShortCutToKey(NewShortCut, Key, Shift);
  
  if not (ssCtrl in Shift) then
    DebugLn('Note: Carbon menus supports only shortcuts with Ctrl!');
    
  SetMenuItemCommandKey(AsMenuRef(AMenuItem.Parent.Handle),
    AMenuItem.MenuIndex + 1, False, Key);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetVisible
  Params:  AMenuItem - LCL menu item
           Visible   - Menu item visibility
  Returns: Nothing

  Sets the visibility of menu item in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin
  if not CheckMenuItem(AMenuItem, 'SetVisible') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'SetVisible', 'Parent') then Exit;

  TCarbonMenu(AMenuItem.Handle).SetVisible(Visible);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetCheck
  Params:  AMenuItem - LCL menu item
           Checked   - Menu item checked
  Returns: If the function succeeds

  Sets the check of menu item in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
begin
  Result := False;
  if not CheckMenuItem(AMenuItem, 'SetCheck') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'SetCheck', 'Parent') then Exit;
  
  if AMenuItem.Checked then
  begin
    {if AMenuItem.RadioItem then
      SetItemMark(AsMenuRef(AMenuItem.Parent.Handle), AMenuItem.MenuIndex + 1,
        Char(kDiamondCharCode)) // or kBulletCharCode
    else
      SetItemMark(AsMenuRef(AMenuItem.Parent.Handle), AMenuItem.MenuIndex + 1,
        Char(kCheckCharCode));}
  end
  else
    if AMenuItem.Count = 0 then
      SetItemMark(AsMenuRef(AMenuItem.Parent.Handle), AMenuItem.MenuIndex + 1, #0);
  FPCMacOSAll.CheckMenuItem(AsMenuRef(AMenuItem.Parent.Handle), AMenuItem.MenuIndex + 1,
    AMenuItem.Checked);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetEnable
  Params:  AMenuItem - LCL menu item
           Enabled   - Menu item enabled
  Returns: If the function succeeds

  Sets the enabled of menu item in Carbon interface
  TODO: disable menu bar items
 ------------------------------------------------------------------------------}
class function TCarbonWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result := False;
  if not CheckMenuItem(AMenuItem, 'SetEnable') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'SetEnable', 'Parent') then Exit;

  TCarbonMenu(AMenuItem.Handle).SetEnable(Enabled);
  
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetRadioItem
  Params:  AMenuItem - LCL menu item
           RadioItem - Menu item has radio
  Returns: If the function succeeds

  Sets the radio behaviour of menu item in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
begin
  Result := SetCheck(AMenuItem, AMenuItem.Checked);
end;

{ TCarbonWSPopupMenu }

{------------------------------------------------------------------------------
  Method:  TCarbonWSPopupMenu.Popup
  Params:  APopupMenu - LCL popup menu
           X, Y       - Screen coordinates to popup
  Returns: Nothing

  Creates new menu in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
  Y: integer);
begin
  if not CheckMenu(APopupMenu.Handle, 'TCarbonWSPopupMenu.Popup') then Exit;
  
  PopUpMenuSelect(AsMenuRef(APopupMenu.Handle), Y, X, 0);
end;                                         // ^- order top, left is correct!

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TCarbonWSMenuItem);
  RegisterWSComponent(TMenu, TCarbonWSMenu);
//  RegisterWSComponent(TMainMenu, TCarbonWSMainMenu);
  RegisterWSComponent(TPopupMenu, TCarbonWSPopupMenu);
////////////////////////////////////////////////////
end.
