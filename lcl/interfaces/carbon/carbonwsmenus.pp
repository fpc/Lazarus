{ $Id$}
{
 *****************************************************************************
 *                               CarbonWSMenus.pp                            * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit CarbonWSMenus;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  // Libs
  MacOSAll,
  CarbonUtils,
  // LCL
  Controls, Forms, Menus, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSMenus, WSLCLClasses,
  // LCL Carbon
  CarbonDef, CarbonMenus, CarbonWSControls;

type

  { TCarbonWSMenuItem }

  TCarbonWSMenuItem = class(TWSMenuItem)
  private
  protected
    class function CheckMenuItem(const AMenuItem: TMenuItem;
      const AMethodName: String; AParamName: String = ''): Boolean;
  published
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
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
  end;

  { TCarbonWSMenu }

  TCarbonWSMenu = class(TWSMenu)
  published
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TCarbonWSMainMenu }

  TCarbonWSMainMenu = class(TWSMainMenu)
  published
  end;

  { TCarbonWSPopupMenu }

  TCarbonWSPopupMenu = class(TWSPopupMenu)
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: Integer); override;
  end;


implementation

uses
  CarbonProc;

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
        DebugLn(Self.ClassName + '.' + AMethodName + ' ' + AParamName + ' for ' + AMenuItem.Name);
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

  Sets the shortcut of menu item in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
  if not CheckMenuItem(AMenuItem, 'SetShortCut') then Exit;
  if not CheckMenuItem(AMenuItem.Parent, 'SetShortCut', 'Parent') then Exit;
  
  TCarbonMenu(AMenuItem.Handle).SetShortCut(NewShortCut);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetVisible
  Params:  AMenuItem - LCL menu item
           Visible   - Menu item visibility

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

  TCarbonMenu(AMenuItem.Handle).SetCheck(Checked);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSMenuItem.SetEnable
  Params:  AMenuItem - LCL menu item
           Enabled   - Menu item enabled
  Returns: If the function succeeds

  Sets the enabled of menu item in Carbon interface
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

class procedure TCarbonWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if not CheckMenu(AMenuItem.Handle, 'UpdateMenuIcon') then Exit;
  TCarbonMenu(AMenuItem.Handle).Update;
end;

{ TCarbonWSPopupMenu }

{------------------------------------------------------------------------------
  Method:  TCarbonWSPopupMenu.Popup
  Params:  APopupMenu - LCL popup menu
           X, Y       - Screen coordinates to popup

  Popups menu in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
  Y: integer);
begin
  if not CheckMenu(APopupMenu.Handle, 'TCarbonWSPopupMenu.Popup') then Exit;
  
  PopUpMenuSelect(TCarbonMenu(APopupMenu.Handle).Menu, Y, X, 0);
                                             // ^- order top, left is correct!
  APopupMenu.Close; // notify LCL popup menu
end;

end.
