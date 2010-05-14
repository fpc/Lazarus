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

{$mode delphi}{$H+}

interface

uses
  // LCL
  SysUtils, Menus, Forms,
  // widgetset
  WSMenus, WSLCLClasses, LCLType, fpguiobjects, fpguiwsprivate,
  // interface
  fpg_base, fpg_main, fpg_menu;

type

  { TFpGuiWSMenuItem }

  TFpGuiWSMenuItem = class(TWSMenuItem)
  private
  protected
  published
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
  published
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
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

uses
  LCLMessageGlue;

{ TFpGuiWSMenuItem }

class procedure TFpGuiWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin

end;

{------------------------------------------------------------------------------
  Function: TFpGuiWSMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu Item
 ------------------------------------------------------------------------------}
class function TFpGuiWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Menu: TFPGUIPrivateMenuItem;
  AMenuName: string;
//  hotkeydef: string;
  { Possible parents }
  ParentPrivateItem: TFPGUIPrivateMenuItem;
  ParentMenuBar: TfpgMenuBar;
  ParentPrivatePopUp: TFPGUIPrivatePopUpMenu;
begin
  {$ifdef VerboseFPGUIIntf}
    WriteLn('trace:> [TFPGuiWSMenuItem.CreateHandle] Caption: ', AMenuItem.Caption,
     ' Subitems: ' + IntToStr(AMenuItem.Count));

    Write('trace:< [TFPGuiWSMenuItem.CreateHandle]');
  {$endif}

  {------------------------------------------------------------------------------
    Set's default values for variables used at several places bellow
   ------------------------------------------------------------------------------}
  AMenuName := AMenuItem.Caption;

  Menu := nil;

  {------------------------------------------------------------------------------
    This case should not happen. A menu item must have a parent, but it seams LCL
   will sometimes create a menu item prior to creating it's parent.
    So, if we arrive here, we must create this item as if it was a TMenu
   ------------------------------------------------------------------------------}
  if (not AMenuItem.HasParent) then
  begin
    {$ifdef VerboseFPGUIIntf}
      Write(' Parent: Menu without parent');
    {$endif}

//    Result := TQtWSMenu.CreateHandle(AMenuItem.GetParentMenu);
  end
  {------------------------------------------------------------------------------
    If the parent has no parent, then this item is directly owned by a TMenu
    In this case we have to detect if the parent is a TMainMenu or a TPopUpMenu
   because TMainMenu uses the special Handle TfpgMenuBar
   ------------------------------------------------------------------------------}
  else
  if AMenuItem.Parent.HasParent then begin
    {------------------------------------------------------------------------------
      If the parent has a parent, then that item's Handle is necessarely a TFPGUIPrivateMenuItem
     ------------------------------------------------------------------------------}
    Menu := TFPGUIPrivateMenuItem.Create;
    Menu.LCLMenuItem := AMenuItem;
    ParentPrivateItem := TFPGUIPrivateMenuItem(AMenuItem.Parent.Handle);
    Menu.MenuItem := ParentPrivateItem.MenuItem.SubMenu.AddMenuItem(AMenuName, '', Menu.HandleOnClick);
    Result := HMENU(Menu);
  end else if (AMenuItem.GetParentMenu is TMainMenu) then begin
    Menu := TFPGUIPrivateMenuItem.Create;
    Menu.LCLMenuItem := AMenuItem;
    ParentMenuBar := TfpgMenuBar(AMenuItem.GetParentMenu.Handle);
    Menu.MenuItem := ParentMenuBar.AddMenuItem(AMenuName, Menu.HandleOnClick);
    Result := HMENU(Menu);
  end else begin
    Menu := TFPGUIPrivateMenuItem.Create;
    Menu.LCLMenuItem := AMenuItem;
    ParentPrivatePopUp := TFPGUIPrivatePopUpMenu(AMenuItem.GetParentMenu.Handle);
    Menu.MenuItem := ParentPrivatePopUp.PopUpMenu.AddMenuItem(AMenuName,'',Menu.HandleOnClick);
    Result := HMENU(Menu);
  end;

  {------------------------------------------------------------------------------
    If the menuitem has submenus, create a popupmenu for the submenu
   ------------------------------------------------------------------------------}
  if AMenuItem.Count > 0 then
  begin
    Menu.MenuItem.SubMenu := TfpgPopupMenu.Create(Menu.MenuItem);
  end;

  {$ifdef VerboseFPGUIIntf}
    WriteLn(' Result: ', IntToStr(Result));
  {$endif}
end;

class procedure TFpGuiWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  TFPGUIPrivateMenuItem(AMenuItem.Handle).Free;
  AMenuItem.Handle:=0;
end;

class procedure TFpGuiWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
var
  APrivate: TfpgMenuItem;
begin
  APrivate:=TfpgMenuItem(AMenuItem.Handle);
  APrivate.Text:=ACaption;
end;

class procedure TFpGuiWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
var
  APrivate: TfpgMenuItem;
begin
  APrivate:=TfpgMenuItem(AMenuItem.Handle);
  APrivate.Visible:=Visible;
end;

class function TFpGuiWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
begin
  Result:=false; //Default by now
end;

class function TFpGuiWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result:=false; //Default by now
end;

{ TFpGuiWSMenu }

class function TFpGuiWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  MenuBar: TfpgMenuBar;
  Menu: TFPGUIPrivatePopUpMenu;
  msg: TfpgMessageParams;
begin
  {------------------------------------------------------------------------------
    If the menu is a main menu, there is no need to create a handle for it.
    It's already created on the window
   ------------------------------------------------------------------------------}
  if (AMenu is TMainMenu) and (AMenu.Owner is TCustomForm) then
  begin
    MenuBar := TFPGUIPrivateWindow(TCustomForm(AMenu.Owner).Handle).MenuBar;
    MenuBar.Visible := True;
    MenuBar.Align := alTop;
    
    Result := HMENU(MenuBar);
    //Notify LCL to repaint because MainMenu changes NCBorders
    msg.rect:=MenuBar.Parent.GetBoundsRect;
    fpgSendMessage(MenuBar,MenuBar.Parent,FPGM_RESIZE,msg);
  end
  {------------------------------------------------------------------------------
    The menu is a popup menu
   ------------------------------------------------------------------------------}
  else if (AMenu is TPopUpMenu) then
  begin
    Menu := TFPGUIPrivatePopUpMenu.Create(TPopUpMenu(AMenu), AMenu.Items);

    Result := HMENU(Menu);
  end
  else
    Result := HMENU(0);

  {$ifdef VerboseFPGUIIntf}
    Write('[TFpGuiWSMenu.CreateHandle] ');

    if (AMenu is TMainMenu) then Write('IsMainMenu ');

    WriteLn(' Handle: ', IntToStr(Result), ' Name: ', AMenu.Name);
  {$endif}
end;

{ TFpGuiWSPopupMenu }

class procedure TFpGuiWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
  TFPGUIPrivatePopUpMenu(APopUpMenu.Handle).PopUp(X, Y);
end;

end.
