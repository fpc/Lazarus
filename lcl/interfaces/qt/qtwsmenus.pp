{ $Id$}
{
 *****************************************************************************
 *                               QtWSMenus.pp                                * 
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
unit QtWSMenus;

{$mode delphi}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets, qtobjects,
  // LCL
  SysUtils, Classes, Menus, Forms, LCLType, LCLProc, Graphics,
  // Widgetset
  WSMenus, WSLCLClasses;

type

  { TQtWSMenuItem }

  TQtWSMenuItem = class(TWSMenuItem)
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
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
  end;

  { TQtWSMenu }

  TQtWSMenu = class(TWSMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TQtWSMainMenu }

  TQtWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TQtWSPopupMenu }

  TQtWSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{ TQtWSMenuItem }

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.AttachMenu
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
  // set proper position
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu Item
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  ParentMenu, Menu: TQtMenu;
  MenuBar: TQtMenuBar;
  Text: WideString;
  Method: TMethod;
begin
  {$ifdef VerboseQt}
    WriteLn('trace:> [TQtWSMenuItem.CreateHandle] Caption: ', AMenuItem.Caption,
     ' Subitems: ' + IntToStr(AMenuItem.Count));

    Write('trace:< [TQtWSMenuItem.CreateHandle]');
  {$endif}

  {------------------------------------------------------------------------------
    This case should not happen. A menu item must have a parent, but it seams LCL
   will sometimes create a menu item prior to creating it's parent.
    So, if we arrive here, we must create this item as if it was a TMenu
   ------------------------------------------------------------------------------}
  if (not AMenuItem.HasParent) then
  begin
    {$ifdef VerboseQt}
      Write(' Parent: Menu without parent');
    {$endif}

    Result := TQtWSMenu.CreateHandle(AMenuItem.GetParentMenu);
  end
  {------------------------------------------------------------------------------
    If the parent has no parent, then this item is directly owned by a TMenu
    In this case we have to detect if the parent is a TMainMenu or a TPopUpMenu
   because TMainMenu uses the special Handle QMenuBar while TPopUpMenu can be
   treat like if this menu item was a subitem of another item
   ------------------------------------------------------------------------------}
  else if ((not AMenuItem.Parent.HasParent) and (AMenuItem.GetParentMenu is TMainMenu)) then
  begin
    MenuBar := TQtMenuBar(AMenuItem.GetParentMenu.Handle);

    {$ifdef VerboseQt}
      Write(' Parent: ', dbghex(PtrInt(MenuBar)), ' The Parent is a TMainMenu');
    {$endif}

    { IsLine indicates that the menu item is a separator }
    if AMenuItem.IsLine then
    begin
      Menu := MenuBar.addSeparator;

      Menu.setHasSubmenu(False);

      Result := HMENU(Menu);
    end
    else
    begin
      Text := UTF8Decode(AMenuItem.Caption);

      Menu := MenuBar.addMenu(@Text);

      Menu.MenuItem := AMenuItem;

      if AMenuItem.HasIcon then
        Menu.setImage(TQtImage(AMenuItem.Bitmap.Handle));

      Menu.setHasSubmenu(AMenuItem.Count > 0);

      Result := HMENU(Menu);
    end;
  end
  {------------------------------------------------------------------------------
    If the parent has a parent, then that item´s Handle is necessarely a TQtMenu
   ------------------------------------------------------------------------------}
  else
  begin
    ParentMenu := TQtMenu(AMenuItem.Parent.Handle);
    
    ParentMenu.setHasSubmenu(True);

    {$ifdef VerboseQt}
      Write(' Parent: ', dbghex(PtrInt(ParentMenu)),
       ' The Parent is a TPopUpMenu or a TMenuItem');
    {$endif}

    { IsLine indicates that the menu item is a separator }
    if AMenuItem.IsLine then
    begin
      Menu := ParentMenu.addSeparator;

      Menu.setHasSubmenu(False);

      Result := HMENU(Menu);
    end
    { Count indicates the number of subitems this item has }
    else
    begin
      Text := UTF8Decode(AMenuItem.Caption);

      Menu := ParentMenu.addMenu(@Text);

      Menu.MenuItem := AMenuItem;

      Menu.setEnabled(AMenuItem.Enabled);

      Menu.setChecked(AMenuItem.Checked);

      if AMenuItem.HasIcon then
        Menu.setImage(TQtImage(AMenuItem.Bitmap.Handle));

      Menu.setHasSubmenu(AMenuItem.Count > 0);
      
      Result := HMENU(Menu);
    end;
  end;
  
  if Menu <> nil then
  begin
    // Trigger event

    QAction_triggered_Event(Method) := Menu.SlotTriggered;

    QAction_hook_hook_triggered(QAction_hook_create(Menu.ActionHandle), Method);
  end;

  {$ifdef VerboseQt}
    WriteLn(' Result: ', dbghex(Result));
  {$endif}
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.DestroyHandle
  Params:  None
  Returns: Nothing

  Dealocates a Menu Item
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  {$ifdef VerboseQt}
    Write('[TQtWSMenuItem.DestroyHandle] Caption: ' + AMenuItem.Caption);
    
    if AMenuItem.HasParent then Write(' HasParent ');
     
    WriteLn('');
  {$endif}
  
  { Apparently LCL tries to dealocate the handle of the menu item internal to TMenu,
   but it doesn´t create a handle for it. Instead it just put´s the handle of the TMenu
   on that item.
    We can detect this menu item checking if HasParent is false }
  if AMenuItem.HasParent then
  begin
    { Here the menu item has a QMenuH handle
      Obs: Commented because they cause access violations inside Qt
      library on the Virtual Magnifying Glass
    }
    TQtMenu(AMenuItem.Handle).Free;
  end;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetCaption
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin

end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut);
begin

end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSMenuItem.SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
begin
  { Here the menu item has a QMenuH handle }
  TQtMenu(AMenuItem.Handle).setVisible(Visible);
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetCheck
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  TQtMenu(AMenuItem.Handle).setChecked(Checked);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetEnable
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
begin
  TQtMenu(AMenuItem.Handle).setEnabled(Enabled);
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetRadioItem
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.SetRightJustify
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin

  Result := True;
end;

class procedure TQtWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if AMenuItem.HasParent then
  begin
    if HasIcon then
      TQtMenu(AMenuItem.Handle).setImage(TQtImage(AIcon.Handle))
    else
      TQtMenu(AMenuItem.Handle).setImage(nil);
  end;
end;

{ TQtWSMenu }

{------------------------------------------------------------------------------
  Function: TQtWSMenu.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu
 ------------------------------------------------------------------------------}
class function TQtWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  MenuBar: TQtMenuBar;
  Menu: TQtMenu;
  Parent: QWidgetH;
begin
  { If the menu is a main menu, there is no need to create a handle for it.
    It´s already created on the window }
  if (AMenu is TMainMenu) and (AMenu.Owner is TCustomForm) then
  begin
    MenuBar := TQtMainWindow(TCustomForm(AMenu.Owner).Handle).MenuBar;

    Result := HMENU(MenuBar);
  end
  else if (AMenu is TPopUpMenu) then
  begin
    Parent := TQtMainWindow(TCustomForm(AMenu.Owner).Handle).Widget;

    Menu := TQtMenu.Create(Parent);
  
    Result := HMENU(Menu);
  end;

  {$ifdef VerboseQt}
    Write('[TQtWSMenu.CreateHandle] ');

    if (AMenu is TMainMenu) then Write('IsMainMenu ');

    WriteLn(' Handle: ', dbghex(Result), ' Name: ', AMenu.Name);
  {$endif}
end;

{ TQtWSPopupMenu }

{------------------------------------------------------------------------------
  Function: TQtWSPopupMenu.Popup
  Params:  None
  Returns: Nothing

  Creates a PopUp menu
 ------------------------------------------------------------------------------}
class procedure TQtWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  Point: TQtPoint;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSPopupMenu.Popup] APopupMenu.Handle ' + dbghex(APopupMenu.Handle)
     + ' FirstItemName: ' + APopupMenu.Items.Name
     + ' FirstItemWND: ' + IntToStr(APopupMenu.Items.Handle)
     + ' FirstItemCount: ' + IntToStr(APopupMenu.Items.Count));
  {$endif}

  Point.X := X;
  Point.Y := Y;

  TQtMenu(APopupMenu.Handle).PopUp(@Point);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TQtWSMenuItem);
  RegisterWSComponent(TMenu, TQtWSMenu);
//  RegisterWSComponent(TMainMenu, TQtWSMainMenu);
  RegisterWSComponent(TPopupMenu, TQtWSPopupMenu);
////////////////////////////////////////////////////
end.
