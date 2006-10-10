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
  qt4, qtwidgets, qtobjects,
  // LCL
  SysUtils, Classes, Menus, Forms, LCLType,
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

class procedure TQtWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin

end;

{------------------------------------------------------------------------------
  Function: TQtWSMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu Item
 ------------------------------------------------------------------------------}
class function TQtWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Action: TQtAction;
  ParentMenu, Menu: TQtMenu;
  MenuBar: TQtMenuBar;
  Text: WideString;
  Method: TMethod;
  ActionHandle: Boolean = False;
begin
  {$ifdef VerboseQt}
    Write('[TQtWSMenuItem.CreateHandle] Caption: ' + AMenuItem.Caption);

    WriteLn(' Subitems: ' + IntToStr(AMenuItem.Count));
  {$endif}

  { This case should not occur. Menu items without a parent don´t need a handle,
   because they won´t be shown }
  if (not AMenuItem.HasParent) then
  begin
    Menu := TQtMenu.Create;

    Result := HMENU(Menu);
  end
  { If the parent has no parent, then this item is directly owned by a TMenu
    In this case we have to detect if the parent is a TMainMenu or a TPopUpMenu
   because TMainMenu uses the special Handle QMenuBar while TPopUpMenu can be
   treat like if this menu item was a subitem of another item }
  else if ((not AMenuItem.Parent.HasParent) and (AMenuItem.GetParentMenu is TMainMenu)) then
  begin
    MenuBar := TQtMenuBar(AMenuItem.GetParentMenu.Handle);
  
    { IsLine indicates that the menu item is a separator }
    if AMenuItem.IsLine then
    begin
      ActionHandle := True;
    
      Action := MenuBar.addSeparator;
      
      Result := HMENU(Action);
    end
    { Count indicates the number of subitems this item has }
    else if AMenuItem.Count > 0 then
    begin
      Text := WideString(AMenuItem.Caption);

      Menu := MenuBar.addMenu(@Text);

      Result := HMENU(Menu);
    end
    else
    begin
      ActionHandle := True;

      Text := WideString(AMenuItem.Caption);

      Action := MenuBar.addAction(@Text);
      
      Action.MenuItem := AMenuItem;

      Result := HMENU(Action);
    end;
  end
  { If the parent has a parent, then that item´s Handle is necessarely a QMenuH }
  else
  begin
    if ((not AMenuItem.Parent.HasParent) and (AMenuItem.GetParentMenu is TPopUpMenu)) then
     ParentMenu := TQtMenu(AMenuItem.GetParentMenu.Handle)
    else ParentMenu := TQtMenu(AMenuItem.Parent.Handle);

    { IsLine indicates that the menu item is a separator }
    if AMenuItem.IsLine then
    begin
      ActionHandle := True;
      
      Action := ParentMenu.addSeparator;

      Result := HMENU(Action);
    end
    { Count indicates the number of subitems this item has }
    else if AMenuItem.Count > 0 then
    begin
      Text := WideString(AMenuItem.Caption);

      Menu := ParentMenu.addMenu(@Text);

      Result := HMENU(Menu);
    end
    else
    begin
      ActionHandle := True;
    
      Text := WideString(AMenuItem.Caption);

      Action := ParentMenu.addAction(@Text);

      Action.MenuItem := AMenuItem;

      Result := HMENU(Action);
    end;
  end;
  
  if ActionHandle then
  begin
    // Trigger event

    QAction_triggered_Event(Method) := Action.SlotTriggered;

    QAction_hook_hook_triggered(QAction_hook_create(Action.Handle), Method);
  end;
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
    { Here the menu item has a QMenuH handle }
    if AMenuItem.Count > 0 then
    begin
    end
    { Here the menu item has a QActionH handle }
    else
    begin
    end;
  end;
end;

class procedure TQtWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin

end;

class procedure TQtWSMenuItem.SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut);
begin

end;

class procedure TQtWSMenuItem.SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
begin
  { Here the menu item has a QMenuH handle }
  if AMenuItem.Count > 0 then
  begin
    TQtMenu(AMenuItem.Handle).setVisible(Visible);
  end
  { Here the menu item has a QActionH handle }
  else
  begin
    TQtAction(AMenuItem.Handle).setVisible(Visible);
  end;
end;

class function TQtWSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  { Here the menu item has a QMenuH handle }
  if AMenuItem.Count > 0 then
  begin

  end
  { Here the menu item has a QActionH handle }
  else
  begin
    TQtAction(AMenuItem.Handle).setChecked(Checked);
  end;

  Result := True;
end;

class function TQtWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
begin
  { Here the menu item has a QMenuH handle }
  if AMenuItem.Count > 0 then
  begin
    TQtMenu(AMenuItem.Handle).setEnabled(Enabled);
  end
  { Here the menu item has a QActionH handle }
  else
  begin
    TQtAction(AMenuItem.Handle).setEnabled(Enabled);
  end;

  Result := True;
end;

class function TQtWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean;
begin
  Result := True;
end;

class function TQtWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin

  Result := True;
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
  {$ifdef VerboseQt}
    Write('[TQtWSMenu.CreateHandle] ');
    
    if (AMenu is TMainMenu) then Write('IsMainMenu ');

    WriteLn(' Name: ' + AMenu.Name);
  {$endif}

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
  Point: TPoint;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSPopupMenu.Popup] APopupMenu.Handle ' + IntToStr(APopupMenu.Handle)
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
