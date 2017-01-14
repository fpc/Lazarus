{
 *****************************************************************************
 *                               MuiWSMenus.pp                                *
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
unit MuiWSMenus;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  Mui, utility, MuiFormsUnit, tagsparamshelper,
  // LCL
  SysUtils, Classes, Types, LCLType, LCLProc, Graphics, Controls, Forms, Menus,
  // Widgetset
  WSMenus, WSLCLClasses;

type

  { TQtWSMenuItem }

  TMuiWSMenuItem = class(TWSMenuItem)
  protected
    //class function CreateMenuFromMenuItem(const AMenuItem: TMenuItem): TMuiMenu;
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
      override;
    class procedure SetShortCut(const AMenuItem: TMenuItem;
      const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
      override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
      override;
    class function SetEnable(const AMenuItem: TMenuItem;
      const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem;
      const RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem;
      const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem;
      const HasIcon: boolean; const AIcon: TBitmap); override;
  end;

  { TMuiWSMenu }

  TMuiWSMenu = class(TWSMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure SetBiDiMode(const AMenu: TMenu;
      UseRightToLeftAlign, UseRightToLeftReading: boolean); override;
  end;

  { TMuiWSMainMenu }

  TMuiWSMainMenu = class(TWSMainMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TMuiWSPopupMenu }

  TMuiWSPopupMenu = class(TWSPopupMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{ TMuiWSMainMenu }

class function TMuiWSMainMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  Menu: Pointer;
begin
  //writeln('--> createHandle MAINMENU');
  Menu := NIL;
  If (AMenu.Parent <> nil) and (AMenu.Parent is TCustomForm) then
  begin
    Menu := TMuiWindow(TCustomForm(AMenu.Parent).Handle).MainMenu;
    TMuiWindow(TCustomForm(AMenu.Parent).Handle).HasMenu := True;
  end;
  Result := HMENU(Menu);
end;


{ TMuiWSMenuItem }

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.AttachMenu
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);

begin
  //writeln('TMuiWidgetSet.AttachMenu START ',AMenuItem.Name,':',AMenuItem.ClassName,' Parent=',AMenuItem.Parent.Name,':',AMenuItem.Parent.ClassName);
  //writeln('--> attachmenu ', HexStr(Pointer(AMenuItem.Handle)));
  if (AMenuItem.Handle <= 1) or (AMenuItem.Parent.Handle <= 1) then
    Exit;
  TMuiFamily(AMenuItem.Parent.Handle).AddTail(TMuiFamily(AMenuItem.Handle));
  TMuiFamily(AMenuItem.Handle).Par := TMuiFamily(AMenuItem.Parent.Handle);
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu Item
 ------------------------------------------------------------------------------}
class function TMuiWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  Menu: TMuiMenuItem;
  Menu1: TMuiMenu;
  Tags: TATagList;
begin
  //write('->Create MenuItem..', AMenuItem.MenuIndex,' ', AMenuItem.Caption, ' ', AMenuItem.Count);
  case AMenuItem.MenuIndex of
    -1:begin
      Result := HMENU(AMenuItem.GetParentMenu.Handle);
    end;
    else
    begin
      if AMenuItem.Count > 0 then
      begin
        //write('..as menu');
        Tags.Clear;
        Menu1 := TMuiMenu.Create(Tags);
        Menu1.Title := AMenuItem.Caption;
        Menu1.PasObject := TControl(TObject(AMenuItem));
        Result := HMENU(Menu1);
      end else
      begin
        //write('..as menuitem');
        Tags.Clear;
        Menu := TMuiMenuItem.Create(Tags);
        Menu.Title := AMenuItem.Caption;
        Menu.CheckIt := AMenuItem.ShowAlwaysCheckable or AMenuItem.RadioItem;
        Menu.Checked := AMenuItem.Checked;
        Menu.Enabled := AMenuItem.Enabled;
        Menu.PasObject := TControl(TObject(AMenuItem));
        Result := HMENU(Menu);
      end;
    end;
  end;
  //writeln('..done');
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.DestroyHandle
  Params:  None
  Returns: Nothing

  Dealocates a Menu Item
 ------------------------------------------------------------------------------}
class procedure TMuiWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
var
  MuiMenu: TMuiFamily;
begin
  MuiMenu:= TMuiFamily(AMenuItem.Handle);
  if AMenuItem.HasParent then
  begin
    if (AMenuItem.Handle <= 1) or (AMenuItem.Parent.Handle <= 1) then
      Exit;
    TMuiFamily(AMenuItem.Parent.Handle).Remove(MuiMenu);
    MuiMenu.Free;
  end;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetCaption
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSMenuItem.SetCaption(const AMenuItem: TMenuItem;
  const ACaption: string);
var
  MuiMenu: TMuiFamily;
begin
  //WriteLn('[TMuiWSMenuItem.SetCaption] Caption: ' + AMenuItem.Caption +
  //  ' NewCaption: ', ACaption);

  MuiMenu := TMuiFamily(AMenuItem.Handle);
  if MuiMenu is TMuiMenuItem then
  begin
    TMuiMenuItem(MuiMenu).Title := ACaption;
  end;
  if MuiMenu is TMuiMenu then
  begin
    TMuiMenu(MuiMenu).Title := ACaption;
  end;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetShortCut
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const ShortCutK1, ShortCutK2: TShortCut);
var
  s: string;
  MuiMenu: TMuiFamily;
begin
  s := ShortCutToText(ShortCutK1);
  //writeln('shortcut: ', s);
  MuiMenu := TMuiFamily(AMenuItem.Handle);
  if MuiMenu is TMuiMenuItem then
  begin
    TMuiMenuItem(MuiMenu).ShortCut := s;
  end;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetVisible
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
var
  MuiMenu: TMuiFamily;
begin
  //WriteLn('[TMuiWSMenuItem.SetVisible] SetVisible: ' + AMenuItem.Caption +
  //  ' Visible: ', Visible);
  MuiMenu := TMuiFamily(AMenuItem.Handle);
  if MuiMenu is TMuiMenuItem then
    TMuiMenuItem(MuiMenu).Visible := Visible;
  if MuiMenu is TMuiMenu then
    TMuiMenu(MuiMenu).Visible := Visible;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetCheck
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TMuiWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
var
  MuiMenu: TMuiFamily;
begin
  //WriteLn('[TMuiWSMenuItem.SetCheck] ' + AMenuItem.Caption +
  //  ' Checked: ', Checked);
  MuiMenu := TMuiFamily(AMenuItem.Handle);
  if MuiMenu is TMuiMenuItem then
  begin
    TMuiMenuItem(MuiMenu).Checkit := True;
    TMuiMenuItem(MuiMenu).Checked := Checked;
  end;
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetEnable
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TMuiWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
var
  MuiMenu: TMuiFamily;
begin
  //WriteLn('[TMuiWSMenuItem.SetEnabled] ' + AMenuItem.Caption +
  //  ' Enabled: ', Enabled);
  MuiMenu := TMuiFamily(AMenuItem.Handle);
  if MuiMenu is TMuiMenuItem then
    TMuiMenuItem(MuiMenu).Enabled := Enabled;
  if MuiMenu is TMuiMenu then
    TMuiMenu(MuiMenu).Enabled := Enabled;
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetRadioItem
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TMuiWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
var
  MuiMenu: TMuiFamily;
begin
  //WriteLn('[TMuiWSMenuItem.SetRadio] ' + AMenuItem.Caption +
  //  ' Radio: ', RadioItem);
  MuiMenu := TMuiFamily(AMenuItem.Handle);
  if MuiMenu is TMuiMenuItem then
    TMuiMenuItem(MuiMenu).CheckIt := RadioItem;
  Result := True;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSMenuItem.SetRightJustify
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TMuiWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem;
  const Justified: boolean): boolean;
begin
  Result := True;
end;

class procedure TMuiWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: boolean; const AIcon: TBitmap);
begin

end;

{ TMuiWSMenu }

{------------------------------------------------------------------------------
  Function: TMuiWSMenu.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Menu
 ------------------------------------------------------------------------------}
class function TMuiWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  //writeln('_____>>>>Create Menu');
  REsult := HMENU(1);
end;

class procedure TMuiWSMenu.SetBiDiMode(const AMenu: TMenu;
  UseRightToLeftAlign, UseRightToLeftReading: boolean);
begin
end;


{ TMuiWSPopupMenu }

class function TMUIWSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  //writeln('create Popupmenu ', HexStr(Amenu.Parent));
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSPopupMenu.Popup
  Params:  None
  Returns: Nothing

  Creates a PopUp menu
 ------------------------------------------------------------------------------}
class procedure TMuiWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
  //writeln(' open PopupMenu');
  //
end;

end.

