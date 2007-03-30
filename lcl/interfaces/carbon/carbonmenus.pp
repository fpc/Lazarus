{ $Id: $
                  ----------------------------------------
                  carbonmenus.pp  -  Carbon internal menus
                  ----------------------------------------

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
unit CarbonMenus;

{$mode objfpc}{$H+}

interface

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math,
 // carbon bindings
  FPCMacOSAll, CarbonUtils, CarbonExtra,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,
 // LCL Carbon
  CarbonDef;
  
type

  { TCarbonMenu }

  TCarbonMenu = class
  private
    FAttached: Boolean;
    procedure MenuNeeded;
    function GetParentMenu: MenuRef;
  public
    LCLMenuItem: TMenuItem; // LCL menu item which created this widget
    Menu: MenuRef;          // Reference to the Carbon menu
  public
    constructor Create(const AMenuItem: TMenuItem; WithMenu: Boolean = False);
    destructor Destroy; override;

    procedure Add(AMenu: TCarbonMenu);
    procedure Attach(AParentMenu: TCarbonMenu);
    procedure AttachToMenuBar;
    
    procedure SetCaption(const ACaption: String);
    procedure SetVisible(AVisible: Boolean);
    procedure SetEnable(AEnabled: Boolean);
  end;

implementation

uses
  CarbonProc;

{ TCarbonMenu }

procedure TCarbonMenu.MenuNeeded;
begin
  if Menu <> nil then Exit;
  
  if CreateNewMenu(0, 0, Menu) = noErr then
  begin
    if FAttached then
    begin
      SetMenuItemHierarchicalMenu(GetParentMenu, LCLMenuItem.MenuIndex + 1, Menu);
      SetCaption(LCLMenuItem.Caption);
    end;
      
    // install event handlers
  end
  else
    raise Exception.CreateFmt('Unable to create Carbon menu for %s: %s!',
        [LCLMenuItem.Name, LCLMenuItem.ClassName]);
end;

function TCarbonMenu.GetParentMenu: MenuRef;
begin
  if LCLMenuItem.Parent <> nil then
    Result := AsMenuRef(LCLMenuItem.Parent.Handle)
  else
    Result := nil;
end;

constructor TCarbonMenu.Create(const AMenuItem: TMenuItem; WithMenu: Boolean);
begin
  inherited Create;
  
  FAttached := False;
  LCLMenuItem := AMenuItem;
  Menu := nil;
  
  if WithMenu then MenuNeeded;
end;

destructor TCarbonMenu.Destroy;
begin
  if Menu <> nil then
  begin
    DisposeMenu(Menu);
    Menu := nil;
  end;
  
  if FAttached then
  begin // remove menu from parent
    DeleteMenuItem(GetParentMenu, LCLMenuItem.MenuIndex + 1);
  end;
  
  inherited Destroy;
end;

procedure TCarbonMenu.Add(AMenu: TCarbonMenu);
begin
  if AMenu = nil then Exit;
  MenuNeeded;
  
  AMenu.Attach(Self);
end;

procedure TCarbonMenu.Attach(AParentMenu: TCarbonMenu);
var
  CFString: CFStringRef;
begin
  DebugLn('TCarbonMenu.Attach ' + LCLMenuItem.Name + ' Menu: ' + DbgS(Menu));

  if LCLMenuItem.Caption = cLineCaption then // menu item is separator
    InsertMenuItemTextWithCFString(AParentMenu.Menu, nil,
      LCLMenuItem.MenuIndex, kMenuItemAttrSeparator, 0)
  else
  begin
    CreateCFString(LCLMenuItem.Caption, CFString);
    try
      InsertMenuItemTextWithCFString(AParentMenu.Menu, CFString,
        LCLMenuItem.MenuIndex, kMenuItemAttrIgnoreMeta, 0);

      if Menu <> nil then SetMenuTitleWithCFString(Menu, CFString);
    finally
      FreeCFString(CFString);
    end;
  end;
  
  // add sub menu if exists
  if Menu <> nil then
    SetMenuItemHierarchicalMenu(AParentMenu.Menu, LCLMenuItem.MenuIndex + 1, Menu);

  SetVisible(LCLMenuItem.Visible);
  SetEnable(LCLMenuItem.Enabled);
  
  FAttached := True;
end;

procedure TCarbonMenu.AttachToMenuBar;
var
  I: Integer;
begin
  for I := 0 to LCLMenuItem.Count - 1 do
  begin
    if LCLMenuItem.Items[I].HandleAllocated then
      TCarbonMenu(LCLMenuItem.Items[I].Handle).MenuNeeded;
  end;

  SetRootMenu(Menu);
end;

procedure TCarbonMenu.SetCaption(const ACaption: String);
var
  CFString: CFStringRef;
begin
  if LCLMenuItem.Caption = cLineCaption then // menu item is separator
    ChangeMenuItemAttributes(GetParentMenu,
      LCLMenuItem.MenuIndex + 1, kMenuItemAttrSeparator, 0)
  else
  begin
    ChangeMenuItemAttributes(GetParentMenu,
      LCLMenuItem.MenuIndex + 1, 0, kMenuItemAttrSeparator);
    
    CreateCFString(LCLMenuItem.Caption, CFString);
    try
      SetMenuItemTextWithCFString(GetParentMenu, LCLMenuItem.MenuIndex + 1,
        CFString);

      if Menu <> nil then SetMenuTitleWithCFString(Menu, CFString);
    finally
      FreeCFString(CFString);
    end;
  end;
end;

procedure TCarbonMenu.SetVisible(AVisible: Boolean);
begin
  if AVisible then
  begin
    ChangeMenuItemAttributes(GetParentMenu,
      LCLMenuItem.MenuIndex + 1, 0, kMenuItemAttrHidden);

    if Menu <> nil then
      ChangeMenuAttributes(Menu, 0, kMenuAttrHidden);
  end
  else
  begin
    ChangeMenuItemAttributes(GetParentMenu,
      LCLMenuItem.MenuIndex + 1, kMenuItemAttrHidden, 0);

    if Menu <> nil then
      ChangeMenuAttributes(Menu, kMenuAttrHidden, 0);
  end;
end;

procedure TCarbonMenu.SetEnable(AEnabled: Boolean);
begin
  if AEnabled then
  begin
    ChangeMenuItemAttributes(GetParentMenu,
      LCLMenuItem.MenuIndex + 1, 0, kMenuItemAttrDisabled)
  end
  else
  begin
    ChangeMenuItemAttributes(GetParentMenu,
      LCLMenuItem.MenuIndex + 1, kMenuItemAttrDisabled, 0);
  end;
end;

end.

