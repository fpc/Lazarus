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

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll, CarbonUtils,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,
 // LCL Carbon
  CarbonDef, CarbonGDIObjects;
  
type

  { TCarbonMenu }

  TCarbonMenu = class
  private
    FParentMenu: TCarbonMenu;
    FRoot: Boolean;
    FItems: TObjectList; // of TCarbonMenu
    procedure MenuNeeded;
    function GetIndex: Integer;
  protected
    procedure RegisterEvents; virtual;
    procedure UnregisterEvents; virtual;
    procedure Update;
  public
    LCLMenuItem: TMenuItem; // LCL menu item which created this widget
    Menu: MenuRef;          // Reference to the Carbon menu
  public
    constructor Create(const AMenuItem: TMenuItem; WithMenu: Boolean = False);
    destructor Destroy; override;

    procedure Add(AMenu: TCarbonMenu);
    procedure Remove(AMenu: TCarbonMenu);
    procedure Attach(AParentMenu: TCarbonMenu);
    procedure AttachToMenuBar;
    
    procedure SetCaption(const ACaption: String);
    procedure SetVisible(AVisible: Boolean);
    procedure SetEnable(AEnabled: Boolean);
    procedure SetBitmap(const ABitmap: TBitmap);
    procedure SetCheck(AChecked: Boolean);
    procedure SetShortCut(AShortCut: TShortCut);
    procedure SetStyle;
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
    if FParentMenu <> nil then
    begin
      SetCaption(LCLMenuItem.Caption);

      Update;
    end;

    if FItems = nil then FItems := TObjectList.Create(False);
    // install event handlers
  end
  else
    raise Exception.CreateFmt('Unable to create Carbon menu for %s: %s!',
        [LCLMenuItem.Name, LCLMenuItem.ClassName]);
end;

function TCarbonMenu.GetIndex: Integer;
begin
  if FParentMenu <> nil then Result := FParentMenu.FItems.IndexOf(Self)
  else
  begin
    Result := -1;
    DebugLn('TCarbonMenu.GetIndex Error - menu item ' + LCLMenuItem.Name +
      ' is not attached to parent!');
  end;
end;

procedure TCarbonMenu.RegisterEvents;
begin
  //
end;

procedure TCarbonMenu.UnregisterEvents;
begin
  //
end;

procedure TCarbonMenu.Update;
begin
  if FParentMenu = nil then Exit;

  // add sub menu if exists
  if Menu <> nil then
    SetMenuItemHierarchicalMenu(FParentMenu.Menu, GetIndex + 1, Menu);

  SetCheck(LCLMenuItem.Checked);
  SetBitmap(LCLMenuItem.Bitmap);
  SetStyle;

  SetVisible(LCLMenuItem.Visible);
  SetEnable(LCLMenuItem.Enabled);
end;

constructor TCarbonMenu.Create(const AMenuItem: TMenuItem; WithMenu: Boolean);
begin
  inherited Create;
  
  FRoot := False;
  LCLMenuItem := AMenuItem;
  Menu := nil;
  FParentMenu := nil;
  FItems := nil;
  
  if WithMenu then MenuNeeded;
end;

destructor TCarbonMenu.Destroy;
begin
  if Menu <> nil then
  begin
    DisposeMenu(Menu);
    Menu := nil;
  end;
  
  if FParentMenu <> nil then
  begin // remove menu from parent
    FParentMenu.Remove(Self);
    FItems.Free;
  end;
  
  inherited Destroy;
end;

procedure TCarbonMenu.Add(AMenu: TCarbonMenu);
var
  I: Integer;
begin
  if AMenu = nil then Exit;
  MenuNeeded;
  
  // find where to insert the menu item, beacause hidden items are not added!
  I := FItems.Count;
  while I > 0 do
  begin
    if (FItems[I - 1] as TCarbonMenu).LCLMenuItem.MenuIndex <
      AMenu.LCLMenuItem.MenuIndex then Break;
    Dec(I);
  end;
  FItems.Insert(I, AMenu);
  
  AMenu.Attach(Self);
end;

procedure TCarbonMenu.Remove(AMenu: TCarbonMenu);
var
  I: Integer;
begin
  I := FItems.IndexOf(AMenu);
  if I < 0 then Exit;
  
  DeleteMenuItem(Menu, I + 1);
  FItems.Delete(I);
end;

procedure TCarbonMenu.Attach(AParentMenu: TCarbonMenu);
var
  CFString: CFStringRef;
  Index: Integer;
begin
  FParentMenu := AParentMenu;

  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.Attach ' + LCLMenuItem.Name + ' Index: ' + DbgS(GetIndex) +
      ' Menu: ' + DbgS(Menu));
  {$ENDIF}
  
  Index := GetIndex;
  if FParentMenu.FRoot then MenuNeeded; // menu tiem is in toplevel of root menu
  
  if LCLMenuItem.Caption = cLineCaption then // menu item is separator
    InsertMenuItemTextWithCFString(FParentMenu.Menu, nil, Index,
      kMenuItemAttrSeparator, 0)
  else
  begin
    CreateCFString(LCLMenuItem.Caption, CFString);
    try
      InsertMenuItemTextWithCFString(FParentMenu.Menu, CFString, Index,
        kMenuItemAttrIgnoreMeta, 0);

      if Menu <> nil then SetMenuTitleWithCFString(Menu, CFString);
    finally
      FreeCFString(CFString);
    end;
  end;
  // Note: menu item indexing in Carbon is, except inserting, one-based!
  
  // set Command ID for catching click events
  SetMenuItemCommandID(FParentMenu.Menu, Index + 1, MENU_FOURCC);
  
  // set property for identification
  SetMenuItemProperty(FParentMenu.Menu, Index + 1,
    LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(TCarbonMenu), @Self);
  
  Update;
end;

procedure TCarbonMenu.AttachToMenuBar;
var
  I: Integer;
begin
  // create Carbon menu objects for toplevel items
  for I := 0 to FItems.Count - 1 do TCarbonMenu(FItems[I]).MenuNeeded;

  FRoot := True;
  
  SetRootMenu(Menu);
end;

procedure TCarbonMenu.SetCaption(const ACaption: String);
var
  CFString: CFStringRef;
  Index: Integer;
  S: String;
begin
  if FParentMenu = nil then Exit;
  
  if LCLMenuItem.Caption = cLineCaption then // menu item is separator
    ChangeMenuItemAttributes(FParentMenu.Menu,
      GetIndex + 1, kMenuItemAttrSeparator, 0)
  else
  begin
    Index := GetIndex;
    ChangeMenuItemAttributes(FParentMenu.Menu, Index + 1, 0, kMenuItemAttrSeparator);
    
    S := LCLMenuItem.Caption;
    DeleteAmpersands(S);
    
    CreateCFString(S, CFString);
    try
      SetMenuItemTextWithCFString(FParentMenu.Menu, Index + 1, CFString);

      if Menu <> nil then SetMenuTitleWithCFString(Menu, CFString);
    finally
      FreeCFString(CFString);
    end;
  end;
end;

procedure TCarbonMenu.SetVisible(AVisible: Boolean);
begin
  if FParentMenu = nil then Exit;
  
  if AVisible then
  begin
    ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1, 0,
      kMenuItemAttrHidden);

    if Menu <> nil then
      ChangeMenuAttributes(Menu, 0, kMenuAttrHidden);
  end
  else
  begin
    ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1,
      kMenuItemAttrHidden, 0);

    if Menu <> nil then
      ChangeMenuAttributes(Menu, kMenuAttrHidden, 0);
  end;
end;

procedure TCarbonMenu.SetEnable(AEnabled: Boolean);
begin
  if FParentMenu = nil then Exit;
  
  if AEnabled then
  begin
    ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1, 0,
      kMenuItemAttrDisabled)
  end
  else
  begin
    ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1,
      kMenuItemAttrDisabled, 0);
  end;
end;

procedure TCarbonMenu.SetBitmap(const ABitmap: TBitmap);
var
  IconType: Byte;
  AIcon: CGImageRef;
  AHandle: FPCMacOSAll.Handle;
const SName = 'TCarbonMenu.SetBitmap';
begin
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.SetBitmap ' + LCLMenuItem.Name + ' ABitmap: ' + DbgS(ABitmap)
      + ' ParentMenu: ' + DbgS(FParentMenu.Menu));
  {$ENDIF}
  if FParentMenu = nil then Exit;

  AIcon := nil;
  
  if ABitmap <> nil then
  begin
    if not CheckBitmap(ABitmap.Handle, SName) then Exit;
    IconType := kMenuCGImageRefType;
    AIcon := TCarbonBitmap(ABitmap.Handle).CGImage;
  end;
  
  AIcon := nil;
  if AIcon = nil then
  begin
    IconType := kMenuNoIcon;
    AHandle := nil;
  end
  else AHandle := @AIcon;
  
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.SetBitmap IconType: ' + DbgS(IconType) + ' AIcon: ' + DbgS(AIcon));
  {$ENDIF}
  
  if OSError(
    SetMenuItemIconHandle(FParentMenu.Menu, GetIndex + 1, IconType, AHandle),
    SName, 'SetMenuItemIconHandle') then Exit;
      
  if Menu <> nil then
    OSError(SetMenuTitleIcon(Menu, IconType, AIcon), SName, 'SetMenuTitleIcon');
end;

procedure TCarbonMenu.SetCheck(AChecked: Boolean);
var
  I: Integer;
  Item: TCarbonMenu;
begin
  DebugLn('TCarbonMenu.SetShortCut ' + LCLMenuItem.Caption + ' ' + DbgS(AChecked));
  if FParentMenu = nil then Exit;
  
  if AChecked then
  begin
    if LCLMenuItem.RadioItem then
    begin
      SetItemMark(FParentMenu.Menu, GetIndex + 1, Char(kBulletCharCode)); // or kDiamondCharCode
      
      // uncheck siblings
      for I := 0 to FParentMenu.FItems.Count - 1 do
      begin
        Item := TCarbonMenu(FParentMenu.FItems[I]);
        if Item = Self then Continue;
        
        if Item.LCLMenuItem.RadioItem and Item.LCLMenuItem.AutoCheck and
          (Item.LCLMenuItem.GroupIndex = LCLMenuItem.GroupIndex) then
            Item.SetCheck(False);
      end;
    end
    else
      SetItemMark(FParentMenu.Menu, GetIndex + 1, Char(kCheckCharCode));
  end
  else
    if (FItems = nil) or (FItems.Count = 0) then
      SetItemMark(FParentMenu.Menu, GetIndex + 1, #0);
end;

procedure TCarbonMenu.SetShortCut(AShortCut: TShortCut);
var
  Shift: TShiftState;
  Key: Word;
  Index: Integer;
const SName = 'SetShortCut';
begin
  DebugLn('TCarbonMenu.SetShortCut ' + ShortCutToText(AShortCut));
  if FParentMenu = nil then Exit;
  
  ShortCutToKey(AShortCut, Key, Shift);

  Index := GetIndex;
  if OSError(SetMenuItemModifiers(FParentMenu.Menu, Index + 1,
      ShiftStateToModifiers(Shift)),
    Self, SName, 'SetMenuItemModifiers') then Exit;

  OSError(SetMenuItemCommandKey(FParentMenu.Menu, Index + 1, False, Key),
    Self, SName, 'SetMenuItemCommandKey');
end;

procedure TCarbonMenu.SetStyle;
var
  Style: StyleParameter;
begin
  if FParentMenu = nil then Exit;
  
  if LCLMenuItem.Default then Style := FPCMAcOSAll.bold
  else Style := FPCMAcOSAll.normal;

  SetItemStyle(FParentMenu.Menu, GetIndex + 1, Style);
end;


end.

