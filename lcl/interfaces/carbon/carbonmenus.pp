{ $Id: $
                  ----------------------------------------
                  carbonmenus.pp  -  Carbon internal menus
                  ----------------------------------------

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
unit CarbonMenus;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  MacOSAll,
  CarbonUtils,
 // widgetset
  WSControls, WSLCLClasses, WSProc,
 // LCL
  LMessages, LCLMessageGlue, LCLProc, LCLType, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,
 // LCL Carbon
  CarbonDef, CarbonGDIObjects, CarbonProc, CarbonDbgConsts;
  
type

  { TCarbonMenu }

  TCarbonMenu = class
  private
    FParentMenu: TCarbonMenu;
    FRoot: Boolean;
    FItems: TObjectList; // of TCarbonMenu
    fDismissed: Integer;
    procedure MenuNeeded(UpdateWithParent: Boolean=True);
    function GetIndex: Integer;
  protected
    procedure RegisterEvents;
    procedure Opening;
    procedure Closed;
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

    function GetShortCutKey: AnsiChar;
    procedure Update;

    property Parent: TCarbonMenu read FParentMenu;
    property Dismissed: Integer read fDismissed write fDismissed;
  end;
  
function CheckMenu(const Menu: HMENU; const AMethodName: String; AParamName: String = ''): Boolean;

implementation

{------------------------------------------------------------------------------
  Name:    CheckMenu
  Params:  Menu        - Handle of menu
           AMethodName - Method name
           AParamName  - Param name
  Returns: If the menu is valid
 ------------------------------------------------------------------------------}
function CheckMenu(const Menu: HMENU; const AMethodName: String;
  AParamName: String): Boolean;
begin
  if TObject(Menu) is TCarbonMenu then Result := True
  else
  begin
    Result := False;
        
    if Pos('.', AMethodName) = 0 then
      DebugLn(SCarbonWSPrefix + AMethodName + ' Error - invalid menu ' +
        AParamName + ' = ' + DbgS(Menu) + '!')
    else
      DebugLn(AMethodName + ' Error - invalid menu ' + AParamName + ' = ' +
        DbgS(Menu) + '!');
  end;
end;

{ TCarbonMenu }

{------------------------------------------------------------------------------
  Name:  CarbonMenu_Opening
  Handles menu opening
 ------------------------------------------------------------------------------}
function CarbonMenu_Opening(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TObject): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseMenu}
    DebugLn('CarbonMenu_Opening');
  {$ENDIF}
  
  if AWidget is TCarbonMenu then
    (AWidget as TCarbonMenu).Opening;

  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

{------------------------------------------------------------------------------
  Name:  CarbonMenu_Closed
  Handles menu closed
 ------------------------------------------------------------------------------}
function CarbonMenu_Closed(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TObject): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
begin
  {$IFDEF VerboseMenu}
    DebugLn('CarbonMenu_Closed');
  {$ENDIF}

  if AWidget is TCarbonMenu then
    (AWidget as TCarbonMenu).Closed;

  Result := CallNextEventHandler(ANextHandler, AEvent);
end;

{------------------------------------------------------------------------------
  Name:  CarbonMenu_EndTracking
  Handles menu end tracking
 ------------------------------------------------------------------------------}
function CarbonMenu_EndTracking(ANextHandler: EventHandlerCallRef;
  AEvent: EventRef;
  AWidget: TObject): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}
var
  dis : Integer;
begin
  {$IFDEF VerboseMenu}
    DebugLn('CarbonMenu_EndTracking');
  {$ENDIF}

  if AWidget is TCarbonMenu then
  begin
    if GetEventParameter(AEvent, kEventParamMenuDismissed, typeUInt32, nil, sizeof(dis), nil, @dis)<>noErr then
      dis:=0;
    (AWidget as TCarbonMenu).Dismissed:=dis;
  end;

  Result := CallNextEventHandler(ANextHandler, AEvent);
end;


{------------------------------------------------------------------------------
  Method:  TCarbonMenu.MenuNeeded

  Creates Carbon menu object for sub menu
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.MenuNeeded(UpdateWithParent: Boolean);
begin
  if Menu <> nil then Exit;
  
  if OSError(CreateNewMenu(0, kMenuAttrAutoDisable, Menu),
    Self, 'MenuNeeded', 'CreateNewMenu') then
  begin
    raise Exception.CreateFmt('Unable to create Carbon menu for %s: %s!',
      [LCLMenuItem.Name, LCLMenuItem.ClassName]);
  end;
  
  RegisterEvents;
  
  if (FParentMenu <> nil) and UpdateWithParent then
  begin
    SetCaption(LCLMenuItem.Caption);

    Update;
  end;

  if FItems = nil then FItems := TObjectList.Create(False);
  
  // install event handlers here
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.GetIndex
  Returns: The real index of Carbon menu item
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Update

  Updates Carbon menu item properties (caption, checked, visible, submenu ...)
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.Update;
begin
  if FParentMenu = nil then Exit;

  // add sub menu if exists
  if Menu <> nil then
    OSError(SetMenuItemHierarchicalMenu(FParentMenu.Menu, GetIndex + 1, Menu),
      Self, 'Update', 'SetMenuItemHierarchicalMenu');

  SetCheck(LCLMenuItem.Checked);
  if (LCLMenuItem.HasIcon) then
    SetBitmap(LCLMenuItem.Bitmap)
  else
    SetBitmap(nil);
  SetStyle;

  SetVisible(LCLMenuItem.Visible);
  SetEnable(LCLMenuItem.Enabled);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.RegisterEvents

  Register menu events
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.RegisterEvents;
var
  TmpSpec: EventTypeSpec;
begin
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.RegisterEvents ' + LCLMenuItem.Name);
  {$ENDIF}
  
  TmpSpec := MakeEventSpec(kEventClassMenu, kEventMenuOpening);
  InstallMenuEventHandler(Menu, RegisterObjectEventHandler(@CarbonMenu_Opening),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassMenu, kEventMenuClosed);
  InstallMenuEventHandler(Menu, RegisterObjectEventHandler(@CarbonMenu_Closed),
    1, @TmpSpec, Pointer(Self), nil);

  TmpSpec := MakeEventSpec(kEventClassMenu, kEventMenuEndTracking);
  InstallMenuEventHandler(Menu, RegisterObjectEventHandler(@CarbonMenu_EndTracking),
    1, @TmpSpec, Pointer(Self), nil);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Opening
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.Opening;
var
  Msg: TLMessage;
begin
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.Opening ' + LCLMenuItem.Name);
  {$ENDIF}
  
  // menu item has sub menu - call click when opening
  if LCLMenuItem.IsInMenuBar or (LCLMenuItem.Count > 0) then
  begin
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.msg := LM_ACTIVATE;
    LCLMenuItem.Dispatch(Msg);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Closed
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.Closed;
begin
  //DebugLn('TCarbonMenu.Closed');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Create
  Params:  AMenuItem - LCL Menu item to create
           WithMenu  - Create sub menu

  Creates new Carbon menu item
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Destroy

  Frees Carbon menu item
 ------------------------------------------------------------------------------}
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
  end;

  FItems.Free;
  
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Add
  Params:  AMenu - Sub menu item to add

  Adds Carbon sub menu item
 ------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Remove
  Params:  AMenu - Sub menu item to remove

  Removes Carbon sub menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.Remove(AMenu: TCarbonMenu);
var
  I: Integer;
begin
  I := FItems.IndexOf(AMenu);
  if I < 0 then Exit;
  
  DeleteMenuItem(Menu, I + 1);
  FItems.Delete(I);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.Attach
  Params:  AParentMenu - Parent menu item

  Attaches Carbon sub menu item to the parent
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.Attach(AParentMenu: TCarbonMenu);
var
  S: String;
  CFString: CFStringRef;
  Index: Integer;
const
  SName = 'Attach';
  SInsertMenu = 'InsertMenuItemTextWithCFString';
begin
  FParentMenu := AParentMenu;

  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.Attach ' + LCLMenuItem.Name + ' Index: ' + DbgS(GetIndex) +
      ' Menu: ' + DbgS(Menu));
  {$ENDIF}
  
  Index := GetIndex;
  if FParentMenu.FRoot then MenuNeeded(False); // menu item is in toplevel of root menu.

  if LCLMenuItem.Caption = cLineCaption then // menu item is separator
    OSError(
      InsertMenuItemTextWithCFString(FParentMenu.Menu, nil, Index,
        kMenuItemAttrSeparator, 0),
      Self, SName, SInsertMenu, '-')
  else
  begin
    S := LCLMenuItem.Caption;
    DeleteAmpersands(S);
    
    CreateCFString(S, CFString);
    try
      OSError(InsertMenuItemTextWithCFString(FParentMenu.Menu, CFString, Index,
          kMenuItemAttrIgnoreMeta, 0),
      Self, SName, SInsertMenu);
      
      if Menu <> nil then
        OSError(SetMenuTitleWithCFString(Menu, CFString), Self, SName,
          SSetMenuTitle);
    finally
      FreeCFString(CFString);
    end;
  end;
  // Note: menu item indexing in Carbon is, except inserting, one-based!
  
  // set Command ID for catching click events
  OSError(SetMenuItemCommandID(FParentMenu.Menu, Index + 1, MENU_FOURCC),
    Self, SName, 'SetMenuItemCommandID');
  
  // set property for identification
  OSError(SetMenuItemProperty(FParentMenu.Menu, Index + 1,
      LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(TCarbonMenu), @Self),
    Self, SName, 'SetMenuItemProperty');
  
  Update;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.AttachToMenuBar

  Attaches Carbon menu to the menu bar
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.AttachToMenuBar;
var
  I: Integer;
begin
  // create Carbon menu objects for toplevel items
  for I := 0 to FItems.Count - 1 do TCarbonMenu(FItems[I]).MenuNeeded;

  FRoot := True;
  
  OSError(SetRootMenu(Menu), Self, 'AttachToMenuBar', 'SetRootMenu');
  
  HiliteMenu(0);
  //DrawMenuBar;
  //InvalMenuBar;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetCaption
  Params:  ACaption - New menu item caption

  Sets the caption of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetCaption(const ACaption: String);
var
  CFString: CFStringRef;
  Index: Integer;
  S: String;
const
  SName = 'SetCaption';
begin
  if FParentMenu = nil then Exit;

  if LCLMenuItem.Caption = cLineCaption then // menu item is separator
  begin
    OSError(
      ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1,
        kMenuItemAttrSeparator, 0),
      Self, SName, SChangeMenuItemAttrs, '-');
  end
  else
  begin
    Index := GetIndex;
    OSError(
      ChangeMenuItemAttributes(FParentMenu.Menu, Index + 1, 0, kMenuItemAttrSeparator),
      Self, SName, SChangeMenuItemAttrs);
  
    S := LCLMenuItem.Caption;
    DeleteAmpersands(S);
    
    CreateCFString(S, CFString);
    try
      OSError(SetMenuItemTextWithCFString(FParentMenu.Menu, Index + 1, CFString),
        Self, SName, 'SetMenuItemTextWithCFString');

      if Menu <> nil then
        OSError(SetMenuTitleWithCFString(Menu, CFString), Self, SName, SSetMenuTitle);
    finally
      FreeCFString(CFString);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetVisible
  Params:  AVisible - New menu item visibility

  Sets the visibility of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetVisible(AVisible: Boolean);
const
  SName = 'SetVisible';
begin
  if FParentMenu = nil then Exit;
  
  if AVisible then
  begin
    OSError(
      ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1, 0, kMenuItemAttrHidden),
      Self, SName, SChangeMenuItemAttrs, 'show');

    if Menu <> nil then
      OSError(ChangeMenuAttributes(Menu, 0, kMenuAttrHidden), Self, SName,
        SChangeMenuAttrs, 'show');
  end
  else
  begin
    OSError(
      ChangeMenuItemAttributes(FParentMenu.Menu, GetIndex + 1, kMenuItemAttrHidden, 0),
      Self, SName, SChangeMenuItemAttrs, 'hide');

    if Menu <> nil then
      OSError(ChangeMenuAttributes(Menu, kMenuAttrHidden, 0), Self, SName,
        SChangeMenuAttrs, 'hide');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetEnable
  Params:  AEnabled - New menu item enabled

  Sets the enabled of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetEnable(AEnabled: Boolean);
var
  I: Integer;
begin
  if FParentMenu = nil then
  begin
    // diable sub items for top most menus
    if FItems <> nil then
      for I := 0 to FItems.Count - 1 do
      begin
        if AEnabled then
          TCarbonMenu(FItems[I]).SetEnable(TCarbonMenu(FItems[I]).LCLMenuItem.Enabled)
        else
          TCarbonMenu(FItems[I]).SetEnable(False);
      end;
      
    Exit;
  end;
  
  if AEnabled then
  begin
    EnableMenuItem(FParentMenu.Menu, GetIndex + 1);
    
    // enable sub menu
    if Menu <> nil then
    begin
      EnableMenuItem(Menu, 0);
    end;
  end
  else
  begin
    DisableMenuItem(FParentMenu.Menu, GetIndex + 1);

    // disable sub menu
    if Menu <> nil then
    begin
      DisableMenuItem(Menu, 0);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetBitmap
  Params:  ABitmap - New menu item bitmap

  Sets the bitmap of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetBitmap(const ABitmap: TBitmap);
var
  IconType: Byte;
  AHandle: MacOSAll.Handle;
  CGImage: CGImageRef;
const
  SName = 'SetBitmap';
begin
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.SetBitmap ' + LCLMenuItem.Name + ' ABitmap: ' + DbgS(ABitmap)
      + ' ParentMenu: ' + DbgS(FParentMenu.Menu));
  {$ENDIF}
  if FParentMenu = nil then Exit;

  AHandle := nil;
  CGImage := nil;
  
  if (ABitmap <> nil) and (ABitmap.Width > 0) and (ABitmap.Height > 0) then
  begin
    if not CheckBitmap(ABitmap.Handle, SName) then Exit;

    IconType := kMenuCGImageRefType;
    CGImage := TCarbonBitmap(ABitmap.Handle).CreateMaskedImage(TCarbonBitmap(ABitmap.MaskHandle));
    if CGImage <> nil then
      AHandle := Pointer(CGImage);
  end;
  
  if AHandle = nil then IconType := kMenuNoIcon;
  
  try
    {$IFDEF VerboseMenu}
      DebugLn('TCarbonMenu.SetBitmap IconType: ' + DbgS(IconType) + ' AIcon: ' + DbgS(AHandle));
    {$ENDIF}

    if OSError(
      SetMenuItemIconHandle(FParentMenu.Menu, GetIndex + 1, IconType, AHandle),
      Self, SName, 'SetMenuItemIconHandle') then Exit;
  finally
    if CGImage <> nil then
      CGImageRelease(CGImage);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetCheck
  Params:  AChecked - New menu item check

  Sets the check of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetCheck(AChecked: Boolean);
var
  I: Integer;
  Item: TCarbonMenu;
begin
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.SetCheck ' + LCLMenuItem.Caption + ' ' + DbgS(AChecked));
  {$ENDIF}
  if FParentMenu = nil then Exit;
  
  if AChecked then
  begin
    if LCLMenuItem.RadioItem then
    begin
      SetItemMark(FParentMenu.Menu, GetIndex + 1, Char(kDiamondCharCode));
      
      // uncheck siblings
      for I := 0 to FParentMenu.FItems.Count - 1 do
      begin
        Item := TCarbonMenu(FParentMenu.FItems[I]);
        if Item = Self then Continue;
        
        if Item.LCLMenuItem.RadioItem and
          (Item.LCLMenuItem.GroupIndex = LCLMenuItem.GroupIndex) then
            SetItemMark(FParentMenu.Menu, Item.GetIndex + 1, #0);

      end;
    end
    else
      SetItemMark(FParentMenu.Menu, GetIndex + 1, Char(kCheckCharCode));
  end
  else
    if (FItems = nil) or (FItems.Count = 0) then
      SetItemMark(FParentMenu.Menu, GetIndex + 1, #0);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetShortCut
  Params:  AShortCut - New menu item shortcut

  Sets the shortcut of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetShortCut(AShortCut: TShortCut);
var
  Shift: TShiftState;
  Key, MacKey: Word;
  Index: Integer;
const
  SName = 'SetShortCut';
  SSetCmdKey = 'SetMenuItemCommandKey';
begin
  {$IFDEF VerboseMenu}
    DebugLn('TCarbonMenu.SetShortCut ' + ShortCutToText(AShortCut));
  {$ENDIF}
  if FParentMenu = nil then Exit;
  
  ShortCutToKey(AShortCut, Key, Shift);

  Index := GetIndex;
  if OSError(SetMenuItemModifiers(FParentMenu.Menu, Index + 1,
      ShiftStateToModifiers(Shift)),
    Self, SName, 'SetMenuItemModifiers') then Exit;

  MacKey := VirtualKeyCodeToMac(Key);
  if MacKey = 0 then // character or number
  begin
    OSError(SetMenuItemCommandKey(FParentMenu.Menu, Index + 1, True, 0),
      Self, SName, SSetCmdKey);
    // unset virtual key short cut
    OSError(ChangeMenuItemAttributes(FParentMenu.Menu, Index + 1, 0, kMenuItemAttrUseVirtualKey),
      Self, Sname, SChangeMenuItemAttrs);
    OSError(SetMenuItemCommandKey(FParentMenu.Menu, Index + 1, False, Key),
      Self, SName, SSetCmdKey);
  end
  else // use mac virtual key code
  begin
    {$IFDEF VerboseMenu}
      DebugLn('TCarbonMenu.SetShortCut Virtual: VK = ' + DbgSVKCode(Key) +
        ' MK = ' + DbgS(MacKey));
    {$ENDIF}
    
    OSError(SetMenuItemCommandKey(FParentMenu.Menu, Index + 1, False, 0),
      Self, SName, SSetCmdKey, 'virtual');
    // set virtual key short cut
    OSError(ChangeMenuItemAttributes(FParentMenu.Menu, Index + 1, kMenuItemAttrUseVirtualKey, 0),
      Self, Sname, SChangeMenuItemAttrs);
    OSError(SetMenuItemCommandKey(FParentMenu.Menu, Index + 1, True, MacKey),
      Self, SName, SSetCmdKey, 'virtual');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMenu.SetStyle

  Sets the style (default) of Carbon menu item
 ------------------------------------------------------------------------------}
procedure TCarbonMenu.SetStyle;
var
  Style: StyleParameter;
begin
  if FParentMenu = nil then Exit;
  
  if LCLMenuItem.Default then
    Style := MacOSAll.bold
  else
    Style := MacOSAll.normal;

  SetItemStyle(FParentMenu.Menu, GetIndex + 1, Style);
end;

function TCarbonMenu.GetShortCutKey: AnsiChar;
var
  KeyValue: UInt16;
begin
  if GetMenuItemCommandKey(FParentMenu.Menu, GetIndex + 1, False, KeyValue)<>noErr then
    Result:=#0
  else
    Result:=AnsiChar(KeyValue);
end;

end.

