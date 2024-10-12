unit CocoaMenus;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  // RTL
  sysutils,
  // LCL
  Forms, Menus, LCLType, Classes, LCLStrConsts,
  // LCL Cocoa
  CocoaAll, CocoaPrivate, CocoaCallback, CocoaUtils, CocoaConfig, CocoaConst;

type
  IMenuItemCallback = interface(ICommonCallBack)
    procedure ItemSelected;
    function MenuItemTarget: TMenuItem;
  end;

  TCocoaMenuItem = objcclass;

  { TCocoaMenu }

  TCocoaMenu = objcclass(NSMenu)
  private
    appleMenu: TCocoaMenuItem;
    attachedAppleMenu: Boolean;
    isKeyEq: Boolean;
  public
    procedure dealloc; override;
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
    procedure createAppleMenu(); message 'createAppleMenu';
    procedure overrideAppleMenu(AItem: TCocoaMenuItem); message 'overrideAppleMenu:';
    procedure attachAppleMenu(); message 'attachAppleMenu';
    function performKeyEquivalent(theEvent: NSEvent): LCLObjCBoolean; override;
    function lclIsKeyEquivalent: LCLObjCBoolean; message 'lclIsKeyEquivalent';
  end;

  { TCocoaMenuItem }

  TCocoaMenuItem = objcclass(NSMenuItem, NSMenuDelegateProtocol)
  public
    menuItemCallback: IMenuItemCallback;
    attachedAppleMenuItems: Boolean;
    FMenuItemTarget: TMenuItem;
    procedure UncheckSiblings(AIsChangingToChecked: LCLObjCBoolean = False); message 'UncheckSiblings:';
    function GetMenuItemHandle(): TMenuItem; message 'GetMenuItemHandle';
    function FindSubmenuByLclItem(lclItem: TMenuItem): TCocoaMenuItem; message 'FindSubmenuByLclItem:';
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
    function lclGetCallback: IMenuItemCallback; override;
    procedure lclClearCallback; override;
    procedure attachAppleMenuItems(); message 'attachAppleMenuItems';
    function isValidAppleMenu(): LCLObjCBoolean; message 'isValidAppleMenu';
    // menuWillOpen cannot be used. Because it SHOULD NOT change the contents
    // of the menu. While LCL allows to modify the menu contents when the submenu
    // is about to be activated.
    procedure menuNeedsUpdate(AMenu: NSMenu); message 'menuNeedsUpdate:';
    procedure menuDidClose(AMenu: NSMenu); message 'menuDidClose:';
    //procedure menuDidClose(AMenu: NSMenu); message 'menuDidClose:';
    function worksWhenModal: LCLObjCBoolean; message 'worksWhenModal';
  end;

  TMenuItemHandleCreateFunc = function(const AMenuItem: TMenuItem): NSMenuItem;

const
  isMenuEnabled : Boolean = true;

var
  menuItemHandleCreateFunc: TMenuItemHandleCreateFunc;

procedure MenuTrackStarted(mn: NSMenu);
procedure MenuTrackEnded(mn: NSMenu);
procedure MenuTrackCancelAll;

// the returned "Key" should not be released, as it's not memory owned
procedure ShortcutToKeyEquivalent(const AShortCut: TShortcut; out Key: NSString; out shiftKeyMask: NSUInteger);

function LCLMenuItemInit(item: NSMenuItem; const lclMenuItem: TMenuItem): id;
function LCLMenuItemInit(item: NSMenuItem; const atitle: string; ashortCut: TShortCut): id;
function LCLMenuItemInit(item: NSMenuItem; const atitle: string; VKKey: Word = 0; State: TShiftState = []): id;
function ToggleAppMenu(ALogicalEnabled: Boolean): Boolean;
procedure Do_SetCheck(const ANSMenuItem: NSMenuItem; const Checked: boolean);

function FindEditMenu(const menu:NSMenu; const title:NSString): NSMenuItem;
procedure AttachEditMenu(const menu:NSMenu; const index:Integer; const title:NSString );
procedure NSMenuAddItemsFromLCLMenu(menu: NSMenu; lclMenu: TMenuItem);

implementation

type
  TCocoaMenuItem_HideApp = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  TCocoaMenuItem_HideOthers = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  TCocoaMenuItem_ShowAllApp = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  TCocoaMenuItem_Quit = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

var
  // menuTrack is needed due to the LCL architecture vs Cocoa menu handling.
  //
  // (below "Modal" refers to LCL "modal" state. Not verified with Cocoa modal)
  //
  // All Menu hangling in Cocoa is done within a "tracking" loop. (similar to Modal even loop)
  // The major issue, is that WS code never knows that a menu was clicked,
  // outside of the tracking loop. (The delegate is being notified already within the loop)
  //
  // If LCL handler is calling for any modal window at the time,
  // the menu tracking should stop, and the model window is the only be processed
  //
  // In order to track "opened" menus, menuTrack list is used.
  // If modal window is called MenuTrackCancelAll() will terminate all openned sub-menus
  //
  // The issue of the conflict between "Menu tracking" and "Modal tracking"
  // is only affecting Node-Menus (menu items with submenus)
  // because we call LM_ACTIVATE within tracking loop.
  // The Leaf-menuitems (menu items w/o submenuis) are calling "itemSElected" (LM_ACTIVATE)
  // when the tracking loop is over
  //
  // See topic: https://forum.lazarus.freepascal.org/index.php/topic,56419.0.html
  menuTrack : NSMutableArray;

procedure ShortcutToKeyEquivalent(const AShortCut: TShortcut; out Key: NSString; out shiftKeyMask: NSUInteger);
var
  w: word;
  s: TShiftState;
begin
  ShortCutToKey(AShortCut, w, s);
  key := VirtualKeyCodeToMacString(w);
  shiftKeyMask := 0;
  if ssShift in s then
    ShiftKeyMask := ShiftKeyMask + NSShiftKeyMask;
  if ssAlt in s then
    ShiftKeyMask := ShiftKeyMask + NSAlternateKeyMask;
  if ssCtrl in s then
    ShiftKeyMask := ShiftKeyMask + NSControlKeyMask;
  if ssMeta in s then
    ShiftKeyMask := ShiftKeyMask + NSCommandKeyMask;

  // as a key , +/= is a rare case, both + and = are used as primary keys.
  // ‘Shift+=’ for ‘+’
  // ‘=’ for ‘='
  if key.isEqualToString(NSSTR_KEY_PLUS) then begin
    if (ShiftKeyMask and NSShiftKeyMask)=0 then
      key := NSSTR_KEY_EQUALS
    else
      ShiftKeyMask := ShiftKeyMask - NSShiftKeyMask;
  end;
end;

procedure ToggleAppNSMenu(mn: NSMenu; ALogicalEnabled: Boolean);
var
  it  : NSMenuItem;
  obj : NSObject;
  enb : Boolean;
begin
  if not Assigned(mn) then Exit;
  for obj in mn.itemArray do begin
    if not obj.isKindOfClass(NSMenuItem) then continue;
    it := NSMenuItem(obj);
    enb := ALogicalEnabled;
    if enb and (it.isKindOfClass(TCocoaMenuItem)) then
    begin
      enb := not Assigned(TCocoaMenuItem(it).FMenuItemTarget)
         or ( TCocoaMenuItem(it).FMenuItemTarget.Enabled );
    end;
    {$ifdef BOOLFIX}
    it.setEnabled_( Ord(enb));
    {$else}
    it.setEnabled(enb);
    {$endif}
    if (it.hasSubmenu) then
    begin
      ToggleAppNSMenu(it.submenu, ALogicalEnabled);
    end;
  end;
end;

function ToggleAppMenu(ALogicalEnabled: Boolean): Boolean;
begin
  Result := isMenuEnabled;
  ToggleAppNSMenu( NSApplication(NSApp).mainMenu, ALogicalEnabled );
  isMenuEnabled := ALogicalEnabled;
end;

procedure Do_SetCheck(const ANSMenuItem: NSMenuItem; const Checked: boolean);
const
  menustate : array [Boolean] of NSInteger = (NSOffState, NSOnState);
begin
  ANSMenuItem.setState( menustate[Checked] );
end;

function FindEditMenuByKeyEquivalent(const menu: NSMenu;
  const keyEquivalent:NSString): NSMenuItem;
var
  item: NSMenuItem;
  subItem: NSMenuItem;
begin
  Result:= nil;
  if NOT Assigned(menu) then
    Exit;

  for item in menu.itemArray do begin
    if item.hasSubmenu then begin
      for subItem in item.submenu.itemArray do begin
        if NOT keyEquivalent.isEqualToString(subItem.keyEquivalent) then
          continue;
        if subItem.keyEquivalentModifierMask <> NSCommandKeyMask then
          continue;
        Result:= item;
        Exit;
      end;
    end;
  end;
end;

function getHotkeyFromTitle( aTitle:String ): Word;
var
  i: Integer;
  hotkeyChar: Char;
begin
  Result:= 0;
  i:= aTitle.IndexOf( cHotkeyPrefix );
  if (i<0) or (i>=aTitle.Length-1) then
    Exit;

  hotkeyChar:= aTitle.Chars[i+1];
  if hotkeyChar <> cHotkeyPrefix then
    Result:= Word( UpCase(hotkeyChar) );
end;

function LCLMenuItemInit(item: NSMenuItem; const lclMenuItem: TMenuItem): id;
var
  aShortCut: TShortCut;
  aTitle: String;
  key: Word;
begin
  aTitle := lclMenuItem.Caption;
  aShortCut := lclMenuItem.ShortCut;

  if (lclMenuItem.Owner is TPopupMenu) and (aShortCut=0) then begin
    if not Assigned(lclMenuItem.Action) then begin
      key:= getHotkeyFromTitle( aTitle );
      if key<>0 then
        aShortCut:= ShortCut( key, [] );
    end;
  end;

  Result:= LCLMenuItemInit(item, aTitle, aShortCut);
end;

function LCLMenuItemInit(item: NSMenuItem; const atitle: string; ashortCut: TShortCut): id;
var
  key   : NSString;
  mask  : NSUInteger;
begin
  ShortcutToKeyEquivalent(ashortCut, key, mask);
  Result := item.initWithTitle_action_keyEquivalent(
    ControlTitleToNSStr(Atitle),
    objcselector('lclItemSelected:'), // Selector is Hard-coded, that's why it's LCLMenuItemInit
    key);
  NSMenuItem(Result).setKeyEquivalentModifierMask(mask);
  NSMenuItem(Result).setTarget(Result);
end;

function LCLMenuItemInit(item: NSMenuItem; const atitle: string; VKKey: Word; State: TShiftState): id;
var
  key   : NSString;
  mask  : NSUInteger;
begin
  Result := LCLMenuItemInit(item, atitle, ShortCut(VKKey, State));
end;

function FindEditMenu(const menu: NSMenu; const title: NSString): NSMenuItem;
var
  index: NSInteger;
begin
  if NOT Assigned(menu) then
    Exit;

  index:= menu.indexOfItemWithTitle(title);
  if index >= 0 then begin
    Result:= menu.itemAtIndex(index);
    Exit;
  end;

  Result:= FindEditMenuByKeyEquivalent(menu, NSSTR('c')); // Command+C
end;

procedure AttachEditMenu(const menu: NSMenu; const index: Integer;
  const title: NSString);
var
  editMenu: NSMenuItem;
  editSubmenu: NSMenu;
begin
  editMenu:= NSMenuItem.alloc.init;
  editMenu.setTitle( title );
  menu.insertItem_atIndex(editMenu, index);
  editMenu.release;

  editSubmenu:= NSMenu.alloc.initWithTitle(title);
  editMenu.setSubmenu(editSubmenu);
  editSubmenu.release;

  editSubmenu.addItemWithTitle_action_keyEquivalent(
    CocoaConst.NSSTR_EDIT_MENU_UNDO, objcselector('undo:'), NSSTR('z'));
  editSubmenu.addItemWithTitle_action_keyEquivalent(
    CocoaConst.NSSTR_EDIT_MENU_REDO, objcselector('redo:'), NSSTR('Z'));
  editSubmenu.addItem(NSMenuItem.separatorItem);

  editSubmenu.addItemWithTitle_action_keyEquivalent(
    CocoaConst.NSSTR_EDIT_MENU_CUT, objcselector('cut:'), NSSTR('x'));
  editSubmenu.addItemWithTitle_action_keyEquivalent(
    CocoaConst.NSSTR_EDIT_MENU_COPY, objcselector('copy:'), NSSTR('c'));
  editSubmenu.addItemWithTitle_action_keyEquivalent(
    CocoaConst.NSSTR_EDIT_MENU_PASTE, objcselector('paste:'), NSSTR('v'));
  editSubmenu.addItemWithTitle_action_keyEquivalent(
    CocoaConst.NSSTR_EDIT_MENU_SELECTALL, objcselector('selectAll:'), NSSTR('a'));
end;

procedure NSMenuAddItemsFromLCLMenu(menu: NSMenu; lclMenu: TMenuItem);
var
  index: Integer;
  lclItem: TMenuItem;
  item: NSMenuItem;
begin
  if NOT Assigned(menu) then
    Exit;
  if NOT Assigned(lclMenu) then
    Exit;

  for index:=0 to lclMenu.Count-1 do begin
    lclItem:= lclMenu.Items[index];
    item:= menuItemHandleCreateFunc(lclItem);
    menu.addItem(item);
  end;
end;

{ TCocoaMenu }

procedure TCocoaMenu.dealloc;
begin
  if appleMenu <> nil then begin
    if indexOfItem(appleMenu) >= 0 then
      removeItem(appleMenu);
    appleMenu.release;
    appleMenu := nil;
  end;
  inherited dealloc;
end;

procedure TCocoaMenu.lclItemSelected(sender:id);
begin

end;

// For when there is no menu item with title 
procedure TCocoaMenu.createAppleMenu();
var
  nskey, nstitle, nssubmeykey: NSString;
  lNSSubmenu: NSMenu;
begin
  // create the menu item
  nstitle := NSStringUtf8('');
  appleMenu := TCocoaMenuItem.alloc.initWithTitle_action_keyEquivalent(nstitle,
    objcselector('lclItemSelected:'), NSString.string_);
  nstitle.release;

  // add the submenu
  lNSSubmenu := NSMenu.alloc.initWithTitle(NSString.string_);
  appleMenu.setSubmenu(lNSSubmenu);
  lNSSubmenu.release;
end;

// For when there is a menu item with title 
procedure TCocoaMenu.overrideAppleMenu(AItem: TCocoaMenuItem);
begin
  if appleMenu <> nil then
  begin
    if indexOfItem(appleMenu) >= 0 then
      removeItem(appleMenu);
    appleMenu.release;
    appleMenu := nil;
  end;
  attachedAppleMenu := False;
  AItem.attachAppleMenuItems();
end;

procedure TCocoaMenu.attachAppleMenu();
begin
  if attachedAppleMenu then Exit;
  if appleMenu = nil then Exit;
  appleMenu.attachAppleMenuItems();
  attachedAppleMenu := True;
  insertItem_atIndex(appleMenu, 0);
end;

function TCocoaMenu.performKeyEquivalent(theEvent: NSEvent): LCLObjCBoolean;
var
  OldKeyEq: boolean;
begin
  OldKeyEq:=isKeyEq;
  isKeyEq := true;
  try
    Result := inherited performKeyEquivalent(theEvent);
  finally
    isKeyEq := OldKeyEq;
  end;
end;

function TCocoaMenu.lclIsKeyEquivalent: LCLObjCBoolean;
begin
  Result := isKeyEq;
end;

{ TCocoaMenuITem }

procedure TCocoaMenuItem.UncheckSiblings(AIsChangingToChecked: LCLObjCBoolean);
var
  i: Integer;
  lMenuItem, lSibling, lParentMenu: TMenuItem;
  lSiblingHandle: NSMenuItem;
begin
  //lMenuItem := GetMenuItemHandle();
  lMenuItem := FMenuItemTarget;
  if lMenuItem = nil then Exit;
  if not lMenuItem.RadioItem then Exit;
  if (not AIsChangingToChecked) and (not lMenuItem.Checked) then Exit;
  lParentMenu := lMenuItem.Parent;
  if lParentMenu = nil then Exit;
  for i := 0 to lParentMenu.Count - 1 do
  begin
    lSibling := lParentMenu.Items[i];
    if lSibling = nil then Continue;
    if lSibling = lMenuItem then Continue;

    if lSibling.RadioItem and (lSibling.GroupIndex = lMenuItem.GroupIndex) and
      lSibling.HandleAllocated() then
    begin
      lSiblingHandle := NSMenuItem(lSibling.Handle);
      Do_SetCheck(lSiblingHandle, False);
    end;
  end;
end;

function TCocoaMenuItem.GetMenuItemHandle(): TMenuItem;
begin
  Result := nil;
  if menuItemCallback = nil then Exit;
  Result := menuItemCallback.MenuItemTarget;
end;

function TCocoaMenuItem.FindSubmenuByLclItem(lclItem: TMenuItem): TCocoaMenuItem;
var
  nsItem: NSMenuItem;
begin
  Result:= nil;
  if NOT Assigned(lclItem) then
    Exit;

  for nsItem in self.submenu.itemArray do begin
    if NOT nsItem.isKindOfClass(TCocoaMenuItem) then
      continue;
    if TCocoaMenuItem(nsItem).FMenuItemTarget<>lclItem then
      continue;
    Result:= TCocoaMenuItem(nsItem);
    break;
  end;
end;

procedure TCocoaMenuItem.lclItemSelected(sender:id);
begin
  menuItemCallback.ItemSelected;
  UncheckSiblings();
end;

function TCocoaMenuItem.lclGetCallback: IMenuItemCallback;
begin
  result:=menuItemCallback;
end;

procedure TCocoaMenuItem.lclClearCallback;
begin
  menuItemCallback := nil;
end;

procedure TCocoaMenuItem.attachAppleMenuItems();
var
  item    : NSMenuItem;
  itemSubMenu: NSMenu;
  currentIndex: NSInteger = 0;

  procedure attachSpecialMenuItem(
    lclItem: TMenuItem;
    title: String;
    key: Word = 0;
    state: TShiftState = [] );
  var
    keyEquivalent: NSString;
    keyMask: NSUInteger;
  begin
    if NOT Assigned(lclItem) then
      Exit;

    item:= self.FindSubmenuByLclItem(lclItem);
    if NOT Assigned(item) then begin
      item:= menuItemHandleCreateFunc(lclItem);
      submenu.insertItem_atIndex(item, currentIndex);
      item.release;
      inc(currentIndex);
      submenu.insertItem_atIndex(NSMenuItem.separatorItem, currentIndex);
      inc(currentIndex);
    end;
    item.setTitle( StrToNSString(title) );
    item.setImage(nil);
    ShortcutToKeyEquivalent(ShortCut(key,state), keyEquivalent, keyMask );
    item.setKeyEquivalent(keyEquivalent);
    item.setKeyEquivalentModifierMask(keyMask);
  end;

begin
  if attachedAppleMenuItems then Exit;
  if not hasSubmenu() then Exit;
  attachedAppleMenuItems := True;

  if NOT Assigned(self.FMenuItemTarget) then
    self.FMenuItemTarget:= CocoaConfigMenu.appMenu.customMenus;

  // APP Custom
  NSMenuAddItemsFromLCLMenu(self.submenu, CocoaConfigMenu.appMenu.customMenus);

  // About
  attachSpecialMenuItem(
    CocoaConfigMenu.appMenu.aboutItem,
    Format(rsMacOSMenuAbout, [Application.Title]) );

  // Preferences
  attachSpecialMenuItem(
    CocoaConfigMenu.appMenu.preferencesItem,
    rsMacOSMenuPreferences,
    VK_OEM_COMMA, [ssMeta]);

  // Custom onCreate
  if Assigned( CocoaConfigMenu.appMenu.onCreate ) then begin
    submenu.addItem( NSMenuItem.separatorItem );
    CocoaConfigMenu.appMenu.onCreate( submenu );
  end;

  // Auto Create App Menu below?
  if CocoaConfigMenu.appMenu.dontAutoCreateItems then
    Exit;

  // Separator
  submenu.addItem(NSMenuItem.separatorItem);

  // Services
  item := LCLMenuItemInit( TCocoaMenuItem.alloc, rsMacOSMenuServices);
  item.setTarget(nil);
  item.setAction(nil);
  submenu.addItem(item);
  itemSubMenu := NSMenu.alloc.initWithTitle( ControlTitleToNSStr(rsMacOSMenuServices));
  item.setSubmenu(itemSubMenu);
  NSApplication(NSApp).setServicesMenu(itemSubMenu);
  itemSubMenu.release;
  item.release;

  // Separator
  submenu.addItem(NSMenuItem.separatorItem);

  // Hide App     Meta-H
  item := LCLMenuItemInit( TCocoaMenuItem_HideApp.alloc, Format(rsMacOSMenuHide, [Application.Title]), VK_H, [ssMeta]);
  submenu.addItem(item);
  item.release;

  // Hide Others  Meta-Alt-H
  item := LCLMenuItemInit( TCocoaMenuItem_HideOthers.alloc, rsMacOSMenuHideOthers, VK_H, [ssMeta, ssAlt]);
  submenu.addItem(item);
  item.release;

  // Show All
  item := LCLMenuItemInit( TCocoaMenuItem_ShowAllApp.alloc, rsMacOSMenuShowAll);
  submenu.addItem(item);
  item.release;

  // Separator
  submenu.addItem(NSMenuItem.separatorItem);

  // Quit   Meta-Q
  item := LCLMenuItemInit( TCocoaMenuItem_Quit.alloc, Format(rsMacOSMenuQuit, [Application.Title]), VK_Q, [ssMeta]);
  submenu.addItem(item);
  item.release;
end;

function TCocoaMenuItem.isValidAppleMenu(): LCLObjCBoolean;
begin
  Result := hasSubmenu() and (submenu() <> nil);
  Result := Result and ('' = NSStringToString(title));
end;

procedure TCocoaMenuItem.menuNeedsUpdate(AMenu: NSMenu);
begin
  if not Assigned(menuItemCallback) then Exit;
  if not isMenuEnabled then Exit;

  MenuTrackStarted(AMenu);

  if (menu.isKindOfClass(TCocoaMenu)) then
  begin
    // Issue #37789
    // Cocoa tries to find, if there's a menu with the key event
    // so item is not actually selected yet. Thus should not send ItemSelected
    if TCocoaMenu(menu).lclIsKeyEquivalent then
      Exit;
  end;

  //todo: call "measureItem"
  menuItemCallback.ItemSelected;
end;

procedure TCocoaMenuItem.menuDidClose(AMenu: NSMenu);
begin
  MenuTrackEnded(AMenu);
end;

function TCocoaMenuItem.worksWhenModal: LCLObjCBoolean;
begin
  // refer to NSMenuItem.target (Apple) documentation
  // the method must be implemented in target and return TRUE
  // otherwise it won't work for modal!
  //
  // The method COULD be used to protect the main menu from being clicked
  // if a modal window doesn't have a menu.
  // But LCL disables (is it?) the app menu manually on modal
  Result := true;
end;

{  menuDidClose should not change the structure of the menu.
   The restructuring is causing issues on Apple's special menus (i.e. HELP menu)
   See bug #35625

procedure TCocoaMenuItem.menuDidClose(AMenu: NSMenu);
var
  par : NSMenu;
  idx : NSInteger;
  mn  : NSMenuItem;
begin
  // the only purpose of this code is to "invalidate" the submenu of the item.
  // an invalidated menu will call menuNeedsUpdate.
  // There's no other way in Cocoa to do the "invalidate"
  par := amenu.supermenu;
  if Assigned(par) then
  begin
    idx := par.indexOfItemWithSubmenu(AMenu);
    if idx<>NSNotFound then
    begin
      mn := par.itemAtIndex(idx);
      mn.setSubmenu(nil);
      mn.setSubmenu(AMenu);
    end;
  end;
end;
}

procedure TCocoaMenuItem_HideApp.lclItemSelected(sender: id);
begin
  // Applicaiton.Minimize, calls WidgetSet.AppMinimize;
  // which calls NSApplication.hide() anyway
  Application.Minimize;
end;

procedure TCocoaMenuItem_HideOthers.lclItemSelected(sender: id);
begin
  NSApplication(NSApp).hideOtherApplications(sender);
end;

procedure TCocoaMenuItem_ShowAllApp.lclItemSelected(sender: id);
begin
  NSApplication(NSApp).unhideAllApplications(sender);
end;

procedure TCocoaMenuItem_Quit.lclItemSelected(sender: id);
begin
  {$ifdef COCOALOOPHIJACK}
  // see bug #36265. if hot-key (Cmd+Q) is used the menu item
  // would be called once. 1) in LCL controlled loop 2) after the loop finished
  // The following if statement prevents "double" form close
  if LoopHiJackEnded then Exit;
  {$endif}
  // Should be used instead of Application.Terminate when possible
  // to allow events to be sent, see bug 32148
  if Assigned(Application.MainForm) then
    Application.MainForm.Close
  else
    Application.Terminate;
end;

procedure MenuTrackStarted(mn: NSMenu);
begin
  if not Assigned(menuTrack) then menuTrack := NSMutableArray.alloc.init;
  menuTrack.addObject(mn);
end;

procedure MenuTrackEnded(mn: NSMenu);
begin
  if Assigned(menuTrack) then
    menuTrack.removeObject(mn);
end;

procedure MenuTrackCancelAll;
var
  mn : NSMenu;
begin
  if not Assigned(menuTrack) then Exit;
  if menuTrack.count = 0 then Exit;
  for mn in menuTrack do
  begin
    if Assigned(mn) then
      mn.cancelTracking;
  end;
  menuTrack.removeAllObjects;
end;

finalization
  MenuTrackCancelAll;
  if menuTrack <> nil then menuTrack.release;

end.

