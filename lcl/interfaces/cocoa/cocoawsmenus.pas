{ $Id: Cocoawsmenus.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                               CocoaWSMenus.pp                             *
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSMenus;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // Libs
  CocoaAll,
  MacOSAll,
  // RTL
  sysutils,
  // LCL
  Controls, Forms, Menus, Graphics, LCLType, LMessages, LCLProc, Classes,
  LCLMessageGlue,
  // Widgetset
  WSMenus, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate, CocoaWSCommon, CocoaUtils;

type

  IMenuItemCallback = interface(ICommonCallBack)
    procedure ItemSelected;
    function MenuItemTarget: TMenuItem;
  end;

  { TLCLMenuItemCallback }

  TLCLMenuItemCallback = class(TLCLCommonCallback, IMenuItemCallback)
  private
    FMenuItemTarget: TMenuItem;
  public
    constructor Create(AOwner: NSObject; AMenuItemTarget: TMenuItem); reintroduce;
    procedure ItemSelected;
    function MenuItemTarget: TMenuItem;
  end;

  TCocoaMenuItem = objcclass;

  { TCocoaMenu }

  TCocoaMenu = objcclass(NSMenu)
  private
    appleMenu: TCocoaMenuItem;
    attachedAppleMenu: Boolean;
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
    function lclIsHandle: Boolean; override;
    procedure createAppleMenu(); message 'createAppleMenu';
    procedure overrideAppleMenu(AItem: TCocoaMenuItem); message 'overrideAppleMenu:';
    procedure attachAppleMenu(); message 'attachAppleMenu';
  end;

  { TCocoaMenuItem }

  TCocoaMenuItem = objcclass(NSMenuItem, NSMenuDelegateProtocol)
  public
    menuItemCallback: IMenuItemCallback;
    attachedAppleMenuItems: Boolean;
    FMenuItemTarget: TMenuItem;
    procedure UncheckSiblings(AIsChangingToChecked: Boolean = False); message 'UncheckSiblings:';
    function GetMenuItemHandle(): TMenuItem; message 'GetMenuItemHandle';
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
    function lclGetCallback: IMenuItemCallback; override;
    procedure lclClearCallback; override;
    function lclIsHandle: Boolean; override;
    procedure attachAppleMenuItems(); message 'attachAppleMenuItems';
    function isValidAppleMenu(): Boolean; message 'isValidAppleMenu';
    function validateMenuItem(menuItem: NSMenuItem): Boolean; override;
    procedure menuNeedsUpdate(AMenu: NSMenu); message 'menuNeedsUpdate:';
  end;

  TCocoaMenuItem_HideApp = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  TCocoaMenuItem_HideOthers = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  TCocoaMenuItem_Quit = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  { TCocoaWSMenuItem }

  TCocoaWSMenuItem = class(TWSMenuItem)
  private
    class procedure Do_SetCheck(const ANSMenuItem: NSMenuItem; const Checked: boolean);
    // used from the MenuMadness example
    class function NSMenuCheckmark: NSImage;
    class function NSMenuRadio: NSImage;
    class function isSeparator(const ACaption: AnsiString): Boolean;
    class function MenuCaption(const ACaption: AnsiString): AnsiString;
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    //class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
  end;

  { TCocoaWSMenu }

  TCocoaWSMenu = class(TWSMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TCocoaWSMainMenu }

  TCocoaWSMainMenu = class(TWSMainMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TCocoaWSPopupMenu }

  TCocoaWSPopupMenu = class(TWSPopupMenu)
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: Integer); override;
  end;


implementation

{ TLCLMenuItemCallback }

constructor TLCLMenuItemCallback.Create(AOwner: NSObject; AMenuItemTarget: TMenuItem);
begin
  Owner := AOwner;
  FMenuItemTarget := AMenuItemTarget;
end;

procedure TLCLMenuItemCallback.ItemSelected;
var
  Msg:TLMessage;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), 0);
  Msg.msg := LM_ACTIVATE;
  // debugln('send LM_Activate');
  LCLMessageGlue.DeliverMessage(FMenuItemTarget,Msg);
end;

function TLCLMenuItemCallback.MenuItemTarget: TMenuItem;
begin
  Result:=FMenuItemTarget;
end;

{ TCocoaMenu }

function TCocoaMenu.lclIsHandle: Boolean;
begin
  Result:=true;
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
  nskey := NSStringUtf8('');
  appleMenu := TCocoaMenuItem.alloc.initWithTitle_action_keyEquivalent(nstitle,
    objcselector('lclItemSelected:'), nskey);
  nstitle.release;
  nskey.release;

  // add the submenu
  nssubmeykey := NSStringUtf8('');
  lNSSubmenu := NSMenu.alloc.initWithTitle(nssubmeykey);
  appleMenu.setSubmenu(lNSSubmenu);
  nssubmeykey.release;

  appleMenu.attachAppleMenuItems();
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
  attachedAppleMenu := True;
  insertItem_atIndex(appleMenu, 0);
end;

{ TCocoaMenuITem }

function TCocoaMenuItem.lclIsHandle: Boolean;
begin
  Result:=true;
end;

procedure TCocoaMenuItem.UncheckSiblings(AIsChangingToChecked: Boolean);
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
      TCocoaWSMenuItem.Do_SetCheck(lSiblingHandle, False);
    end;
  end;
end;

function TCocoaMenuItem.GetMenuItemHandle(): TMenuItem;
begin
  Result := nil;
  if menuItemCallback = nil then Exit;
  Result := menuItemCallback.MenuItemTarget;
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
  ns, nsCharCode: NSString;
begin
  if attachedAppleMenuItems then Exit;
  if not hasSubmenu() then Exit;

  // Separator
  submenu.insertItem_atIndex(NSMenuItem.separatorItem, submenu.itemArray.count);
  // Hide App
  ns := NSStringUtf8('Hide ' + Application.Title);
  nsCharCode := NSStringUtf8('h');
  item := TCocoaMenuItem_HideApp.alloc.initWithTitle_action_keyEquivalent(ns,
    objcselector('lclItemSelected:'), nsCharCode);
  submenu.insertItem_atIndex(item, submenu.itemArray.count);
  item.setTarget(item);
  // release mem
  nsCharCode.release;
  ns.release;
  // Separator
  submenu.insertItem_atIndex(NSMenuItem.separatorItem, submenu.itemArray.count);
  // Quit
  ns := NSStringUtf8('Quit');
  nsCharCode := NSStringUtf8('q');
  item := TCocoaMenuItem_Quit.alloc.initWithTitle_action_keyEquivalent(ns,
    objcselector('lclItemSelected:'), nsCharCode);
  submenu.insertItem_atIndex(item, submenu.itemArray.count);
  item.setTarget(item);
  // release mem
  nsCharCode.release;
  ns.release;

  attachedAppleMenuItems := True;
end;

function TCocoaMenuItem.isValidAppleMenu(): Boolean;
begin
  Result := hasSubmenu() and (submenu() <> nil);
  Result := Result and ('' = NSStringToString(title));
end;

function TCocoaMenuItem.validateMenuItem(menuItem: NSMenuItem): Boolean;
begin
  Result := FMenuItemTarget.Enabled;
end;

procedure TCocoaMenuItem.menuNeedsUpdate(AMenu: NSMenu);
begin
  menuItemCallback.ItemSelected;
end;

procedure TCocoaMenuItem_HideApp.lclItemSelected(sender: id);
begin
  Application.Minimize;
end;

procedure TCocoaMenuItem_HideOthers.lclItemSelected(sender: id);
begin
end;

procedure TCocoaMenuItem_Quit.lclItemSelected(sender: id);
begin
  // Should be used instead of Application.Terminate to allow events to be sent, see bug 32148
  Application.MainForm.Close;
end;

{ TCocoaWSMenu }

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenu.CreateHandle
  Params:  AMenu - LCL menu
  Returns: Handle to the menu in Cocoa interface

  Creates new menu in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  ns: NSString;
begin
  //WriteLn(':>[TCocoaWSMenu.CreateHandle]');
  ns := NSStringUtf8('');
  Result := HMENU(TCocoaMenu.alloc.initWithTitle(ns));
  ns.release;
end;

{ TCocoaWSMainMenu }

class function TCocoaWSMainMenu.CreateHandle(const AMenu: TMenu): HMENU;
var
  ns: NSString;
begin
  ns := NSStringUtf8('');
  Result := HMENU(TCocoaMenu.alloc.initWithTitle(ns));
  ns.release;
  TCocoaMenu(Result).createAppleMenu();
end;

{ TCocoaWSMenuItem }

class procedure TCocoaWSMenuItem.Do_SetCheck(const ANSMenuItem: NSMenuItem; const Checked: boolean);
const
  menustate : array [Boolean] of NSInteger = (NSOffState, NSOnState);
begin
  ANSMenuItem.setOnStateImage(NSMenuCheckmark);
  ANSMenuItem.setState( menustate[Checked] );
end;

// used from the MenuMadness example
class function TCocoaWSMenuItem.NSMenuCheckmark: NSImage;
begin
  Result:=NSImage.imageNamed(NSStringUtf8('NSMenuCheckmark'));
end;

class function TCocoaWSMenuItem.NSMenuRadio: NSImage;
begin
  Result:=NSImage.imageNamed(NSStringUtf8('NSMenuRadio'))
end;

class function TCocoaWSMenuItem.isSeparator(const ACaption: AnsiString): Boolean;
begin
  Result:=ACaption='-';
end;

class function TCocoaWSMenuItem.MenuCaption(const ACaption: AnsiString): AnsiString;
var
  i : Integer;
begin
  i:=Pos('&', ACaption);
  if i>0 then
    Result:=Copy(ACaption, 1, i-1)+Copy(ACaption,i+1, length(ACaption))
  else
    Result:=ACaption;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.AttachMenu
  Params:  AMenuItem - LCL menu item

  Attaches menu item to its parent menu in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  ParObj  : NSObject;
  Parent  : TCocoaMenu;
  item    : NSMenuItem;
  ns      : NSString;
  str       : string;
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) or not Assigned(AMenuItem.Parent) or (AMenuItem.Parent.Handle=0) then Exit;
  ParObj:=NSObject(AMenuItem.Parent.Handle);
  item:=NSMenuItem(AMenuItem.Handle);

  if ParObj.isKindOfClass_(NSMenuItem) then
  begin
    if not NSMenuItem(ParObj).hasSubmenu then
    begin
      str := AMenuItem.Parent.Caption;
      DeleteAmpersands(str);
      ns := NSStringUtf8(pchar(str));
      Parent := TCocoaMenu.alloc.initWithTitle(ns);
      Parent.setDelegate(TCocoaMenuItem(ParObj));
      NSMenuItem(ParObj).setSubmenu(Parent);
      ns.release;
    end
    else
      Parent:=TCocoaMenu(NSMenuItem(ParObj).submenu);
  end else if ParObj.isKindOfClass_(NSMenu) then
    Parent:=TCocoaMenu(ParObj)
  else
    Exit;

  Parent.insertItem_atIndex(item, AMenuItem.MenuVisibleIndex);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.CreateHandle
  Params:  AMenuItem - LCL menu item
  Returns: Handle to the menu item in Cocoa interface

  Creates new menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
var
  item    : NSMenuItem;
  ANSMenu : NSMenu;
  s       : string;
  ns      : NSString;
  nsKey   : NSString;
  key     : string;
  ShiftSt : NSUInteger;
begin
  if not Assigned(AMenuItem) then
  begin
    Result:=0;
    Exit;
  end;

  // A handle of TMenu.fItems (TMenuItem) could be recreated.
  // in this case LCL calls TCocoaWSMenuItem.CreateHandle
  // instead of the proper owner.
  if (AMenuItem.Owner is TMainMenu) and (TMainMenu(AMenuItem.Owner).Items = AMenuItem) then begin
    Result:=TCocoaWSMainMenu.CreateHandle(TMenu(AMenuItem.Owner));
    Exit;
  end else if (AMenuItem.Owner is TMenu) and (TMenu(AMenuItem.Owner).Items = AMenuItem) then begin
    Result:=TCocoaWSMenu.CreateHandle(TMenu(AMenuItem.Owner));
    Exit;
  end;

  if AMenuItem.Caption = '-' then
  begin
    item := NSMenuItem.separatorItem;
  end
  else
  begin
    s := AMenuItem.Caption;
    DeleteAmpersands(s);
    ShortcutToKeyEquivalent(AMenuItem.ShortCut, nsKey, ShiftSt);

    ns := NSStringUtf8(s);
    item := TCocoaMenuItem(TCocoaMenuItem.alloc).initWithTitle_action_keyEquivalent(ns,
      objcselector('lclItemSelected:'), nsKey);
    item.setKeyEquivalentModifierMask(ShiftSt);
    TCocoaMenuItem(item).FMenuItemTarget := AMenuItem;

    if AMenuItem.IsInMenuBar then
    begin
      ANSMenu := TCocoaMenu.alloc.initWithTitle(ns);
      ANSMenu.setDelegate(TCocoaMenuItem(item));
      item.setSubmenu(ANSMenu);
    end;

    ns.release;
    item.setTarget(item);
    TCocoaMenuItem(item).menuItemCallback:=TLCLMenuItemCallback.Create(item, AMenuItem);

    // initial set of properties
    item.setEnabled(AMenuItem.Enabled);
    Do_SetCheck(item, AMenuItem.Checked);
  end;

  Result:=HMENU(item);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.DestroyHandle
  Params:  AMenuItem - LCL menu item

  Destroys menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
var
  callback: IMenuItemCallback;
  callbackObject: TObject;
  item    : NSObject;
  parItem : NSObject;
begin
  if AMenuItem.Caption <> '-' then
    begin
    item:=NSObject(AMenuItem.Handle);
    if item.isKindOfClass_(TCocoaMenuItem) then
      begin
      callback := TCocoaMenuItem(item).lclGetCallback;
      if Assigned(callback) then
        begin
        callbackObject := callback.GetCallbackObject;
        callback := nil;
        TCocoaMenuItem(item).lclClearCallback;
        callbackObject.Free;
        end;
      parItem := TCocoaMenuItem(Item).parentItem;
      if assigned(parItem) and parItem.isKindOfClass_(NSMenuItem) then
        NSMenuItem(paritem).submenu.removeItem(NSMenuItem(item));
      Item.Release;
      AMenuItem.Handle := 0;
      end
    end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetCaption
  Params:  AMenuItem - LCL menu item
           ACaption  - Menu item caption

  Sets the caption of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
var
  ns : NSString;
  s: string;
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) then Exit;
  s := ACaption;
  DeleteAmpersands(s);
  ns:=NSStringUtf8(s);
  NSMenuItem(AMenuItem.Handle).setTitle(ns);
  if NSMenuItem(AMenuItem.Handle).hasSubmenu then
    NSMenuItem(AMenuItem.Handle).submenu.setTitle(ns);
  ns.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetShortCut
  Params:  AMenuItem   - LCL menu item
           ShortCutK1 and ShortCutK2 - New shortcut key1 and key2

  Sets the shortcut of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const ShortCutK1, ShortCutK2: TShortCut);
var
  ShiftState: NSUInteger;
  ns: NSString;
begin
  ShortcutToKeyEquivalent(ShortCutK1, ns, ShiftState);
  TCocoaMenuItem(AMenuItem.Handle).setKeyEquivalentModifierMask(ShiftState);
  TCocoaMenuItem(AMenuItem.Handle).setKeyEquivalent(ns);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetVisible
  Params:  AMenuItem - LCL menu item
           Visible   - Menu item visibility

  Sets the visibility of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetVisible(const AMenuItem: TMenuItem;
  const Visible: boolean);
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) then Exit;
  NSMenuItem(AMenuItem.Handle).setHidden( not Visible );
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetCheck
  Params:  AMenuItem - LCL menu item
           Checked   - Menu item checked
  Returns: If the function succeeds

  Sets the check of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
var
  lHandle: NSMenuItem;
  lCocoaHandle: TCocoaMenuItem absolute lHandle;
begin
  Result := Assigned(AMenuItem) and AMenuItem.HandleAllocated() and (AMenuItem.Handle<>0);
  if not Result then Exit;
  lHandle := NSMenuItem(AMenuItem.Handle);
  Result := Result and lHandle.isKindOfClass_(TCocoaMenuItem);
  if not Result then Exit;
  TCocoaWSMenuItem.Do_SetCheck(lHandle, Checked);
  lCocoaHandle.UncheckSiblings(True);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetEnable
  Params:  AMenuItem - LCL menu item
           Enabled   - Menu item enabled
  Returns: If the function succeeds

  Sets the enabled of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.SetEnable(const AMenuItem: TMenuItem;
  const Enabled: boolean): boolean;
begin
  Result:=Assigned(AMenuItem) and (AMenuItem.Handle<>0);
  if not Result then Exit;
  NSMenuItem(AMenuItem.Handle).setEnabled( Enabled );
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetRadioItem
  Params:  AMenuItem - LCL menu item
           RadioItem - Menu item has radio
  Returns: If the function succeeds

  Sets the radio behaviour of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
const
  menustate : array [Boolean] of NSInteger = (NSOffState, NSOnState);
begin
  Result:=Assigned(AMenuItem) and (AMenuItem.Handle<>0);
  if not Result then Exit;
  //todo: disable relative radio items
  NSMenuItem(AMenuItem.Handle).setOnStateImage( NSMenuRadio );
  NSMenuItem(AMenuItem.Handle).setState( menustate[RadioItem] );
end;

{ TCocoaWSPopupMenu }

{------------------------------------------------------------------------------
  Method:  TCocoaWSPopupMenu.Popup
  Params:  APopupMenu - LCL popup menu
           X, Y       - Screen coordinates to popup

  Popups menu in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X,
  Y: Integer);
//var
  //w : NSWindow;
begin
  if Assigned(APopupMenu) and (APopupMenu.Handle<>0) then
  begin
    // old method which doesn't consider position but supports 10.0+ (useless since we target 10.6+)
    {w:=NSApp.keyWindow;
    if Assigned(w) then
    begin
      NSMenu.popUpContextMenu_withEvent_forView( TCocoaMenu(APopupMenu.Handle),
        NSApp.currentEvent, NSView(w.contentView));
    end;}

    // New method for 10.6+
    TCocoaMenu(APopupMenu.Handle).popUpMenuPositioningItem_atLocation_inView(
      nil, LCLCoordsToCocoa(nil, X, Y), nil);

    APopupMenu.Close; // notify LCL popup menu
  end;
end;

end.
