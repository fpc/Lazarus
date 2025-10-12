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
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  // Libs
  CocoaAll,
  MacOSAll,
  // RTL
  math,
  // LCL
  Forms, Menus, ImgList, Graphics, LCLType, LMessages, LCLProc, Classes, LCLMessageGlue,
  // Widgetset
  WSMenus,
  // LCL Cocoa
  CocoaConfig, CocoaInt, CocoaMenus, CocoaWSCommon, CocoaUtils, CocoaGDIObjects;

type

  { TLCLMenuItemCallback }

  TLCLMenuItemCallback = class(TLCLCommonCallback, IMenuItemCallback)
  private
    FMenuItemTarget: TMenuItem;
  public
    constructor Create(AOwner: NSObject; AMenuItemTarget: TMenuItem); reintroduce;
    procedure ItemSelected;
    function MenuItemTarget: TMenuItem;
  end;

  { TCocoaWSMenuItem }

  TCocoaWSMenuItem = class(TWSMenuItem)
  private
    class procedure Do_SetCheck(const ANSMenuItem: NSMenuItem; const Checked: boolean);
    // used from the MenuMadness example
    class function NSMenuCheckmark: NSImage;
    class function NSMenuRadio: NSImage;
    class function isSeparator(const ACaption: AnsiString): Boolean;
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
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;
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

procedure NSMenuItemSetBitmap(item: TMenuItem; mn: NSMenuItem; bmp: TBitmap);

function AllocCocoaMenu(const atitle: string = ''): TCocoaMenu;

implementation

function AllocCocoaMenu(const atitle: string = ''): TCocoaMenu;
begin
  Result := TCocoaMenu.alloc.initWithTitle(ControlTitleToNSStr(atitle));
  Result.setAutoenablesItems(false);
end;

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

{ TCocoaWSMenu }

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenu.CreateHandle
  Params:  AMenu - LCL menu
  Returns: Handle to the menu in Cocoa interface

  Creates new menu in Cocoa interface
 ------------------------------------------------------------------------------}
class function TCocoaWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  //WriteLn(':>[TCocoaWSMenu.CreateHandle]');
  Result := HMENU(AllocCocoaMenu);
end;

{ TCocoaWSMainMenu }

class function TCocoaWSMainMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := HMENU(AllocCocoaMenu);
end;

{ TCocoaWSMenuItem }

class procedure TCocoaWSMenuItem.Do_SetCheck(const ANSMenuItem: NSMenuItem; const Checked: boolean);
const
  menustate : array [Boolean] of NSInteger = (NSOffState, NSOnState);
begin
  ANSMenuItem.setState( menustate[Checked] );
end;

// used from the MenuMadness example
class function TCocoaWSMenuItem.NSMenuCheckmark: NSImage;
begin
  Result:=NSImage.imageNamed(CocoaConfigMenu.menuItem.defaultCheckImageName);
end;

class function TCocoaWSMenuItem.NSMenuRadio: NSImage;
begin
  Result:=NSImage.imageNamed(CocoaConfigMenu.menuItem.defaultRadioImageName);
end;

class function TCocoaWSMenuItem.isSeparator(const ACaption: AnsiString): Boolean;
begin
  Result:=ACaption='-';
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
  MenuObj : NSObject;
  Menu    : NSMenu;
  idx     : Integer;
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) or not Assigned(AMenuItem.Parent) or (AMenuItem.Parent.Handle=0) then Exit;
  ParObj:=NSObject(AMenuItem.Parent.Handle);
  item:=NSMenuItem(AMenuItem.Handle);

  if ParObj.isKindOfClass(NSMenuItem) then
  begin
    if not NSMenuItem(ParObj).hasSubmenu then
    begin
      Parent := AllocCocoaMenu(AMenuItem.Parent.Caption);
      Parent.setDelegate(TCocoaMenuItem(ParObj));
      NSMenuItem(ParObj).setSubmenu(Parent);
      Parent.release;

      // no longer respond to clicks. LCL might still need to get an event
      // yet the menu should not close
      NSMenuItem(ParObj).setAction(nil);
    end
    else
      Parent:=TCocoaMenu(NSMenuItem(ParObj).submenu);
  end else if ParObj.isKindOfClass(NSMenu) then
    Parent:=TCocoaMenu(ParObj)
  else
    Exit;

  idx := AMenuItem.MenuVisibleIndex;
  if idx < 0 then idx := Parent.numberOfItems;

  MenuObj := NSObject(AMenuItem.Handle);
  if MenuObj.isKindOfClass(NSMenuItem) then begin
    item := NSMenuItem(MenuObj);
    if idx > Parent.numberOfItems then
      idx:= Parent.numberOfItems;
    Parent.insertItem_atIndex(NSMenuItem(item), idx);
  end
  else if MenuObj.isKindOfClass(NSMenu) then
  begin
    Menu := NSMenu(MenuObj);
    item := NSMenuItem(NSMenuItem.alloc).initWithTitle_action_keyEquivalent(
      ControlTitleToNSStr(AMenuItem.Caption), nil, NSString.string_ );
    item.setSubmenu( Menu );
    Parent.insertItem_atIndex(NSMenuItem(item), idx);
    item.release;
  end;
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
    item := LCLMenuItemInit(TCocoaMenuItem.alloc, AMenuItem);
    TCocoaMenuItem(item).FMenuItemTarget := AMenuItem;

    if AMenuItem.IsInMenuBar then
    begin
      ANSMenu := AllocCocoaMenu(AMenuItem.Caption);
      ANSMenu.setDelegate(TCocoaMenuItem(item));
      item.setSubmenu(ANSMenu);
      ANSMenu.release;
    end;

    TCocoaMenuItem(item).menuItemCallback:=TLCLMenuItemCallback.Create(item, AMenuItem);

    // initial set of properties
    {$ifdef BOOLFIX}
    item.setEnabled_(Ord(AMenuItem.Enabled));
    {$else}
    item.setEnabled(AMenuItem.Enabled);
    {$endif}

    if AMenuItem.RadioItem then
      item.setOnStateImage( NSMenuRadio )
    else
      item.setOnStateImage(NSMenuCheckmark);

    Do_SetCheck(item, AMenuItem.Checked);

    if AMenuItem.HasIcon and ((AMenuItem.ImageIndex>=0) or (AMenuItem.HasBitmap)) then
      NSMenuItemSetBitmap(AMenuItem, item, AMenuItem.Bitmap);
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
  item     : NSObject;
  menuitem : TCocoaMenuItem;
  nsitem   : NSMenuItem;
begin
  item:=NSObject(AMenuItem.Handle);
  if item.isKindOfClass_(TCocoaMenuItem) then
  begin
    menuitem := TCocoaMenuItem(item);
    callback := menuitem.lclGetCallback;
    if Assigned(callback) then
    begin
      callbackObject := callback.GetCallbackObject;
      callback := nil;
      menuitem.lclClearCallback;
      callbackObject.Free;
    end;
    if Assigned(menuitem.menu) then
      menuitem.menu.removeItem(menuitem);
    AMenuItem.Handle := 0;
    menuitem.release; // TCocoaMenuItems are "alloced" - thus should be released;
  end else if item.isKindOfClass_(NSMenuItem) then begin
    nsitem := NSMenuItem(item);
    if nsitem.isSeparatorItem and Assigned(nsitem.menu) then
      nsitem.menu.removeItem(nsitem);
    // separator items are not "alloced", thus should not be released
  end else if item.isKindOfClass_(TCocoaMenu) then begin
    item.release;
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
  if NSMenuItem(AMenuItem.Handle).isSeparatorItem <> (ACaption='-') then
    AMenuItem.RecreateHandle
  else
  begin
    s := ACaption;
    DeleteAmpersands(s);
    ns:=NSStringUtf8(s);
    NSMenuItem(AMenuItem.Handle).setTitle(ns);
    if NSMenuItem(AMenuItem.Handle).hasSubmenu then
      NSMenuItem(AMenuItem.Handle).submenu.setTitle(ns);
    ns.release;
  end;
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
  {$ifdef BOOLFIX}
  NSMenuItem(AMenuItem.Handle).setHidden_( Ord(not Visible) );
  {$else}
  NSMenuItem(AMenuItem.Handle).setHidden( not Visible );
  {$endif}
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
  {$ifdef BOOLFIX}
  NSMenuItem(AMenuItem.Handle).setEnabled_( Ord(Enabled) );
  {$else}
  NSMenuItem(AMenuItem.Handle).setEnabled( Enabled );
  {$endif}
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
  if RadioItem then
    NSMenuItem(AMenuItem.Handle).setOnStateImage( NSMenuRadio )
  else
    NSMenuItem(AMenuItem.Handle).setOnStateImage(NSMenuCheckmark);

  NSMenuItem(AMenuItem.Handle).setState( menustate[RadioItem] );
end;

procedure NSMenuItemSetBitmap(item: TMenuItem; mn: NSMenuItem; bmp: TBitmap);
var
  image: NSImage;
  imageWidth: Integer;
  size: NSSize;
  list: TCustomImageList;
begin
  if not Assigned(mn) then Exit;
  if not Assigned(bmp) or (bmp.Handle = 0) then begin
    mn.setImage(nil);
    Exit;
  end;

  image:= TCocoaBitmap(bmp.Handle).Image;
  size:= image.size;
  item.GetImageList(list, imageWidth);
  if imageWidth = 0 then
    imageWidth:= Round(size.width);
  if Round(size.width) = imageWidth then begin
    mn.setImage(image);
    Exit;
  end;

  size.width:= imageWidth;
  size.height:= imageWidth;
  image:= image.copy;
  image.setSize(size);
  mn.setImage(image);
  image.release;
end;

class procedure TCocoaWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: TBitmap);
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) then Exit;

  if NSObject(AMenuItem.Handle).isKindOfClass(NSMenuItem) then
    NSMenuItemSetBitmap(AMenuItem, NSMenuItem(AMenuItem.Handle), AIcon);
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
var
  menu: TCocoaMenu;
  point: TPoint;
  screen: NSScreen;
  mouseY: CGFloat;
  menuY: CGFloat;
begin
  if (not Assigned(APopupMenu)) or (APopupMenu.Handle=0) then
    exit;

  menu := TCocoaMenu(APopupMenu.Handle);
  point:= TPoint.Create( x, y );
  screen:= getScreenFromHMonitor( CocoaWidgetSet.MonitorFromPoint(point, MONITOR_DEFAULTTONULL) );

  mouseY:= NSGlobalScreenBottom - y;
  if Assigned(screen) then begin
    menuY:= screen.visibleFrame.origin.y + menu.size.height + 1;
    menuY:= max( mouseY, MenuY );
  end else begin
    menuY:= mouseY;
  end;

  menu.popUpMenuPositioningItem_atLocation_inView(nil, NSMakePoint(x,menuY), nil);
  APopupMenu.Close; // notify LCL popup menu
end;


function CreateMenuItemHandle(const AMenuItem: TMenuItem): NSMenuItem;
begin
  Result:= NSMenuItem( TCocoaWSMenuItem.CreateHandle(AMenuItem) );
end;

initialization
  CocoaMenus.menuItemHandleCreateFunc:= @CreateMenuItemHandle;

end.
