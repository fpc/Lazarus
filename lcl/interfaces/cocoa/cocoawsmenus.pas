{ $Id: Cocoawsmenus.pp 15309 2008-06-04 22:12:59Z vincents $}
{
 *****************************************************************************
 *                               CocoaWSMenus.pp                             *
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
unit CocoaWSMenus;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // Libs
  CocoaAll,
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  // LCL
  Controls, Forms, Menus, Graphics, LCLType, LMessages, LCLProc, Classes,
  // Widgetset
  WSMenus, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate, CocoaWSCommon, CocoaUtils;

type

  { TCocoaWSMenuItem }

  TCocoaWSMenuItem = class(TWSMenuItem)
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
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
  private
  protected
  public
  end;

  { TCocoaWSPopupMenu }

  TCocoaWSPopupMenu = class(TWSPopupMenu)
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: Integer); override;
  end;


implementation

// used from the MenuMadness example
function NSMenuCheckmark: NSImage;
begin
  Result:=NSImage.imageNamed(NSString.alloc.initWithCString('NSMenuCheckmark'));
end;

function NSMenuRadio: NSImage;
begin
  Result:=NSImage.imageNamed(NSString.alloc.initWithCString('NSMenuRadio'))
end;

function isSeparator(const ACaption: AnsiString): Boolean;
begin
  Result:=ACaption='-';
end;

function MenuCaption(const ACaption: AnsiString): AnsiString;
var
  i : Integer;
begin
  i:=Pos('&', ACaption);
  if i>0 then
    Result:=Copy(ACaption, 1, i-1)+Copy(ACaption,i+1, length(ACaption))
  else
    Result:=ACaption;
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
  Result:=HMENU(TCocoaMenu.alloc.initWithTitle(NSString.alloc.initWithCString('hello world')));
end;

{ TCocoaWSMenuItem }

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
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) or not Assigned(AMenuItem.Parent) or (AMenuItem.Parent.Handle=0) then Exit;
  ParObj:=NSObject(AMenuItem.Parent.Handle);

  if ParObj.isKindOfClass_(NSMenuItem) then begin
    item:=NSMenuItem(AMenuItem.Handle);
    if not item.hasSubmenu then item.setSubmenu(TCocoaMenu.alloc.initWithTitle(NSString.alloc.init));
    Parent:=TCocoaMenu(item.submenu);
  end else if ParObj.isKindOfClass_(NSMenu) then
    Parent:=TCocoaMenu(ParObj)
  else
    Exit;

  item:=NSMenuItem(AMenuItem.Handle);
  Parent.insertItem_atIndex(item, Parent.itemArray.count);
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
begin
  if not Assigned(AMenuItem) then Exit;

  if AMenuItem.Caption='-' then
    item:=NSMenuItem.separatorItem
  else begin
    item:=TCocoaMenuItem.alloc.initWithTitle_action_keyEquivalent(
      NSStringUtf8(AMenuItem.Caption),
      objcselector('lclItemSelected:'), NSString.alloc.init);
    item.setTarget(item);
    item.setEnabled(AMenuItem.Enabled);
  end;

  Result:=HMENU(item);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.DestroyHandle
  Params:  AMenuItem - LCL menu item

  Destroys menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin

end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetCaption
  Params:  AMenuItem - LCL menu item
           ACaption  - Menu item caption

  Sets the caption of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
  if not Assigned(AMenuItem) or (AMenuItem.Handle=0) then Exit;
  NSMenuItem(AMenuItem.Handle).setTitle( NSStringUtf8(ACaption) );
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSMenuItem.SetShortCut
  Params:  AMenuItem   - LCL menu item
           OldShortCut - Old shortcut
           NewShortCut - New shortcut

  Sets the shortcut of menu item in Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin

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
const
  menustate : array [Boolean] of NSInteger = (NSOffState, NSOnState);
begin
  Result:=Assigned(AMenuItem) and (AMenuItem.Handle<>0);
  if not Result then Exit;
  NSMenuItem(AMenuItem.Handle).setOnStateImage( NSMenuCheckmark );
  NSMenuItem(AMenuItem.Handle).setState( menustate[Checked] );
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
class procedure TCocoaWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  w : NSWindow;
begin
  // todo: there's no way to control X,Y coordinates of the Popup menu in the OSX
  // prior to 10.6. Check the if there's the method and use it, if available
  if Assigned(APopupMenu) and (APopupMenu.Handle<>0) then begin
    w:=NSApp.keyWindow;
    if Assigned(w) then
      NSMenu.popUpContextMenu_withEvent_forView( TCocoaMenu(APopupMenu.Handle),
        NSApp.currentEvent, NSView(w.contentView));
  end;
end;

end.
