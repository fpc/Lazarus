{%mainunit appkit.pas}
(*
        NSMenu.h
        Application Kit
        Copyright (c) 1996-2005, Apple Computer, Inc.
        All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSMENU_PAS_H}
{$define NSMENU_PAS_H}

{$include AppKitDefines.inc}
{$include NSMenuItem.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}

//APPKIT_EXTERN NSString *NSMenuWillSendActionNotification;
//APPKIT_EXTERN NSString *NSMenuDidSendActionNotification;
//APPKIT_EXTERN NSString *NSMenuDidAddItemNotification;
//APPKIT_EXTERN NSString *NSMenuDidRemoveItemNotification;
//APPKIT_EXTERN NSString *NSMenuDidChangeItemNotification;
//APPKIT_EXTERN NSString *NSMenuDidEndTrackingNotification    AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSMenu = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMENU_PAS_C}
{$define NSMENU_PAS_C}

{$include AppKitDefines.inc}
{$include NSMenuItem.inc}

  { NSMenu }

  NSMenu = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    class procedure setMenuZone(_aZone: NSZone);
    class function menuZone: NSZone;
    class procedure popUpContextMenu_withEvent_forView(_menu: lobjc.id {NSMenu}; _event: lobjc.id {NSEvent}; _view: lobjc.id {NSView});
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    class procedure popUpContextMenu_withEvent_forView_withFont(_menu: lobjc.id {NSMenu}; _event: lobjc.id {NSEvent}; _view: lobjc.id {NSView}; _font: lobjc.id {NSFont});
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    class procedure setMenuBarVisible(_visible: LongBool);
    class function menuBarVisible: LongBool;
    {.$endif}
    constructor initWithTitle(_aTitle: CFStringRef);
    procedure setTitle(_aString: CFStringRef);
    function title: CFStringRef;
    procedure setSupermenu(_supermenu: lobjc.id {NSMenu});
    function supermenu: lobjc.id;{NSMenu}
    //  Never call the set method directly it is there only for subclassers.
    procedure insertItem_atIndex(_newItem: lobjc.id; __index: Integer);
    procedure addItem(_newItem: lobjc.id);
    function insertItemWithTitle_action_keyEquivalent_atIndex(_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef; __index: Integer): lobjc.id;
    function addItemWithTitle_action_keyEquivalent(_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef): lobjc.id;
    procedure removeItemAtIndex(__index: Integer);
    procedure removeItem(_item: lobjc.id);
    procedure setSubmenu_forItem(_aMenu: lobjc.id {NSMenu}; _anItem: lobjc.id);
    function itemArray: CFArrayRef;
    function numberOfItems: Integer;
    function indexOfItem(__index: lobjc.id): Integer;
    function indexOfItemWithTitle(_aTitle: CFStringRef): Integer;
    function indexOfItemWithTag(_aTag: Integer): Integer;
    function indexOfItemWithRepresentedObject(__object: lobjc.id): Integer;
    function indexOfItemWithSubmenu(_submenu: lobjc.id {NSMenu}): Integer;
    function indexOfItemWithTarget_andAction(_target: lobjc.id; _actionSelector: SEL): Integer;
    function itemAtIndex(__index: Integer): lobjc.id;
    function itemWithTitle(_aTitle: CFStringRef): lobjc.id;
    function itemWithTag(_tag: Integer): lobjc.id;
    procedure setAutoenablesItems(_flag: LongBool);
    function autoenablesItems: LongBool;
    function performKeyEquivalent(_theEvent: lobjc.id {NSEvent}): LongBool;
    procedure update;
    procedure setMenuChangedMessagesEnabled(_flag: LongBool);
    function menuChangedMessagesEnabled: LongBool;
    procedure itemChanged(_item: lobjc.id);
    procedure helpRequested(_eventPtr: lobjc.id {NSEvent});
    procedure setMenuRepresentation(_menuRep: lobjc.id);
    function menuRepresentation: lobjc.id;
    procedure setContextMenuRepresentation(_menuRep: lobjc.id);
    function contextMenuRepresentation: lobjc.id;
    procedure setTearOffMenuRepresentation(_menuRep: lobjc.id);
    function tearOffMenuRepresentation: lobjc.id;
    function isTornOff: LongBool;
    //  These methods are platform specific.  They really make little sense on Windows.
    //  Their use is discouraged.
    function attachedMenu: lobjc.id;{NSMenu}
    function isAttached: LongBool;
    procedure sizeToFit;
    function locationForSubmenu(_aSubmenu: lobjc.id {NSMenu}): NSPoint;
    procedure performActionForItemAtIndex(__index: Integer);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setDelegate(_anObject: lobjc.id);
    function delegate: lobjc.id;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function menuBarHeight: Single;
    {.$endif}
    procedure submenuAction(_sender: lobjc.id);
  end;

    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    {.$endif}
{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSMenu_NSMenu = 'NSMenu';
  StrNSMenu_setMenuZone = 'setMenuZone:';
  StrNSMenu_menuZone = 'menuZone';
  StrNSMenu_popUpContextMenu_withEvent_forView = 'popUpContextMenu:withEvent:forView:';
  StrNSMenu_popUpContextMenu_withEvent_forView_withFont = 'popUpContextMenu:withEvent:forView:withFont:';
  StrNSMenu_setMenuBarVisible = 'setMenuBarVisible:';
  StrNSMenu_menuBarVisible = 'menuBarVisible';
  StrNSMenu_initWithTitle = 'initWithTitle:';
  StrNSMenu_setTitle = 'setTitle:';
  StrNSMenu_title = 'title';
  StrNSMenu_setSupermenu = 'setSupermenu:';
  StrNSMenu_supermenu = 'supermenu';
  StrNSMenu_insertItem_atIndex = 'insertItem:atIndex:';
  StrNSMenu_addItem = 'addItem:';
  StrNSMenu_insertItemWithTitle_action_keyEquivalent_atIndex = 'insertItemWithTitle:action:keyEquivalent:atIndex:';
  StrNSMenu_addItemWithTitle_action_keyEquivalent = 'addItemWithTitle:action:keyEquivalent:';
  StrNSMenu_removeItemAtIndex = 'removeItemAtIndex:';
  StrNSMenu_removeItem = 'removeItem:';
  StrNSMenu_setSubmenu_forItem = 'setSubmenu:forItem:';
  StrNSMenu_itemArray = 'itemArray';
  StrNSMenu_numberOfItems = 'numberOfItems';
  StrNSMenu_indexOfItem = 'indexOfItem:';
  StrNSMenu_indexOfItemWithTitle = 'indexOfItemWithTitle:';
  StrNSMenu_indexOfItemWithTag = 'indexOfItemWithTag:';
  StrNSMenu_indexOfItemWithRepresentedObject = 'indexOfItemWithRepresentedObject:';
  StrNSMenu_indexOfItemWithSubmenu = 'indexOfItemWithSubmenu:';
  StrNSMenu_indexOfItemWithTarget_andAction = 'indexOfItemWithTarget:andAction:';
  StrNSMenu_itemAtIndex = 'itemAtIndex:';
  StrNSMenu_itemWithTitle = 'itemWithTitle:';
  StrNSMenu_itemWithTag = 'itemWithTag:';
  StrNSMenu_setAutoenablesItems = 'setAutoenablesItems:';
  StrNSMenu_autoenablesItems = 'autoenablesItems';
  StrNSMenu_performKeyEquivalent = 'performKeyEquivalent:';
  StrNSMenu_update = 'update';
  StrNSMenu_setMenuChangedMessagesEnabled = 'setMenuChangedMessagesEnabled:';
  StrNSMenu_menuChangedMessagesEnabled = 'menuChangedMessagesEnabled';
  StrNSMenu_itemChanged = 'itemChanged:';
  StrNSMenu_helpRequested = 'helpRequested:';
  StrNSMenu_setMenuRepresentation = 'setMenuRepresentation:';
  StrNSMenu_menuRepresentation = 'menuRepresentation';
  StrNSMenu_setContextMenuRepresentation = 'setContextMenuRepresentation:';
  StrNSMenu_contextMenuRepresentation = 'contextMenuRepresentation';
  StrNSMenu_setTearOffMenuRepresentation = 'setTearOffMenuRepresentation:';
  StrNSMenu_tearOffMenuRepresentation = 'tearOffMenuRepresentation';
  StrNSMenu_isTornOff = 'isTornOff';
  StrNSMenu_attachedMenu = 'attachedMenu';
  StrNSMenu_isAttached = 'isAttached';
  StrNSMenu_sizeToFit = 'sizeToFit';
  StrNSMenu_locationForSubmenu = 'locationForSubmenu:';
  StrNSMenu_performActionForItemAtIndex = 'performActionForItemAtIndex:';
  StrNSMenu_setDelegate = 'setDelegate:';
  StrNSMenu_delegate = 'delegate';
  StrNSMenu_menuBarHeight = 'menuBarHeight';

  StrNSMenu_submenuAction = 'submenuAction:';

{ NSMenu }

class function NSMenu.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMenu_NSMenu);
end;

class procedure NSMenu.setMenuZone(_aZone: NSZone);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aZone: NSZone); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSMenu_setMenuZone)), _aZone);
end;

class function NSMenu.menuZone: NSZone;
begin
  Result := NSZone(objc_msgSend(getClass, sel_registerName(PChar(StrNSMenu_menuZone)), []));
end;

class procedure NSMenu.popUpContextMenu_withEvent_forView(_menu: lobjc.id {NSMenu}; _event: lobjc.id {NSEvent}; _view: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menu: lobjc.id {NSMenu}; _event: lobjc.id {NSEvent}; _view: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSMenu_popUpContextMenu_withEvent_forView)), _menu, _event, _view);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
class procedure NSMenu.popUpContextMenu_withEvent_forView_withFont(_menu: lobjc.id {NSMenu}; _event: lobjc.id {NSEvent}; _view: lobjc.id {NSView}; _font: lobjc.id {NSFont});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menu: lobjc.id {NSMenu}; _event: lobjc.id {NSEvent}; _view: lobjc.id {NSView}; _font: lobjc.id {NSFont}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSMenu_popUpContextMenu_withEvent_forView_withFont)), _menu, _event, _view, _font);
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
class procedure NSMenu.setMenuBarVisible(_visible: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_visible: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSMenu_setMenuBarVisible)), _visible);
end;

class function NSMenu.menuBarVisible: LongBool;
begin
  Result := LongBool(objc_msgSend(getClass, sel_registerName(PChar(StrNSMenu_menuBarVisible)), []));
end;

{.$endif}
constructor NSMenu.initWithTitle(_aTitle: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aTitle: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMenu_initWithTitle)), _aTitle);
end;

procedure NSMenu.setTitle(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setTitle)), _aString);
end;

function NSMenu.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_title)), []));
end;

procedure NSMenu.setSupermenu(_supermenu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_supermenu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setSupermenu)), _supermenu);
end;

function NSMenu.supermenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_supermenu)), []));
end;

procedure NSMenu.insertItem_atIndex(_newItem: lobjc.id; __index: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newItem: lobjc.id; __index: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_insertItem_atIndex)), _newItem, __index);
end;

procedure NSMenu.addItem(_newItem: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newItem: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_addItem)), _newItem);
end;

function NSMenu.insertItemWithTitle_action_keyEquivalent_atIndex(_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef; __index: Integer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef; __index: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSMenu_insertItemWithTitle_action_keyEquivalent_atIndex)), _aString, _aSelector, _charCode, __index));
end;

function NSMenu.addItemWithTitle_action_keyEquivalent(_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSMenu_addItemWithTitle_action_keyEquivalent)), _aString, _aSelector, _charCode));
end;

procedure NSMenu.removeItemAtIndex(__index: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__index: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_removeItemAtIndex)), __index);
end;

procedure NSMenu.removeItem(_item: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_item: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_removeItem)), _item);
end;

procedure NSMenu.setSubmenu_forItem(_aMenu: lobjc.id {NSMenu}; _anItem: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aMenu: lobjc.id {NSMenu}; _anItem: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setSubmenu_forItem)), _aMenu, _anItem);
end;

function NSMenu.itemArray: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_itemArray)), []));
end;

function NSMenu.numberOfItems: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_numberOfItems)), []));
end;

function NSMenu.indexOfItem(__index: lobjc.id): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__index: lobjc.id): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSMenu_indexOfItem)), __index));
end;

function NSMenu.indexOfItemWithTitle(_aTitle: CFStringRef): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aTitle: CFStringRef): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSMenu_indexOfItemWithTitle)), _aTitle));
end;

function NSMenu.indexOfItemWithTag(_aTag: Integer): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aTag: Integer): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSMenu_indexOfItemWithTag)), _aTag));
end;

function NSMenu.indexOfItemWithRepresentedObject(__object: lobjc.id): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__object: lobjc.id): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSMenu_indexOfItemWithRepresentedObject)), __object));
end;

function NSMenu.indexOfItemWithSubmenu(_submenu: lobjc.id {NSMenu}): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_submenu: lobjc.id {NSMenu}): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSMenu_indexOfItemWithSubmenu)), _submenu));
end;

function NSMenu.indexOfItemWithTarget_andAction(_target: lobjc.id; _actionSelector: SEL): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_target: lobjc.id; _actionSelector: SEL): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSMenu_indexOfItemWithTarget_andAction)), _target, _actionSelector));
end;

function NSMenu.itemAtIndex(__index: Integer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__index: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSMenu_itemAtIndex)), __index));
end;

function NSMenu.itemWithTitle(_aTitle: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aTitle: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSMenu_itemWithTitle)), _aTitle));
end;

function NSMenu.itemWithTag(_tag: Integer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_tag: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSMenu_itemWithTag)), _tag));
end;

procedure NSMenu.setAutoenablesItems(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setAutoenablesItems)), _flag);
end;

function NSMenu.autoenablesItems: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_autoenablesItems)), []));
end;

function NSMenu.performKeyEquivalent(_theEvent: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSMenu_performKeyEquivalent)), _theEvent));
end;

procedure NSMenu.update;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_update)), []);
end;

procedure NSMenu.setMenuChangedMessagesEnabled(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setMenuChangedMessagesEnabled)), _flag);
end;

function NSMenu.menuChangedMessagesEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_menuChangedMessagesEnabled)), []));
end;

procedure NSMenu.itemChanged(_item: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_item: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_itemChanged)), _item);
end;

procedure NSMenu.helpRequested(_eventPtr: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_eventPtr: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_helpRequested)), _eventPtr);
end;

procedure NSMenu.setMenuRepresentation(_menuRep: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menuRep: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setMenuRepresentation)), _menuRep);
end;

function NSMenu.menuRepresentation: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_menuRepresentation)), []));
end;

procedure NSMenu.setContextMenuRepresentation(_menuRep: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menuRep: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setContextMenuRepresentation)), _menuRep);
end;

function NSMenu.contextMenuRepresentation: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_contextMenuRepresentation)), []));
end;

procedure NSMenu.setTearOffMenuRepresentation(_menuRep: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menuRep: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setTearOffMenuRepresentation)), _menuRep);
end;

function NSMenu.tearOffMenuRepresentation: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_tearOffMenuRepresentation)), []));
end;

function NSMenu.isTornOff: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_isTornOff)), []));
end;

function NSMenu.attachedMenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_attachedMenu)), []));
end;

function NSMenu.isAttached: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_isAttached)), []));
end;

procedure NSMenu.sizeToFit;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_sizeToFit)), []);
end;

function NSMenu.locationForSubmenu(_aSubmenu: lobjc.id {NSMenu}): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aSubmenu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSMenu_locationForSubmenu)), _aSubmenu);
end;

procedure NSMenu.performActionForItemAtIndex(__index: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__index: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_performActionForItemAtIndex)), __index);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSMenu.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_setDelegate)), _anObject);
end;

function NSMenu.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_delegate)), []));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSMenu.menuBarHeight: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenu_menuBarHeight)), []));
end;

{.$endif}
procedure NSMenu.submenuAction(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenu_submenuAction)), _sender);
end;

{$endif}
