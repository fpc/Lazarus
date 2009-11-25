{%mainunit appkit.pas}
(*
	NSToolbarItem.h
	Application Kit
	Copyright (c) 2000-2004, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSTOOLBARITEM_PAS_H}
{$define NSTOOLBARITEM_PAS_H}

{$include NSUserInterfaceValidation.inc}
{$include NSText.inc}
{$include NSToolbar.inc}

var
  NSToolbarSeparatorItemIdentifier: CFStringRef; cvar; external;
  NSToolbarSpaceItemIdentifier: CFStringRef; cvar; external;
  NSToolbarFlexibleSpaceItemIdentifier: CFStringRef; cvar; external;
  NSToolbarShowColorsItemIdentifier: CFStringRef; cvar; external;        // Shows the color panel.
  NSToolbarShowFontsItemIdentifier: CFStringRef; cvar; external;         // Shows the font panel.
  NSToolbarCustomizeToolbarItemIdentifier: CFStringRef; cvar; external;  // Puts the current toolbar into customize mode.
  NSToolbarPrintItemIdentifier: CFStringRef; cvar; external;             // Sends printDocument: to firstResponder, but you can change this in toolbarWillAddItem: if you need to do so.

const
  NSToolbarItemVisibilityPriorityStandard = 0;
  NSToolbarItemVisibilityPriorityLow = 1000;
  NSToolbarItemVisibilityPriorityHigh = 1000;
  NSToolbarItemVisibilityPriorityUser = 2000;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSToolbarItem = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOOLBARITEM_PAS_C}
{$define NSTOOLBARITEM_PAS_C}

{$include NSUserInterfaceValidation.inc}
{$include NSText.inc}
{$include NSToolbar.inc}

  { NSToolbarItem }

  NSToolbarItem = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    constructor initWithItemIdentifier(_itemIdentifier: CFStringRef);
    //  Initialize the toolbar item with an identifier which is a development language string
    // used by the toolbar and its delegate for identification purposes. 
    function itemIdentifier: CFStringRef;
    function toolbar: lobjc.id;{NSToolbar}
    //  Use this to determine the toolbar in which an item is currently displayed.
    //  ----- Setters and Getters -----
    procedure setLabel(__label: CFStringRef);
    function label_: CFStringRef;
    //  Use this to set the item's label that appears in the toolbar.  The implication here
    // is that the toolbar will draw the label for the item, and a redraw is triggered by
    // this method.  The toolbar is in charge of the label area.  It is fine for an item
    // to have no toolbar label.  Also, developers should make sure the length of the label
    // is appropriate and not too long. 
    procedure setPaletteLabel(_paletteLabel: CFStringRef);
    function paletteLabel: CFStringRef;
    //  Use this to set the item's label that appears when the item is in the customization
    // palette.  All Items must have a palette label, and for most things it is reasonable
    // to set them to the same string as the label used in the toolbar.
    procedure setToolTip(_toolTip: CFStringRef);
    function toolTip: CFStringRef;
    //  Use this to set a tooltip to be used when the item is displayed in the toolbar.
    //  (forwards to -view if it responds)
    procedure setMenuFormRepresentation(_menuItem: lobjc.id {NSMenuItem});
    function menuFormRepresentation: lobjc.id;{NSMenuItem}
    //  The menu form of a toolbar item's purpose is twofold.  First, when the window is
    // too small to display an item, it will be clipped but remain accessible from a "clipped
    // items" menu containing the menu item returned here.  Second, in text only mode, the
    // menu returned will be used to create the displayed items.  Singleton menu items will
    // be clickable, while submenu items will be represented as a pull down.  For instance,
    // say you want a button that allows you to switch between modes A, B, and C.  You could
    // represent this as a menu by :  a menu item "mode" with three submenu items "A", "B",
    // and "C".   By default, this method returns a singleton menu item with item label
    // as the title.  For standard items, the target, action is set. 
    procedure setTag(_tag: Integer);
    function tag: Integer;
    //  Tag for your own custom purpose. (forwards to -view if it responds)
    procedure setTarget(_target: lobjc.id);
    function target: lobjc.id;
    //  Set and get the action of an item. (forwards to -view if it responds)
    procedure setAction(_action: SEL);
    function action: SEL;
    //  Set and get the action of an item. For custom views, this method will call setAction:/action
    // on the view if it responds. (forwards to -view if it responds)
    procedure setEnabled(_enabled: LongBool);
    function isEnabled: LongBool;
    //  Set and get the enabled flag of an item.  For custom views, this method will call
    // setEnabled:/isEnabled on the view if it responds. (forwards to -view if it responds)
    procedure setImage(_image: lobjc.id {NSImage});
    function image: lobjc.id;{NSImage}
    //  Set and get the image of an item.  For custom views, this method will call setImage:/image
    // on the view if it responds. (forwards to -view if it responds)
    procedure setView(_view: lobjc.id {NSView});
    function view: lobjc.id;{NSView}
    //  Use setView: if you want your toolbar item to use something other than the standard.
    //  Note that, by default, many of the set/get methods will be implemented by calls
    // forwarded to the view you set, if it responds to it.  Also, your view must be archivable
    // (in order for the toolbar to make copies of your item to hand off to the config palette).
    procedure setMinSize(_size: NSSize);
    function minSize: NSSize;
    //  Unless you have already set your own custom view, you should not call this method.
    //  The min size should be small enough to look nice in all display modes.
    procedure setMaxSize(_size: NSSize);
    function maxSize: NSSize;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure setVisibilityPriority(_visibilityPriority: Integer);
    function visibilityPriority: Integer;
    {.$endif}
    //  ----- Validation of the items -----
    procedure validate;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure setAutovalidates(_resistance: LongBool);
    function autovalidates: LongBool;
    {.$endif}
    //  ----- Controlling Duplicates In The Toolbar -----
    function allowsDuplicatesInToolbar: LongBool;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSToolbarItem_NSToolbarItem = 'NSToolbarItem';
  StrNSToolbarItem_initWithItemIdentifier = 'initWithItemIdentifier:';
  StrNSToolbarItem_itemIdentifier = 'itemIdentifier';
  StrNSToolbarItem_toolbar = 'toolbar';
  StrNSToolbarItem_setLabel = 'setLabel:';
  StrNSToolbarItem_label = 'label';
  StrNSToolbarItem_setPaletteLabel = 'setPaletteLabel:';
  StrNSToolbarItem_paletteLabel = 'paletteLabel';
  StrNSToolbarItem_setToolTip = 'setToolTip:';
  StrNSToolbarItem_toolTip = 'toolTip';
  StrNSToolbarItem_setMenuFormRepresentation = 'setMenuFormRepresentation:';
  StrNSToolbarItem_menuFormRepresentation = 'menuFormRepresentation';
  StrNSToolbarItem_setTag = 'setTag:';
  StrNSToolbarItem_tag = 'tag';
  StrNSToolbarItem_setTarget = 'setTarget:';
  StrNSToolbarItem_target = 'target';
  StrNSToolbarItem_setAction = 'setAction:';
  StrNSToolbarItem_action = 'action';
  StrNSToolbarItem_setEnabled = 'setEnabled:';
  StrNSToolbarItem_isEnabled = 'isEnabled';
  StrNSToolbarItem_setImage = 'setImage:';
  StrNSToolbarItem_image = 'image';
  StrNSToolbarItem_setView = 'setView:';
  StrNSToolbarItem_view = 'view';
  StrNSToolbarItem_setMinSize = 'setMinSize:';
  StrNSToolbarItem_minSize = 'minSize';
  StrNSToolbarItem_setMaxSize = 'setMaxSize:';
  StrNSToolbarItem_maxSize = 'maxSize';
  StrNSToolbarItem_setVisibilityPriority = 'setVisibilityPriority:';
  StrNSToolbarItem_visibilityPriority = 'visibilityPriority';
  StrNSToolbarItem_validate = 'validate';
  StrNSToolbarItem_setAutovalidates = 'setAutovalidates:';
  StrNSToolbarItem_autovalidates = 'autovalidates';
  StrNSToolbarItem_allowsDuplicatesInToolbar = 'allowsDuplicatesInToolbar';

{ NSToolbarItem }

class function NSToolbarItem.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSToolbarItem_NSToolbarItem);
end;

constructor NSToolbarItem.initWithItemIdentifier(_itemIdentifier: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_itemIdentifier: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSToolbarItem_initWithItemIdentifier)), _itemIdentifier);
end;

function NSToolbarItem.itemIdentifier: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_itemIdentifier)), []));
end;

function NSToolbarItem.toolbar: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_toolbar)), []));
end;

procedure NSToolbarItem.setLabel(__label: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__label: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setLabel)), __label);
end;

function NSToolbarItem.label_: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_label)), []));
end;

procedure NSToolbarItem.setPaletteLabel(_paletteLabel: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_paletteLabel: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setPaletteLabel)), _paletteLabel);
end;

function NSToolbarItem.paletteLabel: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_paletteLabel)), []));
end;

procedure NSToolbarItem.setToolTip(_toolTip: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_toolTip: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setToolTip)), _toolTip);
end;

function NSToolbarItem.toolTip: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_toolTip)), []));
end;

procedure NSToolbarItem.setMenuFormRepresentation(_menuItem: lobjc.id {NSMenuItem});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menuItem: lobjc.id {NSMenuItem}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setMenuFormRepresentation)), _menuItem);
end;

function NSToolbarItem.menuFormRepresentation: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_menuFormRepresentation)), []));
end;

procedure NSToolbarItem.setTag(_tag: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_tag: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setTag)), _tag);
end;

function NSToolbarItem.tag: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_tag)), []));
end;

procedure NSToolbarItem.setTarget(_target: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_target: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setTarget)), _target);
end;

function NSToolbarItem.target: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_target)), []));
end;

procedure NSToolbarItem.setAction(_action: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_action: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setAction)), _action);
end;

function NSToolbarItem.action: SEL;
begin
  Result := SEL(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_action)), []));
end;

procedure NSToolbarItem.setEnabled(_enabled: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_enabled: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setEnabled)), _enabled);
end;

function NSToolbarItem.isEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_isEnabled)), []));
end;

procedure NSToolbarItem.setImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setImage)), _image);
end;

function NSToolbarItem.image: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_image)), []));
end;

procedure NSToolbarItem.setView(_view: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_view: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setView)), _view);
end;

function NSToolbarItem.view: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_view)), []));
end;

procedure NSToolbarItem.setMinSize(_size: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_size: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setMinSize)), _size);
end;

function NSToolbarItem.minSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSToolbarItem_minSize)), []);
end;

procedure NSToolbarItem.setMaxSize(_size: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_size: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setMaxSize)), _size);
end;

function NSToolbarItem.maxSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSToolbarItem_maxSize)), []);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSToolbarItem.setVisibilityPriority(_visibilityPriority: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_visibilityPriority: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setVisibilityPriority)), _visibilityPriority);
end;

function NSToolbarItem.visibilityPriority: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_visibilityPriority)), []));
end;

{.$endif}
procedure NSToolbarItem.validate;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_validate)), []);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSToolbarItem.setAutovalidates(_resistance: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_resistance: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbarItem_setAutovalidates)), _resistance);
end;

function NSToolbarItem.autovalidates: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_autovalidates)), []));
end;

{.$endif}
function NSToolbarItem.allowsDuplicatesInToolbar: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbarItem_allowsDuplicatesInToolbar)), []));
end;

{$endif}
