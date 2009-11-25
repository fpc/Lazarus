{%mainunit appkit.pas}
(*
	NSToolbar.h
	Application Kit
	Copyright (c) 2000-2004, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSTOOLBAR_PAS_H}
{$define NSTOOLBAR_PAS_H}

{$include AppKitDefines.inc}

type
  NSToolbarDisplayMode = (
    NSToolbarDisplayModeDefault, 
    NSToolbarDisplayModeIconAndLabel, 
    NSToolbarDisplayModeIconOnly, 
    NSToolbarDisplayModeLabelOnly
  );


type
  NSToolbarSizeMode = (
    NSToolbarSizeModeDefault, 
    NSToolbarSizeModeRegular, 
    NSToolbarSizeModeSmall
  );


//APPKIT_EXTERN NSString *NSToolbarWillAddItemNotification;
//APPKIT_EXTERN NSString *NSToolbarDidRemoveItemNotification;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSToolbar = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOOLBAR_PAS_C}
{$define NSTOOLBAR_PAS_C}

{$include AppKitDefines.inc}

  { NSToolbar }

  NSToolbar = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    constructor initWithIdentifier(_identifier: CFStringRef);
    //  The identifier is used to form the toolbar's autosave name.  Also, toolbars with
    // the same identifier are implicitly synchronized so that they maintain the same state.
    procedure insertItemWithItemIdentifier_atIndex(_itemIdentifier: CFStringRef; __index: Integer);
    procedure removeItemAtIndex(__index: Integer);
    //  Primitives for explicitly adding and removing items.  Any change made will be propogated
    // immediately to all other toolbars with the same identifier.
    procedure setDelegate(_delegate: lobjc.id);
    function delegate: lobjc.id;
    //  Customizable toolbars must have a delegate, and must implement the required NSToolbarDelegate
    // methods.
    procedure setVisible(_shown: LongBool);
    function isVisible: LongBool;
    //  toggles the visibliity of the toolbar.
    procedure runCustomizationPalette(_sender: lobjc.id);
    function customizationPaletteIsRunning: LongBool;
    //  Customizable toolbars (those with delegates) can show a palette which allows users
    // to populate the toolbar with individual items or to reset the toolbar to some default
    // set of items.  The items and item sets in the palette are specified by the delegate
    // (-toolbarAllowedItemIdentifiers: and -toolbarDefaultItemIdentifiers:).  When the
    // user is done configuring, they will dismiss the palette.
    //  ----- Toolbar Attributes  -----
    procedure setDisplayMode(_displayMode: NSToolbarDisplayMode);
    function displayMode: NSToolbarDisplayMode;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setSelectedItemIdentifier(_itemIdentifier: CFStringRef);
    function selectedItemIdentifier: CFStringRef;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure setSizeMode(_sizeMode: NSToolbarSizeMode);
    function sizeMode: NSToolbarSizeMode;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    //  Use this API to hide the baseline NSToolbar draws between itself and the main window
    // contents.  The default is YES.  This method should only be used before the toolbar
    // is attached to its window (-[NSWindow setToolbar:]).
    procedure setShowsBaselineSeparator(_flag: LongBool);
    function showsBaselineSeparator: LongBool;
    {.$endif}
    procedure setAllowsUserCustomization(_allowCustomization: LongBool);
    function allowsUserCustomization: LongBool;
    //  This flag controls whether or not users can configure the toolbar by dragging items
    // around, and whether or not the customization palette can be used.  The default value
    // is NO, but can be changed at any time.  For instance, a developer may not want users
    // to be able to edit the toolbar while some event is being processed. 
    //  ----- Accessing toolbar info -----
    function identifier: CFStringRef;
    //  All toolbars with the same name will share the same display attributes, and item
    // order.  Also, if a toolbar autosaves its configuration, the item identifier will
    // be used as the autosave name.
    function items: CFArrayRef;
    //  Allows you to access all current items in the toolbar.
    function visibleItems: CFArrayRef;
    //  Allows you to access the current visible items (non clipped).
    //  ----- Autosaving The Configuration -----
    procedure setAutosavesConfiguration(_flag: LongBool);
    function autosavesConfiguration: LongBool;
    //  If autosavesConfiguration is YES, the toolbar will automatically write changes the
    // user makes to user defaults.  Customizable toolbars will want to set this flag to
    // YES.  Setting this to NO means changes in configuration are not written automatically,
    // however you can use the configurationDictionary method to do it yourself.  Default
    // is NO.
    procedure setConfigurationFromDictionary(_configDict: CFDictionaryRef);
    function configurationDictionary: CFDictionaryRef;
    //  Set and get the current toolbar configuration using a dictionary representation.
    //  ----- Validation of the items -----
    procedure validateVisibleItems;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSToolbar_NSToolbar = 'NSToolbar';
  StrNSToolbar_initWithIdentifier = 'initWithIdentifier:';
  StrNSToolbar_insertItemWithItemIdentifier_atIndex = 'insertItemWithItemIdentifier:atIndex:';
  StrNSToolbar_removeItemAtIndex = 'removeItemAtIndex:';
  StrNSToolbar_setDelegate = 'setDelegate:';
  StrNSToolbar_delegate = 'delegate';
  StrNSToolbar_setVisible = 'setVisible:';
  StrNSToolbar_isVisible = 'isVisible';
  StrNSToolbar_runCustomizationPalette = 'runCustomizationPalette:';
  StrNSToolbar_customizationPaletteIsRunning = 'customizationPaletteIsRunning';
  StrNSToolbar_setDisplayMode = 'setDisplayMode:';
  StrNSToolbar_displayMode = 'displayMode';
  StrNSToolbar_setSelectedItemIdentifier = 'setSelectedItemIdentifier:';
  StrNSToolbar_selectedItemIdentifier = 'selectedItemIdentifier';
  StrNSToolbar_setSizeMode = 'setSizeMode:';
  StrNSToolbar_sizeMode = 'sizeMode';
  StrNSToolbar_setShowsBaselineSeparator = 'setShowsBaselineSeparator:';
  StrNSToolbar_showsBaselineSeparator = 'showsBaselineSeparator';
  StrNSToolbar_setAllowsUserCustomization = 'setAllowsUserCustomization:';
  StrNSToolbar_allowsUserCustomization = 'allowsUserCustomization';
  StrNSToolbar_identifier = 'identifier';
  StrNSToolbar_items = 'items';
  StrNSToolbar_visibleItems = 'visibleItems';
  StrNSToolbar_setAutosavesConfiguration = 'setAutosavesConfiguration:';
  StrNSToolbar_autosavesConfiguration = 'autosavesConfiguration';
  StrNSToolbar_setConfigurationFromDictionary = 'setConfigurationFromDictionary:';
  StrNSToolbar_configurationDictionary = 'configurationDictionary';
  StrNSToolbar_validateVisibleItems = 'validateVisibleItems';

{ NSToolbar }

class function NSToolbar.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSToolbar_NSToolbar);
end;

constructor NSToolbar.initWithIdentifier(_identifier: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_identifier: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSToolbar_initWithIdentifier)), _identifier);
end;

procedure NSToolbar.insertItemWithItemIdentifier_atIndex(_itemIdentifier: CFStringRef; __index: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_itemIdentifier: CFStringRef; __index: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_insertItemWithItemIdentifier_atIndex)), _itemIdentifier, __index);
end;

procedure NSToolbar.removeItemAtIndex(__index: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__index: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_removeItemAtIndex)), __index);
end;

procedure NSToolbar.setDelegate(_delegate: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_delegate: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setDelegate)), _delegate);
end;

function NSToolbar.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_delegate)), []));
end;

procedure NSToolbar.setVisible(_shown: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_shown: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setVisible)), _shown);
end;

function NSToolbar.isVisible: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_isVisible)), []));
end;

procedure NSToolbar.runCustomizationPalette(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_runCustomizationPalette)), _sender);
end;

function NSToolbar.customizationPaletteIsRunning: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_customizationPaletteIsRunning)), []));
end;

procedure NSToolbar.setDisplayMode(_displayMode: NSToolbarDisplayMode);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_displayMode: NSToolbarDisplayMode); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setDisplayMode)), _displayMode);
end;

function NSToolbar.displayMode: NSToolbarDisplayMode;
begin
  Result := NSToolbarDisplayMode(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_displayMode)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSToolbar.setSelectedItemIdentifier(_itemIdentifier: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_itemIdentifier: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setSelectedItemIdentifier)), _itemIdentifier);
end;

function NSToolbar.selectedItemIdentifier: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_selectedItemIdentifier)), []));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSToolbar.setSizeMode(_sizeMode: NSToolbarSizeMode);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sizeMode: NSToolbarSizeMode); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setSizeMode)), _sizeMode);
end;

function NSToolbar.sizeMode: NSToolbarSizeMode;
begin
  Result := NSToolbarSizeMode(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_sizeMode)), []));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSToolbar.setShowsBaselineSeparator(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setShowsBaselineSeparator)), _flag);
end;

function NSToolbar.showsBaselineSeparator: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_showsBaselineSeparator)), []));
end;

{.$endif}
procedure NSToolbar.setAllowsUserCustomization(_allowCustomization: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_allowCustomization: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setAllowsUserCustomization)), _allowCustomization);
end;

function NSToolbar.allowsUserCustomization: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_allowsUserCustomization)), []));
end;

function NSToolbar.identifier: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_identifier)), []));
end;

function NSToolbar.items: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_items)), []));
end;

function NSToolbar.visibleItems: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_visibleItems)), []));
end;

procedure NSToolbar.setAutosavesConfiguration(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setAutosavesConfiguration)), _flag);
end;

function NSToolbar.autosavesConfiguration: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_autosavesConfiguration)), []));
end;

procedure NSToolbar.setConfigurationFromDictionary(_configDict: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_configDict: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSToolbar_setConfigurationFromDictionary)), _configDict);
end;

function NSToolbar.configurationDictionary: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_configurationDictionary)), []));
end;

procedure NSToolbar.validateVisibleItems;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSToolbar_validateVisibleItems)), []);
end;

{$endif}
