{%mainunit appkit.pas}
(*
        NSMenuItem.h
        Application Kit
        Copyright (c) 1996-2005, Apple Computer, Inc.
        All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSMENUITEM_PAS_H}
{$define NSMENUITEM_PAS_H}

{$include AppKitDefines.inc}
{$include NSUserInterfaceValidation.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}

//@protocol NSMenuItem <NSObject, NSCopying, NSCoding, NSValidatedUserInterfaceItem>
//+ (void)setUsesUserKeyEquivalents:(BOOL)flag;
//+ (BOOL)usesUserKeyEquivalents;
//+ (id <NSMenuItem>)separatorItem;
//- (id)initWithTitle:(NSString *)aString action:(SEL)aSelector keyEquivalent:(NSString *)charCode;
//- (void)setMenu:(NSMenu *)menu;
//- (NSMenu *)menu;
//- (BOOL)hasSubmenu;
//- (void)setSubmenu:(NSMenu *)submenu;
//- (NSMenu *)submenu;
//- (void)setTitle:(NSString *)aString;
//- (NSString *)title;
//- (void)setAttributedTitle:(NSAttributedString*)string;
//- (NSAttributedString*)attributedTitle;
//- (BOOL)isSeparatorItem;
//- (void)setKeyEquivalent:(NSString *)aKeyEquivalent;
//- (NSString *)keyEquivalent;
//- (void)setKeyEquivalentModifierMask:(unsigned int)mask;
//- (unsigned int)keyEquivalentModifierMask;
//- (NSString *)userKeyEquivalent;
//- (unsigned int)userKeyEquivalentModifierMask;
//- (void)setMnemonicLocation:(unsigned)location;
//- (unsigned)mnemonicLocation;
//- (NSString *)mnemonic;
//- (void)setTitleWithMnemonic:(NSString *)stringWithAmpersand;
//- (void)setImage:(NSImage *)menuImage;
//- (NSImage *)image;
//- (void)setState:(int)state;
//- (int)state;
//- (void)setOnStateImage:(NSImage *)image;  // checkmark by default
//- (NSImage *)onStateImage;
//- (void)setOffStateImage:(NSImage *)image;  // none by default
//- (NSImage *)offStateImage;
//- (void)setMixedStateImage:(NSImage *)image;  // horizontal line by default?
//- (NSImage *)mixedStateImage;
//- (void)setEnabled:(BOOL)flag;
//- (BOOL)isEnabled;
//- (void) setAlternate:(BOOL)isAlternate;
//- (BOOL) isAlternate;
//- (void) setIndentationLevel:(int)indentationLevel;
//- (int) indentationLevel;
//- (void)setTarget:(id)anObject;
//- (id)target;
//- (void)setAction:(SEL)aSelector;
//- (SEL)action;
//- (void)setTag:(int)anInt;
//- (int)tag;
//- (void)setRepresentedObject:(id)anObject;
//- (id)representedObject;
//- (void) setToolTip:(NSString*)toolTip;
//- (NSString*) toolTip;
//@end
{$endif}
{$endif}

{$ifdef FORWARD}
  NSMenuItem = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSMENUITEM_PAS_C}
{$define NSMENUITEM_PAS_C}

{$include AppKitDefines.inc}
{$include NSUserInterfaceValidation.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    {.$endif}

  { NSMenuItem }

  NSMenuItem = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    class procedure setUsesUserKeyEquivalents(_flag: LongBool);
    class function usesUserKeyEquivalents: LongBool;
    constructor separatorItem;
    constructor initWithTitle_action_keyEquivalent(_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef);
    procedure setMenu(_menu: lobjc.id {NSMenu});
    function menu: lobjc.id;{NSMenu}
    //  Never call the set method directly it is there only for subclassers.
    function hasSubmenu: LongBool;
    procedure setSubmenu(_submenu: lobjc.id {NSMenu});
    function submenu: lobjc.id;{NSMenu}
    procedure setTitle(_aString: CFStringRef);
    function title: CFStringRef;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setAttributedTitle(__string: lobjc.id {NSAttributedString});
    function attributedTitle: lobjc.id;{NSAttributedString}
    {.$endif}
    function isSeparatorItem: LongBool;
    procedure setKeyEquivalent(_aKeyEquivalent: CFStringRef);
    function keyEquivalent: CFStringRef;
    procedure setKeyEquivalentModifierMask(_mask: LongWord);
    function keyEquivalentModifierMask: LongWord;
    function userKeyEquivalent: CFStringRef;
    procedure setMnemonicLocation(_location: LongWord);
    function mnemonicLocation: LongWord;
    function mnemonic: CFStringRef;
    procedure setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
    procedure setImage(_menuImage: lobjc.id {NSImage});
    function image: lobjc.id;{NSImage}
    procedure setState(_state: NSCellStateValue);
    function state: NSCellStateValue;
    procedure setOnStateImage(_image: lobjc.id {NSImage});
    //  checkmark by default
    function onStateImage: lobjc.id;{NSImage}
    procedure setOffStateImage(_image: lobjc.id {NSImage});
    //  none by default
    function offStateImage: lobjc.id;{NSImage}
    procedure setMixedStateImage(_image: lobjc.id {NSImage});
    //  horizontal line by default?
    function mixedStateImage: lobjc.id;{NSImage}
    procedure setEnabled(_flag: LongBool);
    function isEnabled: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setAlternate(_isAlternate: LongBool);
    function isAlternate: LongBool;
    procedure setIndentationLevel(_indentationLevel: Integer);
    function indentationLevel: Integer;
    {.$endif}
    procedure setTarget(_anObject: lobjc.id);
    function target: lobjc.id;
    procedure setAction(_aSelector: SEL);
    function action: SEL;
    procedure setTag(_anInt: PtrInt);
    function tag: PtrInt;
    procedure setRepresentedObject(_anObject: lobjc.id);
    function representedObject: lobjc.id;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setToolTip(_toolTip: CFStringRef);
    function toolTip: CFStringRef;
    {.$endif}
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSMenuItem_NSMenuItem = 'NSMenuItem';
  StrNSMenuItem_setUsesUserKeyEquivalents = 'setUsesUserKeyEquivalents:';
  StrNSMenuItem_usesUserKeyEquivalents = 'usesUserKeyEquivalents';
  StrNSMenuItem_separatorItem = 'separatorItem';
  StrNSMenuItem_initWithTitle_action_keyEquivalent = 'initWithTitle:action:keyEquivalent:';
  StrNSMenuItem_setMenu = 'setMenu:';
  StrNSMenuItem_menu = 'menu';
  StrNSMenuItem_hasSubmenu = 'hasSubmenu';
  StrNSMenuItem_setSubmenu = 'setSubmenu:';
  StrNSMenuItem_submenu = 'submenu';
  StrNSMenuItem_setTitle = 'setTitle:';
  StrNSMenuItem_title = 'title';
  StrNSMenuItem_setAttributedTitle = 'setAttributedTitle:';
  StrNSMenuItem_attributedTitle = 'attributedTitle';
  StrNSMenuItem_isSeparatorItem = 'isSeparatorItem';
  StrNSMenuItem_setKeyEquivalent = 'setKeyEquivalent:';
  StrNSMenuItem_keyEquivalent = 'keyEquivalent';
  StrNSMenuItem_setKeyEquivalentModifierMask = 'setKeyEquivalentModifierMask:';
  StrNSMenuItem_keyEquivalentModifierMask = 'keyEquivalentModifierMask';
  StrNSMenuItem_userKeyEquivalent = 'userKeyEquivalent';
  StrNSMenuItem_setMnemonicLocation = 'setMnemonicLocation:';
  StrNSMenuItem_mnemonicLocation = 'mnemonicLocation';
  StrNSMenuItem_mnemonic = 'mnemonic';
  StrNSMenuItem_setTitleWithMnemonic = 'setTitleWithMnemonic:';
  StrNSMenuItem_setImage = 'setImage:';
  StrNSMenuItem_image = 'image';
  StrNSMenuItem_setState = 'setState:';
  StrNSMenuItem_state = 'state';
  StrNSMenuItem_setOnStateImage = 'setOnStateImage:';
  StrNSMenuItem_onStateImage = 'onStateImage';
  StrNSMenuItem_setOffStateImage = 'setOffStateImage:';
  StrNSMenuItem_offStateImage = 'offStateImage';
  StrNSMenuItem_setMixedStateImage = 'setMixedStateImage:';
  StrNSMenuItem_mixedStateImage = 'mixedStateImage';
  StrNSMenuItem_setEnabled = 'setEnabled:';
  StrNSMenuItem_isEnabled = 'isEnabled';
  StrNSMenuItem_setAlternate = 'setAlternate:';
  StrNSMenuItem_isAlternate = 'isAlternate';
  StrNSMenuItem_setIndentationLevel = 'setIndentationLevel:';
  StrNSMenuItem_indentationLevel = 'indentationLevel';
  StrNSMenuItem_setTarget = 'setTarget:';
  StrNSMenuItem_target = 'target';
  StrNSMenuItem_setAction = 'setAction:';
  StrNSMenuItem_action = 'action';
  StrNSMenuItem_setTag = 'setTag:';
  StrNSMenuItem_tag = 'tag';
  StrNSMenuItem_setRepresentedObject = 'setRepresentedObject:';
  StrNSMenuItem_representedObject = 'representedObject';
  StrNSMenuItem_setToolTip = 'setToolTip:';
  StrNSMenuItem_toolTip = 'toolTip';

{ NSMenuItem }

class function NSMenuItem.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMenuItem_NSMenuItem);
end;

class procedure NSMenuItem.setUsesUserKeyEquivalents(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSMenuItem_setUsesUserKeyEquivalents)), _flag);
end;

class function NSMenuItem.usesUserKeyEquivalents: LongBool;
begin
  Result := LongBool(objc_msgSend(getClass, sel_registerName(PChar(StrNSMenuItem_usesUserKeyEquivalents)), []));
end;

constructor NSMenuItem.separatorItem;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSMenuItem_separatorItem)));
end;

constructor NSMenuItem.initWithTitle_action_keyEquivalent(_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aString: CFStringRef; _aSelector: SEL; _charCode: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMenuItem_initWithTitle_action_keyEquivalent)), _aString, _aSelector, _charCode);
end;

procedure NSMenuItem.setMenu(_menu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setMenu)), _menu);
end;

function NSMenuItem.menu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_menu)), []));
end;

function NSMenuItem.hasSubmenu: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_hasSubmenu)), []));
end;

procedure NSMenuItem.setSubmenu(_submenu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_submenu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setSubmenu)), _submenu);
end;

function NSMenuItem.submenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_submenu)), []));
end;

procedure NSMenuItem.setTitle(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setTitle)), _aString);
end;

function NSMenuItem.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_title)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSMenuItem.setAttributedTitle(__string: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setAttributedTitle)), __string);
end;

function NSMenuItem.attributedTitle: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_attributedTitle)), []));
end;

{.$endif}
function NSMenuItem.isSeparatorItem: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_isSeparatorItem)), []));
end;

procedure NSMenuItem.setKeyEquivalent(_aKeyEquivalent: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aKeyEquivalent: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setKeyEquivalent)), _aKeyEquivalent);
end;

function NSMenuItem.keyEquivalent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_keyEquivalent)), []));
end;

procedure NSMenuItem.setKeyEquivalentModifierMask(_mask: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mask: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setKeyEquivalentModifierMask)), _mask);
end;

function NSMenuItem.keyEquivalentModifierMask: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_keyEquivalentModifierMask)), []));
end;

function NSMenuItem.userKeyEquivalent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_userKeyEquivalent)), []));
end;

procedure NSMenuItem.setMnemonicLocation(_location: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_location: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setMnemonicLocation)), _location);
end;

function NSMenuItem.mnemonicLocation: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_mnemonicLocation)), []));
end;

function NSMenuItem.mnemonic: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_mnemonic)), []));
end;

procedure NSMenuItem.setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_stringWithAmpersand: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setTitleWithMnemonic)), _stringWithAmpersand);
end;

procedure NSMenuItem.setImage(_menuImage: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menuImage: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setImage)), _menuImage);
end;

function NSMenuItem.image: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_image)), []));
end;

procedure NSMenuItem.setState(_state: NSCellStateValue);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_state: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setState)), Integer(_state));
end;

function NSMenuItem.state: NSCellStateValue;
begin
  Result := NSCellStateValue(Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_state)), [])));
end;

procedure NSMenuItem.setOnStateImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setOnStateImage)), _image);
end;

function NSMenuItem.onStateImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_onStateImage)), []));
end;

procedure NSMenuItem.setOffStateImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setOffStateImage)), _image);
end;

function NSMenuItem.offStateImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_offStateImage)), []));
end;

procedure NSMenuItem.setMixedStateImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setMixedStateImage)), _image);
end;

function NSMenuItem.mixedStateImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_mixedStateImage)), []));
end;

procedure NSMenuItem.setEnabled(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setEnabled)), _flag);
end;

function NSMenuItem.isEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_isEnabled)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSMenuItem.setAlternate(_isAlternate: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_isAlternate: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setAlternate)), _isAlternate);
end;

function NSMenuItem.isAlternate: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_isAlternate)), []));
end;

procedure NSMenuItem.setIndentationLevel(_indentationLevel: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_indentationLevel: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setIndentationLevel)), _indentationLevel);
end;

function NSMenuItem.indentationLevel: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_indentationLevel)), []));
end;

{.$endif}
procedure NSMenuItem.setTarget(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setTarget)), _anObject);
end;

function NSMenuItem.target: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_target)), []));
end;

procedure NSMenuItem.setAction(_aSelector: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSelector: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setAction)), _aSelector);
end;

function NSMenuItem.action: SEL;
begin
  Result := SEL(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_action)), []));
end;

procedure NSMenuItem.setTag(_anInt: PtrInt);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: PtrInt); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setTag)), _anInt);
end;

function NSMenuItem.tag: PtrInt;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_tag)), []));
end;

procedure NSMenuItem.setRepresentedObject(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setRepresentedObject)), _anObject);
end;

function NSMenuItem.representedObject: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_representedObject)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSMenuItem.setToolTip(_toolTip: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_toolTip: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMenuItem_setToolTip)), _toolTip);
end;

function NSMenuItem.toolTip: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMenuItem_toolTip)), []));
end;

{.$endif}
{$endif}
