{%mainunit appkit.pas}
(*
	NSButton.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSBUTTON_PAS_H}
{$define NSBUTTON_PAS_H}

{$include NSControl.inc}
{$include NSButtonCell.inc}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSButton = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUTTON_PAS_C}
{$define NSBUTTON_PAS_C}

{$include NSControl.inc}
{$include NSButtonCell.inc}

  { NSButton }

  NSButton = class(NSControl)
  public
    class function getClass: lobjc.id; override;
    function title: CFStringRef;
    procedure setTitle(_aString: CFStringRef);
    function alternateTitle: CFStringRef;
    procedure setAlternateTitle(_aString: CFStringRef);
    function image: lobjc.id;{NSImage}
    procedure setImage(_image: lobjc.id {NSImage});
    function alternateImage: lobjc.id;{NSImage}
    procedure setAlternateImage(_image: lobjc.id {NSImage});
    function imagePosition: NSCellImagePosition;
    procedure setImagePosition(_aPosition: NSCellImagePosition);
    procedure setButtonType(_aType: NSButtonType);
    function state: Integer;
    procedure setState(_value: Integer);
    function isBordered: LongBool;
    procedure setBordered(_flag: LongBool);
    function isTransparent: LongBool;
    procedure setTransparent(_flag: LongBool);
    procedure setPeriodicDelay_interval(_delay: Single; _interval: Single);
    procedure getPeriodicDelay_interval(_delay: Single; _interval: Single);
    function keyEquivalent: CFStringRef;
    procedure setKeyEquivalent(_charCode: CFStringRef);
    function keyEquivalentModifierMask: LongWord;
    procedure setKeyEquivalentModifierMask(_mask: LongWord);
    procedure highlight(_flag: LongBool);
    function performKeyEquivalent(_key: lobjc.id {NSEvent}): LongBool;
    procedure setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
    function attributedTitle: lobjc.id;{NSAttributedString}
    procedure setAttributedTitle(_aString: lobjc.id {NSAttributedString});
    function attributedAlternateTitle: lobjc.id;{NSAttributedString}
    procedure setAttributedAlternateTitle(_obj: lobjc.id {NSAttributedString});
    procedure setBezelStyle(_bezelStyle: NSBezelStyle);
    function bezelStyle: NSBezelStyle;
    procedure setAllowsMixedState(_flag: LongBool);
    function allowsMixedState: LongBool;
    procedure setNextState;
    procedure setShowsBorderOnlyWhileMouseInside(_show: LongBool);
    function showsBorderOnlyWhileMouseInside: LongBool;
    procedure setSound(_aSound: lobjc.id {NSSound});
    function sound: lobjc.id;{NSSound}
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSButton_NSButton = 'NSButton';
  StrNSButton_title = 'title';
  StrNSButton_setTitle = 'setTitle:';
  StrNSButton_alternateTitle = 'alternateTitle';
  StrNSButton_setAlternateTitle = 'setAlternateTitle:';
  StrNSButton_image = 'image';
  StrNSButton_setImage = 'setImage:';
  StrNSButton_alternateImage = 'alternateImage';
  StrNSButton_setAlternateImage = 'setAlternateImage:';
  StrNSButton_imagePosition = 'imagePosition';
  StrNSButton_setImagePosition = 'setImagePosition:';
  StrNSButton_setButtonType = 'setButtonType:';
  StrNSButton_state = 'state';
  StrNSButton_setState = 'setState:';
  StrNSButton_isBordered = 'isBordered';
  StrNSButton_setBordered = 'setBordered:';
  StrNSButton_isTransparent = 'isTransparent';
  StrNSButton_setTransparent = 'setTransparent:';
  StrNSButton_setPeriodicDelay_interval = 'setPeriodicDelay:interval:';
  StrNSButton_getPeriodicDelay_interval = 'getPeriodicDelay:interval:';
  StrNSButton_keyEquivalent = 'keyEquivalent';
  StrNSButton_setKeyEquivalent = 'setKeyEquivalent:';
  StrNSButton_keyEquivalentModifierMask = 'keyEquivalentModifierMask';
  StrNSButton_setKeyEquivalentModifierMask = 'setKeyEquivalentModifierMask:';
  StrNSButton_highlight = 'highlight:';
  StrNSButton_performKeyEquivalent = 'performKeyEquivalent:';

  StrNSButton_setTitleWithMnemonic = 'setTitleWithMnemonic:';

  StrNSButton_attributedTitle = 'attributedTitle';
  StrNSButton_setAttributedTitle = 'setAttributedTitle:';
  StrNSButton_attributedAlternateTitle = 'attributedAlternateTitle';
  StrNSButton_setAttributedAlternateTitle = 'setAttributedAlternateTitle:';

  StrNSButton_setBezelStyle = 'setBezelStyle:';
  StrNSButton_bezelStyle = 'bezelStyle';

  StrNSButton_setAllowsMixedState = 'setAllowsMixedState:';
  StrNSButton_allowsMixedState = 'allowsMixedState';
  StrNSButton_setNextState = 'setNextState';

  StrNSButton_setShowsBorderOnlyWhileMouseInside = 'setShowsBorderOnlyWhileMouseInside:';
  StrNSButton_showsBorderOnlyWhileMouseInside = 'showsBorderOnlyWhileMouseInside';

  StrNSButton_setSound = 'setSound:';
  StrNSButton_sound = 'sound';

{ NSButton }

class function NSButton.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSButton_NSButton);
end;

function NSButton.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_title)), []));
end;

procedure NSButton.setTitle(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setTitle)), _aString);
end;

function NSButton.alternateTitle: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_alternateTitle)), []));
end;

procedure NSButton.setAlternateTitle(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setAlternateTitle)), _aString);
end;

function NSButton.image: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_image)), []));
end;

procedure NSButton.setImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setImage)), _image);
end;

function NSButton.alternateImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_alternateImage)), []));
end;

procedure NSButton.setAlternateImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setAlternateImage)), _image);
end;

function NSButton.imagePosition: NSCellImagePosition;
begin
  Result := NSCellImagePosition(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_imagePosition)), []));
end;

procedure NSButton.setImagePosition(_aPosition: NSCellImagePosition);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aPosition: NSCellImagePosition); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setImagePosition)), _aPosition);
end;

procedure NSButton.setButtonType(_aType: NSButtonType);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aType: NSButtonType); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setButtonType)), _aType);
end;

function NSButton.state: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_state)), []));
end;

procedure NSButton.setState(_value: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_value: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setState)), _value);
end;

function NSButton.isBordered: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_isBordered)), []));
end;

procedure NSButton.setBordered(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setBordered)), _flag);
end;

function NSButton.isTransparent: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_isTransparent)), []));
end;

procedure NSButton.setTransparent(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setTransparent)), _flag);
end;

procedure NSButton.setPeriodicDelay_interval(_delay: Single; _interval: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_delay: Single; _interval: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setPeriodicDelay_interval)), _delay, _interval);
end;

procedure NSButton.getPeriodicDelay_interval(_delay: Single; _interval: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_delay: Single; _interval: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_getPeriodicDelay_interval)), _delay, _interval);
end;

function NSButton.keyEquivalent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_keyEquivalent)), []));
end;

procedure NSButton.setKeyEquivalent(_charCode: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_charCode: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setKeyEquivalent)), _charCode);
end;

function NSButton.keyEquivalentModifierMask: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_keyEquivalentModifierMask)), []));
end;

procedure NSButton.setKeyEquivalentModifierMask(_mask: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mask: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setKeyEquivalentModifierMask)), _mask);
end;

procedure NSButton.highlight(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_highlight)), _flag);
end;

function NSButton.performKeyEquivalent(_key: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_key: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSButton_performKeyEquivalent)), _key));
end;

procedure NSButton.setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_stringWithAmpersand: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setTitleWithMnemonic)), _stringWithAmpersand);
end;

function NSButton.attributedTitle: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_attributedTitle)), []));
end;

procedure NSButton.setAttributedTitle(_aString: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setAttributedTitle)), _aString);
end;

function NSButton.attributedAlternateTitle: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_attributedAlternateTitle)), []));
end;

procedure NSButton.setAttributedAlternateTitle(_obj: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_obj: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setAttributedAlternateTitle)), _obj);
end;

procedure NSButton.setBezelStyle(_bezelStyle: NSBezelStyle);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bezelStyle: NSBezelStyle); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setBezelStyle)), _bezelStyle);
end;

function NSButton.bezelStyle: NSBezelStyle;
begin
  Result := NSBezelStyle(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_bezelStyle)), []));
end;

procedure NSButton.setAllowsMixedState(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setAllowsMixedState)), _flag);
end;

function NSButton.allowsMixedState: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_allowsMixedState)), []));
end;

procedure NSButton.setNextState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_setNextState)), []);
end;

procedure NSButton.setShowsBorderOnlyWhileMouseInside(_show: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_show: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setShowsBorderOnlyWhileMouseInside)), _show);
end;

function NSButton.showsBorderOnlyWhileMouseInside: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_showsBorderOnlyWhileMouseInside)), []));
end;

procedure NSButton.setSound(_aSound: lobjc.id {NSSound});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSound: lobjc.id {NSSound}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSButton_setSound)), _aSound);
end;

function NSButton.sound: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSButton_sound)), []));
end;

{$endif}
