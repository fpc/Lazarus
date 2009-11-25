{%mainunit foundation.pas}
(*	NSAttributedString.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSATTRIBUTEDSTRING_PAS_H}
{$define NSATTRIBUTEDSTRING_PAS_H}


//#import <Foundation/NSString.h>
//#import <Foundation/NSDictionary.h>
{$endif}
{$endif}

{$ifdef FORWARD}
  NSAttributedString = class;
  NSMutableAttributedString = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSATTRIBUTEDSTRING_PAS_C}
{$define NSATTRIBUTEDSTRING_PAS_C}


  { NSAttributedString }

  NSAttributedString = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    function string_: CFStringRef;
    function attributesAtIndex_effectiveRange(_location: LongWord; _range: NSRangePointer): CFDictionaryRef;
    function length: LongWord;
    function attribute_atIndex_effectiveRange(_attrName: CFStringRef; _location: LongWord; _range: NSRangePointer): lobjc.id;
    function attributedSubstringFromRange(_range: NSRange): lobjc.id;{NSAttributedString}
    function attributesAtIndex_longestEffectiveRange_inRange(_location: LongWord; _range: NSRangePointer; _rangeLimit: NSRange): CFDictionaryRef;
    function attribute_atIndex_longestEffectiveRange_inRange(_attrName: CFStringRef; _location: LongWord; _range: NSRangePointer; _rangeLimit: NSRange): lobjc.id;
    function isEqualToAttributedString(_other: lobjc.id {NSAttributedString}): LongBool;
    constructor initWithString(_str: CFStringRef);
    constructor initWithString_attributes(_str: CFStringRef; _attrs: CFDictionaryRef);
    constructor initWithAttributedString(_attrStr: lobjc.id {NSAttributedString});
    { Application Kit Additions }
    constructor initWithHTML_documentAttributes(data: lobjc.id {NSData}; docAttributes: Pointer {PNSDictionary});
  end;


  { NSMutableAttributedString }

  NSMutableAttributedString = class(NSAttributedString)
  public
    class function getClass: lobjc.id; override;
    procedure replaceCharactersInRange_withString(_range: NSRange; _str: CFStringRef);
    procedure setAttributes_range(_attrs: CFDictionaryRef; _range: NSRange);
    function mutableString: CFMutableStringRef;
    procedure addAttribute_value_range(_name: CFStringRef; _value: lobjc.id; _range: NSRange);
    procedure addAttributes_range(_attrs: CFDictionaryRef; _range: NSRange);
    procedure removeAttribute_range(_name: CFStringRef; _range: NSRange);
    procedure replaceCharactersInRange_withAttributedString(_range: NSRange; _attrString: lobjc.id {NSAttributedString});
    procedure insertAttributedString_atIndex(_attrString: lobjc.id {NSAttributedString}; _loc: LongWord);
    procedure appendAttributedString(_attrString: lobjc.id {NSAttributedString});
    procedure deleteCharactersInRange(_range: NSRange);
    procedure setAttributedString(_attrString: lobjc.id {NSAttributedString});
    procedure beginEditing;
    procedure endEditing;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSAttributedString_NSAttributedString = 'NSAttributedString';
  StrNSAttributedString_string = 'string';
  StrNSAttributedString_attributesAtIndex_effectiveRange = 'attributesAtIndex:effectiveRange:';

  StrNSAttributedString_length = 'length';
  StrNSAttributedString_attribute_atIndex_effectiveRange = 'attribute:atIndex:effectiveRange:';
  StrNSAttributedString_attributedSubstringFromRange = 'attributedSubstringFromRange:';
  StrNSAttributedString_attributesAtIndex_longestEffectiveRange_inRange = 'attributesAtIndex:longestEffectiveRange:inRange:';
  StrNSAttributedString_attribute_atIndex_longestEffectiveRange_inRange = 'attribute:atIndex:longestEffectiveRange:inRange:';
  StrNSAttributedString_isEqualToAttributedString = 'isEqualToAttributedString:';
  StrNSAttributedString_initWithString = 'initWithString:';
  StrNSAttributedString_initWithString_attributes = 'initWithString:attributes:';
  StrNSAttributedString_initWithAttributedString = 'initWithAttributedString:';

  StrNSMutableAttributedString_NSMutableAttributedString = 'NSMutableAttributedString';
  StrNSMutableAttributedString_replaceCharactersInRange_withString = 'replaceCharactersInRange:withString:';
  StrNSMutableAttributedString_setAttributes_range = 'setAttributes:range:';

  StrNSMutableAttributedString_mutableString = 'mutableString';
  StrNSMutableAttributedString_addAttribute_value_range = 'addAttribute:value:range:';
  StrNSMutableAttributedString_addAttributes_range = 'addAttributes:range:';
  StrNSMutableAttributedString_removeAttribute_range = 'removeAttribute:range:';
  StrNSMutableAttributedString_replaceCharactersInRange_withAttributedString = 'replaceCharactersInRange:withAttributedString:';
  StrNSMutableAttributedString_insertAttributedString_atIndex = 'insertAttributedString:atIndex:';
  StrNSMutableAttributedString_appendAttributedString = 'appendAttributedString:';
  StrNSMutableAttributedString_deleteCharactersInRange = 'deleteCharactersInRange:';
  StrNSMutableAttributedString_setAttributedString = 'setAttributedString:';
  StrNSMutableAttributedString_beginEditing = 'beginEditing';
  StrNSMutableAttributedString_endEditing = 'endEditing';

  { Application Kit Additions }
  StrNSAttributedString_initWithHTML_documentAttributes = 'initWithHTML:documentAttributes:';

{ NSAttributedString }

class function NSAttributedString.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSAttributedString_NSAttributedString);
end;

function NSAttributedString.string_: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSAttributedString_string)), []));
end;

function NSAttributedString.attributesAtIndex_effectiveRange(_location: LongWord; _range: NSRangePointer): CFDictionaryRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_location: LongWord; _range: NSRangePointer): CFDictionaryRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDictionaryRef(vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_attributesAtIndex_effectiveRange)), _location, _range));
end;

function NSAttributedString.length: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSAttributedString_length)), []));
end;

function NSAttributedString.attribute_atIndex_effectiveRange(_attrName: CFStringRef; _location: LongWord; _range: NSRangePointer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_attrName: CFStringRef; _location: LongWord; _range: NSRangePointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_attribute_atIndex_effectiveRange)), _attrName, _location, _range));
end;

function NSAttributedString.attributedSubstringFromRange(_range: NSRange): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRange): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_attributedSubstringFromRange)), _range));
end;

function NSAttributedString.attributesAtIndex_longestEffectiveRange_inRange(_location: LongWord; _range: NSRangePointer; _rangeLimit: NSRange): CFDictionaryRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_location: LongWord; _range: NSRangePointer; _rangeLimit: NSRange): CFDictionaryRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDictionaryRef(vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_attributesAtIndex_longestEffectiveRange_inRange)), _location, _range, _rangeLimit));
end;

function NSAttributedString.attribute_atIndex_longestEffectiveRange_inRange(_attrName: CFStringRef; _location: LongWord; _range: NSRangePointer; _rangeLimit: NSRange): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_attrName: CFStringRef; _location: LongWord; _range: NSRangePointer; _rangeLimit: NSRange): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_attribute_atIndex_longestEffectiveRange_inRange)), _attrName, _location, _range, _rangeLimit));
end;

function NSAttributedString.isEqualToAttributedString(_other: lobjc.id {NSAttributedString}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_other: lobjc.id {NSAttributedString}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_isEqualToAttributedString)), _other));
end;

constructor NSAttributedString.initWithString(_str: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_str: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSAttributedString_initWithString)), _str);
end;

constructor NSAttributedString.initWithString_attributes(_str: CFStringRef; _attrs: CFDictionaryRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_str: CFStringRef; _attrs: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSAttributedString_initWithString_attributes)), _str, _attrs);
end;

constructor NSAttributedString.initWithAttributedString(_attrStr: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_attrStr: lobjc.id {NSAttributedString}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSAttributedString_initWithAttributedString)), _attrStr);
end;

{ Application Kit Additions }

constructor NSAttributedString.initWithHTML_documentAttributes(data: lobjc.id {NSData}; docAttributes: Pointer {PNSDictionary});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL; _data: lobjc.id; _attr: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(Handle, sel_registerName(PChar(StrNSAttributedString_initWithHTML_documentAttributes)), data, docAttributes);
end;

{ NSMutableAttributedString }

class function NSMutableAttributedString.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMutableAttributedString_NSMutableAttributedString);
end;

procedure NSMutableAttributedString.replaceCharactersInRange_withString(_range: NSRange; _str: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _str: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_replaceCharactersInRange_withString)), _range, _str);
end;

procedure NSMutableAttributedString.setAttributes_range(_attrs: CFDictionaryRef; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attrs: CFDictionaryRef; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_setAttributes_range)), _attrs, _range);
end;

function NSMutableAttributedString.mutableString: CFMutableStringRef;
begin
  Result := CFMutableStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableAttributedString_mutableString)), []));
end;

procedure NSMutableAttributedString.addAttribute_value_range(_name: CFStringRef; _value: lobjc.id; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_name: CFStringRef; _value: lobjc.id; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_addAttribute_value_range)), _name, _value, _range);
end;

procedure NSMutableAttributedString.addAttributes_range(_attrs: CFDictionaryRef; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attrs: CFDictionaryRef; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_addAttributes_range)), _attrs, _range);
end;

procedure NSMutableAttributedString.removeAttribute_range(_name: CFStringRef; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_name: CFStringRef; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_removeAttribute_range)), _name, _range);
end;

procedure NSMutableAttributedString.replaceCharactersInRange_withAttributedString(_range: NSRange; _attrString: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _attrString: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_replaceCharactersInRange_withAttributedString)), _range, _attrString);
end;

procedure NSMutableAttributedString.insertAttributedString_atIndex(_attrString: lobjc.id {NSAttributedString}; _loc: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attrString: lobjc.id {NSAttributedString}; _loc: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_insertAttributedString_atIndex)), _attrString, _loc);
end;

procedure NSMutableAttributedString.appendAttributedString(_attrString: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attrString: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_appendAttributedString)), _attrString);
end;

procedure NSMutableAttributedString.deleteCharactersInRange(_range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_deleteCharactersInRange)), _range);
end;

procedure NSMutableAttributedString.setAttributedString(_attrString: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attrString: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableAttributedString_setAttributedString)), _attrString);
end;

procedure NSMutableAttributedString.beginEditing;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableAttributedString_beginEditing)), []);
end;

procedure NSMutableAttributedString.endEditing;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableAttributedString_endEditing)), []);
end;

{$endif}
