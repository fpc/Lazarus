{%mainunit foundation.pas}
(*	NSValue.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSVALUE_PAS_H}
{$define NSVALUE_PAS_H}

{$include NSObject.inc}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSValue = class;
  NSNumber = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSVALUE_PAS_C}
{$define NSVALUE_PAS_C}

{$include NSObject.inc}

  { NSValue }

  NSValue = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    procedure getValue(_value: Pointer);
    function objCType: PChar;
    constructor initWithBytes_objCType(_value: Pointer; __type: PChar);
    constructor valueWithBytes_objCType(_value: Pointer; __type: PChar);
    constructor value_withObjCType(_value: Pointer; __type: PChar);
    constructor valueWithNonretainedObject(_anObject: lobjc.id);
    function nonretainedObjectValue: lobjc.id;
    constructor valueWithPointer(_pointer: Pointer);
    function pointerValue: Pointer;
    function isEqualToValue(_value: lobjc.id {NSValue}): LongBool;
  end;


  { NSNumber }

  NSNumber = class(NSValue)
  public
    class function getClass: lobjc.id; override;
    function charValue: char;
    function unsignedCharValue: byte;
    function shortValue: SmallInt;
    function unsignedShortValue: Word;
    function intValue: Integer;
    function unsignedIntValue: LongWord;
    function longValue: Integer;
    function unsignedLongValue: LongWord;
    function longLongValue: Int64;
    function unsignedLongLongValue: Int64;
    function floatValue: Single;
    function doubleValue: double;
    function boolValue: LongBool;
    function stringValue: CFStringRef;
    function compare(_otherNumber: CFNumberRef): NSComparisonResult;
    function isEqualToNumber(_number: CFNumberRef): LongBool;
    function descriptionWithLocale(_locale: CFDictionaryRef): CFStringRef;
    constructor initWithChar(_value: char);
    constructor initWithUnsignedChar(_value: byte);
    constructor initWithShort(_value: SmallInt);
    constructor initWithUnsignedShort(_value: Word);
    constructor initWithInt(_value: Integer);
    constructor initWithUnsignedInt(_value: LongWord);
    constructor initWithLong(_value: Integer);
    constructor initWithUnsignedLong(_value: LongWord);
    constructor initWithLongLong(_value: Int64);
    constructor initWithUnsignedLongLong(_value: Int64);
    constructor initWithFloat(_value: Single);
    constructor initWithDouble(_value: double);
    constructor initWithBool(_value: LongBool);
    constructor numberWithChar(_value: char);
    constructor numberWithUnsignedChar(_value: byte);
    constructor numberWithShort(_value: SmallInt);
    constructor numberWithUnsignedShort(_value: Word);
    constructor numberWithInt(_value: Integer);
    constructor numberWithUnsignedInt(_value: LongWord);
    constructor numberWithLong(_value: Integer);
    constructor numberWithUnsignedLong(_value: LongWord);
    constructor numberWithLongLong(_value: Int64);
    constructor numberWithUnsignedLongLong(_value: Int64);
    constructor numberWithFloat(_value: Single);
    constructor numberWithDouble(_value: double);
    constructor numberWithBool(_value: LongBool);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSValue_NSValue = 'NSValue';
  StrNSValue_getValue = 'getValue:';
  StrNSValue_objCType = 'objCType';

  StrNSValue_initWithBytes_objCType = 'initWithBytes:objCType:';
  StrNSValue_valueWithBytes_objCType = 'valueWithBytes:objCType:';
  StrNSValue_value_withObjCType = 'value:withObjCType:';

  StrNSValue_valueWithNonretainedObject = 'valueWithNonretainedObject:';
  StrNSValue_nonretainedObjectValue = 'nonretainedObjectValue';
  StrNSValue_valueWithPointer = 'valueWithPointer:';
  StrNSValue_pointerValue = 'pointerValue';
  StrNSValue_isEqualToValue = 'isEqualToValue:';

  StrNSNumber_NSNumber = 'NSNumber';
  StrNSNumber_charValue = 'charValue';
  StrNSNumber_unsignedCharValue = 'unsignedCharValue';
  StrNSNumber_shortValue = 'shortValue';
  StrNSNumber_unsignedShortValue = 'unsignedShortValue';
  StrNSNumber_intValue = 'intValue';
  StrNSNumber_unsignedIntValue = 'unsignedIntValue';
  StrNSNumber_longValue = 'longValue';
  StrNSNumber_unsignedLongValue = 'unsignedLongValue';
  StrNSNumber_longLongValue = 'longLongValue';
  StrNSNumber_unsignedLongLongValue = 'unsignedLongLongValue';
  StrNSNumber_floatValue = 'floatValue';
  StrNSNumber_doubleValue = 'doubleValue';
  StrNSNumber_boolValue = 'boolValue';
  StrNSNumber_stringValue = 'stringValue';
  StrNSNumber_compare = 'compare:';
  StrNSNumber_isEqualToNumber = 'isEqualToNumber:';
  StrNSNumber_descriptionWithLocale = 'descriptionWithLocale:';

  StrNSNumber_initWithChar = 'initWithChar:';
  StrNSNumber_initWithUnsignedChar = 'initWithUnsignedChar:';
  StrNSNumber_initWithShort = 'initWithShort:';
  StrNSNumber_initWithUnsignedShort = 'initWithUnsignedShort:';
  StrNSNumber_initWithInt = 'initWithInt:';
  StrNSNumber_initWithUnsignedInt = 'initWithUnsignedInt:';
  StrNSNumber_initWithLong = 'initWithLong:';
  StrNSNumber_initWithUnsignedLong = 'initWithUnsignedLong:';
  StrNSNumber_initWithLongLong = 'initWithLongLong:';
  StrNSNumber_initWithUnsignedLongLong = 'initWithUnsignedLongLong:';
  StrNSNumber_initWithFloat = 'initWithFloat:';
  StrNSNumber_initWithDouble = 'initWithDouble:';
  StrNSNumber_initWithBool = 'initWithBool:';
  StrNSNumber_numberWithChar = 'numberWithChar:';
  StrNSNumber_numberWithUnsignedChar = 'numberWithUnsignedChar:';
  StrNSNumber_numberWithShort = 'numberWithShort:';
  StrNSNumber_numberWithUnsignedShort = 'numberWithUnsignedShort:';
  StrNSNumber_numberWithInt = 'numberWithInt:';
  StrNSNumber_numberWithUnsignedInt = 'numberWithUnsignedInt:';
  StrNSNumber_numberWithLong = 'numberWithLong:';
  StrNSNumber_numberWithUnsignedLong = 'numberWithUnsignedLong:';
  StrNSNumber_numberWithLongLong = 'numberWithLongLong:';
  StrNSNumber_numberWithUnsignedLongLong = 'numberWithUnsignedLongLong:';
  StrNSNumber_numberWithFloat = 'numberWithFloat:';
  StrNSNumber_numberWithDouble = 'numberWithDouble:';
  StrNSNumber_numberWithBool = 'numberWithBool:';

{ NSValue }

class function NSValue.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSValue_NSValue);
end;

procedure NSValue.getValue(_value: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_value: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSValue_getValue)), _value);
end;

function NSValue.objCType: PChar;
begin
  Result := PChar(objc_msgSend(Handle, sel_registerName(PChar(StrNSValue_objCType)), []));
end;

constructor NSValue.initWithBytes_objCType(_value: Pointer; __type: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Pointer; __type: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSValue_initWithBytes_objCType)), _value, __type);
end;

constructor NSValue.valueWithBytes_objCType(_value: Pointer; __type: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Pointer; __type: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSValue_valueWithBytes_objCType)), _value, __type);
end;

constructor NSValue.value_withObjCType(_value: Pointer; __type: PChar);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Pointer; __type: PChar): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSValue_value_withObjCType)), _value, __type);
end;

constructor NSValue.valueWithNonretainedObject(_anObject: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSValue_valueWithNonretainedObject)), _anObject);
end;

function NSValue.nonretainedObjectValue: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSValue_nonretainedObjectValue)), []));
end;

constructor NSValue.valueWithPointer(_pointer: Pointer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pointer: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSValue_valueWithPointer)), _pointer);
end;

function NSValue.pointerValue: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSValue_pointerValue)), []));
end;

function NSValue.isEqualToValue(_value: lobjc.id {NSValue}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: lobjc.id {NSValue}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSValue_isEqualToValue)), _value));
end;

{ NSNumber }

class function NSNumber.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSNumber_NSNumber);
end;

function NSNumber.charValue: char;
begin
  Result := {%H-}char(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_charValue)), []));
end;

function NSNumber.unsignedCharValue: byte;
begin
  Result := {%H-}byte(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_unsignedCharValue)), []));
end;

function NSNumber.shortValue: SmallInt;
begin
  Result := {%H-}SmallInt(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_shortValue)), []));
end;

function NSNumber.unsignedShortValue: Word;
begin
  Result := {%H-}Word(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_unsignedShortValue)), []));
end;

function NSNumber.intValue: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_intValue)), []));
end;

function NSNumber.unsignedIntValue: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_unsignedIntValue)), []));
end;

function NSNumber.longValue: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_longValue)), []));
end;

function NSNumber.unsignedLongValue: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_unsignedLongValue)), []));
end;

function NSNumber.longLongValue: Int64;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSNumber_longLongValue)), []);
end;

function NSNumber.unsignedLongLongValue: Int64;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSNumber_unsignedLongLongValue)), []);
end;

function NSNumber.floatValue: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_floatValue)), []));
end;

function NSNumber.doubleValue: double;
begin
  Result := objc_msgSend_fpret(Handle, sel_registerName(PChar(StrNSNumber_doubleValue)), []);
end;

function NSNumber.boolValue: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_boolValue)), []));
end;

function NSNumber.stringValue: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSNumber_stringValue)), []));
end;

function NSNumber.compare(_otherNumber: CFNumberRef): NSComparisonResult;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherNumber: CFNumberRef): NSComparisonResult; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSComparisonResult(vmethod(Handle, sel_registerName(PChar(StrNSNumber_compare)), _otherNumber));
end;

function NSNumber.isEqualToNumber(_number: CFNumberRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_number: CFNumberRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSNumber_isEqualToNumber)), _number));
end;

function NSNumber.descriptionWithLocale(_locale: CFDictionaryRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_locale: CFDictionaryRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSNumber_descriptionWithLocale)), _locale));
end;

constructor NSNumber.initWithChar(_value: char);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: char): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithChar)), _value);
end;

constructor NSNumber.initWithUnsignedChar(_value: byte);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: byte): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithUnsignedChar)), _value);
end;

constructor NSNumber.initWithShort(_value: SmallInt);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: SmallInt): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithShort)), _value);
end;

constructor NSNumber.initWithUnsignedShort(_value: Word);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Word): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithUnsignedShort)), _value);
end;

constructor NSNumber.initWithInt(_value: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithInt)), _value);
end;

constructor NSNumber.initWithUnsignedInt(_value: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithUnsignedInt)), _value);
end;

constructor NSNumber.initWithLong(_value: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithLong)), _value);
end;

constructor NSNumber.initWithUnsignedLong(_value: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithUnsignedLong)), _value);
end;

constructor NSNumber.initWithLongLong(_value: Int64);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Int64): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithLongLong)), _value);
end;

constructor NSNumber.initWithUnsignedLongLong(_value: Int64);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Int64): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithUnsignedLongLong)), _value);
end;

constructor NSNumber.initWithFloat(_value: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithFloat)), _value);
end;

constructor NSNumber.initWithDouble(_value: double);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: double): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithDouble)), _value);
end;

constructor NSNumber.initWithBool(_value: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSNumber_initWithBool)), _value);
end;

constructor NSNumber.numberWithChar(_value: char);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: char): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithChar)), _value);
end;

constructor NSNumber.numberWithUnsignedChar(_value: byte);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: byte): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithUnsignedChar)), _value);
end;

constructor NSNumber.numberWithShort(_value: SmallInt);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: SmallInt): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithShort)), _value);
end;

constructor NSNumber.numberWithUnsignedShort(_value: Word);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Word): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithUnsignedShort)), _value);
end;

constructor NSNumber.numberWithInt(_value: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithInt)), _value);
end;

constructor NSNumber.numberWithUnsignedInt(_value: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithUnsignedInt)), _value);
end;

constructor NSNumber.numberWithLong(_value: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithLong)), _value);
end;

constructor NSNumber.numberWithUnsignedLong(_value: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithUnsignedLong)), _value);
end;

constructor NSNumber.numberWithLongLong(_value: Int64);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Int64): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithLongLong)), _value);
end;

constructor NSNumber.numberWithUnsignedLongLong(_value: Int64);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Int64): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithUnsignedLongLong)), _value);
end;

constructor NSNumber.numberWithFloat(_value: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithFloat)), _value);
end;

constructor NSNumber.numberWithDouble(_value: double);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: double): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithDouble)), _value);
end;

constructor NSNumber.numberWithBool(_value: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_value: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSNumber_numberWithBool)), _value);
end;

{$endif}
