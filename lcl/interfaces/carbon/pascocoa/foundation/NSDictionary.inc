{%mainunit foundation.pas}
(*	NSDictionary.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSDICTIONARY_PAS_H}
{$define NSDICTIONARY_PAS_H}


//#import <Foundation/NSObject.h>
{$endif}
{$endif}

{$ifdef FORWARD}
  NSDictionary = class;
  NSMutableDictionary = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDICTIONARY_PAS_C}
{$define NSDICTIONARY_PAS_C}

    // ***************	Immutable Dictionary	***************

  { NSDictionary }

  NSDictionary = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    function count: LongWord;
    function keyEnumerator: lobjc.id;{NSEnumerator}
    function objectForKey(_aKey: lobjc.id): lobjc.id;
    function allKeys: CFArrayRef;
    function allKeysForObject(_anObject: lobjc.id): CFArrayRef;
    function allValues: CFArrayRef;
    function description: CFStringRef;
    function descriptionInStringsFileFormat: CFStringRef;
    function descriptionWithLocale(_locale: CFDictionaryRef): CFStringRef;
    function descriptionWithLocale_indent(_locale: CFDictionaryRef; _level: LongWord): CFStringRef;
    function isEqualToDictionary(_otherDictionary: CFDictionaryRef): LongBool;
    function objectEnumerator: lobjc.id;{NSEnumerator}
    function objectsForKeys_notFoundMarker(_keys: CFArrayRef; _marker: lobjc.id): CFArrayRef;
    function writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
    function writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
    //  the atomically flag is ignored if url of a type that cannot be written atomically.
    function keysSortedByValueUsingSelector(_comparator: SEL): CFArrayRef;
    constructor dictionary;
    constructor dictionaryWithContentsOfFile(_path: CFStringRef);
    constructor dictionaryWithContentsOfURL(_url: CFURLRef);
    constructor dictionaryWithObjects_forKeys(_objects: CFArrayRef; _keys: CFArrayRef);
    constructor dictionaryWithObjects_forKeys_count(_objects: lobjc.id; _keys: lobjc.id; _count: LongWord);
    constructor dictionaryWithObjectsAndKeys(_firstObject: lobjc.id);
    constructor initWithContentsOfFile(_path: CFStringRef);
    constructor initWithContentsOfURL(_url: CFURLRef);
    constructor initWithObjects_forKeys(_objects: CFArrayRef; _keys: CFArrayRef);
    constructor initWithObjects_forKeys_count(_objects: lobjc.id; _keys: lobjc.id; _count: LongWord);
    constructor initWithObjectsAndKeys(_firstObject: lobjc.id);
    constructor initWithDictionary(_otherDictionary: CFDictionaryRef);
    constructor dictionaryWithDictionary(_dict: CFDictionaryRef);
    constructor dictionaryWithObject_forKey(__object: lobjc.id; _key: lobjc.id);
    constructor initWithDictionary_copyItems(_otherDictionary: CFDictionaryRef; _aBool: LongBool);
  end;

    // ***************	Mutable Dictionary	***************

  { NSMutableDictionary }

  NSMutableDictionary = class(NSDictionary)
  public
    class function getClass: lobjc.id; override;
    procedure removeObjectForKey(_aKey: lobjc.id);
    procedure setObject_forKey(_anObject: lobjc.id; _aKey: lobjc.id);
    procedure addEntriesFromDictionary(_otherDictionary: CFDictionaryRef);
    procedure removeAllObjects;
    procedure removeObjectsForKeys(_keyArray: CFArrayRef);
    procedure setDictionary(_otherDictionary: CFDictionaryRef);
    constructor dictionaryWithCapacity(_numItems: LongWord);
    constructor initWithCapacity(_numItems: LongWord);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSDictionary_NSDictionary = 'NSDictionary';
  StrNSDictionary_count = 'count';
  StrNSDictionary_keyEnumerator = 'keyEnumerator';
  StrNSDictionary_objectForKey = 'objectForKey:';

  StrNSDictionary_allKeys = 'allKeys';
  StrNSDictionary_allKeysForObject = 'allKeysForObject:';
  StrNSDictionary_allValues = 'allValues';
  StrNSDictionary_description = 'description';
  StrNSDictionary_descriptionInStringsFileFormat = 'descriptionInStringsFileFormat';
  StrNSDictionary_descriptionWithLocale = 'descriptionWithLocale:';
  StrNSDictionary_descriptionWithLocale_indent = 'descriptionWithLocale:indent:';
  StrNSDictionary_isEqualToDictionary = 'isEqualToDictionary:';
  StrNSDictionary_objectEnumerator = 'objectEnumerator';
  StrNSDictionary_objectsForKeys_notFoundMarker = 'objectsForKeys:notFoundMarker:';
  StrNSDictionary_writeToFile_atomically = 'writeToFile:atomically:';
  StrNSDictionary_writeToURL_atomically = 'writeToURL:atomically:';
  StrNSDictionary_keysSortedByValueUsingSelector = 'keysSortedByValueUsingSelector:';

  StrNSDictionary_dictionary = 'dictionary';
  StrNSDictionary_dictionaryWithContentsOfFile = 'dictionaryWithContentsOfFile:';
  StrNSDictionary_dictionaryWithContentsOfURL = 'dictionaryWithContentsOfURL:';
  StrNSDictionary_dictionaryWithObjects_forKeys = 'dictionaryWithObjects:forKeys:';
  StrNSDictionary_dictionaryWithObjects_forKeys_count = 'dictionaryWithObjects:forKeys:count:';
  StrNSDictionary_dictionaryWithObjectsAndKeys = 'dictionaryWithObjectsAndKeys:';
  StrNSDictionary_initWithContentsOfFile = 'initWithContentsOfFile:';
  StrNSDictionary_initWithContentsOfURL = 'initWithContentsOfURL:';
  StrNSDictionary_initWithObjects_forKeys = 'initWithObjects:forKeys:';
  StrNSDictionary_initWithObjects_forKeys_count = 'initWithObjects:forKeys:count:';
  StrNSDictionary_initWithObjectsAndKeys = 'initWithObjectsAndKeys:';
  StrNSDictionary_initWithDictionary = 'initWithDictionary:';
  StrNSDictionary_dictionaryWithDictionary = 'dictionaryWithDictionary:';
  StrNSDictionary_dictionaryWithObject_forKey = 'dictionaryWithObject:forKey:';
  StrNSDictionary_initWithDictionary_copyItems = 'initWithDictionary:copyItems:';

  StrNSMutableDictionary_NSMutableDictionary = 'NSMutableDictionary';
  StrNSMutableDictionary_removeObjectForKey = 'removeObjectForKey:';
  StrNSMutableDictionary_setObject_forKey = 'setObject:forKey:';

  StrNSMutableDictionary_addEntriesFromDictionary = 'addEntriesFromDictionary:';
  StrNSMutableDictionary_removeAllObjects = 'removeAllObjects';
  StrNSMutableDictionary_removeObjectsForKeys = 'removeObjectsForKeys:';
  StrNSMutableDictionary_setDictionary = 'setDictionary:';

  StrNSMutableDictionary_dictionaryWithCapacity = 'dictionaryWithCapacity:';
  StrNSMutableDictionary_initWithCapacity = 'initWithCapacity:';

{ NSDictionary }

class function NSDictionary.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSDictionary_NSDictionary);
end;

function NSDictionary.count: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_count)), []));
end;

function NSDictionary.keyEnumerator: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_keyEnumerator)), []));
end;

function NSDictionary.objectForKey(_aKey: lobjc.id): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aKey: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_objectForKey)), _aKey));
end;

function NSDictionary.allKeys: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_allKeys)), []));
end;

function NSDictionary.allKeysForObject(_anObject: lobjc.id): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_allKeysForObject)), _anObject));
end;

function NSDictionary.allValues: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_allValues)), []));
end;

function NSDictionary.description: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_description)), []));
end;

function NSDictionary.descriptionInStringsFileFormat: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_descriptionInStringsFileFormat)), []));
end;

function NSDictionary.descriptionWithLocale(_locale: CFDictionaryRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_locale: CFDictionaryRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_descriptionWithLocale)), _locale));
end;

function NSDictionary.descriptionWithLocale_indent(_locale: CFDictionaryRef; _level: LongWord): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_locale: CFDictionaryRef; _level: LongWord): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_descriptionWithLocale_indent)), _locale, _level));
end;

function NSDictionary.isEqualToDictionary(_otherDictionary: CFDictionaryRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherDictionary: CFDictionaryRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_isEqualToDictionary)), _otherDictionary));
end;

function NSDictionary.objectEnumerator: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSDictionary_objectEnumerator)), []));
end;

function NSDictionary.objectsForKeys_notFoundMarker(_keys: CFArrayRef; _marker: lobjc.id): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_keys: CFArrayRef; _marker: lobjc.id): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_objectsForKeys_notFoundMarker)), _keys, _marker));
end;

function NSDictionary.writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_writeToFile_atomically)), _path, _useAuxiliaryFile));
end;

function NSDictionary.writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _atomically: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_writeToURL_atomically)), _url, _atomically));
end;

function NSDictionary.keysSortedByValueUsingSelector(_comparator: SEL): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_comparator: SEL): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSDictionary_keysSortedByValueUsingSelector)), _comparator));
end;

constructor NSDictionary.dictionary;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionary)));
end;

constructor NSDictionary.dictionaryWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithContentsOfFile)), _path);
end;

constructor NSDictionary.dictionaryWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithContentsOfURL)), _url);
end;

constructor NSDictionary.dictionaryWithObjects_forKeys(_objects: CFArrayRef; _keys: CFArrayRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_objects: CFArrayRef; _keys: CFArrayRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithObjects_forKeys)), _objects, _keys);
end;

constructor NSDictionary.dictionaryWithObjects_forKeys_count(_objects: lobjc.id; _keys: lobjc.id; _count: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_objects: lobjc.id; _keys: lobjc.id; _count: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithObjects_forKeys_count)), _objects, _keys, _count);
end;

constructor NSDictionary.dictionaryWithObjectsAndKeys(_firstObject: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_firstObject: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithObjectsAndKeys)), _firstObject);
end;

constructor NSDictionary.initWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithContentsOfFile)), _path);
end;

constructor NSDictionary.initWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithContentsOfURL)), _url);
end;

constructor NSDictionary.initWithObjects_forKeys(_objects: CFArrayRef; _keys: CFArrayRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_objects: CFArrayRef; _keys: CFArrayRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithObjects_forKeys)), _objects, _keys);
end;

constructor NSDictionary.initWithObjects_forKeys_count(_objects: lobjc.id; _keys: lobjc.id; _count: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_objects: lobjc.id; _keys: lobjc.id; _count: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithObjects_forKeys_count)), _objects, _keys, _count);
end;

constructor NSDictionary.initWithObjectsAndKeys(_firstObject: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_firstObject: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithObjectsAndKeys)), _firstObject);
end;

constructor NSDictionary.initWithDictionary(_otherDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherDictionary: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithDictionary)), _otherDictionary);
end;

constructor NSDictionary.dictionaryWithDictionary(_dict: CFDictionaryRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_dict: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithDictionary)), _dict);
end;

constructor NSDictionary.dictionaryWithObject_forKey(__object: lobjc.id; _key: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__object: lobjc.id; _key: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSDictionary_dictionaryWithObject_forKey)), __object, _key);
end;

constructor NSDictionary.initWithDictionary_copyItems(_otherDictionary: CFDictionaryRef; _aBool: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherDictionary: CFDictionaryRef; _aBool: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSDictionary_initWithDictionary_copyItems)), _otherDictionary, _aBool);
end;

{ NSMutableDictionary }

class function NSMutableDictionary.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMutableDictionary_NSMutableDictionary);
end;

procedure NSMutableDictionary.removeObjectForKey(_aKey: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aKey: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableDictionary_removeObjectForKey)), _aKey);
end;

procedure NSMutableDictionary.setObject_forKey(_anObject: lobjc.id; _aKey: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id; _aKey: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableDictionary_setObject_forKey)), _anObject, _aKey);
end;

procedure NSMutableDictionary.addEntriesFromDictionary(_otherDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_otherDictionary: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableDictionary_addEntriesFromDictionary)), _otherDictionary);
end;

procedure NSMutableDictionary.removeAllObjects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableDictionary_removeAllObjects)), []);
end;

procedure NSMutableDictionary.removeObjectsForKeys(_keyArray: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_keyArray: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableDictionary_removeObjectsForKeys)), _keyArray);
end;

procedure NSMutableDictionary.setDictionary(_otherDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_otherDictionary: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableDictionary_setDictionary)), _otherDictionary);
end;

constructor NSMutableDictionary.dictionaryWithCapacity(_numItems: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_numItems: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSMutableDictionary_dictionaryWithCapacity)), _numItems);
end;

constructor NSMutableDictionary.initWithCapacity(_numItems: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_numItems: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMutableDictionary_initWithCapacity)), _numItems);
end;

{$endif}
