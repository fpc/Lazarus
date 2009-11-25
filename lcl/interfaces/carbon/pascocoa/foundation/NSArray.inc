{%mainunit foundation.pas}
(*	NSArray.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSARRAY_PAS_H}
{$define NSARRAY_PAS_H}

{$include NSObject.inc}
{$include NSRange.inc}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSArray = class;
  NSMutableArray = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSARRAY_PAS_C}
{$define NSARRAY_PAS_C}

  TProcNSArraycomparator0 = function (_param1: lobjc.id; _param2: lobjc.id; _param3: Pointer): Integer; cdecl;
  TProcNSArraycomparator1 = function (_param1: lobjc.id; _param2: lobjc.id; _param3: Pointer): Integer; cdecl;
  TProcNSMutableArraycompare2 = function (_param1: lobjc.id; _param2: lobjc.id; _param3: Pointer): Integer; cdecl;

{$include NSObject.inc}
{$include NSRange.inc}
    // ***************	Immutable Array		***************

  { NSArray }

  NSArray = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    function count: LongWord;
    function objectAtIndex(__index: LongWord): lobjc.id;
    function arrayByAddingObject(_anObject: lobjc.id): CFArrayRef;
    function arrayByAddingObjectsFromArray(_otherArray: CFArrayRef): CFArrayRef;
    function componentsJoinedByString(_separator: CFStringRef): CFStringRef;
    function containsObject(_anObject: lobjc.id): LongBool;
    function description: CFStringRef;
    function descriptionWithLocale(_locale: CFDictionaryRef): CFStringRef;
    function descriptionWithLocale_indent(_locale: CFDictionaryRef; _level: LongWord): CFStringRef;
    function firstObjectCommonWithArray(_otherArray: CFArrayRef): lobjc.id;
    procedure getObjects(_objects: lobjc.id);
    procedure getObjects_range(_objects: lobjc.id; _range: NSRange);
    function indexOfObject(_anObject: lobjc.id): LongWord;
    function indexOfObject_inRange(_anObject: lobjc.id; _range: NSRange): LongWord;
    function indexOfObjectIdenticalTo(_anObject: lobjc.id): LongWord;
    function indexOfObjectIdenticalTo_inRange(_anObject: lobjc.id; _range: NSRange): LongWord;
    function isEqualToArray(_otherArray: CFArrayRef): LongBool;
    function lastObject: lobjc.id;
    function objectEnumerator: lobjc.id;{NSEnumerator}
    function reverseObjectEnumerator: lobjc.id;{NSEnumerator}
    function sortedArrayHint: CFDataRef;
    function sortedArrayUsingFunction_context(_comparator: TProcNSArraycomparator0; _context: Pointer): CFArrayRef;
    function sortedArrayUsingFunction_context_hint(_comparator: TProcNSArraycomparator1; _context: Pointer; _hint: CFDataRef): CFArrayRef;
    function sortedArrayUsingSelector(_comparator: SEL): CFArrayRef;
    function subarrayWithRange(_range: NSRange): CFArrayRef;
    function writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
    function writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
    procedure makeObjectsPerformSelector(_aSelector: SEL);
    procedure makeObjectsPerformSelector_withObject(_aSelector: SEL; _argument: lobjc.id);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function objectsAtIndexes(_indexes: lobjc.id {NSIndexSet}): CFArrayRef;
    {.$endif}
    constructor array_;
    constructor arrayWithContentsOfFile(_path: CFStringRef);
    constructor arrayWithContentsOfURL(_url: CFURLRef);
    constructor arrayWithObject(_anObject: lobjc.id);
    constructor arrayWithObjects(_firstObj: lobjc.id);
    constructor initWithArray(__array: CFArrayRef);
    {.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    constructor initWithArray_copyItems(__array: CFArrayRef; _flag: LongBool);
    {.$endif}
    constructor initWithContentsOfFile(_path: CFStringRef);
    constructor initWithContentsOfURL(_url: CFURLRef);
    constructor initWithObjects_count(_objects: lobjc.id; _count: LongWord);
    constructor initWithObjects(_firstObj: lobjc.id);
    constructor arrayWithArray(__array: CFArrayRef);
    constructor arrayWithObjects_count(_objs: lobjc.id; _cnt: LongWord);
  end;

    // ***************	Mutable Array		***************

  { NSMutableArray }

  NSMutableArray = class(NSArray)
  public
    class function getClass: lobjc.id; override;
    procedure addObject(_anObject: lobjc.id);
    procedure insertObject_atIndex(_anObject: lobjc.id; __index: LongWord);
    procedure removeLastObject;
    procedure removeObjectAtIndex(__index: LongWord);
    procedure replaceObjectAtIndex_withObject(__index: LongWord; _anObject: lobjc.id);
    procedure addObjectsFromArray(_otherArray: CFArrayRef);
    procedure exchangeObjectAtIndex_withObjectAtIndex(_idx1: LongWord; _idx2: LongWord);
    procedure removeAllObjects;
    procedure removeObject_inRange(_anObject: lobjc.id; _range: NSRange);
    procedure removeObject(_anObject: lobjc.id);
    procedure removeObjectIdenticalTo_inRange(_anObject: lobjc.id; _range: NSRange);
    procedure removeObjectIdenticalTo(_anObject: lobjc.id);
    procedure removeObjectsFromIndices_numIndices(_indices: PLongWord; _count: LongWord);
    procedure removeObjectsInArray(_otherArray: CFArrayRef);
    procedure removeObjectsInRange(_range: NSRange);
    procedure replaceObjectsInRange_withObjectsFromArray_range(_range: NSRange; _otherArray: CFArrayRef; _otherRange: NSRange);
    procedure replaceObjectsInRange_withObjectsFromArray(_range: NSRange; _otherArray: CFArrayRef);
    procedure setArray(_otherArray: CFArrayRef);
    procedure sortUsingFunction_context(_compare: TProcNSMutableArraycompare2; _context: Pointer);
    procedure sortUsingSelector(_comparator: SEL);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure insertObjects_atIndexes(_objects: CFArrayRef; _indexes: lobjc.id {NSIndexSet});
    procedure removeObjectsAtIndexes(_indexes: lobjc.id {NSIndexSet});
    procedure replaceObjectsAtIndexes_withObjects(_indexes: lobjc.id {NSIndexSet}; _objects: CFArrayRef);
    {.$endif}
    constructor arrayWithCapacity(_numItems: LongWord);
    constructor initWithCapacity(_numItems: LongWord);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSArray_NSArray = 'NSArray';
  StrNSArray_count = 'count';
  StrNSArray_objectAtIndex = 'objectAtIndex:';

  StrNSArray_arrayByAddingObject = 'arrayByAddingObject:';
  StrNSArray_arrayByAddingObjectsFromArray = 'arrayByAddingObjectsFromArray:';
  StrNSArray_componentsJoinedByString = 'componentsJoinedByString:';
  StrNSArray_containsObject = 'containsObject:';
  StrNSArray_description = 'description';
  StrNSArray_descriptionWithLocale = 'descriptionWithLocale:';
  StrNSArray_descriptionWithLocale_indent = 'descriptionWithLocale:indent:';
  StrNSArray_firstObjectCommonWithArray = 'firstObjectCommonWithArray:';
  StrNSArray_getObjects = 'getObjects:';
  StrNSArray_getObjects_range = 'getObjects:range:';
  StrNSArray_indexOfObject = 'indexOfObject:';
  StrNSArray_indexOfObject_inRange = 'indexOfObject:inRange:';
  StrNSArray_indexOfObjectIdenticalTo = 'indexOfObjectIdenticalTo:';
  StrNSArray_indexOfObjectIdenticalTo_inRange = 'indexOfObjectIdenticalTo:inRange:';
  StrNSArray_isEqualToArray = 'isEqualToArray:';
  StrNSArray_lastObject = 'lastObject';
  StrNSArray_objectEnumerator = 'objectEnumerator';
  StrNSArray_reverseObjectEnumerator = 'reverseObjectEnumerator';
  StrNSArray_sortedArrayHint = 'sortedArrayHint';
  StrNSArray_sortedArrayUsingFunction_context = 'sortedArrayUsingFunction:context:';
  StrNSArray_sortedArrayUsingFunction_context_hint = 'sortedArrayUsingFunction:context:hint:';
  StrNSArray_sortedArrayUsingSelector = 'sortedArrayUsingSelector:';
  StrNSArray_subarrayWithRange = 'subarrayWithRange:';
  StrNSArray_writeToFile_atomically = 'writeToFile:atomically:';
  StrNSArray_writeToURL_atomically = 'writeToURL:atomically:';
  StrNSArray_makeObjectsPerformSelector = 'makeObjectsPerformSelector:';
  StrNSArray_makeObjectsPerformSelector_withObject = 'makeObjectsPerformSelector:withObject:';
  StrNSArray_objectsAtIndexes = 'objectsAtIndexes:';

  StrNSArray_array = 'array';
  StrNSArray_arrayWithContentsOfFile = 'arrayWithContentsOfFile:';
  StrNSArray_arrayWithContentsOfURL = 'arrayWithContentsOfURL:';
  StrNSArray_arrayWithObject = 'arrayWithObject:';
  StrNSArray_arrayWithObjects = 'arrayWithObjects:';
  StrNSArray_initWithArray = 'initWithArray:';
  StrNSArray_initWithArray_copyItems = 'initWithArray:copyItems:';
  StrNSArray_initWithContentsOfFile = 'initWithContentsOfFile:';
  StrNSArray_initWithContentsOfURL = 'initWithContentsOfURL:';
  StrNSArray_initWithObjects_count = 'initWithObjects:count:';
  StrNSArray_initWithObjects = 'initWithObjects:';
  StrNSArray_arrayWithArray = 'arrayWithArray:';
  StrNSArray_arrayWithObjects_count = 'arrayWithObjects:count:';

  StrNSMutableArray_NSMutableArray = 'NSMutableArray';
  StrNSMutableArray_addObject = 'addObject:';
  StrNSMutableArray_insertObject_atIndex = 'insertObject:atIndex:';
  StrNSMutableArray_removeLastObject = 'removeLastObject';
  StrNSMutableArray_removeObjectAtIndex = 'removeObjectAtIndex:';
  StrNSMutableArray_replaceObjectAtIndex_withObject = 'replaceObjectAtIndex:withObject:';

  StrNSMutableArray_addObjectsFromArray = 'addObjectsFromArray:';
  StrNSMutableArray_exchangeObjectAtIndex_withObjectAtIndex = 'exchangeObjectAtIndex:withObjectAtIndex:';
  StrNSMutableArray_removeAllObjects = 'removeAllObjects';
  StrNSMutableArray_removeObject_inRange = 'removeObject:inRange:';
  StrNSMutableArray_removeObject = 'removeObject:';
  StrNSMutableArray_removeObjectIdenticalTo_inRange = 'removeObjectIdenticalTo:inRange:';
  StrNSMutableArray_removeObjectIdenticalTo = 'removeObjectIdenticalTo:';
  StrNSMutableArray_removeObjectsFromIndices_numIndices = 'removeObjectsFromIndices:numIndices:';
  StrNSMutableArray_removeObjectsInArray = 'removeObjectsInArray:';
  StrNSMutableArray_removeObjectsInRange = 'removeObjectsInRange:';
  StrNSMutableArray_replaceObjectsInRange_withObjectsFromArray_range = 'replaceObjectsInRange:withObjectsFromArray:range:';
  StrNSMutableArray_replaceObjectsInRange_withObjectsFromArray = 'replaceObjectsInRange:withObjectsFromArray:';
  StrNSMutableArray_setArray = 'setArray:';
  StrNSMutableArray_sortUsingFunction_context = 'sortUsingFunction:context:';
  StrNSMutableArray_sortUsingSelector = 'sortUsingSelector:';
  StrNSMutableArray_insertObjects_atIndexes = 'insertObjects:atIndexes:';
  StrNSMutableArray_removeObjectsAtIndexes = 'removeObjectsAtIndexes:';
  StrNSMutableArray_replaceObjectsAtIndexes_withObjects = 'replaceObjectsAtIndexes:withObjects:';

  StrNSMutableArray_arrayWithCapacity = 'arrayWithCapacity:';
  StrNSMutableArray_initWithCapacity = 'initWithCapacity:';

{ NSArray }

class function NSArray.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSArray_NSArray);
end;

function NSArray.count: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSArray_count)), []));
end;

function NSArray.objectAtIndex(__index: LongWord): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__index: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSArray_objectAtIndex)), __index));
end;

function NSArray.arrayByAddingObject(_anObject: lobjc.id): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_arrayByAddingObject)), _anObject));
end;

function NSArray.arrayByAddingObjectsFromArray(_otherArray: CFArrayRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherArray: CFArrayRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_arrayByAddingObjectsFromArray)), _otherArray));
end;

function NSArray.componentsJoinedByString(_separator: CFStringRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_separator: CFStringRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_componentsJoinedByString)), _separator));
end;

function NSArray.containsObject(_anObject: lobjc.id): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSArray_containsObject)), _anObject));
end;

function NSArray.description: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSArray_description)), []));
end;

function NSArray.descriptionWithLocale(_locale: CFDictionaryRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_locale: CFDictionaryRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_descriptionWithLocale)), _locale));
end;

function NSArray.descriptionWithLocale_indent(_locale: CFDictionaryRef; _level: LongWord): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_locale: CFDictionaryRef; _level: LongWord): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_descriptionWithLocale_indent)), _locale, _level));
end;

function NSArray.firstObjectCommonWithArray(_otherArray: CFArrayRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherArray: CFArrayRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSArray_firstObjectCommonWithArray)), _otherArray));
end;

procedure NSArray.getObjects(_objects: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_objects: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSArray_getObjects)), _objects);
end;

procedure NSArray.getObjects_range(_objects: lobjc.id; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_objects: lobjc.id; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSArray_getObjects_range)), _objects, _range);
end;

function NSArray.indexOfObject(_anObject: lobjc.id): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSArray_indexOfObject)), _anObject));
end;

function NSArray.indexOfObject_inRange(_anObject: lobjc.id; _range: NSRange): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id; _range: NSRange): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSArray_indexOfObject_inRange)), _anObject, _range));
end;

function NSArray.indexOfObjectIdenticalTo(_anObject: lobjc.id): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSArray_indexOfObjectIdenticalTo)), _anObject));
end;

function NSArray.indexOfObjectIdenticalTo_inRange(_anObject: lobjc.id; _range: NSRange): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id; _range: NSRange): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSArray_indexOfObjectIdenticalTo_inRange)), _anObject, _range));
end;

function NSArray.isEqualToArray(_otherArray: CFArrayRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_otherArray: CFArrayRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSArray_isEqualToArray)), _otherArray));
end;

function NSArray.lastObject: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSArray_lastObject)), []));
end;

function NSArray.objectEnumerator: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSArray_objectEnumerator)), []));
end;

function NSArray.reverseObjectEnumerator: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSArray_reverseObjectEnumerator)), []));
end;

function NSArray.sortedArrayHint: CFDataRef;
begin
  Result := CFDataRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSArray_sortedArrayHint)), []));
end;

function NSArray.sortedArrayUsingFunction_context(_comparator: TProcNSArraycomparator0; _context: Pointer): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_comparator: TProcNSArraycomparator0; _context: Pointer): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_sortedArrayUsingFunction_context)), _comparator, _context));
end;

function NSArray.sortedArrayUsingFunction_context_hint(_comparator: TProcNSArraycomparator1; _context: Pointer; _hint: CFDataRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_comparator: TProcNSArraycomparator1; _context: Pointer; _hint: CFDataRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_sortedArrayUsingFunction_context_hint)), _comparator, _context, _hint));
end;

function NSArray.sortedArrayUsingSelector(_comparator: SEL): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_comparator: SEL): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_sortedArrayUsingSelector)), _comparator));
end;

function NSArray.subarrayWithRange(_range: NSRange): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRange): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_subarrayWithRange)), _range));
end;

function NSArray.writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSArray_writeToFile_atomically)), _path, _useAuxiliaryFile));
end;

function NSArray.writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _atomically: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSArray_writeToURL_atomically)), _url, _atomically));
end;

procedure NSArray.makeObjectsPerformSelector(_aSelector: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSelector: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSArray_makeObjectsPerformSelector)), _aSelector);
end;

procedure NSArray.makeObjectsPerformSelector_withObject(_aSelector: SEL; _argument: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSelector: SEL; _argument: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSArray_makeObjectsPerformSelector_withObject)), _aSelector, _argument);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSArray.objectsAtIndexes(_indexes: lobjc.id {NSIndexSet}): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_indexes: lobjc.id {NSIndexSet}): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSArray_objectsAtIndexes)), _indexes));
end;

{.$endif}
constructor NSArray.array_;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_array)));
end;

constructor NSArray.arrayWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_arrayWithContentsOfFile)), _path);
end;

constructor NSArray.arrayWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_arrayWithContentsOfURL)), _url);
end;

constructor NSArray.arrayWithObject(_anObject: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anObject: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_arrayWithObject)), _anObject);
end;

constructor NSArray.arrayWithObjects(_firstObj: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_firstObj: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_arrayWithObjects)), _firstObj);
end;

constructor NSArray.initWithArray(__array: CFArrayRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__array: CFArrayRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSArray_initWithArray)), __array);
end;

{.$ifdef  MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
constructor NSArray.initWithArray_copyItems(__array: CFArrayRef; _flag: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__array: CFArrayRef; _flag: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSArray_initWithArray_copyItems)), __array, _flag);
end;

{.$endif}
constructor NSArray.initWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSArray_initWithContentsOfFile)), _path);
end;

constructor NSArray.initWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSArray_initWithContentsOfURL)), _url);
end;

constructor NSArray.initWithObjects_count(_objects: lobjc.id; _count: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_objects: lobjc.id; _count: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSArray_initWithObjects_count)), _objects, _count);
end;

constructor NSArray.initWithObjects(_firstObj: lobjc.id);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_firstObj: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSArray_initWithObjects)), _firstObj);
end;

constructor NSArray.arrayWithArray(__array: CFArrayRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__array: CFArrayRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_arrayWithArray)), __array);
end;

constructor NSArray.arrayWithObjects_count(_objs: lobjc.id; _cnt: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_objs: lobjc.id; _cnt: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSArray_arrayWithObjects_count)), _objs, _cnt);
end;

{ NSMutableArray }

class function NSMutableArray.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMutableArray_NSMutableArray);
end;

procedure NSMutableArray.addObject(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_addObject)), _anObject);
end;

procedure NSMutableArray.insertObject_atIndex(_anObject: lobjc.id; __index: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id; __index: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_insertObject_atIndex)), _anObject, __index);
end;

procedure NSMutableArray.removeLastObject;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableArray_removeLastObject)), []);
end;

procedure NSMutableArray.removeObjectAtIndex(__index: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__index: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectAtIndex)), __index);
end;

procedure NSMutableArray.replaceObjectAtIndex_withObject(__index: LongWord; _anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__index: LongWord; _anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_replaceObjectAtIndex_withObject)), __index, _anObject);
end;

procedure NSMutableArray.addObjectsFromArray(_otherArray: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_otherArray: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_addObjectsFromArray)), _otherArray);
end;

procedure NSMutableArray.exchangeObjectAtIndex_withObjectAtIndex(_idx1: LongWord; _idx2: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_idx1: LongWord; _idx2: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_exchangeObjectAtIndex_withObjectAtIndex)), _idx1, _idx2);
end;

procedure NSMutableArray.removeAllObjects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableArray_removeAllObjects)), []);
end;

procedure NSMutableArray.removeObject_inRange(_anObject: lobjc.id; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObject_inRange)), _anObject, _range);
end;

procedure NSMutableArray.removeObject(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObject)), _anObject);
end;

procedure NSMutableArray.removeObjectIdenticalTo_inRange(_anObject: lobjc.id; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectIdenticalTo_inRange)), _anObject, _range);
end;

procedure NSMutableArray.removeObjectIdenticalTo(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectIdenticalTo)), _anObject);
end;

procedure NSMutableArray.removeObjectsFromIndices_numIndices(_indices: PLongWord; _count: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_indices: PLongWord; _count: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectsFromIndices_numIndices)), _indices, _count);
end;

procedure NSMutableArray.removeObjectsInArray(_otherArray: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_otherArray: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectsInArray)), _otherArray);
end;

procedure NSMutableArray.removeObjectsInRange(_range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectsInRange)), _range);
end;

procedure NSMutableArray.replaceObjectsInRange_withObjectsFromArray_range(_range: NSRange; _otherArray: CFArrayRef; _otherRange: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _otherArray: CFArrayRef; _otherRange: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_replaceObjectsInRange_withObjectsFromArray_range)), _range, _otherArray, _otherRange);
end;

procedure NSMutableArray.replaceObjectsInRange_withObjectsFromArray(_range: NSRange; _otherArray: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _otherArray: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_replaceObjectsInRange_withObjectsFromArray)), _range, _otherArray);
end;

procedure NSMutableArray.setArray(_otherArray: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_otherArray: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_setArray)), _otherArray);
end;

procedure NSMutableArray.sortUsingFunction_context(_compare: TProcNSMutableArraycompare2; _context: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_compare: TProcNSMutableArraycompare2; _context: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_sortUsingFunction_context)), _compare, _context);
end;

procedure NSMutableArray.sortUsingSelector(_comparator: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_comparator: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_sortUsingSelector)), _comparator);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSMutableArray.insertObjects_atIndexes(_objects: CFArrayRef; _indexes: lobjc.id {NSIndexSet});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_objects: CFArrayRef; _indexes: lobjc.id {NSIndexSet}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_insertObjects_atIndexes)), _objects, _indexes);
end;

procedure NSMutableArray.removeObjectsAtIndexes(_indexes: lobjc.id {NSIndexSet});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_indexes: lobjc.id {NSIndexSet}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_removeObjectsAtIndexes)), _indexes);
end;

procedure NSMutableArray.replaceObjectsAtIndexes_withObjects(_indexes: lobjc.id {NSIndexSet}; _objects: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_indexes: lobjc.id {NSIndexSet}; _objects: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableArray_replaceObjectsAtIndexes_withObjects)), _indexes, _objects);
end;

{.$endif}
constructor NSMutableArray.arrayWithCapacity(_numItems: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_numItems: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSMutableArray_arrayWithCapacity)), _numItems);
end;

constructor NSMutableArray.initWithCapacity(_numItems: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_numItems: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMutableArray_initWithCapacity)), _numItems);
end;

{$endif}
