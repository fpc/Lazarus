{%mainunit foundation.pas}
(*	NSData.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSDATA_PAS_H}
{$define NSDATA_PAS_H}


//#import <Foundation/NSObject.h>
//#import <Foundation/NSRange.h>
  // ***************	Read/Write Options	***************
const
  NSMappedRead = 1;
  NSUncachedRead = 2;

const
  NSAtomicWrite = 1;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSData = class;
  NSMutableData = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDATA_PAS_C}
{$define NSDATA_PAS_C}

    // ***************	Immutable Data		***************

  { NSData }

  NSData = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    function length: LongWord;
    function bytes: Pointer;
    function description: CFStringRef;
    procedure getBytes(_buffer: Pointer);
    procedure getBytes_length(_buffer: Pointer; _length: LongWord);
    procedure getBytes_range(_buffer: Pointer; _range: NSRange);
    function isEqualToData(_other: CFDataRef): LongBool;
    function subdataWithRange(_range: NSRange): CFDataRef;
    function writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
    function writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
    function writeToFile_options_error(_path: CFStringRef; _writeOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): LongBool;
    function writeToURL_options_error(_url: CFURLRef; _writeOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): LongBool;
    constructor data;
    constructor dataWithBytes_length(_bytes: Pointer; _length: LongWord);
    constructor dataWithBytesNoCopy_length(_bytes: Pointer; _length: LongWord);
    constructor dataWithBytesNoCopy_length_freeWhenDone(_bytes: Pointer; _length: LongWord; _b: LongBool);
    {.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    constructor dataWithContentsOfFile_options_error(_path: CFStringRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
    constructor dataWithContentsOfURL_options_error(_url: CFURLRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
    constructor dataWithContentsOfFile(_path: CFStringRef);
    constructor dataWithContentsOfURL(_url: CFURLRef);
    constructor dataWithContentsOfMappedFile(_path: CFStringRef);
    constructor initWithBytes_length(_bytes: Pointer; _length: LongWord);
    constructor initWithBytesNoCopy_length(_bytes: Pointer; _length: LongWord);
    constructor initWithBytesNoCopy_length_freeWhenDone(_bytes: Pointer; _length: LongWord; _b: LongBool);
    {.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
    constructor initWithContentsOfFile_options_error(_path: CFStringRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
    constructor initWithContentsOfURL_options_error(_url: CFURLRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
    constructor initWithContentsOfFile(_path: CFStringRef);
    constructor initWithContentsOfURL(_url: CFURLRef);
    constructor initWithContentsOfMappedFile(_path: CFStringRef);
    constructor initWithData(_data: CFDataRef);
    constructor dataWithData(_data: CFDataRef);
  end;

    // ***************	Mutable Data		***************

  { NSMutableData }

  NSMutableData = class(NSData)
  public
    class function getClass: lobjc.id; override;
    function mutableBytes: Pointer;
    procedure setLength(_length: LongWord);
    procedure appendBytes_length(_bytes: Pointer; _length: LongWord);
    procedure appendData(_other: CFDataRef);
    procedure increaseLengthBy(_extraLength: LongWord);
    procedure replaceBytesInRange_withBytes(_range: NSRange; _bytes: Pointer);
    procedure resetBytesInRange(_range: NSRange);
    procedure setData(_data: CFDataRef);
    procedure replaceBytesInRange_withBytes_length(_range: NSRange; _replacementBytes: Pointer; _replacementLength: LongWord);
    constructor dataWithCapacity(_aNumItems: LongWord);
    constructor dataWithLength(_length: LongWord);
    constructor initWithCapacity(_capacity: LongWord);
    constructor initWithLength(_length: LongWord);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSData_NSData = 'NSData';
  StrNSData_length = 'length';
  StrNSData_bytes = 'bytes';

  StrNSData_description = 'description';
  StrNSData_getBytes = 'getBytes:';
  StrNSData_getBytes_length = 'getBytes:length:';
  StrNSData_getBytes_range = 'getBytes:range:';
  StrNSData_isEqualToData = 'isEqualToData:';
  StrNSData_subdataWithRange = 'subdataWithRange:';
  StrNSData_writeToFile_atomically = 'writeToFile:atomically:';
  StrNSData_writeToURL_atomically = 'writeToURL:atomically:';
  StrNSData_writeToFile_options_error = 'writeToFile:options:error:';
  StrNSData_writeToURL_options_error = 'writeToURL:options:error:';

  StrNSData_data = 'data';
  StrNSData_dataWithBytes_length = 'dataWithBytes:length:';
  StrNSData_dataWithBytesNoCopy_length = 'dataWithBytesNoCopy:length:';
  StrNSData_dataWithBytesNoCopy_length_freeWhenDone = 'dataWithBytesNoCopy:length:freeWhenDone:';
  StrNSData_dataWithContentsOfFile_options_error = 'dataWithContentsOfFile:options:error:';
  StrNSData_dataWithContentsOfURL_options_error = 'dataWithContentsOfURL:options:error:';
  StrNSData_dataWithContentsOfFile = 'dataWithContentsOfFile:';
  StrNSData_dataWithContentsOfURL = 'dataWithContentsOfURL:';
  StrNSData_dataWithContentsOfMappedFile = 'dataWithContentsOfMappedFile:';
  StrNSData_initWithBytes_length = 'initWithBytes:length:';
  StrNSData_initWithBytesNoCopy_length = 'initWithBytesNoCopy:length:';
  StrNSData_initWithBytesNoCopy_length_freeWhenDone = 'initWithBytesNoCopy:length:freeWhenDone:';
  StrNSData_initWithContentsOfFile_options_error = 'initWithContentsOfFile:options:error:';
  StrNSData_initWithContentsOfURL_options_error = 'initWithContentsOfURL:options:error:';
  StrNSData_initWithContentsOfFile = 'initWithContentsOfFile:';
  StrNSData_initWithContentsOfURL = 'initWithContentsOfURL:';
  StrNSData_initWithContentsOfMappedFile = 'initWithContentsOfMappedFile:';
  StrNSData_initWithData = 'initWithData:';
  StrNSData_dataWithData = 'dataWithData:';

  StrNSMutableData_NSMutableData = 'NSMutableData';
  StrNSMutableData_mutableBytes = 'mutableBytes';
  StrNSMutableData_setLength = 'setLength:';

  StrNSMutableData_appendBytes_length = 'appendBytes:length:';
  StrNSMutableData_appendData = 'appendData:';
  StrNSMutableData_increaseLengthBy = 'increaseLengthBy:';
  StrNSMutableData_replaceBytesInRange_withBytes = 'replaceBytesInRange:withBytes:';
  StrNSMutableData_resetBytesInRange = 'resetBytesInRange:';
  StrNSMutableData_setData = 'setData:';
  StrNSMutableData_replaceBytesInRange_withBytes_length = 'replaceBytesInRange:withBytes:length:';

  StrNSMutableData_dataWithCapacity = 'dataWithCapacity:';
  StrNSMutableData_dataWithLength = 'dataWithLength:';
  StrNSMutableData_initWithCapacity = 'initWithCapacity:';
  StrNSMutableData_initWithLength = 'initWithLength:';

{ NSData }

class function NSData.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSData_NSData);
end;

function NSData.length: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSData_length)), []));
end;

function NSData.bytes: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSData_bytes)), []));
end;

function NSData.description: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSData_description)), []));
end;

procedure NSData.getBytes(_buffer: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_buffer: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSData_getBytes)), _buffer);
end;

procedure NSData.getBytes_length(_buffer: Pointer; _length: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_buffer: Pointer; _length: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSData_getBytes_length)), _buffer, _length);
end;

procedure NSData.getBytes_range(_buffer: Pointer; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_buffer: Pointer; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSData_getBytes_range)), _buffer, _range);
end;

function NSData.isEqualToData(_other: CFDataRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_other: CFDataRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSData_isEqualToData)), _other));
end;

function NSData.subdataWithRange(_range: NSRange): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRange): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSData_subdataWithRange)), _range));
end;

function NSData.writeToFile_atomically(_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _useAuxiliaryFile: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSData_writeToFile_atomically)), _path, _useAuxiliaryFile));
end;

function NSData.writeToURL_atomically(_url: CFURLRef; _atomically: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _atomically: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSData_writeToURL_atomically)), _url, _atomically));
end;

function NSData.writeToFile_options_error(_path: CFStringRef; _writeOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _writeOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSData_writeToFile_options_error)), _path, _writeOptionsMask, _errorPtr));
end;

function NSData.writeToURL_options_error(_url: CFURLRef; _writeOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _writeOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSData_writeToURL_options_error)), _url, _writeOptionsMask, _errorPtr));
end;

constructor NSData.data;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_data)));
end;

constructor NSData.dataWithBytes_length(_bytes: Pointer; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithBytes_length)), _bytes, _length);
end;

constructor NSData.dataWithBytesNoCopy_length(_bytes: Pointer; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithBytesNoCopy_length)), _bytes, _length);
end;

constructor NSData.dataWithBytesNoCopy_length_freeWhenDone(_bytes: Pointer; _length: LongWord; _b: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord; _b: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithBytesNoCopy_length_freeWhenDone)), _bytes, _length, _b);
end;

{.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
constructor NSData.dataWithContentsOfFile_options_error(_path: CFStringRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithContentsOfFile_options_error)), _path, _readOptionsMask, _errorPtr);
end;

constructor NSData.dataWithContentsOfURL_options_error(_url: CFURLRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithContentsOfURL_options_error)), _url, _readOptionsMask, _errorPtr);
end;

constructor NSData.dataWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithContentsOfFile)), _path);
end;

constructor NSData.dataWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithContentsOfURL)), _url);
end;

constructor NSData.dataWithContentsOfMappedFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithContentsOfMappedFile)), _path);
end;

constructor NSData.initWithBytes_length(_bytes: Pointer; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithBytes_length)), _bytes, _length);
end;

constructor NSData.initWithBytesNoCopy_length(_bytes: Pointer; _length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithBytesNoCopy_length)), _bytes, _length);
end;

constructor NSData.initWithBytesNoCopy_length_freeWhenDone(_bytes: Pointer; _length: LongWord; _b: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord; _b: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithBytesNoCopy_length_freeWhenDone)), _bytes, _length, _b);
end;

{.$ifdef  MAC_OS_X_VERSION_10_4 <= MAC_OS_X_VERSION_MAX_ALLOWED}
constructor NSData.initWithContentsOfFile_options_error(_path: CFStringRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithContentsOfFile_options_error)), _path, _readOptionsMask, _errorPtr);
end;

constructor NSData.initWithContentsOfURL_options_error(_url: CFURLRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef; _readOptionsMask: LongWord; _errorPtr: lobjc.id {NSError}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithContentsOfURL_options_error)), _url, _readOptionsMask, _errorPtr);
end;

constructor NSData.initWithContentsOfFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithContentsOfFile)), _path);
end;

constructor NSData.initWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithContentsOfURL)), _url);
end;

constructor NSData.initWithContentsOfMappedFile(_path: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithContentsOfMappedFile)), _path);
end;

constructor NSData.initWithData(_data: CFDataRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSData_initWithData)), _data);
end;

constructor NSData.dataWithData(_data: CFDataRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSData_dataWithData)), _data);
end;

{ NSMutableData }

class function NSMutableData.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSMutableData_NSMutableData);
end;

function NSMutableData.mutableBytes: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSMutableData_mutableBytes)), []));
end;

procedure NSMutableData.setLength(_length: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_length: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_setLength)), _length);
end;

procedure NSMutableData.appendBytes_length(_bytes: Pointer; _length: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bytes: Pointer; _length: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_appendBytes_length)), _bytes, _length);
end;

procedure NSMutableData.appendData(_other: CFDataRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_other: CFDataRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_appendData)), _other);
end;

procedure NSMutableData.increaseLengthBy(_extraLength: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_extraLength: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_increaseLengthBy)), _extraLength);
end;

procedure NSMutableData.replaceBytesInRange_withBytes(_range: NSRange; _bytes: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _bytes: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_replaceBytesInRange_withBytes)), _range, _bytes);
end;

procedure NSMutableData.resetBytesInRange(_range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_resetBytesInRange)), _range);
end;

procedure NSMutableData.setData(_data: CFDataRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_data: CFDataRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_setData)), _data);
end;

procedure NSMutableData.replaceBytesInRange_withBytes_length(_range: NSRange; _replacementBytes: Pointer; _replacementLength: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _replacementBytes: Pointer; _replacementLength: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSMutableData_replaceBytesInRange_withBytes_length)), _range, _replacementBytes, _replacementLength);
end;

constructor NSMutableData.dataWithCapacity(_aNumItems: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aNumItems: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSMutableData_dataWithCapacity)), _aNumItems);
end;

constructor NSMutableData.dataWithLength(_length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSMutableData_dataWithLength)), _length);
end;

constructor NSMutableData.initWithCapacity(_capacity: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_capacity: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMutableData_initWithCapacity)), _capacity);
end;

constructor NSMutableData.initWithLength(_length: LongWord);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_length: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSMutableData_initWithLength)), _length);
end;

{$endif}
