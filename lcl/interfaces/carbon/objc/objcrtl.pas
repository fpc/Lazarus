{
 objcrtl.pas

 Copyright (C) 2009 Dmitry Boyarintsev

 This unit is a pascal binding for dynamic Objective-C Run-time Library
 headers included with XCode 3.1.2
 The original copyright note of is kept on each include file
}
unit objcrtl;

{$mode objfpc}{$H+}

interface

uses
  dynlibs;

const
  DefaultObjCLibName : AnsiString = 'libobjc.A.dylib';


{ Overview
  --------

  This document describes the Mac OS X Objective-C 2.0 runtime library support
 functions and data structures. The functions are implemented in the shared
 library found at /usr/lib/libobjc.A.dylib. This shared library provides support
 for the dynamic properties of the Objective-C language, and as such is linked
 to by all Objective-C applications.

  This reference is useful primarily for developing bridge layers between
 Objective-C and other languages, or for low-level debugging. You typically do
 not need to use the Objective-C runtime library directly when programming
 in Objective-C.

   The Mac OS X implementation of the Objective-C runtime library is unique
 to the Mac OS X platform. For other platforms, the GNU Compiler Collection
 provides a different implementation with a similar API. This document covers
 only the Mac OS X implementation.

   The low-level Objective-C runtime API is significantly updated in Mac OS X
 version 10.5. Many functions and all existing data structures are replaced
 with new functions. The old functions and structures are deprecated in 32-bit
 and absent in 64-bit mode. The API constrains several values to 32-bit ints
 even in 64-bit mode—class count, protocol count, methods per class, ivars
 per class, arguments per method, sizeof(all arguments) per method, and
 class version number. In addition, the new Objective-C ABI (not described here)
 further constrains sizeof(anInstance) to 32 bits, and three other values
 to 24 bits—methods per class, ivars per class, and sizeof(a single ivar).
 Finally, the obsolete NXHashTable and NXMapTable are limited to 4 billion items.

   “Deprecated” below means “deprecated in Mac OS X version 10.5 for 32-bit code,
 and disallowed for 64-bit code.

  Legacy and Modern Versions
  --------------------------

  There are two versions of the Objective-C runtime—“modern” and “legacy”.
 The modern version was introduced with Objective-C 2.0 and includes a number
 of new features. The programming interface for the legacy version of the runtime
 is described in Objective-C 1 Runtime Reference; the programming interface
 for the modern version of the runtime is described in Objective-C 2.0 Runtime
 Reference.

  The most notable new feature is that instance variables in the modern runtime are “non-fragile”:
 * In the legacy runtime, if you change the layout of instance variables in a class,
   you must recompile classes that inherit from it.
 * In the modern runtime, if you change the layout of instance variables in a class,
   you do not have to recompile classes that inherit from it.

  In addition, the modern runtime supports instance variable synthesis
 for declared properties (see Declared Properties in The Objective-C 2.0 Programming Language).

  Platforms
  ---------

  iPhone applications and 64-bit programs on Mac OS X v10.5 and later use
 the modern version of the runtime.

  Other programs (32-bit programs on Mac OS X desktop) use the legacy version
 of the runtime.
}

type
  //todo: types MUST BE declared properly as 2.0 opaques
  SEL = Pointer;
  IMP = Pointer;
  id  = Pointer; //??
  size_t = LongWord;
  _Class = Pointer;
  Ivar = Pointer;
  PProtocol = Pointer;
  PArrayPProtocol = Pointer;
  BOOL = Boolean;
  PIvar = Pointer;
  Method = Pointer;
  PMethod = Pointer;
  Protocol = Pointer;
  objc_property_t = Pointer;
  Pobjc_property_t = Pointer;
  uint8_t = byte;
  Pobjc_method_description = Pointer;
  ptrdiff_t = Pointer;
  objc_method_description = Pointer;
  TMutationHandlerProc = Pointer;

  pobjc_super = ^objc_super;
  objc_super = packed record
    reciever : id;
    class_   : _class;
  end;

var
  sel_getName : function (sel: SEL): PChar; cdecl = nil;
  sel_registerName : function (str: PChar): SEL; cdecl = nil;
  object_getClassName : function (obj: id): PChar; cdecl = nil;
  object_getIndexedIvars : function (obj: id ): Pointer; cdecl = nil;

  sel_isMapped: function (sel: SEL): Boolean; cdecl = nil;
  sel_getUid: function (const str: PChar): SEL; cdecl = nil;

  object_copy : function (obj:id; size:size_t):id; cdecl = nil;
  object_dispose : function (obj:id):id; cdecl = nil;

  object_getClass : function (obj:id): _Class; cdecl = nil;
  object_setClass : function (obj:id; cls: _Class):_Class; cdecl = nil;

  object_getIvar : function (obj:id; ivar:Ivar):id; cdecl = nil;
  object_setIvar : procedure (obj:id; ivar:Ivar; value:id); cdecl = nil;

  object_setInstanceVariable : function (obj:id; name:pchar; value:pointer):Ivar; cdecl = nil;
  object_getInstanceVariable : function (obj:id; name:pchar; var outValue: Pointer):Ivar; cdecl = nil;

  objc_getClass : function (name:pchar):id; cdecl = nil;
  objc_getMetaClass : function (name:pchar):id; cdecl = nil;
  objc_lookUpClass : function (name:pchar):id; cdecl = nil;
  objc_getRequiredClass : function (name:pchar):id; cdecl = nil;
  objc_getFutureClass : function (name:pchar):_Class; cdecl = nil;
  objc_setFutureClass : procedure (cls:_Class; name:pchar); cdecl = nil;
  objc_getClassList : function (buffer:pClass; bufferCount:longint):longint; cdecl = nil;

  objc_getProtocol : function (name:pchar): PProtocol; cdecl = nil;
  objc_copyProtocolList : function (outCount:pdword):PArrayPProtocol; cdecl = nil;

  class_getName : function (cls:_Class):PChar; cdecl = nil;
  class_isMetaClass : function (cls:_Class):BOOL; cdecl = nil;
  class_getSuperclass : function (cls:_Class):_Class; cdecl = nil;
  class_setSuperclass : function (cls: _Class; newSuper: _Class): _Class; cdecl = nil;


  class_getVersion : function (cls:_Class):longint; cdecl = nil;
  class_setVersion : procedure (cls:_Class; version:longint); cdecl = nil;

  class_getInstanceSize : function (cls:_Class):size_t; cdecl = nil;

  class_getInstanceVariable : function (cls:_Class; name:pchar):Ivar; cdecl = nil;
  class_getClassVariable : function (cls:_Class; name:pchar):Ivar; cdecl = nil;
  class_copyIvarList : function (cls:_Class; outCount:pdword):PIvar; cdecl = nil;

  class_getInstanceMethod : function (cls:_Class; name:SEL):Method; cdecl = nil;
  class_getClassMethod : function (cls:_Class; name:SEL):Method; cdecl = nil;
  class_getMethodImplementation : function (cls:_Class; name:SEL):IMP; cdecl = nil;
  class_getMethodImplementation_stret : function (cls:_Class; name:SEL):IMP; cdecl = nil;
  class_respondsToSelector : function (cls:_Class; sel:SEL):BOOL; cdecl = nil;
  class_copyMethodList : function (cls:_Class; outCount:pdword):PMethod; cdecl = nil;

  class_conformsToProtocol : function (cls:_Class; var protocol: Protocol):BOOL; cdecl = nil;
  class_copyProtocolList : function (cls:_Class; var outCount: dword):PArrayPProtocol; cdecl = nil;

  class_getProperty : function (cls:_Class; name: pchar): objc_property_t; cdecl = nil;
  class_copyPropertyList : function (cls:_Class; var Count:dword):Pobjc_property_t; cdecl = nil;

  class_getIvarLayout : function (cls:_Class):Pchar; cdecl = nil;
  class_getWeakIvarLayout : function (cls:_Class):Pchar; cdecl = nil;

  class_createInstance : function (cls:_Class; extraBytes:size_t):id; cdecl = nil;

  objc_allocateClassPair : function (superclass:_Class; name:pchar; extraBytes:size_t):_Class; cdecl = nil;
  objc_registerClassPair : procedure (cls:_Class); cdecl = nil;
  objc_duplicateClass : function (original:_Class; name:pchar; extraBytes:size_t):_Class; cdecl = nil;
  objc_disposeClassPair : procedure (cls:_Class); cdecl = nil;

  class_addMethod : function (cls:_Class; name:SEL; imp:IMP; types:pchar):BOOL; cdecl = nil;
  class_replaceMethod : function (cls:_Class; name:SEL; imp:IMP; types:pchar):IMP; cdecl = nil;
  class_addIvar: function (cls:_Class; name:pchar; size:size_t; alignment:uint8_t; types:pchar):BOOL; cdecl = nil;
  class_addProtocol : function (cls:_Class; protocol:pProtocol):BOOL; cdecl = nil;
  class_setIvarLayout : procedure (cls:_Class; layout:pchar); cdecl = nil;
  class_setWeakIvarLayout : procedure (cls:_Class; layout:pchar); cdecl = nil;

  method_getName : function (m:Method):SEL; cdecl = nil;
  method_getImplementation : function (m:Method):IMP; cdecl = nil;
  method_getTypeEncoding : function (m:Method):Pchar; cdecl = nil;

  method_getNumberOfArguments : function (m:Method):dword; cdecl = nil;
  method_copyReturnType : function (m:Method):Pchar; cdecl = nil;
  method_copyArgumentType : function (m:Method; index:dword):Pchar; cdecl = nil;
  method_getReturnType : procedure (m:Method; dst:pchar; dst_len:size_t); cdecl = nil;
  method_getArgumentType : procedure (m:Method; index:dword; dst:pchar; dst_len:size_t); cdecl = nil;
  method_getDescription : function (m: Method) : Pobjc_method_description; cdecl = nil;

  method_setImplementation: function (m:Method; imp:IMP):IMP; cdecl = nil;
  method_exchangeImplementations : procedure (m1:Method; m2:Method); cdecl = nil;

  ivar_getName : function (v:Ivar):Pchar; cdecl = nil;
  ivar_getTypeEncoding : function (v:Ivar):Pchar; cdecl = nil;
  ivar_getOffset : function (v:Ivar):ptrdiff_t; cdecl = nil;

  property_getName :function (_property:objc_property_t):Pchar; cdecl = nil;
  property_getAttributes : function (_property:objc_property_t):Pchar; cdecl = nil;


  protocol_conformsToProtocol : function (proto:pProtocol; other:pProtocol):BOOL; cdecl = nil;
  protocol_isEqual : function (proto:pProtocol; other:pProtocol):BOOL; cdecl = nil;
  protocol_getMethodDescription : function (p: PProtocol; aSel: SEL; isRequiredMethod, isInstanceMethod: BOOL): objc_method_description; cdecl = nil;
  protocol_copyMethodDescriptionList : function (p: PProtocol; isRequiredMethod, isInstanceMethod: BOOL ; var outCount: LongWord): Pobjc_method_description; cdecl = nil;
  protocol_getProperty : function (proto:PProtocol; name:pchar; isRequiredProperty:BOOL; isInstanceProperty:BOOL):objc_property_t; cdecl = nil;
  protocol_copyPropertyList : function (proto:PProtocol; outCount:pdword):Pobjc_property_t; cdecl = nil;
  protocol_copyProtocolList : function (proto:PProtocol; outCount:pdword):PArrayPProtocol; cdecl = nil;

  objc_copyImageNames : function (var outCount:dword): PPchar; cdecl = nil;
  class_getImageName : function (cls:_Class):Pchar; cdecl = nil;
  objc_copyClassNamesForImage : function (image:pchar; var outCount: Dword):PPchar; cdecl = nil;

  sel_isEqual : function (lhs:SEL; rhs:SEL):BOOL; cdecl = nil;
  objc_enumerationMutation : procedure (_para1:id); cdecl = nil;
  objc_setEnumerationMutationHandler : procedure (handler:TMutationHandlerProc); cdecl = nil;
  objc_setForwardHandler: procedure (fwd:pointer; fwd_stret:pointer); cdecl = nil;

  {$WARNINGS OFF} // warning: cdecl'ared funtions have no high parameter
  objc_msgSend       : function  (self: id; op: SEL; param3: array of const): id; cdecl = nil;
  objc_msgSendSuper  : function  (super: pobjc_super; op: SEL; param3: array of const): id; cdecl = nil;
  objc_msgSend_stret : procedure (stret: Pointer; self: id; op: SEL; param3: array of const); cdecl= nil;
  objc_msgSend_stretreg : function (self: id; op: SEL; param3: array of const): id; cdecl= nil;
  objc_msgSendSuper_stret : procedure (stret: Pointer; super: pobjc_super; op: SEL; param3: array of const); cdecl = nil;
  objc_msgSend_fpret : function  (self: id; op: SEL; param3: array of const): double; cdecl = nil;
  {$WARNINGS ON}

  method_invoke : function (receiver: id; m: Method {, ...}): id= nil;
  method_invoke_stret : procedure (receiver: id; m: Method{ , ...})= nil;
  objc_collect : procedure (options: LongWord); cdecl= nil;
  objc_collectingEnabled : function : BOOL; cdecl= nil;

const
  _C_ID	      = '@';
  _C_CLASS    = '#';
  _C_SEL 	    = ':';
  _C_CHR 	    = 'c';
  _C_UCHR     = 'C';
  _C_SHT 	    = 's';
  _C_USHT     = 'S';
  _C_INT 	    = 'i';
  _C_UINT     = 'I';
  _C_LNG 	    = 'l';
  _C_ULNG     = 'L';
  _C_FLT 	    = 'f';
  _C_DBL 	    = 'd';
  _C_BFLD     = 'b';
  _C_VOID     = 'v';
  _C_UNDEF    = '?';
  _C_PTR	    = '^';
  _C_CHARPTR  = '*';
  _C_ARY_B    = '[';
  _C_ARY_E    = ']';
  _C_UNION_B  = '(';
  _C_UNION_E  = ')';
  _C_STRUCT_B = '{';
  _C_STRUCT_E = '}';
  _C_PASOBJ   = _C_PTR + _C_VOID;
  _C_SELF_AND_SEL = '@:';

// objc-exception.h

// compiler reserves a setjmp buffer + 4 words as localExceptionData

type
  Tobjc_exception_throw     = procedure (exception: id); cdecl;
  Tobjc_exception_try_enter = procedure (localExceptionData: Pointer); cdecl;
  Tobjc_exception_try_exit  = procedure (localExceptionData: Pointer); cdecl;
  Tobjc_exception_extract   = function (localExceptionData: Pointer): id; cdecl;
  Tobjc_exception_match     = function (exceptionClass:_Class; exception:id ): Integer; cdecl;

var
  objc_exception_throw     : Tobjc_exception_throw = nil;
  objc_exception_try_enter : Tobjc_exception_try_enter = nil;
  objc_exception_try_exit  : Tobjc_exception_try_exit = nil;
  objc_exception_extract   : Tobjc_exception_extract = nil;
  objc_exception_match     : Tobjc_exception_match = nil;

type
  pobjc_exception_functions_t = ^objc_exception_functions_t;
  objc_exception_functions_t = packed record
    version   : Integer;
    throw_exc : Tobjc_exception_throw;      // version 0
    try_enter : Tobjc_exception_try_enter;  // version 0
    try_exit  : Tobjc_exception_try_exit;   // version 0
    extract   : Tobjc_exception_extract;    // version 0
    match     : Tobjc_exception_match;      // version 0
  end;

// get table; version tells how many
var
  objc_exception_get_functions : procedure (var table: objc_exception_functions_t); cdecl = nil;
  objc_exception_set_functions : procedure (table: pobjc_exception_functions_t); cdecl = nil;


// __LP64__  // 64-bit only functions
{
typedef id (*objc_exception_preprocessor)(id exception);
typedef int (*objc_exception_matcher)(Class catch_type, id exception);
typedef void (*objc_uncaught_exception_handler)(id exception);
typedef void (*objc_exception_handler)(id unused, void *context);

OBJC_EXPORT void objc_exception_throw(id exception);
OBJC_EXPORT void objc_exception_rethrow(void);
OBJC_EXPORT id objc_begin_catch(void *exc_buf);
OBJC_EXPORT void objc_end_catch(void);

OBJC_EXPORT uintptr_t objc_addExceptionHandler(objc_exception_handler fn, void *context);
OBJC_EXPORT void objc_removeExceptionHandler(uintptr_t token);

OBJC_EXPORT objc_exception_preprocessor objc_setExceptionPreprocessor(objc_exception_preprocessor fn);
OBJC_EXPORT objc_exception_matcher objc_setExceptionMatcher(objc_exception_matcher fn);
OBJC_EXPORT objc_uncaught_exception_handler objc_setUncaughtExceptionHandler(objc_uncaught_exception_handler fn);
}



// objc-sync.h

var
  // Begin synchronizing on 'obj'.
  // Allocates recursive pthread_mutex associated with 'obj' if needed.
  // Returns OBJC_SYNC_SUCCESS once lock is acquired.
  objc_sync_enter: function (obj: id): Integer; cdecl = nil;
  // End synchronizing on 'obj'.
  // Returns OBJC_SYNC_SUCCESS or OBJC_SYNC_NOT_OWNING_THREAD_ERROR
  objc_sync_exit : function (obj: id) : Integer; cdecl = nil;
  // Temporarily release lock on 'obj' and wait for another thread to notify on 'obj'
  // Return OBJC_SYNC_SUCCESS, OBJC_SYNC_NOT_OWNING_THREAD_ERROR, OBJC_SYNC_TIMED_OUT
  objc_sync_wait : function (obj: id; milliSecondsMaxWait: Int64): Integer; cdecl = nil;
  // Wake up another thread waiting on 'obj'
  // Return OBJC_SYNC_SUCCESS, OBJC_SYNC_NOT_OWNING_THREAD_ERROR
  objc_sync_notify : function (obj: id): Integer; cdecl = nil;
  // Wake up all threads waiting on 'obj'
  // Return OBJC_SYNC_SUCCESS, OBJC_SYNC_NOT_OWNING_THREAD_ERROR
  objc_sync_notifyAll : function (obj: id): Integer; cdecl = nil;

const
	OBJC_SYNC_SUCCESS                 = 0;
	OBJC_SYNC_NOT_OWNING_THREAD_ERROR = -1;
	OBJC_SYNC_TIMED_OUT               = -2;
	OBJC_SYNC_NOT_INITIALIZED         = -3;

// since exception handling does not change from version to version
// it's nice to make a common RTL loading function for exception functions.
// this proc, MUST BE called by run-time initialization proc!
function LoadDefaultObjCExepction(hnd: TLibHandle): Boolean;
function LoadDefaultObjCSync(hnd: TLibHandle): Boolean;
function LoadDefaultObjCMessaging(hnd: TLibHandle): Boolean;

implementation

function LoadDefaultObjCExepction(hnd: TLibHandle): Boolean;
begin
  Result := hnd <> 0;
  if not Result then Exit;

  objc_exception_throw     := Tobjc_exception_throw(GetProcedureAddress(hnd, 'objc_exception_throw'));
  objc_exception_try_enter := Tobjc_exception_try_enter(GetProcedureAddress(hnd, 'objc_exception_try_enter'));
  objc_exception_try_exit  := Tobjc_exception_try_exit(GetProcedureAddress(hnd, 'objc_exception_try_exit'));
  objc_exception_extract   := Tobjc_exception_extract(GetProcedureAddress(hnd, 'objc_exception_extract'));
  objc_exception_match     := Tobjc_exception_match(GetProcedureAddress(hnd, 'objc_exception_match'));
end;

function LoadDefaultObjCSync(hnd: TLibHandle): Boolean;
begin
  Result := hnd <> 0;
  if not Result then Exit;
  Pointer(objc_sync_enter) := GetProcedureAddress(hnd, 'objc_sync_enter');
  Pointer(objc_sync_exit) := GetProcedureAddress(hnd, 'objc_sync_exit');
  Pointer(objc_sync_wait) := GetProcedureAddress(hnd, 'objc_sync_wait');
  Pointer(objc_sync_notify) := GetProcedureAddress(hnd, 'objc_sync_notify');
  Pointer(objc_sync_notifyAll) := GetProcedureAddress(hnd, 'objc_sync_notifyAll');
end;

function LoadDefaultObjCMessaging(hnd: TLibHandle): Boolean;
begin
  Pointer(objc_msgSend) := GetProcedureAddress(hnd, 'objc_msgSend');
  Pointer(objc_msgSendSuper) := GetProcedureAddress(hnd, 'objc_msgSendSuper');
  Pointer(objc_msgSend_stret) := GetProcedureAddress(hnd, 'objc_msgSend_stret');
  Pointer(objc_msgSendSuper_stret) := GetProcedureAddress(hnd, 'objc_msgSendSuper_stret');

  {$ifndef CPUPOWERPC} // arm also uses objc_msgSend_fpret?
  Pointer(objc_msgSend_fpret) := GetProcedureAddress(hnd, 'objc_msgSend_fpret');
  Pointer(objc_msgSend_stretreg) := GetProcedureAddress(hnd, 'objc_msgSend');
  {$else}
  Pointer(objc_msgSend_fpret) := GetProcedureAddress(hnd, 'objc_msgSend');
  Pointer(objc_msgSend_stretreg) := GetProcedureAddress(hnd, 'objc_msgSend_stret');
  {$endif}
  Result := true;
end;

initialization

end.

