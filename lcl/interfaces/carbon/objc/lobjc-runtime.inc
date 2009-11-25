{
 * Copyright (c) 1999 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * Copyright (c) 1999-2003 Apple Computer, Inc.  All Rights Reserved.
 * 
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. Please obtain a copy of the License at
 * http://www.opensource.apple.com/apsl/ and read it before using this
 * file.
 * 
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 }
{
 *	lobjc-runtime.h
 *	Copyright 1988-1996, NeXT Software, Inc.
 }

//#import <stdarg.h>
//#import <AvailabilityMacros.h>
//#import <objc/objc.h>
//#import <objc/objc-class.h>

{$ifndef AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER}
    { For 10.2 compatibility }
    {$define AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER}
{$endif}

type
  Pobjc_symtab = ^objc_symtab;
  
  Symtab = Pobjc_symtab;

  objc_symtab = record
    sel_ref_cnt: culong;
    refs: PSEL;
    cls_def_cnt: cushort;
    cat_def_cnt: cushort;
    defs: array [0..0] of Pointer;	{ variable size }
  end;

  Pobjc_module = ^objc_module;
  
  Module = Pobjc_module;

  objc_module = record
    version: culong;
    size: culong;
    name: PChar;
    _symtab: Symtab;
  end;

  Pobjc_super = ^objc_super;

  objc_super = record
    receiver: id;
    super_class: _Class;
  end;

{
 * Messaging Primitives (prototypes)
 }

function objc_getClass(const name: PChar): id; cdecl; external;
function objc_getMetaClass(const name: PChar): id; cdecl; external;
function objc_msgSend(self: id; op: SEL; param3: array of const): id; cdecl; external;
function objc_msgSendSuper(super: Pobjc_super; op: SEL; param3: array of const): id; cdecl; external;


{* Floating-point-returning Messaging Primitives (prototypes)
 * 
 * On some platforms, the ABI for functions returning a floating-point 
 * value is incompatible with that for functions returning an integral type. 
 * objc_msgSend_fpret must be used for these. 
 * 
 * ppc: objc_msgSend_fpret not used
 * ppc64: objc_msgSend_fpret not used
 * i386: objc_msgSend_fpret REQUIRED
 *
 * For `float` or `long double` return types, cast the function 
 * to an appropriate function pointer type first.
 *}

{$if defined(CPUI386) or defined(__i386__)}
function objc_msgSend_fpret(self: id; op: SEL; param3: array of const): cdouble; cdecl; external;
{$else}
function objc_msgSend_fpret(self: id; op: SEL; param3: array of const): cdouble; cdecl; external name 'objc_msgSend';
{$endif}


{* Struct-returning Messaging Primitives (prototypes)
 *
 * For historical reasons, the prototypes for the struct-returning 
 * messengers are unusual. The portable, correct way to call these functions 
 * is to cast them to your desired return type first.
 * 
 * For example, `NSRect result = [myNSView frame]` could be written as:
 *   NSRect (*msgSend_stret_fn)(id, SEL, ...) = (NSRect(*)(id, SEL, ...))objc_msgSend_stret;
 *   NSRect result = (*msgSend_stret_fn)(myNSView, @selector(frame));
 * or, without the function pointer:
 *   NSRect result = (*(NSRect(*)(id, SEL, ...))objc_msgSend_stret)(myNSView, @selector(frame));
 * 
 * BE WARNED that these prototypes have changed in the past and will change 
 * in the future. Code that uses a cast like the example above will be 
 * unaffected. 
 *}

{$ifdef __cplusplus}
  OBJC_EXPORT id objc_msgSend_stret(id self, SEL op, ...); cdecl; external;
  OBJC_EXPORT id objc_msgSendSuper_stret(struct objc_super *super, SEL op, ...); cdecl; external;
{$else}
  procedure objc_msgSend_stret(stretAddr: Pointer; self: id; op: SEL; others: array of const); cdecl; external;
  procedure objc_msgSendSuper_stret(stretAddr: Pointer; super: Pobjc_super; op: SEL; others: array of const); cdecl; external;
{$endif}


{ Forwarding }

{ Note that objc_msgSendv_stret() does not return a structure type, 
 * and should not be cast to do so. This is unlike objc_msgSend_stret() 
 * and objc_msgSendSuper_stret().
 }

function objc_msgSendv(self: id; op: SEL; arg_size: cunsigned; arg_frame: marg_list): id; cdecl; external;
procedure objc_msgSendv_stret(stretAddr: Pointer; self: id; op: SEL; arg_size: cunsigned; arg_frame: marg_list); cdecl; external;
{$if defined(CPUI386) or defined(__i386__)}
function objc_msgSendv_fpret(self: id; op: SEL; arg_size: cunsigned; arg_frame: marg_list): cdouble; cdecl; external;
{$endif}


{ 
    getting all the classes in the application...
    
    int objc_getClassList(buffer, bufferLen)
	classes is an array of Class values (which are pointers)
		which will be filled by the function; if this
		argument is NULL, no copying is done, only the
		return value is returned
	bufferLen is the number of Class values the given buffer
		can hold; if the buffer is not large enough to
		hold all the classes, the buffer is filled to
		the indicated capacity with some arbitrary subset
		of the known classes, which could be different
		from call to call
	returns the number of classes, which is the number put
		in the buffer if the buffer was large enough,
		or the length the buffer should have been

    int numClasses = 0, newNumClasses = objc_getClassList(NULL, 0);
    Class *classes = NULL;
    while (numClasses < newNumClasses) [
        numClasses = newNumClasses;
        classes = realloc(classes, sizeof(Class) * numClasses);
        newNumClasses = objc_getClassList(classes, numClasses);
    ]
    // now, can use the classes list; if NULL, there are no classes
    free(classes);

}
function objc_getClassList(buffer: PClass; bufferLen: cint): cint; cdecl; external;

{#define OBSOLETE_OBJC_GETCLASSES}
{$ifdef OBSOLETE_OBJC_GETCLASSES}
function objc_getClasses(): Pointer; cdecl; external;
{$endif}

function objc_lookUpClass(const name: PChar): id; cdecl; external;
function objc_getRequiredClass(const name: PChar): id; cdecl; external;
procedure objc_addClass(myClass: _Class); cdecl; external;

{ customizing the error handling for objc_getClass/objc_getMetaClass }

type
  setClassHandler_callback = function (const param1: PChar): cint; cdecl;

procedure objc_setClassHandler(param1: setClassHandler_callback); cdecl; external;

{ Making the Objective-C runtime thread safe. }
procedure objc_setMultithreaded (flag: BOOL); cdecl; external;

{ overriding the default object allocation and error handling routines }

{OBJC_EXPORT id	(_alloc)(Class, unsigned int);
OBJC_EXPORT id	(_copy)(id, unsigned int);
OBJC_EXPORT id	(_realloc)(id, unsigned int);
OBJC_EXPORT id	(_dealloc)(id);
OBJC_EXPORT id	(_zoneAlloc)(Class, unsigned int, void *);
OBJC_EXPORT id	(_zoneRealloc)(id, unsigned int, void *);
OBJC_EXPORT id	(_zoneCopy)(id, unsigned int, void *);

OBJC_EXPORT void	(_error)(id, const char *, va_list);
}

