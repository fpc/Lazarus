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
 *	objc-class.h
 *	Copyright 1988-1996, NeXT Software, Inc.
 }

//#import <objc/objc.h>

{
 *	Class Template
 }
type
  Pobjc_class = ^objc_class;
 
  Pobjc_ivar_list = ^objc_ivar_list;

  Pobjc_method_list = ^objc_method_list;
  PPobjc_method_list = ^Pobjc_method_list;
  
  Pobjc_cache = ^objc_cache;

  Pobjc_protocol_list = ^objc_protocol_list;

  objc_class = record
	  isa: Pobjc_class;
	  super_class: Pobjc_class;
	  name: PChar;		
	  version: culong;
	  info: culong;
	  instance_size: culong;
	  ivars: Pobjc_ivar_list;

	  methodLists: PPobjc_method_list;

	  cache: Pobjc_cache;
 	  protocols: Pobjc_protocol_list;
  end;
  
//#define CLS_GETINFO(cls,infomask)	((cls)->info & (infomask))
//#define CLS_SETINFO(cls,infomask)	((cls)->info |= (infomask))

{ Constants here were moved to the end }

{
 *	Category Template
 }
  Pobjc_category = ^objc_category;

  Category = Pobjc_category;

  objc_category = record
	category_name: PChar;
	class_name: PChar;
	instance_methods: Pobjc_method_list;
	class_methods: Pobjc_method_list;
 	protocols: Pobjc_protocol_list;
  end;

{ 
 *	Instance Variable Template
 }
  Pobjc_ivar = ^objc_ivar;
 
  Ivar = Pobjc_ivar;

  objc_ivar = record
	ivar_name: PChar;
	ivar_type: PChar;
	ivar_offset: cint;
{$ifdef __alpha__}
	space: cint;
{$endif}
  end;

  objc_ivar_list = record
	ivar_count: cint;
{$ifdef __alpha__}
	space: cint;
{$endif}
	ivar_list: array[0..0] of objc_ivar;		{ variable length structure }
  end;

{ functions here were moved down }

{ 
 *	Method Template
 }
  Pobjc_method = ^objc_method;
  Method = Pobjc_method;

  objc_method = record
    method_name: SEL;
    method_types: PChar;
    method_imp: IMP;
  end;

  objc_method_list = record
    obsolete: Pobjc_method_list;

    method_count: cint;
{$ifdef __alpha__}
    space: cint;
{$endif}
    method_list: array[0..0] of objc_method;	{ variable length structure }
  end;

{ Protocol support }

  Protocol = objc_object;

  objc_protocol_list = record
    next: Pobjc_protocol_list;
    count: cint;
    list: array[0..0] of Protocol;
  end;

{ Constants here moved down }

{ Structure for method cache - allocated/sized at runtime }

  Cache = Pobjc_cache;

  objc_cache = record
    mask: cuint;            { total = mask + 1 }
    occupied: cuint;
    buckets: array[0..0] of Method;
  end;

{#define CACHE_BUCKET_NAME(B)  ((B)->method_name)
#define CACHE_BUCKET_IMP(B)   ((B)->method_imp)
#define CACHE_BUCKET_VALID(B) (B)
#define CACHE_HASH(sel, mask) (((uarith_t)(sel)>>2) & (mask))}

{ operations }
function class_createInstance(param1: _Class; idxIvars: cunsigned): id; cdecl; external;
function class_createInstanceFromZone(param1: _Class; idxIvars: cunsigned; z: Pointer): id; cdecl; external;

procedure class_setVersion(param1: _Class; param2: cint); cdecl; external;
function class_getVersion(param1: _Class): cint; cdecl; external;

function class_getInstanceVariable(param1: _Class; const param2: PChar): Ivar; cdecl; external;
function class_getInstanceMethod(param1: _Class; param2: SEL): Method; cdecl; external;
function class_getClassMethod(param1: _Class; param2: SEL): Method; cdecl; external;

procedure class_addMethods(param1: _Class; param2: Pobjc_method_list); cdecl; external;
procedure class_removeMethods(param1: _Class; param2: Pobjc_method_list); cdecl; external;

function class_poseAs(imposter, original: _Class): _Class; cdecl; external;

function method_getNumberOfArguments(param1: Method): cunsigned; cdecl; external;
function method_getSizeOfArguments(param1: Method): cunsigned; cdecl; external;
function method_getArgumentInfo(m: Method; arg: cint; _type: PPChar; offset: Pcint): cunsigned; cdecl; external;

// usage for nextMethodList
//
// void *iterator = 0;
// struct objc_method_list *mlist;
// while ( mlist = class_nextMethodList( cls, &iterator ) )
//    ;
const OBJC_NEXT_METHOD_LIST = 1;
function class_nextMethodList(param1: _Class; param2: PPPointer): Pobjc_method_list; cdecl; external;

type
  marg_list = Pointer;

{$if defined(PowerPC) or defined(__ppc__) or defined(ppc)}
const marg_prearg_size = 128;
{$else}
const marg_prearg_size = 0;
{$endif}

{#define marg_malloc(margs, method) \
	do ( \
		margs = (marg_list *)malloc (marg_prearg_size + ((7 + method_getSizeOfArguments(method)) & ~7)); \
	) while (0)


#define marg_free(margs) \
	do ( \
		free(margs); \
	) while (0)
	
#define marg_adjustedOffset(method, offset) \
	(marg_prearg_size + offset)




#define marg_getRef(margs, offset, type) \
	( (type *)((char *)margs + marg_adjustedOffset(method,offset) ) )

#define marg_getValue(margs, offset, type) \
	( *marg_getRef(margs, offset, type) )

#define marg_setValue(margs, offset, type, value) \
	( marg_getValue(margs, offset, type) = (value) )   }

{ Load categories and non-referenced classes from libraries. }

{ Constants moved to the end }

const
  CLS_CLASS		= $1;//L
  CLS_META		= $2;//L
  CLS_INITIALIZED	= $4;//L
  CLS_POSING		= $8;//L
  CLS_MAPPED		= $10;//L
  CLS_FLUSH_CACHE	= $20;//L
  CLS_GROW_CACHE	= $40;//L
  CLS_NEED_BIND		= $80;//L
  CLS_METHOD_ARRAY      = $100;//L
// the JavaBridge constructs classes with these markers
  CLS_JAVA_HYBRID	= $200;//L
  CLS_JAVA_CLASS	= $400;//L
// thread-safe +initialize
  CLS_INITIALIZING	= $800;
// bundle unloading
  CLS_FROM_BUNDLE	= $1000;//L
// C++ ivar support
  CLS_HAS_CXX_STRUCTORS	= $2000;//L
// Lazy method list arrays
  CLS_NO_METHOD_ARRAY	= $4000;//L
// +load implementation
// #define CLS_HAS_LOAD_METHOD	0x8000L

{ Functions moved to the end }

function object_setInstanceVariable(param1: id; const name: PChar; param3: Pointer): Ivar; cdecl; external;
function object_getInstanceVariable(param1: id; const name: PChar; param3: PPointer): Ivar; cdecl; external;

{ Constants moved down }

{ Definitions of filer types }

const
  _C_ID	         = '@';
  _C_CLASS       = '#';
  _C_SEL 	 = ':';
  _C_CHR 	 = 'c';
  _C_UCHR        = 'C';
  _C_SHT 	 = 's';
  _C_USHT        = 'S';
  _C_INT 	 = 'i';
  _C_UINT        = 'I';
  _C_LNG 	 = 'l';
  _C_ULNG        = 'L';
  _C_FLT 	 = 'f';
  _C_DBL 	 = 'd';
  _C_BFLD	 = 'b';
  _C_VOID        = 'v';
  _C_UNDEF       = '?';
  _C_PTR	 = '^';
  _C_CHARPTR     = '*';
  _C_ARY_B       = '[';
  _C_ARY_E       = ']';
  _C_UNION_B     = '(';
  _C_UNION_E     = ')';
  _C_STRUCT_B    = '{';
  _C_STRUCT_E    = '}';

