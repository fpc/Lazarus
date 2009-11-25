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
 *	lobjc.h
 *	Copyright 1988-1996, NeXT Software, Inc.
 }

//#import <objc/lobjc-api.h>		// for OBJC_EXPORT

type
  P_Class = ^_Class;

  _Class = Pointer; // Is actually Pobjc_class, declared on lobjc-class.inc
                    // But, because this type is declared on another file,
                    // which depends on this one, it would not be possible
                    // to declare it as Pobjc_class without changing the
                    // headers a lot.
                    //
                    // Cast variables of this type to Pobjc_class when necessary

  Pobjc_object = ^objc_object;

  objc_object = record
	isa: _Class;
  end;

  Pid = ^id;
  id = Pobjc_object;

  Pobjc_selector = Pointer;
  
  PSEL = ^SEL;
  
  SEL = Pobjc_selector;

  IMP = function (param1: id; param2: SEL; param3: array of const): id; cdecl;

  BOOL = ShortInt;

// BOOL is explicitly signed so @encode(BOOL) == "c" rather than "C" 
// even if -funsigned-char is used.


const
  YES            = BOOL(1);
  NO             = BOOL(0);

{#ifndef __OBJC_GC__
# define __strong
#endif}

{$ifndef STRICT_OPENSTEP}

type
  STR = PChar; // Possibly change to TSTR

function sel_isMapped(_sel: SEL): BOOL; cdecl; external;
function sel_getName(_sel: SEL): PChar; cdecl; external;
function sel_getUid(const str: PChar): SEL; cdecl; external;
function sel_registerName(const str: PChar): SEL; cdecl; external;
function object_getClassName(obj: id): PChar; cdecl; external;
function object_getIndexedIvars(obj: id): Pointer; cdecl; external;

{#define ISSELECTOR(sel) sel_isMapped(sel)
#define SELNAME(sel)	sel_getName(sel)
#define SELUID(str)	sel_getUid(str)
#define NAMEOF(obj)     object_getClassName(obj)
#define IV(obj)         object_getIndexedIvars(obj)}

{$if defined(__osf__) and defined(__alpha__)}
type
  arith_t = clong;
  uarith_t = culong;
const ARITH_SHIFT = 32;
{$else}
type
  arith_t = cint;
  uarith_t = cunsigned;
const ARITH_SHIFT = 16;
{$endif}

{$endif}  { !STRICT_OPENSTEP }


