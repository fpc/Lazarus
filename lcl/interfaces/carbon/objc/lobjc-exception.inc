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
 
//  objc_exception.h
//  Support for Objective-C language Exceptions
//
//  Created by Blaine Garst on Fri Nov 01 2002.
//  Copyright (c) 2002-3 Apple Computer, Inc. All rights reserved.
//

//#import "objc/objc-class.h"

// compiler reserves a setjmp buffer + 4 words as localExceptionData

procedure objc_exception_throw(exception: id); cdecl; external;
procedure objc_exception_try_enter(localExceptionData: Pointer); cdecl; external;
procedure objc_exception_try_exit(localExceptionData: Pointer); cdecl; external;
function objc_exception_extract(localExceptionData: Pointer): id; cdecl; external;
function objc_exception_match(exceptionClass: _Class; exception: id): cint; cdecl; external;


type
  throw_exc_t = procedure (param1: id); cdecl;
  try_enter_t = procedure (param1: Pointer); cdecl;
  try_exit_t = procedure (param1: Pointer); cdecl;
  extract_t = function (param1: Pointer): id; cdecl;
  match_t = function (param1: _Class; param2: id): cint; cdecl;

  Pobjc_exception_functions_t = ^objc_exception_functions_t;

  objc_exception_functions_t = record
    version: cint;
    throw_exc: throw_exc_t;   // version 0
    try_enter: try_enter_t; // version 0
    try_exit: try_exit_t;   // version 0
    extract: extract_t;     // version 0
    match: match_t;	    // version 0
  end;

// get table; version tells how many
procedure objc_exception_get_functions(table: Pobjc_exception_functions_t); cdecl; external;

// set table
procedure objc_exception_set_functions(table: Pobjc_exception_functions_t); cdecl; external;

