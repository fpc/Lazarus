{
 * Copyright (c) 2004 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * Copyright (c) 2004 Apple Computer, Inc.  All Rights Reserved.
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
 *	lobjc-auto.h
 *	Copyright 2004 Apple Computer, Inc.
 }

//#import <objc/objc.h>
//#include <sys/types.h>

{ Collection utilities }

const
  OBJC_GENERATIONAL = (1 shl 0);

procedure objc_collect_if_needed(options: culong); cdecl; external;
function objc_numberAllocated(): cuint; cdecl; external;
function objc_collecting_enabled(): BOOL; cdecl; external;

{ Memory management }
function objc_allocate_object(cls: _Class; extra: cint): id; cdecl; external;

{ Write barriers }
function objc_assign_strongCast(val: id; dest: Pid): id; cdecl; external;
function objc_assign_global(val: id; dest: Pid): id; cdecl; external;
function objc_assign_ivar(value, dest: id; offset: cuint): id; cdecl; external;
function objc_memmove_collectable(dst: Pointer; const src: Pointer; size: size_t): Pointer; cdecl; external;

{ Testing tools }
function objc_is_finalized(ptr: Pointer): BOOL; cdecl; external;

