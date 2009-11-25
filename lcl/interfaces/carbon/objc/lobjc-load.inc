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
 *	lobjc-load.h
 *	Copyright 1988-1996, NeXT Software, Inc.
 }

//#import <objc/objc-class.h>

//#import <mach-o/loader.h>

type
  class_callback_t = procedure (param1: _Class; param2: Category); cdecl;
  unloadCallback_t = procedure (param1: _Class; param2: Category); cdecl; 

{ dynamically loading Mach-O object files that contain Objective-C code }

function objc_loadModules (
	modlist: array of PChar; 
	errStream: Pointer;
	class_callback: class_callback_t;
	{headerType} hdr_addr: PPointer; //PPmach_header;
	debug_file: PChar
): clong; cdecl; external;

function objc_loadModule (
	moduleName: PChar; 
	class_callback: class_callback_t;
	errorCode: Pcint): cint; cdecl; external;

function objc_unloadModules(
	errorStream: Pointer;				        { input (optional) }
	unloadCallback: unloadCallback_t		{ input (optional) }
): clong; cdecl; external;

procedure objc_register_header_name(
	name: PChar					{ input }
); cdecl; external;

procedure objc_register_header(
	name: PChar					{ input }
); cdecl; external;

