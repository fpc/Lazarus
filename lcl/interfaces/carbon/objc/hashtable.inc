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
    hashtable2.h
    Scalable hash table.
    Copyright 1989-1996 NeXT Software, Inc.
}

//#warning The API in this header is obsoleted by NSHashtable.h

//#import <objc/objc.h>

{*************************************************************************
 *	Hash tables of arbitrary data
 *************************************************************************}

{ This module allows hashing of arbitrary data.  Such data must be pointers or integers, and client is responsible for allocating/deallocating this data.  A deallocation call-back is provided.
The objective C class HashTable is preferred when dealing with (key, values) associations because it is easier to use in that situation.
As well-behaved scalable data structures, hash tables double in size when they start becoming full, thus guaranteeing both average constant time access and linear size. }

type
  hash_t = function (const info, data: Pointer): uarith_t; cdecl;
  isEqual_t = function (const info, data1, data2: Pointer): cint; cdecl;
  free_t = procedure (const info, data: Pointer); cdecl;

  PNXHashTablePrototype = ^NXHashTablePrototype;
  
  NXHashTablePrototype = record
    hash: hash_t;
    isEqual: isEqual_t;
    free: free_t;
    style: cint; { reserved for future expansion; currently 0 }
  end;
    
{ the info argument allows a certain generality, such as freeing according to some owner information }
{ invariants assumed by the implementation: 
	1 - data1 = data2 => hash(data1) = hash(data2)
	    when data varies over time, hash(data) must remain invariant
		    e.g. if data hashes over a string key, the string must not be changed
	2- isEqual (data1, data2) => data1= data2
 }

type
  PNXHashTable = ^NXHashTable;

  NXHashTable = record
    prototype: PNXHashTablePrototype;
    count: cunsigned;
    nbBuckets: cunsigned;
    buckets: Pointer;
    info: Pointer;
  end;
    { private data structure; may change }
    
function NXCreateHashTableFromZone (prototype: NXHashTablePrototype; capacity: cunsigned; const info: Pointer; z: Pointer): PNXHashTable; cdecl; external;
function NXCreateHashTable (prototype: NXHashTablePrototype; capacity: cunsigned; const info: Pointer): PNXHashTable; cdecl; external;
    { if hash is 0, pointer hash is assumed }
    { if isEqual is 0, pointer equality is assumed }
    { if free is 0, elements are not freed }
    { capacity is only a hint; 0 creates a small table }
    { info allows call backs to be very general }

procedure NXFreeHashTable (table: PNXHashTable); cdecl; external;
    { calls free for each data, and recovers table }
	
procedure NXEmptyHashTable (table: PNXHashTable); cdecl; external;
    { does not deallocate table nor data; keeps current capacity }

procedure NXResetHashTable (table: PNXHashTable); cdecl; external;
    { frees each entry; keeps current capacity }

function NXCompareHashTables (table1, table2: PNXHashTable): BOOL; cdecl; external;
    { Returns YES if the two sets are equal (each member of table1 in table2, and table have same size) }

function NXCopyHashTable (table: PNXHashTable): PNXHashTable; cdecl; external;
    { makes a fresh table, copying data pointers, not data itself.  }
	
function NXCountHashTable (table: PNXHashTable): cunsigned; cdecl; external;
    { current number of data in table }
	
function NXHashMember (table: PNXHashTable; const data: Pointer): cint; cdecl; external;
    { returns non-0 iff data is present in table.
    Example of use when the hashed data is a struct containing the key,
    and when the callee only has a key:
	MyStruct	pseudo;
	pseudo.key = myKey;
	return NXHashMember (myTable, &pseudo)
    }
	
function NXHashGet (table: PNXHashTable; const data: Pointer): Pointer; cdecl; external;
    { return original table data or NULL.
    Example of use when the hashed data is a struct containing the key,
    and when the callee only has a key:
	MyStruct	pseudo;
	MyStruct	*original;
	pseudo.key = myKey;
	original = NXHashGet (myTable, &pseudo)
    }
	
function NXHashInsert (table: PNXHashTable; const data: Pointer): Pointer; cdecl; external;
    { previous data or NULL is returned. }
	
function NXHashInsertIfAbsent (table: PNXHashTable; const data: Pointer): Pointer; cdecl; external;
    { If data already in table, returns the one in table
    else adds argument to table and returns argument. }

function NXHashRemove (table: PNXHashTable; const data: Pointer): Pointer; cdecl; external;
    { previous data or NULL is returned }
	
{ Iteration over all elements of a table consists in setting up an iteration state and then to progress until all entries have been visited.  An example of use for counting elements in a table is:
    unsigned	count = 0;
    MyData	*data;
    NXHashState	state = NXInitHashState(table);
    while (NXNextHashState(table, &state, &data)) )
	count++;
    (
}

type
  NXHashState = record
    i, j: cint;
  end;
  
    { callers should not rely on actual contents of the struct }

function NXInitHashState(table: PNXHashTable): NXHashState; cdecl; external;

function NXNextHashState(table, state: PNXHashTable; data: PPointer): cint; cdecl; external;
    { returns 0 when all elements have been visited }

{*************************************************************************
 *	Conveniences for writing hash, isEqual and free functions
 *	and common prototypes
 *************************************************************************}

function NXPtrHash(const info, data: Pointer): uarith_t; cdecl; external;
    { scrambles the address bits; info unused }
function NXStrHash(const info, data: Pointer): uarith_t; cdecl; external;
    { string hashing; info unused }
function NXPtrIsEqual(const info, data1, data2: Pointer): cint; cdecl; external;
    { pointer comparison; info unused }
function NXStrIsEqual(const info, data1, data2: Pointer): cint; cdecl; external;
    { string comparison; NULL ok; info unused }
procedure NXNoEffectFree(const info, data: Pointer); cdecl; external;
    { no effect; info unused }
procedure NXReallyFree(const info, data: Pointer); cdecl; external;
    { frees it; info unused }

{ The two following prototypes are useful for manipulating set of pointers or set of strings; For them free is defined as NXNoEffectFree }
//OBJC_EXPORT const NXHashTablePrototype NXPtrPrototype;
    { prototype when data is a pointer (void *) }
//OBJC_EXPORT const NXHashTablePrototype NXStrPrototype;
    { prototype when data is a string (char *) }

{ following prototypes help describe mappings where the key is the first element of a struct and is either a pointer or a string.
For example NXStrStructKeyPrototype can be used to hash pointers to Example, where Example is:
	typedef struct (
	    char	*key;
	    int		data1;
	    ...
	    ) Example
    
For the following prototypes, free is defined as NXReallyFree.
 }
//OBJC_EXPORT const NXHashTablePrototype NXPtrStructKeyPrototype;
//OBJC_EXPORT const NXHashTablePrototype NXStrStructKeyPrototype;

{*************************************************************************
 *	Unique strings and buffers
 *************************************************************************}

{ Unique strings allows C users to enjoy the benefits of Lisp's atoms:
A unique string is a string that is allocated once for all (never de-allocated) and that has only one representant (thus allowing comparison with == instead of strcmp).  A unique string should never be modified (and in fact some memory protection is done to ensure that).  In order to more explicitly insist on the fact that the string has been uniqued, a synonym of (const char *) has been added, NXAtom. }

type
  NXAtom = PChar;

function NXUniqueString(const buffer: PChar): NXAtom; cdecl; external;
    { assumes that buffer is \0 terminated, and returns
     a previously created string or a new string that is a copy of buffer.
    If NULL is passed returns NULL.
    Returned string should never be modified.  To ensure this invariant,
    allocations are made in a special read only zone. }
	
function NXUniqueStringWithLength(const buffer: PChar; length: cint): NXAtom; cdecl; external;
    { assumes that buffer is a non NULL buffer of at least 
    length characters.  Returns a previously created string or 
    a new string that is a copy of buffer. 
    If buffer contains \0, string will be truncated.
    As for NXUniqueString, returned string should never be modified.  }
	
function NXUniqueStringNoCopy(const buffer: PChar): NXAtom; cdecl; external;
    { If there is already a unique string equal to string, returns the original.  
    Otherwise, string is entered in the table, without making a copy.  Argument should then never be modified.  }
	
function NXCopyStringBuffer(const buffer: PChar): PChar; cdecl; external;
    { given a buffer, allocates a new string copy of buffer.  
    Buffer should be \0 terminated; returned string is \0 terminated. }

function NXCopyStringBufferFromZone(const buffer: PChar; z: Pointer): PChar; cdecl; external;
    { given a buffer, allocates a new string copy of buffer.  
    Buffer should be \0 terminated; returned string is \0 terminated. }
