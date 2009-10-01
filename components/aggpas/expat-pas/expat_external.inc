//----------------------------------------------------------------------------
// Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
//                                and Clark Cooper
// Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.pasports.org/pasexpat
// Copyright (c) 2006
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// [Pascal Port History] -----------------------------------------------------
//
// 03.05.2006-Milano: Unit port establishment
//
{ expat_external.inc }
(* General Integer Types *)
 int8   = shortint;
 int8u  = byte;
 int16  = smallint;
 int16u = word;
 int32  = longint;
 int32u = longword;
 int64  = system.int64;

{$IFDEF FPC }
 int64u = qword;

{$ELSE }
 int64u = system.int64;

{$ENDIF }

(* General Character Types *)
 char8  = char;
 char16 = int16u;
 char32 = int32u;

(* C/C++ compatibility Types *)
 int      = int32;
 unsigned = int32u;
 size_t   = int32u;

(* Pascal Pointer Computation Type *)
{$IFDEF CPU64 }
 ptrcomp = system.int64;

{$ELSE }
 ptrcomp = integer;
 
{$ENDIF } 

(* Type Pointers *)
 int8_ptr         = ^int8;
 int8_ptr_ptr     = ^int8_ptr;

 int8u_ptr        = ^int8u;
 int8u_ptr_ptr    = ^int8u_ptr;

 int16_ptr        = ^int16;
 int16_ptr_ptr    = ^int16_ptr;

 int16u_ptr       = ^int16u;
 int16u_ptr_ptr   = ^int16u_ptr;

 int32_ptr        = ^int32;
 int32_ptr_ptr    = ^int32_ptr;

 int32u_ptr       = ^int32u;
 int32u_ptr_ptr   = ^int32u_ptr;

 int64_ptr        = ^int64;
 int64_ptr_ptr    = ^int64_ptr;

 int64u_ptr       = ^int64u;
 int64u_ptr_ptr   = ^int64u_ptr;

 char8_ptr        = ^char8;
 char8_ptr_ptr    = ^char8_ptr;

 char16_ptr       = ^char16;
 char16_ptr_ptr   = ^char16_ptr;

 char32_ptr       = ^char32;
 char32_ptr_ptr   = ^char32_ptr;

 int_ptr          = ^int;
 int_ptr_ptr      = ^int_ptr;

 unsigned_ptr     = ^unsigned;
 unsigned_ptr_ptr = ^unsigned_ptr;

 char_ptr         = ^char;
 char_ptr_ptr     = ^char_ptr;

(* Expat Types *)
{$IFDEF XML_UNICODE } // Information is UTF-16 encoded.
{$IFDEF XML_UNICODE_WCHAR_T }
 XML_Char  = int16u;
 XML_LChar = int16u;

{$ELSE }
 XML_Char  = word;
 XML_LChar = char;

{$ENDIF }

{$ELSE }              // Information is UTF-8 encoded.
 XML_Char  = char;
 XML_LChar = char;

{$ENDIF }

 XML_Char_ptr  = ^XML_Char;
 XML_LChar_ptr = ^XML_LChar;

 XML_Char_ptr_ptr = ^XML_Char_ptr;

{$IFDEF XML_LARGE_SIZE } // Use large integers for file/stream positions.
 XML_Index = int64;
 XML_Size  = int64u;

{$ELSE }
 XML_Index = longint;
 XML_Size  = longword;

{$ENDIF }

