{%MainUnit android_sockets.pas}
{*
 * Copyright (C) 2008 The Android Open Source Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *}
{.$ifndef _ARPA_INET_H_}
{.$define _ARPA_INET_H_}

{#include <stdint.h>
#include <sys/types.h>
#include <netinet/in.h>}

type
  in_addr_t = Cardinal;//uint32_t;
  Pin_addr_t = ^in_addr_t;

function inet_addr(const Param1: PChar): Cardinal{uint32_t}; cdecl; external libname name 'inet_addr';

function inet_aton(const Param1: PChar; Param2: Pin_addr): cint; cdecl; external libname name 'inet_aton';
function inet_ntoa(Param1: Tin_addr): PChar; cdecl; external libname name 'inet_ntoa';

function inet_pton(Param1: cint; const Param2: PChar; Param3: Pointer): cint; cdecl; external libname name 'inet_pton';
function inet_ntop(Param1: cint; const Param2: Pointer; Param3: PChar; Param4: csize_t): PChar; cdecl; external libname name 'inet_ntop';

function inet_nsap_addr(const Param1: PChar; Param2: PByte{uchar}; Param3: cint): cuint; cdecl; external libname name 'inet_nsap_addr';
function inet_nsap_ntoa(Param1: cint; const Param2: PChar; Param3: PChar): PChar; cdecl; external libname name 'inet_nsap_ntoa';

{.$endif} //* _ARPA_INET_H_ */


