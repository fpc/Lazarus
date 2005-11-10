{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    libc based wide string support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{$mode objfpc}

unit LazCWString;

interface

procedure SetCWidestringManager;

implementation

{$linklib c}

{$ifndef linux}  // Linux (and maybe glibc platforms in general), have iconv in glibc.
{$linklib iconv}
{$endif linux}

Uses 
  BaseUnix,
  ctypes,
  unix,
  unixtype,
  sysutils,
  initc,
  LCLProc;

const
{$ifdef Linux}
    libiconvname='c';  // is in libc under Linux.
{$else}
    libiconvname='iconv';
{$endif}

{$ifdef darwin}
type
// defined in libc, but there the iconv* functions are in the wrong lib
// they aren't defind in darwin/ptypes :(
  size_t = cuint32;
  psize_t = ^size_t;    
  wint_t = cint32;
  wchar_t = widechar;
  pwchar_t = ^wchar_t;
{$endif}

{ Case-mapping "arrays" }
//var
//  AnsiUpperChars: AnsiString; // 1..255
//  AnsiLowerChars: AnsiString; // 1..255
//  WideUpperChars: WideString; // 1..65535
//  WideLowerChars: WideString; // 1..65535

{ the following declarations are from the libc unit for linux so they
  might be very linux centric
  maybe this needs to be splitted in an os depend way later }
function towlower(__wc:wint_t):wint_t;cdecl;external libiconvname name 'towlower';
function towupper(__wc:wint_t):wint_t;cdecl;external libiconvname name 'towupper';
function wcscoll (__s1:pwchar_t; __s2:pwchar_t):cint;cdecl;external libiconvname name 'wcscoll';

const
{$ifdef linux}
  __LC_CTYPE = 0;
  _NL_CTYPE_CLASS = (__LC_CTYPE shl 16);
  _NL_CTYPE_CODESET_NAME = (_NL_CTYPE_CLASS)+14;
  CODESET = _NL_CTYPE_CODESET_NAME;
{$else linux}
{$ifdef darwin}
  CODESET = 0;
  ESysEILSEQ = 92;
{$else darwin}
{$ifdef FreeBSD} // actually FreeBSD5. internationalisation is afaik not default on 4.
  CODESET = 0;
{$else freebsd}
{$error lookup the value of CODESET in /usr/include/langinfo.h for your OS }
// and while doing it, check if iconv is in libc, and if the symbols are prefixed with iconv_ or libiconv_
{$endif FreeBSD}
{$endif darwin}
{$endif linux}

{ unicode encoding name }
{$ifdef FPC_LITTLE_ENDIAN}
  unicode_encoding = 'UNICODELITTLE';
{$else  FPC_LITTLE_ENDIAN}
  unicode_encoding = 'UNICODEBIG';
{$endif  FPC_LITTLE_ENDIAN}

{$ifdef darwin}
  _ICONVSYMPREFIX = 'lib';
{$else}  
  _ICONVSYMPREFIX = '';
{$endif}

type
  piconv_t = ^iconv_t;
  iconv_t = pointer;
  nl_item = cint;
  

function nl_langinfo(__item:nl_item):pchar;cdecl;external libiconvname name 'nl_langinfo';
function iconv_open(__tocode:pchar; __fromcode:pchar):iconv_t;cdecl;external libiconvname name _ICONVSYMPREFIX+'iconv_open';
function iconv(__cd:iconv_t; __inbuf:ppchar; __inbytesleft:psize_t; __outbuf:ppchar; __outbytesleft:psize_t):size_t;cdecl;external libiconvname name _ICONVSYMPREFIX+'iconv';
function iconv_close(__cd:iconv_t):cint;cdecl;external libiconvname name _ICONVSYMPREFIX+'iconv_close';

var
  iconv_ansi2wide,
  iconv_wide2ansi : iconv_t;

procedure Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
  var
    outlength,
    outoffset,
    srclen,
    outleft : size_t;
    srcpos : pwidechar;
    destpos: pchar;
    mynil : pchar;
    my0 : size_t;
  begin
    mynil:=nil;
    my0:=0;
    { rought estimation }
    setlength(dest,len*3);
    outlength:=len*3;
    srclen:=len*2;
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength;
    while iconv(iconv_wide2ansi,@srcpos,@srclen,@destpos,@outleft)=size_t(-1) do
      begin
        case fpgetCerrno of
          ESysEILSEQ:
            begin
              { skip and set to '?' }
              inc(srcpos);
              dec(srclen,2);
              destpos^:='?';
              inc(destpos);
              dec(outleft);
              { reset }
              iconv(iconv_wide2ansi,@mynil,@my0,@mynil,@my0);
            end;
          ESysE2BIG:
            begin
              outoffset:=destpos-pchar(dest);
              { extend }
              setlength(dest,outlength+cardinal(len)*3);
              inc(outleft,len*3);
              inc(outlength,len*3);
              { string could have been moved }
              destpos:=pchar(dest)+outoffset;
            end;
          else
            raise EConvertError.Create('iconv error');
        end;
      end;
    // truncate string
    setlength(dest,cardinal(length(dest))-outleft);
    //if Source<>nil then
    //  writeln('Wide2AnsiMove Source="',DbgWideStr(source),'" Dest="',DbgStr(Dest),'"');
  end;


procedure Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
  var
    outlength,
    outoffset,
    outleft : size_t;
    srcpos,
    destpos: pchar;
    //mynil : pchar;
    //my0 : size_t;
  begin
    //mynil:=nil;
    //my0:=0;
    // extra space
    outlength:=len+1;
    setlength(dest,outlength);
    outlength:=len+1;
    srcpos:=source;
    destpos:=pchar(dest);
    outleft:=outlength*2;
    while iconv(iconv_ansi2wide,@srcpos,@len,@destpos,@outleft)=size_t(-1) do
      begin
        case fpgetCerrno of
          ESysE2BIG:
            begin
              outoffset:=destpos-pchar(dest);
              { extend }
              setlength(dest,outlength+cardinal(len));
              inc(outleft,len*2);
              inc(outlength,len);
              { string could have been moved }
              destpos:=pchar(dest)+outoffset;
            end;
          else
            raise EConvertError.Create('iconv error');
        end;
      end;
    // truncate string
    setlength(dest,cardinal(length(dest))-outleft div 2);
    //if Source<>nil then
    //  debugln('Ansi2WideMove Source="',DbgStr(source),'" Dest="',DbgWideStr(Dest),'"');
  end;


function LowerWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=1 to length(s) do
      result[i]:=WideChar(towlower(wint_t(s[i])));
  end;


function UpperWideString(const s : WideString) : WideString;
  var
    i : SizeInt;
  begin
    SetLength(result,length(s));
    for i:=1 to length(s) do
      result[i]:=WideChar(towupper(wint_t(s[i])));
  end;


//function CompareWideString(const s1, s2 : WideString) : PtrInt;
//  begin
//  end;


//function CompareTextWideString(const s1, s2 : WideString): PtrInt;
//  begin
//  end;

Var
  CWideStringManager : TWideStringManager;

Procedure SetCWideStringManager;

begin
  CWideStringManager:=widestringmanager;
  With CWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Wide2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;

      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;
      {
      CompareWideStringProc
      CompareTextWideStringProc
      CharLengthPCharProc

      UpperAnsiStringProc
      LowerAnsiStringProc
      CompareStrAnsiStringProc
      CompareTextAnsiStringProc
      StrCompAnsiStringProc
      StrICompAnsiStringProc
      StrLCompAnsiStringProc
      StrLICompAnsiStringProc
      StrLowerAnsiStringProc
      StrUpperAnsiStringProc
      }
    end;
  SetWideStringManager(CWideStringManager);
end;


initialization
  SetCWideStringManager;
  { init conversion tables }
  iconv_wide2ansi:=iconv_open(nl_langinfo(CODESET),unicode_encoding);
  iconv_ansi2wide:=iconv_open(unicode_encoding,nl_langinfo(CODESET));
finalization
  iconv_close(iconv_ansi2wide);
end.
