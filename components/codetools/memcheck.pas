{
 ***************************************************************************
 *                                                                         *
 * This unit is an altered heaptrc.pp from the fpc sources                 *
 *                                                                         *
 ***************************************************************************
}
{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Heap tracer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit MemCheck;

{$goto on}

interface

Procedure DumpHeap;
Procedure MarkHeap;

{ define EXTRA to add more
  tests :
   - keep all memory after release and
   check by CRC value if not changed after release
   WARNING this needs extremely much memory (PM) }

type
    FillExtraInfoType = procedure(p : pointer);

    { allows to add several longint value that can help
      to debug :
      see for instance ppheap.pas unit of the compiler source PM }

Procedure SetExtraInfo( size : longint;func : FillExtraInfoType);
Procedure SetHeapTraceOutput(const name : string);

const
  { tracing level
    splitted in two if memory is released !! }
{$ifdef EXTRA}
  tracesize = 16;
{$else EXTRA}
  tracesize = 8;
{$endif EXTRA}
  quicktrace : boolean=true;
  { calls halt() on error by default !! }
  HaltOnError : boolean = true;
  { set this to true if you suspect that memory
    is freed several times }
{$ifdef EXTRA}
  keepreleased : boolean=true;
  add_tail : boolean = true;
{$else EXTRA}
  keepreleased : boolean=false;
  add_tail : boolean = false;
{$endif EXTRA}
  { put crc in sig
    this allows to test for writing into that part }
  usecrc : boolean = true;

// StartAddition for CodeTools
procedure CheckHeap;
procedure CheckHeap(const txt: ansistring);
procedure CheckHeapWrtMemCnt(const txt: ansistring);

function MemCheck_getmem_cnt: longint;
function MemCheck_freemem_cnt: longint;
function MemCheck_getmem_size: longint;
function MemCheck_freemem_size: longint;
function MemCheck_getmem8_size: longint;
function MemCheck_freemem8_size: longint;

// Addition for CodeTools

implementation


type
   plongint = ^longint;

const
  { allows to add custom info in heap_mem_info }
  extra_info_size : longint = 0;
  exact_info_size : longint = 0;
  EntryMemUsed    : longint = 0;
  { function to fill this info up }
  fill_extra_info : FillExtraInfoType = nil;
  error_in_heap : boolean = false;
  inside_trace_getmem : boolean = false;

type
  pheap_mem_info = ^theap_mem_info;
  { warning the size of theap_mem_info
    must be a multiple of 8
    because otherwise you will get
    problems when releasing the usual memory part !!
    sizeof(theap_mem_info = 16+tracesize*4 so
    tracesize must be even !! PM }
  theap_mem_info = record
    previous,
    next     : pheap_mem_info;
    size     : longint;
    sig      : longint;
{$ifdef EXTRA}
    release_sig : longint;
    prev_valid  : pheap_mem_info;
{$endif EXTRA}
    calls    : array [1..tracesize] of longint;
    extra_info : record
                 end;
  end;

var
  ptext : ^text;
  ownfile : text;
{$ifdef EXTRA}
  error_file : text;
  heap_valid_first,
  heap_valid_last : pheap_mem_info;
{$endif EXTRA}
  heap_mem_root : pheap_mem_info;
  getmem_cnt,
  freemem_cnt   : longint;
  getmem_size,
  freemem_size   : longint;
  getmem8_size,
  freemem8_size   : longint;

// StartAddition for CodeTools
procedure CheckHeap;
var p: pointer;
begin
  writeln('>>> memcheck.pp - CheckHeap');
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=true;
end;

procedure CheckHeap(const txt: ansistring);
var p: pointer;
begin
  writeln('>>> memcheck.pp - CheckHeap "',txt,'"');
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=true;
end;

procedure CheckHeapWrtMemCnt(const txt: ansistring);
var p: pointer;
begin
  writeln('>>> memcheck.pp - CheckHeap2 "',txt,'" ',MemCheck_getmem_cnt);
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=true;
end;

function MemCheck_getmem_cnt: longint;
begin
  MemCheck_getmem_cnt:=getmem_cnt;
end;

function MemCheck_freemem_cnt: longint;
begin
  MemCheck_freemem_cnt:=freemem_cnt;
end;

function MemCheck_getmem_size: longint;
begin
  MemCheck_getmem_size:=getmem_size;
end;

function MemCheck_freemem_size: longint;
begin
  MemCheck_freemem_size:=freemem_size;
end;

function MemCheck_getmem8_size: longint;
begin
  MemCheck_getmem8_size:=getmem8_size;
end;

function MemCheck_freemem8_size: longint;
begin
  MemCheck_freemem8_size:=freemem8_size;
end;
// Addition for CodeTools

{*****************************************************************************
                                   Crc 32
*****************************************************************************}

var
{$ifdef Delphi}
  Crc32Tbl : array[0..255] of longword;
{$else Delphi}
  Crc32Tbl : array[0..255] of longint;
{$endif Delphi}

procedure MakeCRC32Tbl;
var
{$ifdef Delphi}
  crc : longword;
{$else Delphi}
  crc : longint;
{$endif Delphi}
  i,n : byte;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if odd(crc) then
       crc:=(crc shr 1) xor $edb88320
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


{$ifopt R+}
{$define Range_check_on}
{$endif opt R+}

{$R- needed here }

Function UpdateCrc32(InitCrc:longint;var InBuf;InLen:Longint):longint;
var
  i : longint;
  p : pchar;
begin
  p:=@InBuf;
  for i:=1 to InLen do
   begin
     InitCrc:=Crc32Tbl[byte(InitCrc) xor byte(p^)] xor (InitCrc shr 8);
     inc(longint(p));
   end;
  UpdateCrc32:=InitCrc;
end;

Function calculate_sig(p : pheap_mem_info) : longint;
var
   crc : longint;
   pl : plongint;
begin
   crc:=$ffffffff;
   crc:=UpdateCrc32(crc,p^.size,sizeof(longint));
   crc:=UpdateCrc32(crc,p^.calls,tracesize*sizeof(longint));
   if extra_info_size>0 then
     crc:=UpdateCrc32(crc,p^.extra_info,exact_info_size);
   if add_tail then
     begin
        { Check also 4 bytes just after allocation !! }
        pl:=pointer(p)+extra_info_size+sizeof(theap_mem_info)+p^.size;
        crc:=UpdateCrc32(crc,pl^,sizeof(longint));
     end;
   calculate_sig:=crc;
end;

{$ifdef EXTRA}
Function calculate_release_sig(p : pheap_mem_info) : longint;
var
   crc : longint;
   pl : plongint;
begin
   crc:=$ffffffff;
   crc:=UpdateCrc32(crc,p^.size,sizeof(longint));
   crc:=UpdateCrc32(crc,p^.calls,tracesize*sizeof(longint));
   if extra_info_size>0 then
     crc:=UpdateCrc32(crc,p^.extra_info,exact_info_size);
   { Check the whole of the whole allocation }
   pl:=pointer(p)+extra_info_size+sizeof(theap_mem_info);
   crc:=UpdateCrc32(crc,pl^,p^.size);
   { Check also 4 bytes just after allocation !! }
   if add_tail then
     begin
        { Check also 4 bytes just after allocation !! }
        pl:=pointer(p)+extra_info_size+sizeof(theap_mem_info)+p^.size;
        crc:=UpdateCrc32(crc,pl^,sizeof(longint));
     end;
   calculate_release_sig:=crc;
end;
{$endif EXTRA}

{$ifdef Range_check_on}
{$R+}
{$undef Range_check_on}
{$endif Range_check_on}

{*****************************************************************************
                                Helpers
*****************************************************************************}

procedure call_stack(pp : pheap_mem_info;var ptext : text);
var
  i  : longint;
begin
  writeln(ptext,'Call trace for block 0x',hexstr(longint(pointer(pp)+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(ptext,BackTraceStrFunc(pp^.calls[i]));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(pointer(@pp^.extra_info)+4*i)^);
end;

procedure call_free_stack(pp : pheap_mem_info;var ptext : text);
var
  i  : longint;

begin
  writeln(ptext,'Call trace for block at 0x',hexstr(longint(pointer(pp)+sizeof(theap_mem_info)),8),' size ',pp^.size);
  for i:=1 to tracesize div 2 do
   if pp^.calls[i]<>0 then
     writeln(ptext,BackTraceStrFunc(pp^.calls[i]));
  writeln(ptext,' was released at ');
  for i:=(tracesize div 2)+1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(ptext,BackTraceStrFunc(pp^.calls[i]));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(pointer(@pp^.extra_info)+4*i)^);
end;


procedure dump_already_free(p : pheap_mem_info;var ptext : text);
begin
  Writeln(ptext,'Marked memory at 0x',HexStr(longint(pointer(p)+sizeof(theap_mem_info)),8),' released');
  call_free_stack(p,ptext);
  Writeln(ptext,'freed again at');
  dump_stack(ptext,get_caller_frame(get_frame));
end;

procedure dump_error(p : pheap_mem_info;var ptext : text);
begin
  Writeln(ptext,'Marked memory at 0x',HexStr(longint(pointer(p)+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong signature $',hexstr(p^.sig,8)
    ,' instead of ',hexstr(calculate_sig(p),8));
  dump_stack(ptext,get_caller_frame(get_frame));
end;

{$ifdef EXTRA}
procedure dump_change_after(p : pheap_mem_info;var ptext : text);
 var pp : pchar;
     i : longint;
begin
  Writeln(ptext,'Marked memory at 0x',HexStr(longint(pointer(p)+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong release CRC $',hexstr(p^.release_sig,8)
    ,' instead of ',hexstr(calculate_release_sig(p),8));
  Writeln(ptext,'This memory was changed after call to freemem !');
  call_free_stack(p,ptext);
  pp:=pointer(p)+sizeof(theap_mem_info)+extra_info_size;
  for i:=0 to p^.size-1 do
    if byte(pp[i])<>$F0 then
      Writeln(ptext,'offset',i,':$',hexstr(i,8),'"',pp[i],'"');
end;
{$endif EXTRA}

procedure dump_wrong_size(p : pheap_mem_info;size : longint;var ptext : text);
var
  i : longint;
begin
  Writeln(ptext,'Marked memory at 0x',HexStr(longint(pointer(p)+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong size : ',p^.size,' allocated ',size,' freed');
  dump_stack(ptext,get_caller_frame(get_frame));
  for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(@p^.extra_info+4*i)^);
  call_stack(p,ptext);
end;


function is_in_getmem_list (p : pheap_mem_info) : boolean;
var
  i  : longint;
  pp : pheap_mem_info;
begin
  is_in_getmem_list:=false;
  pp:=heap_mem_root;
  i:=0;
  while pp<>nil do
   begin
     if ((pp^.sig<>$DEADBEEF) or usecrc) and
        ((pp^.sig<>calculate_sig(pp)) or not usecrc) and
        (pp^.sig <> $AAAAAAAA) then
      begin
        writeln(ptext^,'error in linked list of heap_mem_info');
        RunError(204);
      end;
     if pp=p then
      is_in_getmem_list:=true;
     pp:=pp^.previous;
     inc(i);
     if i>getmem_cnt-freemem_cnt then
      writeln(ptext^,'error in linked list of heap_mem_info');
   end;
end;


{*****************************************************************************
                               TraceGetMem
*****************************************************************************}

Function TraceGetMem(size:longint):pointer;
var
  i,bp : longint;
  pl : plongint;
  p : pointer;
begin
  inc(getmem_size,size);
  inc(getmem8_size,((size+7) div 8)*8);
{ Do the real GetMem, but alloc also for the info block }
  bp:=size+sizeof(theap_mem_info)+extra_info_size;
  if add_tail then
    inc(bp,sizeof(longint));
  p:=SysGetMem(bp);
{ Create the info block }
  pheap_mem_info(p)^.sig:=$DEADBEEF;
  pheap_mem_info(p)^.size:=size;
  if add_tail then
    begin
      pl:=pointer(p)+bp-sizeof(longint);
      pl^:=$DEADBEEF;
    end;
  bp:=get_caller_frame(get_frame);
  for i:=1 to tracesize do
   begin
     pheap_mem_info(p)^.calls[i]:=get_caller_addr(bp);
     bp:=get_caller_frame(bp);
   end;
  { insert in the linked list }
  if heap_mem_root<>nil then
   heap_mem_root^.next:=pheap_mem_info(p);
  pheap_mem_info(p)^.previous:=heap_mem_root;
  pheap_mem_info(p)^.next:=nil;
{$ifdef EXTRA}
  pheap_mem_info(p)^.prev_valid:=heap_valid_last;
  heap_valid_last:=pheap_mem_info(p);
  if not assigned(heap_valid_first) then
    heap_valid_first:=pheap_mem_info(p);
{$endif EXTRA}
  heap_mem_root:=p;
  { must be changed before fill_extra_info is called
    because checkpointer can be called from within
    fill_extra_info PM }
  inc(getmem_cnt);
  if assigned(fill_extra_info) then
    begin
      inside_trace_getmem:=true;
      fill_extra_info(@pheap_mem_info(p)^.extra_info);
      inside_trace_getmem:=false;
    end;
{ update the pointer }
  if usecrc then
    pheap_mem_info(p)^.sig:=calculate_sig(pheap_mem_info(p));
  inc(p,sizeof(theap_mem_info)+extra_info_size);
  TraceGetmem:=p;
end;


{*****************************************************************************
                                TraceFreeMem
*****************************************************************************}

function TraceFreeMemSize(var p:pointer;size:longint):longint;
var
  i,bp, ppsize : longint;
  pp : pheap_mem_info;
{$ifdef EXTRA}
  pp2 : pheap_mem_info;
{$endif}
begin
  inc(freemem_size,size);
  inc(freemem8_size,((size+7) div 8)*8);
  ppsize:= size + sizeof(theap_mem_info)+extra_info_size;
  if add_tail then
    ppsize:=ppsize+sizeof(longint);
  dec(p,sizeof(theap_mem_info)+extra_info_size);
  pp:=pheap_mem_info(p);
  if not quicktrace and not(is_in_getmem_list(pp)) then
    RunError(204);
  if (pp^.sig=$AAAAAAAA) and not usecrc then
    begin
       error_in_heap:=true;
       dump_already_free(pp,ptext^);
       if haltonerror then halt(1);
    end
  else if ((pp^.sig<>$DEADBEEF) or usecrc) and
        ((pp^.sig<>calculate_sig(pp)) or not usecrc) then
    begin
       error_in_heap:=true;
       dump_error(pp,ptext^);
{$ifdef EXTRA}
       dump_error(pp,error_file);
{$endif EXTRA}
       { don't release anything in this case !! }
       if haltonerror then halt(1);
       exit;
    end
  else if pp^.size<>size then
    begin
       error_in_heap:=true;
       dump_wrong_size(pp,size,ptext^);
{$ifdef EXTRA}
       dump_wrong_size(pp,size,error_file);
{$endif EXTRA}
       if haltonerror then halt(1);
       { don't release anything in this case !! }
       exit;
    end;
  { now it is released !! }
  pp^.sig:=$AAAAAAAA;
  if not keepreleased then
    begin
       if pp^.next<>nil then
         pp^.next^.previous:=pp^.previous;
       if pp^.previous<>nil then
         pp^.previous^.next:=pp^.next;
       if pp=heap_mem_root then
         heap_mem_root:=heap_mem_root^.previous;
    end
  else
    begin
       bp:=get_caller_frame(get_frame);
       for i:=(tracesize div 2)+1 to tracesize do
        begin
          pp^.calls[i]:=get_caller_addr(bp);
          bp:=get_caller_frame(bp);
        end;
    end;
  inc(freemem_cnt);
  { release the normal memory at least !! }
  { this way we keep all info about all released memory !! }
  if keepreleased then
    begin
{$ifndef EXTRA}
       dec(ppsize,sizeof(theap_mem_info)+extra_info_size);
       inc(p,sizeof(theap_mem_info)+extra_info_size);
{$else EXTRA}
      inc(p,sizeof(theap_mem_info)+extra_info_size);
      fillchar(p^,size,#240); { $F0 will lead to GFP if used as pointer ! }
      { We want to check if the memory was changed after release !! }
       pp^.release_sig:=calculate_release_sig(pp);
       if pp=heap_valid_last then
         begin
            heap_valid_last:=pp^.prev_valid;
            if pp=heap_valid_first then
              heap_valid_first:=nil;
            exit;
         end;
       pp2:=heap_valid_last;
       while assigned(pp2) do
         begin
            if pp2^.prev_valid=pp then
              begin
                 pp2^.prev_valid:=pp^.prev_valid;
                 if pp=heap_valid_first then
                   heap_valid_first:=pp2;
                 exit;
              end
            else
              pp2:=pp2^.prev_valid;
         end;
       exit;
{$endif EXTRA}
    end;
  i:=SysFreeMemSize(p,ppsize);
  dec(i,sizeof(theap_mem_info)+extra_info_size);
  if add_tail then
   dec(i,sizeof(longint));
  TraceFreeMemSize:=i;
end;


function TraceMemSize(p:pointer):Longint;
var
  l : longint;
begin
  l:=SysMemSize(p-(sizeof(theap_mem_info)+extra_info_size));
  dec(l,sizeof(theap_mem_info)+extra_info_size);
  if add_tail then
   dec(l,sizeof(longint));
  TraceMemSize:=l;
end;


function TraceFreeMem(var p:pointer):longint;
var
  size : longint;
  pp : pheap_mem_info;
begin
  pp:=pheap_mem_info(pointer(p)-(sizeof(theap_mem_info)+extra_info_size));
  size:=TraceMemSize(p);
  { this can never happend normaly }
  if pp^.size>size then
   begin
     dump_wrong_size(pp,size,ptext^);
{$ifdef EXTRA}
     dump_wrong_size(pp,size,error_file);
{$endif EXTRA}
   end;
  TraceFreeMem:=TraceFreeMemSize(p,pp^.size);
end;


{*****************************************************************************
                                ReAllocMem
*****************************************************************************}

function TraceReAllocMem(var p:pointer;size:longint):Pointer;
var
  newP: pointer;
  oldsize,
  i,bp : longint;
  pl : plongint;
  pp : pheap_mem_info;
begin
{ Free block? }
  if size=0 then
   begin
     if p<>nil then
      TraceFreeMem(p);
     TraceReallocMem:=P;
     exit;
   end;
{ Allocate a new block? }
  if p=nil then
   begin
     p:=TraceGetMem(size);
     TraceReallocMem:=P;
     exit;
   end;
{ Resize block }
  dec(p,sizeof(theap_mem_info)+extra_info_size);
  pp:=pheap_mem_info(p);
  { test block }
  if ((pp^.sig<>$DEADBEEF) or usecrc) and
     ((pp^.sig<>calculate_sig(pp)) or not usecrc) then
   begin
     error_in_heap:=true;
     dump_error(pp,ptext^);
{$ifdef EXTRA}
     dump_error(pp,error_file);
{$endif EXTRA}
     { don't release anything in this case !! }
     if haltonerror then halt(1);
     exit;
   end;
  { Do the real ReAllocMem, but alloc also for the info block }
  bp:=size+sizeof(theap_mem_info)+extra_info_size;
  if add_tail then
   inc(bp,sizeof(longint));
  { the internal ReAllocMem is not allowed to move any data }
  if not SysTryResizeMem(p,bp) then
   begin
     { restore p }
     inc(p,sizeof(theap_mem_info)+extra_info_size);
     { get a new block }
     oldsize:=TraceMemSize(p);
     newP := TraceGetMem(size);
     { move the data }
     if newP <> nil then
       move(p^,newP^,oldsize);
     { release p }
     traceFreeMem(p);
     p := newP;
     traceReAllocMem := p;
     exit;
   end;
  pp:=pheap_mem_info(p);
{ adjust like a freemem and then a getmem, so you get correct
  results in the summary display }
  inc(freemem_size,pp^.size);
  inc(freemem8_size,((pp^.size+7) div 8)*8);
  inc(getmem_size,size);
  inc(getmem8_size,((size+7) div 8)*8);
{ Create the info block }
  pp^.sig:=$DEADBEEF;
  pp^.size:=size;
  if add_tail then
    begin
      pl:=pointer(p)+bp-sizeof(longint);
      pl^:=$DEADBEEF;
    end;
  bp:=get_caller_frame(get_frame);
  for i:=1 to tracesize do
   begin
     pp^.calls[i]:=get_caller_addr(bp);
     bp:=get_caller_frame(bp);
   end;
  if assigned(fill_extra_info) then
    fill_extra_info(@pp^.extra_info);
{ update the pointer }
  if usecrc then
    pp^.sig:=calculate_sig(pp);
  inc(p,sizeof(theap_mem_info)+extra_info_size);
  TraceReAllocmem:=p;
end;


{***************************************************************************** Check pointer *****************************************************************************}
{$ifndef unix}
  {$S-}
{$endif}

{$ifdef go32v2}
var
   __stklen : cardinal;external name '__stklen';
   __stkbottom : cardinal;external name '__stkbottom';
   edata : cardinal; external name 'edata';
   heap_at_init : pointer;
{$endif go32v2}

{$ifdef win32}
var
   StartUpHeapEnd : pointer;
   { I found no symbol for start of text section :(
     so we usee the _mainCRTStartup which should be
     in wprt0.ow or wdllprt0.ow PM }
   text_begin : cardinal;external name '_mainCRTStartup';
   data_end : cardinal;external name '__data_end__';
{$endif}

{$ifdef unix}
const
  global_stack_top : cardinal = 0;

procedure _start; cdecl; external;
{$endif unix}

procedure CheckPointer(p : pointer);[saveregisters,public, alias : 'FPC_CHECKPOINTER'];
var
  i  : longint;
  pp : pheap_mem_info;
  get_ebp,stack_top : cardinal;
  data_end : cardinal;
label
  _exit;
begin
  if p=nil then
    goto _exit;

  i:=0;

{$ifdef go32v2}
  if cardinal(p)<$1000 then
    runerror(216);
  asm
     movl %ebp,get_ebp
     leal edata,%eax
     movl %eax,data_end
  end;
  stack_top:=__stkbottom+__stklen;
  { allow all between start of code and end of data }
  if cardinal(p)<=data_end then
    goto _exit;
  { .bss section }
  if cardinal(p)<=cardinal(heap_at_init) then
    goto _exit;
  { stack can be above heap !! }

  if (cardinal(p)>=get_ebp) and (cardinal(p)<=stack_top) then
    goto _exit;
{$endif go32v2}

{$ifdef unix}
  if (cardinal(p)>=cardinal(@_start)) and
     (cardinal(p)<=cardinal(heaporg)) then exit;
{$ifdef cpui386}
  asm
    movl %ebp, get_ebp
  end;
{$endif cpui386}
{$ifdef cpu68k}
  asm
    move.l a6, get_ebp
  end;
{$endif cpu86}
  if cardinal (p) > cardinal (get_ebp) then
    begin
      if (global_stack_top = 0) then
        begin
{$ifdef cpui386}
          asm
            movl %ebp,%eax
	    movl %eax,%ebx
          .Lnext:
            orl  %eax,%eax
            je   .Ltopfound
            movl (%eax),%eax
            cmpl %eax,%ebx
            jae  .Ltopfound
            movl %eax,%ebx
            jmp  .Lnext
          .Ltopfound:
            movl %ebx,global_stack_top
          end;
{$endif cpui386}
{$ifdef cpu68k}
          asm
            move.l a6,d0
	    move.l d0,d1
          @Lnext:
            or.l  d0,d0
            beq   @Ltopfound
            move.l d0,a0
            move.l (a0),d0
            cmp.l a0,a1
            bgt  @Ltopfound
            move.l a0,a1
            bra  @Lnext
          @Ltopfound:
            move.l a1,global_stack_top
          end;
{$endif cpu86}
          { argv and argc are above this value :( }
          global_stack_top := global_stack_top or $fffff;
        end;
      if cardinal(p) <= global_stack_top then
        exit;
    end;
{$endif unix}

  { I don't know where the stack is in other OS !! }
{$ifdef win32}
  if (cardinal(p)>=$40000) and (p<=HeapOrg) then
    goto _exit;
  { inside stack ? }
  asm
     movl %ebp,get_ebp
  end;
  if (cardinal(p)>get_ebp) and
     (cardinal(p)<Win32StackTop) then
    goto _exit;
{$endif win32}

  if p>=heapptr then
    runerror(216);
  { first try valid list faster }

{$ifdef EXTRA}
  pp:=heap_valid_last;
  while pp<>nil do
   begin
     { inside this valid block ! }
     { we can be changing the extrainfo !! }
     if (cardinal(p)>=cardinal(pp)+sizeof(theap_mem_info){+extra_info_size}) and
        (cardinal(p)<=cardinal(pp)+sizeof(theap_mem_info)+extra_info_size+pp^.size) then
       begin
          { check allocated block }
          if ((pp^.sig=$DEADBEEF) and not usecrc) or
             ((pp^.sig=calculate_sig(pp)) and usecrc) or
          { special case of the fill_extra_info call }
             ((pp=heap_valid_last) and usecrc and (pp^.sig=$DEADBEEF)
              and inside_trace_getmem) then
            goto _exit
          else
            begin
              writeln(ptext^,'corrupted heap_mem_info');
              dump_error(pp,ptext^);
              halt(1);
            end;
       end
     else
       pp:=pp^.prev_valid;
     inc(i);
     if i>getmem_cnt-freemem_cnt then
      begin
         writeln(ptext^,'error in linked list of heap_mem_info');
         halt(1);
      end;
   end;
  i:=0;
{$endif EXTRA}
  pp:=heap_mem_root;
  while pp<>nil do
   begin
     { inside this block ! }
     if (cardinal(p)>=cardinal(pp)+sizeof(theap_mem_info)+cardinal(extra_info_size)) and
        (cardinal(p)<=cardinal(pp)+sizeof(theap_mem_info)+cardinal(extra_info_size)+cardinal(pp^.size)) then
        { allocated block }
       if ((pp^.sig=$DEADBEEF) and not usecrc) or
          ((pp^.sig=calculate_sig(pp)) and usecrc) then
          goto _exit
       else
         begin
            writeln(ptext^,'pointer $',hexstr(longint(p),8),' points into invalid memory block');
            dump_error(pp,ptext^);
            runerror(204);
         end;
     pp:=pp^.previous;
     inc(i);
     if i>getmem_cnt then
      begin
         writeln(ptext^,'error in linked list of heap_mem_info');
         halt(1);
      end;
   end;
  writeln(ptext^,'pointer $',hexstr(longint(p),8),' does not point to valid memory block');
  runerror(204);
_exit:
end;

{*****************************************************************************
                              Dump Heap
*****************************************************************************}

procedure dumpheap;
var
  pp : pheap_mem_info;
  i : longint;
  ExpectedMemAvail : longint;
begin
  pp:=heap_mem_root;
  Writeln(ptext^,'Heap dump by heaptrc unit');
{$ifdef EXTRA}
  Writeln(ptext^,'compiled with EXTRA features');
{$endif EXTRA}
  Writeln(ptext^,getmem_cnt, ' memory blocks allocated : ',getmem_size,'/',getmem8_size);
  Writeln(ptext^,freemem_cnt,' memory blocks freed     : ',freemem_size,'/',freemem8_size);
  Writeln(ptext^,getmem_cnt-freemem_cnt,' unfreed memory blocks : ',getmem_size-freemem_size);
  Write(ptext^,'True heap size : ',system.HeapSize);
  if EntryMemUsed > 0 then
    Writeln(ptext^,' (',EntryMemUsed,' used in System startup)')
  else
    Writeln(ptext^);
  Writeln(ptext^,'True free heap : ',MemAvail);
  ExpectedMemAvail:=system.HeapSize-(getmem8_size-freemem8_size)-
    (getmem_cnt-freemem_cnt)*(sizeof(theap_mem_info)+extra_info_size)-EntryMemUsed;
  If ExpectedMemAvail<>MemAvail then
    Writeln(ptext^,'Should be : ',ExpectedMemAvail);
  i:=getmem_cnt-freemem_cnt;
  while pp<>nil do
   begin
     if i<0 then
       begin
          Writeln(ptext^,'Error in heap memory list');
          Writeln(ptext^,'More memory blocks than expected');
          exit;
       end;
     if ((pp^.sig=$DEADBEEF) and not usecrc) or
        ((pp^.sig=calculate_sig(pp)) and usecrc) then
       begin
          { this one was not released !! }
          if exitcode<>203 then
            call_stack(pp,ptext^);
          dec(i);
       end
     else if pp^.sig<>$AAAAAAAA then
       begin
          dump_error(pp,ptext^);
{$ifdef EXTRA}
          dump_error(pp,error_file);
{$endif EXTRA}
          error_in_heap:=true;
       end
{$ifdef EXTRA}
     else if pp^.release_sig<>calculate_release_sig(pp) then
       begin
          dump_change_after(pp,ptext^);
          dump_change_after(pp,error_file);
          error_in_heap:=true;
       end
{$endif EXTRA}
       ;
     pp:=pp^.previous;
   end;
end;


procedure markheap;
var
  pp : pheap_mem_info;
begin
  pp:=heap_mem_root;
  while pp<>nil do
   begin
     pp^.sig:=$AAAAAAAA;
     pp:=pp^.previous;
   end;
end;


{*****************************************************************************
                                AllocMem
*****************************************************************************}

function TraceAllocMem(size:longint):Pointer;
begin
  TraceAllocMem:=SysAllocMem(size);
end;


{*****************************************************************************
                            No specific tracing calls
*****************************************************************************}

function TraceMemAvail:longint;
begin
  TraceMemAvail:=SysMemAvail;
end;

function TraceMaxAvail:longint;
begin
  TraceMaxAvail:=SysMaxAvail;
end;

function TraceHeapSize:longint;
begin
  TraceHeapSize:=SysHeapSize;
end;


{*****************************************************************************
                           Install MemoryManager
*****************************************************************************}

const
  TraceManager:TMemoryManager=(
    Getmem  : @TraceGetMem;
    Freemem : @TraceFreeMem;
    FreememSize : @TraceFreeMemSize;
    AllocMem : @TraceAllocMem;
    ReAllocMem : @TraceReAllocMem;
    MemSize : @TraceMemSize;
    MemAvail : @TraceMemAvail;
    MaxAvail : @TraceMaxAvail;
    HeapSize : @TraceHeapsize;
  );

procedure TraceExit;
begin
  { no dump if error
    because this gives long long listings }
  { clear inoutres, in case the program that quit didn't }
  ioresult;
  if (exitcode<>0) and (erroraddr<>nil) then
    begin
       Writeln(ptext^,'No heap dump by heaptrc unit');
       Writeln(ptext^,'Exitcode = ',exitcode);
       if ptext<>@stderr then
         begin
            ptext:=@stderr;
            close(ownfile);
         end;
       exit;
    end;
  if not error_in_heap then
    Dumpheap;
  if error_in_heap and (exitcode=0) then
    exitcode:=203;
{$ifdef EXTRA}
  Close(error_file);
{$endif EXTRA}
   if ptext<>@stderr then
     begin
        ptext:=@stderr;
        close(ownfile);
     end;
end;

Procedure SetHeapTraceOutput(const name : string);
var i : longint;
begin
   if ptext<>@stderr then
     begin
        ptext:=@stderr;
        close(ownfile);
     end;
   assign(ownfile,name);
{$I-}
   append(ownfile);
   if IOResult<>0 then
     Rewrite(ownfile);
{$I+}
   ptext:=@ownfile;
   for i:=0 to Paramcount do
     write(ptext^,paramstr(i),' ');
   writeln(ptext^);
end;

procedure SetExtraInfo( size : longint;func : fillextrainfotype);

  begin
     if getmem_cnt>0 then
       begin
         writeln(ptext^,'Setting extra info is only possible at start !! ');
         dumpheap;
       end
     else
       begin
          { the total size must stay multiple of 8 !! }
          exact_info_size:=size;
          extra_info_size:=((size+7) div 8)*8;
          fill_extra_info:=func;
       end;
  end;

Initialization
  EntryMemUsed:=System.HeapSize-MemAvail;
  MakeCRC32Tbl;
  SetMemoryManager(TraceManager);
  ptext:=@stderr;
{$ifdef EXTRA}
  Assign(error_file,'heap.err');
  Rewrite(error_file);
{$endif EXTRA}
  { checkpointer init }
{$ifdef go32v2}
  Heap_at_init:=HeapPtr;
{$endif}
{$ifdef win32}
  StartupHeapEnd:=HeapEnd;
{$endif}
finalization
  TraceExit;
end.
{
  $Log$
  Revision 1.9  2002/09/13 08:11:46  lazarus
  MG: fixed memcheck output

  Revision 1.8  2002/09/13 07:01:20  lazarus
  MG: fixed memcheck

  Revision 1.1.2.10  2002/07/16 13:52:59  pierre
   * fix compilation for m68k linux

  Revision 1.1.2.9  2002/06/19 13:56:09  pierre
   * try to fix handling for unix

  Revision 1.1.2.8  2002/05/31 11:18:20  marco
   * Rename fest for 1.0.x step one. Compiler and RTL

  Revision 1.1.2.7  2001/09/22 04:48:42  carl
  - remove unused define

  Revision 1.1.2.6  2001/07/24 09:11:38  pierre
   * added goto on to avoid need of -Sg option

  Revision 1.1.2.5  2001/06/06 14:27:14  jonas
    * fixed wrong typed constant procvars in preparation of my fix which will
      disallow them in FPC mode

  Revision 1.1.2.4  2001/04/22 00:38:47  carl
  + make it portable

  Revision 1.1.2.3  2001/04/16 20:31:31  carl
  * 386DX bugfix with popal

  Revision 1.1.2.2  2000/12/15 13:02:30  jonas
    * added some typecasts so some expressiosn aren't evaluated anymore in
      64bit when rangechecking is on

  Revision 1.1.2.1  2000/08/24 08:59:35  jonas
    * clear inoutres in traceexit before writing anything (to avoid an RTE
      when writing the heaptrc output when a program didn't handle ioresult)

  Revision 1.1  2000/07/13 06:30:47  michael
  + Initial import

  Revision 1.43  2000/05/18 17:03:27  peter
    * fixed reallocmem with double removing from heap_mem_root list
    * fixed reallocmem getmem/freemem count, now both are increased and
      the _size8 counts are also increased

  Revision 1.42  2000/04/27 15:35:50  pierre
   * fix for bug report 929

  Revision 1.41  2000/02/10 13:59:35  peter
    * fixed bug with reallocmem to use the wrong size when copying the
      data to the new allocated pointer

  Revision 1.40  2000/02/09 16:59:30  peter
    * truncated log

  Revision 1.39  2000/02/07 10:42:44  peter
    * use backtracestrfunc()

  Revision 1.38  2000/02/02 11:13:15  peter
    * fixed tracereallocmem which supplied the wrong size for tryresize

  Revision 1.37  2000/01/31 23:41:30  peter
    * reallocmem fixed for freemem() call when size=0

  Revision 1.36  2000/01/20 14:25:51  jonas
    * finally fixed tracereallocmem completely

  Revision 1.35  2000/01/20 13:17:11  jonas
    * another problme with realloc fixed (one left)

  Revision 1.34  2000/01/20 12:35:35  jonas
    * fixed problem with reallocmem and heaptrc

  Revision 1.33  2000/01/07 16:41:34  daniel
    * copyright 2000

  Revision 1.32  2000/01/07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.31  2000/01/05 13:56:55  jonas
    * fixed traceReallocMem with nil pointer (simply calls traceGetMem now in
      such a case)

  Revision 1.30  2000/01/03 19:37:52  peter
    * fixed reallocmem with p=nil

  Revision 1.29  1999/11/14 21:35:04  peter
    * removed warnings

  Revision 1.28  1999/11/09 22:32:23  pierre
   * several extra_size_info fixes

  Revision 1.27  1999/11/06 14:35:38  peter
    * truncated log

  Revision 1.26  1999/11/01 13:56:50  peter
    * freemem,reallocmem now get var argument

  Revision 1.25  1999/10/30 17:39:05  peter
    * memorymanager expanded with allocmem/reallocmem

  Revision 1.24  1999/09/17 17:14:12  peter
    + new heap manager supporting delphi freemem(pointer)

  Revision 1.23  1999/09/10 17:13:41  peter
    * fixed missing var

  Revision 1.22  1999/09/08 16:14:41  peter
    * pointer fixes

  Revision 1.21  1999/08/18 12:03:16  peter
    * objfpc mode for 0.99.12

  Revision 1.20  1999/08/17 14:56:03  michael
  Removed the mode for objpas

}
