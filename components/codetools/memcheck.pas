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

interface

{ 1.0.x doesn't have good rangechecking for cardinals }
{$ifdef VER1_0}

{$goto on}

Procedure DumpHeap;
Procedure MarkHeap;

{ define EXTRA to add more
  tests :
   - keep all memory after release and
   check by CRC value if not changed after release
   WARNING this needs extremely much memory (PM) }

type
    FillExtraInfoType = procedure(p : pointer);
    ExtraInfoStringType = function(p : pointer) : string;
    { allows to add several longint value that can help
      to debug :
      see for instance ppheap.pas unit of the compiler source PM }

Procedure SetExtraInfo( size : longint;func : FillExtraInfoType);
Procedure SetExtraInfoString(func : ExtraInfoStringType);
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
  no_trace_if_halt : boolean = false;

// StartAddition for CodeTools
procedure CheckHeap;
procedure CheckHeap(const txt: ansistring);
procedure CheckHeapWrtMemCnt(const txt: ansistring);
procedure WriteGetMemCount(const txt: ansistring);

function MemCheck_getmem_cnt: longint;
function MemCheck_freemem_cnt: longint;
function MemCheck_getmem_size: longint;
function MemCheck_freemem_size: longint;
function MemCheck_getmem8_size: longint;
function MemCheck_freemem8_size: longint;
// End Addition for CodeTools

implementation

{$ifdef win32}
  uses
    windows;
{$endif}

type
   plongint = ^longint;

const
  { allows to add custom info in heap_mem_info }
  extra_info_size : longint = 0;
  exact_info_size : longint = 0;
  EntryMemUsed    : longint = 0;
  { function to fill this info up }
  fill_extra_info : FillExtraInfoType = nil;
  extra_info_string_func : ExtraInfoStringType = nil;
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
{$endif EXTRA}
{$ifdef EXTRA_CHAIN}
    prev_valid  : pheap_mem_info;
{$endif EXTRA_CHAIN}
    calls    : array [1..tracesize] of longint;
    extra_info : record
                 end;
  end;

var
  ptext : ^text;
  ownfile : text;
{$ifdef EXTRA}
  error_file : text;
{$endif EXTRA}
{$ifdef EXTRA_CHAIN}
  heap_valid_first,
  heap_valid_last : pheap_mem_info;
{$endif EXTRA_CHAIN}
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

const
  LastWrittenGetMemCnt: longint = 0;
  HiddenGetMemCnt: longint = 0;

procedure CheckHeapWrtMemCnt(const txt: ansistring);
var
  p: pointer;
  StartGetMemCnt, CurGetMemCount, DiffGetMemCount: longint;
begin
  StartGetMemCnt:=MemCheck_getmem_cnt;
  CurGetMemCount:=StartGetMemCnt-HiddenGetMemCnt;
  DiffGetMemCount:=CurGetMemCount-LastWrittenGetMemCnt;
  LastWrittenGetMemCnt:=CurGetMemCount;

  writeln('>>> memcheck.pp - CheckHeap2 "',txt,'" ',
    CurGetMemCount,'(',StartGetMemCnt,') +',DiffGetMemCount);
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=true;

  // don't count mem counts of this proc
  inc(HiddenGetMemCnt,MemCheck_getmem_cnt-StartGetMemCnt);
end;

procedure WriteGetMemCount(const txt: ansistring);
var
  StartGetMemCnt, CurGetMemCount, DiffGetMemCount: longint;
begin
  StartGetMemCnt:=MemCheck_getmem_cnt;
  CurGetMemCount:=StartGetMemCnt-HiddenGetMemCnt;
  DiffGetMemCount:=CurGetMemCount-LastWrittenGetMemCnt;
  LastWrittenGetMemCnt:=CurGetMemCount;

  writeln('>>> memcheck.pp - WriteGetMemCount "',txt,'" ',
    CurGetMemCount,'(',StartGetMemCnt,') +',DiffGetMemCount);

  // don't count mem counts of this proc
  inc(HiddenGetMemCnt,MemCheck_getmem_cnt-StartGetMemCnt);
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
  writeln(ptext,'Call trace for block 0x',
    hexstr(longint(pointer(pp)+sizeof(theap_mem_info)
    +extra_info_size),8),' size ',pp^.size);
  for i:=1 to tracesize do
   if pp^.calls[i]<>0 then
     writeln(ptext,BackTraceStrFunc(pp^.calls[i]));
  if assigned(extra_info_string_func) then
    writeln(ptext,extra_info_string_func(@pp^.extra_info))
  else for i:=0 to (exact_info_size div 4)-1 do
    writeln(ptext,'info ',i,'=',plongint(pointer(@pp^.extra_info)+4*i)^);
end;

procedure call_free_stack(pp : pheap_mem_info;var ptext : text);
var
  i  : longint;

begin
  writeln(ptext,'Call trace for block at 0x',
    hexstr(longint(pointer(pp)+sizeof(theap_mem_info))
    +extra_info_size,8),' size ',pp^.size);
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
  Writeln(ptext,'Marked memory at 0x',
    HexStr(longint(pointer(p)+sizeof(theap_mem_info))
    +extra_info_size,8),' released');
  call_free_stack(p,ptext);
  Writeln(ptext,'freed again at');
  dump_stack(ptext,get_caller_frame(get_frame));
end;

procedure dump_error(p : pheap_mem_info;var ptext : text);
begin
  Writeln(ptext,'Marked memory at 0x',
    HexStr(longint(pointer(p)+sizeof(theap_mem_info))
    +extra_info_size,8),' invalid');
  Writeln(ptext,'Wrong signature $',hexstr(p^.sig,8)
    ,' instead of ',hexstr(calculate_sig(p),8));
  dump_stack(ptext,get_caller_frame(get_frame));
end;

{$ifdef EXTRA}
procedure dump_change_after(p : pheap_mem_info;var ptext : text);
 var pp : pchar;
     i : longint;
begin
  Writeln(ptext,'Marked memory at 0x',
    HexStr(longint(pointer(p)+sizeof(theap_mem_info))
    +extra_info_size,8),' invalid');
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
     // MG: changes for codetools:
     inc(i);
     if i>getmem_cnt-freemem_cnt then begin
       writeln(ptext^,'error in linked list of heap_mem_info',
         ' FreedCnt=',getmem_cnt-freemem_cnt,' RealCnt=',i);
       RunError(204);
     end;
     pp:=pp^.previous;
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
{$ifdef EXTRA_CHAIN}
  pheap_mem_info(p)^.prev_valid:=heap_valid_last;
  heap_valid_last:=pheap_mem_info(p);
  if not assigned(heap_valid_first) then
    heap_valid_first:=pheap_mem_info(p);
{$endif EXTRA_CHAIN}
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
{$ifdef EXTRA_CHAIN}
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
{$endif EXTRA_CHAIN}
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
     // MG: changes for codetools: P:=nil
     P:=nil;
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



{*****************************************************************************
                              Check pointer
*****************************************************************************}

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
   { I found no symbol for start of text section :(
     so we usee the _mainCRTStartup which should be
     in wprt0.ow or wdllprt0.ow PM }
   win32_text_begin : cardinal;external name '_mainCRTStartup';
   win32_data_end : cardinal;external name '__data_end__';
const
  global_stack_top : cardinal = 0;
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
{$ifdef win32}
  MemInfo :  MEMORY_BASIC_INFORMATION;
{$endif win32}
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

{$ifdef win32}
  if (p>=@win32_text_begin) and (p<=@win32_data_end) then
    exit;
  { check if in stack }
  asm
    movl %ebp, get_ebp
  end;
  if (cardinal(p)>=get_ebp) then
    begin
      if (global_stack_top = 0) then
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
     if (cardinal(p)<=global_stack_top) then
       exit;
    end;
{$endif win32}
  { I don't know where the stack is in other OS !! }

{ heapptr is not always the highest allocated pointer
 if p>=heapptr then
    runerror(216); }
  { first try valid list faster }

{$ifdef EXTRA_CHAIN}
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
{$ifdef win32}
  VirtualQuery(p,MemInfo,Sizeof(MemInfo));
  MemInfo.Protect:=MemInfo.Protect and not (PAGE_GUARD or PAGE_NOCACHE);
  if (MemInfo.State<>MEM_COMMIT) or
     ((MemInfo.Protect <> PAGE_READONLY) and
      (MemInfo.Protect <> PAGE_READWRITE) and
      (MemInfo.Protect <> PAGE_WRITECOPY) and
      (MemInfo.Protect <> PAGE_EXECUTE) and
      (MemInfo.Protect <> PAGE_EXECUTE_READ) and
      (MemInfo.Protect <> PAGE_EXECUTE_READWRITE) and
      (MemInfo.Protect <> PAGE_EXECUTE_WRITECOPY)) then
    begin
      writeln(ptext^,'pointer $',hexstr(longint(p),8),' does not point to valid memory block');
      runerror(204);
    end
  else
    exit;
{$else not win32}
  writeln(ptext^,'pointer $',hexstr(longint(p),8),' does not point to valid memory block');

  runerror(204);
{$endif not win32}
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
          if (exitcode<>203) and
             ((exitcode=0) or not no_trace_if_halt) then
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
  if (exitcode<>0) and
     (erroraddr<>nil) then
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
     if (size<>exact_info_size) and (getmem_cnt>0) then
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

Procedure SetExtraInfoString(func : ExtraInfoStringType);
  begin
    extra_info_string_func:=func;
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
finalization
  TraceExit;
end.

{$endif}

{$ifdef VER1_1}

{$goto on}

{off $DEFINE EXTRA}

Procedure DumpHeap;
Procedure MarkHeap;

{ define EXTRA to add more
  tests :
   - keep all memory after release and
   check by CRC value if not changed after release
   WARNING this needs extremely much memory (PM) }

type
   tFillExtraInfoProc = procedure(p : pointer);
   tdisplayextrainfoProc = procedure (var ptext : text;p : pointer);

{ Allows to add info pre memory block, see ppheap.pas of the compiler
  for example source }
procedure SetHeapExtraInfo( size : longint;fillproc : tfillextrainfoproc;displayproc : tdisplayextrainfoproc);

{ Redirection of the output to a file }
procedure SetHeapTraceOutput(const name : string);

const
  { tracing level
    splitted in two if memory is released !! }
{$ifdef EXTRA}
  tracesize = 16;
{$else EXTRA}
  tracesize = 8;
{$endif EXTRA}
  { install heaptrc memorymanager }
  useheaptrace : boolean=true;
  { less checking }
  quicktrace : boolean=true;
  { calls halt() on error by default !! }
  HaltOnError : boolean = true;
  { set this to true if you suspect that memory
    is freed several times }
{$ifdef EXTRA}
  keepreleased : boolean=true;
{$else EXTRA}
  keepreleased : boolean=false;
{$endif EXTRA}
  { add a small footprint at the end of memory blocks, this
    can check for memory overwrites at the end of a block }
  add_tail : boolean = true;
  { put crc in sig
    this allows to test for writing into that part }
  usecrc : boolean = true;

// StartAddition for CodeTools
procedure CheckHeap;
procedure CheckHeap(const txt: ansistring);
procedure CheckHeapWrtMemCnt(const txt: ansistring);
procedure WriteGetMemCount(const txt: ansistring);

function MemCheck_getmem_cnt: longint;
function MemCheck_freemem_cnt: longint;
function MemCheck_getmem_size: longint;
function MemCheck_freemem_size: longint;
function MemCheck_getmem8_size: longint;
function MemCheck_freemem8_size: longint;
// End Addition for CodeTools

implementation

type
   plongint = ^longint;

const
  { allows to add custom info in heap_mem_info, this is the size that will
    be allocated for this information }
  extra_info_size : longint = 0;
  exact_info_size : longint = 0;
  EntryMemUsed    : longint = 0;
  { function to fill this info up }
  fill_extra_info_proc : TFillExtraInfoProc = nil;
  display_extra_info_proc : TDisplayExtraInfoProc = nil;
  error_in_heap : boolean = false;
  inside_trace_getmem : boolean = false;
  { indicates where the output will be redirected }
  { only set using environment variables          }
  outputstr : shortstring = '';

type
  pheap_extra_info = ^theap_extra_info;
  theap_extra_info = record
    check       : cardinal;  { used to check if the procvar is still valid }
    fillproc    : tfillextrainfoProc;
    displayproc : tdisplayextrainfoProc;
    data : record
           end;
  end;

  { warning the size of theap_mem_info
    must be a multiple of 8
    because otherwise you will get
    problems when releasing the usual memory part !!
    sizeof(theap_mem_info = 16+tracesize*4 so
    tracesize must be even !! PM }
  pheap_mem_info = ^theap_mem_info;
  theap_mem_info = record
    previous,
    next     : pheap_mem_info;
    size     : longint;
    sig      : longword;
{$ifdef EXTRA}
    release_sig : longword;
    prev_valid  : pheap_mem_info;
{$endif EXTRA}
    calls    : array [1..tracesize] of longint;
    exact_info_size : word;
    extra_info_size : word;
    extra_info      : pheap_extra_info;
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

const
  LastWrittenGetMemCnt: longint = 0;
  HiddenGetMemCnt: longint = 0;

procedure CheckHeapWrtMemCnt(const txt: ansistring);
var
  p: pointer;
  StartGetMemCnt, CurGetMemCount, DiffGetMemCount: longint;
begin
  StartGetMemCnt:=MemCheck_getmem_cnt;
  CurGetMemCount:=StartGetMemCnt-HiddenGetMemCnt;
  DiffGetMemCount:=CurGetMemCount-LastWrittenGetMemCnt;
  LastWrittenGetMemCnt:=CurGetMemCount;

  writeln('>>> memcheck.pp - CheckHeap2 "',txt,'" ',
    CurGetMemCount,'(',StartGetMemCnt,') +',DiffGetMemCount);
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=true;

  // don't count mem counts of this proc
  inc(HiddenGetMemCnt,MemCheck_getmem_cnt-StartGetMemCnt);
end;

procedure WriteGetMemCount(const txt: ansistring);
var
  StartGetMemCnt, CurGetMemCount, DiffGetMemCount: longint;
begin
  StartGetMemCnt:=MemCheck_getmem_cnt;
  CurGetMemCount:=StartGetMemCnt-HiddenGetMemCnt;
  DiffGetMemCount:=CurGetMemCount-LastWrittenGetMemCnt;
  LastWrittenGetMemCnt:=CurGetMemCount;

  writeln('>>> memcheck.pp - WriteGetMemCount "',txt,'" ',
    CurGetMemCount,'(',StartGetMemCnt,') +',DiffGetMemCount);

  // don't count mem counts of this proc
  inc(HiddenGetMemCnt,MemCheck_getmem_cnt-StartGetMemCnt);
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
  Crc32Tbl : array[0..255] of longword;

procedure MakeCRC32Tbl;
var
  crc : longword;
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


Function UpdateCrc32(InitCrc:longword;var InBuf;InLen:Longint):longword;
var
  i : longint;
  p : pchar;
begin
  p:=@InBuf;
  for i:=1 to InLen do
   begin
     InitCrc:=Crc32Tbl[byte(InitCrc) xor byte(p^)] xor (InitCrc shr 8);
     inc(p);
   end;
  UpdateCrc32:=InitCrc;
end;

Function calculate_sig(p : pheap_mem_info) : longword;
var
   crc : longword;
   pl : plongint;
begin
   crc:=cardinal($ffffffff);
   crc:=UpdateCrc32(crc,p^.size,sizeof(longint));
   crc:=UpdateCrc32(crc,p^.calls,tracesize*sizeof(longint));
   if p^.extra_info_size>0 then
     crc:=UpdateCrc32(crc,p^.extra_info^,p^.exact_info_size);
   if add_tail then
     begin
        { Check also 4 bytes just after allocation !! }
        pl:=pointer(p)+p^.extra_info_size+sizeof(theap_mem_info)+p^.size;
        crc:=UpdateCrc32(crc,pl^,sizeof(longint));
     end;
   calculate_sig:=crc;
end;

{$ifdef EXTRA}
Function calculate_release_sig(p : pheap_mem_info) : longword;
var
   crc : longword;
   pl : plongint;
begin
   crc:=$ffffffff;
   crc:=UpdateCrc32(crc,p^.size,sizeof(longint));
   crc:=UpdateCrc32(crc,p^.calls,tracesize*sizeof(longint));
   if p^.extra_info_size>0 then
     crc:=UpdateCrc32(crc,p^.extra_info^,p^.exact_info_size);
   { Check the whole of the whole allocation }
   pl:=pointer(p)+p^.extra_info_size+sizeof(theap_mem_info);
   crc:=UpdateCrc32(crc,pl^,p^.size);
   { Check also 4 bytes just after allocation !! }
   if add_tail then
     begin
        { Check also 4 bytes just after allocation !! }
        pl:=pointer(p)+p^.extra_info_size+sizeof(theap_mem_info)+p^.size;
        crc:=UpdateCrc32(crc,pl^,sizeof(longint));
     end;
   calculate_release_sig:=crc;
end;
{$endif EXTRA}


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
  { the check is done to be sure that the procvar is not overwritten }
  if assigned(pp^.extra_info) and
     (pp^.extra_info^.check=$12345678) and
     assigned(pp^.extra_info^.displayproc) then
   pp^.extra_info^.displayproc(ptext,@pp^.extra_info^.data);
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
  { the check is done to be sure that the procvar is not overwritten }
  if assigned(pp^.extra_info) and
     (pp^.extra_info^.check=$12345678) and
     assigned(pp^.extra_info^.displayproc) then
   pp^.extra_info^.displayproc(ptext,@pp^.extra_info^.data);
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
  Writeln(ptext,'Wrong signature $',hexstr(p^.sig,8),' instead of ',hexstr(calculate_sig(p),8));
  dump_stack(ptext,get_caller_frame(get_frame));
end;

{$ifdef EXTRA}
procedure dump_change_after(p : pheap_mem_info;var ptext : text);
 var pp : pchar;
     i : longint;
begin
  Writeln(ptext,'Marked memory at 0x',HexStr(longint(pointer(p)+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong release CRC $',hexstr(p^.release_sig,8),' instead of ',hexstr(calculate_release_sig(p),8));
  Writeln(ptext,'This memory was changed after call to freemem !');
  call_free_stack(p,ptext);
  pp:=pointer(p)+sizeof(theap_mem_info);
  for i:=0 to p^.size-1 do
    if byte(pp[i])<>$F0 then
      Writeln(ptext,'offset',i,':$',hexstr(i,8),'"',pp[i],'"');
end;
{$endif EXTRA}

procedure dump_wrong_size(p : pheap_mem_info;size : longint;var ptext : text);
begin
  Writeln(ptext,'Marked memory at 0x',HexStr(longint(pointer(p)+sizeof(theap_mem_info)),8),' invalid');
  Writeln(ptext,'Wrong size : ',p^.size,' allocated ',size,' freed');
  dump_stack(ptext,get_caller_frame(get_frame));
  { the check is done to be sure that the procvar is not overwritten }
  if assigned(p^.extra_info) and
     (p^.extra_info^.check=$12345678) and
     assigned(p^.extra_info^.displayproc) then
   p^.extra_info^.displayproc(ptext,@p^.extra_info^.data);
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
        (pp^.sig <>$AAAAAAAA) then
      begin
        writeln(ptext^,'error in linked list of heap_mem_info');
        RunError(204);
      end;
     if pp=p then
      is_in_getmem_list:=true;
     // MG: changes for codetools:
     inc(i);
     if i>getmem_cnt-freemem_cnt then begin
       writeln(ptext^,'error in linked list of heap_mem_info',
         ' FreedCnt=',getmem_cnt-freemem_cnt,' RealCnt=',i);
       RunError(204);
     end;
     pp:=pp^.previous;
   end;
end;


{*****************************************************************************
                               TraceGetMem
*****************************************************************************}

Function TraceGetMem(size:longint):pointer;
var
  allocsize,i,bp : longint;
  pl : pdword;
  p : pointer;
  pp : pheap_mem_info;
begin
  inc(getmem_size,size);
  inc(getmem8_size,((size+7) div 8)*8);
{ Do the real GetMem, but alloc also for the info block }
  allocsize:=size+sizeof(theap_mem_info)+extra_info_size;
  if add_tail then
    inc(allocsize,sizeof(longint));
  p:=SysGetMem(allocsize);
  pp:=pheap_mem_info(p);
  inc(p,sizeof(theap_mem_info));
{ Create the info block }
  pp^.sig:=$DEADBEEF;
  pp^.size:=size;
  pp^.extra_info_size:=extra_info_size;
  pp^.exact_info_size:=exact_info_size;
  {
    the end of the block contains:
    <tail>   4 bytes
    <extra_info>   X bytes
  }
  if extra_info_size>0 then
   begin
     pp^.extra_info:=pointer(pp)+allocsize-extra_info_size;
     fillchar(pp^.extra_info^,extra_info_size,0);
     pp^.extra_info^.check:=$12345678;
     pp^.extra_info^.fillproc:=fill_extra_info_proc;
     pp^.extra_info^.displayproc:=display_extra_info_proc;
     if assigned(fill_extra_info_proc) then
      begin
        inside_trace_getmem:=true;
        fill_extra_info_proc(@pp^.extra_info^.data);
        inside_trace_getmem:=false;
      end;
   end
  else
   pp^.extra_info:=nil;
  if add_tail then
    begin
      pl:=pointer(pp)+allocsize-pp^.extra_info_size-sizeof(longint);
      pl^:=$DEADBEEF;
    end;
  { clear the memory }
  fillchar(p^,size,#255);
  { retrieve backtrace info }
  bp:=get_caller_frame(get_frame);
  for i:=1 to tracesize do
   begin
     pp^.calls[i]:=get_caller_addr(bp);
     bp:=get_caller_frame(bp);
   end;
  { insert in the linked list }
  if heap_mem_root<>nil then
   heap_mem_root^.next:=pp;
  pp^.previous:=heap_mem_root;
  pp^.next:=nil;
{$ifdef EXTRA}
  pp^.prev_valid:=heap_valid_last;
  heap_valid_last:=pp;
  if not assigned(heap_valid_first) then
    heap_valid_first:=pp;
{$endif EXTRA}
  heap_mem_root:=pp;
  { must be changed before fill_extra_info is called
    because checkpointer can be called from within
    fill_extra_info PM }
  inc(getmem_cnt);
  { update the signature }
  if usecrc then
    pp^.sig:=calculate_sig(pp);
  TraceGetmem:=p;
end;


{*****************************************************************************
                                TraceFreeMem
*****************************************************************************}

function TraceFreeMemSize(p:pointer;size:longint):longint;
var
  i,bp, ppsize : longint;
  pp : pheap_mem_info;
{$ifdef EXTRA}
  pp2 : pheap_mem_info;
{$endif}
  extra_size : longint;
begin
  inc(freemem_size,size);
  inc(freemem8_size,((size+7) div 8)*8);
  pp:=pheap_mem_info(p-sizeof(theap_mem_info));
  ppsize:= size + sizeof(theap_mem_info)+pp^.extra_info_size;
  if add_tail then
    inc(ppsize,sizeof(longint));
  if not quicktrace then
    begin
      if not(is_in_getmem_list(pp)) then
       RunError(204);
    end;
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
  { save old values }
  extra_size:=pp^.extra_info_size;
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
  { clear the memory }
  fillchar(p^,size,#240); { $F0 will lead to GFP if used as pointer ! }
  { this way we keep all info about all released memory !! }
  if keepreleased then
    begin
{$ifdef EXTRA}
       { We want to check if the memory was changed after release !! }
       pp^.release_sig:=calculate_release_sig(pp);
       if pp=heap_valid_last then
         begin
            heap_valid_last:=pp^.prev_valid;
            if pp=heap_valid_first then
              heap_valid_first:=nil;
            TraceFreememsize:=size;
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
                 TraceFreememsize:=size;
                 exit;
              end
            else
              pp2:=pp2^.prev_valid;
         end;
{$endif EXTRA}
       TraceFreememsize:=size;
       exit;
    end;
   { release the normal memory at least }
   i:=SysFreeMemSize(pp,ppsize);
   { return the correct size }
   dec(i,sizeof(theap_mem_info)+extra_size);
   if add_tail then
     dec(i,sizeof(longint));
   TraceFreeMemSize:=i;
end;


function TraceMemSize(p:pointer):Longint;
var
  l : longint;
  pp : pheap_mem_info;
begin
  pp:=pheap_mem_info(p-sizeof(theap_mem_info));
  l:=SysMemSize(pp);
  dec(l,sizeof(theap_mem_info)+pp^.extra_info_size);
  if add_tail then
   dec(l,sizeof(longint));
  TraceMemSize:=l;
end;


function TraceFreeMem(p:pointer):longint;
var
  size : longint;
  pp : pheap_mem_info;
begin
  pp:=pheap_mem_info(p-sizeof(theap_mem_info));
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
  allocsize,
  i,bp : longint;
  pl : pdword;
  pp : pheap_mem_info;
  oldextrasize,
  oldexactsize : longint;
  old_fill_extra_info_proc : tfillextrainfoproc;
  old_display_extra_info_proc : tdisplayextrainfoproc;
begin
{ Free block? }
  if size=0 then
   begin
     if p<>nil then
      TraceFreeMem(p);
     // MG: changes for codetools: P:=nil
     P:=nil;
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
  pp:=pheap_mem_info(p-sizeof(theap_mem_info));
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
  { save info }
  oldextrasize:=pp^.extra_info_size;
  oldexactsize:=pp^.exact_info_size;
  if pp^.extra_info_size>0 then
   begin
     old_fill_extra_info_proc:=pp^.extra_info^.fillproc;
     old_display_extra_info_proc:=pp^.extra_info^.displayproc;
   end;
  { Do the real ReAllocMem, but alloc also for the info block }
  allocsize:=size+sizeof(theap_mem_info)+pp^.extra_info_size;
  if add_tail then
   inc(allocsize,sizeof(longint));
  { Try to resize the block, if not possible we need to do a
    getmem, move data, freemem }
  if not SysTryResizeMem(pp,allocsize) then
   begin
     { get a new block }
     oldsize:=TraceMemSize(p);
     newP := TraceGetMem(size);
     { move the data }
     if newP <> nil then
       move(p^,newP^,oldsize);
     { release p }
     traceFreeMem(p);
     { return the new pointer }
     p:=newp;
     traceReAllocMem := newp;
     exit;
   end;
{ adjust like a freemem and then a getmem, so you get correct
  results in the summary display }
  inc(freemem_size,pp^.size);
  inc(freemem8_size,((pp^.size+7) div 8)*8);
  inc(getmem_size,size);
  inc(getmem8_size,((size+7) div 8)*8);
{ Recreate the info block }
  pp^.sig:=$DEADBEEF;
  pp^.size:=size;
  pp^.extra_info_size:=oldextrasize;
  pp^.exact_info_size:=oldexactsize;
  { add the new extra_info and tail }
  if pp^.extra_info_size>0 then
   begin
     pp^.extra_info:=pointer(pp)+allocsize-pp^.extra_info_size;
     fillchar(pp^.extra_info^,extra_info_size,0);
     pp^.extra_info^.check:=$12345678;
     pp^.extra_info^.fillproc:=old_fill_extra_info_proc;
     pp^.extra_info^.displayproc:=old_display_extra_info_proc;
     if assigned(pp^.extra_info^.fillproc) then
      pp^.extra_info^.fillproc(@pp^.extra_info^.data);
   end
  else
   pp^.extra_info:=nil;
  if add_tail then
    begin
      pl:=pointer(pp)+allocsize-pp^.extra_info_size-sizeof(longint);
      pl^:=$DEADBEEF;
    end;
  { generate new backtrace }
  bp:=get_caller_frame(get_frame);
  for i:=1 to tracesize do
   begin
     pp^.calls[i]:=get_caller_addr(bp);
     bp:=get_caller_frame(bp);
   end;
  { regenerate signature }
  if usecrc then
    pp^.sig:=calculate_sig(pp);
  { return the pointer }
  p:=pointer(pp)+sizeof(theap_mem_info);
  TraceReAllocmem:=p;
end;



{*****************************************************************************
                              Check pointer
*****************************************************************************}

{$ifndef Unix}
  {$S-}
{$endif}

{$ifdef go32v2}
var
   __stklen : longword;external name '__stklen';
   __stkbottom : longword;external name '__stkbottom';
   edata : longword; external name 'edata';
   heap_at_init : pointer;
{$endif go32v2}

{$ifdef win32}
var
   StartUpHeapEnd : pointer;
   { I found no symbol for start of text section :(
     so we usee the _mainCRTStartup which should be
     in wprt0.ow or wdllprt0.ow PM }
   text_begin : longword;external name '_mainCRTStartup';
   data_end : longword;external name '__data_end__';
{$endif}

procedure CheckPointer(p : pointer);[saveregisters,public, alias : 'FPC_CHECKPOINTER'];
var
  i  : longint;
  pp : pheap_mem_info;
  //get_ebp,stack_top : longword;
  //data_end : longword;
label
  _exit;
begin
  if p=nil then
    runerror(204);

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
                             Program Hooks
*****************************************************************************}

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

procedure SetHeapExtraInfo( size : longint;fillproc : tfillextrainfoproc;displayproc : tdisplayextrainfoproc);
begin
  { the total size must stay multiple of 8, also allocate 2 pointers for
    the fill and display procvars }
  exact_info_size:=size + sizeof(theap_extra_info);
  extra_info_size:=((exact_info_size+7) div 8)*8;
  fill_extra_info_proc:=fillproc;
  display_extra_info_proc:=displayproc;
end;


{*****************************************************************************
                           Install MemoryManager
*****************************************************************************}

const
  TraceManager:TMemoryManager=(
    NeedLock : true;
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


procedure TraceInit;
begin
  EntryMemUsed:=System.HeapSize-MemAvail;
  MakeCRC32Tbl;
  SetMemoryManager(TraceManager);
  ptext:=@stderr;
  if outputstr <> '' then
     SetHeapTraceOutput(outputstr);
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
end;


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

{$ifdef win32}
   function GetEnvironmentStrings : pchar;
     external 'kernel32' name 'GetEnvironmentStringsA';
   function FreeEnvironmentStrings(p : pchar) : longbool;
     external 'kernel32' name 'FreeEnvironmentStringsA';
Function  GetEnv(envvar: string): string;
var
   s : string;
   i : longint;
   hp,p : pchar;
begin
   getenv:='';
   p:=GetEnvironmentStrings;
   hp:=p;
   while hp^<>#0 do
     begin
        s:=strpas(hp);
        i:=pos('=',s);
        if upcase(copy(s,1,i-1))=upcase(envvar) then
          begin
             getenv:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+strlen(hp)+1;
     end;
   FreeEnvironmentStrings(p);
end;
{$else}
Function GetEnv(P:string):Pchar;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}
var
  ep    : ppchar;
  i     : longint;
  found : boolean;
Begin
  p:=p+'=';            {Else HOST will also find HOSTNAME, etc}
  ep:=envp;
  found:=false;
  if ep<>nil then
   begin
     while (not found) and (ep^<>nil) do
      begin
        found:=true;
        for i:=1 to length(p) do
         if p[i]<>ep^[i-1] then
          begin
            found:=false;
            break;
          end;
        if not found then
         inc(ep);
      end;
   end;
  if found then
   getenv:=ep^+length(p)
  else
   getenv:=nil;
end;
{$endif}

procedure LoadEnvironment;
var
  i,j : longint;
  s: string;
begin
  s:=Getenv('HEAPTRC');
  if pos('keepreleased',s)>0 then
   keepreleased:=true;
  if pos('disabled',s)>0 then
   useheaptrace:=false;
  if pos('nohalt',s)>0 then
   haltonerror:=false;
  i:=pos('log=',s);
  if i>0 then
   begin
     outputstr:=copy(s,i+4,255);
     j:=pos(' ',outputstr);
     if j=0 then
      j:=length(outputstr)+1;
     delete(outputstr,j,255);
   end;
end;


Initialization
  LoadEnvironment;
  { heaptrc can be disabled from the environment }
  if useheaptrace then
   TraceInit;
finalization
  if useheaptrace then
   TraceExit;
end.

{$endif}
{
  $Log$
  Revision 1.17  2002/12/24 12:52:53  mattias
  fixed ReAllocmem of memcheck and added memcheck for fpc 1.1

}
