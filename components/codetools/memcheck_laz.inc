{%MainUnit memcheck.pas}

{$IFDEF MC_Interface}
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
{$ENDIF}

{$IFDEF MC_ImplementationStart}
// override RunError and Halt for better debugging
procedure RunError(RunErrorNumber: word); forward;
procedure Halt(ErrNum: byte); forward;
{$ENDIF MC_ImplementationStart}

{$IFDEF MC_ImplementationEnd}
// override RunError, so we can handle them according to the flags
procedure RunError(RunErrorNumber: word);
begin
  if ExceptOnError then begin
    // create an gdb catchable exception
    if 0=(1 div ((ord(ExceptOnError) and 1) shr 1)) then ;
  end;
  if HaltOnError then System.Halt(1);
  System.RunError(RunErrorNumber);
end;

// override RunError, so we can handle them according to the flags
procedure Halt(ErrNum: byte);
begin
  if ExceptOnError then begin
    // create an gdb catchable exception
    if 0=(1 div ((ord(ExceptOnError) and 1) shr 1)) then ;
  end;
  System.Halt(1);
end;

// StartAddition for CodeTools
procedure CheckHeap;
var
  p: pointer;
  OldQuickTrace: boolean;
begin
  writeln('>>> memcheck.pp - CheckHeap');
  OldQuickTrace:=QuickTrace;
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=OldQuickTrace;
end;

procedure CheckHeap(const txt: ansistring);
var
  p: pointer;
  OldQuickTrace: boolean;
begin
  writeln('>>> memcheck.pp - CheckHeap "',txt,'"');
  OldQuickTrace:=QuickTrace;
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=OldQuickTrace;
end;

const
  LastWrittenGetMemCnt: longint = 0;
  HiddenGetMemCnt: longint = 0;

procedure CheckHeapWrtMemCnt(const txt: ansistring);
var
  p: pointer;
  StartGetMemCnt, CurGetMemCount, DiffGetMemCount: longint;
  OldQuickTrace: boolean;
begin
  StartGetMemCnt:=MemCheck_getmem_cnt;
  CurGetMemCount:=StartGetMemCnt-HiddenGetMemCnt;
  DiffGetMemCount:=CurGetMemCount-LastWrittenGetMemCnt;
  LastWrittenGetMemCnt:=CurGetMemCount;

  writeln('>>> memcheck.pp - CheckHeap2 "',txt,'" ',
    CurGetMemCount,'(',StartGetMemCnt,') +',DiffGetMemCount);
  OldQuickTrace:=QuickTrace;
  QuickTrace:=false;
  GetMem(p,4);
  FreeMem(p);
  QuickTrace:=OldQuickTrace;

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

{$ENDIF MC_ImplementationEnd}

