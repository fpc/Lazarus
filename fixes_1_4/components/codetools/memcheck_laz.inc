{%MainUnit memcheck.pas}

{$IFDEF MC_Interface}

{$R-}
{$S-}

var
  ExceptOnError: boolean = true;

procedure CheckHeap;
procedure CheckHeap(const txt: ansistring);
procedure CheckHeapWrtMemCnt(const txt: ansistring);
procedure WriteGetMemCount(const txt: ansistring);

function MemCheck_getmem_cnt: ptruint;
function MemCheck_freemem_cnt: ptruint;
function MemCheck_getmem_size: ptruint;
function MemCheck_freemem_size: ptruint;
function MemCheck_getmem8_size: ptruint;
function MemCheck_freemem8_size: ptruint;
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

function MemCheck_getmem_cnt: ptruint;
begin
  MemCheck_getmem_cnt:=heap_info.getmem_cnt;
end;

function MemCheck_freemem_cnt: ptruint;
begin
  MemCheck_freemem_cnt:=heap_info.freemem_cnt;
end;

function MemCheck_getmem_size: ptruint;
begin
  MemCheck_getmem_size:=heap_info.getmem_size;
end;

function MemCheck_freemem_size: ptruint;
begin
  MemCheck_freemem_size:=heap_info.freemem_size;
end;

function MemCheck_getmem8_size: ptruint;
begin
  MemCheck_getmem8_size:=heap_info.getmem8_size;
end;

function MemCheck_freemem8_size: ptruint;
begin
  MemCheck_freemem8_size:=heap_info.freemem8_size;
end;
// Addition for CodeTools

{$ENDIF MC_ImplementationEnd}

