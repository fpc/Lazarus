unit LazTracer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazLoggerBase, LazUtilities, LazUtilsStrConsts;

type
  TStackTracePointers = array of Pointer;
  TLineInfoCacheItem = record
    Addr: Pointer;
    Info: string;
  end;
  PLineInfoCacheItem = ^TLineInfoCacheItem;


procedure RaiseGDBException(const Msg: string);
procedure RaiseAndCatchException;
function GetStackTrace(UseCache: boolean): string;
procedure GetStackTracePointers(var AStack: TStackTracePointers);
function StackTraceAsString(const AStack: TStackTracePointers;
                            UseCache: boolean): string;
function GetLineInfo(Addr: Pointer; UseCache: boolean): string;


implementation

var
  LineInfoCache: TAvlTree = nil;

{------------------------------------------------------------------------------
  procedure RaiseGDBException(const Msg: string);

  Raises an exception.
  Normally gdb does not catch fpc Exception objects, therefore this procedure
  raises a standard "division by zero" exception which is catched by gdb.
  This allows one to stop a program, without extra gdb configuration.
 ------------------------------------------------------------------------------}
procedure RaiseGDBException(const Msg: string);
begin
  debugln(lrsERRORInLib, Msg);
  // creates an exception, that gdb catches:
  debugln(lrsCreatingGdbCatchableError);
  DumpStack;
  {$ifndef HASAMIGA} // On Amiga Division by 0 is not catchable, just crash
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
  {$endif}
end;

procedure RaiseAndCatchException;
begin
  try
    {$ifndef HASAMIGA} // On Amiga Division by 0 is not catchable, just crash
    if (length(lrsERRORInLib) div (length(lrsERRORInLib) div 10000))=0 then ;
    {$else}
    DumpStack;
    {$endif}
  except
  end;
end;

function GetStackTrace(UseCache: boolean): string;
var
  bp: Pointer;
  addr: Pointer;
  oldbp: Pointer;
  CurAddress: Shortstring;
begin
  Result:='';
  { retrieve backtrace info }
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    addr:=get_caller_addr(bp);
    CurAddress:=GetLineInfo(addr,UseCache);
    //DebugLn('GetStackTrace ',CurAddress);
    Result:=Result+CurAddress+LineEnding;
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
end;

procedure GetStackTracePointers(var AStack: TStackTracePointers);
var
  Depth: Integer;
  bp: Pointer;
  oldbp: Pointer;
begin
  // get stack depth
  Depth:=0;
  bp:=get_caller_frame(get_frame);
  while bp<>nil do begin
    inc(Depth);
    oldbp:=bp;
    bp:=get_caller_frame(bp);
    if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
      bp:=nil;
  end;
  SetLength(AStack,Depth);
  if Depth>0 then begin
    Depth:=0;
    bp:=get_caller_frame(get_frame);
    while bp<>nil do begin
      AStack[Depth]:=get_caller_addr(bp);
      inc(Depth);
      oldbp:=bp;
      bp:=get_caller_frame(bp);
      if (bp<=oldbp) or (bp>(StackBottom + StackLength)) then
        bp:=nil;
    end;
  end;
end;

function StackTraceAsString(const AStack: TStackTracePointers;
  UseCache: boolean): string;
var
  i: Integer;
  CurAddress: String;
begin
  Result:='';
  for i:=0 to length(AStack)-1 do begin
    CurAddress:=GetLineInfo(AStack[i],UseCache);
    Result:=Result+CurAddress+LineEnding;
  end;
end;

function CompareLineInfoCacheItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointers(PLineInfoCacheItem(Data1)^.Addr,
                          PLineInfoCacheItem(Data2)^.Addr);
end;

function CompareAddrWithLineInfoCacheItem(Addr, Item: Pointer): integer;
begin
  Result:=ComparePointers(Addr,PLineInfoCacheItem(Item)^.Addr);
end;

function GetLineInfo(Addr: Pointer; UseCache: boolean): string;
var
  ANode: TAvlTreeNode;
  Item: PLineInfoCacheItem;
begin
  if UseCache then begin
    if LineInfoCache=nil then
      LineInfoCache:=TAvlTree.Create(@CompareLineInfoCacheItems);
    ANode:=LineInfoCache.FindKey(Addr,@CompareAddrWithLineInfoCacheItem);
    if ANode=nil then begin
      Result:=BackTraceStrFunc(Addr);
      New(Item);
      Item^.Addr:=Addr;
      Item^.Info:=Result;
      LineInfoCache.Add(Item);
    end else begin
      Result:=PLineInfoCacheItem(ANode.Data)^.Info;
    end;
  end else
    Result:=BackTraceStrFunc(Addr);
end;

procedure FreeLineInfoCache;
var
  ANode: TAvlTreeNode;
  Item: PLineInfoCacheItem;
begin
  if LineInfoCache=nil then exit;
  ANode:=LineInfoCache.FindLowest;
  while ANode<>nil do begin
    Item:=PLineInfoCacheItem(ANode.Data);
    Dispose(Item);
    ANode:=LineInfoCache.FindSuccessor(ANode);
  end;
  LineInfoCache.Free;
  LineInfoCache:=nil;
end;


finalization
  FreeLineInfoCache;

end.

