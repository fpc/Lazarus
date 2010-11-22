{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Defines TLCLMemManager, which is the base class for various
    memory managers in the lcl and its interfaces.
    An own memory manager is somewhat faster and makes debugging and
    profiling easier.
}
unit LCLMemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math;

type
  PLCLMemManagerItem = ^TLCLMemManagerItem;
  TLCLMemManagerItem = record
    Next: PLCLMemManagerItem;
  end;

  { memory manager template }
  
  TLCLMemManager = class
  private
    procedure SetMaxFreeRatio(NewValue: integer);
    procedure SetMinFree(NewValue: integer);
  protected
    FFirstFree: PLCLMemManagerItem;
    FFreeCount: integer;
    FCount: integer;
    FMinFree: integer;
    FMaxFreeRatio: integer;
    FAllocatedCount: int64;
    FFreedCount: int64;
    procedure DisposeItem(AnItem: PLCLMemManagerItem);
    function NewItem: PLCLMemManagerItem;
    procedure FreeFirstItem; virtual;
  public
    property MinimumFreeCount: integer read FMinFree write SetMinFree;
    property MaximumFreeCountRatio: integer
        read FMaxFreeRatio write SetMaxFreeRatio; // in one eighth steps
    property Count: integer read FCount;
    property FreeCount: integer read FFreeCount;
    property AllocatedCount: int64 read FAllocatedCount;
    property FreedCount: int64 read FFreedCount;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;
  
  
  { TLCLNonFreeMemManager - a memory manager for records without freeing }
  
  TLCLEnumItemsMethod = procedure(Item: Pointer) of object;

  TLCLNonFreeMemManager = class
  private
    FItemSize: PtrInt;
    FItems: TFPList;
    FCurItem: Pointer;
    FEndItem: Pointer;
    FCurSize: PtrInt;
    FFirstSize: PtrInt;
    FMaxItemsPerChunk: PtrInt;
  public
    ClearOnCreate: boolean;
    property ItemSize: PtrInt read FItemSize;
    property MaxItemsPerChunk: PtrInt read FMaxItemsPerChunk write FMaxItemsPerChunk;
    procedure Clear;
    constructor Create(TheItemSize: integer);
    destructor Destroy; override;
    function NewItem: Pointer;
    procedure EnumerateItems(const Method: TLCLEnumItemsMethod);
  end;

{$IF FPC_FULLVERSION>20402}
  TStreamSizeType = PtrInt;
{$ELSE}
  TStreamSizeType = Longint;
{$IFEND}

  { TExtMemoryStream }
  
  TExtMemoryStream = class(TMemoryStream)
  protected
    function Realloc(var NewCapacity: TStreamSizeType): Pointer; override;
  public
    property Capacity;
  end;
  

implementation

{$IFOpt R+}{$Define RangeChecksOn}{$Endif}

{ TLCLMemManager }

procedure TLCLMemManager.Clear;
begin
  while FFirstFree<>nil do begin
    FreeFirstItem;
    inc(FFreedCount);
  end;
  FFreeCount:=0;
end;

constructor TLCLMemManager.Create;
begin
  inherited Create;
  FFirstFree:=nil;
  FFreeCount:=0;
  FCount:=0;
  FAllocatedCount:=0;
  FFreedCount:=0;
  FMinFree:=100000;
  FMaxFreeRatio:=8; // 1:1
end;

destructor TLCLMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLCLMemManager.DisposeItem(AnItem: PLCLMemManagerItem);
begin
  if AnItem<>nil then begin
    if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
    begin
      // add ANode to Free list
      //AddItemToFreeList(AnItem);
      inc(FFreeCount);
    end else begin
      // free list full -> free the ANode
      //FreeItem(AnItem);
      {$R-}
      inc(FFreedCount);
      {$IfDef RangeChecksOn}{$R+}{$Endif}
    end;
    dec(FCount);
  end;
end;

function TLCLMemManager.NewItem: PLCLMemManagerItem;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=FFirstFree;
    FFirstFree:=FFirstFree^.Next;
    Result^.Next:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    New(Result);
    {$R-}
    inc(FAllocatedCount);
    {$IfDef RangeChecksOn}{$R+}{$Endif}
  end;
  inc(FCount);
end;

procedure TLCLMemManager.SetMaxFreeRatio(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMaxFreeRatio then exit;
  FMaxFreeRatio:=NewValue;
end;

procedure TLCLMemManager.SetMinFree(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMinFree then exit;
  FMinFree:=NewValue;
end;

procedure TLCLMemManager.FreeFirstItem;
var Item: PLCLMemManagerItem;
begin
  Item:=FFirstFree;
  FFirstFree:=FFirstFree^.Next;
  Dispose(Item);
end;

{ TLCLNonFreeMemManager }

procedure TLCLNonFreeMemManager.Clear;
var
  i: Integer;
  p: Pointer;
begin
  if FItems<>nil then begin
    for i:=0 to FItems.Count-1 do begin
      p:=FItems[i];
      FreeMem(p);
    end;
    FItems.Free;
    FItems:=nil;
  end;
  FCurItem:=nil;
  FEndItem:=nil;
  FCurSize:=FItemSize*4; // 4 items
end;

constructor TLCLNonFreeMemManager.Create(TheItemSize: integer);
begin
  FItemSize:=TheItemSize;
  FFirstSize:=FItemSize*4; // 4 items => the first item has 8 entries
  FCurSize:=FFirstSize;
end;

destructor TLCLNonFreeMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TLCLNonFreeMemManager.NewItem: Pointer;
begin
  if (FCurItem=FEndItem) then begin
    // each item has double the size of its predecessor
    inc(FCurSize,FCurSize);
    if (FMaxItemsPerChunk>0) and (FCurSize>FMaxItemsPerChunk*FItemSize) then
      FCurSize:=FMaxItemsPerChunk*FItemSize;
    GetMem(FCurItem,FCurSize);
    if ClearOnCreate then
      FillChar(FCurItem^,FCurSize,0);
    if FItems=nil then FItems:=TFPList.Create;
    FItems.Add(FCurItem);
    FEndItem := FCurItem;
    Inc(FEndItem, FCurSize);
  end;
  Result:=FCurItem;
  Inc(FCurItem, FItemSize);
end;

procedure TLCLNonFreeMemManager.EnumerateItems(
  const Method: TLCLEnumItemsMethod);
var
  Cnt: Integer;
  i: Integer;
  p: Pointer;
  Size: Integer;
  Last: Pointer;
begin
  if FItems<>nil then begin
    Cnt:=FItems.Count;
    Size:=FFirstSize;
    for i:=0 to Cnt-1 do begin
      // each item has double the size of its predecessor
      inc(Size,Size);
      p:=FItems[i];
      Last := p;
      Inc(Last, Size);
      if i=Cnt-1 then
        Last:=FEndItem;
      while p<>Last do begin
        Method(p);
        Inc(p, FItemSize);
      end;
    end;
  end;
end;

{ TExtMemoryStream }

function TExtMemoryStream.Realloc(var NewCapacity: TStreamSizeType): Pointer;
begin
  // if we are growing, grow at least a quarter
  if (NewCapacity > Capacity) then
    NewCapacity := Max(NewCapacity, Capacity + Capacity div 4);
  Result := inherited Realloc(NewCapacity);
end;

end.

