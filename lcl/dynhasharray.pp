{
  Author: Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  Abstract:
    This unit defines TDynHashArray, which is very similar to a TList, since
    it also stores pointer/objects.
    It supports Add, Remove, Contains, First, Count and Clear.
    Because of the hashing nature the operations adding, removing and finding is
    done in constant time on average.
    
    Inner structure:
      There are three parts:
        1. The array itself (FItems). Every entry is a pointer to the first
           TDynHashArrayItem of a list with the same hash index. The first item
           of every same index list is the list beginning and its IsOverflow
           flag is set to false. All other items are overflow items.
           To get all items with the same hash index, do a FindHashItem. Then
           search through all "Next" items until Next is nil or its IsOverflow
           flag is set to false.
        2. The items beginning with FFirstItem is a 2-way-connected list of
           TDynHashArrayItem. This list contains all used items.
        3. To reduce GetMem/FreeMem calls, free items are cached.

  Issues:
    The maximum capacity is the PrimeNumber. You can store more items, but the
    performance decreases. The best idea is to provide your own hash function.
    
    Important: Items in the TDynHashArray must not change their key.
      When changing the key of an item, remove it and add it after the change.

}
unit DynHashArray;

{$Mode ObjFPC}{$H+}

interface

uses Classes, SysUtils;

type
  TDynHashArray = class;
  
  THashFunction = function(Sender: TDynHashArray; Item: Pointer): integer;
  TOwnerHashFunction = function(Item: Pointer): integer of object;
  TOnGetKeyForHashItem = function(Item: pointer): pointer;

  PDynHashArrayItem = ^TDynHashArrayItem;
  TDynHashArrayItem = record
    Item: Pointer;
    Next, Prior: PDynHashArrayItem;
    IsOverflow: boolean;
  end; 
  
  TDynHashArrayOption = (dhaoCachingEnabled);
  TDynHashArrayOptions = set of TDynHashArrayOption;

  TDynHashArray = class
  private
    FItems: ^PDynHashArrayItem;
    FCount: integer;
    FCapacity: integer;
    FMinCapacity: integer;
    FMaxCapacity: integer;
    FFirstItem: PDynHashArrayItem;
    FHashCacheItem: Pointer;
    FHashCacheIndex: integer;
    FLowWaterMark: integer;
    FHighWaterMark: integer;
    FCustomHashFunction: THashFunction;
    FOnGetKeyForHashItem: TOnGetKeyForHashItem;
    FOptions: TDynHashArrayOptions;
    FOwnerHashFunction: TOwnerHashFunction;
    function NewHashItem: PDynHashArrayItem;
    procedure DisposeHashItem(ADynHashArrayItem: PDynHashArrayItem);
    procedure ComputeWaterMarks;
    procedure SetCapacity(NewCapacity: integer);
    procedure SetCustomHashFunction(const AValue: THashFunction);
    procedure SetOnGetKeyForHashItem(const AValue: TOnGetKeyForHashItem);
    procedure SetOptions(const AValue: TDynHashArrayOptions);
    procedure SetOwnerHashFunction(const AValue: TOwnerHashFunction);
  protected
    procedure RebuildItems;
    procedure SaveCacheItem(Item: Pointer; Index: integer);
  public
    procedure Add(Item: Pointer);
    function Contains(Item: Pointer): boolean;
    function ContainsKey(Key: Pointer): boolean;
    procedure Remove(Item: Pointer);
    procedure Clear;
    procedure ClearCache;
    function First: Pointer;
    property Count: integer read fCount;
    function IndexOf(AnItem: Pointer): integer;
    function IndexOfKey(Key: Pointer): integer;
    function FindHashItem(Item: Pointer): PDynHashArrayItem;
    function FindHashItemWithKey(Key: Pointer): PDynHashArrayItem;
    function FindItemWithKey(Key: Pointer): Pointer;
    function GetHashItem(HashIndex: integer): PDynHashArrayItem;
    procedure Delete(ADynHashArrayItem: PDynHashArrayItem);
    procedure AssignTo(List: TList);
    
    property FirstHashItem: PDynHashArrayItem read FFirstItem;
    property MinCapacity: integer read FMinCapacity write FMinCapacity;
    property MaxCapacity: integer read FMaxCapacity write FMaxCapacity;
    property Capacity: integer read FCapacity;
    property CustomHashFunction: THashFunction
       read FCustomHashFunction write SetCustomHashFunction;
    property OwnerHashFunction: TOwnerHashFunction
       read FOwnerHashFunction write SetOwnerHashFunction;
    property OnGetKeyForHashItem: TOnGetKeyForHashItem
       read FOnGetKeyForHashItem write SetOnGetKeyForHashItem;
    property Options: TDynHashArrayOptions read FOptions write SetOptions;
    constructor Create(InitialMinCapacity: integer);
    destructor Destroy; override;
    function SlowAlternativeHashMethod(Sender: TDynHashArray;
       Item: Pointer): integer;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
  end;

  TDynHashArrayItemMemManager = class
  private
    FFirstFree: PDynHashArrayItem;
    FFreeCount: integer;
    FCount: integer;
    FMinFree: integer;
    FMaxFreeRatio: integer;
    procedure SetMaxFreeRatio(NewValue: integer);
    procedure SetMinFree(NewValue: integer);
    procedure DisposeFirstFreeItem;
  public
    procedure DisposeItem(ADynHashArrayItem: PDynHashArrayItem);
    function NewItem: PDynHashArrayItem;
    property MinimumFreeCount: integer read FMinFree write SetMinFree;
    property MaximumFreeRatio: integer
        read FMaxFreeRatio write SetMaxFreeRatio; // in one eighth steps
    property Count: integer read FCount;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
  end;
  
const
  ItemMemManager: TDynHashArrayItemMemManager = nil;

implementation

function GetItemMemManager: TDynHashArrayItemMemManager;
begin
  if ItemMemManager=nil then
    ItemMemManager:=TDynHashArrayItemMemManager.Create;
  Result:=ItemMemManager;
end;

const
  PrimeNumber: integer = 5364329;

{ TDynHashArray }

procedure TDynHashArray.WriteDebugReport;
var i, RealHashIndex: integer;
  HashItem: PDynHashArrayItem;
begin
  writeln('TDynHashArray.WriteDebugReport: Consistency=',ConsistencyCheck);
  writeln('  Count=',FCount,'  Capacity=',FCapacity);
  for i:=0 to FCapacity-1 do begin
    HashItem:=FItems[i];
    if HashItem<>nil then begin
      write('  Index=',i);
      while HashItem<>nil do begin
        write(' ',HexStr(Cardinal(HashItem^.Item),8));
        RealHashIndex:=IndexOf(HashItem^.Item);
        if RealHashIndex<>i then write('(H=',RealHashIndex,')');
        HashItem:=HashItem^.Next;
        if (HashItem<>nil) and (HashItem^.IsOverflow=false) then break;
      end;
      writeln;
    end;
  end;
  HashItem:=FFirstItem;
  while HashItem<>nil do begin
    writeln('  ',HexStr(Cardinal(HashItem^.Prior),8),'<-'
                ,HexStr(Cardinal(HashItem),8)
                ,'(',HexStr(Cardinal(HashItem^.Item),8),')'
                ,'->',HexStr(Cardinal(HashItem^.Next),8));
    HashItem:=HashItem^.Next;
  end;
end;

constructor TDynHashArray.Create(InitialMinCapacity: integer);
var Size: integer;
begin
  inherited Create;
  FMinCapacity:=InitialMinCapacity;
  FMaxCapacity:=PrimeNumber;
  if FMinCapacity<5 then FMinCapacity:=137;
  FCapacity:=FMinCapacity;
  Size:=FCapacity * SizeOf(TDynHashArrayItem);
  GetMem(FItems,Size);
  FillChar(FItems^,Size,0);
  FCount:=0;
  FFirstItem:=nil;
  ComputeWaterMarks;
  FHashCacheIndex:=-1;
end;

destructor TDynHashArray.Destroy;
begin
  Clear;
  FreeMem(FItems);
  inherited Destroy;
end;

function TDynHashArray.ConsistencyCheck: integer;
var RealCount, i: integer;
  HashItem, HashItem2: PDynHashArrayItem;
  OldCacheItem: pointer;
  OldCacheIndex: integer;
begin
  RealCount:=0;
  // check first item
  if (FFirstItem<>nil) and (FFirstItem^.IsOverflow) then
    exit(-1);
  if (FItems=nil) and (FFirstItem<>nil) then
    exit(-2);
  // check for doubles and circles
  HashItem:=FFirstItem;
  while HashItem<>nil do begin
    HashItem2:=HashItem^.Prior;
    while HashItem2<>nil do begin
      if HashItem=HashItem2 then
        exit(-3); // circle
      if HashItem^.Item=HashItem2^.Item then
        exit(-4); // double item
      HashItem2:=HashItem2^.Prior;
    end;
    HashItem:=HashItem^.Next;
  end;
  // check chain
  HashItem:=FFirstItem;
  while HashItem<>nil do begin
    inc(RealCount);
    if (HashItem^.Next<>nil) and (HashItem^.Next^.Prior<>HashItem) then
      exit(-6);
    if (HashItem^.Prior<>nil) and (HashItem^.Prior^.Next<>HashItem) then
      exit(-7);
    if (HashItem^.IsOverflow=false)
    and (FItems[IndexOf(HashItem^.Item)]<>HashItem) then
      exit(-8);
    HashItem:=HashItem^.Next;
  end;
  // check count
  if RealCount<>FCount then exit(-9);
  // check FItems
  RealCount:=0;
  for i:=0 to FCapacity-1 do begin
    HashItem:=FItems[i];
    while HashItem<>nil do begin
      inc(RealCount);
      if IndexOf(HashItem^.Item)<>i then exit(-14);
      HashItem:=HashItem^.Next;
      if (HashItem<>nil) and (HashItem^.IsOverflow=false) then break;
    end;
  end;  
  if RealCount<>FCount then exit(-15);
  // check cache
  if FHashCacheIndex>=0 then begin
    OldCacheItem:=FHashCacheItem;
    OldCacheIndex:=FHashCacheIndex;
    ClearCache;
    FHashCacheIndex:=IndexOfKey(OldCacheItem);
    if FHashCacheIndex<>OldCacheIndex then exit(-16);
    FHashCacheItem:=OldCacheItem;
  end;
  Result:=0;
end;

procedure TDynHashArray.ComputeWaterMarks;
begin
  FLowWaterMark:=FCapacity div 4;
  FHighWaterMark:=(FCapacity*3) div 4;
end;

function TDynHashArray.IndexOf(AnItem: Pointer): integer;
begin
  if (AnItem<>nil) and (FItems<>nil) then begin
    if Assigned(OnGetKeyForHashItem) then begin
      AnItem:=OnGetKeyForHashItem(AnItem);
    end;
    Result:=IndexOfKey(AnItem);
  end else
    Result:=-1;
end;

function TDynHashArray.IndexOfKey(Key: Pointer): integer;
begin
  if (FItems<>nil)
  and ((Key<>nil) or Assigned(OnGetKeyForHashItem)) then begin

    if (dhaoCachingEnabled in Options)
    and (Key=FHashCacheItem) and (FHashCacheIndex>=0) then
      exit(FHashCacheIndex);
    if not Assigned(FCustomHashFunction) then begin
      if not Assigned(FOwnerHashFunction) then begin
        Result:=integer((Cardinal(Key) mod Cardinal(PrimeNumber))
                +(Cardinal(Key) mod 17)
                 ) mod FCapacity;
      end else
        Result:=FOwnerHashFunction(Key);
    end else
      Result:=FCustomHashFunction(Self,Key);
    {if (Key=FHashCacheItem) and (FHashCacheIndex>=0)
    and (Result<>FHashCacheIndex) then begin
      writeln(' DAMN: ',HexStr(Cardinal(Key),8),' ',FHashCacheIndex,'<>',Result);
      raise Exception.Create('GROSSER MIST');
    end;}
  end else
    Result:=-1;
end;

procedure TDynHashArray.Clear;
begin
  ClearCache;
  while FFirstItem<>nil do Delete(FFirstItem);
end;

procedure TDynHashArray.ClearCache;
begin
  FHashCacheIndex:=-1;
end;

procedure TDynHashArray.Add(Item: Pointer);
var Index: integer;
  HashItem: PDynHashArrayItem;
begin
  if Item=nil then exit;
  if FCount>=FHighWaterMark then begin
    SetCapacity(FCapacity*2-1);
  end;
  Index:=IndexOf(Item);
  HashItem:=NewHashItem;
  HashItem^.Item:=Item;
  if FItems[Index]=nil then begin
    HashItem^.Next:=FFirstItem;
  end else begin
    HashItem^.Next:=FItems[Index];
    HashItem^.Prior:=HashItem^.Next^.Prior;
    HashItem^.Next^.IsOverflow:=true;
  end;
  if (HashItem^.Next=FFirstItem) then
    FFirstItem:=HashItem;
  FItems[Index]:=HashItem;
  if HashItem^.Next<>nil then begin
    HashItem^.Next^.Prior:=HashItem;
  if HashItem^.Prior<>nil then
    HashItem^.Prior^.Next:=HashItem;
  end;
  inc(FCount);
  SaveCacheItem(Item,Index);
end;

function TDynHashArray.SlowAlternativeHashMethod(Sender: TDynHashArray;
  Item: Pointer): integer;
begin
  Result:=integer((Cardinal(Item) mod Cardinal(PrimeNumber))
          +(Cardinal(Item) mod 17)+(Cardinal(Item) mod 173)
          +(Cardinal(Item) mod 521)
           ) mod FCapacity;
end;

procedure TDynHashArray.Remove(Item: Pointer);
begin
  Delete(FindHashItem(Item));
end;

procedure TDynHashArray.Delete(ADynHashArrayItem: PDynHashArrayItem);
var Index: integer;
  OldNext: PDynHashArrayItem;
begin
  if ADynHashArrayItem=nil then exit;
  // delete from cache
  if (FHashCacheIndex>=0)
  and ((ADynHashArrayItem^.Item=FHashCacheItem)
  or (Assigned(OnGetKeyForHashItem)
    and (OnGetKeyForHashItem(ADynHashArrayItem^.Item)=FHashCacheItem)))
  then
    // if the user removes an item, changes the key and readds it, the hash
    // of the item can change
    // => the cache must be cleared
    ClearCache;
  // delete from FItems
  if not ADynHashArrayItem^.IsOverflow then begin
    // Item is first item with hash
    Index:=IndexOf(ADynHashArrayItem^.Item);
    OldNext:=ADynHashArrayItem^.Next;
    if (OldNext=nil) or (not (OldNext^.IsOverflow)) then
      FItems[Index]:=nil
    else begin
      FItems[Index]:=OldNext;
      OldNext^.IsOverflow:=false;
    end;
  end;
  // adjust FFirstItem
  if FFirstItem=ADynHashArrayItem then
    FFirstItem:=FFirstItem^.Next;
  // free storage item
  DisposeHashItem(ADynHashArrayItem);
  // adjust count and capacity
  dec(FCount);
  if FCount<FLowWaterMark then begin
    // resize
    SetCapacity((FCapacity+1) div 2);
  end;
end;

procedure TDynHashArray.AssignTo(List: TList);
var i: integer;
  HashItem: PDynHashArrayItem;
begin
  List.Count:=Count;
  HashItem:=FirstHashItem;
  i:=0;
  while HashItem<>nil do begin
    List[i]:=HashItem^.Item;
    inc(i);
    HashItem:=HashItem^.Next;
  end;
end;

function TDynHashArray.First: Pointer;
begin
  if FFirstItem<>nil then
    Result:=FFirstItem^.Item
  else
    Result:=nil;
end;

function TDynHashArray.NewHashItem: PDynHashArrayItem;
begin
  Result:=GetItemMemManager.NewItem;
end;

procedure TDynHashArray.DisposeHashItem(ADynHashArrayItem: PDynHashArrayItem);
begin
  GetItemMemManager.DisposeItem(ADynHashArrayItem);
end;

function TDynHashArray.Contains(Item: Pointer): boolean;
begin
  Result:=FindHashItem(Item)<>nil;
end;

function TDynHashArray.ContainsKey(Key: Pointer): boolean;
begin
  Result:=FindHashItemWithKey(Key)<>nil;
end;

function TDynHashArray.FindHashItem(Item: Pointer): PDynHashArrayItem;
var Index: integer;
begin
  if (Item<>nil) and (FItems<>nil) then begin
    Index:=IndexOf(Item);
    Result:=FItems[Index];
    if (Result<>nil) then begin
      while (Result^.Item<>Item) do begin
        Result:=Result^.Next;
        if Result=nil then exit;
        if Result^.IsOverflow=false then begin
          Result:=nil;
          exit;
        end;
      end;
      SaveCacheItem(Item,Index);
    end;
  end else
    Result:=nil;
end;

function TDynHashArray.FindHashItemWithKey(Key: Pointer): PDynHashArrayItem;
var Index: integer;
begin
  if FItems<>nil then begin
    Index:=IndexOfKey(Key);
    Result:=FItems[Index];
    if (Result<>nil) then begin
      if Assigned(OnGetKeyForHashItem) then begin
        if OnGetKeyForHashItem(Result^.Item)=Key then exit;
        // search in overflow hash items
        Result:=Result^.Next;
        while (Result<>nil) and (Result^.IsOverflow) do begin
          if OnGetKeyForHashItem(Result^.Item)=Key then begin
            FHashCacheIndex:=Index;
            FHashCacheItem:=Key;
            exit;
          end;
          Result:=Result^.Next;
        end;
        Result:=nil;
      end;
    end;
  end else
    Result:=nil;
end;

function TDynHashArray.FindItemWithKey(Key: Pointer): Pointer;
var
  Index: integer;
  HashItem: PDynHashArrayItem;
begin
  Result:=nil;
  if FItems<>nil then begin
    Index:=IndexOfKey(Key);
    HashItem:=FItems[Index];
    if (HashItem<>nil)
    and Assigned(OnGetKeyForHashItem) then begin
      if OnGetKeyForHashItem(HashItem^.Item)=Key then exit;
      // search in overflow hash items
      HashItem:=HashItem^.Next;
      while (HashItem<>nil) and (HashItem^.IsOverflow) do begin
        if OnGetKeyForHashItem(HashItem^.Item)=Key then begin
          FHashCacheIndex:=Index;
          FHashCacheItem:=Key;
          Result:=HashItem^.Item;
          exit;
        end;
        HashItem:=HashItem^.Next;
      end;
    end;
  end;
end;

function TDynHashArray.GetHashItem(HashIndex: integer): PDynHashArrayItem;
begin
  Result:=FItems[HashIndex];
end;

procedure TDynHashArray.SetCapacity(NewCapacity: integer);
var Size: integer;
begin
  if NewCapacity<FMinCapacity then NewCapacity:=FMinCapacity;
  if NewCapacity>FMaxCapacity then NewCapacity:=FMaxCapacity;
  if NewCapacity=FCapacity then exit;
  // resize FItems
  FreeMem(FItems);
  FCapacity:=NewCapacity;
  Size:=FCapacity * SizeOf(PDynHashArrayItem);
  GetMem(FItems,Size);
  ComputeWaterMarks;
  // rebuild
  RebuildItems;
end;

procedure TDynHashArray.SetCustomHashFunction(const AValue: THashFunction);
begin
  if FCustomHashFunction=AValue then exit;
  FCustomHashFunction:=AValue;
  FOwnerHashFunction:=nil;
  RebuildItems;
end;

procedure TDynHashArray.SetOwnerHashFunction(const AValue: TOwnerHashFunction);
begin
  if FOwnerHashFunction=AValue then exit;
  FCustomHashFunction:=nil;
  FOwnerHashFunction:=AValue;
  RebuildItems;
end;

procedure TDynHashArray.RebuildItems;
var Index: integer;
  CurHashItem, NextHashItem: PDynHashArrayItem;
begin
  FillChar(FItems^,FCapacity * SizeOf(PDynHashArrayItem),0);
  ClearCache;
  CurHashItem:=FFirstItem;
  FFirstItem:=nil;
  while CurHashItem<>nil do begin
    NextHashItem:=CurHashItem^.Next;
    Index:=IndexOf(CurHashItem^.Item);
    CurHashItem^.IsOverFlow:=false;
    CurHashItem^.Prior:=nil;
    if FItems[Index]=nil then begin
      CurHashItem^.Next:=FFirstItem;
    end else begin
      CurHashItem^.Next:=FItems[Index];
      CurHashItem^.Prior:=CurHashItem^.Next^.Prior;
      CurHashItem^.Next^.IsOverflow:=true;
    end;
    if (CurHashItem^.Next=FFirstItem) then
      FFirstItem:=CurHashItem;
    FItems[Index]:=CurHashItem;
    if CurHashItem^.Next<>nil then begin
      CurHashItem^.Next^.Prior:=CurHashItem;
    if CurHashItem^.Prior<>nil then
      CurHashItem^.Prior^.Next:=CurHashItem;
    end;
    CurHashItem:=NextHashItem;
  end;
end;

procedure TDynHashArray.SaveCacheItem(Item: Pointer; Index: integer);
// Important:
//   !!! Only call this method for items, that exists in the list or for items
//       that can't change their key
begin
  if Assigned(OnGetKeyForHashItem) then Item:=OnGetKeyForHashItem(Item);
  FHashCacheItem:=Item;
  FHashCacheIndex:=Index;
end;

procedure TDynHashArray.SetOnGetKeyForHashItem(
  const AValue: TOnGetKeyForHashItem);
begin
  if FOnGetKeyForHashItem=AValue then exit;
  FOnGetKeyForHashItem:=AValue;
  RebuildItems;
end;

procedure TDynHashArray.SetOptions(const AValue: TDynHashArrayOptions);
begin
  FOptions:=AValue;
end;

{ TDynHashArrayItemMemManager }

procedure TDynHashArrayItemMemManager.SetMaxFreeRatio(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMaxFreeRatio then exit;
  FMaxFreeRatio:=NewValue;
end;

procedure TDynHashArrayItemMemManager.SetMinFree(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMinFree then exit;
  FMinFree:=NewValue;
end;

procedure TDynHashArrayItemMemManager.DisposeFirstFreeItem;
var OldItem: PDynHashArrayItem;
begin
  if FFirstFree=nil then exit;
  OldItem:=FFirstFree;
  FFirstFree:=OldItem^.Next;
  if FFirstFree<>nil then
    FFirstFree^.Prior:=nil;
  Dispose(OldItem);
  dec(FFreeCount);
end;

procedure TDynHashArrayItemMemManager.DisposeItem(
  ADynHashArrayItem: PDynHashArrayItem);
begin
  if ADynHashArrayItem=nil then exit;
  // unbind item
  if ADynHashArrayItem^.Next<>nil then
    ADynHashArrayItem^.Next^.Prior:=ADynHashArrayItem^.Prior;
  if ADynHashArrayItem^.Prior<>nil then
    ADynHashArrayItem^.Prior^.Next:=ADynHashArrayItem^.Next;
  // add to free list
  ADynHashArrayItem^.Next:=FFirstFree;
  FFirstFree:=ADynHashArrayItem;
  if ADynHashArrayItem^.Next<>nil then
    ADynHashArrayItem^.Next^.Prior:=ADynHashArrayItem;
  ADynHashArrayItem^.Prior:=nil;
  inc(FFreeCount);
  // reduce free list
  if (FFreeCount>(((8+FMaxFreeRatio)*FCount) shr 3)) and (FFreeCount>10) then
  begin
    DisposeFirstFreeItem;
    DisposeFirstFreeItem;
  end;
end;

function TDynHashArrayItemMemManager.NewItem: PDynHashArrayItem;
begin
  if FFirstFree<>nil then begin
    Result:=FFirstFree;
    FFirstFree:=FFirstFree^.Next;
    if FFirstFree<>nil then
      FFirstFree^.Prior:=nil;
    dec(FFreeCount);
  end else begin
    New(Result);
  end;
  with Result^ do begin
    Item:=nil;
    Next:=nil;
    Prior:=nil;
    IsOverflow:=false;
  end;
end;

procedure TDynHashArrayItemMemManager.Clear;
begin
  while FFreeCount>0 do DisposeFirstFreeItem;
end;

constructor TDynHashArrayItemMemManager.Create;
begin
  inherited Create;
  FFirstFree:=nil;
  FFreeCount:=0;
  FCount:=0;
  FMinFree:=100;
  FMaxFreeRatio:=8; // 1:1
end;

destructor TDynHashArrayItemMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDynHashArrayItemMemManager.ConsistencyCheck: integer;
var RealFreeCount: integer;
  HashItem: PDynHashArrayItem;
begin
  RealFreeCount:=0;
  HashItem:=FFirstFree;
  while HashItem<>nil do begin
    inc(RealFreeCount);
    if (HashItem^.Next<>nil) and (HashItem^.Next^.Prior<>HashItem) then
      exit(-1);
    if (HashItem^.Prior<>nil) and (HashItem^.Prior^.Next<>HashItem) then
      exit(-2);
    HashItem:=HashItem^.Next;
  end;
  if RealFreeCount<>FFreeCount then exit(-3);
  Result:=0;
end;

procedure TDynHashArrayItemMemManager.WriteDebugReport;
begin
  writeln('TDynHashArrayItemMemManager.WriteDebugReport:'
    ,' Consistency=',ConsistencyCheck,', FreeCount=',FFreeCount);
end;

//==============================================================================

finalization
  FreeAndNil(ItemMemManager);

end.

