{
  Author: Mattias Gaertner

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
        3. The free items beginning with FFirstFreeItems is a
           2-way-connected list of TDynHashArrayItem. This list constains the
           freed items. They are disposed when this list grows bigger than
           the Capacity. They are stored to reduce the usage of New and Dispose.

  Issues:
    The maximum capacity is the PrimeNumber. You can store more items, but the
    performance decreases. The best idea is to provide your own hash function.

}
unit DynHashArray;

{$Mode ObjFPC}{$H+}

interface

uses Classes;

type
  THashFunction = function(Item: Pointer): integer of object;

  PDynHashArrayItem = ^TDynHashArrayItem;
  TDynHashArrayItem = record
    Item: Pointer;
    Next, Prior: PDynHashArrayItem;
    IsOverflow: boolean;
  end; 

  TDynHashArray = class
  private
    FItems: ^PDynHashArrayItem;
    FCount: integer;
    FCapacity: integer;
    FMinCapacity: integer;
    FMaxCapacity: integer;
    FFreeCount: integer;
    FFirstItem: PDynHashArrayItem;
    FFirstFreeItem: PDynHashArrayItem;
    FLowWaterMark: integer;
    FHighWaterMark: integer;
    FOwnerHashFunction: THashFunction;
    function IndexOf(AnItem: Pointer): integer;
    function NewHashItem: PDynHashArrayItem;
    procedure DisposeHashItem(ADynHashArrayItem: PDynHashArrayItem);
    procedure DisposeFirstFreeItem;
    procedure ComputeWaterMarks;
    procedure SetCapacity(NewCapacity: integer);
  public
    procedure Add(Item: Pointer);
    function Contains(Item: Pointer): boolean;
    procedure Remove(Item: Pointer);
    procedure Clear;
    function First: Pointer;
    property Count: integer read fCount;
    function FindHashItem(Item: Pointer): PDynHashArrayItem;
    property FirstHashItem: PDynHashArrayItem read FFirstItem;
    procedure Delete(ADynHashArrayItem: PDynHashArrayItem);
    property MinCapacity: integer read FMinCapacity write FMinCapacity;
    property MaxCapacity: integer read FMaxCapacity write FMaxCapacity;
    property OwnerHashFunction: THashFunction
       read FOwnerHashFunction write FOwnerHashFunction;
    constructor Create(InitialMinCapacity: integer);
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
  end;


implementation

const
  PrimeNumber: integer = 5364329;

{ TDynHashArray }

procedure TDynHashArray.WriteDebugReport;
var i: integer;
  HashItem: PDynHashArrayItem;
begin
  writeln('Report: Consistency=',ConsistencyCheck);
  writeln('  Count=',FCount,'  FreeCount=',FFreeCount,'  Capacity=',FCapacity);
  for i:=0 to FCapacity-1 do begin
    HashItem:=FItems[i];
    if HashItem<>nil then begin
      write('  Index=',i);
      while HashItem<>nil do begin
        write(' ',HexStr(Cardinal(HashItem^.Item),8));
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
  FFreeCount:=0;
  FFirstItem:=nil;
  FFirstFreeItem:=nil;
  ComputeWaterMarks;
end;

destructor TDynHashArray.Destroy;
begin
  Clear;
  while FFirstFreeItem<>nil do DisposeFirstFreeItem;
  FreeMem(FItems);
  inherited Destroy;
end;

function TDynHashArray.ConsistencyCheck: integer;
var RealCount, RealFreeCount, i: integer;
  HashItem: PDynHashArrayItem;
begin
  RealCount:=0;
  HashItem:=FFirstItem;
  while HashItem<>nil do begin
    inc(RealCount);
    if (HashItem^.Next<>nil) and (HashItem^.Next^.Prior<>HashItem) then
      exit(-1);
    if (HashItem^.Prior<>nil) and (HashItem^.Prior^.Next<>HashItem) then
      exit(-2);
    if (HashItem^.IsOverflow=false) 
    and (FItems[IndexOf(HashItem^.Item)]<>HashItem) then
      exit (-3);
    HashItem:=HashItem^.Next;
  end;
  if RealCount<>FCount then exit(-4);
  RealFreeCount:=0;
  HashItem:=FFirstFreeItem;
  while HashItem<>nil do begin
    inc(RealFreeCount);
    if (HashItem^.Next<>nil) and (HashItem^.Next^.Prior<>HashItem) then
      exit(-5);
    if (HashItem^.Prior<>nil) and (HashItem^.Prior^.Next<>HashItem) then
      exit(-6);
    HashItem:=HashItem^.Next;
  end;
  if RealFreeCount<>FFreeCount then exit(-7);
  if FCount+FFreeCount>FCapacity then exit(-8);
  for i:=0 to FCapacity-1 do begin
    HashItem:=FItems[i];
    while HashItem<>nil do begin
      if IndexOf(HashItem^.Item)<>i then exit(-9);
      HashItem:=HashItem^.Next;
      if (HashItem<>nil) and (HashItem^.IsOverflow=false) then break;
    end;
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
  if AnItem=nil then exit(-1);
  if Assigned(FOwnerHashFunction) then
    Result:=FOwnerHashFunction(AnItem)
  else
    Result:=integer((Cardinal(AnItem) mod Cardinal(PrimeNumber))
            +(Cardinal(AnItem) mod 17)
             ) mod FCapacity;
end;

procedure TDynHashArray.Clear;
begin
  while FFirstItem<>nil do Delete(FFirstItem);
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
end;

procedure TDynHashArray.Remove(Item: Pointer);
var Index: integer;
  OldNext, Old: PDynHashArrayItem;
begin
  if Item=nil then exit;
  Index:=IndexOf(Item);
  if (Index<0) or (FItems[Index]=nil) then exit;
  Old:=FItems[Index];
  if Old^.Item=Item then begin
    OldNext:=Old^.Next;
    if (OldNext=nil) or (OldNext^.IsOverflow) then
      FItems[Index]:=OldNext
    else
      FItems[Index]:=nil;
  end else begin
    repeat
      Old:=Old^.Next;
      if Old=nil then exit;
      if Old^.IsOverflow=false then exit;
    until (Old^.Item=Item);
  end;
  Delete(Old);
end;

procedure TDynHashArray.Delete(ADynHashArrayItem: PDynHashArrayItem);
begin
  if ADynHashArrayItem=nil then exit;
  if (ADynHashArrayItem^.IsOverflow=false) and (ADynHashArrayItem^.Next<>nil) then
    ADynHashArrayItem^.Next^.IsOverflow:=false;
  if FFirstItem=ADynHashArrayItem then
    FFirstItem:=FFirstItem^.Next;
  DisposeHashItem(ADynHashArrayItem);
  dec(FCount);
  if FCount<FLowWaterMark then begin
    // resize
    SetCapacity((FCapacity+1) div 2);
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
  if FFirstFreeItem<>nil then begin
    Result:=FFirstFreeItem;
    FFirstFreeItem:=FFirstFreeItem^.Next;
    if FFirstFreeItem<>nil then
      FFirstFreeItem^.Prior:=nil;
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

procedure TDynHashArray.DisposeHashItem(ADynHashArrayItem: PDynHashArrayItem);
begin
  if ADynHashArrayItem=nil then exit;
  if ADynHashArrayItem^.Next<>nil then
    ADynHashArrayItem^.Next^.Prior:=ADynHashArrayItem^.Prior;
  if ADynHashArrayItem^.Prior<>nil then
    ADynHashArrayItem^.Prior^.Next:=ADynHashArrayItem^.Next;
  ADynHashArrayItem^.Next:=FFirstFreeItem;
  FFirstFreeItem:=ADynHashArrayItem;
  if ADynHashArrayItem^.Next<>nil then
    ADynHashArrayItem^.Next^.Prior:=ADynHashArrayItem;
  ADynHashArrayItem^.Prior:=nil;
  inc(FFreeCount);
  if (FFreeCount>2*FCount) and (FFreeCount>10) then begin
    DisposeFirstFreeItem;
    DisposeFirstFreeItem;
  end;
end;

procedure TDynHashArray.DisposeFirstFreeItem;
var OldItem: PDynHashArrayItem;
begin
  if FFirstFreeItem=nil then exit;
  OldItem:=FFirstFreeItem;
  FFirstFreeItem:=OldItem^.Next;
  if FFirstFreeItem<>nil then
    FFirstFreeItem^.Prior:=nil;
  Dispose(OldItem);
  dec(FFreeCount);
end;

function TDynHashArray.Contains(Item: Pointer): boolean;
begin
  Result:=FindHashItem(Item)<>nil;
end;

function TDynHashArray.FindHashItem(Item: Pointer): PDynHashArrayItem;
var Index: integer;
begin
  if Item=nil then exit(nil);
  Index:=IndexOf(Item);
  Result:=FItems[Index];
  if (Result=nil) or (Result^.Item=Item) then exit;
  repeat
    Result:=Result^.Next;
    if Result=nil then break;
    if Result^.IsOverflow=false then Result:=nil;
  until (Result=nil) or (Result^.Item=Item);
end;

procedure TDynHashArray.SetCapacity(NewCapacity: integer);
var Size, Index: integer;
  CurHashItem, NextHashItem: PDynHashArrayItem;
begin
  if NewCapacity<FMinCapacity then NewCapacity:=FMinCapacity;
  if NewCapacity>FMaxCapacity then NewCapacity:=FMaxCapacity;
  if NewCapacity=FCapacity then exit;
  // resize FItems
  FreeMem(FItems);
  FCapacity:=NewCapacity;
  Size:=FCapacity * SizeOf(TDynHashArrayItem);
  GetMem(FItems,Size);
  FillChar(FItems^,Size,0);
  ComputeWaterMarks;
  // rebuild hash table (FItems)
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

end.
