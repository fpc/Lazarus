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
    Defines the simple double connected queue TLazQueue.
    A Queue stores a set of pointers and supports Adding, Deleting, getting
    First and getting Last in O(1).
    Finding can be done in time O(n).
}
unit LazQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PLazQueueItem = ^TLazQueueItem;
  TLazQueueItem = record
    Next, Prior: PLazQueueItem;
    Data: Pointer;
  end;

  TLazQueue = class
  private
    FFirstFree: PLazQueueItem;
    FFreeCount: integer;
    FFirst: PLazQueueItem;
    FLast: PLazQueueItem;
    FCount: integer;
    function GetNewItem: PLazQueueItem;
    procedure DisposeItem(AnItem: PLazQueueItem);
    procedure Unbind(AnItem: PLazQueueItem);
    procedure AddAsLast(AnItem: PLazQueueItem);
  public
    property First: PLazQueueItem read FFirst;
    property Last: PLazQueueItem read FLast;
    function FirstData: Pointer;
    function LastData: Pointer;
    property Count: integer read FCount;
    procedure AddLast(Data: Pointer);
    procedure Delete(AnItem: PLazQueueItem);
    procedure MoveToLast(AnItem: PLazQueueItem);
    function Find(Data: Pointer): PLazQueueItem;
    procedure Clear;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport;
    constructor Create;
    destructor Destroy;  override;
  end;

implementation

{ TLazQueue }

constructor TLazQueue.Create;
begin
  inherited Create;
end;

destructor TLazQueue.Destroy;
var AnItem: PLazQueueItem;
begin
  Clear;
  // clear the free list
  while FFirstFree<>nil do begin
    AnItem:=FFirstFree;
    FFirstFree:=AnItem^.Next;
    Dispose(AnItem);
  end;
  inherited Destroy;
end;

function TLazQueue.FirstData: Pointer;
begin
  if FFirst<>nil then
    Result:=FFirst^.Data
  else
    Result:=nil;
end;

function TLazQueue.LastData: Pointer;
begin
  if FLast<>nil then
    Result:=FLast^.Data
  else
    Result:=nil;
end;

procedure TLazQueue.AddLast(Data: Pointer);
var NewItem: PLazQueueItem;
begin
  NewItem:=GetNewItem;
  NewItem^.Data:=Data;
  AddAsLast(NewItem);
end;

procedure TLazQueue.Delete(AnItem: PLazQueueItem);
begin
  if AnItem=nil then exit;
  Unbind(AnItem);
  DisposeItem(AnItem);
end;

procedure TLazQueue.MoveToLast(AnItem: PLazQueueItem);
begin
  if AnItem=nil then exit;
  Unbind(AnItem);
  AddAsLast(AnItem);
end;

procedure TLazQueue.Clear;
begin
  while First<>nil do Delete(First);
end;

function TLazQueue.GetNewItem: PLazQueueItem;
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
  Result^.Next:=nil;
  Result^.Prior:=nil;
  Result^.Data:=nil;
end;

procedure TLazQueue.DisposeItem(AnItem: PLazQueueItem);
var i: integer;
begin
  if FFreeCount<=2*FCount then begin
    AnItem^.Next:=FFirstFree;
    AnItem^.Prior:=nil;
    AnItem^.Data:=nil;
    FFirstFree:=AnItem;
    if AnItem^.Next<>nil then AnItem^.Next^.Prior:=AnItem;
    inc(FFreeCount);
  end else begin
    Dispose(AnItem);
    if (FCount+5)<2*FFreeCount then begin
      for i:=1 to 2 do begin
        if FFirstFree<>nil then begin
          AnItem:=FFirstFree;
          FFirstFree:=FFirstFree^.Next;
          if FFirstFree<>nil then
            FFirstFree^.Prior:=nil;
          Dispose(AnItem);
          dec(FFreeCount);
        end;
      end;
    end;
  end;
end;

procedure TLazQueue.Unbind(AnItem: PLazQueueItem);
begin
  if AnItem=nil then exit;
  if FFirst=AnItem then FFirst:=FFirst^.Next;
  if FLast=AnItem then FLast:=FLast^.Prior;
  if AnItem^.Prior<>nil then AnItem^.Prior^.Next:=AnItem^.Next;
  if AnItem^.Next<>nil then AnItem^.Next^.Prior:=AnItem^.Prior;
  AnItem^.Prior:=nil;
  AnItem^.Next:=nil;
  dec(FCount);
end;

procedure TLazQueue.AddAsLast(AnItem: PLazQueueItem);
begin
  AnItem^.Prior:=FLast;
  AnItem^.Next:=nil;
  FLast:=AnItem;
  if AnItem^.Prior<>nil then
    AnItem^.Prior^.Next:=AnItem
  else
    FFirst:=AnItem;
  inc(FCount);
end;

function TLazQueue.Find(Data: Pointer): PLazQueueItem;
begin
  Result:=FFirst;
  while (Result<>nil) do
    if Result^.Data=Data then exit;
end;

function TLazQueue.ConsistencyCheck: integer;
var RealCount: integer;
  AnItem: PLazQueueItem;
begin
  // test free list
  RealCount:=0;
  AnItem:=FFirstFree;
  while AnItem<>nil do begin
    inc(RealCount);
    AnItem:=AnItem^.Next;
  end;
  if FFreeCount<>RealCount then begin
    Result:=-1;  exit;
  end;
  // test items
  RealCount:=0;
  AnItem:=FFirst;
  while AnItem<>nil do begin
    if (AnItem^.Next<>nil) and (AnItem^.Next^.Prior<>AnItem) then begin
      Result:=-2;  exit;
    end;
    if (AnItem^.Prior<>nil) and (AnItem^.Prior^.Next<>AnItem) then begin
      Result:=-3;  exit;
    end;
    inc(RealCount);
    AnItem:=AnItem^.Next;
  end;
  if FCount<>RealCount then begin
    Result:=-4;  exit;
  end;
  Result:=0;
end;

procedure TLazQueue.WriteDebugReport;
var AnItem: PLazQueueItem;
begin
  writeln('TLazQueue.WriteDebugReport: Consistency=',ConsistencyCheck
         ,' Count=',Count,' FreeCount=',FFreeCount);
  AnItem:=FFirst;
  while AnItem<>nil do begin
    writeln('  Item: Data=',HexStr(Cardinal(AnItem^.Data),8)
            ,' Self=',HexStr(Cardinal(AnItem),8)
            ,' Next=',HexStr(Cardinal(AnItem^.Next),8)
            ,' Prior=',HexStr(Cardinal(AnItem^.Prior),8)
            );
    AnItem:=AnItem^.Next;
  end;
end;


end.

