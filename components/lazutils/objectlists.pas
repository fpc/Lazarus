{
 /***************************************************************************
                            packagedefs.pas
                            ---------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Classes to associate objects/pointers with objects/pointers.
    Converted to use generics by Juha. Item and Object types can now be defined.
}
unit ObjectLists;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  T2Pointer = record
    Item, Associated: Pointer;
  end;
  P2Pointer = ^T2Pointer;

  { TObjectArray }

  generic TObjectArray<TItem, TObj> = class
  private
    FCapacity: Integer;
    FCount: Integer;
    FList: P2Pointer;
  protected
    function Get(Index: Integer): TItem;
    procedure Put(Index: Integer; const AValue: TItem);
    function GetObject(Index: Integer): TObj;
    procedure PutObject(Index: Integer; const AValue: TObj);
    procedure SetCapacity(const AValue: Integer);
    procedure SetCount(const AValue: Integer);
    procedure Grow;
    procedure Shrink;
  public
    destructor Destroy; override;
    function Add(Item: TItem): Integer;
    function AddObject(Item: TItem; Associated: TObj): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: TItem;
    function IndexOf(Item: TItem): Integer;
    procedure Insert(Index: Integer; Item: TItem);
    procedure InsertObject(Index: Integer; Item: TItem; Associated: TObj);
    function Last: TItem;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(SrcList: TList);
    function Remove(Item: TItem): Integer;
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: TItem read Get write Put; default;
    property Objects[Index: Integer]: TObj read GetObject write PutObject;
    property List: P2Pointer read FList;
  end;

implementation

{ TObjectArray }

function TObjectArray.Get(Index: Integer): TItem;
begin
  Result:=TItem(FList[Index].Item);
end;

procedure TObjectArray.Put(Index: Integer; const AValue: TItem);
begin
  FList[Index].Item:=AValue;
end;

function TObjectArray.GetObject(Index: Integer): TObj;
begin
  Result:=TObj(FList[Index].Associated);
end;

procedure TObjectArray.PutObject(Index: Integer; const AValue: TObj);
begin
  FList[Index].Associated:=AValue;
end;

procedure TObjectArray.SetCapacity(const AValue: Integer);
begin
  if FCapacity=AValue then exit;
  FCapacity:=AValue;
  ReallocMem(FList,SizeOf(T2Pointer)*FCapacity);
  if FCount>FCapacity then FCount:=FCapacity;
end;

procedure TObjectArray.SetCount(const AValue: Integer);
begin
  if FCount=AValue then exit;
  FCount:=AValue;
  if FCount>FCapacity then SetCapacity(AValue);
end;

procedure TObjectArray.Grow;
begin
  if FCapacity<5 then Capacity:=5
  else Capacity:=Capacity*2;
end;

procedure TObjectArray.Shrink;
begin
  Capacity:=Capacity div 2;
end;

destructor TObjectArray.Destroy;
begin
  ReallocMem(FList,0);
  inherited Destroy;
end;

function TObjectArray.Add(Item: TItem): Integer;
begin
  Result:=AddObject(Item,nil);
end;

function TObjectArray.AddObject(Item: TItem; Associated: TObj): Integer;
begin
  if FCount=FCapacity then Grow;
  FList[FCount].Item:=Item;
  FList[FCount].Associated:=Associated;
  Result:=FCount;
  inc(FCount);
end;

procedure TObjectArray.Clear;
begin
  FCount:=0;
  ReallocMem(FList,0);
  FCapacity:=0;
end;

procedure TObjectArray.Delete(Index: Integer);
begin
  if FCount>Index+1 then
    System.Move(FList[Index+1],FList[Index],SizeOf(T2Pointer)*(FCount-Index-1));
  dec(FCount);
  if FCapacity>FCount*4 then Shrink;
end;

procedure TObjectArray.Exchange(Index1, Index2: Integer);
var
  SwapDummy: T2Pointer;
begin
  if Index1=Index2 then exit;
  SwapDummy:=FList[Index1];
  FList[Index1]:=FList[Index2];
  FList[Index2]:=SwapDummy;
end;

function TObjectArray.First: TItem;
begin
  if FCount>0 then
    Result:=TItem(FList[0].Item)
  else
    Result:=nil;
end;

function TObjectArray.IndexOf(Item: TItem): Integer;
begin
  Result:=FCount-1;
  while (Result>=0) and (TItem(FList[Result].Item)<>Item) do dec(Result);
end;

procedure TObjectArray.Insert(Index: Integer; Item: TItem);
begin
  InsertObject(Index,Item,nil);
end;

procedure TObjectArray.InsertObject(Index: Integer; Item: TItem; Associated: TObj);
begin
  if FCount=FCapacity then Grow;
  if Index<FCount then
    System.Move(FList[Index],FList[Index+1],SizeOf(T2Pointer)*(FCount-Index));
  inc(FCount);
  FList[Index].Item:=Item;
  FList[Index].Associated:=Associated;
end;

function TObjectArray.Last: TItem;
begin
  if FCount>0 then
    Result:=TItem(FList[FCount-1].Item)
  else
    Result:=nil;
end;

procedure TObjectArray.Move(CurIndex, NewIndex: Integer);
var
  SwapDummy: T2Pointer;
begin
  if CurIndex=NewIndex then exit;
  SwapDummy:=FList[CurIndex];
  if CurIndex<NewIndex then
    System.Move(FList[CurIndex+1],FList[CurIndex],
                SizeOf(T2Pointer)*(NewIndex-CurIndex))
  else
    System.Move(FList[NewIndex],FList[NewIndex+1],
                SizeOf(T2Pointer)*(CurIndex-NewIndex));
  FList[NewIndex]:=SwapDummy;
end;

procedure TObjectArray.Assign(SrcList: TList);
var
  i: Integer;
begin
  Clear;
  Count:=SrcList.Count;
  for i:=0 to SrcList.Count-1 do begin
    FList[i].Item:=SrcList[i];
    FList[i].Associated:=nil;
  end;
end;

function TObjectArray.Remove(Item: TItem): Integer;
begin
  Result:=IndexOf(Item);
  if Result>=0 then Delete(Result);
end;

procedure TObjectArray.Pack;
var
  SrcID: Integer;
  DestID: Integer;
begin
  SrcID:=0;
  DestID:=0;
  while SrcID<FCount do begin
    if (FList[SrcID].Item<>nil) then begin
      if SrcID<>DestID then
        FList[DestID]:=FList[SrcID];
      inc(DestID);
    end;
    inc(SrcID);
  end;
  FCount:=DestID;
end;

end.

