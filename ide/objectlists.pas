{  $Id$  }
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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Classes to associate objects/pointers with objects/pointers.
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

  TObjectList = class
  private
    FCapacity: Integer;
    FCount: Integer;
    FList: P2Pointer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; const AValue: Pointer);
    function GetObject(Index: Integer): Pointer;
    procedure PutObject(Index: Integer; const AValue: Pointer);
    procedure SetCapacity(const AValue: Integer);
    procedure SetCount(const AValue: Integer);
    procedure Grow;
    procedure Shrink;
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    function AddObject(Item, Associated: Pointer): Integer;
    procedure Clear; dynamic;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: Pointer;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    procedure InsertObject(Index: Integer; Item, Associated: Pointer);
    function Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Assign(SrcList: TList);
    function Remove(Item: Pointer): Integer;
    procedure Pack;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property Objects[Index: Integer]: Pointer read GetObject write PutObject;
    property List: P2Pointer read FList;
  end;

implementation

{ TObjectList }

function TObjectList.GetObject(Index: Integer): Pointer;
begin
  Result:=FList[Index].Associated;
end;

procedure TObjectList.PutObject(Index: Integer; const AValue: Pointer);
begin
  FList[Index].Associated:=AValue;
end;

function TObjectList.Get(Index: Integer): Pointer;
begin
  Result:=FList[Index].Item;
end;

procedure TObjectList.Put(Index: Integer; const AValue: Pointer);
begin
  FList[Index].Item:=AValue;
end;

procedure TObjectList.SetCapacity(const AValue: Integer);
begin
  if FCapacity=AValue then exit;
  FCapacity:=AValue;
  ReallocMem(FList,SizeOf(T2Pointer)*FCapacity);
  if FCount>FCapacity then FCount:=FCapacity;
end;

procedure TObjectList.SetCount(const AValue: Integer);
begin
  if FCount=AValue then exit;
  FCount:=AValue;
  if FCount>FCapacity then SetCapacity(AValue);
end;

procedure TObjectList.Grow;
begin
  if FCapacity<5 then Capacity:=5
  else Capacity:=Capacity*2;
end;

procedure TObjectList.Shrink;
begin
  Capacity:=Capacity div 2;
end;

destructor TObjectList.Destroy;
begin
  ReallocMem(FList,0);
  inherited Destroy;
end;

function TObjectList.Add(Item: Pointer): Integer;
begin
  Result:=AddObject(Item,nil);
end;

function TObjectList.AddObject(Item, Associated: Pointer): Integer;
begin
  if FCount=FCapacity then Grow;
  FList[FCount].Item:=Item;
  FList[FCount].Associated:=Associated;
  Result:=FCount;
  inc(FCount);
end;

procedure TObjectList.Clear;
begin
  FCount:=0;
  ReallocMem(FList,0);
  FCapacity:=0;
end;

procedure TObjectList.Delete(Index: Integer);
begin
  if FCount>Index+1 then
    System.Move(FList[Index+1],FList[Index],SizeOf(T2Pointer)*(FCount-Index-1));
  dec(FCount);
  if FCapacity>FCount*4 then Shrink;
end;

procedure TObjectList.Exchange(Index1, Index2: Integer);
var
  SwapDummy: T2Pointer;
begin
  if Index1=Index2 then exit;
  SwapDummy:=FList[Index1];
  FList[Index1]:=FList[Index2];
  FList[Index2]:=SwapDummy;
end;

function TObjectList.First: Pointer;
begin
  if FCount>0 then
    Result:=FList[0].Item
  else
    Result:=nil;
end;

function TObjectList.IndexOf(Item: Pointer): Integer;
begin
  Result:=FCount-1;
  while (Result>=0) and (FList[Result].Item<>Item) do dec(Result);
end;

procedure TObjectList.Insert(Index: Integer; Item: Pointer);
begin
  InsertObject(Index,Item,nil);
end;

procedure TObjectList.InsertObject(Index: Integer; Item, Associated: Pointer);
begin
  if FCount=FCapacity then Grow;
  if Index<FCount then
    System.Move(FList[Index],FList[Index+1],SizeOf(T2Pointer)*(FCount-Index));
  inc(FCount);
  FList[Index].Item:=Item;
  FList[Index].Associated:=Associated;
end;

function TObjectList.Last: Pointer;
begin
  if FCount>0 then
    Result:=FList[FCount-1].Item
  else
    Result:=nil;
end;

procedure TObjectList.Move(CurIndex, NewIndex: Integer);
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

procedure TObjectList.Assign(SrcList: TList);
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

function TObjectList.Remove(Item: Pointer): Integer;
begin
  Result:=IndexOf(Item);
  if Result>=0 then Delete(Result);
end;

procedure TObjectList.Pack;
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

