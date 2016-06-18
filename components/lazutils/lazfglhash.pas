{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit lazfglhash;

{$mode objfpc}{$H+}

interface

uses
  contnrs;

type
  generic TLazHTGNode<T> = Class(THTCustomNode)
  Private
    FData : T;
  public
    property Data: T read FData write FData;
  end;

  { TLazFPGHashTable }

  generic TLazFPGHashTable<T> = Class(TFPCustomHashTable)
  Protected
    type
      THTGNode = Class(THTCustomNode)
      Private
        FData : T;
      public
        property Data: T read FData write FData;
      end;
      TGIteratorMethod = Procedure(Item: T; const Key: string; var Continue: Boolean) of object;
    Function CreateNewNode(const aKey : String) : THTCustomNode; override;
    Procedure AddNode(ANode : THTCustomNode); override;
    Procedure SetData(const Index: string; AValue: T); virtual;
    Function GetData(const index: string): T; virtual;
    {$if not(defined(ver2) or defined(ver3_0))}
    Function ForEachCall(aMethod: TGIteratorMethod): THTGNode; virtual;
    {$endif}
  Public
    {$if not(defined(ver2) or defined(ver3_0))}
    Function Iterate(aMethod: TGIteratorMethod): T; virtual;
    {$endif}
    Procedure Add(const aKey: string; const aItem: T); virtual;
    property Items[const index: string]: T read GetData write SetData; default;
  end;


implementation

{ TFPGHashTable }

function TLazFPGHashTable.CreateNewNode(const aKey: String): THTCustomNode;
begin
  Result:=THTGNode.CreateWith(aKey);
end;

procedure TLazFPGHashTable.AddNode(ANode: THTCustomNode);
begin
  with THTGNode(ANode) do
    Add(Key,Data);
end;

procedure TLazFPGHashTable.SetData(const Index: string; AValue: T);
begin
  THTGNode(FindOrCreateNew(index)).Data:=AValue;
end;

function TLazFPGHashTable.GetData(const index: string): T;
var
  node: THTGNode;
begin
  node:=THTGNode(Find(Index));
  if Assigned(node) then
    Result:=node.Data;
end;

{$if not(defined(ver2) or defined(ver3_0))}
function TLazFPGHashTable.ForEachCall(aMethod: TGIteratorMethod): THTGNode;
var
  i, j: Longword;
  continue: boolean;
begin
  Result:=nil;
  continue:=True;
  if HashTableSize>0 then
    for i:=0 to HashTableSize-1 do
      if Assigned(Chain(i)) then
        if chain(i).Count>0 then
          for j:=0 to Chain(i).Count-1 do
            begin
            aMethod(THTGNode(Chain(i)[j]).Data, THTGNode(Chain(i)[j]).Key, continue);
            if not continue then
              begin
              Result:=THTGNode(Chain(i)[j]);
              Exit;
              end;
            end;
end;

function TLazFPGHashTable.Iterate(aMethod: TGIteratorMethod): T;
var
  N : THTGNode;
begin
  N:=ForEachCall(AMethod);
  if Assigned(N) then
    Result:=N.Data
end;
{$endif}

procedure TLazFPGHashTable.Add(const aKey: string; const aItem: T);
var
  chn: TFPObjectList;
  NewNode: THTGNode;
begin
  chn:=FindChainForAdd(akey);
  NewNode:=THTGNode(CreateNewNode(aKey));
  NewNode.Data:=aItem;
  chn.Add(NewNode);
end;

end.

