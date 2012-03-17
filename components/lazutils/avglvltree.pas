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
    The Tree is sorted ascending from left to right. That means
    Compare(Node.Left,Node.Right) <= 0 for all nodes.

    TAvgLvlTree is an Average Level binary Tree. This binary tree is always
    balanced, so that inserting, deleting and finding a node is performed in
    O(log(#Nodes)).
}
unit AvgLvlTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAvgLvlTree = class;
  
  TObjectSortCompare = function(Tree: TAvgLvlTree; Data1, Data2: Pointer
                                ): integer of object;

  { TAvgLvlTreeNode }

  TAvgLvlTreeNode = class
  public
    Parent, Left, Right: TAvgLvlTreeNode;
    Balance: integer; // = RightDepth-LeftDepth  -2..+2, after balancing: -1,0,+1
    Data: Pointer;
    function FindSuccessor: TAvgLvlTreeNode; // next right
    function FindPrecessor: TAvgLvlTreeNode; // next left
    function TreeDepth: integer; // longest WAY down. e.g. only one node => 0 !
    procedure ConsistencyCheck(Tree: TAvgLvlTree);
    function GetCount: SizeInt;
  end;
  TAvgLvlTreeNodeClass = class of TAvgLvlTreeNode;
  PAvgLvlTreeNode = ^TAvgLvlTreeNode;

  { TAvgLvlTreeNodeEnumerator - left to right, low to high }

  TAvgLvlTreeNodeEnumerator = class
  protected
    FCurrent: TAvgLvlTreeNode;
    FLowToHigh: boolean;
    FTree: TAvgLvlTree;
  public
    constructor Create(Tree: TAvgLvlTree; aLowToHigh: boolean = true);
    function MoveNext: Boolean;
    property Current: TAvgLvlTreeNode read FCurrent;
    property LowToHigh: boolean read FLowToHigh;
 end;

  { TAvgLvlTree }

  TAvgLvlTree = class
  protected
    fRoot: TAvgLvlTreeNode;
    FCount: SizeInt;
    FNodeClass: TAvgLvlTreeNodeClass;
    FOnCompare: TListSortCompare;
    FOnObjectCompare: TObjectSortCompare;
    procedure BalanceAfterInsert(ANode: TAvgLvlTreeNode);
    procedure BalanceAfterDelete(ANode: TAvgLvlTreeNode);
    function FindInsertPos(Data: Pointer): TAvgLvlTreeNode;
    procedure SetOnCompare(const AValue: TListSortCompare);
    procedure SetOnObjectCompare(const AValue: TObjectSortCompare);
    procedure SetCompares(const NewCompare: TListSortCompare;
                          const NewObjectCompare: TObjectSortCompare);
    procedure Init; virtual;
  public
    constructor Create(OnCompareMethod: TListSortCompare);
    constructor CreateObjectCompare(OnCompareMethod: TObjectSortCompare);
    constructor Create;
    destructor Destroy; override;
    property OnCompare: TListSortCompare read FOnCompare write SetOnCompare;
    property OnObjectCompare: TObjectSortCompare read FOnObjectCompare write SetOnObjectCompare;
    property NodeClass: TAvgLvlTreeNodeClass read FNodeClass write FNodeClass; // used for new nodes

    // add, delete, remove, move
    procedure Add(ANode: TAvgLvlTreeNode);
    function Add(Data: Pointer): TAvgLvlTreeNode;
    procedure Delete(ANode: TAvgLvlTreeNode);
    procedure Remove(Data: Pointer);
    procedure RemovePointer(Data: Pointer);
    procedure MoveDataLeftMost(var ANode: TAvgLvlTreeNode);
    procedure MoveDataRightMost(var ANode: TAvgLvlTreeNode);
    procedure Clear;
    procedure FreeAndClear;
    procedure FreeAndDelete(ANode: TAvgLvlTreeNode);

    // search
    property Root: TAvgLvlTreeNode read fRoot;
    property Count: SizeInt read FCount;
    function Compare(Data1, Data2: Pointer): integer;
    function Find(Data: Pointer): TAvgLvlTreeNode;
    function FindKey(Key: Pointer;
                     OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
    function FindNearestKey(Key: Pointer;
                       OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
    function FindSuccessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode; inline;
    function FindPrecessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode; inline;
    function FindLowest: TAvgLvlTreeNode;
    function FindHighest: TAvgLvlTreeNode;
    function FindNearest(Data: Pointer): TAvgLvlTreeNode;
    function FindPointer(Data: Pointer): TAvgLvlTreeNode;
    function FindLeftMost(Data: Pointer): TAvgLvlTreeNode;
    function FindRightMost(Data: Pointer): TAvgLvlTreeNode;
    function FindLeftMostKey(Key: Pointer;
                       OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
    function FindRightMostKey(Key: Pointer;
                       OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
    function FindLeftMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
    function FindRightMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;

    // enumerators
    function GetEnumerator: TAvgLvlTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TAvgLvlTreeNodeEnumerator;

    // consistency
    procedure ConsistencyCheck;
    procedure WriteReportToStream(s: TStream);
    function ReportAsString: string;
  end;
  PAvgLvlTree = ^TAvgLvlTree;

type
  { TPointerToPointerTree - Associative array }

  TPointerToPointerItem = record
    Key: Pointer;
    Value: Pointer;
  end;
  PPointerToPointerItem = ^TPointerToPointerItem;

  TPointerToPointerTree = class
  private
    FItems: TAvgLvlTree;
    function GetCount: Integer;
    function GetValues(const Key: Pointer): Pointer;
    procedure SetValues(const Key: Pointer; const AValue: Pointer);
    function FindNode(const Key: Pointer): TAvgLvlTreeNode;
    function GetNode(Node: TAvgLvlTreeNode; out Key, Value: Pointer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Remove(Key: Pointer);
    function Contains(const Key: Pointer): Boolean;
    function GetFirst(out Key, Value: Pointer): Boolean;
    function GetLast(out Key, Value: Pointer): Boolean;
    function GetNext(const Key: Pointer; out NextKey, NextValue: Pointer): Boolean;
    function GetPrev(const Key: Pointer; out PrevKey, PrevValue: Pointer): Boolean;
    property Count: Integer read GetCount;
    property Values[const Key: Pointer]: Pointer read GetValues write SetValues; default;
    property Tree: TAvgLvlTree read FItems;
  end;


function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;


type
  TStringMap = class;

  TStringMapItem = record
    Name: string;
  end;
  PStringMapItem = ^TStringMapItem;

  { TCustomStringMapEnumerator }

  TCustomStringMapEnumerator = class
  protected
    FTree: TAvgLvlTree;
    FCurrent: TAvgLvlTreeNode;
  public
    constructor Create(Tree: TAvgLvlTree);
    function MoveNext: boolean;
    // "Current" is implemented by the descendant classes
  end;

  { TCustomStringMap }

  TCustomStringMap = class
  private
    FCompareKeyItemFunc: TListSortCompare;
    FTree: TAvgLvlTree;// tree of PStringMapItem
    FCaseSensitive: boolean;
    function GetCompareItemsFunc: TListSortCompare;
  protected
    procedure DisposeItem(p: PStringMapItem); virtual;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; virtual;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; virtual;
  public
    constructor Create(TheCaseSensitive: boolean);
    constructor Create(const ACompareItems, ACompareNameWithItem: TListSortCompare;
                       TheCaseSensitive: boolean = false);
    destructor Destroy; override;
    procedure Clear; virtual;
    function Contains(const s: string): boolean;
    procedure GetNames(List: TStrings);
    procedure Remove(const Name: string); virtual;
    property CaseSensitive: boolean read FCaseSensitive;
    property Tree: TAvgLvlTree read FTree; // tree of PStringMapItem
    function FindNode(const s: string): TAvgLvlTreeNode;
    function Count: integer; inline;
    function Equals(OtherTree: TCustomStringMap): boolean; reintroduce;
    procedure Assign(Source: TCustomStringMap); virtual;
    property CompareItemsFunc: TListSortCompare read GetCompareItemsFunc;
    property CompareKeyItemFunc: TListSortCompare read FCompareKeyItemFunc;
    procedure SetCompareFuncs(
            const NewCompareItemsFunc, NewCompareKeyItemFunc: TListSortCompare;
            NewCaseSensitive: boolean);
  end;

  { TStringMapEnumerator }

  TStringMapEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: string; inline;
  public
    property Current: string read GetCurrent;
  end;

  { TStringMap }

  TStringMap = class(TCustomStringMap)
  public
    procedure Add(const Name: string);
    function GetEnumerator: TStringMapEnumerator;
  end;

  { TStringToStringTree - Associative array }

  TStringToStringItem = record
    Name: string;
    Value: string;
  end;
  PStringToStringItem = ^TStringToStringItem;

  { TStringToStringTreeEnumerator }

  TStringToStringTreeEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: PStringToStringItem; inline;
  public
    property Current: PStringToStringItem read GetCurrent;
  end;

  { TNewStringToStringTree }

  TNewStringToStringTree = class(TCustomStringMap)
  private
    function GetValues(const s: string): string;
    procedure SetValues(const s: string; const AValue: string);
  protected
    procedure DisposeItem(p: PStringMapItem); override;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; override;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; override;
    function GetNode(Node: TAvgLvlTreeNode; out Name, Value: string): Boolean;
  public
    function GetString(const Name: string; out Value: string): boolean;
    procedure Add(const Name, Value: string); inline;
    procedure Add(const Name, Value, Delimiter: string);
    procedure AddNameValues(List: TStrings);
    procedure AddValues(List: TStrings); inline; deprecated;
    procedure AddNames(List: TStrings);
    procedure Delete(const Name: string); inline; deprecated;
    property Values[const s: string]: string read GetValues write SetValues; default;
    function AsText: string;
    procedure Assign(Source: TCustomStringMap); override;
    function GetEnumerator: TStringToStringTreeEnumerator;
    function GetFirst(out Name, Value: string): Boolean;
    function GetLast(out Name, Value: string): Boolean;
    function GetNext(const Name: string; out NextName, NextValue: string): Boolean;
    function GetPrev(const Name: string; out PrevName, PrevValue: string): Boolean;
  end;

  { TStringToStringTree }

  TStringToStringTree = class
  private
    FCompareItems: TListSortCompare;
    FCompareNameWithItem: TListSortCompare;
    FItems: TAvgLvlTree;
    function GetCount: Integer;
    function GetValues(const Name: string): string;
    procedure SetValues(const Name: string; const AValue: string);
    function FindNode(const Name: string): TAvgLvlTreeNode;
    function GetNode(Node: TAvgLvlTreeNode; out Name, Value: string): Boolean;
  public
    constructor Create(CaseSensitive: boolean);
    constructor Create(const ACompareItems, ACompareNameWithItem: TListSortCompare);
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Src: TStringToStringTree);
    function Contains(const Name: string): Boolean;
    procedure Delete(const Name: string);
    procedure Add(const Name, Value, Delimiter: string);
    procedure AddNameValues(List: TStrings);
    procedure AddValues(List: TStrings); inline; deprecated;
    procedure AddNames(List: TStrings);
    function GetFirst(out Name, Value: string): Boolean;
    function GetLast(out Name, Value: string): Boolean;
    function GetNext(const Name: string; out NextName, NextValue: string): Boolean;
    function GetPrev(const Name: string; out PrevName, PrevValue: string): Boolean;
    property Count: Integer read GetCount;
    property Values[const Name: string]: string read GetValues write SetValues; default;
    property Tree: TAvgLvlTree read FItems;
    property CompareItems: TListSortCompare read FCompareItems;
    property CompareNameWithItem: TListSortCompare read FCompareNameWithItem;
  end;

  { TStringToPointerTree - Associative array }

  TStringToPointerItem = record
    Name: string;
    Value: Pointer;
  end;
  PStringToPointerItem = ^TStringToPointerItem;

  { TStringToPointerTreeEnumerator }

  TStringToPointerTreeEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: PStringToPointerItem; inline;
  public
    property Current: PStringToPointerItem read GetCurrent;
  end;

  TStringToPointerTree = class(TCustomStringMap)
  private
    FFreeValues: boolean;
    function GetValues(const s: string): Pointer;
    procedure SetValues(const s: string; const AValue: Pointer);
  protected
    procedure DisposeItem(p: PStringMapItem); override;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; override;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; override;
  public
    function GetData(const Name: string; out Value: Pointer): boolean;
    property Values[const s: string]: Pointer read GetValues write SetValues; default;
    function GetEnumerator: TStringToPointerTreeEnumerator;
    property FreeValues: boolean read FFreeValues write FFreeValues;
  end;

function CompareStringToStringItems(Data1, Data2: Pointer): integer;
function CompareAnsiStringWithStrToStrItem(Key, Data: Pointer): Integer;
function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
function CompareAnsiStringWithStrToStrItemI(Key, Data: Pointer): Integer;


implementation


function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
begin
  Result:=ComparePointer(PPointerToPointerItem(Data1)^.Key,
                         PPointerToPointerItem(Data2)^.Key);
end;

function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;
begin
  Result:=ComparePointer(Key,PPointerToPointerItem(Data)^.Key);
end;

function CompareStringToStringItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(PStringMapItem(Data1)^.Name,
                     PStringMapItem(Data2)^.Name);
end;

function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
begin
  Result:=CompareText(PStringMapItem(Data1)^.Name,
                      PStringMapItem(Data2)^.Name);
end;

function CompareAnsiStringWithStrToStrItem(Key, Data: Pointer): Integer;
begin
  Result:=CompareStr(AnsiString(Key),PStringMapItem(Data)^.Name);
end;

function CompareAnsiStringWithStrToStrItemI(Key, Data: Pointer): Integer;
begin
  Result:=CompareText(AnsiString(Key),PStringMapItem(Data)^.Name);
end;

{ TAvgLvlTreeNodeEnumerator }

constructor TAvgLvlTreeNodeEnumerator.Create(Tree: TAvgLvlTree;
  aLowToHigh: boolean);
begin
  FTree:=Tree;
  FLowToHigh:=aLowToHigh;
end;

function TAvgLvlTreeNodeEnumerator.MoveNext: Boolean;
begin
  if FLowToHigh then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.FindSuccessor
    else
      FCurrent:=FTree.FindLowest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.FindPrecessor
    else
      FCurrent:=FTree.FindHighest;
  end;
  Result:=FCurrent<>nil;
end;

function TStringToPointerTree.GetValues(const s: string): Pointer;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToPointerItem(Node.Data)^.Value
  else
    Result:=nil
end;

procedure TStringToPointerTree.SetValues(const s: string; const AValue: Pointer
  );
var
  Node: TAvgLvlTreeNode;
  NewItem: PStringToPointerItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    PStringToPointerItem(Node.Data)^.Value:=AValue;
  end else begin
    New(NewItem);
    NewItem^.Name:=s;
    NewItem^.Value:=AValue;
    FTree.Add(NewItem);
  end;
end;

procedure TStringToPointerTree.DisposeItem(p: PStringMapItem);
var
  Item: PStringToPointerItem absolute p;
begin
  if FreeValues then
    TObject(Item^.Value).Free;
  Dispose(Item);
end;

function TStringToPointerTree.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
var
  Item1: PStringToPointerItem absolute p1;
  Item2: PStringToPointerItem absolute p2;
begin
  Result:=(Item1^.Name=Item2^.Name)
      and (Item1^.Value=Item2^.Value);
end;

function TStringToPointerTree.CreateCopy(Src: PStringMapItem): PStringMapItem;
var
  SrcItem: PStringToPointerItem absolute Src;
  NewItem: PStringToPointerItem;
begin
  New(NewItem);
  NewItem^.Name:=SrcItem^.Name;
  NewItem^.Value:=SrcItem^.Value;
  Result:=PStringMapItem(NewItem);
end;

function TStringToPointerTree.GetData(const Name: string; out Value: Pointer
  ): boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToPointerItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TStringToPointerTree.GetEnumerator: TStringToPointerTreeEnumerator;
begin
  Result:=TStringToPointerTreeEnumerator.Create(FTree);
end;

{ TStringToPointerTreeEnumerator }

function TStringToPointerTreeEnumerator.GetCurrent: PStringToPointerItem;
begin
  Result:=PStringToPointerItem(FCurrent.Data);
end;

{ TNewStringToStringTree }

function TNewStringToStringTree.GetValues(const s: string): string;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToStringItem(Node.Data)^.Value
  else
    Result:=''
end;

procedure TNewStringToStringTree.SetValues(const s: string;
  const AValue: string);
var
  Node: TAvgLvlTreeNode;
  NewItem: PStringToStringItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    PStringToStringItem(Node.Data)^.Value:=AValue;
  end else begin
    New(NewItem);
    NewItem^.Name:=s;
    NewItem^.Value:=AValue;
    FTree.Add(NewItem);
  end;
end;

procedure TNewStringToStringTree.DisposeItem(p: PStringMapItem);
var
  Item: PStringToStringItem absolute p;
begin
  Dispose(Item);
end;

function TNewStringToStringTree.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
var
  Item1: PStringToStringItem absolute p1;
  Item2: PStringToStringItem absolute p2;
begin
  Result:=(Item1^.Name=Item2^.Name)
      and (Item1^.Value=Item2^.Value);
end;

function TNewStringToStringTree.CreateCopy(Src: PStringMapItem): PStringMapItem;
var
  SrcItem: PStringToStringItem absolute Src;
  NewItem: PStringToStringItem;
begin
  New(NewItem);
  NewItem^.Name:=SrcItem^.Name;
  NewItem^.Value:=SrcItem^.Value;
  Result:=PStringMapItem(NewItem);
end;

function TNewStringToStringTree.GetNode(Node: TAvgLvlTreeNode; out Name,
  Value: string): Boolean;
var
  Item: PStringToStringItem;
begin
  if Node<>nil then begin
    Item:=PStringToStringItem(Node.Data);
    Name:=Item^.Name;
    Value:=Item^.Value;
    Result:=true;
  end else begin
    Name:='';
    Value:='';
    Result:=false;
  end;
end;

function TNewStringToStringTree.GetString(const Name: string; out Value: string
  ): boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToStringItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TNewStringToStringTree.Add(const Name, Value: string);
begin
  Values[Name]:=Value;
end;

procedure TNewStringToStringTree.Add(const Name, Value, Delimiter: string);
var
  OldValue: string;
begin
  OldValue:=Values[Name];
  if OldValue<>'' then
    OldValue:=OldValue+Delimiter;
  OldValue:=OldValue+Value;
  Values[Name]:=OldValue;
end;

procedure TNewStringToStringTree.AddNameValues(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List.Names[i]]:=List.ValueFromIndex[i];
end;

procedure TNewStringToStringTree.AddValues(List: TStrings);
begin
  AddNames(List);
end;

procedure TNewStringToStringTree.AddNames(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List[i]]:='';
end;

procedure TNewStringToStringTree.Delete(const Name: string);
begin
  Remove(Name);
end;

function TNewStringToStringTree.GetFirst(out Name, Value: string): Boolean;
begin
  Result:=GetNode(Tree.FindLowest,Name,Value);
end;

function TNewStringToStringTree.GetLast(out Name, Value: string): Boolean;
begin
  Result:=GetNode(Tree.FindHighest,Name,Value);
end;

function TNewStringToStringTree.GetNext(const Name: string; out NextName,
  NextValue: string): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.FindSuccessor;
  Result:=GetNode(Node,NextName,NextValue);
end;

function TNewStringToStringTree.GetPrev(const Name: string; out PrevName,
  PrevValue: string): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.FindPrecessor;
  Result:=GetNode(Node,PrevName,PrevValue);
end;

function TNewStringToStringTree.AsText: string;
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  Result:='';
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Result:=Result+Item^.Name+'='+Item^.Value+LineEnding;
    Node:=Node.FindSuccessor;
  end;
end;

procedure TNewStringToStringTree.Assign(Source: TCustomStringMap);
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  if (Source=nil) or (Source.ClassType<>ClassType) then
    raise Exception.Create('invalid class');
  Clear;
  Node:=Source.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Values[Item^.Name]:=Item^.Value;
    Node:=Node.FindSuccessor;
  end;
end;

function TNewStringToStringTree.GetEnumerator: TStringToStringTreeEnumerator;
begin
  Result:=TStringToStringTreeEnumerator.Create(FTree);
end;

{ TStringMapEnumerator }

function TStringMapEnumerator.GetCurrent: string;
begin
  Result:=PStringMapItem(FCurrent.Data)^.Name;
end;

{ TStringMap }

procedure TStringMap.Add(const Name: string);
var
  Node: TAvgLvlTreeNode;
  NewItem: PStringMapItem;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    exit;
  end else begin
    New(NewItem);
    NewItem^.Name:=Name;
    FTree.Add(NewItem);
  end;
end;

function TStringMap.GetEnumerator: TStringMapEnumerator;
begin
  Result:=TStringMapEnumerator.Create(Tree);
end;

{ TStringToStringTreeEnumerator }

function TStringToStringTreeEnumerator.GetCurrent: PStringToStringItem;
begin
  Result:=PStringToStringItem(FCurrent.Data);
end;

{ TAvgLvlTree }

function TAvgLvlTree.Add(Data: Pointer): TAvgLvlTreeNode;
begin
  Result:=NodeClass.Create;
  Result.Data:=Data;
  Add(Result);
end;

procedure TAvgLvlTree.Add(ANode: TAvgLvlTreeNode);
// add a node. If there are already nodes with the same value it will be
// inserted rightmost
var InsertPos: TAvgLvlTreeNode;
  InsertComp: integer;
begin
  ANode.Left:=nil;
  ANode.Right:=nil;
  inc(FCount);
  if fRoot<>nil then begin
    InsertPos:=FindInsertPos(ANode.Data);
    InsertComp:=Compare(ANode.Data,InsertPos.Data);
    ANode.Parent:=InsertPos;
    if InsertComp<0 then begin
      // insert to the left
      InsertPos.Left:=ANode;
    end else begin
      // insert to the right
      InsertPos.Right:=ANode;
    end;
    BalanceAfterInsert(ANode);
  end else begin
    fRoot:=ANode;
    ANode.Parent:=nil;
  end;
end;

function TAvgLvlTree.FindLowest: TAvgLvlTreeNode;
begin
  Result:=fRoot;
  if Result<>nil then
    while Result.Left<>nil do Result:=Result.Left;
end;

function TAvgLvlTree.FindHighest: TAvgLvlTreeNode;
begin
  Result:=fRoot;
  if Result<>nil then
    while Result.Right<>nil do Result:=Result.Right;
end;
    
procedure TAvgLvlTree.BalanceAfterDelete(ANode: TAvgLvlTreeNode);
var OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight,
  OldRightLeftLeft, OldRightLeftRight, OldLeftRightLeft, OldLeftRightRight
  : TAvgLvlTreeNode;
begin
  if (ANode=nil) then exit;
  if ((ANode.Balance=+1) or (ANode.Balance=-1)) then exit;
  OldParent:=ANode.Parent;
  if (ANode.Balance=0) then begin
    // Treeheight has decreased by one
    if (OldParent<>nil) then begin
      if(OldParent.Left=ANode) then
        Inc(OldParent.Balance)
      else
        Dec(OldParent.Balance);
      BalanceAfterDelete(OldParent);
    end;
    exit;
  end;
  if (ANode.Balance=+2) then begin
    // Node is overweighted to the right
    OldRight:=ANode.Right;
    if (OldRight.Balance>=0) then begin
      // OldRight.Balance=={0 or -1}
      // rotate left
      OldRightLeft:=OldRight.Left;
      if (OldParent<>nil) then begin
        if (OldParent.Left=ANode) then
          OldParent.Left:=OldRight
        else
          OldParent.Right:=OldRight;
      end else
        fRoot:=OldRight;
      ANode.Parent:=OldRight;
      ANode.Right:=OldRightLeft;
      OldRight.Parent:=OldParent;
      OldRight.Left:=ANode;
      if (OldRightLeft<>nil) then
        OldRightLeft.Parent:=ANode;
      ANode.Balance:=(1-OldRight.Balance);
      Dec(OldRight.Balance);
      BalanceAfterDelete(OldRight);
    end else begin
      // OldRight.Balance=-1
      // double rotate right left
      OldRightLeft:=OldRight.Left;
      OldRightLeftLeft:=OldRightLeft.Left;
      OldRightLeftRight:=OldRightLeft.Right;
      if (OldParent<>nil) then begin
        if (OldParent.Left=ANode) then
          OldParent.Left:=OldRightLeft
        else
          OldParent.Right:=OldRightLeft;
      end else
        fRoot:=OldRightLeft;
      ANode.Parent:=OldRightLeft;
      ANode.Right:=OldRightLeftLeft;
      OldRight.Parent:=OldRightLeft;
      OldRight.Left:=OldRightLeftRight;
      OldRightLeft.Parent:=OldParent;
      OldRightLeft.Left:=ANode;
      OldRightLeft.Right:=OldRight;
      if (OldRightLeftLeft<>nil) then
        OldRightLeftLeft.Parent:=ANode;
      if (OldRightLeftRight<>nil) then
        OldRightLeftRight.Parent:=OldRight;
      if (OldRightLeft.Balance<=0) then
        ANode.Balance:=0
      else
        ANode.Balance:=-1;
      if (OldRightLeft.Balance>=0) then
        OldRight.Balance:=0
      else
        OldRight.Balance:=+1;
      OldRightLeft.Balance:=0;
      BalanceAfterDelete(OldRightLeft);
    end;
  end else begin
    // Node.Balance=-2
    // Node is overweighted to the left
    OldLeft:=ANode.Left;
    if (OldLeft.Balance<=0) then begin
      // rotate right
      OldLeftRight:=OldLeft.Right;
      if (OldParent<>nil) then begin
        if (OldParent.Left=ANode) then
          OldParent.Left:=OldLeft
        else
          OldParent.Right:=OldLeft;
      end else
        fRoot:=OldLeft;
      ANode.Parent:=OldLeft;
      ANode.Left:=OldLeftRight;
      OldLeft.Parent:=OldParent;
      OldLeft.Right:=ANode;
      if (OldLeftRight<>nil) then
        OldLeftRight.Parent:=ANode;
      ANode.Balance:=(-1-OldLeft.Balance);
      Inc(OldLeft.Balance);
      BalanceAfterDelete(OldLeft);
    end else begin
      // OldLeft.Balance = 1
      // double rotate left right
      OldLeftRight:=OldLeft.Right;
      OldLeftRightLeft:=OldLeftRight.Left;
      OldLeftRightRight:=OldLeftRight.Right;
      if (OldParent<>nil) then begin
        if (OldParent.Left=ANode) then
          OldParent.Left:=OldLeftRight
        else
          OldParent.Right:=OldLeftRight;
      end else
        fRoot:=OldLeftRight;
      ANode.Parent:=OldLeftRight;
      ANode.Left:=OldLeftRightRight;
      OldLeft.Parent:=OldLeftRight;
      OldLeft.Right:=OldLeftRightLeft;
      OldLeftRight.Parent:=OldParent;
      OldLeftRight.Left:=OldLeft;
      OldLeftRight.Right:=ANode;
      if (OldLeftRightLeft<>nil) then
        OldLeftRightLeft.Parent:=OldLeft;
      if (OldLeftRightRight<>nil) then
        OldLeftRightRight.Parent:=ANode;
      if (OldLeftRight.Balance>=0) then
        ANode.Balance:=0
      else
        ANode.Balance:=+1;
      if (OldLeftRight.Balance<=0) then
        OldLeft.Balance:=0
      else
        OldLeft.Balance:=-1;
      OldLeftRight.Balance:=0;
      BalanceAfterDelete(OldLeftRight);
    end;
  end;
end;

procedure TAvgLvlTree.BalanceAfterInsert(ANode: TAvgLvlTreeNode);
var OldParent, OldParentParent, OldRight, OldRightLeft, OldRightRight, OldLeft,
   OldLeftLeft, OldLeftRight: TAvgLvlTreeNode;
begin
  OldParent:=ANode.Parent;
  if (OldParent=nil) then exit;
  if (OldParent.Left=ANode) then begin
    // Node is left son
    dec(OldParent.Balance);
    if (OldParent.Balance=0) then exit;
    if (OldParent.Balance=-1) then begin
      BalanceAfterInsert(OldParent);
      exit;
    end;
    // OldParent.Balance=-2
    if (ANode.Balance=-1) then begin
      // rotate
      OldRight:=ANode.Right;
      OldParentParent:=OldParent.Parent;
      if (OldParentParent<>nil) then begin
        // OldParent has GrandParent. GrandParent gets new child
        if (OldParentParent.Left=OldParent) then
          OldParentParent.Left:=ANode
        else
          OldParentParent.Right:=ANode;
      end else begin
        // OldParent was fRoot node. New fRoot node
        fRoot:=ANode;
      end;
      ANode.Parent:=OldParentParent;
      ANode.Right:=OldParent;
      OldParent.Parent:=ANode;
      OldParent.Left:=OldRight;
      if (OldRight<>nil) then
        OldRight.Parent:=OldParent;
      ANode.Balance:=0;
      OldParent.Balance:=0;
    end else begin
      // Node.Balance = +1
      // double rotate
      OldParentParent:=OldParent.Parent;
      OldRight:=ANode.Right;
      OldRightLeft:=OldRight.Left;
      OldRightRight:=OldRight.Right;
      if (OldParentParent<>nil) then begin
        // OldParent has GrandParent. GrandParent gets new child
        if (OldParentParent.Left=OldParent) then
          OldParentParent.Left:=OldRight
        else
          OldParentParent.Right:=OldRight;
      end else begin
        // OldParent was fRoot node. new fRoot node
        fRoot:=OldRight;
      end;
      OldRight.Parent:=OldParentParent;
      OldRight.Left:=ANode;
      OldRight.Right:=OldParent;
      ANode.Parent:=OldRight;
      ANode.Right:=OldRightLeft;
      OldParent.Parent:=OldRight;
      OldParent.Left:=OldRightRight;
      if (OldRightLeft<>nil) then
        OldRightLeft.Parent:=ANode;
      if (OldRightRight<>nil) then
        OldRightRight.Parent:=OldParent;
      if (OldRight.Balance<=0) then
        ANode.Balance:=0
      else
        ANode.Balance:=-1;
      if (OldRight.Balance=-1) then
        OldParent.Balance:=1
      else
        OldParent.Balance:=0;
      OldRight.Balance:=0;
    end;
  end else begin
    // Node is right son
    Inc(OldParent.Balance);
    if (OldParent.Balance=0) then exit;
    if (OldParent.Balance=+1) then begin
      BalanceAfterInsert(OldParent);
      exit;
    end;
    // OldParent.Balance = +2
    if(ANode.Balance=+1) then begin
      // rotate
      OldLeft:=ANode.Left;
      OldParentParent:=OldParent.Parent;
      if (OldParentParent<>nil) then begin
        // Parent has GrandParent . GrandParent gets new child
        if(OldParentParent.Left=OldParent) then
          OldParentParent.Left:=ANode
        else
          OldParentParent.Right:=ANode;
      end else begin
        // OldParent was fRoot node . new fRoot node
        fRoot:=ANode;
      end;
      ANode.Parent:=OldParentParent;
      ANode.Left:=OldParent;
      OldParent.Parent:=ANode;
      OldParent.Right:=OldLeft;
      if (OldLeft<>nil) then
        OldLeft.Parent:=OldParent;
      ANode.Balance:=0;
      OldParent.Balance:=0;
    end else begin
      // Node.Balance = -1
      // double rotate
      OldLeft:=ANode.Left;
      OldParentParent:=OldParent.Parent;
      OldLeftLeft:=OldLeft.Left;
      OldLeftRight:=OldLeft.Right;
      if (OldParentParent<>nil) then begin
        // OldParent has GrandParent . GrandParent gets new child
        if (OldParentParent.Left=OldParent) then
          OldParentParent.Left:=OldLeft
        else
          OldParentParent.Right:=OldLeft;
      end else begin
        // OldParent was fRoot node . new fRoot node
        fRoot:=OldLeft;
      end;
      OldLeft.Parent:=OldParentParent;
      OldLeft.Left:=OldParent;
      OldLeft.Right:=ANode;
      ANode.Parent:=OldLeft;
      ANode.Left:=OldLeftRight;
      OldParent.Parent:=OldLeft;
      OldParent.Right:=OldLeftLeft;
      if (OldLeftLeft<>nil) then
        OldLeftLeft.Parent:=OldParent;
      if (OldLeftRight<>nil) then
        OldLeftRight.Parent:=ANode;
      if (OldLeft.Balance>=0) then
        ANode.Balance:=0
      else
        ANode.Balance:=+1;
      if (OldLeft.Balance=+1) then
        OldParent.Balance:=-1
      else
        OldParent.Balance:=0;
      OldLeft.Balance:=0;
    end;
  end;
end;

procedure TAvgLvlTree.Clear;

  procedure DeleteNode(ANode: TAvgLvlTreeNode);
  begin
    if ANode<>nil then begin
      if ANode.Left<>nil then DeleteNode(ANode.Left);
      if ANode.Right<>nil then DeleteNode(ANode.Right);
    end;
    ANode.Free;
  end;

// Clear
begin
  DeleteNode(fRoot);
  fRoot:=nil;
  FCount:=0;
end;

constructor TAvgLvlTree.Create(OnCompareMethod: TListSortCompare);
begin
  inherited Create;
  FOnCompare:=OnCompareMethod;
  Init;
end;

constructor TAvgLvlTree.CreateObjectCompare(
  OnCompareMethod: TObjectSortCompare);
begin
  inherited Create;
  FOnObjectCompare:=OnCompareMethod;
  Init;
end;

constructor TAvgLvlTree.Create;
begin
  Create(@ComparePointer);
end;

procedure TAvgLvlTree.Delete(ANode: TAvgLvlTreeNode);
var OldParent, OldLeft, OldRight, Successor, OldSuccParent, OldSuccLeft,
  OldSuccRight: TAvgLvlTreeNode;
  OldBalance: integer;
begin
  OldParent:=ANode.Parent;
  OldBalance:=ANode.Balance;
  ANode.Parent:=nil;
  ANode.Balance:=0;
  if ((ANode.Left=nil) and (ANode.Right=nil)) then begin
    // Node is Leaf (no children)
    if (OldParent<>nil) then begin
      // Node has parent
      if (OldParent.Left=ANode) then begin
        // Node is left Son of OldParent
        OldParent.Left:=nil;
        Inc(OldParent.Balance);
      end else begin
        // Node is right Son of OldParent
        OldParent.Right:=nil;
        Dec(OldParent.Balance);
      end;
      BalanceAfterDelete(OldParent);
    end else begin
      // Node is the only node of tree
      fRoot:=nil;
    end;
    dec(FCount);
    ANode.Free;
    exit;
  end;
  if (ANode.Right=nil) then begin
    // Left is only son
    // and because DelNode is AVL, Right has no childrens
    // replace DelNode with Left
    OldLeft:=ANode.Left;
    ANode.Left:=nil;
    OldLeft.Parent:=OldParent;
    if (OldParent<>nil) then begin
      if (OldParent.Left=ANode) then begin
        OldParent.Left:=OldLeft;
        Inc(OldParent.Balance);
      end else begin
        OldParent.Right:=OldLeft;
        Dec(OldParent.Balance);
      end;
      BalanceAfterDelete(OldParent);
    end else begin
      fRoot:=OldLeft;
    end;
    dec(FCount);
    ANode.Free;
    exit;
  end;
  if (ANode.Left=nil) then begin
    // Right is only son
    // and because DelNode is AVL, Left has no childrens
    // replace DelNode with Right
    OldRight:=ANode.Right;
    ANode.Right:=nil;
    OldRight.Parent:=OldParent;
    if (OldParent<>nil) then begin
      if (OldParent.Left=ANode) then begin
        OldParent.Left:=OldRight;
        Inc(OldParent.Balance);
      end else begin
        OldParent.Right:=OldRight;
        Dec(OldParent.Balance);
      end;
      BalanceAfterDelete(OldParent);
    end else begin
      fRoot:=OldRight;
    end;
    dec(FCount);
    ANode.Free;
    exit;
  end;
  // DelNode has both: Left and Right
  // Replace ANode with symmetric Successor
  Successor:=ANode.FindSuccessor;
  OldLeft:=ANode.Left;
  OldRight:=ANode.Right;
  OldSuccParent:=Successor.Parent;
  OldSuccLeft:=Successor.Left;
  OldSuccRight:=Successor.Right;
  ANode.Balance:=Successor.Balance;
  Successor.Balance:=OldBalance;
  if (OldSuccParent<>ANode) then begin
    // at least one node between ANode and Successor
    ANode.Parent:=Successor.Parent;
    if (OldSuccParent.Left=Successor) then
      OldSuccParent.Left:=ANode
    else
      OldSuccParent.Right:=ANode;
    Successor.Right:=OldRight;
    OldRight.Parent:=Successor;
  end else begin
    // Successor is right son of ANode
    ANode.Parent:=Successor;
    Successor.Right:=ANode;
  end;
  Successor.Left:=OldLeft;
  if OldLeft<>nil then
    OldLeft.Parent:=Successor;
  Successor.Parent:=OldParent;
  ANode.Left:=OldSuccLeft;
  if ANode.Left<>nil then
    ANode.Left.Parent:=ANode;
  ANode.Right:=OldSuccRight;
  if ANode.Right<>nil then
    ANode.Right.Parent:=ANode;
  if (OldParent<>nil) then begin
    if (OldParent.Left=ANode) then
      OldParent.Left:=Successor
    else
      OldParent.Right:=Successor;
  end else
    fRoot:=Successor;
  // delete Node as usual
  Delete(ANode);
end;

procedure TAvgLvlTree.Remove(Data: Pointer);
var ANode: TAvgLvlTreeNode;
begin
  ANode:=Find(Data);
  if ANode<>nil then
    Delete(ANode);
end;

procedure TAvgLvlTree.RemovePointer(Data: Pointer);
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=FindPointer(Data);
  if ANode<>nil then
    Delete(ANode);
end;

destructor TAvgLvlTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TAvgLvlTree.GetEnumerator: TAvgLvlTreeNodeEnumerator;
begin
  Result:=TAvgLvlTreeNodeEnumerator.Create(Self,true);
end;

function TAvgLvlTree.GetEnumeratorHighToLow: TAvgLvlTreeNodeEnumerator;
begin
  Result:=TAvgLvlTreeNodeEnumerator.Create(Self,false);
end;

function TAvgLvlTree.Find(Data: Pointer): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Compare(Data,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TAvgLvlTree.FindKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=OnCompareKeyWithData(Key,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TAvgLvlTree.FindNearestKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=OnCompareKeyWithData(Key,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAvgLvlTree.FindLeftMostKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
begin
  Result:=FindLeftMostSameKey(FindKey(Key,OnCompareKeyWithData));
end;

function TAvgLvlTree.FindRightMostKey(Key: Pointer;
  OnCompareKeyWithData: TListSortCompare): TAvgLvlTreeNode;
begin
  Result:=FindRightMostSameKey(FindKey(Key,OnCompareKeyWithData));
end;

function TAvgLvlTree.FindLeftMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
var
  LeftNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode<>nil then begin
    Data:=ANode.Data;
    Result:=ANode;
    repeat
      LeftNode:=Result.FindPrecessor;
      if (LeftNode=nil) or (Compare(Data,LeftNode.Data)<>0) then break;
      Result:=LeftNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAvgLvlTree.FindRightMostSameKey(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
var
  RightNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode<>nil then begin
    Data:=ANode.Data;
    Result:=ANode;
    repeat
      RightNode:=Result.FindSuccessor;
      if (RightNode=nil) or (Compare(Data,RightNode.Data)<>0) then break;
      Result:=RightNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAvgLvlTree.FindNearest(Data: Pointer): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Compare(Data,Result.Data);
    if Comp=0 then exit;
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAvgLvlTree.FindPointer(Data: Pointer): TAvgLvlTreeNode;
// same as Find, but not comparing for key, but same Data too
begin
  Result:=FindLeftMost(Data);
  while (Result<>nil) do begin
    if Result.Data=Data then break;
    Result:=Result.FindSuccessor;
    if Result=nil then exit(nil);
    if Compare(Data,Result.Data)<>0 then exit(nil);
  end;
end;

function TAvgLvlTree.FindLeftMost(Data: Pointer): TAvgLvlTreeNode;
var
  Left: TAvgLvlTreeNode;
begin
  Result:=Find(Data);
  while (Result<>nil) do begin
    Left:=Result.FindPrecessor;
    if (Left=nil) or (Compare(Data,Left.Data)<>0) then break;
    Result:=Left;
  end;
end;

function TAvgLvlTree.FindRightMost(Data: Pointer): TAvgLvlTreeNode;
var
  Right: TAvgLvlTreeNode;
begin
  Result:=Find(Data);
  while (Result<>nil) do begin
    Right:=Result.FindSuccessor;
    if (Right=nil) or (Compare(Data,Right.Data)<>0) then break;
    Result:=Right;
  end;
end;

function TAvgLvlTree.FindInsertPos(Data: Pointer): TAvgLvlTreeNode;
var Comp: integer;
begin
  Result:=fRoot;
  while (Result<>nil) do begin
    Comp:=Compare(Data,Result.Data);
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else
        exit;
    end;
  end;
end;

function TAvgLvlTree.FindSuccessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.FindSuccessor
  else
    Result:=nil;
end;

function TAvgLvlTree.FindPrecessor(ANode: TAvgLvlTreeNode): TAvgLvlTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.FindPrecessor
  else
    Result:=nil;
end;

procedure TAvgLvlTree.MoveDataLeftMost(var ANode: TAvgLvlTreeNode);
var LeftMost, PreNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode=nil then exit;
  LeftMost:=ANode;
  repeat
    PreNode:=LeftMost.FindPrecessor;
    if (PreNode=nil) or (Compare(ANode,PreNode)<>0) then break;
    LeftMost:=PreNode;
  until false;
  if LeftMost=ANode then exit;
  Data:=LeftMost.Data;
  LeftMost.Data:=ANode.Data;
  ANode.Data:=Data;
  ANode:=LeftMost;
end;

procedure TAvgLvlTree.MoveDataRightMost(var ANode: TAvgLvlTreeNode);
var RightMost, PostNode: TAvgLvlTreeNode;
  Data: Pointer;
begin
  if ANode=nil then exit;
  RightMost:=ANode;
  repeat
    PostNode:=RightMost.FindSuccessor;
    if (PostNode=nil) or (Compare(ANode,PostNode)<>0) then break;
    RightMost:=PostNode;
  until false;
  if RightMost=ANode then exit;
  Data:=RightMost.Data;
  RightMost.Data:=ANode.Data;
  ANode.Data:=Data;
  ANode:=RightMost;
end;

procedure TAvgLvlTree.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAvgLvlTree.ConsistencyCheck: '+Msg);
  end;

var
  RealCount: SizeInt;
begin
  RealCount:=0;
  if FRoot<>nil then begin
    FRoot.ConsistencyCheck(Self);
    RealCount:=FRoot.GetCount;
  end;
  if Count<>RealCount then
    E('Count<>RealCount');
end;

procedure TAvgLvlTree.FreeAndClear;

  procedure FreeNode(ANode: TAvgLvlTreeNode);
  begin
    if ANode=nil then exit;
    FreeNode(ANode.Left);
    FreeNode(ANode.Right);
    if ANode.Data<>nil then TObject(ANode.Data).Free;
    ANode.Data:=nil;
  end;

// TAvgLvlTree.FreeAndClear
begin
  // free all data
  FreeNode(fRoot);
  // free all nodes
  Clear;
end;

procedure TAvgLvlTree.FreeAndDelete(ANode: TAvgLvlTreeNode);
var OldData: TObject;
begin
  OldData:=TObject(ANode.Data);
  Delete(ANode);
  OldData.Free;
end;

procedure TAvgLvlTree.WriteReportToStream(s: TStream);

  procedure WriteStr(const Txt: string);
  begin
    if Txt='' then exit;
    s.Write(Txt[1],length(Txt));
  end;

  procedure WriteTreeNode(ANode: TAvgLvlTreeNode);
  var
    b: String;
    IsLeft: boolean;
    AParent: TAvgLvlTreeNode;
    WasLeft: Boolean;
  begin
    if ANode=nil then exit;
    WriteTreeNode(ANode.Right);
    AParent:=ANode;
    WasLeft:=false;
    b:='';
    while AParent<>nil do begin
      if AParent.Parent=nil then begin
        if AParent=ANode then
          b:='--'+b
        else
          b:='  '+b;
        break;
      end;
      IsLeft:=AParent.Parent.Left=AParent;
      if AParent=ANode then begin
        if IsLeft then
          b:='\-'
        else
          b:='/-';
      end else begin
        if WasLeft=IsLeft then
          b:='  '+b
        else
          b:='| '+b;
      end;
      WasLeft:=IsLeft;
      AParent:=AParent.Parent;
    end;
    b+=Format('%p      Self=%p  Parent=%p  Balance=%d'+LineEnding, [
      ANode.Data, Pointer(ANode),Pointer(ANode.Parent), ANode.Balance]);
    WriteStr(b);
    WriteTreeNode(ANode.Left);
  end;

// TAvgLvlTree.WriteReportToStream
begin
  WriteStr('-Start-of-AVL-Tree-------------------'+LineEnding);
  WriteTreeNode(fRoot);
  WriteStr('-End-Of-AVL-Tree---------------------'+LineEnding);
end;

function TAvgLvlTree.ReportAsString: string;
var ms: TMemoryStream;
begin
  Result:='';
  ms:=TMemoryStream.Create;
  try
    WriteReportToStream(ms);
    ms.Position:=0;
    SetLength(Result,ms.Size);
    if Result<>'' then
      ms.Read(Result[1],length(Result));
  finally
    ms.Free;
  end;
end;

procedure TAvgLvlTree.SetOnCompare(const AValue: TListSortCompare);
begin
  if AValue=nil then
    SetCompares(nil,FOnObjectCompare)
  else
    SetCompares(AValue,nil);
end;

procedure TAvgLvlTree.SetOnObjectCompare(const AValue: TObjectSortCompare);
begin
  if AValue=nil then
    SetCompares(FOnCompare,nil)
  else
    SetCompares(nil,AValue);
end;

procedure TAvgLvlTree.SetCompares(const NewCompare: TListSortCompare;
  const NewObjectCompare: TObjectSortCompare);
var List: PPointer;
  ANode: TAvgLvlTreeNode;
  i, OldCount: integer;
begin
  if (FOnCompare=NewCompare) and (FOnObjectCompare=NewObjectCompare) then exit;
  // sort the tree again
  if Count>0 then begin
    OldCount:=Count;
    GetMem(List,SizeOf(Pointer)*OldCount);
    try
      // save the data in a list
      ANode:=FindLowest;
      i:=0;
      while ANode<>nil do begin
        List[i]:=ANode.Data;
        inc(i);
        ANode:=ANode.FindSuccessor;
      end;
      // clear the tree
      Clear;
      // set the new compare function
      FOnCompare:=NewCompare;
      FOnObjectCompare:=NewObjectCompare;
      // re-add all nodes
      for i:=0 to OldCount-1 do
        Add(List[i]);
    finally
      FreeMem(List);
    end;
  end;
end;

procedure TAvgLvlTree.Init;
begin
  FNodeClass:=TAvgLvlTreeNode;
end;

function TAvgLvlTree.Compare(Data1, Data2: Pointer): integer;
begin
  if Assigned(FOnCompare) then
    Result:=FOnCompare(Data1,Data2)
  else
    Result:=FOnObjectCompare(Self,Data1,Data2);
end;


{ TAvgLvlTreeNode }

function TAvgLvlTreeNode.TreeDepth: integer;
// longest WAY down. e.g. only one node => 0 !
var LeftDepth, RightDepth: integer;
begin
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if LeftDepth>RightDepth then
    Result:=LeftDepth
  else
    Result:=RightDepth;
end;

procedure TAvgLvlTreeNode.ConsistencyCheck(Tree: TAvgLvlTree);

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAvgLvlTreeNode.ConsistencyCheck: '+Msg);
  end;

var
  LeftDepth: SizeInt;
  RightDepth: SizeInt;
begin
  // test left son
  if Left<>nil then begin
    if Left.Parent<>Self then
      E('Left.Parent<>Self');
    if Tree.Compare(Left.Data,Data)>0 then
      E('Compare(Left.Data,Data)>0');
    Left.ConsistencyCheck(Tree);
  end;
  // test right son
  if Right<>nil then begin
    if Right.Parent<>Self then
      E('Right.Parent<>Self');
    if Tree.Compare(Data,Right.Data)>0 then
      E('Compare(Data,Right.Data)>0');
    Right.ConsistencyCheck(Tree);
  end;
  // test balance
  if Left<>nil then
    LeftDepth:=Left.TreeDepth+1
  else
    LeftDepth:=0;
  if Right<>nil then
    RightDepth:=Right.TreeDepth+1
  else
    RightDepth:=0;
  if Balance<>(RightDepth-LeftDepth) then
    E('Balance['+IntToStr(Balance)+']<>(RightDepth['+IntToStr(RightDepth)+']-LeftDepth['+IntToStr(LeftDepth)+'])');
end;

function TAvgLvlTreeNode.GetCount: SizeInt;
begin
  Result:=1;
  if Left<>nil then inc(Result,Left.GetCount);
  if Right<>nil then inc(Result,Right.GetCount);
end;

function TAvgLvlTreeNode.FindSuccessor: TAvgLvlTreeNode;
begin
  Result:=Right;
  if Result<>nil then begin
    while (Result.Left<>nil) do Result:=Result.Left;
  end else begin
    Result:=Self;
    while (Result.Parent<>nil) and (Result.Parent.Right=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

function TAvgLvlTreeNode.FindPrecessor: TAvgLvlTreeNode;
begin
  Result:=Left;
  if Result<>nil then begin
    while (Result.Right<>nil) do Result:=Result.Right;
  end else begin
    Result:=Self;
    while (Result.Parent<>nil) and (Result.Parent.Left=Result) do
      Result:=Result.Parent;
    Result:=Result.Parent;
  end;
end;

{ TStringToStringTree }

function TStringToStringTree.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

function TStringToStringTree.GetValues(const Name: string): string;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Result:=PStringToStringItem(Node.Data)^.Value
  else
    Result:='';
end;

procedure TStringToStringTree.SetValues(const Name: string; const AValue: string);
var
  NewItem: PStringToStringItem;
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if (Node<>nil) then
    PStringToStringItem(Node.Data)^.Value:=AValue
  else begin
    New(NewItem);
    NewItem^.Name:=Name;
    NewItem^.Value:=AValue;
    FItems.Add(NewItem);
  end;
end;

function TStringToStringTree.FindNode(const Name: string): TAvgLvlTreeNode;
begin
   Result:=FItems.FindKey(Pointer(Name),FCompareNameWithItem);
end;

function TStringToStringTree.GetNode(Node: TAvgLvlTreeNode;
  out Name, Value: string): Boolean;
var
  Item: PStringToStringItem;
begin
  if Node<>nil then begin
    Item:=PStringToStringItem(Node.Data);
    Name:=Item^.Name;
    Value:=Item^.Value;
    Result:=true;
  end else begin
    Name:='';
    Value:='';
    Result:=false;
  end;
end;

constructor TStringToStringTree.Create(CaseSensitive: boolean);
begin
  if CaseSensitive then
    Create(@CompareStringToStringItems,@CompareAnsiStringWithStrToStrItem)
  else
    Create(@CompareStringToStringItemsI,@CompareAnsiStringWithStrToStrItemI);
end;

constructor TStringToStringTree.Create(const ACompareItems,
  ACompareNameWithItem: TListSortCompare);
begin
  FCompareItems:=ACompareItems;
  FCompareNameWithItem:=ACompareNameWithItem;
  FItems:=TAvgLvlTree.Create(FCompareItems);
end;

destructor TStringToStringTree.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TStringToStringTree.Clear;
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Dispose(Item);
    Node:=Node.FindSuccessor;
  end;
  FItems.Clear;
end;

procedure TStringToStringTree.Assign(Src: TStringToStringTree);
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  Clear;
  if Src=nil then exit;
  Node:=Src.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Values[Item^.Name]:=Item^.Value;
    Node:=Node.FindSuccessor;
  end;
end;

function TStringToStringTree.Contains(const Name: string): Boolean;
begin
  Result:=FindNode(Name)<>nil;
end;

procedure TStringToStringTree.Delete(const Name: string);
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
begin
  Node:=FindNode(Name);
  if Node=nil then exit;
  Item:=PStringToStringItem(Node.Data);
  FItems.Delete(Node);
  Dispose(Item);
end;

procedure TStringToStringTree.Add(const Name, Value, Delimiter: string);
var
  OldValue: string;
begin
  OldValue:=Values[Name];
  if OldValue<>'' then
    OldValue:=OldValue+Delimiter;
  OldValue:=OldValue+Value;
  Values[Name]:=OldValue;
end;

procedure TStringToStringTree.AddNameValues(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List.Names[i]]:=List.ValueFromIndex[i];
end;

procedure TStringToStringTree.AddValues(List: TStrings);
begin
  AddNames(List);
end;

procedure TStringToStringTree.AddNames(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List[i]]:='';
end;

function TStringToStringTree.GetFirst(out Name, Value: string): Boolean;
begin
  Result:=GetNode(Tree.FindLowest,Name,Value);
end;

function TStringToStringTree.GetLast(out Name, Value: string): Boolean;
begin
  Result:=GetNode(Tree.FindHighest,Name,Value);
end;

function TStringToStringTree.GetNext(const Name: string; out NextName,
  NextValue: string): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.FindSuccessor;
  Result:=GetNode(Node,NextName,NextValue);
end;

function TStringToStringTree.GetPrev(const Name: string; out PrevName,
  PrevValue: string): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.FindPrecessor;
  Result:=GetNode(Node,PrevName,PrevValue);
end;


{ TPointerToPointerTree }

function TPointerToPointerTree.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

function TPointerToPointerTree.GetValues(const Key: Pointer): Pointer;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Result:=PPointerToPointerItem(Node.Data)^.Value
  else
    Result:=nil;
end;

procedure TPointerToPointerTree.SetValues(const Key: Pointer;
  const AValue: Pointer);
var
  NewItem: PPointerToPointerItem;
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if (Node<>nil) then
    PPointerToPointerItem(Node.Data)^.Value:=AValue
  else begin
    New(NewItem);
    NewItem^.Key:=Key;
    NewItem^.Value:=AValue;
    FItems.Add(NewItem);
  end;
end;

function TPointerToPointerTree.FindNode(const Key: Pointer): TAvgLvlTreeNode;
begin
  Result:=FItems.FindKey(Key,@ComparePointerWithPtrToPtrItem)
end;

function TPointerToPointerTree.GetNode(Node: TAvgLvlTreeNode; out Key,
  Value: Pointer): Boolean;
var
  Item: PPointerToPointerItem;
begin
  if Node<>nil then begin
    Item:=PPointerToPointerItem(Node.Data);
    Key:=Item^.Key;
    Value:=Item^.Value;
    Result:=true;
  end else begin
    Key:=nil;
    Value:=nil;
    Result:=false;
  end;
end;

constructor TPointerToPointerTree.Create;
begin
  FItems:=TAvgLvlTree.Create(@ComparePointerToPointerItems);
end;

destructor TPointerToPointerTree.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TPointerToPointerTree.Clear;
var
  Node: TAvgLvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PPointerToPointerItem(Node.Data);
    Dispose(Item);
    Node:=Node.FindSuccessor;
  end;
  FItems.Clear;
end;

procedure TPointerToPointerTree.Remove(Key: Pointer);
var
  Node: TAvgLvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FindNode(Key);
  if Node=nil then exit;
  Item:=PPointerToPointerItem(Node.Data);
  FItems.Delete(Node);
  Dispose(Item);
end;

function TPointerToPointerTree.Contains(const Key: Pointer): Boolean;
begin
  Result:=FindNode(Key)<>nil;
end;

function TPointerToPointerTree.GetFirst(out Key, Value: Pointer): Boolean;
begin
  Result:=GetNode(Tree.FindLowest,Key,Value);
end;

function TPointerToPointerTree.GetLast(out Key, Value: Pointer): Boolean;
begin
  Result:=GetNode(Tree.FindHighest,Key,Value);
end;

function TPointerToPointerTree.GetNext(const Key: Pointer; out NextKey,
  NextValue: Pointer): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Node.FindSuccessor;
  Result:=GetNode(Node,NextKey,NextValue);
end;

function TPointerToPointerTree.GetPrev(const Key: Pointer; out PrevKey,
  PrevValue: Pointer): Boolean;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Node.FindPrecessor;
  Result:=GetNode(Node,PrevKey,PrevValue);
end;

{ TCustomStringMapEnumerator }

constructor TCustomStringMapEnumerator.Create(Tree: TAvgLvlTree);
begin
  FTree:=Tree;
end;

function TCustomStringMapEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FTree.FindLowest
  else
    FCurrent:=FCurrent.FindSuccessor;
  Result:=FCurrent<>nil;
end;

{ TCustomStringMap }

function TCustomStringMap.GetCompareItemsFunc: TListSortCompare;
begin
  Result:=Tree.OnCompare;
end;

function TCustomStringMap.FindNode(const s: string): TAvgLvlTreeNode;
begin
  Result:=FTree.FindKey(Pointer(s),FCompareKeyItemFunc);
end;

procedure TCustomStringMap.DisposeItem(p: PStringMapItem);
begin
  Dispose(p);
end;

function TCustomStringMap.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
begin
  Result:=p1^.Name=p2^.Name;
end;

function TCustomStringMap.CreateCopy(Src: PStringMapItem): PStringMapItem;
begin
  New(Result);
  Result^.Name:=Src^.Name;
end;

constructor TCustomStringMap.Create(TheCaseSensitive: boolean);
begin
  if TheCaseSensitive then
    Create(@CompareStringToStringItems,@CompareAnsiStringWithStrToStrItem,true)
  else
    Create(@CompareStringToStringItemsI,@CompareAnsiStringWithStrToStrItemI,false);
end;

constructor TCustomStringMap.Create(const ACompareItems,
  ACompareNameWithItem: TListSortCompare; TheCaseSensitive: boolean);
begin
  FCaseSensitive:=TheCaseSensitive;
  FCompareKeyItemFunc:=ACompareNameWithItem;
  FTree:=TAvgLvlTree.Create(ACompareItems);
end;

destructor TCustomStringMap.Destroy;
begin
  Clear;
  FTree.Free;
  FTree:=nil;
  inherited Destroy;
end;

procedure TCustomStringMap.Clear;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    DisposeItem(PStringMapItem(Node.Data));
    Node:=Node.FindSuccessor;
  end;
  FTree.Clear;
end;

function TCustomStringMap.Contains(const s: string): boolean;
begin
  Result:=FindNode(s)<>nil;
end;

procedure TCustomStringMap.GetNames(List: TStrings);
var
  Node: TAvgLvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringMapItem(Node.Data);
    List.Add(Item^.Name);
    Node:=Node.FindSuccessor;
  end;
end;

procedure TCustomStringMap.Remove(const Name: string);
var
  Node: TAvgLvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Item:=PStringMapItem(Node.Data);
    FTree.Delete(Node);
    Dispose(Item);
  end;
end;

function TCustomStringMap.Count: integer;
begin
  Result:=Tree.Count;
end;

function TCustomStringMap.Equals(OtherTree: TCustomStringMap): boolean;
var
  Node: TAvgLvlTreeNode;
  OtherNode: TAvgLvlTreeNode;
  OtherItem: PStringMapItem;
  Item: PStringMapItem;
begin
  Result:=false;
  if (OtherTree=nil) or (OtherTree.ClassType<>ClassType) then exit;
  if Tree.Count<>OtherTree.Tree.Count then exit;
  Node:=Tree.FindLowest;
  OtherNode:=OtherTree.Tree.FindLowest;
  while Node<>nil do begin
    if OtherNode=nil then exit;
    Item:=PStringMapItem(Node.Data);
    OtherItem:=PStringMapItem(OtherNode.Data);
    if not ItemsAreEqual(Item,OtherItem) then exit;
    OtherNode:=OtherNode.FindSuccessor;
    Node:=Node.FindSuccessor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TCustomStringMap.Assign(Source: TCustomStringMap);
var
  SrcNode: TAvgLvlTreeNode;
  SrcItem: PStringMapItem;
begin
  if (Source=nil) or (Source.ClassType<>ClassType) then
    raise Exception.Create('invalid class');
  Clear;
  SrcNode:=Source.Tree.FindLowest;
  while SrcNode<>nil do begin
    SrcItem:=PStringMapItem(SrcNode.Data);
    Tree.Add(CreateCopy(SrcItem));
    SrcNode:=SrcNode.FindSuccessor;
  end;
end;

procedure TCustomStringMap.SetCompareFuncs(const NewCompareItemsFunc,
  NewCompareKeyItemFunc: TListSortCompare; NewCaseSensitive: boolean);
begin
  FCompareKeyItemFunc:=NewCompareKeyItemFunc;
  Tree.OnCompare:=NewCompareItemsFunc;
  FCaseSensitive:=NewCaseSensitive;
end;

end.
