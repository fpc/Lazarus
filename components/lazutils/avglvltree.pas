{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Defines classes that use TAvlTree for data storage, and enumerators for them.
    TAvlTree is an Average Level binary Tree,
      located in unit AVL_Tree in FPC packages.
}
unit AvgLvlTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  LazFileUtils, LazDbgLog;

type

  TAvgLvlTree = class;

  TAvgLvlObjectSortCompare = function(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer of object;

  { TAvgLvlTree and TAvgLvlTreeNode for backwards compatibility.
    They used to be fully implemented here but now inherit from TAVLTreeNode and TAvlTree.
  }
  TAvgLvlTreeNode = TAVLTreeNode;
  TAvgLvlTreeNodeEnumerator = TAVLTreeNodeEnumerator;

  TAvgLvlTree = class(TAvlTree)
  private
    FOwnsObjects: boolean;
    function GetObjectCompare: TAvgLvlObjectSortCompare;
    procedure SetObjectCompare(AValue: TAvgLvlObjectSortCompare);
  public
    constructor CreateObjectCompare(const OnCompareMethod: TAvgLvlObjectSortCompare);
    procedure DisposeNode(aNode: TAVLTreeNode); override;
    procedure FreeAndDelete(ANode: TAVLTreeNode); override;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    property OnObjectCompare: TAvgLvlObjectSortCompare read GetObjectCompare write SetObjectCompare;
  end;
  TAvgLvlTreeClass = class of TAvgLvlTree;

  { TIndexedAVLTreeNode }

  TIndexedAVLTreeNode = class(TAvlTreeNode)
  public
    LeftCount: SizeInt; // number of nodes in the Left side
  end;

  { TIndexedAVLTree }

  TIndexedAVLTree = class(TAvgLvlTree)
  private
    function GetItems(Index: SizeInt): Pointer; inline;
  protected
    fLastIndex: SizeInt;
    fLastNode: TIndexedAVLTreeNode;
    procedure DeletingNode(aNode: TAvlTreeNode); override;
    procedure Init; override;
    procedure NodeAdded(aNode: TAvlTreeNode); override;
    procedure RotateLeft(aNode: TAvlTreeNode); override;
    procedure RotateRight(aNode: TAvlTreeNode); override;
    procedure SwitchPositionWithSuccessor(aNode, aSuccessor: TAvlTreeNode); override;
  public
    function GetNodeAtIndex(Index: integer): TIndexedAVLTreeNode;
    function NodeToIndex(Node: TAvlTreeNode): SizeInt;
    function IndexOf(Data: Pointer): SizeInt;
    property Items[Index: SizeInt]: Pointer read GetItems; default;
    procedure ConsistencyCheck; override;
    function NodeToReportStr(aNode: TAvlTreeNode): string; override;
  end;

  { TPointerToPointerTree - Associative array }

  TPointerToPointerItem = record
    Key: Pointer;
    Value: Pointer;
  end;
  PPointerToPointerItem = ^TPointerToPointerItem;

  { TPointerToPointerEnumerator }

  TPointerToPointerEnumerator = class
  protected
    FHighToLow: boolean;
    FTree: TAvlTree;
    FCurrent: TAvlTreeNode;
    function GetCurrent: PPointerToPointerItem; inline;
  public
    constructor Create(Tree: TAvlTree);
    function GetEnumerator: TPointerToPointerEnumerator; inline;
    function MoveNext: Boolean;
    property Current: PPointerToPointerItem read GetCurrent;
    property HighToLow: boolean read FHighToLow;
  end;

  TPointerToPointerTree = class
  private
    FItems: TAvlTree;
    function GetCount: SizeInt; inline;
    function GetValues(const Key: Pointer): Pointer;
    procedure SetValues(const Key: Pointer; const AValue: Pointer);
    function FindNode(const Key: Pointer): TAvlTreeNode;
    function GetNode(Node: TAvlTreeNode; out Key, Value: Pointer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearWithFree; // free Values with TObject(Value).Free
    function Equals(Obj: TObject): boolean; override;
    function IsEqual(aTree: TPointerToPointerTree): boolean;
    procedure Assign(aTree: TPointerToPointerTree);
    procedure Remove(Key: Pointer);
    function Contains(const Key: Pointer): Boolean; inline;
    function GetFirst(out Key, Value: Pointer): Boolean;
    function GetLast(out Key, Value: Pointer): Boolean;
    function GetNext(const Key: Pointer; out NextKey, NextValue: Pointer): Boolean;
    function GetPrev(const Key: Pointer; out PrevKey, PrevValue: Pointer): Boolean;
    property Count: SizeInt read GetCount;
    property Values[const Key: Pointer]: Pointer read GetValues write SetValues; default;
    property Tree: TAvlTree read FItems; // tree of PPointerToPointerItem

    // enumerators
    function GetEnumerator: TPointerToPointerEnumerator;
    function GetEnumeratorHighToLow: TPointerToPointerEnumerator;
  end;

  TStringMapItem = record
    Name: string;
  end;
  PStringMapItem = ^TStringMapItem;

  { TCustomStringMapEnumerator }

  TCustomStringMapEnumerator = class
  protected
    FTree: TAvlTree;
    FCurrent: TAvlTreeNode;
  public
    constructor Create(Tree: TAvlTree);
    function MoveNext: boolean;
    // "Current" is implemented by the descendant classes
  end;

  { TCustomStringMap }

  TCustomStringMap = class
  private
    FCompareKeyItemFunc: TListSortCompare;
    FTree: TAvlTree;// tree of PStringMapItem
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
    function Contains(const s: string): boolean; inline;
    procedure GetNames(List: TStrings);
    procedure Remove(const Name: string); virtual;
    property CaseSensitive: boolean read FCaseSensitive;
    property Tree: TAvlTree read FTree; // tree of PStringMapItem
    function FindNode(const s: string): TAvlTreeNode;
    function Count: SizeInt; inline;
    function Equals(OtherTree: TCustomStringMap): boolean; reintroduce;
    procedure Assign(Source: TCustomStringMap); virtual;
    function CalcMemSize: PtrUint; virtual;
    property CompareItemsFunc: TListSortCompare read GetCompareItemsFunc;
    property CompareKeyItemFunc: TListSortCompare read FCompareKeyItemFunc;
    procedure SetCompareFuncs(const NewCompareItemsFunc,
                                    NewCompareKeyItemFunc: TListSortCompare
                             {; NewCaseSensitive: boolean});
  end;

  { TStringMapEnumerator }

  TStringMapEnumerator = class(TCustomStringMapEnumerator)
  private
    function GetCurrent: string; inline;
  public
    property Current: string read GetCurrent;
  end;

  { TStringMap - associative array string to boolean }

  TStringMap = class(TCustomStringMap)
  private
    function GetValues(const s: string): boolean;
    procedure SetValues(const s: string; AValue: boolean);
  public
    procedure Add(const Name: string);
    function GetEnumerator: TStringMapEnumerator;
    property Values[const s: string]: boolean read GetValues write SetValues; default;
  end;

  { TOldStringToStringTree - Associative array }

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

  { TStringToStringTree }

  TStringToStringTree = class(TCustomStringMap)
  private
    function GetValues(const s: string): string;
    procedure SetValues(const s: string; const AValue: string);
  protected
    procedure DisposeItem(p: PStringMapItem); override;
    function ItemsAreEqual(p1, p2: PStringMapItem): boolean; override;
    function CreateCopy(Src: PStringMapItem): PStringMapItem; override;
    function GetNode(Node: TAvlTreeNode; out Name, Value: string): Boolean;
  public
    function GetString(const Name: string; out Value: string): boolean;
    procedure Add(const Name, Value: string); inline;
    procedure Add(const Name, Value, Delimiter: string);
    procedure AddNameValues(List: TStrings);
    procedure AddNames(List: TStrings);
    property Values[const s: string]: string read GetValues write SetValues; default;
    function GetNodeData(Node: TAVLTreeNode): PStringToStringItem; inline;
    function AsText: string;
    procedure Assign(Source: TCustomStringMap); override;
    function CalcMemSize: PtrUint; override;
    function GetEnumerator: TStringToStringTreeEnumerator;
    function GetFirst(out Name, Value: string): Boolean;
    function GetLast(out Name, Value: string): Boolean;
    function GetNext(const Name: string; out NextName, NextValue: string): Boolean;
    function GetPrev(const Name: string; out PrevName, PrevValue: string): Boolean;
  end;

  TStringToPointerTreeItem = record
    Name: string;
    Value: Pointer;
  end;
  PStringToPointerTreeItem = ^TStringToPointerTreeItem;

  { TStringToPointerTreeEnumerator }

  TStringToPointerTreeEnumerator = class(TStringMapEnumerator)
  private
    function GetCurrent: PStringToPointerTreeItem;
  public
    property Current: PStringToPointerTreeItem read GetCurrent;
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
    function GetNodeData(Node: TAVLTreeNode): PStringToPointerTreeItem; inline;
    function GetEnumerator: TStringToPointerTreeEnumerator;
    property FreeValues: boolean read FFreeValues write FFreeValues;
    property Values[const s: string]: Pointer read GetValues write SetValues; default;
  end;

  { TFilenameToStringTree }

  TFilenameToStringTree = class(TStringToStringTree)
  public
    constructor Create(CaseInsensitive: boolean); // false = system default
  end;

  { TFilenameToPointerTree }

  TFilenameToPointerTree = class(TStringToPointerTree)
  public
    constructor Create(CaseInsensitive: boolean); // false = system default
  end;


function ComparePointer(Data1, Data2: Pointer): integer;
function ComparePointerToPointerItems(Data1, Data2: Pointer): integer;
function ComparePointerWithPtrToPtrItem(Key, Data: Pointer): Integer;

function CompareStringToStringItems(Data1, Data2: Pointer): integer;
function CompareAnsiStringWithStrToStrItem(Key, Data: Pointer): Integer;
function CompareStringToStringItemsI(Data1, Data2: Pointer): integer;
function CompareAnsiStringWithStrToStrItemI(Key, Data: Pointer): Integer;

function CompareFilenameToStringItems(Data1, Data2: Pointer): integer;
function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer): integer;
function CompareFilenameToStringItemsI(Data1, Data2: Pointer): integer;
function CompareFilenameAndFilenameToStringTreeItemI(Key, Data: Pointer): integer;


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

function CompareFilenameToStringItems(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(PStringToStringItem(Data1)^.Name,
                           PStringToStringItem(Data2)^.Name);
end;

function CompareFilenameAndFilenameToStringTreeItem(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenames(String(Key),PStringToStringItem(Data)^.Name);
end;

function CompareFilenameToStringItemsI(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenamesIgnoreCase(PStringToStringItem(Data1)^.Name,
                                     PStringToStringItem(Data2)^.Name);
end;

function CompareFilenameAndFilenameToStringTreeItemI(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenamesIgnoreCase(String(Key),
                                     PStringToStringItem(Data)^.Name);
end;

{ TAvgLvlTree }

constructor TAvgLvlTree.CreateObjectCompare(const OnCompareMethod: TAvgLvlObjectSortCompare);
begin
  inherited CreateObjectCompare(TObjectSortCompare(OnCompareMethod));
end;

procedure TAvgLvlTree.SetObjectCompare(AValue: TAvgLvlObjectSortCompare);
begin
  inherited SetOnObjectCompare(TObjectSortCompare(AValue));
end;

function TAvgLvlTree.GetObjectCompare: TAvgLvlObjectSortCompare;
begin
  Result := TAvgLvlObjectSortCompare(FOnObjectCompare);
end;

procedure TAvgLvlTree.DisposeNode(aNode: TAVLTreeNode);
begin
  if FOwnsObjects and Assigned(aNode) then
  begin
    TObject(aNode.Data).Free;
    aNode.Data := nil;
  end;
  inherited DisposeNode(aNode);
end;

procedure TAvgLvlTree.FreeAndDelete(ANode: TAVLTreeNode);
begin
  if FOwnsObjects then
    Delete(ANode)
  else
    inherited FreeAndDelete(aNode);
end;

{ TPointerToPointerEnumerator }

function TPointerToPointerEnumerator.GetCurrent: PPointerToPointerItem;
begin
  Result:=PPointerToPointerItem(FCurrent.Data);
end;

constructor TPointerToPointerEnumerator.Create(Tree: TAvlTree);
begin
  FTree:=Tree;
end;

function TPointerToPointerEnumerator.GetEnumerator: TPointerToPointerEnumerator;
begin
  Result:=Self;
end;

function TPointerToPointerEnumerator.MoveNext: Boolean;
begin
  if FHighToLow then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Precessor
    else
      FCurrent:=FTree.FindHighest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Successor
    else
      FCurrent:=FTree.FindLowest;
  end;
  Result:=FCurrent<>nil;
end;

{ TStringToPointerTree }

function TStringToPointerTree.GetValues(const s: string): Pointer;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToPointerTreeItem(Node.Data)^.Value
  else
    Result:=nil
end;

procedure TStringToPointerTree.SetValues(const s: string; const AValue: Pointer);
var
  Node: TAvlTreeNode;
  Item: PStringToPointerTreeItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    Item:=PStringToPointerTreeItem(Node.Data);
    if Item^.Value=AValue then exit;
    if FreeValues then
       TObject(Item^.Value).Free;
    Item^.Value:=AValue;
  end else begin
    New(Item);
    Item^.Name:=s;
    Item^.Value:=AValue;
    FTree.Add(Item);
  end;
end;

procedure TStringToPointerTree.DisposeItem(p: PStringMapItem);
var
  Item: PStringToPointerTreeItem absolute p;
begin
  if FreeValues then
    TObject(Item^.Value).Free;
  Dispose(Item);
end;

function TStringToPointerTree.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
var
  Item1: PStringToPointerTreeItem absolute p1;
  Item2: PStringToPointerTreeItem absolute p2;
begin
  Result:=(Item1^.Name=Item2^.Name)
      and (Item1^.Value=Item2^.Value);
end;

function TStringToPointerTree.CreateCopy(Src: PStringMapItem): PStringMapItem;
var
  SrcItem: PStringToPointerTreeItem absolute Src;
  NewItem: PStringToPointerTreeItem;
begin
  New(NewItem);
  NewItem^.Name:=SrcItem^.Name;
  NewItem^.Value:=SrcItem^.Value;
  Result:=PStringMapItem(NewItem);
end;

function TStringToPointerTree.GetData(const Name: string; out Value: Pointer): boolean;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToPointerTreeItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TStringToPointerTree.GetNodeData(Node: TAVLTreeNode): PStringToPointerTreeItem;
begin
  Result:=PStringToPointerTreeItem(Node.Data);
end;

function TStringToPointerTree.GetEnumerator: TStringToPointerTreeEnumerator;
begin
  Result:=TStringToPointerTreeEnumerator.Create(FTree);
end;

{ TFilenameToStringTree }

constructor TFilenameToStringTree.Create(CaseInsensitive: boolean);
begin
  inherited Create(true);
  if CaseInsensitive then
    SetCompareFuncs(@CompareFilenameToStringItemsI,
                    @CompareFilenameAndFilenameToStringTreeItemI)
  else
    SetCompareFuncs(@CompareFilenameToStringItems,
                    @CompareFilenameAndFilenameToStringTreeItem);
end;

{ TFilenameToPointerTree }

constructor TFilenameToPointerTree.Create(CaseInsensitive: boolean);
begin
  inherited Create(true);
  if CaseInsensitive then
    SetCompareFuncs(@CompareFilenameToStringItemsI,
                    @CompareFilenameAndFilenameToStringTreeItemI)
  else
    SetCompareFuncs(@CompareFilenameToStringItems,
                    @CompareFilenameAndFilenameToStringTreeItem);
end;

{ TStringToStringTree }

function TStringToStringTree.GetValues(const s: string): string;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(s);
  if Node<>nil then
    Result:=PStringToStringItem(Node.Data)^.Value
  else
    Result:=''
end;

procedure TStringToStringTree.SetValues(const s: string; const AValue: string);
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
begin
  Node:=FindNode(s);
  if Node<>nil then begin
    Item:=PStringToStringItem(Node.Data);
    Item^.Name:=s; // update case
    Item^.Value:=AValue;
  end else begin
    New(Item);
    Item^.Name:=s;
    Item^.Value:=AValue;
    FTree.Add(Item);
  end;
end;

procedure TStringToStringTree.DisposeItem(p: PStringMapItem);
var
  Item: PStringToStringItem absolute p;
begin
  Dispose(Item);
end;

function TStringToStringTree.ItemsAreEqual(p1, p2: PStringMapItem): boolean;
var
  Item1: PStringToStringItem absolute p1;
  Item2: PStringToStringItem absolute p2;
begin
  Result:=(Item1^.Name=Item2^.Name)
      and (Item1^.Value=Item2^.Value);
end;

function TStringToStringTree.CreateCopy(Src: PStringMapItem): PStringMapItem;
var
  SrcItem: PStringToStringItem absolute Src;
  NewItem: PStringToStringItem;
begin
  New(NewItem);
  NewItem^.Name:=SrcItem^.Name;
  NewItem^.Value:=SrcItem^.Value;
  Result:=PStringMapItem(NewItem);
end;

function TStringToStringTree.GetNode(Node: TAvlTreeNode; out Name, Value: string
  ): Boolean;
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

function TStringToStringTree.GetString(const Name: string; out Value: string): boolean;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Value:=PStringToStringItem(Node.Data)^.Value;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TStringToStringTree.Add(const Name, Value: string);
begin
  Values[Name]:=Value;
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

procedure TStringToStringTree.AddNames(List: TStrings);
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do
    Values[List[i]]:='';
end;

function TStringToStringTree.GetNodeData(Node: TAVLTreeNode): PStringToStringItem;
begin
  Result:=PStringToStringItem(Node.Data);
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
  Node: TAvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.Successor;
  Result:=GetNode(Node,NextName,NextValue);
end;

function TStringToStringTree.GetPrev(const Name: string; out PrevName,
  PrevValue: string): Boolean;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(Name);
  if Node<>nil then
    Node:=Node.Precessor;
  Result:=GetNode(Node,PrevName,PrevValue);
end;

function TStringToStringTree.AsText: string;
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
begin
  Result:='';
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Result:=Result+Item^.Name+'='+Item^.Value+LineEnding;
    Node:=Node.Successor;
  end;
end;

procedure TStringToStringTree.Assign(Source: TCustomStringMap);
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
begin
  if (Source=nil) or (Source.ClassType<>ClassType) then
    raise Exception.Create('invalid class');
  Clear;
  Node:=Source.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Values[Item^.Name]:=Item^.Value;
    Node:=Node.Successor;
  end;
end;

function TStringToStringTree.CalcMemSize: PtrUint;
var
  Node: TAvlTreeNode;
  Item: PStringToStringItem;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(FTree.InstanceSize)
    +PtrUint(FTree.Count)*SizeOf(TAvlTreeNode);
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    inc(Result,MemSizeString(Item^.Name)
       +MemSizeString(Item^.Value)
       +SizeOf(TStringToStringItem));
    Node:=FTree.FindSuccessor(Node);
  end;
end;

function TStringToStringTree.GetEnumerator: TStringToStringTreeEnumerator;
begin
  Result:=TStringToStringTreeEnumerator.Create(FTree);
end;

{ TStringToPointerTreeEnumerator }

function TStringToPointerTreeEnumerator.GetCurrent: PStringToPointerTreeItem;
begin
  Result:=PStringToPointerTreeItem(FCurrent.Data);
end;

{ TStringMapEnumerator }

function TStringMapEnumerator.GetCurrent: string;
begin
  Result:=PStringMapItem(FCurrent.Data)^.Name;
end;

{ TStringMap }

function TStringMap.GetValues(const s: string): boolean;
begin
  Result:=Contains(s);
end;

procedure TStringMap.SetValues(const s: string; AValue: boolean);
begin
  if AValue then
    Add(s)
  else
    Remove(s);
end;

procedure TStringMap.Add(const Name: string);
var
  Node: TAvlTreeNode;
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

{ TIndexedAVLTree }

function TIndexedAVLTree.GetItems(Index: SizeInt): Pointer;
begin
  Result:=GetNodeAtIndex(Index).Data;
end;

procedure TIndexedAVLTree.DeletingNode(aNode: TAvlTreeNode);
var
  aParent: TAvlTreeNode;
begin
  fLastNode:=nil;
  repeat
    aParent:=aNode.Parent;
    if (aParent=nil) then exit;
    if aParent.Left=aNode then
      TIndexedAVLTreeNode(aParent).LeftCount-=1;
    aNode:=aParent;
  until false;
end;

procedure TIndexedAVLTree.Init;
begin
  NodeClass:=TIndexedAVLTreeNode;
end;

procedure TIndexedAVLTree.NodeAdded(aNode: TAvlTreeNode);
var
  aParent: TAvlTreeNode;
begin
  fLastNode:=nil;
  repeat
    aParent:=aNode.Parent;
    if (aParent=nil) then exit;
    if aParent.Left=aNode then
      TIndexedAVLTreeNode(aParent).LeftCount+=1;
    aNode:=aParent;
  until false;
end;

procedure TIndexedAVLTree.RotateLeft(aNode: TAvlTreeNode);
{    Parent                Parent
       |                     |
    CurNode        =>     OldRight
      /  \                  /
   Left OldRight         CurNode
          /               /  \
     OldRightLeft      Left OldRightLeft  }
var
  CurNode: TIndexedAVLTreeNode absolute aNode;
  OldRight: TIndexedAVLTreeNode;
begin
  OldRight:=TIndexedAVLTreeNode(aNode.Right);
  inherited RotateLeft(aNode);
  OldRight.LeftCount += 1+CurNode.LeftCount;
end;

procedure TIndexedAVLTree.RotateRight(aNode: TAvlTreeNode);
{       Parent              Parent
          |                   |
        CurNode        =>   OldLeft
         /   \                 \
    OldLeft  Right          CurNode
        \                     /  \
   OldLeftRight      OldLeftRight Right  }
var
  CurNode: TIndexedAVLTreeNode absolute aNode;
  OldLeft: TIndexedAVLTreeNode;
begin
  OldLeft:=TIndexedAVLTreeNode(aNode.Left);
  inherited RotateRight(aNode);
  CurNode.LeftCount -= (1 + OldLeft.LeftCount);
end;

procedure TIndexedAVLTree.SwitchPositionWithSuccessor(aNode,
  aSuccessor: TAvlTreeNode);
var
  CurNode: TIndexedAVLTreeNode absolute aNode;
  CurSucc: TIndexedAVLTreeNode absolute aSuccessor;
  h: SizeInt;
begin
  h:=CurNode.LeftCount;
  CurNode.LeftCount:=CurSucc.LeftCount;
  CurSucc.LeftCount:=h;
  inherited SwitchPositionWithSuccessor(aNode, aSuccessor);
end;

function TIndexedAVLTree.GetNodeAtIndex(Index: integer): TIndexedAVLTreeNode;

  procedure RaiseOutOfBounds;
  begin
    raise Exception.Create('TIndexedAVLTree: Index '+IntToStr(Index)+' out of bounds 0..'+IntToStr(Count));
  end;

begin
  if (Index<0) or (Index>=Count) then
    RaiseOutOfBounds;

  if fLastNode<>nil then begin
    if Index=fLastIndex then
      exit(fLastNode)
    else if Index=fLastIndex+1 then begin
      fLastIndex:=Index;
      fLastNode:=TIndexedAVLTreeNode(fLastNode.Successor);
      exit(fLastNode);
    end else if Index=fLastIndex-1 then begin
      fLastIndex:=Index;
      fLastNode:=TIndexedAVLTreeNode(fLastNode.Precessor);
      exit(fLastNode);
    end;
  end;

  fLastIndex:=Index;
  Result:=TIndexedAVLTreeNode(Root);
  repeat
    if Result.LeftCount>Index then
      Result:=TIndexedAVLTreeNode(Result.Left)
    else if Result.LeftCount=Index then begin
      fLastNode:=TIndexedAVLTreeNode(Result);
      exit;
    end
    else begin
      Index -= Result.LeftCount+1;
      Result:=TIndexedAVLTreeNode(Result.Right);
    end;
  until false;
end;

function TIndexedAVLTree.NodeToIndex(Node: TAvlTreeNode): SizeInt;
var
  CurNode: TIndexedAVLTreeNode;
  CurParent: TIndexedAVLTreeNode;
begin
  if Node=nil then exit(-1);

  if fLastNode=Node then
    exit(fLastIndex);

  CurNode:=TIndexedAVLTreeNode(Node);
  Result:=CurNode.LeftCount;
  repeat
    CurParent:=TIndexedAVLTreeNode(CurNode.Parent);
    if CurParent=nil then break;
    if CurParent.Right=CurNode then
      inc(Result,CurParent.LeftCount+1);
    CurNode:=CurParent;
  until false;

  fLastNode:=TIndexedAVLTreeNode(Node);
  fLastIndex:=Result;
end;

function TIndexedAVLTree.IndexOf(Data: Pointer): SizeInt;
var
  Node: TAvlTreeNode;
begin
  Node:=FindPointer(Data);
  if Node=nil then exit(-1);
  Result:=NodeToIndex(Node);
end;

procedure TIndexedAVLTree.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TIndexedAVLTree.ConsistencyCheck: '+Msg);
  end;

var
  Node: TAvlTreeNode;
  i: SizeInt;
  LeftCount: SizeInt;
begin
  inherited ConsistencyCheck;
  i:=0;
  for Node in Self do begin
    if Node.Left<>nil then
      LeftCount:=Node.Left.GetCount
    else
      LeftCount:=0;
    if TIndexedAVLTreeNode(Node).LeftCount<>LeftCount then
      E(Format('Node.LeftCount=%d<>%d',[TIndexedAVLTreeNode(Node).LeftCount,LeftCount]));

    if GetNodeAtIndex(i)<>Node then
      E(Format('GetNodeAtIndex(%d)<>%P',[i,Node]));
    fLastNode:=nil;
    if GetNodeAtIndex(i)<>Node then
      E(Format('GetNodeAtIndex(%d)<>%P',[i,Node]));

    if NodeToIndex(Node)<>i then
      E(Format('NodeToIndex(%P)<>%d',[Node,i]));
    fLastNode:=nil;
    if NodeToIndex(Node)<>i then
      E(Format('NodeToIndex(%P)<>%d',[Node,i]));

    inc(i);
  end;
end;

function TIndexedAVLTree.NodeToReportStr(aNode: TAvlTreeNode): string;
begin
  Result:=inherited NodeToReportStr(aNode)+' LeftCount='+IntToStr(TIndexedAVLTreeNode(aNode).LeftCount);
end;

{ TPointerToPointerTree }

function TPointerToPointerTree.GetCount: SizeInt;
begin
  Result:=FItems.Count;
end;

function TPointerToPointerTree.GetValues(const Key: Pointer): Pointer;
var
  Node: TAvlTreeNode;
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
  Node: TAvlTreeNode;
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

function TPointerToPointerTree.FindNode(const Key: Pointer): TAvlTreeNode;
begin
  Result:=FItems.FindKey(Key,@ComparePointerWithPtrToPtrItem)
end;

function TPointerToPointerTree.GetNode(Node: TAvlTreeNode; out Key,
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
  FItems:=TAvlTree.Create(@ComparePointerToPointerItems);
end;

destructor TPointerToPointerTree.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TPointerToPointerTree.Clear;
var
  Node: TAvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PPointerToPointerItem(Node.Data);
    Dispose(Item);
    Node:=Node.Successor;
  end;
  FItems.Clear;
end;

procedure TPointerToPointerTree.ClearWithFree;
var
  Node: TAvlTreeNode;
  Item: PPointerToPointerItem;
begin
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    Item:=PPointerToPointerItem(Node.Data);
    TObject(Item^.Value).Free;
    Dispose(Item);
    Node:=Node.Successor;
  end;
  FItems.Clear;
end;

function TPointerToPointerTree.Equals(Obj: TObject): boolean;
begin
  if Obj is TPointerToPointerTree then
    Result:=IsEqual(TPointerToPointerTree(Obj))
  else
    Result:=inherited Equals(Obj);
end;

function TPointerToPointerTree.IsEqual(aTree: TPointerToPointerTree): boolean;
var
  MyNode: TAvlTreeNode;
  OtherNode: TAvlTreeNode;
  MyItem: PPointerToPointerItem;
  OtherItem: PPointerToPointerItem;
begin
  if aTree=Self then exit(true);
  Result:=false;
  if aTree=nil then exit;
  if Count<>aTree.Count then exit;
  if FItems.OnCompare<>aTree.FItems.OnCompare then exit;
  if FItems.OnObjectCompare<>aTree.FItems.OnObjectCompare then exit;
  if FItems.NodeClass<>aTree.FItems.NodeClass then exit;
  MyNode:=FItems.FindLowest;
  OtherNode:=aTree.FItems.FindLowest;
  while MyNode<>nil do begin
    if OtherNode=nil then exit;
    MyItem:=PPointerToPointerItem(MyNode.Data);
    OtherItem:=PPointerToPointerItem(OtherNode.Data);
    if (MyItem^.Key<>OtherItem^.Key)
    or (MyItem^.Value<>OtherItem^.Value) then exit;
    MyNode:=MyNode.Successor;
    OtherNode:=OtherNode.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TPointerToPointerTree.Assign(aTree: TPointerToPointerTree);
var
  Node: TAvlTreeNode;
  SrcItem, MyItem: PPointerToPointerItem;
begin
  if aTree=nil then
    raise Exception.Create('TPointerToPointerTree.Assign aTree=nil');
  if IsEqual(aTree) then exit;
  // clear and clone node structure, copying Data references
  FItems.Assign(aTree.FItems);
  // clone Data
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    SrcItem:=PPointerToPointerItem(Node.Data);
    New(MyItem);
    MyItem^:=SrcItem^;
    Node.Data:=MyItem;
    Node:=Node.Successor;
  end;
end;

procedure TPointerToPointerTree.Remove(Key: Pointer);
var
  Node: TAvlTreeNode;
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
  Node: TAvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Node.Successor;
  Result:=GetNode(Node,NextKey,NextValue);
end;

function TPointerToPointerTree.GetPrev(const Key: Pointer; out PrevKey,
  PrevValue: Pointer): Boolean;
var
  Node: TAvlTreeNode;
begin
  Node:=FindNode(Key);
  if Node<>nil then
    Node:=Node.Precessor;
  Result:=GetNode(Node,PrevKey,PrevValue);
end;

function TPointerToPointerTree.GetEnumerator: TPointerToPointerEnumerator;
begin
  Result:=TPointerToPointerEnumerator.Create(Tree);
end;

function TPointerToPointerTree.
  GetEnumeratorHighToLow: TPointerToPointerEnumerator;
begin
  Result:=TPointerToPointerEnumerator.Create(Tree);
  Result.fHighToLow:=true;
end;

{ TCustomStringMapEnumerator }

constructor TCustomStringMapEnumerator.Create(Tree: TAvlTree);
begin
  FTree:=Tree;
end;

function TCustomStringMapEnumerator.MoveNext: boolean;
begin
  if FCurrent=nil then
    FCurrent:=FTree.FindLowest
  else
    FCurrent:=FCurrent.Successor;
  Result:=FCurrent<>nil;
end;

{ TCustomStringMap }

function TCustomStringMap.GetCompareItemsFunc: TListSortCompare;
begin
  Result:=Tree.OnCompare;
end;

function TCustomStringMap.FindNode(const s: string): TAvlTreeNode;
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
  FTree:=TAvlTree.Create(ACompareItems);
  FTree.SetNodeManager(nil);
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
  Node: TAvlTreeNode;
begin
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    DisposeItem(PStringMapItem(Node.Data));
    Node:=Node.Successor;
  end;
  FTree.Clear;
end;

function TCustomStringMap.Contains(const s: string): boolean;
begin
  Result:=FindNode(s)<>nil;
end;

procedure TCustomStringMap.GetNames(List: TStrings);
var
  Node: TAvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringMapItem(Node.Data);
    List.Add(Item^.Name);
    Node:=Node.Successor;
  end;
end;

procedure TCustomStringMap.Remove(const Name: string);
var
  Node: TAvlTreeNode;
  Item: PStringMapItem;
begin
  Node:=FindNode(Name);
  if Node<>nil then begin
    Item:=PStringMapItem(Node.Data);
    FTree.Delete(Node);
    Dispose(Item);
  end;
end;

function TCustomStringMap.Count: SizeInt;
begin
  Result:=Tree.Count;
end;

function TCustomStringMap.Equals(OtherTree: TCustomStringMap): boolean;
var
  Node: TAvlTreeNode;
  OtherNode: TAvlTreeNode;
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
    OtherNode:=OtherNode.Successor;
    Node:=Node.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TCustomStringMap.Assign(Source: TCustomStringMap);
var
  SrcNode: TAvlTreeNode;
  SrcItem: PStringMapItem;
begin
  if (Source=nil) or (Source.ClassType<>ClassType) then
    raise Exception.Create('invalid class');
  Clear;
  SrcNode:=Source.Tree.FindLowest;
  while SrcNode<>nil do begin
    SrcItem:=PStringMapItem(SrcNode.Data);
    Tree.Add(CreateCopy(SrcItem));
    SrcNode:=SrcNode.Successor;
  end;
end;

function TCustomStringMap.CalcMemSize: PtrUint;
var
  Node: TAvlTreeNode;
  Item: PStringMapItem;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUInt(FTree.InstanceSize)
    +PtrUint(FTree.Count)*SizeOf(TAvlTreeNode);
  Node:=FTree.FindLowest;
  while Node<>nil do begin
    Item:=PStringMapItem(Node.Data);
    inc(Result,MemSizeString(Item^.Name)
       +SizeOf(TStringMapItem));
    Node:=FTree.FindSuccessor(Node);
  end;
end;

procedure TCustomStringMap.SetCompareFuncs(const NewCompareItemsFunc,
  NewCompareKeyItemFunc: TListSortCompare {; NewCaseSensitive: boolean});
begin
  FCompareKeyItemFunc:=NewCompareKeyItemFunc;
  Tree.OnCompare:=NewCompareItemsFunc;
  //FCaseSensitive:=NewCaseSensitive;
end;

end.
