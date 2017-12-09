{ **********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2017 by Mattias Gaertner
    
    Average Level Tree implementation by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Author: Mattias Gaertner

  Abstract:
    TAVLTree is an Average Level binary Tree. This binary tree is always
    balanced, so that inserting, deleting and finding a node is performed in
    O(log(#Nodes)).

  Note! This is a copy of avl_tree unit from FPC 3.1.1 from 6th Apr 2017.
        Can be removed when FPC 3.2 is the minimun requirement for Lazarus and LazUtils.
}
unit Laz_AVL_Tree;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{off $DEFINE MEM_CHECK}
{off $DEFINE CheckAVLTreeNodeManager}

uses
  {$IFDEF MEM_CHECK}MemCheck,{$ENDIF}
  Classes, SysUtils;

type
  TAVLTree = class;

  TObjectSortCompare = function(Tree: TAVLTree; Data1, Data2: Pointer): integer of object;

  { TAVLTreeNode }

  TAVLTreeNode = class
  public
    Parent, Left, Right: TAVLTreeNode;
    Balance: integer; // = RightDepth-LeftDepth  -2..+2, after balancing: -1,0,+1
    Data: Pointer;
    function Successor: TAVLTreeNode; // next right
    function Precessor: TAVLTreeNode; // next left
    procedure Clear;
    function TreeDepth: integer; // longest WAY down. e.g. only one node => 0 !
    procedure ConsistencyCheck(Tree: TAVLTree); virtual;
    function GetCount: SizeInt;
  end;
  TAVLTreeNodeClass = class of TAVLTreeNode;
  PAVLTreeNode = ^TAVLTreeNode;

  { TBaseAVLTreeNodeManager }

  TBaseAVLTreeNodeManager = class
  public
    procedure DisposeNode(ANode: TAVLTreeNode); virtual; abstract;
    function NewNode: TAVLTreeNode; virtual; abstract;
  end;

  { TAVLTreeNodeEnumerator }

  TAVLTreeNodeEnumerator = class
  protected
    FCurrent: TAVLTreeNode;
    FLowToHigh: boolean;
    FTree: TAVLTree;
  public
    constructor Create(Tree: TAVLTree; aLowToHigh: boolean = true);
    function GetEnumerator: TAVLTreeNodeEnumerator; inline;
    function MoveNext: Boolean;
    property Current: TAVLTreeNode read FCurrent;
    property LowToHigh: boolean read FLowToHigh;
  end;

  TAVLTree = class
  protected
    FCount: SizeInt;
    FNodeClass: TAVLTreeNodeClass;
    fNodeMgr: TBaseAVLTreeNodeManager;
    fNodeMgrAutoFree: boolean;
    FOnCompare: TListSortCompare;
    FOnObjectCompare: TObjectSortCompare;
    FRoot: TAVLTreeNode;
    procedure BalanceAfterInsert(ANode: TAVLTreeNode);
    procedure BalanceAfterDelete(ANode: TAVLTreeNode);
    procedure DeletingNode({%H-}aNode: TAVLTreeNode); virtual;
    function FindInsertPos(Data: Pointer): TAVLTreeNode;
    procedure Init; virtual;
    procedure NodeAdded({%H-}aNode: TAVLTreeNode); virtual;
    procedure RotateLeft(aNode: TAVLTreeNode); virtual;
    procedure RotateRight(aNode: TAVLTreeNode); virtual;
    procedure SwitchPositionWithSuccessor(aNode, aSuccessor: TAVLTreeNode); virtual;
    procedure SetOnCompare(const AValue: TListSortCompare);
    procedure SetOnObjectCompare(const AValue: TObjectSortCompare);
    procedure SetCompares(const NewCompare: TListSortCompare;
                          const NewObjectCompare: TObjectSortCompare);
    procedure SetNodeClass(const AValue: TAVLTreeNodeClass);
  public
    constructor Create(const OnCompareMethod: TListSortCompare);
    constructor CreateObjectCompare(const OnCompareMethod: TObjectSortCompare);
    constructor Create;
    destructor Destroy; override;
    property OnCompare: TListSortCompare read FOnCompare write SetOnCompare;
    property OnObjectCompare: TObjectSortCompare read FOnObjectCompare write SetOnObjectCompare;
    property NodeClass: TAVLTreeNodeClass read FNodeClass write SetNodeClass; // used for new nodes
    procedure SetNodeManager(NewMgr: TBaseAVLTreeNodeManager;
                             AutoFree: boolean = false);
    function NewNode: TAVLTreeNode; virtual; // create a node outside the tree
    procedure DisposeNode(ANode: TAVLTreeNode); virtual; // free the node outside the tree

    // add, delete, remove, move
    procedure Add(ANode: TAVLTreeNode);
    function Add(Data: Pointer): TAVLTreeNode;
    function AddAscendingSequence(Data: Pointer; LastAdded: TAVLTreeNode;
      var Successor: TAVLTreeNode): TAVLTreeNode;
    procedure Delete(ANode: TAVLTreeNode);
    // JuMa: Turned Remove and RemovePointer into functions.
    function Remove(Data: Pointer): boolean;
    function RemovePointer(Data: Pointer): boolean;
    procedure MoveDataLeftMost(var ANode: TAVLTreeNode);
    procedure MoveDataRightMost(var ANode: TAVLTreeNode);
    procedure Clear;
    procedure FreeAndClear;
    procedure FreeAndDelete(ANode: TAVLTreeNode); virtual;
    function Equals(Obj: TObject): boolean; override; // same as IsEqual(aTree,false)
    function IsEqual(aTree: TAVLTree; CheckDataPointer: boolean): boolean; // checks only keys or Data (references), not the data itself, O(n)
    procedure Assign(aTree: TAVLTree); virtual; // clear and copy all Data (references), O(n)

    // search
    property Root: TAVLTreeNode read fRoot;
    property Count: SizeInt read FCount;
    function Compare(Data1, Data2: Pointer): integer;
    function Find(Data: Pointer): TAVLTreeNode; // O(log(n))
    function FindKey(Key: Pointer;
      const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode; // O(log(n))
    function FindNearestKey(Key: Pointer;
      const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode; // O(log(n))
    function FindSuccessor(ANode: TAVLTreeNode): TAVLTreeNode; inline;
    function FindPrecessor(ANode: TAVLTreeNode): TAVLTreeNode; inline;
    function FindLowest: TAVLTreeNode; // O(log(n))
    function FindHighest: TAVLTreeNode; // O(log(n))
    function FindNearest(Data: Pointer): TAVLTreeNode;
    // search in a tree with duplicates (duplicate means here: Compare function returns 0)
    function FindPointer(Data: Pointer): TAVLTreeNode;
    function FindLeftMost(Data: Pointer): TAVLTreeNode;
    function FindRightMost(Data: Pointer): TAVLTreeNode;
    function FindLeftMostKey(Key: Pointer;
      const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode;
    function FindRightMostKey(Key: Pointer;
      const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode;
    function FindLeftMostSameKey(ANode: TAVLTreeNode): TAVLTreeNode;
    function FindRightMostSameKey(ANode: TAVLTreeNode): TAVLTreeNode;

    // enumerators
    function GetEnumerator: TAVLTreeNodeEnumerator;
    function GetEnumeratorHighToLow: TAVLTreeNodeEnumerator;

    // consistency
    procedure ConsistencyCheck; virtual; // JuMa: changed to procedure and added "virtual".
    procedure WriteReportToStream(s: TStream);
    function NodeToReportStr(aNode: TAVLTreeNode): string; virtual;
    function ReportAsString: string;
  end;
  TAVLTreeClass = class of TAVLTree;

  { TAVLTreeNodeMemManager }

  TAVLTreeNodeMemManager = class(TBaseAVLTreeNodeManager)
  private
    FFirstFree: TAVLTreeNode;
    FFreeCount: SizeInt;
    FCount: SizeInt;
    FMinFree: SizeInt;
    FMaxFreeRatio: SizeInt;
    {$IFDEF CheckAVLTreeNodeManager}
    FThreadId: TThreadID;
    {$ENDIF}
    procedure SetMaxFreeRatio(NewValue: SizeInt);
    procedure SetMinFree(NewValue: SizeInt);
    procedure DisposeFirstFreeNode;
  public
    procedure DisposeNode(ANode: TAVLTreeNode); override;
    function NewNode: TAVLTreeNode; override;
    property MinimumFreeNode: SizeInt read FMinFree write SetMinFree;
    property MaximumFreeNodeRatio: SizeInt
        read FMaxFreeRatio write SetMaxFreeRatio; // in one eighth steps
    property Count: SizeInt read FCount;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

var
  LazNodeMemManager: TAVLTreeNodeMemManager;

implementation

function ComparePointer(Data1, Data2: Pointer): integer;
begin
  if Data1>Data2 then Result:=-1
  else if Data1<Data2 then Result:=1
  else Result:=0;
end;

{ TAVLTreeNodeEnumerator }

constructor TAVLTreeNodeEnumerator.Create(Tree: TAVLTree; aLowToHigh: boolean);
begin
  FTree:=Tree;
  FLowToHigh:=aLowToHigh;
end;

function TAVLTreeNodeEnumerator.GetEnumerator: TAVLTreeNodeEnumerator;
begin
  Result:=Self;
end;

function TAVLTreeNodeEnumerator.MoveNext: Boolean;
begin
  if FLowToHigh then begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Successor
    else
      FCurrent:=FTree.FindLowest;
  end else begin
    if FCurrent<>nil then
      FCurrent:=FCurrent.Precessor
    else
      FCurrent:=FTree.FindHighest;
  end;
  Result:=FCurrent<>nil;
end;

{ TAVLTree }

function TAVLTree.Add(Data: Pointer): TAVLTreeNode;
begin
  Result:=NewNode;
  Result.Data:=Data;
  Add(Result);
end;

function TAVLTree.AddAscendingSequence(Data: Pointer; LastAdded: TAVLTreeNode;
  var Successor: TAVLTreeNode): TAVLTreeNode;
{ This is an optimized version of "Add" for adding an ascending sequence of
  nodes.
  It uses the LastAdded and Successor to skip searching for an insert position.
  For nodes with same value the order of the sequence is kept.

  Usage:
    LastNode:=nil; // TAvlTreeNode
    Successor:=nil; // TAvlTreeNode
    for i:=1 to 1000 do
      LastNode:=Tree.AddAscendingSequence(TItem.Create(i),LastNode,Successor);
}
var
  InsertPos: TAVLTreeNode;
begin
  Result:=NewNode;
  Result.Data:=Data;
  if (LastAdded<>nil) and (Compare(LastAdded.Data,Data)<=0)
  and ((Successor=nil) or (Compare(Data,Successor.Data)<=0)) then begin
    // Data is between LastAdded and Successor
    inc(FCount);
    if LastAdded.Right=nil then begin
      Result.Parent:=LastAdded;
      LastAdded.Right:=Result;
    end else begin
      InsertPos:=LastAdded.Right;
      while InsertPos.Left<>nil do
        InsertPos:=InsertPos.Left;
      Result.Parent:=InsertPos;
      InsertPos.Left:=Result;
    end;
    NodeAdded(Result);
    BalanceAfterInsert(Result);
  end else begin
    // normal Add
    Add(Result);
    Successor:=Result.Successor;
  end;
end;

function TAVLTree.NewNode: TAVLTreeNode;
begin
  if fNodeMgr<>nil then
    Result:=fNodeMgr.NewNode
  else
    Result:=NodeClass.Create;
end;

procedure TAVLTree.DisposeNode(ANode: TAVLTreeNode);
begin
  if fNodeMgr<>nil then
    fNodeMgr.DisposeNode(ANode)
  else
    ANode.Free;
end;

procedure TAVLTree.Add(ANode: TAVLTreeNode);
// add a node. If there are already nodes with the same value it will be
// inserted rightmost
var InsertPos: TAVLTreeNode;
  InsertComp: integer;
begin
  ANode.Left:=nil;
  ANode.Right:=nil;
  inc(FCount);
  if Root<>nil then begin
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
    NodeAdded(ANode);
    BalanceAfterInsert(ANode);
  end else begin
    fRoot:=ANode;
    ANode.Parent:=nil;
    NodeAdded(ANode);
  end;
end;

function TAVLTree.FindLowest: TAVLTreeNode;
begin
  Result:=Root;
  if Result<>nil then
    while Result.Left<>nil do Result:=Result.Left;
end;

function TAVLTree.FindHighest: TAVLTreeNode;
begin
  Result:=Root;
  if Result<>nil then
    while Result.Right<>nil do Result:=Result.Right;
end;

procedure TAVLTree.BalanceAfterDelete(ANode: TAVLTreeNode);
var
  OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight: TAVLTreeNode;
begin
  while ANode<>nil do begin
    if ((ANode.Balance=+1) or (ANode.Balance=-1)) then exit;
    OldParent:=ANode.Parent;
    if (ANode.Balance=0) then begin
      // Treeheight has decreased by one
      if (OldParent=nil) then
        exit;
      if(OldParent.Left=ANode) then
        Inc(OldParent.Balance)
      else
        Dec(OldParent.Balance);
      ANode:=OldParent;
    end else if (ANode.Balance=+2) then begin
      // Node is overweighted to the right
      OldRight:=ANode.Right;
      if (OldRight.Balance>=0) then begin
        // OldRight.Balance is 0 or -1
        // rotate ANode,OldRight left
        RotateLeft(ANode);
        ANode.Balance:=(1-OldRight.Balance); // toggle 0 and 1
        Dec(OldRight.Balance);
        ANode:=OldRight;
      end else begin
        // OldRight.Balance=-1
        { double rotate
          = rotate OldRightLeft,OldRight right
            and then rotate ANode,OldRightLeft left
                  OldParent                           OldParent
                      |                                  |
                    ANode                           OldRightLeft
                       \                               /      \
                    OldRight             =>          ANode    OldRight
                      /                                \         /
               OldRightLeft                OldRightLeftLeft OldRightLeftRight
                   /     \
        OldRightLeftLeft OldRightLeftRight
        }
        OldRightLeft:=OldRight.Left;
        RotateRight(OldRight);
        RotateLeft(ANode);
        if (OldRightLeft.Balance<=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=-1;
        if (OldRightLeft.Balance>=0) then
          OldRight.Balance:=0
        else
          OldRight.Balance:=+1;
        OldRightLeft.Balance:=0;
        ANode:=OldRightLeft;
      end;
    end else begin
      // Node.Balance=-2
      // Node is overweighted to the left
      OldLeft:=ANode.Left;
      if (OldLeft.Balance<=0) then begin
        // rotate OldLeft,ANode right
        RotateRight(ANode);
        ANode.Balance:=(-1-OldLeft.Balance); // toggle 0 and -1
        Inc(OldLeft.Balance);
        ANode:=OldLeft;
      end else begin
        // OldLeft.Balance = 1
        { double rotate left right
          = rotate OldLeft,OldLeftRight left
            and then rotate OldLeft,ANode right
                    OldParent                           OldParent
                        |                                  |
                      ANode                            OldLeftRight
                       /                               /         \
                    OldLeft             =>          OldLeft    ANode
                       \                                \         /
                   OldLeftRight               OldLeftRightLeft OldLeftRightRight
                     /     \
          OldLeftRightLeft OldLeftRightRight
        }
        OldLeftRight:=OldLeft.Right;
        RotateLeft(OldLeft);
        RotateRight(ANode);
        if (OldLeftRight.Balance>=0) then
          ANode.Balance:=0
        else
          ANode.Balance:=+1;
        if (OldLeftRight.Balance<=0) then
          OldLeft.Balance:=0
        else
          OldLeft.Balance:=-1;
        OldLeftRight.Balance:=0;
        ANode:=OldLeftRight;
      end;
    end;
  end;
end;

procedure TAVLTree.DeletingNode(aNode: TAVLTreeNode);
// called by Delete
// Node.Left=nil or Node.Right=nil
begin
  // for descendants to override
end;

procedure TAVLTree.SetOnObjectCompare(const AValue: TObjectSortCompare);
begin
  if AValue=nil then
    SetCompares(FOnCompare,nil)
  else
    SetCompares(nil,AValue);
end;

procedure TAVLTree.SetCompares(const NewCompare: TListSortCompare;
  const NewObjectCompare: TObjectSortCompare);
var List: PPointer;
  ANode: TAVLTreeNode;
  i, OldCount: integer;
begin
  if (FOnCompare=NewCompare) and (FOnObjectCompare=NewObjectCompare) then exit;
  if Count<=1 then begin
    FOnCompare:=NewCompare;
    FOnObjectCompare:=NewObjectCompare;
    exit;
  end;
  // sort the tree again
  OldCount:=Count;
  GetMem(List,SizeOf(Pointer)*OldCount);
  try
    // save the data in a list
    ANode:=FindLowest;
    i:=0;
    while ANode<>nil do begin
      List[i]:=ANode.Data;
      inc(i);
      ANode:=ANode.Successor;
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

procedure TAVLTree.SetNodeClass(const AValue: TAVLTreeNodeClass);
begin
  if FNodeClass=AValue then Exit;
  if Count>0 then
    raise Exception.Create(ClassName+'.SetNodeClass Count='+IntToStr(Count)
      +' Old='+fNodeMgr.ClassName+' New='+AValue.ClassName);
  FNodeClass:=AValue;
  if fNodeMgr=LazNodeMemManager then
    fNodeMgr:=nil;
end;

procedure TAVLTree.BalanceAfterInsert(ANode: TAVLTreeNode);
var
  OldParent, OldRight, OldLeft: TAVLTreeNode;
begin
  OldParent:=ANode.Parent;
  while (OldParent<>nil) do begin
    if (OldParent.Left=ANode) then begin
      // Node is left child
      dec(OldParent.Balance);
      if (OldParent.Balance=0) then exit;
      if (OldParent.Balance=-1) then begin
        ANode:=OldParent;
        OldParent:=ANode.Parent;
        continue;
      end;
      // OldParent.Balance=-2
      if (ANode.Balance=-1) then begin
        { rotate ANode,ANode.Parent right
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                 /                        \
              ANode                     OldParent
                \                        /
              OldRight               OldRight      }
        RotateRight(OldParent);
        ANode.Balance:=0;
        OldParent.Balance:=0;
      end else begin
        // Node.Balance = +1
        { double rotate
          = rotate ANode,OldRight left and then rotate OldRight,OldParent right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldRight
                   /            =>          /        \
                 ANode                   ANode      OldParent
                    \                       \          /
                   OldRight          OldRightLeft  OldRightRight
                     / \
          OldRightLeft OldRightRight
        }
        OldRight:=ANode.Right;
        RotateLeft(ANode);
        RotateRight(OldParent);
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
      exit;
    end else begin
      // Node is right child
      Inc(OldParent.Balance);
      if (OldParent.Balance=0) then exit;
      if (OldParent.Balance=+1) then begin
        ANode:=OldParent;
        OldParent:=ANode.Parent;
        continue;
      end;
      // OldParent.Balance = +2
      if(ANode.Balance=+1) then begin
        { rotate OldParent,ANode left
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                    \                   /
                  ANode               OldParent
                   /                      \
                OldLeft                 OldLeft      }
        RotateLeft(OldParent);
        ANode.Balance:=0;
        OldParent.Balance:=0;
      end else begin
        // Node.Balance = -1
        { double rotate
          = rotate OldLeft,ANode right and then rotate OldParent,OldLeft right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldLeft
                     \            =>        /       \
                    ANode               OldParent   ANode
                     /                     \          /
                  OldLeft          OldLeftLeft  OldLeftRight
                    / \
         OldLeftLeft OldLeftRight
        }
        OldLeft:=ANode.Left;
        RotateRight(ANode);
        RotateLeft(OldParent);
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
      exit;
    end;
  end;
end;

procedure TAVLTree.Clear;

  procedure DeleteNode(ANode: TAVLTreeNode);
  begin
    if ANode.Left<>nil then DeleteNode(ANode.Left);
    if ANode.Right<>nil then DeleteNode(ANode.Right);
    DisposeNode(ANode);
  end;

// Clear
begin
  if Root<>nil then
    DeleteNode(Root);
  fRoot:=nil;
  FCount:=0;
end;

constructor TAVLTree.Create(const OnCompareMethod: TListSortCompare);
begin
  FOnCompare:=OnCompareMethod;
  Init;
end;

constructor TAVLTree.CreateObjectCompare(
  const OnCompareMethod: TObjectSortCompare);
begin
  FOnObjectCompare:=OnCompareMethod;
  Init;
end;

constructor TAVLTree.Create;
begin
  Create(@ComparePointer);
end;

procedure TAVLTree.Delete(ANode: TAVLTreeNode);
var
  OldParent, Child: TAVLTreeNode;
begin
  {$IFDEF CheckAVLTreeNodeManager}
  OldParent:=ANode;
  while OldParent.Parent<>nil do OldParent:=OldParent.Parent;
  if OldParent<>Root then
    raise Exception.Create('TAVLTree.Delete'); // not my node
  {$ENDIF}
  if (ANode.Left<>nil) and (ANode.Right<>nil) then begin
    // ANode has both: Left and Right
    // Switch ANode position with Successor
    // Because ANode.Right<>nil the Successor is a child of ANode
    SwitchPositionWithSuccessor(ANode,ANode.Successor);
  end;
  // left or right is nil
  DeletingNode(aNode);
  OldParent:=ANode.Parent;
  ANode.Parent:=nil;
  if ANode.Left<>nil then
    Child:=ANode.Left
  else
    Child:=ANode.Right;
  if Child<>nil then
    Child.Parent:=OldParent;
  if (OldParent<>nil) then begin
    // Node has parent
    if (OldParent.Left=ANode) then begin
      // Node is left child of OldParent
      OldParent.Left:=Child;
      Inc(OldParent.Balance);
    end else begin
      // Node is right child of OldParent
      OldParent.Right:=Child;
      Dec(OldParent.Balance);
    end;
    BalanceAfterDelete(OldParent);
  end else begin
    // Node was Root
    fRoot:=Child;
  end;
  dec(FCount);
  DisposeNode(ANode);
end;

function TAVLTree.Remove(Data: Pointer): boolean;
var
  ANode: TAvlTreeNode;
begin
  ANode:=Find(Data);
  if ANode<>nil then begin
    Delete(ANode);
    Result:=true;
  end else
    Result:=false;
end;

function TAVLTree.RemovePointer(Data: Pointer): boolean;
var
  ANode: TAvlTreeNode;
begin
  ANode:=FindPointer(Data);
  if ANode<>nil then begin
    Delete(ANode);
    Result:=true;
  end else
    Result:=false;
end;

destructor TAVLTree.Destroy;
begin
  Clear;
  if fNodeMgrAutoFree then
    FreeAndNil(fNodeMgr);
  inherited Destroy;
end;

function TAVLTree.GetEnumerator: TAVLTreeNodeEnumerator;
begin
  Result:=TAVLTreeNodeEnumerator.Create(Self,true);
end;

function TAVLTree.GetEnumeratorHighToLow: TAVLTreeNodeEnumerator;
begin
  Result:=TAVLTreeNodeEnumerator.Create(Self,false);
end;

function TAVLTree.Find(Data: Pointer): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
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

function TAVLTree.FindKey(Key: Pointer; const OnCompareKeyWithData: TListSortCompare
  ): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
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

function TAVLTree.FindNearestKey(Key: Pointer;
  const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode;
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

function TAVLTree.FindLeftMostKey(Key: Pointer;
  const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode;
var
  LeftNode: TAVLTreeNode;
begin
  Result:=FindKey(Key,OnCompareKeyWithData);
  if Result=nil then exit;
  repeat
    LeftNode:=Result.Precessor;
    if (LeftNode=nil) or (OnCompareKeyWithData(Key,LeftNode.Data)<>0) then exit;
    Result:=LeftNode;
  until false;
end;

function TAVLTree.FindRightMostKey(Key: Pointer;
  const OnCompareKeyWithData: TListSortCompare): TAVLTreeNode;
var
  RightNode: TAVLTreeNode;
begin
  Result:=FindKey(Key,OnCompareKeyWithData);
  if Result=nil then exit;
  repeat
    RightNode:=Result.Successor;
    if (RightNode=nil) or (OnCompareKeyWithData(Key,RightNode.Data)<>0) then exit;
    Result:=RightNode;
  until false;
end;

function TAVLTree.FindLeftMostSameKey(ANode: TAVLTreeNode): TAVLTreeNode;
var
  LeftNode: TAVLTreeNode;
  Data: Pointer;
begin
  if ANode<>nil then begin
    Data:=ANode.Data;
    Result:=ANode;
    repeat
      LeftNode:=Result.Precessor;
      if (LeftNode=nil) or (Compare(Data,LeftNode.Data)<>0) then break;
      Result:=LeftNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAVLTree.FindRightMostSameKey(ANode: TAVLTreeNode): TAVLTreeNode;
var
  RightNode: TAVLTreeNode;
  Data: Pointer;
begin
  if ANode<>nil then begin
    Data:=ANode.Data;
    Result:=ANode;
    repeat
      RightNode:=Result.Successor;
      if (RightNode=nil) or (Compare(Data,RightNode.Data)<>0) then break;
      Result:=RightNode;
    until false;
  end else begin
    Result:=nil;
  end;
end;

function TAVLTree.FindNearest(Data: Pointer): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
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

function TAVLTree.FindPointer(Data: Pointer): TAVLTreeNode;
// same as Find, but not comparing for key, but same Data too
begin
  Result:=FindLeftMost(Data);
  while (Result<>nil) do begin
    if Result.Data=Data then break;
    Result:=Result.Successor;
    if Result=nil then exit;
    if Compare(Data,Result.Data)<>0 then exit(nil);
  end;
end;

function TAVLTree.FindLeftMost(Data: Pointer): TAVLTreeNode;
var
  Left: TAVLTreeNode;
begin
  Result:=Find(Data);
  while (Result<>nil) do begin
    Left:=Result.Precessor;
    if (Left=nil) or (Compare(Data,Left.Data)<>0) then break;
    Result:=Left;
  end;
end;

function TAVLTree.FindRightMost(Data: Pointer): TAVLTreeNode;
var
  Right: TAVLTreeNode;
begin
  Result:=Find(Data);
  while (Result<>nil) do begin
    Right:=Result.Successor;
    if (Right=nil) or (Compare(Data,Right.Data)<>0) then break;
    Result:=Right;
  end;
end;

function TAVLTree.FindInsertPos(Data: Pointer): TAVLTreeNode;
var Comp: integer;
begin
  Result:=Root;
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

procedure TAVLTree.Init;
begin
  FNodeClass:=TAVLTreeNode;
end;

procedure TAVLTree.NodeAdded(aNode: TAVLTreeNode);
begin
  // for descendants to override
end;

procedure TAVLTree.RotateLeft(aNode: TAVLTreeNode);
{    Parent                Parent
       |                     |
      Node        =>       OldRight
      /  \                  /
   Left OldRight          Node
          /               /  \
     OldRightLeft      Left OldRightLeft  }
var
  AParent, OldRight, OldRightLeft: TAVLTreeNode;
begin
  OldRight:=aNode.Right;
  OldRightLeft:=OldRight.Left;
  AParent:=aNode.Parent;
  if AParent<>nil then begin
    if AParent.Left=aNode then
      AParent.Left:=OldRight
    else
      AParent.Right:=OldRight;
  end else
    fRoot:=OldRight;
  OldRight.Parent:=AParent;
  aNode.Parent:=OldRight;
  aNode.Right:=OldRightLeft;
  if OldRightLeft<>nil then
    OldRightLeft.Parent:=aNode;
  OldRight.Left:=aNode;
end;

procedure TAVLTree.RotateRight(aNode: TAVLTreeNode);
{       Parent              Parent
          |                   |
         Node        =>     OldLeft
         /   \                 \
    OldLeft  Right            Node
        \                     /  \
   OldLeftRight      OldLeftRight Right  }
var
  AParent, OldLeft, OldLeftRight: TAVLTreeNode;
begin
  OldLeft:=aNode.Left;
  OldLeftRight:=OldLeft.Right;
  AParent:=aNode.Parent;
  if AParent<>nil then begin
    if AParent.Left=aNode then
      AParent.Left:=OldLeft
    else
      AParent.Right:=OldLeft;
  end else
    fRoot:=OldLeft;
  OldLeft.Parent:=AParent;
  aNode.Parent:=OldLeft;
  aNode.Left:=OldLeftRight;
  if OldLeftRight<>nil then
    OldLeftRight.Parent:=aNode;
  OldLeft.Right:=aNode;
end;

procedure TAVLTree.SwitchPositionWithSuccessor(aNode, aSuccessor: TAVLTreeNode);
{ called by delete, when aNode.Left<>nil and aNode.Right<>nil
  Switch ANode position with Successor
  Because ANode.Right<>nil the Successor is a child of ANode }
var
  OldBalance: Integer;
  OldParent, OldLeft, OldRight,
  OldSuccParent, OldSuccLeft, OldSuccRight: TAVLTreeNode;
begin
  OldBalance:=aNode.Balance;
  aNode.Balance:=aSuccessor.Balance;
  aSuccessor.Balance:=OldBalance;

  OldParent:=aNode.Parent;
  OldLeft:=aNode.Left;
  OldRight:=aNode.Right;
  OldSuccParent:=aSuccessor.Parent;
  OldSuccLeft:=aSuccessor.Left;
  OldSuccRight:=aSuccessor.Right;

  if OldParent<>nil then begin
    if OldParent.Left=aNode then
      OldParent.Left:=aSuccessor
    else
      OldParent.Right:=aSuccessor;
  end else
    fRoot:=aSuccessor;
  aSuccessor.Parent:=OldParent;

  if OldSuccParent<>aNode then begin
    if OldSuccParent.Left=aSuccessor then
      OldSuccParent.Left:=aNode
    else
      OldSuccParent.Right:=aNode;
    aSuccessor.Right:=OldRight;
    aNode.Parent:=OldSuccParent;
    if OldRight<>nil then
      OldRight.Parent:=aSuccessor;
  end else begin
    {  aNode            aSuccessor
         \          =>    \
         aSuccessor       aNode  }
    aSuccessor.Right:=aNode;
    aNode.Parent:=aSuccessor;
  end;

  aNode.Left:=OldSuccLeft;
  if OldSuccLeft<>nil then
    OldSuccLeft.Parent:=aNode;
  aNode.Right:=OldSuccRight;
  if OldSuccRight<>nil then
    OldSuccRight.Parent:=aNode;
  aSuccessor.Left:=OldLeft;
  if OldLeft<>nil then
    OldLeft.Parent:=aSuccessor;
end;

function TAVLTree.FindSuccessor(ANode: TAVLTreeNode): TAVLTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.Successor
  else
    Result:=nil;
end;

function TAVLTree.FindPrecessor(ANode: TAVLTreeNode): TAVLTreeNode;
begin
  if ANode<>nil then
    Result:=ANode.Precessor
  else
    Result:=nil;
end;

procedure TAVLTree.MoveDataLeftMost(var ANode: TAVLTreeNode);
var
  LeftMost, PreNode: TAVLTreeNode;
  Data: Pointer;
begin
  if ANode=nil then exit;
  LeftMost:=ANode;
  repeat
    PreNode:=FindPrecessor(LeftMost);
    if (PreNode=nil) or (Compare(ANode,PreNode)<>0) then break;
    LeftMost:=PreNode;
  until false;
  if LeftMost=ANode then exit;
  Data:=LeftMost.Data;
  LeftMost.Data:=ANode.Data;
  ANode.Data:=Data;
  ANode:=LeftMost;
end;

procedure TAVLTree.MoveDataRightMost(var ANode: TAVLTreeNode);
var
  RightMost, PostNode: TAVLTreeNode;
  Data: Pointer;
begin
  if ANode=nil then exit;
  RightMost:=ANode;
  repeat
    PostNode:=FindSuccessor(RightMost);
    if (PostNode=nil) or (Compare(ANode,PostNode)<>0) then break;
    RightMost:=PostNode;
  until false;
  if RightMost=ANode then exit;
  Data:=RightMost.Data;
  RightMost.Data:=ANode.Data;
  ANode.Data:=Data;
  ANode:=RightMost;
end;

procedure TAVLTree.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAVLTree.ConsistencyCheck: '+Msg);
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

procedure TAVLTree.FreeAndClear;

  procedure FreeNodeData(ANode: TAVLTreeNode);
  begin
    if ANode=nil then exit;
    FreeNodeData(ANode.Left);
    FreeNodeData(ANode.Right);
    if ANode.Data<>nil then TObject(ANode.Data).Free;
    ANode.Data:=nil;
  end;

// TAVLTree.FreeAndClear
begin
  // free all data
  FreeNodeData(Root);
  // free all nodes
  Clear;
end;

procedure TAVLTree.FreeAndDelete(ANode: TAVLTreeNode);
var OldData: TObject;
begin
  OldData:=TObject(ANode.Data);
  Delete(ANode);
  OldData.Free;
end;

function TAVLTree.Equals(Obj: TObject): boolean;
begin
  if Obj is TAVLTree then
    Result:=IsEqual(TAVLTree(Obj),false)
  else
    Result:=inherited Equals(Obj);
end;

function TAVLTree.IsEqual(aTree: TAVLTree; CheckDataPointer: boolean): boolean;
var
  MyNode, OtherNode: TAVLTreeNode;
begin
  if aTree=Self then exit(true);
  Result:=false;
  if aTree=nil then exit;
  if Count<>aTree.Count then exit;
  if OnCompare<>aTree.OnCompare then exit;
  if OnObjectCompare<>aTree.OnObjectCompare then exit;
  if NodeClass<>aTree.NodeClass then exit;
  MyNode:=FindLowest;
  OtherNode:=aTree.FindLowest;
  while MyNode<>nil do begin
    if OtherNode=nil then exit;
    if CheckDataPointer then begin
      if MyNode.Data<>OtherNode.Data then exit;
    end else begin
      if Compare(MyNode.Data,OtherNode.Data)<>0 then exit;
    end;
    MyNode:=MyNode.Successor;
    OtherNode:=OtherNode.Successor;
  end;
  if OtherNode<>nil then exit;
  Result:=true;
end;

procedure TAVLTree.Assign(aTree: TAVLTree);

  procedure AssignNode(var MyNode: TAVLTreeNode; OtherNode: TAVLTreeNode);
  begin
    MyNode:=NewNode;
    MyNode.Data:=OtherNode.Data;
    MyNode.Balance:=OtherNode.Balance;
    if OtherNode.Left<>nil then begin
      AssignNode(MyNode.Left,OtherNode.Left);
      MyNode.Left.Parent:=MyNode;
    end;
    if OtherNode.Right<>nil then begin
      AssignNode(MyNode.Right,OtherNode.Right);
      MyNode.Right.Parent:=MyNode;
    end;
  end;

begin
  if aTree=nil then
    raise Exception.Create('TAVLTree.Assign aTree=nil');
  if IsEqual(aTree,true) then exit;
  Clear;
  SetCompares(aTree.OnCompare,aTree.OnObjectCompare);
  NodeClass:=aTree.NodeClass;
  if aTree.Root<>nil then
    AssignNode(fRoot,aTree.Root);
  FCount:=aTree.Count;
end;

function TAVLTree.Compare(Data1, Data2: Pointer): integer;
begin
  if Assigned(FOnCompare) then
    Result:=FOnCompare(Data1,Data2)
  else
    Result:=FOnObjectCompare(Self,Data1,Data2);
end;

procedure TAVLTree.WriteReportToStream(s: TStream);

  procedure WriteStr(const Txt: string);
  begin
    if Txt='' then exit;
    s.Write(Txt[1],length(Txt));
  end;

  procedure WriteTreeNode(ANode: TAVLTreeNode);
  var
    b: String;
    IsLeft: boolean;
    AParent: TAVLTreeNode;
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
    b:=b+NodeToReportStr(ANode)+LineEnding;
    WriteStr(b);
    WriteTreeNode(ANode.Left);
  end;

// TAVLTree.WriteReportToStream
begin
  WriteStr('-Start-of-AVL-Tree-------------------'+LineEnding);
  WriteTreeNode(fRoot);
  WriteStr('-End-Of-AVL-Tree---------------------'+LineEnding);
end;

function TAVLTree.NodeToReportStr(aNode: TAVLTreeNode): string;
begin
  Result:=Format('%p      Self=%p  Parent=%p  Balance=%d',
             [aNode.Data, Pointer(aNode),Pointer(aNode.Parent), aNode.Balance]);
end;

function TAVLTree.ReportAsString: string;
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

procedure TAVLTree.SetOnCompare(const AValue: TListSortCompare);
begin
  if AValue=nil then
    SetCompares(nil,FOnObjectCompare)
  else
    SetCompares(AValue,nil);
end;

procedure TAVLTree.SetNodeManager(NewMgr: TBaseAVLTreeNodeManager;
  AutoFree: boolean);
// only allowed just after create.
begin
  if fNodeMgr=NewMgr then exit;
  if Count>0 then
    raise Exception.Create('TAVLTree.SetNodeManager');
  if fNodeMgrAutoFree then
    FreeAndNil(fNodeMgr);
  fNodeMgr:=NewMgr;
  fNodeMgrAutoFree:=AutoFree;
end;

{ TAVLTreeNode }

function TAVLTreeNode.TreeDepth: integer;
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

procedure TAVLTreeNode.ConsistencyCheck(Tree: TAVLTree);

  procedure E(Msg: string);
  begin
    raise Exception.Create('TAVLTreeNode.ConsistencyCheck: '+Msg);
  end;

var
  LeftDepth: SizeInt;
  RightDepth: SizeInt;
begin
  // test left child
  if Left<>nil then begin
    if Left.Parent<>Self then
      E('Left.Parent<>Self');
    if Tree.Compare(Left.Data,Data)>0 then
      E('Compare(Left.Data,Data)>0');
    Left.ConsistencyCheck(Tree);
  end;
  // test right child
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

function TAVLTreeNode.GetCount: SizeInt;
begin
  Result:=1;
  if Left<>nil then inc(Result,Left.GetCount);
  if Right<>nil then inc(Result,Right.GetCount);
end;

function TAVLTreeNode.Successor: TAVLTreeNode;
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

function TAVLTreeNode.Precessor: TAVLTreeNode;
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

procedure TAVLTreeNode.Clear;
begin
  Parent:=nil;
  Left:=nil;
  Right:=nil;
  Balance:=0;
  Data:=nil;
end;



{ TAVLTreeNodeMemManager }

constructor TAVLTreeNodeMemManager.Create;
begin
  {$IFDEF CheckAVLTreeNodeManager}
  FThreadId:=GetCurrentThreadId;
  {$ENDIF}
  inherited Create;
  FFirstFree:=nil;
  FFreeCount:=0;
  FCount:=0;
  FMinFree:=100;
  FMaxFreeRatio:=8; // 1:1
end;

destructor TAVLTreeNodeMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TAVLTreeNodeMemManager.DisposeNode(ANode: TAVLTreeNode);
begin
  if ANode=nil then exit;
  {$IFDEF CheckAVLTreeNodeManager}
  if GetCurrentThreadId<>FThreadId then
    raise Exception.Create('TAVLTreeNodeMemManager.DisposeNode not thread safe!');
  {$ENDIF}
  if FCount < 0 then
    raise Exception.CreateFmt(
      '%s.DisposeNode: FCount (%d) is negative. Should not happen.'
     +' FFreeCount=%d, FMinFree=%d, FMaxFreeRatio=%d.',
      [ClassName, FCount, FFreeCount, FMinFree, FMaxFreeRatio]);
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ANode to Free list
    ANode.Clear;
    ANode.Right:=FFirstFree;
    FFirstFree:=ANode;
    inc(FFreeCount);
    if (FFreeCount>(((8+FMaxFreeRatio)*FCount) shr 3)) then begin
      DisposeFirstFreeNode;
      DisposeFirstFreeNode;
    end;
  end else begin
    // free list full -> free the ANode
    ANode.Free;
  end;
  dec(FCount);
end;

function TAVLTreeNodeMemManager.NewNode: TAVLTreeNode;
begin
  {$IFDEF CheckAVLTreeNodeManager}
  if GetCurrentThreadId<>FThreadId then
    raise Exception.Create('TAVLTreeNodeMemManager.NewNode: not thread safe!');
  {$ENDIF}
  if FFirstFree<>nil then begin
    // take from free list
    Result:=FFirstFree;
    FFirstFree:=FFirstFree.Right;
    Result.Right:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    Result:=TAVLTreeNode.Create;
  end;
  inc(FCount);
end;

procedure TAVLTreeNodeMemManager.Clear;
var ANode: TAVLTreeNode;
begin
  {$IFDEF CheckAVLTreeNodeManager}
  if GetCurrentThreadId<>FThreadId then
    raise Exception.Create('TAVLTreeNodeMemManager.Clear: not thread safe!');
  {$ENDIF}
  while FFirstFree<>nil do begin
    ANode:=FFirstFree;
    FFirstFree:=FFirstFree.Right;
    ANode.Right:=nil;
    ANode.Free;
  end;
  FFreeCount:=0;
end;

procedure TAVLTreeNodeMemManager.SetMaxFreeRatio(NewValue: SizeInt);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMaxFreeRatio then exit;
  FMaxFreeRatio:=NewValue;
end;

procedure TAVLTreeNodeMemManager.SetMinFree(NewValue: SizeInt);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMinFree then exit;
  FMinFree:=NewValue;
end;

procedure TAVLTreeNodeMemManager.DisposeFirstFreeNode;
var OldNode: TAVLTreeNode;
begin
  if FFirstFree=nil then exit;
  OldNode:=FFirstFree;
  FFirstFree:=FFirstFree.Right;
  dec(FFreeCount);
  OldNode.Right:=nil;
  OldNode.Free;
end;


initialization
  LazNodeMemManager:=TAVLTreeNodeMemManager.Create;

finalization
  LazNodeMemManager.Free;
  LazNodeMemManager:=nil;
end.
