{
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
    Cache objects for TFindDeclarationTool.

}
unit FindDeclarationCache;

{$mode objfpc}{$H+}

interface

{$I codetools.inc}

uses
  Classes, SysUtils, BasicCodeTools, AVL_Tree, CodeTree, LinkScanner,
  PascalParserTool, CodeToolMemManager;

type
  {
    1. interface cache:
      Every FindIdentifierInInterface call is cached
        - stores: Identifier -> Node+CleanPos
        - cache must be deleted, everytime the codetree is rebuild
           this is enough update, because it does only store internals
       -> This will improve search time for interface requests
  }
  PInterfaceIdentCacheEntry = ^TInterfaceIdentCacheEntry;
  TInterfaceIdentCacheEntry = record
    Identifier: PChar;
    Node: TCodeTreeNode; // if node = nil then identifier does not exists in
                         //                    this interface
    CleanPos: integer;
    NextEntry: PInterfaceIdentCacheEntry; // used by memory manager
  end;

  TInterfaceIdentifierCache = class
  private
    FItems: TAVLTree; // tree of TInterfaceIdentCacheEntry
    FTool: TPascalParserTool;
    function FindAVLNode(Identifier: PChar): TAVLTreeNode;
  public
    function FindIdentifier(Identifier: PChar): PInterfaceIdentCacheEntry;
    procedure Add(Identifier: PChar; Node: TCodeTreeNode; CleanPos: integer);
    procedure Clear;
    constructor Create(ATool: TPascalParserTool);
    destructor Destroy; override;
    property Tool: TPascalParserTool read FTool;
  end;

  {
    2. code tree node cache:
      Some nodes (class, interface, implementation, program, type, var, const,
        ...) contain a node
      cache. A node cache caches identifier requests of direct child nodes.
      Because node caches can store information of used units, the cahce must be
      deleted every time a used unit is changed. Currently all node caches are
      resetted every time the GlobalWriteLock increases.


       every 'cache' node get a list of
         Identifier+CleanBackwardPos+CleanForwardPos -> TFindContext
         This information means: if an identifier is searched at a
         child node (not sub child node!) within the bounds, the cached
         FindContext is valid.
       'cache' nodes are:
         - section nodes e.g. interface, program, ...
         - class nodes
       this cache must be deleted, every time the code tree changes, or
       one of the used units changes.
  }
  PCodeTreeNodeCacheEntry = ^TCodeTreeNodeCacheEntry;
  TCodeTreeNodeCacheEntry = record
    Identifier: PChar;
    CleanStartPos: integer;
    CleanEndPos: integer;
    NewNode: TCodeTreeNode;
    NewTool: TPascalParserTool;
    NewCleanPos: integer;
    NextEntry: PCodeTreeNodeCacheEntry; // used for mem manager
  end;

  TCodeTreeNodeCache = class
  private
    FItems: TAVLTree; // tree of PCodeTreeNodeCacheEntry
  public
    Next: TCodeTreeNodeCache;
    function FindLeftMostAVLNode(Identifier: PChar): TAVLTreeNode;
    function FindRightMostAVLNode(Identifier: PChar): TAVLTreeNode;
    function FindAVLNode(Identifier: PChar; CleanPos: integer): TAVLTreeNode;
    function FindAVLNodeInRange(Identifier: PChar;
      CleanStartPos, CleanEndPos: integer): TAVLTreeNode;
    function Find(Identifier: PChar): PCodeTreeNodeCacheEntry;
    procedure Add(Identifier: PChar; CleanStartPos, CleanEndPos: integer;
      NewNode: TCodeTreeNode; NewTool: TPascalParserTool; NewCleanPos: integer);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  TGlobalIdentifierTree = class
  private
    FItems: TAVLTree; // tree of PChar;
  public
    function AddCopy(Identifier: PChar): PChar;
    function Find(Identifier: PChar): PChar;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  // Memory Managers

  // memory system for PInterfaceIdentCacheEntry(s)
  TInterfaceIdentCacheEntryMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeEntry(Entry: PInterfaceIdentCacheEntry);
    function NewEntry: PInterfaceIdentCacheEntry;
  end;

  // memory system for PCodeTreeNodeCacheEntry(s)
  TNodeCacheEntryMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeEntry(Entry: PCodeTreeNodeCacheEntry);
    function NewEntry: PCodeTreeNodeCacheEntry;
  end;

  // memory system for TCodeTreeNodeCache(s)
  TNodeCacheMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeNode(Node: TCodeTreeNodeCache);
    function NewNode: TCodeTreeNodeCache;
  end;

var
  GlobalIdentifierTree: TGlobalIdentifierTree;
  InterfaceIdentCacheEntryMemManager: TInterfaceIdentCacheEntryMemManager;
  NodeCacheEntryMemManager: TNodeCacheEntryMemManager;


implementation


{ TNodeCacheEntryMemManager }

procedure TNodeCacheEntryMemManager.DisposeEntry(Entry: PCodeTreeNodeCacheEntry);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Entry to Free list
    Entry^.NextEntry:=PCodeTreeNodeCacheEntry(FFirstFree);
    PCodeTreeNodeCacheEntry(FFirstFree):=Entry;
    inc(FFreeCount);
  end else begin
    // free list full -> free the Entry
    Dispose(Entry);
    inc(FFreedCount);
  end;
  dec(FCount);
end;

function TNodeCacheEntryMemManager.NewEntry: PCodeTreeNodeCacheEntry;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PCodeTreeNodeCacheEntry(FFirstFree);
    PCodeTreeNodeCacheEntry(FFirstFree):=Result^.NextEntry;
    Result^.NextEntry:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new Entry
    New(Result);
    inc(FAllocatedCount);
  end;
  inc(FCount);
end;

procedure TNodeCacheEntryMemManager.FreeFirstItem;
var Entry: PCodeTreeNodeCacheEntry;
begin
  Entry:=PCodeTreeNodeCacheEntry(FFirstFree);
  PCodeTreeNodeCacheEntry(FFirstFree):=Entry^.NextEntry;
  Dispose(Entry);
end;


{ TInterfaceIdentCacheEntryMemManager }

procedure TInterfaceIdentCacheEntryMemManager.DisposeEntry(
  Entry: PInterfaceIdentCacheEntry);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Entry to Free list
    Entry^.NextEntry:=PInterfaceIdentCacheEntry(FFirstFree);
    PInterfaceIdentCacheEntry(FFirstFree):=Entry;
    inc(FFreeCount);
  end else begin
    // free list full -> free the Entry
    Dispose(Entry);
    inc(FFreedCount);
  end;
  dec(FCount);
end;

function TInterfaceIdentCacheEntryMemManager.NewEntry: PInterfaceIdentCacheEntry;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=PInterfaceIdentCacheEntry(FFirstFree);
    PInterfaceIdentCacheEntry(FFirstFree):=Result^.NextEntry;
    Result^.NextEntry:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new Entry
    New(Result);
    inc(FAllocatedCount);
  end;
  inc(FCount);
end;

procedure TInterfaceIdentCacheEntryMemManager.FreeFirstItem;
var Entry: PInterfaceIdentCacheEntry;
begin
  Entry:=PInterfaceIdentCacheEntry(FFirstFree);
  PInterfaceIdentCacheEntry(FFirstFree):=Entry^.NextEntry;
  Dispose(Entry);
end;


{ TInterfaceIdentifierCache }

function CompareTInterfaceIdentCacheEntry(Data1, Data2: Pointer): integer;
begin
  Result:=CompareIdentifiers(PInterfaceIdentCacheEntry(Data1)^.Identifier,
                             PInterfaceIdentCacheEntry(Data2)^.Identifier);
end;


procedure TInterfaceIdentifierCache.Clear;
var
  Node: TAVLTreeNode;
  Entry: PInterfaceIdentCacheEntry;
begin
  if FItems<>nil then begin
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      Entry:=PInterfaceIdentCacheEntry(Node.Data);
      InterfaceIdentCacheEntryMemManager.DisposeEntry(Entry);
      Node:=FItems.FindSuccessor(Node);
    end;
    FItems.Clear;
  end;
end;

constructor TInterfaceIdentifierCache.Create(ATool: TPascalParserTool);
begin
  inherited Create;
  FTool:=ATool;
  if ATool=nil then
    raise Exception.Create('TInterfaceIdentifierCache.Create ATool=nil');
end;

destructor TInterfaceIdentifierCache.Destroy;
begin
  Clear;
  if FItems<>nil then FItems.Free;
  inherited Destroy;
end;

function TInterfaceIdentifierCache.FindAVLNode(Identifier: PChar): TAVLTreeNode;
var
  Entry: PInterfaceIdentCacheEntry;
  comp: integer;
begin
  if FItems<>nil then begin
    Result:=FItems.Root;
    while Result<>nil do begin
      Entry:=PInterfaceIdentCacheEntry(Result.Data);
      comp:=CompareIdentifiers(Identifier,Entry^.Identifier);
      if comp<0 then
        Result:=Result.Left
      else if comp>0 then
        Result:=Result.Right
      else
        exit;
    end;
  end else begin
    Result:=nil;
  end;
end;

function TInterfaceIdentifierCache.FindIdentifier(Identifier: PChar
  ): PInterfaceIdentCacheEntry;
var Node: TAVLTreeNode;
begin
  Node:=FindAVLNode(Identifier);
  if Node<>nil then
    Result:=PInterfaceIdentCacheEntry(Node.Data)
  else
    Result:=nil;
end;

procedure TInterfaceIdentifierCache.Add(Identifier: PChar; Node: TCodeTreeNode;
  CleanPos: integer);
var
  NewEntry: PInterfaceIdentCacheEntry;
begin
  if FItems=nil then
    FItems:=TAVLTree.Create(@CompareTInterfaceIdentCacheEntry);
  NewEntry:=InterfaceIdentCacheEntryMemManager.NewEntry;
  NewEntry^.Identifier:=GlobalIdentifierTree.AddCopy(Identifier);
  NewEntry^.Node:=Node;
  NewEntry^.CleanPos:=CleanPos;
  FItems.Add(NewEntry);
end;


{ TGlobalIdentifierTree }

procedure TGlobalIdentifierTree.Clear;
var Node: TAVLTreeNode;
begin
  if FItems<>nil then begin
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      FreeMem(Node.Data);
      Node:=FItems.FindSuccessor(Node);
    end;
    FItems.Clear;
  end;
end;

constructor TGlobalIdentifierTree.Create;
begin
  inherited Create;
end;

destructor TGlobalIdentifierTree.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TGlobalIdentifierTree.AddCopy(Identifier: PChar): PChar;
var Len: integer;
begin
  Result:=nil;
  if (Identifier=nil) or (not IsIdentChar[Identifier[0]]) then exit;
  Result:=Find(Identifier);
  if Result<>nil then
    exit;
  Len:=0;
  while IsIdentChar[Identifier[Len]] do inc(Len);
  GetMem(Result,Len+1);
  Move(Identifier^,Result^,Len+1);
  if FItems=nil then
    FItems:=TAVLTree.Create(TListSortCompare(@CompareIdentifiers));
  FItems.Add(Result);
end;

function TGlobalIdentifierTree.Find(Identifier: PChar): PChar;
var
  comp: integer;
  Node: TAVLTreeNode;
begin
  Result:=nil;
  if FItems<>nil then begin
    Node:=FItems.Root;
    while Result<>nil do begin
      Result:=PChar(Node.Data);
      comp:=CompareIdentifiers(Identifier,Result);
      if comp<0 then
        Node:=Node.Left
      else if comp>0 then
        Node:=Node.Right
      else
        exit;
    end;
  end;
end;


{ TCodeTreeNodeCache }

function CompareTCodeTreeNodeCacheEntry(Data1, Data2: Pointer): integer;
var Entry1, Entry2: PCodeTreeNodeCacheEntry;
begin
  Entry1:=PCodeTreeNodeCacheEntry(Data1);
  Entry2:=PCodeTreeNodeCacheEntry(Data2);
  Result:=CompareIdentifiers(Entry1^.Identifier,Entry2^.Identifier);
  if Result=0 then begin
    if Entry1^.CleanStartPos>Entry2^.CleanStartPos then
      Result:=-1
    else if Entry1^.CleanStartPos<Entry2^.CleanStartPos then
      Result:=1
    else
      Result:=0;
  end;
end;

constructor TCodeTreeNodeCache.Create;
begin
  inherited Create;

end;

destructor TCodeTreeNodeCache.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCodeTreeNodeCache.FindLeftMostAVLNode(Identifier: PChar): TAVLTreeNode;
// find leftmost avl node with Identifier
var
  Entry: PCodeTreeNodeCacheEntry;
  Node: TAVLTreeNode;
  comp: integer;
begin
  if FItems<>nil then begin
    Result:=FItems.Root;
    while Result<>nil do begin
      Entry:=PCodeTreeNodeCacheEntry(Result.Data);
      comp:=CompareIdentifiers(Identifier,Entry^.Identifier);
      if comp<0 then
        Result:=Result.Left
      else if comp>0 then
        Result:=Result.Right
      else begin
        repeat
          Node:=FItems.FindPrecessor(Result);
          if Node<>nil then begin
            Entry:=PCodeTreeNodeCacheEntry(Node.Data);
            if CompareIdentifiers(Identifier,Entry^.Identifier)=0 then
              Result:=Node
            else
              break;
          end else
            break;
        until false;
        exit;
      end;
    end;
  end else begin
    Result:=nil;
  end;
end;

procedure TCodeTreeNodeCache.Clear;
var
  Node: TAVLTreeNode;
  Entry: PCodeTreeNodeCacheEntry;
begin
  if FItems<>nil then begin
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      Entry:=PCodeTreeNodeCacheEntry(Node.Data);
      NodeCacheEntryMemManager.DisposeEntry(Entry);
      Node:=FItems.FindSuccessor(Node);
    end;
    FItems.Clear;
  end;
end;

procedure TCodeTreeNodeCache.Add(Identifier: PChar;
  CleanStartPos, CleanEndPos: integer;
  NewNode: TCodeTreeNode; NewTool: TPascalParserTool; NewCleanPos: integer);

  procedure AddNewEntry;
  var NewEntry: PCodeTreeNodeCacheEntry;
  begin
    NewEntry:=NodeCacheEntryMemManager.NewEntry;
    NewEntry^.Identifier:=GlobalIdentifierTree.AddCopy(Identifier);
    NewEntry^.CleanStartPos:=CleanStartPos;
    NewEntry^.CleanEndPos:=CleanEndPos;
    NewEntry^.NewNode:=NewNode;
    NewEntry^.NewTool:=NewTool;
    NewEntry^.NewCleanPos:=NewCleanPos;
    FItems.Add(NewEntry);
  end;

var
  OldEntry: PCodeTreeNodeCacheEntry;
  OldNode: TAVLTreeNode;
begin
  if CleanStartPos>=CleanEndPos then
    raise Exception.Create('[TCodeTreeNodeCache.Add] internal error:'
      +' CleanStartPos>=CleanEndPos');
  if FItems=nil then
    FItems:=TAVLTree.Create(@CompareTCodeTreeNodeCacheEntry);
  // if identifier already exists, try to combine them
  OldNode:=FindAVLNodeInRange(Identifier,CleanStartPos,CleanEndPos);
  if OldNode=nil then begin
    // identifier was never searched in this range
    AddNewEntry;
  end else begin
    // identifier was already searched in this range
    OldEntry:=PCodeTreeNodeCacheEntry(OldNode.Data);
    if (NewNode=OldEntry^.NewNode)
    and (NewTool=OldEntry^.NewTool) then
    begin
      // same FindContext with connected search ranges
      // -> combine search ranges
      if OldEntry^.CleanStartPos>CleanStartPos then
        OldEntry^.CleanStartPos:=CleanStartPos;
      if OldEntry^.CleanEndPos<CleanEndPos then
        OldEntry^.CleanEndPos:=CleanEndPos;
    end else begin
      // different FindContext with connected search ranges
      if (OldEntry^.CleanStartPos=CleanEndPos)
      or (OldEntry^.CleanEndPos=CleanStartPos) then begin
        // add new entry
        AddNewEntry;
      end else begin
        raise Exception.Create('[TCodeTreeNodeCache.Add] internal error:'
          +' conflicting cache nodes');
      end;
    end;
  end;
end;

function TCodeTreeNodeCache.Find(Identifier: PChar): PCodeTreeNodeCacheEntry;
var Node: TAVLTreeNode;
begin
  Node:=FindLeftMostAVLNode(Identifier);
  if Node<>nil then begin
    Result:=PCodeTreeNodeCacheEntry(Node.Data);
  end else begin
    Result:=nil;
  end;
end;

function TCodeTreeNodeCache.FindAVLNode(Identifier: PChar; CleanPos: integer
  ): TAVLTreeNode;
begin
  Result:=FindAVLNodeInRange(Identifier,CleanPos,CleanPos);
end;

function TCodeTreeNodeCache.FindRightMostAVLNode(Identifier: PChar
  ): TAVLTreeNode;
// find rightmost avl node with Identifier
var
  Entry: PCodeTreeNodeCacheEntry;
  Node: TAVLTreeNode;
  comp: integer;
begin
  if FItems<>nil then begin
    Result:=FItems.Root;
    while Result<>nil do begin
      Entry:=PCodeTreeNodeCacheEntry(Result.Data);
      comp:=CompareIdentifiers(Identifier,Entry^.Identifier);
      if comp<0 then
        Result:=Result.Left
      else if comp>0 then
        Result:=Result.Right
      else begin
        repeat
          Node:=FItems.FindSuccessor(Result);
          if Node<>nil then begin
            Entry:=PCodeTreeNodeCacheEntry(Node.Data);
            if CompareIdentifiers(Identifier,Entry^.Identifier)=0 then
              Result:=Node
            else
              break;
          end else
            break;
        until false;
        exit;
      end;
    end;
  end else begin
    Result:=nil;
  end;
end;

function TCodeTreeNodeCache.FindAVLNodeInRange(Identifier: PChar;
  CleanStartPos, CleanEndPos: integer): TAVLTreeNode;
var
  Entry: PCodeTreeNodeCacheEntry;
  comp: integer;
begin
  if FItems<>nil then begin
    Result:=FItems.Root;
    while Result<>nil do begin
      Entry:=PCodeTreeNodeCacheEntry(Result.Data);
      comp:=CompareIdentifiers(Identifier,Entry^.Identifier);
      if comp<0 then
        Result:=Result.Left
      else if comp>0 then
        Result:=Result.Right
      else begin
        repeat
          if CleanStartPos>=Entry^.CleanEndPos then
            Result:=FItems.FindSuccessor(Result)
          else if CleanEndPos<Entry^.CleanStartPos then
            Result:=FItems.FindPrecessor(Result)
          else
            exit;
        until Result=nil;
      end;
    end;
  end else begin
    Result:=nil;
  end;
end;

{ TNodeCacheMemManager }

procedure TNodeCacheMemManager.DisposeNode(Node: TCodeTreeNodeCache);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Entry to Free list
    Node.Next:=TCodeTreeNodeCache(FFirstFree);
    TCodeTreeNodeCache(FFirstFree):=Node;
    inc(FFreeCount);
  end else begin
    // free list full -> free the Node
    Node.Free;
    inc(FFreedCount);
  end;
  dec(FCount);
end;

procedure TNodeCacheMemManager.FreeFirstItem;
var Node: TCodeTreeNodeCache;
begin
  Node:=TCodeTreeNodeCache(FFirstFree);
  TCodeTreeNodeCache(FFirstFree):=Node.Next;
  Node.Free;
end;

function TNodeCacheMemManager.NewNode: TCodeTreeNodeCache;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TCodeTreeNodeCache(FFirstFree);
    TCodeTreeNodeCache(FFirstFree):=Result.Next;
    Result.Clear;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new Entry
    Result:=TCodeTreeNodeCache.Create;
    inc(FAllocatedCount);
  end;
  inc(FCount);
end;

//------------------------------------------------------------------------------

procedure InternalInit;
begin
  GlobalIdentifierTree:=TGlobalIdentifierTree.Create;
  InterfaceIdentCacheEntryMemManager:=TInterfaceIdentCacheEntryMemManager.Create;
end;

procedure InternalFinal;
begin
  GlobalIdentifierTree.Free;
  GlobalIdentifierTree:=nil;
  InterfaceIdentCacheEntryMemManager.Free;
  InterfaceIdentCacheEntryMemManager:=nil;
end;

initialization
  InternalInit;

finalization
  InternalFinal;


end.

