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


// for debugging
{ $DEFINE HardExceptions}

uses
  Classes, SysUtils, FileProcs, BasicCodeTools, CodeTree, LinkScanner,
  AVL_Tree, PascalParserTool, KeywordFuncLists,
  CodeToolMemManager;

type
  {
    1. interface cache: (unit interfaces, not class interfaces)
      Every FindIdentifierInInterface call is cached
        - stores: Identifier -> Node+CleanPos
        - cache must be deleted, everytime the codetree is rebuild
           this is enough update, because it does only store internals
       -> This improves search time for interface requests
  }
  PInterfaceIdentCacheEntry = ^TInterfaceIdentCacheEntry;
  TInterfaceIdentCacheEntry = record
    Identifier: PChar;
    Node: TCodeTreeNode; // if node = nil then identifier does not exist in
                         //                    this interface
    CleanPos: integer;
    Overloaded: PInterfaceIdentCacheEntry;
    NextEntry: PInterfaceIdentCacheEntry; // used by memory manager
  end;

  { TInterfaceIdentifierCache }

  TInterfaceIdentifierCache = class
  private
    FComplete: boolean;
    FItems: TAVLTree; // tree of PInterfaceIdentCacheEntry
    FTool: TPascalParserTool;
    function FindAVLNode(Identifier: PChar): TAVLTreeNode;
    procedure SetComplete(const AValue: boolean);
  public
    function FindIdentifier(Identifier: PChar): PInterfaceIdentCacheEntry;
    procedure Add(Identifier: PChar; Node: TCodeTreeNode; CleanPos: integer);
    procedure Clear;
    procedure ClearMissingIdentifiers;
    constructor Create(ATool: TPascalParserTool);
    destructor Destroy; override;
    procedure ConsistencyCheck;
    property Tool: TPascalParserTool read FTool;
    property Complete: boolean read FComplete write SetComplete;
    property Items: TAVLTree read FItems;
    function CalcMemSize: PtrUInt;
  end;

  {
    2. code tree node cache:
      Some nodes (class, proc, record) contain a node cache. A node cache caches
      search results of searched identifiers for child nodes.
      
      Every entry in the node cache describes the following relationship:
        Identifier+Range -> Source Position
      and can be interpreted as:
      Identifier is a PChar to the beginning of an identifier string.
      Range is a cleaned source range (CleanStartPos-CleanEndPos).
      Source position is a tuple of NewTool, NewNode, NewCleanPos.
      If the current context node is a child of a caching node and it is in the
      range, then the result is valid. If NewNode=nil then there is no such
      identifier valid at the context node.

      Every node that defines local identifiers contains a node cache.
      These are: class, interface, proc, record, withstatement
      
      Because node caches can store information of used units, the cache must be
      deleted every time a used unit is changed. Currently all node caches are
      resetted every time the GlobalWriteLock increases.

  }
const
  AllNodeCacheDescs =
    AllClasses+[ctnProcedure, ctnRecordType, ctnWithStatement];
  
type
  TNodeCacheEntryFlag = (ncefSearchedInParents, ncefSearchedInAncestors);
  TNodeCacheEntryFlags = set of TNodeCacheEntryFlag;
  
  PCodeTreeNodeCacheEntry = ^TCodeTreeNodeCacheEntry;
  TCodeTreeNodeCacheEntry = record
    Identifier: PChar;
    CleanStartPos: integer;
    CleanEndPos: integer;
    NewNode: TCodeTreeNode;
    NewTool: TPascalParserTool;
    NewCleanPos: integer;
    Flags: TNodeCacheEntryFlags;
    NextEntry: PCodeTreeNodeCacheEntry; // used for mem manager
  end;

  { TCodeTreeNodeCache }

  TCodeTreeNodeCache = class
  private
    FItems: TAVLTree; // tree of PCodeTreeNodeCacheEntry
  public
    Next: TCodeTreeNodeCache;
    Owner: TCodeTreeNode;
    function FindLeftMostAVLNode(Identifier: PChar): TAVLTreeNode;
    function FindRightMostAVLNode(Identifier: PChar): TAVLTreeNode;
    function FindAVLNode(Identifier: PChar; CleanPos: integer): TAVLTreeNode;
    function FindAVLNodeInRange(Identifier: PChar;
      CleanStartPos, CleanEndPos: integer): TAVLTreeNode;
    function FindInRange(Identifier: PChar;
      CleanStartPos, CleanEndPos: integer): PCodeTreeNodeCacheEntry;
    function FindNearestAVLNode(Identifier: PChar;
      CleanStartPos, CleanEndPos: integer; InFront: boolean): TAVLTreeNode;
    function FindNearest(Identifier: PChar;
      CleanStartPos, CleanEndPos: integer;
      InFront: boolean): PCodeTreeNodeCacheEntry;
    function Find(Identifier: PChar): PCodeTreeNodeCacheEntry;
    procedure Add(Identifier: PChar;
      SrcTool: TPascalParserTool; CleanStartPos, CleanEndPos: integer;
      NewNode: TCodeTreeNode; NewTool: TPascalParserTool; NewCleanPos: integer;
      Flags: TNodeCacheEntryFlags);
    procedure Clear;
    procedure BindToOwner(NewOwner: TCodeTreeNode);
    procedure UnbindFromOwner;
    constructor Create(AnOwner: TCodeTreeNode);
    destructor Destroy; override;
    procedure WriteDebugReport(const Prefix: string);
    procedure ConsistencyCheck;
    function CalcMemSize: PtrUInt;
  end;
  
  {
    3. Base type node cache
    
    All nodes, that are aliases, has this type of cache.
    For example a variable 'i: integer' creates several basetype nodes:
      1. i variable node points to its type node 'integer'.
      2. 'integer' node points to type definition node 'integer'.
      3. 'integer' identifier node points to its base type 'longint'.
      4. 'longint' identifier node points points to its range.
      
      FindBaseTypeOfNode will search this chain, and on success will create
      TBaseTypeCache(s). All four nodes will point directly to the range.

  }

  { TBaseTypeCache }

  TBaseTypeCache = class
  private
  public
    NewNode: TCodeTreeNode;
    NewTool: TPascalParserTool;
    Next: TBaseTypeCache; // used for mem manager
    Owner: TCodeTreeNode;
    procedure BindToOwner(NewOwner: TCodeTreeNode);
    procedure UnbindFromOwner;
    constructor Create(AnOwner: TCodeTreeNode);
    destructor Destroy; override;
    function CalcMemSize: PtrUInt;
  end;

  {
    4. CodeTool Cache Dependencies

    Node- and BaseTypeCache depends on their codetool and the
    node- and basetypecaches of other codetools (=used codetools). The used
    codetools dependencies are saved in the TCodeToolDependencies, which is
    simple an TAVLTree of codetools. This allows to decide, wether the cache of
    a codetools must be rebuild.
  }



  //----------------------------------------------------------------------------
type

  { TGlobalIdentifierTree }

  TGlobalIdentifierTree = class
  private
    FItems: TAVLTree; // tree of PChar;
    FDefaultDataBlockSize: integer;
    FDataBlockSize: integer;
    FDataBlock: Pointer;
    FDataBlockEnd: integer;
    FFullDataBlocks: TFPList; // full blocks of data
    function InternalGetMem(Size: integer): Pointer;
  public
    function AddCopy(Identifier: PChar): PChar;
    function Find(Identifier: PChar): PChar;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function CalcMemSize: PtrUInt;
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
    procedure DisposeNodeCache(NodeCache: TCodeTreeNodeCache);
    function NewNodeCache(AnOwner: TCodeTreeNode): TCodeTreeNodeCache;
  end;

  // memory system for TBaseTypeCache(s)
  TBaseTypeCacheMemManager = class(TCodeToolMemManager)
  protected
    procedure FreeFirstItem; override;
  public
    procedure DisposeBaseTypeCache(BaseTypeCache: TBaseTypeCache);
    function NewBaseTypeCache(AnOwner: TCodeTreeNode): TBaseTypeCache;
  end;

  //----------------------------------------------------------------------------
  // stacks for circle checking
type
  TCodeTreeNodeStackEntry = TCodeTreeNode;

  TCodeTreeNodeStack = record
    Fixedtems: array[0..9] of TCodeTreeNodeStackEntry;
    DynItems: TFPList; // list of TCodeTreeNodeStackEntry
    StackPtr: integer;
  end;
  PCodeTreeNodeStack = ^TCodeTreeNodeStack;

  procedure InitializeNodeStack(NodeStack: PCodeTreeNodeStack);
  function GetNodeStackEntry(NodeStack: PCodeTreeNodeStack;
    Index: integer): TCodeTreeNodeStackEntry;
  procedure AddNodeToStack(NodeStack: PCodeTreeNodeStack;
    NewNode: TCodeTreeNode);
  function NodeExistsInStack(NodeStack: PCodeTreeNodeStack;
    Node: TCodeTreeNode): boolean;
  procedure FinalizeNodeStack(NodeStack: PCodeTreeNodeStack);

const
  ncefAllSearchRanges = [ncefSearchedInAncestors,ncefSearchedInParents];
  NodeCacheEntryFlagNames: array[TNodeCacheEntryFlag] of string = (
      'SearchedInParents', 'SearchedInAncestors'
    );

var
  GlobalIdentifierTree: TGlobalIdentifierTree;
  InterfaceIdentCacheEntryMemManager: TInterfaceIdentCacheEntryMemManager;
  NodeCacheEntryMemManager: TNodeCacheEntryMemManager;
  NodeCacheMemManager: TNodeCacheMemManager;
  BaseTypeCacheMemManager: TBaseTypeCacheMemManager;


function NodeCacheEntryFlagsAsString(Flags: TNodeCacheEntryFlags): string;


implementation


function NodeCacheEntryFlagsAsString(Flags: TNodeCacheEntryFlags): string;
var f: TNodeCacheEntryFlag;
begin
  Result:='';
  for f:=Low(TNodeCacheEntryFlag) to High(TNodeCacheEntryFlag) do begin
    if f in Flags then begin
      if Result<>'' then Result:=rEsult+', ';
      Result:=Result+NodeCacheEntryFlagNames[f];
    end;
  end;
end;

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
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
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
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
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
  if Entry^.Overloaded<>nil then begin
    DisposeEntry(Entry^.Overloaded);
    Entry^.Overloaded:=nil;
  end;
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Entry to Free list
    Entry^.NextEntry:=PInterfaceIdentCacheEntry(FFirstFree);
    PInterfaceIdentCacheEntry(FFirstFree):=Entry;
    inc(FFreeCount);
  end else begin
    // free list full -> free the Entry
    Dispose(Entry);
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
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
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
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

procedure TInterfaceIdentifierCache.ClearMissingIdentifiers;
var
  Node: TAVLTreeNode;
  NextNode: TAVLTreeNode;
  Entry: PInterfaceIdentCacheEntry;
begin
  if FItems=nil then exit;
  Node:=FItems.FindLowest;
  while Node<>nil do begin
    NextNode:=FItems.FindSuccessor(Node);
    Entry:=PInterfaceIdentCacheEntry(Node.Data);
    if Entry^.Node=nil then begin
      FItems.Delete(Node);
      InterfaceIdentCacheEntryMemManager.DisposeEntry(Entry);
    end;
    Node:=NextNode;
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
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TInterfaceIdentifierCache.ConsistencyCheck;
var
  Node: TAVLTreeNode;
  Entry: PInterfaceIdentCacheEntry;
begin
  if FItems<>nil then begin
    if FItems.ConsistencyCheck<>0 then
      RaiseCatchableException('');
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      Entry:=PInterfaceIdentCacheEntry(Node.Data);
      while Entry<>nil do begin
        if (Entry^.Identifier=nil) or (Entry^.Identifier^=#0) then
          RaiseCatchableException('');
        if (Entry^.Node=nil) and Complete then
          RaiseCatchableException('');
        if (Entry^.Overloaded<>nil)
        and (CompareIdentifiers(Entry^.Identifier,Entry^.Overloaded^.Identifier)<>0)
        then begin
          debugln(['TInterfaceIdentifierCache.ConsistencyCheck Entry=',GetIdentifier(Entry^.Identifier),'<>',GetIdentifier(Entry^.Overloaded^.Identifier)]);
          RaiseCatchableException('');
        end;
        Entry:=Entry^.Overloaded;
      end;
      Node:=FItems.FindSuccessor(Node);
    end;
  end;
end;

function TInterfaceIdentifierCache.CalcMemSize: PtrUInt;
var
  Node: TAVLTreeNode;
begin
  Result:=PtrUInt(InstanceSize);
  if FItems<>nil then begin
    inc(Result,FItems.Count*SizeOf(TAVLTreeNode));
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      inc(Result,SizeOf(TInterfaceIdentCacheEntry));
      Node:=FItems.FindSuccessor(Node);
    end;
  end;
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

procedure TInterfaceIdentifierCache.SetComplete(const AValue: boolean);
begin
  if FComplete=AValue then exit;
  FComplete:=AValue;
  if FComplete then
    ClearMissingIdentifiers;
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
  OldNode: TAVLTreeNode;
begin
  if (GetIdentLen(Identifier)=0) then
    RaiseCatchableException('');
  if FItems=nil then
    FItems:=TAVLTree.Create(@CompareTInterfaceIdentCacheEntry);
  OldNode:=FindAVLNode(Identifier);
  NewEntry:=InterfaceIdentCacheEntryMemManager.NewEntry;
  NewEntry^.Identifier:=GlobalIdentifierTree.AddCopy(Identifier);
  NewEntry^.Node:=Node;
  NewEntry^.CleanPos:=CleanPos;
  if OldNode<>nil then begin
    NewEntry^.Overloaded:=PInterfaceIdentCacheEntry(OldNode.Data);
    OldNode.Data:=NewEntry;
  end else begin
    NewEntry^.Overloaded:=nil;
    FItems.Add(NewEntry);
  end;
end;

{ TGlobalIdentifierTree }

procedure TGlobalIdentifierTree.Clear;
var
  i: Integer;
begin
  if FItems<>nil then
    FItems.Clear;
  if FFullDataBlocks<>nil then begin
    for i:=0 to FFullDataBlocks.Count-1 do
      FreeMem(FFullDataBlocks[i]);
    FFullDataBlocks.Clear;
    ReAllocMem(FDataBlock,0);
    FDataBlockEnd:=0;
    FDataBlockSize:=0;
  end;
end;

constructor TGlobalIdentifierTree.Create;
begin
  inherited Create;
  FItems:=TAVLTree.Create(TListSortCompare(@CompareIdentifiers));
  FFullDataBlocks:=TFPList.Create;
  FDefaultDataBlockSize:=256*256*2;
end;

destructor TGlobalIdentifierTree.Destroy;
begin
  Clear;
  FItems.Free;
  FFullDataBlocks.Free;
  inherited Destroy;
end;

function TGlobalIdentifierTree.Count: integer;
begin
  if FItems<>nil then
    Result:=FItems.Count
  else
    Result:=0;
end;

function TGlobalIdentifierTree.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
    +PtrUint(FItems.InstanceSize)
    +PtrUInt(FItems.Count)*PtrUint(TAVLTreeNode.InstanceSize)
    +PtrUInt(FFullDataBlocks.InstanceSize)
    +PtrUInt(FFullDataBlocks.Capacity)*SizeOf(Pointer)
    +PtrUInt(FFullDataBlocks.Count*FDefaultDataBlockSize)
    +PtrUInt(FDataBlockSize);
end;

function TGlobalIdentifierTree.InternalGetMem(Size: integer): Pointer;
begin
  if (FDataBlock=nil) or (FDataBlockEnd+Size>FDataBlockSize) then begin
    // store old block
    FFullDataBlocks.Add(FDataBlock);
    // create a new
    FDataBlockSize:=FDefaultDataBlockSize;
    if FDataBlockSize<Size then
      FDataBlockSize:=Size;
    GetMem(FDataBlock,FDataBlockSize);
    FDataBlockEnd:=0;
  end;
  Result:=FDataBlock+FDataBlockEnd;
  inc(FDataBlockEnd,Size);
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
  Result:=InternalGetMem(Len+1);
  // GetMem(Result,Len+1);
  Move(Identifier^,Result^,Len);
  Result[Len]:=#0;
  FItems.Add(Result);
end;

function TGlobalIdentifierTree.Find(Identifier: PChar): PChar;
var
  comp: integer;
  Node: TAVLTreeNode;
begin
  if FItems<>nil then begin
    Node:=FItems.Root;
    while Node<>nil do begin
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
  Result:=nil;
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

constructor TCodeTreeNodeCache.Create(AnOwner: TCodeTreeNode);
begin
  inherited Create;
  if AnOwner<>nil then BindToOwner(AnOwner);
end;

destructor TCodeTreeNodeCache.Destroy;
begin
  Clear;
  UnbindFromOwner;
  FItems.Free;
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
  SrcTool: TPascalParserTool; CleanStartPos, CleanEndPos: integer;
  NewNode: TCodeTreeNode; NewTool: TPascalParserTool; NewCleanPos: integer;
  Flags: TNodeCacheEntryFlags);

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
    NewEntry^.Flags:=Flags;
    FItems.Add(NewEntry);
  end;
  
var
  OldEntry: PCodeTreeNodeCacheEntry;
  OldNode: TAVLTreeNode;
  NewSearchRangeFlags: TNodeCacheEntryFlags;

  function P2S(CleanPos: integer): string;
  begin
    Result:=SrcTool.CleanPosToStr(CleanPos);
  end;

  function ParamsDebugReport: string;
  var
    s: string;
  begin
    s:=' Ident='+GetIdentifier(Identifier);
    s:=s+' New: Range='+P2S(CleanStartPos)
             +'-'+P2S(CleanEndPos);
    if Owner<>nil then begin
      s:=s+' Owner='+Owner.DescAsString;
      s:=s+' OwnerPos='+P2S(Owner.StartPos);
    end;
    if OldEntry<>nil then begin
      s:=s+' Old: Range='+P2S(OldEntry^.CleanStartPos)
               +'-'+P2S(OldEntry^.CleanEndPos);
      if OldEntry^.NewNode<>nil then begin
        s:=s+' Node='+OldEntry^.NewNode.DescAsString
            +' Pos='+OldEntry^.NewTool.CleanPosToStr(OldEntry^.NewNode.StartPos);
      end else
        s:=s+' Node=nil';
      if OldEntry^.NewTool<>nil then begin
        s:=s+' Tool='+ExtractFilename(OldEntry^.NewTool.MainFilename);
        if OldEntry^.NewNode<>nil then
          s:=s+' Src="'
            +StringToPascalConst(
            copy(OldEntry^.NewTool.Src,OldEntry^.NewNode.StartPos,50))+'"';
      end;
    end;
    if NewNode<>nil then begin
      s:=s+' Node='+NewNode.DescAsString
          +' Pos='+NewTool.CleanPosToStr(NewNode.StartPos);
    end else
      s:=s+' Node=nil';
    if NewTool<>nil then begin
      s:=s+' Tool='+ExtractFileName(NewTool.MainFilename);
      if NewNode<>nil then
        s:=s+' Src="'
          +StringToPascalConst(copy(NewTool.Src,NewNode.StartPos,50))+'"';
    end;
    Result:=s;
  end;
  
  procedure RaiseConflictException(const Msg: string);
  var
    s: string;
  begin
    s:='[TCodeTreeNodeCache.Add] internal error:'+Msg+ParamsDebugReport;
    {$IFDEF HardExceptions}
    DebugLn(s);
    RaiseCatchableException('TCodeTreeNodeCache.Add A');
    {$ELSE}
    raise Exception.Create(s);
    {$ENDIF}
  end;

begin
  OldEntry:=nil;
  // consistency checks
  if CleanStartPos>=CleanEndPos then
    RaiseConflictException('CleanStartPos>=CleanEndPos');
  if (NewNode<>nil) then begin
    if NewTool=nil then
      RaiseConflictException('NewNode<>nil and NewTool=nil');
    if not NewTool.Tree.ContainsNode(NewNode) then
      RaiseConflictException('NewNode is not a node of NewTool');
  end;
  
  {if CompareIdentifiers(Identifier,'FillRect')=0 then begin
    DebugLn('[[[[======================================================');
    DebugLn(['[TCodeTreeNodeCache.Add] Ident=',GetIdentifier(Identifier),
       ' CleanStartPos=',CleanStartPos,' CleanEndPos=',CleanEndPos,
       ' Flags=[',NodeCacheEntryFlagsAsString(Flags),']',
       ' NewNode=',NewNode<>nil
       ]);
    DebugLn('======================================================]]]]');
    CTDumpStack;
  end;}
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
    NewSearchRangeFlags:=(ncefAllSearchRanges * (OldEntry^.Flags+Flags));
    if ((NewNode=OldEntry^.NewNode)
    and (NewTool=OldEntry^.NewTool))
    or ((OldEntry^.NewNode=nil) and (NewSearchRangeFlags<>[])) then
    begin
      // same FindContext or better FindContext with overlapping search ranges
      // -> combine search ranges
      if OldEntry^.CleanStartPos>CleanStartPos then
        OldEntry^.CleanStartPos:=CleanStartPos;
      if OldEntry^.CleanEndPos<CleanEndPos then
        OldEntry^.CleanEndPos:=CleanEndPos;
      OldEntry^.Flags:=NewSearchRangeFlags;
    end else begin
      // different FindContext with overlapping search ranges
      RaiseConflictException('conflicting cache nodes');
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
begin
  Result:=FindNearestAVLNode(Identifier,CleanStartPos,CleanEndPos,true);
  if Result<>nil then begin
    Entry:=PCodeTreeNodeCacheEntry(Result.Data);
    if (CleanStartPos>=Entry^.CleanEndPos)
    or (CleanEndPos<=Entry^.CleanStartPos) then begin
      // node is not in range
      Result:=nil;
    end;
  end;
end;

function TCodeTreeNodeCache.FindNearestAVLNode(Identifier: PChar;
  CleanStartPos, CleanEndPos: integer; InFront: boolean): TAVLTreeNode;
var
  Entry: PCodeTreeNodeCacheEntry;
  comp: integer;
  DirectionSucc: boolean;
  NextNode: TAVLTreeNode;
begin
  if CleanStartPos>CleanEndPos then begin
    raise Exception.Create('[TCodeTreeNodeCache.FindNearestAVLNode]'
      +' internal error: CleanStartPos>CleanEndPos');
  end;
  if (FItems<>nil) and (Identifier<>nil) then begin
    Result:=FItems.Root;
    while Result<>nil do begin
      Entry:=PCodeTreeNodeCacheEntry(Result.Data);
      comp:=CompareIdentifiers(Identifier,Entry^.Identifier);
      if comp<0 then
        Result:=Result.Left
      else if comp>0 then
        Result:=Result.Right
      else begin
        // cached result with identifier found
        // -> check range
        if CleanStartPos>=Entry^.CleanEndPos then begin
          NextNode:=FItems.FindSuccessor(Result);
          DirectionSucc:=true;
        end else if CleanEndPos<=Entry^.CleanStartPos then begin
          NextNode:=FItems.FindPrecessor(Result);
          DirectionSucc:=false;
        end else begin
          // cached result in range found
          exit;
        end;
        while (NextNode<>nil) do begin
          Entry:=PCodeTreeNodeCacheEntry(NextNode.Data);
          if CompareIdentifiers(Identifier,Entry^.Identifier)<>0 then begin
            exit;
          end;
          Result:=NextNode;
          if (CleanStartPos<Entry^.CleanEndPos)
          and (CleanEndPos>Entry^.CleanStartPos) then begin
            // cached result in range found
            exit;
          end;
          if DirectionSucc then
            NextNode:=FItems.FindSuccessor(Result)
          else
            NextNode:=FItems.FindPrecessor(Result);
        end;
        exit;
      end;
    end;
  end else begin
    Result:=nil;
  end;
end;

procedure TCodeTreeNodeCache.ConsistencyCheck;
begin
  if (FItems<>nil) then begin
    if FItems.ConsistencyCheck<>0 then
      raise Exception.Create('');
  end;
  if Owner<>nil then begin
    if Owner.Cache<>Self then
      raise Exception.Create('');
  end;
end;

function TCodeTreeNodeCache.CalcMemSize: PtrUInt;
var
  Node: TAVLTreeNode;
begin
  Result:=PtrUInt(InstanceSize);
  if FItems<>nil then begin
    inc(Result,SizeOf(TAVLTreeNode)*FItems.Count);
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      inc(Result,SizeOf(TCodeTreeNodeCacheEntry));
      Node:=FItems.FindSuccessor(Node);
    end;
  end;
end;

procedure TCodeTreeNodeCache.WriteDebugReport(const Prefix: string);
var Node: TAVLTreeNode;
  Entry: PCodeTreeNodeCacheEntry;
begin
  DebugLn(Prefix+'[TCodeTreeNodeCache.WriteDebugReport] Self='+DbgS(Self));
  if FItems<>nil then begin
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      Entry:=PCodeTreeNodeCacheEntry(Node.Data);
      write(Prefix,' Ident="',GetIdentifier(Entry^.Identifier),'"');
      DbgOut(' Flags=[',NodeCacheEntryFlagsAsString(Entry^.Flags),']');
      DbgOut(' Node=',DbgS(Entry^.NewNode<>nil));
      DebugLn('');
      Node:=FItems.FindSuccessor(Node);
    end;
  end;
  ConsistencyCheck;
end;

procedure TCodeTreeNodeCache.UnbindFromOwner;
begin
  if Owner<>nil then begin
    if Owner.Cache<>Self then
      raise Exception.Create('[TCodeTreeNodeCache.UnbindFromOwner] '
        +' internal error: Owner.Cache<>Self');
    Owner.Cache:=nil;
    Owner:=nil;
  end;
end;

procedure TCodeTreeNodeCache.BindToOwner(NewOwner: TCodeTreeNode);
begin
  if NewOwner<>nil then begin
    if NewOwner.Cache<>nil then
      raise Exception.Create('[TCodeTreeNodeCache.BindToOwner] internal error:'
        +' NewOwner.Cache<>nil');
    NewOwner.Cache:=Self;
  end;
  Owner:=NewOwner;
end;

function TCodeTreeNodeCache.FindNearest(Identifier: PChar; CleanStartPos,
  CleanEndPos: integer; InFront: boolean): PCodeTreeNodeCacheEntry;
var Node: TAVLTreeNode;
begin
  Node:=FindNearestAVLNode(Identifier,CleanStartPos,CleanEndPos,InFront);
  if Node<>nil then
    Result:=PCodeTreeNodeCacheEntry(Node.Data)
  else
    Result:=nil;
end;

function TCodeTreeNodeCache.FindInRange(Identifier: PChar; CleanStartPos,
  CleanEndPos: integer): PCodeTreeNodeCacheEntry;
var Node: TAVLTreeNode;
begin
  Node:=FindAVLNodeInRange(Identifier,CleanStartPos,CleanEndPos);
  if Node<>nil then
    Result:=PCodeTreeNodeCacheEntry(Node.Data)
  else
    Result:=nil;
end;

{ TNodeCacheMemManager }

procedure TNodeCacheMemManager.DisposeNodeCache(NodeCache: TCodeTreeNodeCache);
begin
  NodeCache.UnbindFromOwner;
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Entry to Free list
    NodeCache.Next:=TCodeTreeNodeCache(FFirstFree);
    TCodeTreeNodeCache(FFirstFree):=NodeCache;
    inc(FFreeCount);
  end else begin
    // free list full -> free the NodeCache
    NodeCache.Free;
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

procedure TNodeCacheMemManager.FreeFirstItem;
var NodeCache: TCodeTreeNodeCache;
begin
  NodeCache:=TCodeTreeNodeCache(FFirstFree);
  TCodeTreeNodeCache(FFirstFree):=NodeCache.Next;
  NodeCache.Free;
end;

function TNodeCacheMemManager.NewNodeCache(
  AnOwner: TCodeTreeNode): TCodeTreeNodeCache;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TCodeTreeNodeCache(FFirstFree);
    TCodeTreeNodeCache(FFirstFree):=Result.Next;
    Result.Clear;
    Result.BindToOwner(AnOwner);
    dec(FFreeCount);
  end else begin
    // free list empty -> create new NodeCache
    Result:=TCodeTreeNodeCache.Create(AnOwner);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

//------------------------------------------------------------------------------

procedure InitializeNodeStack(NodeStack: PCodeTreeNodeStack);
begin
  NodeStack^.StackPtr:=0;
  NodeStack^.DynItems:=nil;
end;

function GetNodeStackEntry(NodeStack: PCodeTreeNodeStack;
  Index: integer): TCodeTreeNodeStackEntry;
begin
  if Index<=High(NodeStack^.Fixedtems) then begin
    Result:=NodeStack^.Fixedtems[Index];
  end else begin
    Result:=TCodeTreeNodeStackEntry(
                       NodeStack^.DynItems[Index-High(NodeStack^.Fixedtems)-1]);
  end;
end;

procedure AddNodeToStack(NodeStack: PCodeTreeNodeStack;
  NewNode: TCodeTreeNode);
begin
  if (NodeStack^.StackPtr<=High(NodeStack^.Fixedtems)) then begin
    NodeStack^.Fixedtems[NodeStack^.StackPtr]:=NewNode;
  end else begin
    if NodeStack^.DynItems=nil then begin
      NodeStack^.DynItems:=TFPList.Create;
    end;
    NodeStack^.DynItems.Add(NewNode);
  end;
  inc(NodeStack^.StackPtr);
end;

function NodeExistsInStack(NodeStack: PCodeTreeNodeStack;
  Node: TCodeTreeNode): boolean;
var i: integer;
begin
  Result:=true;
  i:=0;
  while i<NodeStack^.StackPtr do begin
    if i<=High(NodeStack^.Fixedtems) then begin
      if NodeStack^.Fixedtems[i]=Node then exit;
    end else begin
      if NodeStack^.DynItems[i-High(NodeStack^.Fixedtems)-1]=Pointer(Node) then
        exit;
    end;
    inc(i);
  end;
  Result:=false;
end;

procedure FinalizeNodeStack(NodeStack: PCodeTreeNodeStack);
begin
  NodeStack^.DynItems.Free;
end;


{ TBaseTypeCacheMemManager }

procedure TBaseTypeCacheMemManager.DisposeBaseTypeCache(
  BaseTypeCache: TBaseTypeCache);
begin
  BaseTypeCache.UnbindFromOwner;
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add Entry to Free list
    BaseTypeCache.Next:=TBaseTypeCache(FFirstFree);
    TBaseTypeCache(FFirstFree):=BaseTypeCache;
    inc(FFreeCount);
  end else begin
    // free list full -> free the BaseType
    BaseTypeCache.Free;
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

procedure TBaseTypeCacheMemManager.FreeFirstItem;
var BaseTypeCache: TBaseTypeCache;
begin
  BaseTypeCache:=TBaseTypeCache(FFirstFree);
  TBaseTypeCache(FFirstFree):=BaseTypeCache.Next;
  BaseTypeCache.Free;
end;

function TBaseTypeCacheMemManager.NewBaseTypeCache(
  AnOwner: TCodeTreeNode): TBaseTypeCache;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=TBaseTypeCache(FFirstFree);
    TBaseTypeCache(FFirstFree):=Result.Next;
    Result.BindToOwner(AnOwner);
    dec(FFreeCount);
  end else begin
    // free list empty -> create new BaseType
    Result:=TBaseTypeCache.Create(AnOwner);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

{ TBaseTypeCache }

procedure TBaseTypeCache.BindToOwner(NewOwner: TCodeTreeNode);
begin
  if NewOwner<>nil then begin
    if NewOwner.Cache<>nil then
      raise Exception.Create('[TBaseTypeCache.BindToOwner] internal error:'
        +' NewOwner.Cache<>nil');
    NewOwner.Cache:=Self;
  end;
  Owner:=NewOwner;
end;

constructor TBaseTypeCache.Create(AnOwner: TCodeTreeNode);
begin
  inherited Create;
  if AnOwner<>nil then BindToOwner(AnOwner);
end;

destructor TBaseTypeCache.Destroy;
begin
  UnbindFromOwner;
  inherited Destroy;
end;

function TBaseTypeCache.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize);
end;

procedure TBaseTypeCache.UnbindFromOwner;
begin
  if Owner<>nil then begin
    if Owner.Cache<>Self then
      raise Exception.Create('[TBaseTypeCache.UnbindFromOwner] '
        +' internal error: Owner.Cache<>Self');
    Owner.Cache:=nil;
    Owner:=nil;
  end;
end;

//------------------------------------------------------------------------------

procedure InternalInit;
begin
  GlobalIdentifierTree:=TGlobalIdentifierTree.Create;
  InterfaceIdentCacheEntryMemManager:=TInterfaceIdentCacheEntryMemManager.Create;
  NodeCacheEntryMemManager:=TNodeCacheEntryMemManager.Create;
  NodeCacheMemManager:=TNodeCacheMemManager.Create;
  BaseTypeCacheMemManager:=TBaseTypeCacheMemManager.Create;
end;

procedure InternalFinal;
begin
  BaseTypeCacheMemManager.Free;
  BaseTypeCacheMemManager:=nil;
  NodeCacheMemManager.Free;
  NodeCacheMemManager:=nil;
  NodeCacheEntryMemManager.Free;
  NodeCacheEntryMemManager:=nil;
  InterfaceIdentCacheEntryMemManager.Free;
  InterfaceIdentCacheEntryMemManager:=nil;
  GlobalIdentifierTree.Free;
  GlobalIdentifierTree:=nil;
end;

initialization
  InternalInit;

finalization
  InternalFinal;


end.

