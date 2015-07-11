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
    An arbitrary graph for TCodeTreeNode.
}
unit CodeGraph; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeTree, FileProcs, AVL_Tree;
  
type

  { TCodeGraphNode }

  TCodeGraphNode = class
  private
    FInternalFlags: integer;
  public
    Node: TCodeTreeNode;
    InTree: TAVLTree;// tree of TCodeGraphEdge sorted for FromNode (ToNode = Self)
    OutTree: TAVLTree;// tree of TCodeGraphEdge sorted for ToNode (FromNode = Self)
    Data: Pointer;  // custom data
    Flags: cardinal;// custom flags
    function OutTreeCount: integer;
    function InTreeCount: integer;
  end;
  TCodeGraphNodeClass = class of TCodeGraphNode;
  
  PCodeGraphEdgeKey = ^TCodeGraphEdgeKey;
  TCodeGraphEdgeKey = record
    FromNode: TCodeTreeNode;
    ToNode: TCodeTreeNode;
  end;

  { TCodeGraphEdge }

  TCodeGraphEdge = class
  private
    FInternalFlags: integer;
  public
    FromNode: TCodeGraphNode;
    ToNode: TCodeGraphNode;
    Data: Pointer;  // custom data
    Flags: cardinal;// custom flags
  end;
  TCodeGraphEdgeClass = class of TCodeGraphEdge;

  { TCodeGraph }

  TCodeGraph = class
  private
    FEdgeClass: TCodeGraphEdgeClass;
    FNodeClass: TCodeGraphNodeClass;
    procedure ClearInternalNodeFlags;
  public
    Nodes: TAVLTree; // tree of TCodeGraphNode sorted for Node
    Edges: TAVLTree; // tree of TCodeGraphEdge sorted for FromNode,ToNode
    constructor Create(ANodeClass: TCodeGraphNodeClass = nil;
                       AnEdgeClass: TCodeGraphEdgeClass = nil);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearNodeFlags;
    procedure ClearEdgeFlags;
    procedure Assign(Source: TCodeGraph);
    function CreateCopy: TCodeGraph;
    function AddGraphNode(Node: TCodeTreeNode): TCodeGraphNode;
    function GetGraphNode(Node: TCodeTreeNode; CreateIfNotExists: boolean
                          ): TCodeGraphNode;
    procedure DeleteGraphNode(Node: TCodeTreeNode);
    function FindGraphNodeWithNumberOfOutEdges(MinNumber, MaxNumber: integer
                                               ): TCodeGraphNode;
    function FindGraphNodeWithNumberOfInEdges(MinNumber, MaxNumber: integer
                                              ): TCodeGraphNode;

    function PathExists(FromNode, ToNode: TCodeTreeNode): boolean;
    function AddEdge(FromNode, ToNode: TCodeTreeNode): TCodeGraphEdge;
    function GetEdge(FromNode, ToNode: TCodeTreeNode;
                     CreateIfNotExists: boolean): TCodeGraphEdge;
    procedure DeleteEdge(FromNode, ToNode: TCodeTreeNode);
    procedure DeleteEdge(Edge: TCodeGraphEdge);
    procedure DeleteSelfCircles;
    procedure CombineNodes(ListOfGraphNodes: TFPList; GraphNode: TCodeGraphNode);
    function GetTopologicalSortedList(out ListOfGraphNodes: TFPList;
                    InEdgeDirection, // true=start with source nodes (no InEdges)
                    SetTopologicalLvl,// true=set Node.Flags to level
                    SortForStartPos: boolean// true=secondary sort order is Node.StartPos
                    ): TCodeGraphEdge;// is a circle edge (if found, else nil)
    procedure GetMaximumCircle(StartNode: TCodeGraphNode;
                               out ListOfGraphNodes: TFPList);

    function FindAVLNodeOfNode(Node: TCodeTreeNode): TAVLTreeNode;
    function FindAVLNodeOfToNode(InTree: TAVLTree; ToNode: TCodeTreeNode
                                 ): TAVLTreeNode;
    function FindAVLNodeOfFromNode(OutTree: TAVLTree; FromNode: TCodeTreeNode
                                   ): TAVLTreeNode;
    function FindAVLNodeOfEdge(FromNode, ToNode: TCodeTreeNode): TAVLTreeNode;
    
    property NodeClass: TCodeGraphNodeClass read FNodeClass;
    property EdgeClass: TCodeGraphEdgeClass read FEdgeClass;

    procedure ConsistencyCheck;
  end;
  
function CompareGraphNodeByNode(GraphNode1, GraphNode2: Pointer): integer;
function CompareNodeWithGraphNodeNode(p, GraphNode: Pointer): integer;

function CompareGraphEdgeByFromNode(GraphEdge1, GraphEdge2: Pointer): integer;
function CompareNodeWithGraphEdgeFromNode(p, GraphEdge: Pointer): integer;
function CompareGraphEdgeByToNode(GraphEdge1, GraphEdge2: Pointer): integer;
function CompareNodeWithGraphEdgeToNode(p, GraphEdge: Pointer): integer;

function CompareGraphEdgeByNodes(GraphEdge1, GraphEdge2: Pointer): integer;
function CompareEdgeKeyWithGraphEdge(EdgeKey, GraphEdge: Pointer): integer;

implementation

function CompareGraphNodeByNode(GraphNode1, GraphNode2: Pointer): integer;
var
  Node1: TCodeTreeNode;
  Node2: TCodeTreeNode;
begin
  Node1:=TCodeGraphNode(GraphNode1).Node;
  Node2:=TCodeGraphNode(GraphNode2).Node;
  if Pointer(Node1)>Pointer(Node2) then
    Result:=1
  else if Pointer(Node1)<Pointer(Node2) then
    Result:=-1
  else
    Result:=0;
  //DebugLn(['CompareGraphNodeByNode ',Node1.DescAsString,' ',Node2.DescAsString,' ',Result]);
end;

function CompareNodeWithGraphNodeNode(p, GraphNode: Pointer): integer;
var
  Node: TCodeTreeNode;
begin
  Node:=TCodeGraphNode(GraphNode).Node;
  if p>Pointer(Node) then
    Result:=1
  else if p<Pointer(Node) then
    Result:=-1
  else
    Result:=0;
  //DebugLn(['ComparePointerWithGraphNodeNode ',TCodeTreeNode(p).DescAsString,' ',Node.DescAsString,' ',Result]);
end;

function CompareGraphEdgeByFromNode(GraphEdge1, GraphEdge2: Pointer): integer;
var
  Node1: TCodeTreeNode;
  Node2: TCodeTreeNode;
begin
  Node1:=TCodeGraphEdge(GraphEdge1).FromNode.Node;
  Node2:=TCodeGraphEdge(GraphEdge2).FromNode.Node;
  if Pointer(Node1)>Pointer(Node2) then
    Result:=1
  else if Pointer(Node1)<Pointer(Node2) then
    Result:=-1
  else
    Result:=0;
end;

function CompareNodeWithGraphEdgeFromNode(p, GraphEdge: Pointer): integer;
var
  Node: TCodeTreeNode;
begin
  Node:=TCodeGraphEdge(GraphEdge).FromNode.Node;
  if p>Pointer(Node) then
    Result:=1
  else if p<Pointer(Node) then
    Result:=-1
  else
    Result:=0;
end;

function CompareGraphEdgeByToNode(GraphEdge1, GraphEdge2: Pointer): integer;
var
  Node1: TCodeTreeNode;
  Node2: TCodeTreeNode;
begin
  Node1:=TCodeGraphEdge(GraphEdge1).ToNode.Node;
  Node2:=TCodeGraphEdge(GraphEdge2).ToNode.Node;
  if Pointer(Node1)>Pointer(Node2) then
    Result:=1
  else if Pointer(Node1)<Pointer(Node2) then
    Result:=-1
  else
    Result:=0;
end;

function CompareNodeWithGraphEdgeToNode(p, GraphEdge: Pointer): integer;
var
  Node: TCodeTreeNode;
begin
  Node:=TCodeGraphEdge(GraphEdge).ToNode.Node;
  if p>Pointer(Node) then
    Result:=1
  else if p<Pointer(Node) then
    Result:=-1
  else
    Result:=0;
end;

function CompareGraphEdgeByNodes(GraphEdge1, GraphEdge2: Pointer): integer;
var
  Node1: TCodeTreeNode;
  Node2: TCodeTreeNode;
begin
  Node1:=TCodeGraphEdge(GraphEdge1).FromNode.Node;
  Node2:=TCodeGraphEdge(GraphEdge2).FromNode.Node;
  if Pointer(Node1)>Pointer(Node2) then
    exit(1)
  else if Pointer(Node1)<Pointer(Node2) then
    exit(-1);
  Node1:=TCodeGraphEdge(GraphEdge1).ToNode.Node;
  Node2:=TCodeGraphEdge(GraphEdge2).ToNode.Node;
  if Pointer(Node1)>Pointer(Node2) then
    exit(1)
  else if Pointer(Node1)<Pointer(Node2) then
    exit(-1);
  Result:=0;
end;

function CompareEdgeKeyWithGraphEdge(EdgeKey, GraphEdge: Pointer): integer;
var
  Key: PCodeGraphEdgeKey;
  Edge: TCodeGraphEdge;
  Node1: TCodeTreeNode;
  Node2: TCodeTreeNode;
begin
  Key:=PCodeGraphEdgeKey(EdgeKey);
  Edge:=TCodeGraphEdge(GraphEdge);
  Node1:=Key^.FromNode;
  Node2:=Edge.FromNode.Node;
  if Pointer(Node1)>Pointer(Node2) then
    exit(1)
  else if Pointer(Node1)<Pointer(Node2) then
    exit(-1);
  Node1:=Key^.ToNode;
  Node2:=Edge.ToNode.Node;
  if Pointer(Node1)>Pointer(Node2) then
    exit(1)
  else if Pointer(Node1)<Pointer(Node2) then
    exit(-1);
  Result:=0;
end;

function CompareGraphNodesForTopoLvlAndStartPos(
  GraphNode1, GraphNode2: Pointer): integer;
// 1 if lower Level (FInternalFlags) or if lvl is the same and lower Node.StartPos
var
  Level1: LongInt;
  Level2: LongInt;
  StartPos1: LongInt;
  StartPos2: LongInt;
begin
  Level1:=TCodeGraphNode(GraphNode1).FInternalFlags;
  Level2:=TCodeGraphNode(GraphNode2).FInternalFlags;
  if Level1<Level2 then
    Result:=1
  else if Level1>Level2 then
    Result:=-1
  else begin
    StartPos1:=TCodeGraphNode(GraphNode1).Node.StartPos;
    StartPos2:=TCodeGraphNode(GraphNode2).Node.StartPos;
    if StartPos1<StartPos2 then
      Result:=1
    else if StartPos1>StartPos2 then
      Result:=-1
    else
      Result:=0;
  end;
end;

{ TCodeGraph }

procedure TCodeGraph.ClearInternalNodeFlags;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=Nodes.FindLowest;
  while AVLNode<>nil do begin
    TCodeGraphNode(AVLNode.Data).FInternalFlags:=0;
    AVLNode:=Nodes.FindSuccessor(AVLNode);
  end;
end;

constructor TCodeGraph.Create(ANodeClass: TCodeGraphNodeClass;
  AnEdgeClass: TCodeGraphEdgeClass);
begin
  if ANodeClass<>nil then
    FNodeClass:=ANodeClass
  else
    FNodeClass:=TCodeGraphNode;
  if AnEdgeClass<>nil then
    FEdgeClass:=AnEdgeClass
  else
    FEdgeClass:=TCodeGraphEdge;
  Nodes:=TAVLTree.Create(@CompareGraphNodeByNode);
  Edges:=TAVLTree.Create(@CompareGraphEdgeByNodes);
end;

destructor TCodeGraph.Destroy;
begin
  Clear;
  FreeAndNil(Nodes);
  FreeAndNil(Edges);
  inherited Destroy;
end;

procedure TCodeGraph.Clear;
var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
begin
  AVLNode:=Nodes.FindLowest;
  while AVLNode<>nil do begin
    GraphNode:=TCodeGraphNode(AVLNode.Data);
    if GraphNode.InTree<>nil then begin
      GraphNode.InTree.FreeAndClear;// free the TCodeGraphEdge objects
      FreeAndNil(GraphNode.InTree);// free the InTree
    end;
    if GraphNode.OutTree<>nil then
      FreeAndNil(GraphNode.OutTree);// free the OutTree
    AVLNode:=Nodes.FindSuccessor(AVLNode);
  end;
  Nodes.FreeAndClear;// free the TCodeGraphNode objects
  Edges.Clear;
end;

procedure TCodeGraph.ClearNodeFlags;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=Nodes.FindLowest;
  while AVLNode<>nil do begin
    TCodeGraphNode(AVLNode.Data).Flags:=0;
    TCodeGraphNode(AVLNode.Data).FInternalFlags:=0;
    AVLNode:=Nodes.FindSuccessor(AVLNode);
  end;
end;

procedure TCodeGraph.ClearEdgeFlags;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=Edges.FindLowest;
  while AVLNode<>nil do begin
    TCodeGraphEdge(AVLNode.Data).Flags:=0;
    TCodeGraphEdge(AVLNode.Data).FInternalFlags:=0;
    AVLNode:=Edges.FindSuccessor(AVLNode);
  end;
end;

procedure TCodeGraph.Assign(Source: TCodeGraph);
var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
  SrcGraphNode: TCodeGraphNode;
  SrcGraphEdge: TCodeGraphEdge;
  GraphEdge: TCodeGraphEdge;
begin
  if Source=Self then exit;
  Clear;
  FNodeClass:=Source.FNodeClass;
  FEdgeClass:=Source.FEdgeClass;
  // copy nodes
  AVLNode:=Source.Nodes.FindLowest;
  while AVLNode<>nil do begin
    SrcGraphNode:=TCodeGraphNode(AVLNode.Data);
    GraphNode:=AddGraphNode(SrcGraphNode.Node);
    GraphNode.Data:=SrcGraphNode.Data;
    AVLNode:=Source.Nodes.FindSuccessor(AVLNode);
  end;
  // copy edges
  AVLNode:=Source.Edges.FindLowest;
  while AVLNode<>nil do begin
    SrcGraphEdge:=TCodeGraphEdge(AVLNode.Data);
    GraphEdge:=AddEdge(SrcGraphEdge.FromNode.Node,SrcGraphEdge.ToNode.Node);
    GraphEdge.Data:=SrcGraphEdge.Data;
    AVLNode:=Source.Edges.FindSuccessor(AVLNode);
  end;
end;

function TCodeGraph.CreateCopy: TCodeGraph;
begin
  Result:=TCodeGraph.Create;
  Result.Assign(Self);
end;

function TCodeGraph.AddGraphNode(Node: TCodeTreeNode): TCodeGraphNode;
begin
  Result:=GetGraphNode(Node,true);
end;

function TCodeGraph.GetGraphNode(Node: TCodeTreeNode; CreateIfNotExists: boolean
  ): TCodeGraphNode;
var
  AVLNode: TAVLTreeNode;
begin
  if Node=nil then exit(nil);
  AVLNode:=FindAVLNodeOfNode(Node);
  if AVLNode<>nil then
    Result:=TCodeGraphNode(AVLNode.Data)
  else if CreateIfNotExists then begin
    Result:=FNodeClass.Create;
    Result.Node:=Node;
    Nodes.Add(Result);
  end else
    Result:=nil;
end;

procedure TCodeGraph.DeleteGraphNode(Node: TCodeTreeNode);
var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
  OutAVLNode: TAVLTreeNode;
  Edge: TCodeGraphEdge;
  InTree: TAVLTree;
  OutTree: TAVLTree;
  InAVLNode: TAVLTreeNode;
begin
  AVLNode:=FindAVLNodeOfNode(Node);
  if AVLNode=nil then exit;
  GraphNode:=TCodeGraphNode(AVLNode.Data);
  OutTree:=GraphNode.OutTree;
  if OutTree<>nil then begin
    // remove all edges coming from this Node
    OutAVLNode:=OutTree.FindLowest;
    while OutAVLNode<>nil do begin
      Edge:=TCodeGraphEdge(OutAVLNode.Data);
      InTree:=Edge.ToNode.InTree;
      InTree.Remove(Edge);
      Edges.Remove(Edge);
      Edge.Free;
      OutAVLNode:=OutTree.FindSuccessor(OutAVLNode);
    end;
    OutTree.Free;
  end;
  InTree:=GraphNode.InTree;
  if InTree<>nil then begin
    // remove all edges going to this Node
    InAVLNode:=InTree.FindLowest;
    while InAVLNode<>nil do begin
      Edge:=TCodeGraphEdge(InAVLNode.Data);
      OutTree:=Edge.FromNode.OutTree;
      OutTree.Remove(Edge);
      Edges.Remove(Edge);
      Edge.Free;
      InAVLNode:=InTree.FindSuccessor(InAVLNode);
    end;
    InTree.Free;
  end;
  Nodes.Delete(AVLNode);
  GraphNode.Free;
end;

function TCodeGraph.FindGraphNodeWithNumberOfOutEdges(MinNumber,
  MaxNumber: integer): TCodeGraphNode;
var
  AVLNode: TAVLTreeNode;
  Cnt: LongInt;
begin
  AVLNode:=Nodes.FindLowest;
  while AVLNode<>nil do begin
    Result:=TCodeGraphNode(AVLNode.Data);
    Cnt:=Result.OutTreeCount;
    if ((MinNumber<0) or (MinNumber<=Cnt))
    and ((MaxNumber<0) or (MaxNumber>=Cnt)) then
      exit;
    AVLNode:=Nodes.FindSuccessor(AVLNode);
  end;
  Result:=nil;
end;

function TCodeGraph.FindGraphNodeWithNumberOfInEdges(MinNumber,
  MaxNumber: integer): TCodeGraphNode;
var
  AVLNode: TAVLTreeNode;
  Cnt: LongInt;
begin
  AVLNode:=Nodes.FindLowest;
  while AVLNode<>nil do begin
    Result:=TCodeGraphNode(AVLNode.Data);
    Cnt:=Result.InTreeCount;
    if ((MinNumber<0) or (MinNumber<=Cnt))
    and ((MaxNumber<0) or (MaxNumber>=Cnt)) then
      exit;
    AVLNode:=Nodes.FindSuccessor(AVLNode);
  end;
  Result:=nil;
end;

function TCodeGraph.PathExists(FromNode, ToNode: TCodeTreeNode): boolean;

  function Search(GraphNode: TCodeGraphNode): boolean;
  var
    AVLNode: TAVLTreeNode;
    GraphEdge: TCodeGraphEdge;
  begin
    Result:=false;
    if GraphNode=nil then exit;
    if GraphNode.Node=ToNode then exit(true);
    if GraphNode.FInternalFlags>0 then exit;
    GraphNode.FInternalFlags:=1;
    if GraphNode.OutTree=nil then exit;
    AVLNode:=GraphNode.OutTree.FindLowest;
    while AVLNode<>nil do begin
      GraphEdge:=TCodeGraphEdge(AVLNode.Data);
      if Search(GraphEdge.ToNode) then exit(true);
      AVLNode:=GraphNode.OutTree.FindSuccessor(AVLNode);
    end;
  end;

begin
  Result:=false;
  ClearInternalNodeFlags;
  Result:=Search(GetGraphNode(FromNode,false));
end;

function TCodeGraph.AddEdge(FromNode, ToNode: TCodeTreeNode): TCodeGraphEdge;
begin
  Result:=GetEdge(FromNode,ToNode,true);
end;

procedure TCodeGraph.DeleteEdge(FromNode, ToNode: TCodeTreeNode);
begin
  DeleteEdge(GetEdge(FromNode,ToNode,false));
end;

procedure TCodeGraph.DeleteEdge(Edge: TCodeGraphEdge);
begin
  if Edge=nil then exit;
  Edge.FromNode.OutTree.Remove(Edge);
  Edge.ToNode.InTree.Remove(Edge);
  Edges.Remove(Edge);
  Edge.Free;
end;

procedure TCodeGraph.DeleteSelfCircles;
var
  AVLNode: TAVLTreeNode;
  NextNode: TAVLTreeNode;
  Edge: TCodeGraphEdge;
begin
  AVLNode:=Edges.FindLowest;
  while AVLNode<>nil do begin
    NextNode:=Edges.FindSuccessor(AVLNode);
    Edge:=TCodeGraphEdge(AVLNode.Data);
    if Edge.FromNode=Edge.ToNode then
      DeleteEdge(Edge);
    AVLNode:=NextNode;
  end;
end;

procedure TCodeGraph.CombineNodes(ListOfGraphNodes: TFPList;
  GraphNode: TCodeGraphNode);
// combines all nodes in ListOfGraphNodes into the super node Node
var
  i: Integer;
  CurGraphNode: TCodeGraphNode;
  AVLNode: TAVLTreeNode;
  Edge: TCodeGraphEdge;
begin
  if ListOfGraphNodes=nil then exit;
  // create for each edge to/from the List an edge to the super node
  for i:=0 to ListOfGraphNodes.Count-1 do begin
    CurGraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
    if CurGraphNode=GraphNode then continue;
    if CurGraphNode.InTree<>nil then begin
      AVLNode:=CurGraphNode.InTree.FindLowest;
      while AVLNode<>nil do begin
        Edge:=TCodeGraphEdge(AVLNode.Data);
        // add an edge to super node
        if Edge.FromNode<>GraphNode then
          AddEdge(Edge.FromNode.Node,GraphNode.Node);
        AVLNode:=CurGraphNode.InTree.FindSuccessor(AVLNode);
      end;
    end;
    if CurGraphNode.OutTree<>nil then begin
      AVLNode:=CurGraphNode.OutTree.FindLowest;
      while AVLNode<>nil do begin
        Edge:=TCodeGraphEdge(AVLNode.Data);
        // add an edge from super node
        if Edge.ToNode<>GraphNode then
          AddEdge(GraphNode.Node,Edge.ToNode.Node);
        AVLNode:=CurGraphNode.OutTree.FindSuccessor(AVLNode);
      end;
    end;
  end;
  // delete list nodes
  for i:=0 to ListOfGraphNodes.Count-1 do begin
    CurGraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
    if CurGraphNode=GraphNode then continue;
    // remove list node
    DeleteGraphNode(CurGraphNode.Node);
  end;
end;

function TCodeGraph.GetTopologicalSortedList(out ListOfGraphNodes: TFPList;
  InEdgeDirection, SetTopologicalLvl, SortForStartPos: boolean): TCodeGraphEdge;
{ returns nil if there is no circle
  else: returns a circle edge
  ListOfTGraphNodes are all those GraphNodes, that could be sorted topologically
  if InEdgeDirection=true then the list starts with the nodes without in-edges
  else the list start with the nodes without out-edges
  
  if SetTopologicalLvl=true then the GraphNode.Flags will be set to the
    topological level, starting at 0 for nodes with no in edges.
  
  if SortForStartPos=true the nodes will be sorted for Node.StartPos
    as secondary order, keeping the topologically order
}
var
  NodeQueue: array of TCodeGraphNode;
  QueueStart: Integer;
  QueueEnd: Integer;
  
  procedure AddNode(GraphNode: TCodeGraphNode);
  begin
    //DebugLn(['AddNode ',GraphNode.Node.DescAsString]);
    NodeQueue[QueueEnd]:=GraphNode;
    inc(QueueEnd);
  end;
  
var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
  GraphEdge: TCodeGraphEdge;
  CurGraphNode: TCodeGraphNode;
  EdgeAVLNode: TAVLTreeNode;
  i: Integer;
  CurTree: TAVLTree;
  SortedNodes: TAVLTree;
begin
  //DebugLn(['TCodeGraph.GetTopologicalSortedList START']);
  Result:=nil;
  ListOfGraphNodes:=TFPList.Create;
  if (Nodes=nil) or (Nodes.Count=0) then exit;
  ListOfGraphNodes.Capacity:=Nodes.Count;

  try
    // init queue
    SetLength(NodeQueue,Nodes.Count);
    QueueStart:=0;
    QueueEnd:=0;
    // add all nodes without incoming edges and set all FInternalFlags to
    // the number of incoming nodes
    AVLNode:=Nodes.FindLowest;
    while AVLNode<>nil do begin
      GraphNode:=TCodeGraphNode(AVLNode.Data);
      if InEdgeDirection then
        CurTree:=GraphNode.InTree
      else
        CurTree:=GraphNode.OutTree;
      if (CurTree=nil) or (CurTree.Count=0) then begin
        GraphNode.FInternalFlags:=0;
        AddNode(GraphNode);
      end else begin
        GraphNode.FInternalFlags:=CurTree.Count;
      end;
      AVLNode:=Nodes.FindSuccessor(AVLNode);
    end;
    
    // add all nodes without incoming edges from the queue into the list
    while QueueStart<>QueueEnd do begin
      GraphNode:=NodeQueue[QueueStart];
      inc(QueueStart);
      ListOfGraphNodes.Add(GraphNode);
      // update the FInternalFlags counter
      if InEdgeDirection then
        CurTree:=GraphNode.OutTree
      else
        CurTree:=GraphNode.InTree;
      if (CurTree<>nil) then begin
        EdgeAVLNode:=CurTree.FindLowest;
        while EdgeAVLNode<>nil do begin
          GraphEdge:=TCodeGraphEdge(EdgeAVLNode.Data);
          if InEdgeDirection then
            CurGraphNode:=GraphEdge.ToNode
          else
            CurGraphNode:=GraphEdge.FromNode;
          dec(CurGraphNode.FInternalFlags);
          if CurGraphNode.FInternalFlags=0 then
            // a new node has no incoming edges => add to the queue
            AddNode(CurGraphNode);
          EdgeAVLNode:=CurTree.FindSuccessor(EdgeAVLNode);
        end;
      end;
    end;
    
    if ListOfGraphNodes.Count<Nodes.Count then begin
      // there is a circle
      // find a node of a circle
      AVLNode:=Nodes.FindLowest;
      while (AVLNode<>nil) and (Result=nil) do begin
        GraphNode:=TCodeGraphNode(AVLNode.Data);
        if InEdgeDirection then
          CurTree:=GraphNode.OutTree
        else
          CurTree:=GraphNode.InTree;
        if (GraphNode.FInternalFlags>0) and (CurTree<>nil) and (CurTree.Count>0)
        then begin
          // find an edge of a circle
          EdgeAVLNode:=CurTree.FindLowest;
          while EdgeAVLNode<>nil do begin
            GraphEdge:=TCodeGraphEdge(EdgeAVLNode.Data);
            if (InEdgeDirection and (GraphEdge.ToNode.OutTreeCount>0))
            or ((not InEdgeDirection) and (GraphEdge.FromNode.InTreeCount>0))
            then begin
              Result:=GraphEdge;
              break;
            end;
            EdgeAVLNode:=CurTree.FindSuccessor(EdgeAVLNode);
          end;
        end;
        AVLNode:=Nodes.FindSuccessor(AVLNode);
      end;
    end;

    if (ListOfGraphNodes.Count>=1) then begin
      if SortForStartPos or SetTopologicalLvl then begin
        // calculate the topological levels
        if SortForStartPos then
          SortedNodes:=TAVLTree.Create(@CompareGraphNodesForTopoLvlAndStartPos)
        else
          SortedNodes:=nil;
        ClearInternalNodeFlags;
        for i:=0 to ListOfGraphNodes.Count-1 do begin
          GraphNode:=TCodeGraphNode(ListOfGraphNodes[i]);
          // find the maximum incoming topological level
          GraphNode.FInternalFlags:=0;
          if InEdgeDirection then
            CurTree:=GraphNode.InTree
          else
            CurTree:=GraphNode.OutTree;
          if (CurTree<>nil) then begin
            EdgeAVLNode:=CurTree.FindLowest;
            while EdgeAVLNode<>nil do begin
              GraphEdge:=TCodeGraphEdge(EdgeAVLNode.Data);
              if InEdgeDirection then
                CurGraphNode:=GraphEdge.FromNode
              else
                CurGraphNode:=GraphEdge.ToNode;
              if GraphNode.FInternalFlags<=CurGraphNode.FInternalFlags then
                // set the level to one higher than the maximum
                GraphNode.FInternalFlags:=CurGraphNode.FInternalFlags+1;
              EdgeAVLNode:=CurTree.FindSuccessor(EdgeAVLNode);
            end;
          end;
          // now level of this node is complete
          if SetTopologicalLvl then
            GraphNode.Flags:=GraphNode.FInternalFlags;
          if SortForStartPos then
            SortedNodes.Add(GraphNode);
        end;
        if SortForStartPos then begin
          // rebuild list with sorted nodes
          ListOfGraphNodes.Clear;
          AVLNode:=SortedNodes.FindLowest;
          while AVLNode<>nil do begin
            ListOfGraphNodes.Add(AVLNode.Data);
            AVLNode:=SortedNodes.FindSuccessor(AVLNode);
          end;
          SortedNodes.Free;
        end;
      end;
    end;
  finally
    SetLength(NodeQueue,0);
  end;
  //DebugLn(['TCodeGraph.GetTopologicalSortedList END']);
end;

procedure TCodeGraph.GetMaximumCircle(StartNode: TCodeGraphNode; out
  ListOfGraphNodes: TFPList);

  procedure AddNode(ANode: TCodeGraphNode);
  begin
    ANode.FInternalFlags:=2;
    ListOfGraphNodes.Add(ANode);
  end;
  
  procedure MarkReachableNodes(Node: TCodeGraphNode);
  var
    AVLNode: TAVLTreeNode;
    Edge: TCodeGraphEdge;
  begin
    Node.FInternalFlags:=1;
    if Node.OutTree=nil then exit;
    AVLNode:=Node.OutTree.FindLowest;
    while AVLNode<>nil do begin
      Edge:=TCodeGraphEdge(AVLNode.Data);
      if Edge.ToNode.FInternalFlags=0 then
        MarkReachableNodes(Edge.ToNode);
      AVLNode:=Node.OutTree.FindSuccessor(AVLNode);
    end;
  end;
  
  procedure AddCircleNodes(Node: TCodeGraphNode);
  var
    AVLNode: TAVLTreeNode;
    Edge: TCodeGraphEdge;
  begin
    AddNode(Node);
    if Node.InTree=nil then exit;
    AVLNode:=Node.InTree.FindLowest;
    while AVLNode<>nil do begin
      Edge:=TCodeGraphEdge(AVLNode.Data);
      if Edge.FromNode.FInternalFlags=1 then
        AddCircleNodes(Edge.FromNode);
      AVLNode:=Node.InTree.FindSuccessor(AVLNode);
    end;
  end;
  
begin
  ListOfGraphNodes:=TFPList.Create;
  ClearNodeFlags;
  MarkReachableNodes(StartNode);
  AddCircleNodes(StartNode);
end;

function TCodeGraph.GetEdge(FromNode, ToNode: TCodeTreeNode;
  CreateIfNotExists: boolean): TCodeGraphEdge;
var
  ToGraphNode: TCodeGraphNode;
  FromGraphNode: TCodeGraphNode;
  AVLNode: TAVLTreeNode;
begin
  Result:=nil;
  AVLNode:=FindAVLNodeOfEdge(FromNode,ToNode);
  if AVLNode<>nil then begin
    Result:=TCodeGraphEdge(AVLNode.Data);
  end else begin
    if not CreateIfNotExists then exit;
    FromGraphNode:=GetGraphNode(FromNode,true);
    ToGraphNode:=GetGraphNode(ToNode,true);
    Result:=FEdgeClass.Create;
    Result.ToNode:=ToGraphNode;
    Result.FromNode:=FromGraphNode;
    Edges.Add(Result);
    if FromGraphNode.OutTree=nil then
      FromGraphNode.OutTree:=TAVLTree.Create(@CompareGraphEdgeByToNode);
    FromGraphNode.OutTree.Add(Result);
    if ToGraphNode.InTree=nil then
      ToGraphNode.InTree:=TAVLTree.Create(@CompareGraphEdgeByFromNode);
    ToGraphNode.InTree.Add(Result);
  end;
end;

function TCodeGraph.FindAVLNodeOfNode(Node: TCodeTreeNode): TAVLTreeNode;
begin
  Result:=Nodes.FindKey(Node,@CompareNodeWithGraphNodeNode);
end;

function TCodeGraph.FindAVLNodeOfToNode(InTree: TAVLTree; ToNode: TCodeTreeNode
  ): TAVLTreeNode;
begin
  if InTree<>nil then
    Result:=InTree.FindKey(ToNode,@CompareNodeWithGraphEdgeToNode)
  else
    Result:=nil;
end;

function TCodeGraph.FindAVLNodeOfFromNode(OutTree: TAVLTree;
  FromNode: TCodeTreeNode): TAVLTreeNode;
begin
  if OutTree<>nil then
    Result:=OutTree.FindKey(FromNode,@CompareNodeWithGraphEdgeFromNode)
  else
    Result:=nil;
end;

function TCodeGraph.FindAVLNodeOfEdge(FromNode, ToNode: TCodeTreeNode
  ): TAVLTreeNode;
var
  EdgeKey: TCodeGraphEdgeKey;
begin
  EdgeKey.FromNode:=FromNode;
  EdgeKey.ToNode:=ToNode;
  Result:=Edges.FindKey(@EdgeKey,@CompareEdgeKeyWithGraphEdge);
end;

procedure TCodeGraph.ConsistencyCheck;

  procedure e(const Msg: string);
  begin
    raise Exception.Create('TCodeGraph.ConsistencyCheck '+Msg);
  end;

var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
  EdgeAVLNode: TAVLTreeNode;
  Edge: TCodeGraphEdge;
begin
  if Nodes=nil then
    e('');
  if Edges=nil then
    e('');
  if Nodes.ConsistencyCheck<>0 then
    e('');
  if Edges.ConsistencyCheck<>0 then
    e('');
  if AVLTreeHasDoubles(Nodes)<>nil then
    e('');
  if AVLTreeHasDoubles(Edges)<>nil then
    e('');

  AVLNode:=Nodes.FindLowest;
  while AVLNode<>nil do begin
    GraphNode:=TCodeGraphNode(AVLNode.Data);
    if GraphNode.InTree<>nil then begin
      if GraphNode.InTree.ConsistencyCheck<>0 then
        e('');
      if AVLTreeHasDoubles(GraphNode.InTree)<>nil then
        e('');
      EdgeAVLNode:=GraphNode.InTree.FindLowest;
      while EdgeAVLNode<>nil do begin
        Edge:=TCodeGraphEdge(EdgeAVLNode.Data);
        if Edges.Find(Edge)=nil then
          e('');
        if Edge.ToNode<>GraphNode then
          e('');
        EdgeAVLNode:=GraphNode.InTree.FindSuccessor(EdgeAVLNode);
      end;
    end;
    if GraphNode.OutTree<>nil then begin
      if GraphNode.OutTree.ConsistencyCheck<>0 then
        e('');
      if AVLTreeHasDoubles(GraphNode.OutTree)<>nil then
        e('');
      EdgeAVLNode:=GraphNode.OutTree.FindLowest;
      while EdgeAVLNode<>nil do begin
        Edge:=TCodeGraphEdge(EdgeAVLNode.Data);
        if Edges.Find(Edge)=nil then
          e('');
        if Edge.FromNode<>GraphNode then
          e('');
        EdgeAVLNode:=GraphNode.OutTree.FindSuccessor(EdgeAVLNode);
      end;
    end;
    AVLNode:=Nodes.FindSuccessor(AVLNode);
  end;
  
  AVLNode:=Edges.FindLowest;
  while AVLNode<>nil do begin
    Edge:=TCodeGraphEdge(AVLNode.Data);
    if Nodes.Find(Edge.FromNode)=nil then
      e('');
    if Nodes.Find(Edge.ToNode)=nil then
      e('');
    if Edge.FromNode.OutTree.Find(Edge)=nil then
      e('');
    if Edge.ToNode.InTree.Find(Edge)=nil then
      e('');
    AVLNode:=Edges.FindSuccessor(AVLNode);
  end;
end;

{ TCodeGraphNode }

function TCodeGraphNode.OutTreeCount: integer;
begin
  if OutTree<>nil then
    Result:=OutTree.Count
  else
    Result:=0;
end;

function TCodeGraphNode.InTreeCount: integer;
begin
  if InTree<>nil then
    Result:=InTree.Count
  else
    Result:=0;
end;

end.

