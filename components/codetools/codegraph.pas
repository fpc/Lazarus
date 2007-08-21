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
  
  PCodeGraphEdgeKey = ^TCodeGraphEdgeKey;
  TCodeGraphEdgeKey = record
    FromNode: TCodeTreeNode;
    ToNode: TCodeTreeNode;
  end;

  { TCodeGraphEdge }

  TCodeGraphEdge = class
    FromNode: TCodeGraphNode;
    ToNode: TCodeGraphNode;
    Data: Pointer;  // custom data
    Flags: cardinal;// custom flags
  end;

  { TCodeGraph }

  TCodeGraph = class
  public
    Nodes: TAVLTree; // tree of TCodeGraphNode sorted for Node
    Edges: TAVLTree; // tree of TCodeGraphEdge sorted for FromNode,ToNode
    constructor Create;
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

    function AddEdge(FromNode, ToNode: TCodeTreeNode): TCodeGraphEdge;
    function GetEdge(FromNode, ToNode: TCodeTreeNode;
                     CreateIfNotExists: boolean): TCodeGraphEdge;
    procedure DeleteEdge(FromNode, ToNode: TCodeTreeNode);
    procedure DeleteEdge(Edge: TCodeGraphEdge);
    function GetTopologicalSortedList(out ListOfGraphNodes: TFPList
                                      ): TCodeGraphEdge;

    function FindAVLNodeOfNode(Node: TCodeTreeNode): TAVLTreeNode;
    function FindAVLNodeOfToNode(InTree: TAVLTree; ToNode: TCodeTreeNode
                                 ): TAVLTreeNode;
    function FindAVLNodeOfFromNode(OutTree: TAVLTree; FromNode: TCodeTreeNode
                                   ): TAVLTreeNode;
    function FindAVLNodeOfEdge(FromNode, ToNode: TCodeTreeNode): TAVLTreeNode;
  end;
  
function CompareGraphNodeByNode(GraphNode1, GraphNode2: Pointer): integer;
function ComparePointerWithGraphNodeNode(p, GraphNode: Pointer): integer;

function CompareGraphEdgeByFromNode(GraphEdge1, GraphEdge2: Pointer): integer;
function ComparePointerWithGraphEdgeFromNode(p, GraphEdge: Pointer): integer;
function CompareGraphEdgeByToNode(GraphEdge1, GraphEdge2: Pointer): integer;
function ComparePointerWithGraphEdgeToNode(p, GraphEdge: Pointer): integer;

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
end;

function ComparePointerWithGraphNodeNode(p, GraphNode: Pointer): integer;
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

function ComparePointerWithGraphEdgeFromNode(p, GraphEdge: Pointer): integer;
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

function ComparePointerWithGraphEdgeToNode(p, GraphEdge: Pointer): integer;
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

{ TCodeGraph }

constructor TCodeGraph.Create;
begin
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
  AVLNode:=FindAVLNodeOfNode(Node);
  if AVLNode<>nil then
    Result:=TCodeGraphNode(AVLNode.Data)
  else if CreateIfNotExists then begin
    Result:=TCodeGraphNode.Create;
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
  if GraphNode.OutTree<>nil then begin
    // remove all edges coming from this Node
    OutTree:=GraphNode.OutTree;
    OutAVLNode:=OutTree.FindLowest;
    while OutAVLNode<>nil do begin
      Edge:=TCodeGraphEdge(OutAVLNode.Data);
      InTree:=Edge.ToNode.InTree;
      InTree.Remove(Edge);
      Edge.Free;
      OutAVLNode:=OutTree.FindSuccessor(OutAVLNode);
    end;
    OutTree.Clear;
  end;
  if GraphNode.InTree<>nil then begin
    // remove all edges goind to this Node
    InTree:=GraphNode.InTree;
    InAVLNode:=InTree.FindLowest;
    while InAVLNode<>nil do begin
      Edge:=TCodeGraphEdge(InAVLNode.Data);
      OutTree:=Edge.ToNode.OutTree;
      OutTree.Remove(Edge);
      Edge.Free;
      InAVLNode:=InTree.FindSuccessor(InAVLNode);
    end;
    InTree.Clear;
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

function TCodeGraph.GetTopologicalSortedList(out ListOfGraphNodes: TFPList
  ): TCodeGraphEdge;
// returns nil if there is no circle
// else: returns a circle edge
// ListOfTGraphNodes are the GraphNodes, that could be sorted topologically
var
  NodeQueue: array of TCodeGraphNode;
  QueueStart: Integer;
  QueueEnd: Integer;
  
  procedure AddNode(GraphNode: TCodeGraphNode);
  begin
    NodeQueue[QueueEnd]:=GraphNode;
    inc(QueueEnd);
  end;
  
var
  AVLNode: TAVLTreeNode;
  GraphNode: TCodeGraphNode;
  i: Integer;
  WorkGraph: TCodeGraph;
  GraphEdge: TCodeGraphEdge;
  ToGraphNode: TCodeGraphNode;
  EdgeAVLNode: TAVLTreeNode;
begin
  Result:=nil;
  ListOfGraphNodes:=TFPList.Create;
  if (Nodes=nil) or (Nodes.Count=0) then exit(nil);
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
      if (GraphNode.InTree=nil) or (GraphNode.InTree.Count=0) then begin
        GraphNode.FInternalFlags:=0;
        AddNode(GraphNode);
      end else begin
        GraphNode.FInternalFlags:=GraphNode.InTree.Count;
      end;
      AVLNode:=Nodes.FindSuccessor(AVLNode);
    end;
    
    // add all nodes without incoming edges from the queue into the list
    while QueueStart<>QueueEnd do begin
      GraphNode:=NodeQueue[QueueStart];
      inc(QueueStart);
      ListOfGraphNodes.Add(GraphNode);
      // update the FInternalFlags counter
      if (GraphNode.OutTree<>nil) then begin
        EdgeAVLNode:=GraphNode.OutTree.FindLowest;
        while EdgeAVLNode<>nil do begin
          GraphEdge:=TCodeGraphEdge(EdgeAVLNode.Data);
          ToGraphNode:=GraphEdge.ToNode;
          dec(ToGraphNode.FInternalFlags);
          if ToGraphNode.FInternalFlags=0 then
            // a new node has no incoming edges => add to the queue
            AddNode(ToGraphNode);
          EdgeAVLNode:=GraphNode.OutTree.FindSuccessor(EdgeAVLNode);
        end;
      end;
    end;
    
    if ListOfGraphNodes.Count<Nodes.Count then begin
      // there is a circle
      // find a node of a circle
      AVLNode:=Nodes.FindLowest;
      while (AVLNode<>nil) and (Result=nil) do begin
        GraphNode:=TCodeGraphNode(AVLNode.Data);
        if GraphNode.FInternalFlags>0 then begin
          // find an edge of a circle
          EdgeAVLNode:=GraphNode.OutTree.FindLowest;
          while EdgeAVLNode<>nil do begin
            GraphEdge:=TCodeGraphEdge(EdgeAVLNode.Data);
            ToGraphNode:=GraphEdge.ToNode;
            if ToGraphNode.FInternalFlags>0 then begin
              Result:=GraphEdge;
              break;
            end;
            EdgeAVLNode:=GraphNode.OutTree.FindSuccessor(EdgeAVLNode);
          end;
        end;
        AVLNode:=Nodes.FindSuccessor(AVLNode);
      end;
    end;
  finally
    SetLength(NodeQueue,0);
  end;
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
    Result:=TCodeGraphEdge.Create;
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
  Result:=Nodes.FindKey(Node,@ComparePointerWithGraphNodeNode);
end;

function TCodeGraph.FindAVLNodeOfToNode(InTree: TAVLTree; ToNode: TCodeTreeNode
  ): TAVLTreeNode;
begin
  if InTree<>nil then
    Result:=InTree.FindKey(ToNode,@ComparePointerWithGraphEdgeToNode)
  else
    Result:=nil;
end;

function TCodeGraph.FindAVLNodeOfFromNode(OutTree: TAVLTree;
  FromNode: TCodeTreeNode): TAVLTreeNode;
begin
  if OutTree<>nil then
    Result:=OutTree.FindKey(FromNode,@ComparePointerWithGraphEdgeFromNode)
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

