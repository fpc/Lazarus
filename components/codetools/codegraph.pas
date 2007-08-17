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
  public
    Node: TCodeTreeNode;
    InTree: TAVLTree;// tree of TCodeGraphEdge sorted for FromNode (ToNode = Self)
    OutTree: TAVLTree;// tree of TCodeGraphEdge sorted for ToNode (FromNode = Self)
    Data: Pointer;
  end;
  
  { TCodeGraphEdge }

  TCodeGraphEdge = class
    FromNode: TCodeGraphNode;
    ToNode: TCodeGraphNode;
    Data: Pointer;
  end;

  { TCodeGraph }

  TCodeGraph = class
  public
    Nodes: TAVLTree; // tree of TCodeGraphNode sorted for Node
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddGraphNode(Node: TCodeTreeNode): TCodeGraphNode;
    function GetGraphNode(Node: TCodeTreeNode; CreateIfNotExists: boolean
                          ): TCodeGraphNode;
    procedure DeleteGraphNode(Node: TCodeTreeNode);
    function AddEdge(FromNode, ToNode: TCodeTreeNode): TCodeGraphEdge;
    function GetEdge(FromNode, ToNode: TCodeTreeNode;
                     CreateIfNotExists: boolean): TCodeGraphEdge;
    procedure DeleteEdge(FromNode, ToNode: TCodeTreeNode);

    function FindAVLNodeOfNode(Node: TCodeTreeNode): TAVLTreeNode;
    function FindAVLNodeOfToNode(InTree: TAVLTree; ToNode: TCodeTreeNode
                                 ): TAVLTreeNode;
    function FindAVLNodeOfFromNode(OutTree: TAVLTree; FromNode: TCodeTreeNode
                                   ): TAVLTreeNode;
  end;
  
function CompareGraphNodeByNode(GraphNode1, GraphNode2: Pointer): integer;
function ComparePointerWithGraphNodeNode(p, GraphNode: Pointer): integer;

function CompareGraphEdgeByFromNode(GraphEdge1, GraphEdge2: Pointer): integer;
function ComparePointerWithGraphEdgeFromNode(p, GraphEdge: Pointer): integer;
function CompareGraphEdgeByToNode(GraphEdge1, GraphEdge2: Pointer): integer;
function ComparePointerWithGraphEdgeToNode(p, GraphEdge: Pointer): integer;

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

{ TCodeGraph }

constructor TCodeGraph.Create;
begin
  Nodes:=TAVLTree.Create(@CompareGraphNodeByNode);
end;

destructor TCodeGraph.Destroy;
begin
  Clear;
  FreeAndNil(Nodes);
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

function TCodeGraph.AddEdge(FromNode, ToNode: TCodeTreeNode): TCodeGraphEdge;
begin
  Result:=GetEdge(FromNode,ToNode,true);
end;

procedure TCodeGraph.DeleteEdge(FromNode, ToNode: TCodeTreeNode);
var
  Edge: TCodeGraphEdge;
begin
  Edge:=GetEdge(FromNode,ToNode,false);
  if Edge=nil then exit;
  Edge.FromNode.OutTree.Remove(Edge);
  Edge.ToNode.InTree.Remove(Edge);
  Edge.Free;
end;

function TCodeGraph.GetEdge(FromNode, ToNode: TCodeTreeNode;
  CreateIfNotExists: boolean): TCodeGraphEdge;
var
  ToGraphNode: TCodeGraphNode;
  FromGraphNode: TCodeGraphNode;
  AVLNode: TAVLTreeNode;
begin
  Result:=nil;
  FromGraphNode:=GetGraphNode(FromNode,CreateIfNotExists);
  if FromGraphNode=nil then exit;
  AVLNode:=FindAVLNodeOfToNode(FromGraphNode.OutTree,ToNode);
  if AVLNode<>nil then begin
    Result:=TCodeGraphEdge(AVLNode.Data);
  end else begin
    if not CreateIfNotExists then exit;
    ToGraphNode:=GetGraphNode(FromNode,true);
    Result:=TCodeGraphEdge.Create;
    Result.ToNode:=ToGraphNode;
    Result.FromNode:=FromGraphNode;
    if FromGraphNode.OutTree=nil then
      FromGraphNode.OutTree:=TAVLTree.Create(@CompareGraphEdgeByFromNode);
    FromGraphNode.OutTree.Add(Result);
    if ToGraphNode.InTree=nil then
      ToGraphNode.InTree:=TAVLTree.Create(@CompareGraphEdgeByToNode);
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
    Result:=Nodes.FindKey(ToNode,@ComparePointerWithGraphEdgeToNode)
  else
    Result:=nil;
end;

function TCodeGraph.FindAVLNodeOfFromNode(OutTree: TAVLTree;
  FromNode: TCodeTreeNode): TAVLTreeNode;
begin
  if OutTree<>nil then
    Result:=Nodes.FindKey(FromNode,@ComparePointerWithGraphEdgeFromNode)
  else
    Result:=nil;
end;

end.

