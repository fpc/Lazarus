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
    A graph of declaration overloads.
    Create via CodeToolBoss.GatherOverloads(Code,X,Y,Graph).
    Add units via Graph.ScanToolForIdentifier.

}
unit FindOverloads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, BasicCodeTools, CodeAtom, CodeTree, CodeGraph,
  CodeCache, FindDeclarationTool, AVL_Tree, FindDeclarationCache, StdCodeTools;

type
  TOverloadsGraphEdge = class;

  { TOverloadsGraphNode }

  TOverloadsGraphNode = class(TCodeGraphNode)
  public
    Identifier: string;
    Tool: TFindDeclarationTool;
    ShortestPathLength: integer;
    ShortestPathEdge: TOverloadsGraphEdge;
    function AsDebugString: string;
  end;

  TOverloadsGraphEdgeType = (
    ogetParentChild,
    ogetAncestorInherited,
    ogetAliasOld
    );
  TOverloadsGraphEdgeTypes = set of TOverloadsGraphEdgeType;

  { TOverloadsGraphEdge }

  TOverloadsGraphEdge = class(TCodeGraphEdge)
  public
    Typ: TOverloadsGraphEdgeType;
    function AsDebugString: string;
    function Cost: integer;
  end;

  { TDeclarationOverloadsGraph }

  TDeclarationOverloadsGraph = class
  private
    FGraph: TCodeGraph;
    FIdentifier: string;
    FOnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
    FShortestNodes: TAVLTree;
    FStartCode: TCodeBuffer;
    FStartCodeNode: TCodeTreeNode;
    FStartTool: TFindDeclarationTool;
    FStartX: integer;
    FStartY: integer;
    function AddContext(Tool: TFindDeclarationTool;
                        CodeNode: TCodeTreeNode): TOverloadsGraphNode;
    function AddEdge(Typ: TOverloadsGraphEdgeType;
                     FromNode, ToNode: TCodeTreeNode): TOverloadsGraphEdge;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Init(Code: TCodeBuffer; X,Y: integer): Boolean;
    procedure ScanToolForIdentifier(Tool: TStandardCodeTool;
                                    OnlyInterface: boolean);
    procedure ComputeShortestPaths;
  public
    property Graph: TCodeGraph read FGraph;
    property Identifier: string read FIdentifier;
    property ShortestNodes: TAVLTree read FShortestNodes;// nodes sorted for ShortestPathLength (after ComputeShortestPaths)
    property StartCode: TCodeBuffer read FStartCode;
    property StartX: integer read FStartX;
    property StartY: integer read FStartY;
    property StartTool: TFindDeclarationTool read FStartTool;
    property StartCodeNode: TCodeTreeNode read FStartCodeNode;
    property OnGetCodeToolForBuffer: TOnGetCodeToolForBuffer
                     read FOnGetCodeToolForBuffer write FOnGetCodeToolForBuffer;
  end;

const
  OverloadsGraphEdgeTypeNames: array[TOverloadsGraphEdgeType] of string = (
    'Parent-Child',
    'Ancestor-Inherited',
    'Alias-Old'
    );

function CompareOverloadsNodesByPathLen(Node1, Node2: TOverloadsGraphNode): integer;

implementation

function CompareOverloadsNodesByPathLen(Node1, Node2: TOverloadsGraphNode
  ): integer;
begin
  if Node1.ShortestPathLength>Node2.ShortestPathLength then
    Result:=1
  else if Node1.ShortestPathLength<Node2.ShortestPathLength then
    Result:=-1
  else
    Result:=0;
end;

{ TDeclarationOverloadsGraph }

function TDeclarationOverloadsGraph.AddContext(Tool: TFindDeclarationTool;
  CodeNode: TCodeTreeNode): TOverloadsGraphNode;
var
  ParentCodeNode: TCodeTreeNode;
  ParentGraphNode: TOverloadsGraphNode;
  ClassNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  ListOfPFindContext: TFPList;
  i: Integer;
  ContextPtr: PFindContext;
  AncestorGraphNode: TOverloadsGraphNode;
  Context: TFindContext;
  AliasGraphNode: TOverloadsGraphNode;
begin
  Result:=TOverloadsGraphNode(Graph.GetGraphNode(CodeNode,false));
  if Result<>nil then exit;
  // add new node
  //DebugLn(['TDeclarationOverloadsGraph.AddContext ',Tool.MainFilename,' ',CodeNode.DescAsString,' "',dbgstr(copy(Tool.Src,CodeNode.StartPos,20)),'"']);
  Result:=TOverloadsGraphNode(Graph.GetGraphNode(CodeNode,true));
  Result.Tool:=Tool;
  if CodeNode.Desc in AllIdentifierDefinitions then
    Result.Identifier:=Tool.ExtractDefinitionName(CodeNode)
  else begin
    case CodeNode.Desc of
    ctnEnumIdentifier: Result.Identifier:=GetIdentifier(@Tool.Src[CodeNode.StartPos]);
    end;
  end;

  // add parent nodes to graph
  ParentCodeNode:=CodeNode.Parent;
  while ParentCodeNode<>nil do begin
    //DebugLn(['TDeclarationOverloadsGraph.AddContext ',ParentCodeNode.DescAsString]);
    if ParentCodeNode.Desc in
      AllSourceTypes+AllClasses+[ctnRecordType]
    then begin
      //DebugLn(['TDeclarationOverloadsGraph.AddContext ADD parent']);
      ParentGraphNode:=AddContext(Tool,ParentCodeNode);
      AddEdge(ogetParentChild,ParentGraphNode.Node,Result.Node);
      break;
    end;
    if ParentCodeNode.Parent<>nil then
      ParentCodeNode:=ParentCodeNode.Parent
    else
      ParentCodeNode:=ParentCodeNode.PriorBrother;
  end;

  // add ancestors, interfaces
  if (CodeNode.Desc=ctnTypeDefinition)
  and (CodeNode.FirstChild<>nil)
  and (CodeNode.FirstChild.Desc in AllClasses) then begin
    //DebugLn(['TDeclarationOverloadsGraph.AddContext a class or interface']);
    // a class or class interface
    ClassNode:=CodeNode.FirstChild;
    Tool.BuildSubTree(ClassNode);

    if (ClassNode.FirstChild<>nil)
    and (ClassNode.FirstChild.Desc=ctnClassInheritance) then begin
      //DebugLn(['TDeclarationOverloadsGraph.AddContext has ancestor(s)']);
      Params:=TFindDeclarationParams.Create;
      ListOfPFindContext:=nil;
      try
        Tool.FindAncestorsOfClass(ClassNode,ListOfPFindContext,Params,false);
        //DebugLn(['TDeclarationOverloadsGraph.AddContext ancestors found: ',ListOfPFindContext<>nil]);
        if ListOfPFindContext<>nil then begin
          for i:=0 to ListOfPFindContext.Count-1 do begin
            //DebugLn(['TDeclarationOverloadsGraph.AddContext ancestor #',i]);
            ContextPtr:=PFindContext(ListOfPFindContext[i]);
            AncestorGraphNode:=AddContext(ContextPtr^.Tool,ContextPtr^.Node);
            AddEdge(ogetAncestorInherited,AncestorGraphNode.Node,Result.Node);
          end;
        end;
      finally
        FreeListOfPFindContext(ListOfPFindContext);
        Params.Free;
      end;
    end;
  end;

  // ToDo: add alias
  if (CodeNode.Desc=ctnTypeDefinition)
  and (CodeNode.FirstChild<>nil)
  and (CodeNode.FirstChild.Desc=ctnIdentifier) then begin
    //DebugLn(['TDeclarationOverloadsGraph.AddContext alias']);
    Params:=TFindDeclarationParams.Create;
    try
      try
        Context:=Tool.FindBaseTypeOfNode(Params,CodeNode);
        if Context.Node<>nil then begin
          while (Context.Node<>nil)
          and (not (Context.Node.Desc in AllIdentifierDefinitions)) do
            Context.Node:=Context.Node.Parent;
          if Context.Node<>nil then begin
            AliasGraphNode:=AddContext(Context.Tool,Context.Node);
            AddEdge(ogetAliasOld,AliasGraphNode.Node,Result.Node);
          end;
        end;
      except
      end;
    finally
      Params.Free;
    end;
  end;

end;

function TDeclarationOverloadsGraph.AddEdge(Typ: TOverloadsGraphEdgeType;
  FromNode, ToNode: TCodeTreeNode): TOverloadsGraphEdge;
begin
  Result:=TOverloadsGraphEdge(Graph.GetEdge(FromNode,ToNode,false));
  if (Result<>nil) then begin
    if Result.Typ<>Typ then
      RaiseCatchableException('TDeclarationOverloadsGraph.AddEdge Typ conflict');
    exit;
  end;
  // create new edge
  Result:=TOverloadsGraphEdge(Graph.GetEdge(FromNode,ToNode,true));
  Result.Typ:=Typ;
  //DebugLn(['TDeclarationOverloadsGraph.AddEdge ',Result.AsDebugString]);
end;

constructor TDeclarationOverloadsGraph.Create;
begin
  FGraph:=TCodeGraph.Create(TOverloadsGraphNode,TOverloadsGraphEdge);
end;

destructor TDeclarationOverloadsGraph.Destroy;
begin
  Clear;
  FreeAndNil(FGraph);
  inherited Destroy;
end;

procedure TDeclarationOverloadsGraph.Clear;
begin
  FreeAndNil(FShortestNodes);
  Graph.Clear;
  FStartCodeNode:=nil;
  FStartCode:=nil;
  FStartX:=0;
  FStartY:=0;
  FIdentifier:='';
end;

function TDeclarationOverloadsGraph.Init(Code: TCodeBuffer; X, Y: integer
  ): Boolean;
var
  CleanPos: integer;
begin
  Result:=false;
  FStartCode:=Code;
  FStartX:=X;
  FStartY:=Y;

  fStartTool:=OnGetCodeToolForBuffer(Self,Code,true);
  if fStartTool.CaretToCleanPos(CodeXYPosition(X,Y,Code),CleanPos)<>0 then begin
    DebugLn(['TDeclarationOverloadsGraph.Init Tool.CaretToCleanPos failed']);
    exit(false);
  end;
  fStartCodeNode:=fStartTool.FindDeepestNodeAtPos(CleanPos,true);
  DebugLn(['TDeclarationOverloadsGraph.Init Add start context ',FStartTool.MainFilename,' ',FStartCodeNode.DescAsString,' ',dbgstr(copy(FStartTool.Src,FStartCodeNode.StartPos,20))]);
  AddContext(fStartTool,StartCodeNode);

  fIdentifier:='';
  if fStartCodeNode.Desc in AllIdentifierDefinitions+[ctnEnumIdentifier] then
    fIdentifier:=GetIdentifier(@fStartTool.Src[fStartCodeNode.StartPos]);

  Result:=true;
end;

procedure TDeclarationOverloadsGraph.ScanToolForIdentifier(
  Tool: TStandardCodeTool; OnlyInterface: boolean);
var
  Entry: PInterfaceIdentCacheEntry;
  Node: TCodeTreeNode;
begin
  if Identifier='' then exit;
  if OnlyInterface then begin
    // use interface cache
    try
      Tool.BuildInterfaceIdentifierCache(false);
    except
    end;
    if Tool.InterfaceIdentifierCache<>nil then begin
      Entry:=Tool.InterfaceIdentifierCache.FindIdentifier(PChar(Identifier));
      while Entry<>nil do begin
        if CompareIdentifiers(Entry^.Identifier,PChar(Identifier))=0 then
          AddContext(Tool,Entry^.Node);
        Entry:=Entry^.NextEntry;
      end;
    end;
  end else begin
    // scan whole unit/program
    try
      Tool.Explore(false);
    except
    end;
    if Tool.Tree=nil then exit;
    Node:=Tool.Tree.Root;
    while Node<>nil do begin
      case Node.Desc of

      ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition,ctnGenericType,
      ctnEnumIdentifier:
        if CompareIdentifiers(@Tool.Src[Node.StartPos],PChar(Identifier))=0
        then
          AddContext(Tool,Node);

      ctnProcedure:
        begin
          Tool.MoveCursorToProcName(Node,true);
          if CompareIdentifiers(@Tool.Src[Tool.CurPos.StartPos],PChar(Identifier))=0
          then
            AddContext(Tool,Node);
        end;

      ctnProperty:
        begin
          Tool.MoveCursorToPropName(Node);
          if CompareIdentifiers(@Tool.Src[Tool.CurPos.StartPos],PChar(Identifier))=0
          then
            AddContext(Tool,Node);
        end;
      end;
      Node:=Node.Next;
    end;
  end;
end;

procedure TDeclarationOverloadsGraph.ComputeShortestPaths;
var
  AVLNode: TAVLTreeNode;
  GraphNode: TOverloadsGraphNode;
  StartGraphNode: TOverloadsGraphNode;
  WorkNodes: TAVLTree;
  Edge: TOverloadsGraphEdge;
  GraphNode2: TOverloadsGraphNode;
begin
  (* Dijkstra-Algorithm:
    for v in V do l(v):=inf
    l(u) := 0
    W:=V
    while W not empty do
      v := { v in W | l(v) minimal }
      W:=W-{v}
      for x in Adj(v), x in W do
        if l(v)+w(v,x)<l(x) then
          l(x):=l(v)+w(v,x)
          k(x):=(v,x)
  *)
  StartGraphNode:=TOverloadsGraphNode(Graph.GetGraphNode(StartCodeNode,false));
  if StartGraphNode=nil then exit;

  WorkNodes:=nil;
  try
    // set all ShortestPathLength to infinity (except the start which gets 0)
    // and sort all nodes in a tree
    WorkNodes:=TAVLTree.Create(TListSortCompare(@CompareOverloadsNodesByPathLen));
    AVLNode:=Graph.Nodes.FindLowest;
    while AVLNode<>nil do begin
      GraphNode:=TOverloadsGraphNode(AVLNode.Data);
      GraphNode.ShortestPathEdge:=nil;
      if GraphNode.Node=StartCodeNode then
        GraphNode.ShortestPathLength:=0
      else
        GraphNode.ShortestPathLength:=high(integer) div 2;
      WorkNodes.Add(GraphNode);
      AVLNode:=Graph.Nodes.FindSuccessor(AVLNode);
    end;

    // for each remaining node that has currently the shortest path ...
    while WorkNodes.Count>0 do begin
      GraphNode:=TOverloadsGraphNode(WorkNodes.FindLowest.Data);
      // this node's ShortestPathLength is final
      WorkNodes.Remove(GraphNode);
      // update adjacent nodes
      AVLNode:=GraphNode.OutTree.FindLowest;
      while AVLNode<>nil do begin
        Edge:=TOverloadsGraphEdge(AVLNode.Data);
        GraphNode2:=TOverloadsGraphNode(Edge.ToNode);
        if GraphNode.ShortestPathLength+Edge.Cost<GraphNode2.ShortestPathLength
        then begin
          GraphNode2.ShortestPathLength:=GraphNode.ShortestPathLength+Edge.Cost;
          GraphNode2.ShortestPathEdge:=Edge;
        end;
        AVLNode:=GraphNode.OutTree.FindSuccessor(AVLNode);
      end;
      // update incident nodes
      AVLNode:=GraphNode.InTree.FindLowest;
      while AVLNode<>nil do begin
        Edge:=TOverloadsGraphEdge(AVLNode.Data);
        GraphNode2:=TOverloadsGraphNode(Edge.FromNode);
        if GraphNode.ShortestPathLength+Edge.Cost<GraphNode2.ShortestPathLength
        then begin
          GraphNode2.ShortestPathLength:=GraphNode.ShortestPathLength+Edge.Cost;
          GraphNode2.ShortestPathEdge:=Edge;
        end;
        AVLNode:=GraphNode.InTree.FindSuccessor(AVLNode);
      end;
    end;

  finally
    WorkNodes.Free;
  end;

  // build ShortestNodes
  if FShortestNodes=nil then
    FShortestNodes:=TAVLTree.Create(TListSortCompare(@CompareOverloadsNodesByPathLen))
  else
    FShortestNodes.Clear;
  AVLNode:=Graph.Nodes.FindLowest;
  while AVLNode<>nil do begin
    GraphNode:=TOverloadsGraphNode(AVLNode.Data);
    FShortestNodes.Add(GraphNode);
    AVLNode:=Graph.Nodes.FindSuccessor(AVLNode);
  end;
end;

{ TOverloadsGraphNode }

function TOverloadsGraphNode.AsDebugString: string;
begin
  Result:=Identifier;
  if Node<>nil then
    Result:=Result+' Desc="'+Node.DescAsString+'"';
  if (Tool<>nil) and (Node<>nil) and (Identifier='') then begin
    Result:=Result+' "'+dbgstr(copy(Tool.Src,Node.StartPos,20))+'"';
  end;
end;

{ TOverloadsGraphEdge }

function TOverloadsGraphEdge.AsDebugString: string;
begin
  Result:='Typ='+OverloadsGraphEdgeTypeNames[Typ]
    +' '+(FromNode as TOverloadsGraphNode).AsDebugString
    +'->'
    +(ToNode as TOverloadsGraphNode).AsDebugString;
end;

function TOverloadsGraphEdge.Cost: integer;
begin
  case Typ of
  ogetParentChild: Result:=10;
  ogetAncestorInherited: Result:=1;
  ogetAliasOld: Result:=1;
  else Result:=100;
  end;
end;

end.

