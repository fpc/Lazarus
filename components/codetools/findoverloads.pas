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
}
unit FindOverloads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, CodeAtom, CodeTree, CodeGraph, CodeCache,
  FindDeclarationTool;

type
  TOverloadsGraphNode = class(TCodeGraphNode)
  public
    Identifier: string;
    Tool: TFindDeclarationTool;
  end;

  TOverloadsGraphEdgeType = (
    ogetParentChild,
    ogetAncestorInherited,
    ogetInterfaceInherited
    );
  TOverloadsGraphEdgeTypes = set of TOverloadsGraphEdgeType;

  TOverloadsGraphEdge = class(TCodeGraphEdge)
  public
    Typ: TOverloadsGraphEdgeType;
  end;

  { TDeclarationOverloadsGraph }

  TDeclarationOverloadsGraph = class
  private
    FGraph: TCodeGraph;
    FIdentifier: string;
    FOnGetCodeToolForBuffer: TOnGetCodeToolForBuffer;
    FStartCode: TCodeBuffer;
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
    property Identifier: string read FIdentifier;
    property Graph: TCodeGraph read FGraph;
    property StartCode: TCodeBuffer read FStartCode;
    property StartX: integer read FStartX;
    property StartY: integer read FStartY;
    property OnGetCodeToolForBuffer: TOnGetCodeToolForBuffer
                     read FOnGetCodeToolForBuffer write FOnGetCodeToolForBuffer;
  end;

const
  OverloadsGraphEdgeTypeNames: array[TOverloadsGraphEdgeType] of string = (
    'Parent-Child',
    'Ancestor-Inherited',
    'Interface-Inherited'
    );

implementation

{ TDeclarationOverloadsGraph }

function TDeclarationOverloadsGraph.AddContext(Tool: TFindDeclarationTool;
  CodeNode: TCodeTreeNode): TOverloadsGraphNode;
var
  ParentCodeNode: TCodeTreeNode;
  ParentGraphNode: TOverloadsGraphNode;
begin
  Result:=TOverloadsGraphNode(Graph.GetGraphNode(CodeNode,false));
  if Result<>nil then exit;
  // add new node
  DebugLn(['TDeclarationOverloadsGraph.AddContext ',Tool.MainFilename,' ',CodeNode.DescAsString,' "',dbgstr(copy(Tool.Src,CodeNode.StartPos,20)),'"']);
  Result:=TOverloadsGraphNode(Graph.GetGraphNode(CodeNode,true));
  Result.Tool:=Tool;

  // add parent nodes to graph
  ParentCodeNode:=CodeNode.Parent;
  while ParentCodeNode<>nil do begin
    DebugLn(['TDeclarationOverloadsGraph.AddContext ',ParentCodeNode.DescAsString]);
    if ParentCodeNode.Desc in
      AllSourceTypes+[ctnClass,ctnClassInterface,ctnRecordType]
    then begin
      DebugLn(['TDeclarationOverloadsGraph.AddContext ADD parent']);
      ParentGraphNode:=AddContext(Tool,ParentCodeNode);
      AddEdge(ogetParentChild,ParentGraphNode.Node,Result.Node);
      break;
    end;
    if ParentCodeNode.Parent<>nil then
      ParentCodeNode:=ParentCodeNode.Parent
    else
      ParentCodeNode:=ParentCodeNode.PriorBrother;
  end;

  // ToDo: add ancestors, interfaces


  // ToDo: add alias


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
  DebugLn(['TDeclarationOverloadsGraph.AddEdge ',OverloadsGraphEdgeTypeNames[Typ]]);
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

end;

function TDeclarationOverloadsGraph.Init(Code: TCodeBuffer; X, Y: integer
  ): Boolean;
var
  Tool: TFindDeclarationTool;
  CleanPos: integer;
  CodeNode: TCodeTreeNode;
begin
  Result:=false;
  FStartCode:=Code;
  FStartX:=X;
  FStartY:=Y;

  Tool:=OnGetCodeToolForBuffer(Self,Code,true);
  if Tool.CaretToCleanPos(CodeXYPosition(X,Y,Code),CleanPos)<>0 then begin
    DebugLn(['TDeclarationOverloadsGraph.Init Tool.CaretToCleanPos failed']);
    exit(false);
  end;
  CodeNode:=Tool.FindDeepestNodeAtPos(CleanPos,true);
  DebugLn(['TDeclarationOverloadsGraph.Init Add start context']);
  AddContext(Tool,CodeNode);

  Result:=true;
end;

end.

