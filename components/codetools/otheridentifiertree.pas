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
    - tree storing identifiers of other languages (e.g. javascript)
    - identifier completion for other languages
}
unit OtherIdentifierTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, CodeTree;

type

  { TOtherIdentifierTreeNode }

  TOtherIdentifierTreeNode = class
  public
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    Parent: TOtherIdentifierTreeNode;
    NextSibling, PrevSibling: TOtherIdentifierTreeNode;
    FirstChild, LastChild: TOtherIdentifierTreeNode;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearChilds;
    procedure AddLastChild(Child: TOtherIdentifierTreeNode); virtual;
    procedure RemoveChild(Child: TOtherIdentifierTreeNode); virtual;
    function Next: TOtherIdentifierTreeNode;
    function NextSkipChilds: TOtherIdentifierTreeNode;
    function Prev: TOtherIdentifierTreeNode;
    function PrevSkipChilds: TOtherIdentifierTreeNode;
    function HasAsParent(Node: TOtherIdentifierTreeNode): boolean;
    function HasAsChild(Node: TOtherIdentifierTreeNode): boolean;
    function HasParentOfType(ParentDesc: TCodeTreeNodeDesc): boolean;
    function HasAsRoot(RootNode: TOtherIdentifierTreeNode): boolean;
    function GetRootNode: TOtherIdentifierTreeNode;
    function GetParentOfType(ADesc: TCodeTreeNodeDesc): TOtherIdentifierTreeNode;
    function GetParentOfTypes(Descriptors: array of TCodeTreeNodeDesc): TOtherIdentifierTreeNode;
    function ChildCount: integer; virtual;
    function AsString: string; virtual;
    procedure ConsistencyCheck; virtual;
    procedure WriteDebugReport(const Prefix: string; WithChilds: boolean); virtual;
  end;

  TOtherIdentifierTree = class
  public
    Root: TOtherIdentifierTreeNode;
    constructor Create;
    destructor Destroy; override;
    procedure ClearNodes;
  end;

implementation

{ TOtherIdentifierTreeNode }

procedure TOtherIdentifierTreeNode.AddLastChild(Child: TOtherIdentifierTreeNode
  );
begin
  Child.Parent:=Self;
  if LastChild<>nil then
    LastChild.NextSibling:=Child;
  Child.PrevSibling:=LastChild;
  LastChild:=Child;
  if FirstChild=nil then FirstChild:=Child;
end;

procedure TOtherIdentifierTreeNode.RemoveChild(Child: TOtherIdentifierTreeNode
  );
begin
  if FirstChild=Child then FirstChild:=Child.NextSibling;
  if LastChild=Child then LastChild:=Child.PrevSibling;
  Child.Parent:=nil;
end;

constructor TOtherIdentifierTreeNode.Create;
begin

end;

destructor TOtherIdentifierTreeNode.Destroy;
begin
  ClearChilds;
  if Parent<>nil then Parent.RemoveChild(Self);
  if NextSibling<>nil then NextSibling.PrevSibling:=PrevSibling;
  if PrevSibling<>nil then PrevSibling.NextSibling:=NextSibling;
  PrevSibling:=nil;
  NextSibling:=nil;
  inherited Destroy;
end;

procedure TOtherIdentifierTreeNode.ClearChilds;
begin
  while LastChild<>nil do LastChild.Free;
end;

function TOtherIdentifierTreeNode.Next: TOtherIdentifierTreeNode;
begin
  Result:=FirstChild;
  if Result<>nil then exit;
  Result:=NextSkipChilds;
end;

function TOtherIdentifierTreeNode.NextSkipChilds: TOtherIdentifierTreeNode;
begin
  Result:=Self;
  while (Result<>nil) do begin
    if Result.NextSibling<>nil then
      exit(Result.NextSibling);
    Result:=Result.Parent;
  end;
end;

function TOtherIdentifierTreeNode.Prev: TOtherIdentifierTreeNode;
begin
  if PrevSibling=nil then
    Result:=Parent
  else begin
    Result:=PrevSibling;
    while Result.LastChild<>nil do Result:=Result.LastChild;
  end;
end;

function TOtherIdentifierTreeNode.PrevSkipChilds: TOtherIdentifierTreeNode;
begin
  if PrevSibling<>nil then
    Result:=PrevSibling
  else
    Result:=Parent;
end;

function TOtherIdentifierTreeNode.HasAsParent(Node: TOtherIdentifierTreeNode
  ): boolean;
var
  CurNode: TOtherIdentifierTreeNode;
begin
  CurNode:=Parent;
  while CurNode<>nil do begin
    if CurNode=Node then exit(true);
    CurNode:=CurNode.Parent;
  end;
  Result:=false;
end;

function TOtherIdentifierTreeNode.HasAsChild(Node: TOtherIdentifierTreeNode
  ): boolean;
begin
  Result:=(Node<>nil) and Node.HasAsParent(Self);
end;

function TOtherIdentifierTreeNode.HasParentOfType(ParentDesc: TCodeTreeNodeDesc
  ): boolean;
begin
  Result:=GetParentOfType(ParentDesc)<>nil;
end;

function TOtherIdentifierTreeNode.HasAsRoot(RootNode: TOtherIdentifierTreeNode
  ): boolean;
begin
  Result:=GetRootNode=RootNode;
end;

function TOtherIdentifierTreeNode.GetRootNode: TOtherIdentifierTreeNode;
begin
  Result:=Self;
  while Result.Parent<>nil do Result:=Result.Parent;
end;

function TOtherIdentifierTreeNode.GetParentOfType(ADesc: TCodeTreeNodeDesc
  ): TOtherIdentifierTreeNode;
begin
  Result:=Parent;
  while (Result<>nil) do begin
    if Result.Desc=ADesc then exit;
    Result:=Result.Parent;
  end;
end;

function TOtherIdentifierTreeNode.GetParentOfTypes(
  Descriptors: array of TCodeTreeNodeDesc): TOtherIdentifierTreeNode;
var
  i: Integer;
begin
  Result:=Parent;
  while (Result<>nil) do begin
    for i:=low(Descriptors) to high(Descriptors) do
      if Result.Desc=Descriptors[i] then exit;
    Result:=Result.Parent;
  end;
end;

function TOtherIdentifierTreeNode.ChildCount: integer;
var
  Node: TOtherIdentifierTreeNode;
begin
  Result:=0;
  Node:=FirstChild;
  while Node<>nil do begin
    inc(Result);
    Node:=Node.NextSibling;
  end;
end;

function TOtherIdentifierTreeNode.AsString: string;
begin
  Result:=IntToStr(Desc);
end;

procedure TOtherIdentifierTreeNode.ConsistencyCheck;
var
  Node: TOtherIdentifierTreeNode;
begin
  if (NextSibling=Self) then
    raise Exception.Create('');
  if (PrevSibling=Self) then
    raise Exception.Create('');
  if (NextSibling<>nil) and (NextSibling.PrevSibling<>Self) then
    raise Exception.Create('');
  if (PrevSibling<>nil) and (PrevSibling.NextSibling<>Self) then
    raise Exception.Create('');
  if Parent=Self then
    raise Exception.Create('');

  if (FirstChild=nil)<>(LastChild=nil) then
    raise Exception.Create('');
  Node:=FirstChild;
  while Node<>nil do begin
    if Node.Parent<>Self then
      raise Exception.Create('');
    if (Node=FirstChild) and (Node.PrevSibling<>nil) then
      raise Exception.Create('');
    if (Node=LastChild) and (Node.NextSibling<>nil) then
      raise Exception.Create('');
    Node.ConsistencyCheck;
    Node:=Node.NextSibling;
  end;
end;

procedure TOtherIdentifierTreeNode.WriteDebugReport(const Prefix: string;
  WithChilds: boolean);
begin
  writeln(Prefix,AsString);
  if WithChilds then
    WriteDebugReport(Prefix+'  ',true);
end;

{ TOtherIdentifierTree }

constructor TOtherIdentifierTree.Create;
begin

end;

destructor TOtherIdentifierTree.Destroy;
begin
  ClearNodes;
  inherited Destroy;
end;

procedure TOtherIdentifierTree.ClearNodes;
begin
  FreeAndNil(Root);
end;

end.

