unit TreeWalker;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SimpleTreeWalker, released March 2004.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s):
Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ AFS 1 March 04

  Simpler, hopefully faster approach to visiting the tree
  Code is in neither the visitor or the tree
  but uses both

  The tree is a root node with children
  The visitor is a process which is applied to each node in turn

  This is how all processes that transform the program input to output
  are applied. So it is key to the second phase of the program,
  The first being generating the parse tree
}

uses ParseTreeNode, BaseVisitor;

type
  TTreeWalker = class(TObject)
  private
    { flags about the current visitor's needs
      Almost all visitors use the infix walk of leaf nodes
      Few look at the interior nodes }

    { does it do the prefix walk of interior nodes? }
    fbHasPreVisit: Boolean;
    { does it do the postfix walk of interior nodes? }
    fbHasPostVisit: Boolean;

    { does it visit the leaves - almost all do }
    fbHasSourceTokenVisit: Boolean;

    { flag set true when a a visitor request that the current item be deleted,
      and the index is thereafter wrong }
    fbRecalcIndex: Boolean;

    fcVisitor: TBaseTreeNodeVisitor;

    procedure InitialiseFlags;
    procedure VisitTree(const pcNode: TParseTreeNode);

  public
    procedure Visit(const pcRoot: TParseTreeNode; const pcVisitor: TBaseTreeNodeVisitor);
  end;

implementation

uses
  { delphi } SysUtils;

procedure TTreeWalker.InitialiseFlags;
begin
  { read these once only for speed  }

  fbHasPreVisit := fcVisitor.HasPreVisit;
  fbHasPostVisit := fcVisitor.HasPostVisit;
  fbHasSourceTokenVisit := fcVisitor.HasSourceTokenVisit;
end;

procedure TTreeWalker.VisitTree(const pcNode: TParseTreeNode);
const
  { if a node has more than this number of direct children, then something is very wrong
   can have lots in some "header" units that just list a lot of consts
   AFS 8 Oct 2006 upped it for Sourceforge bug 1558885 }
  MAX_NODE_CHILDREN = 4194304;
var
  liLoop: Integer;
  lcChildNode: TParseTreeNode;
  liNewIndex: Integer;
begin
  if pcNode.IsLeaf then
  begin
    if fbHasSourceTokenVisit then
      fbRecalcIndex := fcVisitor.VisitSourceToken(pcNode);
  end
  else
  begin
    { not leaf - visit children }
    if fbHasPreVisit then
      fcVisitor.PreVisitParseTreeNode(pcNode);

    if pcNode.ChildNodeCount > MAX_NODE_CHILDREN then
    begin
      // some parse or insert process has gone bezerk
      raise Exception.Create('Too many child nodes ' + IntToStr(pcNode.ChildNodeCount));
    end;

    liLoop := 0;
    while liLoop < pcNode.ChildNodeCount do
    begin
      lcChildNode := pcNode.ChildNodes[liLoop];
      VisitTree(lcChildNode);

      { fbRecalcIndex flag is for speed
        need to deal with shifting indexes when an item is moved or deleted
        but only then. The rest of the time it just slows us down }
      if fbRecalcIndex then
      begin
        { has this node been moved or removed?
        if so, don't increment counter, as the next item will now be in this slot }
        liNewIndex := pcNode.IndexOfChild(lcChildNode);
        fbRecalcIndex := False;

        if liNewIndex >= 0 then
          // proceed to next one
          liLoop := liNewIndex + 1;
          { else case is that liNewIndex is -1 as the current item has been deleted.
            Stay at same index as the next item will now be in this slot }
      end
      else
        inc(liLoop);
    end;

    if fbHasPostVisit then
      fcVisitor.PostVisitParseTreeNode(pcNode);
  end;
end;

procedure TTreeWalker.Visit(const pcRoot: TParseTreeNode; const pcVisitor: TBaseTreeNodeVisitor);
begin
  Assert(pcRoot <> nil);
  Assert(pcVisitor <> nil);
  fcVisitor := pcVisitor;

  InitialiseFlags;

  VisitTree(pcRoot);
end;

end.
