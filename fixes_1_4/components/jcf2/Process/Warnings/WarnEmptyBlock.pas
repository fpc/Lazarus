unit WarnEmptyBlock;

{ AFS 30 Dec 2002
 warn of an enmpty block, one of
 begin..end, try..except, try..finally, except..end, finally..end
}


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is WarnEmptyBlock, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

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

uses Warning;

type

  TWarnEmptyBlock = class(TWarning)
  public
    constructor Create; override;

    procedure PreVisitParseTreeNode(const pcNode: TObject); override;
  end;

implementation

uses ParseTreeNode, ParseTreeNodeType;

constructor TWarnEmptyBlock.Create;
begin
  inherited;

  HasPreVisit := True;
  HasPostVisit := False;
  HasSourceTokenVisit := False;
end;

procedure TWarnEmptyBlock.PreVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  liSolidChildCount: integer;
begin
  lcNode := TParseTreeNode(pcNode);

  // only look in statements
  if not lcNode.HasParentNode(nBlock) then
    exit;

  { looking for nodes with 2 solid tokens under them
    e.g. 'begin' and 'end'
  }
  liSolidChildCount := lcNode.SolidChildCount;

  if liSolidChildCount = 2 then
  begin
    if lcNode.NodeType = nCompoundStatement then
    begin
      SendWarning(lcNode, 'Empty begin..end block');
    end;

    if lcNode.NodeType = nFinallyBlock then
    begin
      SendWarning(lcNode, 'Empty finally..end block');
    end;

    if lcNode.NodeType = nExceptBlock then
    begin
      SendWarning(lcNode, 'Empty except..end block');
    end;
  end
  else if liSolidChildCount = 1 then
  begin
    if lcNode.NodeType = nTryBlock then
    begin
      SendWarning(lcNode, 'Empty try block');
    end;
  end;

end;

end.
