unit WarnAssignToFunctionName;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is WarnAssignToFunctionName, released May 2003.
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

{ AFS 21 Sept 2001

 warn of assignment to function name in old TurboPascal code

 ie
  function Fred: integer;
  begin
    Fred := 3;
  end;

 should be

  function Fred: integer;
  begin
    Result := 3;
  end;
}

uses Warning;

type

  TWarnAssignToFunctionName = class(TWarning)
  private
    procedure WarnAllAssigns(const psFnName: string; const pcRoot: TObject);
  public
    constructor Create; override;

    procedure PreVisitParseTreeNode(const pcNode: TObject); override;
  end;


implementation

uses
  { delphi }
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils,
  { local }
  ParseTreeNode, ParseTreeNodeType, SourceToken, Tokens, TokenUtils;



{ get the node that represents the identifier that is being assigned to
  node passed in will be statement

  looking for the last id before the ':=',

  e.g. in "TFoo(bar.baz) := fish;" we want "baz"

  NB this may not work in complex examples as the id may be under an expr node
  but may suffice for this fn name assign detection
  }
function GetIdentifierBeforeAssign(const pcNode: TParseTreeNode): TSourceToken;
var
  liLoop: integer;
  lcDes:  TParseTreeNode;
  lcChildNode: TParseTreeNode;
  lcSourceToken: TSourceToken;
begin
  Result := nil;
  Assert(pcNode <> nil);

  lcDes := pcNode.GetImmediateChild(nDesignator);
  Assert(lcDes <> nil);

  for liLoop := 0 to lcDes.ChildNodeCount - 1 do
  begin
    lcChildNode := lcDes.ChildNodes[liLoop];

    if lcChildNode.NodeType = nIdentifier then
    begin
      lcSourceToken := lcChildNode.FirstSolidLeaf as TSourceToken;

      if lcSourceToken.WordType in IdentifierTypes then
        Result := lcSourceToken;
    end
    else if lcChildNode.NodeType = nBracketedQual then
    begin
      // go inside the brackets - should be a designator in there
      Result := GetIdentifierBeforeAssign(lcChildNode);
    end
    else if lcChildNode.NodeType = nAssignment then
      break;

  end;
end;

constructor TWarnAssignToFunctionName.Create;
begin
  inherited;

  HasPreVisit := True;
  HasPostVisit := False;
  HasSourceTokenVisit := False;
end;

procedure TWarnAssignToFunctionName.PreVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  lcFunctionHeading: TParseTreeNode;
  lsName: string;
begin
  lcNode := TParseTreeNode(pcNode);

  if lcNode.NodeType <> nFunctionDecl then
    exit;

  { we now have a function decl
    Find the name, find the assign statements. Compare }
  lcFunctionHeading := lcNode.GetImmediateChild([nFunctionHeading]);
  Assert(lcFunctionHeading <> nil);

  lsName := ExtractNameFromFunctionHeading(lcFunctionHeading, False);

  WarnAllAssigns(lsName, lcNode);
end;

procedure TWarnAssignToFunctionName.WarnAllAssigns(const psFnName: string;
  const pcRoot: TObject);
var
  lcNode:     TParseTreeNode;
  lcLeftName: TSourceToken;
  liLoop:     integer;
begin
  Assert(pcRoot <> nil);
  lcNode := TParseTreeNode(pcRoot);

  if (lcNode.NodeType = nStatement) and (lcNode.HasChildNode(nAssignment, 1)) then
  begin

    // this is an assign statement. Look at the LHS
    lcLeftName := GetIdentifierBeforeAssign(lcNode);

    Assert(lcLeftName <> nil, 'No id before assign');

    if AnsiSameText(lcLeftName.SourceCode, psFnName) then
    begin
      SendWarning(lcLeftName,
        'Assignment to the function name "' + psFnName +
        '" is deprecated, Use assignment to "Result"');
    end;
  end
  else
  begin
    // look at all nodes under here
    for liLoop := 0 to lcNode.ChildNodeCount - 1 do
      WarnAllAssigns(psFnName, lcNode.ChildNodes[liLoop]);
  end;
end;

end.
