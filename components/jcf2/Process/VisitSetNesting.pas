unit VisitSetNesting;

{ visitor to set up nesting levels }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is VisitSetNesting, released May 2003.
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

uses
  Contnrs, SysUtils,
  { local }
  BaseVisitor, Nesting;

type

  TVisitSetNestings = class(TBaseTreeNodeVisitor)
  private
    fcRunningTotals: TNestingLevelList;
    fcIndentNodes: TObjectList;

    procedure ProcessNode(const pcNode: TObject; const pbIncrement: boolean);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure PreVisitParseTreeNode(const pcNode: TObject); override;
    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    function VisitSourceToken(const pcToken: TObject): Boolean; override;

    function FinalSummary(out psMessage: string): boolean; override;
  end;


implementation

uses
  ParseTreeNode, ParseTreeNodeType,
  Tokens, SourceToken;

constructor TVisitSetNestings.Create;
begin
  inherited;

  fcRunningTotals := TNestingLevelList.Create;
  fcIndentNodes   := TObjectList.Create;
  fcIndentNodes.OwnsObjects := False;

  HasPreVisit := True;
  HasPostVisit := True;
  HasSourceTokenVisit := True;
end;

destructor TVisitSetNestings.Destroy;
begin
  FreeAndNil(fcRunningTotals);
  FreeAndNil(fcIndentNodes);
  inherited;
end;

function TVisitSetNestings.FinalSummary(out psMessage: string): boolean;
begin
  psMessage := fcRunningTotals.FinalTest;
  Result    := (psMessage <> '');
end;

procedure TVisitSetNestings.PreVisitParseTreeNode(const pcNode: TObject);
begin
  // increment when you enter
  ProcessNode(pcNode, True);
end;

procedure TVisitSetNestings.PostVisitParseTreeNode(const pcNode: TObject);
begin
  // decrement when you exit
  ProcessNode(pcNode, False);
end;

procedure TVisitSetNestings.ProcessNode(const pcNode: TObject;
  const pbIncrement: boolean);
var
  lcNode:     TParseTreeNode;
  lcNextLeaf: TSourceToken;
  leNestType: TNestingLevelType;
  lbHasNesting: boolean;
begin
  lbHasNesting := False;
  leNestType   := nlProcedure; // must have value to supress warning

  lcNode := TParseTreeNode(pcNode);

  case lcNode.NodeType of
    nBlock, nCaseStatement,
    nIfBlock, nTryBlock, nFinallyBlock, nExceptBlock,
    nRepeatStatement, nWhileStatement, nForStatement,
    nWithStatement, nOnExceptionHandler, nInitSection:
    begin
      leNestType   := nlBlock;
      lbHasNesting := True;
    end;
    nElseBlock:
    begin
      { if the else is immediately followed by if then it is not a block indent }
      lcNextLeaf := TSourceToken(lcNode.FirstLeaf);
      if lcNextLeaf <> nil then
        lcNextLeaf := lcNextLeaf.NextSolidToken;

      if (lcNextLeaf = nil) or (lcNextLeaf.TokenType <> ttIf) then
      begin
        leNestType   := nlBlock;
        lbHasNesting := True;
      end;
    end;
    nCaseSelector, nElseCase:
    begin
      leNestType   := nlCaseSelector;
      lbHasNesting := True;
    end;
    nRecordType:
    begin
      leNestType   := nlRecordType;
      lbHasNesting := True;
    end;
    nRecordVariantSection:
    begin
      leNestType   := nlRecordVariantSection;
      lbHasNesting := True;
    end;
    nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl:
    begin
      leNestType   := nlProcedure;
      lbHasNesting := True;
    end;
    nStatement:
    begin
      { the statement with the label is nested }
      if (lcNode.ChildNodeCount > 0) and (lcNode.ChildNodes[0].NodeType =
        nStatementLabel) then
      begin
        leNestType   := nlStatementLabel;
        lbHasNesting := True;
      end;
    end;
  end;

  { test for a begin..end block with no other indent }
  if ( not lbHasNesting) and (lcNode.Parent <> nil) and
    (lcNode.NodeType = nCompoundStatement) then
  begin
    if (fcIndentNodes.IndexOf(lcNode.Parent) < 0) and
      ((fcIndentNodes.IndexOf(lcNode.Parent.Parent) < 0) or
      (lcNode.Parent.NodeType <> nStatement)) and
      ( not lcNode.HasParentNode(nElseCase, 3)) then
    begin
      leNestType   := nlBlock;
      lbHasNesting := True;
    end;
  end;

  if lbHasNesting then
  begin
    if pbIncrement then
      fcRunningTotals.IncLevel(leNestType)
    else
      fcRunningTotals.DecLevel(leNestType);

    if fcIndentNodes.IndexOf(pcNode) < 0 then
      fcIndentNodes.Add(pcNode);
  end;
end;

function TVisitSetNestings.VisitSourceToken(const pcToken: TObject): Boolean;
var
  lcToken: TSourceToken;
begin
  Result := False;
  lcToken := TSourceToken(pcToken);

  case lcToken.TokenType of
    ttCloseBracket:
      fcRunningTotals.DecLevel(nlRoundBracket);
    ttCloseSquareBracket:
      fcRunningTotals.DecLevel(nlSquareBracket);
  end;


  // store the total so far on this leaf
  lcToken.Nestings.Assign(fcRunningTotals);

  case lcToken.TokenType of
    ttOpenBracket:
      fcRunningTotals.IncLevel(nlRoundBracket);
    ttOpenSquareBracket:
      fcRunningTotals.IncLevel(nlSquareBracket);
  end;

end;


end.
