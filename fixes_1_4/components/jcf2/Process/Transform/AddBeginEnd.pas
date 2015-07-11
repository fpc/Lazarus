unit AddBeginEnd;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AddBeginEnd.pas, March 2004.
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

uses BaseVisitor;

type
  TAddBeginEnd = class(TBaseTreeNodeVisitor)
  private

  protected
  public
    constructor Create; override;

    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  SettingsTypes,
  ParseTreeNode, ParseTreeNodeType,
  JcfSettings, SourceToken, Tokens, TokenUtils;

function IsBlockParent(const pcNode: TParseTreeNode): boolean;
const
  BLOCK_PARENTS: TParseTreeNodeTypeSet =
    [nIfBlock, nElseBlock, nCaseSelector, nElseCase,
    nWhileStatement, nForStatement, nWithStatement];
begin
  Result := (pcNode <> nil) and (pcNode.NodeType in BLOCK_PARENTS);
end;

function HasBlockChild(const pcNode: TParseTreeNode): boolean;
var
  liDepth: integer;
begin
  if pcNode.NodeType = nElseCase then
    liDepth := 3
  else
    liDepth := 2;

  { a compound statement is the begin..end block. }
  Result := pcNode.HasChildNode(nCompoundStatement, liDepth);
end;

procedure TestAddSpaceAtEnd(const pcNode: TParseTreeNode);
var
  lcLeaf: TSourceToken;
begin
  lcLeaf := TSourceToken(pcNode.LastLeaf);
  if (lcLeaf = nil) or lcLeaf.IsSolid then
    pcNode.AddChild(NewSpace(1));
end;


{
  True if the node is an if block that has an else statment
}
function IfStatementHasElse(const pcNode: TParseTreeNode): boolean;
var
  lcParent: TParseTreeNode;
begin
  Result := False;

   if (pcNode <> nil) and (pcNode.NodeType = nIfBlock) then
   begin
       lcParent := pcNode.Parent;
        if lcParent.HasChildNode(nElseBlock, 1) then
         Result := True;
   end;
end;

{
  it is not safe to remove the block from:


  case 1:
    if <cond1> then
    begin
      if <cond2> then
        <statement1>
    end
    else
      <statement2>;

  case 2:
    if <cond1> then
      if <cond2> then
         < statement1>
      else
      begin
        if <cond 3> then
        begin
         <statement2>
        end
      end
    else
      <statement3>;
}
function SafeToRemoveBeginEnd(const pcNode: TParseTreeNode): Boolean;
const
  { if there is an if stament with no else case immediately hereunder,
  should find it by this depth }
  IMMEDIATE_IF_DEPTH = 4;
  MAX_IF_CLIMB = 5;
var
  lcParent: TParseTreeNode;
  lcChildStmnt: TParseTreeNode;
  iLoop: integer;
begin
  Result := True;

  { Case 1: is this an if block? }
  if (pcNode <> nil) and (pcNode.NodeType = nIfBlock) then
  begin
    { does it have an else case after the if block? }
    if IfStatementHasElse(pcNode) then
    begin
      lcChildStmnt := pcNode.GetImmediateChild(nStatement);

      { does the if block contain an if statement? }
      if (lcChildStmnt <> nil) and lcChildStmnt.HasChildNode(nIfBlock) then
        { leave the begin-end in place - don't mix up your ifs }
        Result := False;
    end;
  end;

  { case 2 - an else block inside a containing if/else }
  if (pcNode <> nil) and (pcNode.NodeType = nElseBlock) then
  begin
    { is the else inside an outer if block? }
    lcParent := pcNode;

    iLoop := 0;
    while (iLoop < MAX_IF_CLIMB) and (lcParent <> nil) do
    begin
      lcParent := lcParent.Parent;
      inc(iLoop);

      if (lcParent <> nil) and (lcParent.NodeType = nIfBlock) then
        break;
    end;

    if (lcParent <> nil) and (lcParent.NodeType = nIfBlock) then
    begin
      lcParent := lcParent.Parent;
      if (lcParent <> nil) and lcParent.HasChildNode(nElseBlock, 1) then
        Result := False;
      end;
  end;

end;

procedure AddBlockChild(const pcNode: TParseTreeNode);
var
  liIndex: integer;
  lcTop: TParseTreeNode;
  lcStatement, lcCompound, lcStatementList: TParseTreeNode;
  lcBegin, lcEnd: TSourceToken;
  lcPrior: TSourceToken;
begin
  { this is an if block or the like
    with a single statement under it  }
  if pcNode.NodeType = nElseCase then
    lcTop := pcNode.GetImmediateChild(nStatementList)
  else
    lcTop := pcNode;

  lcStatement := lcTop.GetImmediateChild(nStatement);

  if lcStatement = nil then
  begin
    // a dangling else or the like
    liIndex := 0;
  end
  else
  begin
    liIndex := lcTop.IndexOfChild(lcStatement);
    Assert(liIndex >= 0);
  end;

  { temporarily take it out }
  lcTop.ExtractChild(lcStatement);

  { need some new nodes:
    statement
     - compound statement
       - begin
       - statement list
         -  lcStatement
       - end
    }
    
    lcCompound := TParseTreeNode.Create;
    lcCompound.NodeType := nCompoundStatement;
    lcTop.InsertChild(liIndex, lcCompound);

    lcBegin := TSourceToken.Create;
    lcBegin.SourceCode := 'begin';
    lcBegin.TokenType := ttBegin;
    lcCompound.AddChild(lcBegin);
    lcCompound.AddChild(NewSpace(1));

    { check we have got space before the begin }
    lcPrior := lcBegin.PriorToken;
    if (lcPrior <> nil) and lcPrior.IsSolid then
      lcPrior.Parent.InsertChild(lcPrior.IndexOfSelf + 1, NewSpace(1));

    lcStatementList := TParseTreeNode.Create;
    lcStatementList.NodeType := nStatementList;
    lcCompound.AddChild(lcStatementList);

    { the original statement goes in the middle of this }
    if lcStatement <> nil then
      lcStatementList.AddChild(lcStatement);

    TestAddSpaceAtEnd(lcCompound);

    lcEnd := TSourceToken.Create;
    lcEnd.SourceCode := 'end';
    lcEnd.TokenType := ttEnd;
    lcCompound.AddChild(lcEnd);
end;

procedure RemoveBlockChild(const pcNode: TParseTreeNode);
var
  lcTop, lcTopStatement: TParseTreeNode;
  lcCompoundStatement: TParseTreeNode;
  lcStatementList: TParseTreeNode;
  lcStatement: TParseTreeNode;
  liIndex: integer;
  lcComment, lcNext: TSourceToken;
begin
  if pcNode.NodeType = nElseCase then
    lcTop := pcNode.GetImmediateChild(nStatementList)
  else
    lcTop := pcNode;

  Assert(lcTop <> nil);

  lcTopStatement := lcTop.GetImmediateChild(nStatement);
  if lcTopStatement = nil then
    exit;

  liIndex := lcTop.IndexOfChild(lcTopStatement);
  Assert(liIndex >= 0);

  lcCompoundStatement := lcTopStatement.GetImmediateChild(nCompoundStatement);
  if lcCompoundStatement = nil then
    exit;

  lcStatementList := lcCompoundStatement.GetImmediateChild(nStatementList);
  if lcStatementList = nil then
    exit;

  // if this begin...end owns more than one statement, we can't do it
  if lcStatementList.CountImmediateChild(nStatement) > 1 then
    exit;

  lcStatement := lcStatementList.GetImmediateChild(nStatement);

  if lcStatement <> nil then
  begin
    // right, put this single statement in at the top
    lcStatementList.ExtractChild(lcStatement);
    lcTop.InsertChild(liIndex, lcStatement);
  end;

  // find any comments that would be orphaned
  if lcTopStatement.HasChildNode(ttComment) then
  begin
    lcComment := lcTopStatement.FirstLeaf as TSourceToken;
    repeat
      lcNext := lcComment.NextToken;

      if lcComment.TokenType = ttComment then
      begin
        // keep it
        lcComment.Parent.ExtractChild(lcComment);
        lcTop.AddChild(lcComment);
        if lcComment.CommentStyle = eDoubleSlash then
          InsertReturnAfter(lcComment);
      end;

      lcComment := lcNext;
    until (lcComment = nil) or (not lcComment.HasParentNode(lcTopStatement));
  end;

  // and free the rest of the scaffolding
  lcTopStatement.Free;
end;

constructor TAddBeginEnd.Create;
begin
  inherited;

  HasPostVisit := True;
  HasSourceTokenVisit := False;
end;

procedure TAddBeginEnd.PostVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  lcToken: TSourceToken;
begin
  lcNode := TParseTreeNode(pcNode);

  if not IsBlockParent(lcNode) then
    exit;

  case FormatSettings.Transform.BeginEndStyle of
    eNever:
    begin
      if HasBlockChild(lcNode) and SafeToRemoveBeginEnd(lcNode) then
        RemoveBlockChild(lcNode);
    end;
    eAlways:
    begin
      // do not add begin/end to "else if"
      if (lcNode.NodeType = nElseBlock) then
      begin
        lcToken := TSourceToken(lcNode.FirstSolidLeaf);
        if (lcToken <> nil) and (lcToken.TokenType = ttIf) then
          exit;
      end;

      if (not HasBlockChild(lcNode)) then
        AddBlockChild(lcNode);
    end
    else
      // should not be here
      Assert(false);
  end;
end;

function TAddBeginEnd.IsIncludedInSettings: boolean;
begin
  Result := (FormatSettings.Transform.BeginEndStyle <> eLeave);
end;

end.
