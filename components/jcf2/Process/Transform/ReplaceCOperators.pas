unit ReplaceCOperators;
{(*}
(*------------------------------------------------------------------------------
Delphi Code formatter source code

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

{
replaces c style operators +=, -=, *=, /=  with pascal style.
a+=1 ->  a:=a+1
}

{$mode delphi}

interface

uses BaseVisitor;

type
  TReplaceCOperators = class(TBaseTreeNodeVisitor)
  public
    constructor Create; override;
    function VisitSourceToken(const pcToken: TObject): Boolean;override;
    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  SettingsTypes, ParseTreeNode, ParseTreeNodeType,
  JcfSettings, SourceToken, Tokens, TokenUtils;


function HasMoreThanOneTerm(var piCount: integer; pcNode: TParseTreeNode): boolean; overload;
var
  liIndex: integer;
begin
  if pcNode <> nil then
  begin
    if pcNode.NodeType = nTerm then
      Inc(piCount);
    liIndex := 0;
    while (piCount <= 1) and (liIndex < pcNode.ChildNodeCount) do
    begin
      HasMoreThanOneTerm(piCount, pcNode.ChildNodes[liIndex]);
      Inc(liIndex);
    end;
  end;
  Result := piCount > 1;
end;

function HasMoreThanOneTerm(pcNode: TParseTreeNode): boolean; overload;
var
  liCount: integer;
begin
  liCount:=0;
  result := HasMoreThanOneTerm(liCount,pcNode);
end;

constructor TReplaceCOperators.Create;
begin
  inherited;
  HasPostVisit := False;
  HasSourceTokenVisit := True;
end;

function TReplaceCOperators.VisitSourceToken(const pcToken: TObject): boolean;
var
  lcSourceToken, lcOperator, lcNewNode: TSourceToken;
  lcAssignment, lcClonedSubtree, lcExpression, lcTerm: TParseTreeNode;
  liChildIndex: integer;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcToken);
  if lcSourceToken = nil then
    Exit;
  if not (lcSourceToken.TokenType in [ttPlusAssign, ttMinusAssign,
    ttTimesAssign, ttFloatDivAssign]) then
    Exit;
   {  // a*=2;
   Statement
     Designator
       Identifier
         a
     Asignment
       *=
       Expresion
         Term
           Number2
   }

  // find statement begin
  lcAssignment := lcSourceToken.Parent;
  if (lcAssignment = nil) or (lcAssignment.NodeType <> nAssignment) then
    Exit;
  //expresion
  liChildIndex := lcAssignment.IndexOfChild(lcSourceToken);
  if (liChildIndex > (lcAssignment.ChildNodeCount - 1)) or (liChildIndex < 0) then
    Exit;
  lcExpression := lcAssignment.ChildNodes[liChildIndex + 1];

  if (lcExpression = nil) or (lcExpression.NodeType <> nExpression) then
    Exit;
  //term
  if lcExpression.ChildNodeCount < 1 then
    Exit;
  lcTerm := lcExpression.ChildNodes[0];
  if (lcTerm = nil) or (lcTerm.NodeType <> nTerm) then
    Exit;
  // add brackets if there are more than one term.
  // x-=1-2;    -->  x:=x - (1-2);
  // x*=9+1;    -->  x:=x * (9+1);
  if HasMoreThanOneTerm(lcExpression) then
  begin
    lcNewNode := TSourceToken.Create;
    lcNewNode.NodeType := nExpression;
    lcTerm.InsertChild(0, lcNewNode);

    lcNewNode := TSourceToken.Create;
    lcNewNode.NodeType := nLeaf;
    lcNewNode.TokenType := ttOpenBracket;
    lcNewNode.SourceCode := '(';
    lcTerm.InsertChild(1, lcNewNode);

    lcNewNode := TSourceToken.Create;
    lcNewNode.NodeType := nLeaf;
    lcNewNode.TokenType := ttCloseBracket;
    lcNewNode.SourceCode := ')';
    lcExpression.AddChild(lcNewNode);
  end;
  // add operator
  lcOperator := TSourceToken.Create;
  lcOperator.NodeType := nLeaf;
  case lcSourceToken.TokenType of
    ttPlusAssign:
    begin
      lcOperator.TokenType := ttPlus;
      lcOperator.SourceCode := '+';
    end;
    ttMinusAssign:
    begin
      lcOperator.TokenType := ttMinus;
      lcOperator.SourceCode := '-';
    end;
    ttTimesAssign:
    begin
      lcOperator.TokenType := ttTimes;
      lcOperator.SourceCode := '*';
    end;
    ttFloatDivAssign:
    begin
      lcOperator.TokenType := ttFloatDiv;
      lcOperator.SourceCode := '/';
    end;
  end;
  lcTerm.InsertChild(0, lcOperator);

  // add cloned left hand side subtree.
  lcClonedSubTree := lcAssignment.Parent.Clone;
  lcClonedSubTree.Assign(lcAssignment.Parent, lcTerm,0, lcAssignment.Parent.IndexOfChild(lcAssignment)-1);
  lcClonedSubTree.NodeType := nTerm;
  lcTerm.InsertChild(0, lcClonedSubTree);

  // change operator type.
  lcSourceToken.TokenType := ttAssign;
  lcSourceToken.SourceCode := ':=';
  Result := True;
end;

function TReplaceCOperators.IsIncludedInSettings: boolean;
begin
  // eAlways not implemented.
  Result := (FormattingSettings.Transform.ReplaceCStyleOperators = eNever);
end;

end.

