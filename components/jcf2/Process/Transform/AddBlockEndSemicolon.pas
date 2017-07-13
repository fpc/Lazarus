unit AddBlockEndSemicolon;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is AddBlockEndSemicolon.pas, March 2004.
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
  TBlockEndSemicolon = class(TBaseTreeNodeVisitor)
  private

  protected
  public
    constructor Create; override;

    procedure PostVisitParseTreeNode(const pcNode: TObject); override;
    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses ParseTreeNode, ParseTreeNodeType,
  JcfSettings, SourceToken, Tokens, TokenUtils;

constructor TBlockEndSemicolon.Create;
begin
  inherited;

  HasPostVisit := True;
  HasSourceTokenVisit := False;
end;

procedure TBlockEndSemicolon.PostVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  lcStatementList: TParseTreeNode;
  lcEnd: TSourceToken;
  lcNew: TSourceToken;
begin
  lcNode := TParseTreeNode(pcNode);

  { looking for a compound statement with begin..end }
  if lcNode.NodeType <> nCompoundStatement then
    exit;
  if not lcNode.HasChildNode(ttBegin, 1) then
    exit;
  if not lcNode.HasChildNode(ttEnd, 1) then
    exit;

  { extract the statement list between the begin & end }
  lcStatementList := lcNode.GetImmediateChild(nStatementList);
  if lcStatementList = nil then
    exit;

  { what's the last source token in this block ? }
  lcEnd := TSourceToken(lcStatementList.LastLeaf);
  if lcEnd = nil then
    exit;
    
  if not lcEnd.IsSolid then
    lcEnd := lcEnd.PriorSolidToken;

  if lcEnd.TokenType <> ttSemiColon then
  begin
    lcNew := TSourceToken.Create;
    lcNew.SourceCode := ';';
    lcNew.TokenType := ttSemiColon;
    
    InsertTokenAfter(lcEnd, lcNew);
  end;
end;


function TBlockEndSemicolon.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Transform.AddBlockEndSemiColon;
end;

end.
