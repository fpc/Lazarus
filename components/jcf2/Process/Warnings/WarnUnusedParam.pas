unit WarnUnusedParam;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is WarnUnusedParam, released May 2005.
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

{ AFS May 2005
 emit a warning for any procedure or function param
 that is not used in the procedure body

 e.g.
 function Foo(a, b: integer): integer;
 begin
   Result := a + 1;
 end;

 Should emit a warning that param b is not used.
}

uses Warning, ParseTreeNode;

type

  TWarnUnusedParam = class(TWarning)
  private
    fcRootNode: TObject;

    procedure CheckFormalParamUsage(const pcParam, pcBlock: TParseTreeNode);
    procedure CheckIdentifierUsage(const psIdentName: string;
      const pcBlock: TParseTreeNode);
    function IsIdentifierUsedInParseTree(const psIdentName: string;
      const pcRoot: TParseTreeNode): boolean;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    procedure PreVisitParseTreeNode(const pcNode: TObject); override;
  end;

implementation

uses
  { delphi }
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils,
  { local }
  ParseTreeNodeType, SourceToken, TokenUtils, JCfSettings;

constructor TWarnUnusedParam.Create;
begin
  inherited;

  HasPreVisit := True;
  HasPostVisit := False;
  HasSourceTokenVisit := False;
end;

procedure TWarnUnusedParam.PreVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
  lcBlock, lcFunctionHeading, lcParams, lcParam: TParseTreeNode;
  liLoop: integer;
begin
  lcNode := TParseTreeNode(pcNode);

  if not (lcNode.NodeType in ProcedureNodes) then
    exit;

  fcRootNode := pcNode;

  { should have a function heading }
  lcFunctionHeading := lcNode.GetImmediateChild(ProcedureHeadings);
  if lcFunctionHeading = nil then
    exit;

  { some procs have no params }
  lcParams := lcFunctionHeading.GetImmediateChild([nFormalParams]);
  if lcParams = nil then
    exit;

  { if it's not a forward decl, will have a code block }
  lcBlock := lcNode.GetImmediateChild([nBlock]);
  if lcBlock = nil then
    exit;

  { process the params one by one }
  for liLoop := 0 to lcParams.ChildNodeCount - 1 do
  begin
    lcParam := lcParams.ChildNodes[liLoop];
    if lcParam.NodeType = nFormalParam then
      CheckFormalParamUsage(lcParam, lcBlock);
  end;
end;

procedure TWarnUnusedParam.CheckFormalParamUsage(const pcParam, pcBlock: TParseTreeNode);
var
  lcIdents: TParseTreeNode;
  liLoop: integer;
  lcIdent: TParseTreeNode;
  lcIdentLeaf: TSourceToken;
  psIdentName: string;
begin
  { a parameter has a list of identifiers, e.g. "procedure foo(a, b,c: integer);" }
  lcIdents := pcParam.GetImmediateChild([nIdentList]);

  for liLoop := 0 to lcIdents.ChildNodeCount - 1 do
  begin
    lcIdent := lcIdents.ChildNodes[liLoop];
    if lcIdent.NodeType = nIdentifier then
    begin
      lcIdentLeaf := TSourceToken(lcIdent.FirstSolidLeaf);
      psIdentName := lcIdentLeaf.SourceCode;
      CheckIdentifierUsage(psIdentName, pcBlock);
    end;
  end;
end;

procedure TWarnUnusedParam.CheckIdentifierUsage(const psIdentName: string; const pcBlock: TParseTreeNode);
var
  lbFound: boolean;
begin
  // is this param name on the ignore list?
  if FormatSettings.Clarify.IgnoreUnusedParams.IndexOf(psIdentName) >= 0  then
    exit;

  lbFound := IsIdentifierUsedInParseTree(psIdentName, pcBlock);

  if not lbFound then
  begin
    SendWarning(fcRootNode, 'Parameter ' + psIdentName + ' is not used');
  end;
end;

{ recurse to find it }
function TWarnUnusedParam.IsIdentifierUsedInParseTree(const psIdentName: string; const pcRoot: TParseTreeNode): boolean;
var
  lcToken: TSourceToken;
  liLoop: integer;
begin
  Result := False;

  if pcRoot is TSourceToken then
  begin
    lcToken := TSourceToken(pcRoot);

    if IsIdentifierToken(lcToken, idAny) and AnsiSameText(lcToken.SourceCode, psIdentName) then
    begin
      // found it
      Result := True;
    end;
  end
  else
  begin
    for liLoop := 0 to pcRoot.ChildNodeCount - 1 do
    begin
      Result := IsIdentifierUsedInParseTree(psIdentName, pcRoot.ChildNodes[liLoop]);
      // found it down there?
      if Result then
        break;
    end;
  end;
end;


function TWarnUnusedParam.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Clarify.WarnUnusedParams;
end;

end.
