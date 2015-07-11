unit IndentAsmParam;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is IndentAsmParam, released October 2007.
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

{
  process to indent asm params to the required stop
  this is similar to the alin processes
  but it does not make the params the same indent as the furthest one
  it puts them all at the same fixed indent, if possible
}

uses SwitchableVisitor;

type
  TIndentAsmParam = class(TSwitchableVisitor)
  private
    fcLastProcessed: TObject;
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  ParseTreeNodeType, ParseTreeNode, Nesting,
  TokenUtils,
  SourceToken,
  JcfSettings;

function IsFirstAsmParam(const pcSourceToken: TSourceToken): boolean;
var
  lcPrev: TSourceToken;
begin
  Result := False;

  if not IsInsideAsm(pcSourceToken) then
    exit;

  // looking for the first asm param on the line
  if not pcSourceToken.HasParentNode(nAsmParam, 4) then
    exit;

  lcPrev := pcSourceToken.PriorSolidToken;

  if (lcPrev <> nil) then
  begin
    if lcPrev.HasParentNode(nAsmOpCode, 3) then
    begin
      Result := True;
    end;
  end;
end;

function GetAsmIndent(const pcSourceToken: TSourceToken): integer;
var
  liIndentLevel: integer;
  lcAsm: TParseTreeNode;
begin
  liIndentLevel := pcSourceToken.Nestings.GetLevel(nlBlock);

  // nesting of asm inside a proc is treaded differently from an asm proc
  // which is not a good feature
  // but can work with it
  lcAsm := pcSourceToken.GetParentNode(nAsm);
  if lcAsm = nil then
  begin
    Result := 0;
    exit;
  end;
  if lcAsm.Parent.NodeType = nStatement then
  begin
    inc(liIndentLevel);
  end;

  Result := FormatSettings.Indent.SpacesForIndentLevel(liIndentLevel); 
  Result := Result + FormatSettings.SetAsm.ParamsIndent;
end;

function TIndentAsmParam.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  liDesiredIndent: integer;
  liActualIndent: integer;
begin
  Result := False;

  // prevent it being processed again after a space is inserted
  if pcNode = fcLastProcessed then
    exit;

  lcSourceToken := TSourceToken(pcNode);

  if IsFirstAsmParam(lcSourceToken) then
  begin
    liDesiredIndent := GetAsmIndent(lcSourceToken);
    liActualIndent := lcSourceToken.XPosition - 1;

    if liActualIndent < liDesiredIndent then
    begin
      InsertTokenBefore(lcSourceToken, NewSpace(liDesiredIndent - liActualIndent));
      lcSourceToken.XPosition := liDesiredIndent;
      fcLastProcessed := lcSourceToken;
      Result := True;
    end;
  end;

end;

function TIndentAsmParam.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.SetAsm.ParamsIndentEnabled;
end;

end.
