unit RemoveComment;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is RemoveComment, released May 2003.
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

{ AFS 28 Dec 2002

  Obfuscate by removing comments
}

uses SwitchableVisitor;

type
  TRemoveComment = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;


implementation

uses
  JcfStringUtils,
  SourceToken, Tokens, ParseTreeNodeType, FormatFlags;

function CommentMustStay(const pc: TSourceToken): boolean;
var
  lsPrefix: string;
begin
  Result := False;

  lsPrefix := StrLeft(pc.SourceCode, 2);
  if (lsPrefix = '{$') or (lsPrefix = '{%') then
    Result := True;

  { all curly backets in the uses clause of a program/library def
   must be respected as they link files to dfms, com classes 'n stuff }
  if (pc.CommentStyle in CURLY_COMMENTS) and
    (pc.HasParentNode(TopOfProgramSections)) and pc.HasParentNode(UsesClauses) and
    pc.IsOnRightOf(UsesClauses, UsesWords) then
    Result := True;

  { these comments are flags to the code format program, so leave them }
  if (pc.SourceCode = '{(*}') or (pc.SourceCode = '{*)}') then
    Result := True;

  // these are also flags
  if ((pc.CommentStyle = eDoubleSlash) and
    (StrLeft(pc.SourceCode, FORMAT_COMMENT_PREFIX_LEN) = FORMAT_COMMENT_PREFIX)) then
    Result := True;

end;

constructor TRemoveComment.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

function TRemoveComment.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  (* turn comment to space - may be needed for token sep
    e.g. may be for a :=b{foo}to{bar}baz
  *)
  if lcSourceToken.TokenType = ttComment then
  begin
    if not CommentMustStay(lcSourceToken) then
    begin
      lcSourceToken.TokenType  := ttWhiteSpace;
      lcSourceToken.SourceCode := ' ';
    end;
  end;
end;

end.
