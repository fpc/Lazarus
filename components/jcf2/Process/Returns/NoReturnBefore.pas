unit NoReturnBefore;

{ AFS 11 Jan 2003
  Some tokens should not have a return before them for fomatting
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is NoReturnBefore, released May 2003.
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

uses SwitchableVisitor;

type
  TNoReturnBefore = class(TSwitchableVisitor)
  private
    fbSafeToRemoveReturn: boolean;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses SourceToken, TokenUtils, Tokens, ParseTreeNodeType,
  JcfSettings, FormatFlags, ParseTreeNode, SettingsTypes;

function HasNoReturnBefore(const pt: TSourceToken): boolean;
const
  NoReturnTokens: TTokenTypeSet    = [ttAssign, ttColon, ttSemiColon, ttPlusAssign, ttMinusAssign, ttTimesAssign, ttFloatDivAssign];
  ProcNoReturnWords: TTokenTypeSet = [ttThen, ttDo];
var
  lcPrev: TParseTreeNode;
begin
  Result := False;

  if pt = nil then
    exit;

  if pt.HasParentNode(nAsm) then
    exit;

  { a semicolon should have a return before it if it is the only token in the statement
    e.g.
    begin
     ;
    end
    }

  if (pt.TokenType = ttSemiColon) then
  begin
    lcPrev := pt.Parent.FirstNodeBefore(pt);
    if (lcPrev <> nil) and (lcPrev.NodeType = nStatement) then
      exit;
  end;

  if (pt.TokenType in NoReturnTokens + Operators) then
    exit(True);

  { class helper declaration }
  if IsClassHelperWords(pt) then
    exit(True);

  { no return before then and do  in procedure body }
  if (pt.TokenType in ProcNoReturnWords) and InStatements(pt) then
    exit(True);

  { no return in record def before the record keyword, likewise class & interface
    be carefull with the word 'class' as it also denotes (static) class fns. }
  if pt.HasParentNode(nTypeDecl) and (pt.TokenType in StructuredTypeWords) and
    ( not pt.HasParentNode(nClassVisibility)) then
    exit(True);

  if (pt.TokenType = ttCloseSquareBracket) then
  begin
    // end of guid in interface
    if pt.HasParentNode(nInterfaceTypeGuid, 1) then
      exit(True);

    if pt.HasParentNode(nAttribute) then
      exit(True);
  end;

  // "foo in  Foo.pas, " has return only after the comma
  if InFilesUses(pt) then
  begin
    if (pt.TokenType in [ttComma, ttWord, ttQuotedLiteralString]) or
      ((pt.TokenType = ttComment) and (pt.CommentStyle in CURLY_COMMENTS)) then
      exit(True);
  end;

  if (pt.CommentStyle = eCompilerDirective) and (CompilerDirectiveLineBreak(pt, True) = eNever) then
    exit(True);

end;

constructor TNoReturnBefore.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

function TNoReturnBefore.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lcNext, lcNextComment: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  // not safe to remove return at a comment like this
  if (lcSourceToken.TokenType = ttComment) and
    (lcSourceToken.CommentStyle = eDoubleSlash) then
    fbSafeToRemoveReturn := False
  else if (lcSourceToken.TokenType <> ttReturn) then
    fbSafeToRemoveReturn := True;
  // safe again after the next return

  if (lcSourceToken.TokenType = ttReturn) and fbSafeToRemoveReturn then
  begin
    lcNext := lcSourceToken.NextTokenWithExclusions([ttReturn, ttWhiteSpace]);

    // skip past regular comments
    while (lcNext <> nil) and (lcNext.TokenType = ttComment) and
      (lcNext.CommentStyle <> eCompilerDirective) do
        lcNext := lcNext.NextTokenWithExclusions([ttReturn, ttWhiteSpace]);

    if (lcNext <> nil) and HasNoReturnBefore(lcNext) then
    begin
      { must still check for the case of
          try
            Statement;
          except
            // a comment
            ;
          end;

      -- the return before the comment should not be removed

      This does not hold in a program files uses clause or before a compiler directive
      }
      lcNextComment := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
      if (lcNextComment <> nil) and
        ((lcNextComment.TokenType <> ttComment) or
         (lcNextComment.CommentStyle = eCompilerDirective) or
         (InFilesUses(lcNextComment))) then
        BlankToken(lcSourceToken);
    end;
  end;
end;

function TNoReturnBefore.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Returns.RemoveBadReturns;
end;

end.
