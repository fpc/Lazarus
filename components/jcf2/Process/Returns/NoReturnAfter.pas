unit NoReturnAfter;

{ AFS 11 Jan 2003
  Some tokens should not have a return after them for fomatting
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is NoReturnAfter, released May 2003.
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

uses SourceToken, SwitchableVisitor;


type
  TNoReturnAfter = class(TSwitchableVisitor)
  private
    fcLastSolidToken: TSourceToken;
    fbDoneWork: boolean;

    function NoDeclarationBefore: boolean;
    function CommentBefore: boolean;
    function NoSemiColonBefore: boolean;

    function NeedsNoReturn(const pt: TSourceToken): boolean;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;

    property DoneWork: boolean Read fbDoneWork;
  end;


implementation

uses Tokens, ParseTreeNodeType, TokenUtils,
  SetReturns, JcfSettings, FormatFlags, SettingsTypes;


constructor TNoReturnAfter.Create;
begin
  inherited;
  fcLastSolidToken := nil;
  fbDoneWork  := False;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

function TNoReturnAfter.NeedsNoReturn(const pt: TSourceToken): boolean;
const
  NoReturnWords: TTokenTypeSet = [ttProcedure, ttFunction,
    ttConstructor, ttDestructor, ttProperty, ttGoto];
var
  lcSetReturns: TSetReturns;
  lcNext, lcNext2: TSourceToken;
begin
  Result := False;

  if pt = nil then
    exit;

  if pt.HasParentNode(nAsm) then
    exit;

  lcSetReturns := FormatSettings.Returns;
  Assert(lcSetReturns <> nil);

  if FormatSettings.Returns.RemoveBadReturns then
  begin

    if pt.TokenType in NoReturnWords then
    begin
      Result := True;
      exit;
    end;

    { class helper declaration }
    if IsCLassHelperWords(pt) then
    begin
      Result := True;
      exit;
    end;

    { only place a return after a colon is legit is at a label
      in a proc body }
    if pt.TokenType = ttColon then
    begin
      if ( not InStatements(pt)) and (RoundBracketLevel(pt) = 0) then
      begin
        Result := True;
        exit;
      end;
    end;

    { var x absolute y;  just after absolute is a bad place to break }
    if (pt.TokenType = ttAbsolute) and pt.HasParentNode(nVarDecl) then
    begin
      Result := True;
      exit;
    end;

    { Default property values:
      No return after 'default' in property def on non-array property
      because it's always followed by a number, e.g.
      property FloonCount: integer default 12;
      as opposed to default (array) properties,
      eg property Items[piIndex: integer]; default; }
    if (pt.TokenType = ttDefault) and pt.HasParentNode(nPropertySpecifier) then
    begin
      { use the persence of semicolon to distinguish
      Default property values from default (array) properties }
      if not SemiColonNext(pt) then
      begin
        Result := True;
        exit;
      end;
    end;

    { array property params - no returns }
    if (SquareBracketLevel(pt) > 0) and pt.HasParentNode(nPropertyParameterList) then
    begin
      { use the persence of semicolon to distinguish
      Default property values from default (array) properties }
      if not SemiColonNext(pt) then
      begin
        Result := True;
        exit;
      end;
    end;

    { in procedure params - no return after 'var' or 'const' }
    if pt.HasParentnode(nFormalParams) and (RoundBracketLevel(pt) > 0) then
    begin
      if pt.TokenType in [ttVar, ttConst] then
      begin
        Result := True;
        exit;
      end;
    end;

    { in procedure body - no return directly after 'if' }
    if InStatements(pt) then
    begin
      if pt.TokenType = ttIf then
      begin
        Result := True;
        exit;
      end;
    end;

    if (pt.CommentStyle = eCompilerDirective) and (CompilerDirectiveLineBreak(pt, False) = eNever) then
    begin
      Result := True;
      exit;
    end;

  end;

  { remove returns based on options }

  { the options don't apply after comments }
  if (pt.TokenType = ttComment) then
  begin
    Result := False;
    exit;
  end;

  { or just before them }
  lcNext := pt.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
  if lcNext = nil then
    exit;

  if (lcNext.TokenType = ttComment) or CommentBefore then
  begin
    Result := False;
    exit;
  end;

  if lcSetReturns.RemoveExpressionReturns and pt.HasParentNode(nExpression) then
  begin
    { can have a block that ends in expression without a semicolon, eg Don't remove return here:
      begin
        a := a + 2
      end;    }

    if lcNext.HasParentNode(nExpression) then
    begin
      Result := True;
      exit;
    end;
  end;

  if lcSetReturns.RemoveVarReturns and (pt.TokenType <> ttSemiColon) and
    ( not (pt.TokenType in Declarations)) and pt.HasParentNode(nVarDecl) then
  begin
    if NoDeclarationBefore and NoSemicolonBefore and lcNext.HasParentNode(nVarDecl) then
    begin
      Result := True;
      exit;
    end;
  end;

  if lcSetReturns.RemoveProcedureDefReturns and pt.HasParentNode(nFormalParams) then
  begin
    Result := True;
    exit;
  end;

  if (pt.TokenType = ttOpenSquareBracket) then
  begin
    // start of guid in interface
    if pt.HasParentNode(nInterfaceTypeGuid, 1) then
    begin
      Result := True;
      exit;
    end;

    // start of attribute
    if pt.HasParentNode(nAttribute, 1) then
    begin
      Result := True;
      exit;
    end;
  end;

  // "foo in  Foo.pas, " has return only after the comma
  if InFilesUses(pt) then
  begin
    if (pt.TokenType in [ttComma, ttWord, ttQuotedLiteralString]) or
      ((pt.TokenType = ttComment) and (pt.CommentStyle in CURLY_COMMENTS)) then
    begin
      Result := True;
      exit;
    end;
  end;

  if pt.HasParentNode(nUses) then
  begin
    // no blank line in uses
    lcNext := pt.NextTokenWithExclusions([ttWhitespace]);
    if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
    begin
      lcNext2 := lcNext.NextTokenWithExclusions([ttWhitespace]);
      if (lcNext2 <> nil) and (lcNext2.TokenType = ttReturn) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;

  { "strict private"  - no return in the middle }
  if (pt.TokenType = ttStrict) and (pt.HasParentNode(nClassVisibility, 1)) then
  begin
    Result := True;
  end;
end;

function TNoReturnAfter.NoDeclarationBefore: boolean;
begin
  Result := (fcLastSolidToken = nil) or
    ( not (fcLastSolidToken.TokenType in Declarations));
end;

function TNoReturnAfter.NoSemiColonBefore: boolean;
begin
  Result := (fcLastSolidToken = nil) or
    ( not (fcLastSolidToken.TokenType = ttSemiColon));
end;

function TNoReturnAfter.CommentBefore: boolean;
begin
  Result := (fcLastSolidToken <> nil) and (fcLastSolidToken.TokenType = ttComment)
end;

function TNoReturnAfter.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType = ttReturn) and
    (fcLastSolidToken <> nil) and NeedsNoReturn(fcLastSolidToken) then
  begin
    // must repeat this until all done
    BlankToken(lcSourceToken);
    fbDoneWork := True;
  end
  else
  begin

    { store for next time }
    if not (lcSourceToken.TokenType in [ttWhiteSpace, ttReturn]) then
      fcLastSolidToken := lcSourceToken;
  end;
end;

function TNoReturnAfter.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Returns.RemoveBadReturns;
end;

end.
