unit NoSpaceBefore;

{ AFS 5 Jan 2002
  No space before certain tokens (e.g. '.' ';'
  the Colon has it's own unit }


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is NoSpaceBefore, released May 2003.
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
  TNoSpaceBefore = class(TSwitchableVisitor)
  private
    fbSafeToRemoveReturn: boolean;  // this taken from NoReturnBefore
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses JcfSettings, SourceToken, Tokens, ParseTreeNodeType,
  FormatFlags, TokenUtils, SettingsTypes;

const
  NoSpaceAnywhere: TTokenTypeSet = [ttDot, ttComma,
    ttCloseSquareBracket, ttCloseBracket];

function HasNoSpaceBefore(const pt: TSourceToken): boolean;
var
  lcPrev: TSourceToken;
begin
  Result := False;

  if pt = nil then
    exit;

  { do not apply this rule just after a conditional compilation directive }
  lcPrev := pt.PriorTokenWithExclusions([ttWhiteSpace, ttReturn]);
  if (lcPrev <> nil) and (lcPrev.TokenType = ttComment) and (lcPrev.CommentStyle = eCompilerDirective) then
    exit;

  if pt.HasParentNode(nLiteralString) then
  begin
    Result := not StartsLiteralString(pt);
    exit;
  end;


  // '@@' in asm, e.g. "JE @@initTls" needs the space
  if pt.HasParentNode(nAsm) then
    exit;

  if pt.TokenType in NoSpaceAnywhere then
  begin
    Result := True;
    exit;
  end;

  // semicolon usually, except after 'begin' and another semicolon
  if pt.TokenType = ttSemiColon then
  begin
    lcPrev := pt.PriorTokenWithExclusions(NotSolidTokens);
    if not (lcPrev.TokenType in [ttSemiColon, ttBegin]) then
    begin
      Result := True;
      exit;
    end;
  end;

  { hat (dereference) in expression is unary postfix operator - so no space before it }
  if (pt.HasParentNode(nExpression)) and (pt.TokenType = ttHat) then
  begin
    Result := True;
    exit;
  end;

  { no space before open brackets for fn name - declaration or use }
  if IsActualParamOpenBracket(pt) or IsFormalParamOpenBracket(pt) then
  begin
    Result := True;
    exit;
  end;

  { no space before colon -- anywhere? }
  if pt.TokenType = ttColon then
  begin
    Result := True;
    exit;
  end;

  if (FormatSettings.Spaces.SpaceForOperator = eNever) then
  begin
    if IsSymbolOperator(pt) then
    begin
      Result := True;
      exit;
    end;

  end;

  { '[' of array property definition }
  if (pt.TokenType = ttOpenSquareBracket) and pt.HasParentNode(nProperty) then
  begin
    Result := True;
    exit;
  end;

end;

constructor TNoSpaceBefore.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

function TNoSpaceBefore.EnabledVisitSourceToken(const pcNode: TObject): boolean;
var
  lcSourceToken: TSourceToken;
  lcNext: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  // not safe to remove return at a comment like this
  if (lcSourceToken.TokenType = ttComment) and
    (lcSourceToken.CommentStyle = eDoubleSlash) then
    fbSafeToRemoveReturn := False
  else if (lcSourceToken.TokenType <> ttReturn) then
    fbSafeToRemoveReturn := True;

  // work on whitespace and returns
  if ( not (lcSourceToken.TokenType in [ttWhiteSpace, ttReturn])) or
    ( not fbSafeToRemoveReturn) then
    exit;

  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  if HasNoSpaceBefore(lcNext) then
  begin
    // the space
    BlankToken(lcSourceToken);
  end;
end;

function TNoSpaceBefore.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.FixSpacing;
end;

end.
