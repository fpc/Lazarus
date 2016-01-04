unit RemoveUnneededWhiteSpace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is RemoveUnneededWhiteSpace, released May 2003.
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

{ AFS 29 December 2002
  Obfuscation visitor
  This is to completely remove white space
  when it is not needed, e.g. turn "a := a + 1 ;" to "a:=a+1;"
}

uses SwitchableVisitor;

type
  TRemoveUnneededWhiteSpace = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;

implementation

uses
  { local }
  SourceToken, Tokens, ParseTreeNodeType,
  JcfStringUtils, FormatFlags, TokenUtils;

function TextOrNumberString(const str: String): boolean;
var
  liLoop: integer;
  ch:     Char;
begin
  Result := True;

  for liLoop := 1 to Length(str) do
  begin
    ch := str[liLoop];
    if not (CharIsAlphaNum(ch) or (ch = '_') or (ch = '.')) then
    begin
      Result := False;
      break;
    end;
  end;
end;


const
  MiscUnspacedTokens: TTokenTypeSet = [
    ttQuotedLiteralString, ttSemiColon, ttColon, ttComma,
    ttDot, ttDoubleDot, ttAssign, ttReturn, ttPlusAssign, ttMinusAssign, ttTimesAssign, ttFloatDivAssign];

  DoNotConcat = [ttGreaterThan, ttLessThan, ttEquals];


function NeedSpaceBetween(const pt1, pt2: TSourceToken): boolean;
const
  SPACED_ASM_TOKENS = [ttAtSign, ttAmpersand];
var
  lcLast: TSourceToken;
begin
  Result := True;

  if ((pt1 = nil) or (pt2 = nil)) then
  begin
    Result := False;
    exit;
  end;

  if (pt1.TokenType in DoNotConcat)  and (pt2.TokenType in DoNotConcat) then
  begin
    exit;
  end;
  

  { can loose everything after the final end }
  if (pt1.TokenType = ttDot) then
  begin
    lcLast := pt1.PriorSolidToken;
    if (lcLast.TokenType = ttEnd) and (pt1.HasParentNode(TopOfFileSection, 1)) then
    begin
      Result := False;
      exit;
    end;
  end;

  { need to keep space before Asm @@ and '&' thingy}
  if (pt2.TokenType in SPACED_ASM_TOKENS) and pt2.HasParentNode(nAsmStatement) then
    exit;

  { never need a space next to a bracket }
  if (pt1.TokenType in BracketTokens) or (pt2.TokenType in BracketTokens) then
  begin
    Result := False;
    exit;
  end;

  { never need space around semicolon }
  if (pt1.TokenType = ttSemiColon) or (pt2.TokenType = ttSemiColon) then
  begin
    Result := False;
    exit;
  end;

  { or dot or comma etc }
  if (pt1.TokenType in MiscUnspacedTokens) or (pt2.TokenType in MiscUnspacedTokens) then
  begin
    Result := False;
    exit;
  end;

  { before a comment. Comments are removed by another process (except for compiler directives) }
  if (pt2.TokenType = ttComment) then
  begin
    Result := False;
    exit;
  end;

  { similartly, after a comment unless it starts with '//' }
  if (pt1.TokenType = ttComment) and (pt1.CommentStyle <> eDoubleSlash) then
  begin
    Result := False;
    exit;
  end;

  if (pt1.TokenType in TextOrNumberTokens) and (pt2.TokenType = ttAtSign) and
    (pt1.HasParentnode(nAsm)) then
  begin
    Result := False;
    exit;
  end;

  { if one token is text or number, and the other not, don't need white space
   for this numbers count as text, for e.g.
   "for liLoop := 0to3do" is not valid, neither is "for liLoop := 0 to3 do",
   must be for liLoop := 0 to 3 do
   }

  if TextOrNumberString(pt1.SourceCode) and TextOrNumberString(pt2.SourceCode) then
  begin
    { always space between two text/number tokens }
    Result := True;
    exit;
  end;

  { if one token is a number, even a number like "$000080FF",
   must space them }
  if TextOrNumberString(pt1.SourceCode) and (pt2.TokenType = ttNumber) then
  begin
    Result := True;
    exit;
  end;

  if (pt1.TokenType = ttNumber) and TextOrNumberString(pt2.SourceCode) then
  begin
    Result := True;
    exit;
  end;

  Result := False;
end;


constructor TRemoveUnneededWhiteSpace.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

function TRemoveUnneededWhiteSpace.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSpace, lcBefore, lcAfter: TSourceToken;
begin
  Result := False;

  { this visitor needs to operate on the white space token
    depending on what comes before and after it
  }
  lcSpace := TSourceToken(pcNode);
  if lcSpace.TokenType <> ttWhiteSpace then
    exit;

  lcBefore := lcSpace.PriorToken;
  if lcBefore = nil then
    exit;

  lcAfter := lcSpace.NextToken;
  if lcAfter = nil then
    exit;

  if not NeedSpaceBetween(lcBefore, lcAfter) then
    BlankToken(lcSpace);
end;

end.
