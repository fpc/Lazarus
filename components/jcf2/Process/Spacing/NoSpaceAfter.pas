unit NoSpaceAfter;

{ AFS 9 Dec 1999
  no space after  certain tokens }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is NoSpaceAfter, released May 2003.
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

uses SwitchableVisitor, SourceToken;

type
  TNoSpaceAfter = class(TSwitchableVisitor)
  private
    fcLastSolidToken: TSourceToken;
    fbSafeToRemoveReturn: boolean;  // this taken from NoReturnBefore
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  JcfStringUtils,
  Tokens, ParseTreeNodeType, JcfSettings, FormatFlags,
  TokenUtils, SettingsTypes;

{ TNoSpaceAfter }


function NeedsNoSpace(const pt, ptNext: TSourceToken): boolean;
const
  NoSpaceAnywhere: TTokenTypeSet = [ttOpenBracket, ttOpenSquareBracket, ttDot];
begin
  Result := False;

  if pt = nil then
    exit;

  { if the next thing is a comment, leave well enough alone }
  if ptNext.TokenType = ttComment then
    exit;

  if (pt.TokenType in [ttLessThan,ttGreaterThan]) and pt.HasParentNode(nGeneric,1) then
    exit(True);

  if pt.TokenType in NoSpaceAnywhere then
    exit(True);

  if (FormattingSettings.Spaces.SpaceForOperator = eNever) then
    if IsSymbolOperator(pt) then
      exit(True);

  { no space between method name and open bracket for param list
    no space between type & bracket for cast
    no space between fn name & params for procedure call }
  if pt.HasParentNode([nProcedureDecl, nFunctionDecl, nConstructorDecl,
    nDestructorDecl, nStatementList]) and
    (IsIdentifier(pt, idAllowDirectives) or (pt.TokenType in BuiltInTypes))
  then
    if (ptNext.TokenType in OpenBrackets) and (not IsInsideAsm(ptNext)) then
      exit(True);

  { the above takes care of procedure headers but not procedure type defs
   eg type TFred = procedure(i: integer) of object;
    note no space before the open bracket }
  if pt.HasParentNode(nTypeDecl) and (pt.IsOnRightOf(nTypeDecl, ttEquals)) and
    (pt.TokenType in ProcedureWords)
  then
    if (ptNext.TokenType in OpenBrackets) then
      exit(True);

  { no space after unary operator in expression }
  if pt.HasParentNode(nExpression) and IsUnaryOperator(pt) and (not StrHasAlpha(pt.SourceCode)) then
    exit(True);

  { no space before class heritage ? could be one of 3 things
    TFoo = class; - no space, but "No space before semicolon" should take care of that
    TBar = class(TBaz) - no space unless you are Marcel van Brakel
    TWibble = class of TFish - has space

    see SingleSpaceAfter

    also applies to type TFoo = interface(IDispatch) }
  if (pt.HasParentNode(nRestrictedType)) and (pt.TokenType in ObjectTypeWords) and
    ( not (FormattingSettings.Spaces.SpaceBeforeClassHeritage))
  then
    if (ptNext.TokenType in [ttOpenBracket, ttSemiColon]) then
      exit(True);
end;

constructor TNoSpaceAfter.Create;
begin
  inherited;
  fbSafeToRemoveReturn := True;
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

function TNoSpaceAfter.EnabledVisitSourceToken(const pcNode: TObject): boolean;
var
  lcSourceToken: TSourceToken;
  lcNextSolid:   TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType = ttWhiteSpace then
  begin
    lcNextSolid := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);
    if lcNextSolid <> nil then
    begin
      if NeedsNoSpace(fcLastSolidToken, lcNextSolid) then
        BlankToken(lcSourceToken);
    end;
  end
  else
  begin
    { store for next time }
    if not (lcSourceToken.TokenType in [ttWhiteSpace, ttReturn]) then
      fcLastSolidToken := lcSourceToken;
  end;
end;

function TNoSpaceAfter.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Spaces.FixSpacing;
end;

end.
