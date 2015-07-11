unit FixCase;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is FixCase, released May 2003.
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
  Obfusccate - capitalisation
}

uses SwitchableVisitor;

type
  TFixCase = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;


implementation

uses
  { delphi }
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils,
  { local }
  JcfStringUtils,
  Tokens, SourceToken, SettingsTypes,
  ParseTreeNodeType, JcfSettings, FormatFlags, TokenUtils;


{ identify the cases where the compiler is case sensitive
  don't want ot mess with caps when it affects these issues }
function PutUpWithCompilerBugs(const pt: TSourceToken): boolean;
begin
  Result := False;

  { special case - 'Register' (with a capital R) as a procedure name must be preserved
    or component registration may not work in some versions of Delphi
    This is a known issue in some versions of Delphi
    note intentional use of case-sensitive compare }
  if (IsIdentifier(pt, idStrict)) and AnsiSameStr(pt.SourceCode, 'Register') and
    pt.HasParentNode([nProcedureType, nProcedureDecl, nProcedureHeading]) then
  begin
    Result := True;
    exit;
  end;

  { had problems - IDE could not find the base class frame
    when the frame's ancestor's name was decapitised
    most likely some lazy developer @ borland forgot to match strings without case}
  if (pt.WordType in IdentifierTypes) and (pt.HasParentNode(nClassHeritage)) then
  begin
    Result := True;
    exit;
  end;
end;


procedure FixCaps(const pt: TSourceToken);
begin
  if PutUpWithCompilerBugs(pt) then
    exit;

  case FormatSettings.Obfuscate.Caps of
    ctUpper:
      pt.SourceCode := AnsiUpperCase(pt.SourceCode);
    ctLower:
      pt.SourceCode := AnsiLowerCase(pt.SourceCode);
    ctMixed:
      pt.SourceCode := StrSmartCase(pt.SourceCode, []);
    ctLeaveAlone: ;
  end;
end;


constructor TFixCase.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

function TFixCase.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if (lcSourceToken.TokenType in TextualTokens) and (lcSourceToken.SourceCode <> '') then
    FixCaps(lcSourceToken);
end;

end.
