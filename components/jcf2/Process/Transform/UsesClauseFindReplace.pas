unit UsesClauseFindReplace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is UsesClauseFindReplace.pas, released October 2003.
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

{ AFS 4 October 2003

  - massage the uses clause. Replace units

 }

uses
  { local }
  SourceToken,
  SwitchableVisitor;

type
  TUsesClauseFindReplace = class(TSwitchableVisitor)
  private
    fiCount: integer;
    fbHasFind: boolean;

    function MatchesSearch(const ps: string): boolean;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    function FinalSummary(out psMessage: string): boolean; override;

  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfSettings,
  Tokens,
  FormatFlags,
  ParseTreeNodeType,
  TokenUtils;

constructor TUsesClauseFindReplace.Create;
begin
  inherited;

  FormatFlags := FormatFlags + [eFindReplaceUses];

  fiCount := 0;
end;

function TUsesClauseFindReplace.IsIncludedInSettings: boolean;
begin
  Result := (FormatSettings.UsesClause.InsertInterfaceEnabled or
    FormatSettings.UsesClause.InsertImplementationEnabled);
end;

function TUsesClauseFindReplace.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken, lcSepAfter, lcSepBefore: TSourceToken;
  lbInterface, lbImplementation: boolean;
begin
  Result := False;
  if pcNode = nil then
    exit;

  lcSourceToken := TSourceToken(pcNode);

  { only do this in a uses clause }
  if not lcSourceToken.HasParentNode(nUses) then
    exit;

  lbInterface := lcSourceToken.HasParentNode(nInterfaceSection);
  if lbInterface then
    lbImplementation := False
  else
    lbImplementation := lcSourceToken.HasParentNode(nImplementationSection);

  if not (lbImplementation or lbInterface) then
    exit;

  { only proceed on one of the specified words }
  if not (lcSourceToken.TokenType = ttIdentifier) then
    exit;

  if not MatchesSearch(lcSourceToken.SourceCode) then
    exit;

  if not fbHasFind then
  begin
    { first instance, convert the name }
    fbHasFind := True;
    lcSourceToken.SourceCode := FormatSettings.UsesClause.GetReplace;
    Inc(fiCount);
  end
  else
  begin
    { throw away the word and the trailing comma, as in uses clause remove  }
    BlankToken(lcSourceToken);

    lcSepAfter := lcSourceToken.NextSolidToken;

    { now if this was the last item we have a surplus comma }
    if (lcSepAfter.TokenType = ttComma) then
    begin
      BlankToken(lcSepAfter);
    end
    else if (lcSepAfter.TokenType = ttSemiColon) then
    begin
      { remove the comma before instead }
      lcSepBefore := lcSourceToken.PriorSolidToken;
      if lcSepBefore.TokenType = ttComma then
      begin
        BlankToken(lcSepBefore);
      end
      else if lcSepBefore.TokenType = ttUses then
      begin
        { "uses" before, ";" after. There must have been only 1 unit in the uses clause
          remove it entirely }
        BlankToken(lcSepAfter);
        BlankToken(lcSepBefore);
      end;

    end;
  end;
end;

function TUsesClauseFindReplace.FinalSummary(out psMessage: string): boolean;
begin
  Result := (fiCount > 0);
  if Result then
  begin
    psMessage := 'Uses clause find/replace: ' + IntToStr(fiCount) + ' changes were made';
  end
  else
  begin
    psMessage := '';
  end;
end;

function TUsesClauseFindReplace.MatchesSearch(const ps: string): boolean;
begin
  Result := FormatSettings.UsesClause.Find.IndexOf(ps) >= 0;
end;

end.
