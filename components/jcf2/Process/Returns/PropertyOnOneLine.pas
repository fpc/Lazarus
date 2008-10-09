unit PropertyOnOneLine;

{ AFS 8 March 2003
  put a property all on one line
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is PropertyOnOneLine, released May 2003.
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
  TPropertyOnOneLine = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses JcfSettings, FormatFlags, Tokens, ParseTreeNodeType;

constructor TPropertyOnOneLine.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveReturn, eRemoveSpace];
end;

function TPropertyOnOneLine.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lcNext, lcNext2, lcNextSolid: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  // never remove a return at the end of a comment like this one! ->
  if (lcSourceToken.TokenType = ttComment) and
    (lcSourceToken.CommentStyle = eDoubleSlash) then
    exit;

  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  { inpsect returns in property defs }
  if not (lcNext.TokenType = ttReturn) then
    exit;

  if not lcNext.HasParentNode(nProperty) then
    exit;

  { allow a return before the property starts :)  }
  lcNextSolid := lcNext.NextSolidToken;
  if lcNextSolid = nil then
    exit;
  if lcNextSolid.TokenType = ttProperty then
    exit;


  while (lcNext <> nil) and (lcNext.TokenType in [ttReturn, ttWhiteSpace]) and
    lcNext.HasParentNode(nProperty) do
  begin
    // don't kill returns before a comment
    lcNext2 := lcNext.NextTokenWithExclusions([ttWhiteSpace]);
    if (lcNext2 = nil) or (lcNext2.TokenType = ttComment) then
      break;

    if (lcNext.TokenType = ttReturn) then
    begin
      lcNext.TokenType  := ttWhiteSpace;
      lcNext.SourceCode := ' ';
    end
    else if lcNext.TokenType = ttWhiteSpace then
    begin
      lcNext.SourceCode := ' '; //reduce to a single space
    end;

    lcNext := lcNext.NextToken;
  end;

end;

function TPropertyOnOneLine.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Returns.RemovePropertyReturns;
end;

end.
