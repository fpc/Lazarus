unit MaxSpaces;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is MaxSpaces, released January 2004.
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

{ AFS 4 Jan 2002
  convert spaces tabs }

uses SwitchableVisitor;

type
  TMaxSpaces = class(TSwitchableVisitor)
  private
    fsSpaces: string;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { local }
  JcfStringUtils,
  JcfSettings, SourceToken, Tokens,
  FormatFlags, ParseTreeNodeType;

constructor TMaxSpaces.Create;
begin
  inherited;
  fsSpaces    := StrRepeat(NativeSpace, FormatSettings.Spaces.MaxSpacesInCode);
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

function TMaxSpaces.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken, lcNext: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  { only look at white space }
  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { not in asm blocks }
  if lcSourceToken.HasParentNode(nAsm) then
    exit;

  { not before comments }
  lcNext := lcSourceToken.NextToken;
  if (lcNext <> nil) and (lcNext.TokenType = ttComment) then
    exit;

  { don't truncate the indentation spaces }
  if lcSourceToken.SolidTokenOnLineIndex > 0 then
  begin
    { if the token is too long, truncate it }
    if Length(lcSourceToken.SourceCode) > FormatSettings.Spaces.MaxSpacesInCode then
    begin
      lcSourceToken.SourceCode := fsSpaces;
    end;
  end;
end;

function TMaxSpaces.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.UseMaxSpacesInCode;
end;

end.
