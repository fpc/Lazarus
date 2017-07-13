unit SpaceToTab;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SpaceToTab, released May 2003.
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
  TSpaceToTab = class(TSwitchableVisitor)
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
  SysUtils,
  { local }
  JcfStringUtils,
  JcfSettings, SourceToken, Tokens, FormatFlags;

constructor TSpaceToTab.Create;
begin
  inherited;
  fsSpaces    := string(StrRepeat(NativeSpace, FormattingSettings.Spaces.SpacesForTab));
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

function TSpaceToTab.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken, lcNextToken: TSourceToken;
  ls, lsTab: string;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  { work only on whitespace tokens.
    Indent spaces also occur in multiline comments, but leave them alone }
  if (lcSourceToken.TokenType <> ttWhiteSpace) then
    exit;

  { Merge following space tokens
    can't pass property as var parameter so ls local var is used }

	ls := lcSourceToken.SourceCode;
	lcNextToken := lcSourceToken.NextToken;
	while (lcNextToken <> nil) and (lcNextToken.TokenType = ttWhiteSpace) do
	begin
		ls := ls + lcNextToken.SourceCode;
		lcNextToken.SourceCode := '';
		lcNextToken := lcNextToken.NextToken;
	end;

  lsTab := NativeTab;
  StrReplace(ls, fsSpaces, lsTab, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

function TSpaceToTab.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Spaces.SpacesToTabs;
end;

end.
