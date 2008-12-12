unit TabToSpace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is TabToSpace, released May 2003.
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
  convert tabs to spaces }

uses SwitchableVisitor;

type
  TTabToSpace = class(TSwitchableVisitor)
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
  { Delphi }
  SysUtils,
  { local }
  JcfStringUtils,
  JcfSettings, SourceToken, Tokens, FormatFlags;

constructor TTabToSpace.Create;
begin
  inherited;
  fsSpaces    := StrRepeat(NativeSpace, FormatSettings.Spaces.SpacesPerTab);
  FormatFlags := FormatFlags + [eAddSpace, eRemoveSpace];
end;

function TTabToSpace.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
const
  // this is needed in Delphi 2007
  TabChar: WideChar = WideChar(NativeTab);
var
  lcSourceToken, lcNextToken: TSourceToken;
  ls: WideString;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if not (lcSourceToken.TokenType in [ttWhiteSpace, ttComment]) then
    exit;

  { can't pass property as var parameter so ls local var is used
    Must keep it wide to preserve unicode chars in comments }
  ls := lcSourceToken.SourceCode;

  { merge any following whitespace tokens with a whitespace }
  if (lcSourceToken.TokenType = ttWhiteSpace) then
  begin
    lcNextToken := lcSourceToken.NextToken;
    while (lcNextToken <> nil) and (lcNextToken.TokenType = ttWhiteSpace) do
    begin
      ls := ls + lcNextToken.SourceCode;
      lcNextToken.SourceCode := '';
      lcNextToken := lcNextToken.NextToken;
    end;
  end;

  ls := WideStringReplace(ls, TabChar, fsSpaces, [rfReplaceAll]);
  lcSourceToken.SourceCode := ls;
end;

function TTabToSpace.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.TabsToSpaces;
end;

end.
