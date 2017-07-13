unit RemoveConsecutiveReturns;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveEmptyComment, released Nov 2003.
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

{ AFS 9 Nov 2003
  Remove consecutive returns
  ie put an upper limit on the number of blank lines in a row }

uses SwitchableVisitor;

type
  TRemoveConsecutiveReturns = class(TSwitchableVisitor)
  private
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  { local }
  FormatFlags, SourceToken, Tokens, TokenUtils, JcfSettings;


constructor TRemoveConsecutiveReturns.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveReturn];
end;

function TRemoveConsecutiveReturns.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  liCount: integer;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  liCount := 0;

  while (lcSourceToken <> nil) and (lcSourceToken.TokenType = ttReturn) do
  begin
    Inc(liCount);

    if (liCount - 1) > FormattingSettings.Returns.MaxConsecutiveBlankLines then
    begin
      BlankToken(lcSourceToken);
    end;

    lcSourceToken := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
  end;

end;

function TRemoveConsecutiveReturns.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Returns.RemoveConsecutiveBlankLines;
end;

end.
