unit RemoveSpaceAtLineEnd;

{ AFS 10 May 2003
  remove trainling spaces on lines
  makes test fail, false delta }

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveSpaceAtLineEnd, released May 2003.
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
  TRemoveSpaceAtLineEnd = class(TSwitchableVisitor)
  private
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;



implementation

uses JcfSettings, FormatFlags, SourceToken, Tokens, TokenUtils;


constructor TRemoveSpaceAtLineEnd.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eRemoveSpace];
end;

function TRemoveSpaceAtLineEnd.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken, lcNext: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  { is this white space? }
  if lcSourceToken.TokenType = ttWhiteSpace then
  begin
    { is a return next ? }
    lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace]);
    if (lcNext <> nil) and (lcNext.TokenType = ttReturn) then
    begin
      BlankToken(lcSourceToken);
    end;
  end;

end;

function TRemoveSpaceAtLineEnd.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.Spaces.FixSpacing;
end;

end.
