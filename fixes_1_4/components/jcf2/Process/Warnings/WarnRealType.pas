unit WarnRealType;

{ AFS 30 Dec 2002

  simple warner - these types are obsolete
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is WarnRealType, released May 2003.
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


uses Warning;

type

  TWarnRealType = class(TWarning)
  public
    function EnabledVisitSourceToken(const pcToken: TObject): Boolean; override;
  end;

implementation

uses SourceToken, ParseTreeNodeType, Tokens;

function TWarnRealType.EnabledVisitSourceToken(const pcToken: TObject): Boolean;
const
  REAL_WARNING = ' This type is obsolete and is seldom useful';
  // + 'See the help for details';
var
  lcToken: TSourceToken;
begin
  Result := False;
  lcToken := TSourceToken(pcToken);

  if not lcToken.HasParentNode(nType) then
    exit;

  { see delphi help on 'real' for details.
   I don't know any reason to prefer these types to 'Double'

   If the code was orignally Delphi V1, then it may be better of as "Currency"
   }

  if lcToken.TokenType = ttReal then
  begin
    SendWarning(lcToken, 'Real type used.' + REAL_WARNING);
  end
  else if lcToken.TokenType = ttReal48 then
  begin
    SendWarning(lcToken, 'Real48 type used.' + REAL_WARNING);
  end;

end;

end.
