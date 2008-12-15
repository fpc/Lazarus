unit RemoveReturn;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveReturn, released May 2003.
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
  TRemoveReturn = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;



implementation

uses SourceToken, Tokens, ParseTreeNodeType, FormatFlags;

constructor TRemoveReturn.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

function TRemoveReturn.EnabledVisitSourceToken(const pcNode: TObject): boolean;
var
  lcSourceToken, lcPrev: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  // only act on returns
  if lcSourceToken.TokenType <> ttReturn then
    exit;

  { not in asm }
  if lcSourceToken.HasParentNode(nAsm) then
    exit;

  // never remove the return after a comment like this
  lcPrev := lcSourceToken.PriorTokenWithExclusions([ttWhiteSpace]);

  if (lcPrev <> nil) and (lcPrev.TokenType = ttComment) and
    (lcPrev.CommentStyle = eDoubleSlash) then
    exit;

  // transmute to white space  - may be needed as seperator
  lcSourceToken.SourceCode := ' ';
  lcSourceToken.TokenType  := ttWhiteSpace;
end;

end.
