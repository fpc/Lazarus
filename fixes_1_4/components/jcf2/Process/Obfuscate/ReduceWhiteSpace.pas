unit ReduceWhiteSpace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ReduceWhiteSpace, released May 2003.
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

{ AFS 28 Dec 2002

  Visitor to reduce all whitespace to single spaces
  Obfuscation
}

uses SwitchableVisitor;

type
  TReduceWhiteSpace = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;


implementation

uses SourceToken, Tokens, FormatFlags;

constructor TReduceWhiteSpace.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eObfuscate];
end;

function TReduceWhiteSpace.EnabledVisitSourceToken(const pcNode: TObject): boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.TokenType = ttWhiteSpace then
    lcSourceToken.SourceCode := ' ';
end;

end.
