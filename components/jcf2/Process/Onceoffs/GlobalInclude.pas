{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is GlobalInclude.pas, released May 2008.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 2008 Anthony Steele.
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

unit GlobalInclude;

{ AFS 24 march 2K
 add in the global include
}

{$I JcfGlobal.inc}

interface

uses
  SysUtils,
  BaseVisitor, SourceToken;

type
  TGlobalInclude = class(TBaseTreeNodeVisitor)
  private
    fbWorkIsDone: boolean;
  protected

  public
    constructor Create; override;

    function VisitSourceToken(const pcToken: TObject): Boolean; override;
    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  JclAnsiStrings,
  { local }Tokens, TokenUtils, JcfSettings,
  SettingsTypes, ParseTreeNodeType, SetClarify;


const
  { this directive will be inserted in all files above the unit header }
  includeText = '{$I JcfGlobal.inc}' + AnsiLineBreak + AnsiLineBreak;

function FirstOpportunityForInsert(const pt: TSourceToken): boolean;
begin
  Result := False;

  if (pt.TokenType = ttInterface) and pt.HasParentNode(nUnit, 2) then
  begin
    // before interface in unit 
    Result := True;
  end;
end;


constructor TGlobalInclude.Create;
begin
  inherited;
  fbWorkIsDone := False;
end;

function TGlobalInclude.IsIncludedInSettings: boolean;
begin
  Result := not FormattingSettings.Obfuscate.Enabled and
               (FormattingSettings.Clarify.OnceOffs <> eDoNotRun)
end;


function TGlobalInclude.VisitSourceToken(const pcToken: TObject): Boolean;
var
  lcToken, lcNewComment: TSourceToken;
  lbInContext: boolean;
begin
  Result := False;
  if fbWorkIsDone then
    exit;

  lcToken := TSourceToken(pcToken);

  if (lcToken.TokenType = ttComment) and (Pos(includeText, lcToken.SourceCode) > 0) then
  begin
    fbWorkIsDone := True;
    exit;
  end;

  lbInContext := FirstOpportunityForInsert(lcToken);
  if not lbInContext then
    exit;

  // put the include in front of the unit start word
  lcNewComment := TSourceToken.Create;
  lcNewComment.TokenType := ttComment;
  lcNewComment.SourceCode := includeText;

  lcToken.Parent.InsertChild(lcToken.IndexOfSelf, lcNewComment);

  fbWorkIsDone := True;
end;

end.
