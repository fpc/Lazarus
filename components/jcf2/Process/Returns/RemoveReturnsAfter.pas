unit RemoveReturnsAfter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is RemoveReturnsAfter, released Octoiber 2007.
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
  TRemoveReturnsAfter = class(TSwitchableVisitor)
  private
    function MaxReturnsAfter(const pcToken: TSourceToken): integer;
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;

  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  JcfSettings, Tokens, TokenUtils, ParseTreeNodeType;


constructor TRemoveReturnsAfter.Create;
begin
  inherited;

end;

function TRemoveReturnsAfter.MaxReturnsAfter(const pcToken: TSourceToken): integer;
begin
  Result := -1;

  // for now, we're only interested in asm blocks
  if pcToken.HasParentNode(nAsm) then
  begin
    // look for the colon of a label
    if pcToken.TokenType = ttColon then
    begin
      if pcToken.HasParentNode(nAsmLabel, 1) then
      begin
        Result := FormattingSettings.SetAsm.BreaksAfterLabel;
      end;

    end;

  end;
end;


function TRemoveReturnsAfter.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  liMaxReturnCount: integer;
  lcNext: TSourceToken;
  lcTest: TSourceToken;
  liReturnCount: integer;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  liMaxReturnCount := MaxReturnsAfter(lcSourceToken);
  // -1 is a marker value - no processing needed
  if liMaxReturnCount = -1 then
    exit;

  lcNext := lcSourceToken.NextTokenWithExclusions([ttWhiteSpace, ttReturn]);

  liReturnCount := 0;
  lcTest := lcSourceToken;

  { remove all surplus returns up to that point }
  while (lcTest <> lcNext) do
  begin
    if (lcTest.TokenType = ttReturn) then
    begin
      Inc(liReturnCount);
      if (liReturnCount > liMaxReturnCount) then
      begin
        BlankToken(lcTest);
      end;
    end;
    lcTest := lcTest.NextToken;
  end;
end;

function TRemoveReturnsAfter.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.SetAsm.BreaksAfterLabelEnabled;
end;

end.
