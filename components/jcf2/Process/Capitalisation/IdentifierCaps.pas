unit IdentifierCaps;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is IdentifierCaps, released June 2005.
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

{ AFS 30 December 2002
    - fix capitalisation on specified words
}
{$I JcfGlobal.inc}

interface

uses
  SysUtils,
  SwitchableVisitor;

type
  TIdentifierCaps = class(TSwitchableVisitor)
  private
    fiIdentifierCount: integer;
    fiNonIdentifierCount: integer;
    lsLastChange: string;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    { return true if you want the message logged}
    function FinalSummary(out psMessage: string): boolean; override;
  end;

implementation

uses
  { local }
  SourceToken, Tokens, ParseTreeNodeType,
  JcfSettings, FormatFlags, TokenUtils;


function Excluded(const pt: TSourceToken): boolean;
begin
  Result := False;

  { directives in context are excluded }
  if IsDirectiveInContext(pt) then
  begin
    Result := True;
    exit;
  end;

  // asm has other rules
  if pt.HasParentNode(nAsm) then
  begin
    Result := True;
    exit;
  end;


  { built in types that are actually being used as types are excluded
    eg.
    // this use of 'integer' is definitly the type
    var li: integer;

    // this use is definitely not
    function Integer(const ps: string): integer;

    // this use is ambigous
    li := Integer(SomeVar);

   user defined types are things that we often *want* to set a specific caps on
   so they are not excluded }

  if (pt.TokenType in BuiltInTypes) and (pt.HasParentNode(nType)) then
  begin
    Result := True;
    exit;
  end;
end;


{ TIdentifierCaps }

constructor TIdentifierCaps.Create;
begin
  inherited;
  fiIdentifierCount  := 0;
  fiNonIdentifierCount := 0;

  lsLastChange := '';
  FormatFlags  := FormatFlags + [eCapsSpecificWord];
end;

function TIdentifierCaps.FinalSummary(out psMessage: string): boolean;
begin
  Result := False;
  psMessage := '';


  if (fiIdentifierCount > 0) then
  begin
    Result := True;
    psMessage := 'Identifier caps: ';

    if fiIdentifierCount = 1 then
      psMessage := psMessage + 'One change was made: ' + lsLastChange
    else
      psMessage := psMessage + IntToStr(fiIdentifierCount) + ' changes were made';
  end;

  if (fiNonIdentifierCount > 0) then
  begin
    Result := True;
    if psMessage <> '' then
      psMessage := psMessage + '.   ';

    psMessage := psMessage + 'Non-identifier caps: ';

    if fiNonIdentifierCount = 1 then
      psMessage := psMessage + 'One change was made: ' + lsLastChange
    else
      psMessage := psMessage + IntToStr(fiNonIdentifierCount) + ' changes were made';
  end;

end;

function TIdentifierCaps.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
const
  NEITHER_NOR = [ttReturn, ttComment, ttWhiteSpace,
    ttConditionalCompilationRemoved, ttNumber, ttQuotedLiteralString];
var
  lcSourceToken: TSourceToken;
  lsChange:      string;
begin
  Result := False;

  lcSourceToken := TSourceToken(pcNode);

  { these tokens are common and/or long,
   but are not capitalisable text at all, don't waste time on them }
  if lcSourceToken.TokenType in NEITHER_NOR then
    exit;

  { not in Asm statements}
  if lcSourceToken.HasParentNode(nAsm) then
    exit;

  if lcSourceToken.HasParentNode(nIdentifier, 2) then
  begin
    // it's an identifier

    if FormattingSettings.IdentifierCaps.Enabled
    and FormattingSettings.IdentifierCaps.HasWord(lcSourceToken.SourceCode) then
    begin
      // get the fixed version
      lsChange := FormattingSettings.IdentifierCaps.CapitaliseWord(lcSourceToken.SourceCode);

      // case-sensitive test - see if anything to do.
      if AnsiCompareStr(lcSourceToken.SourceCode, lsChange) <> 0 then
      begin
        lsLastChange := lcSourceToken.SourceCode + ' to ' + lsChange;
        lcSourceToken.SourceCode := lsChange;
        Inc(fiIdentifierCount);
      end;
    end;
  end
  else
  begin
    // it's not an identifier 

    if FormattingSettings.NotIdentifierCaps.Enabled
    and FormattingSettings.NotIdentifierCaps.HasWord(lcSourceToken.SourceCode) then
    begin
      // get the fixed version
      lsChange := FormattingSettings.NotIdentifierCaps.CapitaliseWord(lcSourceToken.SourceCode);

      // case-sensitive test - see if anything to do.
      if AnsiCompareStr(lcSourceToken.SourceCode, lsChange) <> 0 then
      begin
        lsLastChange := lcSourceToken.SourceCode + ' to ' + lsChange;
        lcSourceToken.SourceCode := lsChange;
        Inc(fiNonIdentifierCount);
      end;
    end;
  end;
end;

function TIdentifierCaps.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.IdentifierCaps.Enabled or
            FormattingSettings.NotIdentifierCaps.Enabled;
end;

end.
