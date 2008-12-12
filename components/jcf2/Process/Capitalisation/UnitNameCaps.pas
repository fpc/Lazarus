unit UnitNameCaps;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is UnitNameCaps, released June 2003.
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

{ AFS 16 June 2003
  - fix capitalisation on unit names
}
{$I JcfGlobal.inc}

interface

uses SwitchableVisitor;

type
  TUnitNameCaps = class(TSwitchableVisitor)
  private
    fiCount: integer;
    lsLastChange: string;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    { return true if you want the message logged}
    function FinalSummary(out psMessage: string): boolean; override;
  end;

implementation

uses
  { delphi }
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils,
  { local }
  SourceToken, Tokens, ParseTreeNodeType, JcfSettings, FormatFlags,
  TokenUtils;

function IsUnitName(const pt: TSourceToken): boolean;
var
  lcNext, lcPrev: TSourceToken;
begin
  Result := False;

  if not IsIdentifier(pt, idStrict) then
    exit;

  { unit names can be found in these places:
    in unit names
    uses clause
    and in expressions as a prefix for vars, constants and functions }
  if pt.HasParentNode(nUnitName) then
    Result := True
  else if pt.HasParentNode(nUsesItem) then
    Result := True
  else if pt.HasParentNode(nDesignator) then
  begin
    // must be a dot to resolve unit name
    lcNext := pt.NextSolidToken;
    Result := (lcNext <> nil) and (lcNext.TokenType = ttDot);

    if Result then
    begin
      // unit name is always first part of designator. May not be preceeded by a dot 
      lcPrev := pt.PriorSolidToken;
      Result := (lcPrev <> nil) and (lcPrev.TokenType <> ttDot);
    end;
  end;
end;


{ TUnitNameCaps }

constructor TUnitNameCaps.Create;
begin
  inherited;
  fiCount      := 0;
  lsLastChange := '';
  FormatFlags  := FormatFlags + [eCapsSpecificWord];
end;

function TUnitNameCaps.FinalSummary(out psMessage: string): boolean;
begin
  Result := (fiCount > 0);
  psMessage := '';

  if Result then
  begin
    psMessage := 'Unit name caps: ';

    if fiCount = 1 then
      psMessage := psMessage + 'One change was made: ' + lsLastChange
    else
      psMessage := psMessage + IntToStr(fiCount) + ' changes were made';
  end;
end;

function TUnitNameCaps.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lsChange:      string;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if not IsUnitName(lcSourceToken) then
    exit;

  if FormatSettings.UnitNameCaps.HasWord(lcSourceToken.SourceCode) then
  begin
    // get the fixed version
    lsChange := FormatSettings.UnitNameCaps.CapitaliseWord(lcSourceToken.SourceCode);

    // case-sensitive test - see if anything to do.
    if AnsiCompareStr(lcSourceToken.SourceCode, lsChange) <> 0 then
    begin
      lsLastChange := lcSourceToken.SourceCode + ' to ' + lsChange;
      lcSourceToken.SourceCode := lsChange;
      Inc(fiCount);
    end;
  end;
end;

function TUnitNameCaps.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.UnitNameCaps.Enabled;
end;

end.
