unit SpecificWordCaps;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is SpecificWordCaps, released May 2003.
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

uses SwitchableVisitor;

type
  TSpecificWordCaps = class(TSwitchableVisitor)
  private
    fiCount: integer;
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
  { delphi }
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils,
  { local }
  SourceToken, Tokens, ParseTreeNodeType, JcfSettings, FormatFlags;


function Excluded(const pt: TSourceToken): boolean;
begin
  Result := False;

  { directives in context are excluded }
  if pt.HasParentNode(DirectiveNodes) then
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


{ TSpecificWordCaps }

constructor TSpecificWordCaps.Create;
begin
  inherited;
  fiCount      := 0;
  lsLastChange := '';
  FormatFlags  := FormatFlags + [eCapsSpecificWord];
end;

function TSpecificWordCaps.FinalSummary(out psMessage: string): boolean;
begin
  Result := (fiCount > 0);
  psMessage := '';

  if Result then
  begin
    psMessage := 'Specific word caps: ';

    if fiCount = 1 then
      psMessage := psMessage + 'One change was made: ' + lsLastChange
    else
      psMessage := psMessage + IntToStr(fiCount) + ' changes were made';
  end;
end;

function TSpecificWordCaps.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lsChange:      string;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if Excluded(lcSourceToken) then
    exit;

   { not in Asm statements}
  if lcSourceToken.HasParentNode(nAsm) then
    exit;

  if FormatSettings.SpecificWordCaps.HasWord(lcSourceToken.SourceCode) then
  begin
    // get the fixed version
    lsChange := FormatSettings.SpecificWordCaps.CapitaliseWord(lcSourceToken.SourceCode);

    // case-sensitive test - see if anything to do.
    if AnsiCompareStr(lcSourceToken.SourceCode, lsChange) <> 0 then
    begin
      lsLastChange := lcSourceToken.SourceCode + ' to ' + lsChange;
      lcSourceToken.SourceCode := lsChange;
      Inc(fiCount);
    end;
  end;
end;

function TSpecificWordCaps.IsIncludedInSettings: boolean;
begin
  Result := FormatSettings.SpecificWordCaps.Enabled;
end;

end.
