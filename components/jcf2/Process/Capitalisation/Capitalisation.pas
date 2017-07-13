unit Capitalisation;

{ AFS 30 December 2002
  visitor to do capitalisation according to settings
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Capitalisation, released May 2003.
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
  TCapitalisation = class(TSwitchableVisitor)
  private
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;

implementation

uses
  { system }
  SysUtils,
  { local }
  JcfStringUtils,
  SourceToken, SettingsTypes, Tokens,
  JcfSettings, FormatFlags, TokenUtils;

procedure FixCaps(const pct: TSourceToken; const caps: TCapitalisationType);
begin
  if pct = nil then
    exit;
  if pct.SourceCode = '' then
    exit;

  { if it's covered by specific word caps, then don't touch it
    This was happening with 'true' not coming out as 'True'
    even though specific word caps specifically said it was doing that change
    Capitalisation was changing it back!
  }
  if (FormattingSettings.SpecificWordCaps.Enabled)
  and FormattingSettings.SpecificWordCaps.HasWord(pct.SourceCode) then
    exit;

  case caps of
    ctUpper:
      pct.SourceCode := AnsiUpperCase(pct.SourceCode);
    ctLower:
      pct.SourceCode := AnsiLowerCase(pct.SourceCode);
    ctMixed:
      pct.SourceCode := StrSmartCase(AnsiLowerCase(pct.SourceCode), []);
    ctLeaveAlone: ;
  end;
end;

function TCapitalisation.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Caps.Enabled;
end;

constructor TCapitalisation.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eCapsReservedWord];
end;

function TCapitalisation.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);


  if IsInsideAsm(lcSourceToken) then
  begin
    // underneath an "asm" node - use asm caps on opcode and params
    if HasAsmCaps(lcSourceToken) then
    begin
      FixCaps(lcSourceToken, FormattingSettings.SetAsm.Capitalisation);
    end;
  end
  else
  begin

    case lcSourceToken.WordType of
      wtReservedWord:
        FixCaps(lcSourceToken, FormattingSettings.Caps.ReservedWords);

      wtReservedWordDirective:
      begin
        if IsDirectiveInContext(lcSourceToken) then
        begin
          FixCaps(lcSourceToken, FormattingSettings.Caps.Directives);
        end
      end;

      wtBuiltInConstant:
        FixCaps(lcSourceToken, FormattingSettings.Caps.Constants);
      wtOperator:
        FixCaps(lcSourceToken, FormattingSettings.Caps.Operators);
      wtBuiltInType:
        FixCaps(lcSourceToken, FormattingSettings.Caps.Types);
    end;
  end;

end;

end.
