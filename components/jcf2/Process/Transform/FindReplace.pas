unit FindReplace;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is FindReplace.pas, released April 2000.
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

uses
  SysUtils,
  SwitchableVisitor;

type
  TFindReplace = class(TSwitchableVisitor)
  private
    fiCount: integer;

  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
    function FinalSummary(out psMessage: string): boolean; override;

  end;

implementation

uses
  { local }
  JcfSettings,
  SourceToken,
  FormatFlags;

{ TFindReplace }

constructor TFindReplace.Create;
begin
  inherited;
  fiCount := 0;

  FormatFlags := FormatFlags + [eFindReplace];
end;

function TFindReplace.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Replace.Enabled;
end;

function TFindReplace.FinalSummary(out psMessage: string): boolean;
begin
  Result := (fiCount > 0);
  if Result then
  begin
    psMessage := 'Replace: ' + IntToStr(fiCount) + ' changes were made';
  end
  else
  begin
    psMessage := '';
  end;
end;


function TFindReplace.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
begin
  Result := False;

  if pcNode = nil then
    exit;
  lcSourceToken := TSourceToken(pcNode);

  if lcSourceToken.SourceCode = '' then
    exit;

  if not FormattingSettings.Replace.HasWord(lcSourceToken.SourceCode) then
    exit;

  lcSourceToken.SourceCode := FormattingSettings.Replace.Replace(lcSourceToken.SourceCode);
  Inc(fiCount);
end;

end.
