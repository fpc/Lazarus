unit SetAsm;
{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is SetAsm.pas, released September 2007.
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
  JcfSetBase,
  SettingsTypes, SettingsStream;

type

  TSetAsm = class(TSetBase)
  private
    feCapitalisation: TCapitalisationType;
    fiBreaksAfterLabel: integer;
    fbBreaksAfterLabelEnabled: boolean;
    fbStatementIndentEnabled: boolean;
    fiStatementIndent: integer;
    fbParamsIndentEnabled: boolean;
    fiParamsIndent: integer;

  protected
  public
    constructor Create;

    procedure WriteToStream(const pcOut: TSettingsOutput); override;
    procedure ReadFromStream(const pcStream: TSettingsInput); override;

    property Capitalisation: TCapitalisationType Read feCapitalisation Write feCapitalisation;
    property BreaksAfterLabel: integer read fiBreaksAfterLabel write fiBreaksAfterLabel;

    property BreaksAfterLabelEnabled: boolean Read fbBreaksAfterLabelEnabled Write fbBreaksAfterLabelEnabled;
    property StatementIndentEnabled: boolean Read fbStatementIndentEnabled Write fbStatementIndentEnabled;
    property ParamsIndentEnabled: boolean Read fbParamsIndentEnabled Write fbParamsIndentEnabled;

    property StatementIndent: integer read fiStatementIndent write fiStatementIndent;
    property ParamsIndent: integer read fiParamsIndent write fiParamsIndent;
 end;

implementation

uses
  SysUtils;

const
  REG_CAPS = 'Caps';
  REG_BREAKS_AFTER_LABEL = 'BreaksAfterLabel';
  REG_INDENT_LEVEL = 'Indent_';
  REG_BREAKS_AFTER_LABEL_ENABLED = 'BreaksAfterLabelEnabled';

  REG_STATEMENT_INDENT_ENABLED = 'StatementIndentEnabled';
  REG_PARAMS_INDENT_ENABLED = 'ParamsIndentEnabled';

  REG_STATEMENT_INDENT = 'StatementIndent';
  REG_PARAMS_INDENT = 'ParamsIndent';

  // these are usually 9 and 17
  // but it's already indented 2 for being a statement in a proc
  ASM_STATEMENT_DEFAULT_INDENT = 7;
  ASM_PARAMS_DEFAULT_INDENT = 15;


constructor TSetAsm.Create;
begin
  inherited;
  SetSection('Asm');
end;


procedure TSetAsm.ReadFromStream(const pcStream: TSettingsInput);
begin
  Assert(pcStream <> nil);

  feCapitalisation := TCapitalisationType(pcStream.Read(REG_CAPS, Ord(ctLeaveAlone)));
  fiBreaksAfterLabel := pcStream.Read(REG_BREAKS_AFTER_LABEL, 2);

  fbBreaksAfterLabelEnabled := pcStream.Read(REG_BREAKS_AFTER_LABEL_ENABLED, True);
  fbStatementIndentEnabled := pcStream.Read(REG_STATEMENT_INDENT_ENABLED, True);
  fiStatementIndent := pcStream.Read(REG_STATEMENT_INDENT, ASM_STATEMENT_DEFAULT_INDENT);

  fbParamsIndentEnabled := pcStream.Read(REG_PARAMS_INDENT_ENABLED, True);
  fiParamsIndent := pcStream.Read(REG_PARAMS_INDENT, ASM_PARAMS_DEFAULT_INDENT);
end;

procedure TSetAsm.WriteToStream(const pcOut: TSettingsOutput);
begin
  Assert(pcOut <> nil);

  pcOut.Write(REG_CAPS, Ord(feCapitalisation));
  pcOut.Write(REG_BREAKS_AFTER_LABEL, fiBreaksAfterLabel);

  pcOut.Write(REG_BREAKS_AFTER_LABEL_ENABLED, fbBreaksAfterLabelEnabled);

  pcOut.Write(REG_STATEMENT_INDENT_ENABLED, fbStatementIndentEnabled);
  pcOut.Write(REG_STATEMENT_INDENT, fiStatementIndent);

  pcOut.Write(REG_PARAMS_INDENT_ENABLED, fbParamsIndentEnabled);
  pcOut.Write(REG_PARAMS_INDENT, fiParamsIndent);
end;

end.
