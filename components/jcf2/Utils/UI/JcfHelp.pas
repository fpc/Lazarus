unit JcfHelp;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JCFHelp, released May 2003.
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
  Forms;

{ code interface to JCF help file }

function GetHelpFilePath: string;

const

  { help context ids }
  HELP_MAIN = 10;
  HELP_PROGRAMS = 12;
  HELP_SETTINGS_STORED = 15;
  HELP_SETTINGS_LOGGING = 24;
  HELP_SETTINGS_EXCLUSIONS = 26;

  HELP_BASIC_SETTINGS = 30;
  HELP_FORMAT_FILE = 40;

  HELP_OBFUSCATE_SETTINGS = 50;
  HELP_CLARIFY = 60;
  HELP_CLARIFY_SPACES = 70;
  HELP_CLARIFY_INDENTATION = 80;
  HELP_CLARIFY_LONG_LINES = 85;
  HELP_CLARIFY_RETURNS = 90;
  HELP_CLARIFY_BLANK_LINES = 95;
  HELP_CLARIFY_BLOCKS = 100;
  HELP_CLARIFY_ALIGN = 110;
  HELP_CLARIFY_COMMENTS = 80;

  HELP_CLARIFY_CAPITALISATION = 120;
  HELP_CLARIFY_FIND_AND_REPLACE = 130;
  HELP_CLARIFY_FIND_AND_REPLACE_USES = 140;
  HELP_COMMAND_LINE_PARAMS = 200;

  HELP_CLARIFY_TRANSFORM = 211;
  HELP_CLARIFY_WARNINGS = 212;

  HELP_ISSUES = 220;
  HELP_INFO = 1000;

  HELP_FILE_NAME = 'CodeFormat.chm';

implementation

function GetHelpFilePath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
    HELP_FILE_NAME;
end;

end.
