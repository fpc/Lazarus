unit CommandLineReturnCode;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is CommandLineReturnCode, released August 2008.
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

{$I JcfGlobal.inc}

interface

{
  command line return code
  0 for sucess
  non-sero for failure codes
}
type
  TJcfCommandLineReturnCode =
  (
    rcSuccess = 0,
    rcGeneralFailure = 1,
    rcNoPathFound = 2,
    rcConfigFileNotFound = 3,
    rcSettingsNotRead = 4,
    rcFileNotFound = 5,
    rcDirectoryNotFound = 6,
    rcConvertError = 7
  );

procedure HaltOnError(const returnCode: TJcfCommandLineReturnCode);

implementation

procedure HaltOnError(const returnCode: TJcfCommandLineReturnCode);
var
  liCode: integer;
begin
  liCode := Ord(returnCode);

  if liCode > 0 then
  begin
    Halt(liCode);
  end;

end;

end.
