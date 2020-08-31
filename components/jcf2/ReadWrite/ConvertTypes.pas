{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ConvertTypes.pas, released April 2000.
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

unit ConvertTypes;

{$I JcfGlobal.inc}

interface

{ settings on how to convert
  this unit is simple type defs with no dependencies 
}
type
  TBackupMode = (cmInPlace, cmInPlaceWithBackup, cmSeparateOutput);
  TSourceMode = (fmSingleFile, fmDirectory, fmDirectoryRecursive);

  TStatusMessageType =
    (
    mtException, // an exception was thrown - internal error
    mtInputError, // program input params are not understood, file is readonly, etc
    mtParseError, // could not parse the file
    mtCodeWarning, // wanring issued on the code
    mtFinalSummary, // summary of work down
    mtProgress // summery of work in progress
    );

type
  { type for a proc to receive a message
  from the depths of the fornatter to the ui
  many of them have a line x,y specified }
  TStatusMessageProc = procedure(const psUnit, psMessage: string;
    const peMessageType: TStatusMessageType;
    const piY, piX: integer) of object;

type
  TShowParseTreeOption = (eShowAlways, eShowOnError, eShowNever);

const
  OLD_REG_ROOT_KEY = '\Software\Jedi\JediCodeFormat';
  REG_ROOT_KEY = OLD_REG_ROOT_KEY;

const
  SOURCE_FILE_FILTERS =
    'All source|*.pas; *.dpr; *.dpk; *.pp; *.lpr; *.lpk; *.txt|' +
    'Delphi source (*.pas, *.dpr, *.dpk)|*.pas; *.dpr; *.dpk|' +
    'Lazarus source (*.pas, *.pp, *.lpr, *.lpk)|*.pas; *.pp; *.lpr; *.lpk|' +
    'Pascal Source (*.pas, *.pp)|*.pas; *.pp|' +
    'Text files (*.txt)|*.txt|' +
    'All files (*.*)|*.*';

  CONFIG_FILE_FILTERS =
    'Config files (*.cfg)|*.cfg|' +
    'Text files (*.txt)|*.txt|' +
    'XML files (*.xml)|*.xml|' +
    'All files (*.*)|*.*';

function DescribeFileCount(const piCount: integer): string;

implementation

uses SysUtils;

function DescribeFileCount(const piCount: integer): string;
begin
  if piCount = 1 then
    Result := '1 file'
  else
    Result := IntToStr(piCount) + ' files';
end;

end.


