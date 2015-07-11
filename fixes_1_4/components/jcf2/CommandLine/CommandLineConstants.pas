unit CommandLineConstants;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is CommandLineConstants, released August 2008.
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

uses
  JcfStringUtils,
  JcfVersionConsts;

const
  ABOUT_COMMANDLINE =
    'JEDI Code Format V' + PROGRAM_VERSION + NativeLineBreak +
    ' ' + PROGRAM_DATE + NativeLineBreak +
    ' A Delphi Object-Pascal Source code formatter' + NativeLineBreak +
    ' A GUI version of this program is also available' + NativeLineBreak +
    ' Latest version at ' + PROGRAM_HOME_PAGE + NativeLineBreak + NativeLineBreak +
    'Syntax: jcf [options] path/filename ' + NativeLineBreak +
    ' Parameters to the command-line program: ' + NativeLineBreak + NativeLineBreak +

    ' Mode of operation: ' + NativeLineBreak +
    ' -obfuscate Obfuscate mode or ' + NativeLineBreak +
    ' -clarify Clarify mode' + NativeLineBreak +
    '   When neither is specified, registry setting will be used.' + NativeLineBreak +
    '   This normally means clarify.' + NativeLineBreak + NativeLineBreak +

    ' Mode of source: ' + NativeLineBreak +
    ' -F Format a file. The file name must be specified.' + NativeLineBreak +
    ' -D Format a directory. The directory name must be specified.' + NativeLineBreak +
    ' -R Format a directory tree. The root directory name must be specified.' +
    NativeLineBreak +
    '  When no file mode is specified, registry setting will be used.' +
    NativeLineBreak + NativeLineBreak +

    ' Mode of output: ' + NativeLineBreak +
    ' -inplace change the source file without backup' + NativeLineBreak +
    ' -out output to a new file' + NativeLineBreak +
    ' -backup change the file and leave the original file as a backup' + NativeLineBreak +
    '  If no output mode is specified, registry setting will be used.' +
    NativeLineBreak + NativeLineBreak +

    ' Other options: ' + NativeLineBreak +
    ' -config=filename  To specify a named configuration file' + NativeLineBreak +
    ' -y Overwrite files without confirmation.' + NativeLineBreak +
    ' -? Display this help' + NativeLineBreak;

implementation

end.
