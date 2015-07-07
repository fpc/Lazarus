{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Demonstration how to convert a procedure to a method.
}
program proctomethod;

{$mode objfpc}{$H+}

uses
  SysUtils, CodeCache, CodeToolManager, CodeTree, ProcsAndMethods1;

const
  ConfigFilename = 'codetools.config';
var
  Filename: string;
  Code: TCodeBuffer;
  Tool: TCodeTool;
  CleanDef: String;
  ProcNode: TCodeTreeNode;
begin
  CodeToolBoss.SimpleInit(ConfigFilename);

  // load the file
  Filename:=ExpandFileName(SetDirSeparators('scanexamples/procsandmethods1.pas'));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // parse the unit
  try
    CleanDef:='DoSomething(integer)';
    if not CodeToolBoss.FindProcDeclaration(Code,CleanDef,Tool,ProcNode)
    then
      raise Exception.Create('proc not found: "'+CleanDef+'"');

  finally

  end;
  // write the new source:
  writeln('-----------------------------------');
  writeln('New source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

