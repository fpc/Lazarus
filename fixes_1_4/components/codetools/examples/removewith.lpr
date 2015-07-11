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
    Demonstration how to remove a with block.
}
program RemoveWith;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs, AVL_Tree, CodeAtom,
  BasicCodeTools, SourceChanger, CodeTree, FindDeclarationTool, RemoveWith1;

const
  ConfigFilename = 'codetools.config';
var
  Filename: string;
  Code: TCodeBuffer;
  X: Integer;
  Y: Integer;
begin
  if (ParamCount>=1) and (Paramcount<>3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y>');
    writeln('  ',ParamStr(0),' scanexamples/removewith1.pas 19 38');
  end;

  CodeToolBoss.SimpleInit(ConfigFilename);

  // load the file
  Filename:='scanexamples/removewith1.pas';
  X:=19;
  Y:=38;
  if Paramcount=3 then begin
    Filename:=ParamStrUTF8(1);
    X:=StrToIntDef(ParamStrUTF8(2),1);
    Y:=StrToIntDef(ParamStrUTF8(3),1);
  end;

  Filename:=ExpandFileName(SetDirSeparators(Filename));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed: '+Filename);

  // parse the unit and remove the with variable
  if not CodeToolBoss.RemoveWithBlock(Code,X,Y) then
    raise Exception.Create('RemoveWithBlock failed');

  // write the new source:
  writeln('-----------------------------------');
  writeln('New source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

