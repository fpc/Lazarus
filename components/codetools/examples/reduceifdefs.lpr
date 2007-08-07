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
    Demonstration of how to reduce IFDEFs in a source file.
}
program ReduceIFDEFs;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, SimpleUnit1, FileProcs,
  BasicCodeTools, SourceChanger, CodeToolsConfig, CodeCompletionTool, CodeTree,
  DirectivesTree;
  
var
  Filename: string;
  Code: TCodeBuffer;
  Tree: TCompilerDirectivesTree;
  Changed: Boolean;
begin
  // load the file
  Filename:=ExpandFileName(SetDirSeparators('scanexamples/uglyifdefs.pas'));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // Example: run the ReduceIFDEFs tool
  Tree:=TCompilerDirectivesTree.Create;
  if not Tree.Parse(Code,CodeToolBoss.GetNestedCommentsFlagForFile(Code.Filename))
  then begin
    writeln('failed parsing compiler directives');
    exit;
  end;
  //repeat
    Changed:=false;
    Tree.ReduceCompilerDirectives(Changed);
    Tree.WriteDebugReport;
  //until not Changed;

  // write the new source:
  writeln('-----------------------------------');
  writeln('Changed=',Changed,' Source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

