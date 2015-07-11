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
    Demonstrating, how to add a method to a class and extending the uses section.
}
program AddClass;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs,
  BasicCodeTools, SourceChanger, CodeTree;
  
var
  Filename: string;
  Code: TCodeBuffer;
  Tool: TCodeTool;
  UsesSection: TCodeTreeNode;
  InsertPos: LongInt;
  Indent: LongInt;
  ClassSrc: String;
  SourceChangeCache: TSourceChangeCache;
  i: Integer;
begin
  // load the file
  Filename:=ExpandFileName(SetDirSeparators('scanexamples/simpleunit1.pas'));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // Example: add a new class right after the uses section.
  ClassSrc:='type'+LineEnding
           +'  TNewClass = class'+LineEnding
           +'  end;';
  // parse the unit
  if not CodeToolBoss.Explore(Code,Tool,false,true) then begin
    raise Exception.Create('parser error');
  end;
  SourceChangeCache:=CodeToolBoss.SourceChangeCache;
  // find a nice insert position (behind the uses section)
  UsesSection:=Tool.FindMainUsesSection(false);
  InsertPos:=UsesSection.EndPos;
  // use the same indentation as the start of the uses section
  Indent:=GetLineIndent(Tool.Src,UsesSection.StartPos);
  ClassSrc:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                                                               ClassSrc,Indent);
  // init the SourceChangeCache
  SourceChangeCache.MainScanner:=Tool.Scanner;
  if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
                                   ClassSrc)
  then
    raise Exception.Create('unable to insert there (maybe the InsertPos does not exist?)');

  // write the changed files
  writeln('The following files will be modified:');
  for i:=0 to SourceChangeCache.BuffersToModifyCount-1 do
    writeln(SourceChangeCache.BuffersToModify[i].Filename);

  // modify the files
  if not SourceChangeCache.Apply then
    raise Exception.Create('unable to apply changes (maybe a file is readonly?)');
    
  // write the new source:
  writeln('-----------------------------------');
  writeln('New source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

