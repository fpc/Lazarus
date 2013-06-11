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
    Listing directives of a unit (contrary to directives of a file).
}
program TestUnitDirectives;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs, CodeAtom,
  BasicCodeTools, SourceChanger, CodeTree,
  CodeToolsStructs, PascalParserTool, LinkScanner, directives1;

var
  Scanner: TLinkScanner;
  Filename: String;
  Code: TCodeBuffer;
  i: Integer;
  Dir: PLSDirective;
  FirstSortedIndex: integer;
  LastSortedIndex: integer;
begin
  if (ParamCount>=1) and (Paramcount<>1) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename>');
  end;
  Filename:=ExpandFileName(SetDirSeparators('scanexamples/directives1.pas'));

  // load the file
  Code:=CodeToolBoss.LoadFile(Filename,true,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // parse the unit
  if not CodeToolBoss.ExploreUnitDirectives(Code,Scanner) then
    raise Exception.Create('parser error');

  writeln('-----------------------------------------------');
  writeln(Scanner.CleanedSrc);
  writeln('-----------------------------------------------');
  writeln('Directives in compile order:');
  for i:=0 to Scanner.DirectiveCount-1 do begin
    Dir:=Scanner.Directives[i];
    writeln(i,'/',Scanner.DirectiveCount,
      ' CleanPos=',Dir^.CleanPos,'=',Scanner.CleanedPosToStr(Dir^.CleanPos),
      ' Level=',Dir^.Level,' ',dbgs(Dir^.State),
      ' "',ExtractCommentContent(Scanner.CleanedSrc,Dir^.CleanPos,Scanner.NestedComments),'"'
      );
  end;
  writeln('-----------------------------------------------');
  writeln('Directives sorted for Code and SrcPos:');
  for i:=0 to Scanner.DirectiveCount-1 do begin
    Dir:=Scanner.DirectivesSorted[i];
    write(i,'/',Scanner.DirectiveCount,
      ' CleanPos=',Dir^.CleanPos,'=',Scanner.CleanedPosToStr(Dir^.CleanPos),
      ' Level=',Dir^.Level,' ',dbgs(Dir^.State),
      ' "',ExtractCommentContent(Scanner.CleanedSrc,Dir^.CleanPos,Scanner.NestedComments),'"'
      );
    if Scanner.FindDirective(Code,Dir^.SrcPos,FirstSortedIndex,LastSortedIndex)
    then begin
      if FirstSortedIndex<LastSortedIndex then
        write(' MULTIPLE: ',FirstSortedIndex,'-',LastSortedIndex);
    end else begin
      raise Exception.Create('inconsistency: Scanner.FindDirective failed');
    end;
    writeln;
  end;

  writeln('-----------------------------------------------');
end.

