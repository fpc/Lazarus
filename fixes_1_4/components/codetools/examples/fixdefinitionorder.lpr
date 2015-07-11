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
    FPC allows forward definitions only in a type section.
    When a file was translated from other languages, the forward definitions
    are often wrong.
    This demonstrates how to fix forward definitions automatically.
}
program FixDefinitionOrder;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CodeCache, CodeToolManager, FileProcs, CodeTree,
  DirectivesTree;
  
var
  Filename: string;
  Code: TCodeBuffer;
begin
  // load the file
  if ParamCount>=1 then
    Filename:=ExpandFileName(ParamStr(1))
  else
    Filename:=ExpandFileName(SetDirSeparators('scanexamples/wrongforwarddefinitions.pas'));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);
    
  {if not CodeToolBoss.FixAllAliasDefinitions(Code) then begin
    writeln('FixAllAliasDefinitions failed');
    exit;
  end;}

  // fix constants
  if not CodeToolBoss.FixForwardDefinitions(Code) then begin
    writeln('FixForwardDefinitions failed');
    exit;
  end;

  // write the new source:
  writeln('-----------------------------------');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

