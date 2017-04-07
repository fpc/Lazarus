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
    Demonstrating how to find all units using a unit.
}
program UsedByUnits;

{$mode objfpc}{$H+}

uses
  SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils, LazFileCache,
  // CodeTools
  FileProcs, CTUnitGraph, CodeToolManager;

const
  ConfigFilename = 'codetools.config';
var
  Filename: String;
  i: Integer;
  ExpFilename: String;
  UsesGraph: TUsesGraph;
  Complete: boolean;
  UsingUnits: TAVLTree;
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;

procedure WriteUsage;
begin
  writeln('Usage:');
  writeln('  ',ParamStr(0),' startunit1 <startunit2> ... targetunit');
  writeln;
  writeln('The startunit1 is the file name of a pasal source (e.g. a program)');
  writeln('There can be multiple start units.');
  writeln('The targetunit is the file name of a pascal unit which is used by');
  writeln('the result.');
  writeln('This tool parses recursively all uses sections. It ignores units,');
  writeln('which unit search path can not reach targetunit.');
  writeln('The result is a list of units that uses directly or indirectly');
  writeln('the targetunit.');
  writeln;
  writeln('Example:');
  writeln('./usedbyunits ../stdcodetools.pas ../fileprocs.pas');
  Halt;
end;

begin
  if (Paramcount<2) then
    WriteUsage;

  CodeToolBoss.SimpleInit(ConfigFilename);

  UsesGraph:=CodeToolBoss.CreateUsesGraph;
  UsingUnits:=nil;
  try
    for i:=1 to Paramcount do begin
      Filename:=ParamStr(i);
      if Filename='' then
        WriteUsage;
      ExpFilename:=TrimAndExpandFilename(Filename);
      if not FileExistsCached(ExpFilename) then begin
        writeln('file not found: ',Filename);
        Halt;
      end;
      if i<Paramcount then
        UsesGraph.AddStartUnit(ExpFilename)
      else
        UsesGraph.AddTargetUnit(ExpFilename);
    end;

    if not UsesGraph.Parse(true,Complete) then begin
      debugln('UsesGraph.Parse failed');
    end;

    UsingUnits:=UsesGraph.GetUnitsTreeUsingTargets;
    debugln(['Using units: ',UsingUnits.Count]);
    AVLNode:=UsingUnits.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUGUnit(AVLNode.Data);
      debugln('using unit: ',CurUnit.Filename);
      AVLNode:=UsingUnits.FindSuccessor(AVLNode);
    end;

  finally
    UsingUnits.Free;
    UsesGraph.Free;
  end;
end.

