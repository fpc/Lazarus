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
    Simple demonstrating, how to setup the codetools, FPC and Lazarus Source
    directory to find all references of a declaration.

  Usage:
    findreferences filename line column

    Filename is a unit.
    Line, column is an identifier.
}
program findreferences;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, Laz_AVL_Tree,
  // CodeTools
  CodeCache, CodeToolManager, CTUnitGraph;

const
  ConfigFilename = 'codetools.config';
var
  DeclCode, StartSrcCode, Code: TCodeBuffer;
  Filename, MainFilename: String;
  ListOfPCodeXYPosition: TFPList;
  X, Y, DeclX, DeclY, DeclTopLine, i: Integer;
  Identifier, CurLine: string;
  Graph: TUsesGraph;
  Cache: TFindIdentifierReferenceCache;
  TreeOfPCodeXYPosition: TAVLTree;
  ANode, Node: TAVLTreeNode;
  CodePos: PCodeXYPosition;
  Files: TStringList;
  Completed: boolean;
  UGUnit: TUGUnit;
begin
  if (ParamCount>=1) and (Paramcount<3) then begin
    writeln('Usage:');
    writeln('  ',ParamStr(0));
    writeln('  ',ParamStr(0),' <filename> <X> <Y> <mainfilename, e.g. the program>');
    Halt(1);
  end;

  CodeToolBoss.SimpleInit(ConfigFilename);

  // Example: find all references of "TMyInt"
  Filename:=ExpandFileName('scanexamples/findref_cursor.pas');
  X:=15;
  Y:=11;
  MainFilename:=ExpandFileName('scanexamples/findref_program.pas');

  if (ParamCount>=4) then begin
    Filename:=CleanAndExpandFilename(ParamStr(1));
    X:=StrToInt(ParamStr(2));
    Y:=StrToInt(ParamStr(3));
    MainFilename:=CleanAndExpandFilename(ParamStr(4));
    writeln('File: ',Filename,' Line=',Y,' Column=',X,' MainFilename=',MainFilename);
  end;

  // Step 1: load the file
  StartSrcCode:=CodeToolBoss.LoadFile(Filename,false,false);

  // Step 2: find the main declaration
  if not CodeToolBoss.FindMainDeclaration(StartSrcCode,
    X,Y,
    DeclCode,DeclX,DeclY,DeclTopLine) then
  begin
    writeln('CodeToolBoss.FindMainDeclaration failed: ',CodeToolBoss.ErrorMessage);
    ExitCode:=-1;
    exit;
  end;

  // Step 3: get identifier
  CodeToolBoss.GetIdentifierAt(DeclCode,DeclX,DeclY,Identifier);
  writeln('Found declaration: ',Identifier);

  // Step 4: collect all modules of program
  Files:=TStringList.Create;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  Cache:=nil;
  try
    Files.Add(DeclCode.Filename);
    if CompareFilenames(DeclCode.Filename,StartSrcCode.Filename)<>0 then
      Files.Add(DeclCode.Filename);

    // parse all used units
    Graph:=CodeToolBoss.CreateUsesGraph;
    try
      Graph.AddStartUnit(MainFilename);
      Graph.AddTargetUnit(DeclCode.Filename);
      Graph.Parse(true,Completed);
      Node:=Graph.FilesTree.FindLowest;
      while Node<>nil do begin
        UGUnit:=TUGUnit(Node.Data);
        Files.Add(UGUnit.Filename);
        Node:=Node.Successor;
      end;
    finally
      Graph.Free;
    end;

    // Step 5: find references in all files
    for i:=0 to Files.Count-1 do begin
      Code:=CodeToolBoss.LoadFile(Files[i],true,false);
      if Code=nil then begin
        writeln('unable to load "',Files[i],'"');
        continue;
      end;
      // search references
      CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if not CodeToolBoss.FindReferences(
        DeclCode,DeclX,DeclY,
        Code, true, ListOfPCodeXYPosition, Cache) then
      begin
        writeln('FindReferences failed in "',Code.Filename,'"');
        continue;
      end;
      if ListOfPCodeXYPosition=nil then continue;
      // In order to show all references after any parser error, they are
      // collected in a tree
      if TreeOfPCodeXYPosition=nil then
        TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
      CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                              TreeOfPCodeXYPosition,true,false);
    end;

    // Step 6: show references
    if TreeOfPCodeXYPosition=nil then begin
      writeln('HINT: No references found');
      exit;
    end;
    ANode:=TreeOfPCodeXYPosition.FindHighest;
    while ANode<>nil do begin
      CodePos:=PCodeXYPosition(ANode.Data);
      CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1,false));
      writeln('Reference: ',CodePos^.Code.Filename,'(',CodePos^.Y,',',CodePos^.X,'): ',CurLine);
      ANode:=TreeOfPCodeXYPosition.FindPrecessor(ANode);
    end;

  finally
    Files.Free;
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
  end;
end.

