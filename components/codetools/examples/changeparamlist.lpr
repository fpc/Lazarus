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
    Demonstration how to change the parameter list of a procedure and
    adapt all references.
}
program changeparamlist;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, contnrs, Laz_AVL_Tree,
  // CodeTools
  CodeCache, CodeToolManager, FileProcs, CodeAtom, CodeTree, SourceChanger,
  FindDeclarationTool, CTUnitGraph, ChangeDeclarationTool,
  ChangeParamList1;

const
  ConfigFilename = 'codetools.config';

var
  Filename: string;
  Code: TCodeBuffer;
  Tool: TCodeTool;

procedure ChangeProc(ProcName: string; var Changes: TObjectList);
var
  ProcNode: TCodeTreeNode;
  ProcPos: TCodeXYPosition;
  RefCache: TFindIdentifierReferenceCache;
  ListOfPCodeXYPosition: TFPList;
  TreeOfPCodeXYPosition: TAVLTree;
begin
  RefCache:=nil;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
  try
    if not CodeToolBoss.FindProcDeclaration(Code,ProcName,Tool,ProcNode) then
      raise Exception.Create('procedure '+ProcName+' not found in '+Filename);
    if not Tool.CleanPosToCaret(ProcNode.FirstChild.StartPos,ProcPos) then
      raise Exception.Create('Tool.CleanPosToCaret for ProcNode failed');
    debugln(['Proc at ',dbgs(ProcPos)]);

    if not CodeToolBoss.FindReferences(ProcPos.Code,ProcPos.X,ProcPos.Y,Code,
       true,ListOfPCodeXYPosition,RefCache)
    then
      raise Exception.Create('FindReferences failed for '+Code.Filename);

    CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                                TreeOfPCodeXYPosition,true,false);

    if not CodeToolBoss.ChangeParamList(ProcPos.Code,Changes,ProcPos,
       TreeOfPCodeXYPosition)
    then
      raise Exception.Create('ChangeParamList failed for '+Code.Filename);

  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    RefCache.Free;
    Changes.Clear;
  end;
  // write the new source:
  writeln('-----------------------------------');
  writeln('New source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end;

var
  Changes: TObjectList;
begin
  CodeToolBoss.SimpleInit(ConfigFilename);

  // load the file
  Filename:='scanexamples/changeparamlist1.pas';
  Filename:=ExpandFileName(SetDirSeparators(Filename));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed: '+Filename);

  Changes:=TObjectList.create(true);
  try
    // Test: add the first parameter to a procedure
    //Changes.Add(TChangeParamListItem.CreateInsertNewParam(0,'','p1','integer'));
    //ChangeProc('DoNoParams',Changes);

    // Test: add another parameter as first to a procedure
    //Changes.Add(TChangeParamListItem.CreateInsertNewParam(0,'','p2','integer'));
    //ChangeProc('DoOneParam(char)',Changes);

    // Test: add another parameter as last to a procedure
    //Changes.Add(TChangeParamListItem.CreateInsertNewParam(1,'','p3','integer'));
    //ChangeProc('DoOneParam(char)',Changes);

    // Test: insert another parameter between two procedure parameters
    Changes.Add(TChangeParamListItem.CreateInsertNewParam(1,'','p3','integer'));
    ChangeProc('DoTwoParams1(,word)',Changes);
  finally
    Changes.Free;
  end;


  // write the new source:
  writeln('-----------------------------------');
  writeln('New source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

