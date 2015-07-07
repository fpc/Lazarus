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
    Demonstrating, how to add a method Assign to a class.
}
program AddMethodAssign;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, AVL_Tree, CodeCache, CodeToolManager, FileProcs,
  BasicCodeTools, CodeTree, FindDeclarationTool, AssignExample1;

const
  ConfigFilename = 'codetools.config';
var
  Filename: string;
  Code: TCodeBuffer;
  Tool: TCodeTool;
  AssignDeclNode: TCodeTreeNode;
  MemberNodeExts: TAVLTree;
  AssignBodyNode: TCodeTreeNode;
  AVLNode: TAVLTreeNode;
  NodeExt: TCodeTreeNodeExtension;
  NextAVLNode: TAVLTreeNode;
  ClassNode: TCodeTreeNode;
  InheritedDeclContext: TFindContext;
  ParamName: String;
  ParamType: String;
  ParamNode: TCodeTreeNode;
  InheritedIsTPersistent: boolean;
  InheritedClassNode: TCodeTreeNode;
  AssignMembers: TFPList;
  NewPos: TCodeXYPosition;
  NewTopline: integer;
  i: Integer;
begin
  CodeToolBoss.SimpleInit(ConfigFilename);

  // load the file
  Filename:=ExpandFileName(SetDirSeparators('scanexamples/assignexample1.pas'));
  Code:=CodeToolBoss.LoadFile(Filename,false,false);
  if Code=nil then
    raise Exception.Create('loading failed '+Filename);

  // parse the unit, check if in a class with an Assign method
  AssignMembers:=TFPList.Create;
  try
    MemberNodeExts:=nil;
    if not CodeToolBoss.FindAssignMethod(Code,3,18,
      Tool,ClassNode,AssignDeclNode,MemberNodeExts,AssignBodyNode,
      InheritedDeclContext) then
      raise Exception.Create('parser error');

    debugln(['Assign declaration found: ',AssignDeclNode<>nil]);
    debugln(['Assign body found: ',AssignBodyNode<>nil]);
    debugln(['Inherited Assign found: ',InheritedDeclContext.Node<>nil]);

    // remove nodes which are written by a property
    if MemberNodeExts<>nil then begin
      AVLNode:=MemberNodeExts.FindLowest;
      while AVLNode<>nil do begin
        NextAVLNode:=MemberNodeExts.FindSuccessor(AVLNode);
        NodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
        if NodeExt.Data<>nil then begin
          debugln(['skipping identifier ',NodeExt.Txt,' because it is written by a property']);
        end else begin
          debugln('assigning identifier ',NodeExt.Txt,' ...');
          AssignMembers.Add(NodeExt);
          MemberNodeExts.Delete(AVLNode);
        end;
        AVLNode:=NextAVLNode;
      end;
    end;
    if (AssignMembers.Count=0) then begin
      debugln('no assignable members found');
      exit;
    end;

    ParamName:='Source';
    ParamType:='TObject';
    InheritedIsTPersistent:=false;

    // check if inherited exists, if it is TPersistent.Assign and use the
    // inherited parameter name and type
    if InheritedDeclContext.Node<>nil then begin
      InheritedClassNode:=InheritedDeclContext.Tool.FindClassOrInterfaceNode(InheritedDeclContext.Node);
      InheritedIsTPersistent:=(InheritedClassNode<>nil)
        and (InheritedClassNode.Parent.Desc=ctnTypeDefinition)
        and (CompareIdentifiers('TPersistent',@InheritedDeclContext.Tool.Src[InheritedClassNode.Parent.StartPos])=0);
      ParamNode:=InheritedDeclContext.Tool.GetProcParamList(InheritedDeclContext.Node);
      if ParamNode<>nil then begin
        ParamNode:=ParamNode.FirstChild;
        if ParamNode<>nil then begin
          ParamName:=InheritedDeclContext.Tool.ExtractDefinitionName(ParamNode);
          if (ParamNode.FirstChild<>nil) and (ParamNode.FirstChild.Desc=ctnIdentifier) then
            ParamType:=GetIdentifier(@InheritedDeclContext.Tool.Src[ParamNode.FirstChild.StartPos]);
        end;
      end;
    end;

    // add assign method
    if AssignDeclNode=nil then begin
      if not Tool.AddAssignMethod(ClassNode,AssignMembers,
             'Assign',ParamName,ParamType,
             InheritedDeclContext.Node<>nil,true,InheritedIsTPersistent,
             CodeToolBoss.SourceChangeCache,NewPos,NewTopline)
      then
        raise Exception.Create('AddAssignMethod failed');
    end else begin
      debugln(['there is already an Assign method']);
    end;

  finally
    DisposeAVLTree(MemberNodeExts);
    for i:=0 to AssignMembers.Count-1 do
      TObject(AssignMembers[i]).Free;
    FreeAndNil(AssignMembers);
  end;
  // write the new source:
  writeln('-----------------------------------');
  writeln('New source:');
  writeln(Code.Source);
  writeln('-----------------------------------');
end.

