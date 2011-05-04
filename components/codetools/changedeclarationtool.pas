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
    TChangeDeclarationTool enhances TExtractProcTool.
    TChangeDeclarationTool provides functions to change/move declarations.
}
unit ChangeDeclarationTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AVL_Tree, contnrs,
  FileProcs, CodeTree, CodeAtom, ExtractProcTool, FindDeclarationTool,
  LinkScanner, SourceChanger;

type
  TChangeParamListAction = (
    cplaInsertNewParam,  // insert at Index a new parameter. Use DefaultValue in callers.
    cplaDeleteParam, // delete parameter at Index. In callers too.
    cplaMoveParam,  // move parameter at OldIndex to Index
    cplaMakeParamOptional, // add DefaultValue to parameter at Index. If callers use this value, remove it.
    cplaMakeParamNonOptional // remove default value from parameter at Index. Use DefaultValue at callers.
    );
  TChangeParamListActions = set of TChangeParamListAction;

  { TChangeParamListItem }

  TChangeParamListItem = class
  public
    Action: TChangeParamListAction;
    Index: integer;
    OldIndex: integer;
    ParamModifier: string;
    ParamName: string;
    ParamType: string;
    Value: string;
    constructor CreateInsertNewParam(TheIndex: integer;
                                     aModifier, aName, aType: string);
    constructor CreateDeleteParam(TheIndex: integer);
    constructor CreateMoveParam(TheOldIndex, NewIndex: integer);
    constructor CreateMakeParamOptional(TheIndex: integer; aValue: string);
    constructor CreateMakeParamNonOptional(TheIndex: integer; aValue: string);
  end;

  { TChangeDeclarationTool }

  TChangeDeclarationTool = class(TExtractProcTool)
  private
    function ChangeProcParamListDeclaration(ProcNode: TCodeTreeNode;
      Changes: TObjectList; // list of TChangeParamListItem
      SourceChanger: TSourceChangeCache): boolean;
    function ChangeParamListDeclaration(CleanPos: integer;
      Changes: TObjectList; // list of TChangeParamListItem
      SourceChanger: TSourceChangeCache): boolean;
  public
    function ChangeParamList(Changes: TObjectList; // list of TChangeParamListItem
       var ProcPos: TCodeXYPosition; // if it is in this unit the proc declaration is changed and this position is cleared
       TreeOfPCodeXYPosition: TAVLTree; // positions in this unit are processed and removed from the tree
       SourceChanger: TSourceChangeCache): boolean;
  end;

implementation

{ TChangeParamListItem }

constructor TChangeParamListItem.CreateInsertNewParam(TheIndex: integer;
  aModifier, aName, aType: string);
begin
  Action:=cplaInsertNewParam;
  Index:=TheIndex;
  ParamModifier:=aModifier;
  ParamName:=aName;
  ParamType:=aType;
end;

constructor TChangeParamListItem.CreateDeleteParam(TheIndex: integer);
begin
  Action:=cplaDeleteParam;
  Index:=TheIndex;
end;

constructor TChangeParamListItem.CreateMoveParam(TheOldIndex, NewIndex: integer);
begin
  Action:=cplaMoveParam;
  Index:=NewIndex;
  OldIndex:=TheOldIndex;
end;

constructor TChangeParamListItem.CreateMakeParamOptional(TheIndex: integer;
  aValue: string);
begin
  Action:=cplaMakeParamOptional;
  Index:=TheIndex;
  Value:=aValue;
end;

constructor TChangeParamListItem.CreateMakeParamNonOptional(TheIndex: integer;
  aValue: string);
begin
  Action:=cplaMakeParamNonOptional;
  Index:=TheIndex;
  Value:=aValue;
end;

{ TChangeDeclarationTool }

function TChangeDeclarationTool.ChangeProcParamListDeclaration(
  ProcNode: TCodeTreeNode; Changes: TObjectList;
  SourceChanger: TSourceChangeCache): boolean;
var
  HeadNode: TCodeTreeNode;
  ParamListNode: TCodeTreeNode;
  i: Integer;
  Change: TChangeParamListItem;
  NewCode: String;
  InsertPos: LongInt;
  ParamNode: TCodeTreeNode;
  j: Integer;
begin
  Result:=false;
  HeadNode:=ProcNode.FirstChild;
  if NodeNeedsBuildSubTree(HeadNode) then
    BuildSubTreeForProcHead(HeadNode);
  ParamListNode:=HeadNode.FirstChild;
  if (ParamListNode<>nil) and (ParamListNode.Desc<>ctnParameterList) then
    ParamListNode:=nil;
  for i:=0 to Changes.Count-1 do begin
    Change:=TChangeParamListItem(Changes[i]);
    if Change.Action=cplaInsertNewParam then begin
      // add a new parameter
      NewCode:='';
      if Change.ParamModifier<>'' then
        NewCode:=NewCode+Change.ParamModifier+' ';
      NewCode:=NewCode+Change.ParamName+':'+Change.ParamType;
      if ParamListNode=nil then begin
        debugln(['TChangeDeclarationTool.ChangeProcParamListDeclaration start new param list']);
        if Change.Index<>0 then
          raise Exception.Create('TChangeDeclarationTool.ChangeProcParamListDeclaration: add first: wrong index');
        MoveCursorBehindProcName(HeadNode);
        InsertPos:=CurPos.StartPos;
        NewCode:='('+NewCode+')';
        NewCode:=SourceChanger.BeautifyCodeOptions.BeautifyStatement(NewCode,0);
        Result:=SourceChanger.Replace(gtNone,gtNone,InsertPos,InsertPos,NewCode);
        exit;
      end;
      debugln(['TChangeDeclarationTool.ChangeProcParamListDeclaration extend existing param list']);
      if (Change.Index<0) or (Change.Index>ParamListNode.ChildCount) then
        raise Exception.Create('TChangeDeclarationTool.ChangeProcParamListDeclaration: insert: wrong index');
      if Change.Index=0 then begin
        // insert as first
        InsertPos:=ParamListNode.StartPos+1;
        if ParamListNode.FirstChild<>nil then
          NewCode:=NewCode+';';
        NewCode:=SourceChanger.BeautifyCodeOptions.BeautifyStatement(NewCode,0);
        Result:=SourceChanger.Replace(gtNone,gtSpace,InsertPos,InsertPos,NewCode);
        exit;
      end;
      ParamNode:=ParamListNode.FirstChild;
      for j:=1 to Change.Index-1 do
        ParamNode:=ParamNode.NextBrother;
      // insert behind ParamNode
      if ParamNode.NextBrother=nil then begin
        // insert as last
        InsertPos:=ParamNode.EndPos;
        NewCode:=';'+NewCode;
        NewCode:=SourceChanger.BeautifyCodeOptions.BeautifyStatement(NewCode,0);
        Result:=SourceChanger.Replace(gtNone,gtNone,InsertPos,InsertPos,NewCode);
        exit;
      end;
      // insert between two parameters
      if ParamNode.FirstChild<>nil then begin
        // e.g. a:t1; b:t2
        InsertPos:=ParamNode.EndPos;
        NewCode:=';'+NewCode;
        NewCode:=SourceChanger.BeautifyCodeOptions.BeautifyStatement(NewCode,0);
        Result:=SourceChanger.Replace(gtNone,gtNone,InsertPos,InsertPos,NewCode);
        exit;
      end;

      debugln(['TChangeDeclarationTool.ChangeProcParamListDeclaration ToDo: implement insert between']);
      exit;
    end;
  end;

  Result:=true;
end;

function TChangeDeclarationTool.ChangeParamListDeclaration(CleanPos: integer;
  Changes: TObjectList; SourceChanger: TSourceChangeCache): boolean;
var
  Node: TCodeTreeNode;
  ProcNode: TCodeTreeNode;
  ProcNode2: TCodeTreeNode;
begin
  Result:=false;
  Node:=FindDeepestNodeAtPos(CleanPos,true);
  if Node.Desc=ctnProcedureHead then
    Node:=Node.Parent;
  if Node.Desc=ctnProcedure then begin
    // change the parameter list of a procedure
    ProcNode:=Node;
    Result:=ChangeProcParamListDeclaration(ProcNode,Changes,SourceChanger);
    if not Result then exit;
    ProcNode2:=FindCorrespondingProcNode(ProcNode);
    if ProcNode2<>nil then begin
      Result:=ChangeProcParamListDeclaration(ProcNode2,Changes,SourceChanger);
      if not Result then exit;
    end;
  end else begin
    debugln(['TChangeDeclarationTool.ChangeParamListDeclaration unsupported node=',Node.DescAsString]);
    exit;
  end;
  Result:=true;
end;

function TChangeDeclarationTool.ChangeParamList(Changes: TObjectList;
  var ProcPos: TCodeXYPosition; TreeOfPCodeXYPosition: TAVLTree;
  SourceChanger: TSourceChangeCache): boolean;
var
  CleanPos: integer;
begin
  Result:=false;
  if (Changes=nil) or (Changes.Count=0) then exit(true);
  if Changes.Count<>1 then begin
    debugln(['TChangeDeclarationTool.ChangeParamList sorry, only one change supported']);
    exit;
  end;
  BuildTree(lsrEnd);
  SourceChanger.MainScanner:=Scanner;
  if (ProcPos.Code<>nil) and (CaretToCleanPos(ProcPos,CleanPos)=0) then begin
    // declaration is in this unit
    ProcPos:=CleanCodeXYPosition;
    if not ChangeParamListDeclaration(CleanPos,Changes,SourceChanger) then exit;
  end;
  Result:=SourceChanger.Apply;
end;

end.

