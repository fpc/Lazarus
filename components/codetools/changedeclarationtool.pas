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
    cplaChangeDefaultValue // if caller use default change to old value, if caller use new value remove it
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
    DefaultValue: string;
    constructor CreateInsertNewParam(TheIndex: integer;
                                     aModifier, aName, aType: string);
    constructor CreateDeleteParam(TheIndex: integer);
    constructor CreateMoveParam(TheOldIndex, NewIndex: integer);
    constructor CreateChangeDefaultValue(TheIndex: integer; aValue: string);
  end;

  { TChangeDeclarationTool }

  TChangeDeclarationTool = class(TExtractProcTool)
  private
    function ChangeParamListDeclaration(ParentNode: TCodeTreeNode;
      Changes: TObjectList; // list of TChangeParamListItem
      SourceChanger: TSourceChangeCache): boolean;
    function ChangeParamListDeclarationAtPos(CleanPos: integer;
      Changes: TObjectList; // list of TChangeParamListItem
      SourceChanger: TSourceChangeCache): boolean;
  public
    function ChangeParamList(Changes: TObjectList; // list of TChangeParamListItem
       var ProcPos: TCodeXYPosition; // if it is in this unit the proc declaration is changed and this position is cleared
       TreeOfPCodeXYPosition: TAVLTree; // positions in this unit are processed and removed from the tree
       SourceChanger: TSourceChangeCache): boolean;
  end;

implementation

type

  { TChangeParamTransactionInsert }

  TChangeParamTransactionInsert = class
  public
    Src: string; // if Src='' then use Modifier+Name+Typ
    Modifier: string;
    Name: string;
    Typ: string;
    Value: string;
    MergeAllowed: boolean;
    constructor Create(aSrc, aModifier, aName, aType, aValue: string;
                       aMergeAllowed: boolean = true);
  end;

  { TChangeParamTransactionPos }

  TChangeParamTransactionPos = class
  public
    Node: TCodeTreeNode; // old param node
    Delete: boolean;
    NewDefaultValue: string;
    InsertBehind: TObjectList;// list of TChangeParamTransactionInsert
    constructor Create;
    destructor Destroy; override;
  end;

  { TChangeParamListTransactions }

  TChangeParamListTransactions = class
  public
    OldNodes: array of TChangeParamTransactionPos; // one for each old param node
    InsertFirst: TObjectList;// list of TChangeParamTransactionInsert
    Node: TCodeTreeNode; // ctnParameterList
    constructor Create(ParamList: TCodeTreeNode);
    destructor Destroy; override;
    function MaxPos: integer;
    procedure Insert(Index: integer; Insertion: TChangeParamTransactionInsert);
  end;

{ TChangeParamTransactionInsert }

constructor TChangeParamTransactionInsert.Create(aSrc, aModifier, aName, aType,
  aValue: string; aMergeAllowed: boolean);
begin
  Src:=aSrc;
  Modifier:=aModifier;
  Name:=aName;
  Typ:=aType;
  Value:=aValue;
  MergeAllowed:=aMergeAllowed;
end;

constructor TChangeParamTransactionPos.Create;
begin
  InsertBehind:=TObjectList.create(true);
end;

destructor TChangeParamTransactionPos.Destroy;
begin
  InsertBehind.Free;
  inherited Destroy;
end;

{ TChangeParamListInfos }

function TChangeParamListTransactions.MaxPos: integer;
begin
  Result:=length(OldNodes);
end;

procedure TChangeParamListTransactions.Insert(Index: integer;
  Insertion: TChangeParamTransactionInsert);
begin
  if Index=0 then
    InsertFirst.Add(Insertion)
  else
    OldNodes[Index-1].InsertBehind.Add(Insertion);
end;

constructor TChangeParamListTransactions.Create(ParamList: TCodeTreeNode);
var
  ParamNode: TCodeTreeNode;
  i: Integer;
begin
  InsertFirst:=TObjectList.create(true);
  Node:=ParamList;
  if Node<>nil then begin
    SetLength(OldNodes,Node.ChildCount);
    ParamNode:=Node.FirstChild;
    i:=0;
    while ParamNode<>nil do begin
      OldNodes[i]:=TChangeParamTransactionPos.Create;
      OldNodes[i].Node:=ParamNode;
      ParamNode:=ParamNode.NextBrother;
    end;
  end;
end;

destructor TChangeParamListTransactions.Destroy;
var
  i: Integer;
begin
  for i:=0 to length(OldNodes)-1 do
    FreeAndNil(OldNodes[i]);
  SetLength(OldNodes,0);
  InsertFirst.Free;
  inherited Destroy;
end;

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

constructor TChangeParamListItem.CreateChangeDefaultValue(TheIndex: integer;
  aValue: string);
begin
  Action:=cplaChangeDefaultValue;
  Index:=TheIndex;
  DefaultValue:=aValue;
end;

{ TChangeDeclarationTool }

function TChangeDeclarationTool.ChangeParamListDeclaration(
  ParentNode: TCodeTreeNode; Changes: TObjectList;
  SourceChanger: TSourceChangeCache): boolean;
var
  ParamListNode: TCodeTreeNode;
  i: Integer;
  Change: TChangeParamListItem;
  Transactions: TChangeParamListTransactions;
begin
  Result:=false;

  // for procs: use ctnProcedureHead as parent
  if ParentNode.Desc=ctnProcedure then
    ParentNode:=ParentNode.FirstChild;
  if (ParentNode.Desc=ctnProcedureHead) and NodeNeedsBuildSubTree(ParentNode) then
    BuildSubTreeForProcHead(ParentNode);

  ParamListNode:=ParentNode.FirstChild;
  if (ParamListNode<>nil) and (ParamListNode.Desc<>ctnParameterList) then
    ParamListNode:=nil;
  Transactions:=TChangeParamListTransactions.Create(ParamListNode);
  try
    // ToDo: parse param list


    for i:=0 to Changes.Count-1 do begin
      Change:=TChangeParamListItem(Changes[i]);
      if (Change.Index<0) or (Change.Index>Transactions.MaxPos) then
        raise Exception.Create('TChangeDeclarationTool.ChangeProcParamListDeclaration: index out of bounds');
      case Change.Action of
      cplaInsertNewParam:
        Transactions.Insert(Change.Index,
          TChangeParamTransactionInsert.Create('',Change.ParamModifier,
                               Change.ParamName,Change.ParamType,Change.DefaultValue));

      cplaDeleteParam:
        Transactions.OldNodes[Change.Index].Delete:=true;

      cplaMoveParam:
        begin
          if (Change.OldIndex<0) or (Change.OldIndex>Transactions.MaxPos) then
            raise Exception.Create('TChangeDeclarationTool.ChangeProcParamListDeclaration: index out of bounds');
          if Change.OldIndex<>Change.Index then begin
            Transactions.OldNodes[Change.OldIndex].Delete:=true;
            // ToDo: check if param contains comments
            //  if yes: copy with comments
            //  if not: extract parts
            raise Exception.Create('TChangeDeclarationTool.ChangeProcParamListDeclaration: ToDo: move');
          end;
        end;

      cplaChangeDefaultValue:
        Transactions.OldNodes[Change.OldIndex].NewDefaultValue:=Change.DefaultValue;

      end;
    end;

    // ToDo: apply transactions

  finally
    Transactions.Free;
  end;
  Result:=true;
end;

function TChangeDeclarationTool.ChangeParamListDeclarationAtPos(CleanPos: integer;
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
    Result:=ChangeParamListDeclaration(ProcNode,Changes,SourceChanger);
    if not Result then exit;
    ProcNode2:=FindCorrespondingProcNode(ProcNode);
    if ProcNode2<>nil then begin
      Result:=ChangeParamListDeclaration(ProcNode2,Changes,SourceChanger);
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
  BuildTree(lsrEnd);
  SourceChanger.MainScanner:=Scanner;
  if (ProcPos.Code<>nil) and (CaretToCleanPos(ProcPos,CleanPos)=0) then begin
    // declaration is in this unit
    ProcPos:=CleanCodeXYPosition;
    if not ChangeParamListDeclarationAtPos(CleanPos,Changes,SourceChanger) then exit;
  end;
  Result:=SourceChanger.Apply;
end;

end.

