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
  BasicCodeTools, KeywordFuncLists, LinkScanner, SourceChanger;

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
    procedure CDTParseParamList(ParentNode: TCodeTreeNode; Transactions: TObject);
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
    Src: string; // if Src='' then use Modifier+Name+Typ+Value
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
    // example: (var buf; {header} a,b:c; d:word=3 {footer}; ...)
    HeaderCommentPos: integer;
    Modifier: TAtomPosition; // ... (macpas varargs), const, var out, constref
    Name: TAtomPosition; // '...' has no name
    Typ: TAtomPosition;
    DefaultValue: TAtomPosition;
    HasComments: boolean;
    FooterCommentEndPos: integer;
    Separator: integer; // the comma or semicolon to the next parameter
    CommentAfterSeparator: TAtomPosition;
    FirstInGroup: integer; // index of first parameter i a group, e.g. a,b:c
    LastInGroup: integer;

    Delete: boolean;
    NewDefaultValue: string;
    InsertBehind: TObjectList;// list of TChangeParamTransactionInsert
    constructor Create;
    destructor Destroy; override;
    function GetFirstPos: integer;
    function GetLastPos(WithSeparator: boolean): integer;
  end;

  { TChangeParamListTransactions }

  TChangeParamListTransactions = class
  public
    OldNodes: array of TChangeParamTransactionPos; // one for each old param node
    InsertFirst: TObjectList;// list of TChangeParamTransactionInsert
    Node: TCodeTreeNode; // ctnParameterList
    BehindNamePos: integer;
    BracketOpenPos: integer;
    BracketClosePos: integer;
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

function TChangeParamTransactionPos.GetFirstPos: integer;
begin
  if HeaderCommentPos>0 then
    Result:=HeaderCommentPos
  else if Modifier.StartPos>0 then
    Result:=Modifier.StartPos
  else
    Result:=Name.StartPos;
end;

function TChangeParamTransactionPos.GetLastPos(WithSeparator: boolean): integer;
begin
  Result:=0;
  if WithSeparator then begin
    if CommentAfterSeparator.EndPos>0 then
      Result:=CommentAfterSeparator.EndPos
    else if Separator>0 then
      Result:=Separator;
    if Result>0 then exit;
  end;
  if FooterCommentEndPos>0 then
    Result:=FooterCommentEndPos
  else if DefaultValue.EndPos>0 then
    Result:=DefaultValue.EndPos
  else if Typ.EndPos>0 then
    Result:=Typ.EndPos
  else if Name.EndPos>0 then
    Result:=Name.EndPos
  else
    Result:=Modifier.EndPos;
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

procedure TChangeDeclarationTool.CDTParseParamList(ParentNode: TCodeTreeNode;
  Transactions: TObject);
var
  t: TChangeParamListTransactions;
  ParamIndex: Integer;
  CurParam: TChangeParamTransactionPos;
  FirstInGroup: integer;
  i: LongInt;
  CloseBracket: Char;
  StartPos: LongInt;
  EndPos: Integer;
  p: PChar;

  procedure ReadPrefixModifier;
  begin
    // read parameter prefix modifier
    if UpAtomIs('VAR') or UpAtomIs('CONST') or UpAtomIs('CONSTREF')
    or (UpAtomIs('OUT') and (Scanner.CompilerMode in [cmOBJFPC,cmDELPHI,cmFPC]))
    then begin
      CurParam.Modifier:=CurPos;
      ReadNextAtom;
    end;
  end;

begin
  t:=Transactions as TChangeParamListTransactions;
  // parse param list
  if ParentNode.Desc=ctnProcedureHead then
    MoveCursorBehindProcName(ParentNode)
  else if ParentNode.Desc=ctnProperty then
    MoveCursorBehindPropName(ParentNode)
  else
    raise Exception.Create('TChangeDeclarationTool.ChangeParamListDeclaration kind not supported: '+ParentNode.DescAsString);
  t.BehindNamePos:=LastAtoms.GetValueAt(0).EndPos;
  // read bracket
  if CurPos.Flag=cafRoundBracketOpen then
    CloseBracket:=')'
  else if CurPos.Flag=cafEdgedBracketOpen then
    CloseBracket:=']'
  else
    exit; // no param list

  t.BracketOpenPos:=CurPos.StartPos;
  ParamIndex:=0;
  ReadNextAtom;
  repeat
    CurParam:=t.OldNodes[ParamIndex];
    FirstInGroup:=-1;
    if AtomIs('...') then begin
      // MacPas '...' VarArgs parameter
      ReadNextAtom;
      // parse end of parameter list
      if (CurPos.StartPos>SrcLen)
      or (Src[CurPos.StartPos]<>CloseBracket) then
        RaiseCharExpectedButAtomFound(CloseBracket);
      break;
    end else begin
      ReadPrefixModifier;
      // read parameter name(s)
      repeat
        AtomIsIdentifier(true);
        CurParam.Name:=CurPos;
        ReadNextAtom;
        if CurPos.Flag<>cafComma then
          break;
        CurParam.Separator:=CurPos.StartPos;
        // A group. Example: b,c:char;
        if FirstInGroup<0 then FirstInGroup:=ParamIndex;
        inc(ParamIndex);
        CurParam:=t.OldNodes[ParamIndex];
        ReadNextAtom;
      until false;
      if FirstInGroup>=0 then begin
        for i:=FirstInGroup to ParamIndex do begin
          t.OldNodes[i].FirstInGroup:=FirstInGroup;
          t.OldNodes[i].LastInGroup:=ParamIndex;
        end;
      end;
      // read parameter type
      if CurPos.Flag=cafColon then begin
        ReadNextAtom;
        CurParam.Typ:=CurPos;
        if not ReadParamType(true,false,[]) then exit;
        CurParam.Typ.EndPos:=LastAtoms.GetValueAt(0).EndPos;
        if CurPos.Flag=cafEqual then begin
          // read default value
          ReadNextAtom;
          CurParam.DefaultValue:=CurPos;
          ReadConstant(true,false,[]);
          CurParam.DefaultValue.EndPos:=LastAtoms.GetValueAt(0).EndPos;
        end;
      end;
      // close bracket or semicolon
      if CurPos.Flag in [cafRoundBracketClose,cafEdgedBracketClose] then begin
        t.BracketClosePos:=CurPos.StartPos;
        break;
      end;
      if CurPos.Flag<>cafSemicolon then
        RaiseCharExpectedButAtomFound(CloseBracket);
      CurParam.Separator:=CurPos.StartPos;
      inc(ParamIndex);
    end;
  until false;

  // check for each parameter if it has comments
  for i:=0 to t.MaxPos-1 do begin
    CurParam:=t.OldNodes[i];

    // check if the param has a comment in front belonging to the param
    if i=0 then
      StartPos:=t.BracketOpenPos+1
    else
      StartPos:=t.OldNodes[i-1].GetLastPos(true);
    EndPos:=CurParam.GetFirstPos;
    while (StartPos<EndPos) and IsSpaceChar[Src[StartPos]] do inc(StartPos);
    if StartPos<EndPos then begin
      // there is a comment in front
      CurParam.HeaderCommentPos:=StartPos;
    end;

    // check if the param has a comment behind, but in front of the next separator
    StartPos:=CurParam.GetLastPos(false);
    if CurParam.Separator>0 then
      EndPos:=CurParam.Separator
    else
      EndPos:=t.BracketClosePos;
    while (StartPos<EndPos) and IsSpaceChar[Src[EndPos-1]] do dec(EndPos);
    if StartPos<EndPos then begin
      // there is a comment behind param and in front of the next separator
      CurParam.FooterCommentEndPos:=EndPos;
    end;

    // check if the param has a comment behind the next separator
    if CurParam.Separator>0 then begin
      StartPos:=CurParam.Separator;
      p:=@Src[StartPos];
      while p^ in [' ',#9] do inc(p);
      if (p^='{') or ((p^='(') and (p[1]='*')) or ((p^='/') and (p[1]='/')) then
      begin
        // there is a comment after the separator and it belongs to this param
        StartPos:=p-PChar(Src)+1;
        CurParam.CommentAfterSeparator.StartPos:=StartPos;
        EndPos:=FindCommentEnd(Src,StartPos,Scanner.NestedComments);
        CurParam.CommentAfterSeparator.EndPos:=EndPos;
      end;
    end;

  end;
end;

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
    CDTParseParamList(ParentNode,Transactions);

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

