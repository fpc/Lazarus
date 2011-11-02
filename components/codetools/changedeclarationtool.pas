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
  Classes, SysUtils, AVL_Tree, contnrs, CodeAtom, CodeCache,
  FileProcs, CodeTree, ExtractProcTool, FindDeclarationTool,
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
    function ApplyParamListTransactions(Transactions: TObject;
      SourceChanger: TSourceChangeCache): boolean;
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

  { TChgPrmInsertNew }

  TChgPrmInsertNew = class
  public
    Src: string; // if Src='' then use Modifier+Name+Typ+Value
    Modifier: string;
    Name: string;
    Typ: string;
    DefaultValue: string;
    CopyFromParamIndex: integer;
    constructor Create(aSrc, aModifier, aName, aType, aValue: string;
                       aCopyFrom: integer);
  end;

  { TChgPrmModify }

  TChgPrmModify = class
  public
    Node: TCodeTreeNode; // old param node
    // example: (var buf; {header} a,b:c; d:word=3 {footer}; ...)
    HeaderCommentPos: integer;
    Modifier: TAtomPosition; // optional: const, var out, constref
    Name: TAtomPosition; // name or '...' (MacPas varargs)
    Typ: TAtomPosition; // optional
    DefaultValue: TAtomPosition; // optional
    HasComments: boolean;
    FooterCommentEndPos: integer;
    Separator: integer; // the comma or semicolon to the next parameter
    CommentAfterSeparator: TAtomPosition;
    FirstInGroup: integer; // index of first parameter i a group, e.g. a,b:c
    LastInGroup: integer;

    Delete: boolean;
    ChangeDefaultValue: boolean;
    NewDefaultValue: string;
    InsertBehind: TObjectList;// list of TChgPrmInsertNew
    constructor Create;
    destructor Destroy; override;
    function GetFirstPos: integer;
    function GetLastPos(WithSeparator: boolean): integer;
  end;

  { TChangeParamListTransactions }

  TChangeParamListTransactions = class
  public
    Node: TCodeTreeNode; // ctnParameterList
    OldNodes: array of TChgPrmModify; // one for each old param node
    InsertFirst: TObjectList;// list of TChgPrmInsertNew
    Changes: TObjectList;
    BehindNamePos: integer;
    BracketOpenPos: integer;
    BracketClosePos: integer;
    constructor Create(ParamList: TCodeTreeNode);
    destructor Destroy; override;
    function MaxPos: integer;
    procedure Insert(Index: integer; Insertion: TChgPrmInsertNew);
    procedure CreateChanges;
  end;

{ TChangeParamTransactionInsert }

constructor TChgPrmInsertNew.Create(aSrc, aModifier, aName, aType,
  aValue: string; aCopyFrom: integer);
begin
  Src:=aSrc;
  Modifier:=aModifier;
  Name:=aName;
  Typ:=aType;
  DefaultValue:=aValue;
  CopyFromParamIndex:=aCopyFrom;
end;

constructor TChgPrmModify.Create;
begin
  InsertBehind:=TObjectList.create(true);
end;

destructor TChgPrmModify.Destroy;
begin
  InsertBehind.Free;
  inherited Destroy;
end;

function TChgPrmModify.GetFirstPos: integer;
begin
  if HeaderCommentPos>0 then
    Result:=HeaderCommentPos
  else if Modifier.StartPos>0 then
    Result:=Modifier.StartPos
  else
    Result:=Name.StartPos;
end;

function TChgPrmModify.GetLastPos(WithSeparator: boolean): integer;
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
  else
    Result:=Name.EndPos;
end;

{ TChangeParamListInfos }

function TChangeParamListTransactions.MaxPos: integer;
begin
  Result:=length(OldNodes);
end;

procedure TChangeParamListTransactions.Insert(Index: integer;
  Insertion: TChgPrmInsertNew);
begin
  if Index=0 then
    InsertFirst.Add(Insertion)
  else
    OldNodes[Index-1].InsertBehind.Add(Insertion);
end;

procedure TChangeParamListTransactions.CreateChanges;
var
  i, j: Integer;
begin
  FreeAndNil(Changes);
  Changes:=TObjectList.create(false);
  for i:=0 to InsertFirst.Count-1 do
    Changes.Add(InsertFirst[i]);
  for i:=0 to length(OldNodes)-1 do begin
    Changes.Add(OldNodes[i]);
    for j:=0 to OldNodes[i].InsertBehind.Count-1 do
      Changes.Add(OldNodes[i].InsertBehind[j]);
  end;
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
      OldNodes[i]:=TChgPrmModify.Create;
      OldNodes[i].Node:=ParamNode;
      ParamNode:=ParamNode.NextBrother;
    end;
  end;
end;

destructor TChangeParamListTransactions.Destroy;
var
  i: Integer;
begin
  FreeAndNil(Changes);
  for i:=0 to length(OldNodes)-1 do
    FreeAndNil(OldNodes[i]);
  SetLength(OldNodes,0);
  FreeAndNil(InsertFirst);
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
  CurParam: TChgPrmModify;
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
    or (UpAtomIs('OUT') and (cmsOut in Scanner.CompilerModeSwitches))
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
    raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration kind not supported: '+ParentNode.DescAsString);
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
      CurParam.Name:=CurPos;
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

    // check if the param has a comment inside
    StartPos:=CurParam.GetFirstPos;
    EndPos:=CurParam.GetLastPos(false);
    CurParam.HasComments:=FindNextComment(Src,StartPos,EndPos-1)>=EndPos;

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
      CurParam.HasComments:=true;
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
      CurParam.HasComments:=true;
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

function TChangeDeclarationTool.ApplyParamListTransactions(
  Transactions: TObject; SourceChanger: TSourceChangeCache): boolean;
var
  t: TChangeParamListTransactions;
  InsertCode: String;
  InsertPos: Integer; // behind the last kept parameter
  ReplaceStartPos: Integer;
  ReplaceEndPos: Integer;
  LastChgPos: integer;

  function GetLastChgPos: integer;
  var
    i: Integer;
  begin
    Result:=-1;
    for i:=0 to t.Changes.Count-1 do begin
      if (t.Changes[i] is TChgPrmInsertNew)
      or ((t.Changes[i] is TChgPrmModify)
          and (not TChgPrmModify(t.Changes[i]).Delete))
      then
        Result:=i
    end;
  end;

  function GetOldGroupIndex(ChgPos: integer): integer;
  var
    InsertNew: TChgPrmInsertNew;
  begin
    Result:=-1;
    if (ChgPos<0) or (ChgPos>=t.Changes.Count) then exit;
    if t.Changes[ChgPos] is TChgPrmModify then begin
      Result:=TChgPrmModify(t.Changes[ChgPos]).FirstInGroup;
    end else if t.Changes[ChgPos] is TChgPrmInsertNew then begin
      InsertNew:=TChgPrmInsertNew(t.Changes[ChgPos]);
      if InsertNew.CopyFromParamIndex<0 then exit;
      Result:=t.OldNodes[InsertNew.CopyFromParamIndex].FirstInGroup;
    end;
  end;

  function IsGroupedWithNext(ChgPos: integer): boolean;
  var
    Grp1: integer;
  begin
    Result:=false;
    Grp1:=GetOldGroupIndex(ChgPos);
    if Grp1<0 then exit;
    Result:=Grp1=GetOldGroupIndex(ChgPos+1);
  end;

  function ExtractCode(FromPos, ToPos: integer): string;
  begin
    Result:=copy(Src,FromPos,ToPos-FromPos);
  end;

  procedure InsertParam(Insertion: TChgPrmInsertNew; ChgPos: integer);
  // Insert a new or moved parameter
  var
    SrcParam: TChgPrmModify;
    StartPos: LongInt;
    EndPos: LongInt;
  begin
    if ReplaceStartPos=0 then begin
      ReplaceStartPos:=InsertPos;
      ReplaceEndPos:=ReplaceStartPos;
    end;
    if (InsertCode<>'') and (not (InsertCode[length(InsertCode)] in [';','(','[']))
    then
      InsertCode:=InsertCode+';';
    if Insertion.Src<>'' then
      // add directly
      InsertCode:=InsertCode+Insertion.Src
    else if Insertion.CopyFromParamIndex>=0 then begin
      // copy an existing parameter (the deletion is done by the replace)
      //  Try to copy comments and groups
      //  For example:
      //    var {%H-}a: char = 3; //about a
      //    var a,b,c: word; //comment
      SrcParam:=t.OldNodes[Insertion.CopyFromParamIndex];

      if IsGroupedWithNext(ChgPos-1) then begin
        // grouped with parameter in front: ..., a...
        InsertCode:=InsertCode+',';
        StartPos:=SrcParam.Name.StartPos;
        if SrcParam.Modifier.StartPos<1 then
          StartPos:=SrcParam.GetFirstPos;
        if IsGroupedWithNext(ChgPos) then begin
          // grouped with parameter in front and behind: ..., a, ...
          EndPos:=SrcParam.Name.EndPos;
          if SrcParam.Typ.StartPos<1 then
            EndPos:=SrcParam.GetLastPos(false);
          InsertCode:=InsertCode+ExtractCode(StartPos,EndPos);
        end else begin
          // last parameter in a group: ..., a: word;
          if ChgPos=t.Changes.Count-1 then begin
            // copy without separator
            InsertCode:=InsertCode+ExtractCode(StartPos,SrcParam.GetLastPos(false));
            InsertCode:=InsertCode+ExtractCode(SrcParam.CommentAfterSeparator.StartPos,
               SrcParam.CommentAfterSeparator.EndPos);
          end else begin
            // copy with separator
            InsertCode:=InsertCode+ExtractCode(StartPos,SrcParam.GetLastPos(true));
          end;
        end;
      end else begin
        // not grouped in front
        StartPos:=SrcParam.GetFirstPos;
        if IsGroupedWithNext(ChgPos) then begin
          // first parameter in a group: var a,
          // ToDo: copy comment behind name
          InsertCode:=InsertCode+ExtractCode(StartPos,SrcParam.Name.EndPos);
        end else begin
          // not grouped => copy completely
          if ChgPos=t.Changes.Count-1 then begin
            // copy without separator
            InsertCode:=InsertCode+ExtractCode(StartPos,SrcParam.GetLastPos(false));
            InsertCode:=InsertCode+ExtractCode(SrcParam.CommentAfterSeparator.StartPos,
               SrcParam.CommentAfterSeparator.EndPos);
          end else begin
            // copy with separator
            InsertCode:=InsertCode+ExtractCode(StartPos,SrcParam.GetLastPos(true));
          end;
        end;
      end;
    end else begin
      // new parameter (not copied)
      if Insertion.Modifier<>'' then
        InsertCode:=InsertCode+Insertion.Modifier+' ';
      InsertCode:=InsertCode+Insertion.Name;
      if Insertion.Typ<>'' then
        InsertCode:=InsertCode+':'+Insertion.Typ;
      if Insertion.DefaultValue<>'' then
        InsertCode:=InsertCode+'='+Insertion.DefaultValue;
    end;
  end;

  procedure ChangeParam(aParam: TChgPrmModify; ChgPos: integer);
  var
    Code: String;
    p: LongInt;
  begin
    if aParam.Delete then begin
      if ReplaceStartPos<1 then begin
        // ToDo: delete the last parameter => delete separator from previous parameter
        // ToDo: delete space in front
        ReplaceStartPos:=aParam.GetFirstPos;
      end;
      // extend the deletion range
      ReplaceEndPos:=aParam.GetLastPos(true);
      // ToDo: delete space behind
    end else begin
      // keep this parameter at this place
      if ReplaceStartPos>0 then begin
        // insert the changes in front
        ReplaceEndPos:=aParam.GetFirstPos;
        if not SourceChanger.Replace(gtNone,gtNone,
          ReplaceStartPos,ReplaceEndPos,InsertCode)
        then exit;
        ReplaceStartPos:=0;
        ReplaceEndPos:=0;
        InsertCode:='';
      end;
      if aParam.ChangeDefaultValue then begin
        // keep modifier, name and type and change default value
        if aParam.DefaultValue.StartPos>0 then begin
          // replace the default value
          Code:=aParam.NewDefaultValue;
          if Code<>'' then Code:='='+Code;
          if not SourceChanger.Replace(gtNone,gtNone,
            aParam.DefaultValue.StartPos,aParam.DefaultValue.EndPos,Code)
          then exit;
        end else if aParam.NewDefaultValue<>'' then begin
          // insert a default value
          Code:=':'+aParam.NewDefaultValue;
          p:=aParam.Typ.EndPos;
          if not SourceChanger.Replace(gtNone,gtNone,p,p,Code)
          then exit;
        end;
      end;
    end;
  end;

var
  i: Integer;
begin
  Result:=false;
  t:=Transactions as TChangeParamListTransactions;
  t.CreateChanges;

  LastChgPos:=GetLastChgPos;
  if LastChgPos<0 then begin
    // delete whole param list
    if (t.BracketOpenPos>0) and (t.BracketClosePos>0) then begin
      if not SourceChanger.Replace(gtNone,gtNone,t.BracketOpenPos,t.BracketClosePos+1,'')
      then
        exit;
    end;
    exit(true);
  end;

  InsertCode:='';
  InsertPos:=0;
  ReplaceStartPos:=0;
  ReplaceEndPos:=0;
  if t.BracketOpenPos<1 then begin
    // start a new param list
    if t.Node.Desc=ctnProperty then
      InsertCode:='['
    else
      InsertCode:='(';
    InsertPos:=t.BehindNamePos;
    ReplaceStartPos:=InsertPos;
    ReplaceEndPos:=ReplaceStartPos;
  end else begin
    // keep brackets
    InsertPos:=t.BracketOpenPos+1;
  end;

  for i:=0 to t.Changes.Count-1 do begin
    if t.Changes[i] is TChgPrmInsertNew then
      InsertParam(TChgPrmInsertNew(t.Changes[i]),i)
    else if t.Changes[i] is TChgPrmModify then
      ChangeParam(TChgPrmModify(t.Changes[i]),i);
  end;

  if t.BracketOpenPos<1 then begin
    // end a new param list
    if t.Node.Desc=ctnProperty then
      InsertCode:=InsertCode+']'
    else
      InsertCode:=InsertCode+')';
  end;

  if ReplaceStartPos>0 then
    if not SourceChanger.Replace(gtNone,gtNone,ReplaceStartPos,ReplaceEndPos,InsertCode)
    then
      exit;
  Result:=true;
end;

function TChangeDeclarationTool.ChangeParamListDeclaration(
  ParentNode: TCodeTreeNode; Changes: TObjectList;
  SourceChanger: TSourceChangeCache): boolean;
var
  FoundVarArgs: Boolean;
  FoundDefaultValue: boolean;
  Transactions: TChangeParamListTransactions;

  procedure CheckInsert(Insertion: TChgPrmInsertNew);
  var
    SrcParam: TChgPrmModify;
    HasDefaultValue: Boolean;
  begin
    // check that '...' (MacPas vararg) is last
    // check that after a parameter with default value all have default values
    if FoundVarArgs then
      raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: ... parameter must be the last');
    if Insertion.CopyFromParamIndex>=0 then begin
      SrcParam:=Transactions.OldNodes[Insertion.CopyFromParamIndex];
      if GetAtom(SrcParam.Name)='...' then
        FoundVarArgs:=true;
      if SrcParam.ChangeDefaultValue then
        HasDefaultValue:=SrcParam.NewDefaultValue<>''
      else
        HasDefaultValue:=SrcParam.DefaultValue.StartPos>0;
    end else begin
      if (Insertion.Name='...') then
        FoundVarArgs:=true;
      HasDefaultValue:=Insertion.DefaultValue<>'';
    end;
    if HasDefaultValue then
      FoundDefaultValue:=true
    else if FoundDefaultValue then
      raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: after a parameter with default value all parameters must have default values');
  end;

  procedure CheckParam(aParam: TChgPrmModify);
  var
    i: Integer;
    HasDefaultValue: Boolean;
  begin
    if not aParam.Delete then begin
      // check that '...' (MacPas vararg) is last
      if FoundVarArgs then
        raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: ... parameter must be the last');
      if GetAtom(aParam.Name)='...' then
        FoundVarArgs:=true;
      if not aParam.Delete then begin
        if aParam.ChangeDefaultValue then
          HasDefaultValue:=aParam.NewDefaultValue<>''
        else
          HasDefaultValue:=aParam.DefaultValue.StartPos>0;
        if HasDefaultValue then
          FoundDefaultValue:=true
        else if FoundDefaultValue then
          raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: after a parameter with default value all parameters must have default values');
      end;
    end;
    for i:=0 to aParam.InsertBehind.Count-1 do
      CheckInsert(TChgPrmInsertNew(aParam.InsertBehind[i]));
  end;

var
  ParamListNode: TCodeTreeNode;
  i: Integer;
  Change: TChangeParamListItem;
  Transaction: TChgPrmModify;
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
        raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: index '+dbgs(Change.Index)+' out of bounds');
      case Change.Action of
      cplaInsertNewParam:
        Transactions.Insert(Change.Index,
          TChgPrmInsertNew.Create('',Change.ParamModifier,
                     Change.ParamName,Change.ParamType,Change.DefaultValue,-1));

      cplaDeleteParam:
        begin
          Transaction:=Transactions.OldNodes[Change.Index];
          if Transaction.Delete then
            raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: index '+dbgs(Change.Index)+' already deleted');
          Transaction.Delete:=true;
        end;

      cplaMoveParam:
        begin
          if (Change.OldIndex<0) or (Change.OldIndex>Transactions.MaxPos) then
            raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: index out of bounds');
          if Change.OldIndex<>Change.Index then begin
            Transaction:=Transactions.OldNodes[Change.OldIndex];
            if Transaction.Delete then
              raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: index '+dbgs(Change.OldIndex)+' already deleted');
            Transaction.Delete:=true;
            Transactions.Insert(Change.Index,
              TChgPrmInsertNew.Create('','','','','',Change.OldIndex));
          end;
        end;

      cplaChangeDefaultValue:
        begin
          Transaction:=Transactions.OldNodes[Change.Index];
          if Transaction.Typ.StartPos<1 then
            raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: can not change the default value, because index '+dbgs(Change.Index)+' has no type');
          if Transaction.Delete then
            raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: index '+dbgs(Change.Index)+' already deleted');
          if Transaction.ChangeDefaultValue then
            raise EInvalidOperation.Create('TChangeDeclarationTool.ChangeParamListDeclaration: index '+dbgs(Change.Index)+' default value already changed');

          Transaction.ChangeDefaultValue:=true;
          Transaction.NewDefaultValue:=Change.DefaultValue;
        end;

      end;
    end;

    FoundVarArgs:=false;
    FoundDefaultValue:=false;
    for i:=0 to Transactions.InsertFirst.Count-1 do
      CheckInsert(TChgPrmInsertNew(Transactions.InsertFirst[i]));
    for i:=0 to Transactions.MaxPos-1 do
      CheckParam(Transactions.OldNodes[i]);

    // apply
    Result:=ApplyParamListTransactions(Transactions,SourceChanger);
  finally
    Transactions.Free;
  end;
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

