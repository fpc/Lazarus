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
    TExtractProcTool enhances TCodeCompletionCodeTool.
    TExtractProcTool provides functions to extract statements from procedures
    and to move them to new procedure, sub procedures or methods. Parameter
    list is auto created and local variables are automatically created and/or
    removed.
    Note: Extracting a procedure from a method needs manual fixing of used
    method variables.

  ToDo:
    - check if selection bounds on statement bounds
    - heuristic for parameter specifiers 'var'
    - with statements
}
unit ExtractProcTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeToolsStrConsts, CodeTree, CodeAtom, PascalParserTool,
  CodeCompletionTool, KeywordFuncLists, BasicCodeTools, LinkScanner, AVL_Tree,
  SourceChanger, FindDeclarationTool;
  
type
  { TExtractProcTool }
  
  TExtractProcType = (
    eptProcedure,
    eptProcedureWithInterface,
    eptSubProcedure,
    eptSubProcedureSameLvl,
    eptPrivateMethod,
    eptProtectedMethod,
    eptPublicMethod,
    eptPublishedMethod
    );

  TExtractProcTool = class(TCodeCompletionCodeTool)
  public
    function CheckExtractProc(const StartPos, EndPos: TCodeXYPosition;
      var MethodPossible, SubProcSameLvlPossible: boolean): boolean;
    function ExtractProc(const StartPos, EndPos: TCodeXYPosition;
      ProcType: TExtractProcType; const ProcName: string;
      var NewPos: TCodeXYPosition; var NewTopLine: integer;
      SourceChangeCache: TSourceChangeCache): boolean;
  end;
  
const
  ExtractProcTypeNames: array[TExtractProcType] of string = (
    'Procedure',
    'ProcedureWithInterface',
    'SubProcedure',
    'SubProcedureSameLvl',
    'PrivateMethod',
    'ProtectedMethod',
    'PublicMethod',
    'PublishedMethod'
    );

implementation

type
  TExtractedProcVariableType = (
    epvtParameter,
    epvtLocalVar
    );

  TExtractedProcVariable = class
  public
    Node: TCodeTreeNode;
    VarType: TExtractedProcVariableType;
    UsedInSelection: boolean;
    UsedInNonSelection: boolean;
    RemovedFromOldProc: boolean;
  end;
  
function CompareExtractedProcVariables(V1, V2: TExtractedProcVariable): integer;
var
  cmp: Integer;
begin
  cmp:=V2.Node.StartPos-V1.Node.StartPos;
  if cmp<0 then
    Result:=-1
  else if cmp>0 then
    Result:=1
  else
    Result:=0;
end;

function CompareNodeWithExtractedProcVariable(Node: TCodeTreeNode;
  V: TExtractedProcVariable): integer;
var
  cmp: Integer;
begin
  cmp:=V.Node.StartPos-Node.StartPos;
  if cmp<0 then
    Result:=-1
  else if cmp>0 then
    Result:=1
  else
    Result:=0;
end;

{ TExtractProcTool }

function TExtractProcTool.CheckExtractProc(const StartPos,
  EndPos: TCodeXYPosition;
  var MethodPossible, SubProcSameLvlPossible: boolean): boolean;
var
  CleanStartPos, CleanEndPos: integer;
  CursorNode: TCodeTreeNode;
  BlockCleanStart: Integer;
  BeginBlockNode: TCodeTreeNode;
  BlockCleanEnd: Integer;
  ANode: TCodeTreeNode;
  ProcLvl: Integer;
begin
  Result:=false;
  MethodPossible:=false;
  SubProcSameLvlPossible:=false;
  {$IFDEF CTDebug}
  writeln('TExtractProcTool.CheckExtractProc syntax and cursor check ..');
  {$ENDIF}
  // check syntax
  BuildTreeAndGetCleanPos(trAll,StartPos,CleanStartPos,[]);
  if CaretToCleanPos(EndPos,CleanEndPos)<>0 then exit;
  if CleanStartPos>=CleanEndPos then exit;
  {$IFDEF CTDebug}
  writeln('TExtractProcTool.CheckExtractProc node check ..');
  {$ENDIF}
  // check if in a Begin..End block
  CursorNode:=FindDeepestNodeAtPos(CleanStartPos,true);
  if CursorNode=nil then exit;
  BeginBlockNode:=CursorNode.GetNodeOfType(ctnBeginBlock);
  if BeginBlockNode=nil then exit;
  {$IFDEF CTDebug}
  writeln('TExtractProcTool.CheckExtractProc Start/End check ..');
  {$ENDIF}
  // check if Start and End on same block level
  MoveCursorToNodeStart(CursorNode);
  BlockCleanStart:=CurPos.StartPos;
  while true do begin
    ReadNextAtom;
    if (CurPos.StartPos>SrcLen) or (CurPos.StartPos>CursorNode.EndPos)
    or (CurPos.StartPos>CleanStartPos) then
      break;
    if WordIsLogicalBlockStart.DoItUpperCase(UpperSrc,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then
      BlockCleanStart:=CurPos.StartPos;
  end;
  MoveCursorToCleanPos(BlockCleanStart);
  ReadNextAtom;
  if not ReadTilBlockEnd(true,false) then exit;
  BlockCleanEnd:=CurPos.EndPos;
  if BlockCleanEnd<CleanEndPos then exit;
  // check if start not in a statement
  // ToDo
  // check if end not in a statement
  // ToDo
  {$IFDEF CTDebug}
  writeln('TExtractProcTool.CheckExtractProc Method check ..');
  {$ENDIF}
  // check if in a method body
  ANode:=CursorNode;
  ProcLvl:=0;
  while ANode<>nil do begin
    if (ANode.Desc=ctnProcedure) then begin
      inc(ProcLvl);
      if NodeIsInAMethod(ANode) then begin
        MethodPossible:=true;
      end;
    end;
    ANode:=ANode.Parent;
  end;
  SubProcSameLvlPossible:=(ProcLvl>1);
  {$IFDEF CTDebug}
  writeln('TExtractProcTool.CheckExtractProc END');
  {$ENDIF}
  Result:=true;
end;

function TExtractProcTool.ExtractProc(const StartPos, EndPos: TCodeXYPosition;
  ProcType: TExtractProcType; const ProcName: string;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
const
  ShortProcFormat = [phpWithoutClassKeyword];
var
  BlockStartPos, BlockEndPos: integer; // the selection
  ProcNode: TCodeTreeNode; // the main proc node of the selection
  VarTree: TAVLTree;
  
  procedure AddVariableToTree(VarNode: TCodeTreeNode; IsInSelection,
    IsParameter: boolean);
  var
    NewProcVar: TExtractedProcVariable;
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
    UsedInNonSelection: Boolean;
  begin
    {$IFDEF CTDebug}
    writeln('AddVariableToTree A IsInSelection=',IsInSelection,' IsParameter=',IsParameter);
    {$ENDIF}
    UsedInNonSelection:=(not IsInSelection) or IsParameter;
    if VarTree=nil then
      VarTree:=TAVLTree.Create(@CompareExtractedProcVariables);
    AVLNode:=VarTree.FindKey(VarNode,@CompareNodeWithExtractedProcVariable);
    if AVLNode<>nil then begin
      ProcVar:=TExtractedProcVariable(AVLNode.Data);
      ProcVar.UsedInSelection:=ProcVar.UsedInSelection or IsInSelection;
      ProcVar.UsedInNonSelection:=ProcVar.UsedInSelection or UsedInNonSelection;
    end else begin
      NewProcVar:=TExtractedProcVariable.Create;
      NewProcVar.Node:=VarNode;
      NewProcVar.UsedInSelection:=IsInSelection;
      NewProcVar.UsedInNonSelection:=UsedInNonSelection;
      if IsParameter then
        NewProcVar.VarType:=epvtParameter
      else
        NewProcVar.VarType:=epvtLocalVar;
      VarTree.Add(NewProcVar);
    end;
  end;

  function CheckVariableAtCursor: boolean;
  // find declaration of identifier at cursor and add to variable tree
  var
    Params: TFindDeclarationParams;
    VarStartPos: Integer;
    VarNode: TCodeTreeNode;
    IsInSelection: Boolean;
    ClosestProcNode: TCodeTreeNode;
    IsParameter: boolean;
  begin
    Result:=false;
    // find start of variable
    VarStartPos:=FindStartOfVariable(CurPos.StartPos);
    IsInSelection:=(VarStartPos>=BlockStartPos) and (VarStartPos<BlockEndPos);
    MoveCursorToCleanPos(VarStartPos);
    Params:=TFindDeclarationParams.Create;
    try
      // find declaration
      Params.ContextNode:=FindDeepestNodeAtPos(VarStartPos,true);
      Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound,
                     fdfTopLvlResolving,fdfSearchInAncestors];
      // ToDo: Params.OnTopLvlIdentifierFound:=@OnTopLvlIdentifierFound;
      Params.SetIdentifier(Self,@Src[VarStartPos],@CheckSrcIdentifier);
      {$IFDEF CTDebug}
      writeln('AddVariableAtCursor A ',GetIdentifier(Params.Identifier));
      {$ENDIF}
      if not FindDeclarationOfIdentAtCursor(Params) then begin
        {$IFDEF CTDebug}
        writeln('AddVariableAtCursor B found');
        {$ENDIF}
        exit;
      end;
      // check if declaration is local variable
      if (Params.NewCodeTool=Self) and (Params.NewNode<>nil) then begin
        VarNode:=Params.NewNode;
        if (VarNode.Desc=ctnVarDefinition)
        and (VarNode.HasAsParent(ProcNode)) then begin
          // Now we know: VarNode is a variable defined in the main proc
          // or one of its sub procs
          ClosestProcNode:=VarNode.GetNodeOfType(ctnProcedure);
          if ClosestProcNode=ProcNode then begin
            // VarNode is a variable defined by the main proc
            IsParameter:=VarNode.GetNodeOfType(ctnProcedureHead)<>nil;
            AddVariableToTree(VarNode,IsInSelection,IsParameter);
          end;
        end;
      end;
    finally
      Params.Free;
    end;
    Result:=true;
  end;

  function ScanSourceForVariables(CleanStartPos, CleanEndPos: integer): boolean;
  // scan part of the source for variables
  var
    LastAtomType: TCommonAtomFlag;
    OldCursor: Integer;
  begin
    Result:=false;
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.ScanSourceForVariables A "',copy(Src,CleanStartPos,CleanEndPos-CleanStartPos),'"');
    {$ENDIF}
    MoveCursorToNearestAtom(CleanStartPos);
    while CurPos.StartPos<CleanEndPos do begin
      LastAtomType:=CurPos.Flag;
      ReadNextAtom;
      if AtomIsIdentifier(false) and (LastAtomType<>cafPoint) then begin
        // this could be the start of a variable -> check
        {$IFDEF CTDebug}
        writeln('ScanSourceForVariables B Identifier=',GetAtom);
        {$ENDIF}
        OldCursor:=CurPos.StartPos;
        if not CheckVariableAtCursor then exit;
        // restore cursor
        MoveCursorToCleanPos(OldCursor);
        ReadNextAtom;
      end;
    end;
    Result:=true;
  end;
  
  function ScanNodesForVariables(StartNode: TCodeTreeNode): boolean;
  // scan recursively all statements for variables
  var
    ChildNode: TCodeTreeNode;
  begin
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.ScanNodesForVariables A ',StartNode.DescAsString);
    {$ENDIF}
    Result:=false;
    ChildNode:=StartNode.FirstChild;
    while ChildNode<>nil do begin
      if (ChildNode.Desc in [ctnBeginBlock,ctnAsmBlock])
      and (ChildNode.Parent.Desc=ctnProcedure) then begin
        if not ScanSourceForVariables(ChildNode.StartPos,ChildNode.EndPos) then
          exit;
      end;
      if not ScanNodesForVariables(ChildNode) then exit;
      ChildNode:=ChildNode.NextBrother;
    end;
    Result:=true;
  end;
  
  function ReplaceSelectionWithCall: boolean;
  var
    Indent: Integer;
    CallCode: String;
    ParamListCode: String;
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
  begin
    Result:=false;
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.ReplaceSelectionWithCall A');
    {$ENDIF}
    Indent:=GetLineIndent(Src,BlockStartPos);
    ParamListCode:='';
    // gather all variables, that are used in the selection and in the rest of
    // the old proc. These are the parameters for the new proc.
    if (VarTree<>nil) and (ProcType<>eptSubProcedure) then begin
      AVLNode:=VarTree.FindLowest;
      while AVLNode<>nil do begin
        ProcVar:=TExtractedProcVariable(AVLNode.Data);
        {$IFDEF CTDebug}
        writeln('TExtractProcTool.ReplaceSelectionWithCall B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' UsedInSelection=',ProcVar.UsedInSelection,
          ' UsedInNonSelection=',ProcVar.UsedInNonSelection);
        {$ENDIF}
        if ProcVar.UsedInSelection and ProcVar.UsedInNonSelection then begin
          // variables
          if ParamListCode<>'' then ParamListCode:=ParamListCode+',';
          ParamListCode:=ParamListCode+GetIdentifier(@Src[ProcVar.Node.StartPos]);
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    if ParamListCode<>'' then
      ParamListCode:='('+ParamListCode+')';
    CallCode:=ProcName+ParamListCode+';';
    CallCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                                                               CallCode,Indent);
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.ReplaceSelectionWithCall C "',CallCode,'" Indent=',Indent);
    {$ENDIF}
    SourceChangeCache.Replace(gtNewLine,gtNewLine,BlockStartPos,BlockEndPos,
                              CallCode);
    Result:=true;
  end;
  
  function DeleteLocalVariable(ProcVar: TExtractedProcVariable): boolean;
  
    function VariableNodeShouldBeDeleted(VarNode: TCodeTreeNode;
      var CurProcVar: TExtractedProcVariable): boolean;
    var
      AVLNode: TAVLTreeNode;
    begin
      CurProcVar:=nil;
      AVLNode:=VarTree.FindKey(VarNode,@CompareNodeWithExtractedProcVariable);
      if AVLNode=nil then begin
        Result:=false;
      end else begin
        CurProcVar:=TExtractedProcVariable(AVLNode.Data);
        if (not CurProcVar.UsedInSelection)
        or (CurProcVar.UsedInNonSelection) then begin
          Result:=false;
        end else begin
          Result:=true;
        end;
      end;
    end;
  
  var
    VarNode: TCodeTreeNode;
    FirstVarNode: TCodeTreeNode;
    LastVarNode: TCodeTreeNode;
    DeleteCompleteDefinition: Boolean;
    DeleteStartPos: Integer;
    DeleteEndPos: Integer;
    CurProcVar: TExtractedProcVariable;
  begin
    Result:=false;
    if not ProcVar.RemovedFromOldProc then begin
      // check all variables of the definition (e.g. 'i,j,k: integer')
      FirstVarNode:=ProcVar.Node;
      while (FirstVarNode.PriorBrother<>nil)
      and (FirstVarNode.PriorBrother.Desc=ctnVarDefinition)
      and (FirstVarNode.PriorBrother.FirstChild=nil) do
        FirstVarNode:=FirstVarNode.PriorBrother;
      LastVarNode:=FirstVarNode;
      while (LastVarNode.NextBrother<>nil)
      and (LastVarNode.NextBrother.Desc=ctnVarDefinition)
      and (LastVarNode.FirstChild=nil) do
        LastVarNode:=LastVarNode.NextBrother;
      VarNode:=FirstVarNode;
      // delete variables
      DeleteCompleteDefinition:=true;
      DeleteStartPos:=0;
      DeleteEndPos:=0;
      repeat
        if VariableNodeShouldBeDeleted(VarNode,CurProcVar) then begin
          // delete variable name and comma
          // if the whole definition is deleted, this is handled behind the
          // loop. Examples:
          //   var i, X: integer;     ->  var i[, X]: integer;
          //   var i, X, j: integer;  ->  var i, [X, ]j: integer;
          //   var X, i: integer;     ->  var [X, ]i: integer;
          if DeleteStartPos<1 then
            DeleteStartPos:=VarNode.StartPos;
          MoveCursorToNodeStart(VarNode);
          ReadNextAtom;
          AtomIsIdentifier(true);
          ReadNextAtom;
          if CurPos.Flag=cafComma then begin
            // there is a next variable in the same var definition
            ReadNextAtom;
            DeleteEndPos:=CurPos.StartPos;
          end else if CurPos.Flag=cafColon then begin
            // this is the last variable in the definition
            DeleteEndPos:=CurPos.StartPos;
            if (DeleteStartPos=VarNode.StartPos)
            and (VarNode.PriorBrother<>nil)
            and (VarNode.PriorBrother.Desc=ctnVarDefinition)
            and (VarNode.PriorBrother.FirstChild=nil) then begin
              // there is a variable in front in the same definition, that is
              // not deleted. Delete also the comma in front. Example:
              //   var i, X: integer;   ->  var i[, X]: integer;
              MoveCursorToNodeStart(VarNode.PriorBrother);
              ReadNextAtom; // prior identifier
              ReadNextAtom; // comma
              DeleteStartPos:=CurPos.StartPos;
            end;
          end;
          // mark as removed
          CurProcVar.RemovedFromOldProc:=true;
        end else begin
          // this variable is kept
          DeleteCompleteDefinition:=false;
          if DeleteStartPos>0 then begin
            // delete variables in front
            {$IFDEF CTDebug}
            writeln('DeleteLocalVariable Delete last vars: "',copy(Src,DeleteStartPos,DeleteEndPos-DeleteStartPos),'"');
            {$ENDIF}
            if not SourceChangeCache.Replace(gtNone,gtNone,
                                             DeleteStartPos,DeleteEndPos,'')
            then
              exit;
            DeleteStartPos:=0;
            DeleteEndPos:=0;
          end;
        end;
        if VarNode=LastVarNode then break;
        VarNode:=VarNode.NextBrother;
      until VarNode=nil;
      if DeleteCompleteDefinition and (DeleteStartPos>0) then begin
        // all variables of the definition should be deleted
        // -> delete type declaration
        DeleteEndPos:=FindLineEndOrCodeAfterPosition(LastVarNode.EndPos);
        if (FirstVarNode.PriorBrother=nil)
        and (LastVarNode.NextBrother=nil) then begin
          // all variables of the 'var' section are deleted
          // -> delete var section
          DeleteStartPos:=FirstVarNode.Parent.StartPos;
        end;
        DeleteStartPos:=FindLineEndOrCodeInFrontOfPosition(DeleteStartPos,true);
      end;
      if DeleteStartPos>0 then begin
        {$IFDEF CTDebug}
        writeln('DeleteLocalVariable Delete Rest: "',copy(Src,DeleteStartPos,DeleteEndPos-DeleteStartPos),'"');
        {$ENDIF}
        if not SourceChangeCache.Replace(gtNone,gtNone,
                                         DeleteStartPos,DeleteEndPos,'')
        then
          exit;
      end;
    end;
    Result:=true;
  end;
  
  function DeleteMovedLocalVariables: boolean;
  var
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
  begin
    Result:=false;
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.DeleteMovedLocalVariables A');
    {$ENDIF}
    // gather all variables, that are used in the selection, but not in the
    // rest of the old proc. These are local variables, that are moved to the
    // new proc.
    if (VarTree<>nil) then begin
      AVLNode:=VarTree.FindLowest;
      while AVLNode<>nil do begin
        ProcVar:=TExtractedProcVariable(AVLNode.Data);
        {$IFDEF CTDebug}
        writeln('TExtractProcTool.DeleteMovedLocalVariables B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' UsedInSelection=',ProcVar.UsedInSelection,
          ' UsedInNonSelection=',ProcVar.UsedInNonSelection);
        {$ENDIF}
        if ProcVar.UsedInSelection and (not ProcVar.UsedInNonSelection) then
        begin
          if not DeleteLocalVariable(ProcVar) then exit;
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    {$IFDEF CTDebug}
    writeln('DeleteMovedLocalVariables END ');
    {$ENDIF}
    Result:=true;
  end;
  
  function CreateProcNameParts(var ProcClassName: string;
    var ProcClassNode: TCodeTreeNode): boolean;
  begin
    Result:=false;
    ProcClassName:='';
    if ProcType in [eptPrivateMethod,eptProtectedMethod,eptPublicMethod,
      eptPublishedMethod] then
    begin
      {$IFDEF CTDebug}
      writeln('CreateProcNameParts A searching class name ..');
      {$ENDIF}
      ProcClassName:=ExtractClassNameOfProcNode(ProcNode);
      if ProcClassName='' then exit;
      ProcClassNode:=FindClassNodeInUnit(UpperCaseStr(ProcClassName),
                                         true,false,true);
      if ProcClassNode=nil then exit;
      ProcClassName:=ExtractClassName(ProcClassNode,false);
    end;
    {$IFDEF CTDebug}
    writeln('CreateProcNameParts END ProcClassName="',ProcClassName,'"');
    {$ENDIF}
    Result:=true;
  end;

  function CreateProcParamList(var CompleteParamListCode,
    BaseParamListCode: string): boolean;
  var
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
    ParamName: String;
    ParamTypeCode: String;
    ParamSpecifier: String;
  begin
    Result:=false;
    CompleteParamListCode:='';
    BaseParamListCode:='';
    // gather all variables, that are used in the selection and in the rest of
    // the old proc. These are the parameters for the new proc.
    if (VarTree<>nil) and (ProcType<>eptSubProcedure) then begin
      AVLNode:=VarTree.FindLowest;
      while AVLNode<>nil do begin
        ProcVar:=TExtractedProcVariable(AVLNode.Data);
        {$IFDEF CTDebug}
        writeln('TExtractProcTool.CreateProcParamList B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' UsedInSelection=',ProcVar.UsedInSelection,
          ' UsedInNonSelection=',ProcVar.UsedInNonSelection);
        {$ENDIF}
        if ProcVar.UsedInSelection and ProcVar.UsedInNonSelection then begin
          // extract identifier and type
          if CompleteParamListCode<>'' then
            CompleteParamListCode:=CompleteParamListCode+';';
          if BaseParamListCode<>'' then
            BaseParamListCode:=BaseParamListCode+';';
          ParamName:=GetIdentifier(@Src[ProcVar.Node.StartPos]);
          ParamTypeCode:=ExtractDefinitionNodeType(ProcVar.Node);
          {$IFDEF CTDebug}
          writeln('TExtractProcTool.CreateProcParamList C ParamName="',ParamName,'" ParamType="',ParamTypeCode,'"');
          {$ENDIF}
          // ToDo: ParamSpecifier 'var ' and none
          ParamSpecifier:='const ';
          CompleteParamListCode:=CompleteParamListCode
                                 +ParamSpecifier+ParamName+':'+ParamTypeCode;
          BaseParamListCode:=BaseParamListCode+':'+ParamTypeCode;
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    if CompleteParamListCode<>'' then begin
      CompleteParamListCode:='('+CompleteParamListCode+')';
      BaseParamListCode:='('+BaseParamListCode+')';
    end;
    {$IFDEF CTDebug}
    writeln('CreateProcParamList END CompleteParamListCode="',CompleteParamListCode,'"');
    {$ENDIF}
    Result:=true;
  end;
  
  function CreateProcVarSection(var VarSectionCode: string): boolean;
  var
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
    VariableName: String;
    VariableTypeCode: String;
    VarTypeNode: TCodeTreeNode;
    TypeDefEndPos: Integer;
  begin
    Result:=false;
    VarSectionCode:='';
    // gather all variables, that are used in the selection, but not in the
    // rest of the old proc. These are the local variables of the new proc.
    if (VarTree<>nil) then begin
      AVLNode:=VarTree.FindLowest;
      while AVLNode<>nil do begin
        ProcVar:=TExtractedProcVariable(AVLNode.Data);
        {$IFDEF CTDebug}
        writeln('TExtractProcTool.CreateProcVarSection B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' UsedInSelection=',ProcVar.UsedInSelection,
          ' UsedInNonSelection=',ProcVar.UsedInNonSelection);
        {$ENDIF}
        if ProcVar.UsedInSelection and (not ProcVar.UsedInNonSelection) then
        begin
          // extract identifier and type
          if VarSectionCode='' then
            VarSectionCode:='var'
              +SourceChangeCache.BeautifyCodeOptions.LineEnd;
          VarSectionCode:=VarSectionCode
            +GetIndentStr(SourceChangeCache.BeautifyCodeOptions.Indent);
          VariableName:=GetIdentifier(@Src[ProcVar.Node.StartPos]);
          VarTypeNode:=FindTypeNodeOfDefinition(ProcVar.Node);
          {$IFDEF CTDebug}
          writeln('AAA1 VarTypeNode=',copy(Src,VarTypeNode.StartPos,VarTypeNode.EndPos-VarTypeNode.StartPos));
          {$ENDIF}
          TypeDefEndPos:=FindLineEndOrCodeAfterPosition(VarTypeNode.EndPos);
          {$IFDEF CTDebug}
          writeln('AAA2 PlusComment=',copy(Src,VarTypeNode.StartPos,TypeDefEndPos-VarTypeNode.StartPos));
          {$ENDIF}
          VariableTypeCode:=copy(Src,VarTypeNode.StartPos,
                                 TypeDefEndPos-VarTypeNode.StartPos);
          {$IFDEF CTDebug}
          writeln('TExtractProcTool.CreateProcVarSection C VariableName="',VariableName,'" VariableType="',VariableTypeCode,'"');
          {$ENDIF}
          VarSectionCode:=VarSectionCode+VariableName+':'+VariableTypeCode
                          +SourceChangeCache.BeautifyCodeOptions.LineEnd;
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.CreateProcVarSection END VarSectionCode="',VarSectionCode,'"');
    {$ENDIF}
    VarSectionCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                                                               VarSectionCode,0);
    Result:=true;
  end;
  
  function CreateProcBeginEndBlock(var BeginEndCode: string): boolean;
  var
    DirtyStartPos, DirtyEndPos: integer;
    le, s: String;
    Indent: Integer;
    DirtySelection: String;
  begin
    Result:=false;
    le:=SourceChangeCache.BeautifyCodeOptions.LineEnd;
    // extract dirty source, so that compiler directives are moved.
    StartPos.Code.LineColToPosition(StartPos.Y,StartPos.X,DirtyStartPos);
    StartPos.Code.LineColToPosition(EndPos.Y,EndPos.X,DirtyEndPos);
    DirtySelection:=copy(StartPos.Code.Source,
                         DirtyStartPos,DirtyEndPos-DirtyStartPos);
    // append line end
    if (DirtySelection<>'')
    and (not (DirtySelection[length(DirtySelection)] in [#10,#13])) then
      DirtySelection:=DirtySelection+le;
    // trim empty lines at start and end
    DirtySelection:=TrimLineEnds(DirtySelection,true,true);
    // adjust indent
    Indent:=GetBlockMinIndent(DirtySelection,1,length(DirtySelection));
    IndentText(DirtySelection,
               SourceChangeCache.BeautifyCodeOptions.Indent-Indent,
               SourceChangeCache.BeautifyCodeOptions.TabWidth,
               s);
    DirtySelection:=s;
    // create Begin..End block
    BeginEndCode:='begin'+le
                  +DirtySelection
                  +'end;';
    {$IFDEF CTDebug}
    writeln('TExtractProcTool.CreateProcBeginEndBlock END BeginEndCode="',BeginEndCode,'"');
    {$ENDIF}
    Result:=true;
  end;
  
  function FindInsertPositionForProcBody(
    var InsertPos, Indent: integer): boolean;
  var
    BeginNode: TCodeTreeNode;
    ANode: TCodeTreeNode;
    InsertNode: TCodeTreeNode;
  begin
    Result:=false;
    case ProcType of
    
    eptSubProcedure:
      begin
        BeginNode:=ProcNode.LastChild;
        while BeginNode.Desc<>ctnBeginBlock do
          BeginNode:=BeginNode.PriorBrother;
        InsertPos:=BeginNode.StartPos;
        Indent:=GetLineIndent(Src,InsertPos)
                +SourceChangeCache.BeautifyCodeOptions.Indent;
      end;
      
    eptSubProcedureSameLvl:
      begin
        // -> insert in front of old proc
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(ProcNode.StartPos);
        Indent:=GetLineIndent(Src,ProcNode.StartPos);
      end;

    eptProcedure,eptProcedureWithInterface:
      begin
        // insert in front of top level proc
        InsertNode:=ProcNode;
        ANode:=InsertNode;
        while (ANode<>nil) do begin
          if ANode.Desc=ctnProcedure then
            InsertNode:=ANode;
          ANode:=ANode.Parent;
        end;
        if NodeIsMethodBody(InsertNode) then begin
          // insert in front of all methods
          while (InsertNode.PriorBrother<>nil)
          and (InsertNode.PriorBrother.Desc=ctnProcedure)
          and (NodeIsMethodBody(InsertNode)) do
            InsertNode:=InsertNode.PriorBrother;
        end;
        // -> insert in front of top level proc
        Indent:=GetLineIndent(Src,InsertNode.StartPos);
        if InsertNode.PriorBrother<>nil then begin
          InsertPos:=FindLineEndOrCodeAfterPosition(
                                                InsertNode.PriorBrother.EndPos);
        end else if InsertNode.Parent.Desc=ctnImplementation then begin
          MoveCursorToNodeStart(InsertNode.Parent);
          ReadNextAtom;
          InsertPos:=FindLineEndOrCodeAfterPosition(CurPos.EndPos);
        end else begin
          InsertPos:=FindLineEndOrCodeInFrontOfPosition(InsertNode.StartPos,true);
        end;
      end;
      
    eptPublishedMethod,eptPrivateMethod,eptProtectedMethod,eptPublicMethod:
      begin
        // set default values
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(ProcNode.StartPos);
        Indent:=GetLineIndent(Src,ProcNode.StartPos);
      end;

    else
      exit;
    end;
    Result:=true;
  end;

  function FindInsertPositionForProcIntf(
    var IntfInsertPos, IntfIndent: integer): boolean;
  begin
    Result:=false;
    IntfInsertPos:=0;
    IntfIndent:=0;
    case ProcType of
    
    eptProcedureWithInterface:
      begin
        FindInsertPositionForProcInterface(IntfIndent,IntfInsertPos,
                                           SourceChangeCache);
      end;
      
    end;
    
    Result:=true;
  end;

  function NewProcAlreadyExists(const ProcClassName, BaseParamList: string;
    InsertPos: integer): boolean;
  var
    ContextNode: TCodeTreeNode;
    ConflictProcNode: TCodeTreeNode;
    ProcHead: String;
  begin
    // find context at insert position
    ContextNode:=FindDeepestNodeAtPos(InsertPos,true);
    if (ContextNode.Parent<>nil) then
      ContextNode:=ContextNode.FirstChild;
    // search proc in context
    if ProcClassName<>'' then
      ProcHead:=ProcClassName+'.'
    else
      ProcHead:='';
    ProcHead:=ProcHead+ProcName+BaseParamList;
    ConflictProcNode:=FindProcNode(ContextNode,ProcHead,
                                   ShortProcFormat+[phpIgnoreForwards]);
    Result:=ConflictProcNode<>nil;
    if Result then begin
      RaiseException('New procedure "'+ProcName+'" exists already');
    end;
    {$IFDEF CTDebug}
    writeln('NewProcAlreadExists END ProcHead="',ProcHead,'" Found=',Result);
    {$ENDIF}
  end;

  function InsertProcIntf(IntfInsertPos, IntfIndent: integer;
    const CompleteParamList, BaseParamList, ProcCode: string;
    const ProcClassName: string; ProcClassNode: TCodeTreeNode): boolean;
  var
    ProcHeader: String;
    FrontGap: TGapTyp;
    AfterGap: TGapTyp;
    InsertNode: TCodeTreeNode;
    MethodDefinition: String;
    CleanMethodDefinition: String;
    NewClassPart: TNewClassPart;
  begin
    Result:=false;
    case ProcType of
    
    eptProcedureWithInterface:
      begin
        ProcHeader:='procedure '+ProcName+CompleteParamList+';';
        ProcHeader:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
          ProcHeader,IntfIndent);
        {$IFDEF CTDebug}
        writeln('TExtractProcTool.InsertProcIntf END ProcHeader="',ProcHeader,'"');
        {$ENDIF}
        FrontGap:=gtEmptyLine;
        AfterGap:=gtEmptyLine;
        InsertNode:=FindDeepestNodeAtPos(IntfInsertPos,false);
        if (InsertNode<>nil) then begin
          if (InsertNode.Desc=ctnProcedure) then
            AfterGap:=gtNewLine;
          if (InsertNode.PriorBrother<>nil)
          and (InsertNode.PriorBrother.Desc=ctnProcedure) then
            FrontGap:=gtNewLine;
        end;
        if not SourceChangeCache.Replace(FrontGap,AfterGap,
                                         IntfInsertPos,IntfInsertPos,ProcHeader)
        then exit;
      end;
      
    eptPublishedMethod,eptPrivateMethod,eptProtectedMethod,eptPublicMethod:
      begin
        // initialize class for code completion
        CodeCompleteClassNode:=ProcClassNode;
        CodeCompleteSrcChgCache:=SourceChangeCache;

        // insert new method to class
        MethodDefinition:='procedure '+ProcName+CompleteParamList+';';
        CleanMethodDefinition:='procedure '+ProcName+BaseParamList+';';
        if ProcExistsInCodeCompleteClass(CleanMethodDefinition) then exit;
        case ProcType of
        eptPrivateMethod:   NewClassPart:=ncpPrivateProcs;
        eptProtectedMethod: NewClassPart:=ncpProtectedProcs;
        eptPublicMethod:    NewClassPart:=ncpPublicProcs;
        else                NewClassPart:=ncpPublishedProcs;
        end;
        AddClassInsertion(nil, CleanMethodDefinition, MethodDefinition,
                          ProcName, ProcCode, NewClassPart);
        if not InsertAllNewClassParts then
          RaiseException(ctsErrorDuringInsertingNewClassParts);
      end;

    end;
    Result:=true;
  end;

  function CreateProcBody(const ProcClassName, ParamList,
    VarSection, BeginEndCode: string; var ProcCode: string): boolean;
  var
    le: String;
    ProcHeader: String;
  begin
    le:=SourceChangeCache.BeautifyCodeOptions.LineEnd;
    ProcHeader:='procedure ';
    if ProcClassName<>'' then
      ProcHeader:=ProcHeader+ProcClassName+'.';
    ProcHeader:=ProcHeader+ProcName+ParamList+';'+le;
    ProcHeader:=SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                                                                  ProcHeader,0);
    ProcCode:=ProcHeader+VarSection+BeginEndCode;
    Result:=true;
  end;

  function InsertProcBody(InsertPos,Indent: integer;
    const ProcCode: string): boolean;
  var
    TabWidth: Integer;
    IndentedProcCode: string;
  begin
    Result:=false;
    if ProcType in [eptPublishedMethod,eptPrivateMethod,eptProtectedMethod,
      eptPublicMethod] then
    begin
      if not CreateMissingProcBodies then
        RaiseException(ctsErrorDuringCreationOfNewProcBodies);
    end else begin
      TabWidth:=SourceChangeCache.BeautifyCodeOptions.TabWidth;
      IndentText(ProcCode,Indent,TabWidth,IndentedProcCode);
      {$IFDEF CTDebug}
      writeln('TExtractProcTool.InsertProcBody END ProcCode="',ProcCode,'"');
      {$ENDIF}
      if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,
                                InsertPos,InsertPos,IndentedProcCode) then exit;
    end;
    Result:=true;
  end;

  function CreatePathForNewProc(InsertPos: integer;
    const ProcClassName, BaseParamList: string;
    var NewProcPath: TStrings): boolean;
  var
    ContextNode: TCodeTreeNode;
    ProcHead: String;
  begin
    Result:=false;
    // find context at insert position
    ContextNode:=FindDeepestNodeAtPos(InsertPos,true);
    if (ContextNode.Desc=ctnProcedure) and (ContextNode.StartPos=InsertPos)
    or ((ContextNode.LastChild<>nil) and (ContextNode.LastChild.StartPos<InsertPos))
    then
      // ContextNode is a procedure below or above the insert position
      // => after the insert the new proc will not be a child
      // -> it will become a child of its parent
      ContextNode:=ContextNode.Parent;
    NewProcPath:=CreateSubProcPath(ContextNode,ShortProcFormat);
    // add new proc
    if ProcClassName<>'' then
      ProcHead:=ProcClassName+'.'
    else
      ProcHead:='';
    ProcHead:=ProcHead+ProcName+BaseParamList+';';
    NewProcPath.Add(ProcHead);
    Result:=true;
  end;

  function FindJumpPointToNewProc(SubProcPath: TStrings): boolean;
  var
    NewProcNode: TCodeTreeNode;
  begin
    Result:=false;
    // reparse code and find jump point into new proc
    BuildTree(false);
    NewProcNode:=FindSubProcPath(SubProcPath,ShortProcFormat,true);
    {$IFDEF CTDebug}
    writeln('FindJumpPointToNewProc A found=',NewProcNode<>nil);
    {$ENDIF}
    if NewProcNode=nil then exit;
    Result:=FindJumpPointInProcNode(NewProcNode,NewPos,NewTopLine);
    {$IFDEF CTDebug}
    writeln('FindJumpPointToNewProc END ',NewProcNode.DescAsString,' ',Result,' ',NewPos.X,',',NewPos.Y,' ',NewTopLine);
    {$ENDIF}
  end;

var
  MethodPossible: Boolean;
  SubProcSameLvlPossible: boolean;
  ProcClassName, CompleteParamList, BaseParamList, VarSection,
  BeginEndCode: string;
  InsertPos, Indent: integer;
  IntfInsertPos, IntfIndent: integer;
  NewProcPath: TStrings;
  ProcClassNode: TCodeTreeNode;
  ProcCode: string;
begin
  Result:=false;
  MethodPossible:=false;
  SubProcSameLvlPossible:=false;
  {$IFDEF CTDebug}
  writeln('ExtractProc A ProcName="',ProcName,'" ProcType=',ExtractProcTypeNames[ProcType]);
  {$ENDIF}
  if not CheckExtractProc(StartPos,EndPos,MethodPossible,SubProcSameLvlPossible)
  then exit;
  if (not MethodPossible) and (ProcType in [eptPrivateMethod,eptProtectedMethod,
    eptPublicMethod,eptPublishedMethod])
  then
    exit;
  if (not SubProcSameLvlPossible) and (ProcType=eptSubProcedureSameLvl) then
    exit;
  if CaretToCleanPos(StartPos,BlockStartPos)<>0 then exit;
  if CaretToCleanPos(EndPos,BlockEndPos)<>0 then exit;
  BuildSubTree(BlockStartPos);
  CodeCompleteSrcChgCache:=SourceChangeCache;
  ProcNode:=FindDeepestNodeAtPos(BlockStartPos,true).GetNodeOfType(ctnProcedure);

  VarTree:=nil;
  NewProcPath:=nil;
  try
    if not ScanNodesForVariables(ProcNode) then exit;
    if not ReplaceSelectionWithCall then exit;
    if not DeleteMovedLocalVariables then exit;
    if not CreateProcNameParts(ProcClassName,ProcClassNode) then exit;
    if not CreateProcParamList(CompleteParamList,BaseParamList) then exit;
    if not CreateProcVarSection(VarSection) then exit;
    if not CreateProcBeginEndBlock(BeginEndCode) then exit;
    if not FindInsertPositionForProcIntf(IntfInsertPos,IntfIndent) then exit;
    if not FindInsertPositionForProcBody(InsertPos,Indent) then exit;
    if NewProcAlreadyExists(ProcClassName,BaseParamList,InsertPos) then exit;
    if not CreateProcBody(ProcClassName,CompleteParamList,
                          VarSection,BeginEndCode,ProcCode) then exit;
    if not InsertProcIntf(IntfInsertPos,IntfIndent,CompleteParamList,
                  BaseParamList,ProcCode,ProcClassName,ProcClassNode) then exit;
    if not InsertProcBody(InsertPos,Indent,ProcCode) then exit;
    if not CreatePathForNewProc(InsertPos,ProcClassName,BaseParamList,
                                NewProcPath) then exit;
    if not SourceChangeCache.Apply then exit;
    if not FindJumpPointToNewProc(NewProcPath) then exit;
  finally
    if VarTree<>nil then begin
      VarTree.FreeAndClear;
      VarTree.Free;
    end;
    NewProcPath.Free;
  end;
  Result:=true;
end;

end.

