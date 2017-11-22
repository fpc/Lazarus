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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
    - with statements

  Explode With Blocks todos:
    - check if selection bounds on statement bounds
    - keep Begin..End in case
    - support Expressions
    - with Canvas do with Self do (e.g. shape.inc)
    - dialog in cody to replace a long expression with a short local variable
    - bug: shape.inc : with Self do
}
unit ExtractProcTool;

{$mode objfpc}{$H+}

{ $define CTDEBUG}
{off $Define VerboseAddWithBlock}

interface

uses
  Classes, SysUtils, math, Laz_AVL_Tree,
  // Codetools
  FileProcs, CodeToolsStrConsts, CodeTree, CodeAtom,
  CodeCache, CustomCodeTool, PascalReaderTool,
  PascalParserTool, CodeCompletionTool, KeywordFuncLists, BasicCodeTools,
  LinkScanner, SourceChanger, FindDeclarationTool;
  
type
  TExtractedProcVariableType = (
    epvtParameter,
    epvtLocalVar
    //epvtExternVar // variable is defined outside (e.g. a global variable or a class member)
    );

  TExtractedProcVariable = class
  public
    Node: TCodeTreeNode;
    Tool: TFindDeclarationTool;
    VarType: TExtractedProcVariableType;
    ReadInSelection: boolean;
    WriteInSelection: boolean;
    UsedInNonSelection: boolean;
    ReadAfterSelection: boolean;
    ReadAfterSelectionValid: boolean;
    RemovedFromOldProc: boolean;
    function UsedInSelection: boolean;
  end;

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
  protected
    function ScanNodesForVariables(const StartPos, EndPos: TCodeXYPosition;
        out BlockStartPos, BlockEndPos: integer; // the selection
        out BlockNode: TCodeTreeNode;
        VarTree: TAVLTree;  // tree of TExtractedProcVariable
        IgnoreIdentifiers: TAVLTree; // tree of PCodeXYPosition
        MissingIdentifiers: TAVLTree // tree of PCodeXYPosition
        ): boolean;
    function CheckIfRangeOnSameLevel(const StartPos, EndPos: TCodeXYPosition;
      out CleanStartPos, CleanEndPos: integer; out StartNode: TCodeTreeNode): boolean;
    function InitExtractProc(const StartPos, EndPos: TCodeXYPosition;
      out MethodPossible, SubProcPossible, SubProcSameLvlPossible: boolean): boolean;
  public
    function CheckExtractProc(const StartPos, EndPos: TCodeXYPosition;
      out MethodPossible, SubProcPossible, SubProcSameLvlPossible: boolean;
      out MissingIdentifiers: TAVLTree; // tree of PCodeXYPosition
      VarTree: TAVLTree = nil  // tree of TExtractedProcVariable
      ): boolean;
    function ExtractProc(const StartPos, EndPos: TCodeXYPosition;
      ProcType: TExtractProcType; const ProcName: string;
      IgnoreIdentifiers: TAVLTree; // tree of PCodeXYPosition
      out NewPos: TCodeXYPosition; out NewTopLine, BlockTopLine, BlockBottomLine: integer;
      SourceChangeCache: TSourceChangeCache;
      FunctionResultVariableStartPos: integer = 0): boolean;

    function RemoveWithBlock(const CursorPos: TCodeXYPosition;
      SourceChangeCache: TSourceChangeCache): boolean;
    function AddWithBlock(const StartPos, EndPos: TCodeXYPosition;
      const WithExpr: string; // if empty: collect Candidates
      Candidates: TStrings; SourceChangeCache: TSourceChangeCache): boolean;

    procedure CalcMemSize(Stats: TCTMemStats); override;
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

function CreateExtractProcVariableTree: TAVLTree;
procedure ClearExtractProcVariableTree(VarTree: TAVLTree; FreeTree: boolean);

implementation

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

function CreateExtractProcVariableTree: TAVLTree;
begin
  Result:=TAVLTree.Create(TListSortCompare(@CompareExtractedProcVariables));
end;

procedure ClearExtractProcVariableTree(VarTree: TAVLTree; FreeTree: boolean);
begin
  if VarTree=nil then exit;
  VarTree.FreeAndClear;
  if FreeTree then
    VarTree.Free;
end;

{ TExtractedProcVariable }

function TExtractedProcVariable.UsedInSelection: boolean;
begin
  Result:=ReadInSelection or WriteInSelection;
end;

{ TExtractProcTool }

function TExtractProcTool.InitExtractProc(const StartPos,
  EndPos: TCodeXYPosition; out MethodPossible, SubProcPossible,
  SubProcSameLvlPossible: boolean): boolean;
var
  CleanStartPos, CleanEndPos: integer;
  StartNode: TCodeTreeNode;
  ANode: TCodeTreeNode;
  ProcLvl: Integer;
begin
  Result:=false;
  MethodPossible:=false;
  SubProcPossible:=false;
  SubProcSameLvlPossible:=false;
  {$IFDEF CTDebug}
  DebugLn('TExtractProcTool.InitExtractProc syntax and cursor check ..');
  {$ENDIF}
  Result:=CheckIfRangeOnSameLevel(StartPos,EndPos,CleanStartPos,CleanEndPos,
                                  StartNode);
  // check if start not in a statement
  // ToDo
  // check if end not in a statement
  // ToDo
  {$IFDEF CTDebug}
  DebugLn('TExtractProcTool.InitExtractProc Method check ..');
  {$ENDIF}
  // check if in a method body
  ANode:=StartNode;
  ProcLvl:=0;
  while ANode<>nil do begin
    if (ANode.Desc=ctnProcedure) then begin
      SubProcPossible:=true;
      inc(ProcLvl);
      if NodeIsInAMethod(ANode) then begin
        MethodPossible:=true;
      end;
    end;
    ANode:=ANode.Parent;
  end;
  SubProcSameLvlPossible:=(ProcLvl>1);
  {$IFDEF CTDebug}
  DebugLn('TExtractProcTool.InitExtractProc END');
  {$ENDIF}
  Result:=true;
end;

function TExtractProcTool.CheckExtractProc(const StartPos,
  EndPos: TCodeXYPosition; out MethodPossible, SubProcPossible,
  SubProcSameLvlPossible: boolean; out MissingIdentifiers: TAVLTree;
  VarTree: TAVLTree): boolean;
var
  BlockStartPos: integer;
  BlockEndPos: integer;
  ProcNode: TCodeTreeNode;
begin
  Result:=false;
  MissingIdentifiers:=nil;
  ActivateGlobalWriteLock;
  try
    if not InitExtractProc(StartPos,EndPos,MethodPossible,
      SubProcPossible,SubProcSameLvlPossible)
    then exit;
    MissingIdentifiers:=CreateTreeOfPCodeXYPosition;
    if not ScanNodesForVariables(StartPos,EndPos,BlockStartPos,BlockEndPos,
                                 ProcNode,VarTree,nil,MissingIdentifiers) then exit;
  finally
    DeactivateGlobalWriteLock;
  end;
  Result:=true;
end;

function TExtractProcTool.ExtractProc(const StartPos, EndPos: TCodeXYPosition;
  ProcType: TExtractProcType; const ProcName: string;
  IgnoreIdentifiers: TAVLTree; out NewPos: TCodeXYPosition; out NewTopLine,
  BlockTopLine, BlockBottomLine: integer;
  SourceChangeCache: TSourceChangeCache; FunctionResultVariableStartPos: integer
  ): boolean;
const
  ShortProcFormat = [phpWithoutClassKeyword];
var
  BlockStartPos, BlockEndPos: integer; // the selection
  MainBlockNode: TCodeTreeNode; // the main proc node of the selection, or main begin block of program
  VarTree: TAVLTree;
  ResultNode: TCodeTreeNode;
  Beauty: TBeautifyCodeOptions;

  function FindFunctionResultNode: boolean;
  var
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
  begin
    Result:=false;
    ResultNode:=nil;
    if FunctionResultVariableStartPos<1 then exit(true); // create a proc, not a function
    AVLNode:=VarTree.FindLowest;
    while AVLNode<>nil do begin
      ProcVar:=TExtractedProcVariable(AVLNode.Data);
      if ProcVar.Node.StartPos=FunctionResultVariableStartPos then begin
        ProcVar.UsedInNonSelection:=true;
        ProcVar.ReadAfterSelection:=true;
        Result:=true;
        ResultNode:=ProcVar.Node;
        exit;
      end;
      AVLNode:=VarTree.FindSuccessor(AVLNode);
    end;
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
    DebugLn('TExtractProcTool.ReplaceSelectionWithCall A');
    {$ENDIF}
    Indent:=Beauty.GetLineIndent(Src,BlockStartPos);
    ParamListCode:='';
    // gather all variables, that are used in the selection and in the rest of
    // the old proc (in front or behind). These are the parameters for the new proc.
    if (VarTree<>nil) and (ProcType<>eptSubProcedure) then begin
      AVLNode:=VarTree.FindLowest;
      while AVLNode<>nil do begin
        ProcVar:=TExtractedProcVariable(AVLNode.Data);
        {$IFDEF CTDebug}
        DebugLn('TExtractProcTool.ReplaceSelectionWithCall B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' ReadInSelection=',dbgs(ProcVar.ReadInSelection),
          ' WriteInSelection=',dbgs(ProcVar.WriteInSelection),
          ' UsedInNonSelection=',dbgs(ProcVar.UsedInNonSelection),
          ' ReadAfterSelection=',dbgs(ProcVar.ReadAfterSelection),
          '');
        {$ENDIF}
        if (ProcVar.UsedInSelection and ProcVar.UsedInNonSelection)
        and (ResultNode<>ProcVar.Node) then begin
          // parameter
          if ParamListCode<>'' then ParamListCode:=ParamListCode+',';
          ParamListCode:=ParamListCode+GetIdentifier(@Src[ProcVar.Node.StartPos]);
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    if ParamListCode<>'' then
      ParamListCode:='('+ParamListCode+')';
    CallCode:=ProcName+ParamListCode+';';
    if ResultNode<>nil then begin
      CallCode:=GetIdentifier(@Src[ResultNode.StartPos])+':='+CallCode;
    end;
    CallCode:=Beauty.BeautifyStatement(CallCode,Indent);
    {$IFDEF CTDebug}
    DebugLn('TExtractProcTool.ReplaceSelectionWithCall C "',CallCode,'" Indent=',dbgs(Indent));
    {$ENDIF}
    SourceChangeCache.Replace(gtNewLine,gtNewLine,BlockStartPos,BlockEndPos,
                              CallCode);
    Result:=true;
  end;
  
  function DeleteLocalVariable(ProcVar: TExtractedProcVariable): boolean;
  
    function VariableNodeShouldBeDeleted(VarNode: TCodeTreeNode;
      out CurProcVar: TExtractedProcVariable): boolean;
    var
      AVLNode: TAVLTreeNode;
    begin
      CurProcVar:=nil;
      AVLNode:=VarTree.FindKey(VarNode,
                       TListSortCompare(@CompareNodeWithExtractedProcVariable));
      if AVLNode=nil then begin
        Result:=false;
      end else begin
        CurProcVar:=TExtractedProcVariable(AVLNode.Data);
        Result:=(not CurProcVar.UsedInNonSelection)
                and CurProcVar.UsedInSelection;
      end;
    end;
    
    function VarSectionIsEmpty: boolean;
    var
      VarNode: TCodeTreeNode;
      SectionNode: TCodeTreeNode;
      CurProcVar: TExtractedProcVariable;
    begin
      Result:=false;
      SectionNode:=ProcVar.Node;
      if SectionNode.Desc=ctnVarDefinition then
        SectionNode:=SectionNode.Parent;
      if SectionNode.Desc<>ctnVarSection then exit;
      VarNode:=SectionNode.FirstChild;
      while VarNode<>nil do begin
        CurProcVar:=nil;
        if not VariableNodeShouldBeDeleted(VarNode,CurProcVar) then exit;
        if not CurProcVar.RemovedFromOldProc then exit;
        VarNode:=VarNode.NextBrother;
      end;
      Result:=true;
    end;
  
  var
    VarNode: TCodeTreeNode;
    FirstVarNode: TCodeTreeNode;
    LastVarNode: TCodeTreeNode;
    DeleteCompleteDefinition: Boolean;
    DeleteStartPos: Integer;
    DeleteEndPos: Integer;
    CurProcVar: TExtractedProcVariable;
    FrontGap: TGapTyp;
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
          AtomIsIdentifierE;
          ReadNextAtom;
          if CurPos.Flag=cafComma then begin
            // there is a next variable in the same var definition
            ReadNextAtom;
            DeleteEndPos:=CurPos.StartPos;
          end else if CurPos.Flag=cafColon then begin
            // this is the last variable in the definition
            DeleteEndPos:=CurPos.StartPos;
            if (DeleteStartPos=VarNode.StartPos)
            and (VarNode<>FirstVarNode) then begin
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
            DebugLn('DeleteLocalVariable Delete last vars: "',copy(Src,DeleteStartPos,DeleteEndPos-DeleteStartPos),'"');
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
      FrontGap:=gtNone;
      if DeleteCompleteDefinition and (DeleteStartPos>0) then begin
        // all variables of the definition should be deleted
        // -> delete type declaration
        DeleteEndPos:=FindLineEndOrCodeAfterPosition(LastVarNode.EndPos);
        if VarSectionIsEmpty then begin
          // all variables of the 'var' section are deleted
          // -> delete var section
          DeleteStartPos:=FirstVarNode.Parent.StartPos;
        end else if FirstVarNode.PriorBrother=nil then begin
          // keep a space between 'var' and the next identifier
          FrontGap:=gtSpace;
        end;
        DeleteStartPos:=FindLineEndOrCodeInFrontOfPosition(DeleteStartPos,true);
      end;
      if DeleteStartPos>0 then begin
        {$IFDEF CTDebug}
        DebugLn('DeleteLocalVariable Delete Rest: "',copy(Src,DeleteStartPos,DeleteEndPos-DeleteStartPos),'"');
        {$ENDIF}
        if not SourceChangeCache.Replace(FrontGap,gtNone,
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
    DebugLn('TExtractProcTool.DeleteMovedLocalVariables A');
    {$ENDIF}
    // gather all variables, that are used in the selection, but not in the
    // rest of the old proc. These are local variables, that are moved to the
    // new proc.
    if (VarTree<>nil) then begin
      AVLNode:=VarTree.FindLowest;
      while AVLNode<>nil do begin
        ProcVar:=TExtractedProcVariable(AVLNode.Data);
        {$IFDEF CTDebug}
        DebugLn('TExtractProcTool.DeleteMovedLocalVariables B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' ReadInSelection=',dbgs(ProcVar.ReadInSelection),
          ' WriteInSelection=',dbgs(ProcVar.WriteInSelection),
          ' UsedInNonSelection=',dbgs(ProcVar.UsedInNonSelection),
          ' ReadAfterSelection=',dbgs(ProcVar.ReadAfterSelection),
          '');
        {$ENDIF}
        if ProcVar.UsedInSelection and (not ProcVar.UsedInNonSelection) then
        begin
          if not DeleteLocalVariable(ProcVar) then exit;
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    {$IFDEF CTDebug}
    DebugLn('DeleteMovedLocalVariables END ');
    {$ENDIF}
    Result:=true;
  end;
  
  function CreateProcNameParts(out ProcClassName: string;
    out ProcClassNode: TCodeTreeNode): boolean;
  begin
    Result:=false;
    ProcClassName:='';
    ProcClassNode:=nil;
    if ProcType in [eptPrivateMethod,eptProtectedMethod,eptPublicMethod,
      eptPublishedMethod] then
    begin
      {$IFDEF CTDebug}
      DebugLn('CreateProcNameParts A searching class name ..');
      {$ENDIF}
      if (MainBlockNode=nil) or (MainBlockNode.Desc<>ctnProcedure) then begin
        debugln(['CreateProcNameParts not in a procedure']);
        exit;
      end;
      ProcClassName:=ExtractClassNameOfProcNode(MainBlockNode);
      if ProcClassName='' then begin
        debugln(['CreateProcNameParts not in a method']);
        exit;
      end;
      ProcClassNode:=FindClassNodeInUnit(ProcClassName,
                                         true,false,false,true);
      if ProcClassNode=nil then begin
        debugln(['CreateProcNameParts class not found ',ProcClassName]);
        exit;
      end;
      ProcClassName:=ExtractClassName(ProcClassNode,false);
    end;
    {$IFDEF CTDebug}
    DebugLn('CreateProcNameParts END ProcClassName="',ProcClassName,'"');
    {$ENDIF}
    Result:=true;
  end;

  function CreateProcParamList(
    out CompleteParamListCode, // including modifiers, brackets and result type
    BaseParamListCode: string // without modifiers and result type
    ): boolean;
  var
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
    ParamName: String;
    ParamTypeCode: String;
    ParamSpecifier: String;
    ResultType: String;
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
        DebugLn('TExtractProcTool.CreateProcParamList B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' ReadInSelection=',dbgs(ProcVar.ReadInSelection),
          ' WriteInSelection=',dbgs(ProcVar.WriteInSelection),
          ' UsedInNonSelection=',dbgs(ProcVar.UsedInNonSelection),
          ' ReadAfterSelection=',dbgs(ProcVar.ReadAfterSelection),
          '');
        {$ENDIF}
        if ProcVar.UsedInSelection and ProcVar.UsedInNonSelection
        and (ProcVar.Node<>ResultNode) then begin
          // extract identifier and type
          if CompleteParamListCode<>'' then
            CompleteParamListCode:=CompleteParamListCode+';';
          if BaseParamListCode<>'' then
            BaseParamListCode:=BaseParamListCode+';';
          ParamName:=GetIdentifier(@Src[ProcVar.Node.StartPos]);
          ParamTypeCode:=ExtractDefinitionNodeType(ProcVar.Node);
          {$IFDEF CTDebug}
          DebugLn('TExtractProcTool.CreateProcParamList C ParamName="',ParamName,'" ParamType="',ParamTypeCode,'"');
          {$ENDIF}
          // ToDo: ParamSpecifier 'var ' and none
          if ProcVar.WriteInSelection then
            ParamSpecifier:=''
          else
            ParamSpecifier:='const ';
          if ProcVar.ReadAfterSelection then
            ParamSpecifier:='var ';
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
    if ResultNode<>nil then begin
      ResultType:=ExtractDefinitionNodeType(ResultNode);
      CompleteParamListCode:=CompleteParamListCode+':'+ResultType;
    end;
    {$IFDEF CTDebug}
    DebugLn('CreateProcParamList END CompleteParamListCode="',CompleteParamListCode,'"');
    {$ENDIF}
    Result:=true;
  end;
  
  function CreateProcVarSection(out VarSectionCode: string): boolean;
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
        DebugLn('TExtractProcTool.CreateProcVarSection B ',GetIdentifier(@Src[ProcVar.Node.StartPos]),
          ' ReadInSelection=',dbgs(ProcVar.ReadInSelection),
          ' WriteInSelection=',dbgs(ProcVar.WriteInSelection),
          ' UsedInNonSelection=',dbgs(ProcVar.UsedInNonSelection),
          ' ReadAfterSelection=',dbgs(ProcVar.ReadAfterSelection),'');
        {$ENDIF}
        if ProcVar.UsedInSelection
        and ((not ProcVar.UsedInNonSelection) or (ProcVar.Node=ResultNode)) then
        begin
          // extract identifier and type
          if VarSectionCode='' then
            VarSectionCode:='var'+Beauty.LineEnd;
          VarSectionCode:=VarSectionCode+Beauty.GetIndentStr(Beauty.Indent);
          VariableName:=GetIdentifier(@Src[ProcVar.Node.StartPos]);
          VarTypeNode:=FindTypeNodeOfDefinition(ProcVar.Node);
          {$IFDEF CTDebug}
          DebugLn('TExtractProcTool.CreateProcVarSection VarTypeNode=',copy(Src,VarTypeNode.StartPos,VarTypeNode.EndPos-VarTypeNode.StartPos));
          {$ENDIF}
          TypeDefEndPos:=FindLineEndOrCodeAfterPosition(VarTypeNode.EndPos);
          {$IFDEF CTDebug}
          DebugLn('TExtractProcTool.CreateProcVarSection PlusComment=',copy(Src,VarTypeNode.StartPos,TypeDefEndPos-VarTypeNode.StartPos));
          {$ENDIF}
          VariableTypeCode:=copy(Src,VarTypeNode.StartPos,
                                 TypeDefEndPos-VarTypeNode.StartPos);
          {$IFDEF CTDebug}
          DebugLn('TExtractProcTool.CreateProcVarSection C VariableName="',VariableName,'" VariableType="',VariableTypeCode,'"');
          {$ENDIF}
          VarSectionCode:=VarSectionCode+VariableName+':'+VariableTypeCode
                          +Beauty.LineEnd;
        end;
        AVLNode:=VarTree.FindSuccessor(AVLNode);
      end;
    end;
    {$IFDEF CTDebug}
    DebugLn('TExtractProcTool.CreateProcVarSection END VarSectionCode="',VarSectionCode,'"');
    {$ENDIF}
    VarSectionCode:=Beauty.BeautifyStatement(VarSectionCode,0);
    Result:=true;
  end;
  
  function CreateProcBeginEndBlock(out BeginEndCode: string): boolean;
  var
    DirtyStartPos, DirtyEndPos: integer;
    le, s: String;
    Indent: Integer;
    DirtySelection: String;
  begin
    Result:=false;
    BeginEndCode:='';
    le:=Beauty.LineEnd;
    // extract dirty source, so that compiler directives are moved too
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
               Beauty.Indent-Indent,
               Beauty.TabWidth,
               s);
    DirtySelection:=s;
    if ResultNode<>nil then begin
      DirtySelection:=DirtySelection
              +Beauty.GetIndentStr(Beauty.Indent)
              +'Result:='+GetIdentifier(@Src[ResultNode.StartPos])+';'+le;
    end;
    // create Begin..End block
    BeginEndCode:='begin'+le
                  +DirtySelection
                  +'end;';
    {$IFDEF CTDebug}
    DebugLn('TExtractProcTool.CreateProcBeginEndBlock END BeginEndCode="',BeginEndCode,'"');
    {$ENDIF}
    Result:=true;
  end;
  
  function FindInsertPositionForProcBody(
    out InsertPos, Indent: integer): boolean;
  var
    BeginNode: TCodeTreeNode;
    ANode: TCodeTreeNode;
    InsertNode: TCodeTreeNode;
  begin
    Result:=false;
    case ProcType of
    
    eptSubProcedure:
      begin
        if MainBlockNode.Desc<>ctnProcedure then begin
          debugln(['FindInsertPositionForProcBody subprocedure: not in a procedure']);
          exit;
        end;
        BeginNode:=MainBlockNode.LastChild;
        while BeginNode.Desc<>ctnBeginBlock do
          BeginNode:=BeginNode.PriorBrother;
        InsertPos:=BeginNode.StartPos;
        Indent:=Beauty.GetLineIndent(Src,InsertPos)+Beauty.Indent;
      end;
      
    eptSubProcedureSameLvl:
      begin
        // -> insert in front of old proc
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(MainBlockNode.StartPos);
        Indent:=Beauty.GetLineIndent(Src,MainBlockNode.StartPos);
      end;

    eptProcedure,eptProcedureWithInterface:
      begin
        // insert in front of top level proc
        InsertNode:=MainBlockNode;
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
        Indent:=Beauty.GetLineIndent(Src,InsertNode.StartPos);
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
        InsertPos:=FindLineEndOrCodeInFrontOfPosition(MainBlockNode.StartPos);
        Indent:=Beauty.GetLineIndent(Src,MainBlockNode.StartPos);
      end;

    else
      exit;
    end;
    Result:=true;
  end;

  function FindInsertPositionForProcIntf(
    out IntfInsertPos, IntfIndent: integer): boolean;
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
    ConflictProcNode:=FindProcNode(ContextNode,ProcHead,mgMethod,
                                   ShortProcFormat+[phpIgnoreForwards]);
    Result:=ConflictProcNode<>nil;
    if Result then begin
      RaiseException(20170421201925,'New procedure "'+ProcName+'" exists already');
    end;
    {$IFDEF CTDebug}
    DebugLn('NewProcAlreadExists END ProcHead="',ProcHead,'" Found=',dbgs(Result));
    {$ENDIF}
  end;

  function InsertProcIntf(IntfInsertPos, IntfIndent: integer;
    const CompleteParamList, BaseParamList, ProcCode: string;
    ProcClassNode: TCodeTreeNode): boolean;
  var
    ProcHeader: String;
    FrontGap: TGapTyp;
    AfterGap: TGapTyp;
    InsertNode: TCodeTreeNode;
    MethodDefinition: String;
    CleanMethodDefinition: String;
    NewClassPart: TNewClassPart;
    Keyword: String;
  begin
    Result:=false;
    if ResultNode=nil then
      Keyword:='procedure'
    else
      Keyword:='function';

    case ProcType of
    
    eptProcedureWithInterface:
      begin
        ProcHeader:=Keyword+' '+ProcName+CompleteParamList+';';
        ProcHeader:=Beauty.BeautifyStatement(ProcHeader,IntfIndent);
        {$IFDEF CTDebug}
        DebugLn('TExtractProcTool.InsertProcIntf END ProcHeader="',ProcHeader,'"');
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
        MethodDefinition:=Keyword+' '+ProcName+CompleteParamList+';';
        CleanMethodDefinition:=Keyword+' '+ProcName+BaseParamList+';';
        if ProcExistsInCodeCompleteClass(CleanMethodDefinition) then exit;
        case ProcType of
        eptPrivateMethod:   NewClassPart:=ncpPrivateProcs;
        eptProtectedMethod: NewClassPart:=ncpProtectedProcs;
        eptPublicMethod:    NewClassPart:=ncpPublicProcs;
        else                NewClassPart:=ncpPublishedProcs;
        end;
        AddClassInsertion(CleanMethodDefinition, MethodDefinition,
                          ProcName, NewClassPart, nil, ProcCode);
        if not InsertAllNewClassParts then
          RaiseException(20170421201927,ctsErrorDuringInsertingNewClassParts);
      end;

    end;
    Result:=true;
  end;

  function CreateProcBody(const ProcClassName, ParamList,
    VarSection, BeginEndCode: string; out ProcCode: string): boolean;
  var
    le: String;
    ProcHeader: String;
  begin
    le:=Beauty.LineEnd;
    if ResultNode=nil then
      ProcHeader:='procedure '
    else
      ProcHeader:='function ';
    if ProcClassName<>'' then
      ProcHeader:=ProcHeader+ProcClassName+'.';
    ProcHeader:=ProcHeader+ProcName+ParamList+';'+le;
    ProcHeader:=Beauty.BeautifyStatement(ProcHeader,0);
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
      if not CreateMissingClassProcBodies(false) then
        RaiseException(20170421201930,ctsErrorDuringCreationOfNewProcBodies);
    end else begin
      TabWidth:=Beauty.TabWidth;
      IndentText(ProcCode,Indent,TabWidth,IndentedProcCode);
      {$IFDEF CTDebug}
      DebugLn('TExtractProcTool.InsertProcBody END ProcCode="',ProcCode,'"');
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
    BuildTree(lsrInitializationStart);
    NewProcNode:=FindSubProcPath(SubProcPath,ShortProcFormat,true);
    {$IFDEF CTDebug}
    DebugLn('FindJumpPointToNewProc A found=',dbgs(NewProcNode<>nil));
    {$ENDIF}
    if NewProcNode=nil then exit;
    Result:=FindJumpPointInProcNode(NewProcNode,NewPos,NewTopLine,BlockTopLine,BlockBottomLine);
    {$IFDEF CTDebug}
    DebugLn('FindJumpPointToNewProc END ',NewProcNode.DescAsString,' ',dbgs(Result),' ',dbgs(NewPos.X),',',dbgs(NewPos.Y),' ',dbgs(NewTopLine));
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
  SubProcPossible: boolean;
begin
  Result:=false;
  {$IFDEF CTDebug}
  DebugLn(['ExtractProc A ProcName="',ProcName,'" ProcType=',ExtractProcTypeNames[ProcType],' FunctionResultVariableStartPos=',FunctionResultVariableStartPos]);
  {$ENDIF}
  if not InitExtractProc(StartPos,EndPos,MethodPossible,
    SubProcPossible,SubProcSameLvlPossible)
  then exit;
  if (not MethodPossible) and (ProcType in [eptPrivateMethod,eptProtectedMethod,
    eptPublicMethod,eptPublishedMethod])
  then
    exit;
  if (not SubProcPossible)
  and (ProcType in [eptSubProcedure,eptSubProcedureSameLvl]) then
    exit;
  if (not SubProcSameLvlPossible) and (ProcType=eptSubProcedureSameLvl) then
    exit;
  CodeCompleteSrcChgCache:=SourceChangeCache;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;

  VarTree:=CreateExtractProcVariableTree;
  NewProcPath:=nil;
  try
    if not ScanNodesForVariables(StartPos,EndPos,BlockStartPos,BlockEndPos,
                                 MainBlockNode,VarTree,IgnoreIdentifiers,nil) then exit;
    if not FindFunctionResultNode then exit;
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
                  BaseParamList,ProcCode,ProcClassNode) then exit;
    if not InsertProcBody(InsertPos,Indent,ProcCode) then exit;
    if not CreatePathForNewProc(InsertPos,ProcClassName,BaseParamList,
                                NewProcPath) then exit;
    if not SourceChangeCache.Apply then exit;
    if not FindJumpPointToNewProc(NewProcPath) then exit;
  finally
    ClearExtractProcVariableTree(VarTree,true);
    NewProcPath.Free;
  end;
  Result:=true;
end;

function TExtractProcTool.RemoveWithBlock(const CursorPos: TCodeXYPosition;
  SourceChangeCache: TSourceChangeCache): boolean;
type
  TWithVarCache = record
    WithVarNode: TCodeTreeNode;
    VarEndPos: integer;
    WithVarExpr: TExpressionType;
  end;
  PWithVarCache = ^TWithVarCache;

var
  WithVarNode: TCodeTreeNode;
  StatementNode: TCodeTreeNode;
  WithIdentifiers: TAVLTree; // identifiers to change
  WithVarCache: TFPList; // list of PWithVarCache
  WithVarEndPos: LongInt;
  Beauty: TBeautifyCodeOptions;
  WithKeyWord, DoKeyWord, BeginKeyWord, EndKeyWord: TAtomPosition;
  EndSemiColon: integer; // position of the ending semicolon, 0=not there
  IndentWith: integer; // indent of the line containing the WITH keyword
  IndentInnerWith: integer; // indent of the first statement in the WITH
  DeleteHeaderEndPos, DeleteFooterStartPos: integer;
  KeepBeginEnd: boolean;

  procedure AddIdentifier(CleanPos: integer);
  var
    p: Pointer;
  begin
    p:={%H-}Pointer(PtrUInt(CleanPos));
    if WithIdentifiers=nil then WithIdentifiers:=TAVLTree.Create;
    if WithIdentifiers.Find(p)<>nil then exit;
    {$IFDEF CTDEBUG}
    debugln(['AddIdentifier ',GetIdentifier(@Src[CleanPos])]);
    {$ENDIF}
    WithIdentifiers.Add(p);
  end;

  function IdentifierDefinedByWith(CleanPos: integer;
    WithVarNode: TCodeTreeNode): boolean;
  var
    i: Integer;
    Cache: PWithVarCache;
    ParentParams, Params: TFindDeclarationParams;
  begin
    Result:=false;

    ParentParams := TFindDeclarationParams.Create(Self,WithVarNode);
    try
      // check cache
      if WithVarCache=nil then
        WithVarCache:=TFPList.Create;
      i:=WithVarCache.Count-1;
      while (i>=0) and (PWithVarCache(WithVarCache[i])^.WithVarNode<>WithVarNode) do
        dec(i);
      if i>=0 then begin
        Cache:=PWithVarCache(WithVarCache[i]);
      end else begin
        // resolve type of With variable
        {$IFDEF CTDEBUG}
        debugln(['IdentifierDefinedByWith NEW WithVar']);
        {$ENDIF}
        New(Cache);
        WithVarCache.Add(Cache);
        Cache^.WithVarNode:=WithVarNode;
        Cache^.WithVarExpr:=CleanExpressionType;
        Cache^.VarEndPos:=FindEndOfTerm(WithVarNode.StartPos,false,true);
        Params:=TFindDeclarationParams.Create(ParentParams);
        try
          Params.ContextNode:=WithVarNode;
          Params.Flags:=[fdfExceptionOnNotFound,fdfFunctionResult,fdfFindChildren];
          Cache^.WithVarExpr:=FindExpressionTypeOfTerm(WithVarNode.StartPos,-1,Params,true);
          if (Cache^.WithVarExpr.Desc<>xtContext)
          or (Cache^.WithVarExpr.Context.Node=nil)
          or (not (Cache^.WithVarExpr.Context.Node.Desc
                   in (AllClasses+[ctnEnumerationType])))
          then begin
            MoveCursorToCleanPos(Cache^.WithVarNode.StartPos);
            RaiseException(20170421201932,ctsExprTypeMustBeClassOrRecord);
          end;
          {$IFDEF CTDEBUG}
          debugln(['IdentifierDefinedByWith WithVarExpr=',ExprTypeToString(Cache^.WithVarExpr)]);
          {$ENDIF}
        finally
          Params.Free;
        end;
      end;

      if CleanPos<=Cache^.VarEndPos then exit;

      // search identifier in with var context
      Params:=TFindDeclarationParams.Create(ParentParams);
      try
        Params.SetIdentifier(Self,@Src[CleanPos],nil);
        Params.Flags:=[fdfSearchInAncestors,fdfSearchInHelpers];
        Params.ContextNode:=Cache^.WithVarExpr.Context.Node;
        Result:=Cache^.WithVarExpr.Context.Tool.FindIdentifierInContext(Params);
        {$IFDEF CTDEBUG}
        debugln(['IdentifierDefinedByWith Identifier=',GetIdentifier(@Src[CleanPos]),' FoundInWith=',Result,' WithVar="',dbgstr(Src,WithVarNode.StartPos,10),'"']);
        {$ENDIF}
      finally
        Params.Free;
      end;
    finally
      ParentParams.Free;
    end;
  end;

  procedure CheckIdentifierAtCursor;
  var
    IdentifierCleanPos: LongInt;
    Node: TCodeTreeNode;
  begin
    IdentifierCleanPos:=CurPos.StartPos;
    // search identifier in all WITH contexts
    Node:=FindDeepestNodeAtPos(IdentifierCleanPos,true);
    while Node<>nil do begin
      if Node.Desc=ctnWithVariable then begin
        if IdentifierDefinedByWith(IdentifierCleanPos,Node) then begin
          if Node=WithVarNode then begin
            // identifier uses the removing WITH
            // ToDo: check if it resolves without the WITH to the same
            AddIdentifier(IdentifierCleanPos);
          end else begin
            // identifier is defined in a sub With
            break;
          end;
        end;
        // next
        if Node=WithVarNode then
          break
        else if (Node.PriorBrother<>nil)
        and (Node.PriorBrother.Desc=ctnWithVariable)
        and (Node.PriorBrother.FirstChild=nil) then
          // e.g. with A,B do
          Node:=Node.PriorBrother
        else
          Node:=Node.Parent;
      end else
        Node:=Node.Parent;
    end;
  end;

  function NeedBrackets(StartPos, EndPos: integer): boolean;
  begin
    Result:=false;
    MoveCursorToCleanPos(StartPos);
    repeat
      ReadNextAtom;
      if WordIsTermOperator.DoItCaseInsensitive(Src,
          CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
      then exit(true);
    until (CurPos.StartPos>=EndPos) or (CurPos.StartPos>SrcLen);
  end;

  function FindBounds: boolean;
  var
    p: Integer;
    NeedBeginEnd: Boolean;
  begin
    Result:=false;
    WithKeyWord:=CleanAtomPosition;
    DoKeyWord:=CleanAtomPosition;
    BeginKeyWord:=CleanAtomPosition;
    EndKeyWord:=CleanAtomPosition;
    EndSemiColon:=0;
    KeepBeginEnd:=false;
    NeedBeginEnd:=false;
    MoveCursorToNodeStart(WithVarNode.Prior);
    repeat
      ReadNextAtom;
      if (CurPos.StartPos<WithVarNode.StartPos) then begin
        NeedBeginEnd:=UpAtomIs('DO') or UpAtomIs('THEN') or UpAtomIs('ELSE');
        if NeedBeginEnd then
          ReadNextAtom;
        if UpAtomIs('WITH') then begin
          WithKeyWord:=CurPos;
          KeepBeginEnd:=NeedBeginEnd;
        end;
      end else if (DoKeyword.EndPos=0) and (WithKeyWord.StartPos>0) and UpAtomIs('DO')
      then begin
        DoKeyWord:=CurPos;
        ReadNextAtom;
        if UpAtomIs('BEGIN') then begin
          BeginKeyWord:=CurPos;
          ReadTilBlockEnd(false,false);
          EndKeyWord:=CurPos;
          ReadNextAtom;
          if CurPos.Flag=cafSemicolon then
            EndSemiColon:=CurPos.StartPos;
        end;
        break;
      end;
    until (CurPos.StartPos>SrcLen) or (CurPos.StartPos>StatementNode.EndPos);
    IndentWith:=Beauty.GetLineIndent(Src,WithKeyWord.StartPos);
    p:=FindLineEndOrCodeAfterPosition(Max(DoKeyWord.EndPos,BeginKeyWord.EndPos),true,true);
    IndentInnerWith:=Beauty.GetLineIndent(Src,p);
    Result:=true;
  end;

  function RemoveWithHeader: boolean;
  var
    StartPos: LongInt;
    EndPos: LongInt;
  begin
    DeleteHeaderEndPos:=0;
    DeleteFooterStartPos:=SrcLen;
    if (WithVarNode.FirstChild<>nil)
    and ((WithVarNode.PriorBrother=nil)
       or (WithVarNode.PriorBrother.Desc<>ctnWithVariable)
       or (WithVarNode.PriorBrother.FirstChild<>nil))
    then begin
      // remove WITH header and footer
      // e.g. with A do
      //      with A do begin end;
      // remove 'with .. do [begin..end;]'
      StartPos:=FindLineEndOrCodeInFrontOfPosition(WithKeyword.StartPos);
      EndPos:=DoKeyWord.EndPos;
      if (not KeepBeginEnd) and (BeginKeyWord.StartPos>0) then
        EndPos:=BeginKeyWord.EndPos;
      EndPos:=FindLineEndOrCodeAfterPosition(EndPos);
      if not SourceChangeCache.Replace(gtSpace,gtNone,StartPos,EndPos,'')
      then exit(false);
      DeleteHeaderEndPos:=EndPos;

      // remove 'end;'
      if (not KeepBeginEnd) and (EndKeyWord.StartPos>0) then begin
        StartPos:=FindLineEndOrCodeInFrontOfPosition(EndKeyWord.StartPos);
        EndPos:=Max(StatementNode.EndPos,EndSemiColon+1);
        EndPos:=FindLineEndOrCodeAfterPosition(EndPos);
        if not SourceChangeCache.Replace(gtSpace,gtNone,StartPos,EndPos,'')
        then exit(false);
        DeleteFooterStartPos:=StartPos;
      end;
    end else begin
      // remove only variable
      // e.g. with A,B do
      StartPos:=WithVarNode.StartPos;
      EndPos:=WithVarEndPos;
      if Src[EndPos]=',' then begin
        inc(EndPos);
      end else if (WithVarNode.PriorBrother<>nil)
      and (WithVarNode.PriorBrother.Desc=ctnWithVariable)
      and (WithVarNode.PriorBrother.FirstChild=nil) then begin
        StartPos:=FindEndOfTerm(WithVarNode.PriorBrother.StartPos,true,true);
        StartPos:=FindLineEndOrCodeAfterPosition(StartPos);
      end;
      EndPos:=FindLineEndOrCodeAfterPosition(EndPos,true);
      StartPos:=FindLineEndOrCodeInFrontOfPosition(StartPos);
      if not SourceChangeCache.Replace(gtSpace,gtNone,StartPos,EndPos,'') then
        exit(false);
    end;
    Result:=true;
  end;

  function PrefixSubIdentifiers: boolean;
  var
    WithVar: String;
    AVLNode: TAVLTreeNode;
    CleanPos: Integer;
  begin
    // insert all 'variable.'
    if WithIdentifiers<>nil then begin
      WithVar:=ExtractCode(WithVarNode.StartPos,WithVarEndPos,[]);
      if NeedBrackets(WithVarNode.StartPos,WithVarEndPos) then
        WithVar:='('+WithVar+')';
      WithVar:=WithVar+'.';
      //debugln(['Replace WithVar="',dbgstr(WithVar),'"']);

      AVLNode:=WithIdentifiers.FindLowest;
      while AVLNode<>nil do begin
        CleanPos:=integer({%H-}PtrUInt(AVLNode.Data));
        //debugln(['Replace Prefix identifier: ',GetIdentifier(@Src[CleanPos])]);
        if not SourceChangeCache.Replace(gtNone,gtNone,CleanPos,CleanPos,WithVar)
        then
          exit(false);
        AVLNode:=WithIdentifiers.FindSuccessor(AVLNode);
      end;
    end;
    Result:=true;
  end;

  function UnindentAndEncloseSkippedCode: boolean;

    function UnIndent(FromPos,ToPos: integer): boolean;
    begin
      Result:=true;
      FromPos:=Max(FromPos,DeleteHeaderEndPos);
      ToPos:=Min(ToPos,DeleteFooterStartPos);
      if FromPos>=ToPos then exit;
      if IndentWith>=IndentInnerWith then exit;
      // unindent
      FromPos:=FindLineEndOrCodeAfterPosition(FromPos,true,true);
      //debugln(['UnIndent FromPos=',CleanPosToStr(FromPos),' ToPos=',CleanPosToStr(ToPos),' Src="',dbgstr(Src,FromPos,ToPos),'"']);
      if not SourceChangeCache.IndentBlock(FromPos,ToPos,IndentWith-IndentInnerWith)
      then begin
        debugln(['UnindentAndEncloseSkippedCode.UnIndent failed: ']);
        exit(false);
      end;
    end;

  var
    p: Integer;
    EndPos: Integer;
    WithHeader: String;
    InsertPos: Integer;
    WithFooter: String;
    StartPos: Integer;
  begin
    // enclose all $ELSE code in WITH blocks
    Result:=false;
    WithHeader:='';
    WithFooter:='';
    p:=Max(StatementNode.StartPos,BeginKeyWord.EndPos);
    EndPos:=StatementNode.EndPos;
    if EndPos>SrcLen then EndPos:=SrcLen;
    StartPos:=p;
    while (p<EndPos) do begin
      if (Src[p]='{') and (Src[p+1]=#3) then begin
        if not Unindent(StartPos,p) then exit;
        // start of skipped code
        if WithHeader='' then begin
          // Header: WITH <var> DO [BEGIN]
          WithHeader:=ExtractCode(WithVarNode.StartPos,WithVarEndPos,[]);
          if NeedBrackets(WithVarNode.StartPos,WithVarEndPos) then
            WithHeader:='('+WithHeader+')';
          WithHeader:=GetAtom(WithKeyWord)+' '+WithHeader+' '+GetAtom(DoKeyWord)+' ';
          if BeginKeyWord.StartPos>0 then
            WithHeader+=GetAtom(BeginKeyWord)
          else
            WithHeader+=Beauty.BeautifyKeyWord('begin');
        end;
        InsertPos:=FindLineEndOrCodeAfterPosition(p+2);
        //debugln(['EncloseSkippedCode Header=',dbgstr(WithHeader)]);
        if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
          Beauty.GetIndentStr(IndentWith)+WithHeader)
        then
          exit(false);
        p:=FindCommentEnd(Src,p,Scanner.NestedComments);
        // end of skipped code
        InsertPos:=p-2;
        if WithFooter='' then begin
          // Footer: END;
          if EndKeyWord.StartPos>0 then
            WithFooter:=GetAtom(EndKeyWord)
          else
            WithFooter:=Beauty.BeautifyKeyWord('end');
          WithFooter+=';';
        end;
        //debugln(['EncloseSkippedCode Footer=',dbgstr(WithFooter)]);
        if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
          Beauty.GetIndentStr(IndentWith)+WithFooter)
        then
          exit(false);
        StartPos:=p;
      end;
      inc(p);
    end;
    Result:=Unindent(StartPos,p);
  end;

var
  CleanPos: integer;
  LastAtom: TAtomPosition;
  i: Integer;
  Cache: PWithVarCache;
begin
  Result:=false;
  WithIdentifiers:=nil;
  WithVarCache:=nil;
  BuildTreeAndGetCleanPos(CursorPos,CleanPos);
  WithVarNode:=FindDeepestNodeAtPos(CleanPos,true);
  if WithVarNode.Desc<>ctnWithVariable then begin
    debugln(['TExtractProcTool.RemoveWithBlock cursor not at a with variable, but ',WithVarNode.DescAsString]);
    exit;
  end;
  StatementNode:=WithVarNode;
  while (StatementNode<>nil) and (StatementNode.FirstChild=nil) do
    StatementNode:=StatementNode.NextBrother;
  if StatementNode=nil then begin
    debugln(['TExtractProcTool.RemoveWithBlock missing statement']);
    exit;
  end;
  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  // parse block
  WithVarEndPos:=FindEndOfTerm(WithVarNode.StartPos,false,true);
  MoveCursorToCleanPos(WithVarEndPos);
  ReadNextAtom;
  try
    repeat
      LastAtom:=CurPos;
      ReadNextAtom;
      if AtomIsIdentifier and (LastAtom.Flag<>cafPoint) then begin
        LastAtom:=CurPos;
        CheckIdentifierAtCursor;
        // restore cursor
        MoveCursorToAtomPos(LastAtom);
      end;
    until (CurPos.StartPos>SrcLen) or (CurPos.StartPos>=StatementNode.EndPos);
    {$IFDEF CTDEBUG}
    debugln(['TExtractProcTool.RemoveWithBlock Statement=',copy(Src,StatementNode.StartPos,StatementNode.EndPos-StatementNode.StartPos)]);
    {$ENDIF}

    // RemoveWithHeader
    SourceChangeCache.MainScanner:=Scanner;
    if not FindBounds then begin
      debugln(['TExtractProcTool.RemoveWithBlock FindBounds failed']);
      exit;
    end;
    if not RemoveWithHeader then begin
      debugln(['TExtractProcTool.RemoveWithBlock RemoveWithHeader failed']);
      exit;
    end;
    if not UnindentAndEncloseSkippedCode then begin
      debugln(['TExtractProcTool.RemoveWithBlock UnindentAndEncloseSkippedCode failed']);
      exit;
    end;
    if not PrefixSubIdentifiers then begin
      debugln(['TExtractProcTool.RemoveWithBlock PrefixSubIdentifiers failed']);
      exit;
    end;

    Result:=SourceChangeCache.Apply;
    //debugln(['TExtractProcTool.RemoveWithBlock SOURCE:']);
    //debugln(TCodeBuffer(Scanner.MainCode).Source);
  finally
    WithIdentifiers.Free;
    if WithVarCache<>nil then begin
      for i:=0 to WithVarCache.Count-1 do begin
        Cache:=PWithVarCache(WithVarCache[i]);
        Dispose(Cache);
      end;
      WithVarCache.Free;
    end;
  end;
end;

function TExtractProcTool.AddWithBlock(const StartPos, EndPos: TCodeXYPosition;
  const WithExpr: string; Candidates: TStrings;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  CleanStartPos: integer;
  CleanEndPos: integer;
  StartNode: TCodeTreeNode;
  Beauty: TBeautifyCodeOptions;

  function Add(IdentifierStart, IdentifierEnd: integer;
    const Identifier: string): boolean;
  var
    i: Integer;
  begin
    Result:=true;
    if (IdentifierStart<CleanStartPos) or (IdentifierEnd>CleanEndPos) then
      exit;
    if WithExpr<>'' then begin
      if CompareText(Identifier,WithExpr)=0 then begin
        if not SourceChangeCache.Replace(gtNone,gtNone,
          IdentifierStart,IdentifierEnd,'')
        then
          exit(false);
      end;
    end else begin
      if Candidates=nil then exit;
      {$IFDEF VerboseAddWithBlock}
      debugln(['TExtractProcTool.AddWithBlock.Add Candidate="',Identifier,'"']);
      {$ENDIF}
      i:=Candidates.IndexOf(Identifier);
      if i<0 then
        Candidates.AddObject(Identifier,TObject(Pointer(1)))
      else
        Candidates.Objects[i]:=TObject(PtrUInt(Candidates.Objects[i])+1);
    end;
  end;

  function ReadBlock(Code: PAnsiString): boolean;
  var
    LastPos: TAtomPosition;
    Identifier: String;
    StartFlag: TCommonAtomFlag;
    IdentifierStart, aStartPos: Integer;
  begin
    {$IFDEF VerboseAddWithBlock}
    debugln(['TExtractProcTool.AddWithBlock.ReadBlock START Atom=',GetAtom]);
    {$ENDIF}
    Result:=false;
    StartFlag:=CurPos.Flag;
    aStartPos:=CurPos.StartPos;
    while true do begin
      {$IFDEF VerboseAddWithBlock}
      debugln(['  ReadBlock Atom="',GetAtom,'"']);
      {$ENDIF}
      if (CurPos.EndPos>CleanEndPos) or (CurPos.StartPos>SrcLen)
      or (CurPos.StartPos>StartNode.EndPos) then
        break;
      case CurPos.Flag of
      cafRoundBracketOpen,cafEdgedBracketOpen:
        if (CurPos.StartPos>aStartPos) then begin
          // nested brackets
          if not ReadBlock(Code) then exit;
        end;
      cafRoundBracketClose:
        if (StartFlag=cafRoundBracketOpen) then
          break
        else if StartFlag=cafEdgedBracketOpen then
          RaiseCharExpectedButAtomFound(20170421201936,']')
        else
          RaiseStringExpectedButAtomFound(20170421201938,'end');
      cafEdgedBracketClose:
        if (StartFlag=cafEdgedBracketOpen) then
          break
        else if StartFlag=cafRoundBracketOpen then
          RaiseCharExpectedButAtomFound(20170421201942,')')
        else
          RaiseStringExpectedButAtomFound(20170421201946,'end');
      end;
      if AtomIsIdentifier then begin
        LastPos:=LastAtoms.GetPriorAtom;
        if not ((LastPos.Flag in [cafPoint]) or LastAtomIs(0,'^')
          or LastUpAtomIs(0,'INHERITED'))
        then begin
          // start of identifier
          {$IFDEF VerboseAddWithBlock}
          debugln(['  ReadBlock identifier START Atom="',GetAtom,'"']);
          {$ENDIF}
          Identifier:=GetAtom;
          IdentifierStart:=CurPos.StartPos;
          repeat
            ReadNextAtom;
            {$IFDEF VerboseAddWithBlock}
            debugln(['  ReadBlock identifier NEXT Atom="',GetAtom,'" Identifier="',Identifier,'"']);
            {$ENDIF}
            if CurPos.Flag in [cafRoundBracketOpen,cafEdgedBracketOpen] then
            begin
              if not ReadBlock(@Identifier) then exit;
            end else if (CurPos.Flag=cafPoint) then begin
              if not Add(IdentifierStart,CurPos.EndPos,Identifier) then exit;
            end else if AtomIsChar('^') then begin
            end else if AtomIsIdentifier and (LastAtomIs(0,'.')) then begin
            end else begin
              break;
            end;
            Identifier:=Identifier+GetAtom;
          until false;
          {$IFDEF VerboseAddWithBlock}
          debugln(['  ReadBlock identifier END Atom="',GetAtom,'" Identifier="',Identifier,'"']);
          {$ENDIF}
          if Code<>nil then
            Code^:=Code^+Identifier;
          continue;
        end;
      end;
      if Code<>nil then
        Code^:=Code^+GetAtom;
      ReadNextAtom;
    end;
    {$IFDEF VerboseAddWithBlock}
    debugln(['ReadBlock END Atom="',GetAtom,'"']);
    {$ENDIF}
    Result:=true;
  end;

var
  Code: String;
  Indent: Integer;
begin
  Result:=false;
  if not CheckIfRangeOnSameLevel(StartPos,EndPos,CleanStartPos,CleanEndPos,
                                 StartNode) then exit;
  {$IFDEF VerboseAddWithBlock}
  debugln(['TExtractProcTool.AddWithBlock ',SrcLen,' ',CleanStartPos,' ',CleanEndPos]);
  debugln(['TExtractProcTool.AddWithBlock Src="',copy(Src,CleanStartPos,CleanEndPos-CleanStartPos),'"']);
  {$ENDIF}
  MoveCursorToNodeStart(StartNode);
  if WithExpr<>'' then
    SourceChangeCache.MainScanner:=Scanner;
  ReadNextAtom;
  if not ReadBlock(nil) then exit;

  // ToDo: check if identifiers are variables

  Beauty:=SourceChangeCache.BeautifyCodeOptions;
  if WithExpr<>'' then begin
    // add 'with expr do begin'
    Indent:=Beauty.GetLineIndent(Src,CleanStartPos);
    Code:='with '+WithExpr+' do begin';
    Code:=Beauty.BeautifyStatement(Code,Indent);
    {$IFDEF VerboseAddWithBlock}
    debugln(['TExtractProcTool.AddWithBlock Header=',Code]);
    {$ENDIF}
    if not SourceChangeCache.Replace(gtNewLine,gtNewLine,
      CleanStartPos,CleanStartPos,Code) then exit;
    // add 'end;'
    Code:='end;';
    Code:=Beauty.BeautifyStatement(Code,Indent);
    {$IFDEF VerboseAddWithBlock}
    debugln(['TExtractProcTool.AddWithBlock Footer=',Code]);
    {$ENDIF}
    if not SourceChangeCache.Replace(gtNewLine,gtNewLine,
      CleanEndPos,CleanEndPos,Code) then exit;
    // indent all between
    {$IFDEF VerboseAddWithBlock}
    debugln(['TExtractProcTool.AddWithBlock Indent...']);
    {$ENDIF}
    if not SourceChangeCache.IndentBlock(CleanStartPos,CleanEndPos,
      Beauty.Indent) then exit;
    {$IFDEF VerboseAddWithBlock}
    debugln(['TExtractProcTool.AddWithBlock Apply']);
    {$ENDIF}
    if not SourceChangeCache.Apply then exit;
  end;
  Result:=true;
end;

procedure TExtractProcTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
end;

function TExtractProcTool.ScanNodesForVariables(const StartPos,
  EndPos: TCodeXYPosition; out BlockStartPos, BlockEndPos: integer;
  out BlockNode: TCodeTreeNode;
  VarTree: TAVLTree;  // tree of TExtractedProcVariable
  IgnoreIdentifiers: TAVLTree; // tree of PCodeXYPosition
  MissingIdentifiers: TAVLTree// tree of PCodeXYPosition
  ): boolean;
type
  TParameterType = (ptNone, ptConst, ptVar, ptOut, ptNoSpecifier);
var
  {$IFDEF CTDebug}
  s: string;
  {$ENDIF}
  VarCandidates: TAVLTree; // tree of PChar

  procedure ScanForLocalVariables(Node: TCodeTreeNode);
  begin
    if Node=nil then exit;
    if Node.Desc=ctnVarDefinition then begin
      VarCandidates.Add(@Src[Node.StartPos]);
    end;
    Node:=Node.FirstChild;
    while Node<>nil do begin
      ScanForLocalVariables(Node);
      Node:=Node.NextBrother;
    end;
  end;

  procedure AddVariableToTree(VarNode: TCodeTreeNode; IsInSelection,
    IsAfterSelection, IsChanged: boolean; ParameterType: TParameterType);
  var
    AVLNode: TAVLTreeNode;
    ProcVar: TExtractedProcVariable;
  begin
    {$IFDEF CTDebug}
    WriteStr(s, ParameterType);
    DebugLn(['AddVariableToTree A Ident=',GetIdentifier(@Src[VarNode.StartPos]),
      ' IsInSelection=',dbgs(IsInSelection),' ParameterType=',s]);
    {$ENDIF}
    if VarTree=nil then exit;
    
    AVLNode:=VarTree.FindKey(VarNode,TListSortCompare(@CompareNodeWithExtractedProcVariable));
    if AVLNode<>nil then begin
      ProcVar:=TExtractedProcVariable(AVLNode.Data);
    end else begin
      ProcVar:=TExtractedProcVariable.Create;
      ProcVar.Node:=VarNode;
      ProcVar.Tool:=Self;
    end;
    ProcVar.ReadInSelection:=ProcVar.ReadInSelection or IsInSelection;
    ProcVar.WriteInSelection:=ProcVar.WriteInSelection
                              or (IsInSelection and IsChanged);
    ProcVar.UsedInNonSelection:=ProcVar.UsedInNonSelection
                              or (not IsInSelection) or (ParameterType<>ptNone);
    if (not ProcVar.ReadAfterSelectionValid) then begin
      // a) variable is a var or out parameter
      //    => the variable value IS needed after the extracted proc
      // b) just after the selection the variable is read
      //    => the variable value IS needed after the extracted proc
      // c) just after the selection the variable is written
      //    => the variable value IS NOT needed after the extracted proc
      if (ParameterType in [ptOut,ptVar]) then begin
        ProcVar.ReadAfterSelectionValid:=true;
        ProcVar.ReadAfterSelection:=true;
      end else if (not IsInSelection) and IsAfterSelection then begin
        ProcVar.ReadAfterSelectionValid:=true;
        ProcVar.ReadAfterSelection:=not IsChanged;
      end;
    end;
    if AVLNode=nil then begin
      if ParameterType<>ptNone then
        ProcVar.VarType:=epvtParameter
      else
        ProcVar.VarType:=epvtLocalVar;
      VarTree.Add(ProcVar);
    end;
  end;

  function VariableIsChanged(VarStartPos: integer): boolean;
  begin
    Result:=false;
    MoveCursorToCleanPos(VarStartPos);
    // read identifier
    ReadNextAtom;
    if CurPos.Flag in [cafRoundBracketOpen] then
      ReadTilBracketClose(true);
    // read next atom
    ReadNextAtom;
    if AtomIs(':=') or AtomIs('+=') or AtomIs('-=') or AtomIs('*=')
    or AtomIs('/=') then begin
      Result:=true;
      exit;
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
    IsChanged: Boolean;
    IsAfterSelection: Boolean;
    ParameterType: TParameterType;
    NewCodePos: TCodeXYPosition;
  begin
    Result:=false;

    // check if there is a local variable with this name
    if VarCandidates.Find(@Src[CurPos.StartPos])=nil then exit(true);

    // now do a real search

    // find start of variable
    VarStartPos:=FindStartOfTerm(CurPos.StartPos,false);
    if (IgnoreIdentifiers<>nil) then begin
      if not CleanPosToCaret(VarStartPos,NewCodePos) then exit;
      if IgnoreIdentifiers.Find(@NewCodePos)<>nil then exit(true);
    end;
    
    IsInSelection:=(VarStartPos>=BlockStartPos) and (VarStartPos<BlockEndPos);
    IsAfterSelection:=(VarStartPos>=BlockEndPos);
    MoveCursorToCleanPos(VarStartPos);
    VarNode:=FindDeepestNodeAtPos(VarStartPos,true);
    Params:=TFindDeclarationParams.Create(Self, VarNode);
    try
      // find declaration
      Params.ContextNode:=VarNode;
      Params.Flags:=[fdfSearchInParentNodes,fdfExceptionOnNotFound,
                     fdfTopLvlResolving,fdfSearchInAncestors,fdfSearchInHelpers];
      Params.SetIdentifier(Self,@Src[VarStartPos],@CheckSrcIdentifier);
      {$IFDEF CTDebug}
      DebugLn('AddVariableAtCursor Searching ',GetIdentifier(Params.Identifier));
      {$ENDIF}
      try
        FindDeclarationOfIdentAtParam(Params);
      except
        on E: ECodeToolError do begin
          {$IFDEF CTDebug}
          DebugLn('AddVariableAtCursor identifier not found ',GetIdentifier(@Src[VarStartPos]));
          {$ENDIF}
          if MissingIdentifiers=nil then
            raise;
          // collect missing identifiers
          if not CleanPosToCaret(VarStartPos,NewCodePos) then exit;
          AddCodePosition(MissingIdentifiers,NewCodePos);
          Result:=true;
          exit;
        end;
      end;
      // check if declaration is local variable
      if (Params.NewCodeTool=Self) and (Params.NewNode<>nil) then begin
        VarNode:=Params.NewNode;
        if (VarNode.Desc=ctnVarDefinition)
        and (VarNode.HasAsParent(BlockNode)) then begin
          // Now we know: VarNode is a variable defined in the main proc
          // or one of its sub procs
          ClosestProcNode:=VarNode.GetNodeOfType(ctnProcedure);
          if ClosestProcNode=BlockNode then begin
            // VarNode is a variable defined by the main proc
            IsParameter:=VarNode.GetNodeOfType(ctnProcedureHead)<>nil;
            ParameterType:=ptNone;
            if IsParameter then begin
              MoveCursorToParameterSpecifier(VarNode);
              if UpAtomIs('CONST') then
                ParameterType:=ptConst
              else if UpAtomIs('VAR') then
                ParameterType:=ptVar
              else if UpAtomIs('OUT') and (cmsOut in Scanner.CompilerModeSwitches) then
                ParameterType:=ptOut
              else
                ParameterType:=ptNoSpecifier;
            end;
            IsChanged:=VariableIsChanged(VarStartPos);
            AddVariableToTree(VarNode,IsInSelection,IsAfterSelection,IsChanged,
                              ParameterType);
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
    DebugLn('TExtractProcTool.ScanSourceForVariables A "',copy(Src,CleanStartPos,CleanEndPos-CleanStartPos),'"');
    {$ENDIF}
    MoveCursorToNearestAtom(CleanStartPos);
    while CurPos.StartPos<CleanEndPos do begin
      LastAtomType:=CurPos.Flag;
      ReadNextAtom;
      if AtomIsIdentifier and (LastAtomType<>cafPoint) then begin
        // this could be the start of a variable -> check
        {$IFDEF CTDebug}
        DebugLn('ScanSourceForVariables B Identifier=',GetAtom);
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

  function ScanNodesForVariablesRecursive(StartNode: TCodeTreeNode): boolean;
  // scan recursively all statements for variables
  var
    ChildNode: TCodeTreeNode;
  begin
    {$IFDEF CTDebug}
    DebugLn('ScanNodesForVariablesRecursive A Node=',StartNode.DescAsString);
    {$ENDIF}
    Result:=false;
    ChildNode:=StartNode.FirstChild;
    while ChildNode<>nil do begin
      if (ChildNode.Desc in [ctnBeginBlock,ctnAsmBlock]) then begin
        if not ScanSourceForVariables(ChildNode.StartPos,ChildNode.EndPos) then
          exit;
      end else if not ScanNodesForVariablesRecursive(ChildNode) then
        exit;
      ChildNode:=ChildNode.NextBrother;
    end;
    Result:=true;
  end;

begin
  Result:=false;
  ActivateGlobalWriteLock;
  VarCandidates:=TAVLTree.Create(@CompareIdentifierPtrs);
  try
    if CaretToCleanPos(StartPos,BlockStartPos)<>0 then exit;
    if CaretToCleanPos(EndPos,BlockEndPos)<>0 then exit;
    BuildSubTree(BlockStartPos);
    BlockNode:=FindDeepestNodeAtPos(BlockStartPos,true);
    while BlockNode<>nil do begin
      if BlockNode.Desc in [ctnInitialization,ctnFinalization,ctnProcedure]
      then break;
      if (BlockNode.Desc=ctnBeginBlock)
      and (BlockNode.Parent.Desc in AllSourceTypes) then
        break;
      BlockNode:=BlockNode.Parent;
    end;

    if BlockNode=nil then begin
      debugln(['TExtractProcTool.ScanNodesForVariables invalid context ',FindDeepestNodeAtPos(BlockStartPos,false).DescAsString]);
      exit;
    end;

    // collect local variables to speed up search
    ScanForLocalVariables(BlockNode);

    if not ScanNodesForVariablesRecursive(BlockNode) then exit;
  finally
    VarCandidates.Free;
    DeactivateGlobalWriteLock;
  end;
  Result:=true;
end;

function TExtractProcTool.CheckIfRangeOnSameLevel(const StartPos,
  EndPos: TCodeXYPosition; out CleanStartPos, CleanEndPos: integer; out
  StartNode: TCodeTreeNode): boolean;
var
  BeginBlockNode: TCodeTreeNode;
  BlockCleanStart: Integer;
  BlockCleanEnd: Integer;
begin
  Result:=false;
  {$IFDEF CTDebug}
  DebugLn('TExtractProcTool.CheckIfRangeOnSameLevel syntax and cursor check ..');
  {$ENDIF}
  CleanStartPos:=0;
  CleanEndPos:=0;
  StartNode:=nil;
  // check syntax
  BuildTreeAndGetCleanPos(StartPos,CleanStartPos);
  if CaretToCleanPos(EndPos,CleanEndPos)<>0 then exit;
  if CleanStartPos>=CleanEndPos then exit;
  {$IFDEF CTDebug}
  debugln('TExtractProcTool.CheckIfRangeOnSameLevel Selection="',copy(Src,CleanStartPos,CleanEndPos-CleanStartPos),'"');
  DebugLn('TExtractProcTool.CheckIfRangeOnSameLevel node check ..');
  {$ENDIF}
  // check if in a Begin..End block
  StartNode:=FindDeepestNodeAtPos(CleanStartPos,true);
  if StartNode=nil then exit;
  BeginBlockNode:=StartNode.GetNodeOfType(ctnBeginBlock);
  if BeginBlockNode=nil then exit;
  {$IFDEF CTDebug}
  DebugLn('TExtractProcTool.CheckIfRangeOnSameLevel Start/End check ..');
  {$ENDIF}
  // check if Start and End on same block level
  MoveCursorToNodeStart(StartNode);
  // check every block in selection
  while true do begin
    ReadNextAtom;
    if (CurPos.EndPos>CleanEndPos) or (CurPos.StartPos>SrcLen)
    or (CurPos.StartPos>StartNode.EndPos) then
      exit(true);
    //debugln('TExtractProcTool.CheckIfRangeOnSameLevel A "',GetAtom,'"');
    if WordIsBlockStatementStart.DoItCaseInsensitive(Src,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      //debugln('TExtractProcTool.CheckIfRangeOnSameLevel WordIsBlockStatementStart "',GetAtom,'"');
      BlockCleanStart:=CurPos.StartPos;
      if not ReadTilBlockStatementEnd(true) then exit;
      BlockCleanEnd:=CurPos.EndPos;
      //debugln(copy(Src,BlockCleanStart,BlockCleanEnd-BlockCleanStart));
      //debugln('TExtractProcTool.CheckIfRangeOnSameLevel BlockEnd "',GetAtom,'" BlockCleanEnd=',dbgs(BlockCleanEnd),' CleanEndPos=',dbgs(CleanEndPos),' Result=',dbgs(Result),' BlockStartedInside=',dbgs(BlockCleanStart>=CleanStartPos));
      if BlockCleanStart<CleanStartPos then begin
        // this block started outside the selection
        // -> it should end outside
        if (BlockCleanEnd>=CleanStartPos) and (BlockCleanEnd<CleanEndPos) then
        begin
          // block overlaps selection
          exit;
        end;
        if BlockCleanEnd>=CleanEndPos then begin
          // set cursor back to block start
          MoveCursorToCleanPos(BlockCleanStart);
          ReadNextAtom;
        end;
      end else begin
        // this block started inside the selection
        // -> it should end inside
        if (BlockCleanEnd>CleanEndPos) then begin
          // block overlaps selection
          exit;
        end;
      end;
      //debugln('TExtractProcTool.CheckIfRangeOnSameLevel Block ok');
    end
    else if WordIsBlockStatementEnd.DoItCaseInsensitive(Src,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // a block ended inside, that started outside
      exit;
    end
    else if WordIsBlockStatementMiddle.DoItCaseInsensitive(Src,
      CurPos.StartPos,CurPos.EndPos-CurPos.StartPos)
    then begin
      // a block ended inside, that started outside
      exit;
    end;
  end;
end;

end.

