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
    TMethodJumpingCodeTool enhances TCodeTemplatesTool with functions to jump
    between a method definition and its body and a forward procedure and its
    body.

}
unit MethodJumpTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{off $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils,
  // Codetools
  FileProcs, CodeTree, CodeToolsStrConsts, PascalParserTool, StdCodeTools,
  KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, PascalReaderTool;


type

  { TMethodJumpingCodeTool }

  TMethodJumpingCodeTool = class(TStandardCodeTool)
  protected
    procedure RemoveCorrespondingProcNodes(Tree1, Tree2: TAVLTree;
        KeepTree1: boolean);
    procedure IntersectProcNodes(Tree1, Tree2: TAVLTree; AddLink: boolean);
    function FindProcNodeInTreeWithName(ATree: TAVLTree;
        const UpperProcName: string): TCodeTreeNode;
    function FindAVLNodeWithNode(AVLTree: TAVLTree;
        Node: TCodeTreeNode): TAVLTreeNode;
  public
    function FindJumpPoint(CursorPos: TCodeXYPosition;
        out NewPos: TCodeXYPosition; out NewTopLine, BlockTopLine, BlockBottomLine: integer;
        out RevertableJump: boolean): boolean;
    function FindJumpPointInProcNode(ProcNode: TCodeTreeNode;
        out NewPos: TCodeXYPosition; out NewTopLine, BlockTopLine, BlockBottomLine: integer): boolean;
    function GatherProcNodes(StartNode: TCodeTreeNode;
        Attr: TProcHeadAttributes; const FilterClassName: string): TAVLTree;
    function FindFirstDifferenceNode(SearchForNodes, SearchInNodes: TAVLTree;
        var DiffTxtPos: integer): TAVLTreeNode;
    function JumpToMethod(const ProcHead: string; Attr: TProcHeadAttributes;
        var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindProc(const ProcHead: string; Attr: TProcHeadAttributes): TCodeTreeNode;
    function JumpToMethod(const ProcHead: string; Attr: TProcHeadAttributes;
        out NewPos: TCodeXYPosition;
        out NewTopLine, BlockTopLine, BlockBottomLine: integer): boolean;
    function FindNodeExtInTree(ATree: TAVLTree;
        const UpperCode: string): TCodeTreeNodeExtension;
    function CreateSubProcPath(StartNode: TCodeTreeNode;
        Attr: TProcHeadAttributes): TStringList;
    function FindSubProcPath(SubProcPath: TStrings; Attr: TProcHeadAttributes;
        SkipInterface: boolean): TCodeTreeNode;

    function FindJumpPointForLinkerPos(
        const SourceFilename: string; SourceLine: integer;
        const MangledFunction, Identifier: string;
        out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
        
    procedure WriteCodeTreeNodeExtTree(ExtTree: TAVLTree);
    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;


implementation

{ TMethodJumpingCodeTool }

procedure TMethodJumpingCodeTool.RemoveCorrespondingProcNodes(Tree1,
  Tree2: TAVLTree; KeepTree1: boolean);
// removes all nodes from Tree1 and Tree2 that exists in both
// if KeepTree1=true then the equal nodes in Tree1 will not be deleted
var AVLNode1, AVLNode2, OldAVLNode1, OldAVLNode2: TAVLTreeNode;
  cmp: integer;
begin
  AVLNode1:=Tree1.FindLowest;
  AVLNode2:=Tree2.FindLowest;
  while (AVLNode1<>nil) and (AVLNode2<>nil) do begin
    cmp:=CompareCodeTreeNodeExtMethodHeaders(
      TCodeTreeNodeExtension(AVLNode1.Data),
      TCodeTreeNodeExtension(AVLNode2.Data));
    if cmp<0 then
      AVLNode1:=Tree1.FindSuccessor(AVLNode1)
    else if cmp>0 then
      AVLNode2:=Tree2.FindSuccessor(AVLNode2)
    else begin
      // nodes correspond -> remove both nodes
      OldAVLNode1:=AVLNode1;
      AVLNode1:=Tree1.FindSuccessor(AVLNode1);
      if not KeepTree1 then begin
        Tree1.FreeAndDelete(OldAVLNode1);
      end;
      OldAVLNode2:=AVLNode2;
      AVLNode2:=Tree2.FindSuccessor(AVLNode2);
      Tree2.FreeAndDelete(OldAVLNode2);
    end;
  end;
end;

procedure TMethodJumpingCodeTool.IntersectProcNodes(Tree1, Tree2: TAVLTree;
  AddLink: boolean);
var
  AVLNode1, NextAVLNode1, AVLNode2: TAVLTreeNode;
  NodeExt1, NodeExt2: TCodeTreeNodeExtension;
  cmp: integer;
begin
  AVLNode1:=Tree1.FindLowest;
  AVLNode2:=Tree2.FindLowest;
  while AVLNode1<>nil do begin
    NextAVLNode1:=Tree1.FindSuccessor(AVLNode1);
    NodeExt1:=TCodeTreeNodeExtension(AVLNode1.Data);
    if AVLNode2<>nil then begin
      NodeExt2:=TCodeTreeNodeExtension(AVLNode2.Data);
      cmp:=CompareTextIgnoringSpace(NodeExt1.Txt,NodeExt2.Txt,false);
      if cmp<0 then begin
        // node of tree1 does not exist in tree2
        // -> delete
        Tree1.FreeAndDelete(AVLNode1);
      end else if cmp=0 then begin
        // node of tree1 exists in tree2
        if AddLink then
          NodeExt1.Data:=AVLNode2;
        AVLNode2:=Tree2.FindSuccessor(AVLNode2);
      end else begin
        // node of tree2 does not exist in tree1
        // -> skip node of tree2
        AVLNode2:=Tree2.FindSuccessor(AVLNode2);
        continue;
      end;
    end else begin
      // node of tree1 does not exist in tree2
      // -> delete
      Tree1.FreeAndDelete(AVLNode1);
    end;
    AVLNode1:=NextAVLNode1;
  end;
end;

function TMethodJumpingCodeTool.FindProcNodeInTreeWithName(ATree: TAVLTree;
  const UpperProcName: string): TCodeTreeNode;
var AnAVLNode: TAVLTreeNode;
begin
  AnAVLNode:=ATree.FindLowest;
  while AnAVLNode<>nil do begin
    Result:=TCodeTreeNodeExtension(AnAVLNode.Data).Node;
    if (ExtractProcName(Result,[phpWithoutClassName,phpInUpperCase])=
      UpperProcName) then
    begin
      // proc body found
      exit;
    end;
    AnAVLNode:=ATree.FindSuccessor(AnAVLNode);
  end;
  Result:=nil;
end;

function TMethodJumpingCodeTool.FindAVLNodeWithNode(AVLTree: TAVLTree;
  Node: TCodeTreeNode): TAVLTreeNode;
begin
  if (AVLTree=nil) or (Node=nil) then begin
    Result:=nil;
    exit;
  end;
  Result:=AVLTree.FindLowest;
  while (Result<>nil) and (TCodeTreeNodeExtension(Result.Data).Node<>Node) do
    Result:=AVLTree.FindSuccessor(Result);
end;

function TMethodJumpingCodeTool.FindJumpPoint(CursorPos: TCodeXYPosition; out
  NewPos: TCodeXYPosition; out NewTopLine, BlockTopLine,
  BlockBottomLine: integer; out RevertableJump: boolean): boolean;

const
  JumpToProcAttr = [phpInUpperCase,phpWithoutClassName,phpWithVarModifiers,
                    phpWithParameterNames,phpWithResultType];

  function JumpToProc(
    FromProcNode: TCodeTreeNode; FromProcAttr: TProcHeadAttributes;
    ToProcNode: TCodeTreeNode; ToProcAttr: TProcHeadAttributes): boolean;
  // compare both proc heads
  // if there is a difference then jump to the difference
  // if there is a body then jump to the body
  // else jump to the proc name
  var
    FromProcHead, ToProcHead: string;
    DiffPos: integer;
  begin
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc A ',dbgs(FromProcNode<>nil),' ',dbgs(ToProcNode<>nil));
    debugln(['  JumpToProc FromProcAttr=[',dbgs(FromProcAttr),']']);
    debugln(['  JumpToProc ToProcAttr=[',dbgs(ToProcAttr),']']);
    {$ENDIF}
    FromProcHead:=ExtractProcHead(FromProcNode,FromProcAttr);
    ToProcHead:=ExtractProcHead(ToProcNode,ToProcAttr);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc B FromProcHead="',FromProcHead,'"',
    ' ToProcHead="',ToProcHead,'"');
    {$ENDIF}
    // search for difference in filtered proc headers
    DiffPos:=1;
    while (DiffPos<=length(FromProcHead)) and (DiffPos<=length(ToProcHead))
    and (FromProcHead[DiffPos]=ToProcHead[DiffPos]) do
      inc(DiffPos);
    if (DiffPos>length(ToProcHead)) and (DiffPos<=length(FromProcHead)) then
      DiffPos:=length(ToProcHead);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc C DiffPos=',dbgs(DiffPos),' length(ToProcHead)=',dbgs(length(ToProcHead)));
    {$ENDIF}
    if DiffPos<=length(ToProcHead) then begin
      // procs differ -> search difference in code
      ExtractSearchPos:=DiffPos;
      try
        ExtractProcHead(ToProcNode,ToProcAttr);
        DiffPos:=ExtractFoundPos;
      finally
        ExtractSearchPos:=-1;
      end;
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc D CleanDiffPos=',dbgs(DiffPos));
      {$ENDIF}
      Result:=JumpToCleanPos(DiffPos,ToProcNode.StartPos,ToProcNode.EndPos,
                             NewPos,NewTopLine,BlockTopLine,BlockBottomLine,true);
    end else begin
      // procs are equal
      if (ToProcNode.LastChild.Desc=ctnBeginBlock) then begin
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc E proc has body');
        {$ENDIF}
        // proc has a body -> jump to start of body
        Result:=FindJumpPointInProcNode(ToProcNode,NewPos,NewTopLine,BlockTopLine,BlockBottomLine);
      end else begin
        // proc has no body -> jump to proc name
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc F proc has no body');
        {$ENDIF}
        Result:=JumpToCleanPos(ToProcNode.FirstChild.StartPos,
                               ToProcNode.StartPos,ToProcNode.EndPos,NewPos,
                               NewTopLine,BlockTopLine,BlockBottomLine,false);
      end;
      RevertableJump:=true;
    end;
  end;
  
  function FindBestProcNode(
    SearchForProcNode: TCodeTreeNode; SearchForProcAttr: TProcHeadAttributes;
    StartNode: TCodeTreeNode; SearchInProcAttr: TProcHeadAttributes;
    SearchAlsoDifferentParamList: boolean): boolean;
  // search first for proc node with same name and param list and jump,
  // if this fails:
  //   search for a proc node with same name and jump to difference in param list
  // returns true if jumped, false if no target proc found
  var
    SearchedProcHead: TPascalMethodHeader;
    ProcNode: TCodeTreeNode;
  begin
    Result:=false;
    if SearchForProcNode=nil then exit;
    SearchedProcHead:=ExtractProcHeadWithGroup(SearchForProcNode,SearchForProcAttr);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.FindBestProcNode Searching ',SearchForProcNode.DescAsString,' "',SearchedProcHead.Name,'" ',ProcHeadAttributesToStr(SearchForProcAttr));
    {$ENDIF}
    if SearchedProcHead.Name='' then exit;
    ProcNode:=FindProcNode(StartNode,SearchedProcHead,SearchInProcAttr);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.FindBestProcNode Found:',dbgs(ProcNode<>nil));
    {$ENDIF}
    if ProcNode<>nil then begin
      Result:=JumpToProc(SearchForProcNode,JumpToProcAttr,
                         ProcNode,JumpToProcAttr);
      exit;
    end;
    // there is no exact corresponding proc
    // -> search for a proc with the same name but different param list
    if not SearchAlsoDifferentParamList then exit;
    SearchForProcAttr:=SearchForProcAttr-[phpWithVarModifiers,
       phpWithParameterNames, phpWithDefaultValues, phpWithResultType,
       phpWithComments];
    SearchForProcAttr:=SearchForProcAttr+[phpWithoutBrackets,
       phpWithoutParamList];
    SearchedProcHead:=ExtractProcHeadWithGroup(SearchForProcNode,SearchForProcAttr);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.FindBestProcNode Searching without params "',SearchedProcHead.Name,'"');
    {$ENDIF}
    if SearchedProcHead.Name='' then exit;
    ProcNode:=FindProcNode(StartNode,SearchedProcHead,SearchForProcAttr);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint.FindBestProcNode Found:',dbgs(ProcNode<>nil));
    {$ENDIF}
    if ProcNode<>nil then begin
      // there is a proc with the same name, but with different parameters
      Result:=JumpToProc(SearchForProcNode,JumpToProcAttr,
                         ProcNode,JumpToProcAttr);
    end;
  end;
  
  
var CursorNode, ClassNode, ProcNode, StartNode, TypeSectionNode,
  ANode: TCodeTreeNode;
  CleanCursorPos, LineStart, LineEnd, FirstAtomStart, LastAtomEnd: integer;
  SearchedClassname, SearchedProcName, SearchedParamList: string;
  SearchForNodes, SearchInNodes: TAVLTree;
  BodyAVLNode, DefAVLNode: TAVLTreeNode;
  ProcName: String;
begin
  Result:=false;
  RevertableJump:=false;
  NewPos:=CursorPos;
  // build code tree
  {$IFDEF CTDEBUG}
  DebugLn('TMethodJumpingCodeTool.FindJumpPoint START  CursorPos=',dbgs(CursorPos.X),',',dbgs(CursorPos.Y));
  {$ENDIF}
  BuildTreeAndGetCleanPos(trTillRange,lsrInitializationStart,
    CursorPos,CleanCursorPos);
  {debugln(['TMethodJumpingCodeTool.FindJumpPoint Clean Src START:']);
  debugln(DbgText(Src));
  debugln(['TMethodJumpingCodeTool.FindJumpPoint Clean Src END']);
  debugln(['TMethodJumpingCodeTool.FindJumpPoint CleanCursorPos=',dbgstr(Src,CleanCursorPos-10,10),'|',dbgstr(Src,CleanCursorPos,10)]);}

  GetLineInfo(CleanCursorPos,LineStart,LineEnd,FirstAtomStart,LastAtomEnd);
  if CleanCursorPos<FirstAtomStart then CleanCursorPos:=FirstAtomStart;
  if CleanCursorPos>=LastAtomEnd then CleanCursorPos:=LastAtomEnd-1;
  if (CleanCursorPos<=SrcLen) and (Src[CleanCursorPos]=';') then begin
    MoveCursorToCleanPos(CleanCursorPos);
    ReadPriorAtom;
    if CurPos.StartPos>=FirstAtomStart then
      CleanCursorPos:=CurPos.StartPos;
  end;
  // find CodeTreeNode at cursor
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true);
  {$IFDEF CTDEBUG}
  DebugLn('TMethodJumpingCodeTool.FindJumpPoint CursorNode=',CursorNode.DescAsString);
  {$ENDIF}
  // first test if in a class
  ClassNode:=CursorNode.GetNodeOfTypes([ctnClass,ctnClassInterface,
      ctnDispinterface,ctnObject,ctnRecordType,
      ctnClassHelper,ctnRecordHelper,ctnTypeHelper,
      ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,
      ctnCPPClass]);
  if ClassNode<>nil then begin
    // cursor is in class/object/interface definition
    // Interfaces have no method bodies, but if the class was refactored it has
    // and then jumping is a nide feature
    // => search in all implemented class procedures for the body
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint ClassNode=',ClassNode.DescAsString);
    {$ENDIF}
    if (ClassNode.SubDesc and ctnsForwardDeclaration)>0 then exit;
    // parse class and build CodeTreeNodes for all properties/methods
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint E ',dbgs(CleanCursorPos),', |',copy(Src,CleanCursorPos,8));
    {$ENDIF}
    TypeSectionNode:=ClassNode.GetTopMostNodeOfType(ctnTypeSection);
    // search the method node under the cursor
    CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,true).
                                                    GetNodeOfType(ctnProcedure);
    if (CursorNode=nil) then exit;
    // search corresponding proc node with same name
    Result:=FindBestProcNode(CursorNode,[phpAddClassName,phpInUpperCase],
                             TypeSectionNode,[phpIgnoreForwards,phpInUpperCase],
                             false);
    {$IFDEF CTDEBUG}
    DebugLn('TMethodJumpingCodeTool.FindJumpPoint F FindBestProcNode=',dbgs(Result));
    {$ENDIF}
    if not Result then begin
      // find the method bodies which are not defined in class
      
      // gather the methods in class
      StartNode:=ClassNode.FirstChild;
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint G Gather method definitions ...');
      {$ENDIF}
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint H Gather SearchForNodes ...');
      {$ENDIF}
      SearchForNodes:=GatherProcNodes(StartNode,
         [phpInUpperCase,phpAddClassname,phpIgnoreProcsWithBody],
         '');
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint I Gather SearchInNodes ...');
      {$ENDIF}
      // gather the method bodies
      SearchInNodes:=GatherProcNodes(TypeSectionNode,
         [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
         ExtractClassName(ClassNode,true,true));
      try
        {$IFDEF CTDEBUG}
        DebugLn(['TMethodJumpingCodeTool.FindJumpPoint J Gather SearchForNodes=',SearchForNodes.Count,' SearchInNodes=',SearchInNodes.Count]);
        {$ENDIF}
        // remove all corresponding methods
        RemoveCorrespondingProcNodes(SearchInNodes,SearchForNodes,false);
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint K DiffMethods found = ',dbgs(SearchInNodes.Count));
        {$ENDIF}
        if SearchInNodes.Count=0 then exit;
        // SearchForNodes now contains all method bodies, which do not have any
        // definition in class
        // -> first search for a method body with the same name
        ProcNode:=FindProcNodeInTreeWithName(SearchInNodes,
              ExtractProcName(CursorNode,[phpWithoutClassName,phpInUpperCase]));
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint H DiffMethod with same name found = ',dbgs(ProcNode<>nil));
        {$ENDIF}
        if (ProcNode=nil) then begin
          // no method body with same name
          // -> take the first different node
          ProcNode:=TCodeTreeNodeExtension(SearchInNodes.FindLowest.Data).Node;
        end;
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint L jump ...');
        {$ENDIF}
        Result:=JumpToProc(CursorNode,JumpToProcAttr,
                           ProcNode,JumpToProcAttr);
      finally
        DisposeAVLTree(SearchForNodes);
        DisposeAVLTree(SearchInNodes);
      end;
    end;
    exit;
  end;
  
  // then test if cursor is in a procedure
  ProcNode:=CursorNode.GetNodeOfType(ctnProcedure);
  {$IFDEF CTDEBUG}
  DebugLn('TMethodJumpingCodeTool.FindJumpPoint Checking if in a proc ... ',dbgs(ProcNode<>nil));
  {$ENDIF}
  while (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) do begin
    if (ProcNode.SubDesc and ctnsForwardDeclaration)>0 then begin
      // forward declaration -> search procedure
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint This is a forward proc ... ');
      {$ENDIF}

      // build the method name + parameter list (without default values)
      Result:=FindBestProcNode(ProcNode,[phpInUpperCase],
                               ProcNode,[phpInUpperCase,phpIgnoreForwards],
                               false);
      if Result then exit;
      
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint Searching left over ... ');
      {$ENDIF}
      // there is no proc with same name and param list
      // gather forward procs
      if (ProcNode.Parent.Desc=ctnImplementation)
      and (ProcNode.Parent.PriorBrother.FirstChild<>nil) then
        StartNode:=ProcNode.Parent.PriorBrother.FirstChild
      else
        StartNode:=ProcNode.Parent.FirstChild;
      SearchForNodes:=GatherProcNodes(StartNode,
         [phpInUpperCase,phpIgnoreProcsWithBody,phpIgnoreMethods],'');

      // gather proc bodies
      SearchInNodes:=GatherProcNodes(StartNode,
         [phpInUpperCase,phpIgnoreForwards,phpIgnoreMethods],'');

      try
        // remove corresponding procs
        RemoveCorrespondingProcNodes(SearchForNodes,SearchInNodes,true);

        //DebugLn('TMethodJumpingCodeTool.FindJumpPoint 2E Unforwarded Body Procs:');
        //WriteCodeTreeNodeExtTree(SearchInNodes);

        // search for a proc body with same name
        // and no corresponding forward proc
        SearchedProcname:=ExtractProcName(ProcNode,[phpInUpperCase]);
        BodyAVLNode:=SearchInNodes.FindLowest;
        while BodyAVLNode<>nil do begin
          ANode:=TCodeTreeNodeExtension(BodyAVLNode.Data).Node;
          if (ANode.StartPos>ProcNode.StartPos)
          and (CompareNodeIdentChars(ANode.FirstChild,SearchedProcname)=0) then
          begin
            // proc body found
            Result:=JumpToProc(ProcNode,JumpToProcAttr,
                               ANode,JumpToProcAttr);
            exit;
          end;
          BodyAVLNode:=SearchInNodes.FindSuccessor(BodyAVLNode);
        end;

        // search for a proc with same param list
        // and no corresponding forward proc
        SearchedParamList:=ExtractProcHead(ProcNode,[phpInUpperCase,
                        phpWithStart,phpWithoutClassKeyword,phpWithoutClassName,
                        phpWithoutName]);
        BodyAVLNode:=SearchInNodes.FindLowest;
        while BodyAVLNode<>nil do begin
          ANode:=TCodeTreeNodeExtension(BodyAVLNode.Data).Node;
          if (ANode.StartPos>ProcNode.StartPos)
          and (CompareTextIgnoringSpace(SearchedParamList,
            ExtractProcHead(ANode,[phpInUpperCase,phpWithStart,
                    phpWithoutClassKeyword,phpWithoutClassName,phpWithoutName]),
                    false)=0) then
          begin
            // proc body found
            Result:=JumpToProc(ProcNode,JumpToProcAttr,
                               ANode,JumpToProcAttr);
            exit;
          end;
          BodyAVLNode:=SearchInNodes.FindSuccessor(BodyAVLNode);
        end;
        
      finally
        DisposeAVLTree(SearchForNodes);
        DisposeAVLTree(SearchInNodes);
      end;
    end else begin
      // procedure is not forward, search on same proc level
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint proc body');
      {$ENDIF}
      SearchedClassname:=ExtractClassNameOfProcNode(ProcNode,true);
      StartNode:=FindFirstNodeOnSameLvl(ProcNode);
      {$IFDEF CTDEBUG}
      DebugLn('TMethodJumpingCodeTool.FindJumpPoint body to decl: ',dbgs(StartNode<>nil),' Class="',SearchedClassName,'"');
      {$ENDIF}
      if StartNode=nil then exit;
      if SearchedClassname<>'' then begin
        // search class node
        ClassNode:=FindClassNode(StartNode,SearchedClassName,true,false);
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint class found: ',dbgs(ClassNode<>nil));
        {$ENDIF}
        if ClassNode=nil then begin
          MoveCursorToProcName(ProcNode,false);
          RaiseExceptionFmt(20170421201402,ctsClassNotFound2, [SearchedClassname]);
        end;
        // search first class grand child node
        StartNode:=ClassNode.FirstChild;
        while (StartNode<>nil) and (StartNode.FirstChild=nil) do
          StartNode:=StartNode.NextBrother;
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint 4D ',dbgs(StartNode<>nil));
        {$ENDIF}
        if StartNode=nil then begin
          ProcName:=ExtractProcName(ProcNode,[]);
          MoveCursorToNodeStart(ClassNode);
          RaiseExceptionFmt(20170421201417,ctsMethodHasNoDeclaration, [ProcName]);
        end;
        // search method with same name and param list
        Result:=FindBestProcNode(ProcNode,[phpWithoutClassName,phpInUpperCase],
                                 StartNode,[phpInUpperCase],false);
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint 4E FindBestProcNode=',dbgs(Result));
        {$ENDIF}
        if Result then exit;
        
        // gather method definitions
        SearchInNodes:=GatherProcNodes(StartNode,
           [phpInUpperCase,phpAddClassname,phpIgnoreProcsWithBody],'');
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint 4F ');
        {$ENDIF}
        // gather method bodies
        TypeSectionNode:=ClassNode.GetTopMostNodeOfType(ctnTypeSection);
        SearchForNodes:=GatherProcNodes(TypeSectionNode,
           [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
           ExtractClassName(ClassNode,true,true));
        try
          // remove corresponding methods
          RemoveCorrespondingProcNodes(SearchForNodes,SearchInNodes,false);
          {$IFDEF CTDEBUG}
          DebugLn('TMethodJumpingCodeTool.FindJumpPoint 4G DiffNodes=',dbgs(SearchInNodes.Count));
          {$ENDIF}
          if SearchInNodes.Count=0 then begin
            ProcName:=ExtractProcName(ProcNode,[]);
            MoveCursorToNodeStart(ClassNode);
            RaiseExceptionFmt(20170421201432,ctsMethodHasNoDeclaration, [ProcName]);
          end;
          // search for a method with same name but different param list
          ProcNode:=FindProcNodeInTreeWithName(SearchInNodes,
                ExtractProcName(ProcNode,[phpWithoutClassName,phpInUpperCase]));
          if ProcNode=nil then begin
            ProcNode:=TCodeTreeNodeExtension(SearchInNodes.FindLowest.Data).Node;
          end;
          Result:=JumpToProc(CursorNode,JumpToProcAttr,ProcNode,JumpToProcAttr);
        finally
          DisposeAVLTree(SearchForNodes);
          DisposeAVLTree(SearchInNodes);
        end;
        exit;
      end else begin
        // search forward procedure
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint 5A searching exact forward proc ...');
        {$ENDIF}
        Result:=FindBestProcNode(ProcNode,[phpInUpperCase],
                             StartNode,[phpInUpperCase,phpIgnoreProcsWithBody],
                             false);
        if Result then exit;
        
        {$IFDEF CTDEBUG}
        DebugLn('TMethodJumpingCodeTool.FindJumpPoint 5B searching similar forward proc');
        {$ENDIF}
        // there is no proc with same name and param list
        // gather forward procs
        if (ProcNode.Parent.Desc=ctnImplementation)
        and (ProcNode.Parent.PriorBrother.FirstChild<>nil) then
          StartNode:=ProcNode.Parent.PriorBrother.FirstChild
        else
          StartNode:=ProcNode.Parent.FirstChild;
        SearchInNodes:=GatherProcNodes(StartNode,
           [phpInUpperCase,phpIgnoreProcsWithBody,phpIgnoreMethods],'');
           
        // gather proc bodies
        SearchForNodes:=GatherProcNodes(StartNode,
           [phpInUpperCase,phpIgnoreForwards,phpIgnoreMethods],'');

        try
          // remove corresponding procs
          RemoveCorrespondingProcNodes(SearchForNodes,SearchInNodes,true);

          //DebugLn('TMethodJumpingCodeTool.FindJumpPoint 5E Forward Procs without body');
          //WriteCodeTreeNodeExtTree(SearchInNodes);

          // search for a forward proc with same name
          // and no corresponding proc body
          SearchedProcname:=ExtractProcName(ProcNode,[phpInUpperCase]);
          DefAVLNode:=SearchInNodes.FindLowest;
          while DefAVLNode<>nil do begin
            ANode:=TCodeTreeNodeExtension(DefAVLNode.Data).Node;
            if (ANode.StartPos<ProcNode.StartPos)
            and (CompareNodeIdentChars(ANode.FirstChild,SearchedProcname)=0)
            then begin
              // proc body found
              Result:=JumpToProc(ProcNode,JumpToProcAttr,
                                 ANode,JumpToProcAttr);
              exit;
            end;
            DefAVLNode:=SearchInNodes.FindSuccessor(DefAVLNode);
          end;

          // search for a forward proc with same param list
          // and no corresponding proc body
          SearchedParamList:=ExtractProcHead(ProcNode,[phpInUpperCase,
                      phpWithStart,phpWithoutClassKeyword,phpWithoutClassName,
                      phpWithoutName]);
          DefAVLNode:=SearchInNodes.FindLowest;
          while DefAVLNode<>nil do begin
            ANode:=TCodeTreeNodeExtension(DefAVLNode.Data).Node;
            if (ANode.StartPos<ProcNode.StartPos)
            and (CompareTextIgnoringSpace(SearchedParamList,
              ExtractProcHead(ANode,[phpInUpperCase,phpWithStart,
                  phpWithoutClassKeyword,phpWithoutClassName,phpWithoutName]),
                  false)=0) then
            begin
              // proc body found
              Result:=JumpToProc(ProcNode,JumpToProcAttr,
                                 ANode,JumpToProcAttr);
              exit;
            end;
            DefAVLNode:=SearchInNodes.FindSuccessor(DefAVLNode);
          end;

        finally
          DisposeAVLTree(SearchForNodes);
          DisposeAVLTree(SearchInNodes);
        end;
      end;
    end;
    if Result then begin
      exit;
    end else begin
      // no proc found
      // -> try parent proc ...
      ProcNode:=ProcNode.Parent;
    end;
  end; //while (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) do begin
end;

function TMethodJumpingCodeTool.FindJumpPointInProcNode(
  ProcNode: TCodeTreeNode; out NewPos: TCodeXYPosition; out NewTopLine,
  BlockTopLine, BlockBottomLine: integer): boolean;
var DestNode: TCodeTreeNode;
  i, NewCleanPos: integer;
  LineStartPos: LongInt;
begin
  Result:=false;
  if ProcNode=nil then exit;
  // search method body
  DestNode:=FindProcBody(ProcNode);
  if DestNode=nil then begin
    // proc without body -> jump to proc node header
    Result:=JumpToCleanPos(ProcNode.FirstChild.StartPos,ProcNode.StartPos,
                           ProcNode.EndPos,NewPos,NewTopLine,false);
    exit;
  end;
  // search good position
  { examples
      begin |end

      asm
      |end

      begin
         |DoSomething;
      end

      asm
        |

      end
  }
  MoveCursorToNodeStart(DestNode);
  // if begin is indented then indent the cursor as well
  i:=0;
  while (CurPos.StartPos-i>1) and (Src[CurPos.StartPos-i-1] in [' ',#8]) do
    inc(i);
  {$IFDEF CTDEBUG}
  DebugLn('[TMethodJumpingCodeTool.FindJumpPointInProcNode] A i=',dbgs(i));
  {$ENDIF}
  if (CurPos.StartPos-i>1) and (not (Src[CurPos.StartPos-i-1] in [#10,#13]))
  then
    i:=0;
  {$IFDEF CTDEBUG}
  DebugLn('[TMethodJumpingCodeTool.FindJumpPointInProcNode] B i=',dbgs(i),' IndentSize=',dbgs(IndentSize));
  {$ENDIF}
  // set cursor in the next line but before the next token/comment
  // read 'begin' or 'asm'
  ReadNextAtom;
  NewCleanPos:=CurPos.EndPos;
  // skip spaces
  while (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [' ',#8]) do
    inc(NewCleanPos);
  if (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [#13,#10]) then begin
    // skip newline chars
    inc(NewCleanPos);
    if (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [#13,#10])
    and (Src[NewCleanPos-1]<>Src[NewCleanPos]) then
      inc(NewCleanPos);
    // check if there is code in the line
    LineStartPos:=NewCleanPos;
    while (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [' ',#8]) do
      inc(NewCleanPos);
    if (NewCleanPos>SrcLen) or (Src[NewCleanPos] in [#10,#13]) then begin
      // empty line
      inc(i,IndentSize);
      if NewCleanPos>LineStartPos+i then
        NewCleanPos:=LineStartPos+i
      else if NewCleanPos<LineStartPos+i then
        i:=(LineStartPos+i)-NewCleanPos;
    end else begin
      // code in line
      i:=0;
    end;
  end else
    i:=0;
  if NewCleanPos>SrcLen then begin
    NewCleanPos:=SrcLen;
    inc(i);
  end;
  
  if not JumpToCleanPos(NewCleanPos,ProcNode.StartPos,ProcNode.EndPos,
                        NewPos,NewTopLine,BlockTopLine, BlockBottomLine,true)
  then exit;
  if CursorBeyondEOL then
    inc(NewPos.x,i);
  Result:=true;
end;

function TMethodJumpingCodeTool.GatherProcNodes(StartNode: TCodeTreeNode;
  Attr: TProcHeadAttributes; const FilterClassName: string): TAVLTree;
// create a tree of TCodeTreeNodeExtension sorted with CompareCodeTreeNodeExt
// Node.Desc = ctnProcedure
// Node.Txt = ExtractProcHead(Node,Attr)
var CurProcName: string;
  ANode: TCodeTreeNode;
  NewNodeExt: TCodeTreeNodeExtension;
  cmp: boolean;
  CurClassName: String;
begin
  //debugln(['TMethodJumpingCodeTool.GatherProcNodes START FilterClassName="',FilterClassName,'" Attr=[',dbgs(Attr),']']);
  Result:=TAVLTree.Create(@CompareCodeTreeNodeExtMethodHeaders);
  if (StartNode=nil) or (StartNode.Parent=nil) then exit;
  ANode:=StartNode;
  while (ANode<>nil) do begin
    //debugln(['TMethodJumpingCodeTool.GatherProcNodes ',ANode.DescAsString]);
    if ANode.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
           and ((ANode.SubDesc and ctnsForwardDeclaration)>0)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(ANode)<>nil))) then
      begin
        //DebugLn('[TMethodJumpingCodeTool.GatherProcNodes] Proc found');
        cmp:=true;
        if (phpOnlyWithClassname in Attr) then begin
          CurClassName:=ExtractClassNameOfProcNode(ANode,true);
          //DebugLn('[TMethodJumpingCodeTool.GatherProcNodes] Proc Class="',CurClassName,'" =? ',FilterClassName,'=Filter');

          if CompareText(FilterClassName,CurClassName)<>0 then
            cmp:=false;
        end;
        if cmp and (phpIgnoreMethods in Attr) then begin
          if (ANode.GetNodeOfTypes([ctnClass,ctnObject,ctnRecordType,ctnClassHelper,ctnRecordHelper,ctnTypeHelper,
                                ctnObjCClass,ctnObjCCategory,ctnCPPClass])<>nil)
          or (ExtractClassNameOfProcNode(ANode,true)<>'')
          then
            cmp:=false;
        end;
        if cmp then begin
          //DebugLn('[TMethodJumpingCodeTool.GatherProcNodes] Proc with right class');
          CurProcName:=ExtractProcHead(ANode,Attr);
          //DebugLn(['[TMethodJumpingCodeTool.GatherProcNodes] Proc with right class, name="',CurProcName,'" phpInUpperCase=',phpInUpperCase in Attr]);
          if (CurProcName<>'') then begin
            NewNodeExt:=TCodeTreeNodeExtension.Create;
            with NewNodeExt do begin
              Node:=ANode;
              Txt:=CurProcName;
              Flags:=Ord(ExtractProcedureGroup(ANode));
              if TPascalMethodGroup(Flags)=mgClassOperator then
                ExtTxt4:=ExtractFuncResultType(ANode,Attr);
            end;
            Result.Add(NewNodeExt);
          end;
        end;
      end;
    end;
    // next node
    if (ANode.FirstChild<>nil)
    and (ANode.Desc in (AllClassSections+[ctnImplementation])) then
      ANode:=ANode.FirstChild
    else begin
      while ANode.NextBrother=nil do begin
        ANode:=ANode.Parent;
        if ANode=nil then break;
        if not (ANode.Desc in (AllClassSections+[ctnImplementation])) then
          break;
      end;
      if ANode=nil then break;
      ANode:=ANode.NextBrother;
    end;
  end;
  //debugln(['TMethodJumpingCodeTool.GatherProcNodes END']);
end;

function TMethodJumpingCodeTool.FindFirstDifferenceNode(
  SearchForNodes, SearchInNodes: TAVLTree;
  var DiffTxtPos: integer): TAVLTreeNode;
// search the first AVL node in SearchForNodes, that is not in SearchInNodes
var SearchInNode: TAVLTreeNode;
  cmp: integer;
  NodeTxt1, NodeTxt2: string;
  Attr: TProcHeadAttributes;
begin
  Result:=SearchForNodes.FindLowest;
  if Result=nil then exit;
  SearchInNode:=SearchInNodes.FindLowest;
  //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] ',SearchInNode<>nil);

  DiffTxtPos:=-1;
  while (SearchInNode<>nil) do begin
    //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] B ',SearchInNode<>nil);
    cmp:=CompareCodeTreeNodeExt(Result.Data,SearchInNode.Data);
    
    //NodeTxt1:=TCodeTreeNodeExtension(Result.Data).Txt;
    //NodeTxt2:=TCodeTreeNodeExtension(SearchInNode.Data).Txt;
    //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] ',NodeTxt1,' ?',cmp,'= ',NodeTxt2);

    if cmp<0 then begin
      // result node not found in SearchInNodes
      // -> search for first difference
      //NodeTxt1:=TCodeTreeNodeExtension(Result.Data).Txt;
      //NodeTxt2:=TCodeTreeNodeExtension(SearchInNode.Data).Txt;
      Attr:=[phpWithStart, phpWithoutClassName, phpWithVarModifiers,
         phpWithResultType, phpInUpperCase];
      NodeTxt1:=ExtractProcHead(TCodeTreeNodeExtension(Result.Data).Node,Attr);
      NodeTxt2:=ExtractProcHead(TCodeTreeNodeExtension(SearchInNode.Data).Node,
                                Attr);
      //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] C Result=',NodeTxt1);
      //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] C SearchInNode=',NodeTxt2);
      DiffTxtPos:=1;
      while (DiffTxtPos<=length(NodeTxt1)) and (DiffTxtPos<=length(NodeTxt2)) do
      begin
        if NodeTxt1[DiffTxtPos]<>NodeTxt2[DiffTxtPos] then
          break;
        inc(DiffTxtPos);
      end;
      //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] D DiffTxtPos=',DiffTxtPos);
      ExtractSearchPos:=DiffTxtPos;
      try
        ExtractProcHead(TCodeTreeNodeExtension(Result.Data).Node,Attr);
        DiffTxtPos:=ExtractFoundPos;
      finally
        ExtractSearchPos:=-1;
      end;
      //DebugLn('[TMethodJumpingCodeTool.FindFirstDifferenceNode] E DiffTxtPos=',DiffTxtPos);
      exit;
    end else if cmp=0 then begin
      // node found in SearchInNodes -> search next
      Result:=SearchForNodes.FindSuccessor(Result);
      SearchInNode:=SearchInNodes.FindSuccessor(SearchInNode);
      if (Result=nil) or (SearchInNode=nil) then exit;
    end else begin
      // node in SearchInNodes does not exist in SearchForNodes
      // -> ignore and search next
      SearchInNode:=SearchInNodes.FindSuccessor(SearchInNode);
    end;
  end;
end;

function TMethodJumpingCodeTool.FindNodeExtInTree(ATree: TAVLTree;
  const UpperCode: string): TCodeTreeNodeExtension;
var cmp: integer;
  ANode: TAVLTreeNode;
begin
  ANode:=ATree.Root;
  while ANode<>nil do begin
    Result:=TCodeTreeNodeExtension(ANode.Data);
    cmp:=CompareTextIgnoringSpace(UpperCode,Result.Txt,true);
    if cmp<0 then
      ANode:=ANode.Left
    else if cmp>0 then
      ANode:=ANode.Right
    else
      exit;
  end;
  Result:=nil;
end;

function TMethodJumpingCodeTool.CreateSubProcPath(StartNode: TCodeTreeNode;
  Attr: TProcHeadAttributes): TStringList;
var
  ProcHead: String;
begin
  Result:=TStringList.Create;
  while StartNode<>nil do begin
    if StartNode.Desc=ctnProcedure then begin
      ProcHead:=ExtractProcHead(StartNode,Attr);
      Result.Insert(0,ProcHead);
    end;
    StartNode:=StartNode.Parent;
  end;
  //DebugLn('TMethodJumpingCodeTool.CreateSubProcPath END "',Result.Text,'"');
end;

function TMethodJumpingCodeTool.FindSubProcPath(SubProcPath: TStrings;
  Attr: TProcHeadAttributes; SkipInterface: boolean): TCodeTreeNode;
  
  function SearchSubProcPath(StartNode: TCodeTreeNode; PathIndex: integer
    ): TCodeTreeNode;
  var
    ProcHead: string;
    ProcNode: TCodeTreeNode;
  begin
    Result:=nil;
    if (PathIndex>SubProcPath.Count) or (StartNode=nil) then exit;
    ProcHead:=SubProcPath[PathIndex];
    ProcNode:=FindProcNode(StartNode,ProcHead,mgMethod,Attr);
    //DebugLn('TMethodJumpingCodeTool.SearchSubProcPath A ProcHead="',ProcHead,'" Found=',dbgs(ProcNode<>nil));
    if ProcNode=nil then exit;
    if PathIndex=SubProcPath.Count-1 then begin
      Result:=ProcNode;
      exit;
    end;
    Result:=SearchSubProcPath(ProcNode.FirstChild,PathIndex+1);
  end;
  
var
  StartNode: TCodeTreeNode;
begin
  StartNode:=FindFirstSectionChild;
  if SkipInterface and (StartNode<>nil) and (StartNode.Parent<>nil)
  and (StartNode.Parent.Desc=ctnInterface) then begin
    StartNode:=FindImplementationNode;
    if StartNode<>nil then StartNode:=StartNode.FirstChild;
  end;
  //debugln(['TMethodJumpingCodeTool.FindSubProcPath ',StartNode.DescAsString]);
  Result:=SearchSubProcPath(StartNode,0);
end;

function TMethodJumpingCodeTool.FindJumpPointForLinkerPos(
  const SourceFilename: string; SourceLine: integer;
  const MangledFunction, Identifier: string;
  out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
{ Examples:

  MangledFunction:
  
    GTK2_GTK_TYPE_CELL_RENDERER_COMBO$$LONGWORD

      GTK2 is the unit.
      GTK_TYPE_CELL_RENDERER_COMBO is the function or procedure name.
      LONGWORD is the list of parameter types.


    ADDFILETOAPACKAGEDLG_TADDFILETOAPACKAGEDIALOG_$__ADDFILETOAPACKAGEDLGCLOSE$TOBJECT$TCLOSEACTION

      ADDFILETOAPACKAGEDLG is the unit.
      TADDFILETOAPACKAGEDIALOG is the class.
      ADDFILETOAPACKAGEDLGCLOSE is the method name.
      $TOBJECT$TCLOSEACTION is the list of parameter types


    SUBBY
      Unit name and parent procedues are missing.
}
var
  ProcName: String;
  BestProcNode: TCodeTreeNode;
  ProcPos: integer;
  
  function FindFirstIdentifier(const Identifier: string): boolean;
  begin
    ProcPos:=1;
    while (ProcPos<=length(ProcName))
    and (not IsIdentStartChar[ProcName[ProcPos]]) do
      inc(ProcPos);
    Result:=BasicCodeTools.CompareIdentifiers(@ProcName[ProcPos],
                                              PChar(Pointer(Identifier)))=0;
  end;
  
  function FindNextIdentifier(const Identifier: string): boolean;
  begin
    while (ProcPos<=length(ProcName)) and (IsIdentChar[ProcName[ProcPos]]) do
      inc(ProcPos);
    while (ProcPos<=length(ProcName))
    and (not IsIdentStartChar[ProcName[ProcPos]]) do
      inc(ProcPos);
    Result:=BasicCodeTools.CompareIdentifiers(@ProcName[ProcPos],
                                              PChar(Pointer(Identifier)))=0;
  end;
  
  function SearchNode(Node: TCodeTreeNode): boolean;
  var
    CurProcName: String;
    p: LongInt;
    CurClassName: String;
  begin
    Result:=false;
    while Node<>nil do begin
      if Node.Desc=ctnProcedure then begin
        CurProcName:=ExtractProcName(Node,[phpInUpperCase]);
        p:=System.Pos('.',CurProcName);
        if p>0 then begin
          // classname.procname
          CurClassName:=copy(CurProcName,1,p-1);
          CurProcName:=copy(CurProcName,p+1,length(CurProcName));
          if FindFirstIdentifier(CurClassName)
          and FindNextIdentifier(CurProcName) then begin
            // proc found
            BestProcNode:=Node;
            Result:=true;
          end;
        end else begin
          // procname
          if FindFirstIdentifier(CurProcName) then begin
            // proc found
            BestProcNode:=Node;
            Result:=true;
          end;
        end;
      end;
      if Node.Desc in ([ctnImplementation,ctnProcedure]+AllSourceTypes) then
        SearchNode(Node.FirstChild);
      Node:=Node.NextBrother;
    end;
  end;

var
  CurSourceName: String;
  p: LongInt;
  ShortIdentifier: ShortString;
  BestPos: Integer;
  ASrcFilename: String;
  LinkCode: TCodeBuffer;
  Link: TSourceLink;
  i: Integer;
  CurLine: String;
  StartPos, EndPos: integer;
begin
  Result:=false;
  BuildTree(lsrEnd);
  DebugLn(['TMethodJumpingCodeTool.FindJumpPointForLinkerPos ']);

  BestPos:=0;
  ShortIdentifier:=UpperCaseStr(copy(Identifier,1,255));
  
  if (BestPos<1) and (SourceFilename<>'') then begin
    // try to find the source (unit or include file)
    ASrcFilename:=ExtractFileName(SourceFilename);
    i:=0;
    while (i<Scanner.LinkCount) do begin
      Link:=Scanner.Links[i];
      LinkCode:=TCodeBuffer(Link.Code);
      if (LinkCode<>nil)
      and (CompareFilenames(ExtractFilename(LinkCode.Filename),ASrcFilename)=0)
      then begin
        BestPos:=Link.CleanedPos;
        if (SourceLine>0) and (SourceLine<=LinkCode.LineCount) then begin
          // there is a SourceLine => use that
          NewPos.X:=1;
          if Identifier<>'' then begin
            // there is an Identifier => search it in line
            CurLine:=LinkCode.GetLine(SourceLine-1,false);
            EndPos:=1;
            while (EndPos<=length(CurLine)) do begin
              BasicCodeTools.ReadRawNextPascalAtom(CurLine,EndPos,StartPos,
                                                   Scanner.NestedComments,true);
              if (EndPos<=length(CurLine))
              and (CompareIdentifiers(@CurLine[StartPos],PChar(Identifier))=0)
              then begin
                NewPos.X:=StartPos;
                break;
              end;
            end;
          end;
          NewPos.Code:=LinkCode;
          NewPos.Y:=SourceLine;
          NewTopLine:=NewPos.Y-VisibleEditorLines div 2;
          if NewTopLine<1 then NewTopLine:=1;
          Result:=true;
          exit;
        end;
        break;
      end;
      inc(i);
    end;
  end;

  if (BestPos<1) and (MangledFunction<>'') then begin
    // try to find the function
    ProcName:=MangledFunction;
    ProcPos:=1;

    // remove unitname from ProcName
    CurSourceName:=GetSourceName(false);
    if CurSourceName<>'' then begin
      p:=System.Pos('_',ProcName);
      if p>0 then begin
        if CompareIdentifiers(@ProcName[1],PChar(CurSourceName))=0 then begin
          while (p<=length(ProcName)) and (ProcName[p]='_') do inc(p);
          ProcName:=copy(ProcName,p,length(ProcName));
        end;
      end;
    end;

    // find procedure
    BestProcNode:=nil;
    SearchNode(Tree.Root);
    if BestProcNode<>nil then begin
      if Identifier<>'' then begin
        MoveCursorToCleanPos(BestProcNode.StartPos);
        repeat
          ReadNextAtom;
          if (CurPos.StartPos>SrcLen) or (CurPos.StartPos>BestProcNode.EndPos)
          then
            break;
          if UpAtomIs(ShortIdentifier) then begin
            BestPos:=CurPos.StartPos;
            break;
          end;
        until false;
      end else begin
        BestPos:=BestProcNode.StartPos;
      end;
    end;
  end;
  
  if BestPos<1 then exit;
  
  // find jump point
  Result:=JumpToCleanPos(BestPos,-1,-1,NewPos,NewTopLine,false);
end;

procedure TMethodJumpingCodeTool.WriteCodeTreeNodeExtTree(ExtTree: TAVLTree);
var
  AVLNode: TAVLTreeNode;
  ANodeExt: TCodeTreeNodeExtension;
begin
  DebugLn('TMethodJumpingCodeTool.WriteCodeTreeNodeExtTree ExtTree.Count=',DbgS(ExtTree.Count));
  AVLNode:=ExtTree.FindLowest;
  while AVLNode<>nil do begin
    ANodeExt:=TCodeTreeNodeExtension(AVLNode.Data);
    DbgOut('  ');
    if ANodeExt.Node<>nil then begin
      DbgOut('Node=',ANodeExt.Node.DescAsString,' Node.Start=',DbgS(ANodeExt.Node.StartPos));
      DbgOut(' "',StringToPascalConst(copy(Src,ANodeExt.Node.StartPos,30)),'"');
    end else
      DbgOut('Node=nil');
    DbgOut(' Position=',Dbgs(ANodeExt.Position));
    DbgOut(' Txt="',ANodeExt.Txt,'"');
    DbgOut(' ExtTxt1="',ANodeExt.ExtTxt1,'"');
    DbgOut(' ExtTxt2="',ANodeExt.ExtTxt2,'"');
    DebugLn();
    AVLNode:=ExtTree.FindSuccessor(AVLNode);
  end;
end;

procedure TMethodJumpingCodeTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
end;

function TMethodJumpingCodeTool.JumpToMethod(const ProcHead: string;
  Attr: TProcHeadAttributes; out NewPos: TCodeXYPosition; out NewTopLine,
  BlockTopLine, BlockBottomLine: integer): boolean;
var
  ProcNode: TCodeTreeNode;
begin
  ProcNode:=FindProc(ProcHead,Attr);
  if ProcNode=nil then exit(false);
  Result:=FindJumpPointInProcNode(ProcNode,
                                NewPos,NewTopLine,BlockTopLine,BlockBottomLine);
end;

function TMethodJumpingCodeTool.JumpToMethod(const ProcHead: string;
  Attr: TProcHeadAttributes; var NewPos: TCodeXYPosition;
  var NewTopLine: integer): boolean;
var
  BlockTopLine, BlockBottomLine: integer;
begin
  Result := JumpToMethod(ProcHead, Attr, NewPos, NewTopLine, BlockTopLine, BlockBottomLine);
end;

function TMethodJumpingCodeTool.FindProc(const ProcHead: string;
  Attr: TProcHeadAttributes): TCodeTreeNode;
var SectionNode, CurProcNode: TCodeTreeNode;
  CurProcHead: string;
begin
  Result:=nil;
  BuildTree(lsrInitializationStart);
  SectionNode:=Tree.Root;
  while (SectionNode<>nil) do begin
    if SectionNode.Desc in [ctnProgram,ctnImplementation] then begin
      CurProcNode:=SectionNode.FirstChild;
      while CurProcNode<>nil do begin
        if CurProcNode.Desc=ctnProcedure then begin
          CurProcHead:=ExtractProcHead(CurProcNode,Attr);
          debugln(['TMethodJumpingCodeTool.FindProc "',CurProcHead,'"']);
          if CompareTextIgnoringSpace(ProcHead,CurProcHead,false)=0 then
            exit(CurProcNode);
        end;
        CurProcNode:=CurProcNode.NextBrother;
      end;
    end;
    SectionNode:=SectionNode.NextBrother;
  end;
end;

end.
