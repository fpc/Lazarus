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
    TMethodJumpingCodeTool enhances TStandardCodeTool with functions to jump
    between a method definition and its body and a forward procedure and its
    body.

}
unit MethodJumpTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, PascalParserTool, StdCodeTools,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  TypInfo, SourceChanger;

{ $DEFINE CTDEBUG}

type
  TMethodJumpingCodeTool = class(TStandardCodeTool)
  private
    FAdjustTopLineDueToComment: boolean;
  public
    function FindJumpPoint(CursorPos: TCodeXYPosition;
        var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindJumpPointInProcNode(ProcNode: TCodeTreeNode;
        var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function GatherProcNodes(StartNode: TCodeTreeNode;
        Attr: TProcHeadAttributes; const UpperClassName: string): TAVLTree;
    function FindFirstDifferenceNode(SearchForNodes, SearchInNodes: TAVLTree;
        var DiffTxtPos: integer): TAVLTreeNode;
    function JumpToNode(ANode: TCodeTreeNode;
        var NewPos: TCodeXYPosition; var NewTopLine: integer;
        IgnoreJumpCentered: boolean): boolean;
    function JumpToCleanPos(NewCleanPos, NewTopLineCleanPos: integer;
        var NewPos: TCodeXYPosition; var NewTopLine: integer;
        IgnoreJumpCentered: boolean): boolean;
    function FindNodeInTree(ATree: TAVLTree;
        const UpperCode: string): TCodeTreeNodeExtension;
    property AdjustTopLineDueToComment: boolean
        read FAdjustTopLineDueToComment write FAdjustTopLineDueToComment;
  end;


implementation

{ TMethodJumpingCodeTool }

function TMethodJumpingCodeTool.FindJumpPoint(CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;

const
  JumpToProcAttr = [phpInUpperCase,phpTrimSpace,phpWithoutClassName,
      phpWithVarModifiers,phpWithParameterNames,phpWithResultType];

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
writeln('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc A ',FromProcNode<>nil,' ',ToProcNode<>nil);
{$ENDIF}
    FromProcHead:=ExtractProcHead(FromProcNode,FromProcAttr);
    ToProcHead:=ExtractProcHead(ToProcNode,ToProcAttr);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc B FromProcHead="',FromProcHead,'"',
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
writeln('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc C DiffPos=',DiffPos,' length(ToProcHead)=',length(ToProcHead));
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
writeln('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc D CleanDiffPos=',DiffPos);
{$ENDIF}
      Result:=JumpToCleanPos(DiffPos,ToProcNode.StartPos,NewPos,NewTopLine,true);
    end else begin
      // procs are equal
      if (ToProcNode.LastChild.Desc=ctnBeginBlock) then begin
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc E proc has body');
{$ENDIF}
        // proc has a body -> jump to start of body
        Result:=FindJumpPointInProcNode(ToProcNode,NewPos,NewTopLine);
      end else begin
        // proc has no body -> jump to proc name
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint.JumpToProc F proc has no body');
{$ENDIF}
        Result:=JumpToCleanPos(ToProcNode.FirstChild.StartPos,
                               ToProcNode.StartPos,NewPos,NewTopLine,false);
      end;
    end;
  end;
  
  function FindBestProcNode(
    SearchForProcNode: TCodeTreeNode; SearchForProcAttr: TProcHeadAttributes;
    StartNode: TCodeTreeNode; SearchInProcAttr: TProcHeadAttributes): boolean;
  // search first for proc node with same name and param list and jump,
  // if this fails
  // search for a proc node with same name and jump to difference in param list
  // returns true on jumped, false if no target proc found
  var SearchedProcHead: string;
    //FromProcHead, ToProcHead: string;
    //Attr: TProcHeadAttributes;
    //DiffPos: integer;
    ProcNode: TCodeTreeNode;
  begin
    Result:=false;
    SearchedProcHead:=ExtractProcHead(SearchForProcNode,SearchForProcAttr);
    if SearchedProcHead='' then exit;
    ProcNode:=FindProcNode(StartNode,SearchedProcHead,SearchInProcAttr);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint.FindBestProcNode A ',ProcNode<>nil,' "',SearchedProcHead,'"');
{$ENDIF}
    if ProcNode<>nil then begin
      Result:=JumpToProc(SearchForProcNode,JumpToProcAttr,
                         ProcNode,JumpToProcAttr);
      exit;
    end;
    // there is no exact corresponding proc
    // -> search for a proc with the same name but different param list
    SearchForProcAttr:=SearchForProcAttr-[phpWithVarModifiers,
       phpWithParameterNames, phpWithDefaultValues, phpWithResultType,
       phpWithComments];
    SearchForProcAttr:=SearchForProcAttr+[phpWithoutBrackets,
       phpWithoutParamList];
    SearchedProcHead:=ExtractProcHead(SearchForProcNode,SearchForProcAttr);
    if SearchedProcHead='' then exit;
    ProcNode:=FindProcNode(StartNode,SearchedProcHead,SearchInProcAttr);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint.FindBestProcNode B ',ProcNode<>nil,' "',SearchedProcHead,'"');
{$ENDIF}
    if ProcNode<>nil then begin
      // there is a proc with the same name, but with different parameters
      Result:=JumpToProc(SearchForProcNode,JumpToProcAttr,
                         ProcNode,JumpToProcAttr);
    end;
  end;
  
  
var CursorNode, ClassNode, ProcNode, StartNode, TypeSectionNode: TCodeTreeNode;
  CleanCursorPos, r, LineStart, LineEnd, FirstAtomStart, LastAtomEnd,
  DiffTxtPos: integer;
  SearchedClassname: string;
  SearchForNodes, SearchInNodes: TAVLTree;
  DiffNode: TAVLTreeNode;
begin
  Result:=false;
  NewPos:=CursorPos;
  // build code tree
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint A CursorPos=',CursorPos.X,',',CursorPos.Y);
{$ENDIF}
  BuildTree(false);
  if not EndOfSourceFound then 
    RaiseException('End Of Source not found');
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint B');
{$ENDIF}
  // find the CursorPos in cleaned source
  r:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (r<>0) and (r<>-1) then 
    RaiseException('Cursor outside of code');
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
writeln('TMethodJumpingCodeTool.FindJumpPoint C ',NodeDescriptionAsString(CursorNode.Desc));
{$ENDIF}

  // first test if in a class
  ClassNode:=CursorNode;
  while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
    ClassNode:=ClassNode.Parent;
  if ClassNode<>nil then begin
    // cursor is in class/object definition
    // search in all implemented class procedures for the body 
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint C2 ',NodeDescriptionAsString(ClassNode.Desc));
{$ENDIF}
    if (ClassNode.SubDesc and ctnsForwardDeclaration)>0 then exit;
    // parse class and build CodeTreeNodes for all properties/methods
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint D ',CleanCursorPos,', |',copy(Src,CleanCursorPos,8));
{$ENDIF}
    BuildSubTreeForClass(ClassNode);
    TypeSectionNode:=ClassNode.Parent;
    if (TypeSectionNode<>nil) and (TypeSectionNode.Parent<>nil)
    and (TypeSectionNode.Parent.Desc=ctnTypeSection) then
      TypeSectionNode:=TypeSectionNode.Parent;
    // search the method node under the cursor
    CursorNode:=FindDeepestNodeAtPos(CleanCursorPos,false);
    if (CursorNode=nil)
    or (not (CursorNode.Desc in [ctnProcedureHead,ctnProcedure])) then
      exit;
    // build the method name + parameter list (without default values)
    Result:=FindBestProcNode(CursorNode,[phpAddClassName,phpInUpperCase],
                             TypeSectionNode,[phpIgnoreForwards,phpInUpperCase]);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint F FindBestProcNode=',Result);
{$ENDIF}
    if not Result then begin
      // find the first implemented class method that it is not defined in class
      StartNode:=ClassNode.FirstChild;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint H');
{$ENDIF}
      while (StartNode<>nil) and (StartNode.FirstChild=nil) do
        StartNode:=StartNode.NextBrother;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint I');
{$ENDIF}
      if StartNode=nil then exit;
      StartNode:=StartNode.FirstChild;
      if StartNode=nil then exit;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint J SearchInNodes');
{$ENDIF}
      SearchInNodes:=GatherProcNodes(StartNode,
         [phpInUpperCase,phpAddClassname,phpIgnoreProcsWithBody],
         '');
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint K SearchForNodes');
{$ENDIF}
      SearchForNodes:=GatherProcNodes(TypeSectionNode,
         [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
         ExtractClassName(ClassNode,true));
      try
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint L');
{$ENDIF}
        DiffNode:=FindFirstDifferenceNode(SearchForNodes,SearchInNodes,
                   DiffTxtPos);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint M ',DiffNode<>nil,' ',DiffTxtPos);
{$ENDIF}
        if DiffNode<>nil then begin
          ProcNode:=TCodeTreeNodeExtension(DiffNode.Data).Node;
          Result:=JumpToProc(CursorNode,JumpToProcAttr,
                             ProcNode,JumpToProcAttr);
        end;
      finally
        NodeExtMemManager.DisposeAVLTree(SearchForNodes);
        NodeExtMemManager.DisposeAVLTree(SearchInNodes);
      end;
    end;
    exit;
  end;
  
  // then test if cursor in a procedure
  ProcNode:=CursorNode;
  while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
    ProcNode:=ProcNode.Parent;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 2A ',ProcNode<>nil);
{$ENDIF}
  while (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) do begin
    if (ProcNode.SubDesc and ctnsForwardDeclaration)>0 then begin
      // forward declaration -> search procedure
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 2B ');
{$ENDIF}

      // build the method name + parameter list (without default values)
      Result:=FindBestProcNode(ProcNode,[phpInUpperCase],
                               ProcNode,[phpInUpperCase,phpIgnoreForwards]);
      exit;
    end else begin
      // procedure is not forward, search on same proc level
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4A');
{$ENDIF}
      SearchedClassname:=ExtractClassNameOfProcNode(ProcNode);
      StartNode:=FindFirstNodeOnSameLvl(ProcNode);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4B ',StartNode<>nil,' ',SearchedClassName);
{$ENDIF}
      if StartNode=nil then exit;
      if SearchedClassname<>'' then begin
        // search class node
        ClassNode:=FindClassNode(StartNode,UpperCaseStr(SearchedClassName),
                     true,false);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4C ',ClassNode<>nil);
{$ENDIF}
        if ClassNode=nil then exit;
        BuildSubTreeForClass(ClassNode);
        StartNode:=ClassNode.FirstChild;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4C2 ',StartNode<>nil,' ',NodeDescToStr(StartNode.Desc));
{$ENDIF}
        while (StartNode<>nil) and (StartNode.FirstChild=nil) do
          StartNode:=StartNode.NextBrother;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4D ',StartNode<>nil);
{$ENDIF}
        if StartNode=nil then exit;
        StartNode:=StartNode.FirstChild;
        Result:=FindBestProcNode(ProcNode,[phpWithoutClassName,phpInUpperCase],
                                 StartNode,[phpInUpperCase]);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4E ',Result);
{$ENDIF}
        if not Result then begin
          // search first undefined proc node with body
          SearchForNodes:=GatherProcNodes(StartNode,
             [phpInUpperCase,phpAddClassname,phpIgnoreProcsWithBody],'');
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4F ');
{$ENDIF}
          TypeSectionNode:=ClassNode.Parent;
          if (TypeSectionNode<>nil) and (TypeSectionNode.Parent<>nil)
          and (TypeSectionNode.Parent.Desc=ctnTypeSection) then
            TypeSectionNode:=TypeSectionNode.Parent;
          SearchInNodes:=GatherProcNodes(TypeSectionNode,
             [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname],
             ExtractClassName(ClassNode,true));
          try
            DiffNode:=FindFirstDifferenceNode(SearchForNodes,SearchInNodes,
                       DiffTxtPos);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4G ',DiffNode<>nil);
{$ENDIF}
            if DiffNode<>nil then begin
              ProcNode:=TCodeTreeNodeExtension(DiffNode.Data).Node;
              Result:=JumpToProc(CursorNode,JumpToProcAttr,
                                 ProcNode,JumpToProcAttr);
            end;
          finally
            NodeExtMemManager.DisposeAVLTree(SearchForNodes);
            NodeExtMemManager.DisposeAVLTree(SearchInNodes);
          end;
        end;
        exit;
      end else begin
        // search forward procedure
        Result:=FindBestProcNode(ProcNode,[phpInUpperCase],
                             StartNode,[phpInUpperCase,phpIgnoreProcsWithBody]);
      end;
    end;
    if Result then begin
      exit;
    end else begin
      // no proc found
      ProcNode:=ProcNode.Parent;
      // try parent proc ...
    end;
  end; //while (ProcNode<>nil) and (ProcNode.Desc=ctnProcedure) do begin
end;

function TMethodJumpingCodeTool.FindJumpPointInProcNode(ProcNode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var DestNode: TCodeTreeNode;
  i, NewCleanPos: integer;
begin
  Result:=false;
  // search method body
  DestNode:=FindProcBody(ProcNode);
  if DestNode=nil then begin
    // proc without body -> jump to proc node header
    Result:=JumpToCleanPos(ProcNode.FirstChild.StartPos,ProcNode.Startpos,
                           NewPos,NewTopLine,false);
    exit;
  end;
  // search good position
  { examples
      begin |end

      asm
      |end

      asm
        |

      end

      begin
        |DoSomething;
      end
  }
  MoveCursorToNodeStart(DestNode);
  // if begin is indented then indent the cursor as well
  i:=0;
  while (CurPos.StartPos-i>1) and (Src[CurPos.StartPos-i-1] in [' ',#8]) do
    inc(i);
{$IFDEF CTDEBUG}
writeln('[TMethodJumpingCodeTool.FindJumpPointInProcNode] A i=',i);
{$ENDIF}
  if (CurPos.StartPos-i>1) and (not (Src[CurPos.StartPos-i-1] in [#10,#13]))
  then
    i:=0;
{$IFDEF CTDEBUG}
writeln('[TMethodJumpingCodeTool.FindJumpPointInProcNode] B i=',i,' IndentSize=',IndentSize);
{$ENDIF}
  // set cursor in the next line but before the next token/comment
  ReadNextAtom;
  NewCleanPos:=CurPos.EndPos;
  while (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [' ',#8]) do
    inc(NewCleanPos);
  if (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [#13,#10]) then begin
    inc(NewCleanPos);
    if (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [#13,#10])
    and (Src[NewCleanPos-1]<>Src[NewCleanPos]) then
      inc(NewCleanPos);
    inc(i,IndentSize);
    while (i>0) and (NewCleanPos<=SrcLen) and (Src[NewCleanPos] in [' ',#8])
    do begin
      inc(NewCleanPos);
      dec(i);
    end;
    if not (Src[NewCleanPos] in [#13,#10]) then
      i:=0;
  end else
    i:=0;
  if NewCleanPos>SrcLen then NewCleanPos:=SrcLen;
  if not JumpToCleanPos(NewCleanPos,ProcNode.StartPos,NewPos,NewTopLine,true)
  then exit;
  if CursorBeyondEOL then
    inc(NewPos.x,i);
  Result:=true;
end;

function TMethodJumpingCodeTool.GatherProcNodes(StartNode: TCodeTreeNode;
  Attr: TProcHeadAttributes; const UpperClassName: string): TAVLTree;
var CurProcName: string;
  ANode: TCodeTreeNode;
  NewNodeExt: TCodeTreeNodeExtension;
  cmp: boolean;
begin
  Result:=TAVLTree.Create(@CompareCodeTreeNodeExt);
  ANode:=StartNode;
  while (ANode<>nil) do begin
//writeln('[TMethodJumpingCodeTool.GatherProcNodes] A ',NodeDescriptionAsString(ANode.Desc));
    if ANode.Desc=ctnProcedure then begin
      if (not ((phpIgnoreForwards in Attr)
           and ((ANode.SubDesc and ctnsForwardDeclaration)>0)))
      and (not ((phpIgnoreProcsWithBody in Attr)
            and (FindProcBody(ANode)<>nil))) then
      begin
//writeln('[TMethodJumpingCodeTool.GatherProcNodes] B');
        cmp:=true;
        if (phpOnlyWithClassname in Attr) then begin
          CurProcName:=ExtractProcName(ANode,true);
//writeln('[TMethodJumpingCodeTool.GatherProcNodes] B2 "',CurProcName,'" =? ',UpperClassName);

          if (UpperClassName<>copy(CurProcName,1,length(UpperClassName)))
          or (length(CurProcName)<length(UpperClassName)+2)
          or (CurProcName[length(UpperClassName)+1] in ['A'..'Z','_','0'..'9'])
          then
            cmp:=false;
        end;
        if cmp then begin
//writeln('[TMethodJumpingCodeTool.GatherProcNodes] C');
          CurProcName:=ExtractProcHead(ANode,Attr);
//writeln('[TMethodJumpingCodeTool.GatherProcNodes] D "',CurProcName,'" ',phpInUpperCase in Attr);
          if (CurProcName<>'') then begin
            NewNodeExt:=TCodeTreeNodeExtension.Create;
            with NewNodeExt do begin
              Node:=ANode;
              Txt:=CurProcName;
            end;
            Result.Add(NewNodeExt);
          end;
        end;
      end;
    end;
    // next node
    if (ANode.NextBrother=nil) and (ANode.Parent<>nil)
    and (ANode.Parent.NextBrother<>nil)
    and (ANode.Parent.Desc in (AllCodeSections+AllClassSections)) then
      ANode:=ANode.Parent.NextBrother.FirstChild
    else
      ANode:=ANode.NextBrother;
  end;
end;

function TMethodJumpingCodeTool.FindFirstDifferenceNode(
  SearchForNodes, SearchInNodes: TAVLTree;
  var DiffTxtPos: integer): TAVLTreeNode;
var SearchInNode: TAVLTreeNode;
  cmp: integer;
  NodeTxt1, NodeTxt2: string;
  Attr: TProcHeadAttributes;
begin
  Result:=SearchForNodes.FindLowest;
  if Result=nil then exit;
  SearchInNode:=SearchInNodes.FindLowest;
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] ',SearchInNode<>nil);

  DiffTxtPos:=-1;
  while (SearchInNode<>nil) do begin
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] B ',SearchInNode<>nil);
    cmp:=CompareCodeTreeNodeExt(Result.Data,SearchInNode.Data);
    
//NodeTxt1:=TCodeTreeNodeExtension(Result.Data).Txt;
//NodeTxt2:=TCodeTreeNodeExtension(SearchInNode.Data).Txt;
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] ',NodeTxt1,' ?',cmp,'= ',NodeTxt2);

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
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] C Result=',NodeTxt1);
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] C SearchInNode=',NodeTxt2);
      DiffTxtPos:=1;
      while (DiffTxtPos<=length(NodeTxt1)) and (DiffTxtPos<=length(NodeTxt2)) do
      begin
        if NodeTxt1[DiffTxtPos]<>NodeTxt2[DiffTxtPos] then
          break;
        inc(DiffTxtPos);
      end;
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] D DiffTxtPos=',DiffTxtPos);
      ExtractSearchPos:=DiffTxtPos;
      try
        ExtractProcHead(TCodeTreeNodeExtension(Result.Data).Node,Attr);
        DiffTxtPos:=ExtractFoundPos;
      finally
        ExtractSearchPos:=-1;
      end;
//writeln('[TMethodJumpingCodeTool.FindFirstDifferenceNode] E DiffTxtPos=',DiffTxtPos);
      exit;
    end else if cmp=0 then begin
      // node found in SearchInNodes -> search next
      Result:=SearchForNodes.FindSuccessor(Result);
      SearchInNode:=SearchInNodes.FindSuccessor(SearchInNode);
      if (Result=nil) or (SearchInNode=nil) then exit;
    end else begin
      // search in successor
      SearchInNode:=SearchInNodes.FindSuccessor(SearchInNode);
    end;
  end;
  Result:=nil;
end;

function TMethodJumpingCodeTool.JumpToNode(ANode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer;
  IgnoreJumpCentered: boolean): boolean;
begin
  Result:=false;
  if (ANode=nil) or (ANode.StartPos<1) then exit;
  Result:=JumpToCleanPos(ANode.StartPos,ANode.StartPos,
                         NewPos,NewTopLine,IgnoreJumpCentered);
end;

function TMethodJumpingCodeTool.FindNodeInTree(ATree: TAVLTree;
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

function TMethodJumpingCodeTool.JumpToCleanPos(NewCleanPos,
  NewTopLineCleanPos: integer; var NewPos: TCodeXYPosition;
  var NewTopLine: integer; IgnoreJumpCentered: boolean): boolean;
var CenteredTopLine: integer;
  NewTopLinePos: TCodeXYPosition;
begin
  Result:=false;
  // convert clean position to line, column and code
  if not CleanPosToCaret(NewCleanPos,NewPos) then exit;
  NewTopLine:=NewPos.Y;
  if AdjustTopLineDueToComment then begin
    // if there is a comment in front of the top position, it probably belongs
    // to the destination code
    // -> adjust the topline position, so that the comment is visible
    NewTopLineCleanPos:=FindLineEndOrCodeInFrontOfPosition(Src,
                         NewTopLineCleanPos,Scanner.NestedComments);
    if (NewTopLineCleanPos>=1) and (Src[NewTopLineCleanPos] in [#13,#10])
    then begin
      inc(NewTopLineCleanPos);
      if (Src[NewTopLineCleanPos] in [#10,#13])
      and (Src[NewTopLineCleanPos]<>Src[NewTopLineCleanPos-1]) then
        inc(NewTopLineCleanPos);
    end;
  end;
  // convert clean top line position to line, column and code
  if not CleanPosToCaret(NewTopLineCleanPos,NewTopLinePos) then exit;
  if NewTopLinePos.Code=NewPos.Code then begin
    // top line position is in the same code as the destination position
    NewTopLine:=NewTopLinePos.Y;
    if JumpCentered and (not IgnoreJumpCentered) then begin
      // center the destination position in the source editor
      CenteredTopLine:=NewPos.Y-VisibleEditorLines div 2;
      if CenteredTopLine<NewTopLine then
        NewTopLine:=CenteredTopLine;
    end;
    if NewTopLine<1 then NewTopLine:=1;
    if NewTopLine<=NewPos.Y-VisibleEditorLines then
      NewTopLine:=NewPos.Y-VisibleEditorLines+1;
  end else
    NewTopLine:=1;
  Result:=true;
end;



end.
