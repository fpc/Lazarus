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

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, PascalParserTool, StdCodeTools,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  TypInfo, SourceChanger;

type
  TMethodJumpingCodeTool = class(TStandardCodeTool)
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
        var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function FindNodeInTree(ATree: TAVLTree;
        const UpperCode: string): TCodeTreeNodeExtension;
  end;


implementation

{ TMethodJumpingCodeTool }

function TMethodJumpingCodeTool.FindJumpPoint(CursorPos: TCodeXYPosition;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var CursorNode, ClassNode, ProcNode, StartNode, TypeSectionNode: TCodeTreeNode;
  CleanCursorPos, r, LineStart, LineEnd, FirstAtomStart, LastAtomEnd,
  DiffTxtPos: integer;
  SearchedProc, SearchedClassname: string;
  SearchForNodes, SearchInNodes: TAVLTree;
  DiffNode: TAVLTreeNode;
  NewProcCaret: TCodeXYPosition;
begin
  Result:=false;
  NewPos:=CursorPos;
  // build code tree
  //   scan for classes, objects and procedure definitions.
  //   there will be no nodes in a class/object
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint A CursorPos=',CursorPos.X,',',CursorPos.Y);
{$ENDIF}
  BuildTree(false);
  if not EndOfSourceFound then exit;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint B');
{$ENDIF}
  // find the CursorPos in cleaned source
  r:=CaretToCleanPos(CursorPos, CleanCursorPos);
  if (r<>0) and (r<>-1) then exit;
  GetLineInfo(CleanCursorPos,LineStart,LineEnd,FirstAtomStart,LastAtomEnd);
  if CleanCursorPos<FirstAtomStart then CleanCursorPos:=FirstAtomStart;
  if CleanCursorPos>=LastAtomEnd then CleanCursorPos:=LastAtomEnd-1;
  // find CodeTreeNode at cursor
  CursorNode:=FindDeepestNodeAtPos(CleanCursorPos);
  if CursorNode=nil then
    exit;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint C ',NodeDescriptionAsString(CursorNode.Desc));
{$ENDIF}

  // first test if in a class
  ClassNode:=CursorNode;
  while (ClassNode<>nil) and (ClassNode.Desc<>ctnClass) do
    ClassNode:=ClassNode.Parent;
  if ClassNode<>nil then begin
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint C2 ',NodeDescriptionAsString(ClassNode.Desc));
{$ENDIF}
    // cursor is in class/object definition
    if CursorNode.SubDesc=ctnsForwardDeclaration then exit;
    // parse class and build CodeTreeNodes for all properties/methods
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint D ',CleanCursorPos,', |',copy(Src,CleanCursorPos,8));
{$ENDIF}
    BuildSubTreeForClass(ClassNode);
    // search the method node under the cursor
    CursorNode:=FindDeepestNodeAtPos(CleanCursorPos);
    if (CursorNode=nil)
    or (not (CursorNode.Desc in [ctnProcedureHead,ctnProcedure])) then
      exit;
    // build the method name + parameter list (without default values)
    SearchedProc:=ExtractProcHead(CursorNode,
                                 [phpWithParameterNames,phpAddClassname]);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint E SearchedProc="',SearchedProc,'"');
{$ENDIF}
    if SearchedProc='' then exit;
    // search the method
    TypeSectionNode:=ClassNode.Parent;
    if (TypeSectionNode<>nil) and (TypeSectionNode.Parent<>nil)
    and (TypeSectionNode.Parent.Desc=ctnTypeSection) then
      TypeSectionNode:=TypeSectionNode.Parent;
    ProcNode:=FindProcNode(TypeSectionNode,SearchedProc,
                 [phpWithParameterNames,phpIgnoreForwards]);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint F FindProcNode=',ProcNode<>nil);
{$ENDIF}
    if ProcNode<>nil then begin
      // find good position in procedure body
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint G');
{$ENDIF}
      Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
    end else begin
      // find the first not defined method
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
          ExtractSearchPos:=DiffTxtPos;
          ExtractProcHead(ProcNode,[phpWithParameterNames,phpFindCleanPosition]);
          DiffTxtPos:=ExtractFoundPos;
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint N ',DiffTxtPos);
{$ENDIF}
          if DiffTxtPos>0 then begin
            // move cursor to first difference in procedure head
            if not CleanPosToCaret(DiffTxtPos,NewPos) then exit;
            // calculate NewTopLine
            if not CleanPosToCaret(ProcNode.StartPos,NewProcCaret) then exit;
            if NewPos.Code=NewProcCaret.Code then
              NewTopLine:=NewProcCaret.Y
            else
              NewTopLine:=1;
            if NewTopLine<=NewPos.Y-VisibleEditorLines then
              NewTopLine:=NewPos.Y-VisibleEditorLines+1;
            Result:=true;
          end else
            // find good position in procedure body
            Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
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
  if ProcNode<>nil then begin
    if ProcNode.SubDesc=ctnsForwardDeclaration then begin
      // forward declaration -> search procedure
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 2B ');
{$ENDIF}

      // build the method name + parameter list (without default values)
      SearchedProc:=ExtractProcHead(ProcNode,[phpWithParameterNames]);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 2C SearchedProc="',SearchedProc,'"');
{$ENDIF}
      if SearchedProc='' then exit;
      // search the method
      ProcNode:=FindProcNode(ProcNode,SearchedProc,
                   [phpWithParameterNames,phpIgnoreForwards]);
      if ProcNode=nil then exit;
      // find good position in procedure body
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 2D');
{$ENDIF}
      Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
    end else begin
      // procedure without forward, search on same level
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
writeln('  ',NodeDescToStr(ClassNode.Desc));
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
        SearchedProc:=ExtractProcHead(ProcNode,
                     [phpWithoutClassName,phpWithParameterNames]);
        ProcNode:=FindProcNode(StartNode,SearchedProc,[phpWithParameterNames]);
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4E ',ProcNode<>nil,' ',SearchedProc);
{$ENDIF}
        if ProcNode=nil then begin
          // search first undefined proc node with body
          SearchForNodes:=GatherProcNodes(StartNode,
             [phpInUpperCase,phpAddClassname,phpIgnoreProcsWithBody],
             '');
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
              ExtractSearchPos:=DiffTxtPos;
              ExtractProcHead(ProcNode,[phpWithParameterNames,
                             phpFindCleanPosition]);
              DiffTxtPos:=ExtractFoundPos;
              if DiffTxtPos>0 then begin
                // move cursor to first difference in procedure head
                if not CleanPosToCaret(DiffTxtPos,NewPos) then exit;
                // calculate NewTopLine
                if not CleanPosToCaret(ProcNode.StartPos,NewProcCaret) then
                  exit;
                if NewPos.Code=NewProcCaret.Code then
                  NewTopLine:=NewProcCaret.Y
                else
                  NewTopLine:=1;
                if NewTopLine<=NewPos.Y-VisibleEditorLines then
                  NewTopLine:=NewPos.Y-VisibleEditorLines+1;
                Result:=true;
              end else
                // find good position in procedure body
                Result:=FindJumpPointInProcNode(ProcNode,NewPos,NewTopLine);
            end;
          finally
            NodeExtMemManager.DisposeAVLTree(SearchForNodes);
            NodeExtMemManager.DisposeAVLTree(SearchInNodes);
          end;
        end;
        Result:=JumpToNode(ProcNode,NewPos,NewTopLine);
      end else begin
        // search forward procedure
        SearchedProc:=ExtractProcHead(ProcNode,[phpWithParameterNames]);
        ProcNode:=FindProcNode(StartNode,SearchedProc,
                     [phpWithParameterNames,phpIgnoreProcsWithBody]);
        if ProcNode=nil then exit;
        // find good position in forward procedure
{$IFDEF CTDEBUG}
writeln('TMethodJumpingCodeTool.FindJumpPoint 4B');
{$ENDIF}
        ProcNode:=ProcNode.FirstChild;
        Result:=JumpToNode(ProcNode,NewPos,NewTopLine);
      end;
    end;
  end;
end;

function TMethodJumpingCodeTool.FindJumpPointInProcNode(ProcNode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var DestNode: TCodeTreeNode;
  i, NewCleanPos: integer;
  NewProcCaret: TCodeXYPosition;
begin
  Result:=false;
  // search method body
  DestNode:=FindProcBody(ProcNode);
  if DestNode=nil then exit;
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
  if not CleanPosToCaret(NewCleanPos,NewPos) then exit;
  if CursorBeyondEOL then
    inc(NewPos.x,i);
  // calculate NewTopLine
  if not CleanPosToCaret(ProcNode.StartPos,NewProcCaret) then exit;
  if NewPos.Code=NewProcCaret.Code then
    NewTopLine:=NewProcCaret.Y
  else
    NewTopLine:=1;
  if NewTopLine<=NewPos.Y-VisibleEditorLines then
    NewTopLine:=NewPos.Y-VisibleEditorLines+1;
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
           and (ANode.SubDesc=ctnsForwardDeclaration)))
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
      // node not found in SearchInNodes
      NodeTxt1:=TCodeTreeNodeExtension(Result.Data).Txt;
      NodeTxt2:=TCodeTreeNodeExtension(SearchInNode.Data).Txt;
      DiffTxtPos:=1;
      while (DiffTxtPos<=length(NodeTxt1)) and (DiffTxtPos<=length(NodeTxt2)) do
      begin
        if UpChars[NodeTxt1[DiffTxtPos]]<>UpChars[NodeTxt2[DiffTxtPos]] then
          break;
        inc(DiffTxtPos);
      end;
      exit;
    end else if cmp=0 then begin
      // node found in SearchInNodes -> search next
      Result:=SearchForNodes.FindSuccessor(Result);
      if Result=nil then exit;
    end else begin
      // search in successor
      SearchInNode:=SearchInNodes.FindSuccessor(SearchInNode);
    end;
  end;
  Result:=nil;
end;

function TMethodJumpingCodeTool.JumpToNode(ANode: TCodeTreeNode;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
begin
  Result:=false;
  if (ANode=nil) or (ANode.StartPos<1) then exit;
  if not CleanPosToCaret(ANode.StartPos,NewPos) then exit;
  NewTopLine:=NewPos.Y;
  if JumpCentered then begin
    dec(NewTopLine,VisibleEditorLines div 2);
    if NewTopLine<1 then NewTopLine:=1;
  end;
  Result:=true;
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



end.
