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
    TEventsCodeTool enhances TMethodJumpingCodeTool.
    TEventsCodeTool provides functions to work with published methods in the
    source. It can gather a list of compatible methods, test if method exists,
    jump to the method body, create a method
}
unit EventCodeTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{ $DEFINE CTDEBUG}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, CodeTree, CodeAtom, PascalParserTool, MethodJumpTool,
  SourceLog, KeywordFuncLists, BasicCodeTools, LinkScanner, CodeCache, AVL_Tree,
  TypInfo, SourceChanger, FindDeclarationTool;

type
  TGetStringProc = procedure(const s: string) of object;
  
  TEventsCodeTool = class(TMethodJumpingCodeTool)
  private
    GetCompatibleMethodsProc: TGetStringProc;
    SearchedExprList: TExprTypeList;
    SearchedCompatibilityList: TTypeCompatibilityList;
    function FindIdentifierNodeInClass(ClassNode: TCodeTreeNode;
      Identifier: PChar): TCodeTreeNode;
  protected
    function InsertNewMethodToClass(ClassSectionNode: TCodeTreeNode;
        const AMethodName,NewMethod: string;
        SourceChangeCache: TSourceChangeCache): boolean;
    function CollectPublishedMethods(Params: TFindDeclarationParams;
      FoundContext: TFindContext): TIdentifierFoundResult;
  public
    function GetCompatiblePublishedMethods(const UpperClassName: string;
        TypeData: PTypeData; Proc: TGetStringProc): boolean;
    function GetCompatiblePublishedMethods(ClassNode: TCodeTreeNode;
        TypeData: PTypeData; Proc: TGetStringProc): boolean;
    function PublishedMethodExists(const UpperClassName,
        UpperMethodName: string; TypeData: PTypeData;
        var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function PublishedMethodExists(ClassNode: TCodeTreeNode;
        const UpperMethodName: string; TypeData: PTypeData;
        var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function JumpToPublishedMethodBody(const UpperClassName,
        UpperMethodName: string;
        var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
    function RenamePublishedMethod(const UpperClassName, UpperOldMethodName,
        NewMethodName: string; SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedMethod(ClassNode: TCodeTreeNode;
        const UpperOldMethodName, NewMethodName: string;
        SourceChangeCache: TSourceChangeCache): boolean;
    function CreatePublishedMethod(const UpperClassName,
        AMethodName: string; ATypeInfo: PTypeInfo;
        SourceChangeCache: TSourceChangeCache): boolean;
    function CreatePublishedMethod(ClassNode: TCodeTreeNode;
        const AMethodName: string; ATypeInfo: PTypeInfo;
        SourceChangeCache: TSourceChangeCache): boolean;

    function CreateExprListFromMethodTypeData(TypeData: PTypeData;
        Params: TFindDeclarationParams): TExprTypeList;
    function FindPublishedMethodNodeInClass(ClassNode: TCodeTreeNode;
        const UpperMethodName: string): TFindContext;
    function FindProcNodeInImplementation(const UpperClassName,
        UpperMethodName: string; BuildTreeBefore: boolean): TCodeTreeNode;
    function MethodTypeInfoToStr(ATypeInfo: PTypeInfo): string;
    function MethodTypeDataToStr(TypeData: PTypeData;
        Attr: TProcHeadAttributes): string;
  end;


implementation

const
  MethodKindAsString: array[TMethodKind] of shortstring = (
        'procedure', 'function', 'constructor', 'destructor',
        'class procedure', 'class function'
      );


{ TEventsCodeTool }

function TEventsCodeTool.MethodTypeDataToStr(TypeData: PTypeData;
  Attr: TProcHeadAttributes): string;
type
  TParamType = record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
  end;

var i, ParamCount, Len, Offset: integer;
  ParamType: TParamType;
  s, ParamString: string;
begin
  Result:='';
  if TypeData=nil then exit;
  // transform TypeData into a ProcHead String
  ParamCount:=TypeData^.ParamCount;
//writeln('TEventsCodeTool.MethodTypeDataToStr A ParamCount=',ParamCount);
  if ParamCount>0 then begin
    Result:=Result+'(';
    ParamString:='';
    Offset:=0;
    for i:=0 to ParamCount-1 do begin
      // read ParamFlags
      // ToDo: check this: SizeOf(TParamFlags) is 4, but the data is only 1 byte
      Len:=1; // typinfo.pp comment is wrong: SizeOf(TParamFlags)
      Move(TypeData^.ParamList[Offset],ParamType.Flags,Len);
      inc(Offset,Len);

      // read ParamName
      Len:=ord(TypeData^.ParamList[Offset]);
      inc(Offset);
      SetLength(ParamType.ParamName,Len);
      Move(TypeData^.ParamList[Offset],ParamType.ParamName[1],Len);
      inc(Offset,Len);
      if ParamType.ParamName='' then begin
        if ParamCount>1 then
          ParamType.ParamName:='AValue'+IntToStr(ParamCount-i)
        else
          ParamType.ParamName:='AValue';
      end;

      // read ParamType
      Len:=ord(TypeData^.ParamList[Offset]);
      inc(Offset);
      SetLength(ParamType.TypeName,Len);
      Move(TypeData^.ParamList[Offset],ParamType.TypeName[1],Len);
      inc(Offset,Len);

      // build string
      if phpWithVarModifiers in Attr then begin
        if pfVar in ParamType.Flags then
          s:='var '
        else if pfConst in ParamType.Flags then
          s:='const '
        else if pfOut in ParamType.Flags then
          s:='out '
        else
          s:='';
      end else
        s:='';
      if phpWithParameterNames in Attr then
        s:=s+ParamType.ParamName;
      s:=s+':'+ParamType.TypeName;
      if i>0 then s:=s+';';
      ParamString:=s+ParamString;
    end;
    Result:=Result+ParamString+')';
  end;
  if phpInUpperCase in Attr then Result:=UpperCaseStr(Result);
  Result:=Result+';';
end;

function TEventsCodeTool.GetCompatiblePublishedMethods(
  const UpperClassName: string; TypeData: PTypeData;
  Proc: TGetStringProc): boolean;
var ClassNode: TCodeTreeNode;
begin
  Result:=false;
  ActivateGlobalWriteLock;
  try
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.GetCompatiblePublishedMethods] A UpperClassName=',UpperClassName);
{$ENDIF}
    BuildTree(true);
    if not InterfaceSectionFound then exit;
    ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.GetCompatiblePublishedMethods] B ',ClassNode<>nil);
{$ENDIF}
    Result:=GetCompatiblePublishedMethods(ClassNode,TypeData,Proc);
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.GetCompatiblePublishedMethods(
  ClassNode: TCodeTreeNode; TypeData: PTypeData; Proc: TGetStringProc): boolean;
var
  Params: TFindDeclarationParams;
  CompListSize: integer;
begin
  Result:=false;
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.GetCompatiblePublishedMethods] C ',ClassNode<>nil);
{$ENDIF}
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (TypeData=nil)
  or (Proc=nil) then exit;
  ActivateGlobalWriteLock;
  try
    BuildSubTreeForClass(ClassNode);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.GetCompatiblePublishedMethods]');
{$ENDIF}
    // 1. convert the TypeData to an expression type list
    Params:=TFindDeclarationParams.Create;
    try
      Params.ContextNode:=ClassNode.Parent;
      SearchedExprList:=CreateExprListFromMethodTypeData(TypeData,Params);
      // create compatibility list
      CompListSize:=SizeOf(TTypeCompatibility)*SearchedExprList.Count;
      if CompListSize>0 then begin
        GetMem(SearchedCompatibilityList,CompListSize);
      end else begin
        SearchedCompatibilityList:=nil;
      end;
      try
        // 2. search all compatible published procs
        GetCompatibleMethodsProc:=Proc;
        Params.ContextNode:=ClassNode;
        Params.Flags:=[fdfCollect,fdfSearchInAncestors,fdfClassPublished];
        Params.SetIdentifier(Self,nil,@CollectPublishedMethods);
        FindIdentifierInContext(Params);
      finally
        SearchedExprList.Free;
        SearchedExprList:=nil;
        if SearchedCompatibilityList<>nil then
          FreeMem(SearchedCompatibilityList);
        SearchedCompatibilityList:=nil;
      end;
    finally
      Params.Free;
    end;
    Result:=true;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.FindPublishedMethodNodeInClass(
  ClassNode: TCodeTreeNode; const UpperMethodName: string): TFindContext;
var
  Params: TFindDeclarationParams;
begin
  Result:=CleanFindContext;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (UpperMethodName='')
  or (Scanner=nil) then exit;
  ActivateGlobalWriteLock;
  try
    Params:=TFindDeclarationParams.Create;
    try
      Params.ContextNode:=ClassNode;
      Params.SetIdentifier(Self,@UpperMethodName[1],nil);
      Params.Flags:=[fdfSearchInAncestors,fdfClassPublished];
      if FindIdentifierInContext(Params)
      and (Params.NewNode.Desc=ctnProcedure) then begin
        Result:=CreateFindContext(Params);
      end;
    finally
      Params.Free;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.FindProcNodeInImplementation(const UpperClassName,
  UpperMethodName: string; BuildTreeBefore: boolean): TCodeTreeNode;
var SectionNode, ANode: TCodeTreeNode;
begin
  Result:=nil;
  if (UpperMethodName='') or (UpperClassName='') then exit;
  if BuildTreeBefore then BuildTree(false);
  // find implementation node
  SectionNode:=FindImplementationNode;
  if SectionNode=nil then exit;
  ANode:=SectionNode.FirstChild;
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.FindProcNodeInImplementation] A');
{$ENDIF}
  while (ANode<>nil) do begin
    if (ANode.Desc=ctnProcedure) and (ANode.FirstChild<>nil)
    and CompareSrcIdentifiers(ANode.FirstChild.StartPos,@UpperClassName[1])
    then begin
      MoveCursorToNodeStart(ANode.FirstChild);
      ReadNextAtom; // read class name
      ReadNextAtom; // read '.'
      if AtomIsChar('.') then begin
        ReadNextAtom;
        if CompareSrcIdentifiers(CurPos.StartPos,@UpperMethodName[1]) then
        begin
          Result:=ANode;
          exit;
        end;
      end;
    end;
    ANode:=ANode.NextBrother;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(const UpperClassName,
  UpperMethodName: string; TypeData: PTypeData;
  var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
var ClassNode: TCodeTreeNode;
begin
  ActivateGlobalWriteLock;
  try
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.PublishedMethodExists] A UpperClassName=',UpperClassName);
{$ENDIF}
    BuildTree(true);
    if not InterfaceSectionFound then exit;
    ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.PublishedMethodExists] B ',ClassNode<>nil);
{$ENDIF}
    Result:=PublishedMethodExists(ClassNode,UpperMethodName,TypeData,
               MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(ClassNode: TCodeTreeNode;
  const UpperMethodName: string; TypeData: PTypeData;
  var MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
var
  FoundContext: TFindContext;
  CompListSize: integer;
  ParamCompatibility: TTypeCompatibility;
  FirstParameterNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
begin
  Result:=false;
  MethodIsCompatible:=false;
  IdentIsmethod:=false;
  MethodIsPublished:=false;
  ActivateGlobalWriteLock;
  try
    Params:=TFindDeclarationParams.Create;
    try
      // first search a published method definition with same name
      Params.ContextNode:=ClassNode;
      Params.SetIdentifier(Self,@UpperMethodName[1],nil);
      Params.Flags:=[fdfSearchInAncestors,fdfClassPublished];
      if FindIdentifierInContext(Params) then begin
        IdentIsmethod:=(Params.NewNode.Desc=ctnProcedure);
        MethodIsPublished:=(Params.NewNode.Parent.Desc=ctnClassPublished);
        if IdentIsmethod and MethodIsPublished then begin
          // published method with same name found
          FoundContext:=CreateFindContext(Params);
          // -> test for compatibility

          // convert the TypeData to an expression type list
          Params.ContextNode:=Params.NewNode;
          SearchedExprList:=CreateExprListFromMethodTypeData(TypeData,Params);
          // create compatibility list
          CompListSize:=SizeOf(TTypeCompatibility)*SearchedExprList.Count;
          if CompListSize>0 then begin
            GetMem(SearchedCompatibilityList,CompListSize);
          end else begin
            SearchedCompatibilityList:=nil;
          end;
          try
            // check for compatibility
            FirstParameterNode:=FoundContext.Tool.GetFirstParameterNode(
              FoundContext.Node);
            ParamCompatibility:=FoundContext.Tool.IsParamListCompatible(
              FirstParameterNode,
              SearchedExprList,false,
              Params,SearchedCompatibilityList);
            if ParamCompatibility=tcExact then begin
              MethodIsCompatible:=true;
            end;
          finally
            SearchedExprList.Free;
            SearchedExprList:=nil;
            if SearchedCompatibilityList<>nil then
              FreeMem(SearchedCompatibilityList);
            SearchedCompatibilityList:=nil;
          end;
          Result:=true;
        end;
      end;
    finally
      Params.Free;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.JumpToPublishedMethodBody(const UpperClassName,
  UpperMethodName: string;
  var NewPos: TCodeXYPosition; var NewTopLine: integer): boolean;
var ANode: TCodeTreeNode;
begin
  ANode:=FindProcNodeInImplementation(UpperClassName,UpperMethodName,true);
  Result:=FindJumpPointInProcNode(ANode,NewPos,NewTopLine);
end;

function TEventsCodeTool.RenamePublishedMethod(const UpperClassName,
  UpperOldMethodName, NewMethodName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(false);
  if not EndOfSourceFound then exit;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  Result:=RenamePublishedMethod(ClassNode,UpperOldMethodName,NewMethodName,
                                SourceChangeCache);
end;

function TEventsCodeTool.RenamePublishedMethod(ClassNode: TCodeTreeNode;
  const UpperOldMethodName, NewMethodName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
// rename published method in class and in procedure itself
var ANode, ProcHeadNode: TCodeTreeNode;
  NameStart, NameEnd: integer;
  UpperClassName: string;
begin
  Result:=false;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (UpperOldMethodName='')
  or (NewMethodName='') or (SourceChangeCache=nil) or (Scanner=nil) then
    exit;
  SourceChangeCache.MainScanner:=Scanner;
  // rename in class
  ANode:=FindIdentifierNodeInClass(ClassNode,@UpperOldMethodName[1]);
  if (ANode=nil) then begin
    MoveCursorToNodeStart(ClassNode);
    RaiseException('old method not found: '+UpperOldMethodName);
  end;
  if (ANode.Desc<>ctnProcedure) then begin
    MoveCursorToNodeStart(ANode);
    RaiseException('old method not found: '+UpperOldMethodName);
  end;
  ProcHeadNode:=ANode.FirstChild;
  if ProcHeadNode=nil then exit;
  NameStart:=ProcHeadNode.StartPos;
  NameEnd:=NameStart;
  while (NameEnd<=SrcLen) and (IsIdentChar[UpperSrc[NameEnd]]) do
    inc(NameEnd);
  if not SourceChangeCache.Replace(gtNone,gtNone,NameStart,NameEnd,
      NewMethodName) then exit;
  // rename procedure itself -> find implementation node
  UpperClassName:=ExtractClassName(ClassNode,true);
  ANode:=FindProcNodeInImplementation(UpperClassName,UpperOldMethodName,false);
  if ANode=nil then exit;
  ProcHeadNode:=ANode.FirstChild;
  if ProcHeadNode=nil then exit;
  MoveCursorToNodeStart(ProcHeadNode);
  ReadNextAtom; // read class name
  ReadNextAtom; // read '.'
  ReadNextAtom; // read method name
  Result:=SourceChangeCache.Replace(gtNone,gtNone,
      CurPos.StartPos,CurPos.EndPos,NewMethodName);
end;

function TEventsCodeTool.CreatePublishedMethod(const UpperClassName,
  AMethodName: string; ATypeInfo: PTypeInfo;
  SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(false);
  if not EndOfSourceFound then exit;
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false);
  Result:=CreatePublishedMethod(ClassNode,AMethodName,ATypeInfo,
               SourceChangeCache);
end;

function TEventsCodeTool.CreatePublishedMethod(ClassNode: TCodeTreeNode;
  const AMethodName: string; ATypeInfo: PTypeInfo;
  SourceChangeCache: TSourceChangeCache): boolean;
var PublishedNode: TCodeTreeNode;
  NewMethod: string;
begin
  Result:=false;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (AMethodName='')
  or (ATypeInfo=nil) or (SourceChangeCache=nil) or (Scanner=nil) then exit;
  // convert TypeInfo to string
  NewMethod:=MethodTypeInfoToStr(ATypeInfo);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreatePublishedMethod] A NewMethod="',NewMethod,'"');
{$ENDIF}
  // add method to published section
  SourceChangeCache.MainScanner:=Scanner;
  BuildSubTreeForClass(ClassNode);
  PublishedNode:=ClassNode.FirstChild;
  if PublishedNode=nil then exit;
  if (PublishedNode.StartPos=PublishedNode.EndPos)
  and (PublishedNode.NextBrother<>nil)
  and (PublishedNode.NextBrother.Desc=ctnClassPublished) then
    PublishedNode:=PublishedNode.NextBrother;
//    NewMethod:=MethodKindAsString[TypeData^.MethodKind]+' '+AMethodName+
//        MethodTypeDataToStr(TypeData,[phpWithVarModifiers,phpWithParameterNames]);

  // ToDo: check if parts already exists

  Result:=InsertNewMethodToClass(PublishedNode,AMethodName,NewMethod,
              SourceChangeCache);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreatePublishedMethod] B ',Result);
{$ENDIF}
  if not Result then exit;
  Result:=SourceChangeCache.Apply;
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreatePublishedMethod] END');
{$ENDIF}
end;

function TEventsCodeTool.InsertNewMethodToClass(
  ClassSectionNode: TCodeTreeNode; const AMethodName,NewMethod: string;
  SourceChangeCache: TSourceChangeCache): boolean;
// NewMethod is for example 'class function Lol(c: char): char;'
var InsertNode, ClassNode, ImplementationNode, StartNode, ANode: TCodeTreeNode;
  Indent, InsertPos, cmp: integer;
  UpperMethodName, CurProcName, ProcCode, UpperClassName,
  AClassName: string;
  StartClassCode: boolean;
  ClassBodyProcs: TAVLTree;
  AnAVLNode: TAVLTreeNode;
begin
  Result:=false;
  if (ClassSectionNode=nil) or (SourceChangeCache=nil) or (AMethodName='')
  or (NewMethod='') then exit;
  ClassNode:=ClassSectionNode.Parent;
  if ClassNode=nil then exit;
  AClassName:=ExtractClassName(ClassNode,false);
  UpperClassName:=UpperCaseStr(AClassName);
  UpperMethodName:=UpperCaseStr(AMethodName);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] A ',
ClassSectionNode.FirstChild<>nil,' "',NewMethod,'"');
{$ENDIF}
  // find a nice inserting position
  if ClassSectionNode.FirstChild<>nil then begin
    // there are already other child nodes
    if (cpipLast=SourceChangeCache.BeautifyCodeOptions.ClassPartInsertPolicy)
    then begin
      // insert as last
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] B');
{$ENDIF}
      InsertNode:=ClassSectionNode.LastChild;
      Indent:=GetLineIndent(Src,InsertNode.StartPos);
      InsertPos:=FindFirstLineEndAfterInCode(Src,InsertNode.EndPos,
                      Scanner.NestedComments);
    end else begin
      // insert alphabetically
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] C');
{$ENDIF}
      InsertNode:=ClassSectionNode.FirstChild;
      while (InsertNode<>nil) do begin
        if (InsertNode.Desc=ctnProcedure) then begin
          CurProcName:=ExtractProcName(InsertNode,true);
          if CurProcName>UpperMethodName then
            break;
        end;
        InsertNode:=InsertNode.NextBrother;
      end;
      if InsertNode<>nil then begin
        // insert before insertnode
        if InsertNode.PriorBrother<>nil then begin
          // insert after InsertNode.PriorBrother
          InsertNode:=InsertNode.PriorBrother;
          Indent:=GetLineIndent(Src,InsertNode.StartPos);
          InsertPos:=FindFirstLineEndAfterInCode(Src,InsertNode.EndPos,
                      Scanner.NestedComments);
        end else begin
          // insert as first
          Indent:=GetLineIndent(Src,InsertNode.StartPos);
          InsertPos:=FindFirstLineEndAfterInCode(Src,
                             ClassSectionNode.EndPos,Scanner.NestedComments);
        end;
      end else begin
        // insert as last
        InsertNode:=ClassSectionNode.LastChild;
        Indent:=GetLineIndent(Src,InsertNode.StartPos);
        InsertPos:=FindLineEndOrCodeAfterPosition(Src,InsertNode.EndPos,
                      Scanner.NestedComments);
      end;
    end;
  end else begin
    // will become first and only child node of section
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] D');
{$ENDIF}
    Indent:=GetLineIndent(Src,ClassSectionNode.StartPos)
                  +SourceChangeCache.BeautifyCodeOptions.Indent;
    InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                             ClassSectionNode.StartPos,Scanner.NestedComments);
  end;
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] E');
{$ENDIF}
  ProcCode:=SourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
        NewMethod,'',AMethodName);
  ProcCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyProc(
                     ProcCode,Indent,false);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] E2 ProcCode="',ProcCode,'"');
{$ENDIF}
  if not SourceChangeCache.Replace(gtNewLine,gtNewLine,InsertPos,InsertPos,
           ProcCode) then exit;
           
  // add method body to implementation section

  ImplementationNode:=FindImplementationNode;
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] F ',ImplementationNode<>nil);
{$ENDIF}
  if ImplementationNode=nil then exit;
  StartNode:=ImplementationNode.FirstChild;
  if StartNode<>nil then begin
    // implementation section contains some procs or classes

    // gather proc nodes in implementation section
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] G');
{$ENDIF}
    ClassBodyProcs:=GatherProcNodes(StartNode,
           [phpInUpperCase,phpIgnoreForwards,phpOnlyWithClassname,
            phpWithoutClassName],UpperClassName);
            
    // ToDo: check if proc already exists
            
    StartClassCode:=(ClassBodyProcs.Count=0);
    UpperMethodName:=UpperClassName+'.'+UpperMethodName;
    if not StartClassCode then begin
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] H');
{$ENDIF}
      // find a nice insert position for the proc body
      case SourceChangeCache.BeautifyCodeOptions.MethodInsertPolicy of
        mipAlphabetically:
          // insert proc in alphabetic order
          begin
            AnAVLNode:=ClassBodyProcs.Root;
            while AnAVLNode<>nil do begin
              InsertNode:=TCodeTreeNodeExtension(AnAVLNode.Data).Node;
              CurProcName:=ExtractProcName(InsertNode,true);
              cmp:=AnsiCompareStr(UpperMethodName,CurProcName);
              if cmp<0 then
                AnAVLNode:=AnAVLNode.Left
              else if cmp>0 then
                AnAVLNode:=AnAVLNode.Right
              else
                break;
            end;
            repeat
              AnAVLNode:=ClassBodyProcs.FindSuccessor(AnAVLNode);
              if AnAVLNode=nil then break;
              ANode:=TCodeTreeNodeExtension(AnAVLNode.Data).Node;
              CurProcName:=ExtractProcName(ANode,true);
              cmp:=AnsiCompareStr(UpperMethodName,CurProcName);
              if cmp=0 then
                InsertNode:=ANode;
            until cmp<>0;
            CurProcName:=ExtractProcName(InsertNode,true);
            cmp:=AnsiCompareStr(UpperMethodName,CurProcName);
            if cmp<0 then begin
              // insert in front of InsertNode
              Indent:=GetLineIndent(Src,InsertNode.StartPos);
              InsertPos:=FindLineEndOrCodeInFrontOfPosition(Src,
                                  InsertNode.StartPos-1,Scanner.NestedComments);
              if InsertPos<1 then InsertPos:=1;
            end else begin
              // insert behind InsertNode
              Indent:=GetLineIndent(Src,InsertNode.StartPos);
              InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                                      InsertNode.EndPos,Scanner.NestedComments);
            end;
          end;
        else  // mipLast
        
          // ToDo: mipClassOrder
        
          // insert proc body behind last proc body
          begin
            AnAVLNode:=ClassBodyProcs.FindLowest;
            InsertNode:=TCodeTreeNodeExtension(AnAVLNode.Data).Node;
            while (AnAVLNode<>nil) do begin
              ANode:=TCodeTreeNodeExtension(AnAVLNode.Data).Node;
              if InsertNode.StartPos<ANode.StartPos then
                InsertNode:=ANode;
              AnAVLNode:=ClassBodyProcs.FindSuccessor(AnAVLNode);
            end;
            // insert after InsertNode
            Indent:=GetLineIndent(Src,InsertNode.StartPos);
            InsertPos:=FindLineEndOrCodeAfterPosition(Src,
                                     InsertNode.EndPos,Scanner.NestedComments);
          end;
      end;
    end else begin
      // this is the first class body
      // -> add proc body at the end of the implementation section
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] I');
{$ENDIF}
      Indent:=GetLineIndent(Src,InsertNode.StartPos);
      InsertPos:=ImplementationNode.EndPos;
    end;
  end else begin
    // implementation section contains no procs or classes
    // -> add proc body at the end of the implementation section
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] J');
{$ENDIF}
    StartClassCode:=true;
    Indent:=GetLineIndent(Src,ImplementationNode.StartPos);
    InsertPos:=ImplementationNode.EndPos;
  end;

  // insert classname to Method string
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] K');
{$ENDIF}
  ProcCode:=SourceChangeCache.BeautifyCodeOptions.AddClassAndNameToProc(
                                              NewMethod,AClassName,AMethodName);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.InsertNewMethodToClass] L ProcCode="',ProcCode,'"');
{$ENDIF}
  // build nice proc
  ProcCode:=SourceChangeCache.BeautifyCodeOptions.BeautifyProc(ProcCode,
                     Indent,true);
  if StartClassCode then
    ProcCode:=SourceChangeCache.BeautifyCodeOptions.LineEnd
                +GetIndentStr(Indent)+'{ '+AClassName+' }'
                +SourceChangeCache.BeautifyCodeOptions.LineEnd
                +ProcCode;
  if not SourceChangeCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
           ProcCode) then exit;

  Result:=true;
end;

function TEventsCodeTool.CreateExprListFromMethodTypeData(
  TypeData: PTypeData; Params: TFindDeclarationParams): TExprTypeList;
var i, ParamCount, Len, Offset: integer;
  CurTypeIdentifier: string;
  OldInput: TFindDeclarationInput;
  CurExprType: TExpressionType;
begin
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreateExprListFromMethodTypeData] START');
{$ENDIF}
  Result:=TExprTypeList.Create;
  if TypeData=nil then exit;
  ParamCount:=TypeData^.ParamCount;
  if ParamCount>0 then begin

    //Result:=Result+'(';
    //ParamString:='';
    Offset:=0;
    
    for i:=0 to ParamCount-1 do begin
    
      // skip ParamFlags
      // ToDo: check this: SizeOf(TParamFlags) is 4, but the data is only 1 byte
      Len:=1; // typinfo.pp comment is wrong: SizeOf(TParamFlags)
      inc(Offset,Len);

      // skip ParamName
      Len:=ord(TypeData^.ParamList[Offset]);
      inc(Offset,Len+1);

      // read ParamType
      Len:=ord(TypeData^.ParamList[Offset]);
      inc(Offset);
      SetLength(CurTypeIdentifier,Len);
      if CurTypeIdentifier<>'' then
        Move(TypeData^.ParamList[Offset],CurTypeIdentifier[1],Len);
      inc(Offset,Len);
      
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreateExprListFromMethodTypeData] A ',
' i=',i,'/',ParamCount,
' Ident=',CurTypeIdentifier
);
{$ENDIF}

      // convert ParamType to TExpressionType
      Params.Save(OldInput);
      Params.SetIdentifier(Self,@CurTypeIdentifier[1],nil);
      Params.Flags:=[fdfSearchInParentNodes,
                     fdfIgnoreCurContextNode,fdfClassPublished]
                     +(fdfGlobals*Params.Flags)
                     -[fdfSearchInAncestors,
                       fdfClassPublic,fdfClassProtected,fdfClassPrivate];
      CurExprType:=GetExpressionTypeOfTypeIdentifier(Params);
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CreateExprListFromMethodTypeData] B ',
' i=',i,'/',ParamCount,
' Ident=',CurTypeIdentifier,
' CurExprType=',ExprTypeToString(CurExprType)
);
{$ENDIF}

      Result.Add(CurExprType);
      Params.Load(OldInput);

      {// build string
      if phpWithVarModifiers in Attr then begin
        if pfVar in ParamType.Flags then
          s:='var '
        else if pfConst in ParamType.Flags then
          s:='const '
        else if pfOut in ParamType.Flags then
          s:='out '
        else
          s:='';
      end else
        s:='';
      if phpWithParameterNames in Attr then
        s:=s+ParamType.ParamName;
      s:=s+':'+ParamType.TypeName;
      if i>0 then s:=s+';';
      ParamString:=s+ParamString;}
    end;
    //Result:=Result+ParamString+')';
  end;
  {if phpInUpperCase in Attr then Result:=UpperCaseStr(Result);
  Result:=Result+';';}

end;

function TEventsCodeTool.CollectPublishedMethods(
  Params: TFindDeclarationParams; FoundContext: TFindContext
  ): TIdentifierFoundResult;
var
  ParamCompatibility: TTypeCompatibility;
  FirstParameterNode: TCodeTreeNode;
begin
  if (FoundContext.Node.Desc=ctnProcedure) then begin
{$IFDEF CTDEBUG}
writeln('[TEventsCodeTool.CollectPublishedMethods] ',
' Node=',FoundContext.Node.DescAsString,
' "',copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,20),'"',
' Tool=',FoundContext.Tool.MainFilename);
{$ENDIF}
    FirstParameterNode:=FoundContext.Tool.GetFirstParameterNode(
      FoundContext.Node);
    ParamCompatibility:=FoundContext.Tool.IsParamListCompatible(
      FirstParameterNode,
      SearchedExprList,false,
      Params,SearchedCompatibilityList);
    if ParamCompatibility=tcExact then begin
{$IFDEF CTDEBUG}
writeln('ParamCompatibility=',TypeCompatibilityNames[ParamCompatibility]);
{$ENDIF}

      // ToDo: ppu, ppw, dcu

      GetCompatibleMethodsProc(
        FoundContext.Tool.ExtractProcName(FoundContext.Node,false));
    end;
  end;
  Result:=ifrProceedSearch;
end;

function TEventsCodeTool.MethodTypeInfoToStr(ATypeInfo: PTypeInfo): string;
var TypeName: string;
  Params: TFindDeclarationParams;
begin
  ActivateGlobalWriteLock;
  try
    // find method type declaration
    TypeName:=ATypeInfo^.Name;
    Params:=TFindDeclarationParams.Create;
    try
      // find method type in used units
      Params.ContextNode:=FindMainUsesSection;
      if Params.ContextNode=nil then
        Params.ContextNode:=Tree.Root;
      Params.SetIdentifier(Self,@TypeName[1],nil);
      Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInParentNodes];
      FindIdentifierInContext(Params);
      // find proc node
      if Params.NewNode.Desc<>ctnTypeDefinition then begin
        Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
        Params.NewCodeTool.RaiseException('method type definition not found');
      end;
      Params.NewNode:=FindTypeNodeOfDefinition(Params.NewNode);
      if Params.NewNode.Desc<>ctnProcedureType then begin
        Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
        Params.NewCodeTool.RaiseException('method type definition not found');
      end;
      Result:=Params.NewCodeTool.ExtractProcHead(Params.NewNode,
                  [phpWithStart, phpWithoutClassName, phpWithVarModifiers,
                   phpWithParameterNames, phpWithResultType]);
      Result:=TrimCodeSpace(Result);
{ $IFDEF CTDEBUG}
writeln('[TEventsCodeTool.MethodTypeInfoToStr] Result="',Result,'"');
{ $ENDIF}
    finally
      Params.Free;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.FindIdentifierNodeInClass(ClassNode: TCodeTreeNode;
  Identifier: PChar): TCodeTreeNode;
var
  VisibilityNode: TCodeTreeNode;
begin
  BuildSubTreeForClass(ClassNode);
  VisibilityNode:=ClassNode.FirstChild;
  while (VisibilityNode<>nil) do begin
    Result:=VisibilityNode.FirstChild;
    while Result<>nil do begin
      if CompareSrcIdentifiers(Result.FirstChild.StartPos,Identifier) then
      begin
        exit;
      end;
      Result:=Result.NextBrother;
    end;
    VisibilityNode:=VisibilityNode.NextBrother;
  end;
  Result:=nil;
end;



end.

