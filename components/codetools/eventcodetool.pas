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
    TEventsCodeTool enhances TExtractProcTool.
    TEventsCodeTool provides functions to work with published methods in the
    source. It can gather a list of compatible methods, test if method exists,
    jump to the method body, create a method, complete all missing published
    variables and events from a root component.
}
unit EventCodeTool;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

{off $DEFINE CTDEBUG}
{off $DEFINE VerboseTypeData}
{off $DEFINE ShowAllProcs}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeToolsStrConsts, CodeTree, CodeAtom,
  PascalParserTool, CodeCompletionTool, KeywordFuncLists, BasicCodeTools,
  LinkScanner, AVL_Tree, TypInfo, CodeToolsStructs,
  SourceChanger, FindDeclarationTool, ExtractProcTool;

type
  { TEventsCodeTool }

  TEventsCodeTool = class(TExtractProcTool)
  private
    fGatheredCompatibleMethods: TAVLTree; // tree of PChar in Src
    SearchedExprList: TExprTypeList;
    SearchedCompatibilityList: TTypeCompatibilityList;
    function FindIdentifierNodeInClass(ClassNode: TCodeTreeNode;
      Identifier: PChar): TCodeTreeNode;
  protected
    function CollectPublishedMethods(Params: TFindDeclarationParams;
      const FoundContext: TFindContext): TIdentifierFoundResult;
  public
    function CompleteComponent(AComponent, AncestorComponent: TComponent;
        SourceChangeCache: TSourceChangeCache): boolean;
  
    function GetCompatiblePublishedMethods(const UpperClassName: string;
        TypeData: PTypeData; Proc: TGetStrProc): boolean;
    function GetCompatiblePublishedMethods(ClassNode: TCodeTreeNode;
        TypeData: PTypeData; Proc: TGetStrProc): boolean;
    function PublishedMethodExists(const UpperClassName,
        UpperMethodName: string; TypeData: PTypeData;
        out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function PublishedMethodExists(ClassNode: TCodeTreeNode;
        const UpperMethodName: string; TypeData: PTypeData;
        out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function JumpToPublishedMethodBody(const UpperClassName,
        UpperMethodName: string;
        out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
    function RenamePublishedMethod(const UpperClassName, UpperOldMethodName,
        NewMethodName: string; SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedMethod(ClassNode: TCodeTreeNode;
        const UpperOldMethodName, NewMethodName: string;
        SourceChangeCache: TSourceChangeCache): boolean;
        
    function CreateMethod(const UpperClassName,
        AMethodName: string; ATypeInfo: PTypeInfo;
        const APropertyUnitName, APropertyPath: string;
        SourceChangeCache: TSourceChangeCache;
        UseTypeInfoForParameters: boolean = false;
        Section: TPascalClassSection = pcsPublished;
        const CallAncestorMethod: string = ''): boolean;
    function CreateMethod(ClassNode: TCodeTreeNode;
        const AMethodName: string;
        ATypeInfo: PTypeInfo; const APropertyUnitName, APropertyPath: string;
        SourceChangeCache: TSourceChangeCache;
        UseTypeInfoForParameters: boolean = false;
        Section: TPascalClassSection = pcsPublished;
        const CallAncestorMethod: string = ''): boolean;

    function CreateExprListFromMethodTypeData(TypeData: PTypeData;
        Params: TFindDeclarationParams; out List: TExprTypeList): boolean;
    function FindPublishedMethodNodeInClass(ClassNode: TCodeTreeNode;
        const UpperMethodName: string;
        ExceptionOnNotFound: boolean): TFindContext;
    function FindMethodNodeInImplementation(const UpperClassName,
        UpperMethodName: string; BuildTreeBefore: boolean): TCodeTreeNode;
    function FindMethodTypeInfo(ATypeInfo: PTypeInfo;
        const AStartUnitName: string = ''): TFindContext;
    function MethodTypeDataToStr(TypeData: PTypeData;
        Attr: TProcHeadAttributes): string;

    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;

const
  MethodKindAsString: array[TMethodKind] of shortstring = (
        'procedure', 'function', 'constructor', 'destructor',
        'class procedure', 'class function'
        {$IF high(TMethodKind)<>   mkClassFunction}
          ,'class constructor', 'class destructor'
          {$IF high(TMethodKind)<>   mkClassDestructor}
            ,'operator overload'
          {$ENDIF}
        {$ENDIF}
      );

function ReverseRTTIParamList: boolean;

implementation

function ReverseRTTIParamList: boolean;
begin
  Result:=false;
end;

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
  s, ParamString, ResultType: string;
  Reverse: Boolean;
begin
  Result:='';
  if TypeData=nil then exit;
  if phpWithStart in Attr then begin
    case TypeData^.MethodKind of
    mkProcedure: Result:=Result+'procedure ';
    mkFunction: Result:=Result+'function ';
    mkConstructor: Result:=Result+'constructor ';
    mkDestructor: Result:=Result+'destructor ';
    mkClassProcedure: Result:=Result+'class procedure ';
    mkClassFunction: Result:=Result+'class function ';
    end;
  end;
  // transform TypeData into a ProcHead String
  ParamCount:=TypeData^.ParamCount;
  Reverse:=ReverseRTTIParamList;
  //DebugLn(['TEventsCodeTool.MethodTypeDataToStr A ParamCount=',ParamCount]);
  Offset:=0;
  if ParamCount>0 then begin
    Result:=Result+'(';
    ParamString:='';
    for i:=0 to ParamCount-1 do begin
      // read ParamFlags
      // ToDo: check this: SizeOf(TParamFlags) is 4, but the data is only 1 byte
      Len:=1; // typinfo.pp comment is wrong: SizeOf(TParamFlags)
      ParamType.Flags:=[];
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
        s:=s+ParamType.ParamName+':';
      s:=s+ParamType.TypeName;
      if Reverse then begin
        if i>0 then s:=s+';';
        ParamString:=s+ParamString;
      end else begin
        if i>0 then s:=';'+s;
        ParamString:=ParamString+s;
      end;
    end;
    Result:=Result+ParamString+')';
  end;
  if phpWithResultType in Attr then begin
    Len:=ord(TypeData^.ParamList[Offset]);
    inc(Offset);
    SetLength(ResultType,Len);
    Move(TypeData^.ParamList[Offset],ResultType[1],Len);
    inc(Offset,Len);
    if (ResultType<>'') and (IsIdentStartChar[ResultType[1]]) then // e.g. $void
      Result:=Result+':'+ResultType;
  end;
  if phpInUpperCase in Attr then Result:=UpperCaseStr(Result);
  Result:=Result+';';
end;

procedure TEventsCodeTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
  if fGatheredCompatibleMethods<>nil then
    Stats.Add('TEventsCodeTool.fGatheredCompatibleMethods',
      fGatheredCompatibleMethods.Count*SizeOf(TAVLTreeNode));
  if SearchedExprList<>nil then
    Stats.Add('TEventsCodeTool.SearchedExprList',SearchedExprList.CalcMemSize);
end;

function TEventsCodeTool.GetCompatiblePublishedMethods(
  const UpperClassName: string; TypeData: PTypeData;
  Proc: TGetStrProc): boolean;
var ClassNode: TCodeTreeNode;
begin
  Result:=false;
  ActivateGlobalWriteLock;
  try
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] A UpperClassName=',
      UpperClassName);
    {$ENDIF}
    BuildTree(true);
    if not InterfaceSectionFound then exit;
    ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] B ',dbgs(ClassNode<>nil));
    {$ENDIF}
    Result:=GetCompatiblePublishedMethods(ClassNode,TypeData,Proc);
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.GetCompatiblePublishedMethods(
  ClassNode: TCodeTreeNode; TypeData: PTypeData;
  Proc: TGetStrProc): boolean;
var
  Params: TFindDeclarationParams;
  CompListSize: integer;
  Node: TAVLTreeNode;
  ProcName: String;
begin
  Result:=false;
  {$IFDEF CTDEBUG}
  DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] C ',dbgs(ClassNode<>nil));
  {$ENDIF}
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (TypeData=nil)
  or (Proc=nil) or (fGatheredCompatibleMethods<>nil) then exit;
  Params:=nil;
  ActivateGlobalWriteLock;
  FreeAndNil(SearchedExprList);
  try
    BuildSubTreeForClass(ClassNode);
    fGatheredCompatibleMethods:=TAVLTree.Create(@CompareIdentifierPtrs);

    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] D');
    {$ENDIF}
    // 1. convert the TypeData to an expression type list
    Params:=TFindDeclarationParams.Create;
    Params.ContextNode:=ClassNode.Parent;
    if not CreateExprListFromMethodTypeData(TypeData,Params,SearchedExprList) then
      exit(false);
    {$IFDEF ShowAllProcs}
    DebugLn(['TEventsCodeTool.GetCompatiblePublishedMethods SearchedExprList=',SearchedExprList.AsString]);
    {$ENDIF}
    // create compatibility list
    CompListSize:=SizeOf(TTypeCompatibility)*SearchedExprList.Count;
    if CompListSize>0 then begin
      GetMem(SearchedCompatibilityList,CompListSize);
    end else begin
      SearchedCompatibilityList:=nil;
    end;
    try
      // 2. search all compatible published procs
      Params.ContextNode:=ClassNode;
      Params.Flags:=[fdfCollect,fdfSearchInAncestors];
      Params.SetIdentifier(Self,nil,@CollectPublishedMethods);
      {$IFDEF CTDEBUG}
      DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] E Searching ...');
      {$ENDIF}
      FindIdentifierInContext(Params);
      
      // collect all method names
      Node:=fGatheredCompatibleMethods.FindLowest;
      while Node<>nil do begin
        ProcName:=GetIdentifier(Node.Data);
        Proc(ProcName);
        Node:=fGatheredCompatibleMethods.FindSuccessor(Node);
      end;
    finally
      if SearchedCompatibilityList<>nil then
        FreeMem(SearchedCompatibilityList);
      SearchedCompatibilityList:=nil;
    end;
    Result:=true;
  finally
    FreeAndNil(SearchedExprList);
    DeactivateGlobalWriteLock;
    Params.Free;
    FreeAndNil(fGatheredCompatibleMethods);
  end;
end;

function TEventsCodeTool.FindPublishedMethodNodeInClass(
  ClassNode: TCodeTreeNode; const UpperMethodName: string;
  ExceptionOnNotFound: boolean): TFindContext;
var
  Params: TFindDeclarationParams;
begin
  Result:=CleanFindContext;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (UpperMethodName='')
  or (Scanner=nil) then begin
    DebugLn(['TEventsCodeTool.FindPublishedMethodNodeInClass invalid parameters']);
    exit;
  end;
  ActivateGlobalWriteLock;
  Params:=nil;
  try
    Params:=TFindDeclarationParams.Create;
    Params.ContextNode:=ClassNode;
    Params.SetIdentifier(Self,@UpperMethodName[1],nil);
    Params.Flags:=[fdfSearchInAncestors];
    if ExceptionOnNotFound then Include(Params.Flags,fdfExceptionOnNotFound);
    if FindIdentifierInContext(Params)
    and (Params.NewNode.Desc=ctnProcedure)
    and (Params.NewNode.Parent<>nil)
    and (Params.NewNode.Parent.Desc=ctnClassPublished) then begin
      Result:=CreateFindContext(Params);
    end;
  finally
    DeactivateGlobalWriteLock;
    Params.Free;
  end;
end;

function TEventsCodeTool.FindMethodNodeInImplementation(const UpperClassName,
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
  DebugLn('[TEventsCodeTool.FindMethodNodeInImplementation] A UpperMethodName=',UpperClassName,'.',UpperMethodName);
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
          {$IFDEF CTDEBUG}
          DebugLn('[TEventsCodeTool.FindMethodNodeInImplementation] B  body found');
          {$ENDIF}
          Result:=ANode;
          exit;
        end;
      end;
    end;
    ANode:=ANode.NextBrother;
  end;
end;

function TEventsCodeTool.FindMethodTypeInfo(ATypeInfo: PTypeInfo;
  const AStartUnitName: string): TFindContext;
var
  Tool: TFindDeclarationTool;

  procedure RaiseTypeNotFound;
  begin
    RaiseException('type '+ATypeInfo^.Name+' not found, because tool is '+dbgsname(Tool));
  end;
  
var TypeName: string;
  Params: TFindDeclarationParams;
begin
  if AStartUnitName<>'' then begin
    // start searching in another unit
    Tool:=FindCodeToolForUsedUnit(AStartUnitName,'',true);
    if not (Tool is TEventsCodeTool) then
      RaiseTypeNotFound;
    TEventsCodeTool(Tool).BuildTree(true);
    Result:=TEventsCodeTool(Tool).FindMethodTypeInfo(ATypeInfo,'');
    exit;
  end;

  ActivateGlobalWriteLock;
  Params:=nil;
  try
    // find method type declaration
    TypeName:=ATypeInfo^.Name;
    Params:=TFindDeclarationParams.Create;
    try
      // find method in interface and used units
      Params.ContextNode:=FindImplementationNode;
      if Params.ContextNode=nil then
        Params.ContextNode:=FindMainBeginEndNode;
      if Params.ContextNode=nil then begin
        MoveCursorToNodeStart(Tree.Root);
        RaiseException(Format(ctsIdentifierNotFound,[GetIdentifier(@TypeName[1])]));
      end;
      Params.SetIdentifier(Self,@TypeName[1],nil);
      Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInParentNodes];
      //DebugLn(['TEventsCodeTool.FindMethodTypeInfo TypeName=',TypeName,' MainFilename=',MainFilename]);
      FindIdentifierInContext(Params);
      // find proc node
      if Params.NewNode.Desc<>ctnTypeDefinition then begin
        Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
        Params.NewCodeTool.RaiseException(ctsMethodTypeDefinitionNotFound);
      end;
      Params.NewNode:=Params.NewCodeTool.FindTypeNodeOfDefinition(Params.NewNode);
      if Params.NewNode.Desc<>ctnProcedureType then begin
        Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
        Params.NewCodeTool.RaiseException(ctsMethodTypeDefinitionNotFound);
      end;
      Result:=CreateFindContext(Params);
    finally
      Params.Free;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(const UpperClassName,
  UpperMethodName: string; TypeData: PTypeData;
  out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
var ClassNode: TCodeTreeNode;
begin
  ActivateGlobalWriteLock;
  try
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.PublishedMethodExists] A UpperClassName=',UpperClassName);
    {$ENDIF}
    BuildTree(true);
    if not InterfaceSectionFound then exit;
    ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.PublishedMethodExists] B ',dbgs(ClassNode<>nil));
    {$ENDIF}
    Result:=PublishedMethodExists(ClassNode,UpperMethodName,TypeData,
               MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(ClassNode: TCodeTreeNode;
  const UpperMethodName: string; TypeData: PTypeData;
  out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
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
  FreeAndNil(SearchedExprList);
  try
    Params:=TFindDeclarationParams.Create;
    try
      // first search a published method definition with same name
      Params.ContextNode:=ClassNode;
      Params.SetIdentifier(Self,@UpperMethodName[1],nil);
      Params.Flags:=[fdfSearchInAncestors];
      if FindIdentifierInContext(Params) then begin
        IdentIsmethod:=(Params.NewNode.Desc=ctnProcedure);
        MethodIsPublished:=(Params.NewNode.Parent.Desc=ctnClassPublished);
        if IdentIsmethod and MethodIsPublished then begin
          // published method with same name found
          FoundContext:=CreateFindContext(Params);
          // -> test for compatibility

          // convert the TypeData to an expression type list
          Params.ContextNode:=Params.NewNode;
          if not CreateExprListFromMethodTypeData(TypeData,Params,SearchedExprList)
          then
            exit(false);
          // create compatibility list
          CompListSize:=SizeOf(TTypeCompatibility)*SearchedExprList.Count;
          if CompListSize>0 then begin
            GetMem(SearchedCompatibilityList,CompListSize);
          end else begin
            SearchedCompatibilityList:=nil;
          end;
          try
            // check if the method fits into the TypeData
            FirstParameterNode:=FoundContext.Tool.GetFirstParameterNode(
              FoundContext.Node);
            ParamCompatibility:=
              FoundContext.Tool.IsParamNodeListCompatibleToExprList(
                SearchedExprList,
                FirstParameterNode,
                Params,SearchedCompatibilityList);
            if ParamCompatibility=tcExact then begin
              MethodIsCompatible:=true;
            end;
          finally
            if SearchedCompatibilityList<>nil then
              FreeMem(SearchedCompatibilityList);
            SearchedCompatibilityList:=nil;
          end;
          Result:=true;
        end;
      end;
    finally
      FreeAndNil(SearchedExprList);
      Params.Free;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.JumpToPublishedMethodBody(const UpperClassName,
  UpperMethodName: string;
  out NewPos: TCodeXYPosition; out NewTopLine: integer): boolean;
var
  ANode: TCodeTreeNode;
  ClassNode: TCodeTreeNode;
  AFindContext: TFindContext;
  SrcTool: TEventsCodeTool;
  SrcClassName: String;
begin
  Result:=false;
  ActivateGlobalWriteLock;
  try
    BuildTree(false);
    ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
    AFindContext:=FindPublishedMethodNodeInClass(ClassNode,UpperMethodName,true);
    if AFindContext.Node=nil then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody method not found: ',UpperClassName,'.',UpperMethodName]);
      exit;
    end;
    SrcTool:=TEventsCodeTool(AFindContext.Tool);
    ClassNode:=AFindContext.Node.Parent.Parent;
    if ClassNode.Desc<>ctnClass then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody method found in non class',UpperClassName,'.',UpperMethodName,' in ',SrcTool.MainFilename,' Node=',ClassNode.DescAsString]);
      exit;
    end;
    SrcClassName:=SrcTool.ExtractClassName(ClassNode,true);
    ANode:=SrcTool.FindMethodNodeInImplementation(
                                             SrcClassName,UpperMethodName,true);
    if ANode=nil then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody method not found ',SrcClassName,'.',UpperMethodName,' in ',SrcTool.MainFilename]);
      exit;
    end;
    Result:=SrcTool.FindJumpPointInProcNode(ANode,NewPos,NewTopLine);
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.RenamePublishedMethod(const UpperClassName,
  UpperOldMethodName, NewMethodName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(false);
  ClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
  Result:=RenamePublishedMethod(ClassNode,UpperOldMethodName,NewMethodName,
                                SourceChangeCache);
end;

function TEventsCodeTool.RenamePublishedMethod(ClassNode: TCodeTreeNode;
  const UpperOldMethodName, NewMethodName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
// rename published method in class and in procedure itself
var ProcNode, ProcHeadNode: TCodeTreeNode;
  NameStart, NameEnd: integer;
  UpperClassName: string;
  ProcBodyNode: TCodeTreeNode;
begin
  Result:=false;
  if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) then
    RaiseException('Invalid class node');
  if (UpperOldMethodName='') then
    RaiseException('Invalid UpperOldMethodName="'+UpperOldMethodName+'"');
  if (NewMethodName='') then
    RaiseException('Invalid NewMethodName="'+NewMethodName+'"');
  if (SourceChangeCache=nil) or (Scanner=nil) then
    RaiseException('Invalid SourceChangeCache or Scanner');
  SourceChangeCache.MainScanner:=Scanner;
  // rename in class
  ProcNode:=FindIdentifierNodeInClass(ClassNode,@UpperOldMethodName[1]);
  if (ProcNode=nil) then begin
    MoveCursorToNodeStart(ClassNode);
    RaiseExceptionFmt(ctsOldMethodNotFound,[UpperOldMethodName]);
  end;
  if (ProcNode.Desc<>ctnProcedure) then begin
    MoveCursorToNodeStart(ProcNode);
    RaiseExceptionFmt(ctsOldMethodNotFound,[UpperOldMethodName]);
  end;
  ProcHeadNode:=ProcNode.FirstChild;
  if ProcHeadNode=nil then begin
    MoveCursorToNodeStart(ProcNode);
    RaiseException('Invalid proc header');
  end;
  NameStart:=ProcHeadNode.StartPos;
  NameEnd:=NameStart;
  while (NameEnd<=SrcLen) and (IsIdentChar[Src[NameEnd]]) do
    inc(NameEnd);
  if not SourceChangeCache.Replace(gtNone,gtNone,NameStart,NameEnd,
      NewMethodName)
  then begin
    MoveCursorToNodeStart(ProcHeadNode);
    RaiseException('Unable to rename method declaration');
  end;
  // main goal achieved
  Result:=true;

  // rename procedure body -> find implementation node
  UpperClassName:=ExtractClassName(ClassNode,true);
  ProcBodyNode:=FindMethodNodeInImplementation(UpperClassName,
                                               UpperOldMethodName,false);
  if (ProcBodyNode<>nil) and (ProcBodyNode<>nil) then begin
    ProcHeadNode:=ProcBodyNode.FirstChild;
    MoveCursorToNodeStart(ProcHeadNode);
    ReadNextAtom; // read class name
    ReadNextAtom; // read '.'
    ReadNextAtom; // read method name
    SourceChangeCache.Replace(gtNone,gtNone,
        CurPos.StartPos,CurPos.EndPos,NewMethodName);
  end;
  
  Result:=SourceChangeCache.Apply;
end;

function TEventsCodeTool.CreateMethod(const UpperClassName,
  AMethodName: string; ATypeInfo: PTypeInfo;
  const APropertyUnitName, APropertyPath: string;
  SourceChangeCache: TSourceChangeCache;
  UseTypeInfoForParameters: boolean;
  Section: TPascalClassSection;
  const CallAncestorMethod: string): boolean;
var AClassNode: TCodeTreeNode;
begin
  Result:=false;
  BuildTree(false);
  AClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
  Result:=CreateMethod(AClassNode,AMethodName,ATypeInfo,
                       APropertyUnitName,APropertyPath,
                       SourceChangeCache,UseTypeInfoForParameters,Section,
                       CallAncestorMethod);
end;

function TEventsCodeTool.CreateMethod(ClassNode: TCodeTreeNode;
  const AMethodName: string; ATypeInfo: PTypeInfo;
  const APropertyUnitName, APropertyPath: string;
  SourceChangeCache: TSourceChangeCache; UseTypeInfoForParameters: boolean;
  Section: TPascalClassSection;
  const CallAncestorMethod: string): boolean;

  procedure AddNeededUnits(const AFindContext: TFindContext);
  var
    MethodUnitName: String;
    ProcHeadNode: TCodeTreeNode;
    ParamListNode: TCodeTreeNode;
    Node: TCodeTreeNode;
    ParamNode: TCodeTreeNode;
  begin
    MethodUnitName:=AFindContext.Tool.GetSourceName(false);
    AddNeededUnitToMainUsesSection(PChar(MethodUnitName));

    // search every parameter type and collect units
    if not (AFindContext.Tool is TCodeCompletionCodeTool) then exit;
    if not (AFindContext.Node.Desc in [ctnProcedureType,ctnProcedure]) then exit;
    ProcHeadNode:=AFindContext.Node.FirstChild;
    if (ProcHeadNode=nil) or (ProcHeadNode.Desc<>ctnProcedureHead) then exit;
    if ProcHeadNode.FirstChild=nil then
      AFindContext.Tool.BuildSubTreeForProcHead(ProcHeadNode);
    ParamListNode:=ProcHeadNode.FirstChild;
    if (ParamListNode=nil) or (ParamListNode.Desc<>ctnParameterList) then exit;
    Node:=ParamListNode.FirstChild;
    while Node<>nil do begin
      ParamNode:=Node.FirstChild;
      if ParamNode<>nil then begin
        TCodeCompletionCodeTool(AFindContext.Tool).
          AddNeededUnitsToMainUsesSectionForRange(ParamNode.StartPos,ParamNode.EndPos,Self);
      end;
      Node:=Node.NextBrother;
    end;
  end;

  function FindPropertyType(out FindContext: TFindContext): boolean;
  var
    Tool: TFindDeclarationTool;
  begin
    Result:=false;
    if APropertyPath<>'' then begin
      // find unit of property
      Tool:=nil;
      FindContext:=CleanFindContext;
      if APropertyUnitName='' then begin
        Tool:=Self;
      end else begin
        Tool:=FindCodeToolForUsedUnit(APropertyUnitName,'',true);
        if Tool=nil then
          raise Exception.Create('failed to get codetool for unit '+APropertyUnitName);
      end;
      // find property with type
      if not Tool.FindDeclarationOfPropertyPath(APropertyPath,FindContext,true)
      then begin
        DebugLn(['FindPropertyType FindDeclarationOfPropertyPath failed: ',Tool.MainFilename,' APropertyPath=',APropertyPath]);
        exit;
      end;
      if FindContext.Node.Desc<>ctnProperty then
        FindContext.Tool.RaiseException(
          APropertyPath+' is not a property.'
          +' See '+FindContext.Tool.MainFilename
          +' '+FindContext.Tool.CleanPosToStr(FindContext.Node.StartPos));
      // find type
      FindContext:=(FindContext.Tool as TEventsCodeTool)
                                              .FindMethodTypeInfo(ATypeInfo,'');
    end else
      FindContext:=FindMethodTypeInfo(ATypeInfo,APropertyUnitName);
    Result:=true;
  end;
  
var
  CleanMethodDefinition, MethodDefinition: string;
  FindContext: TFindContext;
  ATypeData: PTypeData;
  NewSection: TNewClassPart;
  InsertCall: String;
  ProcBody: String;
  BeautifyCodeOpts: TBeautifyCodeOptions;
begin
  Result:=false;
  try
    if (ClassNode=nil) or (ClassNode.Desc<>ctnClass) or (AMethodName='')
    or (ATypeInfo=nil) or (SourceChangeCache=nil) or (Scanner=nil) then exit;
    { $IFDEF CTDEBUG}
    DebugLn(['[TEventsCodeTool.CreateMethod] A AMethodName="',AMethodName,'" in "',MainFilename,'" UseTypeInfoForParameters=',UseTypeInfoForParameters]);
    { $ENDIF}
    // initialize class for code completion
    CodeCompleteClassNode:=ClassNode;
    CodeCompleteSrcChgCache:=SourceChangeCache;
    // check if method definition already exists in class
    if UseTypeInfoForParameters then begin
      // do not lookup the declaration in the source, use RTTI instead
      ATypeData:=GetTypeData(ATypeInfo);
      if ATypeData=nil then exit(false);
      CleanMethodDefinition:=UpperCaseStr(AMethodName)
                         +MethodTypeDataToStr(ATypeData,
                         [phpWithoutClassName, phpWithoutName, phpInUpperCase]);
    end else begin
      // search typeinfo in source
      if not FindPropertyType(FindContext) then exit;
      AddNeededUnits(FindContext);
      CleanMethodDefinition:=UpperCaseStr(AMethodName)
             +FindContext.Tool.ExtractProcHead(FindContext.Node,
                         [phpWithoutClassName, phpWithoutName, phpInUpperCase]);
    end;
    if not ProcExistsInCodeCompleteClass(CleanMethodDefinition) then begin
      { $IFDEF CTDEBUG}
      DebugLn('[TEventsCodeTool.CreateMethod] insert method definition to class');
      { $ENDIF}
      // insert method definition into class
      InsertCall:='';
      if UseTypeInfoForParameters then begin
        MethodDefinition:=MethodTypeDataToStr(ATypeData,
                    [phpWithStart, phpWithoutClassKeyword, phpWithoutClassName,
                     phpWithoutName, phpWithVarModifiers, phpWithParameterNames,
                     phpWithDefaultValues, phpWithResultType]);
        if CallAncestorMethod<>'' then begin
          InsertCall:=CallAncestorMethod+MethodTypeDataToStr(ATypeData,
                      [phpWithoutClassKeyword, phpWithoutClassName,
                       phpWithoutName, phpWithParameterNames,
                       phpWithoutParamTypes]);
        end;
      end else begin
        MethodDefinition:=TrimCodeSpace(FindContext.Tool.ExtractProcHead(
                     FindContext.Node,
                    [phpWithStart, phpWithoutClassKeyword, phpWithoutClassName,
                     phpWithoutName, phpWithVarModifiers, phpWithParameterNames,
                     phpWithDefaultValues, phpWithResultType]));
        if CallAncestorMethod<>'' then begin
          InsertCall:=CallAncestorMethod
                    +TrimCodeSpace(FindContext.Tool.ExtractProcHead(
                     FindContext.Node,
                    [phpWithoutClassKeyword, phpWithoutClassName,
                     phpWithoutName, phpWithParameterNames,
                     phpWithoutParamTypes]));
        end;
      end;
      { $IFDEF CTDEBUG}
      DebugLn('[TEventsCodeTool.CreateMethod] MethodDefinition="',MethodDefinition,'"');
      { $ENDIF}
      if Section in [pcsPublished,pcsPublic] then
        NewSection:=ncpPublishedProcs
      else
        NewSection:=ncpPrivateProcs;
      ProcBody:='';
      if InsertCall<>'' then begin
        BeautifyCodeOpts:=SourceChangeCache.BeautifyCodeOptions;
        ProcBody:=SourceChangeCache.BeautifyCodeOptions.
                         AddClassAndNameToProc(MethodDefinition,
                         ExtractClassName(CodeCompleteClassNode,false),
                         AMethodName)
          +BeautifyCodeOpts.LineEnd
          +'begin'+BeautifyCodeOpts.LineEnd
          +GetIndentStr(BeautifyCodeOpts.Indent)
            +InsertCall+BeautifyCodeOpts.LineEnd
          +'end;';
        //DebugLn(['TEventsCodeTool.CreateMethod ProcBody=""',ProcBody,'']);
      end;
        
      MethodDefinition:=SourceChangeCache.BeautifyCodeOptions.
                         AddClassAndNameToProc(MethodDefinition, '', AMethodName);
      AddClassInsertion(CleanMethodDefinition, MethodDefinition, AMethodName,
                        NewSection,nil,ProcBody);
    end;
    { $IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.CreateMethod] invoke class completion');
    { $ENDIF}
    if not InsertAllNewClassParts then
      RaiseException(ctsErrorDuringInsertingNewClassParts);
    if not CreateMissingProcBodies then
      RaiseException(ctsErrorDuringCreationOfNewProcBodies);
    if not InsertAllNewUnitsToMainUsesSection then
      RaiseException(ctsErrorDuringInsertingNewUsesSection);

    // apply the changes
    if not SourceChangeCache.Apply then
      RaiseException(ctsUnableToApplyChanges);
    { $IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.CreateMethod] END');
    { $ENDIF}
    Result:=true;
  finally
    FreeClassInsertionList;
  end;
end;

function TEventsCodeTool.CreateExprListFromMethodTypeData(
  TypeData: PTypeData; Params: TFindDeclarationParams;
  out List: TExprTypeList): boolean;
var i, ParamCount, Len, Offset: integer;
  CurTypeIdentifier: string;
  OldInput: TFindDeclarationInput;
  CurExprType: TExpressionType;
  Reverse: Boolean;
  {$IFDEF VerboseTypeData}
  CurParamName: string;
  {$ENDIF}
begin
  {$IFDEF VerboseTypeData}
  DebugLn('[TEventsCodeTool.CreateExprListFromMethodTypeData] START');
  {$ENDIF}
  Result:=false;
  List:=TExprTypeList.Create;
  if TypeData=nil then exit(true);
  ParamCount:=TypeData^.ParamCount;
  {$IFDEF VerboseTypeData}
  DebugLn(['[TEventsCodeTool.CreateExprListFromMethodTypeData] ParamCount=',ParamCount]);
  {$ENDIF}
  Reverse:=ReverseRTTIParamList;
  if ParamCount>0 then begin
    Offset:=0;
    
    for i:=0 to ParamCount-1 do begin
    
      // skip ParamFlags
      // ToDo: check this: SizeOf(TParamFlags) is 4, but the data is only 1 byte
      Len:=1; // typinfo.pp comment is wrong: SizeOf(TParamFlags)
      inc(Offset,Len);

      // skip ParamName
      Len:=ord(TypeData^.ParamList[Offset]);
      {$IFDEF VerboseTypeData}
      SetLength(CurParamName,Len);
      if Len>0 then
        Move(TypeData^.ParamList[Offset+1],CurParamName[1],Len);
      {$ENDIF}
      inc(Offset,Len+1);

      // read ParamType
      Len:=ord(TypeData^.ParamList[Offset]);
      inc(Offset);
      SetLength(CurTypeIdentifier,Len);
      if CurTypeIdentifier<>'' then
        Move(TypeData^.ParamList[Offset],CurTypeIdentifier[1],Len);
      inc(Offset,Len);
      
      {$IFDEF VerboseTypeData}
      DebugLn('[TEventsCodeTool.CreateExprListFromMethodTypeData] A ',
      ' i=',dbgs(i),'/',dbgs(ParamCount),
      ' ParamName=',CurParamName,
      ' TypeIdent=',CurTypeIdentifier
      );
      {$ENDIF}

      // convert ParamType to TExpressionType
      Params.Save(OldInput);
      Params.SetIdentifier(Self,@CurTypeIdentifier[1],nil);
      Params.Flags:=[fdfSearchInParentNodes,
                     fdfIgnoreCurContextNode]
                     +(fdfGlobals*Params.Flags)
                     -[fdfSearchInAncestors];
      CurExprType:=GetExpressionTypeOfTypeIdentifier(Params);
      {$IFDEF VerboseTypeData}
      DebugLn('[TEventsCodeTool.CreateExprListFromMethodTypeData] B ',
      ' i=',dbgs(i),'/',dbgs(ParamCount),
      ' Ident=',CurTypeIdentifier,
      ' CurExprType=',ExprTypeToString(CurExprType)
      );
      {$ENDIF}
      if Reverse then
        List.AddFirst(CurExprType)
      else
        List.Add(CurExprType);
      Params.Load(OldInput,true);
    end;
  end;
  {$IFDEF VerboseTypeData}
  DebugLn('[TEventsCodeTool.CreateExprListFromMethodTypeData] END');
  {$ENDIF}
  Result:=true;
end;

function TEventsCodeTool.CollectPublishedMethods(
  Params: TFindDeclarationParams; const FoundContext: TFindContext
  ): TIdentifierFoundResult;
var
  ParamCompatibility: TTypeCompatibility;
  FirstParameterNode: TCodeTreeNode;
  ProcName: PChar;
begin
  //DebugLn(['TEventsCodeTool.CollectPublishedMethods Node=',FoundContext.Node.DescAsString]);
  if (FoundContext.Node.Desc=ctnProcedure)
  and (FoundContext.Node.Parent<>nil)
  and (FoundContext.Node.Parent.Desc=ctnClassPublished) then begin
    {$IFDEF ShowAllProcs}
    DebugLn('');
    DebugLn('[TEventsCodeTool.CollectPublishedMethods] A ',
    ' Node=',FoundContext.Node.DescAsString,
    ' "',copy(FoundContext.Tool.Src,FoundContext.Node.StartPos,50),'"',
    ' Tool=',FoundContext.Tool.MainFilename);
    {$ENDIF}
    FirstParameterNode:=FoundContext.Tool.GetFirstParameterNode(
      FoundContext.Node);
    // check if the found proc fits into
    // the method mask (= current expression list)
    ParamCompatibility:=FoundContext.Tool.IsParamNodeListCompatibleToExprList(
      SearchedExprList,
      FirstParameterNode,
      Params,SearchedCompatibilityList);
    {$IFDEF ShowAllProcs}
    DebugLn('[TEventsCodeTool.CollectPublishedMethods] A',
      ' ParamCompatibility=',TypeCompatibilityNames[ParamCompatibility]);
    {$ENDIF}
    if ParamCompatibility=tcExact then begin

      // ToDo: ppu, dcu

      ProcName:=FoundContext.Tool.GetProcNameIdentifier(FoundContext.Node);
      if fGatheredCompatibleMethods.Find(ProcName)=nil then begin
        // new method name -> add
        fGatheredCompatibleMethods.Add(ProcName);
      end;
    end;
  end;
  Result:=ifrProceedSearch;
end;

function TEventsCodeTool.CompleteComponent(AComponent,
  AncestorComponent: TComponent;
  SourceChangeCache: TSourceChangeCache): boolean;
{ - Adds all missing published variable declarations to the class definition
    in the source
}
var
  UpperClassName: String;
  i: Integer;
  CurComponent: TComponent;
  VarName: String;
  UpperCurComponentName: String;
  VarType: String;
begin
  try
    Result:=false;
    ClearIgnoreErrorAfter;
    BuildTree(true);
    UpperClassName:=UpperCaseStr(AComponent.ClassName);
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.CompleteComponent] A Component="',AComponent.Name,':',AComponent.ClassName);
    {$ENDIF}
    // initialize class for code completion
    CodeCompleteClassNode:=FindClassNodeInInterface(UpperClassName,true,false,true);
    CodeCompleteSrcChgCache:=SourceChangeCache;
    // complete all child components
    for i:=0 to AComponent.ComponentCount-1 do begin
      CurComponent:=AComponent.Components[i];
      DebugLn('[TEventsCodeTool.CompleteComponent]  CurComponent=',CurComponent.Name,':',CurComponent.ClassName);
      VarName:=CurComponent.Name;
      if VarName='' then continue;
      if (AncestorComponent<>nil)
      and (AncestorComponent.FindComponent(VarName)<>nil) then continue;
      UpperCurComponentName:=UpperCaseStr(VarName);
      VarType:=CurComponent.ClassName;
      // add missing published variable
      if VarExistsInCodeCompleteClass(UpperCurComponentName) then begin
      end else begin
        //DebugLn('[TEventsCodeTool.CompleteComponent] ADDING variable ',CurComponent.Name,':',CurComponent.ClassName);
        AddClassInsertion(UpperCurComponentName,
                VarName+':'+VarType+';',VarName,ncpPublishedVars);
      end;
    end;
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.CompleteComponent] invoke class completion');
    {$ENDIF}
    Result:=ApplyClassCompletion(false);
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.CompleteComponent] END');
    {$ENDIF}
  finally
    FreeClassInsertionList;
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
    if VisibilityNode.Desc in AllClassBaseSections then begin
      Result:=VisibilityNode.FirstChild;
      while Result<>nil do begin
        if CompareSrcIdentifiers(Result.FirstChild.StartPos,Identifier) then
        begin
          exit;
        end;
        Result:=Result.NextBrother;
      end;
    end;
    VisibilityNode:=VisibilityNode.NextBrother;
  end;
  Result:=nil;
end;

end.

