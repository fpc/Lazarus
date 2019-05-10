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
    TEventsCodeTool enhances TChangeDeclarationTool.
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
{$DEFINE VerboseTypeData}
{off $DEFINE ShowAllProcs}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, TypInfo, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils,
  // Codetools
  FileProcs, CodeToolsStrConsts, CodeTree, CodeCache, PascalParserTool,
  CodeCompletionTool, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeToolsStructs, SourceChanger, FindDeclarationTool, ChangeDeclarationTool;

type
  { TEventsCodeTool }

  TEventsCodeTool = class(TChangeDeclarationTool)
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
  
    function GetCompatiblePublishedMethods(const AClassName: string;
        PropInstance: TPersistent; const PropName: string;
        const Proc: TGetStrProc): boolean;
    function GetCompatiblePublishedMethods(const AClassName: string;
        TypeData: PTypeData; const Proc: TGetStrProc): boolean;
    function GetCompatiblePublishedMethods(ClassNode: TCodeTreeNode;
        TypeData: PTypeData; const Proc: TGetStrProc): boolean;
    function PublishedMethodExists(const AClassName, AMethodName: string;
        PropInstance: TPersistent; const PropName: string;
        out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function PublishedMethodExists(const AClassName, AMethodName: string;
        TypeData: PTypeData;
        out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function PublishedMethodExists(ClassNode: TCodeTreeNode;
        const AMethodName: string; TypeData: PTypeData;
        out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean
        ): boolean;
    function JumpToPublishedMethodBody(const AClassName,
        AMethodName: string;
        out NewPos: TCodeXYPosition; out NewTopLine,BlockTopLine,BlockBottomLine: integer;
        ErrorOnNotFound: boolean = false): boolean;
    function RenamePublishedMethod(const AClassName, AOldMethodName,
        NewMethodName: string; SourceChangeCache: TSourceChangeCache): boolean;
    function RenamePublishedMethod(ClassNode: TCodeTreeNode;
        const AOldMethodName, NewMethodName: string;
        SourceChangeCache: TSourceChangeCache): boolean;
        
    function CreateMethod(const AClassName,
        AMethodName: string; ATypeInfo: PTypeInfo;
        const APropertyUnitName, APropertyPath: string;
        SourceChangeCache: TSourceChangeCache;
        UseTypeInfoForParameters: boolean = false;
        Section: TPascalClassSection = pcsPublished;
        const CallAncestorMethod: string = '';
        AddOverride: boolean = false): boolean;
    function CreateMethod(ClassNode: TCodeTreeNode;
        const AMethodName: string;
        ATypeInfo: PTypeInfo; const APropertyUnitName, APropertyPath: string;
        SourceChangeCache: TSourceChangeCache;
        UseTypeInfoForParameters: boolean = false;
        Section: TPascalClassSection = pcsPublished;
        const CallAncestorMethod: string = '';
        AddOverride: boolean = false): boolean;

    function FindClassOfInstance(Instance: TObject;
        out FindContext: TFindContext; ExceptionOnNotFound: boolean): boolean;
    function FindTypeOfInstanceProperty(Instance: TPersistent;
        const PropName: string; out TypeContext: TFindContext;
        ExceptionOnNotFound: boolean): boolean;
    function CreateExprListFromInstanceProperty(Instance: TPersistent;
        const PropName: string; UseRTTI: boolean = false): TExprTypeList;

    function CreateExprListFromMethodTypeData(TypeData: PTypeData;
        Params: TFindDeclarationParams; out List: TExprTypeList): boolean;
    function CreateExprListFromMethodTypeData(Instance: TPersistent;
        const PropName: string;
        Params: TFindDeclarationParams; out List: TExprTypeList): boolean;
    function FindPublishedMethodNodeInClass(ClassNode: TCodeTreeNode;
        const AMethodName: string;
        ExceptionOnNotFound: boolean): TFindContext;
    function FindMethodNodeInImplementation(const AClassName,
        AMethodName: string; BuildTreeBefore: boolean): TCodeTreeNode;
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
      Len:={$IF FPC_FULLVERSION>=30000}SizeOf(TParamFlags){$ELSE}1{$ENDIF};
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
  const AClassName: string; TypeData: PTypeData;
  const Proc: TGetStrProc): boolean;
var ClassNode: TCodeTreeNode;
begin
  Result:=false;
  ActivateGlobalWriteLock;
  try
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] A UpperClassName=',
      AClassName);
    {$ENDIF}
    BuildTree(lsrImplementationStart);
    ClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
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
  const Proc: TGetStrProc): boolean;
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
  if (ClassNode=nil) or (not (ClassNode.Desc in [ctnClass,ctnObjCClass])) or (TypeData=nil)
  or (Proc=nil) or (fGatheredCompatibleMethods<>nil) then exit;
  Params:=nil;
  ActivateGlobalWriteLock;
  FreeAndNil(SearchedExprList);
  try
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

function TEventsCodeTool.PublishedMethodExists(const AClassName, AMethodName: string;
  PropInstance: TPersistent; const PropName: string;
  out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
var
  FoundContext: TFindContext;
  CompListSize: integer;
  ParamCompatibility: TTypeCompatibility;
  FirstParameterNode: TCodeTreeNode;
  Params: TFindDeclarationParams;
  ClassNode: TCodeTreeNode;
begin
  Result:=false;
  MethodIsCompatible:=false;
  IdentIsMethod:=false;
  MethodIsPublished:=false;
  FreeAndNil(SearchedExprList);

  if (AClassName='') or (AMethodName='') or (PropInstance=nil) or (PropName='')
  then exit;

  BuildTree(lsrImplementationStart);
  //debugln(['TEventsCodeTool.PublishedMethodExists START']);
  ClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
  //debugln(['TEventsCodeTool.PublishedMethodExists classnode=',ClassNode.DescAsString]);

  Params:=TFindDeclarationParams.Create;
  try
    // first search a published method definition with same name
    Params.ContextNode:=ClassNode;
    Params.SetIdentifier(Self,@AMethodName[1],nil);
    Params.Flags:=[fdfSearchInAncestors];
    if not FindIdentifierInContext(Params) then begin
      debugln(['TEventsCodeTool.PublishedMethodExists method not found ',AMethodName]);
      exit;
    end;
    IdentIsMethod:=(Params.NewNode.Desc=ctnProcedure);
    MethodIsPublished:=(Params.NewNode.Parent.Desc=ctnClassPublished);
    if (not IdentIsMethod) or (not MethodIsPublished) then begin
      debugln(['TEventsCodeTool.PublishedMethodExists not a method: ',AMethodName]);
      exit;
    end;
    // published method with same name found
    FoundContext:=CreateFindContext(Params);
    // -> test for compatibility

    // convert the event property to an expression type list
    SearchedExprList:=CreateExprListFromInstanceProperty(PropInstance,PropName);
    if SearchedExprList=nil then begin
      debugln(['TEventsCodeTool.PublishedMethodExists invalid property ',DbgSName(PropInstance),'.',PropName]);
      exit;
    end;

    //debugln(['TEventsCodeTool.PublishedMethodExists SearchedExprList=[',SearchedExprList.AsString,']']);
    // create compatibility list
    CompListSize:=SizeOf(TTypeCompatibility)*SearchedExprList.Count;
    if CompListSize>0 then begin
      GetMem(SearchedCompatibilityList,CompListSize);
    end else begin
      SearchedCompatibilityList:=nil;
    end;
    try
      // check if the method fits into the event
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
  finally
    FreeAndNil(SearchedExprList);
    Params.Free;
  end;
end;

function TEventsCodeTool.FindPublishedMethodNodeInClass(
  ClassNode: TCodeTreeNode; const AMethodName: string;
  ExceptionOnNotFound: boolean): TFindContext;
var
  Params: TFindDeclarationParams;
begin
  Result:=CleanFindContext;
  if (ClassNode=nil) or (not (ClassNode.Desc in [ctnClass, ctnObjCClass])) or (AMethodName='')
  or (Scanner=nil) then begin
    DebugLn(['TEventsCodeTool.FindPublishedMethodNodeInClass invalid parameters']);
    exit;
  end;
  ActivateGlobalWriteLock;
  Params:=nil;
  try
    Params:=TFindDeclarationParams.Create;
    Params.ContextNode:=ClassNode;
    Params.SetIdentifier(Self,@AMethodName[1],nil);
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

function TEventsCodeTool.FindMethodNodeInImplementation(const AClassName,
  AMethodName: string; BuildTreeBefore: boolean): TCodeTreeNode;
var SectionNode, ANode: TCodeTreeNode;
begin
  Result:=nil;
  if (AMethodName='') or (AClassName='') then exit;
  if BuildTreeBefore then BuildTree(lsrInitializationStart);
  // find implementation node
  SectionNode:=Tree.Root;
  while SectionNode<>nil do begin
    if SectionNode.Desc in [ctnProgram,ctnImplementation] then begin
      ANode:=SectionNode.FirstChild;
      {$IFDEF CTDEBUG}
      DebugLn('[TEventsCodeTool.FindMethodNodeInImplementation] A AMethodName=',AClassName,'.',AMethodName);
      {$ENDIF}
      while (ANode<>nil) do begin
        if (ANode.Desc=ctnProcedure) and (ANode.FirstChild<>nil)
        and CompareSrcIdentifiers(ANode.FirstChild.StartPos,@AClassName[1])
        then begin
          MoveCursorToNodeStart(ANode.FirstChild);
          ReadNextAtom; // read class name
          ReadNextAtom; // read '.'
          if AtomIsChar('.') then begin
            ReadNextAtom;
            if CompareSrcIdentifiers(CurPos.StartPos,@AMethodName[1]) then
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
    SectionNode:=SectionNode.NextBrother;
  end;
end;

function TEventsCodeTool.FindMethodTypeInfo(ATypeInfo: PTypeInfo;
  const AStartUnitName: string): TFindContext;
var
  Tool: TFindDeclarationTool;

  procedure RaiseTypeNotFound;
  begin
    RaiseException(20170421201954,'type '+ATypeInfo^.Name+' not found, because tool is '+dbgsname(Tool));
  end;
  
var
  TypeName: string;
  Params: TFindDeclarationParams;
  TypeContext: TFindContext;
  ContextNode: TCodeTreeNode;
begin
  Result:=CleanFindContext;
  if AStartUnitName<>'' then begin
    // start searching in another unit
    Tool:=FindCodeToolForUsedUnit(AStartUnitName,'',true);
    if not (Tool is TEventsCodeTool) then
      RaiseTypeNotFound;
    TEventsCodeTool(Tool).BuildTree(lsrImplementationStart);
    Result:=TEventsCodeTool(Tool).FindMethodTypeInfo(ATypeInfo,'');
    exit;
  end;

  ActivateGlobalWriteLock;
  try
    // find method type declaration
    TypeName:=ATypeInfo^.Name;
    // find method in interface and used units
    ContextNode:=FindImplementationNode;
    if ContextNode=nil then
      ContextNode:=FindMainBeginEndNode;
    if ContextNode=nil then
      ContextNode:=FindInterfaceNode;
    if ContextNode=nil then begin
      MoveCursorToNodeStart(Tree.Root);
      RaiseExceptionFmt(20170421202000,ctsIdentifierNotFound,[GetIdentifier(@TypeName[1])]);
    end;
    Params:=TFindDeclarationParams.Create(Self,ContextNode);
    try
      Params.SetIdentifier(Self,@TypeName[1],nil);
      Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInParentNodes];
      {$IFDEF VerboseMethodPropEdit}
      DebugLn(['TEventsCodeTool.FindMethodTypeInfo TypeName=',TypeName,' MainFilename=',MainFilename,' ContextNode=',ContextNode.DescAsString]);
      {$ENDIF}
      FindIdentifierInContext(Params);
      // find proc node
      if Params.NewNode.Desc<>ctnTypeDefinition then begin
        Params.NewCodeTool.MoveCursorToNodeStart(Params.NewNode);
        Params.NewCodeTool.RaiseException(20170421202006,ctsMethodTypeDefinitionNotFound);
      end;
      TypeContext:=CreateFindContext(Params);
    finally
      Params.Free;
    end;
    Params:=TFindDeclarationParams.Create(Self,nil);
    try
      Params.Flags:=[fdfExceptionOnNotFound,fdfSearchInParentNodes];
      Result:=TypeContext.Tool.FindBaseTypeOfNode(Params,TypeContext.Node);
      if Result.Node=nil then begin
        TypeContext.Tool.MoveCursorToNodeStart(TypeContext.Node);
        TypeContext.Tool.RaiseException(20170421202013,ctsMethodTypeDefinitionNotFound);
      end;
      if not (Result.Node.Desc in AllProcTypes) then begin
        TypeContext.Tool.MoveCursorToNodeStart(TypeContext.Node);
        TypeContext.Tool.RaiseExceptionFmt(20170421202019,ctsExpectedAMethodTypeButFound, [
          Result.Node.DescAsString]);
      end;
    finally
      Params.Free;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(const AClassName, AMethodName: string;
  TypeData: PTypeData;
  out MethodIsCompatible, MethodIsPublished, IdentIsMethod: boolean): boolean;
var ClassNode: TCodeTreeNode;
begin
  ActivateGlobalWriteLock;
  try
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.PublishedMethodExists] A AClassName=',AClassName);
    {$ENDIF}
    BuildTree(lsrImplementationStart);
    ClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
    {$IFDEF CTDEBUG}
    DebugLn('[TEventsCodeTool.PublishedMethodExists] B ',dbgs(ClassNode<>nil));
    {$ENDIF}
    Result:=PublishedMethodExists(ClassNode,AMethodName,TypeData,
               MethodIsCompatible,MethodIsPublished,IdentIsMethod);
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.PublishedMethodExists(ClassNode: TCodeTreeNode;
  const AMethodName: string; TypeData: PTypeData;
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
  IdentIsMethod:=false;
  MethodIsPublished:=false;
  ActivateGlobalWriteLock;
  FreeAndNil(SearchedExprList);
  try
    Params:=TFindDeclarationParams.Create;
    try
      // first search a published method definition with same name
      Params.ContextNode:=ClassNode;
      Params.SetIdentifier(Self,@AMethodName[1],nil);
      Params.Flags:=[fdfSearchInAncestors];
      if FindIdentifierInContext(Params) then begin
        IdentIsMethod:=(Params.NewNode.Desc=ctnProcedure);
        MethodIsPublished:=(Params.NewNode.Parent.Desc=ctnClassPublished);
        if IdentIsMethod and MethodIsPublished then begin
          // published method with same name found
          FoundContext:=CreateFindContext(Params);
          // -> test for compatibility

          // convert the TypeData to an expression type list
          Params.ContextNode:=Params.NewNode;
          if not CreateExprListFromMethodTypeData(TypeData,Params,SearchedExprList)
          then
            exit(false);
          debugln(['TEventsCodeTool.PublishedMethodExists SearchedExprList=[',SearchedExprList.AsString,']']);
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

function TEventsCodeTool.JumpToPublishedMethodBody(const AClassName,
  AMethodName: string; out NewPos: TCodeXYPosition; out NewTopLine,
  BlockTopLine, BlockBottomLine: integer; ErrorOnNotFound: boolean): boolean;
var
  ANode: TCodeTreeNode;
  ClassNode: TCodeTreeNode;
  AFindContext: TFindContext;
  SrcTool: TEventsCodeTool;
  SrcClassName: String;
  Caret: TCodeXYPosition;
begin
  Result:=false;
  ActivateGlobalWriteLock;
  try
    BuildTree(lsrInitializationStart);
    ClassNode:=FindClassNodeInInterface(AClassName,true,false,ErrorOnNotFound);
    if ClassNode=nil then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody class not found: ',AClassName]);
      exit;
    end;
    AFindContext:=FindPublishedMethodNodeInClass(ClassNode,AMethodName,ErrorOnNotFound);
    if AFindContext.Node=nil then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody method not found: ',AClassName,'.',AMethodName]);
      exit;
    end;
    SrcTool:=TEventsCodeTool(AFindContext.Tool);
    ClassNode:=AFindContext.Node.Parent.Parent;
    if not (ClassNode.Desc in [ctnClass,ctnObjCClass]) then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody method found in non class: ',AClassName,'.',AMethodName,' in ',SrcTool.MainFilename,' Node=',ClassNode.DescAsString]);
      if ErrorOnNotFound then
        RaiseExceptionFmt(20170421202025,'method "%s" not found in class "%s" (%s) in %s',
          [AClassName,AMethodName,ClassNode.DescAsString,SrcTool.MainFilename]);
      exit;
    end;
    SrcClassName:=SrcTool.ExtractClassName(ClassNode,true);
    ANode:=SrcTool.FindMethodNodeInImplementation(SrcClassName,AMethodName,true);
    if ANode=nil then begin
      DebugLn(['TEventsCodeTool.JumpToPublishedMethodBody method body not found ',SrcClassName,'.',AMethodName,' in ',SrcTool.MainFilename]);
      if ErrorOnNotFound then
        RaiseExceptionFmt(20170421202044,'implementation of method "%s.%s" in %s', [AClassName,AMethodName,SrcTool.MainFilename]);
      exit;
    end;
    Result:=SrcTool.FindJumpPointInProcNode(ANode,NewPos,NewTopLine, BlockTopLine, BlockBottomLine);
    if Result then
    begin
      if CleanPosToCaret(ANode.StartPos, Caret) then
        BlockTopLine := Caret.Y
      else
        BlockTopLine := NewPos.Y;
      if CleanPosToCaret(ANode.EndPos, Caret) then
        BlockBottomLine := Caret.Y
      else
        BlockBottomLine := NewPos.Y;
    end;
  finally
    DeactivateGlobalWriteLock;
  end;
end;

function TEventsCodeTool.RenamePublishedMethod(const AClassName,
  AOldMethodName, NewMethodName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
var ClassNode: TCodeTreeNode;
begin
  BuildTree(lsrEnd);
  ClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
  Result:=RenamePublishedMethod(ClassNode,AOldMethodName,NewMethodName,
                                SourceChangeCache);
end;

function TEventsCodeTool.RenamePublishedMethod(ClassNode: TCodeTreeNode;
  const AOldMethodName, NewMethodName: string;
  SourceChangeCache: TSourceChangeCache): boolean;
// rename published method in class and in procedure itself
var ProcNode, ProcHeadNode: TCodeTreeNode;
  NameStart, NameEnd: integer;
  AClassName: string;
  ProcBodyNode: TCodeTreeNode;
begin
  Result:=false;
  if (ClassNode=nil) or not (ClassNode.Desc in [ctnClass,ctnObjCClass]) then
    RaiseException(20170421202048,'Invalid class node');
  if (AOldMethodName='') then
    RaiseException(20170421202051,'Invalid AOldMethodName="'+AOldMethodName+'"');
  if (NewMethodName='') then
    RaiseException(20170421202054,'Invalid NewMethodName="'+NewMethodName+'"');
  if (SourceChangeCache=nil) or (Scanner=nil) then
    RaiseException(20170421202056,'Invalid SourceChangeCache or Scanner');
  SourceChangeCache.MainScanner:=Scanner;
  // rename in class
  ProcNode:=FindIdentifierNodeInClass(ClassNode,@AOldMethodName[1]);
  if (ProcNode=nil) then begin
    MoveCursorToNodeStart(ClassNode);
    RaiseExceptionFmt(20170421202101,ctsOldMethodNotFound,[AOldMethodName]);
  end;
  if (ProcNode.Desc<>ctnProcedure) then begin
    MoveCursorToNodeStart(ProcNode);
    RaiseExceptionFmt(20170421202103,ctsOldMethodNotFound,[AOldMethodName]);
  end;
  ProcHeadNode:=ProcNode.FirstChild;
  if ProcHeadNode=nil then begin
    MoveCursorToNodeStart(ProcNode);
    RaiseException(20170421202106,'Invalid proc header');
  end;
  NameStart:=ProcHeadNode.StartPos;
  NameEnd:=NameStart;
  while (NameEnd<=SrcLen) and (IsIdentChar[Src[NameEnd]]) do
    inc(NameEnd);
  if not SourceChangeCache.Replace(gtNone,gtNone,NameStart,NameEnd,
      NewMethodName)
  then begin
    MoveCursorToNodeStart(ProcHeadNode);
    RaiseException(20170421202109,'Unable to rename method declaration');
  end;
  // main goal achieved
  Result:=true;

  // rename procedure body -> find implementation node
  AClassName:=ExtractClassName(ClassNode,false);
  ProcBodyNode:=FindMethodNodeInImplementation(AClassName,
                                               AOldMethodName,false);
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

function TEventsCodeTool.CreateMethod(const AClassName, AMethodName: string;
  ATypeInfo: PTypeInfo; const APropertyUnitName, APropertyPath: string;
  SourceChangeCache: TSourceChangeCache; UseTypeInfoForParameters: boolean;
  Section: TPascalClassSection; const CallAncestorMethod: string;
  AddOverride: boolean): boolean;
var AClassNode: TCodeTreeNode;
begin
  Result:=false;
  BuildTree(lsrInitializationStart);
  AClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
  Result:=CreateMethod(AClassNode,AMethodName,ATypeInfo,
                       APropertyUnitName,APropertyPath,
                       SourceChangeCache,UseTypeInfoForParameters,Section,
                       CallAncestorMethod,AddOverride);
end;

function TEventsCodeTool.CreateMethod(ClassNode: TCodeTreeNode;
  const AMethodName: string; ATypeInfo: PTypeInfo; const APropertyUnitName,
  APropertyPath: string; SourceChangeCache: TSourceChangeCache;
  UseTypeInfoForParameters: boolean; Section: TPascalClassSection;
  const CallAncestorMethod: string; AddOverride: boolean): boolean;

  procedure AddNeededUnits(const AFindContext: TFindContext);
  var
    MethodUnitName: String;
    ProcHeadNode: TCodeTreeNode;
    ParamListNode: TCodeTreeNode;
    Node: TCodeTreeNode;
    ParamNode: TCodeTreeNode;
  begin
    Node:=AFindContext.Node;
    MethodUnitName:=AFindContext.Tool.GetSourceName(false);
    AddNeededUnitToMainUsesSection(PChar(MethodUnitName));

    // search every parameter type and collect units
    if not (AFindContext.Tool is TCodeCompletionCodeTool) then exit;
    if Node.Desc=ctnReferenceTo then begin
      Node:=Node.FirstChild;
      if Node=nil then exit;
    end;
    if not (Node.Desc in [ctnProcedureType,ctnProcedure]) then exit;
    ProcHeadNode:=Node.FirstChild;
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
      {$IFDEF VerboseMethodPropEdit}
      debugln(['FindPropertyType APropertyPath="',APropertyPath,'"']);
      {$ENDIF}
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
      {$IFDEF VerboseMethodPropEdit}
      debugln(['FindPropertyType SUCCESS Tool=',Tool.MainFilename,' Found APropertyPath="',APropertyPath,'" Found=',FindContextToString(FindContext)]);
      {$ENDIF}
      if FindContext.Node.Desc<>ctnProperty then
        FindContext.Tool.RaiseException(20170421202114,
          APropertyPath+' is not a property.'
          +' See '+FindContext.Tool.MainFilename
          +' '+FindContext.Tool.CleanPosToStr(FindContext.Node.StartPos));
      // find type
      {$IFDEF VerboseMethodPropEdit}
      debugln(['FindPropertyType ATypeInfo^.Name=',ATypeInfo^.Name]);
      {$ENDIF}
      FindContext:=(FindContext.Tool as TEventsCodeTool)
                                              .FindMethodTypeInfo(ATypeInfo,'');
      {$IFDEF VerboseMethodPropEdit}
      debugln(['FindPropertyType SUCCESS Found MethodTypeInfo="',FindContextToString(FindContext),'"']);
      {$ENDIF}
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
  Beauty: TBeautifyCodeOptions;
begin
  Result:=false;
  try
    if (ClassNode=nil) or (not (ClassNode.Desc in [ctnClass,ctnObjCClass])) or (AMethodName='')
    or (ATypeInfo=nil) or (SourceChangeCache=nil) or (Scanner=nil) then begin
      debugln(['TEventsCodeTool.CreateMethod failed, missing parameter']);
      exit;
    end;
    if CallAncestorMethod<>'' then
      AddOverride:=true;
    {$IFDEF VerboseMethodPropEdit}
    DebugLn(['[TEventsCodeTool.CreateMethod] A AMethodName="',AMethodName,'" in "',MainFilename,'"',
      ' APropertyUnitName="',APropertyUnitName,'"',
      ' APropertyPath="',APropertyPath,'"',
      ' UseTypeInfoForParameters=',UseTypeInfoForParameters,
      ' CallAncestorMethod="',CallAncestorMethod,'"',
      ' AddOverride=',AddOverride]);
    {$ENDIF}
    // initialize class for code completion
    CodeCompleteClassNode:=ClassNode;
    CodeCompleteSrcChgCache:=SourceChangeCache;
    // check if method definition already exists in class
    if UseTypeInfoForParameters then begin
      // do not lookup the declaration in the source, use RTTI instead
      ATypeData:=GetTypeData(ATypeInfo);
      if ATypeData=nil then begin
        {$IFDEF VerboseMethodPropEdit}
        DebugLn('ERROR: [TEventsCodeTool.CreateMethod] GetTypeData failed');
        {$ENDIF}
        exit;
      end;
      CleanMethodDefinition:=UpperCaseStr(AMethodName)
                         +MethodTypeDataToStr(ATypeData,
                         [phpWithoutClassName, phpWithoutName, phpInUpperCase]);
    end else begin
      // search typeinfo in source
      if not FindPropertyType(FindContext) then begin
        {$IFDEF VerboseMethodPropEdit}
        DebugLn('ERROR: [TEventsCodeTool.CreateMethod] FindPropertyType failed');
        {$ENDIF}
        exit;
      end;
      AddNeededUnits(FindContext);
      CleanMethodDefinition:=UpperCaseStr(AMethodName)
             +FindContext.Tool.ExtractProcHead(FindContext.Node,
                         [phpWithoutClassName, phpWithoutName, phpInUpperCase]);
    end;
    if not ProcExistsInCodeCompleteClass(CleanMethodDefinition,not AddOverride)
    then begin
      // insert method definition into class
      {$IFDEF VerboseMethodPropEdit}
      DebugLn('[TEventsCodeTool.CreateMethod] insert method definition to class');
      {$ENDIF}
      InsertCall:='';
      if CompareTextIgnoringSpace(CallAncestorMethod,'inherited',false)=0 then
        InsertCall:=CallAncestorMethod+';';
      if UseTypeInfoForParameters then begin
        MethodDefinition:=MethodTypeDataToStr(ATypeData,
                    [phpWithStart, phpWithoutClassKeyword, phpWithoutClassName,
                     phpWithoutName, phpWithVarModifiers, phpWithParameterNames,
                     phpWithDefaultValues, phpWithResultType]);
        if (InsertCall='') and (CallAncestorMethod<>'') then begin
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
        if (InsertCall='') and (CallAncestorMethod<>'') then begin
          InsertCall:=CallAncestorMethod
                    +TrimCodeSpace(FindContext.Tool.ExtractProcHead(
                     FindContext.Node,
                    [phpWithoutClassKeyword, phpWithoutClassName,
                     phpWithoutName, phpWithParameterNames,
                     phpWithoutParamTypes]));
        end;
      end;
      {$IFDEF VerboseMethodPropEdit}
      DebugLn('[TEventsCodeTool.CreateMethod] MethodDefinition="',MethodDefinition,'"');
      {$ENDIF}
      if Section in [pcsPublished,pcsPublic] then
        NewSection:=ncpPublishedProcs
      else
        NewSection:=ncpPrivateProcs;
      ProcBody:='';
      if InsertCall<>'' then begin
        Beauty:=SourceChangeCache.BeautifyCodeOptions;
        ProcBody:=SourceChangeCache.BeautifyCodeOptions.
                         AddClassAndNameToProc(MethodDefinition,
                         ExtractClassName(CodeCompleteClassNode,false),
                         AMethodName)
          +Beauty.LineEnd
          +'begin'+Beauty.LineEnd
          +Beauty.GetIndentStr(Beauty.Indent)
            +InsertCall+Beauty.LineEnd
          +'end;';
        //DebugLn(['TEventsCodeTool.CreateMethod ProcBody=""',ProcBody,'']);
      end;
        
      MethodDefinition:=SourceChangeCache.BeautifyCodeOptions.
                         AddClassAndNameToProc(MethodDefinition, '', AMethodName);
      AddClassInsertion(CleanMethodDefinition, MethodDefinition, AMethodName,
                        NewSection,nil,ProcBody);
    end;
    {$IFDEF VerboseMethodPropEdit}
    DebugLn('[TEventsCodeTool.CreateMethod] invoke class completion');
    {$ENDIF}
    if not InsertAllNewClassParts then
      RaiseException(20170421202116,ctsErrorDuringInsertingNewClassParts);
    if not CreateMissingClassProcBodies(false) then
      RaiseException(20170421202118,ctsErrorDuringCreationOfNewProcBodies);
    if not InsertAllNewUnitsToMainUsesSection then
      RaiseException(20170421202120,ctsErrorDuringInsertingNewUsesSection);

    // apply the changes
    if not SourceChangeCache.Apply then
      RaiseException(20170421202122,ctsUnableToApplyChanges);
    {$IFDEF VerboseMethodPropEdit}
    DebugLn('[TEventsCodeTool.CreateMethod] END');
    {$ENDIF}
    Result:=true;
  finally
    FreeClassInsertionList;
  end;
end;

function TEventsCodeTool.FindClassOfInstance(Instance: TObject;
  out FindContext: TFindContext; ExceptionOnNotFound: boolean): boolean;
var
  AClassName: String;

  procedure RaiseClassNotFound;
  begin
    RaiseExceptionFmt(20170421202124,ctsClassSNotFound, [AClassName]);
  end;

var
  Node: TCodeTreeNode;
  AUnitName: String;
  Tool: TFindDeclarationTool;
begin
  Result:=false;
  //debugln(['TEventsCodeTool.FindClassOfInstance START']);
  FindContext:=CleanFindContext;
  AClassName:=Instance.ClassName;
  if AClassName='' then exit;
  AUnitName:=Instance.UnitName;
  {$IFDEF VerboseMethodPropEdit}
  debugln(['TEventsCodeTool.FindClassOfInstance Unit=',ExtractFileNameOnly(MainFilename),' Class=',AClassName,' Instance.Unit=',AUnitName]);
  {$ENDIF}
  if (AUnitName='')
  or (CompareIdentifiers(PChar(ExtractFileNameOnly(MainFilename)),
    PChar(AUnitName))=0)
  then begin
    // search in this unit
    BuildTree(lsrInitializationStart);
    FindContext.Node:=FindClassNodeInUnit(AClassName,true,false,false,
                                          ExceptionOnNotFound);
    if FindContext.Node=nil then begin
      debugln(['TEventsCodeTool.FindClassOfInstance FindClassNodeInUnit failed']);
      if ExceptionOnNotFound then RaiseClassNotFound;
      exit;
    end;
    FindContext.Tool:=Self;
    exit(true);
  end;

  // find unit
  // Note: when a component was loaded by an ancestor, its class may not
  //       be in the uses section of the current unit
  Tool:=FindCodeToolForUsedUnit(AUnitName,'',ExceptionOnNotFound);
  if Tool=nil then exit(false);

  // find class
  Node:=Tool.FindDeclarationNodeInInterface(AClassName,true);
  // check if it is a class
  if (Node=nil)
  or (not (Node.Desc in [ctnTypeDefinition,ctnGenericType]))
  or (Node.LastChild=nil)
  or (not (Node.LastChild.Desc in AllClassObjects)) then begin
    debugln(['TEventsCodeTool.FindClassOfInstance found node is not a class: ',Node.DescAsString]);
    if ExceptionOnNotFound then RaiseClassNotFound;
    exit;
  end;
  FindContext.Node:=Node.LastChild;
  FindContext.Tool:=Tool;
  Result:=true;
end;

function TEventsCodeTool.FindTypeOfInstanceProperty(Instance: TPersistent;
  const PropName: string; out TypeContext: TFindContext;
  ExceptionOnNotFound: boolean): boolean;
var
  AClassName: String;
  AClassContext: TFindContext;
  Params: TFindDeclarationParams;
  PropNode: TCodeTreeNode;
begin
  Result:=false;
  //debugln(['TEventsCodeTool.FindTypeOfPropertyInfo START']);
  TypeContext:=CleanFindContext;
  if (Instance=nil) then exit;

  AClassName:=Instance.ClassName;
  if AClassName='' then begin
    debugln(['TEventsCodeTool.FindTypeOfPropertyInfo instance has no class name']);
    exit;
  end;
  if PropName='' then begin
    debugln(['TEventsCodeTool.FindTypeOfPropertyInfo prop info has no class name']);
    exit;
  end;

  // search class of instance
  //debugln(['TEventsCodeTool.FindTypeOfPropertyInfo searching class of instance ...']);
  if not FindClassOfInstance(Instance,AClassContext,ExceptionOnNotFound) then exit;
  //debugln(['TEventsCodeTool.FindTypeOfPropertyInfo found: ',FindContextToString(AClassContext)]);

  // search property
  Params:=TFindDeclarationParams.Create(AClassContext.Tool, AClassContext.Node);
  try
    Params.Flags:=[fdfSearchInAncestors,fdfSearchInHelpers];
    if ExceptionOnNotFound then Include(Params.Flags,fdfExceptionOnNotFound);
    Params.SetIdentifier(Self,PChar(PropName),nil);
    if not AClassContext.Tool.FindIdentifierInContext(Params) then begin
      RaiseException(20170421202129,'property not found '+DbgSName(Instance)+'.'+PropName);
      exit;
    end;
    if Params.NewNode=nil then exit;
    PropNode:=Params.NewNode;
    if PropNode.Desc<>ctnProperty then begin
      debugln(['TEventsCodeTool.FindTypeOfPropertyInfo identifier ',DbgSName(Instance)+'.'+PropName,' is not property, found ',PropNode.DescAsString]);
      RaiseException(20170421202135,'identifier is not a property: '+DbgSName(Instance)+'.'+PropName);
    end;
    // search base type of property
    TypeContext:=Params.NewCodeTool.FindBaseTypeOfNode(Params,PropNode);
    //debugln(['TEventsCodeTool.FindTypeOfPropertyInfo ',FindContextToString(TypeContext)]);
    Result:=true;
  finally
    Params.Free;
  end;
end;

function TEventsCodeTool.CreateExprListFromInstanceProperty(
  Instance: TPersistent; const PropName: string; UseRTTI: boolean
  ): TExprTypeList;
var
  Params: TFindDeclarationParams;
  TypeContext: TFindContext;
begin
  Result:=nil;
  Params:=TFindDeclarationParams.Create;
  try
    if UseRTTI then begin
      if not CreateExprListFromMethodTypeData(Instance,PropName,Params,Result)
      then
        exit;
    end else begin
      if not FindTypeOfInstanceProperty(Instance,PropName,TypeContext,true) then exit;
      if not (TypeContext.Node.Desc in AllProcTypes) then begin
        debugln(['TEventsCodeTool.CreateExprListFromPropertyInfo property '+DbgSName(Instance)+'.'+PropName+' is not method: ',TypeContext.Node.DescAsString]);
        exit;
      end;
      Result:=TypeContext.Tool.CreateParamExprListFromProcNode(TypeContext.Node,Params);
    end;
  finally
    Params.Free;
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
  if (TypeData=nil) then exit(true);
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
                     -[fdfSearchInAncestors,fdfSearchInHelpers];
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

function TEventsCodeTool.CreateExprListFromMethodTypeData(
  Instance: TPersistent; const PropName: string;
  Params: TFindDeclarationParams; out List: TExprTypeList): boolean;
var
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  Result:=false;
  PropInfo:=GetPropInfo(Instance,PropName);
  if PropInfo=nil then begin
    debugln(['TEventsCodeTool.CreateExprListFromMethodTypeData property not found: ',DbgSName(Instance),' property=',PropName]);
    exit;
  end;
  if PropInfo^.PropType^.Kind<>tkMethod then begin
    debugln(['TEventsCodeTool.CreateExprListFromMethodTypeData property is not a method: ',DbgSName(Instance),' property=',PropName]);
    exit;
  end;
  TypeData:=GetTypeData(PropInfo^.PropType);
  Result:=CreateExprListFromMethodTypeData(TypeData,Params,List);
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

function TEventsCodeTool.CompleteComponent(AComponent, AncestorComponent: TComponent;
  SourceChangeCache: TSourceChangeCache): boolean;
{ - Adds all missing published variable declarations to the class definition
    in the source
}
var
  i: Integer;
  CurComponent: TComponent;
  VarName, VarType: String;
  UpperClassName, UpperCompName: String;
begin
  try
    Result:=false;
    ClearIgnoreErrorAfter;
    BuildTree(lsrImplementationStart);
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
      {$IFDEF CTDEBUG}
      DebugLn('[TEventsCodeTool.CompleteComponent]  CurComponent=',CurComponent.Name,':',CurComponent.ClassName);
      {$ENDIF}
      VarName:=CurComponent.Name;
      if VarName='' then continue;
      if (AncestorComponent<>nil)
      and (AncestorComponent.FindComponent(VarName)<>nil) then continue;
      UpperCompName:=UpperCaseStr(VarName);
      VarType:=CurComponent.ClassName;
      // add missing published variable
      if not VarExistsInCodeCompleteClass(UpperCompName) then begin
        //DebugLn('[TEventsCodeTool.CompleteComponent] ADDING variable ',CurComponent.Name,':',CurComponent.ClassName);
        AddClassInsertion(UpperCompName,VarName+':'+VarType+';',VarName,ncpPublishedVars);
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

function TEventsCodeTool.GetCompatiblePublishedMethods(
  const AClassName: string; PropInstance: TPersistent; const PropName: string;
  const Proc: TGetStrProc): boolean;
var
  Params: TFindDeclarationParams;
  CompListSize: Integer;
  ClassNode: TCodeTreeNode;
  Node: TAVLTreeNode;
  ProcName: String;
begin
  Result:=false;
  // find class
  BuildTree(lsrImplementationStart);
  //debugln(['TEventsCodeTool.GetCompatiblePublishedMethods START']);
  ClassNode:=FindClassNodeInInterface(AClassName,true,false,true);
  {$IFDEF VerboseMethodPropEdit}
  debugln(['TEventsCodeTool.GetCompatiblePublishedMethods ClassName="',AClassName,'" PropInstance=',DbgSName(PropInstance),' PropName="',PropName,'" classnode=',ClassNode.DescAsString]);
  {$ENDIF}
  // create type list of property
  SearchedExprList:=nil;
  SearchedCompatibilityList:=nil;
  fGatheredCompatibleMethods:=nil;
  Params:=TFindDeclarationParams.Create;
  try
    SearchedExprList:=CreateExprListFromInstanceProperty(PropInstance,PropName);
    {$IFDEF VerboseMethodPropEdit}
    debugln(['TEventsCodeTool.GetCompatiblePublishedMethods ExprList=',SearchedExprList.AsString]);
    {$ENDIF}
    fGatheredCompatibleMethods:=TAVLTree.Create(@CompareIdentifierPtrs);
    // create compatibility list
    CompListSize:=SizeOf(TTypeCompatibility)*SearchedExprList.Count;
    if CompListSize>0 then
      GetMem(SearchedCompatibilityList,CompListSize);
    // search all compatible published procs
    Params.ContextNode:=ClassNode;
    Params.Flags:=[fdfCollect,fdfSearchInAncestors];
    Params.SetIdentifier(Self,nil,@CollectPublishedMethods);
    {$IFDEF VerboseMethodPropEdit}
    DebugLn('[TEventsCodeTool.GetCompatiblePublishedMethods] Searching ...');
    {$ENDIF}
    FindIdentifierInContext(Params);

    // collect all method names
    Node:=fGatheredCompatibleMethods.FindLowest;
    while Node<>nil do begin
      ProcName:=GetIdentifier(Node.Data);
      Proc(ProcName);
      Node:=fGatheredCompatibleMethods.FindSuccessor(Node);
    end;
    Result:=true;
  finally
    if SearchedCompatibilityList<>nil then begin
      FreeMem(SearchedCompatibilityList);
      SearchedCompatibilityList:=nil;
    end;
    FreeAndNil(SearchedExprList);
    Params.Free;
    FreeAndNil(fGatheredCompatibleMethods);
  end;
end;

function TEventsCodeTool.FindIdentifierNodeInClass(ClassNode: TCodeTreeNode;
  Identifier: PChar): TCodeTreeNode;
var
  VisibilityNode: TCodeTreeNode;
begin
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

